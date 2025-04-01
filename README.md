# mio
An easy to use header-only cross-platform C++20 memory mapping library with an MIT license.

mio has been created with the goal to be easily includable (i.e. no dependencies) in any C++ project that needs memory mapped file IO without the need to pull in Boost.

Please feel free to open an issue, I'll try to address any concerns as best I can.

### Why?
Because memory mapping is the best thing since sliced bread!

More seriously, the primary motivation for writing this library instead of using Boost.Iostreams, was the lack of support for establishing a memory mapping with an already open file handle/descriptor. This is possible with mio.

Furthermore, Boost.Iostreams' solution requires that the user pick offsets exactly at page boundaries, which is cumbersome and error prone. mio, on the other hand, manages this internally, accepting any offset and finding the nearest page boundary.

Albeit a minor nitpick, Boost.Iostreams implements memory mapped file IO with a `std::shared_ptr` to provide shared semantics, even if not needed, and the overhead of the heap allocation may be unnecessary and/or unwanted.
In mio, there are two classes to cover the two use-cases: one that is move-only (basically a zero-cost abstraction over the system specific mmapping functions), and the other that acts just like its Boost.Iostreams counterpart, with shared semantics.

### How to Create a Memory Mapping

> **Note:** The file must exist and be non-empty before mapping.

There are two primary ways to create a memory mapping:

#### 1. Using the Constructor

Directly construct a memory mapping. On failure, a `std::system_error` is thrown.

```c++
mio::mmap_source mmap(path, offset, size_to_map);
```

You can also omit the `offset` and `size_to_map` parameters to map the entire file:

```c++
mio::mmap_source mmap(path);
```

#### 2. Using the Member Function

Alternatively, create an uninitialized mapping and then invoke the `map` member function. Like the constructor, this API throws an exception on error.

```c++
mio::mmap_source mmap;
mmap.map(path, offset, size_to_map);
```

Or simply map the entire file:

```c++
mmap.map(path);
```

**Important:** All these APIs now use exceptions for error reporting. Internally, error codes are still used, but they are hidden from the external interface.

Moreover, you may provide either a file path (as any common string type) or an existing valid file handle. For example:

```c++
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <mio/mmap.hpp>
#include <algorithm>

int main() {
    // Ensure the file exists before mapping.
    const int fd = open("file.txt", O_RDONLY);
    mio::mmap_source mmap(fd, 0, mio::map_entire_file);
    // ...
}
```

**Windows Users:** Wide character types are supported for path parameters.

---

### Example

Below is a sample program that demonstrates both read-write and read-only mappings. Notice that no error code parameters are needed—the APIs throw exceptions upon failure.

```c++
#include <mio/mmap.hpp>
#include <cstdio>
#include <cassert>
#include <algorithm>
#include <fstream>
#include <iostream>

void allocate_file(const std::string& path, int size) {
    std::ofstream file(path);
    file << std::string(size, '0');
}

int main() {
    const std::string path = "file.txt";
    allocate_file(path, 155);

    // Create a read-write mapping for the entire file.
    mio::mmap_sink rw_mmap = mio::make_mmap_sink(path, 0, mio::map_entire_file);

    // Fill the mapping with 'a' characters.
    std::fill(rw_mmap.begin(), rw_mmap.end(), 'a');

    // Modify each byte.
    for (auto& b : rw_mmap) {
        b += 10;
    }

    // Change a single byte in the middle.
    const int mid = rw_mmap.size() / 2;
    rw_mmap[mid] = 42;

    // Flush changes and unmap. If the mapping goes out of scope, the destructor will also flush.
    rw_mmap.sync();
    rw_mmap.unmap();

    // Create a read-only mapping of the entire file.
    mio::mmap_source ro_mmap;
    ro_mmap.map(path);

    // Verify that the modification was successful.
    assert(ro_mmap[mid] == 42);

    std::printf("All tests passed!\n");
    return 0;
}
```

---

### Additional Features

This version of `mio` takes advantage of modern C++ features:

- **Source Location and Filesystem:**  
  Debugging is improved by leveraging `std::source_location` to report detailed context (file name, function name, line, and column) when assertions fail. Filesystem support now enables more natural path handling.
  
- **Future C++23 Support:**  
  In C++23 and above, assertions will include a full stack trace using `std::stacktrace`, making it easier to trace runtime errors.

For example, the new assertion function might look like:

```c++
#if __cplusplus >= 202002L
inline void cpp2020_assert(bool condition, const char* errorMessage,
                             std::source_location location = std::source_location::current()) {
    if (!condition) {
        std::cout << "Error: " << errorMessage << "\n"
                  << "File: " << location.file_name() << "\n"
                  << "Function: " << location.function_name() << "\n"
                  << "Line: " << location.line() << "\n"
                  << "Column: " << location.column() << std::endl;
#if __cplusplus >= 202300L
        std::cout << "Stack trace:\n";
        for (const auto& frame : std::stacktrace::current())
            std::cout << frame << std::endl;
#endif
        throw std::runtime_error(errorMessage);
    }
}
#endif
```

---

### Test Suite Overview

The test suite now combines both the new features and the core mapping functionality. It demonstrates:

- Mapping an entire file or a portion of it.
- Both read-write and read-only mappings.
- Mapping with a file descriptor as well as a file path.
- Validation of mapped content against expected data.
- Handling of invalid mapping cases without exposing internal error codes.

For example, one test case maps the file at various offsets to verify that the correct segment of the file is mapped:

```c++
void test_at_offset(const std::string& buffer, const char* path, size_t offset) {
    // Map the file region starting at the given offset.
    mio::mmap_source file_view = mio::make_mmap_source(path, offset, mio::map_entire_file);
    assert(file_view.is_open());
    // Compare the mapped content to the original buffer...
}
```

This comprehensive test case confirms that the modernized `mio` library is robust and adheres to modern C++ best practices.

### Test Suite

The test code below serves a dual purpose. It demonstrates the standard usage of the library (mapping, reading, writing, and unmapping files) while also showcasing intentional error cases. In the error cases, invalid inputs (such as an invalid file path, empty path, or incorrect file handle) will trigger internal assertion exceptions. This change highlights that internal error checking no longer uses `std::error_code` in the public API.

```c++
#include <string>
#include <fstream>
#include <iostream>
#include <cassert>
#include <numeric>
#include <vector>

#include "../single_include/mio/mio.hpp"

// Just make sure this compiles.
#include <cstddef>
using mmap_source = mio::basic_mmap_source<std::byte>;

template<class MMap>
void test_at_offset(const MMap& file_view, const std::string& buffer, const size_t offset);
inline void test_at_offset(const std::string& buffer, const char* path, const size_t offset);

inline void allocate_file(const std::string& path, const int size)
{
    std::ofstream file(path);
    std::string s(size, '0');
    file << s;
}

inline void test_rewrite_file()
{
    const auto path = "test_rewrite.txt";

    // NOTE: mio does *not* create the file for you if it doesn't exist!
    // You must ensure that the file exists and is non-empty before mapping.
    allocate_file(path, 204800);

    // Create a read-write mapping for the entire file.
    mio::mmap_sink rw_mmap = mio::make_mmap_sink(
        path, 0, mio::map_entire_file);

    // Use any iterator-based function to modify the mapping.
    std::fill(rw_mmap.begin(), rw_mmap.end(), 'a');

    // Or manually iterate through the mapped region and change each byte.
    for (auto& b : rw_mmap)
    {
        b += 10;
    }

    // Change one specific byte using the subscript operator.
    const int answer_index = rw_mmap.size() / 2;
    rw_mmap[answer_index] = 42;

    // Flush changes and unmap.
    rw_mmap.sync();
    rw_mmap.unmap();

    // Create a read-only mapping for the entire file.
    mio::mmap_source ro_mmap;
    ro_mmap.map(path);

    const int the_answer_to_everything = ro_mmap[answer_index];
    assert(the_answer_to_everything == 42);
}

inline void test_error_case(char* path, const std::string& buffer)
{
    // Macro to check that an invalid mapping results in an empty mapping.
#define CHECK_INVALID_MMAP(m) do { \
        assert(m.empty()); \
        assert(!m.is_open()); \
        } while(0)

    mio::mmap_source m;

    // Attempt mapping an invalid file name.
    m = mio::make_mmap_source("garbage-that-hopefully-doesnt-exist", 0, 0);
    CHECK_INVALID_MMAP(m);

    // Attempt mapping with an empty path.
    m = mio::make_mmap_source(static_cast<const char*>(0), 0, 0);
    CHECK_INVALID_MMAP(m);
    m = mio::make_mmap_source(std::string(), 0, 0);
    CHECK_INVALID_MMAP(m);

    // Attempt mapping with an invalid handle.
    m = mio::make_mmap_source(mio::invalid_handle, 0, 0);
    CHECK_INVALID_MMAP(m);

    // Attempt mapping with an invalid offset.
    m = mio::make_mmap_source(path, 100 * buffer.size(), buffer.size());
    CHECK_INVALID_MMAP(m);
}

int main()
{
    std::system("chcp 65001");

    // Verify that mio compiles with non-const char* strings too.
    const char _path[] = "test-file";
    const int path_len = sizeof(_path);
    char* path = new char[path_len];
    std::copy(_path, _path + path_len, path);

    const auto page_size = mio::page_size();
    // Prepare a buffer and write it to a file.
    const int file_size = 4 * page_size - 250; // For example, 16134 bytes for a 4KiB page size.
    std::string buffer(file_size, 0);
    // Fill buffer starting at the first printable ASCII character.
    char v = 33;
    for (auto& b : buffer) {
        b = v;
        ++v;
        // Cycle back after reaching the last printable ASCII character.
        v %= 126;
        if (v == 0) {
            v = 33;
        }
    }

    std::ofstream file(path);
    file << buffer;
    file.close();

    // Test mapping the whole file.
    test_at_offset(buffer, path, 0);

    // Test mapping starting from an offset just below the page size.
    test_at_offset(buffer, path, page_size - 3);

    // Test mapping starting from an offset just above the page size.
    test_at_offset(buffer, path, page_size + 3);

    // Test mapping starting from an offset further ahead.
    test_at_offset(buffer, path, 2 * page_size + 3);

    std::cout << "Continuing with tests..." << std::endl;

    // Uncomment the next line to run tests for error cases.
    // Note: In these cases, the internal assertion exceptions will be thrown.
    // test_error_case(path, buffer);

    // The following code ensures that all API variations compile correctly.
    {
        mio::ummap_source _1;
        mio::shared_ummap_source _2;
        // shared_mmap mapping compiles as well.
        mio::shared_mmap_source _3(path, 0, mio::map_entire_file);
        auto _4 = mio::make_mmap_source(path);
        auto _5 = mio::make_mmap<mio::shared_mmap_source>(path, 0, mio::map_entire_file);
#ifdef _WIN32
        const std::wstring wpath1 = L"file";

        // If the file exists, perform mapping.
        if (std::filesystem::exists(wpath1))
        {
            auto _6 = mio::make_mmap_source(wpath1);
            mio::mmap_source _7;
            _7.map(wpath1);
        }
        else
        {
            std::wcerr << L"Cannot open file: " << wpath1 << std::endl;
        }

        // Even if the file cannot be opened, the following lines are executed.
        const std::wstring wpath2 = wpath1 + L"000";
        if (std::filesystem::exists(wpath2))
        {
            auto _8 = mio::make_mmap_source(wpath2);
            mio::mmap_source _9;
            _9.map(wpath1);
        }
        else
        {
            std::wcerr << L"Cannot open file: " << wpath2 << std::endl;
        }
#else
        const char* path = "path_to_file";  // Replace with an actual file path
        const int fd = open(path, O_RDONLY);

        if (fd < 0)
        {
            std::cerr << "Failed to open file: " << path << std::endl;
        }
        else
        {
            // File opened successfully; perform mmap operations.
            mio::mmap_source _fdmmap(fd, 0, mio::map_entire_file);
            // Unmap if needed.
            _fdmmap.unmap();
            // Remap using the same file descriptor.
            _fdmmap.map(fd);

            // Close the file descriptor if it's no longer needed.
            close(fd);
        }
#endif
    }

    std::printf("all tests passed!\n");
    return 0;
}

void test_at_offset(const std::string& buffer, const char* path, const size_t offset)
{
    // Sanity check.
    assert(offset < buffer.size());

    // Map the region of the file starting at the given offset.
    mio::mmap_source file_view = mio::make_mmap_source(path, offset, mio::map_entire_file);

    assert(file_view.is_open());
    const size_t mapped_size = buffer.size() - offset;
    assert(file_view.size() == mapped_size);

    test_at_offset(file_view, buffer, offset);

    // Convert the mapping to a shared mmap.
    mio::shared_mmap_source shared_file_view(std::move(file_view));
    assert(!file_view.is_open());
    assert(shared_file_view.is_open());
    assert(shared_file_view.size() == mapped_size);

    // Optionally, you can run the test on the shared mapping as well.
    // test_at_offset(shared_file_view, buffer, offset);
}

template<class MMap>
void test_at_offset(const MMap& file_view, const std::string& buffer, const size_t offset)
{
    // Verify that the bytes in the mapping match those in the buffer.
    for (size_t buf_idx = offset, view_idx = 0;
         buf_idx < buffer.size() && view_idx < file_view.size();
         ++buf_idx, ++view_idx)
    {
        if (file_view[view_idx] != buffer[buf_idx])
        {
            std::printf("%luth byte mismatch: expected(%d) <> actual(%d)",
                        buf_idx, buffer[buf_idx], file_view[view_idx]);
            std::cout << std::flush;
            assert(0);
        }
    }
}
```

```c++
#include <random>
#include <iomanip>
#include <iostream>
#include <cassert>
#include <cstddef>
#include <string>

// Make sure to include your mio header.
#include "../single_include/mio/mio.hpp"

inline void test_rewrite_random_file()
{
	const auto path = "test_rewrite_random.dat";

	// Ensure the file exists and is non-empty.
	// 20MB = 20971520 Bytes
	allocate_file(path, 20971520);

	// Seed the random number generator.
	std::random_device rd;
	std::mt19937 gen(rd());
	std::uniform_int_distribution<> dis(0, 255);

	// Create a read-write mapping for the entire file.
	mio::mmap_sink rw_mmap = mio::make_mmap_sink(path, 0, mio::map_entire_file);

	// Fill the mapping with random binary bytes.
	for (auto& byte : rw_mmap)
	{
		byte = static_cast<char>(dis(gen));
	}

	// Choose an offset near the end (e.g., 75% into the file) and write the value 42.
	const size_t answer_index = rw_mmap.size() * 3 / 4;
	rw_mmap[answer_index] = static_cast<char>(42);

	// Flush changes and unmap.
	rw_mmap.sync();
	rw_mmap.unmap();

	// Reopen the file as a read-only mapping.
	mio::mmap_source ro_mmap;
	ro_mmap.map(path);

	// Verify that the byte at the chosen offset is 42.
	const int the_answer = static_cast<int>(ro_mmap[answer_index]);
	assert(the_answer == 42);

	// Print the entire file content in hexadecimal format.
	std::cout << "Hex dump of " << path << ":\n";
	for (size_t i = 0; i < ro_mmap.size(); ++i)
	{
		//last 10240 bytes are printed in one line
		if (ro_mmap.size() - 1 - i < 10240)
		{
			std::cout << std::hex << std::setw(2) << std::setfill('0')
				<< static_cast<int>(static_cast<unsigned char>(ro_mmap[i])) << " ";
			if ((i + 1) % 16 == 0)
				std::cout << "\n";
		}
	}
	std::cout << std::dec << "\n"; // Restore default number format.
}

int main()
{
    test_rewrite_random_file();
}
```

---

### Key Points

- **Error Handling via Exceptions:**  
  The revised API now uses internal assertions that throw exceptions when encountering errors. This design change replaces the previous use of `std::error_code` in the public API. In the test cases, mapping failures (for example, due to an invalid path) trigger these assertion exceptions.  

- **Demonstrated Error Cases:**  
  The `test_error_case` function illustrates various invalid inputs (non-existent files, empty paths, invalid handles, and incorrect offsets). These cases are now shown explicitly to guide developers on how the library responds to erroneous usage. (By default, these tests are commented out to avoid interrupting the normal flow; they can be enabled for debugging.)

- **Modern C++ Features:**  
  The test suite (along with the rest of the library) leverages modern C++ features such as source location for better debug messages. Future updates may also incorporate stack traces when using C++23 or later.

---

### Single Header File

`mio` can be added to your project as a single header file by including:

```c++
#include "single_include/mio/mio.hpp"
```

## CMake
As a header-only library, mio has no compiled components. Nevertheless, a [CMake](https://cmake.org/overview/) build system is provided to allow easy testing, installation, and subproject composition on many platforms and operating systems.

### Testing
Mio is distributed with a small suite of tests and examples.
When mio is configured as the highest level CMake project, this suite of executables is built by default.
Mio's test executables are integrated with the CMake test driver program, [CTest](https://cmake.org/cmake/help/latest/manual/ctest.1.html).

CMake supports a number of backends for compilation and linking.

To use a static configuration build tool, such as GNU Make or Ninja:

```sh
cd <mio source directory>
mkdir build
cd build

# Configure the build
cmake -D CMAKE_BUILD_TYPE=<Debug | Release> \
      -G <"Unix Makefiles" | "Ninja"> ..

# build the tests
< make | ninja | cmake --build . >

# run the tests
< make test | ninja test | cmake --build . --target test | ctest >
```

To use a dynamic configuration build tool, such as Visual Studio or Xcode:

```sh
cd <mio source directory>
mkdir build
cd build

# Configure the build
cmake -G <"Visual Studio 14 2015 Win64" | "Xcode"> ..

# build the tests
cmake --build . --config <Debug | Release>

# run the tests via ctest...
ctest --build-config <Debug | Release>

# ... or via CMake build tool mode...
cmake --build . --config <Debug | Release> --target test
```

Of course the **build** and **test** steps can also be executed via the **all** and **test** targets, respectively, from within the IDE after opening the project file generated during the configuration step.

Mio's testing is also configured to operate as a client to the [CDash](https://www.cdash.org/) software quality dashboard application. Please see the [Kitware documentation](https://cmake.org/cmake/help/latest/manual/ctest.1.html#dashboard-client) for more information on this mode of operation.

### Installation

Mio's build system provides an installation target and support for downstream consumption via CMake's [`find_package`](https://cmake.org/cmake/help/v3.0/command/find_package.html) intrinsic function.
CMake allows installation to an arbitrary location, which may be specified by defining `CMAKE_INSTALL_PREFIX` at configure time.
In the absense of a user specification, CMake will install mio to conventional location based on the platform operating system.

To use a static configuration build tool, such as GNU Make or Ninja:

```sh
cd <mio source directory>
mkdir build
cd build

# Configure the build
cmake [-D CMAKE_INSTALL_PREFIX="path/to/installation"] \
      [-D BUILD_TESTING=False]                         \
      -D CMAKE_BUILD_TYPE=Release                      \
      -G <"Unix Makefiles" | "Ninja"> ..

# install mio
<make install | ninja install | cmake --build . --target install>
```

To use a dynamic configuration build tool, such as Visual Studio or Xcode:

```sh
cd <mio source directory>
mkdir build
cd build

# Configure the project
cmake [-D CMAKE_INSTALL_PREFIX="path/to/installation"] \
      [-D BUILD_TESTING=False]                         \
      -G <"Visual Studio 14 2015 Win64" | "Xcode"> ..

# install mio
cmake --build . --config Release --target install
```

Note that the last command of the installation sequence may require administrator privileges (e.g. `sudo`) if the installation root directory lies outside your home directory.

This installation
+ copies the mio header files to the `include/mio` subdirectory of the installation root
+ generates and copies several CMake configuration files to the `share/cmake/mio` subdirectory of the installation root

This latter step allows downstream CMake projects to consume mio via `find_package`, e.g.

```cmake
find_package( mio REQUIRED )
target_link_libraries( MyTarget PUBLIC mio::mio )
```

**WINDOWS USERS**: The `mio::mio` target `#define`s `WIN32_LEAN_AND_MEAN` and `NOMINMAX`. The former ensures the imported surface area of the Win API is minimal, and the latter disables Windows' `min` and `max` macros so they don't intefere with `std::min` and `std::max`. Because *mio* is a header only library, these defintions will leak into downstream CMake builds. If their presence is causing problems with your build then you can use the alternative `mio::mio_full_winapi` target, which adds none of these defintions.

If mio was installed to a non-conventional location, it may be necessary for downstream projects to specify the mio installation root directory via either

+ the `CMAKE_PREFIX_PATH` configuration option,
+ the `CMAKE_PREFIX_PATH` environment variable, or
+ `mio_DIR` environment variable.

Please see the [Kitware documentation](https://cmake.org/cmake/help/v3.0/command/find_package.html) for more information.

In addition, mio supports packaged relocatable installations via [CPack](https://cmake.org/cmake/help/latest/manual/cpack.1.html).
Following configuration, from the build directory, invoke cpack as follows to generate a packaged installation:

```sh
cpack -G <generator name> -C Release
```

The list of supported generators varies from platform to platform. See the output of `cpack --help` for a complete list of supported generators on your platform.

### Subproject Composition
To use mio as a subproject, copy the mio repository to your project's dependencies/externals folder.
If your project is version controlled using git, a git submodule or git subtree can be used to syncronize with the updstream repository.
The [use](https://services.github.com/on-demand/downloads/submodule-vs-subtree-cheat-sheet/) and [relative advantages](https://andrey.nering.com.br/2016/git-submodules-vs-subtrees/) of these git facilities is beyond the scope of this document, but in brief, each may be established as follows:

```sh
# via git submodule
cd <my project's dependencies directory>
git submodule add -b master https://github.com/mandreyel/mio.git

# via git subtree
cd <my project's root directory>
git subtree add --prefix <path/to/dependencies>/mio       \
    https://github.com/mandreyel/mio.git master --squash
```

Given a mio subdirectory in a project, simply add the following lines to your project's to add mio include directories to your target's include path.

```cmake
add_subdirectory( path/to/mio/ )
target_link_libraries( MyTarget PUBLIC <mio::mio | mio> )
```

Note that, as a subproject, mio's tests and examples will not be built and CPack integration is deferred to the host project.

