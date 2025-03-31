#include <string>
#include <fstream>
#include <iostream>
#include <cassert>
#include <system_error>
#include <numeric>
#include <vector>

#include "../single_include/mio/mio.hpp"

// Just make sure this compiles.
# include <cstddef>
using mmap_source = mio::basic_mmap_source<std::byte>;

template<class MMap>
void test_at_offset(const MMap& file_view, const std::string& buffer,
	const size_t offset);
inline void test_at_offset(const std::string& buffer, const char* path,
	const size_t offset, std::error_code& error);
inline int handle_error(const std::error_code& error);

inline void allocate_file(const std::string& path, const int size)
{
	std::ofstream file(path);
	std::string s(size, '0');
	file << s;
}

inline int handle_error(const std::error_code& error)
{
	const auto& errmsg = error.message();
	std::printf("Error mapping file: %s, exiting...\n", errmsg.c_str());
	return error.value();
}

inline int test_rewrite_file()
{
	const auto path = "test_rewrite.txt";

	// NOTE: mio does *not* create the file for you if it doesn't exist! You
	// must ensure that the file exists before establishing a mapping. It
	// must also be non-empty. So for illustrative purposes the file is
	// created now.
	allocate_file(path, 204800);

	// Read-write memory map the whole file by using `map_entire_file` where the
	// length of the mapping is otherwise expected, with the factory method.
	std::error_code error;
	mio::mmap_sink rw_mmap = mio::make_mmap_sink(
		path, 0, mio::map_entire_file, error);
	if (error) { return handle_error(error); }

	// You can use any iterator based function.
	std::fill(rw_mmap.begin(), rw_mmap.end(), 'a');

	// Or manually iterate through the mapped region just as if it were any other 
	// container, and change each byte's value (since this is a read-write mapping).
	for (auto& b : rw_mmap)
	{
		b += 10;
	}

	// Or just change one value with the subscript operator.
	const int answer_index = rw_mmap.size() / 2;
	rw_mmap[answer_index] = 42;

	// Don't forget to flush changes to disk before unmapping. However, if
	// `rw_mmap` were to go out of scope at this point, the destructor would also
	// automatically invoke `sync` before `unmap`.
	rw_mmap.sync(error);
	if (error) { return handle_error(error); }

	// We can then remove the mapping, after which rw_mmap will be in a default
	// constructed state, i.e. this and the above call to `sync` have the same
	// effect as if the destructor had been invoked.
	rw_mmap.unmap();

	// Now create the same mapping, but in read-only mode. Note that calling the
	// overload without the offset and file length parameters maps the entire
	// file.
	mio::mmap_source ro_mmap;
	ro_mmap.map(path, error);
	if (error) { return handle_error(error); }

	const int the_answer_to_everything = ro_mmap[answer_index];
	assert(the_answer_to_everything == 42);
}

int main()
{
	std::error_code error;

	// Make sure mio compiles with non-const char* strings too.
	const char _path[] = "test-file";
	const int path_len = sizeof(_path);
	char* path = new char[path_len];
	std::copy(_path, _path + path_len, path);

	const auto page_size = mio::page_size();
	// Fill buffer, then write it to file.
	const int file_size = 4 * page_size - 250; // 16134, if page size is 4KiB
	std::string buffer(file_size, 0);
	// Start at first printable ASCII character.
	char v = 33;
	for (auto& b : buffer) {
		b = v;
		++v;
		// Limit to last printable ASCII character.
		v %= 126;
		if (v == 0) {
			v = 33;
		}
	}

	std::ofstream file(path);
	file << buffer;
	file.close();

	// Test whole file mapping.
	test_at_offset(buffer, path, 0, error);
	if (error) { return handle_error(error); }

	//Test starting from below the page size.
	test_at_offset(buffer, path, page_size - 3, error);
	if (error) { return handle_error(error); }

	// Test starting from above the page size.
	test_at_offset(buffer, path, page_size + 3, error);
	if (error) { return handle_error(error); }

	// Test starting from above the page size.
	test_at_offset(buffer, path, 2 * page_size + 3, error);
	if (error) { return handle_error(error); }

	{
#define CHECK_INVALID_MMAP(m) do { \
        assert(error); \
        assert(m.empty()); \
        assert(!m.is_open()); \
        error.clear(); } while(0)

		mio::mmap_source m;

		// See if mapping an invalid file results in an error.
		m = mio::make_mmap_source("garbage-that-hopefully-doesnt-exist", 0, 0, error);
		CHECK_INVALID_MMAP(m);

		// Empty path?
		m = mio::make_mmap_source(static_cast<const char*>(0), 0, 0, error);
		CHECK_INVALID_MMAP(m);
		m = mio::make_mmap_source(std::string(), 0, 0, error);
		CHECK_INVALID_MMAP(m);

		// Invalid handle?
		m = mio::make_mmap_source(mio::invalid_handle, 0, 0, error);
		CHECK_INVALID_MMAP(m);

		// Invalid offset?
		m = mio::make_mmap_source(path, 100 * buffer.size(), buffer.size(), error);
		CHECK_INVALID_MMAP(m);
	}

	// Make sure these compile.
	{
		mio::ummap_source _1;
		mio::shared_ummap_source _2;
		// Make sure shared_mmap mapping compiles as all testing was done on
		// normal mmaps.
		mio::shared_mmap_source _3(path, 0, mio::map_entire_file);
		auto _4 = mio::make_mmap_source(path, error);
		auto _5 = mio::make_mmap<mio::shared_mmap_source>(path, 0, mio::map_entire_file, error);
#ifdef _WIN32
		const wchar_t* wpath1 = L"dasfsf";
		auto _6 = mio::make_mmap_source(wpath1, error);
		mio::mmap_source _7;
		_7.map(wpath1, error);
		const std::wstring wpath2 = wpath1;
		auto _8 = mio::make_mmap_source(wpath2, error);
		mio::mmap_source _9;
		_9.map(wpath1, error);
#else
		const int fd = open(path, O_RDONLY);
		mio::mmap_source _fdmmap(fd, 0, mio::map_entire_file);
		_fdmmap.unmap();
		_fdmmap.map(fd, error);
#endif
	}

	if (test_rewrite_file())
		throw std::runtime_error("test_rewrite_file failed");

	std::printf("all tests passed!\n");
}

void test_at_offset(const std::string& buffer, const char* path,
	const size_t offset, std::error_code& error)
{
	// Sanity check.
	assert(offset < buffer.size());

	// Map the region of the file to which buffer was written.
	mio::mmap_source file_view = mio::make_mmap_source(
		path, offset, mio::map_entire_file, error);
	if (error) { return; }

	assert(file_view.is_open());
	const size_t mapped_size = buffer.size() - offset;
	assert(file_view.size() == mapped_size);

	test_at_offset(file_view, buffer, offset);

	// Turn file_view into a shared mmap.
	mio::shared_mmap_source shared_file_view(std::move(file_view));
	assert(!file_view.is_open());
	assert(shared_file_view.is_open());
	assert(shared_file_view.size() == mapped_size);

	//test_at_offset(shared_file_view, buffer, offset);
}

template<class MMap>
void test_at_offset(const MMap& file_view, const std::string& buffer,
	const size_t offset)
{
	// Then verify that mmap's bytes correspond to that of buffer.
	for (size_t buf_idx = offset, view_idx = 0;
		buf_idx < buffer.size() && view_idx < file_view.size();
		++buf_idx, ++view_idx) {
		if (file_view[view_idx] != buffer[buf_idx]) {
			std::printf("%luth byte mismatch: expected(%d) <> actual(%d)",
				buf_idx, buffer[buf_idx], file_view[view_idx]);
			std::cout << std::flush;
			assert(0);
		}
	}
}
