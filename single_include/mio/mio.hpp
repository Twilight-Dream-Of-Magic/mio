/* This is the mio library, using C++20 standard with clean code practices.
 * Replaced system error codes with exceptions and `std::source_location` for error messages.
 * Assertions are used for validation.
 * Copyright 2017-2030 https://github.com/mandreyel
 * Modified by https://github.com/Twilight-Dream-Of-Magic
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this
 * software and associated documentation files (the "Software"), to deal in the Software
 * without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies
 * or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef MIO_MMAP_HEADER
#define MIO_MMAP_HEADER

#ifndef MIO_PAGE_HEADER
#define MIO_PAGE_HEADER

#ifdef _WIN32
# include <windows.h>
#else
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#endif

#include <iterator>
#include <string>
#include <cstdint>
#include <cstdlib>
#include <locale>

#include <system_error> // std::error_code
#include <memory> // std::shared_ptr

#ifdef _WIN32
# ifndef WIN32_LEAN_AND_MEAN
#  define WIN32_LEAN_AND_MEAN
# endif // WIN32_LEAN_AND_MEAN
# include <windows.h>
#else // ifdef _WIN32
# define INVALID_HANDLE_VALUE -1
#endif // ifdef _WIN32

#include <type_traits>

#include <algorithm>

#include <filesystem>

#if __cplusplus >= 202002L
#include <source_location>
#endif

#if __cplusplus >= 202300L
#include <stacktrace>
#endif


namespace mio
{

#ifdef min
#undef min
#endif //! min

#ifdef max
#undef max
#endif //! max

#if __cplusplus >= 202002L

	inline void my_cpp2020_assert(const bool JudgmentCondition, const char* ErrorMessage, std::source_location AssertExceptionDetailTrackingObject)
	{
		if (!JudgmentCondition)
		{
			std::system("chcp 65001");

			std::cout << "The error message is(错误信息是):\n" << ErrorMessage << std::endl;
			std::cout << "Oh, crap, some of the code already doesn't match the conditions at runtime.(哦，糟糕，有些代码在运行时已经不匹配条件。)\n\n\n" << std::endl;
			std::cout << "Here is the trace before the assertion occurred(下面是发生断言之前的追踪信息):\n\n" << std::endl;
			std::cout << "The condition determines the code file that appears to be a mismatch(条件判断出现不匹配的代码文件):\n" << AssertExceptionDetailTrackingObject.file_name() << std::endl;
			std::cout << "Name of the function where this assertion is located(该断言所在的函数的名字):\n" << AssertExceptionDetailTrackingObject.function_name() << std::endl;
			std::cout << "Number of lines of code where the assertion is located(该断言所在的代码行数):\n" << AssertExceptionDetailTrackingObject.line() << std::endl;
			std::cout << "Number of columns of code where the assertion is located(该断言所在的代码列数):\n" << AssertExceptionDetailTrackingObject.column() << std::endl;

			// Print stack trace for C++23 and above
#if __cplusplus >= 202300L
			std::cout << "Stack trace before assertion:\n";

			for (const auto& frame : std::stacktrace::current())
			{
				std::cout << frame << std::endl;
			}
#endif

			throw std::runtime_error(ErrorMessage);
		}
		else
		{
			return;
		}
	}

#endif

	/**
	 * This is used by `basic_mmap` to determine whether to create a read-only or
	 * a read-write memory mapping.
	 */
	enum class access_mode
	{
		read,
		write,
		private_page
	};

	/**
	 * Determines the operating system's page allocation granularity.
	 *
	 * On the first call to this function, it invokes the operating system specific syscall
	 * to determine the page size, caches the value, and returns it. Any subsequent call to
	 * this function serves the cached value, so no further syscalls are made.
	 */
	inline size_t page_size()
	{
		static const size_t page_size = []
			{
#ifdef _WIN32
				SYSTEM_INFO SystemInfo;
				GetSystemInfo(&SystemInfo);
				return SystemInfo.dwAllocationGranularity;
#else
				return sysconf(_SC_PAGE_SIZE);
#endif
			}();
		return page_size;
	}

	/**
	 * Alligns `offset` to the operating's system page size such that it subtracts the
	 * difference until the nearest page boundary before `offset`, or does nothing if
	 * `offset` is already page aligned.
	 */
	inline size_t make_offset_page_aligned(size_t offset) noexcept
	{
		const size_t page_size_ = page_size();
		// Use integer division to round down to the nearest page alignment.
		return offset / page_size_ * page_size_;
	}

} // namespace mio

#endif // MIO_PAGE_HEADER

#ifndef MIO_STRING_UTIL_HEADER
#define MIO_STRING_UTIL_HEADER

namespace mio {
	namespace detail {
#if __cplusplus >= 201103L && __cplusplus <= 201703L
#include <codecvt>
		inline std::wstring cpp2017_string2wstring(const std::string& _string)
		{
			using convert_typeX = std::codecvt_utf8<wchar_t>;
			std::wstring_convert<convert_typeX, wchar_t> converterX;

			return converterX.from_bytes(_string);
		}

		inline std::string cpp2017_wstring2string(const std::wstring& _wstring)
		{
			using convert_typeX = std::codecvt_utf8<wchar_t>;
			std::wstring_convert<convert_typeX, wchar_t> converterX;

			return converterX.to_bytes(_wstring);
		}
#endif

		inline std::wstring string2wstring(const std::string& _string)
		{
			::setlocale(LC_ALL, "");
			std::vector<wchar_t> wide_character_buffer;
			std::size_t source_string_count = 1;
			std::size_t found_not_ascii_count = 0;
			for (auto begin = _string.begin(), end = _string.end(); begin != end; begin++)
			{
				if (static_cast<const long long>(*begin) > 0)
				{
					++source_string_count;
				}
				else if (static_cast<const long long>(*begin) < 0)
				{
					++found_not_ascii_count;
				}
			}

			std::size_t target_wstring_count = source_string_count + (found_not_ascii_count / 2);

			wide_character_buffer.resize(target_wstring_count);

#if defined(_MSC_VER)
			std::size_t _converted_count = 0;
			::mbstowcs_s(&_converted_count, &wide_character_buffer[0], target_wstring_count, _string.c_str(), ((size_t)-1));
#else
			::mbstowcs(&wide_character_buffer[0], _string.c_str(), target_wstring_count);
#endif

			std::size_t _target_wstring_size = 0;
			for (auto begin = wide_character_buffer.begin(), end = wide_character_buffer.end(); begin != end && *begin != L'\0'; begin++)
			{
				++_target_wstring_size;
			}
			std::wstring _wstring{ wide_character_buffer.data(),  _target_wstring_size };

#if defined(_MSC_VER)
			my_cpp2020_assert(_converted_count != 0, "The function string2wstring is not work !", std::source_location::current());
#endif

			if (found_not_ascii_count > 0)
			{
				//Need Contains character('\0') then check size
				if (((_target_wstring_size + 1) - source_string_count) != (found_not_ascii_count / 2))
				{
					my_cpp2020_assert(false, "The function string2wstring, An error occurs during conversion !", std::source_location::current());
				}
				else
				{
					return _wstring;
				}
			}
			else
			{
				//Need Contains character('\0') then check size
				if ((_target_wstring_size + 1) != source_string_count)
				{
					my_cpp2020_assert(false, "The function string2wstring, An error occurs during conversion !", std::source_location::current());
				}
				else
				{
					return _wstring;
				}
			}

		}

		inline std::string wstring2string(const std::wstring& _wstring)
		{
			::setlocale(LC_ALL, "");
			std::vector<char> character_buffer;
			std::size_t source_wstring_count = 1;
			std::size_t found_not_ascii_count = 0;
			for (auto begin = _wstring.begin(), end = _wstring.end(); begin != end; begin++)
			{
				if (static_cast<const long long>(*begin) < 256)
				{
					++source_wstring_count;
				}
				else if (static_cast<const long long>(*begin) >= 256)
				{
					++found_not_ascii_count;
				}
			}
			std::size_t target_string_count = source_wstring_count + found_not_ascii_count * 2;

			character_buffer.resize(target_string_count);

#if defined(_MSC_VER)
			std::size_t _converted_count = 0;
			::wcstombs_s(&_converted_count, &character_buffer[0], target_string_count, _wstring.c_str(), ((size_t)-1));
#else
			::wcstombs(&character_buffer[0], _wstring.c_str(), target_string_count);
#endif

			std::size_t _target_string_size = 0;
			for (auto begin = character_buffer.begin(), end = character_buffer.end(); begin != end && *begin != '\0'; begin++)
			{
				++_target_string_size;
			}
			std::string _string{ character_buffer.data(),  _target_string_size };

#if defined(_MSC_VER)
			if (_converted_count == 0)
			{
				my_cpp2020_assert(_converted_count != 0, "The function wstring2string is not work !", std::source_location::current());
			}
#endif

			if (found_not_ascii_count > 0)
			{
				if (((_target_string_size + 1) - source_wstring_count) != (found_not_ascii_count * 2))
				{
					my_cpp2020_assert(false, "The function wstring2string, An error occurs during conversion !", std::source_location::current());
				}
				else
				{
					return _string;
				}
			}
			else
			{
				if ((_target_string_size + 1) != source_wstring_count)
				{
					my_cpp2020_assert(false, "The function wstring2string, An error occurs during conversion !", std::source_location::current());
				}
				else
				{
					return _string;
				}
			}
		}
	}
}

namespace mio {
	namespace detail {

#include <concepts>

		template<typename T>
		struct normalized
		{
			using type = std::remove_cvref_t<
				std::conditional_t<std::is_pointer_v<T>,
				std::remove_pointer_t<T>,
				T>
			>;
		};

		template<typename T, std::size_t N>
		struct normalized<T[N]>
		{
			using type = std::remove_cvref_t<T>;
		};

		template<typename T>
		using normalized_t = typename normalized<T>::type;

		template<typename CharacterType, typename AnyType>
		struct type_helper
		{
			static constexpr bool is_character_type()
			{
				return std::same_as<CharacterType, normalized_t<AnyType>>;
			}
		};

		template<typename AnyType>
		constexpr bool is_char_type = type_helper<char, AnyType>::is_character_type();

#ifdef _WIN32
		template<typename AnyType>
		constexpr bool is_wchar_type = type_helper<wchar_t, AnyType>::is_character_type();

		template<typename AnyType>
		constexpr bool is_char_or_wchar_type = is_char_type<AnyType> || is_wchar_type<AnyType>;
#else
		template<typename AnyType>
		constexpr bool is_char_or_wchar_type = is_char_type<AnyType>;
#endif

		template<typename T>
		concept narrow_string_like =
			std::is_class_v<std::remove_cvref_t<T>> &&                              // 必须是类类型
			requires(T t) {
				{ t.data() } -> std::convertible_to<const char*>;                   // data() 返回 const char*
				{ t.c_str() } -> std::same_as<const char*>;                         // c_str() 必须是 const char*
				{ t.empty() } -> std::convertible_to<bool>;                         // empty() 返回 bool
		}&& std::is_same_v<typename std::remove_cvref_t<T>::value_type, char>;   // 内部字符类型必须为 char

#ifdef _WIN32
		template<typename T>
		concept wide_string_like =
			std::is_class_v<std::remove_cvref_t<T>> &&                              // 必须是类类型
			requires(T t) {
				{ t.data() } -> std::convertible_to<const wchar_t*>;                // data() 返回 const wchar_t*
				{ t.c_str() } -> std::same_as<const wchar_t*>;                      // c_str() 必须是 const wchar_t*
				{ t.empty() } -> std::convertible_to<bool>;                         // empty() 返回 bool
		}&& std::is_same_v<typename std::remove_cvref_t<T>::value_type, wchar_t>; // 内部字符类型必须为 wchar_t
#endif

		template<typename StringType>
		concept filesystem_path_like = std::same_as<normalized_t<std::decay_t<StringType>>, std::filesystem::path>;

#ifdef _WIN32
		// Windows 平台：允许窄字符、宽字符字符串以及文件系统路径
		template<typename StringType>
		concept stringable_path = narrow_string_like<StringType> ||
			wide_string_like<StringType> ||
			filesystem_path_like<StringType>;
#else
		// 非 Windows 平台：仅允许窄字符字符串和文件系统路径
		template<typename StringType>
		concept stringable_path = narrow_string_like<StringType> ||
			filesystem_path_like<StringType>;
#endif

#ifdef _WIN32
		template<wide_string_like StringType>
		const wchar_t* c_str(const StringType& s)
		{
			return s.data();
		}
#endif
		template<narrow_string_like StringType>
		const char* c_str(const StringType& s)
		{
			return s.data();
		}

		template<typename StringType>
#ifdef _WIN32
			requires (narrow_string_like<StringType> || wide_string_like<StringType>)
#else
			requires (narrow_string_like<StringType>)
#endif
		bool empty(const StringType& s)
		{
			if (stringable_path<StringType>)
			{
				return s.empty();
			}
			else
			{
				return s == nullptr;
			}
		}

		template<access_mode Mode>
		concept FileReadAccess = (Mode == access_mode::read);

		template<access_mode Mode>
		concept FileWriteAccess = (Mode == access_mode::write);

		template<access_mode Mode>
		concept FilePrivateAccess = (Mode == access_mode::private_page);

	} // namespace detail
} // namespace mio

#endif // MIO_STRING_UTIL_HEADER

// -- Base Class --

namespace mio {

	// This value may be provided as the `length` parameter to the constructor or
	// `map`, in which case a memory mapping of the entire file is created.
	enum { map_entire_file = 0 };

	// This value represents an invalid file handle type. This can be used to
	// determine whether `basic_mmap::file_handle` is valid, for example.
#ifdef _WIN32
	using file_handle_type = HANDLE;
	static const file_handle_type invalid_handle = INVALID_HANDLE_VALUE;
#else
	using file_handle_type = int;
	constexpr file_handle_type invalid_handle = -1;
#endif

	template<access_mode AccessMode, typename ByteT>
	struct basic_mmap
	{
		using value_type = ByteT;
		using size_type = size_t;
		using reference = value_type&;
		using const_reference = const value_type&;
		using pointer = value_type*;
		using const_pointer = const value_type*;
		using difference_type = std::ptrdiff_t;
		using iterator = pointer;
		using const_iterator = const_pointer;
		using reverse_iterator = std::reverse_iterator<iterator>;
		using const_reverse_iterator = std::reverse_iterator<const_iterator>;
		using iterator_category = std::random_access_iterator_tag;
		using handle_type = file_handle_type;

		static_assert(sizeof(ByteT) == sizeof(char), "ByteT must be the same size as char.");

	private:
		// Points to the first requested byte, and not to the actual start of the mapping.
		pointer data_ = nullptr;

		// Length--in bytes--requested by user (which may not be the length of the
		// full mapping) and the length of the full mapping.
		size_type length_ = 0;
		size_type mapped_length_ = 0;

		// Letting user map a file using both an existing file handle and a path
		// introcudes some complexity (see `is_handle_internal_`).
		// On POSIX, we only need a file handle to create a mapping, while on
		// Windows systems the file handle is necessary to retrieve a file mapping
		// handle, but any subsequent operations on the mapped region must be done
		// through the latter.
		handle_type file_handle_ = INVALID_HANDLE_VALUE;
#ifdef _WIN32
		handle_type file_mapping_handle_ = INVALID_HANDLE_VALUE;
#endif

		// Letting user map a file using both an existing file handle and a path
		// introcudes some complexity in that we must not close the file handle if
		// user provided it, but we must close it if we obtained it using the
		// provided path. For this reason, this flag is used to determine when to
		// close `file_handle_`.
		bool is_handle_internal_;

	public:
		/**
		 * The default constructed mmap object is in a non-mapped state, that is,
		 * any operation that attempts to access nonexistent underlying data will
		 * result in undefined behaviour/segmentation faults.
		 */
		basic_mmap() = default;

#ifdef __cpp_exceptions
		/**
		 * The same as invoking the `map` function, except any error that may occur
		 * while establishing the mapping is wrapped in a `std::system_error` and is
		 * thrown.
		 */
		template<typename String>
		basic_mmap(const String& path, const size_type offset = 0, const size_type length = map_entire_file)
		{
			map(path, offset, length);
		}

		/**
		 * The same as invoking the `map` function, except any error that may occur
		 * while establishing the mapping is wrapped in a `std::system_error` and is
		 * thrown.
		 */
		basic_mmap(const handle_type handle, const size_type offset = 0, const size_type length = map_entire_file)
		{
			map(handle, offset, length);
		}
#endif // __cpp_exceptions

		/**
		 * `basic_mmap` has single-ownership semantics, so transferring ownership
		 * may only be accomplished by moving the object.
		 */
		basic_mmap(const basic_mmap&) = delete;
		basic_mmap(basic_mmap&&);
		basic_mmap& operator=(const basic_mmap&) = delete;
		basic_mmap& operator=(basic_mmap&&);

		/**
		 * If this is a read-write mapping, the destructor invokes sync. Regardless
		 * of the access mode, unmap is invoked as a final step.
		 */
		~basic_mmap();

		/**
		 * On UNIX systems 'file_handle' and 'mapping_handle' are the same. On Windows,
		 * however, a mapped region of a file gets its own handle, which is returned by
		 * 'mapping_handle'.
		 */
		handle_type file_handle() const noexcept { return file_handle_; }
		handle_type mapping_handle() const noexcept;

		/** Returns whether a valid memory mapping has been created. */
		bool is_open() const noexcept { return file_handle_ != invalid_handle; }

		/**
		 * Returns true if no mapping was established, that is, conceptually the
		 * same as though the length that was mapped was 0. This function is
		 * provided so that this class has Container semantics.
		 */
		bool empty() const noexcept { return length() == 0; }

		/** Returns true if a mapping was established. */
		bool is_mapped() const noexcept;

		/**
		 * `size` and `length` both return the logical length, i.e. the number of bytes
		 * user requested to be mapped, while `mapped_length` returns the actual number of
		 * bytes that were mapped which is a multiple of the underlying operating system's
		 * page allocation granularity.
		 */
		size_type size() const noexcept { return length(); }
		size_type length() const noexcept { return length_; }
		size_type mapped_length() const noexcept { return mapped_length_; }

		/** Returns the offset relative to the start of the mapping. */
		size_type mapping_offset() const noexcept
		{
			return mapped_length_ - length_;
		}

		/**
		 * Returns a pointer to the first requested byte, or `nullptr` if no memory mapping
		 * exists.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		pointer data() noexcept { return data_; }
		const_pointer data() const noexcept { return data_; }

		/**
		 * Returns an iterator to the first requested byte, if a valid memory mapping
		 * exists, otherwise this function call is undefined behaviour.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		iterator begin() noexcept { return data(); }
		const_iterator begin() const noexcept { return data(); }
		const_iterator cbegin() const noexcept { return data(); }

		/**
		 * Returns an iterator one past the last requested byte, if a valid memory mapping
		 * exists, otherwise this function call is undefined behaviour.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		iterator end() noexcept { return data() + length(); }
		const_iterator end() const noexcept { return data() + length(); }
		const_iterator cend() const noexcept { return data() + length(); }

		/**
		 * Returns a reverse iterator to the last memory mapped byte, if a valid
		 * memory mapping exists, otherwise this function call is undefined
		 * behaviour.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		reverse_iterator rbegin() noexcept { return reverse_iterator(end()); }
		const_reverse_iterator rbegin() const noexcept
		{
			return const_reverse_iterator(end());
		}
		const_reverse_iterator crbegin() const noexcept
		{
			return const_reverse_iterator(end());
		}

		/**
		 * Returns a reverse iterator past the first mapped byte, if a valid memory
		 * mapping exists, otherwise this function call is undefined behaviour.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		reverse_iterator rend() noexcept { return reverse_iterator(begin()); }
		const_reverse_iterator rend() const noexcept
		{
			return const_reverse_iterator(begin());
		}
		const_reverse_iterator crend() const noexcept
		{
			return const_reverse_iterator(begin());
		}

		/**
		 * Establishes a memory mapping with AccessMode. If the mapping is
		 * unsuccesful, the reason is reported via `error` and the object remains in
		 * a state as if this function hadn't been called.
		 *
		 * `handle`, which must be a valid file handle, which is used to memory map the
		 * requested region. Upon failure, `error` is set to indicate the reason and the
		 * object remains in an unmapped state.
		 *
		 * `offset` is the number of bytes, relative to the start of the file, where the
		 * mapping should begin. When specifying it, there is no need to worry about
		 * providing a value that is aligned with the operating system's page allocation
		 * granularity. This is adjusted by the implementation such that the first requested
		 * byte (as returned by `data` or `begin`), so long as `offset` is valid, will be at
		 * `offset` from the start of the file.
		 *
		 * `length` is the number of bytes to map. It may be `map_entire_file`, in which
		 * case a mapping of the entire file is created.
		 */
		void map(const handle_type handle, const size_type offset, const size_type length);

		/**
		 * Establishes a memory mapping with AccessMode. If the mapping is
		 * unsuccesful, the reason is reported via `error` and the object remains in
		 * a state as if this function hadn't been called.
		 *
		 * `handle`, which must be a valid file handle, which is used to memory map the
		 * requested region. Upon failure, `error` is set to indicate the reason and the
		 * object remains in an unmapped state.
		 *
		 * The entire file is mapped.
		 */
		void map(const handle_type handle)
		{
			map(handle, 0, map_entire_file);
		}

		/**
		 * Returns a reference to the `i`th byte from the first requested byte (as returned
		 * by `data`). If this is invoked when no valid memory mapping has been created
		 * prior to this call, undefined behaviour ensues.
		 */
		reference operator[](const size_type i) noexcept { return data_[i]; }
		const_reference operator[](const size_type i) const noexcept { return data_[i]; }

		void map_sp(const char* path, const size_type offset, const size_type length);
#ifdef _WIN32
		void map_wsp(const wchar_t* path, const size_type offset, const size_type length);
#endif

		/**
		 * Establishes a memory mapping with AccessMode. If the mapping is unsuccesful, the
		 * reason is reported via `error` and the object remains in a state as if this
		 * function hadn't been called.
		 *
		 * `path`, which must be a path to an existing file, is used to retrieve a file
		 * handle (which is closed when the object destructs or `unmap` is called), which is
		 * then used to memory map the requested region. Upon failure, `error` is set to
		 * indicate the reason and the object remains in an unmapped state.
		 *
		 * `offset` is the number of bytes, relative to the start of the file, where the
		 * mapping should begin. When specifying it, there is no need to worry about
		 * providing a value that is aligned with the operating system's page allocation
		 * granularity. This is adjusted by the implementation such that the first requested
		 * byte (as returned by `data` or `begin`), so long as `offset` is valid, will be at
		 * `offset` from the start of the file.
		 *
		 * `length` is the number of bytes to map. It may be `map_entire_file`, in which
		 * case a mapping of the entire file is created.
		 */
		template<typename String>
		void map(const String& path, const size_type offset, const size_type length);

		/**
		 * Establishes a memory mapping with AccessMode. If the mapping is unsuccesful, the
		 * reason is reported via `error` and the object remains in a state as if this
		 * function hadn't been called.
		 *
		 * `path`, which must be a path to an existing file, is used to retrieve a file
		 * handle (which is closed when the object destructs or `unmap` is called), which is
		 * then used to memory map the requested region. Upon failure, `error` is set to
		 * indicate the reason and the object remains in an unmapped state.
		 *
		 * The entire file is mapped.
		 */
		template<typename StringPath>
		void map(const StringPath& path)
		{
			map(path, 0, map_entire_file);
		}

		/**
		 * If a valid memory mapping has been created prior to this call, this call
		 * instructs the kernel to unmap the memory region and disassociate this object
		 * from the file.
		 *
		 * The file handle associated with the file that is mapped is only closed if the
		 * mapping was created using a file path. If, on the other hand, an existing
		 * file handle was used to create the mapping, the file handle is not closed.
		 */
		void unmap();

		void swap(basic_mmap& other);

		/** Flushes the memory mapped page to disk. Errors are reported via `error`. */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		void sync();

		/**
		 * All operators compare the address of the first byte and size of the two mapped
		 * regions.
		 */

	private:
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		pointer get_mapping_start() noexcept
		{
			return !data() ? nullptr : data() - mapping_offset();
		}

		const_pointer get_mapping_start() const noexcept
		{
			return !data() ? nullptr : data() - mapping_offset();
		}

		/**
		 * The destructor syncs changes to disk if `AccessMode` is `write`, but not
		 * if it's `read`, but since the destructor cannot be templated, we need to
		 * do SFINAE in a dedicated function, where one syncs and the other is a noop.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		void conditional_sync();
		template<access_mode Mode = AccessMode>
		requires (detail::FileReadAccess<Mode>)
		void conditional_sync();
	};

	template<access_mode AccessMode, typename ByteT>
	bool operator==(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b);

	template<access_mode AccessMode, typename ByteT>
	bool operator!=(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b);

	template<access_mode AccessMode, typename ByteT>
	bool operator<(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b);

	template<access_mode AccessMode, typename ByteT>
	bool operator<=(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b);

	template<access_mode AccessMode, typename ByteT>
	bool operator>(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b);

	template<access_mode AccessMode, typename ByteT>
	bool operator>=(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b);

	/**
	 * This is the basis for all read-only mmap objects and should be preferred over
	 * directly using `basic_mmap`.
	 */
	template<typename ByteT>
	using basic_mmap_source = basic_mmap<access_mode::read, ByteT>;

	/**
	 * This is the basis for all read-write mmap objects and should be preferred over
	 * directly using `basic_mmap`.
	 */
	template<typename ByteT>
	using basic_mmap_sink = basic_mmap<access_mode::write, ByteT>;

	/**
	 * These aliases cover the most common use cases, both representing a raw byte stream
	 * (either with a char or an unsigned char/uint8_t).
	 */
	using mmap_source = basic_mmap_source<char>;
	using ummap_source = basic_mmap_source<unsigned char>;

	using mmap_sink = basic_mmap_sink<char>;
	using ummap_sink = basic_mmap_sink<unsigned char>;

	/**
	 * Convenience factory method that constructs a mapping for any `basic_mmap` or
	 * `basic_mmap` type.
	 */
	template<
		typename MMap,
		typename MappingToken
	> MMap make_mmap(const MappingToken& token,
		int64_t offset, int64_t length)
	{
		MMap mmap;

		mmap.map(token, offset, length);

		return mmap;
	}

	// Generic template for mmap_source for various MappingToken types
	template<typename MappingToken>
	mmap_source make_mmap_source(const MappingToken& token, mmap_source::size_type offset, mmap_source::size_type length)
	{
		return make_mmap<mmap_source>(token, offset, length);
	}

	// Overload specialized for std::filesystem::path
	inline mmap_source make_mmap_source(const std::filesystem::path& path, mmap_source::size_type offset, mmap_source::size_type length)
	{
		// Convert filesystem path to std::string and call the generic make_mmap function.
		return make_mmap<mmap_source>(path, offset, length);
	}

	// Overload for mmap_source that omits offset and length parameters (defaults to 0 and map_entire_file).
	template<typename MappingToken>
	mmap_source make_mmap_source(const MappingToken& token)
	{
		return make_mmap_source(token, 0, map_entire_file);
	}

	// Generic template for mmap_sink for various MappingToken types
	template<typename MappingToken>
	mmap_sink make_mmap_sink(const MappingToken& token, mmap_sink::size_type offset, mmap_sink::size_type length)
	{
		return make_mmap<mmap_sink>(token, offset, length);
	}

	// Overload specialized for std::filesystem::path for mmap_sink
	inline mmap_sink make_mmap_sink(const std::filesystem::path& path, mmap_sink::size_type offset, mmap_sink::size_type length)
	{
		return make_mmap<mmap_sink>(path.string(), offset, length);
	}

	// Overload for mmap_sink that omits offset and length parameters (defaults to 0 and map_entire_file).
	template<typename MappingToken>
	mmap_sink make_mmap_sink(const MappingToken& token)
	{
		return make_mmap_sink(token, 0, map_entire_file);
	}

} // namespace mio

#ifndef MIO_BASIC_MMAP_IMPL
#define MIO_BASIC_MMAP_IMPL

namespace mio
{
	// -- OS API ---

	namespace detail
	{

		inline std::error_code last_error() noexcept
		{
			std::error_code error;
#ifdef _WIN32
			error.assign(GetLastError(), std::system_category());
#else
			error.assign(errno, std::system_category());
#endif
			return error;
		}

		// Struct providing cross-platform file opening utilities
		struct mio_open_file_helper
		{

#ifdef _WIN32
			/**
			 * Windows-specific helper function to open a file given a wide-character (wstring) path.
			 * @param path The file path as a wide-character string.
			 * @param mode The access mode (read or write).
			 * @return A file handle if successful, invalid_handle if an error occurs.
			 */
			static file_handle_type open_file_helper_wstring(const std::wstring& path, const access_mode mode)
			{
				//File read or read/write mode.
				DWORD desired_access = (mode == access_mode::read) ? GENERIC_READ : (GENERIC_READ | GENERIC_WRITE);
				//Private files do not have any attributes.
				DWORD flags_and_attributes = (mode == access_mode::private_page) ? FILE_ATTRIBUTE_NORMAL : (mode == access_mode::read ? FILE_ATTRIBUTE_READONLY : FILE_ATTRIBUTE_TEMPORARY);
				//Is the file inaccessible to other processes?
				DWORD file_share_mode = (mode == access_mode::private_page) ? 0 : (FILE_SHARE_READ | FILE_SHARE_WRITE);
				return ::CreateFileW
				(
					path.c_str(),
					desired_access,
					file_share_mode, //ShareMode
					nullptr, //SecurityAttributes
					OPEN_EXISTING, //CreationDisposition
					flags_and_attributes, //FlagsAndAttributes
					nullptr
				);
			}

			/**
			 * Windows-specific helper function to open a file given a narrow-character (string) path.
			 * @param path The file path as a narrow-character string.
			 * @param mode The access mode (read or write).
			 * @return A file handle if successful, invalid_handle if an error occurs.
			 */
			static file_handle_type open_file_helper(const std::string& path, const access_mode mode)
			{
				std::wstring ws_path{ mio::detail::string2wstring(path) };
				return open_file_helper_wstring(ws_path, mode);
			}

			/**
			 * Windows-specific helper function to open a file given a std::filesystem::path.
			 * @param path The file path as a std::filesystem::path object.
			 * @param mode The access mode (read or write).
			 * @return A file handle if successful, invalid_handle if an error occurs.
			 */
			static file_handle_type open_file_helper_filesystem_path(const std::filesystem::path& path, const access_mode mode)
			{
				return open_file_helper_wstring(path.wstring(), mode);
			}
#endif // _WIN32

			/**
			 * Internal helper function to check if a given path is empty.
			 * The function is specialized for stringable paths.
			 * @param path The path to check.
			 * @return True if the path is empty, false otherwise.
			 */
			template<typename Path>
			static bool is_empty(const Path& path)
			{
				if constexpr (detail::stringable_path<Path>)
					return path.empty();
				else
					return path == nullptr;
			}

			/**
			 * Internal platform-specific implementation to open a file based on the path type.
			 * This function determines the appropriate platform-specific API to call.
			 * @param path The file path to open.
			 * @param mode The access mode (read or write).
			 * @return A file handle if successful, invalid_handle if an error occurs.
			 */
			template<typename Path>
			static file_handle_type open_file_implement(const Path& path, const access_mode mode)
			{
				file_handle_type handle = invalid_handle;

				//For windows, we need to use CreateFileW to open a file with a wide-character path.
#ifdef _WIN32
				if constexpr (std::same_as<Path, std::string>)
					handle = open_file_helper(path, mode);
				else if constexpr (std::same_as<Path, std::wstring>)
					handle = open_file_helper_wstring(path, mode);
				else if constexpr (std::same_as<Path, std::filesystem::path>)
					handle = open_file_helper_filesystem_path(path, mode);
				else
					// Attempt to convert other types to std::string
					handle = open_file_helper(std::string(path), mode);
#else
		//For macos and linux with POSIX stdandard, we can use the open() function to open a file with a narrow-character path.

				int flags = mode == access_mode::read ? O_RDONLY : O_RDWR;
#ifdef _LARGEFILE64_SOURCE
				flags |= O_LARGEFILE;
#endif
				if constexpr (std::same_as<Path, std::filesystem::path>)
					handle = ::open(path.c_str(), flags, S_IRWXU);
				else
					handle = ::open(c_str(path), flags, S_IRWXU);
#endif
				return handle;
			}

			/**
			 * Internal struct to support file literal types (narrow or wide strings) as file paths.
			 * This struct helps handle string literals with different character encodings.
			 */
			struct file_literal
			{
				enum class literal_type
				{
					narrow, wide
				} type;

				union
				{
					const char* narrow;
					const wchar_t* wide;
				};

				file_literal(const char* s) : type(literal_type::narrow), narrow(s) {}
				file_literal(const wchar_t* s) : type(literal_type::wide), wide(s) {}

				/**
				 * Converts the file literal to a std::string (narrow string) representation.
				 * @return The converted narrow string.
				 */
				std::string to_string() const
				{
					if (type == literal_type::narrow)
						return std::string(narrow);
					else
						return mio::detail::wstring2string(std::wstring(wide));
				}

				/**
				 * Converts the file literal to a std::wstring (wide string) representation.
				 * @return The converted wide string.
				 */
				std::wstring to_wstring() const
				{
					if (type == literal_type::wide)
						return std::wstring(wide);
					else
						return mio::detail::string2wstring(std::string(narrow));
				}
			};

			/**
			 * Public interface to open a file with various path types.
			 * It returns a file handle and also sets an error code if any issues arise.
			 * @param path The file path to open (can be string, wstring, or filesystem::path).
			 * @param mode The access mode (read or write).
			 * @return A file handle if successful, invalid_handle if an error occurs.
			 */
			template<typename Path>
			static file_handle_type open_file(const Path& path, const access_mode mode)
			{
				if (is_empty(path))
				{
					std::error_code error = std::make_error_code(std::errc::invalid_argument);
					std::string error_message = "String literal path is empty! " + error.message();
					my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
				}

				file_handle_type handle = open_file_implement(path, mode);

				return handle;
			}

			/**
			 * Overloaded function to open a file using file literal types (either narrow or wide strings).
			 * This helps open files using string literals directly.
			 * @param literal The file literal (either narrow or wide string).
			 * @param mode The access mode (read or write).
			 * @return A file handle if successful, invalid_handle if an error occurs.
			 */
			static file_handle_type open_file(const file_literal& literal, const access_mode mode)
			{
				bool empty_literal = false;
				if (literal.type == file_literal::literal_type::narrow)
					empty_literal = (literal.narrow == nullptr || literal.narrow[0] == '\0');
				else
					empty_literal = (literal.wide == nullptr || literal.wide[0] == L'\0');

				if (empty_literal)
				{
					std::error_code error = std::make_error_code(std::errc::invalid_argument);
					std::string error_message = "String literal path is empty! " + error.message();
					my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
				}

#ifdef _WIN32
				file_handle_type handle = (literal.type == file_literal::literal_type::narrow)
					? open_file_helper(literal.to_string(), mode)
					: open_file_helper_wstring(literal.to_wstring(), mode);
#else
				// On POSIX platforms, treat all literals as narrow strings
				std::string pathStr = literal.to_string();
				file_handle_type handle = ::open(pathStr.c_str(), mode == access_mode::read ? O_RDONLY : O_RDWR);
#endif
				return handle;
			}

			/**
			 * Overloaded function to support opening files using a const char* path literal.
			 * @param path The file path as a const char* string.
			 * @param mode The access mode (read or write).
			 * @param error The error code to capture any errors.
			 * @return A file handle if successful, invalid_handle if an error occurs.
			 */
			static file_handle_type open_file(const char* path, const access_mode mode)
			{
				return open_file(file_literal(path), mode);
			}

			/**
			 * Overloaded function to support opening files using a const wchar_t* path literal.
			 * @param path The file path as a const wchar_t* string.
			 * @param mode The access mode (read or write).
			 * @param error The error code to capture any errors.
			 * @return A file handle if successful, invalid_handle if an error occurs.
			 */
			static file_handle_type open_file(const wchar_t* path, const access_mode mode)
			{
				return open_file(file_literal(path), mode);
			}
		};

		static inline mio_open_file_helper open_file_helper;

		/**
		 * Queries the size of the file associated with the given file handle.
		 * This function retrieves the file size in a platform-dependent manner (Windows or POSIX).
		 * @param handle The file handle to query.
		 * @param error The error code to capture any errors that occur during the operation.
		 * @return The size of the file in bytes, or 0 if an error occurs.
		 */
		inline size_t query_file_size(file_handle_type handle)
		{
#ifdef _WIN32
			LARGE_INTEGER file_size;
			if (::GetFileSizeEx(handle, &file_size) == 0)
			{
				return 0;
			}
			return static_cast<int64_t>(file_size.QuadPart);
#else // POSIX
			struct stat sbuf;
			if (::fstat(handle, &sbuf) == -1)
			{
				return 0;
			}
			return sbuf.st_size;
#endif
		}

		/**
		 * Struct representing the context for memory-mapped file access.
		 * It holds the mapped data and related information such as file length and the mapping handle.
		 */
		struct mmap_context
		{
			char* data;                    ///< Pointer to the start of the mapped memory
			int64_t length;                 ///< The length of the memory-mapped region
			int64_t mapped_length;          ///< The actual length of the memory-mapped region
#ifdef _WIN32
			file_handle_type file_mapping_handle; ///< Handle to the file mapping object on Windows
#endif
		};

		/**
		 * Maps a file into memory at a specified offset and length.
		 * This function creates a memory-mapped region of the file to allow efficient access to its contents.
		 * @param file_handle The handle to the file to be mapped.
		 * @param offset The offset within the file where the mapping should start.
		 * @param length The length of the region to map, starting from the offset.
		 * @param mode The access mode (read or write) for the memory-mapped region.
		 * @param error The error code to capture any errors that occur during the mapping operation.
		 * @return An `mmap_context` struct containing information about the memory-mapped region, or an empty context if an error occurs.
		 */
		inline mmap_context memory_map(const file_handle_type file_handle, const int64_t offset,
			const int64_t length, const access_mode mode)
		{
			// Align the offset to the page boundary
			const int64_t aligned_offset = make_offset_page_aligned(offset);
			// Calculate the length of the region to map
			const int64_t length_to_map = offset - aligned_offset + length;

#ifdef _WIN32
			DWORD protect = (mode == access_mode::private_page) ? PAGE_WRITECOPY : (mode == access_mode::read ? PAGE_READONLY : PAGE_READWRITE);
			const int64_t max_file_size = offset + length;
			// Create a file mapping object on Windows
			const auto file_mapping_handle = ::CreateFileMapping
			(
				file_handle,
				0, //FileMappingAttributes
				protect, //Protect
				0, //MaximumSizeHigh
				0, //MaximumSizeLow
				0 //Name
			);

			if (file_mapping_handle == invalid_handle)
			{
				std::string error_message = "CreateFileMapping failed: " + detail::last_error().message();
				my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
				return {};
			}

			DWORD desired_access = (mode == access_mode::private_page) ? FILE_MAP_COPY : (mode == access_mode::read ? FILE_MAP_READ : FILE_MAP_WRITE);
			// Map the file view into memory
			char* mapping_start = static_cast<char*>
			(
				::MapViewOfFileEx
				(
					file_mapping_handle, //FileMappingObject
					desired_access, //DesiredAccess
					static_cast<DWORD>(aligned_offset >> 32), //FileOffsetHigh
					static_cast<DWORD>(aligned_offset & 0xffffffff), //FileOffsetLow
					static_cast<size_t>(length_to_map) != static_cast<size_t>(-1) ? length_to_map : 0, //NumberOfBytesToMap
					reinterpret_cast<LPVOID>(0) //BaseAddress
				)
			);

			//char* mapping_start = static_cast<char*>
			//(
			//	::MapViewOfFile
			//	(
			//	file_mapping_handle, //FileMappingObject
			//	desired_access, //DesiredAccess
			//	static_cast<DWORD>(aligned_offset >> 32), //FileOffsetHigh
			//	static_cast<DWORD>(aligned_offset & 0xffffffff), //FileOffsetLow
			//	static_cast<size_t>(length_to_map) != static_cast<size_t>(-1) ? length_to_map : 0 //NumberOfBytesToMap
			//	)
			//);

			if (mapping_start == nullptr)
			{
				// Close file handle if mapping failed
				::CloseHandle(file_mapping_handle);

				std::string error_message = "MapViewOfFile failed: " + detail::last_error().message();
				my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());

				return {};
			}
#else // POSIX
			// Create a memory mapping on POSIX
			char* mapping_start = static_cast<char*>
			(
				::mmap
				(
					const_cast<char*>(0), // No hint on where to map
					length_to_map,
					mode == access_mode::read ? PROT_READ : (PROT_READ | PROT_WRITE),
					mode == access_mode::private_page ? MAP_PRIVATE : MAP_SHARED,
					file_handle,
					aligned_offset
				)
			);
			if (mapping_start == MAP_FAILED)
			{
				error = detail::last_error();
				return {};
			}
#endif

			// Return a context containing the memory-mapped region details
			mmap_context ctx;
			ctx.data = mapping_start + (offset - aligned_offset); // Adjust the pointer to the original offset
			ctx.length = length;
			ctx.mapped_length = length_to_map;
#ifdef _WIN32
			ctx.file_mapping_handle = file_mapping_handle;  // Store file mapping handle on Windows
#endif
			return ctx;
		}

		inline void sync_memory_map(auto* data_pointer, auto* mapping_pointer, size_t mapped_length, mio::file_handle_type file_handle)
		{
			if (data_pointer != nullptr)
			{

#ifdef _WIN32
				if (::FlushViewOfFile(mapping_pointer, mapped_length) == 0 || ::FlushFileBuffers(file_handle) == 0)
#else // POSIX
				if (::msync(mapping_pointer, mapped_length, MS_SYNC) != 0)
#endif
				{
					std::string error_message = "FlushViewOfFile or mync failed: " + detail::last_error().message();
					my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
				}
			}

#ifdef _WIN32
			if (::FlushFileBuffers(file_handle) == 0)
			{
				std::string error_message = "FlushFileBuffers failed: " + detail::last_error().message();
				my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
			}
#endif
		}
	}
}

namespace mio {

	// -- basic_mmap --

	template<access_mode AccessMode, typename ByteT>
	basic_mmap<AccessMode, ByteT>::~basic_mmap()
	{
		conditional_sync();
		unmap();
	}

	template<access_mode AccessMode, typename ByteT>
	basic_mmap<AccessMode, ByteT>::basic_mmap(basic_mmap&& other)
		: data_(std::move(other.data_))
		, length_(std::move(other.length_))
		, mapped_length_(std::move(other.mapped_length_))
		, file_handle_(std::move(other.file_handle_))
#ifdef _WIN32
		, file_mapping_handle_(std::move(other.file_mapping_handle_))
#endif
		, is_handle_internal_(std::move(other.is_handle_internal_))
	{
		other.data_ = nullptr;
		other.length_ = other.mapped_length_ = 0;
		other.file_handle_ = invalid_handle;
#ifdef _WIN32
		other.file_mapping_handle_ = invalid_handle;
#endif
	}

	template<access_mode AccessMode, typename ByteT>
	basic_mmap<AccessMode, ByteT>&
		basic_mmap<AccessMode, ByteT>::operator=(basic_mmap&& other)
	{
		if (this != &other)
		{
			// First the existing mapping needs to be removed.
			unmap();
			data_ = std::move(other.data_);
			length_ = std::move(other.length_);
			mapped_length_ = std::move(other.mapped_length_);
			file_handle_ = std::move(other.file_handle_);
#ifdef _WIN32
			file_mapping_handle_ = std::move(other.file_mapping_handle_);
#endif
			is_handle_internal_ = std::move(other.is_handle_internal_);

			// The moved from basic_mmap's fields need to be reset, because
			// otherwise other's destructor will unmap the same mapping that was
			// just moved into this.
			other.data_ = nullptr;
			other.length_ = other.mapped_length_ = 0;
			other.file_handle_ = invalid_handle;
#ifdef _WIN32
			other.file_mapping_handle_ = invalid_handle;
#endif
			other.is_handle_internal_ = false;
		}
		return *this;
	}

	template<access_mode AccessMode, typename ByteT>
	typename basic_mmap<AccessMode, ByteT>::handle_type
		basic_mmap<AccessMode, ByteT>::mapping_handle() const noexcept
	{
#ifdef _WIN32
		return file_mapping_handle_;
#else
		return file_handle_;
#endif
	}

	template<access_mode AccessMode, typename ByteT>
	void basic_mmap<AccessMode, ByteT>::map_sp(const char* path, const size_type offset,
		const size_type length)
	{
		if (path == nullptr || path[0] == '\0')
		{
			std::error_code error = std::make_error_code(std::errc::invalid_argument);
			std::string error_message = "Path is empty! " + error.message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
			return;
		}

		const auto handle = mio::detail::mio_open_file_helper::open_file(path, AccessMode);
		if (handle == invalid_handle)
		{
			std::string error_message = "Open file failed, file handle is invalid! " + detail::last_error().message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
		}

		map(handle, offset, length);
		if (file_handle_ != invalid_handle)
		{
			is_handle_internal_ = true;
		}
	}

#ifdef _WIN32
	template<access_mode AccessMode, typename ByteT>
	void basic_mmap<AccessMode, ByteT>::map_wsp(const wchar_t* path, const size_type offset, const size_type length)
	{
		if (path == nullptr || path[0] == L'\0')
		{
			std::error_code error = std::make_error_code(std::errc::invalid_argument);
			std::string error_message = "Path is empty! " + error.message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
		}

		const auto handle = mio::detail::mio_open_file_helper::open_file(path, AccessMode);
		if (handle == invalid_handle)
		{
			std::string error_message = "Open file failed, file handle is invalid! " + detail::last_error().message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
		}

		map(handle, offset, length);
		if (file_handle_ != invalid_handle)
		{
			is_handle_internal_ = true;
		}
	}
#endif

	/**
	 * Maps a file into memory using a path specified as a string-like object.
	 *
	 * This overload supports various path types (e.g. std::string, std::wstring, std::filesystem::path)
	 * via the underlying mio_open_file_helper::open_file() function. It first opens the file using the
	 * provided path and access mode. If the file is successfully opened, it then performs the memory mapping
	 * by calling the overload that accepts a file handle.
	 *
	 * @tparam String The type of the string representing the file path.
	 * @param path The file path to open and map.
	 * @param offset The offset within the file where the mapping should begin.
	 * @param length The length of the region to map. If set to a special value (e.g., map_entire_file),
	 *        the mapping will extend to the end of the file.
	 */
	template<access_mode AccessMode, typename ByteT>
	template<typename String>
	void basic_mmap<AccessMode, ByteT>::map(const String& path, const size_type offset,
		const size_type length)
	{
		// Open the file using the underlying platform-specific API.
		// The mio_open_file_helper::open_file() supports std::string, std::wstring, and std::filesystem::path.
		const auto handle = mio::detail::mio_open_file_helper::open_file(path, AccessMode);
		if (handle == invalid_handle)
		{
			std::string error_message = "Open file failed, file handle is invalid! " + detail::last_error().message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
		}

		// Proceed to map the file into memory using the obtained file handle.
		map(handle, offset, length);

		// Set internal flag indicating that the file handle was not internally managed
		// (i.e., it was provided externally by the user through the file opening process).
		is_handle_internal_ = true;
	}

	/**
	 * Maps a file into memory using an already opened file handle.
	 *
	 * This function performs several operations:
	 * 1. It clears the error code and validates that the provided file handle is not invalid.
	 * 2. It queries the file size using a platform-specific query_file_size() function.
	 * 3. It verifies that the requested mapping region [offset, offset+length) lies within the file bounds.
	 * 4. It creates a memory-mapped region of the file by calling the platform-specific memory_map() function.
	 *
	 * Note that:
	 * // We must unmap the previous mapping that may have existed prior to this call.
	 * // Note that this must only be invoked after a new mapping has been created in
	 * // order to provide the strong guarantee that, should the new mapping fail, the
	 * // map function leaves this instance in a state as though the function had
	 * // never been invoked.
	 *
	 * If a previous mapping exists, it is unmapped only after the new mapping has been successfully created,
	 * ensuring that if the new mapping fails, the internal state remains unchanged.
	 *
	 * 5. It updates the internal state (such as file_handle_, data pointer, mapping lengths,
	 *    and, on Windows, the file_mapping_handle_) to reflect the new mapping.
	 *
	 * @param handle The file handle of the file to be mapped.
	 * @param offset The offset within the file from which to start mapping.
	 * @param length The length of the memory region to map. If length is equal to map_entire_file,
	 *        then the mapping will extend from the offset to the end of the file.
	 */
	template<access_mode AccessMode, typename ByteT>
	void basic_mmap<AccessMode, ByteT>::map(const handle_type handle, const size_type offset, const size_type length)
	{
		if (handle == invalid_handle)
		{
			std::error_code error = std::make_error_code(std::errc::bad_file_descriptor);
			std::string error_message = "file handle is invalid. " + error.message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
			return;
		}

		// Query the size of the file using a platform-specific implementation.
		const auto file_size = detail::query_file_size(handle);
		if (file_size == 0)
		{
#ifdef _WIN32
			std::string error_message = "Query file size is failed - GetFileSizeEx: " + detail::last_error().message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
#else // POSIX
			std::string error_message = "Query file size is failed - fstat: " + detail::last_error().message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
#endif
		}

		// Validate that the requested mapping region [offset, offset+length) is within the file bounds.
		if (offset + length > file_size)
		{
			std::error_code error = std::make_error_code(std::errc::invalid_argument);
			std::string error_message = "requested large mapping region [offset, offset+length) outside of file size bounds. " + error.message();
			my_cpp2020_assert(false, error_message.c_str(), std::source_location::current());
			return;
		}

		// Create a memory mapping of the file region.
		// If length equals map_entire_file, the mapping extends from offset to the end of the file.
		const auto context = detail::memory_map
		(
			handle, offset,
			length == map_entire_file ? (file_size - offset) : length,
			AccessMode
		);

		if (context.data != nullptr)
		{
			// We must unmap the previous mapping that may have existed prior to this call.
			// Note that this must only be invoked after a new mapping has been created in
			// order to provide the strong guarantee that, should the new mapping fail, the
			// map function leaves this instance in a state as though the function had
			// never been invoked.
			unmap();

			// Update internal state with the new mapping details.
			file_handle_ = handle;
			is_handle_internal_ = false;
			data_ = reinterpret_cast<pointer>(context.data);
			length_ = context.length;
			mapped_length_ = context.mapped_length;
#ifdef _WIN32
			file_mapping_handle_ = context.file_mapping_handle;
#endif
		}
	}

	template<access_mode AccessMode, typename ByteT>
	template<access_mode Mode>
	requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
	void basic_mmap<AccessMode, ByteT>::sync()
	{
		if (!is_open())
		{
			//Current file is not open or closed
			return;
		}

		auto* data_pointer = data();
		auto* mapping_pointer = get_mapping_start();

		detail::sync_memory_map(data_pointer, mapping_pointer, mapped_length_, file_handle_);
	}

	template<access_mode AccessMode, typename ByteT>
	void basic_mmap<AccessMode, ByteT>::unmap()
	{
		if (!is_open()) { return; }
		// TODO do we care about errors here?
#ifdef _WIN32
		if (is_mapped())
		{
			::UnmapViewOfFile(get_mapping_start());
			::CloseHandle(file_mapping_handle_);
		}
#else // POSIX
		if (data_) { ::munmap(const_cast<pointer>(get_mapping_start()), mapped_length_); }
#endif

		// If `file_handle_` was obtained by our opening it (when map is called with
		// a path, rather than an existing file handle), we need to close it,
		// otherwise it must not be closed as it may still be used outside this
		// instance.
		if (is_handle_internal_)
		{
#ifdef _WIN32
			::CloseHandle(file_handle_);
#else // POSIX
			::close(file_handle_);
#endif
		}

		// Reset fields to their default values.
		data_ = nullptr;
		length_ = mapped_length_ = 0;
		file_handle_ = invalid_handle;
#ifdef _WIN32
		file_mapping_handle_ = invalid_handle;
#endif
	}

	template<access_mode AccessMode, typename ByteT>
	bool basic_mmap<AccessMode, ByteT>::is_mapped() const noexcept
	{
#ifdef _WIN32
		return file_mapping_handle_ != invalid_handle;
#else // POSIX
		return this->is_open();
#endif
	}

	template<access_mode AccessMode, typename ByteT>
	void basic_mmap<AccessMode, ByteT>::swap(basic_mmap& other)
	{
		if (this != &other)
		{
			using std::swap;
			swap(data_, other.data_);
			swap(file_handle_, other.file_handle_);
#ifdef _WIN32
			swap(file_mapping_handle_, other.file_mapping_handle_);
#endif
			swap(length_, other.length_);
			swap(mapped_length_, other.mapped_length_);
			swap(is_handle_internal_, other.is_handle_internal_);
		}
	}

	template<access_mode AccessMode, typename ByteT>
	template<access_mode Mode>
	requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
	void basic_mmap<AccessMode, ByteT>::conditional_sync()
	{
		// This is invoked from the destructor, so not much we can do about failures here.
		sync();
	}

	template<access_mode AccessMode, typename ByteT>
	template<access_mode Mode>
	requires (detail::FileReadAccess<Mode>)
	void basic_mmap<AccessMode, ByteT>::conditional_sync()
	{
		// noop
	}

	template<access_mode AccessMode, typename ByteT>
	bool operator==(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b)
	{
		return a.data() == b.data()
			&& a.size() == b.size();
	}

	template<access_mode AccessMode, typename ByteT>
	bool operator!=(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b)
	{
		return !(a == b);
	}

	template<access_mode AccessMode, typename ByteT>
	bool operator<(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b)
	{
		if (a.data() == b.data()) { return a.size() < b.size(); }
		return a.data() < b.data();
	}

	template<access_mode AccessMode, typename ByteT>
	bool operator<=(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b)
	{
		return !(a > b);
	}

	template<access_mode AccessMode, typename ByteT>
	bool operator>(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b)
	{
		if (a.data() == b.data()) { return a.size() > b.size(); }
		return a.data() > b.data();
	}

	template<access_mode AccessMode, typename ByteT>
	bool operator>=(const basic_mmap<AccessMode, ByteT>& a,
		const basic_mmap<AccessMode, ByteT>& b)
	{
		return !(a < b);
	}

} // namespace mio

#endif // MIO_BASIC_MMAP_IMPL

#endif // MIO_MMAP_HEADER

#ifndef MIO_SHARED_MMAP_HEADER
#define MIO_SHARED_MMAP_HEADER

namespace mio {

	/**
	 * Exposes (nearly) the same interface as `basic_mmap`, but endowes it with
	 * `std::shared_ptr` semantics.
	 *
	 * This is not the default behaviour of `basic_mmap` to avoid allocating on the heap if
	 * shared semantics are not required.
	 */
	template<
		access_mode AccessMode,
		typename ByteT
	> class basic_shared_mmap
	{
		using impl_type = basic_mmap<AccessMode, ByteT>;
		std::shared_ptr<impl_type> pimpl_;

	public:
		using value_type = typename impl_type::value_type;
		using size_type = typename impl_type::size_type;
		using reference = typename impl_type::reference;
		using const_reference = typename impl_type::const_reference;
		using pointer = typename impl_type::pointer;
		using const_pointer = typename impl_type::const_pointer;
		using difference_type = typename impl_type::difference_type;
		using iterator = typename impl_type::iterator;
		using const_iterator = typename impl_type::const_iterator;
		using reverse_iterator = typename impl_type::reverse_iterator;
		using const_reverse_iterator = typename impl_type::const_reverse_iterator;
		using iterator_category = typename impl_type::iterator_category;
		using handle_type = typename impl_type::handle_type;
		using mmap_type = impl_type;

		basic_shared_mmap() = default;
		basic_shared_mmap(const basic_shared_mmap&) = default;
		basic_shared_mmap& operator=(const basic_shared_mmap&) = default;
		basic_shared_mmap(basic_shared_mmap&&) = default;
		basic_shared_mmap& operator=(basic_shared_mmap&&) = default;

		/** Takes ownership of an existing mmap object. */
		basic_shared_mmap(mmap_type&& mmap)
			: pimpl_(std::make_shared<mmap_type>(std::move(mmap)))
		{
		}

		/** Takes ownership of an existing mmap object. */
		basic_shared_mmap& operator=(mmap_type&& mmap)
		{
			pimpl_ = std::make_shared<mmap_type>(std::move(mmap));
			return *this;
		}

		/** Initializes this object with an already established shared mmap. */
		basic_shared_mmap(std::shared_ptr<mmap_type> mmap) : pimpl_(std::move(mmap)) {}

		/** Initializes this object with an already established shared mmap. */
		basic_shared_mmap& operator=(std::shared_ptr<mmap_type> mmap)
		{
			pimpl_ = std::move(mmap);
			return *this;
		}

#ifdef __cpp_exceptions
		template<typename String>
		basic_shared_mmap(const String& path, const size_type offset = 0, const size_type length = map_entire_file)
		{
			map(path, offset, length);
		}

		basic_shared_mmap(const handle_type handle, const size_type offset = 0, const size_type length = map_entire_file)
		{
			map(handle, offset, length);
		}
#endif // __cpp_exceptions

		/**
		 * If this is a read-write mapping and the last reference to the mapping,
		 * the destructor invokes sync. Regardless of the access mode, unmap is
		 * invoked as a final step.
		 */
		~basic_shared_mmap() = default;

		/** Returns the underlying `std::shared_ptr` instance that holds the mmap. */
		std::shared_ptr<mmap_type> get_shared_ptr() { return pimpl_; }

		/**
		 * On UNIX systems 'file_handle' and 'mapping_handle' are the same. On Windows,
		 * however, a mapped region of a file gets its own handle, which is returned by
		 * 'mapping_handle'.
		 */
		handle_type file_handle() const noexcept
		{
			return pimpl_ ? pimpl_->file_handle() : invalid_handle;
		}

		handle_type mapping_handle() const noexcept
		{
			return pimpl_ ? pimpl_->mapping_handle() : invalid_handle;
		}

		/** Returns whether a valid memory mapping has been created. */
		bool is_open() const noexcept { return pimpl_ && pimpl_->is_open(); }

		/**
		 * Returns true if no mapping was established, that is, conceptually the
		 * same as though the length that was mapped was 0. This function is
		 * provided so that this class has Container semantics.
		 */
		bool empty() const noexcept { return !pimpl_ || pimpl_->empty(); }

		/**
		 * `size` and `length` both return the logical length, i.e. the number of bytes
		 * user requested to be mapped, while `mapped_length` returns the actual number of
		 * bytes that were mapped which is a multiple of the underlying operating system's
		 * page allocation granularity.
		 */
		size_type size() const noexcept { return pimpl_ ? pimpl_->length() : 0; }
		size_type length() const noexcept { return pimpl_ ? pimpl_->length() : 0; }
		size_type mapped_length() const noexcept
		{
			return pimpl_ ? pimpl_->mapped_length() : 0;
		}

		/**
		 * Returns a pointer to the first requested byte, or `nullptr` if no memory mapping
		 * exists.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		pointer data() noexcept { return pimpl_->data(); }
		const_pointer data() const noexcept { return pimpl_ ? pimpl_->data() : nullptr; }

		/**
		 * Returns an iterator to the first requested byte, if a valid memory mapping
		 * exists, otherwise this function call is undefined behaviour.
		 */
		iterator begin() noexcept { return pimpl_->begin(); }
		const_iterator begin() const noexcept { return pimpl_->begin(); }
		const_iterator cbegin() const noexcept { return pimpl_->cbegin(); }

		/**
		 * Returns an iterator one past the last requested byte, if a valid memory mapping
		 * exists, otherwise this function call is undefined behaviour.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		iterator end() noexcept { return pimpl_->end(); }
		const_iterator end() const noexcept { return pimpl_->end(); }
		const_iterator cend() const noexcept { return pimpl_->cend(); }

		/**
		 * Returns a reverse iterator to the last memory mapped byte, if a valid
		 * memory mapping exists, otherwise this function call is undefined
		 * behaviour.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		reverse_iterator rbegin() noexcept { return pimpl_->rbegin(); }
		const_reverse_iterator rbegin() const noexcept { return pimpl_->rbegin(); }
		const_reverse_iterator crbegin() const noexcept { return pimpl_->crbegin(); }

		/**
		 * Returns a reverse iterator past the first mapped byte, if a valid memory
		 * mapping exists, otherwise this function call is undefined behaviour.
		 */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		reverse_iterator rend() noexcept { return pimpl_->rend(); }
		const_reverse_iterator rend() const noexcept { return pimpl_->rend(); }
		const_reverse_iterator crend() const noexcept { return pimpl_->crend(); }

		/**
		 * Returns a reference to the `i`th byte from the first requested byte (as returned
		 * by `data`). If this is invoked when no valid memory mapping has been created
		 * prior to this call, undefined behaviour ensues.
		 */
		reference operator[](const size_type i) noexcept { return (*pimpl_)[i]; }
		const_reference operator[](const size_type i) const noexcept { return (*pimpl_)[i]; }

		/**
		 * Establishes a memory mapping with AccessMode. If the mapping is unsuccesful, the
		 * reason is reported via `error` and the object remains in a state as if this
		 * function hadn't been called.
		 *
		 * `path`, which must be a path to an existing file, is used to retrieve a file
		 * handle (which is closed when the object destructs or `unmap` is called), which is
		 * then used to memory map the requested region. Upon failure, `error` is set to
		 * indicate the reason and the object remains in an unmapped state.
		 *
		 * `offset` is the number of bytes, relative to the start of the file, where the
		 * mapping should begin. When specifying it, there is no need to worry about
		 * providing a value that is aligned with the operating system's page allocation
		 * granularity. This is adjusted by the implementation such that the first requested
		 * byte (as returned by `data` or `begin`), so long as `offset` is valid, will be at
		 * `offset` from the start of the file.
		 *
		 * `length` is the number of bytes to map. It may be `map_entire_file`, in which
		 * case a mapping of the entire file is created.
		 */
		template<typename String>
		void map(const String& path, const size_type offset,
			const size_type length)
		{
			map_impl(path, offset, length);
		}

		/**
		 * Establishes a memory mapping with AccessMode. If the mapping is unsuccesful, the
		 * reason is reported via `error` and the object remains in a state as if this
		 * function hadn't been called.
		 *
		 * `path`, which must be a path to an existing file, is used to retrieve a file
		 * handle (which is closed when the object destructs or `unmap` is called), which is
		 * then used to memory map the requested region. Upon failure, `error` is set to
		 * indicate the reason and the object remains in an unmapped state.
		 *
		 * The entire file is mapped.
		 */
		template<typename String>
		void map(const String& path)
		{
			map_impl(path, 0, map_entire_file);
		}

		/**
		 * Establishes a memory mapping with AccessMode. If the mapping is unsuccesful, the
		 * reason is reported via `error` and the object remains in a state as if this
		 * function hadn't been called.
		 *
		 * `handle`, which must be a valid file handle, which is used to memory map the
		 * requested region. Upon failure, `error` is set to indicate the reason and the
		 * object remains in an unmapped state.
		 *
		 * `offset` is the number of bytes, relative to the start of the file, where the
		 * mapping should begin. When specifying it, there is no need to worry about
		 * providing a value that is aligned with the operating system's page allocation
		 * granularity. This is adjusted by the implementation such that the first requested
		 * byte (as returned by `data` or `begin`), so long as `offset` is valid, will be at
		 * `offset` from the start of the file.
		 *
		 * `length` is the number of bytes to map. It may be `map_entire_file`, in which
		 * case a mapping of the entire file is created.
		 */
		void map(const handle_type handle, const size_type offset,
			const size_type length)
		{
			map_impl(handle, offset, length);
		}

		/**
		 * Establishes a memory mapping with AccessMode. If the mapping is unsuccesful, the
		 * reason is reported via `error` and the object remains in a state as if this
		 * function hadn't been called.
		 *
		 * `handle`, which must be a valid file handle, which is used to memory map the
		 * requested region. Upon failure, `error` is set to indicate the reason and the
		 * object remains in an unmapped state.
		 *
		 * The entire file is mapped.
		 */
		void map(const handle_type handle)
		{
			map_impl(handle, 0, map_entire_file);
		}

		/**
		 * If a valid memory mapping has been created prior to this call, this call
		 * instructs the kernel to unmap the memory region and disassociate this object
		 * from the file.
		 *
		 * The file handle associated with the file that is mapped is only closed if the
		 * mapping was created using a file path. If, on the other hand, an existing
		 * file handle was used to create the mapping, the file handle is not closed.
		 */
		void unmap() { if (pimpl_) pimpl_->unmap(); }

		void swap(basic_shared_mmap& other) { pimpl_.swap(other.pimpl_); }

		/** Flushes the memory mapped page to disk. Errors are reported via `error`. */
		template<access_mode Mode = AccessMode>
		requires (detail::FileWriteAccess<Mode> || detail::FilePrivateAccess<Mode>)
		void sync() { if (pimpl_) pimpl_->sync(); }

		/** All operators compare the underlying `basic_mmap`'s addresses. */

		friend bool operator==(const basic_shared_mmap& a, const basic_shared_mmap& b)
		{
			return a.pimpl_ == b.pimpl_;
		}

		friend bool operator!=(const basic_shared_mmap& a, const basic_shared_mmap& b)
		{
			return !(a == b);
		}

		friend bool operator<(const basic_shared_mmap& a, const basic_shared_mmap& b)
		{
			return a.pimpl_ < b.pimpl_;
		}

		friend bool operator<=(const basic_shared_mmap& a, const basic_shared_mmap& b)
		{
			return a.pimpl_ <= b.pimpl_;
		}

		friend bool operator>(const basic_shared_mmap& a, const basic_shared_mmap& b)
		{
			return a.pimpl_ > b.pimpl_;
		}

		friend bool operator>=(const basic_shared_mmap& a, const basic_shared_mmap& b)
		{
			return a.pimpl_ >= b.pimpl_;
		}

	private:
		template<typename MappingToken>
		void map_impl(const MappingToken& token, const size_type offset,
			const size_type length)
		{
			if (!pimpl_)
			{
				mmap_type mmap = make_mmap<mmap_type>(token, offset, length);
				pimpl_ = std::make_shared<mmap_type>(std::move(mmap));
			}
			else
			{
				pimpl_->map(token, offset, length);
			}
		}
	};

	/**
	 * This is the basis for all read-only mmap objects and should be preferred over
	 * directly using basic_shared_mmap.
	 */
	template<typename ByteT>
	using basic_shared_mmap_source = basic_shared_mmap<access_mode::read, ByteT>;

	/**
	 * This is the basis for all read-write mmap objects and should be preferred over
	 * directly using basic_shared_mmap.
	 */
	template<typename ByteT>
	using basic_shared_mmap_sink = basic_shared_mmap<access_mode::write, ByteT>;

	/**
	 * These aliases cover the most common use cases, both representing a raw byte stream
	 * (either with a char or an unsigned char/uint8_t).
	 */
	using shared_mmap_source = basic_shared_mmap_source<char>;
	using shared_ummap_source = basic_shared_mmap_source<unsigned char>;

	using shared_mmap_sink = basic_shared_mmap_sink<char>;
	using shared_ummap_sink = basic_shared_mmap_sink<unsigned char>;

} // namespace mio

#endif // MIO_SHARED_MMAP_HEADER
