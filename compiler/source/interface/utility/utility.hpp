#pragma once

#if defined(__unix__) || defined(__APPLE__)
# define UNIX_OPERATING_SYSTEM
#elif defined(_WIN32)
# define WINDOWS_OPERATING_SYSTEM
#else
# error "unknown operating system could cause complications"
#endif

#if defined(__GNUC__) || defined(__clang__)
# define GNU_COMPILER
#elif defined(_MSC_VER)
# define MICROSOFT_COMPILER
#elif
# error "unknown compiler could cause complications"
#endif


#if defined(GNU_COMPILER)
# define RESTRICT  __restrict__
#elif defined(MICROSOFT_COMPILER)
# define RESTRICT __restrict
#else
# define RESTRICT
#endif

// has likely unlikely attributes
#if __has_cpp_attribute(likely) == 201803
# define LIKELY [[likely]]
#else
# define LIKELY
#endif
#if __has_cpp_attribute(unlikely) == 201803
# define UNLIKELY [[unlikely]]
#else
# define UNLIKELY
#endif

// has nodiscard for constructors
#if __has_cpp_attribute(nodiscard) == 201907
# define NODISCARD_CONSTRUCTOR [[nodiscard]]
#else
# define NODISCARD_CONSTRUCTOR
#endif