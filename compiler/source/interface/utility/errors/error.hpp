#pragma once

#include <limits>
#include <variant>
#include <optional>
#include <vector>
#include <string>
#include <iostream>
#include "../utility.hpp"

#define VECTOR_HANDLE_RESULT(result) \
	if ((result).is_error()) UNLIKELY \
	{ \
		(result).error().handle(); \
	}

#define VECTOR_ASSERT(condition, error) \
	if (!(condition)) UNLIKELY \
	{ \
		return (error); \
	}
#define VECTOR_ASSERT_RESULT(result) \
	if ((result).is_error()) UNLIKELY \
	{ \
		return (result).error(); \
	}


namespace vector::error
{
	// when adding to "Code" always add a "stringify_code" case

	// error information container
	enum class Code : unsigned char
	{
		// filesystem
		opening_file
		, reading_file


		// syntax
		// general
		, missing_comma

		// top level
		, expected_top_level_keyword

		// functions
		, missing_function_return_arrow
		, missing_function_return_type
		, unterminated_function

		// parameters
		, missing_parameter_identifier
		, missing_parameter_colon
		, missing_parameter_type

		// statements
		, statement_sign
		, wrong_keyword_statement
		, invalid_identifier_statement
		, missing_semicolon
		, missing_calling_parenthese

		// variables
		, missing_variable_identifier
		, missing_variable_equals

		// expressions
		, wrong_keyword_operator
		, missing_closing_parenthese
		, missing_closing_bracket


		// user
		, parameters
	};
	[[nodiscard]] constexpr auto stringify_code(const Code code) noexcept -> std::string_view
	{
		switch (code)
		{
			// filesystem
			case Code::opening_file: return "opening file";
			case Code::reading_file: return "reading file";


			// syntax
			// general
			case Code::missing_comma: return "missing comma";

			// top level
			case Code::expected_top_level_keyword: return "wrong top level token type";

			// functions
			case Code::missing_function_return_arrow: return "missing return arrow";
			case Code::missing_function_return_type: return "missing return type";

			// parameters
			case Code::missing_parameter_identifier: return "missing parameter identifier";
			case Code::missing_parameter_colon: return "missing parameter colon";
			case Code::missing_parameter_type: return "missing parameter type";

			// statements
			case Code::statement_sign: return "statement sign";
			case Code::wrong_keyword_statement: return "wrong keyword statement";
			case Code::missing_semicolon: return "missing semicolon";

			// variables
			case Code::missing_variable_identifier: return "missing variable identifier";
			case Code::missing_variable_equals: return "missing variable equals";

			// expressions
			case Code::wrong_keyword_operator: return "wrong keyword operator";
			case Code::missing_closing_parenthese: return "mising closing parenthese";
			case Code::missing_closing_bracket: return "mising closing bracket";


			// user
			case Code::parameters: return "parameters";

			// dbg
			UNLIKELY default: std::abort();
		}
	}

	struct None {} none;

	using Severity = unsigned char;
	static constexpr auto maximum_severity = std::numeric_limits<Severity>::max();
	static constexpr auto medium_severity = std::numeric_limits<Severity>::max() / 2;
	static constexpr auto minimum_severity = std::numeric_limits<Severity>::min();

	class Error
	{
		using Message = std::string_view;
		using Code = Code;
		using Severity = Severity;

		Code _code;
		Message _message;
		Severity _severity;

	public:
		NODISCARD_CONSTRUCTOR Error
		(const Message message, const Code code, const Severity severity = maximum_severity)
		noexcept
			: _code(code)
			, _message(message)
			, _severity(severity)
		{}

		// setters and getters
		[[nodiscard]] constexpr auto code() noexcept -> Code&
		{
			return _code;
		}
		[[nodiscard]] constexpr auto code() const noexcept -> Code
		{
			return _code;
		}
		[[nodiscard]] constexpr auto message() noexcept -> Message&
		{
			return _message;
		}
		[[nodiscard]] constexpr auto message() const noexcept -> Message
		{
			return _message;
		}
		[[nodiscard]] constexpr auto severity() noexcept -> Severity&
		{
			return _severity;
		}
		[[nodiscard]] constexpr auto severity() const noexcept -> Severity
		{
			return _severity;
		}


		// handling
		auto tell() const noexcept -> void
		{
			std::cerr << stringify_code(code()) << " error: " << message() << '\n';
		}
		constexpr auto handle() const noexcept -> void
		{
			constexpr auto severity_limit = std::numeric_limits<Severity>::max() * 0.75;

			tell();

			if (severity() > severity_limit) UNLIKELY
			{
				std::exit(static_cast<int>(code()));
			}
		}
	};
}