#pragma once

#include <cstddef>
#include <charconv>
#include <vector>
#include <string_view>
#include <variant>
#include "language.hpp"


namespace vector::lexing
{
	// utility for identification
	[[nodiscard]] constexpr auto clamp_character
	(const unsigned char value, const unsigned char low, const unsigned char high) noexcept -> bool
	{
		return value >= low && value <= high;
	}


	// character identification

	// check "word" against keyword database
	// returns true on match
	[[nodiscard]] constexpr auto is_keyword(const language::Source word) noexcept -> bool
	{
		return 
			word == "mutable" ||
			word == "return" ||
			word == "function" ||
			word == "let" ||
			word == "structure" ||
			word == "if" ||
			word == "else" ||
			word == "import";
	}
	[[nodiscard]] constexpr auto is_whitespace(const char character) noexcept -> bool
	{
		return
			character == ' ' ||
			character == '\n' ||
			character == '\t';
	}
	[[nodiscard]] constexpr auto is_comment(const std::pair<char, char>& next_two) noexcept -> bool
	{
		return next_two.first == '/' && next_two.second == '/';
	}
	[[nodiscard]] constexpr auto is_multiline_comment
	(const std::pair<char, char>& next_two) noexcept -> bool
	{
		return next_two.first == '/' && next_two.second == '*';
	}
	// TODO: add support for "as" and other to come word operators
	[[nodiscard]] constexpr auto is_word(const char character) noexcept -> bool
	{
		// not implemented using "std::isalpha" because it is not constexpr
		const auto ascii = static_cast<unsigned char>(character);
		return clamp_character(ascii, 'A', 'Z') || clamp_character(ascii, 'a', 'z') || character == '_';
	}
	[[nodiscard]] constexpr auto is_number(const char character) noexcept -> bool
	{
		return clamp_character(static_cast<unsigned char>(character), '0', '9');
	}


	// actual lexing
	
	// string literal: "abc"
	[[nodiscard]] auto lex_string_literal(language::SourceIterator iterator) noexcept -> language::Token
	{
		auto output = language::Token {language::Token::Type::string_literal, std::string {}};

		while (*iterator != '"' && *iterator != '\0')
		{
			std::get<std::string>(output.value).push_back(*iterator);
			++iterator;
		}

		++iterator;
		return output;
	}
	// character literal: 'x'
	[[nodiscard]] auto lex_char_literal(language::SourceIterator iterator) noexcept -> language::Token
	{
		const auto output = language::Token {language::Token::Type::character_literal, *iterator};
		iterator += 2;

		return output;
	}

	// any word in source code: function
	[[nodiscard]] auto lex_word(language::SourceIterator iterator) noexcept -> language::Token 
	{
	   auto word = std::string {};

		do
		{
			word.push_back(*iterator);
			++iterator;
		}
		while (is_word(*iterator) || is_number(*iterator));

		return
			{
				(
					is_keyword(word) ?
					language::Token::Type::keyword : language::Token::Type::identifier
				),
				word
			}; 
	}
	// numeric literals like: 123
	[[nodiscard]] auto lex_number(language::SourceIterator iterator) noexcept -> language::Token 
	{
		auto has_decimal = bool {};
		auto number_begin = iterator;
		auto is_first_decimal =
			[&iterator, &has_decimal]()
			{
				return *iterator == '.' && !has_decimal;
			};

		while (std::isdigit(*iterator) || is_first_decimal())
		{
			if (is_first_decimal())
			{
				has_decimal = true;
			}
			++iterator;
		}

		if (!has_decimal)
		{
			return {language::Token::Type::whole_literal, std::strtoull(number_begin, nullptr, 10)};
		}
		else
		{
			return {language::Token::Type::fraction_literal, std::strtod(number_begin, nullptr)};
		}
	}
	// any sign or ascii that does not math other descriptions: +
	[[nodiscard]] auto lex_sign(language::SourceIterator iterator) noexcept -> language::Token
	{
		auto disambiguate =
			[&iterator]
			(const char possible_combinator, const language::Sign single, const language::Sign combination)
				noexcept
				-> language::Token::Value
			{
				if (*iterator == possible_combinator) UNLIKELY
				{
					++iterator;
					return combination;
				}
				else
				{
					return single;
				}
			};

		switch (*iterator++)
		{
		// operators
		// univocal
		case '+': return {language::Token::Type::operator_symbol, language::Sign::plus};
		case '/': return {language::Token::Type::operator_symbol, language::Sign::slash};
		case '%': return {language::Token::Type::operator_symbol, language::Sign::percent};
		case 'a': return {language::Token::Type::operator_symbol, language::Sign::as};
		case '.': return {language::Token::Type::operator_symbol, language::Sign::dot};
		case '&': return {language::Token::Type::operator_symbol, language::Sign::ampersand};
		case '|': return {language::Token::Type::operator_symbol, language::Sign::pipe};
		case '^': return {language::Token::Type::operator_symbol, language::Sign::power};
		case '~': return {language::Token::Type::operator_symbol, language::Sign::tilde};

		// ambiguous
		case '-': return {language::Token::Type::operator_symbol, disambiguate('>', language::Sign::minus, language::Sign::arrow)};
		case '*': return {language::Token::Type::operator_symbol, disambiguate('*', language::Sign::astrisk, language::Sign::double_astrisk)};
		case '>': return {language::Token::Type::operator_symbol, disambiguate('=', language::Sign::greater_than, language::Sign::greater_equals)};
		case '<': return {language::Token::Type::operator_symbol, disambiguate('=', language::Sign::less_than, language::Sign::less_equals)};
		case '=': return {language::Token::Type::operator_symbol, disambiguate('=', language::Sign::equals, language::Sign::double_equals)};
		case '!': return {language::Token::Type::operator_symbol, disambiguate('=', language::Sign::exclamation, language::Sign::exclamation_equals)};


		// seperators
		// univocal
		case ',': return {language::Token::Type::seperator_symbol, language::Sign::comma};
		case ';': return {language::Token::Type::seperator_symbol, language::Sign::semicolon};
		case '(': return {language::Token::Type::seperator_symbol, language::Sign::opening_parenthese};
		case ')': return {language::Token::Type::seperator_symbol, language::Sign::closing_parenthese};
		case '[': return {language::Token::Type::seperator_symbol, language::Sign::opening_bracket};
		case ']': return {language::Token::Type::seperator_symbol, language::Sign::closing_bracket};
		case '{': return {language::Token::Type::seperator_symbol, language::Sign::opening_brace};
		case '}': return {language::Token::Type::seperator_symbol, language::Sign::closing_brace};

		// ambiguous
		case ':': return {language::Token::Type::seperator_symbol, disambiguate(':', language::Sign::colon, language::Sign::double_colon)};

		// literal indicators
		case '"': return lex_string_literal(iterator);
		case '\'': return lex_char_literal(iterator);

		// dbg
		UNLIKELY default: abort();
		}
	}

	// comments have no impact after the lexing stage
	// skip iterator to end of on line comment
	auto skip_comment(language::SourceIterator iterator) noexcept -> void
	{
		do
		{
			++iterator;
		}
		while (*iterator != '\n' && *iterator != '\0');
		++iterator;
	}
	// skip iterator to end of multiline comment
	auto skip_multiline_comment(language::SourceIterator iterator) noexcept -> void
	{
		do
		{
			iterator += 2;
		}
		while (*iterator != '*' && iterator[1] != '/' && *iterator != '\0');
		iterator += 2;
	}

	// break up source code into tokens
	[[nodiscard]] auto lex(const language::Source source) noexcept -> language::Tokens
	{
		auto output = language::Tokens {};

		auto iterator = source.cbegin();
		while (iterator < source.cend())
		{
			const auto character = *iterator;

			// whitespace is strictly for readabilty
			if (is_whitespace(character))
			{
				++iterator;
			}
			// comment
			else if (is_comment({iterator[0], iterator[1]}))
			{
				skip_comment(iterator += 2);
			}
			// multiline line comment
			else if (is_multiline_comment({iterator[0], iterator[1]}))
			{
				skip_multiline_comment(iterator += 2);
			}
			// keyword or identifier aka. word
			else if (is_word(character))
			{
				output.push_back(lex_word(iterator));
			}
			// numeric literals
			else if (is_number(character))
			{
				output.push_back(lex_number(iterator));
			}
			// operators and seperators
			else
			{
				output.push_back(lex_sign(iterator));
			}
		}

		return output;
	}
}