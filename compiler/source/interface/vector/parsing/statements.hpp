#pragma once

#include "../language.hpp"
#include "../../utility/errors/result.hpp"
#include "patterns.hpp"
#include "expressions.hpp"


namespace vector::parsing
{
	[[nodiscard]] auto parse_block(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxForest>;


	// parse function return statement
	// expects iterator to expression i.e. after "return" keyword
	[[nodiscard]] auto parse_return_statement(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		const auto expression_result = parse_expression(iterator);
		VECTOR_ASSERT_RESULT(expression_result);
		const auto& expression = expression_result.value();

		return
			language::SyntaxTree
			{
				language::SyntaxTree::Type::return_statement,
				std::make_unique<language::SyntaxTree>(expression)
			};
	}
	// parse let variable definition inside any block
	// expects iterator to identifier i.e. after "let" keyword
	[[nodiscard]] auto parse_let_statement(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		auto is_mutable = false;
		if
		(
			iterator->type == language::Token::Type::keyword &&
			std::get<std::string>(iterator->value) == "mutable"
		)
		{
			++iterator;
			is_mutable = true;
		}

		VECTOR_ASSERT
		(
			iterator->type == language::Token::Type::identifier,
			(error::Error {"expected variable identifier", error::Code::missing_variable_identifier})
		);

		const auto& identifier = std::get<std::string>(iterator++->value);

		VECTOR_ASSERT
		(
			iterator->is(language::Sign::equals, language::Token::Type::operator_symbol),
			(error::Error {"expected variable equals", error::Code::missing_variable_equals})
		)

		const auto initializer_result = parse_expression(++iterator);
		VECTOR_ASSERT_RESULT(initializer_result);
		const auto& initializer = initializer_result.value();

		return
			language::SyntaxTree
			{
				language::SyntaxTree::Type::variable_definition,
				language::VariableDefinition
				{
					identifier,
					std::make_unique<language::SyntaxTree>(initializer),
					is_mutable
				}
			};
	}
	// parse if conditional inside any block
	// expects iterator to condition i.e. after "if" keyword
	[[nodiscard]] auto parse_if_statement(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		const auto condition_result = parse_expression(iterator);
		VECTOR_ASSERT_RESULT(condition_result);
		const auto& condition = condition_result.value();
		++iterator;

		const auto true_block_result = parse_block(iterator);
		VECTOR_ASSERT_RESULT(true_block_result);
		const auto& true_block = true_block_result.value();

		if
		(
			iterator->type != language::Token::Type::keyword ||
			std::get<std::string>(iterator->value) != "else"
		)
		UNLIKELY
		{
			// dbg
			std::abort();
		}
		iterator += 2;

		const auto false_block_result = parse_block(iterator);
		VECTOR_ASSERT_RESULT(false_block_result);
		const auto& false_block = false_block_result.value();

		return
			language::SyntaxTree
			{
				language::SyntaxTree::Type::if_statement,
				language::IfStatement
				{
					std::make_unique<language::SyntaxTree>(condition),
					true_block,
					false_block
				}
			};
	}

	[[nodiscard]] auto parse_reassignment_statement(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		const auto& identifier = std::get<std::string>(iterator++->value);
		VECTOR_ASSERT
		(
			iterator->is(language::Sign::equals, language::Token::Type::operator_symbol),
			(
				error::Error
				{"expected equals or opening parentheses", error::Code::invalid_identifier_statement}
			)
		);
		++iterator;

		const auto value_result = parse_expression(iterator);
		VECTOR_ASSERT_RESULT(value_result);
		const auto& value = value_result.value();

		return
			language::SyntaxTree
			{
				language::SyntaxTree::Type::variable_reassignment,
				language::VariableReassignment
				{identifier, std::make_unique<language::SyntaxTree>(value)}
			};
	}

	// parse any keyword statement inside a block
	// forks into specific parsing fuctions ("return", "let", ...)
	// expects iterator to keyword
	[[nodiscard]] auto parse_keyword_statement(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		const auto& keyword = std::get<std::string>(iterator++->value);

		if (keyword == "return")
		{
			return parse_return_statement(iterator);
		}
		else if (keyword == "let")
		{
			return parse_let_statement(iterator);
		}
		else if (keyword == "if")
		{
			return parse_if_statement(iterator);
		}
		else
		{
			return error::Error {"unkown keyword statement", error::Code::wrong_keyword_statement};
		}
	}

	// parse any statement starting with an arbritray identifier excluding keywords inside block
	// forks into functions: "parse_reassignment_statement", "parse_call_expression"
	[[nodiscard]] auto parse_identifier_statement(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		if (iterator[1].is(language::Sign::opening_parenthese))
		{
			return parse_call_expression(iterator);
		}
		else
		{
			return parse_reassignment_statement(iterator);
		}
	}


	// TODO: repair semicolon check
	// parse any statement into syntax tree
	// forks into parsing for keyword statement and identifier statement
	// expects iterator to block of tokens on the first statement
	[[nodiscard]] auto parse_statement(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		auto statement_result =
			error::Result<language::SyntaxTree>
			{error::Error {"expected statement not symbol", error::Code::statement_sign}};

		switch (iterator->type)
		{
			case language::Token::Type::identifier:
				statement_result = parse_identifier_statement(iterator);
				break;
			case language::Token::Type::keyword:
				statement_result = parse_keyword_statement(iterator);
				break;

			// operators and seperators
			UNLIKELY default:
				break;
		}

		// skip all semicolons
		while (iterator->is(language::Sign::semicolon))
		{
			++iterator;
		}

		return statement_result;
	}

	[[nodiscard]] auto parse_block(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxForest>
	{
		auto block = language::SyntaxForest {};

		while (!iterator->is(language::Sign::closing_brace))
		{
			const auto statement_result = parse_statement(iterator);
			VECTOR_ASSERT_RESULT(statement_result);
			const auto statement = statement_result.value();

			block.push_back(statement);
		}

		++iterator;
		return block;
	}
}