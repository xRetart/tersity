#pragma once

#include "../language.hpp"
#include "patterns.hpp"
#include "../lexing.hpp"
#include "../../utility/errors/result.hpp"


namespace vector::parsing
{
	[[nodiscard]] auto parse_expression(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>;


	[[nodiscard]] constexpr auto binary_operator_precedence(const language::Sign symbol) noexcept
		-> unsigned char
	{
		switch (symbol)
		{
			case language::Sign::equals: return 1;
			case language::Sign::double_equals: return 2;
			case language::Sign::exclamation_equals: return 2;
			case language::Sign::greater_than: return 2;
			case language::Sign::greater_equals: return 2;
			case language::Sign::less_than: return 2;
			case language::Sign::less_equals: return 2;
			case language::Sign::plus: return 3;
			case language::Sign::minus: return 3;
			case language::Sign::astrisk: return 4;
			case language::Sign::slash: return 4;
			case language::Sign::double_astrisk: return 5;
			case language::Sign::as: return 6;
			case language::Sign::dot: return 7;
			case language::Sign::arrow: return 7;
			case language::Sign::double_colon: return 8;

			// unspecified binary operator
			default: return 0;
		}
	}
	[[nodiscard]] auto binary_operation(const language::Sign operator_symbol) noexcept
		-> language::BinaryExpression::Operation
	{
		switch (operator_symbol)
		{
			case language::Sign::plus: return language::BinaryExpression::Operation::addition;
			case language::Sign::minus: return language::BinaryExpression::Operation::subtraction;
			case language::Sign::astrisk: return language::BinaryExpression::Operation::multiplication;
			case language::Sign::slash: return language::BinaryExpression::Operation::division;
			case language::Sign::percent: return language::BinaryExpression::Operation::modulo;
			case language::Sign::double_astrisk: return language::BinaryExpression::Operation::exponentiation;
			case language::Sign::equals: return language::BinaryExpression::Operation::assign;
			case language::Sign::dot: return language::BinaryExpression::Operation::access_instance;
			case language::Sign::arrow: return language::BinaryExpression::Operation::access_pointer;
			case language::Sign::double_colon: return language::BinaryExpression::Operation::access_space;
			case language::Sign::pipe: return language::BinaryExpression::Operation::bitwise_or;
			case language::Sign::ampersand: return language::BinaryExpression::Operation::bitwise_and;
			case language::Sign::power: return language::BinaryExpression::Operation::bitwise_xor;
			case language::Sign::double_equals: return language::BinaryExpression::Operation::equal;
			case language::Sign::exclamation_equals: return language::BinaryExpression::Operation::not_equal;
			case language::Sign::less_than: return language::BinaryExpression::Operation::less_than;
			case language::Sign::less_equals: return language::BinaryExpression::Operation::less_equal;
			case language::Sign::greater_than: return language::BinaryExpression::Operation::greater_than;
			case language::Sign::greater_equals: return language::BinaryExpression::Operation::greater_equal;
			case language::Sign::double_pipe: return language::BinaryExpression::Operation::logical_or;
			case language::Sign::double_ampersand: return language::BinaryExpression::Operation::logical_and;

			// DEBUG
			UNLIKELY default: abort();
		}
	}



	// TODO: make implicit literal "Natural"
	[[nodiscard]] auto parse_whole_literal_expression(language::TokenIterator iterator) noexcept
		-> language::SyntaxTree
	{
		const auto value = std::get<unsigned long long>(iterator++->value);
		auto type = std::string_view {};

		if (iterator->type == language::Token::Type::identifier)
		{
			type = std::get<std::string>(iterator++->value);
		}
		else
		{
			type = "i";
		}

		return
			{
				language::SyntaxTree::Type::whole_literal_expression,
				language::WholeLiteralExpression {type, value}
			};
	}
	[[nodiscard]] auto parse_fraction_literal_expression(language::TokenIterator iterator) noexcept
		-> language::SyntaxTree
	{
		const auto value = std::get<double>(iterator++->value);
		auto type = std::string_view {};

		if (iterator->type == language::Token::Type::identifier)
		{
			type = std::get<std::string>(iterator++->value);
		}
		else
		{
			type = "Real";
		}

		return
			{
				language::SyntaxTree::Type::fraction_literal_expression,
				language::FractionLiteralExpression {type, value}
			};
	}
	[[nodiscard]] auto parse_character_literal_expression(language::TokenIterator iterator) noexcept
		-> language::SyntaxTree
	{
		const auto value = std::get<char>(iterator++->value);
		auto type = std::string_view {};

		if (iterator->type == language::Token::Type::identifier)
		{
			type = std::get<std::string>(iterator++->value);
		}
		else
		{
			type = "Character";
		}

		return
			{
				language::SyntaxTree::Type::character_literal_expression,
				language::CharacterLiteralExpression {type, value}
			};
	}
	[[nodiscard]] auto parse_string_literal_expression(language::TokenIterator iterator) noexcept
		-> language::SyntaxTree
	{
		const auto& value = std::get<std::string>(iterator++->value);
		auto type = std::string_view {};

		if (iterator->type == language::Token::Type::identifier)
		{
			type = std::get<std::string>(iterator++->value);
		}
		else
		{
			type = "[]Character";
		}

		return
			{
				language::SyntaxTree::Type::string_literal_expression,
				language::StringLiteralExpression {type, value}
			};
	}
	[[nodiscard]] auto parse_variable_expression(language::TokenIterator iterator) noexcept
		-> language::SyntaxTree
	{
		return
			{
				language::SyntaxTree::Type::variable_expression,
				std::get<std::string>(iterator++->value)
			};
	}
	[[nodiscard]] auto parse_call_expression(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		const auto& callee = std::get<std::string>(iterator->value);
		iterator += 2;  // skip identifier and assumed parenthese

		auto parameters = decltype(language::CallExpression::parameters) {};
		auto add_to_parameters =
			[&parameters, &iterator]() noexcept -> error::Result<void>
			{
				const auto expression_result = parse_expression(iterator);
				VECTOR_ASSERT_RESULT(expression_result);
				const auto expression = expression_result.value();

				parameters.push_back(std::move(expression));
				return error::none;
			};

		const auto parameter_parsing_result =
			iterate_seperated_list(iterator, add_to_parameters);
		VECTOR_ASSERT_RESULT(parameter_parsing_result);

		return
			language::SyntaxTree
			{
				language::SyntaxTree::Type::call_expression,
				language::CallExpression {parameters, callee}
			};
	}
	[[nodiscard]] auto parse_subscript_expression(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		const auto& identifier = std::get<std::string>(iterator->value);
		iterator += 2;

		const auto index_result = parse_expression(iterator);
		VECTOR_ASSERT_RESULT(index_result);

		VECTOR_ASSERT
		(
			iterator->is(language::Sign::closing_bracket),
			(
				error::Error
				{"subscript was not closed by closing bracket", error::Code::missing_closing_bracket}
			)
		);
		++iterator;

		return 
			language::SyntaxTree
			{
				language::SyntaxTree::Type::subscript_expression,
				language::IdentifiedExpression
				{
					identifier,
					std::make_unique<language::SyntaxTree>(std::move(index_result.value()))
				}
			};
	}
	[[nodiscard]] auto parse_identifier_expression(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		const auto& identifier = std::get<std::string>(iterator->value);

		if (iterator[1].is(language::Sign::opening_parenthese))
		{
			const auto expression_result = parse_call_expression(iterator);
			VECTOR_ASSERT_RESULT(expression_result);
			return std::move(expression_result.value());
		}
		else if (iterator[1].is(language::Sign::opening_bracket))
		{
			const auto subscript_result = parse_subscript_expression(iterator);
			VECTOR_ASSERT_RESULT(subscript_result);
			return std::move(subscript_result.value());
		}
		else
		{
			++iterator;
			return
				language::SyntaxTree
				{language::SyntaxTree::Type::variable_expression, identifier};
		}
	}
	[[nodiscard]] auto parse_parenthese_expression(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		const auto expression_result = parse_expression(iterator);
		VECTOR_ASSERT_RESULT(expression_result);
		const auto expression = expression_result.value();

		VECTOR_ASSERT
		(
			iterator->is(language::Sign::closing_parenthese),
			(error::Error {"expected closing parenthese", error::Code::missing_closing_parenthese})
		);
		++iterator;

		return expression;
	}
	[[nodiscard]] auto parse_array_literal(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		auto elements = language::SyntaxForest {};
		auto add_element =
			[&iterator, &elements]() noexcept -> error::Result<void>
			{
				const auto element_expression_result = parse_expression(iterator);
				VECTOR_ASSERT_RESULT(element_expression_result);
				elements.push_back(element_expression_result.value());

				return error::none;
			};

		VECTOR_ASSERT_RESULT(iterate_seperated_list(iterator, add_element, language::Sign::closing_bracket));

		auto type = std::string_view {};
		if (iterator->type == language::Token::Type::identifier)
		{
			type = std::get<std::string>(iterator++->value);
		}
		// else not defined because at this stage impossible

		return
			language::SyntaxTree
			{
				language::SyntaxTree::Type::array_literal_expression,
				language::ArrayLiteralExpression {type, elements}
			};
	}
	[[nodiscard]] auto parse_seperated_expression(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		switch (std::get<language::Sign>(iterator->value))
		{
		case language::Sign::opening_parenthese:	
			return parse_parenthese_expression(++iterator);
		case language::Sign::opening_bracket:	
			return parse_array_literal(++iterator);

		UNLIKELY default:
			return error::Error {"unkown seperator sign", error::Code::statement_sign};
		}
	}

	[[nodiscard]] auto parse_primary_expression(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		switch (iterator->type)
		{
			case language::Token::Type::identifier:
				return parse_identifier_expression(iterator);
			case language::Token::Type::whole_literal:
				return parse_whole_literal_expression(iterator);
			case language::Token::Type::fraction_literal:
				return parse_fraction_literal_expression(iterator);
			case language::Token::Type::character_literal:
				return parse_character_literal_expression(iterator);
			case language::Token::Type::string_literal:
				return parse_string_literal_expression(iterator);
			case language::Token::Type::seperator_symbol:
				return parse_seperated_expression(iterator);

			// dbg
			UNLIKELY default:
				std::abort();
		}
	}

	// TODO: rethink implementation
	[[nodiscard]] auto parse_binary_expression
	(
		language::TokenIterator iterator,
		language::SyntaxTree& left,
		const unsigned char precedence = 1
	)
		noexcept
		-> error::Result<void>
	{
		while (true)
		{
			if (iterator->type == language::Token::Type::operator_symbol)
			{
				const auto token_operator = std::get<language::Sign>(iterator->value);
				const auto operator_precedence = binary_operator_precedence(token_operator);

				if (operator_precedence < precedence)
				{
					return error::none;
				}

				++iterator;

				auto right_result = parse_primary_expression(iterator);
				VECTOR_ASSERT_RESULT(right_result);
				auto& right = right_result.value();

				if (iterator->type == language::Token::Type::operator_symbol)
				{
					const auto next_operator = std::get<language::Sign>(iterator->value);
					const auto next_precedence = binary_operator_precedence(next_operator);

					if (operator_precedence < next_precedence)
					{
						VECTOR_ASSERT_RESULT
						(parse_binary_expression(iterator, right, precedence + 1));
					}
				}
				

				left =
					language::SyntaxTree
					{
						language::SyntaxTree::Type::binary_expression,
						language::BinaryExpression
						{
							binary_operation(token_operator),
							std::make_unique<language::SyntaxTree>(left),
							std::make_unique<language::SyntaxTree>(right)
						}
					};
			}
			else if (iterator->type == language::Token::Type::identifier)
			{
				const auto& operator_identifier = std::get<std::string>(iterator++->value);

				if (operator_identifier == "as")
				{
					left =
						language::SyntaxTree
						{
							language::SyntaxTree::Type::binary_expression,
							language::BinaryExpression
							{
								language::BinaryExpression::Operation::cast,
								std::make_unique<language::SyntaxTree>(left),
								std::make_unique<language::SyntaxTree>
								(
									language::SyntaxTree
									{
										language::SyntaxTree::Type::type_expression,
										std::get<std::string>(iterator++->value)
									}
								)
							}
						};
					return error::none;
				}
				else
				{
					return
						error::Error
						{
							"found keyword is not a binary operator",
							error::Code::wrong_keyword_operator
						};
				}
			}
			else
			{
				return error::none;
			}
		}
	}

	[[nodiscard]] auto parse_expression(language::TokenIterator iterator) noexcept
		-> error::Result<language::SyntaxTree>
	{
		auto expression_result = parse_primary_expression(iterator);
		VECTOR_ASSERT_RESULT(expression_result);
		auto& expression = expression_result.value();

		VECTOR_ASSERT_RESULT(parse_binary_expression(iterator, expression));

		return expression;
	}
}