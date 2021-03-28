#pragma once

#include "../language.hpp"
#include "../../utility/errors/result.hpp"
#include "patterns.hpp"
#include "statements.hpp"


namespace vector::parsing
{
    [[nodiscard]] auto parse_parameter(language::TokenIterator iterator) noexcept
        -> error::Result<language::Parameter>
    {
        VECTOR_ASSERT
        (
            iterator->type == language::Token::Type::identifier,
            (error::Error {"expected parameter identifier", error::Code::missing_parameter_identifier})
        );
        const auto& identifier = std::get<std::string>(iterator++->value);

        VECTOR_ASSERT
        (
            iterator->is(language::Sign::colon),
            (error::Error {"expected parameter typing colon", error::Code::missing_parameter_colon})
        );
        
        ++iterator;

        VECTOR_ASSERT
        (
            iterator->type == language::Token::Type::identifier,
            (error::Error {"expected parameter type", error::Code::missing_parameter_type})
        );

        return
            language::Parameter
            {
                identifier, std::get<std::string>(iterator++->value)
            };
    }
    [[nodiscard]] auto parse_return_type(language::TokenIterator iterator) noexcept
        -> error::Result<std::string>
    {
        if (!iterator->is(language::Sign::arrow, language::Token::Type::operator_symbol))
        {
            return std::string {};
        }
        ++iterator;

        VECTOR_ASSERT
        (
            iterator->type == language::Token::Type::identifier,
            (error::Error {"expected return type", error::Code::missing_function_return_type})
        );

        return std::get<std::string>(iterator++->value);
    }

    [[nodiscard]] auto parse_function_signature(language::TokenIterator iterator) noexcept
        -> error::Result<language::FunctionSignature>
    {
        const auto& identifier = std::get<std::string>(iterator->value);
        iterator += 2;
        
        auto parameters = decltype(language::FunctionSignature::parameters) {};
        const auto handle_parameter =
            [&iterator, &parameters]() noexcept -> error::Result<void>
            {
                const auto parameter_result = parse_parameter(iterator);
                VECTOR_ASSERT_RESULT(parameter_result);
                const auto& parameter = parameter_result.value();

                parameters.push_back(parameter);
                return error::none;
            };

        // parameters
        const auto parameter_parsing_result = iterate_seperated_list(iterator, handle_parameter);
        VECTOR_ASSERT_RESULT(parameter_parsing_result);

        const auto return_type_result = parse_return_type(iterator);
        VECTOR_ASSERT_RESULT(return_type_result);
        const auto return_type = return_type_result.value();

        return language::FunctionSignature {parameters, identifier, return_type};
    }
    [[nodiscard]] auto parse_function(language::TokenIterator iterator) noexcept
        -> error::Result<language::SyntaxTree>
    {
        const auto signature_result = parse_function_signature(iterator);
        VECTOR_ASSERT_RESULT(signature_result);
        const auto signature = signature_result.value();

        // if body is given -> function definition
        if (iterator->is(language::Sign::opening_brace))
        {
            ++iterator;
            const auto body_result = parse_block(iterator);
            VECTOR_ASSERT_RESULT(body_result);
            const auto& body = body_result.value();

            return language::SyntaxTree
            {
                language::SyntaxTree::Type::function_definition,
                language::FunctionDefinition {signature, body}
            };
        }
        // function decleration
        else if (iterator->is(language::Sign::semicolon))
        {
            ++iterator;
            return language::SyntaxTree
            {
                language::SyntaxTree::Type::function_decleration,
                language::FunctionSignature {signature}
            };
        }
        // error: not closed with either a brace (definition) or semicolon (decleration)
        else
        {
            return error::Error {"expected braces or semicolon", error::Code::unterminated_function};
        }
    }
}