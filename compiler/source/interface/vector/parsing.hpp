#pragma once

#include <vector>
#include <optional>

#include "../utility/errors/error.hpp"
#include "language.hpp"
#include "lexing.hpp"
#include "parsing/patterns.hpp"
#include "parsing/statements.hpp"
#include "parsing/expressions.hpp"
#include "parsing/functions.hpp"


namespace vector::parsing
{
    [[nodiscard]] auto parse_import(language::TokenIterator iterator) noexcept
        -> error::Result<language::SyntaxTree>
    {
        const auto module_identifier_result = parse_expression(iterator);
        VECTOR_ASSERT_RESULT(module_identifier_result);
        const auto module_identifier = module_identifier_result.value();

        VECTOR_ASSERT
        (
            iterator->is(language::Sign::semicolon),
            (error::Error {"expected semicolon", error::Code::missing_semicolon})
        );

        return
            language::SyntaxTree
            {
                language::SyntaxTree::Type::import_statement,
                utility::SmartPointer
                {
                    std::make_unique<language::SyntaxTree>(module_identifier)
                }
            };
    }
    [[nodiscard]] auto parse_top_level(language::TokenIterator iterator) noexcept
        -> error::Result<language::SyntaxTree>
    {
        VECTOR_ASSERT
        (
            iterator->type == language::Token::Type::keyword,
            (error::Error {"unkown top level token", error::Code::expected_top_level_keyword})
        );

        const auto& keyword = std::get<std::string>(iterator->value);
        if (keyword == "import")
        {
            return parse_import(++iterator).value();
        }
        else if (keyword == "function")
        {
            return parse_function(++iterator);
        }
        else
        {
            std::abort();
        }
    }


    // generate syntax from tokens returned from "lexing::lex"
    // returns optional with value on success and vice versa
    [[nodiscard]] auto parse(const language::Tokens& tokens) noexcept
        -> error::Result<language::SyntaxForest>
    {
        auto forest = language::SyntaxForest {};

        auto iterator = tokens.cbegin();
        while (iterator < tokens.cend())
        {
            const auto top_level_result = parse_top_level(iterator);
            VECTOR_ASSERT_RESULT(top_level_result);
            const auto& top_level = top_level_result.value();

            forest.push_back(std::move(top_level));
        }

        return forest;
    }
}