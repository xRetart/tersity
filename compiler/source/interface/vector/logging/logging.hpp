#pragma once

#include <iostream>
#include <algorithm>
#include <variant>
#include "language.hpp"


namespace vector::logging
{
    [[nodiscard]] auto symbolize(const language::Sign symbol) noexcept -> std::string
    {
        switch (symbol)
        {
        // operators
        case language::Sign::plus: return "+";
        case language::Sign::minus: return "-";
        case language::Sign::astrisk: return "*";
        case language::Sign::slash: return "/";
        case language::Sign::percent: return "%";
        case language::Sign::double_astrisk: return "**";
        case language::Sign::as: return "as";
        case language::Sign::equals: return "=";
        case language::Sign::dot: return ".";
        case language::Sign::arrow: return "->";
        case language::Sign::ampersand: return "&";
        case language::Sign::pipe: return "|";
        case language::Sign::power: return "^";
        case language::Sign::tilde: return "~";
        case language::Sign::exclamation: return "!";
        case language::Sign::double_equals: return "==";
        case language::Sign::exclamation_equals: return "!=";
        case language::Sign::less_than: return "<";
        case language::Sign::less_equals: return "<=";
        case language::Sign::greater_than: return ">";
        case language::Sign::greater_equals: return ">=";
        case language::Sign::double_ampersand: return "&&";
        case language::Sign::double_pipe: return "||";

        // seperators
        case language::Sign::comma: return ",";
        case language::Sign::semicolon: return ";";
        case language::Sign::colon: return ":";
        case language::Sign::double_colon: return "::";
        case language::Sign::quote: return "\"";
        case language::Sign::single_quote: return "\'";
        case language::Sign::opening_parenthese: return "(";
        case language::Sign::closing_parenthese: return ")";
        case language::Sign::opening_bracket: return "[";
        case language::Sign::closing_bracket: return "]";
        case language::Sign::opening_brace: return "{";
        case language::Sign::closing_brace: return "}";
        }
    }
    [[nodiscard]] auto symbolize(const language::BinaryExpression::Operation operation) noexcept
        -> std::string
    {
        switch (operation)
        {
            case language::BinaryExpression::Operation::addition: return "+";
            case language::BinaryExpression::Operation::subtraction: return "-";
            case language::BinaryExpression::Operation::multiplication: return "*";
            case language::BinaryExpression::Operation::division: return "/";
            case language::BinaryExpression::Operation::modulo : return "%";
            case language::BinaryExpression::Operation::exponentiation: return "**";
            case language::BinaryExpression::Operation::cast: return "as";
            case language::BinaryExpression::Operation::assign: return "=";
            case language::BinaryExpression::Operation::access_instance: return ".";
            case language::BinaryExpression::Operation::access_pointer: return "->";
            case language::BinaryExpression::Operation::access_space: return "::";
            case language::BinaryExpression::Operation::bitwise_or: return "|";
            case language::BinaryExpression::Operation::bitwise_and: return "&";
            case language::BinaryExpression::Operation::bitwise_xor: return "^";
            case language::BinaryExpression::Operation::equal: return "==";
            case language::BinaryExpression::Operation::not_equal: return "!=";
            case language::BinaryExpression::Operation::less_than: return "<";
            case language::BinaryExpression::Operation::less_equal: return "<=";
            case language::BinaryExpression::Operation::greater_than: return ">";
            case language::BinaryExpression::Operation::greater_equal: return ">=";
            case language::BinaryExpression::Operation::logical_or: return "||";
            case language::BinaryExpression::Operation::logical_and: return "**";

            // dbg
            UNLIKELY default: std::abort();
        }
    }

    auto operator<<(std::ostream& stream, language::Sign operator_symbol) noexcept -> std::ostream&
    {
        return stream << symbolize(operator_symbol);
    }


    [[nodiscard]] auto generate_indentation(const std::size_t level) noexcept -> std::string
    {
        auto indentation = std::string {};
        indentation.reserve(level);
        std::fill_n(indentation, level, '\t');

        return indentation;
    }


    // lexing logging
    auto log_tokens(const std::vector<language::Token>& tokens) noexcept -> void
    {
        for (const auto& token : tokens)
        {
            const auto value {token.value};

            std::cerr << "type = " << static_cast<unsigned short>(token.type) << ", value = ";
            switch (value.index())
            {
                case 0: [[fallthrough]];
                case 1: [[fallthrough]];
                case 2: [[fallthrough]];
                case 3:
                    std::visit([](const auto value) noexcept -> void {std::cerr << value;}, value);
                    break;
                case 4:
                    std::cerr << symbolize(std::get<language::Sign>(value));
                    break;
                UNLIKELY default:
                    std::cerr << "debug error: unrecognised token variant index\n";
                    std::abort();
            }
            std::cerr << "\"\n";
        }
    }
    auto log_tree(const language::SyntaxForest grammar, const std::size_t indent_level) noexcept
        -> void;


    // parsing logging
    auto log_node(const language::SyntaxTree& node, const std::size_t indent_level) noexcept -> void;


    auto log_whole_literal_expression
    (
        const unsigned long long value,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "numeric literal expression {\n" <<
            indentation << "\tvalue = " << value << '\n' <<
            indentation << "}\n";
    }
    auto log_fraction_literal_expression
    (
        const double value,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "numeric literal expression {\n" <<
            indentation << "\tvalue = " << value << '\n' <<
            indentation << "}\n";
    }
    auto log_string_literal_expression
    (
        const language::Source content,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "string literal expression {\n" <<
            indentation << "\tcontent = " << content << '\n' <<
            indentation << "}\n";
    }
    auto log_char_literal_expression
    (
        const char value,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "character literal expression {\n" <<
            indentation << "\tvalue = " << value << '\n' <<
            indentation << "}\n";
    }
    auto log_variable_expression
    (
        const language::Source identifier,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "variable expression node {\n" <<
            indentation << "\tidentifier = " << identifier << '\n' <<
            indentation << "}\n";
    }
    auto log_type_expression
    (
        const language::Source identifier,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "type expression node {\n" <<
            indentation << "\tidentifier = " << identifier << '\n' <<
            indentation << "}\n";
    }
    auto log_binary_expression
    (
        const language::BinaryExpression& expression,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "binary expression node {\n" <<
            indentation << "\toperation = " <<
                symbolize(expression.operation) << ",\n" <<
            indentation << "\toperand0 = {\n"; 
            log_node(*expression.left.pointer, indentation.size() + 2);
            
            std::cerr <<
                indentation << "\t},\n" <<
                indentation << "\toperand1 = {\n";
            log_node(*expression.right.pointer, indentation.size() + 2);

            std::cerr <<
                indentation << "\t}\n" <<
                indentation << "}\n";
    }
    auto log_call_expression
    (
        const language::CallExpression& expression,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "call expression node {\n" <<
            indentation << "\tidentifier = " << expression.callee << ",\n" <<
            indentation << "\tparameters = {\n";

        log_tree(expression.parameters, indentation.size() + 2);

        std::cerr << indentation << "\t}\n" << indentation << "}\n";
    }
    auto log_import_statement
    (
        const utility::SmartPointer<language::SyntaxTree> statement,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "import statement node {\n" <<
            indentation << "\tidentifier = {\n";

        log_node(*statement.pointer, indentation.size() + 2);

        std::cerr << indentation << "}\n";
    }
    auto log_variable_decleration
    (
        const language::Source& name,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "variable declaration node {\n" <<
            indentation << "\tsignature = {\n";

        std::cerr <<
            indentation << "\t}\n" <<
            indentation << "}\n";
    }
    auto log_function_signature
    (
        const language::FunctionSignature& signature,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "function signature node {\n" <<
            indentation << "\tidentifier = " << signature.identifier << '\n' <<
            indentation << "\tparameters = {\n";
        
        std::string next_indentation {generate_indentation(indentation.size() + 2)};
        for (const auto& parameter : signature.parameters)
        {
            log_parameter(parameter, next_indentation);
        }

        std::cerr <<
            indentation << "\t}\n" <<
            indentation << "\treturn type identifier = " << signature.return_type_identifier <<
                '\n' <<
            indentation << "}\n";
    }
    auto log_function_definition
    (
        const language::FunctionDefinition& definition,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "function definition node {\n" <<
            indentation << "\tsignature = {\n";
        log_node
        (
            {language::SyntaxTree::Type::function_decleration, definition.signature},
            indentation.size() + 2
        );
        
        std::cerr << indentation << "\t}\n\tbody = {\n";
        log_tree(definition.body, indentation.size() + 2);
        std::cerr << indentation << "\t}\n";
        
        std::cerr << indentation << "}\n";
    }
    auto log_return_statement
    (
        const utility::SmartPointer<language::SyntaxTree>& returned,
        const language::Source indentation
    )
        noexcept
        -> void
    {
        std::cerr <<
            indentation << "return statement node {\n";

        log_node(*returned.pointer, indentation.size() + 1);

        std::cerr <<
            indentation << "}\n";
    }


    auto log_node(const language::SyntaxTree& node, const std::size_t indent_level) noexcept -> void
    {
        const std::string indentation {generate_indentation(indent_level)};

        switch (node.type)
        {
            case language::SyntaxTree::Type::whole_literal_expression:
            {
                log_whole_literal_expression(std::get<unsigned long long>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::fraction_literal_expression:
            {
                log_fraction_literal_expression(std::get<double>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::string_literal_expression:
            {
                log_string_literal_expression(std::get<language::Source>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::character_literal_expression:
            {
                log_char_literal_expression(std::get<char>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::variable_expression:
            {
                log_variable_expression(std::get<language::Source>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::type_expression:
            {
                log_type_expression(std::get<language::Source>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::binary_expression:
            {
                log_binary_expression(std::get<language::BinaryExpression>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::call_expression:
            {
                log_call_expression(std::get<language::CallExpression>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::import_statement:
            {
                log_import_statement(std::get<utility::SmartPointer<language::SyntaxTree>>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::variable_decleration:
            {
                log_variable_signature(std::get<language::VariableSignature>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::variable_definition:
            {
                log_variable_definition(std::get<language::VariableDefinition>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::function_decleration:
            {
                log_function_signature(std::get<language::FunctionSignature>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::function_definition:
            {
                log_function_definition(std::get<language::FunctionDefinition>(node.data), indentation);
                break;
            }
            case language::SyntaxTree::Type::return_statement:
            {
                log_return_statement(std::get<utility::SmartPointer<language::SyntaxTree>>(node.data), indentation);
                break;
            }
            UNLIKELY default:
            {
                std::fputs("unspecified node type\n", stderr);
                std::abort();
            }
        }
    }
    auto log_tree(const language::SyntaxForest grammar, const std::size_t indent_level = 0) noexcept
        -> void
    {
        for (const auto& node : grammar)
        {
            log_node(node, indent_level);
        }
    }
}