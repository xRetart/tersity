#pragma once

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "language.hpp"


namespace vector::generation
{
    using Identifier = std::string_view;

    struct Llvm
    {
        static constexpr auto global_module_name = "main";

        using Value = llvm::Value*;
        using Type = llvm::Type*;
        using Function = llvm::Function*;

        using Builder = llvm::IRBuilder<>;
        using Symbols = std::unordered_map<Identifier, Value>;
        using Context = llvm::LLVMContext;
        using GlobalModule = llvm::Module;

        Builder builder {context};
        Symbols symbols {};
        Context context {};
        GlobalModule global_module {global_module_name, context};

        NODISCARD_CONSTRUCTOR Llvm() noexcept
        {
            auto type = llvm::FunctionType::get(builder.getInt8Ty(), {}, false);
            llvm::Function::Create(type, llvm::Function::ExternalLinkage, "main", global_module);
        }
    };
    
    [[nodiscard]] auto is_integer_type(const Identifier identifier) noexcept -> bool
    {
        return
            identifier == "Byte" ||
            identifier == "Integer8" ||
            identifier == "Integer16" ||
            identifier == "Integer32" ||
            identifier == "Integer64" ||
            identifier == "Integer";
    }
    [[nodiscard]] auto identifier_to_type(const Identifier identifier, Llvm::Context& context)
        noexcept
        -> Llvm::Type
    {
        if (identifier == "Integer")
        {
            return llvm::Type::getInt64Ty(context);
        }
        else if (identifier == "Real")
        {
            return llvm::Type::getDoubleTy(context);
        }
        else if (identifier == "Byte")
        {
            return llvm::Type::getInt8Ty(context);
        }
        else if (identifier.empty())
        {
            return llvm::Type::getVoidTy(context);
        }
        else
        {
            return nullptr;
        }
    }
    
    // [expression]==================================================================================
    [[nodiscard]] auto generate_expression
    (
        Llvm& state,
        const language::SyntaxTree& expression
    )
        noexcept
        -> Llvm::Value;


    [[nodiscard]] auto generate_fraction_literal_expression
    (
        Llvm::Context& context,
        const language::SyntaxTree& value
    )
        noexcept ->
        llvm::ConstantFP*
    {
        return llvm::ConstantFP::get(context, llvm::APFloat {std::get<double>(value.data)});
    }
    [[nodiscard]] auto generate_whole_literal_expression
    (
        Llvm::Context& context,
        const language::SyntaxTree& value
    )
        noexcept
        -> llvm::ConstantInt*
    {
        return
            llvm::ConstantInt::get(context, llvm::APInt {64, std::get<unsigned long long>(value.data)});
    }
    [[nodiscard]] auto generate_variable_expression
    (
        Llvm::Symbols& symbols,
        const language::SyntaxTree& variable
    )
        noexcept
        -> Llvm::Value
    {
        return symbols[std::get<std::string_view>(variable.data)];
    }


    [[nodiscard]] auto generate_cast
    (
        Llvm& state,
        const language::BinaryExpression& expression
    )
        noexcept
        -> Llvm::Value
    {
        const auto left = generate_expression(state, *expression.left.pointer);
        const auto& right = std::get<std::string_view>(expression.right.pointer->data);

        if (is_integer_type(right))
        {
            return state.builder.CreateIntCast(left, identifier_to_type(right, state.context), false);
        }
        else
        {
            return {};
        }
    }
    [[nodiscard]] auto generate_addition
    (
        Llvm::Builder& builder,
        Llvm::Value left_summand,
        Llvm::Value right_summand
    )
        noexcept
        -> Llvm::Value
    {
        const auto left_type {left_summand->getType()};
        if (left_type == builder.getInt64Ty())
        {
            return builder.CreateAdd(left_summand, right_summand);
        }
        else if (left_type == builder.getDoubleTy())
        {
            return builder.CreateFAdd(left_summand, right_summand);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_subraction
    (
        Llvm::Builder& builder,
        Llvm::Value minuend,
        Llvm::Value subtrahend
    )
        noexcept
        -> Llvm::Value
    {
        const auto minuend_type {minuend->getType()};
        if (minuend_type == builder.getInt64Ty())
        {
            return builder.CreateSub(minuend, subtrahend);
        }
        else if (minuend_type == builder.getDoubleTy())
        {
            return builder.CreateFSub(minuend, subtrahend);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_multiplication
    (
        Llvm::Builder& builder,
        Llvm::Value multiplicand,
        Llvm::Value multiplicator
    )
        noexcept
        -> Llvm::Value
    {
        const auto left_type {multiplicand->getType()};
        if (left_type == builder.getInt64Ty())
        {
            return builder.CreateMul(multiplicand, multiplicator);
        }
        else if (left_type == builder.getDoubleTy())
        {
            return builder.CreateFMul(multiplicand, multiplicator);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_equal
    (
        Llvm::Builder& builder,
        Llvm::Value compared,
        Llvm::Value comparer
    )
        noexcept
        -> Llvm::Value
    {
        const auto dividend_type = compared->getType();

        if (dividend_type == builder.getInt64Ty())
        {
            return builder.CreateICmpEQ(compared, comparer);
        }
        else if (dividend_type == builder.getDoubleTy())
        {
            return builder.CreateFCmpOEQ(compared, comparer);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_not_equal
    (
        Llvm::Builder& builder,
        Llvm::Value compared,
        Llvm::Value comparer
    )
        noexcept
        -> Llvm::Value
    {
        const auto dividend_type = compared->getType();

        if (dividend_type == builder.getInt64Ty())
        {
            return builder.CreateICmpNE(compared, comparer);
        }
        else if (dividend_type == builder.getDoubleTy())
        {
            return builder.CreateFCmpONE(compared, comparer);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_greater_than
    (
        Llvm::Builder& builder,
        Llvm::Value compared,
        Llvm::Value comparer
    )
        noexcept
        -> Llvm::Value
    {
        const auto dividend_type = compared->getType();

        if (dividend_type == builder.getInt64Ty())
        {
            return builder.CreateICmpSGT(compared, comparer);
        }
        else if (dividend_type == builder.getDoubleTy())
        {
            return builder.CreateFCmpOGT(compared, comparer);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_greater_equal
    (
        Llvm::Builder& builder,
        Llvm::Value compared,
        Llvm::Value comparer
    )
        noexcept
        -> Llvm::Value
    {
        const auto dividend_type = compared->getType();

        if (dividend_type == builder.getInt64Ty())
        {
            return builder.CreateICmpSGE(compared, comparer);
        }
        else if (dividend_type == builder.getDoubleTy())
        {
            return builder.CreateFCmpOGE(compared, comparer);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_less_than
    (
        Llvm::Builder& builder,
        Llvm::Value compared,
        Llvm::Value comparer
    )
        noexcept
        -> Llvm::Value
    {
        const auto dividend_type = compared->getType();

        if (dividend_type == builder.getInt64Ty())
        {
            return builder.CreateICmpSLT(compared, comparer);
        }
        else if (dividend_type == builder.getDoubleTy())
        {
            return builder.CreateFCmpOLT(compared, comparer);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_less_equal
    (
        Llvm::Builder& builder,
        Llvm::Value compared,
        Llvm::Value comparer
    )
        noexcept
        -> Llvm::Value
    {
        const auto dividend_type = compared->getType();

        if (dividend_type == builder.getInt64Ty())
        {
            return builder.CreateICmpSLE(compared, comparer);
        }
        else if (dividend_type == builder.getDoubleTy())
        {
            return builder.CreateFCmpOLE(compared, comparer);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    [[nodiscard]] auto generate_division
    (
        Llvm::Builder& builder,
        Llvm::Value dividend,
        Llvm::Value divisor
    )
        noexcept
        -> Llvm::Value
    {
        const auto dividend_type {dividend->getType()};
        if (dividend_type == builder.getInt64Ty())
        {
            return builder.CreateSDiv(dividend, divisor);
        }
        else if (dividend_type == builder.getDoubleTy())
        {
            return builder.CreateFDiv(dividend, divisor);
        }
        // dbg
        else
        {
            std::abort();
        } 
    }
    // TODO: add all operators
    [[nodiscard]] auto generate_binary_expression
    (
        Llvm& state,
        const language::SyntaxTree& tree
    )
        noexcept
        -> Llvm::Value
    {
        const auto& expression = std::get<language::BinaryExpression>(tree.data);

        // right isn't expression
        if (expression.operation == language::BinaryExpression::Operation::cast)
        {
            return generate_cast(state, expression);
        }

        const auto& left =
            generate_expression(state, *expression.left.pointer);
        const auto& right =
            generate_expression(state, *expression.right.pointer);

        switch (expression.operation)
        {
        case language::BinaryExpression::Operation::addition:
            return generate_addition(state.builder, left, right);
        case language::BinaryExpression::Operation::subtraction:
            return generate_subraction(state.builder, left, right);
        case language::BinaryExpression::Operation::multiplication:
            return generate_multiplication(state.builder, left, right);
        case language::BinaryExpression::Operation::division:
            return generate_division(state.builder, left, right);
        case language::BinaryExpression::Operation::equal:
            return generate_equal(state.builder, left, right);
        case language::BinaryExpression::Operation::not_equal:
            return generate_not_equal(state.builder, left, right);
        case language::BinaryExpression::Operation::greater_than:
            return generate_greater_than(state.builder, left, right);
        case language::BinaryExpression::Operation::greater_equal:
            return generate_greater_equal(state.builder, left, right);
        case language::BinaryExpression::Operation::less_than:
            return generate_less_than(state.builder, left, right);
        case language::BinaryExpression::Operation::less_equal:
            return generate_less_equal(state.builder, left, right);
        
        // dbg
        UNLIKELY default:
            std::abort();
        }
    }

    [[nodiscard]] auto generate_call_expression
    (
        Llvm& state,
        const language::SyntaxTree& expression
    )
        noexcept
        -> Llvm::Value
    {
        const auto call_expression = std::get<language::CallExpression>(expression.data);

        const auto callee_value = state.global_module.getFunction(call_expression.callee);
        if
        (
            callee_value == nullptr ||
            callee_value->arg_size() != call_expression.parameters.size()
        )
        UNLIKELY
        {
            return nullptr;
        }

        auto parameter_values = std::vector<Llvm::Value> {call_expression.parameters.size()};
        auto parameter_iterator = call_expression.parameters.cbegin();

        // generate_n because it works with 0-sized vectors aswell
        std::generate_n
        (
            parameter_values.begin(),
            parameter_values.size(),
            [&state, &parameter_iterator]() noexcept
            {
                return generate_expression(state, *parameter_iterator++);
            }
        );

        return state.builder.CreateCall(callee_value, parameter_values);
    }

    [[nodiscard]] auto generate_expression
    (
        Llvm& state,
        const language::SyntaxTree& expression
    )
        noexcept
        -> Llvm::Value
    {
        switch (expression.type)
        {
        case language::SyntaxTree::Type::whole_literal_expression:
            return generate_whole_literal_expression(state.context, expression);
        case language::SyntaxTree::Type::fraction_literal_expression:
            return generate_fraction_literal_expression(state.context, expression);
        case language::SyntaxTree::Type::variable_expression:
            return generate_variable_expression(state.symbols, expression);
        case language::SyntaxTree::Type::call_expression:
            return generate_call_expression(state, expression);
        case language::SyntaxTree::Type::binary_expression:
            return generate_binary_expression(state, expression);

        // dbg
        default:
            std::abort();
        }
    }

    // [functions]===================================================================================
    [[nodiscard]] auto generate_function_decleration
    (
        Llvm::Context& context,
        llvm::Module& global_module,
        const language::SyntaxTree tree
    )
        noexcept
        -> Llvm::Function
    {
        const auto signature = std::get<language::FunctionSignature>(tree.data);

        // return type deduction
        const auto return_type = identifier_to_type(signature.return_type_identifier, context);
        if (return_type == nullptr) UNLIKELY
        {
            return nullptr;
        }

        auto parameter_types = std::vector<Llvm::Type> {signature.parameters.size()};
        auto code_parameter_iterator = parameter_types.begin();
        auto grammar_parameter_iterator = signature.parameters.cbegin();
        while (grammar_parameter_iterator != signature.parameters.end())
        {
            *code_parameter_iterator++ =
                identifier_to_type(grammar_parameter_iterator++->type_identifier, context);
        }

        const auto llvm_signature = llvm::FunctionType::get(return_type, parameter_types, false);
        if (llvm_signature == nullptr) UNLIKELY
        {
            return {};
        }

        const auto function_value =
            llvm::Function::Create
            (
                llvm_signature,
                llvm::Function::ExternalLinkage,
                signature.identifier.data(),
                global_module
            );

        auto parameter_iterator = signature.parameters.begin();
        const auto& parameters = function_value->args();
        for (auto& parameter : parameters)
        {
            const auto is_current_parameter =
                [&parameter_iterator](const llvm::Argument& parameter)
                {
                    const auto& parameter_name = parameter.getName();

                    return
                        std::string_view {parameter_name.begin(), parameter_name.size()} ==
                        parameter_iterator->identifier;
                };
            const auto& last_iterator = parameters.end() - 1;

            if (std::find_if(parameters.begin(), last_iterator, is_current_parameter) != last_iterator)
                UNLIKELY
            {
                return {};
            }
            parameter.setName(parameter_iterator++->identifier.data());
        }

        return function_value;
    }

    [[nodiscard]] auto generate_block
    (
        Llvm& state,
        const language::SyntaxForest& block
    )
        noexcept
        -> bool;
    
    [[nodiscard]] auto generate_return_statement
    (
        Llvm& state,
        const language::SyntaxTree& statement
    )
        noexcept
        -> bool
    {
        return
            state.builder.CreateRet
            (
                generate_expression
                (
                    state,
                    *std::get<utility::SmartPointer<language::SyntaxTree>>(statement.data)
                    .pointer
                )
            )
            != nullptr;
    }
    [[nodiscard]] auto generate_variable_definition
    (
        Llvm& state,
        const language::SyntaxTree& statement
    )
        noexcept
        -> bool
    {
        const auto definition = std::get<language::VariableDefinition>(statement.data);
        const auto llvm_value = generate_expression(state, *definition.initializer.pointer);

        if (llvm_value == nullptr) UNLIKELY
        {
            return {};
        }

        state.symbols.emplace(definition.identifier, llvm_value);

        return true;
    }
    [[nodiscard]] auto generate_if_statement
    (
        Llvm& state,
        const language::SyntaxTree& statement
    )
        noexcept
        -> bool
    {
        const auto branch = std::get<language::IfStatement>(statement.data);
        const auto llvm_condition = generate_expression(state, *branch.condition.pointer);
        const auto parent = state.builder.GetInsertBlock()->getParent();

        // create llvm blocks
        // true (executed if condition is true)
        auto llvm_true_block = llvm::BasicBlock::Create(state.context, "then", parent);
        if (llvm_true_block == nullptr) UNLIKELY
        {
            return {};
        }
        
        // ... is false
        auto llvm_false_block = llvm::BasicBlock::Create(state.context, "else", parent);
        if (llvm_true_block == nullptr) UNLIKELY
        {
            return {};
        } 

        state.builder.CreateCondBr(llvm_condition, llvm_true_block, llvm_false_block);


        // generate true block
        state.builder.SetInsertPoint(llvm_true_block);
        const auto true_block_succeeded = generate_block(state, branch.true_block);
        if (!true_block_succeeded) UNLIKELY
        {
            return {};
        }

        
        // false
        // parent->getBasicBlockList().push_back(llvm_false_block);
        state.builder.SetInsertPoint(llvm_false_block);
        const auto false_block_succeeded = generate_block(state, branch.false_block);
        if (!false_block_succeeded) UNLIKELY
        {
            return {};
        }

        return true;
    }
    [[nodiscard]] auto generate_block
    (
        Llvm& state,
        const language::SyntaxForest& block
    )
        noexcept
        -> bool
    {
        for (const auto& statement : block)
        {
            switch (statement.type)
            {
                case language::SyntaxTree::Type::return_statement:
                    if (!generate_return_statement(state, statement)) UNLIKELY
                    {
                        return {};
                    }
                    break;
                case language::SyntaxTree::Type::variable_definition:
                {
                    if (!generate_variable_definition(state, statement)) UNLIKELY
                    {
                        return {};
                    }
                    break;
                }
                case language::SyntaxTree::Type::if_statement:
                    if (!generate_if_statement(state, statement)) UNLIKELY
                    {
                        return {};
                    }
                    break;
                case language::SyntaxTree::Type::call_expression:
                    if (generate_call_expression(state, statement) == nullptr) UNLIKELY
                    {
                        return {};
                    }
                    break;
                // dbg
                default:
                {
                    std::fputs("unknown body statement", stderr);
                    std::abort();
                    break;
                }
            }
        }

        return true;
    }
    // TODO: include check for sameness if signature exists
    [[nodiscard]] auto generate_function_definition
    (
        Llvm& state,
        const language::SyntaxTree& tree
    )
        noexcept
        -> bool
    {
        const auto definition = std::get<language::FunctionDefinition>(tree.data);

        auto function = state.global_module.getFunction(definition.signature.identifier);
        if (function == nullptr)
        {
            function =
                generate_function_decleration
                (
                    state.context, state.global_module,
                    language::SyntaxTree
                    {
                        language::SyntaxTree::Type::function_decleration,
                        definition.signature
                    }
                );

            if (function == nullptr) UNLIKELY
            {
                return {};
            }
        }
        else if
        (
            function->getReturnType() !=
            identifier_to_type(definition.signature.return_type_identifier, state.context)
        )
            UNLIKELY
        {
            return {};
        }

        if (!function->empty()) UNLIKELY
        {
            return {};
        }

        auto insertion_point = llvm::BasicBlock::Create(state.context, "entry", function);
        state.builder.SetInsertPoint(insertion_point);

        state.symbols.clear();
        for (auto& parameter : function->args())
        {
            state.symbols[parameter.getName()] = &parameter;
        }

        const auto success = generate_block(state, definition.body);
        if (!success) UNLIKELY
        {
            return {};
        }

        if (function->getReturnType() == state.builder.getVoidTy())
        {
            state.builder.CreateRetVoid();
        }

        const auto function_is_invalid = llvm::verifyFunction(*function, &llvm::errs());
        if (function_is_invalid) UNLIKELY
        {
            return {};
        }

        return true;
    }

    // TODO: maybe remove global_module allocation
    // entry point for code (LLVMIR) generation grammar
    [[nodiscard]] auto generate(const language::SyntaxForest& grammar, Llvm& state)
        noexcept
        -> bool
    {
        for (const auto& tree : grammar)
        {
            switch (tree.type)
            {
            case language::SyntaxTree::Type::function_decleration:
            {
                const auto succeeded =
                    generate_function_decleration(state.context, state.global_module, tree);
                if (!succeeded) UNLIKELY
                {
                    return {};
                }
                break;
            }
            case language::SyntaxTree::Type::function_definition:
            {
                const auto succeeded = generate_function_definition(state, tree);
                if (!succeeded) UNLIKELY
                {
                    return {};
                }
                break;
            }

            // dbg
            default:
                std::abort();
            }
        }

        return true;
    }
}