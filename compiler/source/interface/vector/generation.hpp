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
		using Constants = std::unordered_map<Identifier, Value>;
		using Mutables = std::unordered_map<Identifier, llvm::AllocaInst*>;
		using Symbols = std::pair<Constants, Mutables>;
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
			identifier == "Character" ||
			identifier == "Integer8" ||
			identifier == "Integer16" ||
			identifier == "Integer32" ||
			identifier == "Integer64" ||
			identifier == "String" ||
			identifier == "Integer";
	}
	[[nodiscard]] auto identifier_to_type(const Identifier identifier, Llvm::Context& context) noexcept
		-> language::DataType
	{
		if (identifier == "Natural")
		{
			return {llvm::Type::getInt64Ty(context), false};
		}
		if (identifier == "Integer")
		{
			return {llvm::Type::getInt64Ty(context), true};
		}
		else if (identifier == "Real")
		{
			return {llvm::Type::getDoubleTy(context), false};
		}
		else if (identifier == "Byte")
		{
			return {llvm::Type::getInt8Ty(context), false};
		}
		else if (identifier == "Character")
		{
			return {llvm::Type::getInt8Ty(context), false};
		}
		else if (identifier == "String")
		{
			return {llvm::Type::getInt8PtrTy(context), false};
		}
		else if (identifier == "SByte")
		{
			return {llvm::Type::getInt8Ty(context), true};
		}
		else if (identifier.empty())
		{
			return {llvm::Type::getVoidTy(context), false};
		}
		else
		{
			return {nullptr, false};
		}
	}
	[[nodiscard]] constexpr auto literal_type_size(const Identifier identifier) noexcept -> unsigned int
	{
		if (identifier == "n")
		{
			return 64;
		}
		else if (identifier == "i")
		{
			return 64;
		}
		else if (identifier == "r")
		{
			return 64;
		}
		else if (identifier == "c")
		{
			return 8;
		}
		else if (identifier == "b")
		{
			return 8;
		}
		
		// DBG
		else
		{
			return 0;
		}
	}
	
	// [expression]==================================================================================
	[[nodiscard]] auto generate_expression
		(Llvm& state, const language::SyntaxTree& expression) noexcept -> Llvm::Value;


	[[nodiscard]] auto generate_whole_literal_expression
		(Llvm::Context& context, const language::SyntaxTree& value) noexcept -> llvm::ConstantInt*
	{
		const auto literal = std::get<language::WholeLiteralExpression>(value.data);
		return
			llvm::ConstantInt::get
				(context, llvm::APInt {literal_type_size(literal.type), literal.value});
	}
	[[nodiscard]] auto generate_fraction_literal_expression
		(Llvm::Context& context, const language::SyntaxTree& value) noexcept -> llvm::ConstantFP*
	{
		return
			llvm::ConstantFP::get
			(
				context,
				llvm::APFloat {std::get<language::FractionLiteralExpression>(value.data).value}
			);
	}
	[[nodiscard]] auto generate_character_literal_expression
		(Llvm::Context& context, const language::SyntaxTree& value) noexcept -> llvm::ConstantInt*
	{
		const auto literal = std::get<language::CharacterLiteralExpression>(value.data);
		return llvm::ConstantInt::get(context, llvm::APInt {8, static_cast<uint64_t>(literal.value)});
	}
	[[nodiscard]] auto generate_string_literal_expression
		(Llvm& state, const language::SyntaxTree& value) noexcept -> llvm::Value*
	{
		const auto literal = std::get<language::StringLiteralExpression>(value.data);
		
		auto llvm_constants = std::vector<llvm::Constant*> {literal.value.size()};

		auto llvm_constants_iterator = llvm_constants.begin();
		for (const auto& character : literal.value)
		{
			*llvm_constants_iterator++ = state.builder.getInt8(character);
		}

		return
			llvm::ConstantArray::get
				(llvm::ArrayType::get(state.builder.getInt8Ty(), literal.value.size()), llvm_constants);

	}
	// TODO: add literal type support
	[[nodiscard]] auto generate_array_literal_expression
		(Llvm& state, const language::SyntaxTree& value) noexcept -> llvm::AllocaInst*
	{
		const auto literal = std::get<language::ArrayLiteralExpression>(value.data);

		// vector would be overkill, we just want a buffer
		auto llvm_elements = std::make_unique<Llvm::Value[]>(literal.value.size());

		auto llvm_elements_iterator = llvm_elements.get();
		for (const auto& element : literal.value)
		{
			auto& llvm_element = *llvm_elements_iterator;
			llvm_element = generate_expression(state, element);

			if (llvm_element == nullptr) UNLIKELY
			{
				return {};
			}

			++llvm_elements_iterator;
		}

		const auto llvm_allocation =
			state.builder.CreateAlloca
				(llvm::ArrayType::get((*llvm_elements.get())->getType(), literal.value.size()));

		if (llvm_allocation == nullptr) UNLIKELY
		{
			return {};
		}

		for
		(
			auto iterator = llvm_elements.get();
			iterator != llvm_elements.get() + literal.value.size();
			++iterator
		)
		{
			const auto llvm_address =
				state.builder.CreateInBoundsGEP
					(
						llvm_allocation, {state.builder.getInt64(0),
						state.builder.getInt64(iterator - llvm_elements.get())}
					);
			if (llvm_address == nullptr) UNLIKELY
			{
				return {};
			}

			if (state.builder.CreateStore(*iterator, llvm_address) == nullptr) UNLIKELY
			{
				return {};
			}
		}

		return llvm_allocation;
	}

	[[nodiscard]] auto generate_variable_expression
		(Llvm::Symbols& symbols, Llvm::Builder& builder, const language::SyntaxTree& variable)
		noexcept
		-> Llvm::Value
	{
		const auto identifier = std::get<Identifier>(variable.data);

		// note that constants are first
		if (symbols.first.count(identifier) == 1)
		{
			return symbols.first[identifier];
		}
		else if (symbols.second.count(identifier) == 1)
		{
			return builder.CreateLoad(symbols.second[identifier]);
		}
		else
		{
			return {};
		}
	}


	[[nodiscard]] auto generate_cast
		(Llvm& state, const language::BinaryExpression& expression) noexcept -> Llvm::Value
	{
		const auto left = generate_expression(state, *expression.left.pointer);
		const auto& right = std::get<Identifier>(expression.right.pointer->data);

		if (is_integer_type(right))
		{
			const auto type = identifier_to_type(right, state.context);
			return state.builder.CreateIntCast(left, type.llvm, type.is_signed);
		}
		else
		{
			return {};
		}
	}
	[[nodiscard]] auto generate_addition
		(Llvm::Builder& builder, Llvm::Value left_summand, Llvm::Value right_summand) noexcept -> Llvm::Value
	{
		const auto left_type = left_summand->getType();
		if (left_type->isIntegerTy())
		{
			return builder.CreateAdd(left_summand, right_summand);
		}
		else if (left_type->isFloatingPointTy())
		{
			return builder.CreateFAdd(left_summand, right_summand);
		}
		// dbg
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_subraction
		(Llvm::Builder& builder, Llvm::Value minuend, Llvm::Value subtrahend) noexcept -> Llvm::Value
	{
		const auto minuend_type = minuend->getType();
		if (minuend_type->isIntegerTy())
		{
			return builder.CreateSub(minuend, subtrahend);
		}
		else if (minuend_type->isFloatingPointTy())
		{
			return builder.CreateFSub(minuend, subtrahend);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_multiplication
		(Llvm::Builder& builder, Llvm::Value multiplicand, Llvm::Value multiplicator) noexcept -> Llvm::Value
	{
		const auto left_type = multiplicand->getType();
		if (left_type->isIntegerTy())
		{
			return builder.CreateMul(multiplicand, multiplicator);
		}
		else if (left_type->isFloatingPointTy())
		{
			return builder.CreateFMul(multiplicand, multiplicator);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_equal
		(Llvm::Builder& builder, Llvm::Value compared, Llvm::Value comparer) noexcept -> Llvm::Value
	{
		const auto dividend_type = compared->getType();

		if (dividend_type->isIntegerTy())
		{
			return builder.CreateICmpEQ(compared, comparer);
		}
		else if (dividend_type->isFloatingPointTy())
		{
			return builder.CreateFCmpOEQ(compared, comparer);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_not_equal
		(Llvm::Builder& builder, Llvm::Value compared, Llvm::Value comparer) noexcept -> Llvm::Value
	{
		const auto dividend_type = compared->getType();

		if (dividend_type->isIntegerTy())
		{
			return builder.CreateICmpNE(compared, comparer);
		}
		else if (dividend_type->isFloatingPointTy())
		{
			return builder.CreateFCmpONE(compared, comparer);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_greater_than
		(Llvm::Builder& builder, Llvm::Value compared, Llvm::Value comparer) noexcept -> Llvm::Value
	{
		const auto dividend_type = compared->getType();

		if (dividend_type->isIntegerTy())
		{
			return builder.CreateICmpSGT(compared, comparer);
		}
		else if (dividend_type->isFloatingPointTy())
		{
			return builder.CreateFCmpOGT(compared, comparer);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_greater_equal
		(Llvm::Builder& builder, Llvm::Value compared, Llvm::Value comparer) noexcept -> Llvm::Value
	{
		const auto dividend_type = compared->getType();

		if (dividend_type->isIntegerTy())
		{
			return builder.CreateICmpSGE(compared, comparer);
		}
		else if (dividend_type->isFloatingPointTy())
		{
			return builder.CreateFCmpOGE(compared, comparer);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_less_than
		(Llvm::Builder& builder, Llvm::Value compared, Llvm::Value comparer) noexcept -> Llvm::Value
	{
		const auto dividend_type = compared->getType();

		if (dividend_type->isIntegerTy())
		{
			return builder.CreateICmpSLT(compared, comparer);
		}
		else if (dividend_type->isFloatingPointTy())
		{
			return builder.CreateFCmpOLT(compared, comparer);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_less_equal
		(Llvm::Builder& builder, Llvm::Value compared, Llvm::Value comparer) noexcept -> Llvm::Value
	{
		const auto dividend_type = compared->getType();

		if (dividend_type->isIntegerTy())
		{
			return builder.CreateICmpSLE(compared, comparer);
		}
		else if (dividend_type->isFloatingPointTy())
		{
			return builder.CreateFCmpOLE(compared, comparer);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_division
		(Llvm::Builder& builder, Llvm::Value dividend, Llvm::Value divisor) noexcept -> Llvm::Value
	{
		const auto dividend_type = dividend->getType();

		if (dividend_type->isIntegerTy())
		{
			return builder.CreateSDiv(dividend, divisor);
		}
		else if (dividend_type->isFloatingPointTy())
		{
			return builder.CreateFDiv(dividend, divisor);
		}
		else
		{
			return {};
		} 
	}
	[[nodiscard]] auto generate_access_instance
		(Llvm::Builder& builder, Llvm::Value instance, Llvm::Value field) noexcept -> Llvm::Value
	{
		if (instance->getType() == builder.getInt8PtrTy())
		{
		}
	}
	// TODO: add all operators
	[[nodiscard]] auto generate_binary_expression
		(Llvm& state, const language::SyntaxTree& tree) noexcept -> Llvm::Value
	{
		const auto& expression = std::get<language::BinaryExpression>(tree.data);

		// right isn't expression
		if (expression.operation == language::BinaryExpression::Operation::cast)
		{
			return generate_cast(state, expression);
		}

		const auto& left = generate_expression(state, *expression.left.pointer);
		const auto& right = generate_expression(state, *expression.right.pointer);

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
		case language::BinaryExpression::Operation::access_instance:
			return generate_access_instance(state.builder, left, right);
		
		// dbg
		UNLIKELY default:
			std::abort();
		}
	}

	[[nodiscard]] auto generate_call_expression
		(Llvm& state, const language::SyntaxTree& expression) noexcept -> Llvm::Value
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
	[[nodiscard]] auto generate_subscript_expression
		(Llvm& state, const language::SyntaxTree& expression) noexcept -> Llvm::Value
	{
		const auto& subscript = std::get<language::IdentifiedExpression>(expression.data);

		const auto llvm_array = state.symbols.first.find(subscript.identifier);
		if (llvm_array == state.symbols.first.end()) UNLIKELY
		{
			return {};
		}

		const auto llvm_index = generate_expression(state, *subscript.value.pointer);
		if (llvm_index == nullptr) UNLIKELY
		{
			return {};
		}

		const auto llvm_pointer =
			state.builder.CreateInBoundsGEP(llvm_array->second, {state.builder.getInt64(0), llvm_index});
		if (llvm_pointer == nullptr) UNLIKELY
		{
			return {};
		}

		return state.builder.CreateLoad(llvm_pointer);
	}

	[[nodiscard]] auto generate_expression
		(Llvm& state, const language::SyntaxTree& expression) noexcept -> Llvm::Value
	{
		switch (expression.type)
		{
		case language::SyntaxTree::Type::whole_literal_expression:
			return generate_whole_literal_expression(state.context, expression);
		case language::SyntaxTree::Type::fraction_literal_expression:
			return generate_fraction_literal_expression(state.context, expression);
		case language::SyntaxTree::Type::character_literal_expression:
			return generate_character_literal_expression(state.context, expression);
		case language::SyntaxTree::Type::array_literal_expression:
			return generate_array_literal_expression(state, expression);
		case language::SyntaxTree::Type::string_literal_expression:
			return generate_string_literal_expression(state, expression);
		case language::SyntaxTree::Type::variable_expression:
			return generate_variable_expression(state.symbols, state.builder, expression);
		case language::SyntaxTree::Type::call_expression:
			return generate_call_expression(state, expression);
		case language::SyntaxTree::Type::subscript_expression:
			return generate_subscript_expression(state, expression);
		case language::SyntaxTree::Type::binary_expression:
			return generate_binary_expression(state, expression);

		// dbg
		default:
			std::abort();
		}
	}

	// [functions]===================================================================================
	[[nodiscard]] auto generate_function_decleration
		(Llvm::Context& context, llvm::Module& global_module, const language::SyntaxTree tree) noexcept
		-> Llvm::Function
	{
		const auto signature = std::get<language::FunctionSignature>(tree.data);

		// return type deduction
		const auto return_type = identifier_to_type(signature.return_type_identifier, context);
		if (return_type.llvm == nullptr) UNLIKELY
		{
			return nullptr;
		}

		auto parameter_types = std::vector<Llvm::Type> {signature.parameters.size()};
		auto code_parameter_iterator = parameter_types.begin();
		auto grammar_parameter_iterator = signature.parameters.cbegin();
		while (grammar_parameter_iterator != signature.parameters.end())
		{
			*code_parameter_iterator++ =
				identifier_to_type(grammar_parameter_iterator++->type_identifier, context).llvm;
		}

		const auto llvm_signature = llvm::FunctionType::get(return_type.llvm, parameter_types, false);
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
		(Llvm& state, const language::SyntaxForest& block) noexcept -> bool;
	
	[[nodiscard]] auto generate_return_statement
		(Llvm& state, const language::SyntaxTree& statement) noexcept -> bool
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
		(Llvm& state, const language::SyntaxTree& statement) noexcept -> bool
	{
		const auto definition = std::get<language::VariableDefinition>(statement.data);
		const auto llvm_value = generate_expression(state, *definition.initializer.pointer);

		if (llvm_value == nullptr) UNLIKELY
		{
			return {};
		}

		if (definition.is_mutable)
		{
			const auto type = llvm_value->getType();
			const auto llvm_allocation = state.builder.CreateAlloca(type);
			if (llvm_allocation == nullptr) UNLIKELY
			{
				return {};
			}
			state.symbols.second[definition.identifier] = llvm_allocation;

			if (state.builder.CreateStore(llvm_value, llvm_allocation) == nullptr) UNLIKELY
			{
				return {};
			}
		}
		else
		{
			state.symbols.first[definition.identifier] = llvm_value;
		}

		return true;
	}
	[[nodiscard]] auto generate_if_statement
		(Llvm& state, const language::SyntaxTree& statement) noexcept -> bool
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
	[[nodiscard]] auto generate_variable_reassignment_statement
		(Llvm& state, const language::SyntaxTree& statement) noexcept -> bool
	{
		const auto& reassignment = std::get<language::IdentifiedExpression>(statement.data);

		// get allocation
		const auto llvm_allocation_search = state.symbols.second.find(reassignment.identifier);
		if (llvm_allocation_search == state.symbols.second.end()) UNLIKELY
		{
			return {};
		}
		const auto llvm_allocation = llvm_allocation_search->second;

		// get value
		const auto llvm_value = generate_expression(state, *reassignment.value.pointer);
		if (llvm_value == nullptr) UNLIKELY
		{
			return {};
		}

		// merge value and allocation to llvm store
		return state.builder.CreateStore(llvm_value, llvm_allocation);
	}
	[[nodiscard]] auto generate_statement
		(Llvm& state, const language::SyntaxTree& statement) noexcept -> bool
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
			case language::SyntaxTree::Type::variable_reassignment:
				if (!generate_variable_reassignment_statement(state, statement)) UNLIKELY
				{
					return {};
				}
				break;

			// dbg
			default:
				std::fputs("unknown body statement", stderr);
				std::abort();
				break;
		}

		return true;
	}
	[[nodiscard]] auto generate_block
		(Llvm& state, const language::SyntaxForest& block) noexcept -> bool
	{
		for (const auto& statement : block)
		{
			if (!generate_statement(state, statement)) UNLIKELY
			{
				return {};
			}
		}

		return true;
	}
	// TODO: include check for sameness if signature exists
	[[nodiscard]] auto generate_function_definition
		(Llvm& state, const language::SyntaxTree& tree) noexcept -> bool
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
			identifier_to_type(definition.signature.return_type_identifier, state.context).llvm
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

		state.symbols.first.clear();
		state.symbols.second.clear();
		for (auto& parameter : function->args())
		{
			state.symbols.first[parameter.getName()] = &parameter;
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
	// entry point for code (LLVM-IR) generation grammar
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