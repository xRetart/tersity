#pragma once

#include <vector>
#include <tuple>
#include "../utility/smart_pointer.hpp"


namespace vector::language
{
	// lexing
	using Source = std::string_view;
	using SourceIterator = Source::const_iterator&;

	enum class Sign : unsigned char
	{
		// operators
		plus
		, minus
		, astrisk
		, slash
		, percent
		, double_astrisk
		, as
		, equals
		, dot
		, arrow
		, ampersand
		, pipe
		, power
		, tilde
		, exclamation
		, double_equals
		, exclamation_equals
		, less_than
		, less_equals
		, greater_than
		, greater_equals
		, double_pipe
		, double_ampersand

		// seperators
		, comma
		, semicolon
		, colon
		, double_colon
		, quote
		, single_quote
		, opening_parenthese
		, closing_parenthese
		, opening_bracket
		, closing_bracket
		, opening_brace
		, closing_brace
	};

	struct Token
	{
		enum class Type : unsigned char
		{
			keyword
			, identifier
			, whole_literal
			, fraction_literal
			, string_literal
			, character_literal
			, operator_symbol
			, seperator_symbol
		};
		using Value = std::variant<unsigned long long, double, char, std::string, Sign>;

		Type type;
		Value value;

		[[nodiscard]] auto is
		(
			const language::Sign sign,
			const language::Token::Type other_type = language::Token::Type::seperator_symbol
		)
			const noexcept
			-> bool
		{
			return type == other_type && std::get<Sign>(value) == sign;
		}
	};
	using Tokens = std::vector<Token>;
	using TokenIterator = Tokens::const_iterator&;


	// parsing
	struct SyntaxTree;
	using SyntaxForest = std::vector<SyntaxTree>;

	template<typename Value>
	struct LiteralExpression
	{
		std::string_view type;
		Value value;
	};
	using WholeLiteralExpression = LiteralExpression<unsigned long long>;
	using FractionLiteralExpression = LiteralExpression<double>;
	using CharacterLiteralExpression = LiteralExpression<char>;
	using StringLiteralExpression = LiteralExpression<std::string>;

	struct IfStatement
	{
		utility::SmartPointer<SyntaxTree> condition;
		SyntaxForest true_block;
		SyntaxForest false_block;
	};
	struct BinaryExpression
	{
		enum class Operation : unsigned char
		{
			addition, subtraction, multiplication, division, modulo, exponentiation,
			cast,
			assign, access_instance, access_pointer, access_space,
			bitwise_or, bitwise_and, bitwise_xor,
			equal, not_equal, less_than, less_equal, greater_than, greater_equal,
			logical_or, logical_and
		};

		Operation operation;
		utility::SmartPointer<SyntaxTree> left;
		utility::SmartPointer<SyntaxTree> right;
	};
	struct CallExpression
	{
		std::vector<SyntaxTree> parameters;
		std::string callee;
	};
	struct Parameter
	{
		std::string identifier;
		std::string type_identifier;
	};
	using Parameters = std::vector<Parameter>;

	struct FunctionSignature
	{
		Parameters parameters;
		std::string identifier;
		std::string return_type_identifier;
	};
	struct VariableDefinition
	{
		std::string_view identifier;
		utility::SmartPointer<SyntaxTree> initializer;
	};
	struct FunctionDefinition
	{
		FunctionSignature signature;
		SyntaxForest body;
	};
	struct StructureDefinition
	{
		std::vector<std::string> fields;
		std::vector<FunctionDefinition> methods;

		std::string_view  identifier;
	};

	struct SyntaxTree
	{
		enum class Type : unsigned char
		{
			// expressions
			whole_literal_expression
			, fraction_literal_expression
			, character_literal_expression
			, string_literal_expression
			, variable_expression
			, type_expression
			, binary_expression
			, call_expression

			// statements
			, return_statement
			, if_statement

			// declerations
			, variable_decleration
			, function_decleration
			, structure_decleration
			, class_decleration

			// definitions
			, variable_definition
			, function_definition
			, structure_definition
			, class_definition
			
			// top level statements
			, import_statement
		};
		using Data = 
			std::variant
			<
				std::string_view  // identifier expression

				// expressions
				, BinaryExpression
				, CallExpression

				// literals
				, WholeLiteralExpression
				, FractionLiteralExpression
				, CharacterLiteralExpression
				, StringLiteralExpression

				// signatures
				, FunctionSignature

				// statements
				, IfStatement

				// definitions
				, VariableDefinition
				, FunctionDefinition
				, StructureDefinition

				// top levels
				, utility::SmartPointer<SyntaxTree>  // import statement, return statement
			>;


		Type type;
		Data data;
	};


	// generating
	struct DataType
	{
		llvm::Type* llvm;
		bool is_signed;
	};
}