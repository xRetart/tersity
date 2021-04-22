#include <vector>
#include <string>
#include <iostream>
#include "interface/program/configuration.hpp"
#include "interface/utility/errors/error.hpp"
#include "interface/utility/files.hpp"
#include "interface/vector/lexing.hpp"
#include "interface/vector/parsing.hpp"
#include "interface/vector/generation.hpp"


auto main(const int parameter_count, const char* const* const parameters) noexcept -> int
{
    // configurate program
    const auto configuration_result =
        vector::configuration::parse_parameters(parameters, parameter_count);
    VECTOR_HANDLE_RESULT(configuration_result);
    const auto configuration = configuration_result.value();


    // load source file
    const auto source_result = vector::files::load(configuration.source_filename.data());
    VECTOR_HANDLE_RESULT(source_result);
    const auto& source = source_result.value();

    
    // lex source to tokens
    const auto tokens = vector::lexing::lex(source);


    // parse tokens to grammar
    const auto grammar_result = vector::parsing::parse(tokens);
    VECTOR_HANDLE_RESULT(grammar_result);
    const auto& grammar = grammar_result.value();
    

    // generate grammar to llvm-ir
    auto state = vector::generation::Llvm {};
    const auto ir = vector::generation::generate(grammar, state);
    if (!ir) UNLIKELY
    {
        std::fputs("error: generate returned null\n", stderr);
        state.global_module.print(llvm::errs(), nullptr);
        std::abort();
    }


    // output llvm-ir to outfile
    const auto output = 
        vector::files::output_module(state.global_module, configuration.out_filename);
    VECTOR_HANDLE_RESULT(output);

    return 0;
}