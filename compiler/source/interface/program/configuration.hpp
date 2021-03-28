#pragma once

#include <string_view>
#include "../utility/utility.hpp"
#include "../utility/errors/result.hpp"


namespace vector::configuration
{
    struct Program
    {
        std::string_view source_filename;
        std::string_view out_filename;
    };

    constexpr auto parse_parameters(const char* const* const parameters, const int count) noexcept
    -> error::Result<Program>
    {
        if (count < 3) UNLIKELY
        {
            return error::Error {"not enough parameters", error::Code::parameters};
        }
        
        return Program {parameters[1], parameters[2]};
    }
}