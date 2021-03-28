#pragma once

#include "../language.hpp"
#include "../../utility/errors/result.hpp"


namespace vector::parsing
{
    template<typename Predicate>
    auto iterate_seperated_list
    (
        language::TokenIterator iterator,
        const Predicate& predicate,
        const language::Sign terminator = language::Sign::closing_parenthese
    )
        noexcept
        -> error::Result<void>
    {
        auto comma_needed = false;
        while (!iterator->is(terminator))
        {
            if (iterator->is(language::Sign::comma))
            {
                VECTOR_ASSERT
                (
                    comma_needed,
                    (error::Error {"expected comma", error::Code::missing_comma})
                );

                comma_needed = false;
                ++iterator;
            }
            else
            {
                VECTOR_ASSERT
                (
                    !comma_needed,
                    (error::Error {"expected comma", error::Code::missing_comma})
                );
                VECTOR_ASSERT_RESULT(predicate());

                comma_needed = true;
            }
        }

        ++iterator;
        return error::none;
    }
}