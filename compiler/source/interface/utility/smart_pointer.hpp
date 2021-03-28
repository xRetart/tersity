#pragma once

#include <memory>
#include "utility.hpp"


namespace vector::utility
{
    template<typename Data>
    struct SmartPointer
    {
        std::unique_ptr<Data> pointer;

        NODISCARD_CONSTRUCTOR SmartPointer(const std::unique_ptr<Data>& data) noexcept
            : pointer(std::make_unique<Data>(*data))
        {}
        NODISCARD_CONSTRUCTOR SmartPointer(const SmartPointer& copy) noexcept
            : pointer(std::make_unique<Data>(*copy.pointer))
        {}
        auto operator=(const SmartPointer& copy) noexcept -> void
        {
            // TODO: test std::make_unique
            pointer.reset(new Data {*copy.pointer});
        }
    };
} // namespace vector::utility