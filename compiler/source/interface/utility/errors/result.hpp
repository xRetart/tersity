#pragma once

#include "error.hpp"


namespace vector::error
{
    // sumtype reporting error information
    template<typename Value>
    class Result
    {
        using Error = Error;
        using Data = std::variant<Value, Error>;

        Data _data;

    public:
        NODISCARD_CONSTRUCTOR constexpr Result(const Value& value)
            : _data(value)
        {}
        NODISCARD_CONSTRUCTOR constexpr Result(Value&& value)
            : _data(std::move(value))
        {}
        NODISCARD_CONSTRUCTOR constexpr Result(const Error& error)
            : _data(error)
        {}
        NODISCARD_CONSTRUCTOR constexpr Result(Error&& error)
            : _data(std::move(error))
        {}

        [[nodiscard]] constexpr auto is_error() const noexcept -> bool
        {
            return _data.index() == 1;
        }
        [[nodiscard]] constexpr auto is_value() const noexcept -> bool
        {
            return _data.index() == 0;
        }

        [[nodiscard]] constexpr auto value() noexcept -> Value&
        {
            return std::get<Value>(_data);
        }
        [[nodiscard]] constexpr auto value() const noexcept -> const Value&
        {
            return std::get<Value>(_data);
        }
        [[nodiscard]] constexpr auto error() noexcept -> Error&
        {
            return std::get<Error>(_data);
        }
        [[nodiscard]] constexpr auto error() const noexcept -> const Error&
        {
            return std::get<Error>(_data);
        }
    };
    template<>
    class Result<void>
    {
        using Error = Error;
        using Data = std::optional<Error>;

        Data _data;

    public:
        NODISCARD_CONSTRUCTOR constexpr Result(const None)
            : _data()
        {}
        NODISCARD_CONSTRUCTOR constexpr Result(const Error& error)
            : _data(error)
        {}
        NODISCARD_CONSTRUCTOR constexpr Result(Error&& error)
            : _data(std::move(error))
        {}

        [[nodiscard]] constexpr auto is_error() const noexcept -> bool
        {
            return _data.has_value();
        }
        [[nodiscard]] constexpr auto is_value() const noexcept -> bool
        {
            return !_data.has_value();
        }

        [[nodiscard]] constexpr auto error() noexcept -> Error&
        {
            return _data.value();
        }
        [[nodiscard]] constexpr auto error() const noexcept -> const Error&
        {
            return _data.value();
        }
    };

}