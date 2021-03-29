#pragma once

#include <cstdio>
#include <string>
#include "utility.hpp"
#include "errors/error.hpp"
#include "../vector/generation.hpp"


namespace vector::files
{
	[[nodiscard]] auto get_size(FILE* const pointer) noexcept -> std::size_t
	{
		std::fseek(pointer, 0, SEEK_END);

		const auto size = std::ftell(pointer);
		std::fseek(pointer, 0, SEEK_SET);

		return size;
	}
	[[nodiscard]] auto load(const char* const name) noexcept -> error::Result<std::string>
	{
		const auto pointer = std::fopen(name, "r");
		if (pointer == nullptr) UNLIKELY
		{
			return error::Error {"user error: opening infile failed", error::Code::opening_file};
		}

		const auto size = get_size(pointer);

		auto content_buffer = std::vector<char>(size);
		const auto read_result = std::fread(content_buffer.data(), sizeof(char), size, pointer);
		if (read_result == 0) UNLIKELY
		{
			return error::Error {"user error: reading infile failed", error::Code::reading_file};
		}

		return std::string {content_buffer.begin(), content_buffer.end()};
	}
	[[nodiscard]] auto output_module
	(
		const decltype(generation::Llvm::global_module)& global_module,
		const std::string_view filename
	)
	noexcept -> error::Result<void>
	{
		auto error_code = std::error_code {};
		auto out_stream = llvm::raw_fd_ostream {filename, error_code};
		if (error_code) UNLIKELY
		{
			return error::Error {"system error: failed to open outfile", error::Code::opening_file};
		}

		global_module.print(out_stream, nullptr);

		return error::None {};
	}
}