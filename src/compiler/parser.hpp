#pragma once

#include <compiler/ast.hpp>

#include <string_view>
#include <memory>

namespace spc {
    std::unique_ptr<Unit> parse(std::string_view) noexcept;
} // namespace spc