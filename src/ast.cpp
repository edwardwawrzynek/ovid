
#include "ast.hpp"

namespace ovid::ast {

Type *ast::Type::withoutMutability() { return this; }

Type *ast::MutType::withoutMutability() { return type.get(); }
} // namespace ovid::ast