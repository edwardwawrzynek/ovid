
#include "ast.hpp"

namespace ovid::ast {

Type *ast::Type::withoutMutability() { return this; }

Type *ast::MutType::withoutMutability() { return type.get(); }

void ScopedBlock::addStatement(std::unique_ptr<Statement> statement) {
  statements.push_back(std::move(statement));
}
} // namespace ovid::ast