
#include "ast.hpp"

namespace ovid::ast {

Type *ast::Type::withoutMutability() { return this; }

Type *ast::MutType::withoutMutability() { return type.get(); }

void ScopedBlock::addStatement(std::unique_ptr<Statement> statement) {
  statements.push_back(std::move(statement));
}
} // namespace ovid::ast

namespace ovid {
void ActiveScopes::pushScopeByName(const std::vector<std::string> &package,
                                   const std::vector<std::string> &module) {
  assert(names.getRootScope()->getScopeTable(package)->getScopeTable(module) !=
         nullptr);
  assert(types.getRootScope()->getScopeTable(package)->getScopeTable(module) !=
         nullptr);

  names.pushScope(
      names.getRootScope()->getScopeTable(package)->getScopeTable(module));
  types.pushScope(
      types.getRootScope()->getScopeTable(package)->getScopeTable(module));
}

void ActiveScopes::pushScopeByName(const std::vector<std::string> &module) {
  assert(names.getRootScope()->getScopeTable(module) != nullptr);
  assert(types.getRootScope()->getScopeTable(module) != nullptr);
  names.pushScope(names.getRootScope()->getScopeTable(module));
  types.pushScope(types.getRootScope()->getScopeTable(module));
}

void ActiveScopes::popScopeByName(const std::vector<std::string> &package,
                                  const std::vector<std::string> &module) {
  assert(names.getRootScope()->getScopeTable(package)->getScopeTable(module) !=
         nullptr);
  assert(types.getRootScope()->getScopeTable(package)->getScopeTable(module) !=
         nullptr);

  names.popScope(
      names.getRootScope()->getScopeTable(package)->getScopeTable(module));
  types.popScope(
      types.getRootScope()->getScopeTable(package)->getScopeTable(module));
}

void ActiveScopes::popScopeByName(const std::vector<std::string> &module) {
  assert(names.getRootScope()->getScopeTable(module) != nullptr);
  assert(types.getRootScope()->getScopeTable(module) != nullptr);

  names.popScope(names.getRootScope()->getScopeTable(module));
  types.popScope(types.getRootScope()->getScopeTable(module));
}
} // namespace ovid