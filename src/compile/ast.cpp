
#include "ast.hpp"

namespace ovid::ast {

Type *ast::Type::withoutMutability() { return this; }

Type *ast::MutType::withoutMutability() { return type.get(); }

void ScopedBlock::addStatement(std::unique_ptr<Statement> statement) {
  statements.push_back(std::move(statement));
}
} // namespace ovid::ast

namespace ovid {
void ActiveScopes::pushComponentScopesByName(
    const std::vector<std::string> &module) {
  auto nTable =
      names.pushComponentScopesByNameFromRoot(module, names.getRootScope());
  auto tTable =
      types.pushComponentScopesByNameFromRoot(module, types.getRootScope());
  assert(nTable != nullptr);
  assert(tTable != nullptr);
}

void ActiveScopes::popComponentScopesByName(
    const std::vector<std::string> &module) {
  names.popComponentScopesByNameFromRoot(module, names.getRootScope());
  types.popComponentScopesByNameFromRoot(module, types.getRootScope());
}

ActiveScopes::ActiveScopes(const std::vector<std::string> &packageName)
    : names(), types() {
  // add root scopes
  names.pushScope(
      std::make_shared<ovid::ScopeTable<ovid::Symbol>>(true, nullptr, ""));
  types.pushScope(
      std::make_shared<ovid::ScopeTable<ovid::TypeAlias>>(true, nullptr, ""));

  // add package scopes
  auto curNameScope = names.getRootScope();
  auto curTypeScope = types.getRootScope();
  for (auto &scope : packageName) {
    // packages are always public
    curNameScope = curNameScope->addScopeTable(scope, true, curNameScope);
    curTypeScope = curTypeScope->addScopeTable(scope, true, curTypeScope);
  }
}
} // namespace ovid