
#include "ast.hpp"

namespace ovid::ast {

const Type &ast::Type::withoutMutability() const { return *this; }

bool Type::equalToExpected(const Type &expected) { return false; }

const Type &ast::MutType::withoutMutability() const { return *type; }

void ScopedBlock::addStatement(std::unique_ptr<Statement> statement) {
  statements.push_back(std::move(statement));
}

bool UnresolvedType::equalToExpected(const Type &expected) {
  assert(false);

  return false;
}

bool ResolvedAlias::equalToExpected(const Type &expected) {
  return alias->type->equalToExpected(expected);
}

bool VoidType::equalToExpected(const Type &expected) {
  return dynamic_cast<const VoidType *>(&expected) != nullptr;
}

bool BoolType::equalToExpected(const Type &expected) {
  return dynamic_cast<const BoolType *>(&expected) != nullptr;
}

bool IntType::equalToExpected(const Type &expected) {
  const auto expectInt = dynamic_cast<const IntType *>(&expected);

  if (expectInt == nullptr)
    return false;

  return expectInt->isUnsigned == isUnsigned && expectInt->size == size;
}

bool TupleType::equalToExpected(const Type &expected) {
  const auto expectTuple = dynamic_cast<const TupleType *>(&expected);

  if (expectTuple == nullptr)
    return false;
  if (expectTuple->types.size() != types.size())
    return false;

  for (size_t i = 0; i < types.size(); i++) {
    if (!types[i]->equalToExpected(*expectTuple->types[i]))
      return false;
  }

  return true;
}

bool FloatType::equalToExpected(const Type &expected) {
  const auto expectFloat = dynamic_cast<const FloatType *>(&expected);

  if (expectFloat == nullptr)
    return false;

  return expectFloat->size == size;
}

bool MutType::equalToExpected(const Type &expected) {
  // if expected is mut, check inner types
  auto expectMut = dynamic_cast<const MutType *>(&expected);
  if (expectMut != nullptr) {
    return type->equalToExpected(*expectMut->type);
  }
  // otherwise, remove mut in check (converting mut -> non mut is valid)
  return type->equalToExpected(expected);
}

bool PointerType::equalToExpected(const Type &expected) {
  const auto expectPointer = dynamic_cast<const PointerType *>(&expected);

  if (expectPointer == nullptr)
    return false;

  return type->equalToExpected(*expectPointer->type);
}

bool FunctionType::equalToExpected(const Type &expected) {
  const auto expectFunctionType = dynamic_cast<const FunctionType *>(&expected);
  const auto expectNamedFunctionType =
      dynamic_cast<const NamedFunctionType *>(&expected);

  // FunctionType and NamedFunctionType are equivalent if arg types and ret
  // types match (ignoring arg names)
  if (expectFunctionType != nullptr) {

    if (argTypes.size() != expectFunctionType->argTypes.size())
      return false;

    for (size_t i = 0; i < argTypes.size(); i++) {
      if (!argTypes[i]->equalToExpected(*expectFunctionType->argTypes[i]))
        return false;
    }

    return retType->equalToExpected(*expectFunctionType->retType);
  } else if (expectNamedFunctionType != nullptr) {
    return this->equalToExpected(*expectNamedFunctionType->type);
  }

  return false;
}

bool NamedFunctionType::equalToExpected(const Type &expected) {
  const auto expectFunctionType = dynamic_cast<const FunctionType *>(&expected);
  const auto expectNamedFunctionType =
      dynamic_cast<const NamedFunctionType *>(&expected);

  // FunctionType and NamedFunctionType are equivalent if arg types and ret
  // types match (ignoring arg names)
  if (expectFunctionType != nullptr) {
    return type->equalToExpected(*expectFunctionType);
  } else if (expectNamedFunctionType != nullptr) {
    return type->equalToExpected(*expectNamedFunctionType->type);
  }

  return false;
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