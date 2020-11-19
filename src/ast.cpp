#include "ast.hpp"
#include "generics.hpp"

namespace ovid::ast {

static uint64_t ast_id = 0;
/* id generation for type parameters */
uint64_t next_id() { return ast_id++; }

void reset_id() { ast_id = 0; }

size_t TypeConstructor::numTypeParams() const { assert(false); }

std::shared_ptr<Type> TypeConstructor::getFormalBoundType() const {
  assert(false);
}

const FormalTypeParameterList &
TypeConstructor::getFormalTypeParameters() const {
  assert(false);
}

ScopeTable<TypeAlias> *TypeConstructor::getFormalScopeTable() const {
  assert(false);
}

std::shared_ptr<Type> TypeConstructor::trivialConstruct() {
  return std::dynamic_pointer_cast<Type>(shared_from_this());
}

const Type *ast::Type::withoutMutability() const { return this; }
Type *Type::withoutMutability() { return this; }

bool Type::equal(const Type &expected, bool strict) const { return false; }

bool Type::containsPointer() const {
  // false provides a default for most of the builtin types
  return false;
}

size_t Type::numTypeParams() const { return 0; }

bool Type::equalStrict(const Type &other) const { return equal(other, true); }

bool Type::equalToExpected(const Type &other) const {
  return equal(other, false);
}

const Type *ast::MutType::withoutMutability() const { return type.get(); }
Type *MutType::withoutMutability() { return type.get(); }

void ScopedBlock::addStatement(std::unique_ptr<Statement> statement) {
  statements.push_back(std::move(statement));
}

bool UnresolvedType::equal(const Type &expected, bool strict) const {
  assert(false);
}

bool VoidType::equal(const Type &expected, bool strict) const {
  return dynamic_cast<const VoidType *>(&expected) != nullptr;
}

bool BoolType::equal(const Type &expected, bool strict) const {
  return dynamic_cast<const BoolType *>(&expected) != nullptr;
}

bool IntType::equal(const Type &expected, bool strict) const {
  const auto expectInt = dynamic_cast<const IntType *>(&expected);

  if (expectInt == nullptr)
    return false;

  return expectInt->isUnsigned == isUnsigned && expectInt->size == size;
}

bool TupleType::equal(const Type &expected, bool strict) const {
  const auto expectTuple = dynamic_cast<const TupleType *>(&expected);
  if (expectTuple == nullptr)
    return false;
  if (expectTuple->types.size() != types.size())
    return false;
  for (size_t i = 0; i < types.size(); i++) {
    if (!types[i]->equal(*expectTuple->types[i], strict))
      return false;
  }
  return true;
}

bool FloatType::equal(const Type &expected, bool strict) const {
  const auto expectFloat = dynamic_cast<const FloatType *>(&expected);

  if (expectFloat == nullptr)
    return false;

  return expectFloat->size == size;
}

bool MutType::equal(const Type &expected, bool strict) const {
  // if expected is mut, check inner types
  auto expectMut = dynamic_cast<const MutType *>(&expected);
  if (expectMut != nullptr) {
    return type->equal(*expectMut->type, strict);
  }
  // otherwise, remove mut in check (converting mut -> non mut is valid)
  if (!strict) {
    return type->equal(expected, strict);
  }
  return false;
}

bool MutType::containsPointer() const { return type->containsPointer(); }

bool PointerType::equal(const Type &expected, bool strict) const {
  const auto expectPointer = dynamic_cast<const PointerType *>(&expected);

  if (expectPointer == nullptr)
    return false;

  return type->equal(*expectPointer->type, strict);
}

bool PointerType::containsPointer() const { return true; }

bool FunctionType::equal(const Type &expected, bool strict) const {
  const auto expectFunctionType = dynamic_cast<const FunctionType *>(&expected);

  // FunctionType and NamedFunctionType are equivalent if arg types and ret
  // types match (ignoring arg names)
  if (expectFunctionType != nullptr) {

    if (argTypes.size() != expectFunctionType->argTypes.size())
      return false;

    for (size_t i = 0; i < argTypes.size(); i++) {
      if (!argTypes[i]->equal(*expectFunctionType->argTypes[i], strict))
        return false;
    }

    return retType->equal(*expectFunctionType->retType, strict);
  }

  return false;
}

bool NamedFunctionType::equal(const Type &expected, bool strict) const {
  return FunctionType::equal(expected, strict);
}

std::shared_ptr<Type> ProductType::getTypeOfField(int32_t field_index) const {
  assert(false);
}

size_t ProductType::getNumFields() const { assert(false); }

bool ProductType::containsPointer() const {
  auto numFields = getNumFields();
  for (size_t i = 0; i < numFields; i++) {
    if (getTypeOfField(i)->containsPointer())
      return true;
  }

  return false;
}

int32_t ProductType::getNamedFieldIndex(const std::string &field_name) const {
  assert(false);
}

int32_t ProductType::getNumberedFieldIndex(int32_t field) const {
  assert(false);
}

bool ProductType::fieldIsPublic(int32_t field_index) const { assert(false); }

const TypeAlias *ProductType::getTypeAlias() const { assert(false); }

size_t TupleType::getNumFields() const { return types.size(); }

std::shared_ptr<Type> TupleType::getTypeOfField(int32_t field_index) const {
  assert(field_index >= 0 && (uint32_t)field_index < types.size());

  return types[field_index];
}

int32_t TupleType::getNamedFieldIndex(const std::string &field_name) const {
  return -1;
}

int32_t TupleType::getNumberedFieldIndex(int32_t field) const {
  if (field < 0 || field >= (int32_t)(types.size()))
    return -1;

  return field;
}

bool TupleType::fieldIsPublic(int32_t field_index) const { return true; }

const TypeAlias *TupleType::getTypeAlias() const { return nullptr; }

bool StructType::equal(const Type &expected, bool strict) const {
  auto expectedStruct = dynamic_cast<const StructType *>(&expected);
  if (expectedStruct == nullptr)
    return false;

  assert(getTypeAlias() != nullptr);
  assert(expectedStruct->getTypeAlias() != nullptr);

  /* structure types use name equality */
  auto alias_equal = getTypeAlias() == expectedStruct->getTypeAlias();
  if (!alias_equal)
    return false;

  // make sure generic parameters are equal
  assert(actual_generic_params.size() ==
         expectedStruct->actual_generic_params.size());
  for (size_t i = 0; i < actual_generic_params.size(); i++) {
    if (!actual_generic_params[i]->equal(
            *expectedStruct->actual_generic_params[i], strict)) {
      return false;
    }
  }

  return true;
}

size_t StructType::getNumFields() const { return field_types.size(); }

std::shared_ptr<Type> StructType::getTypeOfField(int32_t field_index) const {
  assert(field_index < (int32_t)(field_types.size()) && field_index >= 0);
  return field_types[field_index];
}

int32_t StructType::getNamedFieldIndex(const std::string &field_name) const {
  for (size_t i = 0; i < field_names.size(); i++) {
    if (field_names[i] == field_name)
      return i;
  }

  return -1;
}

int32_t StructType::getNumberedFieldIndex(int32_t field) const {
  /* no numbered fields on a struct */
  return -1;
}

bool StructType::fieldIsPublic(int32_t field_index) const {
  assert(field_index < (int32_t)(field_types.size()) && field_index >= 0);
  return fields_are_public[field_index];
}

const TypeAlias *StructType::getTypeAlias() const { return type_alias; }

bool StructType::hasPublicConstructor() const {
  for (auto pub : fields_are_public) {
    if (!pub)
      return false;
  }

  return true;
}

GenericTypeConstructor::GenericTypeConstructor(const SourceLocation &loc,
                                               FormalTypeParameterList params,
                                               std::shared_ptr<Type> type)
    : TypeConstructor(loc), params(std::move(params)), type(std::move(type)),
      type_scope(
          std::make_unique<ScopeTable<TypeAlias>>(false, nullptr, "", true)) {
  // construct scope table from type parameters
  for (auto &param : this->params) {
    auto type_alias = std::make_shared<TypeAlias>(param->loc, param, false);
    type_scope->addSymbol(param->name, std::move(type_alias));
  }
}

size_t GenericTypeConstructor::numTypeParams() const { return params.size(); }

const FormalTypeParameterList &
GenericTypeConstructor::getFormalTypeParameters() const {
  return params;
}

std::shared_ptr<Type> GenericTypeConstructor::getFormalBoundType() const {
  return type;
}

ScopeTable<TypeAlias> *GenericTypeConstructor::getFormalScopeTable() const {
  return type_scope.get();
}

bool FormalTypeParameter::equal(const Type &other, bool strict) const {
  auto expectedParam = dynamic_cast<const FormalTypeParameter *>(&other);
  return expectedParam != nullptr && expectedParam->id == id;
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

ActiveScopes::ActiveScopes(const std::vector<std::string> &packageName,
                           int64_t package_version,
                           ScopeTable<Symbol> *rootNameScope,
                           ScopeTable<TypeAlias> *rootTypeScope)
    : names(), types() {
  // add root scopes
  names.pushScope(rootNameScope);
  types.pushScope(rootTypeScope);

  // add package scopes
  auto curNameScope = names.getRootScope();
  auto curTypeScope = types.getRootScope();
  for (auto &scope : packageName) {
    // packages are always public
    curNameScope = curNameScope->addScopeTable(scope, true);
    curTypeScope = curTypeScope->addScopeTable(scope, true);
  }

  curNameScope->setVersionInt(package_version);
  curTypeScope->setVersionInt(package_version);
}

std::vector<std::string> Symbol::getFullyScopedName() const {
  size_t scopes_size = 1;
  auto tmp = parent_table;
  while (tmp != nullptr && !tmp->getName().empty()) {
    scopes_size++;
    tmp = tmp->getParent();
  }

  std::vector<std::string> fullName(scopes_size, "");

  fullName[scopes_size - 1] = name;
  tmp = parent_table;
  while (tmp != nullptr && !tmp->getName().empty()) {
    scopes_size--;
    fullName[scopes_size - 1] = tmp->getName();
    tmp = tmp->getParent();
  }

  return fullName;
}

std::vector<std::string> TypeAlias::getFullyScopedName() const {
  size_t scopes_size = 1;
  auto tmp = parent_table;
  while (tmp != nullptr && !tmp->getName().empty()) {
    scopes_size++;
    tmp = tmp->getParent();
  }

  std::vector<std::string> fullName(scopes_size, "");

  fullName[scopes_size - 1] = name;
  tmp = parent_table;
  while (tmp != nullptr && !tmp->getName().empty()) {
    scopes_size--;
    fullName[scopes_size - 1] = tmp->getName();
    tmp = tmp->getParent();
  }

  return fullName;
}

} // namespace ovid