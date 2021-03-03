#include "ast.hpp"
#include "generics.hpp"
#include "type_check.hpp"

#include <functional>

namespace ovid::ast {

static uint64_t ast_id = 0;
/* id generation for type parameters */
uint64_t next_id() { return ast_id++; }

void reset_id() { ast_id = 0; }

size_t TypeConstructor::numTypeParams() const { return 0; }

std::shared_ptr<Type> TypeConstructor::getFormalBoundType() const {
  assert(false);
}

const FormalTypeParameterList &
TypeConstructor::getFormalTypeParameters() const {
  assert(false);
}

FormalTypeParameterList &TypeConstructor::getFormalTypeParameters() {
  assert(false);
}

ScopeTable<TypeAlias> *TypeConstructor::getFormalScopeTable() const {
  assert(false);
}

std::shared_ptr<Type> TypeConstructor::trivialConstruct() {
  return std::dynamic_pointer_cast<Type>(shared_from_this());
}

std::shared_ptr<Type> TypeConstructor::noParamConstruct() {
  return trivialConstruct();
}

const Type *ast::Type::withoutMutability() const { return this; }
Type *Type::withoutMutability() { return this; }

bool Type::equal(const Type &expected, bool strict,
                 const TypeParamEqualPredicate *typeParamFunc) const {
  return false;
}

bool Type::containsPointer() const {
  // false provides a default for most of the builtin types
  return false;
}

size_t Type::numTypeParams() const { return 0; }

bool Type::equalStrict(const Type &other) const {
  return equal(other, true, nullptr);
}

bool Type::equalToExpected(const Type &other) const {
  return equal(other, false, nullptr);
}

const Type *ast::MutType::withoutMutability() const { return type.get(); }
Type *MutType::withoutMutability() { return type.get(); }

void ScopedBlock::addStatement(std::unique_ptr<Statement> statement) {
  statements.push_back(std::move(statement));
}

bool UnresolvedType::equal(const Type &expected, bool strict,
                           const TypeParamEqualPredicate *typeParamFunc) const {
  assert(false);
}

bool VoidType::equal(const Type &expected, bool strict,
                     const TypeParamEqualPredicate *typeParamFunc) const {
  return dynamic_cast<const VoidType *>(&expected) != nullptr;
}

bool BoolType::equal(const Type &expected, bool strict,
                     const TypeParamEqualPredicate *typeParamFunc) const {
  return dynamic_cast<const BoolType *>(&expected) != nullptr;
}

bool IntType::equal(const Type &expected, bool strict,
                    const TypeParamEqualPredicate *typeParamFunc) const {
  const auto expectInt = dynamic_cast<const IntType *>(&expected);

  if (expectInt == nullptr)
    return false;

  return expectInt->isUnsigned == isUnsigned && expectInt->size == size;
}

bool TupleType::equal(const Type &expected, bool strict,
                      const TypeParamEqualPredicate *typeParamFunc) const {
  const auto expectTuple = dynamic_cast<const TupleType *>(&expected);
  if (expectTuple == nullptr)
    return false;
  if (expectTuple->types.size() != types.size())
    return false;
  for (size_t i = 0; i < types.size(); i++) {
    if (!types[i]->equal(*expectTuple->types[i], strict, typeParamFunc))
      return false;
  }
  return true;
}

bool FloatType::equal(const Type &expected, bool strict,
                      const TypeParamEqualPredicate *typeParamFunc) const {
  const auto expectFloat = dynamic_cast<const FloatType *>(&expected);

  if (expectFloat == nullptr)
    return false;

  return expectFloat->size == size;
}

bool MutType::equal(const Type &expected, bool strict,
                    const TypeParamEqualPredicate *typeParamFunc) const {
  // if expected is mut, check inner types
  auto expectMut = dynamic_cast<const MutType *>(&expected);
  if (expectMut != nullptr) {
    return type->equal(*expectMut->type, strict, typeParamFunc);
  }
  // otherwise, remove mut in check (converting mut -> non mut is valid)
  if (!strict) {
    return type->equal(expected, strict, typeParamFunc);
  }
  return false;
}

bool MutType::containsPointer() const { return type->containsPointer(); }

bool PointerType::equal(const Type &expected, bool strict,
                        const TypeParamEqualPredicate *typeParamFunc) const {
  const auto expectPointer = dynamic_cast<const PointerType *>(&expected);

  if (expectPointer == nullptr)
    return false;

  return type->equal(*expectPointer->type, strict, typeParamFunc);
}

bool PointerType::containsPointer() const { return true; }

bool FunctionType::equal(const Type &expected, bool strict,
                         const TypeParamEqualPredicate *typeParamFunc) const {
  const auto expectFunctionType = dynamic_cast<const FunctionType *>(&expected);

  // FunctionType and NamedFunctionType are equivalent if arg types and ret
  // types match (ignoring arg names)
  if (expectFunctionType != nullptr) {

    if (argTypes.size() != expectFunctionType->argTypes.size())
      return false;

    for (size_t i = 0; i < argTypes.size(); i++) {
      if (!argTypes[i]->equal(*expectFunctionType->argTypes[i], strict,
                              typeParamFunc))
        return false;
    }

    return retType->equal(*expectFunctionType->retType, strict, typeParamFunc);
  }

  return false;
}

bool NamedFunctionType::equal(
    const Type &expected, bool strict,
    const TypeParamEqualPredicate *typeParamFunc) const {
  return FunctionType::equal(expected, strict, typeParamFunc);
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

bool StructType::equal(const Type &expected, bool strict,
                       const TypeParamEqualPredicate *typeParamFunc) const {
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
            *expectedStruct->actual_generic_params[i], strict, typeParamFunc)) {
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

// construct a flat scope table from a vector of formal type parameters (name ->
// formal param)
std::unique_ptr<ScopeTable<TypeAlias>>
scopeTableFromFormalTypeParams(const FormalTypeParameterList &params) {
  auto table =
      std::make_unique<ScopeTable<TypeAlias>>(false, nullptr, "", true);
  for (const auto &param : params) {
    auto type_alias = std::make_shared<TypeAlias>(param->loc, param, false);
    table->addSymbol(param->name, std::move(type_alias));
  }

  return table;
}

GenericTypeConstructor::GenericTypeConstructor(const SourceLocation &loc,
                                               FormalTypeParameterList params,
                                               std::shared_ptr<Type> type)
    : TypeConstructor(loc), params(std::move(params)), type(std::move(type)),
      type_scope(scopeTableFromFormalTypeParams(this->params)) {}

size_t GenericTypeConstructor::numTypeParams() const { return params.size(); }

const FormalTypeParameterList &
GenericTypeConstructor::getFormalTypeParameters() const {
  return params;
}

FormalTypeParameterList &GenericTypeConstructor::getFormalTypeParameters() {
  return params;
}

std::shared_ptr<Type> GenericTypeConstructor::getFormalBoundType() const {
  return type;
}

ScopeTable<TypeAlias> *GenericTypeConstructor::getFormalScopeTable() const {
  return type_scope.get();
}

std::shared_ptr<Type> GenericTypeConstructor::noParamConstruct() {
  if (params.size() == 0) {
    return type;
  } else {
    return nullptr;
  }
}

bool FormalTypeParameter::equal(
    const Type &other, bool strict,
    const TypeParamEqualPredicate *typeParamFunc) const {
  if (typeParamFunc != nullptr) {
    return (*typeParamFunc)(*this, other);
  }
  auto expectedParam = dynamic_cast<const FormalTypeParameter *>(&other);
  return expectedParam != nullptr && expectedParam->id == id;
}

std::shared_ptr<NamedFunctionType> FunctionDecl::getFormalBoundFunctionType() {
  auto res =
      std::dynamic_pointer_cast<NamedFunctionType>(type->getFormalBoundType());
  assert(res != nullptr);
  return res;
}

ImplHeader::ImplHeader(FormalTypeParameterList type_params,
                       std::shared_ptr<Type> type,
                       ScopeTable<Symbol> *scope_table)
    : type_params(std::move(type_params)), type(std::move(type)),
      ir_decl(nullptr), scope_table(scope_table) {}

ImplStatement::ImplStatement(const SourceLocation &loc,
                             std::shared_ptr<ImplHeader> header,
                             ScopeTable<Symbol> *fn_scope, StatementList body)
    : Statement(loc), header(std::move(header)), body(std::move(body)),
      type_scope(scopeTableFromFormalTypeParams(this->header->type_params)),
      fn_scope(fn_scope) {
  assert(this->fn_scope->getImpl() == this->header);
}

// Random hash values for each type -- for each type, its hash is its
// random value combined with the value of its subtree
const std::size_t int_hash = 15653828285635008617ULL;
const std::size_t uint_hash = 18039289428081990911ULL;
const std::size_t float_hash = 12766205717137727273ULL;
const std::size_t bool_hash = 6813291328063854507ULL;
const std::size_t void_hash = 11591015837194168803ULL;
const std::size_t mut_hash = 12155074924612969154ULL;
const std::size_t pointer_hash = 2799149878986810139ULL;
const std::size_t function_hash = 632868945383080845ULL;
const std::size_t tuple_hash = 11072401693384440110ULL;
const std::size_t struct_hash = 18441784286747023263ULL;
const std::size_t formal_type_param_hash = 8481408178564299622ULL;
const std::size_t hash_prime = 31;

std::size_t Type::hash() const {
  // Type shouldn't be hashed
  assert(false);
}

std::size_t FormalTypeParameter::hash() const {
  return formal_type_param_hash + id;
}

std::size_t VoidType::hash() const { return void_hash; }

std::size_t BoolType::hash() const { return bool_hash; }

std::size_t IntType::hash() const {
  return (int_hash * (size - 1)) ^ (isUnsigned ? uint_hash : 0);
}

std::size_t FloatType::hash() const { return float_hash * (size - 1); }

std::size_t MutType::hash() const {
  return mut_hash + type->hash() * hash_prime;
}

std::size_t PointerType::hash() const {
  return pointer_hash + type->hash() * hash_prime;
}

std::size_t FunctionType::hash() const {
  std::size_t res = 0;
  for (const auto &arg : argTypes) {
    res += arg->hash();
    res *= hash_prime;
  }
  res += retType->hash();
  res *= hash_prime;
  res += function_hash;

  return res;
}

std::size_t TupleType::hash() const {
  std::size_t res = 0;
  for (const auto &field : types) {
    res += field->hash();
    res *= hash_prime;
  }
  res += tuple_hash;

  return res;
}

std::size_t StructType::hash() const {
  std::size_t res = 0;
  for (const auto &param : actual_generic_params) {
    res += param->hash();
    res *= hash_prime;
  }
  // add struct name to hash
  std::string name = scopedNameToString(type_alias->getFullyScopedName());
  res += std::hash<std::string>{}(name);
  res *= hash_prime;
  // go through struct's ancestor symbol tables and add package versioning to
  // hash
  auto cur = type_alias->parent_table;
  while (cur != nullptr && cur->getImpl() == nullptr) {
    if (cur->getVersionInt() != -1) {
      res += cur->getVersionInt();
      res *= hash_prime;
    }
    cur = cur->getParent();
  }

  res += struct_hash;

  return res;
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

ActiveScopes::ActiveScopes() : names(), types() {}

std::vector<ScopeComponent> Symbol::getFullyScopedName() const {
  size_t scopes_size = 1;
  auto tmp = parent_table;
  while (tmp != nullptr &&
         (!tmp->getName().empty() || tmp->getImpl() != nullptr)) {
    scopes_size++;
    if (tmp->getImpl() == nullptr) {
      tmp = tmp->getParent();
    } else {
      tmp = nullptr;
    }
  }

  std::vector<ScopeComponent> fullName(scopes_size, ScopeComponent(""));

  fullName[scopes_size - 1] = name;
  tmp = parent_table;
  while (tmp != nullptr &&
         (!tmp->getName().empty() || tmp->getImpl() != nullptr)) {
    scopes_size--;
    if (tmp->getImpl() == nullptr) {
      fullName[scopes_size - 1] = tmp->getName();
      tmp = tmp->getParent();
    } else {
      fullName[scopes_size - 1] = tmp->getImpl();
      tmp = nullptr;
    }
  }

  return fullName;
}

std::vector<ScopeComponent> TypeAlias::getFullyScopedName() const {
  size_t scopes_size = 1;
  auto tmp = parent_table;
  while (tmp != nullptr &&
         (!tmp->getName().empty() || tmp->getImpl() != nullptr)) {
    scopes_size++;
    if (tmp->getImpl() == nullptr) {
      tmp = tmp->getParent();
    } else {
      tmp = nullptr;
    }
  }

  std::vector<ScopeComponent> fullName(scopes_size, ScopeComponent(""));

  fullName[scopes_size - 1] = name;
  tmp = parent_table;
  while (tmp != nullptr &&
         (!tmp->getName().empty() || tmp->getImpl() != nullptr)) {
    scopes_size--;
    if (tmp->getImpl() == nullptr) {
      fullName[scopes_size - 1] = tmp->getName();
      tmp = tmp->getParent();
    } else {
      fullName[scopes_size - 1] = tmp->getImpl();
      tmp = nullptr;
    }
  }

  return fullName;
}

std::string scopedNameToString(const std::vector<ScopeComponent> &scopes) {
  std::string res;
  for (size_t i = 0; i < scopes.size(); i++) {
    if (scopes[i].is_ident) {
      res.append(scopes[i].ident);
    } else {
      res.push_back('[');
      res.append(ast::TypePrinter().getType(*scopes[i].impl->type));
      res.push_back(']');
    }
    if (i < scopes.size() - 1) {
      res.push_back(':');
    }
  }

  return res;
}

} // namespace ovid