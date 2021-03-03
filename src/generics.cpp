#include "generics.hpp"

namespace ovid::ast {

TypeConstructorState::TypeConstructorState(
    const FormalTypeParameterList &formal_params, const TypeList &actual_params,
    ActiveScopes &scopes, ScopeTable<TypeAlias> *current_module)
    : formal_params(formal_params), actual_params(actual_params), param_map(),
      scopes(scopes), current_module(current_module) {
  assert(formal_params.size() == actual_params.size());
  for (size_t i = 0; i < formal_params.size(); i++) {
    param_map.emplace(this->formal_params[i]->id, this->actual_params[i]);
  }
}

TypeConstructorState TypeConstructorState::withParams(
    const FormalTypeParameterList &new_formal_params,
    const TypeList &new_actual_params) const {
  return TypeConstructorState(new_formal_params, new_actual_params,
                              this->scopes, this->current_module);
}

TypeConstructorPass::TypeConstructorPass(
    ErrorManager &errorMan, const std::vector<std::string> &package)
    : errorMan(errorMan), package(package) {}

TypeConstructorPassResult<Type> TypeConstructorPass::visitFormalTypeParameter(
    const std::shared_ptr<FormalTypeParameter> &type,
    const TypeConstructorState &state) {
  // check if param is bound in this construction
  if (state.param_map.count(type->id) > 0) {
    return TypeConstructorPassResult(true, state.param_map.at(type->id));
  } else {
    return TypeConstructorPassResult<Type>(false, type);
  }
}

TypeConstructorPassResult<MutType>
TypeConstructorPass::visitMutType(const std::shared_ptr<MutType> &type,
                                  const TypeConstructorState &state) {
  auto inner = visitType(type->type, state);
  if (inner.had_formal_params) {
    return TypeConstructorPassResult(
        true, std::make_shared<MutType>(type->loc, inner.actual_type));
  } else {
    type->type = inner.actual_type;
    return TypeConstructorPassResult(false, type);
  }
}

TypeConstructorPassResult<PointerType>
TypeConstructorPass::visitPointerType(const std::shared_ptr<PointerType> &type,
                                      const TypeConstructorState &state) {
  auto inner = visitType(type->type, state);
  if (inner.had_formal_params) {
    return TypeConstructorPassResult(
        true, std::make_shared<PointerType>(type->loc, inner.actual_type));
  } else {
    type->type = inner.actual_type;
    return TypeConstructorPassResult(false, type);
  }
}

TypeConstructorPassResult<FunctionType> TypeConstructorPass::visitFunctionType(
    const std::shared_ptr<FunctionType> &type,
    const TypeConstructorState &state) {
  bool has_formal_params = false;
  TypeList args;

  for (auto &arg : type->argTypes) {
    auto constructed = visitType(arg, state);
    args.push_back(constructed.actual_type);
    if (constructed.had_formal_params)
      has_formal_params = true;
  }

  auto ret = visitType(type->retType, state);
  if (ret.had_formal_params)
    has_formal_params = true;

  if (has_formal_params) {
    std::shared_ptr<FunctionType> result;
    // if type is a NamedFunctionType, result should be too
    auto namedFType = dynamic_cast<NamedFunctionType *>(type.get());
    if (namedFType != nullptr) {
      return TypeConstructorPassResult<FunctionType>(
          true, std::make_shared<NamedFunctionType>(
                    type->loc, std::move(args), std::move(ret.actual_type),
                    namedFType->argNames, namedFType->is_self_func));
    } else {
      return TypeConstructorPassResult(
          true, std::make_shared<FunctionType>(type->loc, std::move(args),
                                               std::move(ret.actual_type)));
    }
  } else {
    for (size_t i = 0; i < args.size(); i++) {
      type->argTypes[i] = args[i];
    }
    type->retType = ret.actual_type;
    return TypeConstructorPassResult(false, type);
  }
}

TypeConstructorPassResult<TupleType>
TypeConstructorPass::visitTupleType(const std::shared_ptr<TupleType> &type,
                                    const TypeConstructorState &state) {
  TypeList fields;
  bool had_formal_params = false;
  for (auto &field : type->types) {
    auto resolved = visitType(field, state);
    fields.push_back(resolved.actual_type);
    if (resolved.had_formal_params)
      had_formal_params = true;
  }

  if (had_formal_params) {
    return TypeConstructorPassResult(
        true, std::make_shared<TupleType>(type->loc, std::move(fields)));
  } else {
    for (size_t i = 0; i < fields.size(); i++) {
      type->types[i] = fields[i];
    }
    return TypeConstructorPassResult(false, type);
  }
}

TypeConstructorPassResult<StructType>
TypeConstructorPass::visitStructType(const std::shared_ptr<StructType> &type,
                                     const TypeConstructorState &state) {
  ast::TypeList actual_generic_params;
  if (type->constructed) {
    bool had_formal_params = false;
    // if type is already constructed (ie field of other type), then its
    // actual_generic_params is already set. Some of those actual params may be
    // our formal parameters, so we need to substitute them if present
    for (auto &param : type->actual_generic_params) {
      auto formal_param = dynamic_cast<FormalTypeParameter *>(param.get());
      // check if param is bound in this construction
      if (formal_param != nullptr &&
          state.param_map.count(formal_param->id) > 0) {
        actual_generic_params.push_back(state.param_map.at(formal_param->id));
        had_formal_params = true;
      } else {
        actual_generic_params.push_back(param);
      }
    }
    // type has already been constructed and didn't have any formal params that
    // we are replacing, so we don't have to reconstruct
    if (!had_formal_params) {
      return TypeConstructorPassResult(false, type);
    }
  } else {
    actual_generic_params = state.actual_params;
  }

  // in order to break struct type cycles, we need to make sure that we don't
  // recursively visit a field that we are in the process of resolving
  auto visited =
      structTypeInVisitedStructs(*type, state, actual_generic_params);
  if (visited != nullptr) {
    return TypeConstructorPassResult(true, visited->constructed_type);
  }
  // construct result type (without field_types or actual_generic_params set)
  auto res_type = std::make_shared<StructType>(
      type->loc, TypeList(), type->field_names, type->fields_are_public,
      type->type_alias, true, TypeList());
  // resolve field_types
  visited_structs.emplace_back(type->getTypeAlias(), actual_generic_params,
                               res_type);
  ast::TypeList field_types;
  for (auto &field : type->field_types) {
    field_types.push_back(visitType(field, state).actual_type);
  }
  visited_structs.pop_back();

  res_type->field_types = std::move(field_types);
  res_type->actual_generic_params = std::move(actual_generic_params);
  return TypeConstructorPassResult(true, std::move(res_type));
}

std::shared_ptr<TypeAlias> TypeConstructorPass::lookupUnresolvedType(
    const std::shared_ptr<UnresolvedType> &type,
    const TypeConstructorState &state) {
  // lookup type in type tables
  std::shared_ptr<TypeAlias> sym;
  if (type->is_root_scoped) {
    sym =
        state.scopes.types.getRootScope()->findSymbol(type->scopes, type->name);
  } else {
    sym = state.scopes.types.findSymbol(type->scopes, type->name);
  }
  auto scopedName = scopesAndNameToString(type->scopes, type->name, true);

  if (sym == nullptr) {
    errorMan.logError(string_format("use of undeclared type `\x1b[1m%s\x1b[m`",
                                    scopedName.c_str()),
                      type->loc, ErrorType::UndeclaredType);
    return nullptr;
  }
  // check for use of private type
  else if (!checkVisible(
               *sym, state.scopes.types.getRootScope()->getScopeTable(package),
               state.current_module, sym->is_public)) {
    errorMan.logError(string_format("use of private type `\x1b[1m%s\x1b[m`",
                                    scopedName.c_str()),
                      type->loc, ErrorType::UseOfPrivateType);
  }
  return sym;
}

std::shared_ptr<TypeConstructor>
TypeConstructorPass::genericSubsTypeConstructor(
    const std::shared_ptr<TypeConstructor> &type_construct,
    const FormalTypeParameterList &formal_params, const TypeList &actual_params,
    const TypeConstructorState &state) {
  auto new_state = state.withParams(formal_params, actual_params);
  // resolve type
  state.scopes.types.pushScope(type_construct->getFormalScopeTable());
  auto res = visitType(type_construct->getFormalBoundType(), new_state);
  state.scopes.types.popScope();
  auto new_construct = std::make_shared<GenericTypeConstructor>(
      type_construct->loc, type_construct->getFormalTypeParameters(),
      res.actual_type);

  return new_construct;
}

std::shared_ptr<TypeConstructor>
TypeConstructorPass::genericResolveTypeConstructor(
    const std::shared_ptr<TypeConstructor> &type_construct,
    const TypeConstructorState &state) {
  // substitute formal -> formal
  auto formal_params = type_construct->getFormalTypeParameters();
  auto &actual_params = (const TypeList &)formal_params;

  return genericSubsTypeConstructor(type_construct, formal_params,
                                    actual_params, state);
}

ActiveScopes
TypeConstructorPass::scopesForAlias(TypeAlias *alias,
                                    const TypeConstructorState &state) {
  auto res = ActiveScopes();
  // construct the chain of ScopeTables for alias
  std::vector<ScopeTable<TypeAlias> *> tables;
  auto cur = alias->parent_table;
  while (cur != nullptr) {
    tables.push_back(cur);
    cur = cur->getParent();
  }
  // push tables backwards onto scope stack
  for (size_t i = tables.size(); i-- > 0;) {
    res.types.pushScope(tables[i]);
  }

  // go through state's scopes, and push any that have is_func_scope set
  for (int i = 0; i < state.scopes.types.getNumActiveScopes(); i++) {
    auto scope = state.scopes.types.getNthScope(i);
    if (scope->getIsFuncScope()) {
      assert(scope->getName().empty());
      res.types.pushScope(scope);
    }
  }

  return res;
}

TypeConstructorPassResult<Type> TypeConstructorPass::visitUnresolvedType(
    const std::shared_ptr<UnresolvedType> &type,
    const TypeConstructorState &state) {
  auto sym = lookupUnresolvedType(type, state);
  auto scopedName = scopesAndNameToString(type->scopes, type->name, true);
  if (sym == nullptr) {
    return TypeConstructorPassResult<Type>(false, nullptr);
  }

  auto type_construct = sym->type;
  auto params_required = type_construct->numTypeParams();
  if (type->type_params.size() != params_required) {
    errorMan.logError(
        string_format("invalid number of parameters for type constructor "
                      "`\x1b[1m%s\x1b[m` (expected %zu, found %zu)",
                      scopedName.c_str(), params_required,
                      type->type_params.size()),
        type->loc, ErrorType::TypeError);
    return TypeConstructorPassResult<Type>(false, nullptr);
  }

  // if constructor is 0 param and result is cached, use that
  if (params_required == 0 && sym->inner_resolved) {
    auto res = std::dynamic_pointer_cast<Type>(sym->type->noParamConstruct());
    assert(res != nullptr);
    return TypeConstructorPassResult(false, res);
  }

  // the internal type of the type alias (ie type_construct) may have
  // UnresolvedTypes in it. They need to be resolved in the scopes at the
  // alias's declaration location, so we need to generate an ActiveScopes that
  // matches the alias's declaration location
  auto new_scopes = scopesForAlias(sym.get(), state);
  auto newly_scoped_state = TypeConstructorState(
      state.formal_params, state.actual_params, new_scopes, sym->parent_table);

  // if constructor isn't 0 param, then visit it with a formal -> formal
  // param substitution
  // this just resolves UnresolvedTypes and speeds up future constructions
  if (params_required != 0 && !sym->inner_resolved &&
      type_construct->getFormalBoundType() != nullptr) {
    sym->inner_resolved = true;
    sym->type =
        genericResolveTypeConstructor(type_construct, newly_scoped_state);
    type_construct = sym->type;
  }

  bool had_formal_params = false;
  // resolve type parameters
  TypeList resolved_type_params;
  for (auto &param : type->type_params) {
    auto resolved = visitType(param, state);
    resolved_type_params.push_back(resolved.actual_type);
    if (resolved.had_formal_params)
      had_formal_params = true;
  }

  TypeConstructorPassResult<Type> result_type(false, nullptr);

  // check for trivial constructor and use it if available
  auto trivial = type_construct->trivialConstruct();
  if (trivial == nullptr) {
    // call constructor with new formal + actual params
    auto new_state = newly_scoped_state.withParams(
        type_construct->getFormalTypeParameters(), resolved_type_params);
    state.scopes.types.pushScope(type_construct->getFormalScopeTable());
    result_type = visitType(type_construct->getFormalBoundType(), new_state);
    state.scopes.types.popScope();
  } else {
    result_type = visitType(trivial, newly_scoped_state);
  }

  // mark the alias as inner_resolved and cache (if 0 parameter)
  if (params_required == 0 && !had_formal_params &&
      !result_type.had_formal_params) {
    sym->inner_resolved = true;
    sym->type = result_type.actual_type;
  }

  return TypeConstructorPassResult<Type>(true, result_type.actual_type);
}

#define TYPE_CONSTRUCTOR_VISIT_CASE(caseType, caseFunction)                    \
  if (dynamic_cast<caseType *>(type.get()) != nullptr) {                       \
    auto res = caseFunction(std::dynamic_pointer_cast<caseType>(type), state); \
    return TypeConstructorPassResult<Type>(res.had_formal_params,              \
                                           res.actual_type);                   \
  }

TypeConstructorPassResult<Type>
TypeConstructorPass::visitType(const std::shared_ptr<Type> &type,
                               const TypeConstructorState &state) {
  TYPE_CONSTRUCTOR_VISIT_CASE(FormalTypeParameter, visitFormalTypeParameter);
  TYPE_CONSTRUCTOR_VISIT_CASE(MutType, visitMutType);
  TYPE_CONSTRUCTOR_VISIT_CASE(PointerType, visitPointerType);
  TYPE_CONSTRUCTOR_VISIT_CASE(FunctionType, visitFunctionType);
  TYPE_CONSTRUCTOR_VISIT_CASE(TupleType, visitTupleType);
  TYPE_CONSTRUCTOR_VISIT_CASE(StructType, visitStructType);
  TYPE_CONSTRUCTOR_VISIT_CASE(UnresolvedType, visitUnresolvedType);

  return TypeConstructorPassResult(false, type);
}

const TypeConstructorStructVisit *
TypeConstructorPass::structTypeInVisitedStructs(
    const StructType &type, const TypeConstructorState &state,
    const TypeList &actual_generic_params) {
  for (size_t i = visited_structs.size(); i-- > 0;) {
    auto &visited = visited_structs[i];

    if (type.getTypeAlias() != visited.type_alias)
      continue;
    if (typeListEqual(actual_generic_params, visited.actual_params))
      return &visited;
  }

  return nullptr;
}

std::shared_ptr<Type> TypeConstructorPass::constructType(
    const std::shared_ptr<Type> &type,
    const FormalTypeParameterList &formal_params, const TypeList &actual_params,
    ActiveScopes &scopes, ErrorManager &errorMan,
    const std::vector<std::string> &package,
    const std::vector<std::string> &current_module) {
  auto pass = TypeConstructorPass(errorMan, package);
  return pass
      .visitType(
          type, TypeConstructorState(
                    formal_params, actual_params, scopes,
                    scopes.types.getRootScope()->getScopeTable(current_module)))
      .actual_type;
}

std::shared_ptr<TypeConstructor> TypeConstructorPass::subsOnTypeConstructor(
    const std::shared_ptr<TypeConstructor> &type_construct,
    const FormalTypeParameterList &formal_params, const TypeList &actual_params,
    ActiveScopes &scopes, ErrorManager &errorMan,
    const std::vector<std::string> &package,
    const std::vector<std::string> &current_module) {
  auto pass = TypeConstructorPass(errorMan, package);
  return pass.genericSubsTypeConstructor(
      type_construct, formal_params, actual_params,
      TypeConstructorState(
          formal_params, actual_params, scopes,
          scopes.types.getRootScope()->getScopeTable(current_module)));
}

std::shared_ptr<Type> TypeConstructorPass::constructTypeConstructor(
    const std::shared_ptr<TypeConstructor> &type_construct,
    const TypeList &actual_params, ActiveScopes &scopes, ErrorManager &errorMan,
    const std::vector<std::string> &package,
    const std::vector<std::string> &current_module) {
  return constructType(type_construct->getFormalBoundType(),
                       type_construct->getFormalTypeParameters(), actual_params,
                       scopes, errorMan, package, current_module);
}

std::shared_ptr<Type> TypeConstructorPass::resolveType(
    const std::shared_ptr<Type> &type, ActiveScopes &scopes,
    ErrorManager &errorMan, const std::vector<std::string> &package,
    const std::vector<std::string> &current_module) {
  return constructType(type, FormalTypeParameterList(), TypeList(), scopes,
                       errorMan, package, current_module);
}

std::shared_ptr<TypeConstructor> TypeConstructorPass::resolveTypeConstructor(
    const std::shared_ptr<TypeConstructor> &type_construct,
    ActiveScopes &scopes, ErrorManager &errorMan,
    const std::vector<std::string> &package,
    const std::vector<std::string> &current_module) {
  auto constructor = TypeConstructorPass(errorMan, package);
  return constructor.genericResolveTypeConstructor(
      type_construct,
      TypeConstructorState(
          FormalTypeParameterList(), TypeList(), scopes,
          scopes.types.getRootScope()->getScopeTable(current_module)));
}

bool typeListEqual(const TypeList &lhs, const TypeList &rhs) {
  if (lhs.size() != rhs.size())
    return false;

  for (size_t i = 0; i < lhs.size(); i++) {
    if (!lhs[i]->equalStrict(*rhs[i]))
      return false;
  }

  return true;
}

} // namespace ovid::ast