#include "generics.hpp"

namespace ovid::ast {

TypeConstructorPass::TypeConstructorPass(
    const FormalTypeParameterList &formal_params, const TypeList &actual_params)
    : formal_params(formal_params), actual_params(actual_params), param_map(),
      visited_structs() {
  assert(formal_params.size() == actual_params.size());
  for (size_t i = 0; i < formal_params.size(); i++) {
    param_map.emplace(this->formal_params[i]->id, this->actual_params[i]);
  }
}

TypeConstructorPassResult<Type> TypeConstructorPass::visitFormalTypeParameter(
    const std::shared_ptr<FormalTypeParameter> &type) {
  // check if param is bound in this construction
  if (param_map.count(type->id) > 0) {
    return TypeConstructorPassResult(true, param_map[type->id]);
  } else {
    return TypeConstructorPassResult<Type>(false, type);
  }
}

TypeConstructorPassResult<MutType>
TypeConstructorPass::visitMutType(const std::shared_ptr<MutType> &type) {
  auto inner = visitType(type->type);
  if (inner.had_formal_params) {
    return TypeConstructorPassResult(
        true, std::make_shared<MutType>(type->loc, inner.actual_type));
  } else {
    return TypeConstructorPassResult(false, type);
  }
}

TypeConstructorPassResult<PointerType> TypeConstructorPass::visitPointerType(
    const std::shared_ptr<PointerType> &type) {
  auto inner = visitType(type->type);
  if (inner.had_formal_params) {
    return TypeConstructorPassResult(
        true, std::make_shared<PointerType>(type->loc, inner.actual_type));
  } else {
    return TypeConstructorPassResult(false, type);
  }
}

TypeConstructorPassResult<FunctionType> TypeConstructorPass::visitFunctionType(
    const std::shared_ptr<FunctionType> &type) {
  // TODO: handle NamedFunctionType
  // if the function type has formal parameters
  bool has_formal_params = false;
  TypeList args;

  for (auto &arg : type->argTypes) {
    auto constructed = visitType(arg);
    args.push_back(constructed.actual_type);
    if (constructed.had_formal_params)
      has_formal_params = true;
  }

  auto ret = visitType(type->retType);
  if (ret.had_formal_params)
    has_formal_params = true;

  if (has_formal_params) {
    return TypeConstructorPassResult(
        true, std::make_shared<FunctionType>(type->loc, std::move(args),
                                             std::move(ret.actual_type)));
  } else {
    return TypeConstructorPassResult(false, type);
  }
}

TypeConstructorPassResult<TupleType>
TypeConstructorPass::visitTupleType(const std::shared_ptr<TupleType> &type) {
  std::vector<TypeConstructorPassResult<Type>> inners;
  for (auto &field : type->types) {
    inners.push_back(visitType(field));
  }

  if (std::any_of(inners.begin(), inners.end(),
                  [](auto &res) { return res.had_formal_params; })) {
    std::vector<std::shared_ptr<Type>> field_types;
    for (auto &res : inners) {
      field_types.push_back(res.actual_type);
    }
    return TypeConstructorPassResult(
        true, std::make_shared<TupleType>(type->loc, std::move(field_types)));
  } else {
    return TypeConstructorPassResult(false, type);
  }
}

TypeConstructorPassResult<StructType>
TypeConstructorPass::visitStructType(const std::shared_ptr<StructType> &type) {
  assert(type->fields_resolved);

  ast::TypeList actual_generic_params;
  if (type->constructed) {
    // if type is already constructed (ie field of other type), then its
    // actual_generic_params is already set. Some of those actual params may be
    // our formal parameters, so we need to substitute them if present
    bool had_formal_params = false;
    for (auto &param : type->actual_generic_params) {
      auto formal_param = dynamic_cast<FormalTypeParameter *>(param.get());
      // check if param is bound in this construction
      if (formal_param != nullptr && param_map.count(formal_param->id) > 0) {
        actual_generic_params.push_back(param_map[formal_param->id]);
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
    actual_generic_params = actual_params;
  }

  // in order to break struct type cycles, we need to make sure that we don't
  // recursively visit a field that we are im the process of resolving
  auto visited = structTypeInVisitedStructs(*type);
  if (visited != nullptr) {
    return TypeConstructorPassResult(true, visited->constructed_type);
  }
  // construct result type (without field_types set)
  auto res_type = std::make_shared<StructType>(
      type->loc, TypeList(), type->field_names, type->fields_are_public,
      type->type_alias, type->fields_resolved, true,
      std::move(actual_generic_params));
  visited_structs.emplace_back(type->getTypeAlias(), res_type);

  ast::TypeList field_types;
  for (auto &field : type->field_types) {
    field_types.push_back(visitType(field).actual_type);
  }
  res_type->field_types = std::move(field_types);
  visited_structs.pop_back();

  return TypeConstructorPassResult(true, std::move(res_type));
}

#define TYPE_CONSTRUCTOR_VISIT_CASE(caseType, caseFunction)                    \
  if (dynamic_cast<caseType *>(type.get()) != nullptr) {                       \
    auto res = caseFunction(std::dynamic_pointer_cast<caseType>(type));        \
    return TypeConstructorPassResult<Type>(res.had_formal_params,              \
                                           res.actual_type);                   \
  }

TypeConstructorPassResult<Type>
TypeConstructorPass::visitType(const std::shared_ptr<Type> &type) {
  TYPE_CONSTRUCTOR_VISIT_CASE(FormalTypeParameter, visitFormalTypeParameter);
  TYPE_CONSTRUCTOR_VISIT_CASE(MutType, visitMutType);
  TYPE_CONSTRUCTOR_VISIT_CASE(PointerType, visitPointerType);
  TYPE_CONSTRUCTOR_VISIT_CASE(FunctionType, visitFunctionType);
  TYPE_CONSTRUCTOR_VISIT_CASE(TupleType, visitTupleType);
  TYPE_CONSTRUCTOR_VISIT_CASE(StructType, visitStructType);

  return TypeConstructorPassResult(false, type);
}

const TypeConstructorStructVisit *
TypeConstructorPass::structTypeInVisitedStructs(const StructType &type) {
  // if the type isn't bound over the same formal params as we are resolving,
  // then it can't form a cycle against any types in visited_structs
  if (!checkTypesMatchFormalTypes(type.actual_generic_params, formal_params))
    return nullptr;

  for (auto &visited : visited_structs) {
    if (visited.type_alias == type.type_alias)
      return &visited;
  }

  return nullptr;
}

bool TypeConstructorPass::checkTypesMatchFormalTypes(
    const TypeList &types, const FormalTypeParameterList &formal_types) {
  if (types.size() != formal_types.size())
    return false;
  for (size_t i = 0; i < types.size(); i++) {
    auto formal_type = dynamic_cast<FormalTypeParameter *>(types[i].get());
    if (formal_type == nullptr)
      return false;
    if (formal_type->id != formal_types[i]->id)
      return false;
  }

  return true;
}

std::shared_ptr<Type>
TypeConstructorPass::constructType(const std::shared_ptr<Type> &type) {
  return visitType(type).actual_type;
}
} // namespace ovid::ast