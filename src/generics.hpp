#ifndef H_GENERICS_INCL
#define H_GENERICS_INCL

#include "ast.hpp"

namespace ovid::ast {
/* the result of TypeConstructorPass's visit to a type */
template <class T> class TypeConstructorPassResult {
public:
  // whether or not the visited type contained FormalTypeParameter's
  bool had_formal_params;
  // if had_formal_params, the newly constructed type with actual params
  std::shared_ptr<T> actual_type;

  TypeConstructorPassResult(bool had_formal_params,
                            std::shared_ptr<T> actual_type)
      : had_formal_params(had_formal_params),
        actual_type(std::move(actual_type)){};
};

/* TypeConstructorPass builds a stack of TypeConstructorStructVisit records
 * they are used to detect cycles in the type graph and resolve them
 * since only StructType's can cause cycles, only struct visits are recorded
 */
class TypeConstructorStructVisit {
public:
  // struct's type alias entry
  const TypeAlias *type_alias;
  // the constructed type to substitute
  const std::shared_ptr<StructType> &constructed_type;

  explicit TypeConstructorStructVisit(
      const TypeAlias *type_alias,
      const std::shared_ptr<StructType> &constructed_type)
      : type_alias(type_alias), constructed_type(constructed_type){};
};

/* Generic type construction pass
 * The pass visits TypeConstructors and replaces FormalTypeParameter's with
 * concrete types */
class TypeConstructorPass {
  // FormalTypeParameter's to replace
  const FormalTypeParameterList &formal_params;
  // Types to replace them with
  const TypeList &actual_params;

  // map of formal type parameter id's to actual params
  std::unordered_map<uint64_t, std::shared_ptr<Type>> param_map;

  // stack of visited struct type's type aliases
  // used to detect cycles in the type graph and resolve them
  // structs are the only types that can form cycles
  std::vector<TypeConstructorStructVisit> visited_structs;

  TypeConstructorPassResult<Type>
  visitFormalTypeParameter(const std::shared_ptr<FormalTypeParameter> &type);

  TypeConstructorPassResult<MutType>
  visitMutType(const std::shared_ptr<MutType> &type);
  TypeConstructorPassResult<PointerType>
  visitPointerType(const std::shared_ptr<PointerType> &type);
  TypeConstructorPassResult<FunctionType>
  visitFunctionType(const std::shared_ptr<FunctionType> &type);
  TypeConstructorPassResult<TupleType>
  visitTupleType(const std::shared_ptr<TupleType> &type);
  TypeConstructorPassResult<StructType>
  visitStructType(const std::shared_ptr<StructType> &type);

  TypeConstructorPassResult<Type> visitType(const std::shared_ptr<Type> &type);

  // check if a TypeList consists of just FormalTypeParameters and matches a
  // FormalTypeParameterList
  static bool
  checkTypesMatchFormalTypes(const TypeList &types,
                             const FormalTypeParameterList &formal_types);

  // check if a StructType is in visited_structs and that it's
  // actual_generic_params match formal_params
  const TypeConstructorStructVisit *
  structTypeInVisitedStructs(const StructType &type);

public:
  // visit a type, replacing formal_params with actual_params
  // type is not mutated
  std::shared_ptr<Type> constructType(const std::shared_ptr<Type> &type);

  TypeConstructorPass(const FormalTypeParameterList &formal_params,
                      const TypeList &actual_params);
};

} // namespace ovid::ast

#endif