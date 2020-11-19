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
  // actual params on which the type was constructed
  const TypeList &actual_params;
  // the constructed type to substitute
  const std::shared_ptr<StructType> &constructed_type;

  explicit TypeConstructorStructVisit(
      const TypeAlias *type_alias, const TypeList &actual_params,
      const std::shared_ptr<StructType> &constructed_type)
      : type_alias(type_alias), actual_params(actual_params),
        constructed_type(constructed_type){};
};

class TypeConstructorState {
public:
  // FormalTypeParameter's to replace
  const FormalTypeParameterList &formal_params;
  // Types to replace them with
  const TypeList &actual_params;
  // map of formal type parameter id's to actual params
  std::unordered_map<uint64_t, std::shared_ptr<Type>> param_map;

  TypeConstructorState(const FormalTypeParameterList &formal_params,
                       const TypeList &actual_params);
};

/* Generic type construction pass
 * The pass visits TypeConstructors and replaces FormalTypeParameter's with
 * concrete types */
class TypeConstructorPass {
  ActiveScopes &scopes;
  ErrorManager &errorMan;
  const std::vector<std::string> &package;
  const std::vector<std::string> &current_module;
  // stack of visited struct type's type aliases
  // used to detect cycles in the type graph and resolve them
  // structs are the only types that can form cycles
  std::vector<TypeConstructorStructVisit> visited_structs;

  TypeConstructorPassResult<Type>
  visitFormalTypeParameter(const std::shared_ptr<FormalTypeParameter> &type,
                           const TypeConstructorState &state);
  TypeConstructorPassResult<MutType>
  visitMutType(const std::shared_ptr<MutType> &type,
               const TypeConstructorState &state);
  TypeConstructorPassResult<PointerType>
  visitPointerType(const std::shared_ptr<PointerType> &type,
                   const TypeConstructorState &state);
  TypeConstructorPassResult<FunctionType>
  visitFunctionType(const std::shared_ptr<FunctionType> &type,
                    const TypeConstructorState &state);
  TypeConstructorPassResult<TupleType>
  visitTupleType(const std::shared_ptr<TupleType> &type,
                 const TypeConstructorState &state);
  TypeConstructorPassResult<StructType>
  visitStructType(const std::shared_ptr<StructType> &type,
                  const TypeConstructorState &state);
  TypeConstructorPassResult<Type>
  visitUnresolvedType(const std::shared_ptr<UnresolvedType> &type,
                      const TypeConstructorState &state);

  TypeConstructorPassResult<Type> visitType(const std::shared_ptr<Type> &type,
                                            const TypeConstructorState &state);

  // check if a StructType is in visited_structs and that it's
  // actual_generic_params match formal_params
  const TypeConstructorStructVisit *
  structTypeInVisitedStructs(const StructType &type,
                             const TypeConstructorState &state,
                             const TypeList &actual_generic_params);

  // visit a type constructor and resolve UnresolvedTypes inside of it
  // does not apply the constructor
  std::shared_ptr<TypeConstructor> genericResolveTypeConstructor(
      const std::shared_ptr<TypeConstructor> &type_construct);

  // lookup unresolved type in type tables
  std::shared_ptr<TypeAlias>
  lookupUnresolvedType(const std::shared_ptr<UnresolvedType> &type);

public:
  // visit a type, replacing formal_params with actual_params
  // type is not mutated
  std::shared_ptr<Type>
  constructType(const std::shared_ptr<Type> &type,
                const FormalTypeParameterList &formal_params,
                const TypeList &actual_params);

  static std::shared_ptr<Type>
  resolveType(const std::shared_ptr<Type> &type, ActiveScopes &scopes,
              ErrorManager &errorMan, const std::vector<std::string> &package,
              const std::vector<std::string> &current_module);

  TypeConstructorPass(ActiveScopes &scopes, ErrorManager &errorMan,
                      const std::vector<std::string> &package,
                      const std::vector<std::string> &current_module);
};

} // namespace ovid::ast

#endif