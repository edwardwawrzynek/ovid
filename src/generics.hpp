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

public:
  // visit a type, replacing formal_params with actual_params
  // type is not mutated
  std::shared_ptr<Type> constructType(const std::shared_ptr<Type> &type);

  TypeConstructorPass(const FormalTypeParameterList &formal_params,
                      const TypeList &actual_params);
};

} // namespace ovid::ast

#endif