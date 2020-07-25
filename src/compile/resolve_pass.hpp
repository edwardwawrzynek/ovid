#ifndef H_RESOLVE_PASS_INCL
#define H_RESOLVE_PASS_INCL

#include "ast_visitor.hpp"

/*
 * The name resolution pass on the AST
 * the pass visits each ast node, and resolves all identifiers to references to
 * entries in the symbol tables
 * TODO: simplify some syntactical sugar
 */

namespace ovid::ast {

class TypeResolverState {
public:
  const std::vector<std::string> &package;
  const std::vector<std::string> &current_module;

  TypeResolverState(const std::vector<std::string> &package,
                    const std::vector<std::string> &current_module)
      : package(package), current_module(current_module){};
};

class TypeResolver
    : public BaseTypeVisitor<std::shared_ptr<Type>, TypeResolverState> {

  ActiveScopes &scopes;
  ErrorManager &errorMan;

  std::shared_ptr<Type>
  visitUnresolvedType(UnresolvedType &type,
                      const TypeResolverState &state) override;

  std::shared_ptr<Type> visitVoidType(VoidType &type,
                                      const TypeResolverState &state) override;
  std::shared_ptr<Type> visitBoolType(BoolType &type,
                                      const TypeResolverState &state) override;
  std::shared_ptr<Type> visitIntType(IntType &type,
                                     const TypeResolverState &state) override;
  std::shared_ptr<Type> visitFloatType(FloatType &type,
                                       const TypeResolverState &state) override;

  std::shared_ptr<Type> visitMutType(MutType &type,
                                     const TypeResolverState &state) override;
  std::shared_ptr<Type>
  visitPointerType(PointerType &type, const TypeResolverState &state) override;

  std::shared_ptr<Type>
  visitFunctionType(FunctionType &type,
                    const TypeResolverState &state) override;
  std::shared_ptr<Type>
  visitNamedFunctionType(NamedFunctionType &type,
                         const TypeResolverState &state) override;

  std::shared_ptr<Type> visitTupleType(TupleType &type,
                                       const TypeResolverState &state) override;

public:
  TypeResolver(ActiveScopes &scopes, ErrorManager &errorMan);

  // function must be non overloaded to get the right return type (FunctionType,
  // not just the generic Type
  std::shared_ptr<FunctionType>
  visitFunctionTypeNonOverload(FunctionType &type,
                               const TypeResolverState &state);

  std::shared_ptr<NamedFunctionType>
  visitNamedFunctionTypeNonOverload(NamedFunctionType &type,
                                    const TypeResolverState &state);
};

class ResolvePassState {};

// the resolution pass doesn't have a type, it just modifies the ast, so int is
// used as a stand in
class ResolvePass : public BaseASTVisitor<int, ResolvePassState> {
  ErrorManager &errorMan;
  ActiveScopes &scopes;
  const std::vector<std::string> &package;
  std::vector<std::string> current_module;
  bool is_in_global;
  TypeResolver type_resolver;

  int visitVarDecl(VarDecl &node, const ResolvePassState &state) override;
  int visitFunctionDecl(FunctionDecl &node,
                        const ResolvePassState &state) override;
  int visitModuleDecl(ModuleDecl &node, const ResolvePassState &state) override;
  int visitIfStatement(IfStatement &node,
                       const ResolvePassState &state) override;
  int visitReturnStatement(ReturnStatement &node,
                           const ResolvePassState &state) override;

  int visitFunctionCall(FunctionCall &node,
                        const ResolvePassState &state) override;
  int visitIdentifier(Identifier &node, const ResolvePassState &state) override;
  int visitAssignment(Assignment &node, const ResolvePassState &state) override;
  int visitTuple(Tuple &node, const ResolvePassState &state) override;

  int visitTypeAliasDecl(TypeAliasDecl &node,
                         const ResolvePassState &state) override;
  int visitFieldAccess(FieldAccess &node,
                       const ResolvePassState &state) override;

  // check if a variable in the current scope is shadowed, and print an error if
  // it is. name is the name of the variable, predicate is passed to
  // ActiveScope.findSymbol
  bool checkShadowed(const SourceLocation &pos, const std::string &name,
                     std::function<bool(const Symbol &)> predicate,
                     bool is_arg);

  // check if a type in the current type scope is shadowed, and print an error
  // if it is. name is the name of the variable, predicate is passed to
  // ActiveScope.findSymbol
  bool checkTypeShadowed(const SourceLocation &pos, const std::string &name,
                         std::function<bool(const TypeAlias &)> predicate);

public:
  ResolvePass(ActiveScopes &scopes, ErrorManager &errorMan,
              const std::vector<std::string> &package);

  void removePushedPackageScope();
};

} // namespace ovid::ast

#endif