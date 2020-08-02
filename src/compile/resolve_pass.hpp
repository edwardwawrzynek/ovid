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

class TypeResolver {
  ActiveScopes &scopes;
  ErrorManager &errorMan;

  std::shared_ptr<Type>
  visitUnresolvedType(std::shared_ptr<UnresolvedType> type,
                      const TypeResolverState &state);

  std::shared_ptr<MutType> visitMutType(std::shared_ptr<MutType> type,
                                        const TypeResolverState &state);
  std::shared_ptr<PointerType>
  visitPointerType(std::shared_ptr<PointerType> type,
                   const TypeResolverState &state);

  std::shared_ptr<FunctionType>
  visitFunctionType(std::shared_ptr<FunctionType> type,
                    const TypeResolverState &state);

  std::shared_ptr<TupleType> visitTupleType(std::shared_ptr<TupleType> type,
                                            const TypeResolverState &state);
  std::shared_ptr<Type> visitStructType(std::shared_ptr<StructType> type,
                                        const TypeResolverState &state);

public:
  std::shared_ptr<Type> visitType(const std::shared_ptr<Type> &type,
                                  const TypeResolverState &state);

  std::shared_ptr<NamedFunctionType>
  visitNamedFunctionType(std::shared_ptr<NamedFunctionType> type,
                         const TypeResolverState &state);

  TypeResolver(ActiveScopes &scopes, ErrorManager &errorMan);
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
  int visitNativeFunctionDecl(NativeFunctionDecl &node,
                              const ResolvePassState &state) override;
  int visitModuleDecl(ModuleDecl &node, const ResolvePassState &state) override;
  int visitIfStatement(IfStatement &node,
                       const ResolvePassState &state) override;
  int visitWhileStatement(WhileStatement &node,
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