#ifndef H_RESOLVE_PASS_INCL
#define H_RESOLVE_PASS_INCL

#include "ast_visitor.hpp"

/*
 * The name resolution pass on the AST
 * the pass visits each ast node, and resolves all identifiers to references to
 * entries in the symbol tables
 * TODO: Additionally, it fully resolves all of the record type's internals, so
 * that they can be fully specified
 */

namespace ovid::ast {

class ResolvePassState {};

// the resolution pass doesn't have a type, it just modifies the ast
class ResolvePass : public BaseASTVisitor<int, ResolvePassState> {
  ErrorManager &errorMan;
  ActiveScopes &scopes;
  const std::vector<std::string> &package;

  int visitVarDecl(VarDecl &node, const ResolvePassState &state) override;
  int visitFunctionDecl(FunctionDecl &node,
                        const ResolvePassState &state) override;
  int visitModuleDecl(ModuleDecl &node, const ResolvePassState &state) override;

  int visitFunctionCall(FunctionCall &node,
                        const ResolvePassState &state) override;
  int visitIdentifier(Identifier &node, const ResolvePassState &state) override;
  int visitOperatorSymbol(OperatorSymbol &node,
                          const ResolvePassState &state) override;
  int visitAssignment(Assignment &node, const ResolvePassState &state) override;
  int visitIntLiteral(IntLiteral &node, const ResolvePassState &state) override;
  int visitTuple(Tuple &node, const ResolvePassState &state) override;

public:
  ResolvePass(ActiveScopes &scopes, ErrorManager &errorMan,
              const std::vector<std::string> &package);

  ~ResolvePass() override;
};

} // namespace ovid::ast

#endif