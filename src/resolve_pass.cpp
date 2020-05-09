#include "resolve_pass.hpp"

namespace ovid::ast {

int ResolvePass::visitVarDecl(VarDecl &node, const ResolvePassState &state) {}

int ResolvePass::visitFunctionDecl(FunctionDecl &node,
                                   const ResolvePassState &state) {
  // add this function's scope to the active scope stack
}

int ResolvePass::visitModuleDecl(ModuleDecl &node,
                                 const ResolvePassState &state) {
  // add this module's scope to active scope stack
  scopes.pushScopeByName(package, node.scope);

  // visit child nodes
  for (auto &child : node.body) {
    visitNode(*child, state);
  }

  // remove this module's scope from the active scope table
  scopes.popScopeByName(package, node.scope);
}

int ResolvePass::visitFunctionCall(FunctionCall &node,
                                   const ResolvePassState &state) {}

int ResolvePass::visitIdentifier(Identifier &node,
                                 const ResolvePassState &state) {}

int ResolvePass::visitOperatorSymbol(OperatorSymbol &node,
                                     const ResolvePassState &state) {}
int ResolvePass::visitAssignment(Assignment &node,
                                 const ResolvePassState &state) {}
int ResolvePass::visitIntLiteral(IntLiteral &node,
                                 const ResolvePassState &state) {}
int ResolvePass::visitTuple(Tuple &node, const ResolvePassState &state) {}

ResolvePass::ResolvePass(ActiveScopes &scopes, ErrorManager &errorMan,
                         const std::vector<std::string> &package)
    : BaseASTVisitor<int, ResolvePassState>(0), errorMan(errorMan),
      scopes(scopes), package(package) {
  // add package as scope table [1] in scope stack
  assert(scopes.names.getNumActiveScopes() == 1);
  assert(scopes.types.getNumActiveScopes() == 1);
  scopes.pushScopeByName(package);
}

ResolvePass::~ResolvePass() {
  // at this point, just the root table + package table are on the stack
  assert(scopes.names.getNumActiveScopes() == 2);
  assert(scopes.types.getNumActiveScopes() == 2);
  // remove package definitions
  scopes.popScopeByName(package);
}

} // namespace ovid::ast