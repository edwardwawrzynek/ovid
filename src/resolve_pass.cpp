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
  for (auto &scope : node.scope) {
    current_module.push_back(scope);
  }
  scopes.pushComponentScopesByName(current_module);

  // visit child nodes
  for (auto &child : node.body) {
    if (child != nullptr)
      visitNode(*child, state);
  }

  // remove this module's scope from the active scope table
  scopes.popComponentScopesByName(current_module);
  for (auto &scope : node.scope) {
    current_module.pop_back();
  }
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
      scopes(scopes), package(package), current_module(package) {
  // add package as scope table [1] in scope stack
  assert(scopes.names.getNumActiveScopes() == 1);
  assert(scopes.types.getNumActiveScopes() == 1);
  scopes.pushComponentScopesByName(package);
}

void ResolvePass::removePushedPackageScope() {
  // remove the scopes that the constructor pushed
  scopes.popComponentScopesByName(package);
  assert(scopes.names.getNumActiveScopes() == 1);
  assert(scopes.types.getNumActiveScopes() == 1);
}

} // namespace ovid::ast