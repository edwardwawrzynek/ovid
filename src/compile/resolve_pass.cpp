#include "resolve_pass.hpp"

namespace ovid::ast {

int ResolvePass::visitVarDecl(VarDecl &node, const ResolvePassState &state) {
  visitNode(*node.initialValue, state);
  // lookup variable being declared, and mark as declared (only after inital
  // value visited)
  auto declaredSym =
      scopes.names.findSymbol(std::vector<std::string>(), node.name);
  assert(declaredSym != nullptr);
  declaredSym->resolve_pass_declared_yet = true;

  // TODO: change node from refering to name to refer to symbol

  return 0;
}

int ResolvePass::visitFunctionDecl(FunctionDecl &node,
                                   const ResolvePassState &state) {
  // add this function's scope to the active scope stack
  scopes.names.pushScope(node.body.symbols);

  // mark arguments as declared
  for(auto &name: node.proto->argNames) {
    auto sym = node.body.symbols->getDirectScopeTable().findSymbol(name);
    assert(sym != nullptr);
    // TODO: change arguments from referring to names to refer to symbols
    sym->resolve_pass_declared_yet = true;
  }

  for (auto &child : node.body.statements) {
    if (child != nullptr)
      visitNode(*child, state);
  }

  // pop function's scope
  scopes.names.popScope(node.body.symbols);

  return 0;
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

  return 0;
}

int ResolvePass::visitFunctionCall(FunctionCall &node,
                                   const ResolvePassState &state) {}

int ResolvePass::visitIdentifier(Identifier &node,
                                 const ResolvePassState &state) {
  // only lookup declared identifiers
  auto sym =
      scopes.names.findSymbol(node.scope, node.id, [](const Symbol &s) -> bool {
        return s.resolve_pass_declared_yet;
      });

  if(sym == nullptr) {
    auto name = scopesAndNameToString(node.scope, node.id);
    errorMan.logError(string_format("use of undeclared identifier `\x1b[1m%s\x1b[m`", name.c_str()), node.loc, ErrorType::UndeclaredIdentifier);
  }

  // TODO: somehow change node to refer to sym instead of scope/id strings (variant?)
}

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