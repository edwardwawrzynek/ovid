#include "resolve_pass.hpp"
#include "generics.hpp"

#include <utility>

namespace ovid::ast {

int ResolvePass::visitVarDecl(VarDecl &node, const ResolvePassState &state) {
  visitNode(*node.initialValue, state);
  // check for shadowing
  checkShadowed(
      node.loc, node.name, [](const Symbol &sym) -> bool { return true; },
      false);

  // mark as declared (only after initial value visited)
  assert(node.resolved_symbol != nullptr);
  node.resolved_symbol->resolve_pass_declared_yet = true;

  if (node.explicitType != nullptr) {
    node.explicitType = resolveType(node.explicitType);
  }

  return 0;
}

int ResolvePass::visitFunctionDecl(FunctionDecl &node,
                                   const ResolvePassState &state) {
  // add this function's scope to the active scope stack
  scopes.names.pushScope(node.body.symbols.get());

  auto pis_in_global = is_in_global;
  is_in_global = false;

  auto resolvedType = resolveTypeConstructor(node.type);

  assert(resolvedType != nullptr);
  node.type = resolvedType;
  node.resolved_symbol->type = node.type;

  auto formal_bound_type = node.getFormalBoundFunctionType();
  // mark arguments as declared
  assert(formal_bound_type->resolvedArgs.empty());
  assert(formal_bound_type->argNames.size() ==
         formal_bound_type->argTypes.size());
  for (size_t i = 0; i < formal_bound_type->argNames.size(); i++) {
    auto &name = formal_bound_type->argNames[i];
    // check for shadowing
    checkShadowed(
        node.loc, name, [](const Symbol &sym) -> bool { return true; }, true);
    auto sym = node.body.symbols->getDirectScopeTable().findSymbol(name);
    assert(sym != nullptr);
    sym->resolve_pass_declared_yet = true;
    // set arg type appropriately
    sym->type = formal_bound_type->argTypes[i];
    // make function type refer to resolved symbols
    formal_bound_type->resolvedArgs.push_back(sym);
  }

  scopes.types.pushScope(node.type->getFormalScopeTable());
  for (auto &child : node.body.statements) {
    if (child != nullptr)
      visitNode(*child, state);
  }

  is_in_global = pis_in_global;
  // pop function's scope
  scopes.names.popScope(node.body.symbols.get());
  scopes.types.popScope(node.type->getFormalScopeTable());

  return 0;
}

int ResolvePass::visitNativeFunctionDecl(NativeFunctionDecl &node,
                                         const ResolvePassState &state) {
  // resolve function type
  node.sym->type = resolveTypeConstructor(node.sym->type);
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
  for ([[maybe_unused]] auto &scope : node.scope) {
    current_module.pop_back();
  }

  return 0;
}

int ResolvePass::visitIfStatement(IfStatement &node,
                                  const ResolvePassState &state) {
  for (auto &cond : node.conditions) {
    visitNode(*cond, state);
  }

  for (auto &body : node.bodies) {
    // add this body's scope to the active scope stack
    scopes.names.pushScope(body.symbols.get());

    assert(!is_in_global);
    for (auto &statement : body.statements) {
      visitNode(*statement, state);
    }

    scopes.names.popScope(body.symbols.get());
  }

  return 0;
}

int ResolvePass::visitWhileStatement(WhileStatement &node,
                                     const ResolvePassState &state) {
  visitNode(*node.cond, state);

  // push scope onto active scope stack
  scopes.names.pushScope(node.body.symbols.get());
  assert(!is_in_global);

  for (auto &stat : node.body.statements) {
    visitNode(*stat, state);
  }

  scopes.names.popScope(node.body.symbols.get());

  return 0;
}

int ResolvePass::visitFunctionCall(FunctionCall &node,
                                   const ResolvePassState &state) {
  visitNode(*node.funcExpr, state);
  for (auto &arg : node.args) {
    visitNode(*arg, state);
  }

  return 0;
}

int ResolvePass::visitIdentifier(Identifier &node,
                                 const ResolvePassState &state) {
  // find the symbol
  std::shared_ptr<Symbol> sym;
  if (node.is_root_scope) {
    // only check root scope
    sym = scopes.names.getRootScope()->findSymbol(
        node.scope, node.id,
        [](const Symbol &s) -> bool { return s.resolve_pass_declared_yet; });
  } else {
    sym = scopes.names.findSymbol(
        node.scope, node.id,
        [](const Symbol &s) -> bool { return s.resolve_pass_declared_yet; });
  }

  auto scopedName = scopesAndNameToString(node.scope, node.id);
  // error on undeclared symbol
  if (sym == nullptr) {
    errorMan.logError(
        string_format("use of undeclared identifier `\x1b[1m%s\x1b[m`",
                      scopedName.c_str()),
        node.loc, ErrorType::UndeclaredIdentifier);
  }
  // error on usage of inaccessible (private) symbol
  else if (!checkVisible(
               *sym, scopes.names.getRootScope()->getScopeTable(package),
               scopes.names.getRootScope()->getScopeTable(current_module),
               sym->is_public)) {
    errorMan.logError(
        string_format("use of private identifier `\x1b[1m%s\x1b[m`",
                      scopedName.c_str()),
        node.loc, ErrorType::UseOfPrivateIdentifier);
  }

  // change node to refer to sym instead of scope/id strings
  node.resolved_symbol = sym;

  // resolve type params on :<> operator
  for (size_t i = 0; i < node.type_params.size(); i++) {
    node.type_params[i] = resolveType(node.type_params[i]);
  }

  return 0;
}

int ResolvePass::visitAssignment(Assignment &node,
                                 const ResolvePassState &state) {
  visitNode(*node.lvalue, state);
  visitNode(*node.rvalue, state);

  return 0;
}

int ResolvePass::visitTuple(Tuple &node, const ResolvePassState &state) {
  for (auto &expr : node.expressions) {
    visitNode(*expr, state);
  }

  return 0;
}

int ResolvePass::visitStructExpr(StructExpr &node,
                                 const ResolvePassState &state) {
  node.type = resolveType(node.type);
  // resolve field_expr's
  for (auto &expr : node.field_exprs) {
    visitNode(*expr, state);
  }

  return 0;
}

int ResolvePass::visitTypeAliasDecl(TypeAliasDecl &node,
                                    const ResolvePassState &state) {
  // check for type shadowing
  checkTypeShadowed(node.loc, node.name,
                    [](const TypeAlias &t) { return true; });

  // if the aliased type hasn't already been resolved, resolve it
  // needed to reject invalid aliases if they are never otherwise used
  if (!node.type->inner_resolved) {
    // call the constructor on unit type parameters
    auto expected_params = node.type->type->numTypeParams();
    ast::TypeList params;
    for (size_t i = 0; i < expected_params; i++) {
      params.push_back(
          std::make_shared<ast::TupleType>(node.loc, ast::TypeList()));
    }
    auto dummy_construct_call = std::make_shared<UnresolvedType>(
        node.loc, std::vector<std::string>(), node.name, false, params);
    resolveType(dummy_construct_call);
  }

  return 0;
}

bool ResolvePass::checkShadowed(const SourceLocation &pos,
                                const std::string &name,
                                std::function<bool(const Symbol &)> predicate,
                                bool is_arg) {
  // pop top scope of scope stack, so that current declaration isn't found
  auto poppedScope = scopes.names.popScope();

  auto shadowed = scopes.names.findSymbol(std::vector<std::string>(), name,
                                          std::move(predicate));
  if (shadowed) {
    auto scoped_name =
        scopesAndNameToString(current_module, name, is_in_global);

    errorMan.logError(
        string_format(
            "declaration of %s`\x1b[1m%s\x1b[m` shadows higher declaration",
            (is_arg ? "argument " : ""), scoped_name.c_str()),
        pos, ErrorType::VarDeclareShadowed, false);
    errorMan.logError(
        string_format("shadowed declaration of `\x1b[1m%s\x1b[m` here",
                      scoped_name.c_str()),
        shadowed->decl_loc, ErrorType::Note);
  }

  scopes.names.pushScope(poppedScope);

  return shadowed != nullptr;
}

int ResolvePass::visitReturnStatement(ReturnStatement &node,
                                      const ResolvePassState &state) {
  if (node.expression != nullptr) {
    visitNode(*node.expression, state);
  }

  return 0;
}

int ResolvePass::visitFieldAccess(FieldAccess &node,
                                  const ResolvePassState &state) {
  visitNode(*node.lvalue, state);

  return 0;
}

ResolvePass::ResolvePass(ActiveScopes &scopes, ErrorManager &errorMan,
                         const std::vector<std::string> &package)
    : BaseASTVisitor<int, ResolvePassState>(0), errorMan(errorMan),
      scopes(scopes), package(package), current_module(package),
      is_in_global(true) {
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

bool ResolvePass::checkTypeShadowed(
    const SourceLocation &pos, const std::string &name,
    std::function<bool(const TypeAlias &)> predicate) {
  // pop top of type stack. even though type stack doesn't match name scope, top
  // is still the scope in which the visited type is declared in
  auto poppedScope = scopes.types.popScope();

  auto shadowed = scopes.types.findSymbol(std::vector<std::string>(), name,
                                          std::move(predicate));

  if (shadowed) {
    auto scoped_name =
        scopesAndNameToString(current_module, name, is_in_global);

    errorMan.logError(
        string_format(
            "declaration of type `\x1b[1m%s\x1b[m` shadows higher declaration",
            scoped_name.c_str()),
        pos, ErrorType::TypeDeclShadowed, false);
    errorMan.logError(
        string_format("shadowed declaration of type `\x1b[1m%s\x1b[m` here",
                      scoped_name.c_str()),
        shadowed->decl_loc, ErrorType::Note);
  }

  scopes.types.pushScope(poppedScope);

  return shadowed != nullptr;
}

std::shared_ptr<Type>
ResolvePass::resolveType(const std::shared_ptr<Type> &type) {
  return TypeConstructorPass::resolveType(type, scopes, errorMan, package,
                                          current_module);
}
std::shared_ptr<TypeConstructor> ResolvePass::resolveTypeConstructor(
    const std::shared_ptr<TypeConstructor> &type_construct) {
  auto trivial = type_construct->trivialConstruct();
  if (trivial != nullptr) {
    return resolveType(trivial);
  } else {
    return TypeConstructorPass::resolveTypeConstructor(
        type_construct, scopes, errorMan, package, current_module);
  }
}

} // namespace ovid::ast