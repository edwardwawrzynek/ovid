#include "resolve_pass.hpp"

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
    auto type_resolv_state = TypeResolverState(package, current_module);
    node.explicitType =
        type_resolver.visitType(node.explicitType, type_resolv_state);
  }

  return 0;
}

int ResolvePass::visitFunctionDecl(FunctionDecl &node,
                                   const ResolvePassState &state) {
  // add this function's scope to the active scope stack
  scopes.names.pushScope(node.body.symbols.get());

  auto pis_in_global = is_in_global;
  is_in_global = false;

  assert(node.type->resolvedArgs.empty());
  auto typeResolveState = TypeResolverState(package, current_module);
  node.type = type_resolver.visitNamedFunctionType(node.type, typeResolveState);

  node.resolved_symbol->type = node.type;

  // mark arguments as declared
  assert(node.type->argNames.size() == node.type->argTypes.size());
  for (size_t i = 0; i < node.type->argNames.size(); i++) {
    auto &name = node.type->argNames[i];
    // check for shadowing
    checkShadowed(
        node.loc, name, [](const Symbol &sym) -> bool { return true; }, true);
    auto sym = node.body.symbols->getDirectScopeTable().findSymbol(name);
    assert(sym != nullptr);
    sym->resolve_pass_declared_yet = true;
    // set arg type appropriately
    sym->type = node.type->argTypes[i];
    // make function type refer to resolved symbols
    node.type->resolvedArgs.push_back(sym);
  }

  for (auto &child : node.body.statements) {
    if (child != nullptr)
      visitNode(*child, state);
  }

  is_in_global = pis_in_global;
  // pop function's scope
  scopes.names.popScope(node.body.symbols.get());

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
  ScopeTable<Symbol> *containingTable;
  if (node.is_root_scope) {
    // only check root scope
    sym = scopes.names.getRootScope()->findSymbol(
        node.scope, node.id,
        [](const Symbol &s) -> bool { return s.resolve_pass_declared_yet; });
    containingTable = scopes.names.getRootScope();
  } else {
    sym = scopes.names.findSymbol(
        node.scope, node.id,
        [](const Symbol &s) -> bool { return s.resolve_pass_declared_yet; });
    containingTable = scopes.names.findTableContainingSymbol(
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
  else if (!containingTable->checkAccessible(
               node.scope, node.id,
               [](const Symbol &s) -> bool {
                 return s.resolve_pass_declared_yet;
               },
               scopes.names.getRootScope()->getScopeTable(package),
               scopes.names.getRootScope()->getScopeTable(current_module),
               sym->is_public)) {
    errorMan.logError(
        string_format("use of private identifier `\x1b[1m%s\x1b[m`",
                      scopedName.c_str()),
        node.loc, ErrorType::UseOfPrivateIdentifier);
  }

  // change node to refer to sym instead of scope/id strings
  node.resolved_symbol = sym;

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

int ResolvePass::visitTypeAliasDecl(TypeAliasDecl &node,
                                    const ResolvePassState &state) {
  // check for type shadowing
  checkTypeShadowed(node.loc, node.name,
                    [](const TypeAlias &t) { return true; });

  // if the aliased type hasn't already been resolved, resolve it
  // needed to reject invalid aliases if they are never otherwise used
  if (!node.type->inner_resolved) {
    node.type->type = type_resolver.visitType(
        node.type->type, TypeResolverState(package, current_module));
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
      is_in_global(true), type_resolver(scopes, errorMan) {
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

  auto shadowed =
      scopes.types.findSymbol(std::vector<std::string>(), name, predicate);

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
TypeResolver::visitUnresolvedType(std::shared_ptr<UnresolvedType> type,
                                  const TypeResolverState &state) {
  // lookup type in type tables
  std::shared_ptr<TypeAlias> sym;
  ScopeTable<TypeAlias> *containingTable;
  if (type->is_root_scoped) {
    sym = scopes.types.getRootScope()->findSymbol(type->scopes, type->name);
    containingTable = scopes.types.getRootScope();
  } else {
    sym = scopes.types.findSymbol(type->scopes, type->name);
    containingTable =
        scopes.types.findTableContainingSymbol(type->scopes, type->name);
  }

  auto scopedName = scopesAndNameToString(type->scopes, type->name, true);

  if (sym == nullptr) {
    errorMan.logError(string_format("use of undeclared type `\x1b[1m%s\x1b[m`",
                                    scopedName.c_str()),
                      type->loc, ErrorType::UndeclaredType);

    return nullptr;
  }
  // check for use of private type
  else if (!containingTable->checkAccessible(
               type->scopes, type->name,
               scopes.types.getRootScope()->getScopeTable(state.package),
               scopes.types.getRootScope()->getScopeTable(state.current_module),
               sym->is_public)) {
    errorMan.logError(string_format("use of private type `\x1b[1m%s\x1b[m`",
                                    scopedName.c_str()),
                      type->loc, ErrorType::UseOfPrivateType);
  }

  // resolve inner type if it hasn't been already
  if (!sym->inner_resolved) {
    sym->inner_resolved = true;
    sym->type = visitType(sym->type, state);
  }
  return sym->type;
}

std::shared_ptr<MutType>
TypeResolver::visitMutType(std::shared_ptr<MutType> type,
                           const TypeResolverState &state) {
  type->type = visitType(type->type, state);
  return type;
}

std::shared_ptr<PointerType>
TypeResolver::visitPointerType(std::shared_ptr<PointerType> type,
                               const TypeResolverState &state) {
  type->type = visitType(type->type, state);
  return type;
}

std::shared_ptr<FunctionType>
TypeResolver::visitFunctionType(std::shared_ptr<FunctionType> type,
                                const TypeResolverState &state) {
  for (auto &arg : type->argTypes) {
    arg = visitType(arg, state);
  }

  type->retType = visitType(type->retType, state);

  return type;
}

std::shared_ptr<NamedFunctionType>
TypeResolver::visitNamedFunctionType(std::shared_ptr<NamedFunctionType> type,
                                     const TypeResolverState &state) {
  auto funcTypeRes = visitFunctionType(type, state);
  assert(funcTypeRes.get() == type.get());

  return type;
}

std::shared_ptr<TupleType>
TypeResolver::visitTupleType(std::shared_ptr<TupleType> type,
                             const TypeResolverState &state) {
  for (auto &field : type->types) {
    field = visitType(field, state);
  }

  return type;
}

std::shared_ptr<Type>
TypeResolver::visitStructType(std::shared_ptr<StructType> type,
                              const TypeResolverState &state) {
  if (type->fields_resolved) {
    return type;
  }
  type->fields_resolved = true;

  for (auto &field : type->field_types) {
    field = visitType(field, state);
  }

  return type;
}

#define TYPE_RESOLVER_VISIT_CASE(caseType, caseFunction)                       \
  if (std::dynamic_pointer_cast<caseType>(type) != nullptr) {                  \
    return caseFunction(std::dynamic_pointer_cast<caseType>(type), state);     \
  }

std::shared_ptr<Type> TypeResolver::visitType(const std::shared_ptr<Type> &type,
                                              const TypeResolverState &state) {
  TYPE_RESOLVER_VISIT_CASE(UnresolvedType, visitUnresolvedType);
  TYPE_RESOLVER_VISIT_CASE(MutType, visitMutType);
  TYPE_RESOLVER_VISIT_CASE(PointerType, visitPointerType);
  TYPE_RESOLVER_VISIT_CASE(FunctionType, visitFunctionType);
  TYPE_RESOLVER_VISIT_CASE(TupleType, visitTupleType);
  TYPE_RESOLVER_VISIT_CASE(StructType, visitStructType);

  /* all other types can't contain type aliases */
  return type;
}

TypeResolver::TypeResolver(ActiveScopes &scopes, ErrorManager &errorMan)
    : scopes(scopes), errorMan(errorMan) {}

} // namespace ovid::ast