#include "resolve_pass.hpp"

namespace ovid::ast {

int ResolvePass::visitVarDecl(VarDecl &node, const ResolvePassState &state) {
  visitNode(*node.initialValue, state);
  // check for shadowing
  checkShadowed(
      node.loc, node.name, [](const Symbol &sym) -> bool { return true; },
      false);

  // lookup variable being declared, and mark as declared (only after initial
  // value visited)
  auto declaredSym =
      scopes.names.findSymbol(std::vector<std::string>(), node.name);
  assert(declaredSym != nullptr);
  declaredSym->resolve_pass_declared_yet = true;
  // change node from referring to name to refer to symbol
  node.resolved_symbol = declaredSym;

  return 0;
}

int ResolvePass::visitFunctionDecl(FunctionDecl &node,
                                   const ResolvePassState &state) {
  // add this function's scope to the active scope stack
  scopes.names.pushScope(node.body.symbols);

  auto pis_in_global = is_in_global;
  is_in_global = false;

  // mark arguments as declared
  for (auto &name : node.type->argNames) {
    // check for shadowing
    checkShadowed(
        node.loc, name, [](const Symbol &sym) -> bool { return true; }, true);
    auto sym = node.body.symbols->getDirectScopeTable().findSymbol(name);
    assert(sym != nullptr);
    sym->resolve_pass_declared_yet = true;
    // make function type refer to resolved symbols
    node.type->resolvedArgs.push_back(sym);
  }

  auto typeResolveState = TypeResolverState(package, current_module);
  node.type->type = type_resolver.visitFunctionTypeNonOverload(
      *node.type->type, typeResolveState);

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
    scopes.names.pushScope(body.symbols);

    auto pis_in_global = is_in_global;
    is_in_global = false;

    for (auto &statement : body.statements) {
      visitNode(*statement, state);
    }

    is_in_global = pis_in_global;

    scopes.names.popScope(body.symbols.get());
  }

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
  std::shared_ptr<ScopeTable<Symbol>> containingTable;
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

  if (node.type->type == nullptr)
    return 0;

  // run type resolution on the type
  auto resolverState = TypeResolverState(package, current_module);
  node.type->type = type_resolver.visitType(*node.type->type, resolverState);

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

bool ResolvePass::checkShadowed(const SourceLocation &pos,
                                const std::string &name,
                                std::function<bool(const Symbol &)> predicate,
                                bool is_arg) {
  // pop top scope of scope stack, so that current declaration isn't found
  auto poppedScope = scopes.names.popScope();

  auto shadowed =
      scopes.names.findSymbol(std::vector<std::string>(), name, predicate);
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
TypeResolver::visitUnresolvedType(UnresolvedType &type,
                                  const TypeResolverState &state) {
  // lookup type in type tables
  std::shared_ptr<TypeAlias> sym;
  std::shared_ptr<ScopeTable<TypeAlias>> containingTable;
  if (type.is_root_scoped) {
    sym = scopes.types.getRootScope()->findSymbol(type.scopes, type.name);
    containingTable = scopes.types.getRootScope();
  } else {
    sym = scopes.types.findSymbol(type.scopes, type.name);
    containingTable =
        scopes.types.findTableContainingSymbol(type.scopes, type.name);
  }

  auto scopedName = scopesAndNameToString(type.scopes, type.name, true);

  if (sym == nullptr) {
    errorMan.logError(string_format("use of undeclared type `\x1b[1m%s\x1b[m`",
                                    scopedName.c_str()),
                      type.loc, ErrorType::UndeclaredType);

    return nullptr;
  }
  // check for use of private type
  else if (!containingTable->checkAccessible(
               type.scopes, type.name,
               scopes.types.getRootScope()->getScopeTable(state.package),
               scopes.types.getRootScope()->getScopeTable(state.current_module),
               sym->is_public)) {
    errorMan.logError(string_format("use of private type `\x1b[1m%s\x1b[m`",
                                    scopedName.c_str()),
                      type.loc, ErrorType::UseOfPrivateType);
  }

  return std::make_shared<ResolvedAlias>(type.loc, sym);
}

std::shared_ptr<Type>
TypeResolver::visitVoidType(VoidType &type, const TypeResolverState &state) {
  return std::make_shared<VoidType>(type.loc);
}

std::shared_ptr<Type>
TypeResolver::visitBoolType(BoolType &type, const TypeResolverState &state) {
  return std::make_shared<BoolType>(type.loc);
}

std::shared_ptr<Type>
TypeResolver::visitIntType(IntType &type, const TypeResolverState &state) {
  return std::make_shared<IntType>(type.loc, type.size, type.isUnsigned);
}

std::shared_ptr<Type>
TypeResolver::visitFloatType(FloatType &type, const TypeResolverState &state) {
  return std::make_shared<FloatType>(type.loc, type.size);
}

std::shared_ptr<Type>
TypeResolver::visitMutType(MutType &type, const TypeResolverState &state) {
  return std::make_shared<MutType>(type.loc, visitType(*type.type, state));
}

std::shared_ptr<Type>
TypeResolver::visitPointerType(PointerType &type,
                               const TypeResolverState &state) {
  return std::make_shared<PointerType>(type.loc, visitType(*type.type, state));
}

std::shared_ptr<FunctionType>
TypeResolver::visitFunctionTypeNonOverload(FunctionType &type,
                                           const TypeResolverState &state) {
  TypeList argTypes;
  for (auto &arg : type.argTypes) {
    argTypes.push_back(visitType(*arg, state));
  }
  auto retType = visitType(*type.retType, state);
  return std::make_shared<FunctionType>(type.loc, std::move(argTypes),
                                        std::move(retType));
}

std::shared_ptr<Type>
TypeResolver::visitFunctionType(FunctionType &type,
                                const TypeResolverState &state) {
  return visitFunctionTypeNonOverload(type, state);
}

std::shared_ptr<Type>
TypeResolver::visitNamedFunctionType(NamedFunctionType &type,
                                     const TypeResolverState &state) {
  auto newType = visitFunctionTypeNonOverload(*type.type, state);
  return std::make_shared<NamedFunctionType>(type.loc, std::move(newType),
                                             type.argNames);
}

TypeResolver::TypeResolver(ActiveScopes &scopes, ErrorManager &errorMan)
    : BaseTypeVisitor(nullptr), scopes(scopes), errorMan(errorMan) {}

} // namespace ovid::ast