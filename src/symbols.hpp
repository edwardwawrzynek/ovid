#ifndef H_SYMBOLS_INCL
#define H_SYMBOLS_INCL

#include <cassert>
#include <functional>
#include <iterator>
#include <map>
#include <memory>
#include <stack>
#include <unordered_map>
#include <vector>

// forward declare
namespace ovid::ast {
class ImplHeader;
}

namespace ovid {
/**
 * Symbol tables stored for ovid:
 *
 * Storage:
 *    A symbol tables stores the name, type, and other metatdata for symbols. A
 * symbol table containsFrom only symbols that share a scope
 *
 *    A scope table consists of a reference to the symbol table for that scope,
 * as well a table of child scope tables
 *
 *    This works well for imports of external packages with full scope tables
 * already calculated, (and potential future per-file compilation where the
 * combination of partial tables produces a full table).
 *
 * Current Scope:
 *    The current scope is maintained as a stack of active scope tables.
 *    The default elements on this stack:
 *    0 - The root namespace. All imported packages and symbols, as well as the
 * current package (under its full scope). 1 - The current package table 2...m -
 * The scope tables for active modules (if any) m..n - Scope tables for elements
 * not somewhere in the root namespace -- function bodies, control statement
 * bodies
 *
 * Imports:
 *    On an import, the proper header metadata (probably in binary header files
 * or lib files) is located. It is loaded into the root namespace scope table
 * under the appropriate scopes. If items were located without scopes, they are
 * loaded into the root namespace symbol table.
 */

template <class T> class ScopeTable;

/* a hashtable of string -> symbols */
template <class T> class SymbolTable {
  std::unordered_multimap<std::string, std::shared_ptr<T>> table;

public:
  // add a symbol to the table
  void addSymbol(const std::string &name, std::shared_ptr<T> symbol,
                 ScopeTable<T> *parentTable);

  // find a symbol in the table. Expects only one symbol with the name exists
  std::shared_ptr<T> findSymbol(const std::string &name);

  // find a symbol in the table, filtering by the predicate. Useful for
  // overridden symbols
  std::shared_ptr<T> findSymbol(const std::string &name,
                                std::function<bool(const T &)> predicate);

  SymbolTable() : table(){};
};

template <class T>
void SymbolTable<T>::addSymbol(const std::string &name,
                               std::shared_ptr<T> symbol,
                               ScopeTable<T> *parentTable) {
  symbol->name = name;
  symbol->parent_table = parentTable;
  table.emplace(name, std::move(symbol));
}

template <class T>
std::shared_ptr<T> SymbolTable<T>::findSymbol(const std::string &name) {
  // make sure only one symbol exists with this name
  // only multiple matching symbols should be present for overloaded functions,
  // which should use the predicated version
#ifndef NDEBUG
  assert(table.count(name) <= 1);
#endif
  auto res = table.find(name);
  return res == table.end() ? nullptr : res->second;
}

template <class T>
std::shared_ptr<T>
SymbolTable<T>::findSymbol(const std::string &name,
                           std::function<bool(const T &)> predicate) {
  auto range = table.equal_range(name);
  // make sure that only one symbol matches the predicate
  // this is used for overloaded functions, and we shouldn't allow multiple
  // overloads with matching types
#ifndef NDEBUG
  int triggeredCount = 0;
  for (auto it = range.first; it != range.second; it++) {
    if (predicate(*(it->second)))
      triggeredCount++;
  }
  assert(triggeredCount <= 1);
#endif
  for (auto it = range.first; it != range.second; it++) {
    if (predicate(*(it->second)))
      return it->second;
  }

  return nullptr;
}

/* a scope with symbols and child scopes */
template <class T> class ScopeTable {
  std::unique_ptr<SymbolTable<T>> symbols;
  std::unordered_map<std::string, std::unique_ptr<ScopeTable<T>>> scopes;
  bool is_public;
  bool is_func_scope;
  // parent scope table, or null if root
  ScopeTable<T> *parent;
  // name of this part of the table
  std::string name;
  // versioning number for this scope (package version hash)
  int64_t version_int;

  // if this is the scope contained in an impl block, then impl is that block
  // if impl is not nullptr, then parent is the scope containing the impl (not
  // the scope through which the impl is addressed)
  std::shared_ptr<ast::ImplHeader> impl;

public:
  // return the SymbolTable associated with this scope
  SymbolTable<T> &getDirectScopeTable();

  // get a child symbol table with name scopes
  ScopeTable<T> *getScopeTable(const std::vector<std::string> &scopes);

  ScopeTable<T> *getScopeTable(const std::string &scope);

  // add an empty scope with the given name
  ScopeTable<T> *addScopeTable(const std::string &scope, bool is_public);

  // add the given scope table with the given name
  ScopeTable<T> *addScopeTable(const std::string &scope,
                               std::unique_ptr<ScopeTable<T>> table);

  // find a symbol with a scope and name. Expects only one symbol with the name
  // and scope exists
  std::shared_ptr<T> findSymbol(const std::vector<std::string> &scope_names,
                                const std::string &identifier);

  // find a symbol with a scope and name, filtering by the predicate. Useful for
  // finding overrides
  std::shared_ptr<T> findSymbol(const std::vector<std::string> &scope_name,
                                const std::string &identifier,
                                std::function<bool(const T &)> predicate);

  // add a symbol, given scopes and a name
  void addSymbol(const std::vector<std::string> &scope_name,
                 const std::string &name, std::shared_ptr<T> symbol);

  void addSymbol(const std::string &name, std::shared_ptr<T> symbol);

  std::string getName() const;
  ScopeTable<T> *getParent() const;
  std::shared_ptr<ast::ImplHeader> getImpl() const;
  int getVersionInt() const;
  void setVersionInt(int64_t version);
  bool getIsPublic() const;
  bool getIsFuncScope() const;

  ScopeTable(bool is_public, ScopeTable<T> *parent, const std::string &name,
             bool is_func_scope = false, int64_t version_int = -1,
             std::shared_ptr<ast::ImplHeader> impl = nullptr)
      : symbols(std::make_unique<SymbolTable<T>>()), scopes(),
        is_public(is_public), is_func_scope(is_func_scope), parent(parent),
        name(name), version_int(version_int), impl(std::move(impl)){};
};

template <class T> SymbolTable<T> &ScopeTable<T>::getDirectScopeTable() {
  return *symbols;
}

template <class T>
ScopeTable<T> *ScopeTable<T>::getScopeTable(const std::string &scope) {
  return this->scopes[scope].get();
}

template <class T>
ScopeTable<T> *
ScopeTable<T>::getScopeTable(const std::vector<std::string> &scopes) {
  auto curTable = this;
  for (auto &scope : scopes) {
    curTable = curTable->scopes[scope].get();
    if (curTable == nullptr)
      return nullptr;
  }
  return curTable;
}

template <class T>
ScopeTable<T> *
ScopeTable<T>::addScopeTable(const std::string &scope,
                             std::unique_ptr<ScopeTable<T>> table) {
  assert(scopes[scope] == nullptr);
  scopes[scope] = std::move(table);
  return scopes[scope].get();
}

template <class T>
ScopeTable<T> *ScopeTable<T>::addScopeTable(const std::string &scope,
                                            bool is_public) {
  return addScopeTable(scope,
                       std::make_unique<ScopeTable<T>>(is_public, this, scope));
}

template <class T>
std::shared_ptr<T>
ScopeTable<T>::findSymbol(const std::vector<std::string> &scope_names,
                          const std::string &identifier) {
  if (scope_names.empty())
    return symbols->findSymbol(identifier);
  auto scope = getScopeTable(scope_names);
  if (scope == nullptr)
    return nullptr;
  return scope->symbols->findSymbol(identifier);
}

template <class T>
std::shared_ptr<T>
ScopeTable<T>::findSymbol(const std::vector<std::string> &scope_name,
                          const std::string &identifier,
                          std::function<bool(const T &)> predicate) {
  if (scope_name.empty())
    return symbols->findSymbol(identifier, predicate);

  auto scope = getScopeTable(scope_name);
  if (scope == nullptr)
    return nullptr;
  return scope->symbols->findSymbol(identifier, predicate);
}

template <class T>
void ScopeTable<T>::addSymbol(const std::vector<std::string> &scope_name,
                              const std::string &name,
                              std::shared_ptr<T> symbol) {
  if (scope_name.empty()) {
    getDirectScopeTable().addSymbol(name, symbol, this);
  } else {
    getScopeTable(scope_name)
        ->getDirectScopeTable()
        .addSymbol(name, symbol, this);
  }
}

template <class T>
void ScopeTable<T>::addSymbol(const std::string &name,
                              std::shared_ptr<T> symbol) {
  getDirectScopeTable().addSymbol(name, symbol, this);
}

// check if the given symbol (with is_symbol_pub visibility) is visible to
// curModule in curPackage
template <class T>
bool checkVisible(const T &sym, ScopeTable<T> *curPackage,
                  ScopeTable<T> *curModule, bool is_symbol_pub) {
  bool is_in_package = false;

  // table directly containing the symbol
  ScopeTable<T> *containingTable = sym.parent_table;

  // if the symbol is contained in a function scope, it is automatically
  // accessible (ActiveScopes make sure it isn't accessible outside the
  // function)
  if (containingTable->getIsFuncScope())
    return true;

  // determine if the symbol is in curPackage
  ScopeTable<T> *tmp = containingTable;
  while (tmp != nullptr) {
    if (tmp == curPackage) {
      is_in_package = true;
      break;
    }
    tmp = tmp->getParent();
  }

  // check if current module is a descendant of containing table
  bool is_descendant = false;
  tmp = curModule;
  while (tmp != nullptr) {
    if (tmp == containingTable) {
      is_descendant = true;
      break;
    }
    tmp = tmp->getParent();
  }

  // if the current module is a descendant of symbol's module, symbol is
  // automatically accessible
  if (is_descendant)
    return true;
  // if the current module is in the same package as symbol, just check symbol
  // visibility
  if (is_in_package)
    return is_symbol_pub;

  return is_symbol_pub && containingTable->getIsPublic();
}

template <class T> std::string ScopeTable<T>::getName() const { return name; }

template <class T> ScopeTable<T> *ScopeTable<T>::getParent() const {
  return parent;
}

template <class T> int ScopeTable<T>::getVersionInt() const {
  return version_int;
}

template <class T> void ScopeTable<T>::setVersionInt(int64_t version) {
  version_int = version;
}
template <class T> bool ScopeTable<T>::getIsPublic() const { return is_public; }

template <class T> bool ScopeTable<T>::getIsFuncScope() const {
  return is_func_scope;
}
template <class T>
std::shared_ptr<ast::ImplHeader> ScopeTable<T>::getImpl() const {
  return impl;
}

/* the active scopes and symbol tables
 * maintained as a stack -- global namespaces at bottom, local scopes at top */
template <class T> class ActiveScope {
  std::vector<ScopeTable<T> *> scopes;

public:
  // push a scope onto the stack
  void pushScope(ScopeTable<T> *scope);

  // remove the most recently push scope from the stack
  ScopeTable<T> *popScope();

  // remove the most recently pushed scope from the stack, and assert it is the
  // expected scope
  ScopeTable<T> *popScope(const ScopeTable<T> *expected);

  // search the scope top down for the first symbol with the given scope and
  // name. Returns nullptr if not found
  std::shared_ptr<T> findSymbol(const std::vector<std::string> &scope_names,
                                const std::string &identifier);

  // search the scope top down for the first symbol with the given scope, name,
  // and predicate match. Returns nullptr if not found
  std::shared_ptr<T> findSymbol(const std::vector<std::string> &scope_names,
                                const std::string &identifier,
                                std::function<bool(const T &)> predicate);

  // add a set of scope to the stack by a set of nested scope names based on a
  // root scope
  ScopeTable<T> *
  pushComponentScopesByNameFromRoot(const std::vector<std::string> &scopes,
                                    ScopeTable<T> *root);

  void popComponentScopesByNameFromRoot(const std::vector<std::string> &scopes,
                                        ScopeTable<T> *root);

  // get the root namespace scope (scope 0)
  ScopeTable<T> *getRootScope();

  // get the most recently pushed scope
  ScopeTable<T> *getTopScope();

  // get number of active scopes (for debugging)
  int getNumActiveScopes();

  // get the scope at the specified index
  ScopeTable<T> *getNthScope(size_t n);

  // get the most recently pushed scope matching the specified predicate
  ScopeTable<T> *getTopScopeWhere(
      const std::function<bool(const ScopeTable<T> *table)> &predicate);

  // get the most recently pushed scope that is not a function scope table
  ScopeTable<T> *getTopNonFuncScope();

  ActiveScope() : scopes(){};
};

template <class T> void ActiveScope<T>::pushScope(ScopeTable<T> *scope) {
  scopes.push_back(scope);
}

template <class T> ScopeTable<T> *ActiveScope<T>::popScope() {
  auto scope = scopes.back();
  scopes.pop_back();
  return scope;
}

template <class T>
ScopeTable<T> *ActiveScope<T>::popScope(const ScopeTable<T> *expected) {
  auto scope = scopes.back();

  assert(scope == expected);
  scopes.pop_back();

  return scope;
}

template <class T>
std::shared_ptr<T>
ActiveScope<T>::findSymbol(const std::vector<std::string> &scope_names,
                           const std::string &identifier) {
  for (size_t sindex = scopes.size(); sindex--;) {
    auto &scope = scopes[sindex];

    auto found = scope->findSymbol(scope_names, identifier);
    if (found != nullptr)
      return found;
  }

  return nullptr;
}

template <class T>
std::shared_ptr<T>
ActiveScope<T>::findSymbol(const std::vector<std::string> &scope_names,
                           const std::string &identifier,
                           std::function<bool(const T &)> predicate) {
  for (size_t sindex = scopes.size(); sindex--;) {
    auto &scope = scopes[sindex];

    auto found = scope->findSymbol(scope_names, identifier, predicate);
    if (found != nullptr)
      return found;
  }

  return nullptr;
}

template <class T> ScopeTable<T> *ActiveScope<T>::getRootScope() {
  assert(scopes.size() >= 1);
  return scopes[0];
}
template <class T> int ActiveScope<T>::getNumActiveScopes() {
  return scopes.size();
}

template <class T>
ScopeTable<T> *ActiveScope<T>::pushComponentScopesByNameFromRoot(
    const std::vector<std::string> &scopes, ScopeTable<T> *root) {
  // iterate through scopes and push
  auto curTable = root;

  for (auto &scope : scopes) {
    auto newTable = curTable->getScopeTable(scope);
    if (newTable == nullptr)
      return nullptr;
    pushScope(newTable);
    curTable = newTable;
  }

  return curTable;
}

template <class T>
void ActiveScope<T>::popComponentScopesByNameFromRoot(
    const std::vector<std::string> &scopes, ScopeTable<T> *root) {

  std::vector<std::string> mScopes(scopes);
  // iterate backwards through scopes and pop
  while (!mScopes.empty()) {
    popScope(root->getScopeTable(mScopes));
    mScopes.pop_back();
  }
}

template <class T> ScopeTable<T> *ActiveScope<T>::getTopScope() {
  return scopes[scopes.size() - 1];
}

template <class T> ScopeTable<T> *ActiveScope<T>::getNthScope(size_t n) {
  return scopes[n];
}

template <class T>
ScopeTable<T> *ActiveScope<T>::getTopScopeWhere(
    const std::function<bool(const ScopeTable<T> *)> &predicate) {
  for (size_t i = scopes.size(); i-- > 0;) {
    auto scope = scopes[i];
    if (predicate(scope))
      return scope;
  }
  return nullptr;
}

template <class T> ScopeTable<T> *ActiveScope<T>::getTopNonFuncScope() {
  return getTopScopeWhere(
      [](const ScopeTable<T> *t) -> bool { return !t->getIsFuncScope(); });
}

// convert a set of scopes and a name to a printable string
std::string scopesAndNameToString(const std::vector<std::string> &scopes,
                                  const std::string &name);

} // namespace ovid

#endif