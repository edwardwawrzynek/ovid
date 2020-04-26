#ifndef H_SYMBOLS_INCL
#define H_SYMBOLS_INCL

#include <cassert>
#include <functional>
#include <map>
#include <memory>
#include <stack>
#include <unordered_map>
#include <vector>


namespace ovid {
  /**
   * Symbol tables stored for ovid:
   *
   * Storage:
   *    A symbol tables stores the name, type, and other metatdata for symbols. A symbol table contains only symbols that share a scope
   *
   *    A scope table consists of a reference to the symbol table for that scope, as well a table of child scope tables
   *
   *    This works well for imports of external packages with full scope tables already calculated, (and potential future per-file compilation where the combination of partial tables produces a full table).
   *
   * Current Scope:
   *    The current scope is maintained as a stack of active scope tables.
   *    The default elements on this stack:
   *    0 - The root namespace. All imported packages and symbols, as well as the current package (under its full scope).
   *    1 - The current package table
   *    2...m - The scope tables for active modules (if any)
   *    m..n - Scope tables for elements not somewhere in the root namespace -- function bodies, control statement bodies
   *
   * Imports:
   *    On an import, the proper header metadata (probably in binary header files or lib files) is located. It is loaded into the root namespace scope table under the appropriate scopes. If items were located without scopes, they are loaded into the root namespace symbol table.
   */

  /* a hashtable of string -> symbols */
  template<class T>
  class SymbolTable {
    std::unordered_multimap<std::string, std::shared_ptr<T>> table;

  public:
    // add a symbol to the table
    void addSymbol(const std::string &name, std::shared_ptr<T> symbol);

    // find a symbol in the table. Expects only one symbol with the name exists
    std::shared_ptr<T> findSymbol(const std::string &name);

    // find a symbol in the table, filtering by the predicate. Useful for overridden symbols
    std::shared_ptr<T>
    findSymbol(const std::string &name, std::function<bool(const T &)> predicate);

    SymbolTable() :
        table(){};
  };

  template<class T>
  void SymbolTable<T>::addSymbol(const std::string &name, std::shared_ptr<T> symbol) {
    table.emplace(name, symbol);
  }

  template<class T>
  std::shared_ptr<T> SymbolTable<T>::findSymbol(const std::string &name) {
    // make sure only one symbol exists with this name
    // only multiple matching symbols should be present for overloaded functions, which should use the predicated version
#ifndef NDEBUG
    assert(table.count(name) <= 1);
#endif
    auto res = table.find(name);
    return res == table.end() ? nullptr : res->second;
  }

  template<class T>
  std::shared_ptr<T>
  SymbolTable<T>::findSymbol(const std::string &name, std::function<bool(const T &)> predicate) {
    auto range = table.equal_range(name);
    // make sure that only one symbol matches the predicate
    // this is used for overloaded functions, and we shouldn't allow multiple overloads with matching types
#ifndef NDEBUG
    int triggeredCount = 0;
    for (auto it = range.first; it != range.second; it++) {
      if (predicate(*(it->second))) triggeredCount++;
    }
    assert(triggeredCount <= 1);
#endif
    for (auto it = range.first; it != range.second; it++) {
      if (predicate(*(it->second))) return it->second;
    }

    return nullptr;
  }


  /* a scope with symbols and child scopes */
  template<class T>
  class ScopeTable {
    std::unique_ptr<SymbolTable<T>> symbols;
    std::unordered_map<std::string, std::shared_ptr<ScopeTable<T>>> scopes;

  public:
    // return the SymbolTable associated with this scope
    SymbolTable<T> &getDirectScopeTable();

    // get a child symbol table with name scopes
    std::shared_ptr<ScopeTable<T>> getScopeTable(const std::vector<std::string> &scopes);

    // add an empty scope with the given name
    std::shared_ptr<ScopeTable<T>> addScopeTable(const std::string &scope);

    // add the given scope table with the given name
    std::shared_ptr<ScopeTable<T>>
    addScopeTable(const std::string &scope, std::shared_ptr<ScopeTable<T>> table);

    //find a symbol with a scope and name. Expects only one symbol with the name and scope exists
    std::shared_ptr<T>
    findSymbol(const std::vector<std::string> &scope_names, const std::string &identifier);

    //find a symbol with a scope and name, filtering by the predicate. Useful for finding overrides
    std::shared_ptr<T>
    findSymbol(const std::vector<std::string> &scope_name, const std::string &identifier,
               std::function<bool(const T &)> predicate);

    ScopeTable() :
        symbols(std::make_unique<SymbolTable<T>>()), scopes(){};
  };

  template<class T>
  SymbolTable<T> &ScopeTable<T>::getDirectScopeTable() {
    return *symbols;
  }

  template<class T>
  std::shared_ptr<ScopeTable<T>>
  ScopeTable<T>::getScopeTable(const std::vector<std::string> &scopes) {
    auto curTable = this->scopes;
    std::shared_ptr<ScopeTable<T>> childScope = nullptr;
    for (auto &scope : scopes) {
      childScope = curTable[scope];
      if (childScope == nullptr) return nullptr;
      curTable = childScope->scopes;
    }
    return childScope;
  }

  template<class T>
  std::shared_ptr<ScopeTable<T>>
  ScopeTable<T>::addScopeTable(const std::string &scope, std::shared_ptr<ScopeTable<T>> table) {
    assert(scopes[scope] == nullptr);
    scopes[scope] = table;
    return table;
  }

  template<class T>
  std::shared_ptr<ScopeTable<T>> ScopeTable<T>::addScopeTable(const std::string &scope) {
    return addScopeTable(scope, std::make_shared<ScopeTable<T>>());
  }

  template<class T>
  std::shared_ptr<T>
  ScopeTable<T>::findSymbol(const std::vector<std::string> &scope_names,
                            const std::string &identifier) {
    if (scope_names.empty()) return symbols->findSymbol(identifier);
    auto scope = getScopeTable(scope_names);
    if (scope == nullptr) return nullptr;
    return scope->symbols->findSymbol(identifier);
  }

  template<class T>
  std::shared_ptr<T>
  ScopeTable<T>::findSymbol(const std::vector<std::string> &scope_name,
                            const std::string &identifier,
                            std::function<bool(const T &)> predicate) {
    if (scope_name.empty()) return symbols->findSymbol(identifier, predicate);
    auto scope = getScopeTable(scope_name);
    if (scope == nullptr) return nullptr;
    return scope->symbols->findSymbol(identifier, predicate);
  }


  /* the active scopes and symbol tables
   * maintained as a stack -- global namespaces at bottom, local scopes at top */
  template<class T>
  class ActiveScope {
    std::vector<std::shared_ptr<ScopeTable<T>>> scopes;

  public:
    // push a scope onto the stack
    void pushScope(std::shared_ptr<ScopeTable<T>> scope);

    // remove the most recently push scope from the stack
    void popScope();

    // remove the most recently pushed scope from the stack, and assert it is the expected scope
    void popScope(const std::shared_ptr<ScopeTable<T>> &expected);

    // search the scope top down for the first symbol with the given scope and name. Returns nullptr if not found
    std::shared_ptr<T>
    findSymbol(const std::vector<std::string> &scope_names, const std::string &identifier);

    // search the scope top down for the first symbol with the given scope, name, and predicate match. Returns nullptr if not found
    std::shared_ptr<T>
    findSymbol(const std::vector<std::string> &scope_names, const std::string &identifier,
               std::function<bool(const T &)> predicate);

    ActiveScope() :
        scopes(){};
  };

  template<class T>
  void ActiveScope<T>::pushScope(std::shared_ptr<ScopeTable<T>> scope) {
    scopes.push_back(scope);
  }

  template<class T>
  void ActiveScope<T>::popScope() {
    scopes.pop_back();
  }

  template<class T>
  void ActiveScope<T>::popScope(const std::shared_ptr<ScopeTable<T>> &expected) {
    assert(scopes.back().get() == expected.get());
    scopes.pop_back();
  }

  template<class T>
  std::shared_ptr<T>
  ActiveScope<T>::findSymbol(const std::vector<std::string> &scope_names,
                             const std::string &identifier) {
    for (size_t sindex = scopes.size(); sindex--;) {
      auto &scope = scopes[sindex];

      auto found = scope->findSymbol(scope_names, identifier);
      if (found != nullptr) return found;
    }

    return nullptr;
  }

  template<class T>
  std::shared_ptr<T>
  ActiveScope<T>::findSymbol(const std::vector<std::string> &scope_names,
                             const std::string &identifier,
                             std::function<bool(const T &)> predicate) {
    for (size_t sindex = scopes.size(); sindex--;) {
      auto &scope = scopes[sindex];

      auto found = scope->findSymbol(scope_names, identifier, predicate);
      if (found != nullptr) return found;
    }

    return nullptr;
  }

}// namespace ovid

#endif