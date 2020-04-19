#include <vector>
#include <map>
#include <unordered_map>
#include <memory>
#include <stack>
#include <functional>
#include <cassert>
#include "ast.hpp"

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
  /* a symbol and it's metadata (type, etc) */
  struct Symbol {
  public:
    /* type of a the symbol */
    std::unique_ptr<ast::Type> type;
    /* TODO: escape analysis metadata and other information loaded from headers */
  };
  /* a type alias and its metadata */
  struct TypeAlias {
  public:
    std::unique_ptr<ast::Type> type;
  };

  /* a hashtable of string -> symbols */
  template <class T>
  class SymbolTable {
    std::unordered_multimap<std::string, std::shared_ptr<T>> table;

  public:
    void addSymbol(const std::string& name, std::shared_ptr<T> symbol);
    std::shared_ptr<T> findSymbol(const std::string& name);
    std::shared_ptr<T> findSymbol(const std::string& name, std::function<bool(const T&)> predicate);

    SymbolTable(): table() {};
  };

  template<class T>
  void SymbolTable<T>::addSymbol(const std::string& name, std::shared_ptr<T> symbol) {
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
  std::shared_ptr<T> SymbolTable<T>::findSymbol(const std::string &name, std::function<bool(const T &)> predicate) {
    auto range = table.equal_range(name);
    // make sure that only one symbol matches the predicate
    // this is used for overloaded functions, and we shouldn't allow multiple overloads with matching types
    #ifndef NDEBUG
      int triggeredCount = 0;
      for(auto it = range.first; it != range.second; it++) {
        if(predicate(*(it->second))) triggeredCount++;
      }
      assert(triggeredCount <= 1);
    #endif
    for(auto it = range.first; it != range.second; it++) {
      if(predicate(*(it->second))) return it->second;
    }

    return nullptr;
  }


  /* a scope with symbols and child scopes */
  template <class T>
  class ScopeTable {
    std::unique_ptr<SymbolTable<T>> symbols;
    std::unordered_map<std::string, std::shared_ptr<ScopeTable<T>>> scopes;
  public:

    SymbolTable<T>& getDirectScopeTable();

    std::shared_ptr<ScopeTable<T>> getScopeTable(const std::vector<std::string>& scopes);
    std::shared_ptr<ScopeTable<T>> addScopeTable(const std::string& scope);
    std::shared_ptr<ScopeTable<T>> addScopeTable(const std::string& scope, std::shared_ptr<ScopeTable<T>> table);

    std::shared_ptr<T> findSymbol(const std::vector<std::string>& scopes, const std::string& identifier);
    std::shared_ptr<T> findSymbol(const std::vector<std::string>& scopes, const std::string& identifier, std::function<bool(const T&)> predicate);

    ScopeTable(): symbols(std::make_unique<SymbolTable<T>>()), scopes() {};
  };

  template<class T>
  SymbolTable<T>& ScopeTable<T>::getDirectScopeTable() {
    return *symbols;
  }

  template<class T>
  std::shared_ptr<ScopeTable<T>> ScopeTable<T>::getScopeTable(const std::vector<std::string>& scopes) {
    auto curTable = this->scopes;
    std::shared_ptr<ScopeTable<T>> childScope = nullptr;
    for(auto& scope: scopes) {
      childScope = curTable[scope];
      if(childScope == nullptr) return nullptr;
      curTable = childScope->scopes;
    }
    return childScope;
  }

  template<class T>
  std::shared_ptr<ScopeTable<T>> ScopeTable<T>::addScopeTable(const std::string& scope, std::shared_ptr<ScopeTable<T>> table) {
    assert(scopes[scope] == nullptr);
    scopes[scope] = table;
    return table;
  }

  template<class T>
  std::shared_ptr<ScopeTable<T>> ScopeTable<T>::addScopeTable(const std::string &scope) {
    return addScopeTable(scope, std::make_shared<ScopeTable<T>>());
  }

  template<class T>
  std::shared_ptr<T> ScopeTable<T>::findSymbol(const std::vector<std::string> &scopes, const std::string &identifier) {
    if(scopes.empty()) return symbols->findSymbol(identifier);
    auto scope = getScopeTable(scopes);
    if(scope == nullptr) return nullptr;
    return scope->symbols->findSymbol(identifier);
  }

  template<class T>
  std::shared_ptr<T> ScopeTable<T>::findSymbol(const std::vector<std::string> &scopes, const std::string &identifier, std::function<bool(const T &)> predicate) {
    if(scopes.empty()) return symbols->findSymbol(identifier, predicate);
    auto scope = getScopeTable(scopes);
    if(scope == nullptr) return nullptr;
    return scope->symbols->findSymbol(identifier, predicate);
  }


  /* the active scopes and symbol tables */
  template <class T>
  class ActiveScope {
    std::stack<std::shared_ptr<ScopeTable<T>>> scopes;

  public:
    void pushScope(const std::shared_ptr<ScopeTable<T>>& scope);
    void popScope(ScopeTable<T>& expected);

    std::optional<T&> findSymbol(const std::vector<std::string>& scopes, const std::string& identifier);
  };

  class ScopeManager {
  public:
    ActiveScope<Symbol> vars;
    ActiveScope<TypeAlias> types;
  };


}