#include "name_mangle.hpp"

namespace ovid::name_mangling {

enum class MangleType {
  IDENTIFIER, // identifier namespace: _I prefix
  TYPE        // Type namespace: _T prefix
};

template <typename T>
static std::string mangleScope(const ScopeTable<T> *scope, MangleType type) {
  /* choose _I or _T prefix */
  std::string res;
  switch (type) {
  case MangleType::IDENTIFIER:
    res = "_I";
    break;
  case MangleType::TYPE:
    res = "_T";
    break;
  default:
    assert(false);
  }

  std::vector<std::string> scopes;
  std::vector<int64_t> version_nums;
  while (scope != nullptr && !scope->getName().empty()) {
    scopes.push_back(scope->getName());
    version_nums.push_back(scope->getVersionInt());
    scope = scope->getParent();
  }

  for (size_t i = scopes.size(); i--;) {
    auto name = scopes[i];
    res.append(std::to_string(name.size()));
    res.append(name);
    auto ver = version_nums[i];
    if (ver != -1) {
      res.append(std::to_string(std::to_string(ver).size() + 3));
      res.push_back('_');
      res.push_back('v');
      res.append(std::to_string(ver));
      res.push_back('_');
    }
  }

  return res;
}

template <typename T>
static std::string mangle(const std::shared_ptr<T> &sym, MangleType type) {
  // generate c compatible identifier names if unscoped
  if ((sym->parent_table == nullptr || sym->parent_table->getName().empty()) &&
      type == MangleType::IDENTIFIER) {
    return sym->name;
  } else {
    std::string scope = mangleScope(sym->parent_table, type);
    scope.append(std::to_string(sym->name.size()));
    scope.append(sym->name);

    return scope;
  }
}

std::string mangleMainFunc(const ScopeTable<Symbol> *package,
                           const std::string &name) {
  std::string scope = mangleScope(package, MangleType::IDENTIFIER);
  scope.append(std::to_string(name.size()));
  scope.append(name);

  return scope;
}

std::string mangleIdentifier(const std::shared_ptr<Symbol> &sym) {
  return mangle(sym, MangleType::IDENTIFIER);
}

std::string mangleIdentifier(const ir::Value &val) {
  if (val.hasSourceName) {
    return mangleIdentifier(val.sourceName);
  } else {
    std::string res = "_U";
    res.append(std::to_string(val.id));
    return res;
  }
}

std::string mangleType(const std::shared_ptr<TypeAlias> &sym) {
  return mangle(sym, MangleType::TYPE);
}

} // namespace ovid::name_mangling