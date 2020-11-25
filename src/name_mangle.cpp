#include "name_mangle.hpp"
#include "ast_visitor.hpp"

namespace ovid::name_mangling {

enum class MangleType {
  IDENTIFIER, // identifier namespace: _I prefix
  TYPE_ALIAS  // type alias namespace: _T prefix
};

template <typename T>
static std::string mangleScope(const ScopeTable<T> *scope, MangleType type) {
  /* choose _I or _T prefix */
  std::string res;
  switch (type) {
  case MangleType::IDENTIFIER:
    res = "_I";
    break;
  case MangleType::TYPE_ALIAS:
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
      res.push_back('V');
      res.append(std::to_string(std::to_string(ver).size() + 3));
      res.push_back('_');
      res.append(std::to_string(ver));
      res.push_back('_');
    }
  }

  return res;
}

template <typename T> static std::string mangle(const T *sym, MangleType type) {
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

/* type mangler visitor */
class TypeManglerState {};

std::map<int, char> signed_int_codes = {
    {8, 'c'}, {16, 's'}, {32, 'i'}, {64, 'l'}};

std::map<int, char> unsigned_int_codes = {
    {8, 'h'}, {16, 't'}, {32, 'j'}, {64, 'm'}};

class TypeMangler : public ast::BaseTypeVisitor<int, TypeManglerState> {
  std::string res;

  int visitVoidType(ast::VoidType &type,
                    const TypeManglerState &state) override {
    res.push_back('v');
    return 0;
  }

  int visitBoolType(ast::BoolType &type,
                    const TypeManglerState &state) override {
    res.push_back('b');
    return 0;
  }

  int visitIntType(ast::IntType &type, const TypeManglerState &state) override {
    res.push_back(type.isUnsigned ? unsigned_int_codes[type.size]
                                  : signed_int_codes[type.size]);
    return 0;
  }

  int visitFloatType(ast::FloatType &type,
                     const TypeManglerState &state) override {
    res.push_back(type.size == 32 ? 'f' : 'd');
    return 0;
  }

  int visitMutType(ast::MutType &type, const TypeManglerState &state) override {
    res.push_back('M');
    visitType(*type.type, state);
    return 0;
  }

  int visitPointerType(ast::PointerType &type,
                       const TypeManglerState &state) override {
    res.push_back('P');
    visitType(*type.type, state);
    return 0;
  }

  int visitFunctionType(ast::FunctionType &type,
                        const TypeManglerState &state) override {
    res.push_back('F');
    // args
    res.push_back('A');
    for (auto &arg : type.argTypes) {
      visitType(*arg, state);
    }
    res.push_back('E');
    // return type
    visitType(*type.retType, state);
    return 0;
  }

  int visitNamedFunctionType(ast::NamedFunctionType &type,
                             const TypeManglerState &state) override {
    return visitFunctionType(type, state);
  }

  int visitTupleType(ast::TupleType &type,
                     const TypeManglerState &state) override {
    res.push_back('R');
    for (auto &field : type.types) {
      visitType(*field, state);
    }
    res.push_back('E');

    return 0;
  }

  int visitStructType(ast::StructType &type,
                      const TypeManglerState &state) override {
    res.append(mangle(type.getTypeAlias(), MangleType::TYPE_ALIAS));
    res.push_back('G');
    for (auto &param : type.actual_generic_params) {
      visitType(*param, state);
    }
    res.push_back('E');

    return 0;
  }

  int visitTypeList(const ast::TypeList &types, const TypeManglerState &state) {
    res.push_back('G');
    for (auto &type : types) {
      visitType(*type, state);
    }
    res.push_back('E');

    return 0;
  }

public:
  void clear() { res = ""; }

  std::string getRes() { return res; }

  std::string getType(ast::Type &type) {
    clear();
    visitType(type, TypeManglerState());
    return res;
  }

  std::string getTypeList(const ast::TypeList &types) {
    clear();
    visitTypeList(types, TypeManglerState());
    return res;
  }

  explicit TypeMangler() : BaseTypeVisitor(0){};
};

TypeMangler type_mangler;

std::string mangleType(ast::Type &type) { return type_mangler.getType(type); }

std::string mangleMainFunc(const ScopeTable<Symbol> *package,
                           const std::string &name) {
  std::string scope = mangleScope(package, MangleType::IDENTIFIER);
  scope.append(std::to_string(name.size()));
  scope.append(name);

  return scope;
}

std::string mangleIdentifier(const std::shared_ptr<Symbol> &sym) {
  return mangle(sym.get(), MangleType::IDENTIFIER);
}

std::string mangleIdentifier(const ir::Id &id) {
  std::string res;
  if (id.hasSourceName) {
    res.append(mangleIdentifier(id.sourceName));
  } else {
    res.append("_U");
    res.append(std::to_string(id.id));
  }

  if (!id.typeParams.empty()) {
    res.append(type_mangler.getTypeList(id.typeParams));
  }

  return res;
}

std::string mangleIdentifier(const ir::Value &val) {
  return mangleIdentifier(val.id);
}

} // namespace ovid::name_mangling