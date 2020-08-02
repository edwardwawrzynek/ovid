#include "name_mangle.hpp"

namespace ovid::name_mangling {

std::string mangle(const std::vector<std::string> &name, MangleType type) {
  if (name.size() == 1 && type == MangleType::IDENTIFIER) {
    return name[0];
  } else {
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
    for (auto &scope : name) {
      res.append(std::to_string(scope.size()));
      res.append(scope);
    }

    return res;
  }
}

std::string mangle(const std::vector<std::string> &name) {
  return mangle(name, MangleType::IDENTIFIER);
}

std::string mangle(const ir::Value &val) {
  if (val.hasSourceName) {
    return mangle(val.sourceName);
  } else {
    return std::to_string(val.id);
  }
}

} // namespace ovid::name_mangling