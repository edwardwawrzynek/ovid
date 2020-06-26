#include "name_mangle.hpp"

namespace ovid::name_mangling {

std::string mangle(const std::vector<std::string> &name) {
  if (name.size() == 1) {
    return name[0];
  } else {
    std::string res = "_I";
    for (auto &scope : name) {
      res.append(std::to_string(scope.size()));
      res.append(scope);
    }

    return res;
  }
}

std::string mangle(const ir::Value &val) {
  if (val.hasSourceName) {
    return mangle(val.sourceName);
  } else {
    return std::to_string(val.id);
  }
}

} // namespace ovid::name_mangling