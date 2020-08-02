#ifndef NAME_MANGLE_H
#define NAME_MANGLE_H

#include "ir.hpp"
#include <string>
#include <vector>

/* The name mangling scheme for ovid symbols
 * Right now it just encodes scoped names, but TODO: it will have to support
 * generics (and therefor type encoding).
 *
 * Non scoped symbols of a single name are not mangled.
 * Other symbols are mangled:
 * They start with _I, followed by a sequence of Nscope, with N is the length
 * and scope is the scope name
 *
 * The symbol test:scope:func would be:
 * _I4test6scope4func */

namespace ovid::name_mangling {

enum class MangleType {
  IDENTIFIER, // identifier namespace: _I prefix
  TYPE        // Type namespace: _T prefix
};

std::string mangle(const std::vector<std::string> &name, MangleType type);
std::string mangle(const std::vector<std::string> &name);
std::string mangle(const ir::Value &val);

} // namespace ovid::name_mangling

#endif