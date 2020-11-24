#ifndef NAME_MANGLE_H
#define NAME_MANGLE_H

#include "ir.hpp"
#include <string>
#include <vector>

/* The name mangling scheme for ovid symbols
 *
 * Non scoped symbols of a single name are not mangled.
 * Other symbols are mangled:
 * They start with _I, followed by a sequence of Nscope, with N is the length
 * and scope is the scope name
 *
 * The symbol test:scope:func would be:
 * _I4test6scope4func
 *
 * Type aliases are mangled the same, with an encoding starting with _T
 *
 * Generic symbols are mangled with their symbol first, then a G, then generic
 * type parameters mangled (below), then an E
 *
 * Type mangling
 *
 *
 * */

namespace ovid::name_mangling {

std::string mangleIdentifier(const ir::Id &id);
std::string mangleIdentifier(const ir::Value &val);
std::string mangleIdentifier(const std::shared_ptr<Symbol> &sym);
std::string mangleType(ast::Type &type);

std::string mangleMainFunc(const ScopeTable<Symbol> *package,
                           const std::string &name);

} // namespace ovid::name_mangling

#endif