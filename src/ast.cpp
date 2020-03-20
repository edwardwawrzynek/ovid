
#include "ast.hpp"

namespace ovid::ast {

    Type* ast::Type::withoutmutability() {
        return this;
    }

    Type* ast::MutType::withoutmutability() {
        return type.get();
    }
}