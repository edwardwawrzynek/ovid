#ifndef H_ERROR_INCL
#define H_ERROR_INCL

#include <string>
#include <cstdio>
#include <iostream>
#include "tokenizer.hpp"

namespace ovid {
    std::nullptr_t logError(const std::string& msg, SourceLocation location);
    bool errorOccurred();
}

#endif