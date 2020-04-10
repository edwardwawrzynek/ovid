#ifndef H_ERROR_INCL
#define H_ERROR_INCL

#include <string>
#include <cstdio>
#include <iostream>
#include "tokenizer.hpp"

namespace ovid {
    /**
     * Types of errors and warnings
     * These will be grouped into error levels that can be enabled and disabled
     * Fatal errors can be lumped into broad categories
     * Non fatal errors should be in more specific categories
     */
    enum ErrorType {
        PARSE_ERROR
    };

    std::nullptr_t logError(const std::string& msg, SourceLocation location, ErrorType type);
    bool errorOccurred();
}

#endif