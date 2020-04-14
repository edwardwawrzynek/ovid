#include <iostream>
#include <cstdio>
#include "ast.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"
#include "error.hpp"

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cerr << "usage: ovid source.ovd\n";
        exit(1);
    }
    auto filein = std::fstream(argv[1]);
    auto lexer = ovid::Tokenizer(argv[1], &filein);
    lexer.nextToken();
    auto parser = ovid::Parser(lexer);

    auto ast = parser.parseProgram();

    if (ovid::errorOccurred()) {
        std::cout << "\x1b[1;31merror\x1b[;1m: compilation failed\n\x1b[m";
        return 1;
    } else {
        std::cout << "\x1b[1;32mCompilation succeeded\n\x1b[m";
        return 0;
    }

}
