#include <iostream>
#include <cstdio>
#include "ast.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"

int main(int argc, char **argv) {
    /*if(argc != 2) {
        fprintf(stderr, "usage: ovid file.ovd");
        exit(1);
    }
    fin = fopen(argv[1], "r");*/
    auto filein = std::fstream("/home/edward/Documents/ovid/test.ovd");
    auto lexer = ovid::Tokenizer("test.ovd", filein);
    lexer.nextToken();
    auto parser = ovid::Parser(lexer);

    parser.parsePrimary();

}
