#include <iostream>
#include <cstdio>
#include "ast.hpp"
#include "tokenizer.hpp"

FILE * fin;

char input_getc() {
    return fgetc(fin);
}

int main(int argc, char **argv) {
    /*if(argc != 2) {
        fprintf(stderr, "usage: ovid file.ovd");
        exit(1);
    }
    fin = fopen(argv[1], "r");*/
    fin = fopen("/home/edward/Documents/ovid/test.ovd", "r");
    if(fin == NULL) {
        fprintf(stderr, "can't open source files\n");
        exit(1);
    }
    auto lexer = new tokenizer::Tokenizer(&input_getc);

    while(tokenizer::curToken.token != tokenizer::T_EOF) {
        lexer->nextToken();
        printf("token type: %i, int literal: %i, string literal: %s\n", tokenizer::curToken.token, tokenizer::curToken.int_literal, tokenizer::curToken.ident.c_str());
    }

}
