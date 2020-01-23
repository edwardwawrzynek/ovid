#include <iostream>
#include <utility>
#include <cstring>

#include "tokenizer.hpp"

namespace tokenizer {

    Token curToken;

    Tokenizer::Tokenizer(char (*getc)()): putback_char('\0'), getc(getc), line(0), pos_in_line(0) {

    }
    /* put back a character if we read to far */
    void Tokenizer::putback(char c) {
        putback_char = c;
    }

    /* read the next char (no whitespace, etc handling ) */
    char Tokenizer::next() {
        char c;
        if(putback_char != '\0') {
            c = putback_char;
            putback_char = '\0';
            return c;
        }

        c = getc();
        pos_in_line++;
        if(c == '\n') {
            pos_in_line = 0;
            line++;
        }
        return c;
    }

    /* skip whitespace */
    int Tokenizer::skip() {
        int c;

        do {
            c = next();
        } while(c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\f');

        return c;
    }

    /* scan and read the next token */
    void Tokenizer::nextToken() {
        int c;

        c = skip();

        switch(c) {
            case EOF:
                curToken.token = T_EOF;
                break;
            case '+':
                curToken.token = T_ADD;
                break;
            case '-':
                curToken.token = T_SUB;
                break;
            case '*':
                curToken.token = T_MUL;
                break;
            case '/':
                curToken.token = T_DIV;
                break;
            case '=':
                if((c = skip()) == '=') {
                    curToken.token = T_EQ;
                } else {
                    curToken.token = T_ASSIGN;
                    putback(c);
                }
                break;

            default:
                if(isdigit(c)) {
                    /* parse number */
                    curToken.int_literal = 0;
                    curToken.token = T_INTLITERAL;
                    do {
                        curToken.int_literal *= 10;
                        curToken.int_literal += (c - '0');
                        c = next();
                    } while(isdigit(c));
                    putback(c);
                } else if(isalnum(c) || c == '_') {
                    /* parse identifier */
                    curToken.ident = "";
                    curToken.token = T_IDENT;
                    do {
                        curToken.ident += c;
                        c = next();
                    } while(isalnum(c));
                    putback(c);
                } else {
                    curToken.token = T_UNKNOWN;
                    curToken.char_literal = c;
                }
        }

    }
}
