#include <iostream>

#include "tokenizer.hpp"

namespace ovid {
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

        c = file.get();
        pos_in_line++;
        if(c == '\n') {
            pos_in_line = 0;
            line++;
        }
        return c;
    }

    /* skip whitespace and comments */
    int Tokenizer::skip() {
        int c, nnC;
        bool isDocComment;

        do {
            c = next();
        } while(c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\f');

        /* handle comments */
        if(c == '/') {
            int nC = next();
            if(nC == '/') {
                nnC = next();
                isDocComment = nnC == '/';
                if(!isDocComment) putback(nnC);

                do {
                    nC = next();
                    if(isDocComment) curToken.last_doc_comment += nC;
                } while(nC != '\n');
                return skip();
            } else if(nC == '*') {
                comment_nesting_level++;
                /* check for doc comment (/**) */
                nnC = next();
                isDocComment = nnC == '*';
                /* continue until nesting is at level 0 */
                do {
                    nC = next();
                    nnC = next();
                    if(nC == '*' && nnC == '/') comment_nesting_level--;
                    else putback(nnC);
                    if(isDocComment && comment_nesting_level != 0) curToken.last_doc_comment += nC;
                } while(comment_nesting_level > 0);
                if(isDocComment) curToken.last_doc_comment += "\n";
                return skip();

            } else {
                putback(nC);
            }
        }

        return c;
    }

    /* scan and read the next token */
    void Tokenizer::nextToken() {
        int c;

        curToken.last_doc_comment_loc++;

        c = skip();

        curTokenLoc.col = pos_in_line;
        curTokenLoc.row = line;

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
                if((c = next()) == '=') {
                    curToken.token = T_EQ;
                } else {
                    curToken.token = T_ASSIGN;
                    putback(c);
                }
                break;
            case ':':
                if((c = next()) == '=') {
                    curToken.token = T_VARDECL;
                } else {
                    putback(c);
                    curToken.token = T_UNKNOWN;
                    curToken.char_literal = c;
                }
                break;
            case '{':
                curToken.token = T_LBRK;
                break;
            case '}':
                curToken.token = T_RBRK;
                break;
            case '(':
                curToken.token = T_LPAREN;
                break;
            case ')':
                curToken.token = T_RPAREN;
                break;
            case '&':
                curToken.token = T_REF;
                if((c = next()) == 'm') {
                    if((c = next()) == 'u') {
                        if((c = next()) == 't') {
                            curToken.token = T_MUTREF;
                        } else putback(c);
                    } else putback(c);
                } else putback(c);
                break;
            case ',':
                curToken.token = T_COMMA;
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

                    /* check for keywords */
                    if(curToken.ident == "fn") {
                        curToken.token = T_FN;
                    } else if(curToken.ident == "true") {
                        curToken.token = T_BOOLLITERAL;
                        curToken.bool_literal = true;
                    } else if(curToken.ident == "false") {
                        curToken.ident = T_BOOLLITERAL;
                        curToken.bool_literal = false;
                    } else if(curToken.ident == "mut") {
                        curToken.token = T_MUT;
                    }
                } else {
                    curToken.token = T_UNKNOWN;
                    curToken.char_literal = c;
                }
        }

        /* TODO: handle error on T_UNKNOWN */
    }
}
