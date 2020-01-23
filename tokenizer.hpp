#include <iostream>
#include <utility>
#include <cstring>

namespace tokenizer {
    enum TokenType {
        T_EOF = -1,
        /* if unknown, int_literal holds char value */
        T_UNKNOWN = 0,

        T_ADD = 1,
        T_SUB = 2,
        T_MUL = 3,
        T_DIV = 4,

        T_ASSIGN = 5,
        T_EQ = 6,

        T_IDENT = -2,
        T_INTLITERAL = -3,
        T_FLOATLITERAL = -4,
        T_BOOLLITERAL = -5,
        T_CHARLITERAL = -6,
    };

    struct Token {
        enum TokenType token;
        int64_t int_literal;
        double float_literal;
        bool bool_literal;
        char char_literal;
        std::string ident;
    };

    extern Token curToken;

    class Tokenizer {
        /* character to add back if we over read */
        char putback_char;

        char (*getc)();
    public:
        /* current line we are on */
        uint64_t line;
        /* current position in line */
        uint64_t pos_in_line;

        Tokenizer(char (*getc)());
        /* scan and read the next token */
        void nextToken();
    private:
        /* put back a character if we read to far */
        void putback(char c);

        /* read the next char (no whitespace, etc handling ) */
        char next();
        /* skip whitespace */
        int skip();
    };
}