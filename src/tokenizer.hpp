#ifndef H_TOKENIZER_INCL
#define H_TOKENIZER_INCL

#include <utility>
#include <string>
#include <fstream>

namespace ovid {
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

        T_VARDECL = 7,
        T_FN = 8,

        T_LBRK = 9,
        T_RBRK = 10,
        T_LPAREN = 11,
        T_RPAREN = 12,
        T_REF = 13,
        T_MUTREF = 14,
        T_MUT = 15,
        T_COMMA = 16,

        T_IDENT = -2,
        T_INTLITERAL = -3,
        T_FLOATLITERAL = -4,
        T_BOOLLITERAL = -5,
        T_CHARLITERAL = -6,
    };

    /* location in source code */
    struct SourceLocation {
        const std::string filename;
        int64_t col, row;
        std::fstream& file; /* file may be null (if input isn't from a file, or is non seekable (like stdin) */

        SourceLocation(const std::string filename, int64_t row, int64_t col, std::fstream& file): filename(filename), col(col), row(row), file(file) {};
    };

    struct Token {
        enum TokenType token;
        int64_t int_literal;
        double float_literal;
        bool bool_literal;
        char char_literal;
        std::string ident;

        /* last doc comment that was present in the input. Used to allow compiler to embed documentation in its outputs (if needed) */
        std::string last_doc_comment;
        /* how many tokens ago the doc comment occurred */
        int64_t last_doc_comment_loc;
        /* current location to be parsed */
    };

    class Tokenizer {
        /* character to add back if we over read */
        char putback_char;

        int64_t comment_nesting_level;

        /* current line we are on */
        uint64_t line;
        /* current position in line */
        uint64_t pos_in_line;
        std::fstream& file;

    public:
        Tokenizer(const std::string& filename, std::fstream& file): putback_char('\0'), comment_nesting_level(0), line(1), pos_in_line(0), file(file), curTokenLoc(filename, 1, 0, file) {};
        /* scan and read the next token */
        void nextToken();
        /* last token read */
        Token curToken;
        /* location of beginning of curToken*/
        SourceLocation curTokenLoc;
    private:
        /* put back a character if we read to far */
        void putback(char c);

        /* read the next char (no whitespace, etc handling ) */
        char next();
        /* skip whitespace */
        int skip();
    };
}

#endif