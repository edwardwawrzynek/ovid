#ifndef H_TOKENIZER_INCL
#define H_TOKENIZER_INCL

#include "error.hpp"
#include <fstream>
#include <string>
#include <utility>

namespace ovid {
enum TokenType {
  T_EOF,
  T_UNKNOWN,

  T_IDENT,         // identifier
  T_INTLITERAL,    // integer literal
  T_FLOATLITERAL,  // floating point literal
  T_BOOLLITERAL,   // boolean literal
  T_CHARLITERAL,   // character literal
  T_ADD,           // +
  T_ADD_ASSIGN,    // +=
  T_SUB,           // -
  T_SUB_ASSIGN,    // -=
  T_STAR,          // *
  T_DIV,           // /
  T_ADDR,          // &
  T_NOT,           // !
  T_BIN_NOT,       // ~
  T_BIN_OR,        // |
  T_BIN_XOR,       // ^
  T_OR,            // ||
  T_AND,           // &&
  T_LSHF,          // <<
  T_RSHF,          // >>
  T_EQ,            // ==
  T_NEQ,           // !=
  T_GREATER,       // >
  T_GREATER_EQUAL, // >=
  T_LESS,          // <
  T_LESS_EQUAL,    // <=
  T_COMMA,         // ,
  T_SEMICOLON,     // ;
  T_COLON,         // :
  T_DOUBLE_COLON,  // ::
  T_DOT,           // .
  T_RIGHT_ARROW,   // ->
  T_ASSIGN,        // =
  T_LBRK,          // {
  T_RBRK,          // }
  T_LPAREN,        // (
  T_RPAREN,        // )
  T_FN,            // fn
  T_VAL,           // val
  T_MUT,           // mut
  T_MODULE,        // module
  T_IMPORT,        // import
  T_RETURN,        // return
  T_PUB,           // pub
  T_TYPE,          // type
  T_IF,            // if
  T_ELSIF,         // elsif
  T_ELSE,          // else
  T_NATIVE,        // native
  T_WHILE,         // while
  T_STRUCT,        // struct
};

struct Token {
  enum TokenType token;
  int64_t int_literal;
  double float_literal;
  bool bool_literal;
  char char_literal;
  std::string ident;

  /* last doc comment that was present in the input. Used to allow compiler to
   * embed documentation in its outputs (if needed) */
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
  // past line and pos_in_line (for putback)
  uint64_t pline, ppos_in_line;
  std::istream *file;
  ErrorManager &errorMan;

public:
  Tokenizer(const std::string &filename, std::istream *file,
            ErrorManager &errorMan);

  /* scan and read the next token */
  void nextToken();

  /* peek ahead to the next token, but don't change the current token */
  Token peekNextToken();

  /* last token read */
  Token curToken;
  /* location of beginning of curToken*/
  SourceLocation curTokenLoc;

private:
  /* putback token and tokenloc */
  bool doTokenPutback;
  Token tokenPutback;
  SourceLocation locPutback;

  /* level of parenthesis used to control semicolon insertion */
  int parenLevel;

  /* put back a character if we read to far */
  void putback(char c);

  /* read the next char (no whitespace, etc handling ) */
  char next();

  /* skip whitespace */
  int skip();

  void parseNumber(char c);
};
} // namespace ovid

#endif