#ifndef H_TOKENIZER_INCL
#define H_TOKENIZER_INCL

#include "error.hpp"
#include <fstream>
#include <string>
#include <utility>

namespace ovid {
enum TokenType {
  T_EOF = -1,
  /* if unknown, int_literal holds char value */
  T_UNKNOWN = 0,

  T_ADD = 1,
  T_SUB = 2,
  T_STAR = 3, /* also used for pointers */
  T_DIV = 4,

  T_ASSIGN = 5,
  T_EQ = 6,
  T_FN = 8,

  T_LBRK = 9,
  T_RBRK = 10,
  T_LPAREN = 11,
  T_RPAREN = 12,

  T_VAL = 14,
  T_MUT = 15,
  T_COMMA = 16,

  T_MODULE = 17,
  T_IMPORT = 18,

  T_RETURN = 19,

  T_SEMICOLON = 20,
  T_COLON = 21,

  T_RIGHT_ARROW = 22, /* ->, not => */
  T_PUB = 23,
  T_DOUBLE_COLON = 24,

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