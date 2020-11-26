#include <iostream>
#include <map>

#include "tokenizer.hpp"

namespace ovid {
/* put back a character if we read too far */
void Tokenizer::putback(char c) {
  putback_char = c;
  line = pline;
  pos_in_line = ppos_in_line;

  curTokenLoc.end_col = pos_in_line;
  curTokenLoc.end_row = line;
}

/* read the next char (no whitespace, etc handling ) */
char Tokenizer::next() {
  char c;
  if (putback_char != '\0') {
    c = putback_char;
    putback_char = '\0';

    pos_in_line++;
    if (c == '\n') {
      line++;
      pos_in_line = 0;
    }

    curTokenLoc.end_col = pos_in_line;
    curTokenLoc.end_row = line;

    return c;
  }

  auto pEOF = file->eof();
  c = file->get();
  if (file->eof()) {
    // emulate newline at EOF
    if (!pEOF)
      c = '\n';
    else
      c = EOF;
  }

  pline = line;
  ppos_in_line = pos_in_line;

  pos_in_line++;

  if (c == '\n') {
    pos_in_line = 0;
    line++;
  }

  curTokenLoc.end_col = pos_in_line;
  curTokenLoc.end_row = line;

  return c;
}

/* skip whitespace and comments */
int Tokenizer::skip() {
  int c, nnC;
  bool isDocComment;

  do {
    c = next();
  } while (c == ' ' || c == '\t' || c == '\r' || c == '\f');

  /* handle comments */
  if (c == '/') {
    int nC = next();
    if (nC == '/') {
      nnC = next();
      isDocComment = nnC == '/';
      if (!isDocComment)
        putback(nnC);

      do {
        nC = next();
        if (isDocComment)
          curToken.last_doc_comment += nC;
      } while (nC != '\n');
      return '\n';
    } else if (nC == '*') {
      comment_nesting_level++;
      /* check for multi line doc comment */
      nnC = next();
      isDocComment = nnC == '*';
      /* continue until nesting is at level 0 */
      do {
        nC = next();
        nnC = next();
        if (nC == '*' && nnC == '/')
          comment_nesting_level--;
        else
          putback(nnC);
        if (isDocComment && comment_nesting_level != 0)
          curToken.last_doc_comment += nC;
      } while (comment_nesting_level > 0);
      if (isDocComment)
        curToken.last_doc_comment += "\n";
      return skip();

    } else {
      putback(nC);
    }
  }

  return c;
}

static std::string digits = "0123456789abcdef";

/* parse a number (int or float literal) and put it into curToken
 * c is the first digit of the number*/
void Tokenizer::parseNumber(char c) {
  curToken.int_literal = 0;
  curToken.token = T_INTLITERAL;

  int base = 10;

  /* check for hex, binary, or octal */
  if (c == '0') {
    char baseC = next();
    if (baseC == 'x' || baseC == 'X')
      base = 16;
    else if (baseC == 'o' || baseC == 'O')
      base = 8;
    else if (baseC == 'b' || baseC == 'B')
      base = 2;
    else
      putback(baseC);
  }

  unsigned long cValue;

  while ((cValue = digits.find(tolower(c))) != std::string::npos) {
    curToken.int_literal *= base;
    curToken.int_literal += cValue;
    c = next();
  }

  /* if decimal point present, then parse floating point */
  if (c == '.') {
    if (base != 10) {
      putback(c);
      errorMan.logError("Floating point literals must be in base 10",
                        curTokenLoc, ErrorType::ParseError);
      return;
    }
    std::string decimal_part = std::to_string(curToken.int_literal);
    decimal_part.push_back('.');
    while (isdigit(c = next()))
      decimal_part.push_back(c);

    if (c == 'e' || c == 'E') {
      /* handle exponent */
      decimal_part.push_back(c);
      c = next();
      if (c == '-' || c == '+') {
        decimal_part.push_back(c);
      } else {
        putback(c);
      }
      while (isdigit(c = next()))
        decimal_part.push_back(c);
    }

    curToken.float_literal = std::stod(decimal_part);
    curToken.token = T_FLOATLITERAL;
  }

  putback(c);
}

static std::map<char, char> charEscapeMap = {
    {'a', '\a'},  {'b', '\b'},  {'f', '\f'}, {'n', '\n'},
    {'r', '\r'},  {'t', '\t'},  {'v', '\v'}, {'\\', '\\'},
    {'\'', '\''}, {'\"', '\"'}, {'?', '\?'}};

/* keywords */
static std::map<std::string, TokenType> keywordMap = {
    {"fn", T_FN},
    {"mut", T_MUT},
    {"val", T_VAL},
    {"module", T_MODULE},
    {"import", T_IMPORT},
    {"return", T_RETURN},
    {"pub", T_PUB},
    {"type", T_TYPE},
    {"if", T_IF},
    {"elsif", T_ELSIF},
    {"else", T_ELSE},
    {"native", T_NATIVE},
    {"while", T_WHILE},
    {"struct", T_STRUCT},
    {"impl", T_IMPL},
    {"__unsafe_ptr_cast", T_UNSAFE_PTR_CAST},
    {"__unsafe_ptr_add", T_UNSAFE_PTR_ADD},
    {"__unsafe_sizeof", T_UNSAFE_SIZEOF}};

/* scan and read the next token */
void Tokenizer::nextToken() {
  char c;

  if (doTokenPutback) {
    curTokenLoc = locPutback;
    curToken = tokenPutback;

    doTokenPutback = false;

    return;
  }

  curToken.last_doc_comment_loc++;

  c = skip();
  /* save position of start of token */
  curTokenLoc.col = pos_in_line;
  curTokenLoc.row = line;
  /* if newline is present and last token could be the end of a statement, and
   * parenthesis nesting is at level 0, insert a semicolon otherwise, skip
   * newline */
  if (c == '\n') {
    if ((curToken.token == T_IDENT || curToken.token == T_INTLITERAL ||
         curToken.token == T_FLOATLITERAL || curToken.token == T_CHARLITERAL ||
         curToken.token == T_BOOLLITERAL || curToken.token == T_RETURN ||
         curToken.token == T_RPAREN || curToken.token == T_RBRK) &&
        parenLevel <= 0) {
      curToken.token = T_SEMICOLON;
      return;
    } else {
      return nextToken();
    }
  }

  curToken.token = T_UNKNOWN;

  switch (c) {
  case EOF:
    curToken.token = T_EOF;
    break;
  case '+':
    if ((c = next()) == '=') {
      curToken.token = T_ADD_ASSIGN;
    } else {
      putback(c);
      curToken.token = T_ADD;
    }
    break;
  case '-':
    curToken.token = T_SUB;
    if (isdigit(c = next())) {
      parseNumber(c);
      if (curToken.token == T_INTLITERAL)
        curToken.int_literal *= -1;
      else if (curToken.token == T_FLOATLITERAL)
        curToken.float_literal *= -1;
    } else {
      putback(c);
      if ((c = next()) == '>') {
        curToken.token = T_RIGHT_ARROW;
      } else {
        putback(c);
        if ((c = next()) == '=') {
          curToken.token = T_SUB_ASSIGN;
        } else {
          putback(c);
        }
      }
    }
    break;
  case '*':
    curToken.token = T_STAR;
    break;
  case '/':
    curToken.token = T_DIV;
    break;
  case '&':
    curToken.token = T_ADDR;
    if ((c = next()) == '&') {
      curToken.token = T_AND;
    } else {
      putback(c);
    }
    break;
  case '=':
    if ((c = next()) == '=') {
      curToken.token = T_EQ;
    } else {
      curToken.token = T_ASSIGN;
      putback(c);
    }
    break;
  case ':': {
    c = next();
    if (c == ':') {
      curToken.token = T_DOUBLE_COLON;
    } else {
      putback(c);
      curToken.token = T_COLON;
    }
    break;
  }
  case '{':
    curToken.token = T_LBRK;
    break;
  case '}':
    curToken.token = T_RBRK;
    break;
  case '(':
    curToken.token = T_LPAREN;
    parenLevel++;
    break;
  case ')':
    curToken.token = T_RPAREN;
    if (parenLevel > 0)
      parenLevel--;
    break;
  case ',':
    curToken.token = T_COMMA;
    break;
  case ';':
    curToken.token = T_SEMICOLON;
    break;
  case '.':
    curToken.token = T_DOT;
    break;
  case '>':
    curToken.token = T_GREATER;
    if ((c = next()) == '=') {
      curToken.token = T_GREATER_EQUAL;
    } else if (c == '>') {
      curToken.token = T_RSHF;
    } else {
      putback(c);
    }
    break;
  case '<':
    curToken.token = T_LESS;
    if ((c = next()) == '=') {
      curToken.token = T_LESS_EQUAL;
    } else if (c == '<') {
      curToken.token = T_LSHF;
    } else {
      putback(c);
    }
    break;
  case '!':
    curToken.token = T_NOT;
    if ((c = next()) == '=') {
      curToken.token = T_NEQ;
    } else {
      putback(c);
    }
    break;
  case '~':
    curToken.token = T_BIN_NOT;
    break;
  case '^':
    curToken.token = T_BIN_XOR;
    break;
  case '|':
    curToken.token = T_BIN_OR;
    if ((c = next()) == '|') {
      curToken.token = T_OR;
    } else {
      putback(c);
    }
    break;
  case '\'':
    curToken.token = T_CHARLITERAL;
    c = next();
    if (c == '\\') {
      c = next();
      /* TODO: octal, unicode, and hex escape codes */
      char value = charEscapeMap[c];
      if (value == '\0')
        errorMan.logError("Invalid escape character", curTokenLoc,
                          ErrorType::ParseError);
      curToken.char_literal = value;
    } else {
      curToken.char_literal = c;
    }
    if (next() != '\'')
      errorMan.logError("Expected ' to end character literal", curTokenLoc,
                        ErrorType::ParseError);
    break;
  default:
    if (isdigit(c) || curToken.token == T_INTLITERAL) {
      parseNumber(c);
    } else if (isalnum(c) || c == '_') {
      /* parse identifier */
      curToken.ident = "";
      curToken.token = T_IDENT;
      do {
        curToken.ident += c;
        c = next();
      } while (isalnum(c) || c == '_');
      putback(c);

      /* check for keywords */
      if (curToken.ident == "true") {
        curToken.token = T_BOOLLITERAL;
        curToken.bool_literal = true;
      } else if (curToken.ident == "false") {
        curToken.token = T_BOOLLITERAL;
        curToken.bool_literal = false;
      } else if (keywordMap.count(curToken.ident) > 0) {
        curToken.token = keywordMap[curToken.ident];
      }
    } else {
      curToken.token = T_UNKNOWN;
      curToken.char_literal = c;
    }
  }
}

Token Tokenizer::peekNextToken() {
  Token bakToken = curToken;
  SourceLocation locBak = curTokenLoc;

  nextToken();
  /* save token for returning on nextToken */
  doTokenPutback = true;
  tokenPutback = curToken;
  locPutback = curTokenLoc;
  Token res = curToken;
  /* restore previous token */
  curToken = bakToken;
  curTokenLoc = locBak;

  return res;
}

Tokenizer::Tokenizer(const std::string *filename, std::istream *file,
                     ErrorManager &errorMan)
    : putback_char('\0'), comment_nesting_level(0), line(1), pos_in_line(0),
      pline(1), ppos_in_line(0), file(file), errorMan(errorMan), curToken(),
      curTokenLoc(filename, 1, 0, 1, 0, file), doTokenPutback(false),
      locPutback(filename, 1, 0, 1, 0, file), parenLevel(0) {
  nextToken();
}

} // namespace ovid
