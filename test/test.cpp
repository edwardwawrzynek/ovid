#include "gtest/gtest.h"

#include <sstream>
#include "tokenizer.hpp"

namespace ovid {

  TEST(TokenizerTest, Tokens) {
    std::istringstream input("+-*/ = == := fn {}()mut ,module import return;: hello 123 -23 -0xf0f"
                             " 0b110101 "
                             "0.345 true false 'a' '\\n'");
    Tokenizer tokenizer("test", &input);

    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_ADD);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_SUB);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_STAR);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_DIV);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_ASSIGN);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_EQ);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_VARDECL);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_FN);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_LBRK);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_RBRK);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_LPAREN);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_RPAREN);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_MUT);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_COMMA);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_MODULE);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_IMPORT);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_RETURN);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_SEMICOLON);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_COLON);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_IDENT);
    EXPECT_EQ(tokenizer.curToken.ident, "hello");
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
    EXPECT_EQ(tokenizer.curToken.int_literal, 123);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
    EXPECT_EQ(tokenizer.curToken.int_literal, -23);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
    EXPECT_EQ(tokenizer.curToken.int_literal, -3855);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
    EXPECT_EQ(tokenizer.curToken.int_literal, 53);
    tokenizer.nextToken();
    EXPECT_EQ(tokenizer.curToken.token, T_FLOATLITERAL);
    EXPECT_EQ(tokenizer.curToken.float_literal, 0.345);
  }

  TEST(SampleTest, Sub) {
    EXPECT_EQ(1 - 1, 0);
  }

}