#include "gtest/gtest.h"

#include "error.hpp"
#include "symbols.hpp"
#include "tokenizer.hpp"
#include <sstream>
#include <string>

namespace ovid {

/* make sure all tokens are parsed correctly */
TEST(TokenizerTest, Tokens) {
  std::istringstream input(
      "+-*/ = == := fn {}()mut ,module import return;: hello 123 -23 -0xf0f"
      " 0b110101 "
      "0.345 -3. true false 'a' '\\n'");
  auto testError = TestErrorManager();
  Tokenizer tokenizer("test", &input, testError);

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
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_FLOATLITERAL);
  EXPECT_EQ(tokenizer.curToken.float_literal, -3.0);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_BOOLLITERAL);
  EXPECT_EQ(tokenizer.curToken.bool_literal, true);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_BOOLLITERAL);
  EXPECT_EQ(tokenizer.curToken.bool_literal, false);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_CHARLITERAL);
  EXPECT_EQ(tokenizer.curToken.char_literal, 'a');
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_CHARLITERAL);
  EXPECT_EQ(tokenizer.curToken.char_literal, '\n');

  EXPECT_FALSE(testError.errorOccurred());
}

/* make sure semicolons are inserted correctly */
TEST(SemicolonInsertion, Tokens) {
  std::istringstream input("a := 1 +\n 3 + 4\n( 5\n + 1 )");
  auto testError = TestErrorManager();
  Tokenizer tokenizer("test", &input, testError);

  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_IDENT);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_VARDECL);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_ADD);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_ADD);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_SEMICOLON);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_LPAREN);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_ADD);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_INTLITERAL);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_RPAREN);

  EXPECT_FALSE(testError.errorOccurred());
}

/* basic ScopeTable test (make sure namespace nesting works) */
TEST(BasicScopeTable, Symbols) {
  auto table = ScopeTable<int>();
  table.addSymbol(std::vector<std::string>(), "test", std::make_shared<int>(1));
  EXPECT_EQ(*table.findSymbol(std::vector<std::string>(), "test"), 1);

  auto sscope = table.addScopeTable("scope");
  sscope->addSymbol(std::vector<std::string>(), "test",
                    std::make_shared<int>(2));
  EXPECT_EQ(*table.findSymbol(std::vector<std::string>(1, "scope"), "test"), 2);
  EXPECT_EQ(*table.findSymbol(std::vector<std::string>(), "test"), 1);

  sscope->addSymbol(std::vector<std::string>(), "test2",
                    std::make_shared<int>(3));
  EXPECT_EQ(table.findSymbol(std::vector<std::string>(), "test2"), nullptr);

  table.addSymbol(std::vector<std::string>(), "test",
                  std::make_shared<int>(10));
  EXPECT_EQ(*table.findSymbol(std::vector<std::string>(), "test",
                              [](int sym) { return sym == 10; }),
            10);
}
} // namespace ovid