#include "gtest/gtest.h"

#include "ast.hpp"
#include "error.hpp"
#include "symbols.hpp"
#include "tokenizer.hpp"
#include <sstream>
#include <string>

namespace ovid {

/* make sure all tokens are parsed correctly */
TEST(TokenizerTest, Tokens) {
  std::istringstream input(
      "+-*/ = == val fn {}()mut ,module import return;: hello 123 -23 -0xf0f"
      " 0b110101 "
      "0.345 -3. true false 'a' '\\n'");
  auto testError = TestErrorManager();
  Tokenizer tokenizer("test", &input, testError);

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
  EXPECT_EQ(tokenizer.curToken.token, T_VAL);
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
  std::istringstream input("a val 1 +\n 3 + 4\n( 5\n + 1 )");
  auto testError = TestErrorManager();
  Tokenizer tokenizer("test", &input, testError);

  EXPECT_EQ(tokenizer.curToken.token, T_IDENT);
  tokenizer.nextToken();
  EXPECT_EQ(tokenizer.curToken.token, T_VAL);
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

/* basic AciveScopes test (make sure nested + shadowed symbols work) */
TEST(BasicActiveScopesTest, Symbols) {
  std::vector<std::string> package;
  package.emplace_back("s1");
  package.emplace_back("s2");

  auto loc = SourceLocation("test", 0, 0, nullptr);

  auto scopes = ActiveScopes(package);

  scopes.types.getRootScope()->addScopeTable("test1")->addScopeTable("test2");

  auto table1 = scopes.names.getRootScope()->addScopeTable("test1");
  table1->getDirectScopeTable().addSymbol("test",
                                          std::make_shared<Symbol>(loc));

  auto table2 = table1->addScopeTable("test2");
  table2->getDirectScopeTable().addSymbol("test1",
                                          std::make_shared<Symbol>(loc));

  auto table3 = table1->addScopeTable("test3");
  table3->getDirectScopeTable().addSymbol("test3",
                                          std::make_shared<Symbol>(loc));

  std::vector<std::string> mods;
  mods.emplace_back("test1");
  mods.emplace_back("test2");
  scopes.pushComponentScopesByName(mods);

  EXPECT_NE(scopes.names.findSymbol(std::vector<std::string>(), "test"),
            nullptr);
  EXPECT_NE(scopes.names.findSymbol(std::vector<std::string>(), "test1"),
            nullptr);
  EXPECT_EQ(scopes.names.findSymbol(std::vector<std::string>(), "test3"),
            nullptr);
  EXPECT_NE(
      scopes.names.findSymbol(std::vector<std::string>(1, "test3"), "test3"),
      nullptr);
}

TEST(ActiveScopesDeathTest, Symbols) {
  std::vector<std::string> package;
  package.emplace_back("s1");
  package.emplace_back("s2");

  auto scopes = ActiveScopes(package);
  scopes.types.getRootScope()->addScopeTable("test1")->addScopeTable("test2");
  std::vector<std::string> mods;
  mods.emplace_back("test1");
  mods.emplace_back("test2");

  // test1:test2 isn't in scopes.names
  EXPECT_EXIT(scopes.pushComponentScopesByName(mods),
              ::testing::KilledBySignal(SIGABRT), "");

  scopes.names.getRootScope()->addScopeTable("test1")->addScopeTable("test2");
  scopes.pushComponentScopesByName(mods);

  scopes.names.pushScope(std::make_shared<ScopeTable<Symbol>>());
  scopes.types.pushScope(std::make_shared<ScopeTable<TypeAlias>>());

  // blank tables were pushed and not popped
  EXPECT_EXIT(scopes.popComponentScopesByName(mods),
              ::testing::KilledBySignal(SIGABRT), "");

  // table already added
  EXPECT_EXIT(scopes.names.getRootScope()->addScopeTable("test1"),
              ::testing::KilledBySignal(SIGABRT), "");
}

} // namespace ovid
