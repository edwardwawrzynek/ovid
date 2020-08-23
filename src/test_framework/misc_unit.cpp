#include "gtest/gtest.h"

#include "ast.hpp"
#include "error.hpp"
#include "escape_analysis.hpp"
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

  EXPECT_FALSE(testError.criticalErrorOccurred());
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

  EXPECT_FALSE(testError.criticalErrorOccurred());
}

struct MockSymbol {
public:
  int val;
  std::string name;
  ScopeTable<MockSymbol> *parent_table;

  MockSymbol(int val) : val(val), name(), parent_table(nullptr){};
};

bool operator==(const MockSymbol &lhs, const MockSymbol &rhs) {
  return lhs.val == rhs.val;
}

/* basic ScopeTable test (make sure namespace nesting works) */
TEST(BasicScopeTable, Symbols) {
  auto table = std::make_shared<ScopeTable<MockSymbol>>(true, nullptr, "");
  table->addSymbol(std::vector<std::string>(), "test",
                   std::make_shared<MockSymbol>(1));
  EXPECT_EQ(*table->findSymbol(std::vector<std::string>(), "test"), 1);

  auto sscope = table->addScopeTable("scope", true);
  sscope->addSymbol(std::vector<std::string>(), "test",
                    std::make_shared<MockSymbol>(2));
  EXPECT_EQ(*table->findSymbol(std::vector<std::string>(1, "scope"), "test"),
            2);
  EXPECT_EQ(*table->findSymbol(std::vector<std::string>(), "test"), 1);

  sscope->addSymbol(std::vector<std::string>(), "test2",
                    std::make_shared<MockSymbol>(3));
  EXPECT_EQ(table->findSymbol(std::vector<std::string>(), "test2"), nullptr);

  table->addSymbol(std::vector<std::string>(), "test",
                   std::make_shared<MockSymbol>(10));
  EXPECT_EQ(*table->findSymbol(std::vector<std::string>(), "test",
                               [](MockSymbol sym) { return sym.val == 10; }),
            10);
}

/* basic AciveScopes test (make sure nested + shadowed symbols work) */
TEST(BasicActiveScopesTest, Symbols) {
  std::vector<std::string> package;
  package.emplace_back("s1");
  package.emplace_back("s2");

  auto loc = SourceLocation("test", 0, 0, 0, 0, nullptr);

  auto root_scope = ScopesRoot();
  auto scopes =
      ActiveScopes(package, -1, root_scope.names.get(), root_scope.types.get());

  auto t1 = scopes.types.getRootScope()->addScopeTable("test1", true);
  t1->addScopeTable("test2", true);

  auto table1 = scopes.names.getRootScope()->addScopeTable("test1", true);
  table1->addSymbol("test", std::make_shared<Symbol>(loc));

  auto table2 = table1->addScopeTable("test2", true);
  table2->addSymbol("test1", std::make_shared<Symbol>(loc));

  auto table3 = table1->addScopeTable("test3", true);
  table3->addSymbol("test3", std::make_shared<Symbol>(loc));

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

  auto root_scope = ScopesRoot();
  auto scopes =
      ActiveScopes(package, -1, root_scope.names.get(), root_scope.types.get());
  auto t1 = scopes.types.getRootScope()->addScopeTable("test1", true);
  t1->addScopeTable("test2", true);
  std::vector<std::string> mods;
  mods.emplace_back("test1");
  mods.emplace_back("test2");

  // test1:test2 isn't in scopes.names
  EXPECT_EXIT(scopes.pushComponentScopesByName(mods),
              ::testing::KilledBySignal(SIGABRT), "");

  auto t2 = scopes.names.getRootScope()->addScopeTable("test1", true);
  t2->addScopeTable("test2", true);
  scopes.pushComponentScopesByName(mods);

  auto st1 = ScopeTable<Symbol>(true, nullptr, "");
  auto st2 = ScopeTable<TypeAlias>(true, nullptr, "");

  scopes.names.pushScope(&st1);
  scopes.types.pushScope(&st2);

  // blank tables were pushed and not popped
  EXPECT_EXIT(scopes.popComponentScopesByName(mods),
              ::testing::KilledBySignal(SIGABRT), "");

  // table already added
  EXPECT_EXIT(scopes.names.getRootScope()->addScopeTable("test1", true),
              ::testing::KilledBySignal(SIGABRT), "");
}

TEST(EscapeAnalysisValueContains, EscapeAnalsysis) {
  auto loc = SourceLocation("test", 0, 0, 0, 0, nullptr);

  ast::TypeList types1;
  types1.push_back(std::make_shared<ast::BoolType>(loc));

  ast::TypeList types2;
  types2.push_back(std::make_shared<ast::PointerType>(
      loc, std::make_shared<ast::TupleType>(loc, types1)));
  auto expr = ir::Expression(loc, ir::Value(),
                             std::make_shared<ast::TupleType>(loc, types2));

  std::vector<std::vector<int32_t>> field_sels;
  field_sels.emplace_back();
  field_sels.emplace_back();
  auto val1 = ir::FlowValue(expr, 1, field_sels);
  field_sels[0].push_back(0);
  auto val2 = ir::FlowValue(expr, 1, field_sels);
  field_sels[1].push_back(0);
  auto val3 = ir::FlowValue(expr, 1, field_sels);

  EXPECT_TRUE(val1.fieldsMatchOrContain(val2));
  EXPECT_TRUE(val1.fieldsMatchOrContain(val3));
  EXPECT_TRUE(val2.fieldsMatchOrContain(val3));

  EXPECT_FALSE(val2.fieldsMatchOrContain(val1));
  EXPECT_FALSE(val3.fieldsMatchOrContain(val2));
  EXPECT_FALSE(val3.fieldsMatchOrContain(val1));

  EXPECT_TRUE(val1.fieldsMatchOrContain(val1));
  EXPECT_TRUE(val2.fieldsMatchOrContain(val2));
  EXPECT_TRUE(val3.fieldsMatchOrContain(val3));
}

} // namespace ovid
