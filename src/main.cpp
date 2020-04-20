#include <iostream>
#include <cstdio>
#include "ast.hpp"
#include "tokenizer.hpp"
#include "parser.hpp"
#include "error.hpp"


int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "usage: ovid source.ovd\n";
    exit(1);
  }
  auto filein = std::fstream(argv[1]);

  auto errorMan = ovid::ErrorManager();
  auto lexer = ovid::Tokenizer(argv[1], &filein);
  lexer.nextToken();
  auto parser = ovid::Parser(lexer, errorMan);

  auto s = ovid::SymbolTable<ovid::Symbol>();

  auto scope = std::make_shared<ovid::ScopeTable<ovid::Symbol>>();
  scope->addScopeTable("test_scope")->addScopeTable("nested")->getDirectScopeTable().addSymbol("testSymbol", std::make_shared<ovid::Symbol>());

  std::vector<std::string> scopes;
  scopes.push_back("test_scope");
  scopes.push_back("nested");
  scope->findSymbol(scopes, "testSymbol");

  auto stack = ovid::ActiveScope<ovid::Symbol>();
  stack.pushScope(scope);
  //stack.popScope(scope);

  auto a = stack.findSymbol(scopes, "testSymbol");

  auto ast = parser.parseProgram();

  if (errorMan.errorOccurred()) {
    std::cout << "\x1b[1;31merror\x1b[;1m: compilation failed\n\x1b[m";
    return 1;
  } else {
    std::cout << "\x1b[1;32mCompilation succeeded\n\x1b[m";
    return 0;
  }

}
