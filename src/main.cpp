#include "ast.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "tokenizer.hpp"
#include <cstdio>
#include <iostream>

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "usage: ovid source.ovd\n";
    exit(1);
  }
  auto filein = std::fstream(argv[1]);

  auto errorMan = ovid::PrintingErrorManager();
  auto lexer = ovid::Tokenizer(argv[1], &filein, errorMan);
  lexer.nextToken();

  auto scopes = ovid::ActiveScopes();
  // add root scopes
  scopes.names.pushScope(std::make_shared<ovid::ScopeTable<ovid::Symbol>>());
  scopes.types.pushScope(std::make_shared<ovid::ScopeTable<ovid::TypeAlias>>());

  auto parser = ovid::Parser(lexer, errorMan, scopes);

  std::vector<std::string> package;
  // package.push_back("std");
  // package.push_back("test");

  auto ast = parser.parseProgram(package);

  if (errorMan.errorOccurred()) {
    std::cout << "\x1b[1;31merror\x1b[;1m: compilation failed\n\x1b[m";
    return 1;
  } else {
    std::cout << "\x1b[1;32mCompilation succeeded\n\x1b[m";
    return 0;
  }
}
