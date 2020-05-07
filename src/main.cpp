#include "ast.hpp"
#include "ast_visitor.hpp"
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
  // add package namespaces
  scopes.names.getRootScope()->addScopeTable("std")->addScopeTable("test");

  auto parser = ovid::Parser(lexer, errorMan, scopes);

  std::vector<std::string> package;
  package.emplace_back("std");
  package.emplace_back("test");

  auto ast = parser.parseProgram(package);

  auto a = ovid::ast::BaseASTVisitor<int>(0);
  a.visitNode(*ast[0]);

  if (errorMan.errorOccurred()) {
    std::cout << "\x1b[1;31merror\x1b[;1m: compilation failed\n\x1b[m";
    return 1;
  } else {
    std::cout << "\x1b[1;32mCompilation succeeded\n\x1b[m";
    return 0;
  }
}
