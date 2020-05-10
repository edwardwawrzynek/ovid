#include "ast.hpp"
#include "ast_visitor.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "resolve_pass.hpp"
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

  std::vector<std::string> package;
  package.emplace_back("std");
  package.emplace_back("scope");
  auto scopes = ovid::ActiveScopes(package);

  auto parser = ovid::Parser(lexer, errorMan, scopes, package);

  auto ast = parser.parseProgram();
  parser.removePushedPackageScope();

  auto resolvePass = ovid::ast::ResolvePass(scopes, errorMan, package);
  resolvePass.visitNodes(ast, ovid::ast::ResolvePassState());
  resolvePass.removePushedPackageScope();

  if (errorMan.errorOccurred()) {
    std::cout << "\x1b[1;31merror\x1b[;1m: compilation failed\n\x1b[m";
    return 1;
  } else {
    std::cout << "\x1b[1;32mcompilation succeeded\n\x1b[m";
    return 0;
  }
}
