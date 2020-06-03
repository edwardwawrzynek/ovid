#include "ast.hpp"
#include "ast_printer.hpp"
#include "error.hpp"
#include "ir.hpp"
#include "ir_visitor.hpp"
#include "parser.hpp"
#include "resolve_pass.hpp"
#include "tokenizer.hpp"
#include "type_check.hpp"
#include <iostream>

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << "usage: ovid source.ovd\n";
    exit(1);
  }
  auto filein = std::fstream(argv[1]);

  auto errorMan = ovid::PrintingErrorManager();
  auto lexer = ovid::Tokenizer(argv[1], &filein, errorMan);

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

  auto astPrinter = ovid::ast::ASTPrinter(std::cout);
  astPrinter.visitNodes(ast, ovid::ast::ASTPrinterState());

  auto typeCheck = ovid::ast::TypeCheck();

  auto ir = typeCheck.visitNodes(ast, ovid::ast::TypeCheckState());

  if (errorMan.errorOccurred()) {
    std::cout << "\x1b[1;31merror\x1b[;1m: compilation failed\n\x1b[m";
    return 1;
  } else {
    std::cout << "\x1b[1;32mcompilation succeeded\n\x1b[m";
    return 0;
  }
}
