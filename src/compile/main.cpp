#include "ast.hpp"
#include "ast_printer.hpp"
#include "error.hpp"
#include "escape_analysis.hpp"
#include "ir.hpp"
#include "ir_printer.hpp"
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
  ovid::ir::reset_id();
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

  std::cout << "---- AST ----\n";
  auto astPrinter = ovid::ast::ASTPrinter(std::cout);
  astPrinter.visitNodes(ast, ovid::ast::ASTPrinterState());

  auto ir = ovid::ast::typeCheckProduceIR(errorMan, package, ast);

  if (errorMan.criticalErrorOccurred()) {
    std::cout << "\x1b[1;31mtype checking failed";
    return 1;
  }

  std::cout << "\n---- IR ----\n";
  auto irPrinter = ovid::ir::IRPrinter(std::cout);
  irPrinter.visitInstructions(ir, ovid::ast::ASTPrinterState());

  // run escape analysis
  std::cout << "\n---- ESCAPE ANALYSIS ----\n";
  ovid::ir::runEscapeAnalysis(ir, true, std::cout);

  if (errorMan.criticalErrorOccurred()) {
    std::cout << "\x1b[1;31merror\x1b[;1m: compilation failed\n\x1b[m";
    return 1;
  } else {
    std::cout << "\x1b[1;32mcompilation succeeded\n\x1b[m";
    return 0;
  }
}
