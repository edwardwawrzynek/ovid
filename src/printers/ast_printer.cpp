#include "ast_printer.hpp"
#include <iostream>

namespace ovid::ast {

ASTPrinterState ASTPrinterState::withIndent() const {
  auto res = ASTPrinterState();
  res.indent_level = indent_level + 1;
  return res;
}

void ASTPrinterState::printIndent() const {
  for(uint64_t i = 0; i < indent_level; i++) {
    std::cout << "\t";
  }
}

void ASTPrinter::printLoc(const SourceLocation &loc) {
  std::cout << "(" << loc.row << ":" << loc.col << ")";
}

int ASTPrinter::visitModuleDecl(ModuleDecl &node,
                                const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " ModuleDecl: ";
  for(auto &scope: node.scope) {
    std::cout << scope << ":";
  }
  std::cout << "\n";
  for(auto &body: node.body) {
    visitNode(*body, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitVarDecl(VarDecl &node, const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " VarDecl: " << node.name << "\n";
  visitNode(*node.initialValue, state.withIndent());

  return 0;
}

int ASTPrinter::visitFunctionDecl(FunctionDecl &node,
                                  const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " FunctionDecl: " << node.name << "\n";
  state.printIndent();
  std::cout << "  retType:\n";
  typePrinter.visitType(*node.type->type->retType, state.withIndent());
  state.printIndent();
  std::cout << "  argNames:";
  for(auto& argName: node.type->argNames) {
    std::cout << " " << argName;
  }
  std::cout << "\n";
  state.printIndent();
  std::cout << "  argTypes:\n";
  for(auto& type: node.type->type->argTypes) {
    typePrinter.visitType(*type, state.withIndent());
  }
  state.printIndent();
  std::cout << "  body:\n";
  for(auto& body: node.body.statements) {
    visitNode(*body, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitIfStatement(IfStatement &node,
                                 const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " IfStatement\n";
  for(size_t i = 0; i < node.conditions.size(); i++) {
    state.printIndent();
    std::cout << "  condition:\n";
    visitNode(*node.conditions[i], state.withIndent());
    state.printIndent();
    std::cout << "  body:\n";
    for(auto &statement: node.bodies[i].statements) {
      visitNode(*statement, state.withIndent());
    }
  }
}

int ASTPrinter::visitFunctionCall(FunctionCall &node,
                                  const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " FunctionCall\n";
  state.printIndent();
  std::cout << "  function:\n";
  visitNode(*node.funcExpr, state.withIndent());
  state.printIndent();
  std::cout << "  args:\n";
  for(auto& arg: node.args) {
    visitNode(*arg, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitIdentifier(Identifier &node,
                                const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " Identifier: ";
  if(node.is_root_scope) std::cout << "::";
  for(auto& scope: node.scope) std::cout << scope;
  std::cout << node.id;
  std::cout << "\n";

  return 0;
}

static std::map<OperatorType, std::string> printOperatorMap = {
    {OperatorType::ADD, "+"},
    {OperatorType::SUB, "-"},
    {OperatorType::MUL, "*"},
    {OperatorType::DIV, "/"},
    {OperatorType::MOD, "%"},
    {OperatorType::NEGATIVE, "-"},
    {OperatorType::DEREF, "*"},
    {OperatorType::ADDR, "&"},
    {OperatorType::PREFIX_INC, "++"},
    {OperatorType::POSTFIX_INC, "++"},
    {OperatorType::PREFIX_DEC, "--"},
    {OperatorType::POSTFIX_DEC, "--"},
    {OperatorType::BIN_AND, "&"},
    {OperatorType::BIN_OR, "|"},
    {OperatorType::BIN_XOR, "^"},
    {OperatorType::BIN_NOT, "~"},
    {OperatorType::LOG_AND, "&&"},
    {OperatorType::LOG_OR, "||"},
    {OperatorType::LOG_NOT, "!"},
    {OperatorType::EQUAL, "=="},
    {OperatorType::NEQUAL, "!="},
    {OperatorType::GREATER, ">"},
    {OperatorType::GREATER_EQUAL, ">="},
    {OperatorType::LESS, "<"},
    {OperatorType::LESS_EQUAL, "<="},
    {OperatorType::LEFT_SHIFT, "<<"},
    {OperatorType::RIGHT_SHIFT, ">>"}
};

int ASTPrinter::visitOperatorSymbol(OperatorSymbol &node,
                                    const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " OperatorSymbol: " << printOperatorMap[node.op] << "\n";

  return 0;
}

int ASTPrinter::visitAssignment(Assignment &node,
                                const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " Assignment\n";
  state.printIndent();
  std::cout << "  rvalue:\n";
  visitNode(*node.rvalue, state.withIndent());
  state.printIndent();
  std::cout << "  lvalue:\n";
  visitNode(*node.lvalue, state.withIndent());

  return 0;
}

int ASTPrinter::visitIntLiteral(IntLiteral &node,
                                const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " IntLiteral: " << node.value << "\n";

  return 0;
}

int ASTPrinter::visitBoolLiteral(BoolLiteral &node,
                                 const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " BoolLiteral: " << node.value << "\n";

  return 0;
}

int ASTPrinter::visitTuple(Tuple &node, const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " Tuple\n";
  for(auto& expr: node.expressions) {
    visitNode(*expr, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitTypeAliasDecl(TypeAliasDecl &node,
                                   const ASTPrinterState &state) {
  state.printIndent();
  printLoc(node.loc);
  std::cout << " TypeAliasDecl: " << node.name << "\n";
  typePrinter.visitType(*node.type->type, state.withIndent());

  return 0;
}

}