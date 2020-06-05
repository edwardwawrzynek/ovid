#include "ast_printer.hpp"
#include <iomanip>
#include <iostream>

namespace ovid::ast {

ASTPrinterState ASTPrinterState::withIndent() const {
  auto res = ASTPrinterState();
  res.indent_level = indent_level + 1;
  return res;
}

void ASTPrinterState::printIndent(std::ostream &output) const {
  for (uint64_t i = 0; i < indent_level; i++) {
    output << "    ";
  }
}

void printLoc(std::ostream &output, const SourceLocation &loc) {
  output << "(" << loc.row << ":" << loc.col << ")";
}

int ASTPrinter::visitModuleDecl(ModuleDecl &node,
                                const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " ModuleDecl: ";
  for (size_t i = 0; i < node.scope.size(); i++) {
    auto &scope = node.scope[i];
    output << scope;
    if (i < node.scope.size() - 1)
      output << ":";
  }
  output << "\n";
  for (auto &body : node.body) {
    visitNode(*body, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitVarDecl(VarDecl &node, const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " VarDecl: " << node.name << "\n";
  visitNode(*node.initialValue, state.withIndent());

  return 0;
}

int ASTPrinter::visitFunctionDecl(FunctionDecl &node,
                                  const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " FunctionDecl: " << node.name << "\n";
  state.printIndent(output);
  output << "  retType:\n";
  typePrinter.visitType(*node.type->type->retType, state.withIndent());
  state.printIndent(output);
  output << "  argNames:";
  for (auto &argName : node.type->argNames) {
    output << " " << argName;
  }
  output << "\n";
  state.printIndent(output);
  output << "  argTypes:\n";
  for (auto &type : node.type->type->argTypes) {
    typePrinter.visitType(*type, state.withIndent());
  }
  state.printIndent(output);
  output << "  body:\n";
  for (auto &body : node.body.statements) {
    visitNode(*body, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitIfStatement(IfStatement &node,
                                 const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " IfStatement\n";
  for (size_t i = 0; i < node.conditions.size(); i++) {
    state.printIndent(output);
    output << "  condition:\n";
    visitNode(*node.conditions[i], state.withIndent());
    state.printIndent(output);
    output << "  body:\n";
    for (auto &statement : node.bodies[i].statements) {
      visitNode(*statement, state.withIndent());
    }
  }

  return 0;
}

int ASTPrinter::visitFunctionCall(FunctionCall &node,
                                  const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " FunctionCall\n";
  state.printIndent(output);
  output << "  function:\n";
  visitNode(*node.funcExpr, state.withIndent());
  state.printIndent(output);
  output << "  args:\n";
  for (auto &arg : node.args) {
    visitNode(*arg, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitIdentifier(Identifier &node,
                                const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " Identifier: ";
  if (node.is_root_scope)
    output << "::";
  for (auto &scope : node.scope) {
    output << scope << ":";
  }
  output << node.id;
  output << "\n";

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
    {OperatorType::RIGHT_SHIFT, ">>"}};

int ASTPrinter::visitOperatorSymbol(OperatorSymbol &node,
                                    const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " OperatorSymbol: " << printOperatorMap[node.op] << "\n";

  return 0;
}

int ASTPrinter::visitAssignment(Assignment &node,
                                const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " Assignment\n";
  state.printIndent(output);
  output << "  lvalue:\n";
  visitNode(*node.lvalue, state.withIndent());
  state.printIndent(output);
  output << "  rvalue:\n";
  visitNode(*node.rvalue, state.withIndent());

  return 0;
}

int ASTPrinter::visitIntLiteral(IntLiteral &node,
                                const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " IntLiteral: " << node.value << "\n";

  return 0;
}

int ASTPrinter::visitBoolLiteral(BoolLiteral &node,
                                 const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " BoolLiteral: " << (node.value ? "true" : "false") << "\n";

  return 0;
}

int ASTPrinter::visitTuple(Tuple &node, const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " Tuple\n";
  for (auto &expr : node.expressions) {
    visitNode(*expr, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitTypeAliasDecl(TypeAliasDecl &node,
                                   const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " TypeAliasDecl: " << node.name << "\n";
  typePrinter.visitType(*node.type->type, state.withIndent());

  return 0;
}

int ASTTypePrinter::visitVoidType(VoidType &type,
                                  const ASTPrinterState &state) {
  state.printIndent(output);
  output << "VoidType\n";

  return 0;
}

int ASTTypePrinter::visitBoolType(BoolType &type,
                                  const ASTPrinterState &state) {
  state.printIndent(output);
  output << "BoolType\n";

  return 0;
}

int ASTTypePrinter::visitIntType(IntType &type, const ASTPrinterState &state) {
  state.printIndent(output);
  output << "IntType: " << (type.isUnsigned ? "u" : "i") << type.size << "\n";

  return 0;
}

int ASTTypePrinter::visitFloatType(FloatType &type,
                                   const ASTPrinterState &state) {
  state.printIndent(output);
  output << "FloatType: "
         << "f" << type.size << "\n";

  return 0;
}

int ASTTypePrinter::visitMutType(MutType &type, const ASTPrinterState &state) {
  state.printIndent(output);
  output << "MutType\n";
  visitType(*type.type, state.withIndent());

  return 0;
}

int ASTTypePrinter::visitPointerType(PointerType &type,
                                     const ASTPrinterState &state) {
  state.printIndent(output);
  output << "PointerType\n";
  visitType(*type.type, state.withIndent());

  return 0;
}

int ASTTypePrinter::visitUnresolvedType(UnresolvedType &type,
                                        const ASTPrinterState &state) {
  state.printIndent(output);
  output << "UnresolvedType: ";
  if (type.is_root_scoped)
    output << "::";
  for (auto &scope : type.scopes) {
    output << scope << ":";
  }
  output << type.name << "\n";

  return 0;
}

int ASTTypePrinter::visitResolvedAlias(ResolvedAlias &type,
                                       const ASTPrinterState &state) {
  state.printIndent(output);
  output << "ResolvedAlias\n";
  visitType(*type.alias->type, state.withIndent());

  return 0;
}

int ASTTypePrinter::visitFunctionType(FunctionType &type,
                                      const ASTPrinterState &state) {
  state.printIndent(output);
  output << "FunctionType\n";
  state.printIndent(output);
  output << "  retType:\n";
  visitType(*type.retType, state.withIndent());
  state.printIndent(output);
  output << "  argTypes:\n";
  for (auto &arg : type.argTypes) {
    visitType(*arg, state.withIndent());
  }

  return 0;
}

int ASTTypePrinter::visitNamedFunctionType(NamedFunctionType &type,
                                           const ASTPrinterState &state) {
  state.printIndent(output);
  output << "NamedFunctionType\n";
  state.printIndent(output);
  output << "  argNames:";
  for (auto &name : type.argNames) {
    output << " " << name;
  }
  output << "\n";
  state.printIndent(output);
  output << "  type:\n";
  visitType(*type.type, state.withIndent());

  return 0;
}

} // namespace ovid::ast