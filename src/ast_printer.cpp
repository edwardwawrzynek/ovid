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
  auto type = node.getFormalBoundFunctionType();
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " FunctionDecl: " << node.name << "\n";
  state.printIndent(output);
  output << "  retType:\n";
  typePrinter.visitTypeConstructor(*type->retType, state.withIndent());
  state.printIndent(output);
  output << "  argNames:";
  for (auto &argName : type->argNames) {
    output << " " << argName;
  }
  output << "\n";
  state.printIndent(output);
  output << "  argTypes:\n";
  for (auto &type : type->argTypes) {
    typePrinter.visitTypeConstructor(*type, state.withIndent());
  }
  state.printIndent(output);
  output << "  body:\n";
  for (auto &body : node.body.statements) {
    visitNode(*body, state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitImplStatement(ImplStatement &node,
                                   const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " ImplStatement\n";
  state.printIndent(output);
  output << "  type_params:";
  for (const auto &param : node.header->type_params) {
    output << " " << param->name;
  }
  output << "\n";
  state.printIndent(output);
  output << "  type:\n";
  typePrinter.visitType(*node.header->type, state.withIndent());
  state.printIndent(output);
  output << "  body:\n";
  for (auto &body : node.body) {
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

int ASTPrinter::visitWhileStatement(WhileStatement &node,
                                    const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " WhileStatement\n";
  state.printIndent(output);
  output << "  condition:\n";
  visitNode(*node.cond, state.withIndent());
  state.printIndent(output);
  output << "  body:\n";
  for (auto &stat : node.body.statements) {
    visitNode(*stat, state.withIndent());
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
  if (!node.type_params.empty()) {
    state.printIndent(output);
    output << "  type_params:\n";
    for (auto &type : node.type_params) {
      typePrinter.visitType(*type, state.withIndent());
    }
  }

  return 0;
}

std::map<OperatorType, std::string> printOperatorMap = {
    {OperatorType::ADD, "+"},
    {OperatorType::SUB, "-"},
    {OperatorType::MUL, "*"},
    {OperatorType::DIV, "/"},
    {OperatorType::MOD, "%"},
    {OperatorType::NEGATIVE, "-"},
    {OperatorType::DEREF, "*"},
    {OperatorType::ADDR, "&"},
    {OperatorType::ADD_ASSIGN, "+="},
    {OperatorType::SUB_ASSIGN, "-="},
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
    {OperatorType::RIGHT_SHIFT, ">>"},
    {OperatorType::UNSAFE_PTR_ADD, "__unsafe_ptr_add"},
    {OperatorType::UNSAFE_PTR_CAST, "__unsafe_ptr_cast"}};

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

int ASTPrinter::visitFloatLiteral(FloatLiteral &node,
                                  const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " FloatLiteral: " << node.value << "\n";

  return 0;
}

int ASTPrinter::visitCharLiteral(CharLiteral &node,
                                 const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " CharLiteral: 0x" << std::hex << (int)(node.value) << std::dec
         << "\n";

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

int ASTPrinter::visitStructExpr(StructExpr &node,
                                const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " StructExpr:\n";
  typePrinter.visitTypeConstructor(*node.type, state.withIndent());
  for (size_t i = 0; i < node.field_exprs.size(); i++) {
    state.printIndent(output);
    output << "  " << node.field_names[i] << ":\n";
    visitNode(*node.field_exprs[i], state.withIndent());
  }

  return 0;
}

int ASTPrinter::visitTypeAliasDecl(TypeAliasDecl &node,
                                   const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " TypeAliasDecl: " << node.name << "\n";
  typePrinter.visitTypeConstructor(*node.type->type, state.withIndent());

  return 0;
}

int ASTPrinter::visitReturnStatement(ReturnStatement &node,
                                     const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " ReturnStatement\n";
  if (node.expression != nullptr)
    visitNode(*node.expression, state.withIndent());
  return 0;
}

int ASTPrinter::visitFieldAccess(FieldAccess &node,
                                 const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " FieldAccess ";
  if (node.has_field_num)
    output << node.field_num;
  else
    output << node.field;
  output << "\n";
  visitNode(*node.lvalue, state.withIndent());
  return 0;
}

int ASTPrinter::visitNativeFunctionDecl(NativeFunctionDecl &node,
                                        const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " NativeFunctionDecl ";
  auto name = node.sym->getFullyScopedName();
  output << scopedNameToString(name) << "\n";
  typePrinter.visitTypeConstructor(*node.sym->type, state.withIndent());
  return 0;
}

int ASTPrinter::visitSizeof(Sizeof &node, const ASTPrinterState &state) {
  state.printIndent(output);
  printLoc(output, node.loc);
  output << " Sizeof\n";
  typePrinter.visitType(*node.sizeof_type, state.withIndent());
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
  visitTypeConstructor(*type.type, state.withIndent());

  return 0;
}

int ASTTypePrinter::visitPointerType(PointerType &type,
                                     const ASTPrinterState &state) {
  state.printIndent(output);
  output << "PointerType\n";
  visitTypeConstructor(*type.type, state.withIndent());

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

int ASTTypePrinter::visitFunctionType(FunctionType &type,
                                      const ASTPrinterState &state) {
  state.printIndent(output);
  output << "FunctionType\n";
  state.printIndent(output);
  output << "  retType:\n";
  visitTypeConstructor(*type.retType, state.withIndent());
  state.printIndent(output);
  output << "  argTypes:\n";
  for (auto &arg : type.argTypes) {
    visitTypeConstructor(*arg, state.withIndent());
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
  visitFunctionType(type, state.withIndent());

  return 0;
}

int ASTTypePrinter::visitTupleType(TupleType &type,
                                   const ASTPrinterState &state) {
  state.printIndent(output);
  output << "TupleType\n";
  for (auto &child : type.types) {
    visitTypeConstructor(*child, state.withIndent());
  }

  return 0;
}

int ASTTypePrinter::visitStructType(StructType &type,
                                    const ASTPrinterState &state) {
  state.printIndent(output);
  output << "StructType ";
  auto name = type.getTypeAlias()->getFullyScopedName();
  output << scopedNameToString(name) << "\n";
  // if present, print generic type params
  auto &params = type.actual_generic_params;
  if (!params.empty()) {
    state.printIndent(output);
    output << "  actual_generic_params:\n";
    for (auto &param : params) {
      visitTypeConstructor(*param, state.withIndent());
    }
  }

  return 0;
}

int ASTTypePrinter::visitFormalTypeParameter(FormalTypeParameter &type,
                                             const ASTPrinterState &state) {
  state.printIndent(output);
  output << "FormalTypeParameter: " << type.name << "\n";

  return 0;
}

int ASTTypePrinter::visitGenericTypeConstructor(
    GenericTypeConstructor &type_construct, const ASTPrinterState &state) {
  state.printIndent(output);
  output << "GenericTypeConstructor:";
  for (auto &param : type_construct.params) {
    output << " " << param->name;
  }
  output << "\n";
  visitTypeConstructor(*type_construct.type, state.withIndent());

  return 0;
}

} // namespace ovid::ast