#ifndef H_IR_PRINTER_INCL
#define H_IR_PRINTER_INCL

#include "ir_visitor.hpp"
#include "ast_printer.hpp"

namespace ovid::ir {
class IRPrinter: public BaseIRVisitor<int, ast::ASTPrinterState> {
  std::ostream& output;
  ast::ASTTypePrinter type_printer;

  void printValue(const Value& val);

  int visitFunctionDeclare(FunctionDeclare &instruct, const ast::ASTPrinterState &state) override;
  int visitIntLiteral(IntLiteral &instruct, const ast::ASTPrinterState &state) override;
  int visitFunctionCall(FunctionCall &instruct, const ast::ASTPrinterState &state) override;
  int visitAllocation(Allocation &instruct, const ast::ASTPrinterState &state) override;

  int visitStore(Store &instruct, const ast::ASTPrinterState &state) override;
  int visitLabel(Label &instruct, const ast::ASTPrinterState &state) override;
  int visitJump(Jump &instruct, const ast::ASTPrinterState &state) override;
  int visitConditionalJump(ConditionalJump &instruct, const ast::ASTPrinterState &state) override;
  
public:
  IRPrinter(std::ostream& output): BaseIRVisitor(0), output(output), type_printer(output) {};
  
};
}

#endif