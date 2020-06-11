#ifndef H_IR_PRINTER_INCL
#define H_IR_PRINTER_INCL

#include "ast_printer.hpp"
#include "ir_visitor.hpp"
#include "type_check.hpp"

namespace ovid::ir {
class IRPrinter : public BaseIRVisitor<int, ast::ASTPrinterState> {
  std::ostream &output;
  ast::TypePrinter type_printer;

  void printValue(const Value &val);

  int visitFunctionDeclare(FunctionDeclare &instruct,
                           const ast::ASTPrinterState &state) override;
  int visitIntLiteral(IntLiteral &instruct,
                      const ast::ASTPrinterState &state) override;
  int visitBoolLiteral(BoolLiteral &instruct,
                       const ast::ASTPrinterState &state) override;
  int visitTupleLiteral(TupleLiteral &instruct,
                        const ast::ASTPrinterState &state) override;
  int visitFunctionCall(FunctionCall &instruct,
                        const ast::ASTPrinterState &state) override;
  int visitAllocation(Allocation &instruct,
                      const ast::ASTPrinterState &state) override;
  int visitAddress(Address &instruct,
                   const ast::ASTPrinterState &state) override;
  int visitDereference(Dereference &instruct,
                       const ast::ASTPrinterState &state) override;
  int visitBuiltinOperator(BuiltinOperator &instruct,
                           const ast::ASTPrinterState &state) override;
  int visitBuiltinCast(BuiltinCast &instruct,
                       const ast::ASTPrinterState &state) override;

  int visitStore(Store &instruct, const ast::ASTPrinterState &state) override;
  int visitBasicBlock(BasicBlock &instruct,
                      const ast::ASTPrinterState &state) override;
  int visitJump(Jump &instruct, const ast::ASTPrinterState &state) override;
  int visitConditionalJump(ConditionalJump &instruct,
                           const ast::ASTPrinterState &state) override;
  int visitReturn(Return &instruct, const ast::ASTPrinterState &state) override;

public:
  IRPrinter(std::ostream &output)
      : BaseIRVisitor(0), output(output), type_printer(){};
};
} // namespace ovid::ir

#endif