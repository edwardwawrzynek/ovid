#ifndef H_IR_PRINTER_INCL
#define H_IR_PRINTER_INCL

#include "ast_printer.hpp"
#include "ir_visitor.hpp"
#include "type_check.hpp"

namespace ovid::ir {
class IRPrinter : public BaseIRVisitor<int, ast::ASTPrinterState> {
  std::ostream &output;
  ast::TypePrinter type_printer;

  void printId(const Id &id);
  void printValue(const Value &val);

  int visitGenericFunctionDeclare(GenericFunctionDeclare &instruct,
                                  const ast::ASTPrinterState &state) override;
  int visitFunctionDeclare(FunctionDeclare &instruct,
                           const ast::ASTPrinterState &state) override;
  int visitIntLiteral(IntLiteral &instruct,
                      const ast::ASTPrinterState &state) override;
  int visitBoolLiteral(BoolLiteral &instruct,
                       const ast::ASTPrinterState &state) override;
  int visitFloatLiteral(FloatLiteral &instruct,
                        const ast::ASTPrinterState &state) override;
  int visitTupleLiteral(TupleLiteral &instruct,
                        const ast::ASTPrinterState &state) override;
  int visitFunctionCall(FunctionCall &instruct,
                        const ast::ASTPrinterState &state) override;
  int visitAllocation(Allocation &instruct,
                      const ast::ASTPrinterState &state) override;
  int visitGlobalAllocation(GlobalAllocation &instruct,
                            const ast::ASTPrinterState &state) override;
  int visitAddress(Address &instruct,
                   const ast::ASTPrinterState &state) override;
  int visitDereference(Dereference &instruct,
                       const ast::ASTPrinterState &state) override;
  int visitBuiltinOperator(BuiltinOperator &instruct,
                           const ast::ASTPrinterState &state) override;
  int visitBuiltinCast(BuiltinCast &instruct,
                       const ast::ASTPrinterState &state) override;
  int visitFieldSelect(FieldSelect &instruct,
                       const ast::ASTPrinterState &state) override;
  int visitForwardIdentifier(ForwardIdentifier &instruct,
                             const ast::ASTPrinterState &state) override;
  int visitSpecialize(Specialize &instruct,
                      const ast::ASTPrinterState &state) override;
  int visitSizeof(Sizeof &instruct, const ast::ASTPrinterState &state) override;
  int visitGenericForwardIdentifier(GenericForwardIdentifier &instruct,
                                    const ast::ASTPrinterState &state) override;
  int visitImpl(Impl &instruct, const ast::ASTPrinterState &state) override;
  int visitGenericImpl(GenericImpl &instruct,
                       const ast::ASTPrinterState &state) override;
  int visitImplFnExtract(ImplFnExtract &instruct,
                         const ast::ASTPrinterState &state) override;
  int visitImplGenericFnExtract(ImplGenericFnExtract &instruct,
                                const ast::ASTPrinterState &state) override;

  int visitStore(Store &instruct, const ast::ASTPrinterState &state) override;
  int visitBasicBlock(BasicBlock &instruct,
                      const ast::ASTPrinterState &state) override;
  int visitJump(Jump &instruct, const ast::ASTPrinterState &state) override;
  int visitConditionalJump(ConditionalJump &instruct,
                           const ast::ASTPrinterState &state) override;
  int visitReturn(Return &instruct, const ast::ASTPrinterState &state) override;

public:
  explicit IRPrinter(std::ostream &output)
      : BaseIRVisitor(0), output(output), type_printer(){};
};
} // namespace ovid::ir

#endif