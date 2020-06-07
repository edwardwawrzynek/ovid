#include "ir_printer.hpp"

namespace ovid::ir {

void ovid::ir::IRPrinter::printValue(const ovid::ir::Value &val) {
  if (val.hasSourceName) {
    for (size_t i = 0; i < val.sourceName.size(); i++) {
      auto &scope = val.sourceName[i];
      output << scope;

      if (i < val.sourceName.size() - 1)
        output << ":";
    }
  } else {
    output << "%" << val.id;
  }
}

int ovid::ir::IRPrinter::visitIntLiteral(
    ovid::ir::IntLiteral &instruct, const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = INTLITERAL " << type_printer.getType(*instruct.type) << " "
         << instruct.value << "\n";

  return 0;
}

int IRPrinter::visitBoolLiteral(BoolLiteral &instruct,
                                const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = BOOLLITERAL " << type_printer.getType(*instruct.type) << " "
         << (instruct.value ? "true" : "false") << "\n";

  return 0;
}

int ovid::ir::IRPrinter::visitAllocation(
    ovid::ir::Allocation &instruct, const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = ALLOCATION " << type_printer.getType(*instruct.type) << "\n";

  return 0;
}

int ovid::ir::IRPrinter::visitLabel(ovid::ir::Label &instruct,
                                    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\tLABEL @" << instruct.id << "\n";

  return 0;
}

int ovid::ir::IRPrinter::visitJump(ovid::ir::Jump &instruct,
                                   const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\tJUMP @" << instruct.label.id << "\n";

  return 0;
}

int ovid::ir::IRPrinter::visitConditionalJump(
    ovid::ir::ConditionalJump &instruct,
    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\tCONDITIONALJUMP ";
  printValue(instruct.condition.val);
  output << " @" << instruct.true_label.id << "(true) @"
         << instruct.false_label.id << "(false)\n";

  return 0;
}

int ovid::ir::IRPrinter::visitStore(ovid::ir::Store &instruct,
                                    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\tSTORE ";
  printValue(instruct.storage.val);
  output << " <- ";
  printValue(instruct.value.val);
  output << "\n";

  return 0;
}

int ovid::ir::IRPrinter::visitFunctionCall(
    ovid::ir::FunctionCall &instruct, const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);
  output << " = FUNCTIONCALL " << type_printer.getType(*instruct.type) << " ";
  printValue(instruct.function.val);
  output << "(";
  for (size_t i = 0; i < instruct.arguments.size(); i++) {
    auto &arg = instruct.arguments[i].get();
    printValue(arg.val);
    if (i < instruct.arguments.size() - 1)
      output << ", ";
  }
  output << ")\n";

  return 0;
}

int ovid::ir::IRPrinter::visitFunctionDeclare(
    ovid::ir::FunctionDeclare &instruct,
    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = FUNCTIONDECLARE " << type_printer.getType(*instruct.type)
         << " ";
  state.printIndent(output);
  output << "{\n";
  for (auto &body : instruct.body) {
    visitInstruction(*body, state.withIndent());
  }
  state.printIndent(output);
  output << "}\n";

  return 0;
}

int IRPrinter::visitAddress(Address &instruct,
                            const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);
  output << " = ADDRESS " << type_printer.getType(*instruct.type) << " ";
  printValue(instruct.expr.val);
  output << "\n";

  return 0;
}

int IRPrinter::visitDereference(Dereference &instruct,
                                const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);
  output << " = DEREFERENCE " << type_printer.getType(*instruct.type) << " ";
  printValue(instruct.expr.val);
  output << "\n";

  return 0;
}
} // namespace ovid::ir
