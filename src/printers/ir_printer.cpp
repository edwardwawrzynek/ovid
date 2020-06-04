#include "ir_printer.hpp"

void ovid::ir::IRPrinter::printValue(const ovid::ir::Value &val) {
  if(val.hasSourceName) {
    for(size_t i = 0; i < val.sourceName.size(); i++) {
      auto& scope = val.sourceName[i];
      output << scope;

      if(i < val.sourceName.size() - 1) output << ":";
    }
  } else {
    output << "%" << val.id;
  }
}

int ovid::ir::IRPrinter::visitIntLiteral(
    ovid::ir::IntLiteral &instruct, const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << " ";
  printValue(instruct.val);

  output << " = IntLiteral " << instruct.value << "\n";
  type_printer.visitType(*instruct.type, state.withIndent());

  return 0;
}

int ovid::ir::IRPrinter::visitAllocation(
    ovid::ir::Allocation &instruct, const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << " ";
  printValue(instruct.val);

  output << " = Allocation\n";
  type_printer.visitType(instruct.type, state.withIndent());

  return 0;
}

int ovid::ir::IRPrinter::visitLabel(ovid::ir::Label &instruct,
                                    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << " Label @" << instruct.id << "\n";

  return 0;
}

int ovid::ir::IRPrinter::visitJump(ovid::ir::Jump &instruct,
                                   const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << " Jump @" << instruct.label.id << "\n";

  return 0;
}

int ovid::ir::IRPrinter::visitConditionalJump(
    ovid::ir::ConditionalJump &instruct,
    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << " ConditionalJump @" << instruct.label.id << " if ";
  printValue(instruct.condition.val);

  return 0;
}

int ovid::ir::IRPrinter::visitStore(ovid::ir::Store &instruct,
                                    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << " Store ";
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
  output << " ";
  printValue(instruct.val);
  output << " = FunctionCall ";
  printValue(instruct.function.val);
  output << "(";
  for(size_t i = 0; i < instruct.arguments.size(); i++) {
    auto& arg = instruct.arguments[i].get();
    printValue(arg.val);
    if(i < instruct.arguments.size() - 1) output << ", ";
  }
  output << ")\n";

  return 0;
}

int ovid::ir::IRPrinter::visitFunctionDeclare(
    ovid::ir::FunctionDeclare &instruct,
    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << " ";
  printValue(instruct.val);

  output << " = FunctionDeclare\n";
  type_printer.visitType(*instruct.type, state.withIndent());
  state.printIndent(output);
  output << "{\n";
  for(auto &body: instruct.body) {
    visitInstruction(*body, state.withIndent());
  }
  state.printIndent(output);
  output << "}\n";

  return 0;
}
