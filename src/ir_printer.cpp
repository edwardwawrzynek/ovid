#include "ir_printer.hpp"
#include "ast.hpp"

namespace ovid::ir {

void IRPrinter::printId(const Id &id) {
  output << "%" << id.id;
  if (id.hasSourceName) {
    output << "{";
    auto name = id.sourceName->getFullyScopedName();
    output << scopedNameToString(name);
    if (!id.typeParams.empty()) {
      output << type_printer.getGenericTypeList(
          const_cast<ast::TypeList &>(id.typeParams));
    }
    output << "}";
  }
}

void ovid::ir::IRPrinter::printValue(const ovid::ir::Value &val) {
  printId(val.id);
}

int IRPrinter::visitIntLiteral(IntLiteral &instruct,
                               const ast::ASTPrinterState &state) {
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

int IRPrinter::visitFloatLiteral(FloatLiteral &instruct,
                                 const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = FLOATLITERAL " << type_printer.getType(*instruct.type) << " "
         << instruct.value << "\n";

  return 0;
}

int IRPrinter::visitTupleLiteral(TupleLiteral &instruct,
                                 const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);
  output << " = TUPLELITERAL " << type_printer.getType(*instruct.type);
  for (auto &expr : instruct.exprs) {
    output << " ";
    printValue(expr.get().val);
  }
  output << "\n";

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

int ovid::ir::IRPrinter::visitBasicBlock(
    ovid::ir::BasicBlock &instruct, const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\tBASICBLOCK @" << instruct.id << " {\n";
  for (auto &child : instruct.body) {
    visitInstruction(*child, state.withIndent());
  }
  state.printIndent(output);
  output << "}\n";

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
  output << " (";
  for (size_t i = 0; i < instruct.arguments.size(); i++) {
    auto &arg = instruct.arguments[i].get();
    printValue(arg.val);
    if (i < instruct.arguments.size() - 1)
      output << ", ";
  }
  output << ")\n";

  return 0;
}

int IRPrinter::visitGenericFunctionDeclare(GenericFunctionDeclare &instruct,
                                           const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printId(instruct.id);

  output << " = GENERICFUNCTIONDECLARE "
         << type_printer.getFormalTypeParameterList(
                instruct.type_construct->getFormalTypeParameters())
         << " "
         << type_printer.getType(
                *instruct.type_construct->getFormalBoundType());
  if (instruct.impl != nullptr) {
    output << " [";
    printId(getInstrId(instruct.impl));
    output << "]";
  }
  output << " {\n";
  for (auto &body : instruct.body) {
    visitInstruction(*body, state.withIndent());
  }
  state.printIndent(output);
  output << "}\n";

  return 0;
}

int ovid::ir::IRPrinter::visitFunctionDeclare(
    ovid::ir::FunctionDeclare &instruct,
    const ovid::ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = FUNCTIONDECLARE " << type_printer.getType(*instruct.type);
  if (instruct.impl != nullptr) {
    output << " [";
    printId(getInstrId(instruct.impl));
    output << "]";
  }
  output << " {\n";
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

int IRPrinter::visitBuiltinOperator(BuiltinOperator &instruct,
                                    const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);
  output << " = BUILTINOPERATOR " << type_printer.getType(*instruct.type) << " "
         << ast::printOperatorMap[instruct.opType];
  output << "\n";

  return 0;
}

int IRPrinter::visitBuiltinCast(BuiltinCast &instruct,
                                const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);
  output << " = BUILTINCAST " << type_printer.getType(*instruct.type) << " ";
  printValue(instruct.expr.val);
  output << "\n";

  return 0;
}

int IRPrinter::visitReturn(Return &instruct,
                           const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  output << "RETURN";
  if (instruct.expr != nullptr) {
    output << " ";
    printValue(instruct.expr->val);
  }
  output << "\n";

  return 0;
}

int IRPrinter::visitFieldSelect(FieldSelect &instruct,
                                const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);
  output << " = FIELDSELECT " << type_printer.getType(*instruct.type) << " "
         << instruct.field_index << " ";
  printValue(instruct.expr.val);
  output << "\n";

  return 0;
}

int IRPrinter::visitForwardIdentifier(ForwardIdentifier &instruct,
                                      const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);
  output << " = FORWARDIDENTIFIER " << type_printer.getType(*instruct.type)
         << "\n";

  return 0;
}

int IRPrinter::visitGenericForwardIdentifier(
    GenericForwardIdentifier &instruct, const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printId(instruct.id);
  output << " = GENERICFORWARDIDENTIFIER "
         << type_printer.getFormalTypeParameterList(
                instruct.type_construct->getFormalTypeParameters())
         << " "
         << type_printer.getType(*instruct.type_construct->getFormalBoundType())
         << "\n";

  return 0;
}

int IRPrinter::visitGlobalAllocation(GlobalAllocation &instruct,
                                     const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = GLOBALALLOCATION " << type_printer.getType(*instruct.type)
         << " ";
  printValue(instruct.initial_val.val);
  output << "\n";

  return 0;
}

int IRPrinter::visitSpecialize(Specialize &instruct,
                               const ast::ASTPrinterState &state) {
  state.printIndent(output);

  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = SPECIALIZE " << type_printer.getType(*instruct.type) << " ";
  printId(instruct.expr.id);
  output << " " << type_printer.getGenericTypeList(instruct.actual_type_params)
         << "\n";

  return 0;
}

int IRPrinter::visitSizeof(Sizeof &instruct,
                           const ast::ASTPrinterState &state) {
  state.printIndent(output);
  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = SIZEOF " << type_printer.getType(*instruct.sizeof_type) << "\n";
  return 0;
}

int IRPrinter::visitImpl(Impl &instruct, const ast::ASTPrinterState &state) {
  state.printIndent(output);
  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = IMPL " << type_printer.getType(*instruct.type) << " {\n";
  for (auto &fn : instruct.fn_decls) {
    visitInstruction(*fn, state.withIndent());
  }
  state.printIndent(output);
  output << "}\n";

  return 0;
}

int IRPrinter::visitGenericImpl(GenericImpl &instruct,
                                const ast::ASTPrinterState &state) {
  state.printIndent(output);
  ast::printLoc(output, instruct.loc);
  output << "\t";
  printId(instruct.id);

  output << " = GENERICIMPL "
         << type_printer.getFormalTypeParameterList(
                instruct.type_construct->getFormalTypeParameters())
         << " "
         << type_printer.getType(*instruct.type_construct->getFormalBoundType())
         << " {\n";
  for (auto &fn : instruct.fn_decls) {
    visitInstruction(*fn, state.withIndent());
  }
  state.printIndent(output);
  output << "}\n";

  return 0;
}

int IRPrinter::visitImplFnExtract(Select &instruct,
                                  const ast::ASTPrinterState &state) {
  state.printIndent(output);
  ast::printLoc(output, instruct.loc);
  output << "\t";
  printValue(instruct.val);

  output << " = SELECT " << type_printer.getType(*instruct.type) << " ";
  printId(instruct.impl.val.id);
  output << " " << instruct.extract_id << "\n";
  return 0;
}

int IRPrinter::visitImplGenericFnExtract(GenericSelect &instruct,
                                         const ast::ASTPrinterState &state) {
  state.printIndent(output);
  ast::printLoc(output, instruct.loc);
  output << "\t";
  printId(instruct.id);

  output << " = GENERICSELECT "
         << type_printer.getFormalTypeParameterList(
                instruct.type_construct->getFormalTypeParameters())
         << " "
         << type_printer.getType(*instruct.type_construct->getFormalBoundType())
         << " ";
  printId(instruct.impl.val.id);
  output << " " << instruct.extract_id << "\n";
  return 0;
}

} // namespace ovid::ir
