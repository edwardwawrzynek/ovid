#include "type_check.hpp"

namespace ovid::ast {

TypeCheckState TypeCheckState::withoutTypeHint() const {
  auto res = TypeCheckState();
  res.typeHint = nullptr;

  return res;
}

TypeCheckState TypeCheckState::withTypeHint(Type *hint) const {
  auto res = TypeCheckState();
  res.typeHint = hint;

  return res;
}

ir::InstructionList TypeCheck::produceIR(const StatementList& ast) {
  auto typeCheckRes = visitNodes(ast, ovid::ast::TypeCheckState());

  ovid::ir::InstructionList ir;
  for(auto& res: typeCheckRes) {
    for(auto& instr: res.instructions) {
      ir.push_back(std::move(instr));
    }
  }

  return ir;
}

TypeCheckResult TypeCheck::visitIntLiteral(IntLiteral &node,
                                           const TypeCheckState &state) {
  std::unique_ptr<IntType> resType;
  /* if no type hint is present or type hint isn't IntType, the integer is of
   * type i64 */
  if (state.typeHint == nullptr ||
      dynamic_cast<IntType *>(state.typeHint) == nullptr) {
    resType = std::make_unique<IntType>(node.loc, 64, false);
  }
  /* otherwise, follow type hint */
  else {
    auto hint = dynamic_cast<IntType *>(state.typeHint);

    resType = std::make_unique<IntType>(node.loc, hint->size, hint->isUnsigned);
  }
  ir::InstructionList instructions;

  auto &resTypeRef = *resType;

  auto instr = std::make_unique<ir::IntLiteral>(node.loc, ir::Value(),
                                                std::move(resType), node.value);
  auto instrPointer = instr.get();
  instructions.push_back(std::move(instr));

  return TypeCheckResult(resTypeRef, std::move(instructions), instrPointer);
}

TypeCheckResult TypeCheck::visitVarDecl(VarDecl &node,
                                        const TypeCheckState &state) {

  // TODO: if explicit type included on declaration, use it as hint

  assert(node.resolved_symbol != nullptr);

  // visit initial value
  auto initial = visitNode(*node.initialValue, state.withoutTypeHint());

  // create the allocation instruction and add to initial.instructions
  // TODO: preserve source name in Value
  auto alloc = std::make_unique<ir::Allocation>(node.loc, ir::Value(),
                                                initial.resultType);
  const auto allocPointer = alloc.get();
  const auto &allocRef = *alloc;
  initial.instructions.push_back(std::move(alloc));
  // create the store instruction
  auto store = std::make_unique<ir::Store>(node.loc, allocRef,
                                           *initial.resultInstruction);
  initial.instructions.push_back(std::move(store));

  // set symbol table entry for variable to refer to ir alloc
  node.resolved_symbol->ir_decl_instruction = allocPointer;

  return TypeCheckResult(initial.resultType, std::move(initial.instructions),
                         allocPointer);
}

TypeCheckResult TypeCheck::visitAssignment(Assignment &node,
                                           const TypeCheckState &state) {
  // load the address of the lvalue
}

TypeCheckResult TypeCheck::visitModuleDecl(ModuleDecl &node,
                                           const TypeCheckState &state) {
  // nothing special needed for modules - just visit each body node
  for(auto &child: node.body) {
    visitNode(*child, state.withoutTypeHint());
  }

}

} // namespace ovid::ast