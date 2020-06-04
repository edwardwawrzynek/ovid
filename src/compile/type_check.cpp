#include "type_check.hpp"

namespace ovid::ast {

TypeCheckState TypeCheckState::withoutTypeHint() const {
  return TypeCheckState(current_module, nullptr);
}

TypeCheckState TypeCheckState::withTypeHint(Type *hint) const {
  return TypeCheckState(current_module, hint);
}

TypeCheckState
TypeCheckState::withModuleNames(const std::vector<std::string> &scopes) {
  std::vector<std::string> newScopes = current_module;
  newScopes.insert(newScopes.end(), scopes.begin(), scopes.end());
  return TypeCheckState(newScopes, typeHint);
}

TypeCheckState
TypeCheckState::withoutModuleNames(const std::vector<std::string> &scopes) {
  std::vector<std::string> newScopes = current_module;
  for (size_t i = 0; i < scopes.size(); i++) {
    assert(newScopes[newScopes.size() - 1] == scopes[scopes.size() - 1 - i]);

    newScopes.pop_back();
  }

  return TypeCheckState(newScopes, typeHint);
}

std::shared_ptr<Type> TypeCheck::addMutType(const std::shared_ptr<Type> &type,
                                            bool is_mut) {
  std::shared_ptr<Type> res;
  if (is_mut) {
    res = std::make_shared<MutType>(type->loc, type);
  } else {
    res = type;
  }

  return res;
}

ir::InstructionList TypeCheck::produceIR(const StatementList &ast,
                                         const TypeCheckState &state) {
  auto typeCheckRes = visitNodes(ast, state);

  ovid::ir::InstructionList ir;
  for (auto &res : typeCheckRes) {
    for (auto &instr : res.instructions) {
      ir.push_back(std::move(instr));
    }
  }

  return ir;
}

TypeCheckResult TypeCheck::visitIntLiteral(IntLiteral &node,
                                           const TypeCheckState &state) {
  std::shared_ptr<IntType> resType;
  /* if no type hint is present or type hint isn't IntType, the integer is of
   * type i64 */
  if (state.typeHint == nullptr ||
      dynamic_cast<IntType *>(state.typeHint) == nullptr) {
    resType = std::make_shared<IntType>(node.loc, 64, false);
  }
  /* otherwise, follow type hint */
  else {
    auto hint = dynamic_cast<IntType *>(state.typeHint);

    resType = std::make_shared<IntType>(node.loc, hint->size, hint->isUnsigned);
  }
  ir::InstructionList instructions;

  auto instr = std::make_unique<ir::IntLiteral>(node.loc, ir::Value(), resType,
                                                node.value);
  auto instrPointer = instr.get();
  instructions.push_back(std::move(instr));

  return TypeCheckResult(resType, std::move(instructions), instrPointer);
}

TypeCheckResult TypeCheck::visitVarDecl(VarDecl &node,
                                        const TypeCheckState &state) {

  // TODO: if explicit type included on declaration, use it as hint
  assert(node.resolved_symbol != nullptr);

  // visit initial value
  auto initial = visitNode(*node.initialValue, state.withoutTypeHint());

  // recreate source name
  std::vector<std::string> sourceName = state.current_module;
  sourceName.push_back(node.name);

  // create the allocation instruction and add to initial.instructions
  auto alloc = std::make_unique<ir::Allocation>(node.loc, ir::Value(sourceName),
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

  // if variable is mutable, add MutType wrapper
  auto varType =
      addMutType(initial.resultType, node.resolved_symbol->is_mut);

  // copy inferred type to symbol table
  node.resolved_symbol->type = varType;

  return TypeCheckResult(varType, std::move(initial.instructions),
                         allocPointer);
}

TypeCheckResult TypeCheck::visitModuleDecl(ModuleDecl &node,
                                           const TypeCheckState &state) {
  // nothing special needed for modules - just visit each body node (scopes etc
  // have already been handled by resolve pass)
  auto newState = state.withoutTypeHint().withModuleNames(node.scope);
  auto ir = produceIR(node.body, newState);

  return TypeCheckResult(nullptr, std::move(ir), nullptr);
}

TypeCheckResult TypeCheck::visitIdentifier(Identifier &node,
                                           const TypeCheckState &state) {
  assert(node.resolved_symbol != nullptr);
  // TODO: allow use of global variables before they have been visited (esp
  // those in another compilation unit)
  assert(node.resolved_symbol->ir_decl_instruction != nullptr);
  // use of the identifier doesn't generate any ir -- it just selects the
  // appropriate node
  auto alloc_node = node.resolved_symbol->ir_decl_instruction;

  return TypeCheckResult(node.resolved_symbol->type, ir::InstructionList(), alloc_node);
}

TypeCheckResult TypeCheck::visitAssignment(Assignment &node,
                                           const TypeCheckState &state) {
  // load lvalue
  auto lvalueRes = visitNode(*node.lvalue, state.withoutTypeHint());

  // make sure lvalue is an expression and has an address (is a Allocation)
  if (lvalueRes.resultInstruction == nullptr ||
      dynamic_cast<const ir::Allocation *>(lvalueRes.resultInstruction) ==
          nullptr) {
    errorMan.logError("left side of assignment is non assignable",
                      node.lvalue->loc, ErrorType::TypeError);

    return TypeCheckResult(nullptr, ir::InstructionList(), nullptr);
  }

  // make sure that lvalue is mut
  if (dynamic_cast<MutType *>(lvalueRes.resultType.get()) == nullptr) {
    errorMan.logError("left side of assignment is non mutable",
                      node.lvalue->loc, ErrorType::TypeError);
  }

  // load rvalue, and set type hint to type of lvalue
  auto rvalueRes =
      visitNode(*node.rvalue, state.withTypeHint(lvalueRes.resultType.get()));

  if (lvalueRes.resultInstruction == nullptr) {
    errorMan.logError("expression doesn't have a value", node.rvalue->loc,
                      ErrorType::TypeError);
    return TypeCheckResult(lvalueRes.resultType, ir::InstructionList(),
                           nullptr);
  }

  // TODO: check that types match

  // add lvalue and rvalue instructions together (use lvalue result as buffer)
  for (auto &instruct : rvalueRes.instructions) {
    lvalueRes.instructions.push_back(std::move(instruct));
  }

  // create store instruction
  auto store = std::make_unique<ir::Store>(
      node.loc,
      dynamic_cast<const ir::Allocation &>(*lvalueRes.resultInstruction),
      *rvalueRes.resultInstruction);

  lvalueRes.instructions.push_back(std::move(store));

  // treat the lvalue allocation as the value of this expression
  return TypeCheckResult(lvalueRes.resultType,
                         std::move(lvalueRes.instructions),
                         lvalueRes.resultInstruction);
}

} // namespace ovid::ast