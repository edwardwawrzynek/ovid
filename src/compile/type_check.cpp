#include "type_check.hpp"

namespace ovid::ast {

TypeCheckState TypeCheckState::withoutTypeHint() const {
  return TypeCheckState(curInstructionList, nullptr);
}

TypeCheckState TypeCheckState::withTypeHint(Type *hint) const {
  return TypeCheckState(curInstructionList, hint);
}

TypeCheckState TypeCheckState::withNewInstructionList(
    ir::InstructionList &instructionList) const {
  return TypeCheckState(instructionList, typeHint);
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

ir::InstructionList TypeCheck::produceIR(const StatementList &ast) {
  ovid::ir::InstructionList ir;
  visitNodes(ast, TypeCheckState(ir, nullptr));

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

  auto instr = std::make_unique<ir::IntLiteral>(node.loc, ir::Value(), resType,
                                                node.value);
  auto instrPointer = instr.get();

  state.curInstructionList.push_back(std::move(instr));

  return TypeCheckResult(resType, instrPointer);
}

TypeCheckResult TypeCheck::visitVarDecl(VarDecl &node,
                                        const TypeCheckState &state) {

  // TODO: if explicit type included on declaration, use it as hint
  assert(node.resolved_symbol != nullptr);

  // visit initial value
  auto initial = visitNode(*node.initialValue, state.withoutTypeHint());

  // recreate source name
  std::vector<std::string> sourceName = currentModule;
  sourceName.push_back(node.name);

  // create the allocation instruction and add to initial.instructions
  auto alloc = std::make_unique<ir::Allocation>(node.loc, ir::Value(sourceName),
                                                initial.resultType);
  const auto allocPointer = alloc.get();
  const auto &allocRef = *alloc;
  state.curInstructionList.push_back(std::move(alloc));
  // create the store instruction
  auto store = std::make_unique<ir::Store>(node.loc, allocRef,
                                           *initial.resultInstruction);
  state.curInstructionList.push_back(std::move(store));

  // set symbol table entry for variable to refer to ir alloc
  node.resolved_symbol->ir_decl_instruction = allocPointer;

  // if variable is mutable, add MutType wrapper
  auto varType = addMutType(initial.resultType, node.resolved_symbol->is_mut);

  // copy inferred type to symbol table (TODO: handle explicitly specified type)
  assert(node.resolved_symbol->type == nullptr);
  node.resolved_symbol->type = varType;

  return TypeCheckResult(varType, allocPointer);
}

TypeCheckResult TypeCheck::visitModuleDecl(ModuleDecl &node,
                                           const TypeCheckState &state) {
  // nothing special needed for modules - just visit each body node (scopes etc
  // have already been handled by resolve pass)
  for (auto &scope : node.scope) {
    currentModule.push_back(scope);
  }

  for (auto &child : node.body) {
    visitNode(*child, state.withoutTypeHint());
  }

  for ([[maybe_unused]] auto &scope : node.scope) {
    currentModule.pop_back();
  }

  return TypeCheckResult(nullptr, nullptr);
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

  return TypeCheckResult(node.resolved_symbol->type, alloc_node);
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

    return TypeCheckResult(nullptr, nullptr);
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
    return TypeCheckResult(lvalueRes.resultType, nullptr);
  }

  // TODO: check that types match

  // create store instruction
  auto store = std::make_unique<ir::Store>(
      node.loc,
      dynamic_cast<const ir::Allocation &>(*lvalueRes.resultInstruction),
      *rvalueRes.resultInstruction);

  state.curInstructionList.push_back(std::move(store));

  // treat the lvalue allocation as the value of this expression
  return TypeCheckResult(lvalueRes.resultType, lvalueRes.resultInstruction);
}

TypeCheckResult TypeCheck::visitFunctionDecl(FunctionDecl &node,
                                             const TypeCheckState &state) {
  // new body instruction list
  ir::InstructionList body;

  // recreate source name
  std::vector<std::string> sourceName = currentModule;
  sourceName.push_back(node.name);

  // create allocations for arguments
  std::vector<const ir::Allocation *> argAllocs;

  assert(node.type->argNames.size() == node.type->type->argTypes.size());
  assert(node.type->argNames.size() == node.type->resolvedArgs.size());

  for (size_t i = 0; i < node.type->argNames.size(); i++) {
    auto &name = node.type->argNames[i];
    auto &type = node.type->type->argTypes[i];

    std::vector<std::string> argNameSource;
    argNameSource.push_back(name);

    auto argAlloc = std::make_unique<ir::Allocation>(
        type->loc, ir::Value(argNameSource), type);
    auto argAllocPointer = argAlloc.get();

    body.push_back(std::move(argAlloc));
    argAllocs.push_back(argAllocPointer);

    node.type->resolvedArgs[i]->ir_decl_instruction = argAllocPointer;
  }

  // visit body
  for (auto &child : node.body.statements) {
    visitNode(*child, state.withoutTypeHint().withNewInstructionList(body));
  }

  auto instr = std::make_unique<ir::FunctionDeclare>(
      node.loc, ir::Value(sourceName), node.type, argAllocs, std::move(body));
  auto instrPointer = instr.get();

  state.curInstructionList.push_back(std::move(instr));

  return TypeCheckResult(node.type, instrPointer);
}

} // namespace ovid::ast