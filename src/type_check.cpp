#include "type_check.hpp"
#include "ast_printer.hpp"

#include <utility>

namespace ovid::ast {

TypeCheckState TypeCheckState::withoutTypeHint() const {
  return TypeCheckState(nullptr, functionReturnType);
}

TypeCheckState
TypeCheckState::withTypeHint(const std::shared_ptr<Type> &hint) const {
  return TypeCheckState(hint, functionReturnType);
}

TypeCheckState
TypeCheckState::withFunctionReturnType(std::shared_ptr<Type> returnType) const {
  return TypeCheckState(typeHint, std::move(returnType));
}

TypeCheckState TypeCheckState::withoutFunctionReturnType() const {
  return TypeCheckState(typeHint, nullptr);
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

std::shared_ptr<Type>
TypeCheck::withoutMutType(const std::shared_ptr<Type> &type) {
  auto mutType = dynamic_cast<MutType *>(type.get());
  if (mutType == nullptr)
    return type;
  else
    return mutType->type;
}

ir::BasicBlock &TypeCheck::newBasicBlock(const SourceLocation &loc) {
  // create bb
  auto block = std::make_unique<ir::BasicBlock>(loc);
  auto &ref = *block;

  // insert bb
  curBasicBlockList->push_back(std::move(block));
  curInstructionList = &ref.body;

  return ref;
}

void TypeCheck::insertBasicBlock(std::unique_ptr<ir::BasicBlock> block) {
  auto &ref = *block;

  curBasicBlockList->push_back(std::move(block));
  curInstructionList = &ref.body;
}

TypeCheckResult TypeCheck::visitBoolLiteral(BoolLiteral &node,
                                            const TypeCheckState &state) {
  auto instr =
      std::make_unique<ir::BoolLiteral>(node.loc, ir::Value(), node.value);
  auto instrPointer = instr.get();

  curInstructionList->push_back(std::move(instr));

  return TypeCheckResult(instrPointer->type, instrPointer);
}

TypeCheckResult TypeCheck::visitTuple(Tuple &node,
                                      const TypeCheckState &state) {
  auto typeHint = std::dynamic_pointer_cast<TupleType>(state.typeHint);

  std::vector<std::reference_wrapper<ir::Expression>> exprs;
  std::vector<std::shared_ptr<ast::Type>> exprTypes;
  for (size_t i = 0; i < node.expressions.size(); i++) {
    auto &expr = node.expressions[i];

    // construct type hint
    std::shared_ptr<ast::Type> newTypeHint;
    if (typeHint == nullptr || i >= typeHint->types.size())
      newTypeHint = nullptr;
    else
      newTypeHint = typeHint->types[i];

    // visit expression
    auto exprRes = visitNode(*expr, state.withTypeHint(newTypeHint));
    if (exprRes.isNull())
      return TypeCheckResult::nullResult();

    exprs.emplace_back(*exprRes.resExpr);
    exprTypes.push_back(withoutMutType(exprRes.resType));
  }

  // construct instruction
  auto resType =
      std::make_shared<ast::TupleType>(node.loc, std::move(exprTypes));
  auto instr =
      std::make_unique<ir::TupleLiteral>(node.loc, ir::Value(), exprs, resType);
  auto instrPointer = instr.get();

  curInstructionList->push_back(std::move(instr));

  return TypeCheckResult(resType, instrPointer);
}

TypeCheckResult TypeCheck::visitStructExpr(StructExpr &node,
                                           const TypeCheckState &state) {
  auto structType = std::dynamic_pointer_cast<StructType>(node.type);

  if (structType == nullptr) {
    errorMan.logError(
        string_format(
            "expected constructor type (\x1b[1m%s\x1b[m) to be a struct type",
            type_printer.getType(*node.type).c_str()),
        node.loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }

  // if the not all of the structure's fields are public and we are outside of
  // the struct's module, it can't be constructed
  if (!structType->hasPublicConstructor() &&
      !checkVisible(*structType->getTypeAlias(),
                    scopes.types->getScopeTable(package),
                    scopes.types->getScopeTable(currentModule), false)) {
    errorMan.logError(
        string_format("use of private constructor on type \x1b[1m%s\x1b[m",
                      type_printer.getType(*structType).c_str()),
        node.loc, ErrorType::TypeError);
    return TypeCheckResult::nullResult();
  }

  // because expr's may be in any order, store both index + value
  std::vector<std::pair<int32_t, std::reference_wrapper<ir::Expression>>>
      unorder_exprs;

  assert(node.field_exprs.size() == node.field_names.size());
  for (size_t i = 0; i < node.field_exprs.size(); i++) {
    auto field_index = structType->getNamedFieldIndex(node.field_names[i]);
    if (field_index == -1) {
      errorMan.logError(
          string_format(
              "type \x1b[1m%s\x1b[m does not have field \x1b[1m%s\x1b[m",
              type_printer.getType(*structType).c_str(),
              node.field_names[i].c_str()),
          node.loc, ErrorType::TypeError);
      return TypeCheckResult::nullResult();
    }
    auto field_type = structType->getTypeOfField(field_index);
    auto expr = visitNode(*node.field_exprs[i], state.withTypeHint(field_type));

    if (!expr.resType->equalToExpected(*field_type)) {
      errorMan.logError(
          string_format("type of expression \x1b[1m%s\x1b[m doesn't match "
                        "expected type \x1b[1m%s\x1b[m",
                        type_printer.getType(*expr.resType).c_str(),
                        type_printer.getType(*field_type).c_str()),
          node.field_exprs[i]->loc, ErrorType::TypeError);
      return TypeCheckResult::nullResult();
    }

    unorder_exprs.emplace_back(field_index,
                               std::reference_wrapper(*expr.resExpr));
  }

  bool missing_fields = false;
  if (unorder_exprs.size() != structType->getNumFields()) {
    errorMan.logError(string_format("struct expression doesn't initialize all "
                                    "fields of type \x1b[1m%s\x1b[m",
                                    type_printer.getType(*structType).c_str()),
                      node.loc, ErrorType::TypeError);
    missing_fields = true;
  }

  // construct properly ordered list of expressions
  std::vector<std::reference_wrapper<ir::Expression>> exprs;
  for (size_t i = 0; i < structType->getNumFields(); i++) {
    auto expr = std::find_if(
        unorder_exprs.begin(), unorder_exprs.end(),
        [i](auto &entry) -> bool { return entry.first == (int32_t)i; });

    if (expr == std::end(unorder_exprs)) {
      errorMan.logError(
          string_format("field \x1b[1m%s\x1b[m is not initialized",
                        structType->field_names[i].c_str()),
          node.loc, ErrorType::TypeError);
    } else {
      exprs.push_back(expr->second);
    }
  }

  if (missing_fields)
    return TypeCheckResult::nullResult();

  auto instr = std::make_unique<ir::TupleLiteral>(node.loc, ir::Value(), exprs,
                                                  structType);
  auto instrPointer = instr.get();
  curInstructionList->push_back(std::move(instr));

  return TypeCheckResult(structType, instrPointer);
}

TypeCheckResult TypeCheck::visitIntLiteral(IntLiteral &node,
                                           const TypeCheckState &state) {
  std::shared_ptr<IntType> resType;
  /* if no type hint is present or type hint isn't IntType, default to i32
   * TOOD: switch to i64 if doesn't fit in i32 */
  if (state.typeHint == nullptr ||
      dynamic_cast<IntType *>(state.typeHint.get()) == nullptr) {
    resType = std::make_shared<IntType>(node.loc, 32, false);
  }
  /* otherwise, follow type hint */
  else {
    auto hint = dynamic_cast<IntType *>(state.typeHint.get());
    resType = std::make_shared<IntType>(node.loc, hint->size, hint->isUnsigned);
  }

  auto instr = std::make_unique<ir::IntLiteral>(node.loc, ir::Value(), resType,
                                                node.value);
  auto instrPointer = instr.get();
  curInstructionList->push_back(std::move(instr));

  return TypeCheckResult(resType, instrPointer);
}

TypeCheckResult TypeCheck::visitCharLiteral(CharLiteral &node,
                                            const TypeCheckState &state) {
  auto type = std::make_shared<IntType>(node.loc, 8, true);
  auto res =
      std::make_unique<ir::IntLiteral>(node.loc, ir::Value(), type, node.value);
  auto resPointer = res.get();

  curInstructionList->push_back(std::move(res));

  return TypeCheckResult(type, resPointer);
}

TypeCheckResult TypeCheck::visitFloatLiteral(FloatLiteral &node,
                                             const TypeCheckState &state) {
  /* if no type hint present, default to f64 */
  auto floatHint = std::dynamic_pointer_cast<ast::FloatType>(state.typeHint);
  auto type = floatHint != nullptr
                  ? floatHint
                  : std::make_shared<ast::FloatType>(node.loc, 64);

  auto res = std::make_unique<ir::FloatLiteral>(node.loc, ir::Value(), type,
                                                node.value);
  auto resPointer = res.get();

  curInstructionList->push_back(std::move(res));

  return TypeCheckResult(type, resPointer);
}

TypeCheckResult TypeCheck::visitVarDecl(VarDecl &node,
                                        const TypeCheckState &state) {
  assert(node.resolved_symbol != nullptr);

  // visit initial value. if explicit type included on declaration, use it as
  // hint
  auto initial =
      visitNode(*node.initialValue, state.withTypeHint(node.explicitType));
  if (initial.isNull())
    return TypeCheckResult::nullResult();
  // remove mut (if present) from inferred type
  auto initialType = withoutMutType(initial.resType);

  if (node.explicitType != nullptr &&
      !initialType->equalToExpected(*node.explicitType)) {
    errorMan.logError(
        string_format("type of expression \x1b[1m%s\x1b[m doesn't match "
                      "expected type \x1b[1m%s\x1b[m",
                      type_printer.getType(*initialType).c_str(),
                      type_printer.getType(*node.explicitType).c_str()),
        node.initialValue->loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }

  /* reuslting allocation ir node */
  ir::Expression *allocPointer;

  if (node.resolved_symbol->is_global) {
    // create GlobalAllocation instruction
    auto global = std::make_unique<ir::GlobalAllocation>(
        node.loc, ir::Value(node.resolved_symbol), initialType,
        *initial.resExpr, node.resolved_symbol);
    allocPointer = global.get();

    curInstructionList->push_back(std::move(global));
  } else {
    // create the allocation instruction and add to initial.instruction
    auto alloc = std::make_unique<ir::Allocation>(
        node.loc, ir::Value(node.resolved_symbol), initialType,
        ir::AllocationType::UNRESOLVED_LOCAL);
    allocPointer = alloc.get();
    auto &allocRef = *alloc;
    curInstructionList->push_back(std::move(alloc));
    // create the store instruction
    auto store = std::make_unique<ir::Store>(node.loc, allocRef,
                                             *initial.resExpr);
    curInstructionList->push_back(std::move(store));
  }

  // set symbol table entry for variable to refer to ir alloc
  node.resolved_symbol->ir_decl_instruction = allocPointer;

  // if variable is mutable, add MutType wrapper
  auto varType = addMutType(initialType, node.resolved_symbol->is_mut);

  // copy inferred type to symbol table
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

  return TypeCheckResult::nullResult();
}

TypeCheckResult TypeCheck::visitIdentifier(Identifier &node,
                                           const TypeCheckState &state) {
  assert(node.resolved_symbol != nullptr);
  auto node_type = node.resolved_symbol->type->trivialConstruct();
  // TODO: if type is generic, insert specialize node + construct type
  assert(node_type != nullptr);

  ir::Expression *alloc_node;
  // If a variable is forward referenced (or in another compilation unit),
  // insert a ForwardIdentifier node
  if (node.resolved_symbol->ir_decl_instruction == nullptr) {
    // TODO: insert specialize node if needed
    auto forwardIdent = std::make_unique<ir::ForwardIdentifier>(
        node.loc, ir::Value(node.resolved_symbol), node.resolved_symbol, node_type);
    alloc_node = forwardIdent.get();
    curInstructionList->push_back(std::move(forwardIdent));
  } else {
    assert(node.resolved_symbol->type != nullptr);
    // use of the identifier doesn't generate any ir -- it just selects the
    // appropriate node
    auto ir_decl_expr = dynamic_cast<ir::Expression *>(node.resolved_symbol->ir_decl_instruction);
    if(ir_decl_expr != nullptr) {
      alloc_node = ir_decl_expr;
    } else {
      auto ir_generic_decl_expr = dynamic_cast<ir::GenericExpression *>(node.resolved_symbol->ir_decl_instruction);
      if(ir_generic_decl_expr != nullptr) {
        // TODO: insert specialize node
        assert(false);
      } else {
        assert(false);
      }
    }
  }

  // do implicit conversion to type hint if needed
  return doImplicitConversion(
      TypeCheckResult(node_type, alloc_node), state, node.loc);
}

TypeCheckResult TypeCheck::visitAssignment(Assignment &node,
                                           const TypeCheckState &state) {
  // load lvalue
  auto lvalueRes = visitNode(*node.lvalue, state.withoutTypeHint());
  if (lvalueRes.isNull())
    return TypeCheckResult::nullResult();

  // make sure lvalue is an expression and has an address
  if (lvalueRes.resExpr == nullptr ||
      !lvalueRes.resExpr->isAddressable()) {
    errorMan.logError("left side of assignment is non assignable",
                      node.lvalue->loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }

  // make sure that lvalue is mut
  if (dynamic_cast<MutType *>(lvalueRes.resType.get()) == nullptr) {
    errorMan.logError("left side of assignment is non mutable",
                      node.lvalue->loc, ErrorType::TypeError);
  }

  // load rvalue, and set type hint to type of lvalue
  auto rvalueRes = visitNode(
      *node.rvalue, state.withTypeHint(withoutMutType(lvalueRes.resType)));

  if (rvalueRes.isNull())
    return TypeCheckResult::nullResult();

  auto lvalueExpected = withoutMutType(lvalueRes.resType);

  if (!rvalueRes.resType->equalToExpected(*lvalueExpected)) {
    errorMan.logError(
        string_format("type of expression \x1b[1m%s\x1b[m does not match "
                      "expected type \x1b[1m%s\x1b[m",
                      type_printer.getType(*rvalueRes.resType).c_str(),
                      type_printer.getType(*lvalueExpected).c_str()),
        node.rvalue->loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }

  // create store instruction
  auto store = std::make_unique<ir::Store>(
      node.loc, *lvalueRes.resExpr, *rvalueRes.resExpr);

  curInstructionList->push_back(std::move(store));

  // treat the lvalue allocation as the value of this expression
  return TypeCheckResult(lvalueRes.resType, lvalueRes.resExpr);
}

TypeCheckResult TypeCheck::visitFunctionDecl(FunctionDecl &node,
                                             const TypeCheckState &state) {

  // new body basic block
  ir::BasicBlockList body;
  body.push_back(
      std::make_unique<ir::BasicBlock>(node.loc, ir::InstructionList()));

  // save global instruction list state
  auto pCurBasicBlockList = curBasicBlockList;
  auto pcurInstructionList = curInstructionList;

  curBasicBlockList = &body;
  curInstructionList = &body[0]->body;

  auto formal_bound_type = node.getFormalBoundFunctionType();
  // create allocations for arguments
  std::vector<std::reference_wrapper<ir::Allocation>> argAllocs;

  assert(formal_bound_type->argNames.size() == formal_bound_type->argTypes.size());
  assert(formal_bound_type->argNames.size() == formal_bound_type->resolvedArgs.size());

  for (size_t i = 0; i < formal_bound_type->argNames.size(); i++) {
    auto &type = formal_bound_type->argTypes[i];

    auto argAlloc = std::make_unique<ir::Allocation>(
        type->loc, ir::Value(formal_bound_type->resolvedArgs[i]), type,
        ir::AllocationType::UNRESOLVED_FUNC_ARG);
    auto argAllocPointer = argAlloc.get();

    curInstructionList->push_back(std::move(argAlloc));
    argAllocs.emplace_back(*argAllocPointer);
    // set symbol table entries to refer to ir nodes
    formal_bound_type->resolvedArgs[i]->ir_decl_instruction = argAllocPointer;
  }

  // visit body
  auto bodyState =
      state.withoutTypeHint().withFunctionReturnType(formal_bound_type->retType);
  for (auto &child : node.body.statements) {
    visitNode(*child, bodyState);
  }

  // restore previous instruction state
  curInstructionList = pcurInstructionList;
  curBasicBlockList = pCurBasicBlockList;

  // construct the function declaration instruction
  auto instr = std::make_unique<ir::GenericFunctionDeclare>(
      node.loc, node.type, argAllocs,
      std::move(body), node.resolved_symbol->is_public);
  auto instrPointer = instr.get();
  node.resolved_symbol->ir_decl_instruction = instrPointer;

  curInstructionList->push_back(std::move(instr));
  return TypeCheckResult(node.type, instrPointer);
}

TypeCheckResult TypeCheck::visitIfStatement(IfStatement &node,
                                            const TypeCheckState &state) {
  // create end basic block
  auto end = std::make_unique<ir::BasicBlock>(node.loc);
  auto &endRef = *end;

  auto condTypeHint = std::make_shared<BoolType>(node.loc);
  // go through each condition and body
  assert(node.conditions.size() == node.bodies.size());
  for (size_t i = 0; i < node.conditions.size(); i++) {
    auto &cond = node.conditions[i];
    auto &body = node.bodies[i];

    auto condRes = visitNode(*cond, state.withTypeHint(condTypeHint));
    if (condRes.isNull())
      return TypeCheckResult::nullResult();

    // check that condRes is a bool
    if (!condRes.resType->equalToExpected(*condTypeHint)) {
      errorMan.logError("condition is not a boolean", cond->loc,
                        ErrorType::TypeError);

      return TypeCheckResult::nullResult();
    }

    // basic block for body of conditional
    auto condBody = std::make_unique<ir::BasicBlock>(cond->loc);
    auto &condBodyRef = *condBody;
    // basic block for evaluation of next condition (or end block if no more
    // conditions)
    std::unique_ptr<ir::BasicBlock> nextCondHead =
        (i < node.conditions.size() - 1)
            ? std::make_unique<ir::BasicBlock>(cond->loc)
            : std::move(end);
    auto &nextCondHeadRef = *nextCondHead;

    auto condJump = std::make_unique<ir::ConditionalJump>(
        cond->loc, condBodyRef, nextCondHeadRef, *condRes.resExpr);

    curInstructionList->push_back(std::move(condJump));

    // setup condition body
    insertBasicBlock(std::move(condBody));
    // visit body
    for (auto &child : body.statements) {
      visitNode(*child, state.withoutTypeHint());
    }
    // jump to end block
    curInstructionList->emplace_back(
        std::make_unique<ir::Jump>(cond->loc, endRef));

    // setup next condition header (or end)
    insertBasicBlock(std::move(nextCondHead));
  }

  return TypeCheckResult::nullResult();
}

TypeCheckResult TypeCheck::visitWhileStatement(WhileStatement &node,
                                               const TypeCheckState &state) {
  // create end of loop basic block
  auto end = std::make_unique<ir::BasicBlock>(node.loc);
  auto &endRef = *end;

  // setup loop header
  auto header = std::make_unique<ir::BasicBlock>(node.loc);
  auto &headerRef = *header;
  curInstructionList->emplace_back(
      std::make_unique<ir::Jump>(node.loc, headerRef));
  insertBasicBlock(std::move(header));

  auto condTypeHint = std::make_shared<BoolType>(node.loc);
  // visit condition
  auto cond = visitNode(*node.cond, state.withTypeHint(condTypeHint));
  if (cond.isNull())
    return TypeCheckResult::nullResult();

  if (!cond.resType->equalToExpected(*condTypeHint)) {
    errorMan.logError(
        string_format("type of expression \x1b[1m%s\x1b[m is not equal to "
                      "expected type \x1b[1mbool\x1b[m",
                      type_printer.getType(*cond.resType).c_str()),
        node.cond->loc, ErrorType::TypeError);
    return TypeCheckResult::nullResult();
  }
  // create body basic block
  auto body = std::make_unique<ir::BasicBlock>(node.loc);
  auto &bodyRef = *body;
  // conditional jump to end/or body
  auto condJump = std::make_unique<ir::ConditionalJump>(
      node.loc, bodyRef, endRef, *cond.resExpr);
  curInstructionList->push_back(std::move(condJump));

  // visit body
  insertBasicBlock(std::move(body));

  for (auto &stat : node.body.statements) {
    visitNode(*stat, state);
  }

  // jump back to header
  curInstructionList->emplace_back(
      std::make_unique<ir::Jump>(node.loc, headerRef));

  insertBasicBlock(std::move(end));

  return TypeCheckResult::nullResult();
}

std::shared_ptr<FunctionType>
TypeCheck::functionTypeFromType(const std::shared_ptr<Type> &type) {
  auto funcType = std::dynamic_pointer_cast<FunctionType>(type);
  if (funcType != nullptr)
    return funcType;

  return nullptr;
}

/* describe overloaded variants of builtin operators available */
enum class BuiltinTypeArg {
  INT,   // all integer types. Types are converted to the highest width in the
         // operation (eg i8+i16 = i16)
  FLOAT, // f32 and f64. Types converted to highest width in operation
  BOOL,
  NONE
};

struct BuiltinOperatorVariant {
  std::vector<BuiltinTypeArg> args;
  BuiltinTypeArg retType;
};

// table of valid overloaded operators
static std::multimap<OperatorType, BuiltinOperatorVariant>
    builtinOperatorOverloads = {
        // arithmetic operators
        {OperatorType::ADD,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::ADD,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::FLOAT}},
        {OperatorType::SUB,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::SUB,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::FLOAT}},
        {OperatorType::MUL,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::MUL,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::FLOAT}},
        {OperatorType::DIV,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::DIV,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::FLOAT}},
        {OperatorType::MOD,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::MOD,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::FLOAT}},
        // -, >>, <<
        {OperatorType::NEGATIVE, {{BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::NEGATIVE,
         {{BuiltinTypeArg::FLOAT}, BuiltinTypeArg::FLOAT}},
        {OperatorType::RIGHT_SHIFT,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::LEFT_SHIFT,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        // logical boolean algebra
        {OperatorType::LOG_OR,
         {{BuiltinTypeArg::BOOL, BuiltinTypeArg::BOOL}, BuiltinTypeArg::BOOL}},
        {OperatorType::LOG_AND,
         {{BuiltinTypeArg::BOOL, BuiltinTypeArg::BOOL}, BuiltinTypeArg::BOOL}},
        {OperatorType::LOG_NOT, {{BuiltinTypeArg::BOOL}, BuiltinTypeArg::BOOL}},
        // binary operators
        {OperatorType::BIN_OR,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::BIN_AND,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::BIN_XOR,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::BIN_NOT, {{BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        // comparison operators
        {OperatorType::EQUAL,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::BOOL}},
        {OperatorType::EQUAL,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::BOOL}},
        {OperatorType::NEQUAL,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::BOOL}},
        {OperatorType::NEQUAL,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::BOOL}},
        {OperatorType::GREATER,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::BOOL}},
        {OperatorType::GREATER,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::BOOL}},
        {OperatorType::GREATER_EQUAL,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::BOOL}},
        {OperatorType::GREATER_EQUAL,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::BOOL}},
        {OperatorType::LESS,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::BOOL}},
        {OperatorType::LESS,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::BOOL}},
        {OperatorType::LESS_EQUAL,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::BOOL}},
        {OperatorType::LESS_EQUAL,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::BOOL}},
};

TypeCheckResult TypeCheck::visitFunctionCall(FunctionCall &node,
                                             const TypeCheckState &state) {
  /* special handling for address of and dereference operators */
  if (dynamic_cast<OperatorSymbol *>(node.funcExpr.get()) != nullptr) {
    auto &opNode = dynamic_cast<OperatorSymbol &>(*node.funcExpr);
    if (opNode.op == OperatorType::DEREF) {
      return visitFunctionCallDeref(node, state);
    } else if (opNode.op == OperatorType::ADDR) {
      return visitFunctionCallAddress(node, state);
    } else if (opNode.op == OperatorType::LOG_AND ||
               opNode.op == OperatorType::LOG_OR) {
      return visitShortCircuitingCall(node, state);
    } else if (opNode.op == OperatorType::ADD_ASSIGN ||
               opNode.op == OperatorType::SUB_ASSIGN) {
      return visitCompoundAssignmentCall(node, state);
    } else {
      return visitFunctionCallOperator(node, state);
    }
  } else {
    // visit function expression (TODO: construct type hint from type of
    // arguments)
    auto funcRes = visitNode(*node.funcExpr, state.withoutTypeHint());
    if (funcRes.resType == nullptr)
      return TypeCheckResult::nullResult();
    auto funcType = functionTypeFromType(funcRes.resType);

    if (funcType == nullptr) {
      errorMan.logError(
          string_format(
              "cannot do a function call on non function type \x1b[1m%s\x1b[m",
              type_printer.getType(*funcRes.resType).c_str()),
          node.funcExpr->loc, ErrorType::TypeError);

      return TypeCheckResult::nullResult();
    }

    if (funcType->argTypes.size() != node.args.size()) {
      errorMan.logError(string_format("invalid number of arguments for "
                                      "function call (expected %i, found %i)",
                                      funcType->argTypes.size(),
                                      node.args.size()),
                        node.loc, ErrorType::TypeError);

      return TypeCheckResult::nullResult();
    }

    std::vector<std::reference_wrapper<ir::Expression>> args;
    // visit each arg
    for (size_t i = 0; i < node.args.size(); i++) {
      auto &arg = node.args[i];
      auto &argType = funcType->argTypes[i];

      auto argRes = visitNode(*arg, state.withTypeHint(argType));
      if (argRes.isNull())
        return TypeCheckResult::nullResult();

      if (!argRes.resType->equalToExpected(*argType)) {
        errorMan.logError(
            string_format("type of expression \x1b[1m%s\x1b[m doesn't match "
                          "expected type \x1b[1m%s\x1b[m",
                          type_printer.getType(*argRes.resType).c_str(),
                          type_printer.getType(*argType).c_str()),
            arg->loc, ErrorType::TypeError);

        return TypeCheckResult::nullResult();
      }

      args.emplace_back(*argRes.resExpr);
    }

    // construct expression
    auto instr = std::make_unique<ir::FunctionCall>(node.loc, ir::Value(),
                                                    *funcRes.resExpr,
                                                    args, funcType->retType);
    auto instrPointer = instr.get();

    curInstructionList->push_back(std::move(instr));

    // implicitly cast result if requested by type hint
    return doImplicitConversion(
        TypeCheckResult(funcType->retType, instrPointer), state, node.loc);
  }
}

TypeCheckResult
TypeCheck::visitShortCircuitingCall(const FunctionCall &node,
                                    const TypeCheckState &state) {
  auto &opNode = dynamic_cast<OperatorSymbol &>(*node.funcExpr);
  auto boolType = std::make_shared<BoolType>(SourceLocation::nullLocation());
  assert(opNode.op == OperatorType::LOG_AND ||
         opNode.op == OperatorType::LOG_OR);

  /* visit left side of expression */
  auto left = visitNode(*node.args[0], state.withTypeHint(boolType));
  if (left.isNull())
    return TypeCheckResult::nullResult();

  if (!left.resType->equalToExpected(*boolType)) {
    errorMan.logError(
        string_format(
            "type of left hand side of boolean expression \x1b[1m%s\x1b[m "
            "doesn't match expected type \x1b[1mbool\x1b[m",
            type_printer.getType(*left.resType).c_str()),
        node.args[0]->loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }
  /* create storage to store result of expression
   * llvm should turn this into a optimized phi node */
  auto resStorage = std::make_unique<ir::Allocation>(
      node.loc, ir::Value(), boolType, ir::AllocationType::UNRESOLVED_LOCAL);
  auto resStoragePointer = resStorage.get();
  /* store left into result storage */
  auto store = std::make_unique<ir::Store>(node.loc, *resStorage,
                                           *left.resExpr);

  curInstructionList->push_back(std::move(resStorage));
  curInstructionList->push_back(std::move(store));

  /* basic block for evaluation of right side */
  auto rightBlock = std::make_unique<ir::BasicBlock>(node.args[1]->loc);
  auto rightBlockPointer = rightBlock.get();
  /* basic block after evaluation of right side */
  auto endBlock = std::make_unique<ir::BasicBlock>(node.args[1]->loc);
  auto endBlockPointer = endBlock.get();
  /* for &&, evaluate right if left is only true
   * for ||, evaluate right is left if only false */
  auto &trueLabel =
      opNode.op == OperatorType::LOG_AND ? *rightBlock : *endBlock;
  auto &falseLable =
      opNode.op == OperatorType::LOG_AND ? *endBlock : *rightBlock;
  auto initJump = std::make_unique<ir::ConditionalJump>(
      node.loc, trueLabel, falseLable, *left.resExpr);
  curInstructionList->push_back(std::move(initJump));
  /* evaluate right side of expression */
  curBasicBlockList->push_back(std::move(rightBlock));
  curInstructionList = &rightBlockPointer->body;

  auto right = visitNode(*node.args[1], state.withTypeHint(boolType));
  if (right.isNull())
    return TypeCheckResult::nullResult();

  if (!right.resType->equalToExpected(*boolType)) {
    errorMan.logError(
        string_format(
            "type of right hand side of boolean expression \x1b[1m%s\x1b[m "
            "doesn't match expected type \x1b[1mbool\x1b[m",
            type_printer.getType(*right.resType).c_str()),
        node.args[0]->loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }

  /* store right hand expression into result storage */
  curInstructionList->push_back(std::make_unique<ir::Store>(
      node.loc, *resStoragePointer, *right.resExpr));
  curInstructionList->push_back(
      std::make_unique<ir::Jump>(node.loc, *endBlock));

  /* end block */
  curBasicBlockList->push_back(std::move(endBlock));
  curInstructionList = &endBlockPointer->body;

  return TypeCheckResult(boolType, resStoragePointer);
}

TypeCheckResult
TypeCheck::visitFunctionCallAddress(const FunctionCall &node,
                                    const TypeCheckState &state) {
  assert(node.args.size() == 1);
  // visit expression (don't set type hint -- conversions make address not
  // storage)
  auto valueRes = visitNode(*node.args[0], state.withoutTypeHint());
  if (valueRes.resType == nullptr)
    return TypeCheckResult::nullResult();
  // make sure expression is a storage
  if (!valueRes.resExpr->isAddressable()) {
    errorMan.logError("cannot take address of expression", node.args[0]->loc,
                      ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }
  // construct instruction
  auto resType =
      std::make_shared<PointerType>(node.funcExpr->loc, valueRes.resType);
  auto instr = std::make_unique<ir::Address>(
      node.funcExpr->loc, ir::Value(), *valueRes.resExpr, resType);
  auto instrPointer = instr.get();

  curInstructionList->push_back(std::move(instr));

  return TypeCheckResult(resType, instrPointer);
}

TypeCheckResult TypeCheck::visitFunctionCallDeref(const FunctionCall &node,
                                                  const TypeCheckState &state) {
  assert(node.args.size() == 1);
  // visit expression, setting type hint to a pointer to the expected type
  auto typeHint = std::make_shared<PointerType>(node.loc, state.typeHint);
  auto valueRes = visitNode(*node.args[0], state.withTypeHint(typeHint));

  if (valueRes.isNull())
    return TypeCheckResult::nullResult();
  // make sure expression is a pointer
  auto typeRes =
      dynamic_cast<PointerType *>(withoutMutType(valueRes.resType).get());
  if (typeRes == nullptr) {
    errorMan.logError(
        string_format("cannot dereference non pointer type \x1b[1m%s\x1b[m",
                      type_printer.getType(*valueRes.resType).c_str()),
        node.args[0]->loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }
  // construct expression
  auto instr = std::make_unique<ir::Dereference>(
      node.funcExpr->loc, ir::Value(), *valueRes.resExpr,
      typeRes->type);
  auto instrPointer = instr.get();

  curInstructionList->push_back(std::move(instr));

  // do implicit type conversion if needed
  return doImplicitConversion(TypeCheckResult(typeRes->type, instrPointer),
                              state, node.loc);
}

/* if leftRes is not null, place the result of visiting the left side of the
 * expression into it */
TypeCheckResult
TypeCheck::visitFunctionCallOperator(const FunctionCall &node,
                                     const TypeCheckState &state,
                                     TypeCheckResult *leftRes) {
  auto opNode = dynamic_cast<OperatorSymbol &>(*node.funcExpr);
  // visit args, and convert arg types to BuiltinTypeArgs
  std::vector<BuiltinTypeArg> builtinArgTypes;
  std::vector<TypeCheckResult> args;

  bool is_first_arg = true;
  for (auto &arg : node.args) {
    // f opNode expects a boolean, give a type hint
    auto typeHint =
        opNode.op == OperatorType::LOG_NOT
            ? std::make_shared<BoolType>(SourceLocation::nullLocation())
            : nullptr;

    auto argRes = visitNode(*arg, state.withTypeHint(typeHint));
    if (argRes.isNull())
      return TypeCheckResult::nullResult();

    /* if left side of expression, copy arg to leftRes */
    if (is_first_arg && leftRes != nullptr) {
      *leftRes = argRes;
    }
    is_first_arg = false;

    auto argType = withoutMutType(argRes.resType);

    // convert type to BuiltinTypeArgs
    if (dynamic_cast<ast::IntType *>(argType.get()) != nullptr) {
      builtinArgTypes.push_back(BuiltinTypeArg::INT);
    } else if (dynamic_cast<ast::BoolType *>(argType.get()) != nullptr) {
      builtinArgTypes.push_back(BuiltinTypeArg::BOOL);
    } else if (dynamic_cast<ast::FloatType *>(argType.get()) != nullptr) {
      builtinArgTypes.push_back(BuiltinTypeArg::FLOAT);
    } else {
      builtinArgTypes.push_back(BuiltinTypeArg::NONE);
    }

    args.emplace_back(TypeCheckResult(argType, argRes.resExpr));
  }

  // find appropriate overload variant
  auto overloadIter = builtinOperatorOverloads.equal_range(opNode.op);
  const BuiltinOperatorVariant *matchedVariant = nullptr;
  for (auto it = overloadIter.first; it != overloadIter.second; it++) {
    if (builtinArgTypes == it->second.args) {
      matchedVariant = &it->second;
      break;
    }
  }

  if (matchedVariant == nullptr) {
    if (args.size() == 1) {
      errorMan.logError(
          string_format("no overloaded variant of operator \x1b[1m%s\x1b[m "
                        "with argument type \x1b[1m%s\x1b[m",
                        printOperatorMap[opNode.op].c_str(),
                        type_printer.getType(*args[0].resType).c_str()),
          node.funcExpr->loc, ErrorType::TypeError);
    } else if (args.size() == 2) {
      errorMan.logError(
          string_format(
              "no overloaded variant of operator \x1b[1m%s\x1b[m with argument "
              "types \x1b[1m%s\x1b[m and \x1b[1m%s\x1b[m",
              printOperatorMap[opNode.op].c_str(),
              type_printer.getType(*args[0].resType).c_str(),
              type_printer.getType(*args[1].resType).c_str()),
          node.funcExpr->loc, ErrorType::TypeError);
    } else {
      // no operators with more than two arguments
      assert(false);
    }

    return TypeCheckResult::nullResult();
  }

  // if operation is binary, and operands are ints or floats, cast them to be
  // the same size
  if (matchedVariant->args.size() > 1) {
    if (matchedVariant->args[0] == BuiltinTypeArg::INT) {
      std::shared_ptr<IntType> castType =
          std::make_shared<IntType>(node.funcExpr->loc, 0, false);
      for (auto &arg : args) {
        auto type = std::dynamic_pointer_cast<IntType>(arg.resType);
        assert(type != nullptr);
        // cast to largest type
        castType->size = std::max(castType->size, type->size);
        // if any types are unsigned, result is unsigned
        if (type->isUnsigned)
          castType->isUnsigned = true;
      }
      // do casts
      int i = 0;
      for (auto &arg : args) {
        arg = doImplicitConversion(arg, state.withTypeHint(castType),
                                   node.args[i]->loc);
        if (!arg.resType->equalToExpected(*castType)) {
          errorMan.logError(
              string_format("type of expresion \x1b[1m%s\x1b[m doesn't match "
                            "expected type \x1b[1m%s\x1b[1m",
                            type_printer.getType(*arg.resType).c_str(),
                            type_printer.getType(*castType).c_str()),
              node.args[i]->loc, ErrorType::TypeError);

          return TypeCheckResult::nullResult();
        }
      }
      i++;
    } else if (matchedVariant->args[0] == BuiltinTypeArg::FLOAT) {
      std::shared_ptr<FloatType> castType =
          std::make_shared<FloatType>(node.funcExpr->loc, 0);
      for (auto &arg : args) {
        auto type = std::dynamic_pointer_cast<FloatType>(arg.resType);
        assert(type != nullptr);
        // cast to largest type
        castType->size = std::max(castType->size, type->size);
      }
      // do casts
      int i = 0;
      for (auto &arg : args) {
        arg = doImplicitConversion(arg, state.withTypeHint(castType),
                                   node.args[i]->loc);
        if (!arg.resType->equalToExpected(*castType)) {
          errorMan.logError(
              string_format("type of expresion \x1b[1m%s\x1b[m doesn't match "
                            "expected type \x1b[1m%s\x1b[1m",
                            type_printer.getType(*arg.resType).c_str(),
                            type_printer.getType(*castType).c_str()),
              node.args[i]->loc, ErrorType::TypeError);

          return TypeCheckResult::nullResult();
        }
      }
      i++;
    }
  }

  // construct operator instruction
  auto opRetType = matchedVariant->retType == BuiltinTypeArg::BOOL
                       ? std::make_shared<BoolType>(node.funcExpr->loc)
                       : args[0].resType;

  auto opInstr = std::make_unique<ir::BuiltinOperator>(
      node.funcExpr->loc, ir::Value(), opNode.op,
      std::make_shared<FunctionType>(node.funcExpr->loc,
                                     TypeList(args.size(), args[0].resType),
                                     opRetType));
  auto &opInstrRef = *opInstr;

  curInstructionList->push_back(std::move(opInstr));
  // construct function call instruction
  std::vector<std::reference_wrapper<ir::Expression>> argExprs;
  argExprs.reserve(args.size());
  for (auto &arg : args)
    argExprs.emplace_back(*arg.resExpr);

  auto instr = std::make_unique<ir::FunctionCall>(
      node.loc, ir::Value(), opInstrRef, argExprs, opRetType);
  ir::Expression *instrPointer = instr.get();

  curInstructionList->push_back(std::move(instr));
  // do implicit convert if needed
  return doImplicitConversion(TypeCheckResult(opRetType, instrPointer), state,
                              node.loc);
}

TypeCheckResult
TypeCheck::visitFunctionCallOperator(const FunctionCall &node,
                                     const TypeCheckState &state) {
  return visitFunctionCallOperator(node, state, nullptr);
}

static std::map<OperatorType, OperatorType> compoundOpsMap = {
    {OperatorType::ADD_ASSIGN, OperatorType::ADD},
    {OperatorType::SUB_ASSIGN, OperatorType::SUB}};

TypeCheckResult
TypeCheck::visitCompoundAssignmentCall(const FunctionCall &node,
                                       const TypeCheckState &state) {
  auto &opNode = dynamic_cast<OperatorSymbol &>(*node.funcExpr);
  /* set op on node to non-compound op, and emit code to calculate expression
   * without store */
  auto left = TypeCheckResult::nullResult();
  opNode.op = compoundOpsMap[opNode.op];
  auto res = visitFunctionCallOperator(node, state, &left);
  if (res.isNull())
    return TypeCheckResult::nullResult();

  /* make sure left side of expression has an address and is mutable */
  if (!left.resExpr->isAddressable()) {
    errorMan.logError(
        "left side of compound assignment operator is not assignable",
        node.args[0]->loc, ErrorType::TypeError);
    return TypeCheckResult::nullResult();
  }
  if (dynamic_cast<MutType *>(left.resType.get()) == nullptr) {
    errorMan.logError(
        "left side of compound assignment operator is not mutable",
        node.args[0]->loc, ErrorType::TypeError);
    return TypeCheckResult::nullResult();
  }

  /* generate store from result into left side */
  curInstructionList->push_back(std::make_unique<ir::Store>(
      node.loc, *left.resExpr, *res.resExpr));

  return left;
}

TypeCheckResult TypeCheck::visitReturnStatement(ReturnStatement &node,
                                                const TypeCheckState &state) {
  assert(state.functionReturnType != nullptr);

  if (node.expression == nullptr) {
    if (dynamic_cast<VoidType *>(state.functionReturnType.get()) == nullptr) {
      errorMan.logError(
          string_format(
              "return type \x1b[1mvoid\x1b[m doesn't match expected type "
              "\x1b[1m%s\x1b[m",
              type_printer.getType(*state.functionReturnType).c_str()),
          node.loc, ErrorType::TypeError);

      return TypeCheckResult::nullResult();
    }

    curInstructionList->emplace_back(
        std::make_unique<ir::Return>(node.loc, nullptr));

    return TypeCheckResult(std::make_shared<VoidType>(node.loc), (ir::Expression *)nullptr);
  } else {
    auto exprRes = visitNode(*node.expression,
                             state.withTypeHint(state.functionReturnType));

    if (exprRes.isNull())
      return TypeCheckResult::nullResult();

    // do implicit conversion to expected return type
    exprRes = doImplicitConversion(exprRes,
                                   state.withTypeHint(state.functionReturnType),
                                   node.expression->loc);
    if (!exprRes.resType->equalToExpected(*state.functionReturnType)) {
      errorMan.logError(
          string_format(
              "return type \x1b[1m%s\x1b[m doesn't match expected type "
              "\x1b[1m%s\x1b[m",
              type_printer.getType(*exprRes.resType).c_str(),
              type_printer.getType(*state.functionReturnType).c_str()),
          node.loc, ErrorType::TypeError);

      return TypeCheckResult::nullResult();
    }

    curInstructionList->emplace_back(
        std::make_unique<ir::Return>(node.loc, exprRes.resExpr));

    return TypeCheckResult(exprRes.resType, nullptr);
  }
}

TypeCheckResult TypeCheck::visitOperatorSymbol(OperatorSymbol &node,
                                               const TypeCheckState &state) {
  // OperatorSymbol in a function call is explicitly handled in
  // visitFunctionCall
  errorMan.logError("invalid use of operator symbol", node.loc,
                    ErrorType::TypeError);
  return TypeCheckResult::nullResult();
}

TypeCheckResult TypeCheck::visitFieldAccess(FieldAccess &node,
                                            const TypeCheckState &state) {
  // visit left side of expression
  auto lvalueRes = visitNode(*node.lvalue, state.withoutTypeHint());
  if (lvalueRes.isNull())
    return TypeCheckResult::nullResult();

  auto lvalueInstr = lvalueRes.resExpr;
  // type of expression
  auto recordType =
      dynamic_cast<ProductType *>(lvalueRes.resType->withoutMutability());
  // if the resulting fields should be mutable
  bool isMut = dynamic_cast<MutType *>(lvalueRes.resType.get()) != nullptr;
  // check for pointer to a RecordType, and, if so, perform implicit dereference
  if (recordType == nullptr) {
    auto pointerType =
        dynamic_cast<PointerType *>(lvalueRes.resType->withoutMutability());
    if (pointerType != nullptr) {
      auto recordPointerType = std::dynamic_pointer_cast<ProductType>(
          withoutMutType(pointerType->type));
      // preserve mutability
      isMut = dynamic_cast<MutType *>(pointerType->type.get()) != nullptr;
      // if type is pointer to record, add dereference instruction
      if (recordPointerType != nullptr) {
        auto deref = std::make_unique<ir::Dereference>(
            node.loc, ir::Value(), *lvalueInstr, pointerType->type);
        lvalueInstr = deref.get();
        curInstructionList->push_back(std::move(deref));

        recordType = recordPointerType.get();
      }
    }
  }

  // if expression type isn't a product type, error
  if (recordType == nullptr) {
    errorMan.logError(
        string_format("cannot take a field on type \x1b[1m%s\x1b[m",
                      type_printer.getType(*lvalueRes.resType).c_str()),
        node.lvalue->loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }
  // internal field index
  int32_t field_index;

  std::string field_string =
      node.has_field_num ? std::to_string(node.field_num) : node.field;

  // lookup named or numbered field index
  if (!node.has_field_num) {
    field_index = recordType->getNamedFieldIndex(node.field);
  } else {
    field_index = recordType->getNumberedFieldIndex(node.field_num);
  }

  if (field_index == -1) {
    errorMan.logError(
        string_format(
            "type \x1b[1m%s\x1b[m does not have field \x1b[1m%s\x1b[m",
            type_printer.getType(*recordType).c_str(), field_string.c_str()),
        node.loc, ErrorType::TypeError);

    return TypeCheckResult::nullResult();
  }

  // check visibility of field
  auto fieldPublic = recordType->fieldIsPublic(field_index);
  if (!fieldPublic) {
    auto alias = recordType->getTypeAlias();
    assert(alias != nullptr);

    if (!checkVisible(*alias, scopes.types->getScopeTable(package),
                      scopes.types->getScopeTable(currentModule), false)) {
      errorMan.logError(
          string_format(
              "use of private field \x1b[1m%s\x1b[m on type \x1b[1m%s\x1b[m",
              field_string.c_str(), type_printer.getType(*recordType).c_str()),
          node.loc, ErrorType::TypeError);
    }
  }

  auto resType = recordType->getTypeOfField(field_index);
  // if tuple was mutable, add mutability back to field
  if (isMut) {
    resType = addMutType(resType, true);
  }
  // emit ir instruction
  auto instruct = std::make_unique<ir::FieldSelect>(
      node.loc, ir::Value(), *lvalueInstr, field_index, resType);
  auto instructPointer = instruct.get();

  curInstructionList->push_back(std::move(instruct));

  // do implicit conversion
  return doImplicitConversion(TypeCheckResult(resType, instructPointer), state,
                              node.loc);
}

/* do an implicit conversion of expression to type state.typeHint, if such a
 * conversion is valid
 * otherwise, do no conversion (type error should be handled in calling
 * function) */
TypeCheckResult
TypeCheck::doImplicitConversion(const TypeCheckResult &expression,
                                const TypeCheckState &state,
                                const SourceLocation &loc) {
  assert(expression.resType != nullptr &&
         expression.resExpr != nullptr);

  auto expressionType = withoutMutType(expression.resType);

  // if there is no type hint, no need to convert
  if (state.typeHint == nullptr)
    return expression;

  auto typeHint = withoutMutType(state.typeHint);

  // if typeHint matches expected type, no need to convert
  if (expressionType->equalToExpected(*withoutMutType(typeHint)))
    return expression;

  // can convert int -> int or float -> float
  if ((std::dynamic_pointer_cast<IntType>(typeHint) != nullptr &&
       std::dynamic_pointer_cast<IntType>(expressionType) != nullptr) ||
      (std::dynamic_pointer_cast<FloatType>(typeHint) != nullptr &&
       std::dynamic_pointer_cast<FloatType>(expressionType) != nullptr)) {

    if (std::dynamic_pointer_cast<IntType>(typeHint) != nullptr) {
      auto srcType = std::dynamic_pointer_cast<IntType>(expressionType);
      auto dstType = std::dynamic_pointer_cast<IntType>(typeHint);

      if (srcType->size > dstType->size) {
        errorMan.logError(
            string_format("narrowing conversion from type \x1b[1m%s\x1b[m to "
                          "\x1b[1m%s\x1b[m",
                          type_printer.getType(*srcType).c_str(),
                          type_printer.getType((*dstType)).c_str()),
            loc, ErrorType::NarrowingConversion);
      }
    }

    auto instr = std::make_unique<ir::BuiltinCast>(
        expression.resExpr->loc, ir::Value(),
        *expression.resExpr, typeHint);
    auto instrPointer = instr.get();
    curInstructionList->push_back(std::move(instr));

    return TypeCheckResult(typeHint, instrPointer);
  }

  // TODO: implicit conversions to bool

  return expression;
}

ir::InstructionList typeCheckProduceIR(ErrorManager &errorMan,
                                       const std::vector<std::string> &package,
                                       const ScopesRoot &scopes,
                                       const StatementList &ast) {
  ir::InstructionList ir;
  ir::BasicBlockList BBList;

  auto typeCheck = TypeCheck(errorMan, package, scopes, &ir, &BBList);
  typeCheck.visitNodes(ast, TypeCheckState());

  return ir;
}

/* --- type printer --- */
void TypePrinter::clear() { res = ""; }

std::string TypePrinter::getRes() { return res; }

std::string TypePrinter::getType(Type &type) {
  clear();
  if (dynamic_cast<MutType *>(&type) != nullptr) {
    visitType(*dynamic_cast<MutType &>(type).type, TypePrinterState());
  } else {
    visitType(type, TypePrinterState());
  }
  return getRes();
}

int TypePrinter::visitVoidType(VoidType &type, const TypePrinterState &state) {
  res.append("void");
  return 0;
}

int TypePrinter::visitBoolType(BoolType &type, const TypePrinterState &state) {
  res.append("bool");
  return 0;
}

int TypePrinter::visitIntType(IntType &type, const TypePrinterState &state) {
  res.push_back(type.isUnsigned ? 'u' : 'i');
  res.append(std::to_string(type.size));
  return 0;
}

int TypePrinter::visitFloatType(FloatType &type,
                                const TypePrinterState &state) {
  res.push_back('f');
  res.append(std::to_string(type.size));

  return 0;
}

int TypePrinter::visitMutType(MutType &type, const TypePrinterState &state) {
  res.append("mut ");
  visitType(*type.type, state);
  return 0;
}

int TypePrinter::visitPointerType(PointerType &type,
                                  const TypePrinterState &state) {
  res.append("*");
  visitType(*type.type, state);
  return 0;
}

int TypePrinter::visitFunctionType(FunctionType &type,
                                   const TypePrinterState &state) {
  res.push_back('(');
  for (size_t i = 0; i < type.argTypes.size(); i++) {
    visitType(*type.argTypes[i], state);
    if (i < type.argTypes.size() - 1)
      res.append(", ");
  }
  res.append(") -> ");
  visitType(*type.retType, state);

  return 0;
}

int TypePrinter::visitNamedFunctionType(NamedFunctionType &type,
                                        const TypePrinterState &state) {
  res.push_back('(');
  for (size_t i = 0; i < type.argTypes.size(); i++) {
    res.append(type.argNames[i]);
    res.push_back(' ');
    visitType(*type.argTypes[i], state);
    if (i < type.argTypes.size() - 1)
      res.append(", ");
  }
  res.append(") -> ");
  visitType(*type.retType, state);

  return 0;
}

int TypePrinter::visitTupleType(TupleType &type,
                                const TypePrinterState &state) {
  res.push_back('(');
  for (size_t i = 0; i < type.types.size(); i++) {
    visitType(*type.types[i], state);
    if (i < type.types.size() - 1)
      res.append(", ");
  }
  res.push_back(')');

  return 0;
}

int TypePrinter::visitStructType(StructType &type,
                                 const TypePrinterState &state) {
  assert(type.getTypeAlias() != nullptr);
  auto name = type.getTypeAlias()->getFullyScopedName();

  for (size_t i = 0; i < name.size(); i++) {
    res.append(name[i]);
    if (i < name.size() - 1)
      res.push_back(':');
  }

  // if present, print generic type params
  auto &params = type.actual_generic_params;
  if (!params.empty()) {
    res.push_back('<');
    for (size_t i = 0; i < params.size(); i++) {
      visitType(*params[i], state);
      if (i < params.size() - 1)
        res.append(", ");
    }
    res.push_back('>');
  }

  return 0;
}

int TypePrinter::visitUnresolvedType(UnresolvedType &type,
                                     const TypePrinterState &state) {
  assert(false);
  return 0;
}

bool TypeCheckResult::isNull() const {
  return resExpr == nullptr || resType == nullptr || is_generic;
}

TypeCheckResult TypeCheckResult::nullResult() {
  return TypeCheckResult(nullptr, (ir::Expression *)nullptr);
}

} // namespace ovid::ast