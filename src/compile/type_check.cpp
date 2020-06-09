#include "type_check.hpp"
#include "ast_printer.hpp"

#include <utility>

namespace ovid::ast {

TypeCheckState TypeCheckState::withoutTypeHint() const {
  return TypeCheckState(curInstructionList, nullptr);
}

TypeCheckState TypeCheckState::withTypeHint(std::shared_ptr<Type> hint) const {
  return TypeCheckState(curInstructionList, std::move(hint));
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

std::shared_ptr<Type>
TypeCheck::withoutMutType(const std::shared_ptr<Type> &type) {
  auto mutType = dynamic_cast<MutType *>(type.get());
  if (mutType == nullptr)
    return type;
  else
    return mutType->type;
}

ir::InstructionList TypeCheck::produceIR(const StatementList &ast) {
  ovid::ir::InstructionList ir;
  visitNodes(ast, TypeCheckState(ir, nullptr));

  return ir;
}

TypeCheckResult TypeCheck::visitBoolLiteral(BoolLiteral &node,
                                            const TypeCheckState &state) {
  auto instr =
      std::make_unique<ir::BoolLiteral>(node.loc, ir::Value(), node.value);
  auto instrPointer = instr.get();

  state.curInstructionList.push_back(std::move(instr));

  return TypeCheckResult(instrPointer->type, instrPointer);
}

TypeCheckResult TypeCheck::visitIntLiteral(IntLiteral &node,
                                           const TypeCheckState &state) {
  std::shared_ptr<IntType> resType;
  /* if no type hint is present or type hint isn't IntType, the integer is of
   * type i64 */
  if (state.typeHint == nullptr ||
      dynamic_cast<IntType *>(state.typeHint.get()) == nullptr) {
    resType = std::make_shared<IntType>(node.loc, 64, false);
  }
  /* otherwise, follow type hint */
  else {
    auto hint = dynamic_cast<IntType *>(state.typeHint.get());
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
  // remove mut (if present) from inferred type
  auto initialType = withoutMutType(initial.resultType);

  // recreate source name
  auto sourceName =
      (node.resolved_symbol->is_global ? currentModule
                                       : std::vector<std::string>());
  sourceName.push_back(node.name);

  // create the allocation instruction and add to initial.instruction
  auto alloc = std::make_unique<ir::Allocation>(
      node.loc, ir::Value(sourceName), initialType,
      (node.resolved_symbol->is_global ? ir::AllocationType::UNRESOLVED_GLOBAL
                                       : ir::AllocationType::UNRESOLVED_LOCAL));
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
  auto varType = addMutType(initialType, node.resolved_symbol->is_mut);

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

  assert(node.resolved_symbol->type != nullptr);
  // use of the identifier doesn't generate any ir -- it just selects the
  // appropriate node
  auto alloc_node = node.resolved_symbol->ir_decl_instruction;

  return TypeCheckResult(node.resolved_symbol->type, alloc_node);
}

TypeCheckResult TypeCheck::visitAssignment(Assignment &node,
                                           const TypeCheckState &state) {
  // load lvalue
  auto lvalueRes = visitNode(*node.lvalue, state.withoutTypeHint());

  // make sure lvalue is an expression and has an address (is a Storage)
  if (lvalueRes.resultInstruction == nullptr ||
      dynamic_cast<const ir::Storage *>(lvalueRes.resultInstruction) ==
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
      visitNode(*node.rvalue, state.withTypeHint(lvalueRes.resultType));

  if (lvalueRes.resultInstruction == nullptr) {
    errorMan.logError("expression doesn't have a value", node.rvalue->loc,
                      ErrorType::TypeError);
    return TypeCheckResult(lvalueRes.resultType, nullptr);
  }

  auto lvalueExpected = withoutMutType(lvalueRes.resultType);

  if (!rvalueRes.resultType->equalToExpected(*lvalueExpected)) {
    errorMan.logError(
        string_format("type of expression (\x1b[1m%s\x1b[m) does not match "
                      "expected type \x1b[1m%s\x1b[m",
                      type_printer.getType(*rvalueRes.resultType).c_str(),
                      type_printer.getType(*lvalueExpected).c_str()),
        node.rvalue->loc, ErrorType::TypeError);

    return TypeCheckResult(nullptr, nullptr);
  }

  // create store instruction
  auto store = std::make_unique<ir::Store>(
      node.loc, dynamic_cast<const ir::Storage &>(*lvalueRes.resultInstruction),
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
  std::vector<std::reference_wrapper<const ir::Allocation>> argAllocs;

  assert(node.type->argNames.size() == node.type->type->argTypes.size());
  assert(node.type->argNames.size() == node.type->resolvedArgs.size());

  for (size_t i = 0; i < node.type->argNames.size(); i++) {
    auto &name = node.type->argNames[i];
    auto &type = node.type->type->argTypes[i];

    std::vector<std::string> argNameSource;
    argNameSource.push_back(name);

    auto argAlloc = std::make_unique<ir::Allocation>(
        type->loc, ir::Value(argNameSource), type,
        ir::AllocationType::UNRESOLVED_FUNC_ARG);
    auto argAllocPointer = argAlloc.get();

    body.push_back(std::move(argAlloc));
    argAllocs.emplace_back(*argAllocPointer);
    // set symbol table entries to refer to ir nodes
    node.type->resolvedArgs[i]->ir_decl_instruction = argAllocPointer;
  }

  // visit body
  for (auto &child : node.body.statements) {
    visitNode(*child, state.withoutTypeHint().withNewInstructionList(body));
  }
  // construct the function declaration instruction
  auto instr = std::make_unique<ir::FunctionDeclare>(
      node.loc, ir::Value(sourceName), node.type, argAllocs, std::move(body));
  auto instrPointer = instr.get();

  state.curInstructionList.push_back(std::move(instr));

  node.resolved_symbol->ir_decl_instruction = instrPointer;

  return TypeCheckResult(node.type, instrPointer);
}

TypeCheckResult TypeCheck::visitIfStatement(IfStatement &node,
                                            const TypeCheckState &state) {
  // create end label
  auto end = std::make_unique<ir::Label>(node.loc);
  auto &endRef = *end;

  auto condTypeHint = std::make_shared<BoolType>(node.loc);
  // go through each condition and body
  assert(node.conditions.size() == node.bodies.size());
  for (size_t i = 0; i < node.conditions.size(); i++) {
    auto &cond = node.conditions[i];
    auto &body = node.bodies[i];

    auto condRes = visitNode(*cond, state.withTypeHint(condTypeHint));
    if (condRes.resultInstruction == nullptr) {
      errorMan.logError("condition does not have a value", cond->loc,
                        ErrorType::TypeError);
      return TypeCheckResult(nullptr, nullptr);
    }

    // check that condRes is a bool
    if (!condRes.resultType->equalToExpected(*condTypeHint)) {
      errorMan.logError("condition is not a boolean", cond->loc,
                        ErrorType::TypeError);

      return TypeCheckResult(nullptr, nullptr);
    }

    auto startCondLabel = std::make_unique<ir::Label>(cond->loc);
    auto &startCondLabelRef = *startCondLabel;
    auto nextCondLabel = std::make_unique<ir::Label>(cond->loc);
    auto &nextCondLabelRef = *nextCondLabel;

    auto condJump = std::make_unique<ir::ConditionalJump>(
        cond->loc, startCondLabelRef, nextCondLabelRef,
        *condRes.resultInstruction);

    state.curInstructionList.push_back(std::move(condJump));
    state.curInstructionList.push_back(std::move(startCondLabel));
    // visit body
    for (auto &child : body.statements) {
      visitNode(*child, state.withoutTypeHint());
    }
    // jump to end of node
    state.curInstructionList.emplace_back(
        std::make_unique<ir::Jump>(cond->loc, endRef));
    state.curInstructionList.push_back(std::move(nextCondLabel));
  }

  state.curInstructionList.push_back(std::move(end));

  return TypeCheckResult(nullptr, nullptr);
}

std::shared_ptr<FunctionType>
TypeCheck::functionTypeFromType(const std::shared_ptr<Type> &type) {
  auto funcType = std::dynamic_pointer_cast<FunctionType>(type);
  if (funcType != nullptr)
    return funcType;
  auto namedFuncType = std::dynamic_pointer_cast<NamedFunctionType>(type);
  if (namedFuncType != nullptr)
    return namedFuncType->type;

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
        // prefix and postfix
        {OperatorType::PREFIX_INC,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::PREFIX_INC,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::FLOAT}},
        {OperatorType::PREFIX_DEC,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::PREFIX_DEC,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::FLOAT}},
        {OperatorType::POSTFIX_INC,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::POSTFIX_INC,
         {{BuiltinTypeArg::FLOAT, BuiltinTypeArg::FLOAT},
          BuiltinTypeArg::FLOAT}},
        {OperatorType::POSTFIX_DEC,
         {{BuiltinTypeArg::INT, BuiltinTypeArg::INT}, BuiltinTypeArg::INT}},
        {OperatorType::POSTFIX_DEC,
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
    auto opNode = dynamic_cast<OperatorSymbol &>(*node.funcExpr);
    if (opNode.op == OperatorType::DEREF) {
      return visitFunctionCallDeref(node, state);
    } else if (opNode.op == OperatorType::ADDR) {
      return visitFunctionCallAddress(node, state);
    } else {
      return visitFunctionCallOperator(node, state);
    }
  } else {
    // visit function expression (TODO: construct type hint from type of
    // arguments)
    auto funcRes = visitNode(*node.funcExpr, state.withoutTypeHint());
    if (funcRes.resultType == nullptr)
      return TypeCheckResult(nullptr, nullptr);
    auto funcType = functionTypeFromType(funcRes.resultType);

    if (funcType == nullptr) {
      errorMan.logError(
          string_format(
              "cannot do a function call on non function type \x1b[1m%s\x1b[m",
              type_printer.getType(*funcRes.resultType).c_str()),
          node.funcExpr->loc, ErrorType::TypeError);

      return TypeCheckResult(nullptr, nullptr);
    }

    if (funcType->argTypes.size() != node.args.size()) {
      errorMan.logError(string_format("invalid number of arguments for "
                                      "function call (expected %i, found %i)",
                                      funcType->argTypes.size(),
                                      node.args.size()),
                        node.loc, ErrorType::TypeError);

      return TypeCheckResult(nullptr, nullptr);
    }

    std::vector<std::reference_wrapper<const ir::Expression>> args;
    // visit each arg
    for (size_t i = 0; i < node.args.size(); i++) {
      auto &arg = node.args[i];
      auto &argType = funcType->argTypes[i];

      auto argRes = visitNode(*arg, state.withTypeHint(argType));
      if (argRes.resultType == nullptr || argRes.resultInstruction == nullptr)
        return TypeCheckResult(nullptr, nullptr);

      if (!argRes.resultType->equalToExpected(*argType)) {
        errorMan.logError(
            string_format("type of expression (\x1b[1m%s\x1b[m) doesn't match "
                          "expected type \x1b[1m%s\x1b[m",
                          type_printer.getType(*argRes.resultType).c_str(),
                          type_printer.getType(*argType).c_str()),
            arg->loc, ErrorType::TypeError);

        return TypeCheckResult(nullptr, nullptr);
      }

      args.emplace_back(*argRes.resultInstruction);
    }

    // construct expression
    auto instr = std::make_unique<ir::FunctionCall>(node.loc, ir::Value(),
                                                    *funcRes.resultInstruction,
                                                    args, funcType->retType);
    auto instrPointer = instr.get();

    state.curInstructionList.push_back(std::move(instr));

    // TODO: implicitly cast result if requested by type hint

    return TypeCheckResult(funcType->retType, instrPointer);
  }
}

TypeCheckResult
TypeCheck::visitFunctionCallAddress(const FunctionCall &node,
                                    const TypeCheckState &state) {
  assert(node.args.size() == 1);
  // if type hint is a pointer, construct a type hint with that type
  std::shared_ptr<Type> typeHint;
  if (dynamic_cast<PointerType *>(state.typeHint.get()) != nullptr) {
    typeHint = dynamic_cast<PointerType *>(state.typeHint.get())->type;
  } else {
    typeHint = nullptr;
  }
  // visit expression
  auto valueRes = visitNode(*node.args[0], state.withTypeHint(typeHint));
  if (valueRes.resultType == nullptr)
    return TypeCheckResult(nullptr, nullptr);
  // make sure expression is a storage
  auto storageRes =
      dynamic_cast<const ir::Storage *>(valueRes.resultInstruction);
  if (storageRes == nullptr) {
    errorMan.logError("cannot take address of expression", node.args[0]->loc,
                      ErrorType::TypeError);

    return TypeCheckResult(nullptr, nullptr);
  }
  // construct instruction
  auto resType =
      std::make_shared<PointerType>(node.funcExpr->loc, valueRes.resultType);
  auto instr = std::make_unique<ir::Address>(node.funcExpr->loc, ir::Value(),
                                             *storageRes, resType);
  auto instrPointer = instr.get();

  state.curInstructionList.push_back(std::move(instr));

  return TypeCheckResult(resType, instrPointer);
}

TypeCheckResult TypeCheck::visitFunctionCallDeref(const FunctionCall &node,
                                                  const TypeCheckState &state) {
  assert(node.args.size() == 1);
  // visit expression, setting type hint to a pointer to the expected type
  auto typeHint = std::make_shared<PointerType>(node.loc, state.typeHint);
  auto valueRes = visitNode(*node.args[0], state.withTypeHint(typeHint));

  if (valueRes.resultType == nullptr)
    return TypeCheckResult(nullptr, nullptr);
  // make sure expression is a pointer
  auto typeRes =
      dynamic_cast<PointerType *>(withoutMutType(valueRes.resultType).get());
  if (typeRes == nullptr) {
    errorMan.logError(
        string_format("cannot dereference non pointer type \x1b[1m%s\x1b[m",
                      type_printer.getType(*valueRes.resultType).c_str()),
        node.args[0]->loc, ErrorType::TypeError);

    return TypeCheckResult(nullptr, nullptr);
  }
  // construct expression
  auto instr = std::make_unique<ir::Dereference>(
      node.funcExpr->loc, ir::Value(), *valueRes.resultInstruction,
      typeRes->type);
  auto instrPointer = instr.get();

  state.curInstructionList.push_back(std::move(instr));
  // TOOD: do implicit type conversion if needed

  return TypeCheckResult(typeRes->type, instrPointer);
}

TypeCheckResult
TypeCheck::visitFunctionCallOperator(const FunctionCall &node,
                                     const TypeCheckState &state) {
  auto opNode = dynamic_cast<OperatorSymbol &>(*node.funcExpr);
  // visit args, and convert arg types to BuiltinTypeArgs
  std::vector<BuiltinTypeArg> builtinArgTypes;
  std::vector<std::shared_ptr<Type>> argTypes;
  std::vector<std::reference_wrapper<const ir::Expression>> argExprs;
  for (auto &arg : node.args) {
    // TODO: if opNode expects a boolean, give a type hint
    auto argRes = visitNode(*arg, state.withoutTypeHint());
    if (argRes.resultType == nullptr || argRes.resultInstruction == nullptr)
      return TypeCheckResult(nullptr, nullptr);

    auto argType = withoutMutType(argRes.resultType);

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

    argExprs.emplace_back(*argRes.resultInstruction);
    argTypes.push_back(argType);
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
    if (argTypes.size() == 1) {
      errorMan.logError(
          string_format(
              "no overloaded variant of operator %s with argument type %s",
              printOperatorMap[opNode.op].c_str(),
              type_printer.getType(*argTypes[0]).c_str()),
          node.funcExpr->loc, ErrorType::TypeError);
    } else if (argTypes.size() == 2) {
      errorMan.logError(
          string_format("no overloaded variant of operator %s with argument "
                        "types %s and %s",
                        printOperatorMap[opNode.op].c_str(),
                        type_printer.getType(*argTypes[0]).c_str(),
                        type_printer.getType(*argTypes[1]).c_str()),
          node.funcExpr->loc, ErrorType::TypeError);
    } else {
      // no operators with more than two arguments
      assert(false);
    }

    return TypeCheckResult(nullptr, nullptr);
  }

  // TODO
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

int TypePrinter::visitResolvedAlias(ResolvedAlias &type,
                                    const TypePrinterState &state) {
  assert(type.alias->type != nullptr);
  visitType(*type.alias->type, state);

  return 0;
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
  for (size_t i = 0; i < type.type->argTypes.size(); i++) {
    res.append(type.argNames[i]);
    res.push_back(' ');
    visitType(*type.type->argTypes[i], state);
    if (i < type.type->argTypes.size() - 1)
      res.append(", ");
  }
  res.append(") -> ");
  visitType(*type.type->retType, state);

  return 0;
}

} // namespace ovid::ast