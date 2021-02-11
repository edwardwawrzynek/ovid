#include "ir.hpp"

#include "ast.hpp"
#include <utility>

// ir is mostly header -- see ir.hpp

namespace ovid::ir {
static uint64_t ir_id = 0;

uint64_t next_id() { return ir_id++; }

void reset_id() { ir_id = 0; }

bool EscapeTypeIsEscape(EscapeType type) {
  return type == EscapeType::OTHER || type == EscapeType::RETURN;
}

bool AllocationTypeIsArg(AllocationType type) {
  return type == AllocationType::UNRESOLVED_FUNC_ARG ||
         type == AllocationType::ARG ||
         type == AllocationType::ARG_COPY_TO_STACK ||
         type == AllocationType::ARG_HEAP;
}

bool AllocationTypeIsHeap(AllocationType type) {
  return type == AllocationType::HEAP || type == AllocationType::ARG_HEAP;
}

AllocationType AllocationTypeToHeap(AllocationType type) {
  switch (type) {
  case AllocationType::STACK:
  case AllocationType::HEAP:
    return AllocationType::HEAP;
  case AllocationType::UNRESOLVED_FUNC_ARG:
  case AllocationType::ARG:
  case AllocationType::ARG_COPY_TO_STACK:
  case AllocationType::ARG_HEAP:
    return AllocationType::ARG_HEAP;
  default:
    assert(false);
  }
}

bool Expression::isAddressable() const { return false; }

bool Allocation::isAddressable() const {
  // function args don't have addresses
  if (allocType == AllocationType::ARG)
    return false;
  else
    return true;
}

bool Dereference::isAddressable() const {
  // dereference of an address, so result is addressable
  return true;
}

bool FieldSelect::isAddressable() const {
  // if containing expression has address, so does this
  return expr.isAddressable();
}

/* ir instruction constructors */
Id::Id(std::shared_ptr<Symbol> sourceName)
    : sourceName(std::move(sourceName)), id(next_id()), hasSourceName(true) {}

Id::Id() : sourceName(nullptr), id(next_id()), hasSourceName(false) {}

Id::Id(std::shared_ptr<Symbol> sourceName, ast::TypeList typeParams)
    : sourceName(std::move(sourceName)), typeParams(std::move(typeParams)),
      id(next_id()), hasSourceName(true) {}

Id::Id(const Id &old_id, ast::TypeList new_type_params)
    : sourceName(old_id.sourceName), typeParams(std::move(new_type_params)),
      id(next_id()), hasSourceName(old_id.hasSourceName) {}

Id Id::withNewId() const {
  if (hasSourceName) {
    return Id(sourceName, typeParams);
  } else {
    return Id();
  }
}

Value::Value(std::shared_ptr<Symbol> sourceName)
    : id(std::move(sourceName)), llvm_value(nullptr) {}

Value::Value() : id(), llvm_value(nullptr) {}

Value::Value(std::shared_ptr<Symbol> sourceName, ast::TypeList typeParams)
    : id(std::move(sourceName), std::move(typeParams)), llvm_value(nullptr) {}

Value::Value(const Id &old_id, ast::TypeList new_type_params)
    : id(old_id, std::move(new_type_params)), llvm_value(nullptr) {}

Value Value::withNewId() const {
  if (id.hasSourceName) {
    return Value(id.sourceName, id.typeParams);
  } else {
    return Value();
  }
}

Instruction::Instruction(const SourceLocation &loc) : loc(loc) {}

Expression::Expression(const SourceLocation &loc, const Value &val,
                       std::shared_ptr<ast::Type> type)
    : Instruction(loc), val(val), type(std::move(type)) {}

bool Expression::hasFlowMetadata() { return false; }

void Expression::addFlowMetadata(
    FlowList &flows,
    const std::vector<std::reference_wrapper<Expression>> &args,
    Expression &returnExpr) {
  assert(false);
}

GenericExpression::GenericExpression(
    const SourceLocation &loc, const Id &id,
    std::shared_ptr<ast::TypeConstructor> type_construct)
    : Instruction(loc), id(id), type_construct(std::move(type_construct)) {}

GenericImpl::GenericImpl(const SourceLocation &loc, const Id &id,
                         InstructionList fn_decls,
                         std::shared_ptr<ast::ImplHeader> header)
    : GenericExpression(
          loc, id,
          std::make_shared<ast::GenericTypeConstructor>(
              header->type->loc, header->type_params, header->type)),
      fn_decls(std::move(fn_decls)), header(std::move(header)) {}

Impl::Impl(const SourceLocation &loc, const Value &val,
           InstructionList fn_decls, std::shared_ptr<ast::ImplHeader> header)
    : Expression(loc, val, header->type), fn_decls(std::move(fn_decls)),
      header(std::move(header)) {
  assert(this->header->type_params.empty());
}

Expression *Impl::getFnDecl(uint64_t select_id) {
  for (auto &decl : fn_decls) {
    auto expr = dynamic_cast<Expression *>(decl.get());
    if (expr != nullptr && expr->val.id.id == select_id) {
      return expr;
    }
  }
  assert(false);
  return nullptr;
}

GenericExpression *Impl::getGenericFnDecl(uint64_t select_id) {
  for (auto &decl : fn_decls) {
    auto expr = dynamic_cast<GenericExpression *>(decl.get());
    if (expr != nullptr && expr->id.id == select_id) {
      return expr;
    }
  }
  assert(false);
  return nullptr;
}

ImplFnExtract::ImplFnExtract(const SourceLocation &loc, const Value &val,
                             Expression &impl, uint64_t extract_id,
                             std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), impl(impl),
      extract_id(extract_id) {}

ImplGenericFnExtract::ImplGenericFnExtract(
    const SourceLocation &loc, const Id &id, Expression &impl,
    uint64_t extract_id, std::shared_ptr<ast::TypeConstructor> type)
    : GenericExpression(loc, id, std::move(type)), impl(impl),
      extract_id(extract_id) {}

Allocation::Allocation(const SourceLocation &loc, const Value &val,
                       std::shared_ptr<ast::Type> type,
                       AllocationType allocType)
    : Expression(loc, val, std::move(type)), allocType(allocType) {
  assert(allocType == AllocationType::UNRESOLVED_LOCAL ||
         allocType == AllocationType::UNRESOLVED_FUNC_ARG);
}

BasicBlock::BasicBlock(const SourceLocation &loc, InstructionList body)
    : Instruction(loc), id(next_id()), body(std::move(body)), llvm_bb(nullptr) {
}

BasicBlock::BasicBlock(const SourceLocation &loc)
    : Instruction(loc), id(next_id()), body(ir::InstructionList()),
      llvm_bb(nullptr) {}

TupleLiteral::TupleLiteral(
    const SourceLocation &loc, const Value &val,
    const std::vector<std::reference_wrapper<Expression>> &exprs,
    std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), exprs(exprs) {
  // check that types of expressions match tuple types
  auto productType = dynamic_cast<ast::ProductType *>(Expression::type.get());
  assert(productType != nullptr);
  assert(productType->getNumFields() == exprs.size());
  for (size_t i = 0; i < productType->getNumFields(); i++) {
    assert(
        exprs[i].get().type->equalToExpected(*productType->getTypeOfField(i)));
  }
}

FunctionCall::FunctionCall(
    const SourceLocation &loc, const Value &val, Expression &function,
    const std::vector<std::reference_wrapper<Expression>> &arguments,
    std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), function(function),
      arguments(arguments) {
  // check that type matches function return type
  auto funcType = dynamic_cast<ast::FunctionType *>(function.type.get());
  if (funcType != nullptr) {
    assert(Expression::type->equalToExpected(*funcType->retType));
  } else {
    assert(false);
  }
}

Address::Address(const SourceLocation &loc, const Value &val, Expression &expr,
                 std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), expr(expr) {
  // check that type is pointer to type of expr
  auto pointerType = dynamic_cast<ast::PointerType *>(Expression::type.get());
  assert(pointerType != nullptr);
  assert(pointerType->type->equalToExpected(*expr.type));
}

Dereference::Dereference(const SourceLocation &loc, const Value &val,
                         Expression &expr, std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), expr(expr) {
  // check that expr is a pointer type, and that type matches what it points to
  auto exprPointerType =
      dynamic_cast<const ast::PointerType *>(expr.type->withoutMutability());
  assert(exprPointerType != nullptr);
  assert(Expression::type->equalToExpected(*exprPointerType->type));
}

FieldSelect::FieldSelect(const SourceLocation &loc, const Value &val,
                         Expression &expr, int32_t field_index,
                         std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), expr(expr),
      field_index(field_index) {
  // check that expr is product type
  auto exprProductType =
      dynamic_cast<const ast::ProductType *>(expr.type->withoutMutability());
  assert(exprProductType != nullptr);
}

GenericFunctionDeclare::GenericFunctionDeclare(
    const SourceLocation &loc, const Id &id,
    std::shared_ptr<ast::TypeConstructor> type,
    std::vector<std::reference_wrapper<Allocation>> argAllocs,
    BasicBlockList body, bool is_public, Instruction *impl)
    : GenericExpression(loc, id, std::move(type)),
      argAllocs(std::move(argAllocs)), body(std::move(body)),
      is_public(is_public), impl(impl) {}

FunctionDeclare::FunctionDeclare(
    const SourceLocation &loc, const Value &val,
    std::shared_ptr<ast::NamedFunctionType> type,
    std::vector<std::reference_wrapper<Allocation>> argAllocs,
    BasicBlockList body, bool is_public, Instruction *impl)
    : Expression(loc, val, std::move(type)), argAllocs(std::move(argAllocs)),
      body(std::move(body)), is_public(is_public), flow_metadata(),
      flow_state(FunctionEscapeAnalysisState::NOT_VISITED), impl(impl) {}

bool FunctionDeclare::hasFlowMetadata() {
  /* if the function is being visited, metadata isn't available (trying to
   * calculate would be infinite recursion) */
  return flow_state != FunctionEscapeAnalysisState::CUR_VISITING;
}

size_t
findIndexOfArg(const std::vector<std::reference_wrapper<Allocation>> &args,
               const Expression &expr) {
  for (size_t i = 0; i < args.size(); i++) {
    auto arg = args[i].get();
    if (arg.val.id == expr.val.id)
      return i;
  }

  assert(false);
  return 0;
}

void FunctionDeclare::addFlowMetadata(
    FlowList &flows,
    const std::vector<std::reference_wrapper<Expression>> &args,
    Expression &returnExpr) {
  assert(hasFlowMetadata());

  for (auto &flow : flow_metadata) {
    flows.push_back(flow.toFlow(args, returnExpr));
  }
}

Specialize::Specialize(const SourceLocation &loc, const Value &val,
                       GenericExpression &expr,
                       ast::TypeList actual_type_params,
                       std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), expr(expr),
      actual_type_params(std::move(actual_type_params)) {}

IntLiteral::IntLiteral(const SourceLocation &loc, const Value &val,
                       std::shared_ptr<ast::Type> type, uint64_t value)
    : Expression(loc, val, std::move(type)), value(value) {}

BoolLiteral::BoolLiteral(const SourceLocation &loc, const Value &val,
                         bool value)
    : Expression(loc, val, std::make_shared<ast::BoolType>(loc)), value(value) {
}

FloatLiteral::FloatLiteral(const SourceLocation &loc, const Value &val,
                           std::shared_ptr<ast::Type> type, double value)
    : Expression(loc, val, std::move(type)), value(value) {}

BuiltinOperator::BuiltinOperator(const SourceLocation &loc, const Value &val,
                                 ast::OperatorType opType,
                                 std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), opType(opType) {}

bool BuiltinOperator::hasFlowMetadata() { return true; }

void BuiltinOperator::addFlowMetadata(
    FlowList &flows,
    const std::vector<std::reference_wrapper<Expression>> &args,
    Expression &returnExpr) {
  /* pointer addition flow */
  if (opType == ast::OperatorType::UNSAFE_PTR_ADD) {
    // construct *(arg:0) flows to *(%RETURN)
    std::vector<std::vector<int32_t>> field_selects;
    field_selects.emplace_back();
    field_selects.emplace_back();
    auto from = FlowValue(args[0].get(), 1, field_selects);
    auto into = FlowValue(returnExpr, 1, field_selects);
    flows.push_back(Flow(from, into));
  }
  /* other builtin ops don't cause pointer flows */
}

BuiltinCast::BuiltinCast(const SourceLocation &loc, const Value &val,
                         Expression &expr, std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), expr(expr) {}

Store::Store(const SourceLocation &loc, Expression &storage, Expression &value)
    : Instruction(loc), storage(storage), value(value) {
  // storage must have an address
  assert(storage.isAddressable());
}

BasicBlockTerminator::BasicBlockTerminator(const SourceLocation &loc)
    : Instruction(loc) {}

Jump::Jump(const SourceLocation &loc, const BasicBlock &label)
    : BasicBlockTerminator(loc), label(label) {}

ConditionalJump::ConditionalJump(const SourceLocation &loc,
                                 const BasicBlock &true_label,
                                 const BasicBlock &false_label,
                                 Expression &condition)
    : BasicBlockTerminator(loc), true_label(true_label),
      false_label(false_label), condition(condition) {
  // make sure expr is boolean
  auto exprBoolType =
      dynamic_cast<const ast::BoolType *>(condition.type->withoutMutability());
  assert(exprBoolType != nullptr);
}

Return::Return(const SourceLocation &loc, Expression *expression)
    : BasicBlockTerminator(loc), expr(expression) {}

ForwardIdentifier::ForwardIdentifier(const SourceLocation &loc,
                                     const Value &val,
                                     std::shared_ptr<Symbol> symbol_ref,
                                     std::shared_ptr<ast::Type> type)
    : Expression(loc, val, std::move(type)), symbol_ref(std::move(symbol_ref)) {
  assert(Expression::type != nullptr);
}

bool ForwardIdentifier::isAddressable() const {
  if (symbol_ref->ir_decl.instr != nullptr) {
    auto ir_expr = dynamic_cast<Expression *>(symbol_ref->ir_decl.instr);
    return ir_expr != nullptr && ir_expr->isAddressable();
  } else {
    /* function's aren't addressable, globals are */
    if (dynamic_cast<ast::FunctionType *>(type.get()) != nullptr ||
        dynamic_cast<ast::NamedFunctionType *>(type.get()) != nullptr) {
      return false;
    } else {
      assert(symbol_ref->is_global);

      return true;
    }
  }
}

bool ForwardIdentifier::hasFlowMetadata() {
  /* TODO: load external identifier flow info from symbol tables */
  if (symbol_ref->ir_decl.instr == nullptr) {
    return false;
  }
  auto ir_expr = dynamic_cast<Expression *>(symbol_ref->ir_decl.instr);
  return ir_expr != nullptr && ir_expr->hasFlowMetadata();
}

void ForwardIdentifier::addFlowMetadata(
    FlowList &flows,
    const std::vector<std::reference_wrapper<Expression>> &args,
    Expression &returnExpr) {
  /* TODO: load external identifier flow info from symbol tables */
  auto ir_expr = dynamic_cast<Expression *>(symbol_ref->ir_decl.instr);
  assert(ir_expr != nullptr);
  ir_expr->addFlowMetadata(flows, args, returnExpr);
}

GenericForwardIdentifier::GenericForwardIdentifier(
    const SourceLocation &loc, const Id &id, std::shared_ptr<Symbol> symbol_ref,
    std::shared_ptr<ast::TypeConstructor> type_construct)
    : GenericExpression(loc, id, std::move(type_construct)),
      symbol_ref(std::move(symbol_ref)) {}

bool GlobalAllocation::isAddressable() const { return true; }

GlobalAllocation::GlobalAllocation(const SourceLocation &loc, const Value &val,
                                   std::shared_ptr<ast::Type> type,
                                   Expression &initial_val,
                                   std::shared_ptr<Symbol> symbol)
    : Expression(loc, val, std::move(type)), symbol(std::move(symbol)),
      initial_val(initial_val) {}

Sizeof::Sizeof(const SourceLocation &loc, const Value &val,
               std::shared_ptr<ast::Type> sizeof_type)
    : Expression(loc, val, std::make_shared<ast::IntType>(loc, 64, true)),
      sizeof_type(std::move(sizeof_type)) {}

const Id &getInstrId(Instruction *instr) {
  assert(instr != nullptr);
  auto expr = dynamic_cast<Expression *>(instr);
  if (expr) {
    return expr->val.id;
  }
  auto generic_expr = dynamic_cast<GenericExpression *>(instr);
  if (generic_expr) {
    return generic_expr->id;
  }
  assert(false);
}

} // namespace ovid::ir