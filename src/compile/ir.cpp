#include "ir.hpp"
#include "ast.hpp"

// ir is mostly header -- see ir.hpp

namespace ovid::ir {
static uint64_t ir_id = 0;

uint64_t next_id() { return ir_id++; }

void reset_id() { ir_id = 0; }

bool AllocationTypeIsArg(AllocationType type) {
  return type == AllocationType::UNRESOLVED_FUNC_ARG ||
         type == AllocationType::ARG ||
         type == AllocationType::ARG_COPY_TO_STACK ||
         type == AllocationType::ARG_HEAP;
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

/* ir instruction constructors
 * some are non trivial and perform checks on their arguments
 */

Value::Value(const std::vector<std::string> &sourceName)
    : sourceName(sourceName), id(next_id()), hasSourceName(true),
      llvm_value(nullptr) {}

Value::Value() : id(next_id()), hasSourceName(false), llvm_value(nullptr) {}

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
  auto tupleType = dynamic_cast<ast::TupleType *>(Expression::type.get());
  assert(tupleType != nullptr);
  assert(tupleType->types.size() == exprs.size());
  for (size_t i = 0; i < tupleType->types.size(); i++) {
    assert(exprs[i].get().type->equalToExpected(*tupleType->types[i]));
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

FunctionDeclare::FunctionDeclare(
    const SourceLocation &loc, const Value &val,
    std::shared_ptr<ast::NamedFunctionType> type,
    const std::vector<std::reference_wrapper<Allocation>> &argAllocs,
    BasicBlockList body, bool is_public)
    : Expression(loc, val, std::move(type)), argAllocs(argAllocs),
      body(std::move(body)), is_public(is_public), flow_metadata(),
      flow_state(FunctionEscapeAnalysisState::NOT_VISITED) {}

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

IntLiteral::IntLiteral(const SourceLocation &loc, const Value &val,
                       std::shared_ptr<ast::IntType> type, uint64_t value)
    : Expression(loc, val, std::move(type)), value(value) {}

BoolLiteral::BoolLiteral(const SourceLocation &loc, const Value &val,
                         bool value)
    : Expression(loc, val, std::make_shared<ast::BoolType>(loc)), value(value) {
}

FloatLiteral::FloatLiteral(const SourceLocation &loc, const Value &val,
                           std::shared_ptr<ast::FloatType> type, double value)
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
  /* The builtin operator's don't include any flow */
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
                                     std::shared_ptr<Symbol> symbol_ref)
    : Expression(loc, val, symbol_ref->type),
      symbol_ref(std::move(symbol_ref)) {}

bool ForwardIdentifier::isAddressable() const {
  if (symbol_ref->ir_decl_instruction != nullptr) {
    return symbol_ref->ir_decl_instruction->isAddressable();
  } else {
    /* function's aren't addressable, globals are */
    if (dynamic_cast<ast::FunctionType *>(symbol_ref->type.get()) != nullptr ||
        dynamic_cast<ast::NamedFunctionType *>(symbol_ref->type.get()) !=
            nullptr) {
      return false;
    } else {
      assert(symbol_ref->is_global);

      return true;
    }
  }
}

bool ForwardIdentifier::hasFlowMetadata() {
  /* TODO: load external identifier flow info from symbol tables */
  if (symbol_ref->ir_decl_instruction == nullptr) {
    return false;
  }
  return symbol_ref->ir_decl_instruction->hasFlowMetadata();
}

void ForwardIdentifier::addFlowMetadata(
    FlowList &flows,
    const std::vector<std::reference_wrapper<Expression>> &args,
    Expression &returnExpr) {
  /* TODO: allow for external identifiers */
  assert(symbol_ref->ir_decl_instruction != nullptr);

  symbol_ref->ir_decl_instruction->addFlowMetadata(flows, args, returnExpr);
}

bool GlobalAllocation::isAddressable() const { return true; }

GlobalAllocation::GlobalAllocation(const SourceLocation &loc, const Value &val,
                                   std::shared_ptr<ast::Type> type,
                                   Expression &initial_val,
                                   std::shared_ptr<Symbol> symbol)
    : Expression(loc, val, std::move(type)), symbol(std::move(symbol)),
      initial_val(initial_val) {}

} // namespace ovid::ir