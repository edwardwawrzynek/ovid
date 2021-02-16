#ifndef H_IR_INCL
#define H_IR_INCL

#include <functional>
#include <utility>

#include "ast.hpp"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Value.h"

/**
 * The higher level intermediate representation in the compiler (the LLVM IR is
 * the lower level IR) The IR represents the program as a series of discrete
 * instructions Compared to the AST, the IR is represented as a linear series of
 * instructions, instead of as a tree, and has gone through type checking
 *
 * The IR consists of a set of instructions. The major types of instructions
 * are: BasicBlock - a labelled set of instructions that end with a terminator
 * BasicBlockTerminator - instructions that alter program flow (jumps + returns)
 * Expression - instructions that produce a value
 *  FunctionDecl - expression containing args + a number of basic blocks that
 * make a body FunctionCall, etc
 */

namespace ovid::ir {

uint64_t next_id();
void reset_id();

class Instruction;
class Expression;
class Allocation;
typedef std::vector<std::unique_ptr<Instruction>> InstructionList;

/* escape analysis structures */
enum class EscapeType { NONE, RETURN, OTHER };

bool EscapeTypeIsEscape(EscapeType type);

class FlowValue {
public:
  /* expression flowing */
  Expression &expr;
  /* number of dereferences on the expression */
  int32_t indirect_level;
  /* field selections for each indirection
   * [0] is the field select on the value, [1] is the field select on the first
   * dereference of the value, etc
   * length should be equivalent to indirect_level + 1 */
  std::vector<std::vector<int32_t>> field_selects;

  /* if the value should be considered to be an escaping route
   * if RETURN, expr is not meaningful */
  EscapeType is_escape;

  /* get all the types that could be flowing through the value
   * if a product type is dereferenced without a field select, multiple types
   * may be flowing */
  std::vector<const ast::Type *> getFlowingTypes() const;

  /* check if anything is actually flowing through the value */
  bool isEmpty() const;

  void print(std::ostream &output) const;

  /* wrapper around getFlowFromExpressionWithoutCopy that adds the indirection
   * expected in a copy */
  static FlowValue getFlowFromExpression(Expression &expr);

  /* get a flow vaue of the dereference of the expression with no field selects
   */
  static FlowValue getFlowForDereference(Expression &expr);

  /* check if the field selects match or are more general than the passed value,
   * such that the passed value would flow with this value indirection level
   * should match, as should expr */
  bool fieldsMatchOrContain(const FlowValue &value) const;

  /* check if this contains the passed flow value */
  bool contains(const FlowValue &value) const;

  /* return this with the given number of indirections added */
  FlowValue withAddedIndirections(int32_t indirections) const;

  /* apply the field transformation from src->dst onto this */
  FlowValue specializeFields(const FlowValue &src, const FlowValue &dst) const;

  FlowValue(Expression &expr, int32_t indirect_level,
            const std::vector<std::vector<int32_t>> &field_selects,
            EscapeType is_escape);

  FlowValue(Expression &expr, int32_t indirect_level,
            const std::vector<std::vector<int32_t>> &field_selects);

  /* given an ir::Expression, produce a flow value of the expression including
   * field selects, dereferences, and addresses
   *
   * this returns the flow value with the dereference expected if a copy is
   * involved (such as in assignment). Because of this, the indirect_level may
   * be -1 (eg -- an address was taken)
   *
   * if an address is taken, field_selects_on_deref will hold the lost field
   * selects that should be added back on if a dereference is later taken */
  static FlowValue getFlowFromExpressionWithoutCopy(
      Expression &expr, std::vector<int32_t> &field_selects_on_deref);

private:
  /* add any types that may flow a type and indirections and field selects
   *applied to it
   **/
  static std::vector<const ast::Type *> getFlowingTypesFromType(
      const ast::Type *exprType, int32_t indirect_level,
      const std::vector<std::vector<int32_t>> &field_selects);

  static const ast::Type *
  applyFieldSelectToType(const ast::Type *type,
                         const std::vector<int32_t> &field_select);

  /* check if an expression is a global escape */
  static EscapeType isGlobalEscape(Expression &expr);
};

/* flow from one value to another */
class Flow {
public:
  FlowValue from;
  FlowValue into;

  // if anything is actually flowing
  bool isEmpty() const;

  void print(std::ostream &output) const;

  Flow(const FlowValue &from, const FlowValue &into);

  /* return true if the given value flows through this flow
   * ie -- if it is contained in from */
  bool containsFrom(const FlowValue &value) const;
  bool containsInto(const FlowValue &value) const;
  /* produce the most specialized flow for value contained in this flow */
  Flow specializeForFrom(const FlowValue &value) const;
  Flow specializeForInto(const FlowValue &value) const;
};

typedef std::vector<Flow> FlowList;

class Id {
public:
  /* scoped name from the source code */
  std::shared_ptr<Symbol> sourceName;
  /* if the id is a generic, then typeParams is its actual params */
  ast::TypeList typeParams;

  uint64_t id;

  // if the id has a source name (sourceName and typeParams are set)
  bool hasSourceName;

  explicit Id(std::shared_ptr<Symbol> sourceName);
  Id(std::shared_ptr<Symbol> sourceName, ast::TypeList typeParams);
  Id(const Id &old_id, ast::TypeList new_type_params);
  Id withNewId() const;

  Id();
};

inline bool operator==(const Id &lhs, const Id &rhs) {
  return lhs.id == rhs.id;
}

inline bool operator!=(const Id &lhs, const Id &rhs) { return !(lhs == rhs); }

/* a value in the ir -- holds the result of an instruction */
class Value {
public:
  Id id;

  // the generated llvm value (used by llvm_codegen)
  llvm::Value *llvm_value;

  explicit Value(std::shared_ptr<Symbol> sourceName);
  Value(std::shared_ptr<Symbol> sourceName, ast::TypeList typeParams);
  Value(const Id &old_id, ast::TypeList new_type_params);
  Value withNewId() const;

  Value();
};

inline bool operator==(const Value &lhs, const Value &rhs) {
  return lhs.id == rhs.id;
}

inline bool operator!=(const Value &lhs, const Value &rhs) {
  return !(lhs == rhs);
}

/* function flow metadata */
class FuncFlowValue {
public:
  /* index of argument, -1 for return, -2 for escape */
  int32_t arg_index;
  int32_t indirect_level;
  std::vector<std::vector<int32_t>> field_selects;

  FuncFlowValue(int32_t arg_index, int32_t indirect_level,
                const std::vector<std::vector<int32_t>> &field_selects);

  void print(std::ostream &output);

  /* given a flow value and a list of function args, create a func flow value */
  static FuncFlowValue
  fromFlowValue(const FlowValue &value,
                const std::vector<std::reference_wrapper<Allocation>> &args);

  /* given the expressions being passed to a function call, produce a FlowValue
   * adapted to the proper expression returnExpr is the function call
   * instruction escapeValue is a value that can be used if the value is an
   * escape (to get type checking to work). If this isn't an escape, it can be
   * null*/
  FlowValue
  toFlowValue(const std::vector<std::reference_wrapper<Expression>> &args,
              Expression &returnExpr, const FlowValue *escapeValue);
};

class FuncFlow {
public:
  FuncFlowValue from;
  FuncFlowValue into;

  FuncFlow(const FuncFlowValue &from, const FuncFlowValue &into);

  void print(std::ostream &output);

  static FuncFlow
  fromFlow(const Flow &flow,
           const std::vector<std::reference_wrapper<Allocation>> &args);

  Flow toFlow(const std::vector<std::reference_wrapper<Expression>> &args,
              Expression &returnExpr);
};

typedef std::vector<FuncFlow> FuncFlowList;

/* the base instruction type in the ir */
class Instruction {
public:
  SourceLocation loc;

  explicit Instruction(const SourceLocation &loc);

  virtual ~Instruction() = default;
};

/* an instruction which produces some kind of value that is usable in
 * computation - basically everything but a jump, label, or GenericExpression */
class Expression : public Instruction {
public:
  Value val;
  std::shared_ptr<ast::Type> type;

  // if the expression has an address
  virtual bool isAddressable() const;

  // if the expression has flow metadata available to be calculated
  virtual bool hasFlowMetadata();
  /* add flow metadata to flows, adjusting arg flows to args */
  virtual void
  addFlowMetadata(FlowList &flows,
                  const std::vector<std::reference_wrapper<Expression>> &args,
                  Expression &returnExpr);

  Expression(const SourceLocation &loc, const Value &val,
             std::shared_ptr<ast::Type> type);
};

/* an instruction which produces a type constructor. Not a usable value, but can
 * be specialized into one */
class GenericExpression : public Instruction {
public:
  Id id;

  std::shared_ptr<ast::TypeConstructor> type_construct;

  GenericExpression(const SourceLocation &loc, const Id &id,
                    std::shared_ptr<ast::TypeConstructor> type_construct);
};

/* a impl block constructor -- generic impl
 */
class GenericImpl : public GenericExpression {
public:
  // component function declarations
  // Instruction b/c function declares may be GenericExpression
  InstructionList fn_decls;
  std::shared_ptr<ast::ImplHeader> header;

  GenericImpl(const SourceLocation &loc, const Id &id, InstructionList fn_decls,
              std::shared_ptr<ast::ImplHeader> header);
};

/* an impl block on a concrete type
 */
class Impl : public Expression {
public:
  // component function declarations
  InstructionList fn_decls;
  std::shared_ptr<ast::ImplHeader> header;

  // get the associated function declaration with the given name
  Expression *getFnDecl(const std::string &name);
  // get the associated generic function declaration with the given name
  GenericExpression *getGenericFnDecl(const std::string &name);

  Impl(const SourceLocation &loc, const Value &val, InstructionList fn_decls,
       std::shared_ptr<ast::ImplHeader> header);
};

/* a function selection on an impl block
 * ie -- select a function expression from an impl given it's id */
class Select : public Expression {
public:
  Expression &impl;
  std::string method;

  Select(const SourceLocation &loc, const Value &val, Expression &impl,
         const std::string &method, std::shared_ptr<ast::Type> type);
};

/* a generic function selection on an impl block
 * same as Select, but selects a generic function */
class GenericSelect : public GenericExpression {
public:
  Expression &impl;
  std::string method;

  GenericSelect(const SourceLocation &loc, const Id &id, Expression &impl,
                const std::string &method,
                std::shared_ptr<ast::TypeConstructor> type);
};

/* a forward declared expression (likely a function or global) */
class ForwardIdentifier : public Expression {
public:
  std::shared_ptr<Symbol> symbol_ref;

  bool isAddressable() const override;
  bool hasFlowMetadata() override;
  void
  addFlowMetadata(FlowList &flows,
                  const std::vector<std::reference_wrapper<Expression>> &args,
                  Expression &returnExpr) override;

  ForwardIdentifier(const SourceLocation &loc, const Value &val,
                    std::shared_ptr<Symbol> symbol_ref,
                    std::shared_ptr<ast::Type> type);
};

/* a forward declared generic expression */
class GenericForwardIdentifier : public GenericExpression {
public:
  std::shared_ptr<Symbol> symbol_ref;

  GenericForwardIdentifier(
      const SourceLocation &loc, const Id &id,
      std::shared_ptr<Symbol> symbol_ref,
      std::shared_ptr<ast::TypeConstructor> type_construct);
};

/* An allocation of storage space (either stack or heap) with the given type
 * Type of allocation is unknown at instantiation (later determined by escape
 * analysis)
 */

// Allocation types
enum class AllocationType {
  /* unresolved allocation types (type before escape analysis) */
  UNRESOLVED_FUNC_ARG, // a function argument
  UNRESOLVED_LOCAL,    // local variable declare

  /* resolved allocation types */
  STACK,             /* allocated on stack */
  HEAP,              /* allocated on heap */
  ARG,               /* llvm function argument */
  ARG_COPY_TO_STACK, /* an llvm argument copied to stack (for args that need
                        address) */
  ARG_HEAP,          /* an llvm argument copied to heap */

};

bool AllocationTypeIsArg(AllocationType type);
bool AllocationTypeIsHeap(AllocationType type);
AllocationType AllocationTypeToHeap(AllocationType type);

class Allocation : public Expression {
public:
  AllocationType allocType;

  bool isAddressable() const override;

  // type should be a UNRESOLVED_* type
  Allocation(const SourceLocation &loc, const Value &val,
             std::shared_ptr<ast::Type> type, AllocationType allocType);
};

/* a global variable with an initial value */
class GlobalAllocation : public Expression {
public:
  std::shared_ptr<Symbol> symbol;
  Expression &initial_val;

  bool isAddressable() const override;

  GlobalAllocation(const SourceLocation &loc, const Value &val,
                   std::shared_ptr<ast::Type> type, Expression &initial_val,
                   std::shared_ptr<Symbol> symbol);
};

/* A labelled block of code, which can be jumped to */
class BasicBlock : public Instruction {
public:
  uint64_t id;
  InstructionList body;

  /* pointer to llvm block, used during llvm_codegen */
  llvm::BasicBlock *llvm_bb;

  BasicBlock(const SourceLocation &loc, InstructionList body);

  BasicBlock(const SourceLocation &loc);
};

typedef std::vector<std::unique_ptr<BasicBlock>> BasicBlockList;

/* a generic function declaration in the ir */
class GenericFunctionDeclare : public GenericExpression {
public:
  std::vector<std::reference_wrapper<Allocation>> argAllocs;

  BasicBlockList body;
  /* if the function has external linkage */
  bool is_public;

  /* impl containing this function */
  Instruction *impl;

  GenericFunctionDeclare(
      const SourceLocation &loc, const Id &id,
      std::shared_ptr<ast::TypeConstructor> type,
      std::vector<std::reference_wrapper<Allocation>> argAllocs,
      BasicBlockList body, bool is_public, Instruction *impl);
};

/* what state function flow metadata is in */
enum class FunctionEscapeAnalysisState {
  NOT_VISITED,  // no information
  CUR_VISITING, // flow is being calculated
  VISITED       // flow is available
};

/* a function declaration in the ir
 * The function declaration is an expression b/c it produces a usable function
 * object */
class FunctionDeclare : public Expression {
public:
  std::vector<std::reference_wrapper<Allocation>> argAllocs;

  BasicBlockList body;
  /* if the function has to have external linkage */
  bool is_public;

  /* escape analysis metadata */
  std::vector<FuncFlow> flow_metadata;
  FunctionEscapeAnalysisState flow_state;

  /* impl containing this function */
  Instruction *impl;

  bool hasFlowMetadata() override;
  void
  addFlowMetadata(FlowList &flows,
                  const std::vector<std::reference_wrapper<Expression>> &args,
                  Expression &returnExpr) override;

  FunctionDeclare(const SourceLocation &loc, const Value &val,
                  std::shared_ptr<ast::NamedFunctionType> type,
                  std::vector<std::reference_wrapper<Allocation>> argAllocs,
                  BasicBlockList body, bool is_public, Instruction *impl);
};

size_t
findIndexOfArg(const std::vector<std::reference_wrapper<Allocation>> &args,
               const Expression &expr);

/* a specialization of a GenericExpression into an Expression by applying type
 * parameters */
class Specialize : public Expression {
public:
  GenericExpression &expr;
  ast::TypeList actual_type_params;

  Specialize(const SourceLocation &loc, const Value &val,
             GenericExpression &expr, ast::TypeList actual_type_params,
             std::shared_ptr<ast::Type> type);
};

/* int literal instruction */
class IntLiteral : public Expression {
public:
  uint64_t value;

  IntLiteral(const SourceLocation &loc, const Value &val,
             std::shared_ptr<ast::Type> type, uint64_t value);
};

/* boolean literal */
class BoolLiteral : public Expression {
public:
  bool value;

  BoolLiteral(const SourceLocation &loc, const Value &val, bool value);
};

/* floating point literal */
class FloatLiteral : public Expression {
public:
  double value;

  FloatLiteral(const SourceLocation &loc, const Value &val,
               std::shared_ptr<ast::Type> type, double value);
};

/* tuple literal */
class TupleLiteral : public Expression {
public:
  std::vector<std::reference_wrapper<Expression>> exprs;

  TupleLiteral(const SourceLocation &loc, const Value &val,
               const std::vector<std::reference_wrapper<Expression>> &exprs,
               std::shared_ptr<ast::Type> type);
};

/* a function call (including builtins)
 * TODO: represent interface calls somehow */
class FunctionCall : public Expression {
public:
  // the function being called
  Expression &function;
  // arguments to function
  std::vector<std::reference_wrapper<Expression>> arguments;

  FunctionCall(const SourceLocation &loc, const Value &val,
               Expression &function,
               const std::vector<std::reference_wrapper<Expression>> &arguments,
               std::shared_ptr<ast::Type> type);
};

/* a builtin function (arithmetic operators, etc) */
class BuiltinOperator : public Expression {
public:
  ast::OperatorType opType;

  bool hasFlowMetadata() override;
  void
  addFlowMetadata(FlowList &flows,
                  const std::vector<std::reference_wrapper<Expression>> &args,
                  Expression &returnExpr) override;

  BuiltinOperator(const SourceLocation &loc, const Value &val,
                  ast::OperatorType opType, std::shared_ptr<ast::Type> type);
};

/* builtin cast function (casts between int's, float's, etc) */
class BuiltinCast : public Expression {
public:
  Expression &expr;

  BuiltinCast(const SourceLocation &loc, const Value &val, Expression &expr,
              std::shared_ptr<ast::Type> type);
};

/* operation taking the address of a value */
class Address : public Expression {
public:
  Expression &expr;

  Address(const SourceLocation &loc, const Value &val, Expression &expr,
          std::shared_ptr<ast::Type> type);
};

/* dereference operation on a pointer */
class Dereference : public Expression {
public:
  Expression &expr;

  bool isAddressable() const override;

  Dereference(const SourceLocation &loc, const Value &val, Expression &expr,
              std::shared_ptr<ast::Type> type);
};

/* a field selection on a ProductType */
class FieldSelect : public Expression {
public:
  Expression &expr;

  int32_t field_index;

  bool isAddressable() const override;

  FieldSelect(const SourceLocation &loc, const Value &val, Expression &expr,
              int32_t field_index, std::shared_ptr<ast::Type> type);
};

/* sizeof operator
 * TODO: implement usize and isize types */
class Sizeof : public Expression {
public:
  std::shared_ptr<ast::Type> sizeof_type;

  Sizeof(const SourceLocation &loc, const Value &val,
         std::shared_ptr<ast::Type> sizeof_type);
};

/* A store into a value (has to be a value produced by Allocation)
 */
class Store : public Instruction {
public:
  Expression &storage;
  Expression &value;

  Store(const SourceLocation &loc, Expression &storage, Expression &value);
};

/* basic block terminating instructions (jumps + returns) */
class BasicBlockTerminator : public Instruction {
public:
  explicit BasicBlockTerminator(const SourceLocation &loc);
};

/* an unconditional jump to a label */
class Jump : public BasicBlockTerminator {
public:
  const BasicBlock &label;

  Jump(const SourceLocation &loc, const BasicBlock &label);
};

/* a conditional jump to a label */
class ConditionalJump : public BasicBlockTerminator {
public:
  const BasicBlock &true_label;
  const BasicBlock &false_label;
  Expression &condition;

  ConditionalJump(const SourceLocation &loc, const BasicBlock &true_label,
                  const BasicBlock &false_label, Expression &condition);
};

/* a return from a function */
class Return : public BasicBlockTerminator {
public:
  // expr may be null if nothing is returned
  Expression *expr;

  Return(const SourceLocation &loc, Expression *expression);
};

// get the id of an instruction
// instr must be Expression or GenericExpression (ie have an id)
const Id &getInstrId(Instruction *instr);

} // namespace ovid::ir

#endif