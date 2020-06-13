#ifndef H_IR_INCL
#define H_IR_INCL

#include <functional>
#include <utility>

#include "ast.hpp"

/**
 * The higher level intermediate representation in the compiler (the LLVM IR is
 * the lower level IR) The IR represents the program as a series of discrete
 * instructions Compared to the AST, the IR is represented as a linear series of
 * instructions, instead of as a tree, and has gone through type checking
 *
 * The IR consists of global function, variable, and type declarations
 * The body of function declarations contain labels and expressions
 * All expressions in the IR take a number of arguments and produce one value
 * (jumps produce a void value). The result of every instruction is assigned to
 * a value
 *
 * fn f(a i32, b pkg:type i32) {
 *  val var := 5
 *  return (var + 1) * (7 + 5)
 * }
 *
 * EX:
 * FUNCTION pkg:mod:f(a i32, b pkg:type) i32 {
 *  var = ALLOCATE i32 // var is i32
 *  STORE var 5
 *  tmp1 = 1
 *  tmp2 = CALL i32:add var tmp1
 *  tmp3 = 7
 *  tmp4 = 5
 *  tmp5 = CALL i32:add tmp3 tmp4
 *  tmp6 = CALL i32:mul tmp2 tmp5
 *  RETURN tmp6
 * }
 *
 */

namespace ovid::ir {

uint64_t next_id();
void reset_id();

class Instruction;
typedef std::vector<std::unique_ptr<Instruction>> InstructionList;

/* a value in the ir -- holds the result of an instruction */
class Value {
public:
  /* name (scoped) from the source code */
  std::vector<std::string> sourceName;
  // if value was unnamed, it is given an id
  uint64_t id;

  // if the value has a source location or is using id
  bool hasSourceName;

  explicit Value(const std::vector<std::string> &sourceName);

  Value();
};

/* the base instruction type in the ir */
class Instruction {
public:
  SourceLocation loc;

  explicit Instruction(const SourceLocation &loc);

  virtual ~Instruction() = default;
};

/* an instruction which produces some kind of value that is usable in
 * computation basically everything but a jump or label */
class Expression : public Instruction {
public:
  Value val;
  std::shared_ptr<ast::Type> type;

  // if the expression has an address
  virtual bool isAddressable() const;

  Expression(const SourceLocation &loc, const Value &val,
             std::shared_ptr<ast::Type> type);
};

/* An allocation of storage space (either stack or heap) with the given type
 * Type of allocation is unknown at instantiation (later determined by escape
 * analysis)
 */

// Allocation types
enum class AllocationType {
  /* unresolved allocation types (type before escape analysis) */
  UNRESOLVED_FUNC_ARG, // a function argument
  UNRESOLVED_GLOBAL,   // global var declare
  UNRESOLVED_LOCAL,    // local variable declare

  /* resolved allocation types */
  STATIC,            /* static memory (global) */
  STACK,             /* allocated on stack */
  HEAP,              /* allocated on heap */
  ARG,               /* llvm function argument */
  ARG_COPY_TO_STACK, /* an llvm argument copied to stack (for args that need
                        address) */
  ARG_HEAP,          /* an llvm argument copied to heap */

};

class Allocation : public Expression {
public:
  AllocationType allocType;

  bool isAddressable() const override;

  // type should be a UNRESOLVED_* type
  Allocation(const SourceLocation &loc, const Value &val,
             std::shared_ptr<ast::Type> type, AllocationType allocType);
};

/* A labelled block of code, which can be jumped to */
class BasicBlock : public Instruction {
public:
  uint64_t id;
  InstructionList body;

  BasicBlock(const SourceLocation &loc, InstructionList body);
};

typedef std::vector<std::unique_ptr<BasicBlock>> BasicBlockList;

/* a function declaration in the ir
 * The function declaration is an expression b/c it produces a usable function
 * object */
class FunctionDeclare : public Expression {
public:
  std::vector<std::reference_wrapper<const Allocation>> argAllocs;

  BasicBlockList body;

  FunctionDeclare(
      const SourceLocation &loc, const Value &val,
      std::shared_ptr<ast::NamedFunctionType> type,
      const std::vector<std::reference_wrapper<const Allocation>> &argAllocs,
      BasicBlockList body);
};

/* int literal instruction */
class IntLiteral : public Expression {
public:
  uint64_t value;

  IntLiteral(const SourceLocation &loc, const Value &val,
             std::shared_ptr<ast::IntType> type, uint64_t value);
};

/* boolean literal */
class BoolLiteral : public Expression {
public:
  bool value;

  BoolLiteral(const SourceLocation &loc, const Value &val, bool value);
};

/* tuple literal */
class TupleLiteral : public Expression {
public:
  std::vector<std::reference_wrapper<const Expression>> exprs;

  TupleLiteral(
      const SourceLocation &loc, const Value &val,
      const std::vector<std::reference_wrapper<const Expression>> &exprs,
      std::shared_ptr<ast::Type> type);
};

/* a function call (including builtins)
 * TODO: represent interface calls somehow */
class FunctionCall : public Expression {
public:
  // the function being called
  const Expression &function;
  // arguments to function
  std::vector<std::reference_wrapper<const Expression>> arguments;

  FunctionCall(
      const SourceLocation &loc, const Value &val, const Expression &function,
      const std::vector<std::reference_wrapper<const Expression>> &arguments,
      std::shared_ptr<ast::Type> type);
};

/* a builtin function (arithmetic operators, etc) */
class BuiltinOperator : public Expression {
public:
  ast::OperatorType opType;

  BuiltinOperator(const SourceLocation &loc, const Value &val,
                  ast::OperatorType opType, std::shared_ptr<ast::Type> type);
};

/* builtin cast function (casts between int's, float's, etc) */
class BuiltinCast : public Expression {
public:
  const Expression &expr;

  BuiltinCast(const SourceLocation &loc, const Value &val,
              const Expression &expr, std::shared_ptr<ast::Type> type);
};

/* operation taking the address of a value */
class Address : public Expression {
public:
  const Expression &expr;

  Address(const SourceLocation &loc, const Value &val, const Expression &expr,
          std::shared_ptr<ast::Type> type);
};

/* dereference operation on a pointer */
class Dereference : public Expression {
public:
  const Expression &expr;

  bool isAddressable() const override;

  Dereference(const SourceLocation &loc, const Value &val,
              const Expression &expr, std::shared_ptr<ast::Type> type);
};

/* a field selection on a ProductType */
class FieldSelect : public Expression {
public:
  const Expression &expr;

  uint64_t field_index;

  bool isAddressable() const override;

  FieldSelect(const SourceLocation &loc, const Value &val,
              const Expression &expr, uint64_t field_index,
              std::shared_ptr<ast::Type> type);
};

/* A store into a value (has to be a value produced by Allocation)
 */
class Store : public Instruction {
public:
  const Expression &storage;
  const Expression &value;

  Store(const SourceLocation &loc, const Expression &storage,
        const Expression &value);
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
  const Expression &condition;

  ConditionalJump(const SourceLocation &loc, const BasicBlock &true_label,
                  const BasicBlock &false_label, const Expression &condition);
};

/* a return from a function */
class Return : public BasicBlockTerminator {
public:
  // expr may be null if nothing is returned
  const Expression *expr;

  Return(const SourceLocation &loc, const Expression *expression);
};

} // namespace ovid::ir

#endif