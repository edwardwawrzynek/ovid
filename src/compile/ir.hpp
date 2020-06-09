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

  explicit Value(const std::vector<std::string> &sourceName)
      : sourceName(sourceName), id(0), hasSourceName(true){};

  Value() : id(next_id()), hasSourceName(false){};
};

/* the base instruction type in the ir */
class Instruction {
public:
  SourceLocation loc;

  explicit Instruction(const SourceLocation &loc) : loc(loc){};

  virtual ~Instruction() = default;
};

/* an instruction which produces some kind of value that is usable in
 * computation basically everything but a jump or label */
class Expression : public Instruction {
public:
  Value val;
  std::shared_ptr<ast::Type> type;

  explicit Expression(const SourceLocation &loc, const Value &val,
                      std::shared_ptr<ast::Type> type)
      : Instruction(loc), val(val), type(std::move(type)){};
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
  STATIC,           /* static memory (global) */
  STACK,            /* allocated on stack */
  HEAP,             /* allocated on heap */
  ARG,              /* llvm function argument */
  ARG_COPY_TO_STACK /* an llvm argument copied to stack (needs address) */

};

class Storage : public Expression {
public:
  Storage(const SourceLocation &loc, const Value &val,
          std::shared_ptr<ast::Type> type)
      : Expression(loc, val, std::move(type)){};
};

class Allocation : public Storage {
public:
  AllocationType allocType;

  // type should be a UNRESOLVED_* type
  Allocation(const SourceLocation &loc, const Value &val,
             std::shared_ptr<ast::Type> type, AllocationType allocType)
      : Storage(loc, val, std::move(type)), allocType(allocType){};
};

/* a function declaration in the ir
 * The function declaration is an expression b/c it produces a usable function
 * object */
class FunctionDeclare : public Expression {
public:
  std::vector<std::reference_wrapper<const Allocation>> argAllocs;

  InstructionList body;

  FunctionDeclare(
      const SourceLocation &loc, const Value &val,
      std::shared_ptr<ast::NamedFunctionType> type,
      const std::vector<std::reference_wrapper<const Allocation>> &argAllocs,
      InstructionList body)
      : Expression(loc, val, std::move(type)), argAllocs(argAllocs),
        body(std::move(body)){};
};

/* int literal instruction */
class IntLiteral : public Expression {
public:
  uint64_t value;

  IntLiteral(const SourceLocation &loc, const Value &val,
             std::shared_ptr<ast::IntType> type, uint64_t value)
      : Expression(loc, val, std::move(type)), value(value){};
};

/* boolean literal */
class BoolLiteral : public Expression {
public:
  bool value;

  BoolLiteral(const SourceLocation &loc, const Value &val, bool value)
      : Expression(loc, val, std::make_shared<ast::BoolType>(std::move(loc))),
        value(value){};
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
      std::shared_ptr<ast::Type> type)
      : Expression(loc, val, std::move(type)), function(function),
        arguments(arguments){};
};

/* a builtin function (arithmetic operators, etc) */
class BuiltinOperator : public Expression {
public:
  ast::OperatorType opType;

  BuiltinOperator(const SourceLocation &loc, const Value &val,
                  ast::OperatorType opType, std::shared_ptr<ast::Type> type)
      : Expression(loc, val, std::move(type)), opType(opType){};
};

/* operation taking the address of a value */
class Address : public Expression {
public:
  const Storage &expr;

  Address(const SourceLocation &loc, const Value &val, const Storage &expr,
          std::shared_ptr<ast::Type> type)
      : Expression(loc, val, std::move(type)), expr(expr){};
};

/* dereference operation on a pointer */
class Dereference : public Storage {
public:
  const Expression &expr;

  Dereference(const SourceLocation &loc, const Value &val,
              const Expression &expr, std::shared_ptr<ast::Type> type)
      : Storage(loc, val, std::move(type)), expr(expr){};
};

/* A store into a value (has to be a value produced by Allocation)
 */
class Store : public Instruction {
public:
  const Storage &storage;
  const Expression &value;

  Store(const SourceLocation &loc, const Storage &storage,
        const Expression &value)
      : Instruction(loc), storage(storage), value(value){};
};

/* A labelled point in code, which can be jumped to */
class Label : public Instruction {
public:
  uint64_t id;

  explicit Label(const SourceLocation &loc) : Instruction(loc), id(next_id()){};
};

/* an unconditional jump to a label */
class Jump : public Instruction {
public:
  const Label &label;

  Jump(const SourceLocation &loc, const Label &label)
      : Instruction(loc), label(label){};
};

/* a conditional jump to a label */
class ConditionalJump : public Instruction {
public:
  const Label &true_label;
  const Label &false_label;
  const Expression &condition;

  ConditionalJump(const SourceLocation &loc, const Label &true_label,
                  const Label &false_label, const Expression &condition)
      : Instruction(loc), true_label(true_label), false_label(false_label),
        condition(condition){};
};

} // namespace ovid::ir

#endif