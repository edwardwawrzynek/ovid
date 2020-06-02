#include <utility>

#include "ast.hpp"
#include "ir_visitor.hpp"

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
class Instruction;

/* a value in the ir -- holds the result of an instruction
 * T must be of type Instruction of derived class */
template <typename T> class Value {
public:
  /* name (scoped) from the source code */
  std::vector<std::string> sourceName;
  // if value was unnamed, it is given an id
  uint64_t id;

  // if the value has a source location or is using id
  bool hasSourceName;

  std::unique_ptr<T> instruction;

  Value(const std::vector<std::string> &sourceName,
        std::unique_ptr<T> instruction)
      : sourceName(sourceName), hasSourceName(true),
        instruction(std::move(instruction)){};

  explicit Value(std::unique_ptr<Instruction> instruction)
      : id(next_id()), hasSourceName(false),
        instruction(std::move(instruction)){};
};

/* the base instruction type in the ir */
class Instruction {
public:
  SourceLocation loc;

  explicit Instruction(SourceLocation loc) : loc(std::move(loc)){};
};

/* an instruction which produces some kind of value that is usable in
 * computation basically everything but a jump or label */
class Expression : public Instruction {
public:
  explicit Expression(SourceLocation loc) : Instruction(std::move(loc)){};
};

/* a function declaration in the ir
 * The function declaration is an expression b/c it produces a usable function
 * object */
class FunctionDeclare : public Expression {
public:
  std::unique_ptr<ast::NamedFunctionType> type;

  std::vector<std::unique_ptr<Value<Instruction>>> body;

  FunctionDeclare(SourceLocation loc,
                  std::unique_ptr<ast::NamedFunctionType> type,
                  std::vector<std::unique_ptr<Value<Instruction>>> body)
      : Expression(std::move(loc)), type(std::move(type)),
        body(std::move(body)){};
};

/* int literal instruction */
class IntLiteral : public Expression {
public:
  std::unique_ptr<ast::IntType> type;
  uint64_t value;

  IntLiteral(SourceLocation loc, std::unique_ptr<ast::IntType> type,
             uint64_t value)
      : Expression(std::move(loc)), type(std::move(type)), value(value){};
};

/* a function call (including builtins)
 * TODO: represent interface calls somehow */
class FunctionCall : public Expression {
public:
  // the function being called
  std::shared_ptr<Value<FunctionDeclare>> function;
  // arguments to function
  std::vector<std::shared_ptr<Value<Expression>>> arguments;

  FunctionCall(SourceLocation loc,
               std::shared_ptr<Value<FunctionDeclare>> function,
               const std::vector<std::shared_ptr<Value<Expression>>> &arguments)
      : Expression(std::move(loc)), function(std::move(function)),
        arguments(arguments){};
};

/* An allocation of storage space (either stack or heap) with the given type
 * Type of allocation is unknown at instantiation (later determined by escape
 * analysis)
 */
class Allocation : public Expression {
public:
  std::unique_ptr<ast::Type> type;

  bool is_heap_allocated;

  Allocation(SourceLocation loc, std::unique_ptr<ast::Type> type)
      : Expression(std::move(loc)), type(std::move(type)),
        is_heap_allocated(false){};
};

/* A store into a value (has to be a value produced by Allocation)
 */
class Store : public Instruction {
public:
  std::shared_ptr<Value<Allocation>> storage;
  std::shared_ptr<Value<Expression>> value;

  Store(SourceLocation loc, std::shared_ptr<Value<Allocation>> storage,
        std::shared_ptr<Value<Expression>> value)
      : Instruction(std::move(loc)), storage(std::move(storage)),
        value(std::move(value)){};
};

/* A labelled point in code, which can be jumped to */
class Label : public Instruction {
public:
  explicit Label(SourceLocation loc) : Instruction(std::move(loc)){};
};

/* an unconditional jump to a label */
class Jump : public Instruction {
public:
  std::shared_ptr<Value<Label>> label;

  Jump(SourceLocation loc, std::shared_ptr<Value<Label>> label)
      : Instruction(std::move(loc)), label(std::move(label)){};
};

/* a conditional jump to a label */
class ConditionalJump : public Instruction {
public:
  std::shared_ptr<Value<Label>> label;
  std::shared_ptr<Value<Expression>> condition;

  ConditionalJump(SourceLocation loc, std::shared_ptr<Value<Label>> label,
                  std::shared_ptr<Value<Expression>> condition)
      : Instruction(std::move(loc)), label(std::move(label)),
        condition(std::move(condition)){};
};

} // namespace ovid::ir