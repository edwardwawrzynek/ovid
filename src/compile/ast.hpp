#ifndef H_AST_INCL
#define H_AST_INCL

#include "symbols.hpp"
#include "tokenizer.hpp"
#include <iostream>
#include <memory>
#include <utility>
#include <vector>

// forward declare
namespace ovid::ast {
class Type;
}

namespace ovid {
/* a symbol and it's metadata (type, etc) */
struct Symbol {
public:
  // declaration or import location
  SourceLocation decl_loc;
  /* type of a the symbol */
  std::unique_ptr<ast::Type> type;
  /* if the symbol is exported out of it's module or not */
  bool is_public;
  // if the declaration point has been reached in resolve_pass
  // used to disallow using a local before declaration
  // functions and globals are set declared when created
  bool resolve_pass_declared_yet;
  /* if the symbol is mutable
   * only applicable for variables */
  bool is_mut;
  /* TODO: escape analysis metadata and other information loaded from headers */

  Symbol(SourceLocation decl_loc, bool is_public,
         bool resolve_pass_declared_yet, bool is_mut)
      : decl_loc(std::move(decl_loc)), type(), is_public(is_public),
        resolve_pass_declared_yet(resolve_pass_declared_yet), is_mut(is_mut){};

  explicit Symbol(SourceLocation decl_loc)
      : decl_loc(std::move(decl_loc)), type(), is_public(false),
        resolve_pass_declared_yet(false), is_mut(false){};
};
/* a type alias and its metadata */
struct TypeAlias {
public:
  // declaration or import location
  SourceLocation decl_loc;
  std::unique_ptr<ast::Type> type;

  explicit TypeAlias(SourceLocation decl_loc)
      : decl_loc(std::move(decl_loc)), type(){};
};

// name and type symbol tables
class ActiveScopes {
public:
  ActiveScope<Symbol> names;
  ActiveScope<TypeAlias> types;

  // push a scope that is somewhere in the root table
  void pushComponentScopesByName(const std::vector<std::string> &module);

  // pop a scope that has been added and is somewhere in the root table
  void popComponentScopesByName(const std::vector<std::string> &module);

  explicit ActiveScopes(const std::vector<std::string> &packageName);
};
} // namespace ovid

namespace ovid::ast {
class Expression;
class Statement;
class Type;

typedef std::vector<std::unique_ptr<Expression>> ExpressionList;
typedef std::vector<std::unique_ptr<Statement>> StatementList;
typedef std::vector<std::unique_ptr<Type>> TypeList;

/* a scoped block, containing statements as well as a variable symbol table
 * generally used as the container for statement blocks in the ast
 * module expressions don't have an associated scoped block -- instead, they are
 * managed by the global scope table
 *
 * This container is used for scopes inside of functions */
class ScopedBlock {
public:
  StatementList statements;
  // type aliases can't be declared inside functions, so only name table needed
  std::shared_ptr<ScopeTable<Symbol>> symbols;

  explicit ScopedBlock(std::shared_ptr<ScopeTable<Symbol>> symbols)
      : statements(), symbols(std::move(symbols)){};

  void addStatement(std::unique_ptr<Statement> statement);
};

/* ast types */
class Type {
public:
  virtual ~Type() = default;

  virtual Type *withoutMutability();
};

/* an unresolved type
 * not inferred, just not yet resolved by ResolvePass */
class UnresolvedType : public Type {
public:
  std::vector<std::string> scopes;
  std::string name;
  // if the type began with ::
  bool is_root_scoped;

  UnresolvedType(const std::vector<std::string> &scopes,
                 const std::string &name, bool is_root_scoped)
      : scopes(scopes), name(name), is_root_scoped(is_root_scoped){};
};

class VoidType : public Type {
public:
  VoidType(){};
};

class BoolType : public Type {
public:
  BoolType(){};
};

class IntType : public Type {
public:
  int size; // in bits
  bool isUnsigned;

  IntType(int size, bool isUnsigned) : size(size), isUnsigned(isUnsigned){};
};

class FloatType : public Type {
public:
  int size; // in bits
  explicit FloatType(int size) : size(size){};
};

class MutType : public Type {
public:
  std::unique_ptr<Type> type;

  explicit MutType(std::unique_ptr<Type> type) : type(std::move(type)){};

  Type *withoutMutability() override;
};

class PointerType : public Type {
public:
  std::unique_ptr<Type> type;

  explicit PointerType(std::unique_ptr<Type> type) : type(std::move(type)){};
};

class FunctionType : public Type {
public:
  TypeList argTypes;
  std::unique_ptr<Type> retType;

  FunctionType(TypeList argTypes, std::unique_ptr<Type> retType)
      : argTypes(std::move(argTypes)), retType(std::move(retType)){};
};

class NamedFunctionType : public Type {
public:
  std::unique_ptr<FunctionType> type;
  std::vector<std::string> argNames;

  NamedFunctionType(std::unique_ptr<FunctionType> type,
                    std::vector<std::string> argNames)
      : type(std::move(type)), argNames(std::move(argNames)){};
};

class FunctionPrototype {
public:
  std::unique_ptr<FunctionType> type;
  std::vector<std::string> argNames;
  std::string name;

  FunctionPrototype(std::unique_ptr<FunctionType> type,
                    std::vector<std::string> argNames, const std::string &name)
      : type(std::move(type)), argNames(std::move(argNames)), name(name){};
};

/* base ast node */
class Node {
public:
  SourceLocation loc;

  explicit Node(SourceLocation &loc) : loc(loc){};

  virtual ~Node() = default;
};

/* ast statements */
class Statement : public Node {
public:
  explicit Statement(SourceLocation &loc) : Node(loc){};
};

class VarDecl : public Statement {
public:
  std::string name;
  std::unique_ptr<Expression> initialValue;

  // resolved reference to entry for this symbol
  std::shared_ptr<Symbol> resolved_symbol;

  VarDecl(SourceLocation &loc, std::string &name,
          std::unique_ptr<Expression> initialValue)
      : Statement(loc), name(name), initialValue(std::move(initialValue)),
        resolved_symbol(){};
};

class FunctionDecl : public Statement {
public:
  std::unique_ptr<NamedFunctionType> type;
  std::string name;
  ScopedBlock body;

  FunctionDecl(SourceLocation &loc, std::unique_ptr<NamedFunctionType> type,
               const std::string &name, ScopedBlock body)
      : Statement(loc), type(std::move(type)), name(name),
        body(std::move(body)){};
};

class ModuleDecl : public Statement {
public:
  std::vector<std::string> scope;
  StatementList body;

  ModuleDecl(SourceLocation &loc, std::vector<std::string> scope,
             StatementList body)
      : Statement(loc), scope(std::move(scope)), body(std::move(body)){};
};

class TypeAliasDecl : public Statement {
public:
  std::string name;
  std::unique_ptr<Type> type;

  TypeAliasDecl(SourceLocation &loc, const std::string &name,
                std::unique_ptr<Type> type)
      : Statement(loc), name(name), type(std::move(type)){};
};

/* ast expressions */
class Expression : public Statement {
public:
  explicit Expression(SourceLocation &loc) : Statement(loc){};
};

class FunctionCall : public Expression {
public:
  std::unique_ptr<Expression> funcExpr;
  ExpressionList args;

  FunctionCall(SourceLocation &loc, std::unique_ptr<Expression> funcExpr,
               ExpressionList args)
      : Expression(loc), funcExpr(std::move(funcExpr)), args(std::move(args)){};
};

class Identifier : public Expression {
public:
  /* -- parsed symbol info -- */
  std::vector<std::string> scope;
  std::string id;
  bool is_root_scope; // if the identifier began with ::
  /* -- resolved symbol info -- */
  std::shared_ptr<Symbol> resolved_symbol;

  Identifier(SourceLocation &loc, const std::string &id,
             std::vector<std::string> scope, bool is_root_scope)
      : Expression(loc), scope(std::move(scope)), id(id),
        is_root_scope(is_root_scope), resolved_symbol(){};
};

enum class OperatorType {
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  NEGATIVE,
  DEREF,
  ADDR,
  PREFIX_INC,
  PREFIX_DEC,
  POSTFIX_INC,
  POSTFIX_DEC,
  BIN_AND,
  BIN_OR,
  BIN_XOR,
  BIN_NOT,
  LOG_AND,
  LOG_OR,
  LOG_NOT,
  EQUAL,
  NEQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,
  LEFT_SHIFT,
  RIGHT_SHIFT
};

class OperatorSymbol : public Expression {
public:
  OperatorType op;

  OperatorSymbol(SourceLocation &loc, OperatorType op)
      : Expression(loc), op(op){};
};

class Assignment : public Expression {
public:
  std::unique_ptr<Expression> lvalue;
  std::unique_ptr<Expression> rvalue;

  Assignment(SourceLocation &loc, std::unique_ptr<Expression> lvalue,
             std::unique_ptr<Expression> rvalue)
      : Expression(loc), lvalue(std::move(lvalue)), rvalue(std::move(rvalue)){};
};

class Literal : public Expression {
public:
  explicit Literal(SourceLocation &loc) : Expression(loc){};
};

class IntLiteral : public Literal {
public:
  const long value;

  IntLiteral(SourceLocation &loc, const long value)
      : Literal(loc), value(value){};
};

class Tuple : public Expression {
public:
  ExpressionList expressions;

  explicit Tuple(SourceLocation &loc, ExpressionList expressions)
      : Expression(loc), expressions(std::move(expressions)){};
};

} // namespace ovid::ast

#endif