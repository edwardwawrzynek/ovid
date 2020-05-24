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

  ScopedBlock(StatementList statements)
      : statements(std::move(statements)),
        symbols(std::make_shared<ScopeTable<Symbol>>()){};

  explicit ScopedBlock(std::shared_ptr<ScopeTable<Symbol>> symbols)
      : statements(), symbols(std::move(symbols)){};

  ScopedBlock(StatementList statements,
              std::shared_ptr<ScopeTable<Symbol>> symbols)
      : statements(std::move(statements)), symbols(std::move(symbols)){};

  void addStatement(std::unique_ptr<Statement> statement);
};

/* ast types */
class Type {
public:
  virtual ~Type() = default;

  virtual Type *withoutMutability();
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
  FloatType(int size) : size(size){};
};

class MutType : public Type {
public:
  std::unique_ptr<Type> type;

  MutType(std::unique_ptr<Type> type) : type(std::move(type)){};

  Type *withoutMutability() override;
};

class PointerType : public Type {
public:
  std::unique_ptr<Type> type;

  PointerType(std::unique_ptr<Type> type) : type(std::move(type)){};
};

class FunctionType : public Type {
public:
  TypeList argTypes;
  std::unique_ptr<Type> retType;

  FunctionType(TypeList argTypes, std::unique_ptr<Type> retType)
      : argTypes(std::move(argTypes)), retType(std::move(retType)){};
};

class FunctionPrototype {
public:
  std::unique_ptr<FunctionType> type;
  std::vector<std::string> argNames;
  std::string name;

  FunctionPrototype(std::unique_ptr<FunctionType> type,
                    std::vector<std::string> argNames, std::string &name)
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

  VarDecl(SourceLocation &loc, std::string &name,
          std::unique_ptr<Expression> initialValue)
      : Statement(loc), name(name), initialValue(std::move(initialValue)){};
};

class FunctionDecl : public Statement {
public:
  std::unique_ptr<FunctionPrototype> proto;
  ScopedBlock body;

  FunctionDecl(SourceLocation &loc, std::unique_ptr<FunctionPrototype> proto,
               ScopedBlock body)
      : Statement(loc), proto(std::move(proto)), body(std::move(body)){};
};

class ModuleDecl : public Statement {
public:
  std::vector<std::string> scope;
  StatementList body;

  ModuleDecl(SourceLocation &loc, std::vector<std::string> scope,
             StatementList body)
      : Statement(loc), scope(std::move(scope)), body(std::move(body)){};
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
  std::vector<std::string> scope;
  std::string id;
  bool is_root_scope; // if the identifier began with ::

  Identifier(SourceLocation &loc, const std::string &id,
             std::vector<std::string> scope, bool is_root_scope)
      : Expression(loc), scope(std::move(scope)), id(id),
        is_root_scope(is_root_scope){};
};

/* Identifier node, but with symbol resolved to scope table */
class IdentifierResolved : public Expression {
public:
  std::shared_ptr<Symbol> symbol;

  explicit IdentifierResolved(SourceLocation &loc,
                              std::shared_ptr<Symbol> symbol)
      : Expression(loc), symbol(std::move(symbol)){};
};

class OperatorSymbol : public Expression {
public:
  TokenType op;

  OperatorSymbol(SourceLocation &loc, TokenType op) : Expression(loc), op(op){};
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