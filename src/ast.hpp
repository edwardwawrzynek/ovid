#ifndef H_AST_INCL
#define H_AST_INCL

#include <iostream>
#include <utility>
#include <vector>
#include <memory>
#include "tokenizer.hpp"
#include "symbols.hpp"

namespace ovid::ast {
  class Expression;

  class Statement;

  typedef std::vector<std::unique_ptr<Expression>> ExpressionList;
  typedef std::vector<std::unique_ptr<Statement>> StatementList;

  /* a scoped block, containing statements as well as a variable symbol table
   * generally used as the container for statement blocks in the ast
   * scope ... {...} expressions are removed early in ast transformations and are represented in one file wide scope block
   *
   * This container is used for scopes inside of functions */
  class ScopedBlock {
    std::vector<std::unique_ptr<Statement>> statements;
    // type aliases can't be declared inside functions
    std::shared_ptr<ScopeTable<Symbol>> symbols;

  public:
    ScopedBlock(std::vector<std::unique_ptr<Statement>> statements): statements(std::move(statements)), symbols(std::make_shared<ScopeTable<Symbol>>()) {};
  };

  /* ast types */
  class Type {
  public:
    virtual ~Type() {};

    virtual Type *withoutMutability();
  };

  class BoolType : public Type {
  public:
    BoolType() {};
  };

  class IntType : public Type {
  public:
    int size; // in bits
    bool isUnsigned;

    IntType(int size, bool isUnsigned)
        : size(size), isUnsigned(isUnsigned) {};
  };

  class FloatType : public Type {
  public:
    int size; // in bits
    FloatType(int size)
        : size(size) {};
  };

  class MutType : public Type {
  public:
    std::unique_ptr<Type> type;

    MutType(std::unique_ptr<Type> type)
        : type(std::move(type)) {};

    Type *withoutMutability() override;
  };

  class FunctionType : public Type {
  public:
    std::vector<std::unique_ptr<Type>> argTypes;
    std::unique_ptr<Type> retType;

    FunctionType(std::vector<std::unique_ptr<Type>> argTypes, std::unique_ptr<Type> retType)
        : argTypes(std::move(argTypes)), retType(std::move(retType)) {};
  };

  class FunctionPrototype {
  public:
    std::unique_ptr<FunctionType> type;
    std::vector<std::string> argNames;
    std::string name;

    FunctionPrototype(std::unique_ptr<FunctionType> type, std::vector<std::string> argNames, std::string &name)
        : type(std::move(type)), argNames(std::move(argNames)), name(name) {};
  };

  /* base ast node */
  class Node {
  public:
    SourceLocation loc;

    explicit Node(SourceLocation &loc) : loc(loc) {};

    virtual ~Node() {};
  };

  /* ast statements */
  class Statement : public Node {
  public:
    explicit Statement(SourceLocation &loc) : Node(loc) {};
  };

  class VarDecl : public Statement {
  public:
    std::string name;
    std::unique_ptr<Expression> initialValue;

    VarDecl(SourceLocation &loc, std::string &name, std::unique_ptr<Expression> initialValue)
        : Statement(loc),
          name(name),
          initialValue(
              std::move(
                  initialValue)) {};
  };

  class FunctionDecl : public Statement {
  public:
    std::unique_ptr<FunctionPrototype> proto;
    ScopedBlock body;

    FunctionDecl(SourceLocation &loc, std::unique_ptr<FunctionPrototype> proto, ScopedBlock body)
        : Statement(loc), proto(std::move(proto)), body(std::move(body)) {};

  };

  class ModuleDecl : public Statement {
  public:
    std::vector<std::string> scope;
    // not ScopedBlock because moduleDecl's are removed early and transformed into the global ScopedBlock
    StatementList body;

    ModuleDecl(SourceLocation &loc, std::vector<std::string> scope, StatementList body)
        : Statement(loc), scope(std::move(scope)), body(std::move(body)) {};
  };

  /* ast expressions */
  class Expression : public Statement {
  public:
    explicit Expression(SourceLocation &loc)
        : Statement(loc) {};
  };

  class FunctionCall : public Expression {
  public:
    std::unique_ptr<Expression> funcExpr;
    ExpressionList args;

    FunctionCall(SourceLocation &loc, std::unique_ptr<Expression> funcExpr, ExpressionList args)
        : Expression(loc),
          funcExpr(
              std::move(
                  funcExpr)),
          args(std::move(
              args)) {};
  };

  class Identifier : public Expression {
  public:
    std::vector<std::string> scope;
    std::string id;

    Identifier(SourceLocation &loc, std::string &id, std::vector<std::string> scope)
        : Expression(loc),
          scope(std::move(scope)),
          id(id) {};
  };

  class OperatorSymbol : public Expression {
  public:
    TokenType op;

    OperatorSymbol(SourceLocation &loc, TokenType op)
        : Expression(loc), op(op) {};
  };

  class Assignment : public Expression {
  public:
    std::unique_ptr<Expression> lvalue;
    std::unique_ptr<Expression> rvalue;

    Assignment(SourceLocation &loc, std::unique_ptr<Expression> lvalue, std::unique_ptr<Expression> rvalue)
        : Expression(loc), lvalue(std::move(lvalue)), rvalue(std::move(rvalue)) {};
  };

  class Literal : public Expression {
  public:
    explicit Literal(SourceLocation &loc)
        : Expression(loc) {};
  };

  class IntLiteral : public Literal {
  public:
    const long value;

    IntLiteral(SourceLocation &loc, const long value)
        : Literal(loc), value(value) {};
  };

  class Tuple : public Expression {
  public:
    ExpressionList expressions;

    explicit Tuple(SourceLocation &loc, ExpressionList expressions)
        : Expression(loc),
          expressions(std::move(expressions)) {};
  };
}

#endif