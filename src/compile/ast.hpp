#ifndef H_AST_INCL
#define H_AST_INCL

#include "symbols.hpp"
#include "tokenizer.hpp"
#include "llvm/IR/DerivedTypes.h"
#include <iostream>
#include <memory>
#include <utility>
#include <vector>

// forward declare
namespace ovid::ast {
class Type;
}

namespace ovid::ir {
class Expression;
}

namespace ovid {
/* a symbol and it's metadata (type, etc) */
struct Symbol {
public:
  // declaration or import location
  SourceLocation decl_loc;
  /* type of a the symbol */
  std::shared_ptr<ast::Type> type;
  /* if the symbol is exported out of it's module or not */
  bool is_public;
  // if the declaration point has been reached in resolve_pass
  // used to disallow using a local before declaration
  // functions and globals are set declared when created
  bool resolve_pass_declared_yet;
  /* if the symbol is mutable
   * only applicable for variables */
  bool is_mut;
  /* the symbol's declaration location in the ir
   * set and used by the type checker */
  ir::Expression *ir_decl_instruction;
  /* if the symbol is in a global scope */
  bool is_global;
  /* the symbol's name and containing table (used for generating fully scoped
   * names backwards) */
  std::string name;
  ScopeTable<Symbol> *parent_table;
  /* TODO: escape analysis metadata and other information loaded from headers */

  Symbol(const SourceLocation &decl_loc, bool is_public,
         bool resolve_pass_declared_yet, bool is_mut, bool is_global)
      : decl_loc(decl_loc), type(), is_public(is_public),
        resolve_pass_declared_yet(resolve_pass_declared_yet), is_mut(is_mut),
        ir_decl_instruction(nullptr), is_global(is_global), name(),
        parent_table(nullptr){};

  explicit Symbol(const SourceLocation &decl_loc)
      : decl_loc(decl_loc), type(), is_public(false),
        resolve_pass_declared_yet(false), is_mut(false),
        ir_decl_instruction(nullptr), is_global(false), name(),
        parent_table(nullptr){};

  /* get this symbol's fully scoped name */
  std::vector<std::string> getFullyScopedName();
};
/* a type alias and its metadata */
struct TypeAlias {
public:
  // declaration or import location
  SourceLocation decl_loc;
  std::shared_ptr<ast::Type> type;
  // if the type is marked pub
  bool is_public;

  // if the contained type has been run through type resolution
  bool inner_resolved;

  /* the symbol's name and containing table (used for generating fully scoped
   * names backwards) */
  std::string name;
  ScopeTable<TypeAlias> *parent_table;

  explicit TypeAlias(const SourceLocation &decl_loc)
      : decl_loc(decl_loc), type(), is_public(false), inner_resolved(false),
        name(), parent_table(nullptr){};

  TypeAlias(const SourceLocation &decl_loc, std::shared_ptr<ast::Type> type,
            bool is_public)
      : decl_loc(decl_loc), type(std::move(type)), is_public(is_public),
        inner_resolved(false), name(), parent_table(nullptr){};

  /* get this symbol's fully scoped name */
  std::vector<std::string> getFullyScopedName();
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

  ActiveScopes(const std::vector<std::string> &packageName,
               ScopeTable<Symbol> *rootNameScope,
               ScopeTable<TypeAlias> *rootTypeScope);
};

/* container for root name and type scopes */
class ScopesRoot {
public:
  std::unique_ptr<ScopeTable<Symbol>> names;
  std::unique_ptr<ScopeTable<TypeAlias>> types;

  ScopesRoot()
      : names(std::make_unique<ScopeTable<Symbol>>(true, nullptr, "")),
        types(std::make_unique<ScopeTable<TypeAlias>>(true, nullptr, "")){};
};
} // namespace ovid

namespace ovid::ast {
class Expression;
class Statement;
class Type;

typedef std::vector<std::unique_ptr<Expression>> ExpressionList;
typedef std::vector<std::unique_ptr<Statement>> StatementList;
typedef std::vector<std::shared_ptr<Type>> TypeList;

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
  std::unique_ptr<ScopeTable<Symbol>> symbols;

  explicit ScopedBlock(std::unique_ptr<ScopeTable<Symbol>> symbols)
      : statements(), symbols(std::move(symbols)){};

  void addStatement(std::unique_ptr<Statement> statement);
};

/* ast types */
class Type {

public:
  SourceLocation loc;
  virtual ~Type() = default;

  // check if a type is equivalent to the given expected type
  // if this type has a mut (anywhere in the chain) that isn't in expected,
  // valid (mut -> non mut is valid) if the expected type has a mut that this
  // type doesn't, invalid (non mut -> mut invalid)
  virtual bool equalToExpected(const Type &expected) const;
  // check if a type is or contains a pointer
  virtual bool containsPointer() const;

  virtual const Type *withoutMutability() const;
  virtual Type *withoutMutability();

  explicit Type(const SourceLocation &loc) : loc(loc){};
};

/* an unresolved type
 * not inferred, just not yet resolved by ResolvePass */
class UnresolvedType : public Type {
public:
  std::vector<std::string> scopes;
  std::string name;
  // if the type began with ::
  bool is_root_scoped;

  bool equalToExpected(const Type &expected) const override;

  UnresolvedType(const SourceLocation &loc,
                 const std::vector<std::string> &scopes,
                 const std::string &name, bool is_root_scoped)
      : Type(loc), scopes(scopes), name(name), is_root_scoped(is_root_scoped){};
};

/* a usage of an aliased type that has been resolved to it's entry in the type
 * alias tables */
class ResolvedAlias : public Type {
public:
  std::shared_ptr<TypeAlias> alias;

  bool equalToExpected(const Type &expected) const override;
  bool containsPointer() const override;

  ResolvedAlias(const SourceLocation &loc, std::shared_ptr<TypeAlias> alias)
      : Type(loc), alias(std::move(alias)){};
};

class VoidType : public Type {
public:
  bool equalToExpected(const Type &expected) const override;

  VoidType(const SourceLocation &loc) : Type(loc){};
};

class BoolType : public Type {
public:
  bool equalToExpected(const Type &expected) const override;

  BoolType(const SourceLocation &loc) : Type(loc){};
};

class IntType : public Type {
public:
  int size; // in bits
  bool isUnsigned;

  bool equalToExpected(const Type &expected) const override;

  IntType(const SourceLocation &loc, int size, bool isUnsigned)
      : Type(loc), size(size), isUnsigned(isUnsigned){};
};

class FloatType : public Type {
public:
  int size; // in bits

  bool equalToExpected(const Type &expected) const override;

  explicit FloatType(const SourceLocation &loc, int size)
      : Type(loc), size(size){};
};

class MutType : public Type {
public:
  std::shared_ptr<Type> type;

  const Type *withoutMutability() const override;
  Type *withoutMutability() override;

  bool equalToExpected(const Type &expected) const override;

  bool containsPointer() const override;

  explicit MutType(const SourceLocation &loc, std::shared_ptr<Type> type)
      : Type(loc), type(std::move(type)){};
};

class PointerType : public Type {
public:
  std::shared_ptr<Type> type;

  bool equalToExpected(const Type &expected) const override;

  bool containsPointer() const override;

  explicit PointerType(const SourceLocation &loc, std::shared_ptr<Type> type)
      : Type(loc), type(std::move(type)){};
};

class FunctionType : public Type {
public:
  TypeList argTypes;
  std::shared_ptr<Type> retType;

  bool equalToExpected(const Type &expected) const override;

  FunctionType(const SourceLocation &loc, TypeList argTypes,
               std::shared_ptr<Type> retType)
      : Type(loc), argTypes(std::move(argTypes)), retType(std::move(retType)){};
};

class NamedFunctionType : public FunctionType {
public:
  std::vector<std::string> argNames;
  std::vector<std::shared_ptr<Symbol>> resolvedArgs;

  bool equalToExpected(const Type &expected) const override;

  NamedFunctionType(const SourceLocation &loc, TypeList argTypes,
                    std::shared_ptr<Type> retType,
                    std::vector<std::string> argNames)
      : FunctionType(loc, std::move(argTypes), std::move(retType)),
        argNames(std::move(argNames)), resolvedArgs(){};
};

/* product types (types with interior fields -- tuples, struct, arrays) */
class ProductType : public Type {
public:
  ProductType(const SourceLocation &loc) : Type(loc){};

  // getNumFields and getTypeOfField are the internal representation of fields
  virtual std::shared_ptr<Type> getTypeOfField(int32_t field_index) const;
  virtual size_t getNumFields() const;
  // getNamedFieldIndex and getNumberedFieldIndex are the external
  // representation of fields
  virtual int32_t getNamedFieldIndex(const std::string &field_name) const;
  virtual int32_t getNumberedFieldIndex(int32_t field) const;

  bool containsPointer() const override;
};

class TupleType : public ProductType {
public:
  TypeList types;

  bool equalToExpected(const Type &expected) const override;

  std::shared_ptr<Type> getTypeOfField(int32_t field_index) const override;
  size_t getNumFields() const override;

  int32_t getNamedFieldIndex(const std::string &field_name) const override;
  int32_t getNumberedFieldIndex(int32_t field) const override;

  TupleType(const SourceLocation &loc, TypeList types)
      : ProductType(loc), types(std::move(types)){};
};

class StructType : public ProductType {
public:
  TypeList field_types;
  std::vector<std::string> field_names;
  std::vector<bool> fields_are_public;
  // type alias for this structure type
  std::weak_ptr<TypeAlias> type_alias;
  // if the field types have gone through type resolution
  bool fields_resolved;
  // llvm structure type alias
  llvm::StructType *llvm_type;

  bool equalToExpected(const Type &expected) const override;

  std::shared_ptr<Type> getTypeOfField(int32_t field_index) const override;
  size_t getNumFields() const override;

  int32_t getNamedFieldIndex(const std::string &field_name) const override;
  int32_t getNumberedFieldIndex(int32_t field) const override;

  StructType(const SourceLocation &loc, TypeList field_types,
             std::vector<std::string> field_names,
             std::vector<bool> fields_are_public)
      : ProductType(loc), field_types(std::move(field_types)),
        field_names(std::move(field_names)),
        fields_are_public(std::move(fields_are_public)), type_alias(),
        fields_resolved(false), llvm_type(nullptr){};

  StructType(const SourceLocation &loc, TypeList field_types,
             std::vector<std::string> field_names,
             std::vector<bool> fields_are_public,
             std::weak_ptr<TypeAlias> type_alias)
      : ProductType(loc), field_types(std::move(field_types)),
        field_names(std::move(field_names)),
        fields_are_public(std::move(fields_are_public)),
        type_alias(std::move(type_alias)), fields_resolved(false),
        llvm_type(nullptr){};
};

class FunctionPrototype {
public:
  std::shared_ptr<NamedFunctionType> type;
  std::string name;

  FunctionPrototype(std::shared_ptr<NamedFunctionType> type,
                    const std::string &name)
      : type(std::move(type)), name(name){};
};

/* base ast node */
class Node {
public:
  SourceLocation loc;

  explicit Node(const SourceLocation &loc) : loc(loc){};

  virtual ~Node() = default;
};

/* ast statements */
class Statement : public Node {
public:
  explicit Statement(const SourceLocation &loc) : Node(loc){};
};

class VarDecl : public Statement {
public:
  std::string name;
  std::unique_ptr<Expression> initialValue;
  // null if type inference used
  std::shared_ptr<Type> explicitType;

  // resolved reference to entry for this symbol
  std::shared_ptr<Symbol> resolved_symbol;

  VarDecl(const SourceLocation &loc, std::string &name,
          std::unique_ptr<Expression> initialValue,
          std::shared_ptr<Type> explicitType)
      : Statement(loc), name(name), initialValue(std::move(initialValue)),
        explicitType(std::move(explicitType)), resolved_symbol(){};
};

class FunctionDecl : public Statement {
public:
  std::shared_ptr<NamedFunctionType> type;
  std::string name;
  ScopedBlock body;

  std::shared_ptr<Symbol> resolved_symbol;

  FunctionDecl(const SourceLocation &loc,
               std::shared_ptr<NamedFunctionType> type, const std::string &name,
               ScopedBlock body)
      : Statement(loc), type(std::move(type)), name(name),
        body(std::move(body)), resolved_symbol(){};
};

class NativeFunctionDecl : public Statement {
public:
  std::shared_ptr<Symbol> sym;

  NativeFunctionDecl(const SourceLocation &loc, std::shared_ptr<Symbol> sym)
      : Statement(loc), sym(std::move(sym)){};
};

class ModuleDecl : public Statement {
public:
  std::vector<std::string> scope;
  StatementList body;

  ModuleDecl(const SourceLocation &loc, std::vector<std::string> scope,
             StatementList body)
      : Statement(loc), scope(std::move(scope)), body(std::move(body)){};
};

class TypeAliasDecl : public Statement {
public:
  std::string name;
  std::shared_ptr<TypeAlias> type;

  TypeAliasDecl(const SourceLocation &loc, const std::string &name,
                std::shared_ptr<TypeAlias> type)
      : Statement(loc), name(name), type(std::move(type)){};
};

class IfStatement : public Statement {
public:
  // if - elsif... - else chain
  // for else, condition is just true
  ExpressionList conditions;
  std::vector<ScopedBlock> bodies;

  IfStatement(const SourceLocation &loc, ExpressionList conditions,
              std::vector<ScopedBlock> bodies)
      : Statement(loc), conditions(std::move(conditions)),
        bodies(std::move(bodies)){};
};

class WhileStatement : public Statement {
public:
  std::unique_ptr<Expression> cond;
  ScopedBlock body;

  WhileStatement(const SourceLocation &loc, std::unique_ptr<Expression> cond,
                 ScopedBlock body)
      : Statement(loc), cond(std::move(cond)), body(std::move(body)){};
};

/* return statement (expression may be null) */
class ReturnStatement : public Statement {
public:
  std::unique_ptr<Expression> expression;

  ReturnStatement(const SourceLocation &loc,
                  std::unique_ptr<Expression> expression)
      : Statement(loc), expression(std::move(expression)){};
};

/* ast expressions */
class Expression : public Statement {
public:
  explicit Expression(const SourceLocation &loc) : Statement(loc){};
};

class FunctionCall : public Expression {
public:
  std::unique_ptr<Expression> funcExpr;
  ExpressionList args;

  FunctionCall(const SourceLocation &loc, std::unique_ptr<Expression> funcExpr,
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

  Identifier(const SourceLocation &loc, const std::string &id,
             std::vector<std::string> scope, bool is_root_scope)
      : Expression(loc), scope(std::move(scope)), id(id),
        is_root_scope(is_root_scope), resolved_symbol(){};
};

enum class OperatorType {
  ADD,
  ADD_ASSIGN,
  SUB,
  SUB_ASSIGN,
  MUL,
  DIV,
  MOD,
  NEGATIVE,
  DEREF,
  ADDR,
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

  OperatorSymbol(const SourceLocation &loc, OperatorType op)
      : Expression(loc), op(op){};
};

class Assignment : public Expression {
public:
  std::unique_ptr<Expression> lvalue;
  std::unique_ptr<Expression> rvalue;

  Assignment(const SourceLocation &loc, std::unique_ptr<Expression> lvalue,
             std::unique_ptr<Expression> rvalue)
      : Expression(loc), lvalue(std::move(lvalue)), rvalue(std::move(rvalue)){};
};

class FieldAccess : public Expression {
public:
  std::unique_ptr<Expression> lvalue;

  std::string field;
  int32_t field_num;

  bool has_field_num;

  FieldAccess(const SourceLocation &loc, std::unique_ptr<Expression> lvalue,
              const std::string &field)
      : Expression(loc), lvalue(std::move(lvalue)), field(field), field_num(0),
        has_field_num(false){};

  FieldAccess(const SourceLocation &loc, std::unique_ptr<Expression> lvalue,
              int32_t field_num)
      : Expression(loc), lvalue(std::move(lvalue)), field(""),
        field_num(field_num), has_field_num(true){};
};

class Literal : public Expression {
public:
  explicit Literal(const SourceLocation &loc) : Expression(loc){};
};

class IntLiteral : public Literal {
public:
  const int64_t value;

  IntLiteral(const SourceLocation &loc, int64_t value)
      : Literal(loc), value(value){};
};

class BoolLiteral : public Literal {
public:
  const bool value;

  BoolLiteral(const SourceLocation &loc, bool value)
      : Literal(loc), value(value){};
};

class FloatLiteral : public Literal {
public:
  const double value;

  FloatLiteral(const SourceLocation &loc, double value)
      : Literal(loc), value(value){};
};

class CharLiteral : public Literal {
public:
  const char value;

  CharLiteral(const SourceLocation &loc, char value)
      : Literal(loc), value(value){};
};

class Tuple : public Expression {
public:
  ExpressionList expressions;

  explicit Tuple(const SourceLocation &loc, ExpressionList expressions)
      : Expression(loc), expressions(std::move(expressions)){};
};

} // namespace ovid::ast

#endif