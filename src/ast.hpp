#ifndef H_AST_INCL
#define H_AST_INCL

#include "symbols.hpp"
#include "tokenizer.hpp"
#include "llvm/IR/DerivedTypes.h"
#include <cstddef>
#include <iostream>
#include <memory>
#include <utility>
#include <vector>

// forward declare
namespace ovid::ast {
class Expression;
class Statement;
class TypeConstructor;
class Type;
class FormalTypeParameter;

typedef std::vector<std::unique_ptr<Expression>> ExpressionList;
typedef std::vector<std::unique_ptr<Statement>> StatementList;
typedef std::vector<std::shared_ptr<Type>> TypeList;
typedef std::vector<std::shared_ptr<const Type>> ConstTypeList;
typedef std::vector<std::unique_ptr<TypeConstructor>> TypeConstructorList;
typedef std::vector<std::shared_ptr<FormalTypeParameter>>
    FormalTypeParameterList;
typedef std::function<bool(const FormalTypeParameter &param,
                           const Type &expected)>
    TypeParamEqualPredicate;
} // namespace ovid::ast

namespace ovid::ir {
class Instruction;
}

namespace ovid {
/* a component of a scope path */
struct ScopeComponent {
public:
  std::string ident;
  std::shared_ptr<ast::ImplHeader> impl;
  bool is_ident;

  ScopeComponent(std::string ident) : ident(std::move(ident)), is_ident(true){};
  ScopeComponent(std::shared_ptr<ast::ImplHeader> impl)
      : impl(std::move(impl)), is_ident(false){};
};

/* a reference to a declaration entity in the ir
 * this contains both the declaring instruction + the impl block it is contained
 * in (if present) */
class IrDecl {
public:
  ir::Instruction *instr;
  ir::Instruction *impl;

  IrDecl(ir::Instruction *instr, ir::Instruction *impl)
      : instr(instr), impl(impl){};
};

/* a symbol and it's metadata (type, etc) */
struct Symbol {
public:
  // declaration or import location
  SourceLocation decl_loc;
  /* type of a the symbol */
  std::shared_ptr<ast::TypeConstructor> type;
  /* if the symbol is exported out of it's module or not */
  bool is_public;
  // if the declaration point has been reached in resolve_pass
  // used to disallow using a local before declaration
  // functions and globals are set declared when created
  bool resolve_pass_declared_yet;
  /* if the symbol is mutable
   * only applicable for variables */
  bool is_mut;
  /* the symbol's declaration instruction in the ir
   * set. Used by the type checker */
  IrDecl ir_decl;
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
        ir_decl(nullptr, nullptr), is_global(is_global), name(),
        parent_table(nullptr){};

  explicit Symbol(const SourceLocation &decl_loc)
      : decl_loc(decl_loc), type(), is_public(false),
        resolve_pass_declared_yet(false), is_mut(false),
        ir_decl(nullptr, nullptr), is_global(false), name(),
        parent_table(nullptr){};

  /* get this symbol's fully scoped name */
  std::vector<ScopeComponent> getFullyScopedName() const;
};
/* a type alias and its metadata */
struct TypeAlias {
public:
  // declaration or import location
  SourceLocation decl_loc;
  // aliased type/constructor
  std::shared_ptr<ast::TypeConstructor> type;
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

  TypeAlias(const SourceLocation &decl_loc,
            std::shared_ptr<ast::TypeConstructor> type, bool is_public)
      : decl_loc(decl_loc), type(std::move(type)), is_public(is_public),
        inner_resolved(false), name(), parent_table(nullptr){};

  /* get this symbol's fully scoped name */
  std::vector<ScopeComponent> getFullyScopedName() const;
};

/* convert a std::vector<std::string> scoped name to a single string seperated
 * by : */
std::string scopedNameToString(const std::vector<ScopeComponent> &scopes);

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
               int64_t package_version, ScopeTable<Symbol> *rootNameScope,
               ScopeTable<TypeAlias> *rootTypeScope);

  ActiveScopes();
};

/* container for root name and type scopes */
class ScopesRoot {
public:
  std::unique_ptr<ScopeTable<Symbol>> names;
  std::unique_ptr<ScopeTable<TypeAlias>> types;
  // impl scopes don't fit into the normal scope hierarchy, so they are all held
  // at the root
  // TODO: for non generic impl's, we could store a hashtable of type -> impl
  // rather than requiring a search through this list For generic impls, type
  // patterns can't be hashed, so a list is needed
  std::vector<std::unique_ptr<ScopeTable<Symbol>>> impls;

  // specialized impl scopes table
  std::vector<std::unique_ptr<ScopeTable<Symbol>>> specialized_impls;

  ScopesRoot()
      : names(std::make_unique<ScopeTable<Symbol>>(true, nullptr, "")),
        types(std::make_unique<ScopeTable<TypeAlias>>(true, nullptr, "")),
        impls(){};
};

template <class T>
bool vectorContains(const std::vector<T> &vector, const T &value) {
  return std::find(vector.begin(), vector.end(), value) != vector.end();
}

} // namespace ovid

namespace ovid::ast {

/* id generation for type parameters */
uint64_t next_id();
void reset_id();

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
/* a type constructor (ie function that produces a concrete type) */
class TypeConstructor : public std::enable_shared_from_this<TypeConstructor> {
public:
  SourceLocation loc;
  virtual ~TypeConstructor() = default;

  explicit TypeConstructor(const SourceLocation &loc) : loc(loc){};

  // get the number of arguments that the type constructor expects
  virtual size_t numTypeParams() const;

  // if this constructor is trivial (0 param and instance of Type), return
  // construction result (nullptr otherwise)
  virtual std::shared_ptr<Type> trivialConstruct();
  // if this constructor is 0 param, return construction result (nullptr
  // otherwise)
  virtual std::shared_ptr<Type> noParamConstruct();
  // get this constructor's formal type parameters
  virtual const FormalTypeParameterList &getFormalTypeParameters() const;
  virtual FormalTypeParameterList &getFormalTypeParameters();
  // get this constructor's inner type (with formal parameters in it)
  virtual std::shared_ptr<Type> getFormalBoundType() const;
  // get the formal parameter scope table for this type
  virtual ScopeTable<TypeAlias> *getFormalScopeTable() const;
};

/* a type constructor (FormalTypeParameter, FormalTypeParameter, ...) -> Type */
class GenericTypeConstructor : public TypeConstructor {
public:
  // parameters
  FormalTypeParameterList params;
  // concrete type (with FormalTypeParameter's)
  std::shared_ptr<Type> type;

  // type scope table for type parameters
  std::unique_ptr<ScopeTable<TypeAlias>> type_scope;

  size_t numTypeParams() const override;
  const FormalTypeParameterList &getFormalTypeParameters() const override;
  FormalTypeParameterList &getFormalTypeParameters() override;
  std::shared_ptr<Type> getFormalBoundType() const override;
  ScopeTable<TypeAlias> *getFormalScopeTable() const override;
  std::shared_ptr<Type> noParamConstruct() override;

  GenericTypeConstructor(const SourceLocation &loc,
                         FormalTypeParameterList params,
                         std::shared_ptr<Type> type);
};

/* a concrete type -- a type that can be created */
class Type : public TypeConstructor {
public:
  // Check if this and expected are of the same type. If strict is false, mut ->
  // non mut transformed types are considered equal (ex: if strict, *mut i32 !=
  // *i32, if not strict, *mut i32 == *i32). typeParamFunc is called when this
  // is a FormalTypeParameter. It should return true if the this and expected
  // should be considered equal. if typeParamFunc is null, then only matching
  // FormalTypeParameter's are considered equal. typeParamFunc is useful for
  // checking + building param substitutions.
  virtual bool equal(const Type &expected, bool strict,
                     const TypeParamEqualPredicate *typeParamFunc) const;
  // check if a type is equivalent to the given expected type
  // if this type has a mut (anywhere in the chain) that isn't in expected,
  // valid (mut -> non mut is valid) if the expected type has a mut that this
  // type doesn't, invalid (non mut -> mut invalid)
  bool equalToExpected(const Type &other) const;
  // check if a type is exactly equivalent to the given type
  // mutability much match exactly
  bool equalStrict(const Type &other) const;
  // check if a type is or containsFrom a pointer
  virtual bool containsPointer() const;
  // produce a hash of the type
  virtual std::size_t hash() const;

  virtual const Type *withoutMutability() const;
  virtual Type *withoutMutability();

  size_t numTypeParams() const override;

  explicit Type(const SourceLocation &loc) : TypeConstructor(loc){};
};

/* a generic type parameter
 * eg -- the 'T' in 'Type<T>' */
class FormalTypeParameter : public Type {
public:
  std::string name;
  // Because TypeParameters always exist in GenericTypeConstructors, id is used
  // for equality checks to replace type parameters when GenericTypeConstructors
  // are turned into concrete types
  uint64_t id;
  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  FormalTypeParameter(const SourceLocation &loc, const std::string &name)
      : Type(loc), name(name), id(next_id()){};
};

/* an unresolved type (ie use of a type alias)
 * not inferred, just not yet resolved by ResolvePass */
class UnresolvedType : public Type {
public:
  // type identifier
  std::vector<std::string> scopes;
  std::string name;
  // type parameters
  ast::TypeList type_params;
  // if the type began with ::
  bool is_root_scoped;

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;

  UnresolvedType(const SourceLocation &loc, std::vector<std::string> scopes,
                 const std::string &name, bool is_root_scoped,
                 ast::TypeList type_params)
      : Type(loc), scopes(std::move(scopes)), name(name),
        type_params(std::move(type_params)), is_root_scoped(is_root_scoped){};
};

class VoidType : public Type {
public:
  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  VoidType(const SourceLocation &loc) : Type(loc){};
};

class BoolType : public Type {
public:
  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  BoolType(const SourceLocation &loc) : Type(loc){};
};

class IntType : public Type {
public:
  int size; // in bits
  bool isUnsigned;

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  IntType(const SourceLocation &loc, int size, bool isUnsigned)
      : Type(loc), size(size), isUnsigned(isUnsigned){};
};

class FloatType : public Type {
public:
  int size; // in bits

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  explicit FloatType(const SourceLocation &loc, int size)
      : Type(loc), size(size){};
};

class MutType : public Type {
public:
  std::shared_ptr<Type> type;

  const Type *withoutMutability() const override;
  Type *withoutMutability() override;

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  bool containsPointer() const override;

  explicit MutType(const SourceLocation &loc, std::shared_ptr<Type> type)
      : Type(loc), type(std::move(type)){};
};

class PointerType : public Type {
public:
  std::shared_ptr<Type> type;

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  bool containsPointer() const override;

  explicit PointerType(const SourceLocation &loc, std::shared_ptr<Type> type)
      : Type(loc), type(std::move(type)){};
};

class FunctionType : public Type {
public:
  TypeList argTypes;
  std::shared_ptr<Type> retType;

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  FunctionType(const SourceLocation &loc, TypeList argTypes,
               std::shared_ptr<Type> retType)
      : Type(loc), argTypes(std::move(argTypes)), retType(std::move(retType)){};
};

class NamedFunctionType : public FunctionType {
public:
  std::vector<std::string> argNames;
  std::vector<std::shared_ptr<Symbol>> resolvedArgs;
  bool is_self_func;

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;

  NamedFunctionType(const SourceLocation &loc, TypeList argTypes,
                    std::shared_ptr<Type> retType,
                    std::vector<std::string> argNames, bool is_self_func)
      : FunctionType(loc, std::move(argTypes), std::move(retType)),
        argNames(std::move(argNames)), resolvedArgs(),
        is_self_func(is_self_func){};
};

/* product types (types with interior fields -- tuples, struct, arrays) */
class ProductType : public Type {
public:
  explicit ProductType(const SourceLocation &loc) : Type(loc){};

  // getNumFields and getTypeOfField are the internal representation of fields
  virtual std::shared_ptr<Type> getTypeOfField(int32_t field_index) const;
  // check if the given field is pub
  virtual bool fieldIsPublic(int32_t field_index) const;
  // get the type's type alias entry (if present)
  virtual const TypeAlias *getTypeAlias() const;
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

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  std::shared_ptr<Type> getTypeOfField(int32_t field_index) const override;
  size_t getNumFields() const override;
  bool fieldIsPublic(int32_t field_index) const override;
  const TypeAlias *getTypeAlias() const override;

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
  TypeAlias *type_alias;
  // llvm structure type alias
  llvm::StructType *llvm_type;
  // if the type has been through type construction
  // if true, actual_generic_params should be set
  bool constructed;
  // the actual type parameters that this struct was constructed with
  // needed for name mangling + type equality checking
  ast::TypeList actual_generic_params;

  bool equal(const Type &expected, bool strict,
             const TypeParamEqualPredicate *typeParamFunc) const override;
  std::size_t hash() const override;

  std::shared_ptr<Type> getTypeOfField(int32_t field_index) const override;
  size_t getNumFields() const override;
  bool fieldIsPublic(int32_t field_index) const override;
  const TypeAlias *getTypeAlias() const override;

  int32_t getNamedFieldIndex(const std::string &field_name) const override;
  int32_t getNumberedFieldIndex(int32_t field) const override;

  // a struct type has a public constructor if all fields are public
  bool hasPublicConstructor() const;

  StructType(const SourceLocation &loc, TypeList field_types,
             std::vector<std::string> field_names,
             std::vector<bool> fields_are_public)
      : ProductType(loc), field_types(std::move(field_types)),
        field_names(std::move(field_names)),
        fields_are_public(std::move(fields_are_public)), type_alias(),
        llvm_type(nullptr), constructed(false), actual_generic_params(){};

  StructType(const SourceLocation &loc, TypeList field_types,
             std::vector<std::string> field_names,
             std::vector<bool> fields_are_public, TypeAlias *type_alias,
             bool constructed, ast::TypeList actual_generic_params)
      : ProductType(loc), field_types(std::move(field_types)),
        field_names(std::move(field_names)),
        fields_are_public(std::move(fields_are_public)), type_alias(type_alias),
        llvm_type(nullptr), constructed(constructed),
        actual_generic_params(std::move(actual_generic_params)){};
};

inline bool operator==(const Type &lhs, const Type &rhs) {
  return lhs.equalStrict(rhs);
}

class FunctionPrototype {
public:
  std::shared_ptr<TypeConstructor> type;
  std::string name;

  FunctionPrototype(std::shared_ptr<TypeConstructor> type,
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
  std::shared_ptr<TypeConstructor> type;
  std::string name;
  ScopedBlock body;

  std::shared_ptr<Symbol> resolved_symbol;

  std::shared_ptr<NamedFunctionType> getFormalBoundFunctionType();

  FunctionDecl(const SourceLocation &loc, std::shared_ptr<TypeConstructor> type,
               const std::string &name, ScopedBlock body)
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

// ImplHeader is just the impl type and generic parameters.
// body is part of ImplStatement
class ImplHeader {
public:
  FormalTypeParameterList type_params;
  std::shared_ptr<Type> type;
  // the header's declaration location in the ir
  ir::Instruction *ir_decl;

  // the impl's scope table
  ScopeTable<Symbol> *scope_table;

  ImplHeader(FormalTypeParameterList type_params, std::shared_ptr<Type> type,
             ScopeTable<Symbol> *scope_table);
};

class ImplStatement : public Statement {
public:
  std::shared_ptr<ImplHeader> header;
  StatementList body;

  // scope table mapping type params names -> alias decls
  std::unique_ptr<ScopeTable<TypeAlias>> type_scope;

  // scope table for body fn decl's
  // this table has no parent -- it's impl is linked to header
  // this table is owned by ScopesRoot.impls
  ScopeTable<Symbol> *fn_scope;

  ImplStatement(const SourceLocation &loc, std::shared_ptr<ImplHeader> header,
                ScopeTable<Symbol> *fn_scope, StatementList body);
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
  bool is_root_scope;   // if the identifier began with ::
  TypeList type_params; // type params passed in :<> operator
  /* -- resolved symbol info -- */
  std::shared_ptr<Symbol> resolved_symbol;

  Identifier(const SourceLocation &loc, const std::string &id,
             std::vector<std::string> scope, bool is_root_scope,
             TypeList type_params)
      : Expression(loc), scope(std::move(scope)), id(id),
        is_root_scope(is_root_scope), type_params(std::move(type_params)),
        resolved_symbol(){};
};

class ImplSelect : public Expression {
public:
  std::shared_ptr<Type> type;
  std::string method;
  TypeList type_params;

  ImplSelect(const SourceLocation &loc, std::shared_ptr<Type> type,
             const std::string &method, TypeList type_params)
      : Expression(loc), type(std::move(type)), method(method),
        type_params(std::move(type_params)){};
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
  RIGHT_SHIFT,
  UNSAFE_PTR_ADD,
  UNSAFE_PTR_CAST
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

  TypeList type_params; // type params passed in :<> operator -- only applicable
                        // for method calls

  FieldAccess(const SourceLocation &loc, std::unique_ptr<Expression> lvalue,
              const std::string &field, TypeList type_params)
      : Expression(loc), lvalue(std::move(lvalue)), field(field), field_num(0),
        has_field_num(false), type_params(std::move(type_params)){};

  FieldAccess(const SourceLocation &loc, std::unique_ptr<Expression> lvalue,
              int32_t field_num, TypeList type_params)
      : Expression(loc), lvalue(std::move(lvalue)), field(""),
        field_num(field_num), has_field_num(true),
        type_params(std::move(type_params)){};
};

class Sizeof : public Expression {
public:
  std::shared_ptr<Type> sizeof_type;

  Sizeof(const SourceLocation &loc, std::shared_ptr<Type> sizeof_type)
      : Expression(loc), sizeof_type(std::move(sizeof_type)){};
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

  Tuple(const SourceLocation &loc, ExpressionList expressions)
      : Expression(loc), expressions(std::move(expressions)){};
};

class StructExpr : public Expression {
public:
  // UnresolvedType before resolve pass, StructType afterwards
  std::shared_ptr<ast::Type> type;

  std::vector<std::string> field_names;
  ExpressionList field_exprs;

  StructExpr(const SourceLocation &loc,
             std::shared_ptr<ast::UnresolvedType> type_expr,
             std::vector<std::string> field_names, ExpressionList field_exprs)
      : Expression(loc), type(std::move(type_expr)),
        field_names(std::move(field_names)),
        field_exprs(std::move(field_exprs)){};
};

} // namespace ovid::ast

// hashing implementation for types
namespace std {
template <> struct hash<ovid::ast::Type> {
  std::size_t operator()(const ovid::ast::Type &type) { return type.hash(); }
};
} // namespace std

#endif