#ifndef H_TYPE_CHECK_INCL
#define H_TYPE_CHECK_INCL

#include <utility>

#include "ast_visitor.hpp"
#include "ir.hpp"

/* Ovid Type Checker / Semantic Analysis
 * The type checker visits the AST, performs type inference and checking, and
 * lowers the AST into the Ovid IR */

namespace ovid::ast {
/* the type pretty printer for the type checking pass */
class TypePrinterState {};
class TypePrinter : public BaseTypeVisitor<int, TypePrinterState> {

  std::string res;

  int visitVoidType(VoidType &type, const TypePrinterState &state) override;
  int visitBoolType(BoolType &type, const TypePrinterState &state) override;
  int visitIntType(IntType &type, const TypePrinterState &state) override;
  int visitFloatType(FloatType &type, const TypePrinterState &state) override;

  int visitMutType(MutType &type, const TypePrinterState &state) override;
  int visitPointerType(PointerType &type,
                       const TypePrinterState &state) override;

  int visitFunctionType(FunctionType &type,
                        const TypePrinterState &state) override;
  int visitNamedFunctionType(NamedFunctionType &type,
                             const TypePrinterState &state) override;

  int visitTupleType(TupleType &type, const TypePrinterState &state) override;
  int visitStructType(StructType &type, const TypePrinterState &state) override;
  int visitUnresolvedType(UnresolvedType &type,
                          const TypePrinterState &state) override;
  int visitFormalTypeParameter(FormalTypeParameter &type,
                               const TypePrinterState &state) override;

  int visitGenericTypeList(TypeList &type_params,
                           const TypePrinterState &state);
  int visitFormalTypeParameterList(FormalTypeParameterList &types,
                                   const TypePrinterState &state);

public:
  void clear();
  std::string getRes();

  std::string getType(Type &type);
  std::string getGenericTypeList(TypeList &type_params);
  std::string getFormalTypeParameterList(FormalTypeParameterList &type_params);

  explicit TypePrinter() : BaseTypeVisitor(0){};
};

/* Check if type matches pattern. Pattern is a type bound over the formal
 * parameters formal_params. The formal parameters in pattern are allowed to
 * match any type in type. If type matches pattern, the function returns a list
 * of formal param -> concrete type substitutions that create the match. For
 * example, given the pattern: (i32, *mut T) bound on formal parameter T, the
 * type: (i32, *mut Vec<u64>) matches with the substitution T -> Vec<u64>.
 *
 * Returns std::pair(true, substitutions) if the pattern matches,
 * std::pair(false, _) otherwise. */
std::pair<bool, ConstTypeList>
checkTypePattern(const Type &type, const Type &pattern,
                 const FormalTypeParameterList &formal_params);

/* The result of a type checker visit to a node
 * The type of the node and the ir it generated*/
class TypeCheckResult {
public:
  // type of the visited node
  std::shared_ptr<Type> resType;
  // instruction corresponding to the result of the expression (null for
  // statements)
  ir::Expression *resExpr;

  // if the result is generic or not
  bool is_generic;
  std::shared_ptr<TypeConstructor> genericResType;
  ir::GenericExpression *genericResExpr;

  TypeCheckResult(std::shared_ptr<Type> resType, ir::Expression *resExpr)
      : resType(std::move(resType)), resExpr(resExpr), is_generic(false),
        genericResType(nullptr), genericResExpr(nullptr){};

  TypeCheckResult(std::shared_ptr<TypeConstructor> genericResType,
                  ir::GenericExpression *genericResExpr)
      : resType(nullptr), resExpr(nullptr), is_generic(true),
        genericResType(std::move(genericResType)),
        genericResExpr(genericResExpr){};

  // check if the result does not contain an expression (null or generic)
  bool isNull() const;
  static TypeCheckResult nullResult();
};

class TypeCheckState {
public:
  /* a type hint for the currently visited node -- what type is expected
   * primarily used to resolve ambiguity during type inference
   * if no particular type is expected, typeHint is null */
  std::shared_ptr<Type> typeHint;
  /* expected function return type */
  std::shared_ptr<Type> functionReturnType;

  TypeCheckState() : typeHint(nullptr), functionReturnType(nullptr){};

  explicit TypeCheckState(std::shared_ptr<Type> typeHint,
                          std::shared_ptr<Type> functionReturnType)
      : typeHint(std::move(typeHint)),
        functionReturnType(std::move(functionReturnType)){};

  // return the state will a null typeHint
  TypeCheckState withoutTypeHint() const;
  // return the state with a typeHint set
  TypeCheckState withTypeHint(const std::shared_ptr<Type> &typeHint) const;

  // return the state with a function ret type set
  TypeCheckState withFunctionReturnType(std::shared_ptr<Type> returnType) const;
  TypeCheckState withoutFunctionReturnType() const;
};

/*
 * The type checking pass
 * Each visit method returns the type of that node + the ir generated for it
 */
class TypeCheck : public BaseASTVisitor<TypeCheckResult, TypeCheckState> {
  ErrorManager &errorMan;
  std::vector<std::string> currentModule;
  const std::vector<std::string> &package;
  const ScopesRoot &root_scopes;
  ActiveScopes &active_scopes;
  TypePrinter type_printer;

  /* the current instruction list to be inserted into */
  ir::InstructionList *curInstructionList;

  /* the current function basic block context
   * this should only be inserted into to create new basic blocks,
   * curInstructionList should be used otherwise */
  ir::BasicBlockList *curBasicBlockList;

  // apply a MutType wrapper around a type if is_mut
  static std::shared_ptr<Type> addMutType(const std::shared_ptr<Type> &type,
                                          bool is_mut);
  // remove a MutType wrapper, if present
  static std::shared_ptr<Type>
  withoutMutType(const std::shared_ptr<Type> &type);

  // extract a FunctionType from either NamedFunctionType of Function Type
  static std::shared_ptr<FunctionType>
  functionTypeFromType(const std::shared_ptr<Type> &type);

  // create a new basic block and start inserting in it
  ir::BasicBlock &newBasicBlock(const SourceLocation &loc);
  // insert a basic block to be the current insert point
  void insertBasicBlock(std::unique_ptr<ir::BasicBlock> block);

  // apply actual_params to a type constructor
  std::shared_ptr<Type>
  constructType(const std::shared_ptr<TypeConstructor> &type_construct,
                const TypeList &actual_params);

  TypeCheckResult visitVarDecl(VarDecl &node,
                               const TypeCheckState &state) override;
  TypeCheckResult visitFunctionDecl(FunctionDecl &node,
                                    const TypeCheckState &state) override;
  TypeCheckResult visitModuleDecl(ModuleDecl &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitIfStatement(IfStatement &node,
                                   const TypeCheckState &state) override;
  TypeCheckResult visitWhileStatement(WhileStatement &node,
                                      const TypeCheckState &state) override;
  TypeCheckResult visitReturnStatement(ReturnStatement &node,
                                       const TypeCheckState &state) override;

  TypeCheckResult visitFunctionCall(FunctionCall &node,
                                    const TypeCheckState &state) override;
  TypeCheckResult visitIdentifier(Identifier &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitOperatorSymbol(OperatorSymbol &node,
                                      const TypeCheckState &state) override;
  TypeCheckResult visitAssignment(Assignment &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitIntLiteral(IntLiteral &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitBoolLiteral(BoolLiteral &node,
                                   const TypeCheckState &state) override;
  TypeCheckResult visitFloatLiteral(FloatLiteral &node,
                                    const TypeCheckState &state) override;
  TypeCheckResult visitCharLiteral(CharLiteral &node,
                                   const TypeCheckState &state) override;
  TypeCheckResult visitTuple(Tuple &node, const TypeCheckState &state) override;
  TypeCheckResult visitStructExpr(StructExpr &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitSizeof(Sizeof &node,
                              const TypeCheckState &state) override;

  TypeCheckResult visitFieldAccess(FieldAccess &node,
                                   const TypeCheckState &state) override;

  TypeCheckResult visitFunctionCallOperator(const FunctionCall &node,
                                            const TypeCheckState &state);
  TypeCheckResult visitFunctionCallOperator(const FunctionCall &node,
                                            const TypeCheckState &state,
                                            TypeCheckResult *leftRes);
  TypeCheckResult visitFunctionCallDeref(const FunctionCall &node,
                                         const TypeCheckState &state);
  TypeCheckResult visitFunctionCallAddress(const FunctionCall &node,
                                           const TypeCheckState &state);
  TypeCheckResult visitShortCircuitingCall(const FunctionCall &node,
                                           const TypeCheckState &state);
  TypeCheckResult visitCompoundAssignmentCall(const FunctionCall &node,
                                              const TypeCheckState &state);
  TypeCheckResult visitUnsafePtrCastCall(const FunctionCall &node,
                                         const TypeCheckState &state);
  TypeCheckResult visitUnsafePtrAddCall(const FunctionCall &node,
                                        const TypeCheckState &state);

  TypeCheckResult doImplicitConversion(const TypeCheckResult &expression,
                                       const TypeCheckState &state,
                                       const SourceLocation &loc);

public:
  TypeCheck(ErrorManager &errorMan, const std::vector<std::string> &package,
            const ScopesRoot &root_scopes, ActiveScopes &active_scopes,
            ir::InstructionList *ir, ir::BasicBlockList *basicBlockList)
      : BaseASTVisitor(TypeCheckResult::nullResult()), errorMan(errorMan),
        currentModule(package), package(package), root_scopes(root_scopes),
        active_scopes(active_scopes), type_printer(), curInstructionList(ir),
        curBasicBlockList(basicBlockList){};

  static ir::InstructionList produceIR(ErrorManager &errorMan,
                                       const std::vector<std::string> &package,
                                       const ScopesRoot &root_scopes,
                                       ActiveScopes &active_scopes,
                                       const StatementList &ast);
};

} // namespace ovid::ast

#endif