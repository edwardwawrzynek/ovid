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
std::pair<bool, TypeList>
checkTypePattern(Type &type, const Type &pattern,
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

  /* impl block current function is in */
  ir::Instruction *impl_block;

  TypeCheckState()
      : typeHint(nullptr), functionReturnType(nullptr), impl_block(nullptr){};

  explicit TypeCheckState(std::shared_ptr<Type> typeHint,
                          std::shared_ptr<Type> functionReturnType,
                          ir::Instruction *impl_block)
      : typeHint(std::move(typeHint)),
        functionReturnType(std::move(functionReturnType)),
        impl_block(impl_block){};

  // return the state will a null typeHint
  TypeCheckState withoutTypeHint() const;
  // return the state with a typeHint set
  TypeCheckState withTypeHint(const std::shared_ptr<Type> &typeHint) const;

  // return the state with a function ret type set
  TypeCheckState withFunctionReturnType(std::shared_ptr<Type> returnType) const;
  TypeCheckState withoutFunctionReturnType() const;

  // return the state with higher_formal_params set
  TypeCheckState withImplBlock(ir::Instruction *new_impl_block) const;

  // get the impl header associated with impl_block, or nullptr if impl_block is
  // nullptr
  ImplHeader *implHeader() const;

  // get the type params the current impl is bound over, or an empty list if not
  // in an impl block
  const FormalTypeParameterList &implFormalParams() const;
};

typedef std::pair<ScopeTable<Symbol> *, TypeList> ImplSub;
typedef std::vector<ImplSub> ImplSubsList;

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

  // substitute formal_params -> actual_params in a type constructor
  std::shared_ptr<Type> substTypes(const std::shared_ptr<Type> &type,
                                   const FormalTypeParameterList &formal_params,
                                   const TypeList &actual_params);

  // convert a IrDecl into a selection of an ir node
  // a sequence of instr -> (Specialize? ->
  // (Select|GenericSelect)?)? actual_params are the params to
  // specialize the impl type on (if ir_decl is inside an impl block)
  ir::Instruction *genIrDecl(ir::Instruction *impl,
                             const std::shared_ptr<Symbol> &fn_sym,
                             const SourceLocation &loc,
                             const TypeCheckState &state,
                             const TypeList &actual_params);

  // create an instruction sequence to select the first method in subs
  TypeCheckResult selectImplSub(ImplSub impl, const std::string &method,
                                ast::Type &impl_type,
                                const TypeList &method_actual_type_params,
                                const TypeCheckState &state,
                                const SourceLocation &loc);

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
  TypeCheckResult visitImplStatement(ImplStatement &node,
                                     const TypeCheckState &state) override;

  TypeCheckResult visitFunctionCall(FunctionCall &node,
                                    const TypeCheckState &state) override;
  bool
  checkNumTypeParams(const std::shared_ptr<ast::TypeConstructor> &generic_type,
                     const TypeList &type_params, const SourceLocation &loc);
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
  ImplSubsList findImplsForType(Type &type, const std::string *method);
  bool checkNumberOfImpls(const ImplSubsList &impls, const std::string &method,
                          Type &type, const SourceLocation &loc);
  TypeCheckResult visitImplSelect(ImplSelect &node,
                                  const TypeCheckState &state) override;

  // visit args and add visited results to resArgs
  // compare against fn_type args (starting at args_index_start)
  bool visitFuncCallArgs(
      const ExpressionList &args, FunctionType *fn_type, int args_index_start,
      const TypeCheckState &state,
      std::vector<std::reference_wrapper<ir::Expression>> &resArgs,
      const SourceLocation &loc);
  TypeCheckResult visitFunctionCallRegular(const FunctionCall &node,
                                           const TypeCheckState &state);
  TypeCheckResult visitFunctionCallOperator(const FunctionCall &node,
                                            const TypeCheckState &state);
  TypeCheckResult visitFunctionCallOperator(const FunctionCall &node,
                                            const TypeCheckState &state,
                                            TypeCheckResult *leftRes);
  TypeCheckResult doDeref(TypeCheckResult &expr, const TypeCheckState &state,
                          const SourceLocation &loc);
  TypeCheckResult visitFunctionCallDeref(const FunctionCall &node,
                                         const TypeCheckState &state);
  TypeCheckResult doAddress(TypeCheckResult &expr, const TypeCheckState &state,
                            const SourceLocation &loc);
  TypeCheckResult visitFunctionCallAddress(const FunctionCall &node,
                                           const TypeCheckState &state);
  TypeCheckResult visitFunctionCallFieldAccess(const FunctionCall &node,
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