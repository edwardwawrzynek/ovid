#ifndef H_TYPE_CHECK_INCL
#define H_TYPE_CHECK_INCL

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

  int visitResolvedAlias(ResolvedAlias &type,
                         const TypePrinterState &state) override;

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

public:
  void clear();
  std::string getRes();

  std::string getType(Type &type);

  explicit TypePrinter() : BaseTypeVisitor(0){};
};

/* The result of a type checker visit to a node
 * The type of the node and the ir it generated*/
class TypeCheckResult {
public:
  // type of the visited node
  std::shared_ptr<Type> resultType;
  // instruction corresponding to the result of the expression (null for
  // statements)
  const ir::Expression *resultInstruction;

  TypeCheckResult(std::shared_ptr<Type> resultType,
                  const ir::Expression *resultInstruction)
      : resultType(std::move(resultType)),
        resultInstruction(resultInstruction){};
};

class TypeCheckState {
public:
  /* a type hint for the currently visited node -- what type is expected
   * primarily used to resolve ambiguity during type inference
   * if no particular type is expected, typeHint is null */
  Type *typeHint;

  /* the current instruction list to be inserted into */
  ir::InstructionList &curInstructionList;

  explicit TypeCheckState(ir::InstructionList &curInstructionList)
      : typeHint(nullptr), curInstructionList(curInstructionList){};

  TypeCheckState(ir::InstructionList &curInstructionList, Type *typeHint)
      : typeHint(typeHint), curInstructionList(curInstructionList){};

  // return the state will a null typeHint
  TypeCheckState withoutTypeHint() const;
  // return the state with a typeHint set
  TypeCheckState withTypeHint(Type *hint) const;

  // return the state with a new instruction list
  TypeCheckState
  withNewInstructionList(ir::InstructionList &instructionList) const;
};

/*
 * The type checking pass
 * Each visit method returns the type of that node + the ir generated for it
 */
class TypeCheck : public BaseASTVisitor<TypeCheckResult, TypeCheckState> {
  ErrorManager &errorMan;
  std::vector<std::string> currentModule;

  // apply a MutType wrapper around a type if is_mut
  std::shared_ptr<Type> addMutType(const std::shared_ptr<Type> &type,
                                   bool is_mut);

  TypeCheckResult visitVarDecl(VarDecl &node,
                               const TypeCheckState &state) override;
  TypeCheckResult visitFunctionDecl(FunctionDecl &node,
                                    const TypeCheckState &state) override;
  TypeCheckResult visitModuleDecl(ModuleDecl &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitIfStatement(IfStatement &node,
                                   const TypeCheckState &state) override;

  /*TypeCheckResult visitFunctionCall(FunctionCall &node,
                                     const TypeCheckState &state) override;*/
  TypeCheckResult visitIdentifier(Identifier &node,
                                  const TypeCheckState &state) override;
  /*TypeCheckResult visitOperatorSymbol(OperatorSymbol &node,
                                      const TypeCheckState &state) override;*/
  TypeCheckResult visitAssignment(Assignment &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitIntLiteral(IntLiteral &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitBoolLiteral(BoolLiteral &node,
                                   const TypeCheckState &state) override;
  /*TypeCheckResult visitTuple(Tuple &node, const TypeCheckState &state)
  override; TypeCheckResult visitTypeAliasDecl(TypeAliasDecl &node, const
  TypeCheckState &state) override;*/

public:
  TypeCheck(ErrorManager &errorMan, const std::vector<std::string> package)
      : BaseASTVisitor(
            TypeCheckResult(std::make_shared<ast::VoidType>(
                                SourceLocation("", 0, 0, 0, 0, nullptr)),
                            nullptr)),
        errorMan(errorMan), currentModule(package){};

  /* visitNodes wrapper that produces one InstructionList */
  ir::InstructionList produceIR(const StatementList &ast);
};

/* type check type equality pass */

} // namespace ovid::ast

#endif