#ifndef H_TYPE_CHECK_INCL
#define H_TYPE_CHECK_INCL

#include "ast_visitor.hpp"
#include "ir.hpp"

/* Ovid Type Checker / Semantic Analysis
 * The type checker visits the AST, performs type inference and checking, and
 * lowers the AST into the Ovid IR */

namespace ovid::ast {
/* The result of a type checker visit to a node
 * The type of the node and the ir it generated*/
class TypeCheckResult {
public:
  // type of the visited node
  std::shared_ptr<Type> resultType;
  // ir instructions for the node
  ir::InstructionList instructions;
  // instruction corresponding to the result of the expression (null for
  // statements)
  const ir::Expression *resultInstruction;

  TypeCheckResult(std::shared_ptr<Type> resultType,
                  ir::InstructionList instructions,
                  const ir::Expression *resultInstruction)
      : resultType(std::move(resultType)),
        instructions(std::move(instructions)),
        resultInstruction(resultInstruction){};
};

class TypeCheckState {
public:
  /* a type hint for the currently visited node -- what type is expected
   * primarily used to resolve ambiguity during type inference
   * if no particular type is expected, typeHint is null */
  Type *typeHint;

  // used to put source name info on nodes
  std::vector<std::string> current_module;

  TypeCheckState(const std::vector<std::string> &package)
      : typeHint(nullptr), current_module(package){};

  TypeCheckState(const std::vector<std::string> &package, Type *typeHint)
      : typeHint(typeHint), current_module(package){};

  // return the state will a null typeHint
  TypeCheckState withoutTypeHint() const;
  // return the state with a typeHint set
  TypeCheckState withTypeHint(Type *hint) const;

  // return the state with modules added
  TypeCheckState withModuleNames(const std::vector<std::string> &scopes);
  // return the state with the given modules popped
  TypeCheckState withoutModuleNames(const std::vector<std::string> &scopes);
};

/*
 * The type checking pass
 * Each visit method returns the type of that node + the ir generated for it
 */
class TypeCheck : public BaseASTVisitor<TypeCheckResult, TypeCheckState> {
  ErrorManager &errorMan;

  // apply a MutType wrapper around a type if is_mut
  std::shared_ptr<Type> addMutType(const std::shared_ptr<Type> &type,
                                   bool is_mut);

  TypeCheckResult visitVarDecl(VarDecl &node,
                               const TypeCheckState &state) override;
  /*TypeCheckResult visitFunctionDecl(FunctionDecl &node,
                                    const TypeCheckState &state) override;*/
  TypeCheckResult visitModuleDecl(ModuleDecl &node,
                                  const TypeCheckState &state) override;
  /* TypeCheckResult visitIfStatement(IfStatement &node,
                                    const TypeCheckState &state) override;

   TypeCheckResult visitFunctionCall(FunctionCall &node,
                                     const TypeCheckState &state) override;*/
  TypeCheckResult visitIdentifier(Identifier &node,
                                  const TypeCheckState &state) override;
  /*TypeCheckResult visitOperatorSymbol(OperatorSymbol &node,
                                      const TypeCheckState &state) override;*/
  TypeCheckResult visitAssignment(Assignment &node,
                                  const TypeCheckState &state) override;
  TypeCheckResult visitIntLiteral(IntLiteral &node,
                                  const TypeCheckState &state) override;
  /*TypeCheckResult visitBoolLiteral(BoolLiteral &node,
                                   const TypeCheckState &state) override;
  TypeCheckResult visitTuple(Tuple &node, const TypeCheckState &state) override;
  TypeCheckResult visitTypeAliasDecl(TypeAliasDecl &node,
                                     const TypeCheckState &state) override;*/

public:
  TypeCheck(ErrorManager &errorMan)
      : BaseASTVisitor(
            TypeCheckResult(std::make_shared<ast::VoidType>(
                                SourceLocation("", 0, 0, 0, 0, nullptr)),
                            ir::InstructionList(), nullptr)),
        errorMan(errorMan){};

  /* visitNodes wrapper that produces one InstructionList */
  ir::InstructionList produceIR(const StatementList &ast,
                                const TypeCheckState &state);
};
} // namespace ovid::ast

#endif