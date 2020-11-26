#ifndef H_GENERICS_PASS_INCL
#define H_GENERICS_PASS_INCL

#include "ast.hpp"
#include "generics.hpp"
#include "ir_visitor.hpp"

namespace ovid::ir {
/* the generics pass visits the ir and replaces Specialize nodes with specific
 * function implementations -- monomorphization of generic functions */

class GenericSubstitutions {
  GenericSubstitutions *parent;
  std::unordered_map<uint64_t, BasicBlock *> bbSubs;
  std::unordered_map<uint64_t, Expression *> exprSubs;
  std::unordered_map<uint64_t, GenericExpression *> genericExprSubs;

public:
  void addBasicBlock(uint64_t old_id, BasicBlock *newBB);
  void addExpression(uint64_t old_id, Expression *newExpr);
  void addExpression(Expression &old_expr, Expression *newExpr);
  void addGenericExpression(uint64_t old_id, GenericExpression *newGenericExpr);
  void addGenericExpression(GenericExpression &old_expr,
                            GenericExpression *newGenericExpr);
  BasicBlock *useBasicBlock(const BasicBlock *old);
  Expression *useExpression(Expression *old);
  Expression *hasExpression(Expression *old);
  GenericExpression *useGenericExpression(GenericExpression *old);

  explicit GenericSubstitutions(GenericSubstitutions *parent = nullptr)
      : parent(parent), bbSubs(), exprSubs(), genericExprSubs(){};
};

class GenericsPassState {
public:
  // if the pass is currently specializing a function (otherwise just looking
  // for needed specializations)
  bool is_specializing;
  // formal parameters on the function that are being replaced
  const ast::FormalTypeParameterList &formal_params;
  // type parameters on which to specialize the function
  const ast::TypeList &actual_params;
  // current BasicBlockList to add new BasicBlock's to
  BasicBlockList *curBasicBlockList;
  // current instruction list to add new instructions to
  InstructionList *curInstructionList;
  // substitutions to perform on expressions
  GenericSubstitutions &subs;

  GenericsPassState(bool is_specializing,
                    const ast::FormalTypeParameterList &formal_params,
                    const ast::TypeList &actual_params,
                    GenericSubstitutions &subs,
                    BasicBlockList *curBasicBlockList = nullptr,
                    InstructionList *curInstructionList = nullptr);
};

class GenericSpecializations {
  // map of GenericFunctionDeclare id -> [actual_type_params,
  // specialized_function]
  std::unordered_map<uint64_t,
                     std::vector<std::pair<ast::TypeList, FunctionDeclare *>>>
      specializations;

public:
  // check if the pass has a specialization for the given Specialize instruction
  // return the specialization if present, nullptr otherwise
  FunctionDeclare *getSpecialization(GenericExpression &generic_expr,
                                     const ast::TypeList &actual_params);
  // insert a specialization
  void addSpecialization(uint64_t generic_function_id,
                         ast::TypeList actual_params,
                         FunctionDeclare *specialized_function);

  GenericSpecializations() : specializations(){};
};

class GenericsPass : public BaseIRVisitor<int, GenericsPassState> {
  GenericSpecializations specializations;
  ast::TypeConstructorPass type_constructor;
  // instruction list in which to insert specialized functions
  InstructionList *rootInstructionList;
  // global allocation + func declare substitutions
  GenericSubstitutions global_subs;

  int visitGenericFunctionDeclare(GenericFunctionDeclare &instruct,
                                  const GenericsPassState &state) override;
  int visitGenericForwardIdentifier(GenericForwardIdentifier &instruct,
                                    const GenericsPassState &state) override;

  int visitFunctionDeclare(FunctionDeclare &instruct,
                           const GenericsPassState &state) override;
  int visitIntLiteral(IntLiteral &instruct,
                      const GenericsPassState &state) override;
  int visitBoolLiteral(BoolLiteral &instruct,
                       const GenericsPassState &state) override;
  int visitFloatLiteral(FloatLiteral &instruct,
                        const GenericsPassState &state) override;
  int visitTupleLiteral(TupleLiteral &instruct,
                        const GenericsPassState &state) override;
  int visitFunctionCall(FunctionCall &instruct,
                        const GenericsPassState &state) override;
  int visitAllocation(Allocation &instruct,
                      const GenericsPassState &state) override;
  int visitGlobalAllocation(GlobalAllocation &instruct,
                            const GenericsPassState &state) override;
  int visitAddress(Address &instruct, const GenericsPassState &state) override;
  int visitDereference(Dereference &instruct,
                       const GenericsPassState &state) override;
  int visitBuiltinOperator(BuiltinOperator &instruct,
                           const GenericsPassState &state) override;
  int visitBuiltinCast(BuiltinCast &instruct,
                       const GenericsPassState &state) override;
  int visitFieldSelect(FieldSelect &instruct,
                       const GenericsPassState &state) override;
  int visitForwardIdentifier(ForwardIdentifier &instruct,
                             const GenericsPassState &state) override;
  int visitSpecialize(Specialize &instruct,
                      const GenericsPassState &state) override;
  int visitSizeof(Sizeof &instruct, const GenericsPassState &state) override;

  int visitStore(Store &instruct, const GenericsPassState &state) override;
  int visitBasicBlock(BasicBlock &instruct,
                      const GenericsPassState &state) override;

  int visitReturn(Return &instruct, const GenericsPassState &state) override;
  int visitJump(Jump &instruct, const GenericsPassState &state) override;
  int visitConditionalJump(ConditionalJump &instruct,
                           const GenericsPassState &state) override;

  int visitBasicBlockList(
      BasicBlockList &basicBlockList, const GenericsPassState &state,
      const std::vector<std::reference_wrapper<Allocation>> &oldArgAllocs,
      std::vector<std::reference_wrapper<Allocation>> &newArgAllocs);

  std::shared_ptr<ast::Type> fixType(const std::shared_ptr<ast::Type> &type,
                                     const GenericsPassState &state);

  Value newValue(const Value &old_val, const GenericsPassState &state);
  int addExpr(std::unique_ptr<Expression> expr, Expression &old,
              const GenericsPassState &state);

public:
  GenericsPass(ActiveScopes &scopes, ErrorManager &errorMan,
               InstructionList *rootInstructionList)
      : BaseIRVisitor(int()), specializations(),
        type_constructor(scopes, errorMan, std::vector<std::string>(),
                         std::vector<std::string>()),
        rootInstructionList(rootInstructionList){};

  static InstructionList produceIR(ActiveScopes &scopes, ErrorManager &errorMan,
                                   const InstructionList &ir);
};

} // namespace ovid::ir
#endif