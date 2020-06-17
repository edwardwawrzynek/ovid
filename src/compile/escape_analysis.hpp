#ifndef H_ESCAPE_ANALYSIS_INCL
#define H_ESCAPE_ANALYSIS_INCL

#include "ir.hpp"
#include "ir_visitor.hpp"

/**
 * Escape analysis traces the flow of pointers through the ir, with the ultimate
 * goal of converting heap allocations to stack allocations if they can be shown
 * to be safe (not escape the function they are in).
 *
 * NOTES:
 * A flows to B means that A and B *may* be the same allocation (that &A = &B)
 *
 * If A flows to B and B escapes, than A also escapes
 *
 * An expression like:
 *  `val v = c`
 * does not mean that c flows to v (an implicit copy is taking place). It means
 * that the dereference of all pointers contained in c flow to the dereference
 * of all pointers contained in v
 *
 * with flow objects, this would be Flow(FlowValue(c, 1), FlowValue(v, 1))
 */

namespace ovid::ir {

enum class EscapeType { NONE, RETURN, OTHER };

class FlowValue {
public:
  /* expression flowing */
  Expression &expr;
  /* number of dereferences on the expression */
  int32_t indirect_level;
  /* field selections for each indirection
   * [0] is the field select on the value, [1] is the field select on the first
   * dereference of the value, etc
   * length should be equivalent to indirect_level + 1 */
  std::vector<std::vector<int32_t>> field_selects;

  /* if the value should be considered to be an escaping route
   * if RETURN, expr is not meaningful */
  EscapeType is_escape;

  /* get all the types that could be flowing through the value
   * if a product type is dereferenced without a field select, multiple types
   * may be flowing */
  std::vector<const ast::Type *> getFlowingTypes();

  /* check if anything is actually flowing through the value */
  bool isEmpty();

  void print(std::ostream &output);

  /* wrapper around getFlowFromExpressionWithoutCopy that adds the indirection
   * expected in a copy */
  static FlowValue getFlowFromExpression(Expression &expr);

  FlowValue(Expression &expr, int32_t indirect_level,
            const std::vector<std::vector<int32_t>> &field_selects,
            EscapeType is_escape);

  FlowValue(Expression &expr, int32_t indirect_level,
            const std::vector<std::vector<int32_t>> &field_selects);

private:
  /* add any types that may flow a type and indirections and field selects
   *applied to it
   **/
  static std::vector<const ast::Type *>
  getFlowingTypesFromType(const ast::Type *exprType, int32_t indirect_level,
                          std::vector<std::vector<int32_t>> &field_selects);

  static const ast::Type *
  applyFieldSelectToType(const ast::Type *type,
                         const std::vector<int32_t> &field_select);

  /* check if an expression is a global escape */
  static EscapeType isGlobalEscape(Expression &expr);

  /* given an ir::Expression, produce a flow value of the expression including
   * field selects, dereferences, and addresses
   *
   * this returns the flow value with the dereference expected if a copy is
   * involved (such as in assignment). Because of this, the indirect_level may
   * be -1 (eg -- an address was taken) */
  static FlowValue getFlowFromExpressionWithoutCopy(Expression &expr);
};

/* flow from one value to another */
class Flow {
public:
  FlowValue from;
  FlowValue into;

  // if anything is actually flowing
  bool isEmpty();

  void print(std::ostream &output);

  Flow(const FlowValue &from, const FlowValue &into);
};

typedef std::vector<Flow> AliasFlowList;

// given a product type, produce an array of all of its types (including nested
// product types)
std::vector<const ast::Type *> flattenProductType(const ast::ProductType *type);

// given an Expression, get the allocation that owns it (if any)
// field selects on an allocation are owned by that allocation
Allocation *getOwningAllocation(Expression *expr);

class EscapeAnalysisState {
public:
  /* if inside a function declaration or not */
  bool in_func;
  /* current flow list to add to */
  AliasFlowList *curFlowList;

  EscapeAnalysisState(bool in_func, AliasFlowList *curFlowList)
      : in_func(in_func), curFlowList(curFlowList){};

  EscapeAnalysisState() : in_func(false), curFlowList(nullptr){};
};

// main escape analysis pass -- visit ir nodes, calculate pointer flow and
// adjust allocation types
class EscapeAnalysisPass : public BaseIRVisitor<int, EscapeAnalysisState> {
  bool print_flows;
  std::ostream &output;

  int visitFunctionDeclare(FunctionDeclare &instruct,
                           const EscapeAnalysisState &state) override;
  int visitIntLiteral(IntLiteral &instruct,
                      const EscapeAnalysisState &state) override;
  int visitBoolLiteral(BoolLiteral &instruct,
                       const EscapeAnalysisState &state) override;
  int visitTupleLiteral(TupleLiteral &instruct,
                        const EscapeAnalysisState &state) override;
  int visitFunctionCall(FunctionCall &instruct,
                        const EscapeAnalysisState &state) override;
  int visitAllocation(Allocation &instruct,
                      const EscapeAnalysisState &state) override;
  int visitAddress(Address &instruct,
                   const EscapeAnalysisState &state) override;
  int visitBuiltinCast(BuiltinCast &instruct,
                       const EscapeAnalysisState &state) override;
  int visitReturn(Return &instruct, const EscapeAnalysisState &state) override;

  int visitStore(Store &instruct, const EscapeAnalysisState &state) override;
  int visitBasicBlock(BasicBlock &instruct,
                      const EscapeAnalysisState &state) override;

public:
  EscapeAnalysisPass(bool print_flows, std::ostream &output)
      : BaseIRVisitor(0), print_flows(print_flows), output(output){};
};

// run escape analysis on ir (thin wrapper around EscapeAnalysisPass)
void runEscapeAnalysis(const ir::InstructionList &ir, bool print_flows,
                       std::ostream &output);

} // namespace ovid::ir

#endif