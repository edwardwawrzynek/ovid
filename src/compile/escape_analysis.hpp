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

enum class EscapeType {
  NONE,
  RETURN,
  OTHER
};

/* A Value object from the ir, with information about how it flows
 * This contains:
 *  A reference to the Value flowing
 *  An indirection level -- what is flowing (the value, dereference of the
 * value, etc) A field number (for ProductType's) - if the value is a product
 * type, only one field of the type may be flowing
 */
class FlowValue {
public:
  const ir::Expression &expr;
  uint32_t indirect_level; // 0 if the value is flowing, 1 if it's dereference
                           // is, etc
  int32_t field; // -1 if value doesn't have fields or all fields are flowing,
                 // id of field flowing otherwise

  // if the value is an escape (global or return)
  EscapeType is_escape;

  FlowValue(const ir::Expression &value, uint32_t indirect_level);

  FlowValue(const ir::Expression &value, uint32_t indirect_level,
            int32_t field);
  FlowValue(const ir::Expression &value, uint32_t indirect_level, int32_t field,
            EscapeType is_escape);

  /* check if the flow value actually contains anything
   * eg an indirection on a type without pointers (eg i32) is empty */
  bool isEmpty() const;

  void print(std::ostream &output) const;

  /* get the type of the value with indirections + field applied
   * returns an array because indirection on product type may produce multiple
   * types */
  std::vector<const ast::Type *> getFlowingTypes() const;

private:
  // get the type of the value without indirections, with the field selector
  // applied
  const ast::Type *getTypeAfterField() const;
};

// replacement for FlowValue
class AliasValue {
public:
  /* expression flowing */
  Expression &expr;
  /* number of dereferences on the expression */
  uint32_t indirect_level;
  /* field selections for each indirection
   * if field is -1, the whole value is used
   * [0] is the field select on the value, [1] is the field select on the dereference of the value, etc
   * length should be equivalent to indirect_level + 1 */
  std::vector<int32_t> field_selects;

  /* if the value should be considered to be an escaping route
   * if RETURN, expr is not meaningful */
  EscapeType is_escape;

  /* get all the types that could be flowing through the value
   * if a product type is dereferenced without a field select, multiple types may be flowing */
  std::vector<const ast::Type *> getFlowingTypes();

  /* check if anything is actually flowing through the value */
  bool isEmpty();

  AliasValue(Expression& expr, uint32_t indirect_level, const std::vector<int32_t> field_selects);
};

// replacement for Flow
class AliasFlow {
  AliasValue from;
  AliasValue into;

  // if anything is actually flowing
  bool isEmpty();
};

/* given an ir::Expression, produce a flow value of the expression including field selects, dereferences, and addresses
 *
 * this returns the flow value without the dereference expected if a copy is involved (such as in assignment). Because of this, the indirect_level may be -1 (eg -- an address was taken) */
AliasValue getFlowFromExpression(const Expression& expr);

// given a product type, produce an array of all of its types (including nested
// product types)
std::vector<const ast::Type *> flattenProductType(const ast::ProductType *type);

// get the potential type of a derefrencing the given type
// this returns a vector, because indirection on a product type may produce
// multiple types
std::vector<const ast::Type *> getIndirectedTypes(uint32_t indirect_level,
                                                  const ast::Type *type);

// given an Expression, get the allocation that owns it (if any)
// field selects on an allocation are owned by that allocation
Allocation *getOwningAllocation(Expression *expr);

/* a flow of one FlowValue to another */
class Flow {
public:
  FlowValue from;
  FlowValue into;

  Flow(const FlowValue &value, const FlowValue &into);

  /* check if anything is actually flowing
   * if value is empty, so is the flow */
  bool isEmpty();

  /* return a copy of the flow with the given number of indirections added */
  Flow withAddedIndirection(uint32_t indirections) const;

  void print(std::ostream &output);
};

typedef std::vector<Flow> FlowList;

/* given an ir expression, find all of it's uses as the source of flows */
std::vector<FlowValue> findExpressionFlows(const FlowList &flows,
                                           const ir::Expression &expr);

/* given a flowing value and a list of flows, get the full set of where the
 * value flows each result flow value is added to collectedFlows
 * tmpFlows is an set used for scratch. collectedFlows may point to elements of
 * it */
void traceFlow(const FlowList &flows, const FlowValue &flowValue,
               std::vector<FlowValue> &collectedFlows);

class EscapeAnalysisState {
public:
  /* if inside a function declaration or not */
  bool in_func;
  /* current flow list to add to */
  FlowList *curFlowList;

  EscapeAnalysisState(bool in_func, FlowList *curFlowList)
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
  int visitDereference(Dereference &instruct,
                       const EscapeAnalysisState &state) override;
  int visitBuiltinCast(BuiltinCast &instruct,
                       const EscapeAnalysisState &state) override;
  int visitFieldSelect(FieldSelect &instruct,
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