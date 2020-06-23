#ifndef H_ESCAPE_ANALYSIS_INCL
#define H_ESCAPE_ANALYSIS_INCL

#include "ir.hpp"
#include "ir_visitor.hpp"

/**
 * Escape analysis traces the flow of pointers through the ir, with the ultimate
 * goal of converting heap allocations to stack allocations if they can be shown
 * to be safe (not escape the function they are in).
 *
 * The Flow and FlowValue classes used by escape analysis are declared in ir.hpp
 */

namespace ovid::ir {

/* given a value and a list of flows, find all values to which the value may
 * flow values are passed to func
 */
void traceFlow(const FlowValue &value, const FlowList &flows,
               const std::function<void(const FlowValue &)> &func,
               std::vector<FlowValue> &visited);

void traceFlow(const FlowValue &value, const FlowList &flows,
               const std::function<void(const FlowValue &)> &func);

/* same as traceFlow, but if a flow matching a more specialized form of value is
 * found, that more specified form is passed to specializedFlowFunc (but not
 * followed)
 *
 * initValue is the
 */
void traceFlowFindSpecialized(
    const FlowValue &value, const FlowList &flows,
    const std::function<void(const FlowValue &)> &func,
    const std::function<void(const FlowValue &)> &specializedFlowFunc);

void traceFlowFindSpecialized(
    const FlowValue &value, const FlowValue &initValue, const FlowList &flows,
    const std::function<void(const FlowValue &)> &func,
    const std::function<void(const FlowValue &)> &specializedFlowFunc,
    std::vector<FlowValue> &visitedFlows,
    std::vector<FlowValue> &visitedSpecializations);

// given a product type, produce an array of all of its types (including nested
// product types)
std::vector<const ast::Type *> flattenProductType(const ast::ProductType *type);

// given an Expression, get the allocation that owns it (if any)
// field selects on an allocation are owned by that allocation
Allocation *getOwningAllocation(Expression *expr);

/* given a list of basic blocks, call func on each allocation */
void visitAllocations(const BasicBlockList &blocks,
                      const std::function<void(Allocation &)> &func);

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
  /* print internal function flows */
  bool print_flows;
  /* print externally visible function flows */
  bool print_func_flow_metadata;
  /* print escaping variables */
  bool print_escapes;
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

  static bool
  argsContain(const std::vector<std::reference_wrapper<Allocation>> &funcArgs,
              const Expression &expr);
  void calculateFunctionFlowMetadata(
      ir::FlowList &functionFlows,
      const std::vector<std::reference_wrapper<Allocation>> &funcArgs,
      const ir::FlowList &flows, const FlowValue &srcValue);

public:
  EscapeAnalysisPass(bool print_flows, bool print_escapes,
                     bool print_func_flow_metadata, std::ostream &output)
      : BaseIRVisitor(0), print_flows(print_flows),
        print_func_flow_metadata(print_func_flow_metadata),
        print_escapes(print_escapes), output(output){};
};

// run escape analysis on ir (thin wrapper around EscapeAnalysisPass)
void runEscapeAnalysis(const ir::InstructionList &ir, bool print_flows,
                       bool print_escapes, bool print_func_flow_metadata,
                       std::ostream &output);

} // namespace ovid::ir

#endif