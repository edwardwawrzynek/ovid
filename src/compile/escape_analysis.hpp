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

/* A Value object from the ir, with information about how it flows
 * This contains:
 *  A reference to the Value flowing
 *  An indirection level -- what is flowing (the value, dereference of the
 * value, etc) A field number (for ProductType's) - if the value is a product
 * type, only one field of the type may be flowing
 */
class FlowValue {
public:
  const ir::Expression &value;
  uint32_t indirect_level; // 0 if the value is flowing, 1 if it's dereference
                           // is, etc
  int32_t field; // -1 if value doesn't have fields or all fields are flowing,
                 // id of field flowing otherwise

  FlowValue(const ir::Expression &value, uint32_t indirect_level);

  FlowValue(const ir::Expression &value, uint32_t indirect_level,
            int32_t field);

  /* check if the flow value actually contains anything
   * eg an indirection on a type without pointers (eg i32) is empty */
  bool isEmpty();
};

/* a flow of one FlowValue to another */
class Flow {
public:
  FlowValue value;
  FlowValue into;

  Flow(const FlowValue &value, const FlowValue &into);

  /* check if anything is actually flowing
   * if value is empty, so is the flow */
  bool isEmpty();
};

typedef std::vector<Flow> FlowList;

} // namespace ovid::ir

#endif