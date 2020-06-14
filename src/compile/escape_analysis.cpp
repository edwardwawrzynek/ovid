#include "escape_analysis.hpp"

namespace ovid::ir {

Flow::Flow(const FlowValue &value, const FlowValue &into)
    : value(value), into(into) {
  // types of value and into should match (with indirection)
  // TODO: fix to account for indirection
  /*ast::Type* valueType = value.value.type.get();
  ast::Type* intoType = into.value.type.get();
  if(value.field >= 0) {
    valueType = dynamic_cast<ast::ProductType*>(valueType)->getTypeOfField(value.field).get();
  }
  if(into.field >= 0) {
    intoType = dynamic_cast<ast::ProductType*>(intoType)->getTypeOfField(into.field).get();
  }
  assert(valueType->equalToExpected(*intoType));*/
}

FlowValue::FlowValue(const Expression &value, uint32_t indirect_level)
    : value(value), indirect_level(indirect_level), field(-1) {}

FlowValue::FlowValue(const Expression &value, uint32_t indirect_level,
                     int32_t field)
    : value(value), indirect_level(indirect_level), field(field) {
  assert(field >= 0 || field == -1);
  // if field isn't -1, make sure expression is a product type
  if (field >= 0) {
    auto productType = dynamic_cast<const ast::ProductType *>(value.type->withoutMutability());
    assert(productType != nullptr);
    assert(productType->getTypeOfField(field) != nullptr);
  }
}

bool FlowValue::isEmpty() {
  // if no indirection, value can't be empty
  if (indirect_level <= 0)
    return false;
  // otherwise, value is an indirection (dereference)
  // if it doesn't contain pointers, it is empty (nothing to be dereference)
  if (field == -1) {
    return !value.type->containsPointer();
  } else {
    auto productType = dynamic_cast<const ast::ProductType *>(value.type->withoutMutability());
    assert(productType != nullptr);
    return !productType->getTypeOfField(field)->containsPointer();
  }
}

void FlowValue::print(std::ostream &output) {
  for(uint32_t i = 0; i < indirect_level; i++) output << "*";
  // print value
  auto val = value.val;
  output << "%";
  if(val.hasSourceName) {
    for(size_t i = 0; i < val.sourceName.size(); i++) {
      output << val.sourceName[i];
      if(i < val.sourceName.size() - 1) output << ":";
    }
  } else {
    output << val.id;
  }
  if(field >= 0) output << "." << field;
}

bool Flow::isEmpty() {
  // if value is empty, into should also be empty (b/c their types should match)
  //assert(value.isEmpty() == into.isEmpty());

  //return value.isEmpty();
  return false;
}

void Flow::print(std::ostream &output) {
  value.print(output);
  output << "\tflows to\t";
  into.print(output);
  output << "\n";
}

int EscapeAnalysisPass::visitFunctionDeclare(FunctionDeclare &instruct,
                                             const EscapeAnalysisState &state) {
  // pointer flow for this function
  FlowList flows;
  auto newState = EscapeAnalysisState(true, &flows);

  // visit children of function, calculate flow
  for (auto &block : instruct.body) {
    visitInstruction(*block, newState);
  }

  // TODO: unify/solve flows
  std::cout << "flows in function ";
  for(auto &scope: instruct.val.sourceName) {
    std::cout << scope << ":";
  }
  std::cout << "\n";
  for(auto &flow: flows) {
    flow.print(std::cout);
  }
  return 0;
}

int EscapeAnalysisPass::visitBasicBlock(BasicBlock &instruct,
                                        const EscapeAnalysisState &state) {
  // just visit instructions
  for (auto &child : instruct.body) {
    visitInstruction(*child, state);
  }

  return 0;
}

int EscapeAnalysisPass::visitAllocation(Allocation &instruct,
                                        const EscapeAnalysisState &state) {
  // if global, mark allocation appropriately
  if (!state.in_func) {
    assert(instruct.allocType == AllocationType::UNRESOLVED_GLOBAL);
    instruct.allocType = AllocationType::STATIC;
  }
  // if local var, mark as stack variable
  // if it escapes, it will be marked as heap later
  else if (instruct.allocType == AllocationType::UNRESOLVED_LOCAL) {
    instruct.allocType = AllocationType::STACK;
  }
  // if argument, mark as normal llvm arg
  // if it needs address or escapes, it will be marked later
  else if (instruct.allocType == AllocationType::UNRESOLVED_FUNC_ARG) {
    instruct.allocType = AllocationType::ARG;
  } else {
    assert(false);
  }

  return 0;
}

int EscapeAnalysisPass::visitFunctionCall(FunctionCall &instruct,
                                          const EscapeAnalysisState &state) {
  // TODO: if we can get escape information for function, use that. Otherwise,
  // assume FlowValue(arg, 1) -> ESCAPE for all args
  return 0;
}

int EscapeAnalysisPass::visitStore(Store &instruct,
                                   const EscapeAnalysisState &state) {
  // store instruction is equivalent to assignment
  // for store %b <- %a, flow is FlowValue(%a, 1) -> FlowValue(%b, 1)
  auto flow =
      Flow(FlowValue(instruct.value, 1), FlowValue(instruct.storage, 1));
  // only add flow if it isn't empty
  if (!flow.isEmpty())
    state.curFlowList->push_back(flow);

  return 0;
}

int EscapeAnalysisPass::visitAddress(Address &instruct,
                                     const EscapeAnalysisState &state) {
  // for %b = &%a, flow is FlowValue(%a, 0) -> FlowValue(%b, 1)
  auto flow = Flow(FlowValue(instruct.expr, 0), FlowValue(instruct, 1));
  // an address flow can't be empty
  assert(!flow.isEmpty());
  state.curFlowList->push_back(flow);

  return 0;
}

int EscapeAnalysisPass::visitDereference(Dereference &instruct,
                                         const EscapeAnalysisState &state) {
  // for %b = *%a, flow is FlowValue(%a, 2) -> FlowValue(%b, 1)
  auto flow = Flow(FlowValue(instruct.expr, 2), FlowValue(instruct, 1));
  if (!flow.isEmpty())
    state.curFlowList->push_back(flow);

  return 0;
}

int EscapeAnalysisPass::visitFieldSelect(FieldSelect &instruct,
                                         const EscapeAnalysisState &state) {
  // for %b = %a.field, flow is FlowValue(%a, 1, field) -> FlowValue(%b, 1)
  auto flow = Flow(FlowValue(instruct.expr, 1, instruct.field_index),
                   FlowValue(instruct, 1));
  if (!flow.isEmpty())
    state.curFlowList->push_back(flow);

  return 0;
}

int EscapeAnalysisPass::visitTupleLiteral(TupleLiteral &instruct,
                                          const EscapeAnalysisState &state) {
  // all arguments flow into their appropriate fields with indirection level 1
  for (size_t i = 0; i < instruct.exprs.size(); i++) {
    auto &expr = instruct.exprs[i];

    auto flow = Flow(FlowValue(expr, 1), FlowValue(instruct, 1, i));
    // flow may be empty if arg doesn't contain pointers
    if (!flow.isEmpty())
      state.curFlowList->push_back(flow);
  }

  return 0;
}

// no flow for literals that don't contain pointers
int EscapeAnalysisPass::visitIntLiteral(IntLiteral &instruct,
                                        const EscapeAnalysisState &state) {
  return 0;
}

int EscapeAnalysisPass::visitBoolLiteral(BoolLiteral &instruct,
                                         const EscapeAnalysisState &state) {
  return 0;
}

int EscapeAnalysisPass::visitBuiltinCast(BuiltinCast &instruct,
                                         const EscapeAnalysisState &state) {
  // builtin casts shouldn't be casting pointers, so nothing should flow
  assert(!instruct.type->containsPointer());
  assert(!instruct.expr.type->containsPointer());

  return 0;
}

} // namespace ovid::ir