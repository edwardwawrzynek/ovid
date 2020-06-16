#include "escape_analysis.hpp"

namespace ovid::ir {

Flow::Flow(const FlowValue &value, const FlowValue &into)
    : from(value), into(into) {
  // types of value and into should match (with indirections + field selects
  // applied)
  auto valueTypes = value.getFlowingTypes();
  auto intoTypes = into.getFlowingTypes();

  assert(valueTypes.size() == intoTypes.size());
  for (size_t i = 0; i < valueTypes.size(); i++) {
    assert(valueTypes[i]->equalToExpected(*intoTypes[i]));
  }
}

FlowValue::FlowValue(const Expression &value, uint32_t indirect_level)
    : FlowValue(value, indirect_level, -1, false) {}

FlowValue::FlowValue(const Expression &value, uint32_t indirect_level,
                     int32_t field)
    : FlowValue(value, indirect_level, field, false) {}

FlowValue::FlowValue(const Expression &value, uint32_t indirect_level,
                     int32_t field, bool is_escape)
    : expr(value), indirect_level(indirect_level), field(field),
      is_escape(is_escape) {
  assert(field >= 0 || field == -1);
  // if field isn't -1, make sure expression is a product type
  if (field >= 0) {
    auto productType =
        dynamic_cast<const ast::ProductType *>(value.type->withoutMutability());
    assert(productType != nullptr);
    assert(productType->getTypeOfField(field) != nullptr);
  }
}

std::vector<const ast::Type *>
flattenProductType(const ast::ProductType *type) {
  std::vector<const ast::Type *> types;
  for (size_t i = 0; i < type->getNumFields(); i++) {
    auto fieldType = type->getTypeOfField(i)->withoutMutability();
    auto fieldProductType = dynamic_cast<const ast::ProductType *>(fieldType);
    if (fieldProductType != nullptr) {
      auto compTypes = flattenProductType(fieldProductType);
      types.insert(types.end(), compTypes.begin(), compTypes.end());
    } else {
      types.push_back(fieldType);
    }
  }

  return types;
}

std::vector<const ast::Type *> getIndirectedTypes(uint32_t indirect_level,
                                                  const ast::Type *type) {
  std::vector<const ast::Type *> types;
  if (indirect_level == 0) {
    types.push_back(type);
  } else {
    auto pointerType = dynamic_cast<const ast::PointerType *>(type);
    auto productType = dynamic_cast<const ast::ProductType *>(type);
    // if type is pointer, produces dereference type
    if (pointerType != nullptr) {
      auto newTypes = getIndirectedTypes(
          indirect_level - 1, pointerType->type->withoutMutability());
      types.insert(types.end(), newTypes.begin(), newTypes.end());
    }
    // if type is product, produce derefrences of all pointer types in it
    else if (productType != nullptr) {
      auto fieldTypes = flattenProductType(productType);
      for (auto &fieldType : fieldTypes) {
        auto pointerType = dynamic_cast<const ast::PointerType *>(fieldType);
        // if field is a pointer, add it's dereference type
        if (pointerType != nullptr) {
          auto newTypes = getIndirectedTypes(
              indirect_level - 1, pointerType->type->withoutMutability());
          types.insert(types.end(), newTypes.begin(), newTypes.end());
        }
      }
    }
    // otherwise, type shouldn't contain pointers
    else {
      assert(!type->containsPointer());
    }
  }

  return types;
}

Allocation *getOwningAllocation(Expression *expr) {
  auto alloc = dynamic_cast<Allocation *>(expr);
  if (alloc != nullptr)
    return alloc;
  auto fieldselect = dynamic_cast<FieldSelect *>(expr);
  if (fieldselect != nullptr)
    return getOwningAllocation(&fieldselect->expr);

  return nullptr;
}

const ast::Type *FlowValue::getTypeAfterField() const {
  if (field == -1) {
    return expr.type->withoutMutability();
  } else {
    auto productType =
        dynamic_cast<const ast::ProductType *>(expr.type->withoutMutability());
    assert(productType != nullptr);
    return productType->getTypeOfField(field)->withoutMutability();
  }
}

std::vector<const ast::Type *> FlowValue::getFlowingTypes() const {
  return getIndirectedTypes(indirect_level, getTypeAfterField());
}

bool FlowValue::isEmpty() const {
  // if no indirection, value can't be empty
  if (indirect_level == 0)
    return false;
  // otherwise, value is an indirection (dereference)
  // evaluate what it may point to
  auto indirectedTypes = getFlowingTypes();

  return indirectedTypes.empty();
}

void FlowValue::print(std::ostream &output) const {
  if (is_escape) {
    output << "ESCAPE";
  } else {
    for (uint32_t i = 0; i < indirect_level; i++)
      output << "*";
    // print value
    auto val = expr.val;
    output << "%";
    if (val.hasSourceName) {
      for (size_t i = 0; i < val.sourceName.size(); i++) {
        output << val.sourceName[i];
        if (i < val.sourceName.size() - 1)
          output << ":";
      }
    } else {
      output << val.id;
    }
    if (field >= 0)
      output << "." << field;
  }
}

bool Flow::isEmpty() {
  // if value is empty, into should also be empty (b/c their types should match)
  assert(from.isEmpty() == into.isEmpty());

  return from.isEmpty();
}

void Flow::print(std::ostream &output) {
  from.print(output);
  output << "\tflows to\t";
  into.print(output);
  output << "\n";
}

Flow Flow::withAddedIndirection(uint32_t indirections) const {
  auto newValue = FlowValue(from.expr, from.indirect_level + indirections,
                            from.field, from.is_escape);
  auto newInto = FlowValue(into.expr, into.indirect_level + indirections,
                           into.field, from.is_escape);
  return Flow(newValue, newInto);
}

std::vector<FlowValue> findExpressionFlows(const FlowList &flows,
                                           const ir::Expression &expr) {
  std::vector<FlowValue> res;
  for (auto &flow : flows) {
    if (flow.from.expr.val.id == expr.val.id) {
      res.push_back(flow.from);
    }
  }

  return res;
}

void traceFlow(const FlowList &flows, const FlowValue &flowValue,
               std::vector<FlowValue> &collectedFlows) {
  /* look through flow list and find any flows with matching sources */
  for (auto &flow : flows) {
    // only consider flows with matching sources
    if (flow.from.expr.val.id != flowValue.expr.val.id)
      continue;
    // if flow's indirection level is higher than flowValue's, don't consider it
    if (flow.from.indirect_level > flowValue.indirect_level)
      continue;
    // if both flowValue and flow.from have an explicitly specified field, and
    // they don't match, don't continue
    if (flowValue.field != -1 && flow.from.field != -1 &&
        flow.from.field != flowValue.field)
      continue;

    // if flow's indirection level is lower than flowValue, add indirection
    auto levelDiff = flowValue.indirect_level - flow.from.indirect_level;
    auto newFlow = flow.withAddedIndirection(levelDiff);
    // if flowValue had a field set, and both flow.from and flow.into didn't,
    // carry the field forward ie, if flowValue is *%a.field, and *%a -> *%b,
    // then *%a.field -> *%b.field
    if (flowValue.field != -1 && flow.from.field == -1 &&
        flow.into.field == -1 &&
        flow.from.indirect_level == flow.into.indirect_level) {
      newFlow.into.field = flowValue.field;
    }

    if (newFlow.isEmpty())
      continue;

    collectedFlows.push_back(newFlow.into);
    traceFlow(flows, newFlow.into, collectedFlows);
  }
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

  // print flows
  if (print_flows) {
    output << "FUNCTION ";
    for (auto &scope : instruct.val.sourceName) {
      output << scope << ":";
    }
    output << "\n";
    for (auto &flow : flows) {
      output << "\t";
      flow.print(output);
    }
  }

  // calculate flows needed for function metadata
  // these are flows where the source is an argument and the dest is another
  // arg, escape, or return
  for (auto &arg : instruct.argAllocs) {
    // find all uses of the arg in flow sources
    auto argUses = findExpressionFlows(flows, arg.get());
    // visit each use
    for (auto &use : argUses) {
      std::vector<FlowValue> tracedFlows;
      traceFlow(flows, use, tracedFlows);
      // TODO: sort out flows to args/escape/return
      if (print_flows) {
        output << "\tARG ";
        use.print(output);
        output << " flows to:\n";
        for (auto &flowValue : tracedFlows) {
          output << "\t\t";
          flowValue.print(output);
          output << "\n";
        }
      }
    }
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

  // if expression is an argument, set it's type to ARG_COPY_TO_STACK, as it
  // needs to be addressable
  auto alloc = getOwningAllocation(&instruct.expr);
  if (alloc != nullptr &&
      (alloc->allocType == AllocationType::ARG ||
       alloc->allocType == AllocationType::UNRESOLVED_FUNC_ARG)) {
    alloc->allocType = AllocationType::ARG_COPY_TO_STACK;
  }

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

void runEscapeAnalysis(const ir::InstructionList &ir, bool print_flows,
                       std::ostream &output) {
  auto pass = EscapeAnalysisPass(print_flows, output);
  auto globalFlows = ir::FlowList();
  pass.visitInstructions(ir, EscapeAnalysisState(false, &globalFlows));
}

} // namespace ovid::ir