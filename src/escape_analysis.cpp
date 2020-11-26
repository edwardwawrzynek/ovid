#include "escape_analysis.hpp"
#include "ir.hpp"

namespace ovid::ir {

bool operator==(const FlowValue &lhs, const FlowValue &rhs) {
  return lhs.expr.val.id == rhs.expr.val.id &&
         lhs.indirect_level == rhs.indirect_level &&
         lhs.field_selects == rhs.field_selects;
}

bool operator!=(const FlowValue &lhs, const FlowValue &rhs) {
  return !(lhs == rhs);
}

bool operator==(const Flow &lhs, const Flow &rhs) {
  return lhs.from == rhs.from && lhs.into == rhs.into;
}

bool operator!=(const Flow &lhs, const Flow &rhs) { return !(lhs == rhs); }

FlowValue::FlowValue(Expression &expr, int32_t indirect_level,
                     const std::vector<std::vector<int32_t>> &field_selects,
                     EscapeType is_escape)
    : expr(expr), indirect_level(indirect_level), field_selects(field_selects),
      is_escape(is_escape) {
  /* indirection level may reach -1 if an address is used */
  assert(indirect_level >= -1);
  assert(field_selects.size() == (uint32_t)(indirect_level + 1));
}

FlowValue::FlowValue(Expression &expr, int32_t indirect_level,
                     const std::vector<std::vector<int32_t>> &field_selects)
    : FlowValue(expr, indirect_level, field_selects, isGlobalEscape(expr)) {}

EscapeType FlowValue::isGlobalEscape(Expression &expr) {
  /* GlobalAllocation nodes are global escapes */
  if (dynamic_cast<const GlobalAllocation *>(&expr) != nullptr) {
    return EscapeType::OTHER;
  }

  return EscapeType::NONE;
}

const ast::Type *
FlowValue::applyFieldSelectToType(const ast::Type *type,
                                  const std::vector<int32_t> &field_select) {
  for (auto &field : field_select) {
    auto productType = dynamic_cast<const ast::ProductType *>(type);
    assert(productType != nullptr);

    type = productType->getTypeOfField(field)->withoutMutability();
  }

  return type;
}

std::vector<const ast::Type *> FlowValue::getFlowingTypesFromType(
    const ast::Type *exprType, int32_t indirect_level,
    const std::vector<std::vector<int32_t>> &field_selects) {
  std::vector<const ast::Type *> res;
  /* values with indirection -1 can't flow */
  assert(indirect_level >= 0);

  auto type = exprType->withoutMutability();

  /* apply field selections */
  type = applyFieldSelectToType(
      type, field_selects[field_selects.size() - indirect_level - 1]);

  /* if no indirections are left, just return type */
  if (indirect_level == 0) {
    /* if product type, flatten and add component types */
    auto productType = dynamic_cast<const ast::ProductType *>(type);
    if (productType != nullptr) {
      auto componentTypes = flattenProductType(productType);
      for (auto &compType : componentTypes) {
        res.push_back(compType);
      }
    } else {
      res.push_back(type);
    }

    return res;
  } else {
    /* get types of one indirection level down */
    auto pointerType = dynamic_cast<const ast::PointerType *>(type);
    auto productType = dynamic_cast<const ast::ProductType *>(type);
    /* apply indirection */
    if (pointerType != nullptr) {
      auto derefedType =
          getFlowingTypesFromType(pointerType->type->withoutMutability(),
                                  indirect_level - 1, field_selects);
      res.insert(res.end(), derefedType.begin(), derefedType.end());
    } else if (productType != nullptr) {
      /* flatten product type into component types */
      auto componentTypes = flattenProductType(productType);
      for (auto &compType : componentTypes) {
        /* only pointers will flow */
        auto pointerCompType = dynamic_cast<const ast::PointerType *>(
            compType->withoutMutability());
        if (pointerCompType != nullptr) {
          auto derefedType = getFlowingTypesFromType(
              pointerCompType->type->withoutMutability(), indirect_level - 1,
              field_selects);
          res.insert(res.begin(), derefedType.begin(), derefedType.end());
        } else {
          assert(!compType->containsPointer());
        }
      }
    } else {
      assert(!type->containsPointer());
    }
  }

  return res;
}

std::vector<const ast::Type *> FlowValue::getFlowingTypes() const {
  return getFlowingTypesFromType(expr.type.get(), indirect_level,
                                 field_selects);
}

bool FlowValue::isEmpty() const {
  auto flowingTypes = getFlowingTypes();

  return flowingTypes.empty();
}

FlowValue FlowValue::getFlowFromExpressionWithoutCopy(
    Expression &expr, std::vector<int32_t> &field_selects_on_deref) {
  std::vector<int32_t> retroactive_field_selects;
  auto fieldSelect = dynamic_cast<FieldSelect *>(&expr);
  if (fieldSelect != nullptr) {
    auto value = getFlowFromExpressionWithoutCopy(fieldSelect->expr,
                                                  retroactive_field_selects);
    /* can't take a field on a pointer (type check should have checked this
     * already) */
    assert(retroactive_field_selects.empty());
    /* add field select onto value */
    value.field_selects[value.field_selects.size() - 1].push_back(
        fieldSelect->field_index);

    return value;
  }
  auto deref = dynamic_cast<Dereference *>(&expr);
  if (deref != nullptr) {
    auto value = getFlowFromExpressionWithoutCopy(deref->expr,
                                                  retroactive_field_selects);
    /* add dereference (with no field select) */
    value.indirect_level++;
    value.field_selects.push_back(retroactive_field_selects);

    return value;
  }
  auto addr = dynamic_cast<Address *>(&expr);
  if (addr != nullptr) {
    auto value =
        getFlowFromExpressionWithoutCopy(addr->expr, retroactive_field_selects);
    /* can't take an address of an address (type check should have checked this
     * already) */
    assert(retroactive_field_selects.empty());
    /* remove most recent dereference
     * this may produce a value with indirect_level -1, which should be handled
     * by the implicit dereference added on copy */
    assert(value.indirect_level >= 0);
    value.indirect_level--;
    /* field selection is lost when address of field is taken
     * ie -- if &(%a.field), &%a flows, not just &%a.field
     * however, it should be preserved on the next deref
     * ie -- if *&(%a.field), %a.field flows */
    auto lastSelects = value.field_selects.back();
    field_selects_on_deref.insert(field_selects_on_deref.end(),
                                  lastSelects.begin(), lastSelects.end());
    value.field_selects.pop_back();

    return value;
  }

  /* create value with no field select */
  std::vector<std::vector<int32_t>> field_selects;
  field_selects.emplace_back();
  auto value = FlowValue(expr, 0, field_selects);

  return value;
}

void FlowValue::print(std::ostream &output) const {
  for (int32_t i = 0; i < indirect_level; i++) {
    output << "*(";
  }

  output << "%";
  if (is_escape == EscapeType::RETURN) {
    output << "RETURN";
  } else if (is_escape == EscapeType::OTHER) {
    output << "ESCAPE";
  } else {
    if (expr.val.id.hasSourceName) {
      auto name = expr.val.id.sourceName->getFullyScopedName();
      for (size_t i = 0; i < name.size(); i++) {
        output << name[i];
        if (i < name.size() - 1)
          output << ":";
      }
    } else {
      output << expr.val.id.id;
    }
  }

  for (int32_t i = 0; i <= indirect_level; i++) {
    if (i < indirect_level)
      output << ")";

    for (auto &select : field_selects[i]) {
      output << "." << select;
    }
  }
}

FlowValue FlowValue::getFlowFromExpression(Expression &expr) {
  std::vector<int32_t> field_sels_to_apply;
  auto value = getFlowFromExpressionWithoutCopy(expr, field_sels_to_apply);
  value.indirect_level++;
  value.field_selects.push_back(field_sels_to_apply);

  return value;
}

bool FlowValue::fieldsMatchOrContain(const FlowValue &value) const {
  if (indirect_level != value.indirect_level ||
      expr.val.id != value.expr.val.id) {
    assert(false);

    return false;
  }

  assert(field_selects.size() == value.field_selects.size());

  for (size_t i = 0; i < field_selects.size(); i++) {
    auto &thisSelects = field_selects[i];
    auto &valueSelects = value.field_selects[i];
    /* if value is more general than this, fail */
    if (thisSelects.size() > valueSelects.size())
      return false;
    for (size_t j = 0; j < thisSelects.size(); j++) {
      if (valueSelects[j] != thisSelects[j])
        return false;
    }
  }

  return true;
}

FlowValue FlowValue::getFlowForDereference(Expression &expr) {
  std::vector<std::vector<int32_t>> selects;
  selects.emplace_back();
  selects.emplace_back();
  return FlowValue(expr, 1, selects);
}

FlowValue FlowValue::withAddedIndirections(int32_t indirections) const {
  auto newVal = FlowValue(expr, indirect_level, field_selects, is_escape);

  newVal.indirect_level += indirections;
  for (int32_t i = 0; i < indirections; i++) {
    newVal.field_selects.emplace_back();
  }

  return newVal;
}

bool FlowValue::contains(const FlowValue &value) const {
  /* mismathced escape types don't contain each other */
  if (is_escape != value.is_escape)
    return false;
  /* a flow of %b doesn't contain %a */
  if (expr.val.id != value.expr.val.id)
    return false;
  /* a flow of **%a doesn't contain *%a */
  if (indirect_level > value.indirect_level)
    return false;

  /* construct a new flow with matching indirection level
   * ie -- flow of *%a containsFrom **%a */
  const FlowValue &newFlow =
      value.indirect_level > indirect_level
          ? withAddedIndirections(value.indirect_level - indirect_level)
          : *this;

  return newFlow.fieldsMatchOrContain(value);
}

FlowValue FlowValue::specializeFields(const FlowValue &src,
                                      const FlowValue &dst) const {
  assert(src.fieldsMatchOrContain(dst));
  assert(src.indirect_level == dst.indirect_level);

  /* how many indirections to examine */
  auto levelsDeep = std::min(src.indirect_level, dst.indirect_level) + 1;

  auto newValue = FlowValue(expr, indirect_level, field_selects, is_escape);
  for (int32_t i = 0; i < levelsDeep; i++) {
    auto &srcSelects = src.field_selects[src.indirect_level - i];
    auto &dstSelects = dst.field_selects[dst.indirect_level - i];
    auto &newSelects = newValue.field_selects[newValue.indirect_level - i];

    for (size_t j = 0; j < dstSelects.size(); j++) {
      if (j < srcSelects.size()) {
        assert(dstSelects[j] == srcSelects[j]);
      } else {
        /* if value has a field selector that from doesn't, add that selector in
         * the appropriate spot to into */
        newSelects.push_back(dstSelects[j]);
      }
    }
  }

  return newValue;
}

Flow::Flow(const FlowValue &from, const FlowValue &into)
    : from(from), into(into) {
  /* make sure flowing types are the same */
  auto fromFlowing = Flow::from.getFlowingTypes();
  auto intoFlowing = Flow::into.getFlowingTypes();

  assert(fromFlowing.size() == intoFlowing.size());
}

bool Flow::isEmpty() const {
  /* empty-ness of both values should match, as they should have the same types
   * flowing */
  assert(from.isEmpty() == into.isEmpty());
  if (from.isEmpty())
    return true;

  // if into contains from, (eg **%a -> *%a), the flow is empty (implicit rules
  // already implement the flow)
  if (into.contains(from))
    return true;

  return false;
}

void Flow::print(std::ostream &output) const {
  from.print(output);
  output << " flows to ";
  into.print(output);
  output << "\n";
}

bool Flow::containsFrom(const FlowValue &value) const {
  return from.contains(value);
}

bool Flow::containsInto(const FlowValue &value) const {
  return into.contains(value);
}

Flow Flow::specializeForFrom(const FlowValue &value) const {
  assert(containsFrom(value));
  // add indirections such that from's level matches value's
  auto levelsToAdd = value.indirect_level - from.indirect_level;
  auto newFrom = from.withAddedIndirections(levelsToAdd);
  auto newInto = into.withAddedIndirections(levelsToAdd);

  return Flow(value, newInto.specializeFields(newFrom, value));
}

Flow Flow::specializeForInto(const FlowValue &value) const {
  assert(containsInto(value));
  // add indirections such that into's level matches value's
  auto levelsToAdd = value.indirect_level - into.indirect_level;
  auto newFrom = from.withAddedIndirections(levelsToAdd);
  auto newInto = into.withAddedIndirections(levelsToAdd);

  return Flow(newFrom.specializeFields(newInto, value), value);
}

FuncFlowValue::FuncFlowValue(
    int32_t arg_index, int32_t indirect_level,
    const std::vector<std::vector<int32_t>> &field_selects)
    : arg_index(arg_index), indirect_level(indirect_level),
      field_selects(field_selects) {
  assert(field_selects.size() == (uint32_t)(indirect_level + 1));
}

FuncFlow::FuncFlow(const FuncFlowValue &from, const FuncFlowValue &into)
    : from(from), into(into) {}

FuncFlowValue FuncFlowValue::fromFlowValue(
    const FlowValue &value,
    const std::vector<std::reference_wrapper<Allocation>> &args) {
  int32_t arg_index = 0;
  if (value.is_escape == EscapeType::RETURN) {
    arg_index = -1;
  } else if (value.is_escape == EscapeType::OTHER) {
    arg_index = -2;
  } else {
    arg_index = findIndexOfArg(args, value.expr);
  }

  return FuncFlowValue(arg_index, value.indirect_level, value.field_selects);
}

FuncFlow FuncFlow::fromFlow(
    const Flow &flow,
    const std::vector<std::reference_wrapper<Allocation>> &args) {
  return FuncFlow(FuncFlowValue::fromFlowValue(flow.from, args),
                  FuncFlowValue::fromFlowValue(flow.into, args));
}

void FuncFlowValue::print(std::ostream &output) {
  for (int32_t i = 0; i < indirect_level; i++) {
    output << "*(";
  }

  output << "%";
  if (arg_index == -1) {
    output << "RETURN";
  } else if (arg_index == -2) {
    output << "ESCAPE";
  } else {
    output << "arg:" << arg_index;
  }

  for (int32_t i = 0; i <= indirect_level; i++) {
    if (i < indirect_level)
      output << ")";

    for (auto &select : field_selects[i]) {
      output << "." << select;
    }
  }
}

void FuncFlow::print(std::ostream &output) {
  from.print(output);
  output << " flows to ";
  into.print(output);
  output << "\n";
}

FlowValue FuncFlowValue::toFlowValue(
    const std::vector<std::reference_wrapper<Expression>> &args,
    Expression &returnExpr, const FlowValue *escapeValue) {
  if (arg_index == -2) {
    assert(escapeValue != nullptr);
    /* copy escapeValue and set escape flag */
    return FlowValue(escapeValue->expr, escapeValue->indirect_level,
                     escapeValue->field_selects, EscapeType::OTHER);
  }

  /* select expression to use (arg or return) */
  auto &expr = arg_index == -1 ? returnExpr : args[arg_index].get();

  std::vector<int32_t> field_selects_on_deref;
  auto exprValue =
      FlowValue::getFlowFromExpressionWithoutCopy(expr, field_selects_on_deref);

  assert(exprValue.field_selects.size() ==
         (uint32_t)(exprValue.indirect_level + 1));

  /* add this's indirects + field selects to exprValue */
  exprValue.indirect_level += indirect_level;
  /* combine outermost exprValue selects and innermost this selects */
  if (!field_selects.front().empty()) {
    auto &exprSelectsBack = exprValue.field_selects.back();
    exprSelectsBack.insert(exprSelectsBack.end(), field_selects.front().begin(),
                           field_selects.front().end());
  }
  /* add rest of this selects */
  for (size_t i = 1; i < field_selects.size(); i++) {
    exprValue.field_selects.push_back(field_selects[i]);
  }
  /* if needed, apply field_selects_on_deref */
  if (!field_selects_on_deref.empty()) {
    /* if field_selects_on_deref isn't empty, top level of expression was an
     * address. can't take fields on pointers, so none should be present */
    assert(exprValue.field_selects[0].empty());
    /* add field selects */
    exprValue.field_selects[0].insert(exprValue.field_selects[0].end(),
                                      field_selects_on_deref.begin(),
                                      field_selects_on_deref.end());
  }
  assert(exprValue.field_selects.size() ==
         (uint32_t)(exprValue.indirect_level + 1));

  return exprValue;
}

Flow FuncFlow::toFlow(
    const std::vector<std::reference_wrapper<Expression>> &args,
    Expression &returnExpr) {
  /* from shouldn't be a return or escape */
  assert(from.arg_index >= 0);
  auto newFrom = from.toFlowValue(args, returnExpr, nullptr);
  auto newInto = into.toFlowValue(args, returnExpr, &newFrom);

  return Flow(newFrom, newInto);
}

template <class T>
static bool vectorContains(const std::vector<T> &vector, const T &value) {
  return std::find(vector.begin(), vector.end(), value) != vector.end();
}

void traceFlow(
    const FlowValue &value, const FlowValue &initValue, const FlowList &flows,
    const std::function<void(const FlowValue &)> &func,
    const std::function<void(const FlowValue &)> &specializedFlowFunc,
    TraceFlowVisitedState &state) {
  for (auto &flow : flows) {
    // if we've already visited this flow for this value, don't revisit
    if (vectorContains(state.visitedFlows, flow))
      continue;

    if (flow.containsFrom(value)) {
      auto specialized = flow.specializeForFrom(value);
      // new value to trace
      auto &newVal = specialized.into;

      if (vectorContains(state.visitedFroms, newVal))
        continue;

      func(newVal);
      /* trace the flow further, unless it is to an escaping location (no point
       * tracing it when it is already known to escape) */
      if (!EscapeTypeIsEscape(newVal.is_escape)) {
        // don't follow the same flow twice for the same value
        state.visitedFlows.push_back(flow);
        // trace the new from value
        traceFlow(newVal, initValue, flows, func, specializedFlowFunc, state);
        state.visitedFlows.pop_back();
      }

      state.visitedFroms.push_back(newVal);
    } else if (flow.from.expr.val.id == value.expr.val.id) {
      /* flow is more specialized than value, adjust to initValue and call
       * specializedFlowFunc */
      auto generalFlow = Flow(value, initValue);
      /* generalFlow may have field selects incompatible with flow.from */
      if (!generalFlow.containsFrom(flow.from))
        continue;

      auto specialFlow = generalFlow.specializeForFrom(flow.from);
      auto &specialVal = specialFlow.into;

      if (vectorContains(state.visitedSpecialFroms, specialVal))
        continue;

      specializedFlowFunc(specialVal);
      state.visitedSpecialFroms.push_back(specialVal);
    }
  }
}

// trace a specific value backwards
void traceValueBackwards(const FlowList &flows,
                         const std::function<void(const FlowValue &)> &func,
                         TraceFlowVisitedState &state, const FlowValue &value) {
  if (vectorContains(state.visitedFroms, value))
    return;
  func(value);

  for (auto &flow : flows) {
    if (vectorContains(state.visitedFlows, flow))
      continue;

    // whether to further trace this flow
    bool useFlow = false;

    if (value.contains(flow.into)) {
      useFlow = true;
      // flow won't be transformed, so visiting it will always result in the
      // same value
      state.visitedFlows.push_back(flow);
    } else if (flow.into.contains(value)) {
      useFlow = true;
    }

    if (useFlow) {
      // adjusted flow to trace (if useFlow)
      const Flow &newFlow =
          flow.into.contains(value) ? flow.specializeForInto(value) : flow;

      auto &newValue = newFlow.from;
      if (newValue == value)
        continue;

      traceValueBackwards(flows, func, state, newValue);
    }
  }

  state.visitedFroms.push_back(value);
}

// find all escaping into's and pass them to traceValueBackwards
void traceEscapesBackwards(const FlowList &flows,
                           const std::function<void(const FlowValue &)> &func,
                           TraceFlowVisitedState &state) {
  for (auto &flow : flows) {
    if (EscapeTypeIsEscape(flow.into.is_escape)) {
      state.visitedFlows.clear();
      traceValueBackwards(flows, func, state, flow.from);
    }
  }
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

void visitAllocations(const BasicBlockList &blocks,
                      const std::function<void(Allocation &)> &func) {
  for (auto &block : blocks) {
    for (auto &instruct : block->body) {
      auto alloc = dynamic_cast<Allocation *>(instruct.get());
      if (alloc != nullptr) {
        func(*alloc);
      }
    }
  }
}

int EscapeAnalysisPass::visitFunctionDeclare(FunctionDeclare &instruct,
                                             const EscapeAnalysisState &state) {
  // if the function has already been visited, skip it
  if (instruct.flow_state == FunctionEscapeAnalysisState::VISITED ||
      instruct.flow_state == FunctionEscapeAnalysisState::CUR_VISITING) {
    return 0;
  }

  assert(instruct.flow_state == FunctionEscapeAnalysisState::NOT_VISITED);
  instruct.flow_state = FunctionEscapeAnalysisState::CUR_VISITING;

  // pointer flow for this function
  FlowList flows;
  auto newState = EscapeAnalysisState(true, &flows);

  // visit children of function, calculate flow
  for (auto &block : instruct.body) {
    visitInstruction(*block, newState);
  }

  // print flows
  if (print_flows || print_escapes || print_func_flow_metadata) {
    output << "FUNCTION ";
    auto name = instruct.val.id.sourceName->getFullyScopedName();
    for (auto &scope : name) {
      output << scope << ":";
    }
    output << "\n";
  }
  if (print_flows) {
    for (auto &flow : flows) {
      output << "\t";
      flow.print(output);
    }
  }

  // mark escaping allocs in func body
  markEscapingAllocations(flows, instruct.body);
  // print debug info
  if (print_escapes) {
    visitAllocations(instruct.body, [this](Allocation &alloc) {
      if (AllocationTypeIsHeap(alloc.allocType)) {
        output << "%" << alloc.val.id.sourceName->getFullyScopedName()[0]
               << " escapes\n";
      }
    });
  }

  // calculate flows needed for function metadata
  // these are flows where the source is an argument and the dest is another
  // arg, escape, or return
  for (auto &arg : instruct.argAllocs) {
    TraceFlowVisitedState trace_state;

    std::vector<std::vector<int32_t>> selects;
    selects.emplace_back();
    auto value = FlowValue(arg.get(), 0, selects);
    calculateFunctionFlowMetadata(instruct.flow_metadata, instruct.argAllocs,
                                  flows, value, trace_state);
  }

  if (print_func_flow_metadata) {
    for (auto &flow : instruct.flow_metadata) {
      flow.print(output);
    }
  }

  /* mark function metadata as complete */
  instruct.flow_state = FunctionEscapeAnalysisState::VISITED;

  return 0;
}

void EscapeAnalysisPass::markEscapingAllocations(const ir::FlowList &flows,
                                                 const BasicBlockList &blocks) {
  // trace flows from escapes backwards and mark any hit allocations as
  // heap-alloc'd
  TraceFlowVisitedState trace_state;
  traceEscapesBackwards(
      flows,
      [&blocks](const FlowValue &value) {
        if (value.indirect_level != 0)
          return;

        visitAllocations(blocks, [&value](Allocation &alloc) {
          // don't mark allocation more than once
          if (alloc.val.id == value.expr.val.id &&
              !AllocationTypeIsHeap(alloc.allocType)) {
            alloc.allocType = AllocationTypeToHeap(alloc.allocType);
          }
        });
      },
      trace_state);
}

bool EscapeAnalysisPass::argsContain(
    const std::vector<std::reference_wrapper<Allocation>> &funcArgs,
    const Expression &expr) {
  for (auto &arg : funcArgs) {
    if (expr.val.id == arg.get().val.id)
      return true;
  }

  return false;
}

void EscapeAnalysisPass::calculateFunctionFlowMetadata(
    ir::FuncFlowList &functionFlows,
    const std::vector<std::reference_wrapper<Allocation>> &funcArgs,
    const ir::FlowList &flows, const FlowValue &srcValue,
    TraceFlowVisitedState &trace_state) {
  traceFlow(
      srcValue, srcValue, flows,
      [&functionFlows, &funcArgs, &srcValue](const FlowValue &dstValue) {
        /* srcValue flows to dstValue
         * only add flow if it involves an arg->arg, arg->escape or arg->return
         * flow */
        if (argsContain(funcArgs, srcValue.expr) &&
            (EscapeTypeIsEscape(dstValue.is_escape) ||
             argsContain(funcArgs, dstValue.expr))) {
          auto flow = Flow(srcValue, dstValue);
          /* if flow doesn't have any indirections on source, add one to account
           * for implicit copy when args are passed */
          if (srcValue.indirect_level < 1) {
            auto newSelects = srcValue.field_selects;
            newSelects.emplace_back();
            auto newSrc = FlowValue(srcValue.expr, srcValue.indirect_level + 1,
                                    newSelects, srcValue.is_escape);
            auto newFlow = flow.specializeForFrom(newSrc);

            functionFlows.emplace_back(FuncFlow::fromFlow(newFlow, funcArgs));
          } else {
            functionFlows.emplace_back(FuncFlow::fromFlow(flow, funcArgs));
          }
        }
      },
      [this, &functionFlows, &funcArgs, &flows,
       &trace_state](const FlowValue &specialValue) {
        /* trace specialized value */
        calculateFunctionFlowMetadata(functionFlows, funcArgs, flows,
                                      specialValue, trace_state);
      },
      trace_state);
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
  // if local var, mark as stack variable
  // if it escapes, it will be marked as heap later
  if (instruct.allocType == AllocationType::UNRESOLVED_LOCAL) {
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
  /* if we can get escape information for function, use that */
  if (instruct.function.hasFlowMetadata()) {
    visitInstruction(instruct.function, EscapeAnalysisState(false, nullptr));
    /* addjust function flows to  args and add to flow list */
    instruct.function.addFlowMetadata(*state.curFlowList, instruct.arguments,
                                      instruct);
  }
  /* otherwise, assume *arg -> ESCAPE for all args */
  else {
    for (auto &arg : instruct.arguments) {
      std::vector<std::vector<int32_t>> selects;
      selects.emplace_back();
      selects.emplace_back();
      auto from = FlowValue(arg.get(), 1, selects);
      auto into = FlowValue(arg.get(), 1, selects, EscapeType::OTHER);
      auto flow = Flow(from, into);

      if (!flow.isEmpty())
        state.curFlowList->emplace_back(flow);
    }
  }
  return 0;
}

int EscapeAnalysisPass::visitStore(Store &instruct,
                                   const EscapeAnalysisState &state) {
  // store instruction is equivalent to assignment
  // for store %a into %b, flow is *%a -> *%b
  auto from = FlowValue::getFlowFromExpression(instruct.value);
  auto into = FlowValue::getFlowFromExpression(instruct.storage);
  auto flow = Flow(from, into);
  // only add flow if it isn't empty
  if (!flow.isEmpty())
    state.curFlowList->push_back(flow);

  return 0;
}

int EscapeAnalysisPass::visitAddress(Address &instruct,
                                     const EscapeAnalysisState &state) {
  // no flow needs to be calculated for addr (handled by
  // FlowValue::getFlowFromExpression)

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

int EscapeAnalysisPass::visitTupleLiteral(TupleLiteral &instruct,
                                          const EscapeAnalysisState &state) {
  // all arguments flow into their appropriate fields with indirection level 1
  for (size_t i = 0; i < instruct.exprs.size(); i++) {
    auto &expr = instruct.exprs[i];

    auto from = FlowValue::getFlowFromExpression(expr);
    auto into = FlowValue::getFlowFromExpression(instruct);
    /* add field select to into */
    assert(into.field_selects.size() == 2);
    into.field_selects[0].push_back(i);

    auto flow = Flow(from, into);
    // flow may be empty if arg doesn't contain pointers
    if (!flow.isEmpty())
      state.curFlowList->push_back(flow);
  }

  return 0;
}

int EscapeAnalysisPass::visitReturn(Return &instruct,
                                    const EscapeAnalysisState &state) {
  // nothing to do for return without an expression
  if (instruct.expr == nullptr)
    return 0;

  // handle return with an expression
  // reuse expression as dest (to make types match), but label with RETURN
  // escape
  auto from = FlowValue::getFlowFromExpression(*instruct.expr);
  std::vector<std::vector<int32_t>> selects;
  selects.emplace_back();
  selects.emplace_back();
  auto into = FlowValue(*instruct.expr, 1, selects, EscapeType::RETURN);
  auto flow = Flow(from, into);

  if (!flow.isEmpty())
    state.curFlowList->push_back(flow);

  return 0;
}

int EscapeAnalysisPass::visitBuiltinCast(BuiltinCast &instruct,
                                         const EscapeAnalysisState &state) {
  // if a pointer flows into a cast, assume it escapes
  // pointer casts are unsafe, so we can't do complete flow analysis
  if (instruct.expr.type->containsPointer()) {
    auto from = FlowValue::getFlowFromExpression(instruct.expr);
    std::vector<std::vector<int32_t>> selects;
    selects.emplace_back();
    selects.emplace_back();
    auto into = FlowValue(instruct.expr, 1, selects, EscapeType::OTHER);
    auto flow = Flow(from, into);

    if (!flow.isEmpty())
      state.curFlowList->push_back(flow);
  }

  return 0;
}

int EscapeAnalysisPass::visitForwardIdentifier(
    ForwardIdentifier &instruct, const EscapeAnalysisState &state) {
  /* if an ir declaration node isn't set, identifier is external (and should
   * already have escape analysis metadata calculated) */
  if (instruct.symbol_ref->ir_decl_instruction != nullptr) {
    return visitInstruction(*instruct.symbol_ref->ir_decl_instruction, state);
  } else {
    return 0;
  }
}

void runEscapeAnalysis(const ir::InstructionList &ir, bool print_flows,
                       bool print_escapes, bool print_func_flow_metadata,
                       std::ostream &output) {
  auto pass = EscapeAnalysisPass(print_flows, print_escapes,
                                 print_func_flow_metadata, output);
  auto globalFlows = ir::FlowList();
  pass.visitInstructions(ir, EscapeAnalysisState(false, &globalFlows));
}

} // namespace ovid::ir