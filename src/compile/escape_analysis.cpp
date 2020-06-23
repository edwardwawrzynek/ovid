#include "escape_analysis.hpp"
#include "ir.hpp"

namespace ovid::ir {

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

  /* globals will be allocations with allocType set to global */
  auto globalAlloc = dynamic_cast<const Allocation *>(&expr);
  if (globalAlloc != nullptr &&
      (AllocationTypeIsGlobal(globalAlloc->allocType))) {
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
    if (expr.val.hasSourceName) {
      for (size_t i = 0; i < expr.val.sourceName.size(); i++) {
        output << expr.val.sourceName[i];
        if (i < expr.val.sourceName.size() - 1)
          output << ":";
      }
    } else {
      output << expr.val.id;
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

  return from.isEmpty();
}

void Flow::print(std::ostream &output) const {
  from.print(output);
  output << " flows to ";
  into.print(output);
  output << "\n";
}

Flow Flow::specializeFieldsTo(const FlowValue &value) const {
  assert(from.fieldsMatchOrContain(value));
  assert(from.indirect_level == value.indirect_level);

  /* how many indirections to examine */
  auto levelsDeep = std::min(from.indirect_level, value.indirect_level) + 1;

  auto newInto = FlowValue(into.expr, into.indirect_level, into.field_selects,
                           into.is_escape);
  for (int32_t i = 0; i < levelsDeep; i++) {
    auto &valueSelects = value.field_selects[value.indirect_level - i];
    auto &fromSelects = from.field_selects[from.indirect_level - i];
    auto &intoSelects = newInto.field_selects[newInto.indirect_level - i];

    for (size_t j = 0; j < valueSelects.size(); j++) {
      if (j < fromSelects.size()) {
        assert(valueSelects[j] == fromSelects[j]);
      } else {
        /* if value has a field selector that from doesn't, add that selector in
         * the appropriate spot to into */
        intoSelects.push_back(valueSelects[j]);
      }
    }
  }

  return Flow(value, newInto);
}

Flow Flow::indirectionsFor(const FlowValue &value) const {
  assert(from.indirect_level <= value.indirect_level);

  if (from.indirect_level == value.indirect_level)
    return *this;

  auto numToAdd = value.indirect_level - from.indirect_level;

  auto newFrom = FlowValue(from.expr, from.indirect_level, from.field_selects,
                           from.is_escape);
  newFrom.indirect_level += numToAdd;
  for (auto i = 0; i < numToAdd; i++)
    newFrom.field_selects.emplace_back();

  auto newInto = FlowValue(into.expr, into.indirect_level, into.field_selects,
                           into.is_escape);
  newInto.indirect_level += numToAdd;
  for (auto i = 0; i < numToAdd; i++)
    newInto.field_selects.emplace_back();

  return Flow(newFrom, newInto);
}

bool Flow::contains(const FlowValue &value) const {
  /* a flow of %b doesn't contain %a */
  if (from.expr.val.id != value.expr.val.id)
    return false;
  /* a flow of **%a doesn't contain *%a */
  if (from.indirect_level > value.indirect_level)
    return false;

  /* construct a new flow with matching indirection level
   * ie -- flow of *%a contains **%a */
  const Flow &newFlow = value.indirect_level > from.indirect_level
                            ? indirectionsFor(value)
                            : *this;

  return newFlow.from.fieldsMatchOrContain(value);
}

Flow Flow::specializedTo(const FlowValue &value) const {
  assert(contains(value));

  return indirectionsFor(value).specializeFieldsTo(value);
}

template <class T>
static bool vectorContains(const std::vector<T> &vector, const T &value) {
  return std::find(vector.begin(), vector.end(), value) != vector.end();
}

void traceFlow(const FlowValue &value, const FlowList &flows,
               const std::function<void(const FlowValue &)> &func,
               std::vector<FlowValue> &visited) {
  for (auto &flow : flows) {
    if (flow.contains(value)) {
      auto specialized = flow.specializedTo(value);

      auto &newVal = specialized.into;

      if (vectorContains(visited, newVal))
        continue;
      visited.push_back(newVal);

      func(specialized.into);
      /* don't follow escaping flows */
      if (newVal.is_escape == EscapeType::NONE) {
        traceFlow(newVal, flows, func);
      }
    }
  }
}

void traceFlow(const FlowValue &value, const FlowList &flows,
               const std::function<void(const FlowValue &)> &func) {
  std::vector<FlowValue> visited;

  return traceFlow(value, flows, func, visited);
}

void traceFlowFindSpecialized(
    const FlowValue &value, const FlowValue &initValue, const FlowList &flows,
    const std::function<void(const FlowValue &)> &func,
    const std::function<void(const FlowValue &)> &specializedFlowFunc,
    std::vector<FlowValue> &visitedFlows,
    std::vector<FlowValue> &visitedSpecializations) {
  for (auto &flow : flows) {
    if (flow.contains(value)) {
      auto specialized = flow.specializedTo(value);

      auto &newVal = specialized.into;

      if (vectorContains(visitedFlows, newVal))
        continue;
      visitedFlows.push_back(newVal);

      func(newVal);
      /* don't follow escaping flows */
      if (newVal.is_escape == EscapeType::NONE) {
        traceFlowFindSpecialized(newVal, flows, func, specializedFlowFunc);
      }
    } else if (flow.from.expr.val.id == value.expr.val.id) {
      /* flow is more specialized than value, adjust to initValue and call
       * specializedFlowFunc */
      auto generalFlow = Flow(value, initValue);
      auto specialFlow = generalFlow.specializedTo(flow.from);

      auto &specialVal = specialFlow.into;

      if (vectorContains(visitedSpecializations, specialVal))
        continue;
      visitedSpecializations.push_back(specialVal);

      specializedFlowFunc(specialVal);
    }
  }
}

void traceFlowFindSpecialized(
    const FlowValue &value, const FlowList &flows,
    const std::function<void(const FlowValue &)> &func,
    const std::function<void(const FlowValue &)> &specializedFlowFunc) {
  std::vector<FlowValue> visitedFlows;
  std::vector<FlowValue> visitedSpecializations;
  /* at root of search, value and initValue are the same */
  traceFlowFindSpecialized(value, value, flows, func, specializedFlowFunc,
                           visitedFlows, visitedSpecializations);
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

bool operator==(const FlowValue &lhs, const FlowValue &rhs) {
  return lhs.expr.val.id == rhs.expr.val.id &&
         lhs.indirect_level == rhs.indirect_level &&
         lhs.field_selects == rhs.field_selects;
}

bool operator!=(const FlowValue &lhs, const FlowValue &rhs) {
  return !(lhs == rhs);
}

int EscapeAnalysisPass::visitFunctionDeclare(FunctionDeclare &instruct,
                                             const EscapeAnalysisState &state) {
  // if the function has already been visited, skip it
  if (instruct.flow_state == FunctionEscapeAnalysisState::VISITED) {
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
    for (auto &scope : instruct.val.sourceName) {
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

  /* trace flows for each allocation and mark escaping */
  visitAllocations(instruct.body, [this, &flows](Allocation &alloc) {
    /* trace flow of address of allocation */
    std::vector<std::vector<int32_t>> selects;
    selects.emplace_back();
    auto value = FlowValue(alloc, 0, selects);

    traceFlow(value, flows, [this, &alloc](const FlowValue &dst) {
      /* if value is already marked escaping, don't mark again */
      if (alloc.allocType != AllocationType::HEAP &&
          alloc.allocType != AllocationType::ARG_HEAP) {
        if (dst.is_escape != EscapeType::NONE) {
          /* mark as heap allocated */
          if (AllocationTypeIsArg(alloc.allocType)) {
            alloc.allocType = AllocationType::ARG_HEAP;
          } else {
            alloc.allocType = AllocationType::HEAP;
          }
          /* print debug info */
          if (print_escapes) {
            output << "%" << alloc.val.sourceName[0] << " escapes\n";
          }
        }
      }
    });
  });

  // calculate flows needed for function metadata
  // these are flows where the source is an argument and the dest is another
  // arg, escape, or return
  for (auto &arg : instruct.argAllocs) {
    std::vector<std::vector<int32_t>> selects;
    selects.emplace_back();
    auto value = FlowValue(arg.get(), 0, selects);
    calculateFunctionFlowMetadata(instruct.flow_metadata, instruct.argAllocs,
                                  flows, value);
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
    ir::FlowList &functionFlows,
    const std::vector<std::reference_wrapper<Allocation>> &funcArgs,
    const ir::FlowList &flows, const FlowValue &srcValue) {
  traceFlowFindSpecialized(
      srcValue, flows,
      [&functionFlows, funcArgs, flows, srcValue](const FlowValue &dstValue) {
        /* srcValue flows to dstValue
         * only add flow if it involves an arg->arg, arg->escape or arg->return
         * flow */
        if (argsContain(funcArgs, srcValue.expr) &&
            (dstValue.is_escape == EscapeType::RETURN ||
             dstValue.is_escape == EscapeType::OTHER ||
             argsContain(funcArgs, dstValue.expr))) {
          auto flow = Flow(srcValue, dstValue);
          /* if flow doesn't have any indirections on source, add one to account
           * for implicit copy when args are passed */
          if (srcValue.indirect_level < 1) {
            auto newSelects = srcValue.field_selects;
            newSelects.emplace_back();
            auto newSrc = FlowValue(srcValue.expr, srcValue.indirect_level + 1,
                                    newSelects, srcValue.is_escape);
            functionFlows.emplace_back(flow.specializedTo(newSrc));
          } else {
            functionFlows.emplace_back(flow);
          }
        }
      },
      [this, &functionFlows, funcArgs, flows](const FlowValue &specialValue) {
        /* trace specialized value */
        calculateFunctionFlowMetadata(functionFlows, funcArgs, flows,
                                      specialValue);
      });
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
  /* if we can get escape information for function, use that */
  if (instruct.function.hasFlowMetadata()) {
    visitInstruction(instruct.function, EscapeAnalysisState(false, nullptr));
    /* TODO: get flow values for each arg instead of this */
    for (auto &argWrapper : instruct.arguments) {
      auto &arg = argWrapper.get();
      auto argFrom = FlowValue::getFlowFromExpression(arg);
      auto argTo = FlowValue::getFlowForDereference(arg);
      if (argFrom != argTo) {
        auto flow = Flow(argFrom, argTo);
        if (!flow.isEmpty())
          state.curFlowList->push_back(flow);
      }
    }
    /* add arg flows to flow list */
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

      state.curFlowList->emplace_back(Flow(from, into));
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
                       bool print_escapes, bool print_func_flow_metadata,
                       std::ostream &output) {
  auto pass = EscapeAnalysisPass(print_flows, print_escapes,
                                 print_func_flow_metadata, output);
  auto globalFlows = ir::FlowList();
  pass.visitInstructions(ir, EscapeAnalysisState(false, &globalFlows));
}

} // namespace ovid::ir