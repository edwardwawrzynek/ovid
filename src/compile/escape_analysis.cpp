#include "escape_analysis.hpp"

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
      (globalAlloc->allocType == AllocationType::UNRESOLVED_GLOBAL ||
       globalAlloc->allocType == AllocationType::STATIC)) {
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
    std::vector<std::vector<int32_t>> &field_selects) {
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

std::vector<const ast::Type *> FlowValue::getFlowingTypes() {
  auto baseType =
      getFlowingTypesFromType(expr.type.get(), indirect_level, field_selects);
  for (auto &type : baseType) {
    type = applyFieldSelectToType(type, field_selects[indirect_level]);
  }
  return baseType;
}

bool FlowValue::isEmpty() {
  auto flowingTypes = getFlowingTypes();

  return flowingTypes.empty();
}

FlowValue FlowValue::getFlowFromExpressionWithoutCopy(Expression &expr) {
  auto fieldSelect = dynamic_cast<FieldSelect *>(&expr);
  if (fieldSelect != nullptr) {
    auto value = getFlowFromExpressionWithoutCopy(fieldSelect->expr);
    /* add field select onto value */
    value.field_selects[value.field_selects.size() - 1].push_back(
        fieldSelect->field_index);

    return value;
  }
  auto deref = dynamic_cast<Dereference *>(&expr);
  if (deref != nullptr) {
    auto value = getFlowFromExpressionWithoutCopy(deref->expr);
    /* add dereference (with no field select) */
    value.indirect_level++;
    value.field_selects.emplace_back();

    return value;
  }
  auto addr = dynamic_cast<Address *>(&expr);
  if (addr != nullptr) {
    auto value = getFlowFromExpressionWithoutCopy(addr->expr);
    /* remove most recent dereference
     * this may produce a value with indirect_level -1, which should be handled
     * by the implicit dereference added on copy */
    assert(value.indirect_level >= 0);
    value.indirect_level--;
    /* field selection is lost when address of field is taken
     * ie -- if &(%a.field), &%a flows, not just &%a.field */
    value.field_selects.pop_back();

    return value;
  }

  /* create value with no field select */
  std::vector<std::vector<int32_t>> field_selects;
  field_selects.emplace_back();
  auto value = FlowValue(expr, 0, field_selects);

  return value;
}

void FlowValue::print(std::ostream &output) {
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

  for (int32_t i = indirect_level; i >= 0; i--) {
    for (auto &select : field_selects[i]) {
      output << "." << select;
    }

    if (i > 0)
      output << ")";
  }
}

FlowValue FlowValue::getFlowFromExpression(Expression &expr) {
  auto value = getFlowFromExpressionWithoutCopy(expr);
  value.indirect_level++;
  value.field_selects.emplace_back();

  return value;
}

Flow::Flow(const FlowValue &from, const FlowValue &into)
    : from(from), into(into) {}

bool Flow::isEmpty() {
  /* empty-ness of both values should match, as they should have the same types
   * flowing */
  assert(from.isEmpty() == into.isEmpty());

  return from.isEmpty();
  return false;
}

void Flow::print(std::ostream &output) {
  from.print(output);
  output << " flows to ";
  into.print(output);
  output << "\n";
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

int EscapeAnalysisPass::visitFunctionDeclare(FunctionDeclare &instruct,
                                             const EscapeAnalysisState &state) {
  // pointer flow for this function
  AliasFlowList flows;
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
                       std::ostream &output) {
  auto pass = EscapeAnalysisPass(print_flows, output);
  auto globalFlows = ir::AliasFlowList();
  pass.visitInstructions(ir, EscapeAnalysisState(false, &globalFlows));
}

} // namespace ovid::ir