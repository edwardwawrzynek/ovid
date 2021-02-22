#include "generics_pass.hpp"
#include "generics.hpp"

namespace ovid::ir {

GenericsPassState::GenericsPassState(
    bool is_specializing, const ast::FormalTypeParameterList &formal_params,
    const ast::TypeList &actual_params, GenericSubstitutions &subs,
    BasicBlockList *curBasicBlockList, InstructionList *curInstructionList,
    InstructionList *rootInstructionList)
    : is_specializing(is_specializing), formal_params(formal_params),
      actual_params(actual_params), curBasicBlockList(curBasicBlockList),
      curInstructionList(curInstructionList),
      rootInstructionList(rootInstructionList), subs(subs) {}

GenericsPassState
GenericsPassState::withIsSpecializing(bool specializing) const {
  return GenericsPassState(specializing, formal_params, actual_params, subs,
                           curBasicBlockList, curInstructionList,
                           rootInstructionList);
}

void GenericSubstitutions::addBasicBlock(uint64_t old_id, BasicBlock *newBB) {
  assert(bbSubs.count(old_id) == 0);
  bbSubs[old_id] = newBB;
}

void GenericSubstitutions::addExpression(uint64_t old_id, Expression *newExpr) {
  assert(exprSubs.count(old_id) == 0);
  exprSubs[old_id] = newExpr;
}

void GenericSubstitutions::addExpression(Expression &old_expr,
                                         Expression *newExpr) {
  addExpression(old_expr.val.id.id, newExpr);
}

void GenericSubstitutions::addGenericExpression(
    uint64_t old_id, GenericExpression *newGenericExpr) {
  assert(genericExprSubs.count(old_id) == 0);
  genericExprSubs[old_id] = newGenericExpr;
}

void GenericSubstitutions::addGenericExpression(
    GenericExpression &old_expr, GenericExpression *newGenericExpr) {
  addGenericExpression(old_expr.id.id, newGenericExpr);
}

BasicBlock *GenericSubstitutions::useBasicBlock(const BasicBlock *old) {
  assert(bbSubs.count(old->id) > 0);
  return bbSubs[old->id];
}

Expression *GenericSubstitutions::hasExpressionId(uint64_t id) {
  if (exprSubs.count(id) == 0) {
    if (parent == nullptr) {
      return nullptr;
    }
    return parent->hasExpressionId(id);
  } else {
    return exprSubs[id];
  }
}

Expression *GenericSubstitutions::useExpressionId(uint64_t id) {
  auto res = hasExpressionId(id);
  assert(res != nullptr);
  return res;
}

Expression *GenericSubstitutions::hasExpression(Expression *old) {
  return hasExpressionId(old->val.id.id);
}

Expression *GenericSubstitutions::useExpression(Expression *old) {
  return useExpressionId(old->val.id.id);
}

GenericExpression *GenericSubstitutions::hasGenericExpressionId(uint64_t id) {
  if (genericExprSubs.count(id) == 0) {
    if (parent == nullptr)
      return nullptr;

    return parent->hasGenericExpressionId(id);
  } else {
    return genericExprSubs[id];
  }
}

GenericExpression *GenericSubstitutions::useGenericExpressionId(uint64_t id) {
  auto res = hasGenericExpressionId(id);
  assert(res != nullptr);
  return res;
}

GenericExpression *
GenericSubstitutions::useGenericExpression(GenericExpression *old) {
  return useGenericExpressionId(old->id.id);
}

GenericExpression *
GenericSubstitutions::hasGenericExpression(GenericExpression *old) {
  return hasGenericExpressionId(old->id.id);
}

Instruction *GenericSubstitutions::hasInstruction(Instruction *old) {
  auto expr = dynamic_cast<Expression *>(old);
  if (expr) {
    return hasExpression(expr);
  }
  auto generic_expr = dynamic_cast<GenericExpression *>(old);
  if (generic_expr) {
    return hasGenericExpression(generic_expr);
  }
  return nullptr;
}

Instruction *GenericSubstitutions::useInstruction(Instruction *old) {
  auto res = hasInstruction(old);
  assert(res != nullptr);
  return res;
}

Instruction *GenericSubstitutions::hasInstructionId(uint64_t id) {
  auto expr = hasExpressionId(id);
  if (expr != nullptr) {
    return expr;
  }
  auto generic = hasGenericExpressionId(id);
  if (generic != nullptr) {
    return generic;
  }
  return nullptr;
}

Instruction *GenericSubstitutions::useInstructionId(uint64_t id) {
  auto res = hasInstructionId(id);
  assert(res != nullptr);
  return res;
}

Expression *
GenericSpecializations::getSpecialization(GenericExpression &generic_expr,
                                          const ast::TypeList &actual_params) {
  if (specializations.count(generic_expr.id.id) == 0) {
    return nullptr;
  } else {
    auto &entries = specializations[generic_expr.id.id];
    for (auto &entry : entries) {
      if (ast::typeListEqual(entry.first, actual_params)) {
        return entry.second;
      }
    }

    return nullptr;
  }
}

void GenericSpecializations::addSpecialization(uint64_t generic_instr_id,
                                               ast::TypeList actual_params,
                                               Expression *specialized_instr) {
  // if generic function already has entry, add to it
  if (specializations.count(generic_instr_id) > 0) {
    auto &entries = specializations[generic_instr_id];
    entries.emplace_back(
        std::pair(std::move(actual_params), specialized_instr));
  }
  // otherwise create a new entry
  else {
    std::vector<std::pair<ast::TypeList, Expression *>> entries;
    entries.emplace_back(
        std::pair(std::move(actual_params), specialized_instr));
    specializations[generic_instr_id] = entries;
  }
}

std::shared_ptr<ast::Type>
GenericsPass::fixType(const std::shared_ptr<ast::Type> &type,
                      const GenericsPassState &state) {
  if (state.formal_params.empty() && state.actual_params.empty()) {
    return type;
  } else {
    return ast::TypeConstructorPass::constructType(
        type, state.formal_params, state.actual_params, active_scopes, errorMan,
        std::vector<std::string>(), std::vector<std::string>());
  }
}

Value GenericsPass::newValue(const Value &old_val,
                             const GenericsPassState &state) {
  if (!old_val.id.hasSourceName) {
    return Value();
  } else {
    ast::TypeList new_type_params;
    for (auto &param : old_val.id.typeParams) {
      new_type_params.push_back(fixType(param, state));
    }
    return Value(old_val.id.sourceName, std::move(new_type_params));
  }
}

int GenericsPass::addExpr(std::unique_ptr<Expression> expr, Expression &old,
                          const GenericsPassState &state) {
  state.subs.addExpression(old, expr.get());
  state.curInstructionList->push_back(std::move(expr));
  return 0;
}

int GenericsPass::visitImpl(Impl &instruct, const GenericsPassState &state) {
  assert(!state.is_specializing);
  if (global_subs.hasExpression(&instruct)) {
    return 0;
  }

  // construct new impl node
  auto impl = std::make_unique<Impl>(
      instruct.loc, newValue(instruct.val, state), InstructionList(),
      instruct.header, fixType(instruct.type, state));
  global_subs.addExpression(instruct, impl.get());
  // visit component function decls
  for (auto &fn_decl : instruct.fn_decls) {
    visitInstruction(*fn_decl, state);
  }

  state.curInstructionList->push_back(std::move(impl));
  return 0;
}

bool GenericsPass::fnDeclImplNotInState(Instruction *fnDeclImpl,
                                        const GenericsPassState &state) {
  if (fnDeclImpl == nullptr) {
    return false;
  } else {
    visitInstruction(*fnDeclImpl, state.withIsSpecializing(false));
    auto newImplBody =
        dynamic_cast<Impl *>(state.subs.useInstruction(fnDeclImpl));
    assert(newImplBody != nullptr);
    auto implRootInstrList = &newImplBody->fn_decls;
    return state.rootInstructionList != implRootInstrList;
  }
}

GenericsPassState GenericsPass::withFnDeclImpl(Instruction *fnDeclImpl,
                                               const GenericsPassState &state) {
  assert(fnDeclImpl != nullptr);
  visitInstruction(*fnDeclImpl, state.withIsSpecializing(false));
  auto newImplBody =
      dynamic_cast<Impl *>(state.subs.useInstruction(fnDeclImpl));
  assert(newImplBody != nullptr);
  auto implRootInstrList = &newImplBody->fn_decls;
  assert(implRootInstrList != nullptr);

  return GenericsPassState(state.is_specializing, state.formal_params,
                           state.actual_params, state.subs,
                           state.curBasicBlockList, state.curInstructionList,
                           implRootInstrList);
}

int GenericsPass::visitFunctionDeclare(FunctionDeclare &instruct,
                                       const GenericsPassState &state) {
  assert(!state.is_specializing);
  if (global_subs.hasExpression(&instruct)) {
    return 0;
  }

  // if this function is inside an impl, make sure state.rootInstructionList
  // matches it
  if (fnDeclImplNotInState(instruct.impl, state)) {
    return visitFunctionDeclare(instruct, withFnDeclImpl(instruct.impl, state));
  }
  // construct new function declaration
  auto funcType = std::dynamic_pointer_cast<ast::NamedFunctionType>(
      fixType(instruct.type, state));
  assert(funcType != nullptr);
  auto funcDeclare = std::make_unique<FunctionDeclare>(
      instruct.loc, newValue(instruct.val, state), funcType,
      std::vector<std::reference_wrapper<Allocation>>(), BasicBlockList(),
      instruct.is_public, global_subs.hasInstruction(instruct.impl));
  global_subs.addExpression(instruct, funcDeclare.get());
  // create new state for body
  // rootInstructionList is set back to ir global root b/c if we visit a declare
  // inside this function, that declare may not be in the current impl scope
  auto subs = GenericSubstitutions(&global_subs);
  auto bodyState =
      GenericsPassState(false, state.formal_params, state.actual_params, subs,
                        &funcDeclare->body, nullptr, globalRootInstructionList);
  visitBasicBlockList(instruct.body, bodyState, instruct.argAllocs,
                      funcDeclare->argAllocs);

  // insert function declaration
  state.rootInstructionList->push_back(std::move(funcDeclare));
  return 0;
}

int GenericsPass::visitGenericImpl(GenericImpl &instruct,
                                   const GenericsPassState &state) {
  // TODO: component fn_decl.impl point to instruct, but we need to redirect to
  // the new impl somehow (ie when visiting function declares, it sets
  // rootInstructionList to fn_decl.impl.fn_decls). We can't use global_subs,
  // since the sub only lasts for this specialization. Perhaps a new flag in
  // state?
  // TODO: visitGenericFunctionDeclare needs to be extended to allow generating
  // new GenericFunctionDeclare's with just impl bound types replaced. (Perhaps
  // a flag in state to indicate impl specialization?)
  // TODO: the impl's symbol table is used later (looking up methods, name
  // mangling, etc). We need to generate a specialized copy of the symbol table
  // and use that instead. Possibly visitGenericImpl should loop over fn_decl's
  // and explicitly handle prototypes + new symbol table construction (and just
  // delegate basic block visiting to existing methods). (as long as
  // substitutions were added for function declares and state.subs included them
  // when visiting basic blocks, this should work. Specializing component
  // generic functions may be tricky -- we would need substitutions to stay
  // visible inside of them, but not into other generic functions beyond the
  // impl boundry).

  // if we aren't specializing this impl, don't do anything to it
  if (!state.is_specializing) {
    // add dummy sub so this impl doesn't get repeatedly visited for non
    // specialization
    if (!global_subs.hasGenericExpression(&instruct)) {
      global_subs.addGenericExpression(instruct, &instruct);
    }
    return 0;
  }
  // if state.formal_params aren't set, set them properly
  if (!instruct.header->type_params.empty() && state.formal_params.empty()) {
    auto newState =
        GenericsPassState(state.is_specializing, instruct.header->type_params,
                          state.actual_params, state.subs, nullptr, nullptr,
                          state.rootInstructionList);
    return visitGenericImpl(instruct, newState);
  }
  // create specialized impl node
  // create a new header that is specialized
  auto new_header = std::make_shared<ast::ImplHeader>(
      ast::FormalTypeParameterList(), fixType(instruct.header->type, state));
  auto impl = std::make_unique<ir::Impl>(
      instruct.loc, ir::Value(), InstructionList(), instruct.header,
      fixType(instruct.type_construct->getFormalBoundType(), state));
  new_header->ir_decl = impl.get();

  // create a new, specialized scope table for the impl
  // find old scope table's parent (if this impl has no entries, then we don't
  // need a new scope table -- no symbols would be contained in it anyways)
  ScopeTable<Symbol> *old_parent =
      instruct.fn_decls.empty() ? nullptr
                                : getInstrId(instruct.fn_decls[0].get())
                                      .sourceName->parent_table->getParent();
  assert(instruct.fn_decls.empty() || old_parent != nullptr);
  // TODO: ownership of new scope table??
  auto new_scope =
      new ScopeTable<Symbol>(true, old_parent, "", false, -1, new_header);

  specializations.addSpecialization(instruct.id.id, state.actual_params,
                                    impl.get());

  // create substitutions for fn_decl bodies
  auto body_subs = GenericSubstitutions(&global_subs);

  // visit all component function declarations and create new specialized decl's
  // don't visit bodies yet -- we need all fn_decl substitutions before we visit
  // bodies
  for (auto &child : instruct.fn_decls) {
    // recreate function declaration
    auto fn_decl = dynamic_cast<FunctionDeclare *>(child.get());
    auto generic_fn_decl = dynamic_cast<GenericFunctionDeclare *>(child.get());
    // TODO: don't duplicate all this code for FunctionDeclare and
    // GenericFunctionDeclare
    if (fn_decl != nullptr) {
      // create entry in symbol table
      auto &fn_name = fn_decl->val.id.sourceName->name;
      auto &old_sym = *instruct.getFnDecl(fn_name)->val.id.sourceName;
      auto sym = std::make_shared<Symbol>(instruct.loc, old_sym.is_public, true,
                                          old_sym.is_mut, old_sym.is_global);
      // create new function declare
      auto fn_type = std::dynamic_pointer_cast<ast::NamedFunctionType>(
          fixType(fn_decl->type, state));
      assert(fn_type != nullptr);
      auto new_fn_decl = std::make_unique<ir::FunctionDeclare>(
          fn_decl->loc, ir::Value(sym), fn_type,
          std::vector<std::reference_wrapper<Allocation>>(), BasicBlockList(),
          fn_decl->is_public, impl.get());
      // make symbol point to new function declare
      sym->ir_decl = IrDecl(new_fn_decl.get(), impl.get());
      new_scope->addSymbol(fn_name, sym);
      // add function declare to impl
      body_subs.addExpression(*fn_decl, new_fn_decl.get());
      // new -> new global sub is needed to prevent duplicate visiting of impl
      global_subs.addExpression(*new_fn_decl, new_fn_decl.get());
      impl->fn_decls.push_back(std::move(new_fn_decl));
    } else if (generic_fn_decl != nullptr) {
      // TODO
      assert(false);
    } else {
      assert(false);
    }
  }

  // visit the bodies of component fn_decls
  assert(instruct.fn_decls.size() == impl->fn_decls.size());
  for (size_t i = 0; i < impl->fn_decls.size(); i++) {
    // get refs to basic block list + arg allocs for fn/generic fn
    BasicBlockList *new_bb_list = nullptr;
    BasicBlockList *old_bb_list = nullptr;
    std::vector<std::reference_wrapper<Allocation>> *old_arg_allocs = nullptr;
    std::vector<std::reference_wrapper<Allocation>> *new_arg_allocs = nullptr;
    auto fn_decl = dynamic_cast<FunctionDeclare *>(impl->fn_decls[i].get());
    auto generic_fn_decl =
        dynamic_cast<GenericFunctionDeclare *>(impl->fn_decls[i].get());
    if (fn_decl != nullptr) {
      auto old = dynamic_cast<FunctionDeclare *>(instruct.fn_decls[i].get());
      new_bb_list = &fn_decl->body;
      old_bb_list = &old->body;
      new_arg_allocs = &fn_decl->argAllocs;
      old_arg_allocs = &old->argAllocs;
    } else {
      assert(generic_fn_decl != nullptr);
      auto old =
          dynamic_cast<GenericFunctionDeclare *>(instruct.fn_decls[i].get());
      new_bb_list = &generic_fn_decl->body;
      old_bb_list = &old->body;
      new_arg_allocs = &generic_fn_decl->argAllocs;
      old_arg_allocs = &old->argAllocs;
    }
    // visit function body
    auto body_state = GenericsPassState(
        false, state.formal_params, state.actual_params, body_subs, new_bb_list,
        nullptr, globalRootInstructionList);
    visitBasicBlockList(*old_bb_list, body_state, *old_arg_allocs,
                        *new_arg_allocs);
  }

  state.rootInstructionList->push_back(std::move(impl));

  return 0;
}

int GenericsPass::visitGenericFunctionDeclare(GenericFunctionDeclare &instruct,
                                              const GenericsPassState &state) {
  // if we aren't specializing this function, don't visit it
  if (!state.is_specializing) {
    if (!global_subs.hasGenericExpression(&instruct)) {
      global_subs.addGenericExpression(instruct, &instruct);
    }
    return 0;
  }
  // if the function declare is in an impl, visit that first
  if (instruct.impl != nullptr && !global_subs.useInstruction(instruct.impl)) {
    auto implState = GenericsPassState(false, state.formal_params,
                                       state.actual_params, state.subs, nullptr,
                                       nullptr, state.rootInstructionList);
    visitInstruction(*instruct.impl, implState);
    assert(global_subs.hasInstruction(instruct.impl) != nullptr);
  }
  // if the function declare is in an impl and curRootInstructionList isn't set
  // to the body of the impl, set it
  if (fnDeclImplNotInState(instruct.impl, state)) {
    return visitGenericFunctionDeclare(instruct,
                                       withFnDeclImpl(instruct.impl, state));
  }
  // if formal params aren't set, set them to match the function
  if (!instruct.type_construct->getFormalTypeParameters().empty() &&
      state.formal_params.empty()) {
    auto newState = GenericsPassState(
        state.is_specializing,
        instruct.type_construct->getFormalTypeParameters(), state.actual_params,
        state.subs, nullptr, nullptr, state.rootInstructionList);
    return visitGenericFunctionDeclare(instruct, newState);
  }

  // construct specialized function type
  auto constructedType = ast::TypeConstructorPass::constructTypeConstructor(
      instruct.type_construct, state.actual_params, active_scopes, errorMan,
      std::vector<std::string>(), std::vector<std::string>());
  auto funcType =
      std::dynamic_pointer_cast<ast::NamedFunctionType>(constructedType);
  assert(funcType != nullptr);
  // generate empty function declaration and insert it into specializations
  auto funcDeclare = std::make_unique<FunctionDeclare>(
      instruct.loc, Value(instruct.id.sourceName, state.actual_params),
      funcType, std::vector<std::reference_wrapper<Allocation>>(),
      BasicBlockList(), instruct.is_public,
      global_subs.hasInstruction(instruct.impl));
  specializations.addSpecialization(instruct.id.id, state.actual_params,
                                    funcDeclare.get());
  // visit the body of the function
  auto bodySubs = GenericSubstitutions(&global_subs);
  auto bodyState = GenericsPassState(
      state.is_specializing, state.formal_params, state.actual_params, bodySubs,
      &funcDeclare->body, nullptr, globalRootInstructionList);
  visitBasicBlockList(instruct.body, bodyState, instruct.argAllocs,
                      funcDeclare->argAllocs);

  // insert function declaration
  state.rootInstructionList->push_back(std::move(funcDeclare));
  return 0;
}

int GenericsPass::visitBasicBlockList(
    BasicBlockList &basicBlockList, const GenericsPassState &state,
    const std::vector<std::reference_wrapper<Allocation>> &oldArgAllocs,
    std::vector<std::reference_wrapper<Allocation>> &newArgAllocs) {
  // b/c basic blocks forward reference each other, generate empty blocks first,
  // then visit bodies
  assert(state.curBasicBlockList->empty());
  size_t argAlloc_index = 0;
  for (auto &block : basicBlockList) {
    auto newBlock = std::make_unique<BasicBlock>(block->loc);
    state.subs.addBasicBlock(block->id, newBlock.get());
    state.curBasicBlockList->push_back(std::move(newBlock));
  }
  for (size_t i = 0; i < basicBlockList.size(); i++) {
    auto bodyState = GenericsPassState(
        state.is_specializing, state.formal_params, state.actual_params,
        state.subs, state.curBasicBlockList,
        &(*state.curBasicBlockList)[i]->body, state.rootInstructionList);
    for (auto &child : basicBlockList[i]->body) {
      visitInstruction(*child, bodyState);
      // check if child is in oldArgAllocs, and, if so, add its visited version
      // to newArgAllocs
      if (argAlloc_index >= oldArgAllocs.size())
        continue;
      auto childAlloc = dynamic_cast<Allocation *>(child.get());
      if (childAlloc != nullptr) {
        if (oldArgAllocs[argAlloc_index].get().val == childAlloc->val) {
          // add new allocation to newArgAllocs
          // new allocation is the last inserted instruction in
          // bodyState.curInstructionList
          auto newChild =
              (*bodyState
                    .curInstructionList)[bodyState.curInstructionList->size() -
                                         1]
                  .get();
          auto newAlloc = dynamic_cast<Allocation *>(newChild);
          assert(newAlloc != nullptr);
          newArgAllocs.emplace_back(*newAlloc);
          argAlloc_index++;
        }
      }
    }
  }
  assert(argAlloc_index == oldArgAllocs.size());
  return 0;
}

int GenericsPass::visitImplFnExtract(Select &instruct,
                                     const GenericsPassState &state) {
  // TODO: support typeclass impl resolving (instruct.impl may not be an Impl,
  // so we can't just visit the old node)

  // TODO: if impl is a static Impl node, we can't use its substitution (b/c we
  // may be inside the impl and visitImpl doesn't generate headers before
  // generating bodies). This is non ideal -- we should be able to use any nodes
  // substitution
  auto impl = dynamic_cast<Impl *>(&instruct.impl);
  if (impl == nullptr) {
    impl = dynamic_cast<Impl *>(state.subs.useExpression(&instruct.impl));
  }
  assert(impl != nullptr);
  auto func = impl->getFnDecl(instruct.method);
  visitInstruction(*func, state.withIsSpecializing(false));
  auto newFunc = global_subs.useExpression(func);
  state.subs.addExpression(instruct, newFunc);

  return 0;
}

int GenericsPass::visitImplGenericFnExtract(GenericSelect &instruct,
                                            const GenericsPassState &state) {
  // TODO: support typeclass impl resolving (instruct.impl may not be an Impl,
  // so we can't just visit the old node)
  auto impl = dynamic_cast<Impl *>(&instruct.impl);
  assert(impl != nullptr);
  // don't visit the function (b/c it will later be specialized)
  auto func = impl->getGenericFnDecl(instruct.method);
  state.subs.addGenericExpression(instruct, func);

  return 0;
}

int GenericsPass::visitBasicBlock(BasicBlock &instruct,
                                  const GenericsPassState &state) {
  // should be handled by visitBasicBlockList
  assert(false);
}

int GenericsPass::visitAllocation(Allocation &instruct,
                                  const GenericsPassState &state) {
  auto newInstruct = std::make_unique<Allocation>(
      instruct.loc, newValue(instruct.val, state),
      fixType(instruct.type, state), instruct.allocType);
  return addExpr(std::move(newInstruct), instruct, state);
}

int GenericsPass::visitGlobalAllocation(GlobalAllocation &instruct,
                                        const GenericsPassState &state) {
  assert(!state.is_specializing);
  auto newInstruct = std::make_unique<GlobalAllocation>(
      instruct.loc, newValue(instruct.val, state),
      fixType(instruct.type, state),
      *state.subs.useExpression(&instruct.initial_val), instruct.symbol);

  global_subs.addExpression(instruct, newInstruct.get());
  state.curInstructionList->push_back(std::move(newInstruct));
  return 0;
}

#define LITERAL_VISIT(instructType)                                            \
  auto newInstruct = std::make_unique<instructType>(                           \
      instruct.loc, newValue(instruct.val, state), instruct.type,              \
      instruct.value);                                                         \
  return addExpr(std::move(newInstruct), instruct, state);

int GenericsPass::visitIntLiteral(IntLiteral &instruct,
                                  const GenericsPassState &state) {
  LITERAL_VISIT(IntLiteral);
}

int GenericsPass::visitFloatLiteral(FloatLiteral &instruct,
                                    const GenericsPassState &state) {
  LITERAL_VISIT(FloatLiteral);
}

int GenericsPass::visitBoolLiteral(BoolLiteral &instruct,
                                   const GenericsPassState &state) {
  auto newInstruct = std::make_unique<BoolLiteral>(
      instruct.loc, newValue(instruct.val, state), instruct.value);
  return addExpr(std::move(newInstruct), instruct, state);
}

int GenericsPass::visitTupleLiteral(TupleLiteral &instruct,
                                    const GenericsPassState &state) {
  std::vector<std::reference_wrapper<Expression>> exprs;
  for (auto &expr : instruct.exprs) {
    exprs.emplace_back(*state.subs.useExpression(&expr.get()));
  }
  auto newInstruct = std::make_unique<TupleLiteral>(
      instruct.loc, newValue(instruct.val, state), std::move(exprs),
      fixType(instruct.type, state));
  return addExpr(std::move(newInstruct), instruct, state);
}

int GenericsPass::visitFunctionCall(FunctionCall &instruct,
                                    const GenericsPassState &state) {
  std::vector<std::reference_wrapper<Expression>> args;
  for (auto &expr : instruct.arguments) {
    args.emplace_back(*state.subs.useExpression(&expr.get()));
  }
  auto funcExpr = state.subs.hasExpression(&instruct.function);
  if (funcExpr == nullptr) {
    visitInstruction(instruct.function, state.withIsSpecializing(false));
    funcExpr = state.subs.useExpression(&instruct.function);
  }

  auto newInstruct = std::make_unique<FunctionCall>(
      instruct.loc, newValue(instruct.val, state), *funcExpr, std::move(args),
      fixType(instruct.type, state));
  return addExpr(std::move(newInstruct), instruct, state);
}

int GenericsPass::visitBuiltinOperator(BuiltinOperator &instruct,
                                       const GenericsPassState &state) {
  auto newInstruct = std::make_unique<BuiltinOperator>(
      instruct.loc, newValue(instruct.val, state), instruct.opType,
      fixType(instruct.type, state));
  return addExpr(std::move(newInstruct), instruct, state);
}

#define SINGLE_EXPR_OP_VISIT(instructType)                                     \
  auto newInstruct = std::make_unique<instructType>(                           \
      instruct.loc, newValue(instruct.val, state),                             \
      *state.subs.useExpression(&instruct.expr),                               \
      fixType(instruct.type, state));                                          \
  return addExpr(std::move(newInstruct), instruct, state);

int GenericsPass::visitAddress(Address &instruct,
                               const GenericsPassState &state) {
  SINGLE_EXPR_OP_VISIT(Address);
}

int GenericsPass::visitDereference(Dereference &instruct,
                                   const GenericsPassState &state) {
  SINGLE_EXPR_OP_VISIT(Dereference);
}

int GenericsPass::visitBuiltinCast(BuiltinCast &instruct,
                                   const GenericsPassState &state) {
  SINGLE_EXPR_OP_VISIT(BuiltinCast);
}

int GenericsPass::visitFieldSelect(FieldSelect &instruct,
                                   const GenericsPassState &state) {
  auto newInstruct = std::make_unique<FieldSelect>(
      instruct.loc, newValue(instruct.val, state),
      *state.subs.useExpression(&instruct.expr), instruct.field_index,
      fixType(instruct.type, state));
  return addExpr(std::move(newInstruct), instruct, state);
}

int GenericsPass::visitSizeof(Sizeof &instruct,
                              const GenericsPassState &state) {
  auto newInstruct =
      std::make_unique<Sizeof>(instruct.loc, newValue(instruct.val, state),
                               fixType(instruct.sizeof_type, state));
  return addExpr(std::move(newInstruct), instruct, state);
}

int GenericsPass::visitStore(Store &instruct, const GenericsPassState &state) {
  auto newInstruct = std::make_unique<Store>(
      instruct.loc, *state.subs.useExpression(&instruct.storage),
      *state.subs.useExpression(&instruct.value));
  state.curInstructionList->push_back(std::move(newInstruct));

  return 0;
}

int GenericsPass::visitReturn(Return &instruct,
                              const GenericsPassState &state) {
  auto newInstruct = std::make_unique<Return>(
      instruct.loc, instruct.expr == nullptr
                        ? nullptr
                        : state.subs.useExpression(instruct.expr));
  state.curInstructionList->push_back(std::move(newInstruct));

  return 0;
}

int GenericsPass::visitJump(Jump &instruct, const GenericsPassState &state) {
  auto newInstruct = std::make_unique<Jump>(
      instruct.loc, *state.subs.useBasicBlock(&instruct.label));
  state.curInstructionList->push_back(std::move(newInstruct));

  return 0;
}

int GenericsPass::visitConditionalJump(ConditionalJump &instruct,
                                       const GenericsPassState &state) {
  auto newInstruct = std::make_unique<ConditionalJump>(
      instruct.loc, *state.subs.useBasicBlock(&instruct.true_label),
      *state.subs.useBasicBlock(&instruct.false_label),
      *state.subs.useExpression(&instruct.condition));
  state.curInstructionList->push_back(std::move(newInstruct));

  return 0;
}

int GenericsPass::visitForwardIdentifier(ForwardIdentifier &instruct,
                                         const GenericsPassState &state) {
  // if the forward identifier is resolved, just use what it resolves to.
  // otherwise just copy the node
  if (instruct.symbol_ref->ir_decl.instr != nullptr) {
    auto ir_decl_expr =
        dynamic_cast<Expression *>(instruct.symbol_ref->ir_decl.instr);
    assert(ir_decl_expr != nullptr);

    auto newDecl = state.subs.hasExpression(ir_decl_expr);
    if (newDecl == nullptr) {
      visitInstruction(*ir_decl_expr, state.withIsSpecializing(false));
      newDecl = state.subs.useExpression(ir_decl_expr);
    }
    state.subs.addExpression(instruct, newDecl);

    return 0;
  } else {
    auto newInstruct = std::make_unique<ForwardIdentifier>(
        instruct.loc, newValue(instruct.val, state), instruct.symbol_ref,
        fixType(instruct.type, state));
    return addExpr(std::move(newInstruct), instruct, state);
  }
}

int GenericsPass::visitGenericForwardIdentifier(
    GenericForwardIdentifier &instruct, const GenericsPassState &state) {
  // generic forward identifiers should always resolve to an ir node.
  // ie: no external/native generic functions
  assert(instruct.symbol_ref->ir_decl.instr != nullptr);
  auto ir_decl_generic_expr =
      dynamic_cast<GenericExpression *>(instruct.symbol_ref->ir_decl.instr);
  assert(ir_decl_generic_expr != nullptr);

  state.subs.addGenericExpression(instruct, ir_decl_generic_expr);

  return 0;
}

int GenericsPass::visitSpecialize(Specialize &instruct,
                                  const GenericsPassState &state) {
  ast::TypeList actual_params;
  for (auto &param : instruct.actual_type_params) {
    actual_params.push_back(fixType(param, state));
  }

  auto generic_expr = state.subs.hasGenericExpression(&instruct.expr);
  if (generic_expr == nullptr) {
    visitInstruction(instruct.expr, state.withIsSpecializing(false));
    generic_expr = state.subs.useGenericExpression(&instruct.expr);
  }
  // if the needed specialization already exists, use it
  auto existingSpecial =
      specializations.getSpecialization(*generic_expr, actual_params);
  // otherwise, visit the generic expression and generate a new specialization
  if (existingSpecial == nullptr) {
    // let visited generic expression set formal params
    ast::FormalTypeParameterList empty_formal_params;
    auto subs = GenericSubstitutions(&global_subs);
    auto newState =
        GenericsPassState(true, empty_formal_params, actual_params, subs,
                          nullptr, nullptr, state.rootInstructionList);
    visitInstruction(*generic_expr, newState);
    // get generated specialization
    existingSpecial =
        specializations.getSpecialization(*generic_expr, actual_params);
    assert(existingSpecial != nullptr);
  }

  state.subs.addExpression(instruct, existingSpecial);
  return 0;
}

InstructionList GenericsPass::produceIR(ActiveScopes &scopes,
                                        ErrorManager &errorMan,
                                        const InstructionList &ir) {
  InstructionList res;
  auto pass = GenericsPass(scopes, errorMan, &res);
  ast::FormalTypeParameterList empty_formal_params;
  ast::TypeList empty_actual_params;
  auto subs = GenericSubstitutions(&pass.global_subs);
  auto state =
      GenericsPassState(false, empty_formal_params, empty_actual_params, subs,
                        nullptr, &res, &res);
  pass.visitInstructions(ir, state);
  return res;
}

} // namespace ovid::ir
