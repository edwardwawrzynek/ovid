#include "llvm_codegen.hpp"
#include "escape_analysis.hpp"
#include "name_mangle.hpp"

/* needs LLVM 10.0.0 or higher */
static_assert(LLVM_VERSION_MAJOR >= 10);

namespace ovid::ir {

LLVMCodegenPassState
LLVMCodegenPassState::withFunc(llvm::Function *func) const {
  return LLVMCodegenPassState(func);
}

llvm::Type *LLVMTypeGen::visitVoidType(ast::VoidType &type,
                                       const LLVMTypeGenState &state) {
  return llvm::Type::getVoidTy(llvm_context);
}

llvm::Type *LLVMTypeGen::visitBoolType(ast::BoolType &type,
                                       const LLVMTypeGenState &state) {
  return llvm::Type::getInt1Ty(llvm_context);
}

llvm::Type *LLVMTypeGen::visitIntType(ast::IntType &type,
                                      const LLVMTypeGenState &state) {
  return llvm::Type::getIntNTy(llvm_context, type.size);
}

llvm::Type *LLVMTypeGen::visitFloatType(ast::FloatType &type,
                                        const LLVMTypeGenState &state) {
  if (type.size == 32) {
    return llvm::Type::getFloatTy(llvm_context);
  } else if (type.size == 64) {
    return llvm::Type::getDoubleTy(llvm_context);
  } else {
    assert(false);
  }
}

llvm::Type *LLVMTypeGen::visitMutType(ast::MutType &type,
                                      const LLVMTypeGenState &state) {
  return visitType(*type.type, state);
}

llvm::Type *LLVMTypeGen::visitPointerType(ast::PointerType &type,
                                          const LLVMTypeGenState &state) {
  return llvm::PointerType::get(visitType(*type.type, state), 0);
}

llvm::Type *LLVMTypeGen::visitTupleType(ast::TupleType &type,
                                        const LLVMTypeGenState &state) {
  std::vector<llvm::Type *> types;
  size_t numFields = type.getNumFields();

  for (size_t i = 0; i < numFields; i++) {
    types.push_back(visitType(*type.getTypeOfField(i), state));
  }
  return llvm::StructType::get(llvm_context, types);
}

llvm::Type *LLVMTypeGen::visitStructType(ast::StructType &type,
                                         const LLVMTypeGenState &state) {
  if (type.llvm_type != nullptr)
    return type.llvm_type;
  // create struct type
  auto name = name_mangling::mangleType(type.type_alias.lock());
  auto structType = llvm::StructType::create(llvm_context, name);
  type.llvm_type = structType;
  // fill in body
  std::vector<llvm::Type *> types;
  for (auto &field : type.field_types) {
    types.push_back(visitType(*field, state));
  }
  structType->setBody(types);

  return structType;
}

llvm::Type *LLVMTypeGen::visitFunctionType(ast::FunctionType &type,
                                           const LLVMTypeGenState &state) {
  /* TODO (closure type) */
  assert(false);
}

llvm::Type *LLVMTypeGen::visitNamedFunctionType(ast::NamedFunctionType &type,
                                                const LLVMTypeGenState &state) {
  return visitFunctionType(type, state);
}

llvm::Type *LLVMTypeGen::visitType(ast::Type &type) {
  return visitType(type, LLVMTypeGenState());
}

LLVMCodegenPass::LLVMCodegenPass(const std::string &module_name,
                                 ErrorManager &errorMan)
    : BaseIRVisitor(nullptr), llvm_context(), builder(llvm_context),
      llvm_module(std::make_unique<llvm::Module>(module_name, llvm_context)),
      type_gen(llvm_context), errorMan(errorMan), native_fns() {
  initNativeFns();
}

void LLVMCodegenPass::initNativeFns() {
  /* GC_malloc and GC_malloc_atomic are noalias *i8(size_t) */
  std::vector<llvm::Type *> malloc_args;
  malloc_args.push_back(llvm::Type::getInt64Ty(llvm_context));
  auto malloc_type = llvm::FunctionType::get(
      llvm::PointerType::get(llvm::Type::getInt8Ty(llvm_context), 0),
      malloc_args, false);

  native_fns.GC_malloc =
      llvm::Function::Create(malloc_type, llvm::GlobalValue::ExternalLinkage,
                             "GC_malloc", llvm_module.get());
  native_fns.GC_malloc_atomic =
      llvm::Function::Create(malloc_type, llvm::GlobalValue::ExternalLinkage,
                             "GC_malloc_atomic", llvm_module.get());
}

llvm::Value *
LLVMCodegenPass::visitIntLiteral(IntLiteral &instruct,
                                 const LLVMCodegenPassState &state) {
  auto intType = dynamic_cast<ast::IntType *>(instruct.type.get());
  assert(intType != nullptr);
  return instruct.val.llvm_value = llvm::ConstantInt::get(
             llvm_context,
             llvm::APInt(intType->size, instruct.value, !intType->isUnsigned));
}

llvm::Value *
LLVMCodegenPass::visitBoolLiteral(BoolLiteral &instruct,
                                  const LLVMCodegenPassState &state) {
  auto boolType = dynamic_cast<ast::BoolType *>(instruct.type.get());
  assert(boolType != nullptr);
  /* booleans are represented as i1's */
  return instruct.val.llvm_value = llvm::ConstantInt::get(
             llvm_context, llvm::APInt(1, instruct.value ? 1 : 0));
}

llvm::Value *
LLVMCodegenPass::visitFloatLiteral(FloatLiteral &instruct,
                                   const LLVMCodegenPassState &state) {
  auto floatType = dynamic_cast<ast::FloatType *>(instruct.type.get());
  assert(floatType != nullptr &&
         (floatType->size == 32 || floatType->size == 64));
  return instruct.val.llvm_value = llvm::ConstantFP::get(
             llvm_context, floatType->size == 64
                               ? llvm::APFloat((double)instruct.value)
                               : llvm::APFloat((float)instruct.value));
}

llvm::Function *
LLVMCodegenPass::visitFunctionPrototype(ast::NamedFunctionType *proto,
                                        const std::string &name, bool is_public,
                                        const LLVMCodegenPassState &state) {
  /* check for existing prototype */
  auto prevProto = llvm_module->getFunction(name);
  if (prevProto != nullptr)
    return prevProto;

  std::vector<llvm::Type *> argTypes;
  for (auto &type : proto->argTypes) {
    argTypes.push_back(type_gen.visitType(*type));
  }

  auto functionType = llvm::FunctionType::get(
      type_gen.visitType(*proto->retType), argTypes, false);

  auto link_type = is_public ? llvm::Function::ExternalLinkage
                             : llvm::Function::InternalLinkage;

  auto function =
      llvm::Function::Create(functionType, link_type, name, llvm_module.get());

  size_t i = 0;
  for (auto &arg : function->args()) {
    arg.setName(proto->argNames[i++]);
  }

  return function;
}

llvm::Value *
LLVMCodegenPass::visitFunctionDeclare(FunctionDeclare &instruct,
                                      const LLVMCodegenPassState &state) {
  llvm::Function *function;
  auto funcType = dynamic_cast<ast::NamedFunctionType *>(instruct.type.get());
  assert(funcType != nullptr);

  function = visitFunctionPrototype(
      funcType, name_mangling::mangleIdentifier(instruct.val),
      instruct.is_public, state);

  /* shouldn't be redefining a function */
  assert(function->empty());

  /* mark argument allocations with the llvm arg value */
  size_t i = 0;
  for (auto &arg : function->args()) {
    instruct.argAllocs[i++].get().val.llvm_value = &arg;
  }

  /* create basic blocks, but don't insert into function yet
   * basic blocks may be forward referenced */
  for (auto &bb : instruct.body) {
    auto llvmBB =
        llvm::BasicBlock::Create(llvm_context, string_format("bb%i", bb->id));
    bb->llvm_bb = llvmBB;
  }
  /* visit all allocations in the function and put them in an entry block */
  auto entryBlock = llvm::BasicBlock::Create(llvm_context, "allocs", function);
  builder.SetInsertPoint(entryBlock);
  visitAllocations(instruct.body, [this, &state](Allocation &alloc) {
    visitAllocationEntry(alloc, state);
  });
  /* jump to first basic block */
  builder.CreateBr(instruct.body[0]->llvm_bb);

  /* visit basic blocks */
  i = 0;
  for (auto &bb : instruct.body) {
    visitInstruction(*bb, state.withFunc(function));

    /* if last block isn't terminated, add undef/void return */
    if (i++ == instruct.body.size() - 1) {
      if (bb->body.empty() || dynamic_cast<BasicBlockTerminator *>(
                                  &*bb->body.back().get()) == nullptr) {
        if (dynamic_cast<ast::VoidType *>(
                funcType->retType->withoutMutability()) != nullptr) {
          builder.CreateRetVoid();
        } else {
          builder.CreateRet(
              llvm::UndefValue::get(type_gen.visitType(*funcType->retType)));
        }
      }
    }
  }

  if (llvm::verifyFunction(*function, &llvm::outs())) {
    errorMan.logError("llvm function verification failed", instruct.loc,
                      ErrorType::InternalError);
  }
  return instruct.val.llvm_value = function;
}

llvm::Value *
LLVMCodegenPass::visitBasicBlock(BasicBlock &instruct,
                                 const LLVMCodegenPassState &state) {
  assert(state.curFunc != nullptr);
  assert(instruct.llvm_bb != nullptr);

  builder.SetInsertPoint(instruct.llvm_bb);
  state.curFunc->getBasicBlockList().push_back(instruct.llvm_bb);

  bool hitTerminator = false;
  for (auto &child : instruct.body) {
    /* check if instruction terminates basic block */
    auto isTerminator =
        dynamic_cast<BasicBlockTerminator *>(child.get()) != nullptr;
    if (!hitTerminator) {
      visitInstruction(*child, state);
    } else {
      /* if we've already seen a terminator, this instruction should be a
       * redundant jump terminator */
      assert(isTerminator);
    }

    if (isTerminator)
      hitTerminator = true;
  }

  return nullptr;
}

llvm::Value *LLVMCodegenPass::useValue(Expression &value,
                                       const LLVMCodegenPassState &state) {
  if (value.isAddressable()) {
    auto addrValue = value.val.llvm_value;
    /* TODO: preserve source name */
    assert(addrValue != nullptr);
    return builder.CreateLoad(addrValue);
  } else {
    auto val = value.val.llvm_value;
    assert(val != nullptr);
    return val;
  }
}

llvm::Value *LLVMCodegenPass::useAddr(Expression &value,
                                      const LLVMCodegenPassState &state) {
  assert(value.isAddressable());
  assert(value.val.llvm_value != nullptr);

  return value.val.llvm_value;
}

llvm::Value *LLVMCodegenPass::visitReturn(Return &instruct,
                                          const LLVMCodegenPassState &state) {
  if (instruct.expr == nullptr) {
    builder.CreateRetVoid();
  } else {
    auto retVal = useValue(*instruct.expr, state);
    builder.CreateRet(retVal);
  }

  return nullptr;
}

llvm::Value *LLVMCodegenPass::visitJump(Jump &instruct,
                                        const LLVMCodegenPassState &state) {
  builder.CreateBr(instruct.label.llvm_bb);
  return nullptr;
}

llvm::Value *
LLVMCodegenPass::visitConditionalJump(ConditionalJump &instruct,
                                      const LLVMCodegenPassState &state) {
  /* if condition is bool literal true, emit a non conditional jump */
  auto boolLit = dynamic_cast<BoolLiteral *>(&instruct.condition);
  if (boolLit != nullptr && boolLit->value) {
    builder.CreateBr(instruct.true_label.llvm_bb);
    return nullptr;
  } else {
    auto cond = useValue(instruct.condition, state);
    builder.CreateCondBr(cond, instruct.true_label.llvm_bb,
                         instruct.false_label.llvm_bb);
    return nullptr;
  }
}

llvm::Value *
LLVMCodegenPass::visitAllocationEntry(Allocation &instruct,
                                      const LLVMCodegenPassState &state) {
  /* visit allocations that need alloca's setup in the function entry block */
  if (AllocationTypeIsArg(instruct.allocType)) {
    /* function should have set value */
    assert(instruct.val.llvm_value != nullptr);
    if (instruct.allocType == AllocationType::ARG) {
      return instruct.val.llvm_value;
    } else if (instruct.allocType == AllocationType::ARG_COPY_TO_STACK) {
      /* create alloca on stack */
      auto alloca =
          builder.CreateAlloca(type_gen.visitType(*instruct.type), nullptr,
                               name_mangling::mangleIdentifier(instruct.val));
      /* copy from arg */
      builder.CreateStore(instruct.val.llvm_value, alloca);
      return instruct.val.llvm_value = alloca;
    } else if (instruct.allocType == AllocationType::ARG_HEAP) {
      /* heap isn't an alloca, so it doesn't need to be in the entry block.
       * handled in visitAllocation */
    }
  } else {
    assert(instruct.val.llvm_value == nullptr);
    if (instruct.allocType == AllocationType::STACK) {
      auto alloca =
          builder.CreateAlloca(type_gen.visitType(*instruct.type), nullptr,
                               name_mangling::mangleIdentifier(instruct.val));
      return instruct.val.llvm_value = alloca;
    } else if (instruct.allocType == AllocationType::HEAP) {
      /* heap isn't an alloca, so it doesn't need to be in the entry block.
       * handled in visitAllocation */
    } else {
      assert(false);
    }
  }

  return nullptr;
}

llvm::Value *
LLVMCodegenPass::visitAllocation(Allocation &instruct,
                                 const LLVMCodegenPassState &state) {
  auto type = type_gen.visitType(*instruct.type);
  if (instruct.allocType == AllocationType::HEAP ||
      instruct.allocType == AllocationType::ARG_HEAP) {
    /* in order to get the size of the type T, use
     * getelementptr *T (T, *T null, 1) and then ptrtoint */
    auto gep_index =
        llvm::ConstantInt::get(llvm::IntegerType::getInt32Ty(llvm_context), 1);
    auto size_ptr = builder.CreateGEP(
        llvm::ConstantPointerNull::get(llvm::PointerType::get(type, 0)),
        gep_index);
    auto size = builder.CreatePtrToInt(
        size_ptr, llvm::IntegerType::getInt64Ty(llvm_context));
    /* call the gc allocation function. if the type containsFrom pointers, use
     * GC_malloc, otherwise GC_malloc_atomic.
     * bitcast the resulting *i8 to appropriate pointer type */
    std::vector<llvm::Value *> malloc_args;
    malloc_args.push_back(size);

    auto func = instruct.type->containsPointer() ? native_fns.GC_malloc
                                                 : native_fns.GC_malloc_atomic;
    auto i8_alloc = builder.CreateCall(func, malloc_args);
    auto alloc =
        builder.CreateBitCast(i8_alloc, llvm::PointerType::get(type, 0));

    if (instruct.allocType == AllocationType::ARG_HEAP) {
      /* store arg into alloc */
      builder.CreateStore(instruct.val.llvm_value, alloc);
    }

    return instruct.val.llvm_value = alloc;
  }
  /* allocation should have already been visited by FunctionDeclare */
  assert(instruct.val.llvm_value != nullptr);

  return instruct.val.llvm_value;
}

llvm::Value *
LLVMCodegenPass::visitGlobalAllocation(GlobalAllocation &instruct,
                                       const LLVMCodegenPassState &state) {
  auto initVal = instruct.initial_val.val.llvm_value;

  if (!llvm::isa<llvm::Constant>(initVal)) {
    errorMan.logError("global variable initializer isn't a constant",
                      instruct.initial_val.loc, ErrorType::TypeError);
    return nullptr;
  }

  auto global = new llvm::GlobalVariable(
      *llvm_module, type_gen.visitType(*instruct.type),
      !instruct.symbol->is_mut,
      instruct.symbol->is_public ? llvm::GlobalValue::ExternalLinkage
                                 : llvm::GlobalValue::InternalLinkage,
      llvm::dyn_cast<llvm::Constant>(initVal),
      name_mangling::mangleIdentifier(instruct.val));

  return instruct.val.llvm_value = global;
}

llvm::Value *LLVMCodegenPass::visitStore(Store &instruct,
                                         const LLVMCodegenPassState &state) {
  auto addr = useAddr(instruct.storage, state);
  auto val = useValue(instruct.value, state);

  builder.CreateStore(val, addr);

  return nullptr;
}

llvm::Value *LLVMCodegenPass::visitAddress(Address &instruct,
                                           const LLVMCodegenPassState &state) {
  /* addressable values are already just addresses, so nothing to do */
  assert(instruct.expr.isAddressable());
  return instruct.val.llvm_value = useAddr(instruct.expr, state);
}

llvm::Value *
LLVMCodegenPass::visitDereference(Dereference &instruct,
                                  const LLVMCodegenPassState &state) {
  /* don't perform the load here -- it is deferred until the value is actually
   * used (useValue) */
  return instruct.val.llvm_value = useValue(instruct.expr, state);
}

llvm::Value *
LLVMCodegenPass::visitFieldSelect(FieldSelect &instruct,
                                  const LLVMCodegenPassState &state) {
  /* if expr is addressable, use getelementpointer */
  if (instruct.expr.isAddressable()) {
    auto exprAddr = useAddr(instruct.expr, state);
    std::vector<llvm::Value *> indexes;
    /* 0 index (not array) */
    indexes.push_back(
        llvm::ConstantInt::get(llvm_context, llvm::APInt(32, 0, false)));
    /* field index */
    indexes.push_back(llvm::ConstantInt::get(
        llvm_context, llvm::APInt(32, instruct.field_index, false)));
    auto resAddr = builder.CreateInBoundsGEP(exprAddr, indexes);
    return instruct.val.llvm_value = resAddr;
  }
  /* otherwise, use extractvalue */
  else {
    auto expr = useValue(instruct.expr, state);
    std::vector<uint32_t> indexes;
    indexes.push_back(instruct.field_index);
    auto resExpr = builder.CreateExtractValue(expr, indexes);
    return instruct.val.llvm_value = resExpr;
  }
}

llvm::Value *
LLVMCodegenPass::visitTupleLiteral(TupleLiteral &instruct,
                                   const LLVMCodegenPassState &state) {
  auto type = type_gen.visitType(*instruct.type);
  auto structType = llvm::dyn_cast<llvm::StructType>(type);
  assert(structType != nullptr);

  // use repeated insertvalue's, starting on undef, to create the structure
  llvm::Value *curVal = llvm::UndefValue::get(structType);
  for (size_t i = 0; i < instruct.exprs.size(); i++) {
    auto &field = instruct.exprs[i].get();
    std::vector<uint32_t> indexes;
    indexes.push_back(i);
    curVal = builder.CreateInsertValue(curVal, useValue(field, state), indexes);
  }

  return instruct.val.llvm_value = curVal;
}

llvm::Value *
LLVMCodegenPass::visitFunctionCall(FunctionCall &instruct,
                                   const LLVMCodegenPassState &state) {
  /* special handling for builtin operators */
  auto builtinOp = dynamic_cast<BuiltinOperator *>(&instruct.function);

  if (builtinOp != nullptr) {
    return builtinCall(instruct, state);
  } else {
    /* TODO: support closure calls */
    auto funcDecl = useValue(instruct.function, state);
    assert(funcDecl != nullptr);

    std::vector<llvm::Value *> args;
    for (auto &arg : instruct.arguments) {
      args.push_back(useValue(arg.get(), state));
    }

    auto res = builder.CreateCall(funcDecl, args);

    return instruct.val.llvm_value = res;
  }
}

struct InstrIntFloatVariant {
  llvm::Instruction::BinaryOps int_variant;
  llvm::Instruction::BinaryOps float_variant;
};

/* trivial (not short circuiting, etc) binary ops with int + float variant */
static std::map<ast::OperatorType, InstrIntFloatVariant> trivialBinaryOps = {
    {ast::OperatorType::ADD, {llvm::Instruction::Add, llvm::Instruction::FAdd}},
    {ast::OperatorType::SUB, {llvm::Instruction::Sub, llvm::Instruction::FSub}},
    {ast::OperatorType::MUL, {llvm::Instruction::Mul, llvm::Instruction::FMul}},
};

/* trivial binary ops with only int variant */
static std::map<ast::OperatorType, llvm::Instruction::BinaryOps>
    trivialIntBinaryOps = {
        {ast::OperatorType::BIN_AND, llvm::Instruction::BinaryOps::And},
        {ast::OperatorType::BIN_OR, llvm::Instruction::BinaryOps::Or},
        {ast::OperatorType::BIN_XOR, llvm::Instruction::BinaryOps::Xor},
        {ast::OperatorType::LEFT_SHIFT, llvm::Instruction::Shl}};

/* comparison operation predicates */
struct PredIntFloatVariant {
  llvm::CmpInst::Predicate int_variant;
  llvm::CmpInst::Predicate float_variant;
};

static std::map<ast::OperatorType, PredIntFloatVariant> comparisonPreds = {
    {ast::OperatorType::EQUAL,
     {llvm::CmpInst::Predicate::ICMP_EQ, llvm::CmpInst::Predicate::FCMP_OEQ}},
    {ast::OperatorType::NEQUAL,
     {llvm::CmpInst::Predicate::ICMP_NE, llvm::CmpInst::Predicate::FCMP_ONE}},
    {ast::OperatorType::GREATER,
     {llvm::CmpInst::Predicate::ICMP_UGT, llvm::CmpInst::Predicate::FCMP_OGT}},
    {ast::OperatorType::GREATER_EQUAL,
     {llvm::CmpInst::Predicate::ICMP_UGE, llvm::CmpInst::Predicate::FCMP_OGE}},
    {ast::OperatorType::LESS,
     {llvm::CmpInst::Predicate::ICMP_ULT, llvm::CmpInst::Predicate::FCMP_OLT}},
    {ast::OperatorType::LESS_EQUAL,
     {llvm::CmpInst::Predicate::ICMP_ULE, llvm::CmpInst::Predicate::FCMP_OLE}},
};

/* int/float ops with signed/unsigned int variant */
struct InstrSignedIntFloatVariant {
  llvm::Instruction::BinaryOps uint_variant;
  llvm::Instruction::BinaryOps sint_variant;
  llvm::Instruction::BinaryOps float_variant;
};

static std::map<ast::OperatorType, InstrSignedIntFloatVariant>
    signedIntFloatOps = {
        {ast::OperatorType::DIV,
         {llvm::BinaryOperator::UDiv, llvm::BinaryOperator::SDiv,
          llvm::BinaryOperator::FDiv}},
        {ast::OperatorType::MOD,
         {llvm::BinaryOperator::URem, llvm::BinaryOperator::SRem,
          llvm::BinaryOperator::FRem}}};

llvm::Value *LLVMCodegenPass::builtinCall(FunctionCall &instruct,
                                          const LLVMCodegenPassState &state) {
  auto builtinOp = dynamic_cast<BuiltinOperator *>(&instruct.function);
  auto intArg0 = dynamic_cast<const ast::IntType *>(
      instruct.arguments[0].get().type->withoutMutability());
  auto isIntArg0 = intArg0 != nullptr;

  switch (builtinOp->opType) {
  case ast::OperatorType::ADD:
  case ast::OperatorType::SUB:
  case ast::OperatorType::MUL: {
    /* look up operation and pick int or float op */
    auto opVariants = trivialBinaryOps[builtinOp->opType];
    auto op = isIntArg0 ? opVariants.int_variant : opVariants.float_variant;

    auto value =
        builder.CreateBinOp(op, useValue(instruct.arguments[0].get(), state),
                            useValue(instruct.arguments[1].get(), state));
    return instruct.val.llvm_value = value;
  }
  case ast::OperatorType::EQUAL:
  case ast::OperatorType::NEQUAL:
  case ast::OperatorType::GREATER_EQUAL:
  case ast::OperatorType::GREATER:
  case ast::OperatorType::LESS_EQUAL:
  case ast::OperatorType::LESS: {
    auto opVariants = comparisonPreds[builtinOp->opType];
    auto op = isIntArg0 ? opVariants.int_variant : opVariants.float_variant;
    /* if args are ints and either are signed, use signed predicate */
    if (isIntArg0) {
      auto intArg1 = dynamic_cast<const ast::IntType *>(
          instruct.arguments[1].get().type->withoutMutability());
      assert(intArg1 != nullptr);
      if ((!intArg0->isUnsigned || !intArg1->isUnsigned) &&
          builtinOp->opType != ast::OperatorType::EQUAL &&
          builtinOp->opType != ast::OperatorType::NEQUAL) {
        op = llvm::CmpInst::getSignedPredicate(op);
      }
    }

    auto value =
        isIntArg0
            ? builder.CreateICmp(op,
                                 useValue(instruct.arguments[0].get(), state),
                                 useValue(instruct.arguments[1].get(), state))
            : builder.CreateFCmp(op,
                                 useValue(instruct.arguments[0].get(), state),
                                 useValue(instruct.arguments[1].get(), state));
    return instruct.val.llvm_value = value;
  }
  case ast::OperatorType::NEGATIVE:
    if (isIntArg0) {
      // -a = 0-a
      auto value = builder.CreateSub(
          llvm::ConstantInt::get(llvm_context, llvm::APInt(intArg0->size, 0)),
          useValue(instruct.arguments[0].get(), state));

      return instruct.val.llvm_value = value;
    } else {
      auto value =
          builder.CreateFNeg(useValue(instruct.arguments[0].get(), state));
      return instruct.val.llvm_value = value;
    }
  case ast::OperatorType::BIN_AND:
  case ast::OperatorType::BIN_OR:
  case ast::OperatorType::BIN_XOR:
  case ast::OperatorType::LEFT_SHIFT: {
    auto op = trivialIntBinaryOps[builtinOp->opType];
    auto value =
        builder.CreateBinOp(op, useValue(instruct.arguments[0].get(), state),
                            useValue(instruct.arguments[1].get(), state));
    return instruct.val.llvm_value = value;
  }
  case ast::OperatorType::RIGHT_SHIFT: {
    /* select logical or arithmetic shift based on signedness */
    auto op = intArg0->isUnsigned ? llvm::BinaryOperator::LShr
                                  : llvm::BinaryOperator::AShr;
    auto value =
        builder.CreateBinOp(op, useValue(instruct.arguments[0].get(), state),
                            useValue(instruct.arguments[1].get(), state));
    return instruct.val.llvm_value = value;
  }
  case ast::OperatorType::DIV:
  case ast::OperatorType::MOD: {
    /* select op based on int/float and signed/unsigned for int */
    auto opVariants = signedIntFloatOps[builtinOp->opType];
    auto op = isIntArg0 ? (intArg0->isUnsigned ? opVariants.uint_variant
                                               : opVariants.sint_variant)
                        : opVariants.float_variant;

    auto value =
        builder.CreateBinOp(op, useValue(instruct.arguments[0].get(), state),
                            useValue(instruct.arguments[1].get(), state));
    return instruct.val.llvm_value = value;
  }
  case ast::OperatorType::BIN_NOT: {
    // use xor against -1 (~0)
    auto neg_one = llvm::ConstantInt::get(llvm_context,
                                          llvm::APInt(intArg0->size, -1, true));

    auto value =
        builder.CreateXor(useValue(instruct.arguments[0], state), neg_one);
    return instruct.val.llvm_value = value;
  }
  case ast::OperatorType::LOG_NOT: {
    /* use icmp eq 0 */
    auto value = builder.CreateICmpEQ(
        useValue(instruct.arguments[0].get(), state),
        llvm::ConstantInt::get(llvm_context, llvm::APInt(1, 0)));

    return instruct.val.llvm_value = value;
  }
  /* compound assignment operators are handled by type check */
  case ast::OperatorType::ADD_ASSIGN:
  case ast::OperatorType::SUB_ASSIGN:
  /* && and || are short circuiting, and should have been handled by type check
   */
  case ast::OperatorType::LOG_AND:
  case ast::OperatorType::LOG_OR:
  /* deref and address should have been handled by type check */
  case ast::OperatorType::DEREF:
  case ast::OperatorType::ADDR:
  default:
    assert(false);
    return nullptr;
  }
}

llvm::Value *
LLVMCodegenPass::visitBuiltinOperator(BuiltinOperator &instruct,
                                      const LLVMCodegenPassState &state) {
  /* this should be a special case handled in visitFunctionCall */
  return nullptr;
}

llvm::Value *
LLVMCodegenPass::visitBuiltinCast(BuiltinCast &instruct,
                                  const LLVMCodegenPassState &state) {
  auto intSrcType =
      dynamic_cast<ast::IntType *>(instruct.expr.type->withoutMutability());
  auto intDstType =
      dynamic_cast<ast::IntType *>(instruct.type->withoutMutability());
  auto floatSrcType =
      dynamic_cast<ast::FloatType *>(instruct.expr.type->withoutMutability());
  auto floatDstType =
      dynamic_cast<ast::FloatType *>(instruct.type->withoutMutability());

  if (intSrcType != nullptr) {
    assert(intDstType != nullptr);
    /* if dst is smaller than src, truncate */
    if (intDstType->size < intSrcType->size) {
      auto value = builder.CreateTrunc(useValue(instruct.expr, state),
                                       type_gen.visitType(*intDstType));
      return instruct.val.llvm_value = value;
    }
    /* if dst is larger and src is signed, sext */
    else if (intDstType->size > intSrcType->size && !intSrcType->isUnsigned) {
      auto value = builder.CreateSExt(useValue(instruct.expr, state),
                                      type_gen.visitType(*intDstType));
      return instruct.val.llvm_value = value;
    }
    /* if dst is larger and src unsigned, zext */
    else if (intDstType->size > intSrcType->size && intSrcType->isUnsigned) {
      auto value = builder.CreateZExt(useValue(instruct.expr, state),
                                      type_gen.visitType(*intDstType));
      return instruct.val.llvm_value = value;
    }
    /* signed <-> unsigned conversion, nop */
    else {
      auto value = useValue(instruct.expr, state);
      return instruct.val.llvm_value = value;
    }
  } else if (floatSrcType != nullptr) {
    assert(floatDstType != nullptr);

    /* if dst is smaller, fptrunc */
    if (floatDstType->size < floatSrcType->size) {
      auto value = builder.CreateFPTrunc(useValue(instruct.expr, state),
                                         type_gen.visitType(*floatDstType));
      return instruct.val.llvm_value = value;
    }
    /* if dst is bigger, fpext */
    else if (floatDstType->size > floatSrcType->size) {
      auto value = builder.CreateFPExt(useValue(instruct.expr, state),
                                       type_gen.visitType(*floatDstType));
      return instruct.val.llvm_value = value;
    } else {
      assert(false);
    }
  } else {
    assert(false);
  }
}

llvm::Value *
LLVMCodegenPass::visitForwardIdentifier(ForwardIdentifier &instruct,
                                        const LLVMCodegenPassState &state) {

  /* TODO: support forward referenced globals */
  auto funcType =
      dynamic_cast<ast::NamedFunctionType *>(instruct.symbol_ref->type.get());
  assert(funcType != nullptr);

  auto function = visitFunctionPrototype(
      funcType, name_mangling::mangleIdentifier(instruct.val),
      instruct.symbol_ref->is_public, state);

  return instruct.val.llvm_value = function;
}

void LLVMCodegenPass::addMain(const ScopeTable<Symbol> *package,
                              const std::string &main_func_name) {
  auto funcType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(llvm_context), false);
  auto function = llvm::Function::Create(
      funcType, llvm::GlobalValue::ExternalLinkage, "main", llvm_module.get());

  auto block = llvm::BasicBlock::Create(llvm_context, "bb0", function);
  builder.SetInsertPoint(block);

  auto main_func = llvm_module->getFunction(
      name_mangling::mangleMainFunc(package, main_func_name));
  if (main_func == nullptr) {
    errorMan.logError("no main function declared",
                      SourceLocation::nullLocation(), ErrorType::InternalError);
    return;
  }

  auto res = builder.CreateCall(main_func);
  builder.CreateRet(res);
}

void LLVMCodegenPass::optAndEmit(llvm::PassBuilder::OptimizationLevel optLevel,
                                 const std::string &filename,
                                 CodegenOutputType outType, bool genMainFunc,
                                 const ScopeTable<Symbol> *mainPackage,
                                 const std::string *mainFuncName,
                                 llvm::Reloc::Model relocModel,
                                 llvm::CodeModel::Model codeModel) {
  if (genMainFunc) {
    assert(mainFuncName != nullptr && mainPackage != nullptr);
    addMain(mainPackage, *mainFuncName);
  }

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmParser();
  llvm::InitializeNativeTargetAsmPrinter();
  /* get target triple */
  auto targetTripleStr = llvm::sys::getDefaultTargetTriple();
  llvm::Triple targetTriple(targetTripleStr);

  std::string error;
  auto target = llvm::TargetRegistry::lookupTarget(targetTripleStr, error);

  if (target == nullptr) {
    errorMan.logError(
        string_format("error getting llvm target: %s", error.c_str()),
        SourceLocation::nullLocation(), ErrorType::InternalError);
    return;
  }

  auto cpu = "generic";
  auto features = "";

  llvm::TargetOptions opts;
  auto relocateModel = llvm::Optional<llvm::Reloc::Model>(relocModel);
  auto llvmCodeModel = llvm::Optional<llvm::CodeModel::Model>(codeModel);
  auto targetMachine = target->createTargetMachine(
      targetTripleStr, cpu, features, opts, relocateModel, llvmCodeModel);

  /* set target + data layout */
  llvm_module->setDataLayout(targetMachine->createDataLayout());
  llvm_module->setTargetTriple(targetTripleStr);

  /* open output file */
  std::error_code ec;
  llvm::raw_fd_ostream outFile(filename, ec, llvm::sys::fs::OF_None);

  if (ec) {
    errorMan.logError(string_format("error opening file %s: %s",
                                    filename.c_str(), ec.message().c_str()),
                      SourceLocation::nullLocation(), ErrorType::InternalError);
    return;
  }

  /* create pass manager */
  llvm::PipelineTuningOptions PTO;
  PTO.LoopUnrolling =
      optLevel != llvm::PassBuilder::Oz && optLevel != llvm::PassBuilder::Os;
  PTO.LoopInterleaving =
      optLevel != llvm::PassBuilder::Oz && optLevel != llvm::PassBuilder::Os;
  PTO.LoopVectorization = true;
  PTO.SLPVectorization = true;

  llvm::PassInstrumentationCallbacks PIC;
  llvm::StandardInstrumentations SI;
  SI.registerCallbacks(PIC);

  llvm::PassBuilder passBuilder(targetMachine, PTO, llvm::None, &PIC);

  bool DebugPassManager = false;
  llvm::LoopAnalysisManager LAM(DebugPassManager);
  llvm::FunctionAnalysisManager FAM(DebugPassManager);
  llvm::CGSCCAnalysisManager CGAM(DebugPassManager);
  llvm::ModuleAnalysisManager MAM(DebugPassManager);

  FAM.registerPass(
      [&passBuilder] { return passBuilder.buildDefaultAAPipeline(); });

  std::unique_ptr<llvm::TargetLibraryInfoImpl> TLII(
      new llvm::TargetLibraryInfoImpl(targetTriple));

  FAM.registerPass([&] { return llvm::TargetLibraryAnalysis(*TLII); });

  passBuilder.registerModuleAnalyses(MAM);
  passBuilder.registerCGSCCAnalyses(CGAM);
  passBuilder.registerFunctionAnalyses(FAM);
  passBuilder.registerLoopAnalyses(LAM);
  passBuilder.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  llvm::ModulePassManager MPM(DebugPassManager);
  MPM = passBuilder.buildPerModuleDefaultPipeline(optLevel, DebugPassManager);

  if (llvm::verifyModule(*llvm_module, &llvm::outs())) {
    errorMan.logError("llvm module verification failed",
                      SourceLocation::nullLocation(), ErrorType::InternalError);
    return;
  }

  MPM.run(*llvm_module, MAM);

  if (outType == CodegenOutputType::LLVM_IR) {
    llvm_module->print(outFile, nullptr);
  } else {
    /* emit object/assembly code */
    auto fileType = outType == CodegenOutputType::ASM ? llvm::CGFT_AssemblyFile
                                                      : llvm::CGFT_ObjectFile;

    llvm::legacy::PassManager pass;
    if (targetMachine->addPassesToEmitFile(pass, outFile, nullptr, fileType)) {
      errorMan.logError("llvm can't emit an object file of this type",
                        SourceLocation::nullLocation(),
                        ErrorType::InternalError);

      return;
    }

    pass.run(*llvm_module);
    outFile.flush();
  }

  delete targetMachine;
}

} // namespace ovid::ir
