#include "llvm_codegen.hpp"
#include "escape_analysis.hpp"
#include "name_mangle.hpp"

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
  for (auto &field : type.types) {
    types.push_back(visitType(*field, state));
  }
  return llvm::StructType::get(llvm_context, types);
}

llvm::Type *LLVMTypeGen::visitFunctionType(ast::FunctionType &type,
                                           const LLVMTypeGenState &state) {
  /* TODO (closure type) */
  assert(false);
}

llvm::Type *LLVMTypeGen::visitNamedFunctionType(ast::NamedFunctionType &type,
                                                const LLVMTypeGenState &state) {
  return visitFunctionType(*type.type, state);
}

llvm::Type *LLVMTypeGen::visitType(ast::Type &type) {
  return visitType(type, LLVMTypeGenState());
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

llvm::Function *
LLVMCodegenPass::visitFunctionPrototype(ast::NamedFunctionType *proto,
                                        const std::string &name, bool is_public,
                                        const LLVMCodegenPassState &state) {
  /* check for existing prototype */
  auto prevProto = llvm_module->getFunction(name);
  if (prevProto != nullptr)
    return prevProto;

  std::vector<llvm::Type *> argTypes;
  for (auto &type : proto->type->argTypes) {
    argTypes.push_back(type_gen.visitType(*type));
  }

  auto functionType = llvm::FunctionType::get(
      type_gen.visitType(*proto->type->retType), argTypes, false);

  auto link_type = is_public ? llvm::Function::ExternalLinkage
                             : llvm::Function::InternalLinkage;

  auto function =
      llvm::Function::Create(functionType, link_type, name, llvm_module.get());

  size_t i = 0;
  for(auto &arg: function->args()) {
    arg.setName(proto->argNames[i++]);
  }

  return function;
}

llvm::Value *
LLVMCodegenPass::visitFunctionDeclare(FunctionDeclare &instruct,
                                      const LLVMCodegenPassState &state) {
  llvm::Function *function;
  function = visitFunctionPrototype(
      dynamic_cast<ast::NamedFunctionType *>(instruct.type.get()),
      name_mangling::mangle(instruct.val.sourceName), instruct.is_public,
      state);

  /* shouldn't be redefining a function */
  assert(function->empty());

  /* mark argument allocations with the llvm arg value */
  size_t i = 0;
  for(auto& arg: function->args()) {
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
  for (auto &bb : instruct.body) {
    visitInstruction(*bb, state.withFunc(function));
  }

  llvm::verifyFunction(*function);
  return instruct.val.llvm_value = function;
}

llvm::Value *
LLVMCodegenPass::visitBasicBlock(BasicBlock &instruct,
                                 const LLVMCodegenPassState &state) {
  assert(state.curFunc != nullptr);
  assert(instruct.llvm_bb != nullptr);

  builder.SetInsertPoint(instruct.llvm_bb);
  state.curFunc->getBasicBlockList().push_back(instruct.llvm_bb);

  for (auto &child : instruct.body) {
    visitInstruction(*child, state);
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
  auto cond = useValue(instruct.condition, state);
  builder.CreateCondBr(cond, instruct.true_label.llvm_bb,
                       instruct.false_label.llvm_bb);

  return nullptr;
}

llvm::Value *
LLVMCodegenPass::visitAllocationEntry(Allocation &instruct,
                                      const LLVMCodegenPassState &state) {
  if (AllocationTypeIsArg(instruct.allocType)) {
    /* function should have set value */
    assert(instruct.val.llvm_value != nullptr);
    if (instruct.allocType == AllocationType::ARG) {
      return instruct.val.llvm_value;
    } else if (instruct.allocType == AllocationType::ARG_COPY_TO_STACK) {
      /* TODO */
      assert(false);
    } else if (instruct.allocType == AllocationType::ARG_HEAP) {
      /* TODO */
      assert(false);
    }
  } else {
    assert(instruct.val.llvm_value == nullptr);
    if (instruct.allocType == AllocationType::STACK) {
      auto alloca =
          builder.CreateAlloca(type_gen.visitType(*instruct.type), nullptr,
                               name_mangling::mangle(instruct.val));
      return instruct.val.llvm_value = alloca;
    } else if (instruct.allocType == AllocationType::HEAP) {
      /* TODO */
      assert(false);
    } else if (instruct.allocType == AllocationType::STATIC) {
      /* TODO */
      assert(false);
    } else {
      assert(false);
    }
  }

  return nullptr;
}

llvm::Value *
LLVMCodegenPass::visitAllocation(Allocation &instruct,
                                 const LLVMCodegenPassState &state) {
  /* allocation should have already been visited by FunctionDeclare */
  assert(instruct.val.llvm_value != nullptr);

  return instruct.val.llvm_value;
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
  return instruct.val.llvm_value = useAddr(instruct.expr, state);
}

llvm::Value *
LLVMCodegenPass::visitDereference(Dereference &instruct,
                                  const LLVMCodegenPassState &state) {
  /* don't perform the load here -- it is deferred until the value is actually
   * used (useValue) */
  return instruct.val.llvm_value = instruct.expr.val.llvm_value;
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

};

llvm::Value *LLVMCodegenPass::builtinCall(FunctionCall &instruct,
                                          const LLVMCodegenPassState &state) {
  auto builtinOp = dynamic_cast<BuiltinOperator *>(&instruct.function);
  auto intArg =
      dynamic_cast<const ast::IntType *>(
          instruct.arguments[0].get().type->withoutMutability()) != nullptr;
  auto floatArg =
      dynamic_cast<const ast::FloatType *>(
          instruct.arguments[0].get().type->withoutMutability()) != nullptr;

  switch (builtinOp->opType) {
  case ast::OperatorType::ADD:
  case ast::OperatorType::SUB:
  case ast::OperatorType::MUL: {
    /* look up operation and pick int or float op */
    auto opVariants = trivialBinaryOps[builtinOp->opType];
    auto op = intArg ? opVariants.int_variant : opVariants.float_variant;

    auto value =
        builder.CreateBinOp(op, useValue(instruct.arguments[0].get(), state),
                            useValue(instruct.arguments[1].get(), state));
    return instruct.val.llvm_value = value;
  }
  default:
    /* TODO */
    assert(false);
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
    } else {
      assert(false);
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

  auto function =
      visitFunctionPrototype(funcType, name_mangling::mangle(instruct.val),
                             instruct.symbol_ref->is_public, state);

  return instruct.val.llvm_value = function;
}

} // namespace ovid::ir
