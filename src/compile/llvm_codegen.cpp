#include "llvm_codegen.hpp"

namespace ovid::ir {

llvm::Value *
LLVMCodegenPass::visitIntLiteral(IntLiteral &instruct,
                                 const LLVMCodegenPassState &state) {
  auto intType = dynamic_cast<ast::IntType *>(instruct.type.get());
  assert(intType != nullptr);
  return llvm::ConstantInt::get(
      llvm_context,
      llvm::APInt(intType->size, instruct.value, !intType->isUnsigned));
}

llvm::Value *
LLVMCodegenPass::visitBoolLiteral(BoolLiteral &instruct,
                                  const LLVMCodegenPassState &state) {
  auto boolType = dynamic_cast<ast::BoolType *>(instruct.type.get());
  assert(boolType != nullptr);
  /* booleans are represented as i1's */
  return llvm::ConstantInt::get(llvm_context, llvm::APInt(1, instruct.value ? 1 : 0));
}

} // namespace ovid::ir
