#ifndef LLVM_CODEGEN_H
#define LLVM_CODEGEN_H

#include "ir_visitor.hpp"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

namespace ovid::ir {
/* the llvm codegen pass over the ir.
 * the pass visits each ir instruction and emits the appropriate llvm ir
 */
class LLVMCodegenPassState {
public:
  LLVMCodegenPassState() = default;
};

class LLVMCodegenPass
    : public BaseIRVisitor<llvm::Value *, LLVMCodegenPassState> {
public:
  llvm::LLVMContext llvm_context;
  llvm::IRBuilder<> builder;
  std::unique_ptr<llvm::Module> llvm_module;
private:

  /*llvm::Value *visitFunctionDeclare(FunctionDeclare &instruct,
                                    const LLVMCodegenPassState &state) override;*/
  llvm::Value *visitIntLiteral(IntLiteral &instruct,
                               const LLVMCodegenPassState &state) override;
  llvm::Value *visitBoolLiteral(BoolLiteral &instruct,
                                const LLVMCodegenPassState &state) override;
  /*llvm::Value *visitTupleLiteral(TupleLiteral &instruct,
                                 const LLVMCodegenPassState &state) override;
  llvm::Value *visitFunctionCall(FunctionCall &instruct,
                                 const LLVMCodegenPassState &state) override;
  llvm::Value *visitAllocation(Allocation &instruct,
                               const LLVMCodegenPassState &state) override;
  llvm::Value *visitAddress(Address &instruct,
                            const LLVMCodegenPassState &state) override;
  llvm::Value *visitDereference(Dereference &instruct,
                                const LLVMCodegenPassState &state) override;
  llvm::Value *visitBuiltinOperator(BuiltinOperator &instruct,
                                    const LLVMCodegenPassState &state) override;
  llvm::Value *visitBuiltinCast(BuiltinCast &instruct,
                                const LLVMCodegenPassState &state) override;
  llvm::Value *visitFieldSelect(FieldSelect &instruct,
                                const LLVMCodegenPassState &state) override;

  llvm::Value *visitStore(Store &instruct,
                          const LLVMCodegenPassState &state) override;
  llvm::Value *visitBasicBlock(BasicBlock &instruct,
                               const LLVMCodegenPassState &state) override;

  llvm::Value *visitReturn(Return &instruct,
                           const LLVMCodegenPassState &state) override;
  llvm::Value *visitJump(Jump &instruct,
                         const LLVMCodegenPassState &state) override;
  llvm::Value *visitConditionalJump(ConditionalJump &instruct,
                                    const LLVMCodegenPassState &state) override;*/

public:
  LLVMCodegenPass(const std::string &module_name)
      : BaseIRVisitor(nullptr), llvm_context(), builder(llvm_context),
        llvm_module(
            std::make_unique<llvm::Module>(module_name, llvm_context)){};
};

} // namespace ovid::ir

#endif