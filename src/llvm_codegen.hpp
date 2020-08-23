#ifndef LLVM_CODEGEN_H
#define LLVM_CODEGEN_H

#include "ast_visitor.hpp"
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
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

namespace ovid::ir {

/* type visitor, which converts ast types to llvm types */
class LLVMTypeGenState {
public:
  LLVMTypeGenState() = default;
};

class LLVMTypeGen
    : public ast::BaseTypeVisitor<llvm::Type *, LLVMTypeGenState> {
  llvm::LLVMContext &llvm_context;
  llvm::Type *visitVoidType(ast::VoidType &type,
                            const LLVMTypeGenState &state) override;
  llvm::Type *visitBoolType(ast::BoolType &type,
                            const LLVMTypeGenState &state) override;
  llvm::Type *visitIntType(ast::IntType &type,
                           const LLVMTypeGenState &state) override;
  llvm::Type *visitFloatType(ast::FloatType &type,
                             const LLVMTypeGenState &state) override;

  llvm::Type *visitMutType(ast::MutType &type,
                           const LLVMTypeGenState &state) override;
  llvm::Type *visitPointerType(ast::PointerType &type,
                               const LLVMTypeGenState &state) override;

  llvm::Type *visitFunctionType(ast::FunctionType &type,
                                const LLVMTypeGenState &state) override;
  llvm::Type *visitNamedFunctionType(ast::NamedFunctionType &type,
                                     const LLVMTypeGenState &state) override;

  llvm::Type *visitTupleType(ast::TupleType &type,
                             const LLVMTypeGenState &state) override;
  llvm::Type *visitStructType(ast::StructType &type,
                              const LLVMTypeGenState &state) override;

public:
  using BaseTypeVisitor::visitType;
  llvm::Type *visitType(ast::Type &type);

  explicit LLVMTypeGen(llvm::LLVMContext &llvm_context)
      : BaseTypeVisitor(nullptr), llvm_context(llvm_context){};
};

/* native methods that the codegen pass uses */
struct LLVMCodegenNativeMethods {
public:
  llvm::Value *GC_malloc;
  llvm::Value *GC_malloc_atomic;
};

/* the llvm codegen pass over the ir.
 * the pass visits each ir instruction and emits the appropriate llvm ir
 */
class LLVMCodegenPassState {
public:
  llvm::Function *curFunc;

  LLVMCodegenPassState() : curFunc(nullptr){};
  explicit LLVMCodegenPassState(llvm::Function *curFunc) : curFunc(curFunc){};

  LLVMCodegenPassState withFunc(llvm::Function *func) const;
};

enum class CodegenOutputType { LLVM_IR, ASM, OBJ };

class LLVMCodegenPass
    : public BaseIRVisitor<llvm::Value *, LLVMCodegenPassState> {
public:
  llvm::LLVMContext llvm_context;
  llvm::IRBuilder<> builder;
  std::unique_ptr<llvm::Module> llvm_module;
  LLVMTypeGen type_gen;
  ErrorManager &errorMan;

  LLVMCodegenNativeMethods native_fns;

private:
  llvm::Value *visitFunctionDeclare(FunctionDeclare &instruct,
                                    const LLVMCodegenPassState &state) override;
  llvm::Value *visitIntLiteral(IntLiteral &instruct,
                               const LLVMCodegenPassState &state) override;
  llvm::Value *visitBoolLiteral(BoolLiteral &instruct,
                                const LLVMCodegenPassState &state) override;
  llvm::Value *visitFloatLiteral(FloatLiteral &instruct,
                                 const LLVMCodegenPassState &state) override;
  llvm::Value *visitTupleLiteral(TupleLiteral &instruct,
                                 const LLVMCodegenPassState &state) override;
  llvm::Value *visitFunctionCall(FunctionCall &instruct,
                                 const LLVMCodegenPassState &state) override;
  llvm::Value *visitAllocation(Allocation &instruct,
                               const LLVMCodegenPassState &state) override;
  llvm::Value *
  visitGlobalAllocation(GlobalAllocation &instruct,
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
  llvm::Value *
  visitForwardIdentifier(ForwardIdentifier &instruct,
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
                                    const LLVMCodegenPassState &state) override;

  /* generate a function prototype definition */
  llvm::Function *visitFunctionPrototype(ast::NamedFunctionType *proto,
                                         const std::string &name,
                                         bool is_public,
                                         const LLVMCodegenPassState &state);

  /* create the nodes appropriate for using the given value
   * if the value is addressable, this is a load
   * otherwise, nothing */
  llvm::Value *useValue(Expression &value, const LLVMCodegenPassState &state);

  llvm::Value *useAddr(Expression &value, const LLVMCodegenPassState &state);

  /* handle a function call on a builtin operator */
  llvm::Value *builtinCall(FunctionCall &instruct,
                           const LLVMCodegenPassState &state);

  /* visit an allocation while constructing the function entry block */
  llvm::Value *visitAllocationEntry(Allocation &instruct,
                                    const LLVMCodegenPassState &state);

  /* add a main function that calls the given main function */
  void addMain(const ScopeTable<Symbol> *package,
               const std::string &main_func_name);

  /* init llvm decl's for native methods */
  void initNativeFns();

public:
  LLVMCodegenPass(const std::string &module_name, ErrorManager &errorMan);

  void optAndEmit(llvm::PassBuilder::OptimizationLevel optLevel,
                  const std::string &filename, CodegenOutputType outType,
                  bool genMainFunc = false,
                  const ScopeTable<Symbol> *mainPackage = nullptr,
                  const std::string *mainFuncName = nullptr,
                  llvm::Reloc::Model relocModel = llvm::Reloc::PIC_,
                  llvm::CodeModel::Model codeModel = llvm::CodeModel::Small);
};

} // namespace ovid::ir

#endif