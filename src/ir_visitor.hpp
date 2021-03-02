#ifndef H_IR_VISITOR_INCL
#define H_IR_VISITOR_INCL

#include "ir.hpp"

namespace ovid::ir {

template <class T, class S> class BaseIRVisitor {
  T defaultValue;

  virtual T visitGenericExpression(GenericExpression &instruct, const S &state);
  virtual T visitExpression(Expression &instruct, const S &state);
  virtual T visitBasicBlockTerminator(BasicBlockTerminator &instruct,
                                      const S &state);

  virtual T visitGenericFunctionDeclare(GenericFunctionDeclare &instruct,
                                        const S &state);
  virtual T visitGenericForwardIdentifier(GenericForwardIdentifier &instruct,
                                          const S &state);
  virtual T visitGenericImpl(GenericImpl &instruct, const S &state);
  virtual T visitGenericSelect(GenericSelect &instruct, const S &state);
  virtual T visitForwardGenericImpl(ForwardGenericImpl &instruct,
                                    const S &state);

  virtual T visitImpl(Impl &instruct, const S &state);
  virtual T visitSelect(Select &instruct, const S &state);
  virtual T visitFunctionDeclare(FunctionDeclare &instruct, const S &state);
  virtual T visitIntLiteral(IntLiteral &instruct, const S &state);
  virtual T visitBoolLiteral(BoolLiteral &instruct, const S &state);
  virtual T visitFloatLiteral(FloatLiteral &instruct, const S &state);
  virtual T visitTupleLiteral(TupleLiteral &instruct, const S &state);
  virtual T visitFunctionCall(FunctionCall &instruct, const S &state);
  virtual T visitAllocation(Allocation &instruct, const S &state);
  virtual T visitGlobalAllocation(GlobalAllocation &instruct, const S &state);
  virtual T visitAddress(Address &instruct, const S &state);
  virtual T visitDereference(Dereference &instruct, const S &state);
  virtual T visitBuiltinOperator(BuiltinOperator &instruct, const S &state);
  virtual T visitBuiltinCast(BuiltinCast &instruct, const S &state);
  virtual T visitFieldSelect(FieldSelect &instruct, const S &state);
  virtual T visitForwardIdentifier(ForwardIdentifier &instruct, const S &state);
  virtual T visitForwardImpl(ForwardImpl &instruct, const S &state);
  virtual T visitSpecialize(Specialize &instruct, const S &state);
  virtual T visitSizeof(Sizeof &instruct, const S &state);

  virtual T visitStore(Store &instruct, const S &state);
  virtual T visitBasicBlock(BasicBlock &instruct, const S &state);

  virtual T visitReturn(Return &instruct, const S &state);
  virtual T visitJump(Jump &instruct, const S &state);
  virtual T visitConditionalJump(ConditionalJump &instruct, const S &state);

public:
  virtual T visitInstruction(Instruction &instruct, const S &state);
  virtual std::vector<T> visitInstructions(const InstructionList &instructs,
                                           const S &state);

  BaseIRVisitor(T defaultValue) : defaultValue(std::move(defaultValue)){};

  virtual ~BaseIRVisitor() = default;
};

template <class T, class S>
T BaseIRVisitor<T, S>::visitInstruction(Instruction &instruct, const S &state) {
  if (dynamic_cast<Expression *>(&instruct) != nullptr) {
    return visitExpression(dynamic_cast<Expression &>(instruct), state);
  } else if (dynamic_cast<Store *>(&instruct) != nullptr) {
    return visitStore(dynamic_cast<Store &>(instruct), state);
  } else if (dynamic_cast<BasicBlock *>(&instruct) != nullptr) {
    return visitBasicBlock(dynamic_cast<BasicBlock &>(instruct), state);
  } else if (dynamic_cast<BasicBlockTerminator *>(&instruct) != nullptr) {
    return visitBasicBlockTerminator(
        dynamic_cast<BasicBlockTerminator &>(instruct), state);
  } else if (dynamic_cast<GenericExpression *>(&instruct) != nullptr) {
    return visitGenericExpression(dynamic_cast<GenericExpression &>(instruct),
                                  state);
  }

  assert(false);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitGenericExpression(GenericExpression &instruct,
                                              const S &state) {
  if (dynamic_cast<GenericFunctionDeclare *>(&instruct) != nullptr) {
    return visitGenericFunctionDeclare(
        dynamic_cast<GenericFunctionDeclare &>(instruct), state);
  } else if (dynamic_cast<GenericForwardIdentifier *>(&instruct) != nullptr) {
    return visitGenericForwardIdentifier(
        dynamic_cast<GenericForwardIdentifier &>(instruct), state);
  } else if (dynamic_cast<GenericImpl *>(&instruct) != nullptr) {
    return visitGenericImpl(dynamic_cast<GenericImpl &>(instruct), state);
  } else if (dynamic_cast<GenericSelect *>(&instruct) != nullptr) {
    return visitGenericSelect(dynamic_cast<GenericSelect &>(instruct), state);
  } else if (dynamic_cast<ForwardGenericImpl *>(&instruct) != nullptr) {
    return visitForwardGenericImpl(dynamic_cast<ForwardGenericImpl &>(instruct),
                                   state);
  }

  assert(false);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitExpression(Expression &instruct, const S &state) {
  if (dynamic_cast<FunctionCall *>(&instruct) != nullptr) {
    return visitFunctionCall(dynamic_cast<FunctionCall &>(instruct), state);
  } else if (dynamic_cast<FunctionDeclare *>(&instruct) != nullptr) {
    return visitFunctionDeclare(dynamic_cast<FunctionDeclare &>(instruct),
                                state);
  } else if (dynamic_cast<IntLiteral *>(&instruct) != nullptr) {
    return visitIntLiteral(dynamic_cast<IntLiteral &>(instruct), state);
  } else if (dynamic_cast<BoolLiteral *>(&instruct) != nullptr) {
    return visitBoolLiteral(dynamic_cast<BoolLiteral &>(instruct), state);
  } else if (dynamic_cast<FloatLiteral *>(&instruct) != nullptr) {
    return visitFloatLiteral(dynamic_cast<FloatLiteral &>(instruct), state);
  } else if (dynamic_cast<TupleLiteral *>(&instruct) != nullptr) {
    return visitTupleLiteral(dynamic_cast<TupleLiteral &>(instruct), state);
  } else if (dynamic_cast<Address *>(&instruct) != nullptr) {
    return visitAddress(dynamic_cast<Address &>(instruct), state);
  } else if (dynamic_cast<BuiltinOperator *>(&instruct) != nullptr) {
    return visitBuiltinOperator(dynamic_cast<BuiltinOperator &>(instruct),
                                state);
  } else if (dynamic_cast<BuiltinCast *>(&instruct) != nullptr) {
    return visitBuiltinCast(dynamic_cast<BuiltinCast &>(instruct), state);
  } else if (dynamic_cast<Allocation *>(&instruct) != nullptr) {
    return visitAllocation(dynamic_cast<Allocation &>(instruct), state);
  } else if (dynamic_cast<Dereference *>(&instruct) != nullptr) {
    return visitDereference(dynamic_cast<Dereference &>(instruct), state);
  } else if (dynamic_cast<FieldSelect *>(&instruct) != nullptr) {
    return visitFieldSelect(dynamic_cast<FieldSelect &>(instruct), state);
  } else if (dynamic_cast<ForwardIdentifier *>(&instruct) != nullptr) {
    return visitForwardIdentifier(dynamic_cast<ForwardIdentifier &>(instruct),
                                  state);
  } else if (dynamic_cast<GlobalAllocation *>(&instruct) != nullptr) {
    return visitGlobalAllocation(dynamic_cast<GlobalAllocation &>(instruct),
                                 state);
  } else if (dynamic_cast<Specialize *>(&instruct) != nullptr) {
    return visitSpecialize(dynamic_cast<Specialize &>(instruct), state);
  } else if (dynamic_cast<Sizeof *>(&instruct) != nullptr) {
    return visitSizeof(dynamic_cast<Sizeof &>(instruct), state);
  } else if (dynamic_cast<Impl *>(&instruct) != nullptr) {
    return visitImpl(dynamic_cast<Impl &>(instruct), state);
  } else if (dynamic_cast<Select *>(&instruct) != nullptr) {
    return visitSelect(dynamic_cast<Select &>(instruct), state);
  } else if (dynamic_cast<ForwardImpl *>(&instruct) != nullptr) {
    return visitForwardImpl(dynamic_cast<ForwardImpl &>(instruct), state);
  }

  assert(false);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitBasicBlockTerminator(BasicBlockTerminator &instruct,
                                                 const S &state) {
  if (dynamic_cast<Jump *>(&instruct) != nullptr) {
    return visitJump(dynamic_cast<Jump &>(instruct), state);
  } else if (dynamic_cast<ConditionalJump *>(&instruct) != nullptr) {
    return visitConditionalJump(dynamic_cast<ConditionalJump &>(instruct),
                                state);
  } else if (dynamic_cast<Return *>(&instruct) != nullptr) {
    return visitReturn(dynamic_cast<Return &>(instruct), state);
  }

  assert(false);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitGenericFunctionDeclare(
    GenericFunctionDeclare &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitFunctionDeclare(FunctionDeclare &instruct,
                                            const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitIntLiteral(IntLiteral &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitBoolLiteral(BoolLiteral &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitFunctionCall(FunctionCall &instruct,
                                         const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitAllocation(Allocation &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitStore(Store &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitBasicBlock(BasicBlock &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitJump(Jump &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitConditionalJump(ConditionalJump &instruct,
                                            const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitAddress(Address &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitDereference(Dereference &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitBuiltinOperator(BuiltinOperator &instruct,
                                            const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitBuiltinCast(BuiltinCast &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitReturn(Return &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitTupleLiteral(TupleLiteral &instruct,
                                         const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitFieldSelect(FieldSelect &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitForwardIdentifier(ForwardIdentifier &instruct,
                                              const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitGlobalAllocation(GlobalAllocation &instruct,
                                             const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitFloatLiteral(FloatLiteral &instruct,
                                         const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitGenericForwardIdentifier(
    GenericForwardIdentifier &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitSpecialize(Specialize &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitSizeof(Sizeof &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitGenericImpl(GenericImpl &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitGenericSelect(GenericSelect &instruct,
                                          const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitImpl(Impl &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitSelect(Select &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitForwardImpl(ForwardImpl &instruct, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseIRVisitor<T, S>::visitForwardGenericImpl(ForwardGenericImpl &instruct,
                                               const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
std::vector<T>
BaseIRVisitor<T, S>::visitInstructions(const InstructionList &instructs,
                                       const S &state) {
  std::vector<T> res;
  size_t len = instructs.size();

  for (size_t i = 0; i < len; i++) {
    res.push_back(visitInstruction(*instructs[i], state));
  }

  return res;
}
} // namespace ovid::ir

#endif