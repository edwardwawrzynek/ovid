#ifndef H_IR_VISITOR_INCL
#define H_IR_VISITOR_INCL

#include "ir.hpp"

namespace ovid::ir {

template <class T, class S> class BaseIRVisitor {
  T defaultValue;

  virtual T visitExpression(Expression &instruct, const S &state);

  virtual T visitFunctionDeclare(FunctionDeclare &instruct, const S &state);
  virtual T visitIntLiteral(IntLiteral &instruct, const S &state);
  virtual T visitBoolLiteral(BoolLiteral &instruct, const S &state);
  virtual T visitFunctionCall(FunctionCall &instruct, const S &state);
  virtual T visitAllocation(Allocation &instruct, const S &state);

  virtual T visitStore(Store &instruct, const S &state);
  virtual T visitLabel(Label &instruct, const S &state);
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
  } else if (dynamic_cast<Label *>(&instruct) != nullptr) {
    return visitLabel(dynamic_cast<Label &>(instruct), state);
  } else if (dynamic_cast<Jump *>(&instruct) != nullptr) {
    return visitJump(dynamic_cast<Jump &>(instruct), state);
  } else if (dynamic_cast<ConditionalJump *>(&instruct) != nullptr) {
    return visitConditionalJump(dynamic_cast<ConditionalJump &>(instruct),
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
  } else if (dynamic_cast<Allocation *>(&instruct) != nullptr) {
    return visitAllocation(dynamic_cast<Allocation &>(instruct), state);
  }

  assert(false);
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
T BaseIRVisitor<T, S>::visitLabel(Label &instruct, const S &state) {
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
std::vector<T>
BaseIRVisitor<T, S>::visitInstructions(const InstructionList &instructs,
                                       const S &state) {
  std::vector<T> res;

  for (auto &i : instructs) {
    res.push_back(visitInstruction(*i, state));
  }

  return res;
}

} // namespace ovid::ir

#endif