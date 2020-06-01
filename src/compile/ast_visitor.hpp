#ifndef H_AST_VISITOR_INCL
#define H_AST_VISITOR_INCL

#include "ast.hpp"
#include "symbols.hpp"
#include <type_traits>

/* the base generic ast visitor
 * the visitor calls the overridded methods on each ast node
 * the base visitor maintains active scopes */
namespace ovid::ast {

template <class T, class S> class BaseASTVisitor {
  T defaultValue;

  virtual T visitStatement(Statement &node, const S &state);
  virtual T visitExpression(Expression &node, const S &state);
  virtual T visitLiteral(Literal &node, const S &state);

  virtual T visitVarDecl(VarDecl &node, const S &state);
  virtual T visitFunctionDecl(FunctionDecl &node, const S &state);
  virtual T visitModuleDecl(ModuleDecl &node, const S &state);
  virtual T visitIfStatement(IfStatement &node, const S &state);

  virtual T visitFunctionCall(FunctionCall &node, const S &state);
  virtual T visitIdentifier(Identifier &node, const S &state);
  virtual T visitOperatorSymbol(OperatorSymbol &node, const S &state);
  virtual T visitAssignment(Assignment &node, const S &state);
  virtual T visitIntLiteral(IntLiteral &node, const S &state);
  virtual T visitBoolLiteral(BoolLiteral &node, const S &state);
  virtual T visitTuple(Tuple &node, const S &state);
  virtual T visitTypeAliasDecl(TypeAliasDecl &node, const S &state);

public:
  explicit BaseASTVisitor(T defaultValue)
      : defaultValue(std::move(defaultValue)){};
  virtual T visitNode(Node &node, const S &state);
  virtual std::vector<T> visitNodes(const StatementList &nodes, const S &state);

  virtual ~BaseASTVisitor() = default;
};

template <class T, class S>
T BaseASTVisitor<T, S>::visitNode(Node &node, const S &state) {
  if (dynamic_cast<Statement *>(&node) != nullptr) {
    return visitStatement(dynamic_cast<Statement &>(node), state);
  }
  assert(false);
}

template <class T, class S>
T BaseASTVisitor<T, S>::visitStatement(Statement &node, const S &state) {
  if (dynamic_cast<VarDecl *>(&node) != nullptr) {
    return visitVarDecl(dynamic_cast<VarDecl &>(node), state);
  } else if (dynamic_cast<FunctionDecl *>(&node) != nullptr) {
    return visitFunctionDecl(dynamic_cast<FunctionDecl &>(node), state);
  } else if (dynamic_cast<ModuleDecl *>(&node)) {
    return visitModuleDecl(dynamic_cast<ModuleDecl &>(node), state);
  } else if (dynamic_cast<TypeAliasDecl *>(&node) != nullptr) {
    return visitTypeAliasDecl(dynamic_cast<TypeAliasDecl &>(node), state);
  } else if (dynamic_cast<IfStatement *>(&node) != nullptr) {
    return visitIfStatement(dynamic_cast<IfStatement &>(node), state);
  } else if (dynamic_cast<Expression *>(&node) != nullptr) {
    return visitExpression(dynamic_cast<Expression &>(node), state);
  }
  assert(false);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitExpression(Expression &node, const S &state) {
  if (dynamic_cast<FunctionCall *>(&node) != nullptr) {
    return visitFunctionCall(dynamic_cast<FunctionCall &>(node), state);
  } else if (dynamic_cast<Identifier *>(&node) != nullptr) {
    return visitIdentifier(dynamic_cast<Identifier &>(node), state);
  } else if (dynamic_cast<OperatorSymbol *>(&node) != nullptr) {
    return visitOperatorSymbol(dynamic_cast<OperatorSymbol &>(node), state);
  } else if (dynamic_cast<Assignment *>(&node) != nullptr) {
    return visitAssignment(dynamic_cast<Assignment &>(node), state);
  } else if (dynamic_cast<Literal *>(&node) != nullptr) {
    return visitLiteral(dynamic_cast<Literal &>(node), state);
  } else if (dynamic_cast<Tuple *>(&node) != nullptr) {
    return visitTuple(dynamic_cast<Tuple &>(node), state);
  }
  assert(false);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitLiteral(Literal &node, const S &state) {
  if (dynamic_cast<IntLiteral *>(&node) != nullptr) {
    return visitIntLiteral(dynamic_cast<IntLiteral &>(node), state);
  } else if (dynamic_cast<BoolLiteral *>(&node) != nullptr) {
    return visitBoolLiteral(dynamic_cast<BoolLiteral &>(node), state);
  }
  assert(false);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitVarDecl(VarDecl &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitFunctionDecl(FunctionDecl &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitModuleDecl(ModuleDecl &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitIfStatement(IfStatement &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitFunctionCall(FunctionCall &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitIdentifier(Identifier &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitOperatorSymbol(OperatorSymbol &node,
                                            const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitAssignment(Assignment &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitIntLiteral(IntLiteral &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitBoolLiteral(BoolLiteral &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitTuple(Tuple &node, const S &state) {
  return std::move(defaultValue);
}
template <class T, class S>
std::vector<T> BaseASTVisitor<T, S>::visitNodes(const StatementList &nodes,
                                                const S &state) {
  std::vector<T> res;

  for (auto &n : nodes) {
    res.push_back(visitNode(*n, state));
  }

  return res;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitTypeAliasDecl(TypeAliasDecl &node,
                                           const S &state) {
  return std::move(defaultValue);
}

/* the base generic type visitor
 * basically the same as BaseASTVisitor, but for type expressions */
template <class T, class S> class BaseTypeVisitor {
  T defaultValue;

  virtual T visitUnresolvedType(UnresolvedType &type, const S &state);

  virtual T visitVoidType(VoidType &type, const S &state);
  virtual T visitBoolType(BoolType &type, const S &state);
  virtual T visitIntType(IntType &type, const S &state);
  virtual T visitFloatType(FloatType &type, const S &state);

  virtual T visitMutType(MutType &type, const S &state);
  virtual T visitPointerType(PointerType &type, const S &state);

  virtual T visitFunctionType(FunctionType &type, const S &state);
  virtual T visitNamedFunctionType(NamedFunctionType &type, const S &state);

public:
  explicit BaseTypeVisitor(T defaultValue)
      : defaultValue(std::move(defaultValue)){};
  virtual T visitType(Type &type, const S &state);

  virtual ~BaseTypeVisitor() = default;
};

template <class T, class S>
T BaseTypeVisitor<T, S>::visitType(Type &type, const S &state) {
  if (dynamic_cast<UnresolvedType *>(&type) != nullptr) {
    return visitUnresolvedType(dynamic_cast<UnresolvedType &>(type), state);
  } else if (dynamic_cast<VoidType *>(&type) != nullptr) {
    return visitVoidType(dynamic_cast<VoidType &>(type), state);
  } else if (dynamic_cast<BoolType *>(&type) != nullptr) {
    return visitBoolType(dynamic_cast<BoolType &>(type), state);
  } else if (dynamic_cast<IntType *>(&type) != nullptr) {
    return visitIntType(dynamic_cast<IntType &>(type), state);
  } else if (dynamic_cast<FloatType *>(&type) != nullptr) {
    return visitFloatType(dynamic_cast<FloatType &>(type), state);
  } else if (dynamic_cast<MutType *>(&type) != nullptr) {
    return visitMutType(dynamic_cast<MutType &>(type), state);
  } else if (dynamic_cast<PointerType *>(&type) != nullptr) {
    return visitPointerType(dynamic_cast<PointerType &>(type), state);
  } else if (dynamic_cast<FunctionType *>(&type) != nullptr) {
    return visitFunctionType(dynamic_cast<FunctionType &>(type), state);
  } else if (dynamic_cast<NamedFunctionType *>(&type) != nullptr) {
    return visitNamedFunctionType(dynamic_cast<NamedFunctionType &>(type),
                                  state);
  }

  assert(false);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitUnresolvedType(UnresolvedType &type,
                                             const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitVoidType(VoidType &type, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitBoolType(BoolType &type, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitIntType(IntType &type, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitFloatType(FloatType &type, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitMutType(MutType &type, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitPointerType(PointerType &type, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitFunctionType(FunctionType &type, const S &state) {
  return std::move(defaultValue);
}

template <class T, class S>
T BaseTypeVisitor<T, S>::visitNamedFunctionType(NamedFunctionType &type,
                                                const S &state) {
  return std::move(defaultValue);
}

} // namespace ovid::ast

#endif