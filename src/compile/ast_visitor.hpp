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

  virtual T visitFunctionCall(FunctionCall &node, const S &state);
  virtual T visitIdentifier(Identifier &node, const S &state);
  virtual T visitOperatorSymbol(OperatorSymbol &node, const S &state);
  virtual T visitAssignment(Assignment &node, const S &state);
  virtual T visitIntLiteral(IntLiteral &node, const S &state);
  virtual T visitTuple(Tuple &node, const S &state);

public:
  explicit BaseASTVisitor(T defaultValue) : defaultValue(defaultValue){};
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
  }
  assert(false);
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitVarDecl(VarDecl &node, const S &state) {
  return defaultValue;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitFunctionDecl(FunctionDecl &node, const S &state) {
  return defaultValue;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitModuleDecl(ModuleDecl &node, const S &state) {
  return defaultValue;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitFunctionCall(FunctionCall &node, const S &state) {
  return defaultValue;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitIdentifier(Identifier &node, const S &state) {
  return defaultValue;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitOperatorSymbol(OperatorSymbol &node,
                                            const S &state) {
  return defaultValue;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitAssignment(Assignment &node, const S &state) {
  return defaultValue;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitIntLiteral(IntLiteral &node, const S &state) {
  return defaultValue;
}
template <class T, class S>
T BaseASTVisitor<T, S>::visitTuple(Tuple &node, const S &state) {
  return defaultValue;
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

} // namespace ovid::ast

#endif