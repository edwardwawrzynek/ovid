#ifndef H_AST_VISITOR_INCL
#define H_AST_VISITOR_INCL

#include "ast.hpp"
#include "symbols.hpp"
#include <type_traits>

/* the base generic ast visitor
 * the visitor calls the overridded methods on each ast node
 * the base visitor maintains active scopes */
namespace ovid::ast {

template <class T> class BaseASTVisitor {
  T defaultValue;

public:
  explicit BaseASTVisitor(T defaultValue) : defaultValue(defaultValue){};
  virtual T visitNode(Node &node);

  virtual T visitStatement(Statement &node);
  virtual T visitVarDecl(VarDecl &node);
  virtual T visitFunctionDecl(FunctionDecl &node);
  virtual T visitModuleDecl(ModuleDecl &node);

  virtual T visitExpression(Expression &node);
  virtual T visitFunctionCall(FunctionCall &node);
  virtual T visitIdentifier(Identifier &node);
  virtual T visitOperatorSymbol(OperatorSymbol &node);
  virtual T visitAssignment(Assignment &node);
  virtual T visitLiteral(Literal &node);
  virtual T visitIntLiteral(IntLiteral &node);
  virtual T visitTuple(Tuple &node);
};

template <class T> T BaseASTVisitor<T>::visitNode(Node &node) {
  if (dynamic_cast<Statement *>(&node) != nullptr) {
    return visitStatement(dynamic_cast<Statement &>(node));
  } else if (dynamic_cast<Expression *>(&node) != nullptr) {
    return visitStatement(dynamic_cast<Expression &>(node));
  }
  assert(false);
}

template <class T> T BaseASTVisitor<T>::visitStatement(Statement &node) {
  if (dynamic_cast<VarDecl *>(&node) != nullptr) {
    return visitVarDecl(dynamic_cast<VarDecl &>(node));
  } else if (dynamic_cast<FunctionDecl *>(&node) != nullptr) {
    return visitFunctionDecl(dynamic_cast<FunctionDecl &>(node));
  } else if (dynamic_cast<ModuleDecl *>(&node)) {
    return visitModuleDecl(dynamic_cast<ModuleDecl &>(node));
  }
  assert(false);
}
template <class T> T BaseASTVisitor<T>::visitExpression(Expression &node) {
  if (dynamic_cast<FunctionCall *>(&node) != nullptr) {
    return visitFunctionCall(dynamic_cast<FunctionCall &>(node));
  } else if (dynamic_cast<Identifier *>(&node) != nullptr) {
    return visitIdentifier(dynamic_cast<Identifier &>(node));
  } else if (dynamic_cast<OperatorSymbol *>(&node) != nullptr) {
    return visitOperatorSymbol(dynamic_cast<OperatorSymbol &>(node));
  } else if (dynamic_cast<Assignment *>(&node) != nullptr) {
    return visitAssignment(dynamic_cast<Assignment &>(node));
  } else if (dynamic_cast<Literal *>(&node) != nullptr) {
    return visitLiteral(dynamic_cast<Literal &>(node));
  } else if (dynamic_cast<Tuple *>(&node) != nullptr) {
    return visitTuple(dynamic_cast<Tuple &>(node));
  }
  assert(false);
}
template <class T> T BaseASTVisitor<T>::visitLiteral(Literal &node) {
  if (dynamic_cast<IntLiteral *>(&node) != nullptr) {
    return visitIntLiteral(dynamic_cast<IntLiteral &>(node));
  }
  assert(false);
}
template <class T> T BaseASTVisitor<T>::visitVarDecl(VarDecl &node) {
  return defaultValue;
}
template <class T> T BaseASTVisitor<T>::visitFunctionDecl(FunctionDecl &node) {
  return defaultValue;
}
template <class T> T BaseASTVisitor<T>::visitModuleDecl(ModuleDecl &node) {
  return defaultValue;
}
template <class T> T BaseASTVisitor<T>::visitFunctionCall(FunctionCall &node) {
  return defaultValue;
}
template <class T> T BaseASTVisitor<T>::visitIdentifier(Identifier &node) {
  return defaultValue;
}
template <class T>
T BaseASTVisitor<T>::visitOperatorSymbol(OperatorSymbol &node) {
  return defaultValue;
}
template <class T> T BaseASTVisitor<T>::visitAssignment(Assignment &node) {
  return defaultValue;
}
template <class T> T BaseASTVisitor<T>::visitIntLiteral(IntLiteral &node) {
  return defaultValue;
}
template <class T> T BaseASTVisitor<T>::visitTuple(Tuple &node) {
  return defaultValue;
}

} // namespace ovid::ast

#endif