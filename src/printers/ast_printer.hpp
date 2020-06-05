#ifndef H_AST_PRINTER_INCL
#define H_AST_PRINTER_INCL

#include "ast.hpp"
#include "ast_visitor.hpp"

namespace ovid::ast {

void printLoc(std::ostream &output, const SourceLocation &loc);

class ASTPrinterState {
public:
  uint64_t indent_level;

  /* create a new state with an additional indent level */
  ASTPrinterState withIndent() const;

  /* print ident */
  void printIndent(std::ostream &output) const;

  ASTPrinterState() : indent_level(0){};
};

class ASTTypePrinter : public BaseTypeVisitor<int, ASTPrinterState> {

  std::ostream &output;

  int visitUnresolvedType(UnresolvedType &type,
                          const ASTPrinterState &state) override;
  int visitResolvedAlias(ResolvedAlias &type,
                         const ASTPrinterState &state) override;

  int visitVoidType(VoidType &type, const ASTPrinterState &state) override;
  int visitBoolType(BoolType &type, const ASTPrinterState &state) override;
  int visitIntType(IntType &type, const ASTPrinterState &state) override;
  int visitFloatType(FloatType &type, const ASTPrinterState &state) override;

  int visitMutType(MutType &type, const ASTPrinterState &state) override;
  int visitPointerType(PointerType &type,
                       const ASTPrinterState &state) override;

  int visitFunctionType(FunctionType &type,
                        const ASTPrinterState &state) override;
  int visitNamedFunctionType(NamedFunctionType &type,
                             const ASTPrinterState &state) override;

public:
  explicit ASTTypePrinter(std::ostream &output)
      : BaseTypeVisitor(0), output(output){};
};

class ASTPrinter : public BaseASTVisitor<int, ASTPrinterState> {
  ASTTypePrinter typePrinter;
  std::ostream &output;

  int visitVarDecl(VarDecl &node, const ASTPrinterState &state) override;
  int visitFunctionDecl(FunctionDecl &node,
                        const ASTPrinterState &state) override;
  int visitModuleDecl(ModuleDecl &node, const ASTPrinterState &state) override;
  int visitIfStatement(IfStatement &node,
                       const ASTPrinterState &state) override;

  int visitFunctionCall(FunctionCall &node,
                        const ASTPrinterState &state) override;
  int visitIdentifier(Identifier &node, const ASTPrinterState &state) override;
  int visitOperatorSymbol(OperatorSymbol &node,
                          const ASTPrinterState &state) override;
  int visitAssignment(Assignment &node, const ASTPrinterState &state) override;
  int visitIntLiteral(IntLiteral &node, const ASTPrinterState &state) override;
  int visitBoolLiteral(BoolLiteral &node,
                       const ASTPrinterState &state) override;
  int visitTuple(Tuple &node, const ASTPrinterState &state) override;
  int visitTypeAliasDecl(TypeAliasDecl &node,
                         const ASTPrinterState &state) override;

public:
  explicit ASTPrinter(std::ostream &output)
      : BaseASTVisitor(0), typePrinter(output), output(output){};
};

} // namespace ovid::ast

#endif