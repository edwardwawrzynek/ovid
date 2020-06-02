#include "ast.hpp"
#include "ast_visitor.hpp"

namespace ovid::ast {

class ASTPrinterState {
public:
  uint64_t indent_level;

  /* create a new state with an additional indent level */
  ASTPrinterState withIndent() const;

  /* print ident */
  void printIndent() const;

  ASTPrinterState(): indent_level(0) {};
};

class ASTTypePrinter: public BaseTypeVisitor<int, ASTPrinterState> {

public:
  ASTTypePrinter(): BaseTypeVisitor(0) {};
};

class ASTPrinter: public BaseASTVisitor<int, ASTPrinterState> {
  ASTTypePrinter typePrinter;

  int visitVarDecl(VarDecl &node, const ASTPrinterState &state) override;
  int visitFunctionDecl(FunctionDecl &node, const ASTPrinterState &state) override;
  int visitModuleDecl(ModuleDecl &node, const ASTPrinterState &state) override;
  int visitIfStatement(IfStatement &node, const ASTPrinterState &state) override;

  int visitFunctionCall(FunctionCall &node, const ASTPrinterState &state) override;
  int visitIdentifier(Identifier &node, const ASTPrinterState &state) override;
  int visitOperatorSymbol(OperatorSymbol &node, const ASTPrinterState &state) override;
  int visitAssignment(Assignment &node, const ASTPrinterState &state) override;
  int visitIntLiteral(IntLiteral &node, const ASTPrinterState &state) override;
  int visitBoolLiteral(BoolLiteral &node, const ASTPrinterState &state) override;
  int visitTuple(Tuple &node, const ASTPrinterState &state) override;
  int visitTypeAliasDecl(TypeAliasDecl &node, const ASTPrinterState &state) override;

  void printLoc(const SourceLocation& loc);

public:
  ASTPrinter(): BaseASTVisitor(0), typePrinter() {};
};

}