#ifndef H_PARSER_INCL
#define H_PARSER_INCL

#include "ast.hpp"
#include "error.hpp"
#include "tokenizer.hpp"
#include <memory>
#include <vector>

namespace ovid {
struct ParserState {
  // if we are in a global declaration or inside a function
  bool global_level;
  // most recent scope containing block (what scope we are in)
  ast::ScopedBlock *current_scope;
  // current module (including package) name
  std::vector<std::string> current_module;

  ParserState()
      : global_level(true), current_scope(nullptr), current_module(){};
};

class Parser {
  Tokenizer &tokenizer;
  ErrorManager &errorMan;
  ActiveScopes &scopes;

public:
  explicit Parser(Tokenizer &tokenizer, ErrorManager &errorMan,
                  ActiveScopes &scopes)
      : tokenizer(tokenizer), errorMan(errorMan), scopes(scopes){};

  std::vector<std::unique_ptr<ast::Statement>> parseProgram();

  std::unique_ptr<ast::IntLiteral> parseIntLiteral();

  std::unique_ptr<ast::Expression> parseIdentifier();

  std::unique_ptr<ast::Expression> parseParenExpr();

  std::unique_ptr<ast::Expression> parseExpr();

  std::unique_ptr<ast::Expression> parsePrimary();

  std::unique_ptr<ast::Expression>
  parseBinOpRight(int exprPrec, std::unique_ptr<ast::Expression> leftExpr);

  std::unique_ptr<ast::Statement> parseStatement();

  bool isDoneParsing();

  std::unique_ptr<ast::Statement> parseFunctionDecl();

  std::unique_ptr<ast::FunctionPrototype> parseFunctionProto();

  std::unique_ptr<ast::Type> parseType();

  std::unique_ptr<ast::Statement> parseVarDecl();

  bool expectEndStatement();

  std::unique_ptr<ast::ModuleDecl> parseModuleDecl();
};
} // namespace ovid

#endif