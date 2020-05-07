#ifndef H_PARSER_INCL
#define H_PARSER_INCL

#include "ast.hpp"
#include "error.hpp"
#include "tokenizer.hpp"
#include <memory>
#include <utility>
#include <vector>

namespace ovid {
struct ParserState {
  // if we are in a global declaration or inside a function
  bool is_global_level;
  // most recent scope containing block (what scope we are in)
  std::shared_ptr<ScopeTable<Symbol>> current_scope;
  std::shared_ptr<ScopeTable<TypeAlias>> current_type_scope;

  ParserState(bool is_global_level,
              std::shared_ptr<ScopeTable<Symbol>> current_scope,
              std::shared_ptr<ScopeTable<TypeAlias>> current_type_scope)
      : is_global_level(is_global_level),
        current_scope(std::move(current_scope)),
        current_type_scope(std::move(current_type_scope)){};
};

class Parser {
  Tokenizer &tokenizer;
  ErrorManager &errorMan;
  ActiveScopes &scopes;

public:
  explicit Parser(Tokenizer &tokenizer, ErrorManager &errorMan,
                  ActiveScopes &scopes)
      : tokenizer(tokenizer), errorMan(errorMan), scopes(scopes){};

  std::vector<std::unique_ptr<ast::Statement>>
  parseProgram(const std::vector<std::string> &packageName);

  std::unique_ptr<ast::IntLiteral> parseIntLiteral(const ParserState &state);

  std::unique_ptr<ast::Expression> parseIdentifier(const ParserState &state);

  std::unique_ptr<ast::Expression> parseParenExpr(const ParserState &state);

  std::unique_ptr<ast::Expression> parseExpr(const ParserState &state);

  std::unique_ptr<ast::Expression> parsePrimary(const ParserState &state);

  std::unique_ptr<ast::Expression>
  parseBinOpRight(const ParserState &state, int exprPrec,
                  std::unique_ptr<ast::Expression> leftExpr);

  std::unique_ptr<ast::Statement> parseStatement(const ParserState &state);

  bool isDoneParsing();

  std::unique_ptr<ast::Statement> parseFunctionDecl(const ParserState &state);

  std::unique_ptr<ast::FunctionPrototype>
  parseFunctionProto(const ParserState &state);

  std::unique_ptr<ast::Type> parseType(const ParserState &state);

  std::unique_ptr<ast::Statement> parseVarDecl(const ParserState &state);

  bool expectEndStatement();

  std::unique_ptr<ast::ModuleDecl> parseModuleDecl(const ParserState &state);
};
} // namespace ovid

#endif