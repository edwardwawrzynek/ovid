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
  // if we are in a private module
  bool in_private_mod;
  // most recent scope containing block (what scope we are in)
  std::shared_ptr<ScopeTable<Symbol>> current_scope;
  std::shared_ptr<ScopeTable<TypeAlias>> current_type_scope;
  // what module we are in
  std::vector<std::string> current_module;

  ParserState(bool is_global_level,
              std::shared_ptr<ScopeTable<Symbol>> current_scope,
              std::shared_ptr<ScopeTable<TypeAlias>> current_type_scope,
              std::vector<std::string> current_module, bool in_private_mod)
      : is_global_level(is_global_level), in_private_mod(in_private_mod),
        current_scope(std::move(current_scope)),
        current_type_scope(std::move(current_type_scope)),
        current_module(std::move(current_module)){};
};

class Parser {
  Tokenizer &tokenizer;
  ErrorManager &errorMan;
  ActiveScopes &scopes;
  const std::vector<std::string> &package;

  std::string getFullyScopedName(const std::string &name,
                                 const ParserState &state);

  bool checkRedeclaration(const SourceLocation &pos, const std::string &name,
                          const ParserState &state);

  // convert token types to ast::OperatorType
  ast::OperatorType infixTokenToOperatorType(TokenType token);
  ast::OperatorType prefixTokenToOperatorType(TokenType token);
  ast::OperatorType postfixTokenToOperatorType(TokenType token);

  // parse a program without a file wide module declaration
  ast::StatementList
  parseProgramWithoutRootModuleDecl(const ParserState &state);

  std::unique_ptr<ast::IntLiteral> parseIntLiteral(const ParserState &state);

  std::unique_ptr<ast::BoolLiteral> parseBoolLiteral(const ParserState &state);

  std::unique_ptr<ast::Expression> parseIdentifier(const ParserState &state);

  std::unique_ptr<ast::Expression> parseParenExpr(const ParserState &state);

  std::unique_ptr<ast::Expression> parseExpr(const ParserState &state);

  std::unique_ptr<ast::Expression> parsePrimary(const ParserState &state);

  std::unique_ptr<ast::Expression> parsePrefixOp(const ParserState &state);

  std::unique_ptr<ast::Expression>
  parseBinOpRight(const ParserState &state, int exprPrec,
                  std::unique_ptr<ast::Expression> leftExpr);

  std::unique_ptr<ast::Statement> parseStatement(const ParserState &state);

  bool isDoneParsing() const;

  std::unique_ptr<ast::Statement> parseFunctionDecl(const ParserState &state,
                                                    bool is_public);

  std::unique_ptr<ast::FunctionPrototype>
  parseFunctionProto(const ParserState &state,
                     std::vector<SourceLocation> *argLocs);

  std::shared_ptr<ast::Type> parseType(const ParserState &state);
  std::shared_ptr<ast::Type> parseType(const ParserState &state,
                                       bool is_root_of_type);

  std::unique_ptr<ast::Statement> parseVarDecl(const ParserState &state,
                                               bool is_public);

  bool expectEndStatement();

  std::unique_ptr<ast::ModuleDecl> parseModuleDecl(const ParserState &state,
                                                   bool is_public);

  std::vector<std::string> readModuleName(const ParserState &state);

  static ParserState newStateForModule(const ParserState &state,
                                       const std::vector<std::string> &names,
                                       bool is_mod_public);

  std::unique_ptr<ast::ModuleDecl>
  parseModuleDeclBody(const ParserState &state, bool is_public,
                      const std::vector<std::string> &names,
                      SourceLocation pos);

  std::unique_ptr<ast::Statement>
  parsePossiblePubStatement(const ParserState &state, bool is_public);

  std::unique_ptr<ast::TypeAliasDecl>
  parseTypeAliasDecl(const ParserState &state, bool is_public);

  std::unique_ptr<ast::IfStatement> parseIfStatement(const ParserState &state);

  void addTypeAlias(const ParserState &state, std::string name,
                    std::shared_ptr<TypeAlias> alias);

public:
  Parser(Tokenizer &tokenizer, ErrorManager &errorMan, ActiveScopes &scopes,
         const std::vector<std::string> &package);

  void removePushedPackageScope();

  ast::StatementList parseProgram();
};
} // namespace ovid

#endif