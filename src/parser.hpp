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
  ScopeTable<Symbol> *current_scope;
  ScopeTable<TypeAlias> *current_type_scope;
  // what module we are in
  std::vector<std::string> current_module;

  // if a struct literal is allowed in the current position
  // needed to resolve ambigouity with struct literals in front of blocks
  // eg:
  // if Type { field: 1} { /* ... */ }
  // is ambiguous at parse time (without unbounded look ahead), so struct
  // literals are disallowed in some positions
  bool struct_literals_allowed;

  ParserState allowStructLiterals() const;
  ParserState disallowStructLiterals() const;

  ParserState(bool is_global_level, ScopeTable<Symbol> *current_scope,
              ScopeTable<TypeAlias> *current_type_scope,
              const std::vector<std::string> &current_module,
              bool in_private_mod, bool struct_literals_allowed = true)
      : is_global_level(is_global_level), in_private_mod(in_private_mod),
        current_scope(current_scope), current_type_scope(current_type_scope),
        current_module(current_module),
        struct_literals_allowed(struct_literals_allowed){};
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

  std::unique_ptr<ast::CharLiteral> parseCharLiteral(const ParserState &state);

  std::unique_ptr<ast::FloatLiteral>
  parseFloatLiteral(const ParserState &state);

  std::unique_ptr<ast::Expression>
  parseStructLiteral(const ParserState &state, const SourceLocation &loc_start,
                     std::shared_ptr<ast::UnresolvedType> type_expr);

  std::unique_ptr<ast::Expression> parseIdentifier(const ParserState &state);

  std::unique_ptr<ast::Expression> parseParenExpr(const ParserState &state);

  std::unique_ptr<ast::Expression> parseExpr(const ParserState &state);

  std::unique_ptr<ast::Expression> parsePrimary(const ParserState &state);

  std::unique_ptr<ast::Expression>
  parseFunctionCall(const ParserState &state,
                    std::unique_ptr<ast::Expression> funcExpr);
  std::unique_ptr<ast::Expression>
  parseFieldAccess(const ParserState &state,
                   std::unique_ptr<ast::Expression> Expr);

  std::unique_ptr<ast::Expression> parsePostfixOp(const ParserState &state);

  std::unique_ptr<ast::Expression> parsePrefixOp(const ParserState &state);

  std::unique_ptr<ast::Expression> parseBinExpr(const ParserState &state,
                                                int prevPec);

  std::unique_ptr<ast::Statement> parseStatement(const ParserState &state);

  bool isDoneParsing() const;

  std::unique_ptr<ast::Statement> parseFunctionDecl(const ParserState &state,
                                                    bool is_public);

  std::shared_ptr<ast::NamedFunctionType>
  parseNamedFunctionType(const ParserState &state,
                         std::vector<SourceLocation> *argLocs);

  std::unique_ptr<ast::FunctionPrototype>
  parseFunctionProto(const ParserState &state,
                     std::vector<SourceLocation> *argLocs);

  std::shared_ptr<ast::Type> parseType(const ParserState &state);
  std::shared_ptr<ast::Type> parseType(const ParserState &state,
                                       bool is_root_of_type);

  std::unique_ptr<ast::Statement> parseVarDecl(const ParserState &state,
                                               bool is_public);

  bool expectEndStatement();
  bool isEndStatement();

  std::unique_ptr<ast::ReturnStatement>
  parseReturnStatement(const ParserState &state);

  std::unique_ptr<ast::ModuleDecl> parseModuleDecl(const ParserState &state,
                                                   bool is_public);

  std::vector<std::string> readScopedName(const ParserState &state);
  std::vector<std::string> readScopedName(const ParserState &state,
                                          std::vector<SourceLocation> *locs);

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

  ast::FormalTypeParameterList
  parseFormalTypeParameterList(const ParserState &state, bool bounds);

  ast::TypeList parseTypeParameterList(const ParserState &state);

  std::unique_ptr<ast::TypeAliasDecl>
  parseStructStatement(const ParserState &state, bool is_public);

  std::unique_ptr<ast::IfStatement> parseIfStatement(const ParserState &state);

  std::unique_ptr<ast::NativeFunctionDecl>
  parseNativeStatement(const ParserState &state);

  std::unique_ptr<ast::WhileStatement>
  parseWhileStatement(const ParserState &state);

  void addTypeAlias(const ParserState &state, const std::string &name,
                    const std::shared_ptr<TypeAlias> &alias);

  void expectSnakeCase(const std::string &name, SourceLocation &pos);
  void expectUpperCamelCase(const std::string &name, SourceLocation &pos);

public:
  Parser(Tokenizer &tokenizer, ErrorManager &errorMan, ActiveScopes &scopes,
         const std::vector<std::string> &package);

  void removePushedPackageScope();

  ast::StatementList parseProgram();
};
} // namespace ovid

#endif