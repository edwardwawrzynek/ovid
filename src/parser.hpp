#ifndef H_PARSER_INCL
#define H_PARSER_INCL

#include "ast.hpp"
#include "error.hpp"
#include "tokenizer.hpp"
#include <memory>
#include <vector>

namespace ovid {
  class Parser {
    Tokenizer &tokenizer;
    ErrorManager &errorMan;

  public:
    explicit Parser(Tokenizer &tokenizer, ErrorManager &errorMan) :
        tokenizer(tokenizer),
        errorMan(errorMan){};

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
}// namespace ovid

#endif