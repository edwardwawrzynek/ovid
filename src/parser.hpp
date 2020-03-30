#ifndef H_PARSER_INCL
#define H_PARSER_INCL

#include "tokenizer.hpp"
#include <vector>
#include <memory>
#include "ast.hpp"

namespace ovid {
    class Parser {
        Tokenizer tokenizer;

    public:
        explicit Parser(Tokenizer& tokenizer): tokenizer(tokenizer) {};

        std::vector<std::unique_ptr<ast::Statement>> parseProgram();

        std::unique_ptr<ast::IntLiteral> parseIntLiteral();

        std::unique_ptr<ast::Expression> parseIdentifier();

        std::unique_ptr<ast::Expression> parseParenExpr();

        std::unique_ptr<ast::Expression> parseExpr();

        std::unique_ptr<ast::Expression> parsePrimary();

        std::unique_ptr<ast::Expression> parseBinOpRight(int exprPrec, std::unique_ptr<ast::Expression> leftExpr);

        std::unique_ptr<ast::Statement> parseStatement();

        bool isDoneParsing();

        std::unique_ptr<ast::Statement> parseFunctionDecl();

        std::unique_ptr<ast::Statement> parseModuleDecl();

        std::unique_ptr<ast::FunctionPrototype> parseFunctionProto();

        std::unique_ptr<ast::Type> parseType();

        std::unique_ptr<ast::Statement> parseVarDecl();

        bool expectEndStatement();

        std::unique_ptr<ast::ScopeDecl> parseScopeDecl();
    };
}

#endif