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

        std::vector<std::unique_ptr<ast::Node>> parseProgram();
        std::unique_ptr<ast::IntLiteral> parseIntLiteral();

        std::unique_ptr<ast::Identifier> parseIdentifier();

        std::unique_ptr<ast::Expression> parseParenExpr();

        std::unique_ptr<ast::Expression> parseExpr();

        std::unique_ptr<ast::Expression> parsePrimary();
    };
}

#endif