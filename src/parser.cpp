#include "parser.hpp"
#include "error.hpp"

namespace ovid {

    std::vector<std::unique_ptr<ast::Node>> Parser::parseProgram() {
        return std::vector<std::unique_ptr<ast::Node>>();
    }

    // intexpr ::= intliteral
    std::unique_ptr<ast::IntLiteral> Parser::parseIntLiteral() {
        auto res = std::make_unique<ast::IntLiteral>(tokenizer.curToken.int_literal);
        tokenizer.nextToken();
        return res;
    }

    // identexpr ::= identifier
    std::unique_ptr<ast::Identifier> Parser::parseIdentifier() {
        auto res = std::make_unique<ast::Identifier>(tokenizer.curToken.ident);
        tokenizer.nextToken();
        return res;
    }

    // parenexpr ::= '(' expr ')'
    // tupleexpr ::= '(' (expr ',')+ expr ','? ')'
    std::unique_ptr<ast::Expression> Parser::parseParenExpr() {
        tokenizer.nextToken(); // skip '('
        auto expr0 = parseExpr();
        if(!expr0) return nullptr;
        /* check for right paren or comma, indicating tuple */
        if(tokenizer.curToken.token == T_RPAREN) {
            tokenizer.nextToken();
            return expr0;
        }
        if (tokenizer.curToken.token != T_COMMA) return logError("Expected ')' or ',' ", tokenizer.curTokenLoc);
        std::vector<std::unique_ptr<ast::Expression>> tupleExprs;
        tupleExprs.push_back(std::move(expr0));
        // add each element to tuple
        do {
            tokenizer.nextToken();
            auto expr = parseExpr();
            if (!expr) return nullptr;
            tupleExprs.push_back(std::move(expr));
        } while (tokenizer.curToken.token == T_COMMA);
        if (tokenizer.curToken.token == T_RPAREN) {
            tokenizer.nextToken();
            return std::make_unique<ast::Tuple>(std::move(tupleExprs));
        }
        return logError("Expected ')' or ','", tokenizer.curTokenLoc);
    }

    std::unique_ptr<ast::Expression> Parser::parsePrimary() {
        switch(tokenizer.curToken.token) {
            case T_IDENT:
                return parseIdentifier();
            case T_LPAREN:
                return parseParenExpr();
            case T_INTLITERAL:
                return parseIntLiteral();
            default:
                return logError("Unexpected token", tokenizer.curTokenLoc);
        }
    }

    std::unique_ptr<ast::Expression> Parser::parseExpr() {
        return parsePrimary();
    }

}