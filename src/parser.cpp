#include "parser.hpp"
#include "error.hpp"
#include <map>

namespace ovid {

    /* note: precedences must start at > 1, as 1 is passed from primary and 0 is for invalid ops */
    std::map<TokenType, int> opPrecedence = {
            {T_ASSIGN, 20},
            {T_ADD, 30},
            {T_SUB, 30},
            {T_MUL, 40},
            {T_DIV, 40},
    };

    bool Parser::isDoneParsing() {
        return tokenizer.curToken.token == T_EOF;
    }

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
    // funccallexpr ::= identifier '(' (expr ',') * expr ')'
    std::unique_ptr<ast::Expression> Parser::parseIdentifier() {
        auto ident = tokenizer.curToken.ident;
        tokenizer.nextToken();
        if(tokenizer.curToken.token == T_LPAREN) {
            std::vector<std::unique_ptr<ast::Expression>> args;
            do {
                tokenizer.nextToken();
                auto expr = parseExpr();
                if(!expr) return nullptr;
                args.push_back(std::move(expr));
            } while(tokenizer.curToken.token == T_COMMA);
            if(tokenizer.curToken.token != T_RPAREN) return logError("Expected ',' or ')' in argument list", tokenizer.curTokenLoc);
            tokenizer.nextToken();
            return std::make_unique<ast::FunctionCall>(std::make_unique<ast::Identifier>(ident), std::move(args));
        } else {
            return std::make_unique<ast::Identifier>(ident);
        }
    }

    // parenexpr ::= '(' expr ')'
    // tupleexpr ::= '(' (expr ',')+ expr ')'
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
                auto loc = tokenizer.curTokenLoc;
                //consume token (the parse methods normally do this)
                tokenizer.nextToken();
                if(tokenizer.curToken.token == T_EOF) return logError("Unexpected EOF", loc);
                else return logError("Unexpected token", loc);
        }
    }

    // expr ::= primary binoprhs
    std::unique_ptr<ast::Expression> Parser::parseExpr() {
        auto leftExpr = parsePrimary();
        if(!leftExpr) return nullptr;

        return parseBinOpRight(1, std::move(leftExpr));
    }

    // binopright ::= ('op' primary) *
    std::unique_ptr<ast::Expression> Parser::parseBinOpRight(int exprPrec, std::unique_ptr<ast::Expression> leftExpr) {
        while(true) {
            /* find precedence of operator (if not operator, implicitly 0) */
            int tokPrec = opPrecedence[tokenizer.curToken.token];
            if(tokPrec < exprPrec) return leftExpr;
            auto op = tokenizer.curToken.token;
            tokenizer.nextToken();

            auto rightExpr = parsePrimary();
            if(!rightExpr) return nullptr;

            int nextPrec = opPrecedence[tokenizer.curToken.token];
            /* if current op is less tightly bound than next, than rightExpr becomes next operators leftExpr */
            if(tokPrec < nextPrec) {
                rightExpr = parseBinOpRight(tokPrec + 1, std::move(rightExpr));
                if(!rightExpr) return nullptr;
            }
            /* otherwise, leftExpr + rightExpr become next leftExpr */
            /* handle assignment expressions */
            if(op == T_ASSIGN) {
                leftExpr = std::make_unique<ast::Assignment>(std::move(leftExpr), std::move(rightExpr));
            } else {
                ast::ExpressionList args;
                args.push_back(std::move(leftExpr));
                args.push_back(std::move(rightExpr));
                leftExpr = std::make_unique<ast::FunctionCall>(std::make_unique<ast::OperatorSymbol>(op), std::move(args));
            }
        }

    }

    // typeExpr ::= 'i8' | 'u8' | ... | 'string'
    // typeExpr ::= 'mut' typeExpr
    std::unique_ptr<ast::Type> Parser::parseType() {
        if(tokenizer.curToken.token == T_MUT) {
            tokenizer.nextToken();
            return std::make_unique<ast::MutType>(parseType());
        }
        if(tokenizer.curToken.token != T_IDENT) return logError("Expected a type expression", tokenizer.curTokenLoc);
        auto type = tokenizer.curToken.ident;
        auto loc = tokenizer.curTokenLoc;
        tokenizer.nextToken();
        if(type == "i8") return std::make_unique<ast::IntType>(8, false);
        if(type == "i16") return std::make_unique<ast::IntType>(16, false);
        if(type == "i32") return std::make_unique<ast::IntType>(32, false);
        if(type == "i64") return std::make_unique<ast::IntType>(64, false);
        if(type == "u8") return std::make_unique<ast::IntType>(8, true);
        if(type == "u16") return std::make_unique<ast::IntType>(16, true);
        if(type == "u32") return std::make_unique<ast::IntType>(32, true);
        if(type == "u64") return std::make_unique<ast::IntType>(64, true);
        if(type == "bool") return std::make_unique<ast::BoolType>();
        if(type == "f32") return std::make_unique<ast::FloatType>(32);
        if(type == "f64") return std::make_unique<ast::FloatType>(64);

        return logError("Invalid type expression", loc);
    }

    // functionproto ::= ident '(' (arg typeExpr ',')* arg typeExpr ')' typeExpr
    std::unique_ptr<ast::FunctionPrototype> Parser::parseFunctionProto() {
        if(tokenizer.curToken.token != T_IDENT) return logError("Expected function name", tokenizer.curTokenLoc);
        std::string name = tokenizer.curToken.ident;

        // left paren
        tokenizer.nextToken();
        if(tokenizer.curToken.token != T_LPAREN) return logError("Expected '('", tokenizer.curTokenLoc);
        // args
        std::vector<std::string> argNames;
        std::vector<std::unique_ptr<ast::Type>> argTypes;

        do {
            tokenizer.nextToken();
            if(tokenizer.curToken.token != T_IDENT) return logError("Expected argument name", tokenizer.curTokenLoc);
            argNames.push_back(tokenizer.curToken.ident);
            tokenizer.nextToken();
            auto type = parseType();
            if(!type) return nullptr;
            argTypes.push_back(std::move(type));
        } while(tokenizer.curToken.token == T_COMMA);
        if(tokenizer.curToken.token != T_RPAREN) return logError("Expected ')' or ',' in argument list", tokenizer.curTokenLoc);
        tokenizer.nextToken();
        auto retType = parseType();

        return std::make_unique<ast::FunctionPrototype>(std::make_unique<ast::FunctionType>(std::move(argTypes), std::move(retType)), std::move(argNames), name);
    }

    std::unique_ptr<ast::Statement> Parser::parseFunctionDecl() {
        // consume 'fn'
        tokenizer.nextToken();
        auto proto = parseFunctionProto();
        if(!proto) return nullptr;
        if(tokenizer.curToken.token != T_LBRK) return logError("Expected '{'", tokenizer.curTokenLoc);
        tokenizer.nextToken();
        ast::StatementList body;
        while(tokenizer.curToken.token != T_RBRK) {
            auto stat = parseStatement();
            if(!stat) return nullptr;
            body.push_back(std::move(stat));
        }
        tokenizer.nextToken();

        return std::make_unique<ast::FunctionDecl>(std::move(proto), std::move(body));

    }

    std::unique_ptr<ast::Statement> Parser::parseModuleDecl() {

    }

    std::unique_ptr<ast::Statement> Parser::parseStatement() {
        switch(tokenizer.curToken.token) {
            case T_FN:
                return parseFunctionDecl();
            case T_MODULE:
                return parseModuleDecl();
            default:
                return parseExpr();
        }
    }

}