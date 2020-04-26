#include "parser.hpp"
#include "error.hpp"
#include <map>

namespace ovid {

  /* note: precedences must start at > 1, as 1 is passed from primary and 0 is for invalid ops */
  static std::map<TokenType, int> opPrecedence = {
    {T_ASSIGN, 20},
    {T_ADD, 30},
    {T_SUB, 30},
    {T_STAR, 40},
    {T_DIV, 40},
  };

  bool Parser::isDoneParsing() {
    return tokenizer.curToken.token == T_EOF;
  }

  std::vector<std::unique_ptr<ast::Statement>> Parser::parseProgram() {
    std::vector<std::unique_ptr<ast::Statement>> nodes;
    do {
      auto ast = parseStatement();
      if (ast) nodes.push_back(std::move(ast));
    } while (!isDoneParsing());

    return nodes;
  }

  // intexpr ::= intliteral
  std::unique_ptr<ast::IntLiteral> Parser::parseIntLiteral() {
    auto res = std::make_unique<ast::IntLiteral>(tokenizer.curTokenLoc,
                                                 tokenizer.curToken.int_literal);
    tokenizer.nextToken();
    return res;
  }

  // identexpr ::= identifier (':' identifier)*
  // funccallexpr ::= identifier (':' identifier)* '(' (expr ',') * expr ')'
  std::unique_ptr<ast::Expression> Parser::parseIdentifier() {
    std::vector<std::string> scopes;
    std::string ident;
    auto pos = tokenizer.curTokenLoc;
    while (true) {
      ident = tokenizer.curToken.ident;
      tokenizer.nextToken();
      if (tokenizer.curToken.token == T_COLON) {
        scopes.push_back(ident);
        tokenizer.nextToken();
      } else
        break;
    }
    if (tokenizer.curToken.token == T_LPAREN) {
      std::vector<std::unique_ptr<ast::Expression>> args;
      do {
        tokenizer.nextToken();
        auto expr = parseExpr();
        if (!expr) return nullptr;
        args.push_back(std::move(expr));
      } while (tokenizer.curToken.token == T_COMMA);
      if (tokenizer.curToken.token != T_RPAREN)
        return errorMan.logError("Expected ',' or ')' in argument list", tokenizer.curTokenLoc,
                                 ErrorType::ParseError);
      tokenizer.nextToken();
      return std::make_unique<ast::FunctionCall>(pos,
                                                 std::make_unique<ast::Identifier>(pos, ident,
                                                                                   std::move(
                                                                                     scopes)),
                                                 std::move(args));
    } else {
      return std::make_unique<ast::Identifier>(pos, ident, std::move(scopes));
    }
  }

  // parenexpr ::= '(' expr ')'
  // tupleexpr ::= '(' (expr ',')+ expr ')'
  std::unique_ptr<ast::Expression> Parser::parseParenExpr() {
    auto pos = tokenizer.curTokenLoc;

    tokenizer.nextToken();// skip '('
    auto expr0 = parseExpr();
    if (!expr0) return nullptr;
    /* check for right paren or comma, indicating tuple */
    if (tokenizer.curToken.token == T_RPAREN) {
      tokenizer.nextToken();
      return expr0;
    }
    if (tokenizer.curToken.token != T_COMMA)
      return errorMan.logError("Expected ')' or ',' ", tokenizer.curTokenLoc,
                               ErrorType::ParseError);
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
      return std::make_unique<ast::Tuple>(pos, std::move(tupleExprs));
    }
    return errorMan.logError("Expected ')' or ','", tokenizer.curTokenLoc, ErrorType::ParseError);
  }

  std::unique_ptr<ast::Expression> Parser::parsePrimary() {
    switch (tokenizer.curToken.token) {
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
        if (tokenizer.curToken.token == T_EOF) {
          // TODO: bail out of parsing
          return errorMan.logError("unexpected EOF", loc, ErrorType::ParseError);
        } else
          return errorMan.logError("unexpected token", loc, ErrorType::ParseError);
    }
  }

  // expr ::= primary binoprhs
  std::unique_ptr<ast::Expression> Parser::parseExpr() {
    auto leftExpr = parsePrimary();
    if (!leftExpr) return nullptr;

    return parseBinOpRight(1, std::move(leftExpr));
  }

  // binopright ::= ('op' primary) *
  std::unique_ptr<ast::Expression>
  Parser::parseBinOpRight(int exprPrec, std::unique_ptr<ast::Expression> leftExpr) {
    auto startPos = leftExpr->loc;
    while (true) {
      /* find precedence of operator (if not operator, implicitly 0) */
      int tokPrec = opPrecedence[tokenizer.curToken.token];
      if (tokPrec < exprPrec) return leftExpr;
      auto op = tokenizer.curToken.token;
      auto opPos = tokenizer.curTokenLoc;
      tokenizer.nextToken();

      auto rightExpr = parsePrimary();
      if (!rightExpr) return nullptr;

      int nextPrec = opPrecedence[tokenizer.curToken.token];
      /* if current op is less tightly bound than next, than rightExpr becomes next operators leftExpr */
      if (tokPrec < nextPrec) {
        rightExpr = parseBinOpRight(tokPrec + 1, std::move(rightExpr));
        if (!rightExpr) return nullptr;
      }
      /* otherwise, leftExpr + rightExpr become next leftExpr */
      /* handle assignment expressions */
      if (op == T_ASSIGN) {
        leftExpr = std::make_unique<ast::Assignment>(startPos, std::move(leftExpr),
                                                     std::move(rightExpr));
      } else {
        ast::ExpressionList args;
        args.push_back(std::move(leftExpr));
        args.push_back(std::move(rightExpr));
        leftExpr = std::make_unique<ast::FunctionCall>(startPos,
                                                       std::make_unique<ast::OperatorSymbol>(opPos,
                                                                                             op),
                                                       std::move(args));
      }
    }
  }

  // typeExpr ::= 'i8' | 'u8' | ... | 'string'
  // typeExpr ::= 'mut' typeExpr
  std::unique_ptr<ast::Type> Parser::parseType() {
    if (tokenizer.curToken.token == T_MUT) {
      tokenizer.nextToken();
      return std::make_unique<ast::MutType>(parseType());
    }
    if (tokenizer.curToken.token != T_IDENT)
      return errorMan.logError("Expected a type expression", tokenizer.curTokenLoc,
                               ErrorType::ParseError);
    auto type = tokenizer.curToken.ident;
    auto loc = tokenizer.curTokenLoc;
    tokenizer.nextToken();
    if (type == "i8") return std::make_unique<ast::IntType>(8, false);
    if (type == "i16") return std::make_unique<ast::IntType>(16, false);
    if (type == "i32") return std::make_unique<ast::IntType>(32, false);
    if (type == "i64") return std::make_unique<ast::IntType>(64, false);
    if (type == "u8") return std::make_unique<ast::IntType>(8, true);
    if (type == "u16") return std::make_unique<ast::IntType>(16, true);
    if (type == "u32") return std::make_unique<ast::IntType>(32, true);
    if (type == "u64") return std::make_unique<ast::IntType>(64, true);
    if (type == "bool") return std::make_unique<ast::BoolType>();
    if (type == "f32") return std::make_unique<ast::FloatType>(32);
    if (type == "f64") return std::make_unique<ast::FloatType>(64);

    return errorMan.logError("Invalid type expression", loc, ErrorType::ParseError);
  }

  // functionproto ::= ident '(' (arg typeExpr ',')* arg typeExpr ')' typeExpr
  std::unique_ptr<ast::FunctionPrototype> Parser::parseFunctionProto() {
    if (tokenizer.curToken.token != T_IDENT)
      return errorMan.logError("Expected function name", tokenizer.curTokenLoc,
                               ErrorType::ParseError);
    std::string name = tokenizer.curToken.ident;

    // left paren
    tokenizer.nextToken();
    if (tokenizer.curToken.token != T_LPAREN)
      return errorMan.logError("Expected '('", tokenizer.curTokenLoc, ErrorType::ParseError);
    // args
    std::vector<std::string> argNames;
    std::vector<std::unique_ptr<ast::Type>> argTypes;

    do {
      tokenizer.nextToken();
      if (tokenizer.curToken.token != T_IDENT)
        return errorMan.logError("Expected argument name", tokenizer.curTokenLoc,
                                 ErrorType::ParseError);
      argNames.push_back(tokenizer.curToken.ident);
      tokenizer.nextToken();
      auto type = parseType();
      if (!type) return nullptr;
      argTypes.push_back(std::move(type));
    } while (tokenizer.curToken.token == T_COMMA);
    if (tokenizer.curToken.token != T_RPAREN)
      return errorMan.logError("Expected ')' or ',' in argument list", tokenizer.curTokenLoc,
                               ErrorType::ParseError);
    tokenizer.nextToken();
    auto retType = parseType();

    return std::make_unique<ast::FunctionPrototype>(
      std::make_unique<ast::FunctionType>(std::move(argTypes), std::move(retType)),
      std::move(argNames),
      name);
  }

  std::unique_ptr<ast::Statement> Parser::parseFunctionDecl() {
    auto pos = tokenizer.curTokenLoc;
    // consume 'fn'
    tokenizer.nextToken();
    // get function prototype
    auto proto = parseFunctionProto();
    if (!proto) return nullptr;
    // parse body
    if (tokenizer.curToken.token != T_LBRK)
      return errorMan.logError("Expected '{'", tokenizer.curTokenLoc, ErrorType::ParseError);
    tokenizer.nextToken();
    ast::StatementList body;
    while (tokenizer.curToken.token != T_RBRK) {
      auto stat = parseStatement();
      if (!stat)
        return errorMan.logError("expected '}' to end function body", tokenizer.curTokenLoc,
                                 ErrorType::ParseError);
      body.push_back(std::move(stat));
    }
    tokenizer.nextToken();

    return std::make_unique<ast::FunctionDecl>(pos, std::move(proto), std::move(body));
  }

  // module ::= 'module' identifier (':' identifier)* '{' statement* '}'
  std::unique_ptr<ast::ModuleDecl> Parser::parseModuleDecl() {
    std::vector<std::string> names;
    ast::StatementList body;

    auto pos = tokenizer.curTokenLoc;
    do {
      // on first iteration, consume 'scope'
      tokenizer.nextToken();
      if (tokenizer.curToken.token != T_IDENT)
        return errorMan.logError("Expected scope name (identifier expected)", tokenizer.curTokenLoc,
                                 ErrorType::ParseError);
      names.push_back(tokenizer.curToken.ident);
      tokenizer.nextToken();
    } while (tokenizer.curToken.token == T_COLON);

    if (tokenizer.curToken.token != T_LBRK)
      return errorMan.logError("Expected '{' to begin scope body", tokenizer.curTokenLoc,
                               ErrorType::ParseError);
    tokenizer.nextToken();

    while (tokenizer.curToken.token != T_RBRK) {
      auto stat = parseStatement();
      body.push_back(std::move(stat));
    }
    tokenizer.nextToken();

    return std::make_unique<ast::ModuleDecl>(pos, std::move(names), std::move(body));
  }

  // vardecl ::= identifier := expr
  std::unique_ptr<ast::Statement> Parser::parseVarDecl() {
    auto pos = tokenizer.curTokenLoc;
    auto name = tokenizer.curToken.ident;
    tokenizer.nextToken();
    if (tokenizer.curToken.token != T_VARDECL)
      return errorMan.logError("Expected := in variable declaration", tokenizer.curTokenLoc,
                               ErrorType::ParseError);
    tokenizer.nextToken();
    auto initialVal = parseExpr();

    return std::make_unique<ast::VarDecl>(pos, name, std::move(initialVal));
  }

  bool Parser::expectEndStatement() {
    if (tokenizer.curToken.token == T_SEMICOLON) {
      tokenizer.nextToken();
      return true;
    }
    if (tokenizer.curToken.token == T_RBRK) {
      return true;
    }
    errorMan.logError("expected newline or ';' to mark end of statement", tokenizer.curTokenLoc,
                      ErrorType::ParseError);
    return false;
  }

  std::unique_ptr<ast::Statement> Parser::parseStatement() {
    switch (tokenizer.curToken.token) {
      case T_FN: {
        auto res = parseFunctionDecl();
        expectEndStatement();
        return res;
      }
      case T_MODULE: {
        auto res = parseModuleDecl();
        expectEndStatement();
        return res;
      }
      case T_IDENT: {
        /* identifier followed by := is a variable declaration */
        auto nextTok = tokenizer.peekNextToken();
        if (nextTok.token != T_VARDECL) {
          auto res = parseExpr();
          expectEndStatement();
          return res;
        }
        auto res = parseVarDecl();
        expectEndStatement();
        return res;
      }
      case T_SEMICOLON:
        tokenizer.nextToken();
        return parseStatement();
      default: {
        auto res = parseExpr();
        expectEndStatement();
        return res;
      }
    }
  }

}// namespace ovid