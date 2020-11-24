#include "parser.hpp"

#include <map>

#include "error.hpp"

namespace ovid {

bool Parser::isDoneParsing() const { return tokenizer.curToken.token == T_EOF; }

ast::StatementList Parser::parseProgram() {
  // lookup active scope by package name
  ScopeTable<Symbol> *packageNameScope;
  ScopeTable<TypeAlias> *packageTypeScope;

  if (package.empty()) {
    packageNameScope = scopes.names.getRootScope();
    packageTypeScope = scopes.types.getRootScope();
  } else {
    packageNameScope = scopes.names.getRootScope()->getScopeTable(package);
    packageTypeScope = scopes.types.getRootScope()->getScopeTable(package);
    assert(packageNameScope && packageTypeScope);
  }

  auto startPos = tokenizer.curTokenLoc;

  ParserState state(true, packageNameScope, packageTypeScope, package, false);

  // check for file beginning with 'module' or 'pub module'
  if (tokenizer.curToken.token == T_MODULE ||
      (tokenizer.curToken.token == T_PUB &&
       tokenizer.peekNextToken().token == T_MODULE)) {
    // handle 'pub'
    bool is_public = false;
    if (tokenizer.curToken.token == T_PUB) {
      is_public = true;
      tokenizer.nextToken();
    }
    // consume 'module'
    tokenizer.nextToken();

    // read in name
    std::vector<SourceLocation> nameLocs;
    auto names = readScopedName(state, &nameLocs);

    if (names.empty())
      return parseProgramWithoutRootModuleDecl(state);

    for (size_t i = 0; i < names.size(); i++) {
      expectSnakeCase(names[i], nameLocs[i]);
    }

    auto endModHeaderPos = tokenizer.curTokenLoc;

    // if '{' is present, parse as a normal module statement
    if (tokenizer.curToken.token == T_LBRK) {
      // parse module statement, then parse the rest of program and combine
      std::unique_ptr<ast::Statement> firstStmnt = parseModuleDeclBody(
          state, is_public, names, startPos.through(endModHeaderPos));
      expectEndStatement();
      auto restOfProgram = parseProgramWithoutRootModuleDecl(state);
      restOfProgram.insert(restOfProgram.cbegin(), std::move(firstStmnt));

      return restOfProgram;
    } else {
      expectEndStatement();
      // set up environment for the whole program in the module
      auto newState = newStateForModule(state, names, is_public);
      scopes.pushComponentScopesByName(newState.current_module);

      ast::StatementList nodes = parseProgramWithoutRootModuleDecl(newState);

      scopes.popComponentScopesByName(newState.current_module);
      // create the module ast node with the program as body
      ast::StatementList res;
      res.emplace_back(std::make_unique<ast::ModuleDecl>(
          startPos.through(endModHeaderPos), names, std::move(nodes)));

      return res;
    }
  } else {
    return parseProgramWithoutRootModuleDecl(state);
  }
}

ast::StatementList
Parser::parseProgramWithoutRootModuleDecl(const ParserState &state) {
  ast::StatementList nodes;

  if (tokenizer.curToken.token == T_EOF)
    return nodes;

  do {
    auto ast = parseStatement(state);
    if (ast)
      nodes.push_back(std::move(ast));
  } while (!isDoneParsing());

  return nodes;
}

// intexpr ::= intliteral
std::unique_ptr<ast::IntLiteral>
Parser::parseIntLiteral(const ParserState &state) {
  auto res = std::make_unique<ast::IntLiteral>(tokenizer.curTokenLoc,
                                               tokenizer.curToken.int_literal);
  tokenizer.nextToken();
  return res;
}

// boolexpr ::= 'true'|'false'
std::unique_ptr<ast::BoolLiteral>
Parser::parseBoolLiteral(const ParserState &state) {
  auto res = std::make_unique<ast::BoolLiteral>(
      tokenizer.curTokenLoc, tokenizer.curToken.bool_literal);

  tokenizer.nextToken();
  return res;
}

// charexpr ::= 'c'
std::unique_ptr<ast::CharLiteral>
Parser::parseCharLiteral(const ParserState &state) {
  auto res = std::make_unique<ast::CharLiteral>(
      tokenizer.curTokenLoc, tokenizer.curToken.char_literal);

  tokenizer.nextToken();
  return res;
}

std::unique_ptr<ast::FloatLiteral>
Parser::parseFloatLiteral(const ParserState &state) {
  auto res = std::make_unique<ast::FloatLiteral>(
      tokenizer.curTokenLoc, tokenizer.curToken.float_literal);

  tokenizer.nextToken();
  return res;
}

// structliteral ::= '{' (ident ':' expr ',')* ident ':' expr ','? '}'
std::unique_ptr<ast::Expression>
Parser::parseStructLiteral(const ParserState &state,
                           const SourceLocation &loc_start,
                           std::shared_ptr<ast::UnresolvedType> type_expr) {
  // consume '}'
  tokenizer.nextToken();

  std::vector<std::string> field_names;
  std::vector<std::unique_ptr<ast::Expression>> field_exprs;

  while (tokenizer.curToken.token != T_RBRK) {
    if (tokenizer.curToken.token != T_IDENT) {
      return errorMan.logError("expected struct field name",
                               tokenizer.curTokenLoc, ErrorType::ParseError);
    }
    auto name = tokenizer.curToken.ident;
    tokenizer.nextToken();
    if (tokenizer.curToken.token != T_COLON) {
      return errorMan.logError("expected ':' after field name",
                               tokenizer.curTokenLoc, ErrorType::ParseError);
    }
    tokenizer.nextToken();
    auto expr = parseExpr(state);
    if (expr == nullptr)
      return nullptr;

    while (tokenizer.curToken.token == T_SEMICOLON)
      tokenizer.nextToken();

    field_names.push_back(name);
    field_exprs.push_back(std::move(expr));

    if (tokenizer.curToken.token != T_COMMA) {
      if (tokenizer.curToken.token != T_RBRK) {
        return errorMan.logError("expected ',' or '}' after expression",
                                 tokenizer.curTokenLoc, ErrorType::ParseError);
      }
    } else {
      tokenizer.nextToken();
    }

    while (tokenizer.curToken.token == T_SEMICOLON)
      tokenizer.nextToken();
  }

  auto pos = loc_start.through(tokenizer.curTokenLoc);
  tokenizer.nextToken();

  return std::make_unique<ast::StructExpr>(pos, std::move(type_expr),
                                           std::move(field_names),
                                           std::move(field_exprs));
}

// identexpr ::= identifier (':' identifier)* (':' typeparams)?
std::unique_ptr<ast::Expression>
Parser::parseIdentifier(const ParserState &state) {
  std::vector<std::string> varScopes;
  bool is_root_scoped = false;
  ast::TypeList ident_type_params;

  auto pos = tokenizer.curTokenLoc;

  if (tokenizer.curToken.token == T_DOUBLE_COLON) {
    tokenizer.nextToken();
    is_root_scoped = true;
  }

  SourceLocation end = pos;
  while (true) {
    // generic :<> operator
    if (tokenizer.curToken.token == T_LESS) {
      ident_type_params = parseTypeParameterList(state);
      end = tokenizer.curTokenLoc;
      break;
    } else if (tokenizer.curToken.token == T_IDENT) {
      varScopes.push_back(tokenizer.curToken.ident);
      end = tokenizer.curTokenLoc;
      tokenizer.nextToken();
    } else {
      return errorMan.logError(
          "expected identifier or '<' after scope operator :",
          tokenizer.curTokenLoc, ErrorType::ParseError);
    }

    if (tokenizer.curToken.token != T_COLON)
      break;
    tokenizer.nextToken();
  }

  std::string ident = varScopes[varScopes.size() - 1];
  varScopes.pop_back();

  // special handling for structure literal
  // either an opening bracket or angled bracket (generic) indicate a struct
  if ((tokenizer.curToken.token == T_LBRK ||
       tokenizer.curToken.token == T_LESS) &&
      state.struct_literals_allowed) {
    ast::TypeList type_params;
    if (tokenizer.curToken.token == T_LESS) {
      type_params = parseTypeParameterList(state);
    }
    // construct struct type expression
    auto type_expr = std::make_shared<ast::UnresolvedType>(
        pos.through(end), varScopes, ident, is_root_scoped,
        std::move(type_params));
    return parseStructLiteral(state, pos.through(end), std::move(type_expr));
  }

  return std::make_unique<ast::Identifier>(pos.through(end), ident,
                                           std::move(varScopes), is_root_scoped,
                                           std::move(ident_type_params));
}

// parenexpr ::= '(' ')'
// parenexpr ::= '(' expr ')'
// tupleexpr ::= '(' (expr ',')+ expr ')'
std::unique_ptr<ast::Expression>
Parser::parseParenExpr(const ParserState &state) {
  auto pos = tokenizer.curTokenLoc;

  tokenizer.nextToken(); // skip '('
  // if next token is ')', then the expression is the unit tuple
  if (tokenizer.curToken.token == T_RPAREN) {
    auto endPos = tokenizer.curTokenLoc;
    tokenizer.nextToken();
    return std::make_unique<ast::Tuple>(pos.through(endPos),
                                        ast::ExpressionList());
  }

  // not unit tuple, so consume first expression
  auto expr0 = parseExpr(state.allowStructLiterals());
  if (!expr0)
    return nullptr;
  /* check for right paren or comma, indicating tuple */
  if (tokenizer.curToken.token == T_RPAREN) {
    tokenizer.nextToken();
    return expr0;
  }
  if (tokenizer.curToken.token != T_COMMA)
    return errorMan.logError("Expected ')' or ',' ", tokenizer.curTokenLoc,
                             ErrorType::ParseError);
  ast::ExpressionList tupleExprs;
  tupleExprs.push_back(std::move(expr0));
  // add each element to tuple
  do {
    tokenizer.nextToken();
    auto expr = parseExpr(state);
    if (!expr)
      return nullptr;
    tupleExprs.push_back(std::move(expr));
  } while (tokenizer.curToken.token == T_COMMA);
  if (tokenizer.curToken.token == T_RPAREN) {
    auto endPos = tokenizer.curTokenLoc;
    tokenizer.nextToken();
    return std::make_unique<ast::Tuple>(pos.through(endPos),
                                        std::move(tupleExprs));
  }
  return errorMan.logError("Expected ')' or ','", tokenizer.curTokenLoc,
                           ErrorType::ParseError);
}

/* note: precedences must start at > 1, as 1 is passed from primary and 0 is for
 * invalid ops
 *
 * higher number = lower precedence */
static std::map<TokenType, int> infixOpPrecedence = {
    {T_ASSIGN, 20},  {T_ADD_ASSIGN, 20}, {T_SUB_ASSIGN, 20},
    {T_OR, 30},      {T_AND, 40},        {T_BIN_OR, 50},
    {T_BIN_XOR, 50}, {T_ADDR, 60},       {T_EQ, 70},
    {T_NEQ, 70},     {T_GREATER, 80},    {T_GREATER_EQUAL, 80},
    {T_LESS, 80},    {T_LESS_EQUAL, 80}, {T_LSHF, 90},
    {T_RSHF, 90},    {T_ADD, 100},       {T_SUB, 100},
    {T_STAR, 110},   {T_DIV, 110},
};

static std::map<TokenType, bool> isRightAssoc = {
    {T_ASSIGN, true}, {T_ADD_ASSIGN, true}, {T_SUB_ASSIGN, true},
    {T_DOT, false},   {T_ADD, false},       {T_SUB, false},
    {T_STAR, false},  {T_DIV, false}};

static std::map<TokenType, ast::OperatorType> infixOperatorMap = {
    {T_ADD, ast::OperatorType::ADD},
    {T_ADD_ASSIGN, ast::OperatorType::ADD_ASSIGN},
    {T_SUB, ast::OperatorType::SUB},
    {T_SUB_ASSIGN, ast::OperatorType::SUB_ASSIGN},
    {T_STAR, ast::OperatorType::MUL},
    {T_DIV, ast::OperatorType::DIV},
    {T_EQ, ast::OperatorType::EQUAL},
    {T_NEQ, ast::OperatorType::NEQUAL},
    {T_LESS, ast::OperatorType::LESS},
    {T_LESS_EQUAL, ast::OperatorType::LESS_EQUAL},
    {T_GREATER, ast::OperatorType::GREATER},
    {T_GREATER_EQUAL, ast::OperatorType::GREATER_EQUAL},
    {T_BIN_OR, ast::OperatorType::BIN_OR},
    {T_BIN_XOR, ast::OperatorType::BIN_XOR},
    {T_ADDR, ast::OperatorType::BIN_AND},
    {T_LSHF, ast::OperatorType::LEFT_SHIFT},
    {T_RSHF, ast::OperatorType::RIGHT_SHIFT},
    {T_AND, ast::OperatorType::LOG_AND},
    {T_OR, ast::OperatorType::LOG_OR}};

static std::map<TokenType, ast::OperatorType> prefixOperatorMap = {
    {T_SUB, ast::OperatorType::NEGATIVE},
    {T_STAR, ast::OperatorType::DEREF},
    {T_ADDR, ast::OperatorType::ADDR},
    {T_NOT, ast::OperatorType::LOG_NOT},
    {T_BIN_NOT, ast::OperatorType::BIN_NOT}};

static std::map<TokenType, ast::OperatorType> postfixOperatorMap = {};

ast::OperatorType Parser::infixTokenToOperatorType(TokenType token) {
  if (infixOperatorMap.count(token) > 0) {
    return infixOperatorMap[token];
  }
  assert(false);
}

ast::OperatorType Parser::prefixTokenToOperatorType(TokenType token) {
  if (prefixOperatorMap.count(token) > 0) {
    return prefixOperatorMap[token];
  }
  assert(false);
}

ast::OperatorType Parser::postfixTokenToOperatorType(TokenType token) {
  if (postfixOperatorMap.count(token) > 0) {
    return postfixOperatorMap[token];
  }
  assert(false);
}

std::unique_ptr<ast::Expression>
Parser::parsePrimary(const ParserState &state) {
  switch (tokenizer.curToken.token) {
  case T_IDENT:
  case T_DOUBLE_COLON:
    return parseIdentifier(state);
  case T_LPAREN:
    return parseParenExpr(state);
  case T_INTLITERAL:
    return parseIntLiteral(state);
  case T_BOOLLITERAL:
    return parseBoolLiteral(state);
  case T_CHARLITERAL:
    return parseCharLiteral(state);
  case T_FLOATLITERAL:
    return parseFloatLiteral(state);
  default:
    auto loc = tokenizer.curTokenLoc;
    // consume token (the parse methods normally do this)
    tokenizer.nextToken();
    if (tokenizer.curToken.token == T_EOF) {
      // TODO: bail out of parsing
      return errorMan.logError("unexpected EOF", loc, ErrorType::ParseError);
    } else
      return errorMan.logError("unexpected token", loc, ErrorType::ParseError);
  }
}

// funccall ::= postfix '(' (expr ',')* expr ')'
//            | postfix '(' ')'
std::unique_ptr<ast::Expression>
Parser::parseFunctionCall(const ParserState &state,
                          std::unique_ptr<ast::Expression> funcExpr) {
  ast::ExpressionList args;
  do {
    // consume '(' on first iteration, comma on others
    tokenizer.nextToken();
    if (tokenizer.curToken.token == T_RPAREN)
      break;

    auto expr = parseExpr(state.allowStructLiterals());
    if (!expr)
      return nullptr;
    args.push_back(std::move(expr));
  } while (tokenizer.curToken.token == T_COMMA);

  if (tokenizer.curToken.token != T_RPAREN)
    return errorMan.logError("Expected ',' or ')' in argument list",
                             tokenizer.curTokenLoc, ErrorType::ParseError);

  auto endPos = tokenizer.curTokenLoc;

  tokenizer.nextToken();
  return std::make_unique<ast::FunctionCall>(
      funcExpr->loc.through(endPos), std::move(funcExpr), std::move(args));
}

// fieldaccess ::= postfix '.' (id|num)
std::unique_ptr<ast::Expression>
Parser::parseFieldAccess(const ParserState &state,
                         std::unique_ptr<ast::Expression> expr) {
  // consume '.'
  tokenizer.nextToken();

  if (tokenizer.curToken.token == T_IDENT) {
    tokenizer.nextToken();
    return std::make_unique<ast::FieldAccess>(
        expr->loc.through(tokenizer.curTokenLoc), std::move(expr),
        tokenizer.curToken.ident);
  } else if (tokenizer.curToken.token == T_INTLITERAL) {
    tokenizer.nextToken();
    return std::make_unique<ast::FieldAccess>(
        expr->loc.through(tokenizer.curTokenLoc), std::move(expr),
        tokenizer.curToken.int_literal);
  } else {
    auto pos = tokenizer.curTokenLoc;
    tokenizer.nextToken();
    return errorMan.logError("expected expression after field access "
                             "operator (.) to be an identifier or number",
                             pos, ErrorType::ParseError);
  }
}

// postfix ::= postfix op
//           | primary
std::unique_ptr<ast::Expression>
Parser::parsePostfixOp(const ParserState &state) {
  auto startPos = tokenizer.curTokenLoc;
  auto expr = parsePrimary(state);

  while (true) {
    auto op = tokenizer.curToken.token;
    // special handling for function call postfix op
    if (op == T_LPAREN) {
      expr = parseFunctionCall(state, std::move(expr));
    }
    // special handling for . operator postfix op
    else if (op == T_DOT) {
      expr = parseFieldAccess(state, std::move(expr));
    }
    // if next token is a postfix op, apply it
    else if (postfixOperatorMap.count(op) > 0) {
      auto opType = postfixTokenToOperatorType(tokenizer.curToken.token);
      auto endPos = tokenizer.curTokenLoc;
      tokenizer.nextToken();
      ast::ExpressionList args;
      args.push_back(std::move(expr));

      expr = std::make_unique<ast::FunctionCall>(
          startPos.through(endPos),
          std::make_unique<ast::OperatorSymbol>(endPos, opType),
          std::move(args));
    } else {
      return expr;
    }
  }
}

// prefix ::= postfix
//          | op prefix
std::unique_ptr<ast::Expression>
Parser::parsePrefixOp(const ParserState &state) {
  auto startPos = tokenizer.curTokenLoc;
  if (prefixOperatorMap.count(tokenizer.curToken.token) > 0) {
    auto op = prefixTokenToOperatorType(tokenizer.curToken.token);
    tokenizer.nextToken();
    auto right = parsePrefixOp(state);

    ast::ExpressionList args;
    args.push_back(std::move(right));

    return std::make_unique<ast::FunctionCall>(
        startPos.until(tokenizer.curTokenLoc),
        std::make_unique<ast::OperatorSymbol>(startPos, op), std::move(args));
  } else {
    return parsePostfixOp(state);
  }
}

// binop ::= prefix (op prefix) *
std::unique_ptr<ast::Expression> Parser::parseBinExpr(const ParserState &state,
                                                      int prevPec) {
  auto startPos = tokenizer.curTokenLoc;
  // get left side of expression
  auto left = parsePrefixOp(state);
  if (left == nullptr)
    return nullptr;

  auto op = tokenizer.curToken.token;
  auto opPos = tokenizer.curTokenLoc;

  // loop while the token is an operator and is higher precedence than previous
  // (if left associative) or equal/greater precedence (if right associative)
  while (infixOpPrecedence.count(op) > 0 &&
         (isRightAssoc[op] ? infixOpPrecedence[op] >= prevPec
                           : infixOpPrecedence[op] > prevPec)) {
    tokenizer.nextToken();

    // parse right side of operator
    auto right = parseBinExpr(state, infixOpPrecedence[op]);
    if (right == nullptr)
      return nullptr;

    // join left and right branches

    // assign becomes a special node in the ast
    if (op == T_ASSIGN) {
      left = std::make_unique<ast::Assignment>(
          startPos.through(tokenizer.curTokenLoc), std::move(left),
          std::move(right));
    } else {
      // join left and right in function call node
      ast::ExpressionList args;
      args.push_back(std::move(left));
      args.push_back(std::move(right));
      left = std::make_unique<ast::FunctionCall>(
          startPos.until(tokenizer.curTokenLoc),
          std::make_unique<ast::OperatorSymbol>(opPos,
                                                infixTokenToOperatorType(op)),
          std::move(args));
    }

    op = tokenizer.curToken.token;
    opPos = tokenizer.curTokenLoc;
  }

  // return current branch
  return left;
}

// expr ::= binop
std::unique_ptr<ast::Expression> Parser::parseExpr(const ParserState &state) {
  return parseBinExpr(state, 1);
}

static std::map<std::string, std::function<std::shared_ptr<ast::Type>(
                                 const SourceLocation &loc)>>
    builtinTypes = {
        {"i8",
         [](auto loc) {
           return std::make_shared<ast::IntType>(loc, 8, false);
         }},
        {"i16",
         [](auto loc) {
           return std::make_shared<ast::IntType>(loc, 16, false);
         }},
        {"i32",
         [](auto loc) {
           return std::make_shared<ast::IntType>(loc, 32, false);
         }},
        {"i64",
         [](auto loc) {
           return std::make_shared<ast::IntType>(loc, 64, false);
         }},
        {"u8",
         [](auto loc) { return std::make_shared<ast::IntType>(loc, 8, true); }},
        {"u16",
         [](auto loc) {
           return std::make_shared<ast::IntType>(loc, 16, true);
         }},
        {"u32",
         [](auto loc) {
           return std::make_shared<ast::IntType>(loc, 32, true);
         }},
        {"u64",
         [](auto loc) {
           return std::make_shared<ast::IntType>(loc, 64, true);
         }},
        {"f32",
         [](auto loc) { return std::make_shared<ast::FloatType>(loc, 32); }},
        {"f64",
         [](auto loc) { return std::make_shared<ast::FloatType>(loc, 64); }},
        {"bool", [](auto loc) { return std::make_shared<ast::BoolType>(loc); }},
        {"void",
         [](auto loc) { return std::make_shared<ast::VoidType>(loc); }}};

// typeExpr ::= 'i8' | 'u8' | ... | 'string'
// typeExpr ::= '*' typeExpr
// typeExpr ::= 'mut' typeExpr (not at root level)
// typeExpr ::= '(' (typeExpr ',')* typeExpr ')'
std::shared_ptr<ast::Type> Parser::parseType(const ParserState &state) {
  return parseType(state, true);
}

std::shared_ptr<ast::Type> Parser::parseType(const ParserState &state,
                                             bool is_root_of_type) {
  auto pos = tokenizer.curTokenLoc;
  // mut type
  if (tokenizer.curToken.token == T_MUT) {
    // a type with root level mutability (eg mut i32) is invalid -- root
    // mutability is about binding, not type a type like (* mut i32) is valid
    if (is_root_of_type) {
      errorMan.logError(
          "a mutability modifier can't be specified as the root of a type",
          tokenizer.curTokenLoc, ErrorType::MutOnRootOfType);
    }

    tokenizer.nextToken();
    return std::make_shared<ast::MutType>(pos, parseType(state, false));
  }
  // pointer type
  if (tokenizer.curToken.token == T_STAR) {
    tokenizer.nextToken();
    return std::make_shared<ast::PointerType>(pos, parseType(state, false));
  }
  // tuple type
  if (tokenizer.curToken.token == T_LPAREN) {
    tokenizer.nextToken();
    ast::TypeList types;

    while (tokenizer.curToken.token != T_RPAREN) {
      types.push_back(parseType(state));
      if (tokenizer.curToken.token != T_COMMA &&
          tokenizer.curToken.token != T_RPAREN) {
        return errorMan.logError("expected ) or , in type expression",
                                 tokenizer.curTokenLoc, ErrorType::ParseError);
      }

      if (tokenizer.curToken.token == T_COMMA)
        tokenizer.nextToken();
    }
    tokenizer.nextToken();

    if (types.size() == 1) {
      return types[0];
    } else {
      return std::make_shared<ast::TupleType>(pos, std::move(types));
    }
  }

  if (tokenizer.curToken.token != T_IDENT &&
      tokenizer.curToken.token != T_DOUBLE_COLON) {
    return errorMan.logError("Expected a type expression",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }

  /* parse scopes and name */
  bool is_root_scoped = false;
  std::vector<std::string> type_scopes;
  std::string name;

  if (tokenizer.curToken.token == T_DOUBLE_COLON) {
    tokenizer.nextToken();
    is_root_scoped = true;
  }

  SourceLocation endPos = tokenizer.curTokenLoc;

  while (true) {
    if (tokenizer.curToken.token != T_IDENT)
      return errorMan.logError("expected identifier in type expression",
                               tokenizer.curTokenLoc, ErrorType::ParseError);

    auto token = tokenizer.curToken.ident;
    endPos = tokenizer.curTokenLoc;
    tokenizer.nextToken();
    if (tokenizer.curToken.token == T_COLON) {
      type_scopes.push_back(token);
      tokenizer.nextToken();
    } else {
      name = token;
      break;
    }
  }

  // check if type is a builtin
  if (type_scopes.empty() && builtinTypes.count(name) > 0) {
    return builtinTypes[tokenizer.curToken.ident](pos.through(endPos));
  }

  // parse type parameters
  ast::TypeList type_params;
  if (tokenizer.curToken.token == T_LESS) {
    type_params = parseTypeParameterList(state);
  }

  return std::make_shared<ast::UnresolvedType>(pos.through(endPos), type_scopes,
                                               name, is_root_scoped,
                                               std::move(type_params));
}

// typealias ::= 'type' ident '=' typeExpr
std::unique_ptr<ast::TypeAliasDecl>
Parser::parseTypeAliasDecl(const ParserState &state, bool is_public) {
  auto pos = tokenizer.curTokenLoc;

  if (!state.is_global_level)
    errorMan.logError("type cannot be declared inside a non global scope", pos,
                      ErrorType::TypeDeclInFunction);

  // consume 'type'
  tokenizer.nextToken();
  // get name
  if (tokenizer.curToken.token != T_IDENT) {
    return errorMan.logError("expected type alias name (expected identifier)",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  auto name = tokenizer.curToken.ident;
  expectUpperCamelCase(name, tokenizer.curTokenLoc);
  auto endNamePos = tokenizer.curTokenLoc;
  tokenizer.nextToken();

  // check for '<' (generic type parameter list)
  ast::FormalTypeParameterList type_parameters;
  if (tokenizer.curToken.token == T_LESS) {
    type_parameters = parseFormalTypeParameterList(state, false);
  }

  if (tokenizer.curToken.token != T_ASSIGN) {
    return errorMan.logError("expected '=' after type name",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  tokenizer.nextToken();
  auto type = parseType(state);
  auto typeConstruct = std::make_shared<ast::GenericTypeConstructor>(
      type->loc, std::move(type_parameters), std::move(type));

  // add symbol with reference to type to type table
  auto sym = std::make_shared<TypeAlias>(pos.through(endNamePos),
                                         std::move(typeConstruct), is_public);
  addTypeAlias(state, name, sym);

  return std::make_unique<ast::TypeAliasDecl>(pos.through(endNamePos), name,
                                              sym);
}

// add a type alias to the type table and check for duplicate definition
void Parser::addTypeAlias(const ParserState &state, const std::string &name,
                          const std::shared_ptr<TypeAlias> &alias) {
  auto existing =
      state.current_type_scope->getDirectScopeTable().findSymbol(name);
  if (existing) {
    auto scopedName = scopesAndNameToString(state.current_module, name,
                                            state.is_global_level);
    errorMan.logError(string_format("redeclaration of type `\x1b[1m%s\x1b[m`",
                                    scopedName.c_str()),
                      alias->decl_loc, ErrorType::DuplicateTypeDecl, false);
    errorMan.logError(
        string_format("previous declaration of type `\x1b[1m%s\x1b[m` here",
                      scopedName.c_str()),
        existing->decl_loc, ErrorType::Note);
  } else {
    state.current_type_scope->addSymbol(name, alias);
  }
}

// namedFunctionType ::= '(' (arg typeExpr ',')* arg typeExpr ')' ('->'
//// typeExpr)?
// argLocs is an empty vector to put the location of each arg declare
// in. If null, ignored
std::shared_ptr<ast::NamedFunctionType>
Parser::parseNamedFunctionType(const ParserState &state,
                               std::vector<SourceLocation> *argLocs) {
  auto startPos = tokenizer.curTokenLoc;

  // left paren
  if (tokenizer.curToken.token != T_LPAREN)
    return errorMan.logError("Expected '('", tokenizer.curTokenLoc,
                             ErrorType::ParseError);
  // args
  std::vector<std::string> argNames;
  ast::TypeList argTypes;

  do {
    tokenizer.nextToken();
    // if no arguments, break
    if (tokenizer.curToken.token == T_RPAREN)
      break;

    // read argument name
    if (tokenizer.curToken.token != T_IDENT)
      return errorMan.logError("Expected argument name", tokenizer.curTokenLoc,
                               ErrorType::ParseError);
    argNames.push_back(tokenizer.curToken.ident);
    if (argLocs)
      argLocs->push_back(tokenizer.curTokenLoc);
    tokenizer.nextToken();
    // read types
    auto type = parseType(state);
    if (!type)
      return nullptr;
    argTypes.push_back(std::move(type));
  } while (tokenizer.curToken.token == T_COMMA);

  if (tokenizer.curToken.token != T_RPAREN)
    return errorMan.logError("Expected ')' or ',' in argument list",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  tokenizer.nextToken();

  std::shared_ptr<ast::Type> retType;

  if (tokenizer.curToken.token != T_RIGHT_ARROW) {
    retType = std::make_shared<ast::VoidType>(tokenizer.curTokenLoc);
  } else {
    tokenizer.nextToken();
    retType = parseType(state);
  }

  auto loc = startPos.through(retType->loc);

  return std::make_shared<ast::NamedFunctionType>(
      loc, std::move(argTypes), std::move(retType), std::move(argNames));
}

// functionproto ::= ident formaltypeparams? namedFunctionType
//
// argLocs is an empty vector to put the location of each arg declare
// in. If null, ignored
std::unique_ptr<ast::FunctionPrototype>
Parser::parseFunctionProto(const ParserState &state,
                           std::vector<SourceLocation> *argLocs) {
  if (tokenizer.curToken.token != T_IDENT)
    return errorMan.logError("Expected function name", tokenizer.curTokenLoc,
                             ErrorType::ParseError);
  std::string name = tokenizer.curToken.ident;
  expectSnakeCase(name, tokenizer.curTokenLoc);
  tokenizer.nextToken();
  // check for generics
  auto generics_loc_start = tokenizer.curTokenLoc;
  ast::FormalTypeParameterList formal_params;
  if (tokenizer.curToken.token == T_LESS) {
    formal_params = parseFormalTypeParameterList(state, true);
  }

  auto type = parseNamedFunctionType(state, argLocs);
  auto type_construct = std::make_shared<ast::GenericTypeConstructor>(
      generics_loc_start.through(type->loc), formal_params, std::move(type));

  return std::make_unique<ast::FunctionPrototype>(std::move(type_construct),
                                                  name);
}

std::unique_ptr<ast::Statement>
Parser::parseFunctionDecl(const ParserState &state, bool is_public) {
  auto pos = tokenizer.curTokenLoc;

  // no nested functions
  bool did_error = false;
  if (!state.is_global_level) {
    errorMan.logError("functions cannot be nested in each other", pos,
                      ErrorType::NestedFunctionError);
    // wait to return so that rest of function is skipped
    did_error = true;
  }

  // consume 'fn'
  tokenizer.nextToken();
  // get function prototype
  std::vector<SourceLocation> argLocs;
  auto proto = parseFunctionProto(state, &argLocs);
  if (!proto)
    return nullptr;

  // parse body
  if (tokenizer.curToken.token != T_LBRK)
    return errorMan.logError("expected '{'", tokenizer.curTokenLoc,
                             ErrorType::ParseError);
  tokenizer.nextToken();

  // construct scope table for function (private, function scope)
  auto symbolTable =
      std::make_unique<ScopeTable<Symbol>>(false, nullptr, "", true);
  auto symbolTablePointer = symbolTable.get();
  ast::ScopedBlock body(std::move(symbolTable));
  // the current type scope is copied, as function's can't contain type alias
  // declarations inside them
  ParserState bodyState(false, symbolTablePointer, state.current_type_scope,
                        state.current_module, state.in_private_mod);

  // add entries in symbol table for arguments
  auto funcType = dynamic_cast<ast::NamedFunctionType *>(
      proto->type->getFormalBoundType().get());
  assert(funcType != nullptr);
  for (size_t i = 0; i < funcType->argTypes.size(); i++) {
    auto &arg = funcType->argNames[i];
    auto &loc = argLocs[i];

    auto sym = std::make_shared<Symbol>(loc, false, false, false, false);
    sym->type = funcType->argTypes[i];
    bodyState.current_scope->addSymbol(arg, sym);
  }

  while (tokenizer.curToken.token != T_RBRK) {
    auto stat = parseStatement(bodyState);
    if (!stat && tokenizer.curToken.token != T_RBRK)
      errorMan.logError("expected '}' to end function body",
                        tokenizer.curTokenLoc, ErrorType::ParseError);
    body.addStatement(std::move(stat));
  }
  tokenizer.nextToken();

  // add entry for function to symbol table
  if (checkRedeclaration(pos, proto->name, state))
    return nullptr;

  auto ast = std::make_unique<ast::FunctionDecl>(pos, proto->type, proto->name,
                                                 std::move(body));

  auto fun_sym = std::make_shared<Symbol>(pos, is_public, true, false,
                                          state.is_global_level);
  fun_sym->type = proto->type;
  ast->resolved_symbol = fun_sym;

  state.current_scope->addSymbol(proto->name, fun_sym);

  if (did_error)
    return nullptr;

  return ast;
}

// scopedname ::= identifier (':' identifier)*
std::vector<std::string>
Parser::readScopedName(const ParserState &state,
                       std::vector<SourceLocation> *locs) {
  std::vector<std::string> names;
  bool first_iter = true;
  do {
    if (!first_iter)
      tokenizer.nextToken();
    if (tokenizer.curToken.token != T_IDENT) {
      errorMan.logError("expected module name (identifier expected)",
                        tokenizer.curTokenLoc, ErrorType::ParseError);
      return std::vector<std::string>();
    }
    names.push_back(tokenizer.curToken.ident);
    if (locs != nullptr)
      locs->push_back(tokenizer.curTokenLoc);
    tokenizer.nextToken();
    first_iter = false;
  } while (tokenizer.curToken.token == T_COLON);

  return names;
}

std::vector<std::string> Parser::readScopedName(const ParserState &state) {
  return readScopedName(state, nullptr);
}

// create the state needed to parse the body of a module
ParserState Parser::newStateForModule(const ParserState &state,
                                      const std::vector<std::string> &names,
                                      bool is_mod_public) {
  ParserState newState(true, state.current_scope, state.current_type_scope,
                       state.current_module, !is_mod_public);

  for (auto &name : names) {
    // if it doesn't exist already, add module table
    auto existingNamesTable = newState.current_scope->getScopeTable(name);
    auto existingTypesTable = newState.current_type_scope->getScopeTable(name);

    // name and type hierarchies should match
    assert((existingNamesTable == nullptr) == (existingTypesTable == nullptr));

    if (existingNamesTable == nullptr) {
      newState.current_scope =
          newState.current_scope->addScopeTable(name, is_mod_public);
    } else {
      newState.current_scope = existingNamesTable;
    }
    if (existingTypesTable == nullptr) {
      newState.current_type_scope =
          newState.current_type_scope->addScopeTable(name, is_mod_public);
    } else {
      newState.current_type_scope = existingTypesTable;
    }

    newState.current_module.push_back(name);
  }

  return newState;
}

// modulebody ::= '{' statement* '}'
std::unique_ptr<ast::ModuleDecl>
Parser::parseModuleDeclBody(const ParserState &state, bool is_public,
                            const std::vector<std::string> &names,
                            SourceLocation pos) {
  ast::StatementList body;

  if (tokenizer.curToken.token != T_LBRK)
    return errorMan.logError("expected '{' to begin module body",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  tokenizer.nextToken();

  auto newState = newStateForModule(state, names, is_public);

  // add this module's scope to active scope stack
  scopes.pushComponentScopesByName(newState.current_module);

  while (tokenizer.curToken.token != T_RBRK) {
    auto stat = parseStatement(newState);
    if (!stat && tokenizer.curToken.token != T_RBRK)
      errorMan.logError("expected '}' to end module declaration",
                        tokenizer.curTokenLoc, ErrorType::ParseError);
    body.push_back(std::move(stat));
  }
  tokenizer.nextToken();

  // remove this module's scope from the active scope stack
  scopes.popComponentScopesByName(newState.current_module);

  return std::make_unique<ast::ModuleDecl>(pos, names, std::move(body));
}

// module ::= 'module' identifier (':' identifier)* '{' statement* '}'
std::unique_ptr<ast::ModuleDecl>
Parser::parseModuleDecl(const ParserState &state, bool is_public) {
  std::vector<std::string> names;
  auto pos = tokenizer.curTokenLoc;

  if (is_public && state.in_private_mod) {
    errorMan.logError("pub module cannot be declared inside a non-pub module",
                      pos, ErrorType::PublicSymInPrivateMod);
  }

  bool do_bail_after_name = false;
  if (!state.is_global_level) {
    errorMan.logError("module cannot be declared inside a non global scope",
                      pos, ErrorType::ModDeclInFunction);
    do_bail_after_name = true;
  }

  // consume 'module'
  tokenizer.nextToken();

  std::vector<SourceLocation> nameLocs;
  names = readScopedName(state, &nameLocs);
  if (names.empty() || do_bail_after_name)
    return nullptr;

  for (size_t i = 0; i < names.size(); i++) {
    expectSnakeCase(names[i], nameLocs[i]);
  }

  return parseModuleDeclBody(state, is_public, names,
                             pos.until(tokenizer.curTokenLoc));
}

// vardecl ::= ('val' | 'mut') identifier (typeExpr)? = expr
std::unique_ptr<ast::Statement> Parser::parseVarDecl(const ParserState &state,
                                                     bool is_public) {
  if (is_public && !state.is_global_level) {
    errorMan.logError(
        "pub variable cannot be declared inside a non global scope",
        tokenizer.curTokenLoc, ErrorType::PublicSymInFunction);
    is_public = false;
  }

  auto pos = tokenizer.curTokenLoc;

  auto is_mut = tokenizer.curToken.token == T_MUT;
  // consume 'mut' or 'val'
  tokenizer.nextToken();

  if (tokenizer.curToken.token != T_IDENT) {
    return errorMan.logError("expected variable name (identifier expected)",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }

  auto name = tokenizer.curToken.ident;
  expectSnakeCase(name, tokenizer.curTokenLoc);

  auto endNamePos = tokenizer.curTokenLoc;

  tokenizer.nextToken();
  // if token is not =, try to parse a type expression
  std::shared_ptr<ast::Type> explicitType = nullptr;
  if (tokenizer.curToken.token != T_ASSIGN) {
    explicitType = parseType(state);
  }

  // global variable declares can't be type inferred (needed for usages before
  // declaration to work)
  if (state.is_global_level && explicitType == nullptr) {
    errorMan.logError(
        "global variable declaration needs explicit type specified", pos,
        ErrorType::ParseError);
  }

  if (tokenizer.curToken.token != T_ASSIGN)
    return errorMan.logError("expected = in variable declaration",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  tokenizer.nextToken();
  auto initialVal = parseExpr(state);

  auto ast = std::make_unique<ast::VarDecl>(pos.through(endNamePos), name,
                                            std::move(initialVal),
                                            std::move(explicitType));

  if (!checkRedeclaration(pos.through(endNamePos), name, state)) {
    // add entry to symbol table

    // if at global level, set the symbol to be valid before it's declaration
    // for resolve pass
    auto sym = std::make_shared<Symbol>(pos.through(endNamePos), is_public,
                                        state.is_global_level, is_mut,
                                        state.is_global_level);
    state.current_scope->addSymbol(name, sym);

    ast->resolved_symbol = sym;
  } else {
    // refer ast resolved symbol to previous declaration
    auto prevSym = state.current_scope->getDirectScopeTable().findSymbol(name);
    assert(prevSym != nullptr);

    ast->resolved_symbol = prevSym;
  }

  return ast;
}

// ifstatement = 'if' expr '{' statement* '}' ('elsif' expr '{' statement* '}')*
// ('else' '{' statement* '}')? NOTE: this method consumes the end of statement
// signifier (semicolon if present)
std::unique_ptr<ast::IfStatement>
Parser::parseIfStatement(const ParserState &state) {
  ast::ExpressionList conds;
  std::vector<ast::ScopedBlock> bodies;

  auto pos = tokenizer.curTokenLoc;

  bool hitIf = false;

  while (tokenizer.curToken.token == T_IF ||
         tokenizer.curToken.token == T_ELSIF ||
         tokenizer.curToken.token == T_ELSE) {
    auto tok = tokenizer.curToken.token;

    // if current token is if and we already saw it, new statement
    if (tok == T_IF && hitIf)
      break;

    if (tok == T_IF)
      hitIf = true;

    std::unique_ptr<ast::Expression> condition;

    tokenizer.nextToken();
    // 'else' has an implicit true condition
    if (tok == T_ELSE) {
      condition =
          std::make_unique<ast::BoolLiteral>(tokenizer.curTokenLoc, true);
    }
    // 'if' and 'elsif' have a specified condition
    else {
      condition = parseExpr(state.disallowStructLiterals());
    }
    conds.push_back(std::move(condition));

    // consume '{'
    if (tokenizer.curToken.token != T_LBRK) {
      return errorMan.logError(
          string_format("expected '{' to begin %s statement body",
                        tok == T_IF ? "if"
                                    : (tok == T_ELSIF ? "elsif" : "else")),
          tokenizer.curTokenLoc, ErrorType::ParseError);
    }
    tokenizer.nextToken();

    // setup body's scope
    auto symbolTable =
        std::make_unique<ScopeTable<Symbol>>(false, nullptr, "", true);
    auto symbolTablePointer = symbolTable.get();
    ast::ScopedBlock body(std::move(symbolTable));

    ParserState bodyState(false, symbolTablePointer, state.current_type_scope,
                          state.current_module, state.in_private_mod);

    while (tokenizer.curToken.token != T_RBRK) {
      auto stat = parseStatement(bodyState);
      if (!stat && tokenizer.curToken.token != T_RBRK)
        return errorMan.logError("expected '}' to end block",
                                 tokenizer.curTokenLoc, ErrorType::ParseError);
      body.statements.push_back(std::move(stat));
    }

    tokenizer.nextToken();

    while (tokenizer.curToken.token == T_SEMICOLON)
      tokenizer.nextToken();

    bodies.push_back(std::move(body));

    if (tok == T_ELSE)
      break;
  }

  return std::make_unique<ast::IfStatement>(pos, std::move(conds),
                                            std::move(bodies));
}

std::unique_ptr<ast::ReturnStatement>
Parser::parseReturnStatement(const ParserState &state) {
  auto pos = tokenizer.curTokenLoc;
  // consume 'return'
  tokenizer.nextToken();
  // if end of statement, nothing is returned
  std::unique_ptr<ast::Expression> retExpr;
  if (isEndStatement()) {
    retExpr = nullptr;
  } else {
    retExpr = parseExpr(state);
  }

  return std::make_unique<ast::ReturnStatement>(pos, std::move(retExpr));
}

std::unique_ptr<ast::NativeFunctionDecl>
Parser::parseNativeStatement(const ParserState &state) {
  auto pos = tokenizer.curTokenLoc;
  // consume 'native'
  tokenizer.nextToken();

  if (tokenizer.curToken.token != T_FN) {
    return errorMan.logError(
        "expected 'native' to be followed by a function prototype",
        tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  // consume 'fn'
  tokenizer.nextToken();
  // consume name
  auto fn_scopes = readScopedName(state);
  auto name = fn_scopes.back();
  fn_scopes.pop_back();

  auto type = parseNamedFunctionType(state, nullptr);
  // setup symbol table entry (native fn's are always public)
  auto fun_sym = std::make_shared<Symbol>(pos, true, true, false, true);
  fun_sym->type = type;

  // create proper scope tables (if they don't exist), and add symbol
  auto root = scopes.names.getRootScope();
  for (auto &scope : fn_scopes) {
    auto subscope = root->getScopeTable(scope);
    if (subscope != nullptr) {
      root = subscope;
    } else {
      root = root->addScopeTable(scope, true);
    }
  }
  root->addSymbol(name, fun_sym);

  return std::make_unique<ast::NativeFunctionDecl>(pos, fun_sym);
}

std::unique_ptr<ast::WhileStatement>
Parser::parseWhileStatement(const ParserState &state) {
  auto startPos = tokenizer.curTokenLoc;
  // consume 'while'
  tokenizer.nextToken();

  auto cond = parseExpr(state.disallowStructLiterals());
  // expect opening brace
  if (tokenizer.curToken.token != T_LBRK) {
    return errorMan.logError("expected '{' to begin while statement body",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  tokenizer.nextToken();

  // parse body
  // setup body's scope
  auto symbolTable =
      std::make_unique<ScopeTable<Symbol>>(false, nullptr, "", true);
  auto symbolTablePointer = symbolTable.get();
  ast::ScopedBlock body(std::move(symbolTable));

  ParserState bodyState(false, symbolTablePointer, state.current_type_scope,
                        state.current_module, state.in_private_mod);

  while (tokenizer.curToken.token != T_RBRK) {
    auto stat = parseStatement(bodyState);
    if (!stat && tokenizer.curToken.token != T_RBRK)
      return errorMan.logError("expected '}' to end block",
                               tokenizer.curTokenLoc, ErrorType::ParseError);
    body.statements.push_back(std::move(stat));
  }
  tokenizer.nextToken();

  return std::make_unique<ast::WhileStatement>(startPos, std::move(cond),
                                               std::move(body));
}

// formaltypeparams ::= '<' (ident ',')* ident? '>'
// formaltypeparamsbound ::= '<' (ident boundExpr '.')* (ident boundExpr)? '>'
ast::FormalTypeParameterList
Parser::parseFormalTypeParameterList(const ParserState &state, bool bounds) {
  ast::FormalTypeParameterList res;
  // consume '<'
  tokenizer.nextToken();
  while (tokenizer.curToken.token != T_GREATER) {
    if (tokenizer.curToken.token != T_IDENT) {
      errorMan.logError(
          "expected type parameter name or '>' to end type parameter list",
          tokenizer.curTokenLoc, ErrorType::ParseError);
      return res;
    }
    // type params should be upper camel case
    expectUpperCamelCase(tokenizer.curToken.ident, tokenizer.curTokenLoc);
    res.push_back(std::make_shared<ast::FormalTypeParameter>(
        tokenizer.curTokenLoc, tokenizer.curToken.ident));
    tokenizer.nextToken();

    // TODO: parse type bound if bounds is true

    if (tokenizer.curToken.token == T_GREATER)
      break;
    if (tokenizer.curToken.token != T_COMMA) {
      errorMan.logError("expected ',' or '>' in type parameter list",
                        tokenizer.curTokenLoc, ErrorType::ParseError);
      return res;
    }
    tokenizer.nextToken();
  }
  // consume '>'
  tokenizer.nextToken();

  return res;
}

// typeparams ::= '<' (type ',')* type? '>'
ast::TypeList Parser::parseTypeParameterList(const ParserState &state) {
  ast::TypeList res;
  // consume '<'
  tokenizer.nextToken();
  while (tokenizer.curToken.token != T_GREATER) {
    auto type = parseType(state);
    if (type == nullptr)
      break;
    res.push_back(std::move(type));

    if (tokenizer.curToken.token == T_GREATER)
      break;
    if (tokenizer.curToken.token != T_COMMA) {
      errorMan.logError("expected ',' or '>' in type parameter list",
                        tokenizer.curTokenLoc, ErrorType::ParseError);
      return res;
    }
    tokenizer.nextToken();
  }
  // consume '>'
  tokenizer.nextToken();

  return res;
}

std::unique_ptr<ast::TypeAliasDecl>
Parser::parseStructStatement(const ParserState &state, bool is_public) {
  auto startPos = tokenizer.curTokenLoc;
  // consume 'struct'
  tokenizer.nextToken();
  auto pos = startPos.through(tokenizer.curTokenLoc);

  if (!state.is_global_level) {
    return errorMan.logError("struct declaration cannot be inside a function",
                             startPos, ErrorType::ParseError);
  }
  // parse name of struct
  if (tokenizer.curToken.token != T_IDENT) {
    return errorMan.logError("expected struct name (identifier)",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  auto name = tokenizer.curToken.ident;
  expectUpperCamelCase(name, tokenizer.curTokenLoc);
  tokenizer.nextToken();
  // check for opening arrow (generics)
  ast::FormalTypeParameterList type_parameters;
  if (tokenizer.curToken.token == T_LESS) {
    type_parameters = parseFormalTypeParameterList(state, false);
  }
  // expect opening brace
  if (tokenizer.curToken.token != T_LBRK) {
    return errorMan.logError("expected '{' to begin struct body",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  tokenizer.nextToken();
  // read in field declarations
  std::vector<std::string> field_names;
  std::vector<std::shared_ptr<ast::Type>> field_types;
  std::vector<bool> field_publics;

  while (tokenizer.curToken.token != T_RBRK) {
    while (tokenizer.curToken.token == T_SEMICOLON)
      tokenizer.nextToken();
    // check for public field
    if (tokenizer.curToken.token == T_PUB) {
      if (!is_public) {
        errorMan.logError(
            "pub field cannot be declared inside a non-pub struct",
            tokenizer.curTokenLoc, ErrorType::TypeError);
      }
      tokenizer.nextToken();
      field_publics.push_back(true);
    } else {
      field_publics.push_back(false);
    }
    // read name
    if (tokenizer.curToken.token != T_IDENT) {
      return errorMan.logError("expected field name (identifier)",
                               tokenizer.curTokenLoc, ErrorType::ParseError);
    }
    field_names.push_back(tokenizer.curToken.ident);
    expectSnakeCase(tokenizer.curToken.ident, tokenizer.curTokenLoc);
    tokenizer.nextToken();
    // parse type
    auto type = parseType(state);
    if (type == nullptr)
      return nullptr;
    field_types.push_back(type);

    expectEndStatement();
    while (tokenizer.curToken.token == T_SEMICOLON)
      tokenizer.nextToken();
  }
  tokenizer.nextToken();

  auto type = std::make_shared<ast::StructType>(pos, std::move(field_types),
                                                std::move(field_names),
                                                std::move(field_publics));
  // needed to set type_alias
  auto typePtr = type.get();

  auto typeConstruct = std::make_shared<ast::GenericTypeConstructor>(
      pos, std::move(type_parameters), std::move(type));
  auto alias =
      std::make_shared<TypeAlias>(pos, std::move(typeConstruct), is_public);
  // StructType needs to know it's alias for name mangling, etc
  typePtr->type_alias = alias.get();

  addTypeAlias(state, name, alias);
  return std::make_unique<ast::TypeAliasDecl>(pos, name, alias);
}

// check if parser is currently at an end of statement
bool Parser::isEndStatement() {
  return tokenizer.curToken.token == T_SEMICOLON ||
         tokenizer.curToken.token == T_RBRK;
}

// error if not end of statement (semicolon, which may have been automatically
// inserted), or }
bool Parser::expectEndStatement() {
  if (tokenizer.curToken.token == T_SEMICOLON) {
    tokenizer.nextToken();
    return true;
  }
  if (tokenizer.curToken.token == T_RBRK) {
    return true;
  }
  errorMan.logError("expected newline or ';' to mark end of statement",
                    tokenizer.curTokenLoc, ErrorType::ParseError);
  return false;
}

std::unique_ptr<ast::Statement>
Parser::parsePossiblePubStatement(const ParserState &state, bool is_public) {
  switch (tokenizer.curToken.token) {
  case T_FN: {
    auto res = parseFunctionDecl(state, is_public);
    expectEndStatement();
    return res;
  }
  case T_MODULE: {
    auto res = parseModuleDecl(state, is_public);
    expectEndStatement();
    return res;
  }
  case T_MUT:
  case T_VAL: {
    auto res = parseVarDecl(state, is_public);
    expectEndStatement();
    return res;
  }
  case T_TYPE: {
    auto res = parseTypeAliasDecl(state, is_public);
    expectEndStatement();
    return res;
  }
  case T_STRUCT: {
    auto res = parseStructStatement(state, is_public);
    expectEndStatement();
    return res;
  }
  case T_PUB: {
    if (is_public)
      return errorMan.logError("expected 'pub' to be followed by variable, "
                               "function, type, or module declaration",
                               tokenizer.curTokenLoc, ErrorType::ParseError);

    tokenizer.nextToken();
    return parsePossiblePubStatement(state, true);
  }
  default: {
    errorMan.logError(
        "expected 'pub' to be followed by variable, function, type, or "
        "module declaration",
        tokenizer.curTokenLoc, ErrorType::ParseError);
    return parseStatement(state);
  }
  }
}

std::unique_ptr<ast::Statement>
Parser::parseStatement(const ParserState &state) {
  switch (tokenizer.curToken.token) {
  case T_FN:
  case T_MODULE:
  case T_MUT:
  case T_VAL:
  case T_TYPE:
  case T_PUB:
  case T_STRUCT:
    return parsePossiblePubStatement(state, false);
  case T_NATIVE: {
    auto res = parseNativeStatement(state);
    expectEndStatement();
    return res;
  }
  case T_SEMICOLON:
    tokenizer.nextToken();
    return parseStatement(state);
  case T_RETURN: {
    auto res = parseReturnStatement(state);
    expectEndStatement();
    return res;
  }
  case T_IF:
    return parseIfStatement(state);
  case T_ELSIF:
    errorMan.logError("expected elsif to be preceded by if statement",
                      tokenizer.curTokenLoc, ErrorType::ParseError);
    return parseIfStatement(state);
  case T_ELSE:
    errorMan.logError("expected else to be preceded by if or elsif statement",
                      tokenizer.curTokenLoc, ErrorType::ParseError);
    return parseIfStatement(state);
  case T_WHILE: {
    auto res = parseWhileStatement(state);
    expectEndStatement();
    return res;
  }
  default: {
    auto res = parseExpr(state);
    expectEndStatement();
    return res;
  }
  }
}

std::string Parser::getFullyScopedName(const std::string &name,
                                       const ParserState &state) {
  std::string scoped_name;
  // if global, emit appropriate scope ids
  if (state.is_global_level) {
    for (auto &scope : state.current_module) {
      scoped_name.append(scope);
      scoped_name.push_back(':');
    }
    scoped_name.append(name);
  } else {
    scoped_name = name;
  }
  return scoped_name;
}

bool Parser::checkRedeclaration(const SourceLocation &pos,
                                const std::string &name,
                                const ParserState &state) {
  // make sure symbol wasn't already in table
  auto existing = state.current_scope->getDirectScopeTable().findSymbol(name);
  if (existing) {
    auto scoped_name = getFullyScopedName(name, state);

    errorMan.logError(string_format("redeclaration of `\x1b[1m%s\x1b[m`",
                                    scoped_name.c_str()),
                      pos, ErrorType::DuplicateVarDeclare, false);
    errorMan.logError(
        string_format("previous declaration of `\x1b[1m%s\x1b[m` here",
                      scoped_name.c_str()),
        existing->decl_loc, ErrorType::Note);
  }

  return existing != nullptr;
}

void Parser::expectSnakeCase(const std::string &name, SourceLocation &pos) {
  /* snake case does not contain upper case characters */
  bool is_snake = true;
  std::string valid;

  size_t i = 0;
  for (auto c : name) {
    if (isupper(c)) {
      is_snake = false;
      if (i > 0)
        valid.push_back('_');
      valid.push_back(tolower(c));
    } else {
      valid.push_back(c);
    }
    i++;
  }

  if (!is_snake) {
    errorMan.logError(string_format("identifier \x1b[1m%s\x1b[m should be a "
                                    "snake case identifier: \x1b[1m%s\x1b[m",
                                    name.c_str(), valid.c_str()),
                      pos, ErrorType::NameConvention);
  }
}

void Parser::expectUpperCamelCase(const std::string &name,
                                  SourceLocation &pos) {
  /* upper camel case must begin with an upper case character, and not contain
   * underscores */
  bool is_camel = true;
  std::string valid;

  size_t i = 0;
  bool upper_next = false;
  for (auto c : name) {
    if (i == 0 && !isupper(c)) {
      is_camel = false;
      valid.push_back(toupper(c));
    } else if (c == '_') {
      is_camel = false;
      upper_next = true;
    } else {
      if (upper_next)
        valid.push_back(toupper(c));
      else
        valid.push_back(c);

      upper_next = false;
    }

    i++;
  }

  if (!is_camel) {
    errorMan.logError(
        string_format("identifier \x1b[1m%s\x1b[m should be an upper camel "
                      "case identifier: \x1b[1m%s\x1b[m",
                      name.c_str(), valid.c_str()),
        pos, ErrorType::NameConvention);
  }
}

Parser::Parser(Tokenizer &tokenizer, ErrorManager &errorMan,
               ActiveScopes &scopes, const std::vector<std::string> &package)
    : tokenizer(tokenizer), errorMan(errorMan), scopes(scopes),
      package(package) {
  // add package as scope table [1] in scope stack
  assert(scopes.names.getNumActiveScopes() == 1);
  assert(scopes.types.getNumActiveScopes() == 1);
  scopes.pushComponentScopesByName(package);
}

void Parser::removePushedPackageScope() {
  // remove the scopes that the constructor pushed
  scopes.popComponentScopesByName(package);
  assert(scopes.names.getNumActiveScopes() == 1);
  assert(scopes.types.getNumActiveScopes() == 1);
}

ParserState ParserState::allowStructLiterals() const {
  return ParserState(is_global_level, current_scope, current_type_scope,
                     current_module, in_private_mod, true);
}

ParserState ParserState::disallowStructLiterals() const {
  return ParserState(is_global_level, current_scope, current_type_scope,
                     current_module, in_private_mod, false);
}

} // namespace ovid