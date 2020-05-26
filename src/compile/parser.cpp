#include "parser.hpp"

#include <map>

#include "error.hpp"

namespace ovid {

/* note: precedences must start at > 1, as 1 is passed from primary and 0 is for
 * invalid ops */
static std::map<TokenType, int> opPrecedence = {
    {T_ASSIGN, 20}, {T_ADD, 30}, {T_SUB, 30}, {T_STAR, 40}, {T_DIV, 40},
};

bool Parser::isDoneParsing() const { return tokenizer.curToken.token == T_EOF; }

ast::StatementList Parser::parseProgram() {
  // lookup active scope by package name
  std::shared_ptr<ScopeTable<Symbol>> packageNameScope;
  std::shared_ptr<ScopeTable<TypeAlias>> packageTypeScope;

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
    // read in name
    auto names = readModuleName(state);
    if (names.empty())
      return parseProgramWithoutRootModuleDecl(state);

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

// identexpr ::= identifier (':' identifier)*
// funccallexpr ::= identifier (':' identifier)* '(' (expr ',') * expr ')'
std::unique_ptr<ast::Expression>
Parser::parseIdentifier(const ParserState &state) {
  std::vector<std::string> varScopes;
  std::string ident;

  bool is_root_scoped = false;

  auto pos = tokenizer.curTokenLoc;

  if (tokenizer.curToken.token == T_DOUBLE_COLON) {
    tokenizer.nextToken();
    is_root_scoped = true;
  }

  SourceLocation end = pos;
  while (true) {
    if (tokenizer.curToken.token != T_IDENT)
      return errorMan.logError("expected identifier after scope operator :",
                               tokenizer.curTokenLoc, ErrorType::ParseError);

    ident = tokenizer.curToken.ident;
    end = tokenizer.curTokenLoc;

    tokenizer.nextToken();
    if (tokenizer.curToken.token == T_COLON) {
      varScopes.push_back(ident);
      tokenizer.nextToken();
    } else
      break;
  }
  if (tokenizer.curToken.token == T_LPAREN) {
    ast::ExpressionList args;
    do {
      tokenizer.nextToken();
      auto expr = parseExpr(state);
      if (!expr)
        return nullptr;
      args.push_back(std::move(expr));
    } while (tokenizer.curToken.token == T_COMMA);
    if (tokenizer.curToken.token != T_RPAREN)
      return errorMan.logError("Expected ',' or ')' in argument list",
                               tokenizer.curTokenLoc, ErrorType::ParseError);
    tokenizer.nextToken();
    return std::make_unique<ast::FunctionCall>(
        pos,
        std::make_unique<ast::Identifier>(pos, ident, std::move(varScopes),
                                          is_root_scoped),
        std::move(args));
  } else {
    return std::make_unique<ast::Identifier>(
        pos.through(end), ident, std::move(varScopes), is_root_scoped);
  }
}

// parenexpr ::= '(' expr ')'
// tupleexpr ::= '(' (expr ',')+ expr ')'
std::unique_ptr<ast::Expression>
Parser::parseParenExpr(const ParserState &state) {
  auto pos = tokenizer.curTokenLoc;

  tokenizer.nextToken(); // skip '('
  auto expr0 = parseExpr(state);
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
    tokenizer.nextToken();
    return std::make_unique<ast::Tuple>(pos, std::move(tupleExprs));
  }
  return errorMan.logError("Expected ')' or ','", tokenizer.curTokenLoc,
                           ErrorType::ParseError);
}

static std::map<TokenType, ast::OperatorType> infixOperatorMap = {
    {T_ADD, ast::OperatorType::ADD},
    {T_SUB, ast::OperatorType::SUB},
    {T_STAR, ast::OperatorType::MUL},
    {T_DIV, ast::OperatorType::DIV}};

static std::map<TokenType, ast::OperatorType> prefixOperatorMap = {
    {T_SUB, ast::OperatorType::NEGATIVE},
    {T_STAR, ast::OperatorType::DEREF},
    {T_ADDR, ast::OperatorType::ADDR},
};

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

// parse a prefixed operation
std::unique_ptr<ast::Expression>
Parser::parsePrefixOp(const ParserState &state) {
  auto startPos = tokenizer.curTokenLoc;
  if (prefixOperatorMap.count(tokenizer.curToken.token) > 0) {
    auto op = prefixTokenToOperatorType(tokenizer.curToken.token);
    tokenizer.nextToken();
    auto right = parsePrefixOp(state);

    std::vector<std::unique_ptr<ast::Expression>> args;
    args.push_back(std::move(right));

    return std::make_unique<ast::FunctionCall>(
        startPos, std::make_unique<ast::OperatorSymbol>(startPos, op),
        std::move(args));
  } else {
    return parsePrimary(state);
  }
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

// expr ::= primary binoprhs
std::unique_ptr<ast::Expression> Parser::parseExpr(const ParserState &state) {
  auto leftExpr = parsePrefixOp(state);
  if (!leftExpr)
    return nullptr;

  return parseBinOpRight(state, 1, std::move(leftExpr));
}

// binopright ::= ('op' primary) *
std::unique_ptr<ast::Expression>
Parser::parseBinOpRight(const ParserState &state, int exprPrec,
                        std::unique_ptr<ast::Expression> leftExpr) {
  auto startPos = leftExpr->loc;
  while (true) {
    /* find precedence of operator (if not operator, implicitly 0) */
    int tokPrec = opPrecedence[tokenizer.curToken.token];
    if (tokPrec < exprPrec)
      return leftExpr;
    auto op = tokenizer.curToken.token;
    auto opPos = tokenizer.curTokenLoc;
    tokenizer.nextToken();

    auto rightExpr = parsePrefixOp(state);
    if (!rightExpr)
      return nullptr;

    int nextPrec = opPrecedence[tokenizer.curToken.token];
    /* if current op is less tightly bound than next, than rightExpr becomes
     * next operators leftExpr */
    if (tokPrec < nextPrec) {
      rightExpr = parseBinOpRight(state, tokPrec + 1, std::move(rightExpr));
      if (!rightExpr)
        return nullptr;
    }
    /* otherwise, leftExpr + rightExpr become next leftExpr */
    /* handle assignment expressions */
    if (op == T_ASSIGN) {
      leftExpr = std::make_unique<ast::Assignment>(
          startPos, std::move(leftExpr), std::move(rightExpr));
    } else {
      ast::ExpressionList args;
      args.push_back(std::move(leftExpr));
      args.push_back(std::move(rightExpr));
      leftExpr = std::make_unique<ast::FunctionCall>(
          startPos,
          std::make_unique<ast::OperatorSymbol>(opPos,
                                                infixTokenToOperatorType(op)),
          std::move(args));
    }
  }
}

static std::map<std::string, std::function<std::unique_ptr<ast::Type>()>>
    builtinTypes = {
        {"i8", [] { return std::make_unique<ast::IntType>(8, false); }},
        {"i16", [] { return std::make_unique<ast::IntType>(16, false); }},
        {"i32", [] { return std::make_unique<ast::IntType>(32, false); }},
        {"i64", [] { return std::make_unique<ast::IntType>(64, false); }},
        {"u8", [] { return std::make_unique<ast::IntType>(8, true); }},
        {"u16", [] { return std::make_unique<ast::IntType>(16, true); }},
        {"u32", [] { return std::make_unique<ast::IntType>(32, true); }},
        {"u64", [] { return std::make_unique<ast::IntType>(64, true); }},
        {"f32", [] { return std::make_unique<ast::FloatType>(32); }},
        {"f64", [] { return std::make_unique<ast::FloatType>(64); }},
        {"bool", [] { return std::make_unique<ast::BoolType>(); }},
        {"void", [] { return std::make_unique<ast::VoidType>(); }}};

// typeExpr ::= 'i8' | 'u8' | ... | 'string'
// typeExpr ::= '*' typeExpr
// typeExpr ::= 'mut' typeExpr (not at root level)
std::unique_ptr<ast::Type> Parser::parseType(const ParserState &state) {
  return parseType(state, true);
}

std::unique_ptr<ast::Type> Parser::parseType(const ParserState &state,
                                             bool is_root_of_type) {
  if (tokenizer.curToken.token == T_MUT) {
    // a type with root level mutability (eg mut i32) is invalid -- root
    // mutability is about binding, not type a type like (* mut i32) is valid
    if (is_root_of_type) {
      errorMan.logError(
          "a mutability modifier can't be specified as the root of a type",
          tokenizer.curTokenLoc, ErrorType::MutOnRootOfType);
    }

    tokenizer.nextToken();
    return std::make_unique<ast::MutType>(parseType(state, false));
  }
  if (tokenizer.curToken.token == T_STAR) {
    tokenizer.nextToken();
    return std::make_unique<ast::PointerType>(parseType(state, false));
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

  while (true) {
    if (tokenizer.curToken.token != T_IDENT)
      return errorMan.logError("expected identifier in type expression",
                               tokenizer.curTokenLoc, ErrorType::ParseError);

    auto token = tokenizer.curToken.ident;
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
    return builtinTypes[tokenizer.curToken.ident]();
  }

  return std::make_unique<ast::UnresolvedType>(type_scopes, name,
                                               is_root_scoped);
}

// typealias ::= 'type' ident '=' typeExpr
std::unique_ptr<ast::TypeAliasDecl>
Parser::parseTypeAliasDecl(const ParserState &state, bool is_public) {
  auto pos = tokenizer.curTokenLoc;

  if (is_public && state.in_private_mod)
    errorMan.logError("pub type cannot be declared inside a non-pub module",
                      pos, ErrorType::PublicSymInPrivateMod);

  if (!state.is_global_level)
    errorMan.logError("type cannot be declared inside a function", pos,
                      ErrorType::TypeDeclInFunction);

  // consume 'type'
  tokenizer.nextToken();
  // get name
  if (tokenizer.curToken.token != T_IDENT) {
    return errorMan.logError("expected type alias name (expected identifier)",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  auto name = tokenizer.curToken.ident;

  auto endNamePos = tokenizer.curTokenLoc;

  tokenizer.nextToken();
  if (tokenizer.curToken.token != T_ASSIGN) {
    return errorMan.logError("expected '=' after type name",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  tokenizer.nextToken();
  auto type = parseType(state);

  // add symbol with reference to type to type table
  auto sym = std::make_shared<TypeAlias>(pos.through(endNamePos),
                                         std::move(type), is_public);
  addTypeAlias(state, name, sym);

  return std::make_unique<ast::TypeAliasDecl>(pos.through(endNamePos), name,
                                              sym);
}

// add a type alias to the type table and check for duplicate definition
void Parser::addTypeAlias(const ParserState &state, std::string name,
                          std::shared_ptr<TypeAlias> alias) {
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
    state.current_type_scope->getDirectScopeTable().addSymbol(name, alias);
  }
}

// functionproto ::= ident '(' (arg typeExpr ',')* arg typeExpr ')' '->'
// typeExpr argLocs is an empty vector to put the location of each arg declare
// in. If null, ignored
std::unique_ptr<ast::FunctionPrototype>
Parser::parseFunctionProto(const ParserState &state,
                           std::vector<SourceLocation> *argLocs) {
  if (tokenizer.curToken.token != T_IDENT)
    return errorMan.logError("Expected function name", tokenizer.curTokenLoc,
                             ErrorType::ParseError);
  std::string name = tokenizer.curToken.ident;

  // left paren
  tokenizer.nextToken();
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
  if (tokenizer.curToken.token != T_RIGHT_ARROW) {
    return errorMan.logError("expected '->' after function argument list",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  }
  tokenizer.nextToken();
  auto retType = parseType(state);

  return std::make_unique<ast::FunctionPrototype>(
      std::make_unique<ast::FunctionType>(std::move(argTypes),
                                          std::move(retType)),
      std::move(argNames), name);
}

std::unique_ptr<ast::Statement>
Parser::parseFunctionDecl(const ParserState &state, bool is_public) {
  auto pos = tokenizer.curTokenLoc;

  if (is_public && state.in_private_mod) {
    errorMan.logError("pub function cannot be declared inside a non-pub module",
                      pos, ErrorType::PublicSymInPrivateMod);
  }

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

  auto symbolTable = std::make_shared<ScopeTable<Symbol>>();
  ast::ScopedBlock body(symbolTable);
  // the current type scope is copied, as function's can't contain type alias
  // declarations inside them
  ParserState bodyState(false, symbolTable, state.current_type_scope,
                        state.current_module, state.in_private_mod);
  // add module scope to active scope stack
  scopes.names.pushScope(state.current_scope);

  // add entries in symbol table for arguments
  for (size_t i = 0; i < proto->argNames.size(); i++) {
    auto &arg = proto->argNames[i];
    auto &loc = argLocs[i];

    auto sym = std::make_shared<Symbol>(loc, false, false, false);
    bodyState.current_scope->getDirectScopeTable().addSymbol(arg, sym);
  }

  while (tokenizer.curToken.token != T_RBRK) {
    auto stat = parseStatement(bodyState);
    if (!stat && tokenizer.curToken.token != T_RBRK)
      errorMan.logError("expected '}' to end function body",
                        tokenizer.curTokenLoc, ErrorType::ParseError);
    body.addStatement(std::move(stat));
  }
  tokenizer.nextToken();
  // remove module scope to active scope stack
  scopes.names.popScope(state.current_scope);

  // add entry for function to symbol table
  if (checkRedeclaration(pos, proto->name, state))
    return nullptr;

  auto type = std::make_unique<ast::NamedFunctionType>(
      std::move(proto->type), std::move(proto->argNames));
  auto ast = std::make_unique<ast::FunctionDecl>(pos, std::move(type),
                                                 proto->name, std::move(body));

  auto fun_sym = std::make_shared<Symbol>(pos, is_public, true, false);
  state.current_scope->getDirectScopeTable().addSymbol(proto->name, fun_sym);

  if (did_error)
    return nullptr;

  return ast;
}

// modulename ::= 'module' identifier (':' identifier)*
std::vector<std::string> Parser::readModuleName(const ParserState &state) {
  std::vector<std::string> names;
  do {
    // on first iteration, consume 'module'
    tokenizer.nextToken();
    if (tokenizer.curToken.token != T_IDENT) {
      errorMan.logError("expected module name (identifier expected)",
                        tokenizer.curTokenLoc, ErrorType::ParseError);
      return std::vector<std::string>();
    }
    names.push_back(tokenizer.curToken.ident);
    tokenizer.nextToken();
  } while (tokenizer.curToken.token == T_COLON);

  return names;
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
      newState.current_scope = newState.current_scope->addScopeTable(name);
    } else {
      newState.current_scope = existingNamesTable;
    }
    if (existingTypesTable == nullptr) {
      newState.current_type_scope =
          newState.current_type_scope->addScopeTable(name);
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

  names = readModuleName(state);
  if (names.empty())
    return nullptr;

  return parseModuleDeclBody(state, is_public, names,
                             pos.until(tokenizer.curTokenLoc));
}

// vardecl ::= ('val' | 'mut') identifier = expr
std::unique_ptr<ast::Statement> Parser::parseVarDecl(const ParserState &state,
                                                     bool is_public) {
  if (is_public && !state.is_global_level) {
    errorMan.logError("pub variable cannot be declared inside a function",
                      tokenizer.curTokenLoc, ErrorType::PublicSymInFunction);
    is_public = false;
  } else if (is_public && state.in_private_mod) {
    errorMan.logError("pub variable cannot be declared inside a non-pub module",
                      tokenizer.curTokenLoc, ErrorType::PublicSymInPrivateMod);
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
  auto endNamePos = tokenizer.curTokenLoc;

  tokenizer.nextToken();
  if (tokenizer.curToken.token != T_ASSIGN)
    return errorMan.logError("expected = in variable declaration",
                             tokenizer.curTokenLoc, ErrorType::ParseError);
  tokenizer.nextToken();
  auto initialVal = parseExpr(state);

  if (!checkRedeclaration(pos.through(endNamePos), name, state)) {
    // add entry to symbol table

    // if at global level, set the symbol to be valid before it's declaration
    // for resolve pass
    auto sym = std::make_shared<Symbol>(pos.through(endNamePos), is_public,
                                        state.is_global_level, is_mut);
    state.current_scope->getDirectScopeTable().addSymbol(name, sym);
  }

  return std::make_unique<ast::VarDecl>(pos.through(endNamePos), name,
                                        std::move(initialVal));
}

// error if end of statement (semicolon, which may have been automatically
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
    return parsePossiblePubStatement(state, false);
  case T_PUB:
    tokenizer.nextToken();
    return parsePossiblePubStatement(state, true);
  case T_SEMICOLON:
    tokenizer.nextToken();
    return parseStatement(state);
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

} // namespace ovid