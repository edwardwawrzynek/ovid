#include "tester.hpp"
#include "ast.hpp"
#include "error.hpp"
#include "parser.hpp"
#include "resolve_pass.hpp"
#include "tokenizer.hpp"
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <map>

namespace ovid::tester {
/* convert an error type string (eg :ParseError) to the appropriate ErrorType
 * enum */
ErrorType
TesterInstance::errorStringSpecifierToErrorType(const std::string &str) {
  if (str[0] != ':') {
    doError(string_format("invalid error type %s (hint: expected error type to "
                          "being with ':' - eg. :ParseError)",
                          str.c_str()));
    return ErrorType::NONE;
  }
  std::map<std::string, ErrorType> types = {
      {":ParseError", ErrorType::ParseError},
      {":NestedFunctionError", ErrorType::NestedFunctionError},
      {":DuplicateVarDeclare", ErrorType::DuplicateVarDeclare},
      {":VarDeclareShadowed", ErrorType::VarDeclareShadowed},
      {":UndeclaredIdentifier", ErrorType::UndeclaredIdentifier}};
  if (types.count(str) == 0) {
    doError(string_format("invalid error type %s", str.c_str()));
  }
  return types[str];
}

std::string TesterInstance::errorTypeToString(ErrorType type) {
  std::map<ErrorType, std::string> types = {
      {ErrorType::ParseError, ":ParseError"},
      {ErrorType::NestedFunctionError, ":NestedFunctionError"},
      {ErrorType::DuplicateVarDeclare, ":DuplicateVarDeclare"},
      {ErrorType::VarDeclareShadowed, ":VarDeclareShadowed"},
      {ErrorType::UndeclaredIdentifier, ":UndeclaredIdentifier"}};

  return types[type];
}

void TesterInstance::doError(const std::string &message) {
  std::cout << "Ovid compiler test framework:\n";
  std::cout << "A pre-compile error occurred in file " << filename << " before "
            << line << ":" << pos_in_line << ":\n\n";
  std::cout << message << "\n\n";
  std::cout << "Hint: this is probably due to a test program with malformed "
               "test annotation comments.\n";
  // TODO: only stop this test instance
  exit(1);
}

TesterInstance::TesterInstance(const std::string &filename)
    : filename(filename), file(filename), mode(), line(1), pos_in_line(0),
      expectedErrors(), ignoredErrors(1, ErrorType::Note), pline(1),
      ppos_in_line(0) {}

void TesterInstance::readHeader() {
  // rewind
  rewind();
  if (!readToComment()) {
    doError("expected header comment (//__ovid_compiler_test) to begin file");
    return;
  }

  auto head = readToken();
  if (head != "__ovid_compiler_test") {
    doError("first comment in file doesn't match expected header comment "
            "(//__ovid_compiler_test)");
    return;
  }

  if (!readToComment()) {
    doError("expected mode comment (//__mode: __) after header comment");
    return;
  }

  auto mode_begin = readToken();
  if (mode_begin != "__mode:") {
    doError("comment doesn't match expected mode comment (//__mode: __)");
    return;
  }

  auto mode_str = readToken();
  if (mode_str == "compile") {
    mode = TestMode::Compile;
  } else if (mode_str == "run") {
    mode = TestMode::Run;
  } else if (mode_str == "run_check_output") {
    mode = TestMode::RunCheckOutput;
  } else {
    doError("invalid test mode (expected compile, run, or run_check_output)");
    return;
  }

  if (!readToComment()) {
    doError("expected ignored errors comment (//__ignore_errors: __) after "
            "mode comment");
    return;
  }

  auto errors_begin = readToken();
  if (errors_begin != "__ignore_errors:") {
    doError("comment doesn't match expected ignore errors comment "
            "(//__ignore_errors: __)");
    return;
  }

  std::string token;
  while ((token = readToken()) != "") {
    if (token == ":none")
      ignoredErrors.clear();
    else {
      auto type = errorStringSpecifierToErrorType(token);
      ignoredErrors.push_back(type);
    }
  }

  // read errors
  while (readToComment()) {
    auto desc = readToken();
    if (desc != "__error:") {
      doError("invalid comment annotation type (expected __error: __)");
      return;
    }
    std::string token;
    while ((token = readToken()) != "") {
      if (token[0] == ':') {
        expectedErrors.emplace_back(TestErrorRecord(
            errorStringSpecifierToErrorType(token), "", line, 0));
      } else {
        expectedErrors.emplace_back(
            TestErrorRecord(ErrorType::NONE, token, line, 0));
      }
      foundExpected.push_back(false);
    }
  }
}

int TesterInstance::read() {
  ppos_in_line = pos_in_line;
  pline = line;
  char c = file.get();
  if (file.eof() || file.fail())
    return -1;

  if (c == '\n') {
    line++;
    pos_in_line = 0;
  } else {
    pos_in_line++;
  }

  return c;
}

void TesterInstance::rewind() {
  // rewind
  file.clear();
  file.seekg(0);
  line = 1;
  pos_in_line = 0;
  pline = 1;
  ppos_in_line = 0;
}

std::string TesterInstance::readToken() {
  int c;
  do {
    c = read();
    if (c == EOF) {
      doError("expected token, but found EOF");
      return "";
    }
    if (c == '\n')
      return "";
  } while (isspace(c));

  if (c == '"') {
    std::string res;
    do {
      c = read();
      if (c == EOF) {
        doError("expected \" to end token, found EOF");
        return res;
      }
      if (c != '"')
        res.push_back(c);
    } while (c != '"');

    return res;
  } else {
    std::string res;
    res.push_back(c);

    while (true) {
      c = read();
      if (c == EOF)
        return res;
      if (!isspace(c))
        res.push_back(c);
      else
        break;
    }

    putback(c);

    return res;
  }
}

int TesterInstance::readToComment() {
  int found_slash = 0;
  while (true) {
    int c = read();
    if (c == EOF)
      return 0;
    if (c == '/') {
      if (found_slash) {
        return 1;
      } else {
        found_slash = 1;
      }
    } else {
      found_slash = 0;
    }
  }
}

int TesterInstance::run() {
  int failed = 0;
  rewind();

  /* compile program */
  auto errorMan = ovid::TestErrorManager();
  auto lexer = ovid::Tokenizer(filename, &file, errorMan);
  std::vector<std::string> package;
  auto scopes = ovid::ActiveScopes(package);
  auto parser = ovid::Parser(lexer, errorMan, scopes, package);
  auto ast = parser.parseProgram();
  parser.removePushedPackageScope();
  auto resolvePass = ovid::ast::ResolvePass(scopes, errorMan, package);
  resolvePass.visitNodes(ast, ovid::ast::ResolvePassState());
  resolvePass.removePushedPackageScope();

  // TODO: run + output check

  // error manager to pretty print errors
  auto ppErrorMan = ovid::PrintingErrorManager();

  // go through errors generated by compilation and check
  for (auto &error : errorMan.getErrors()) {
    bool ignored = false;
    for (auto &type : ignoredErrors) {
      if (type == error.type)
        ignored = true;
    }
    if (ignored)
      continue;

    // check if expected
    bool isExpected = false;
    for (int i = 0; i < expectedErrors.size(); i++) {
      auto &expected = expectedErrors[i];
      if (expected.type == ErrorType::NONE) {
        auto clean = errorMan.clearEscapeCodes(error.message);
        if (expected.message == clean && expected.row == error.row) {
          foundExpected[i] = true;
          isExpected = true;
        }
      } else {
        if (expected.type == error.type && expected.row == error.row) {
          foundExpected[i] = true;
          isExpected = true;
        }
      }
    }
    if (isExpected)
      continue;

    std::cout << "compile " << filename << ": an unexpected error occurred:\n";
    // pretty print error
    auto loc = SourceLocation(filename, error.row, error.col, &file);
    ppErrorMan.logError(error.message, loc, error.type);

    failed = 1;
  }

  for (int i = 0; i < expectedErrors.size(); i++) {
    auto &expected = expectedErrors[i];
    if (!foundExpected[i]) {
      std::cout << "an annotated error did not occur in test\nexpected error ";
      if (expected.type != ErrorType::NONE) {
        std::cout << errorTypeToString(expected.type);
      } else {
        std::cout << "\"" << expected.message << "\"";
      }

      std::cout << " to occur on line " << expected.row
                << ", no such error occurred\n\n";

      failed = 1;
    }
  }

  return failed;
}

void TesterInstance::putback(int c) {
  line = pline;
  pos_in_line = ppos_in_line;

  file.putback(c);
}

// run test instances on all files in a directory
int testDirectory(const std::string &dirPath) {
  int failed = 0;

  std::cout << "\x1b[1m[ .... ]\x1b[m Starting the Ovid Compiler Test "
               "Framework on testsuite "
            << dirPath << "\n";

  int numTests = 0;
  for (auto &entry : std::filesystem::directory_iterator(dirPath)) {
    numTests++;
  }

  std::cout << "         " << numTests << " tests to run\n\n";

  for (auto &entry : std::filesystem::directory_iterator(dirPath)) {
    std::cout << "\x1b[1m[ .... ]\x1b[m " << entry.path().string()
              << ": beginning test\n";

    auto tester = TesterInstance(entry.path().string());
    tester.readHeader();
    auto res = tester.run();
    if (res == 0) {
      std::cout << "\x1b[1m[  \x1b[32mOK\x1b[0;1m  ]\x1b[m";
    } else {
      failed++;
      std::cout << "\x1b[1m[ \x1b[31mFAIL\x1b[0;1m ]\x1b[m";
    }

    std::cout << " " << entry.path().string() << ": test "
              << ((res == 0) ? "passed" : "failed") << "\n\n";
  }

  if (failed > 0) {
    std::cout << "\x1b[1m[ \x1b[31mFAIL\x1b[0;1m ]\x1b[m " << failed << "/"
              << numTests << " tests failed\n";
  } else {
    std::cout << "\x1b[1m[  \x1b[32mOK\x1b[0;1m  ]\x1b[m " << numTests << "/"
              << numTests << " tests passed\n";
  }

  return failed > 0 ? 1 : 0;
}

} // namespace ovid::tester

int main() {
  const char *env_path = std::getenv("OVIDC_TESTSUITE_PATH");
  if (env_path == nullptr) {
    return ovid::tester::testDirectory("../tests");
  }
  return ovid::tester::testDirectory(env_path);
}