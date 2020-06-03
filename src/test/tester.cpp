#include "tester.hpp"
#include "ast.hpp"
#include "ast_printer.hpp"
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
      {":Note", ErrorType::Note},
      {":ParseError", ErrorType::ParseError},
      {":NestedFunctionError", ErrorType::NestedFunctionError},
      {":DuplicateVarDeclare", ErrorType::DuplicateVarDeclare},
      {":VarDeclareShadowed", ErrorType::VarDeclareShadowed},
      {":UndeclaredIdentifier", ErrorType::UndeclaredIdentifier},
      {":MutOnRootOfType", ErrorType::MutOnRootOfType},
      {":PublicSymInPrivateMod", ErrorType::PublicSymInPrivateMod},
      {":PublicSymInFunction", ErrorType::PublicSymInFunction},
      {":DuplicateTypeDecl", ErrorType::DuplicateTypeDecl},
      {":TypeDeclInFunction", ErrorType::TypeDeclInFunction},
      {":TypeDeclShadowed", ErrorType::TypeDeclShadowed},
      {":UndeclaredType", ErrorType::UndeclaredType},
      {":UseOfPrivateIdentifier", ErrorType::UseOfPrivateIdentifier},
      {":UseOfPrivateType", ErrorType::UseOfPrivateType}};
  if (types.count(str) == 0) {
    doError(string_format("invalid error type %s", str.c_str()));
  }
  return types[str];
}

std::string TesterInstance::errorTypeToString(ErrorType type) {
  std::map<ErrorType, std::string> types = {
      {ErrorType::Note, ":Note"},
      {ErrorType::ParseError, ":ParseError"},
      {ErrorType::NestedFunctionError, ":NestedFunctionError"},
      {ErrorType::DuplicateVarDeclare, ":DuplicateVarDeclare"},
      {ErrorType::VarDeclareShadowed, ":VarDeclareShadowed"},
      {ErrorType::UndeclaredIdentifier, ":UndeclaredIdentifier"},
      {ErrorType::MutOnRootOfType, ":MutOnRootOfType"},
      {ErrorType::PublicSymInPrivateMod, ":PublicSymInPrivateMod"},
      {ErrorType::PublicSymInFunction, ":PublicSymInFunction"},
      {ErrorType::DuplicateTypeDecl, ":DuplicateTypeDecl"},
      {ErrorType::TypeDeclInFunction, ":TypeDeclInFunction"},
      {ErrorType::TypeDeclShadowed, ":TypeDeclShadowed"},
      {ErrorType::UndeclaredType, ":UndeclaredType"},
      {ErrorType::UseOfPrivateIdentifier, ":UseOfPrivateIdentifier"},
      {ErrorType::UseOfPrivateType, ":UseOfPrivateType"}};

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
    : filename(filename), file(filename), modes(), line(1), pos_in_line(0),
      pline(1), ppos_in_line(0), expectedErrors(),
      ignoredErrors(1, ErrorType::Note), packageName(1, "ovidc_test") {}

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

  // read in modes
  std::string arg;
  while (!(arg = readToken()).empty()) {
    if (arg == "compile") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::Compile);
    } else if (arg == "run") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::Compile);
      modes.insert(TestMode::Run);
    } else if (arg == "run_check_output") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::Compile);
      modes.insert(TestMode::Run);
      modes.insert(TestMode::RunCheckOutput);
    } else if (arg == "check_ast") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::CheckAST);
    } else {
      doError("invalid test mode (expected compile, run, run_check_output, or "
              "check_ast)");
      return;
    }
  }

  while (readToComment()) {
    auto type = readToken();
    if (type == "__end_header")
      break;

    std::vector<std::string> args;
    std::string tmp_arg;

    while (!(tmp_arg = readToken()).empty())
      args.push_back(tmp_arg);

    if (type == "__ignore_errors:") {
      for (auto &arg : args) {
        if (arg == ":none")
          ignoredErrors.clear();
        else
          ignoredErrors.push_back(errorStringSpecifierToErrorType(arg));
      }
    } else if (type == "__package_name:") {
      if (args.size() != 1)
        doError("expected only one argument to __package_name:");
      packageName.clear();
      // parse colon separated package name
      size_t pos;
      while ((pos = args[0].find(':')) != std::string::npos) {
        packageName.push_back(args[0].substr(0, pos));
        args[0].erase(0, pos + 1);
      }
      packageName.push_back(args[0]);

    } else {
      doError("invalid header annotation (expected __ignore_errors, "
              "__package_name, or __end_header");
    }
  }

  readInErrors();
}

void TesterInstance::readInErrors() {
  // read errors
  while (readToComment()) {
    auto desc = readToken();
    if (desc != "__error:") {
      doError("invalid comment annotation type (expected __error: __)");
      return;
    }
    std::string token;
    while (!(token = readToken()).empty()) {
      if (token[0] == ':') {
        expectedErrors.emplace_back(
            TestErrorRecord(errorStringSpecifierToErrorType(token), "",
                            SourceLocation(filename, line, 0, line, 0, &file)));
      } else {
        expectedErrors.emplace_back(
            TestErrorRecord(ErrorType::NONE, token,
                            SourceLocation(filename, line, 0, line, 0, &file)));
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
  readHeader();

  int failed = 0;
  rewind();

  /* setup compilation */
  auto errorMan = ovid::TestErrorManager();

  if (modes.count(TestMode::Parse) > 0) {
    // Parse
    auto lexer = ovid::Tokenizer(filename, &file, errorMan);
    auto scopes = ovid::ActiveScopes(packageName);
    auto parser = ovid::Parser(lexer, errorMan, scopes, packageName);
    auto ast = parser.parseProgram();
    parser.removePushedPackageScope();
    // ResolvePass
    auto resolvePass = ovid::ast::ResolvePass(scopes, errorMan, packageName);
    resolvePass.visitNodes(ast, ovid::ast::ResolvePassState());
    resolvePass.removePushedPackageScope();

    if (modes.count(TestMode::CheckAST) > 0) {
      // print ast to string
      std::ostringstream ast_out;
      auto printer = ovid::ast::ASTPrinter(ast_out);
      printer.visitNodes(ast, ovid::ast::ASTPrinterState());
      // read in expected ast
      auto ast_filename = filename + ".expect.ast";
      auto ast_file = std::ifstream(ast_filename);
      if (!ast_file.is_open()) {
        doError(
            string_format("failed to open file %s, expected by mode check_ast",
                          ast_filename.c_str()));
      }
      std::string expected_ast(std::istreambuf_iterator<char>(ast_file), {});

      // compare parsed ast to expected
      if (ast_out.str() != expected_ast) {
        std::cout << "check_ast " << filename
                  << ": parsed ast doesn't match ast in file " << ast_filename
                  << "\n";
        std::cout << "------------ expected ast ------------\n"
                  << expected_ast << "\n";
        std::cout << "------------  parsed ast  ------------\n"
                  << ast_out.str() << "\n";
        failed = 1;
      }
    }

    if (modes.count(TestMode::Compile) > 0) {
      // TODO: further compile + run + run_check_output
    }
  }

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
    for (size_t i = 0; i < expectedErrors.size(); i++) {
      auto &expected = expectedErrors[i];
      if (expected.type == ErrorType::NONE) {
        auto clean = errorMan.clearEscapeCodes(error.message);
        if (expected.message == clean && expected.loc.row == error.loc.row) {
          foundExpected[i] = true;
          isExpected = true;
        }
      } else {
        if (expected.type == error.type && expected.loc.row == error.loc.row) {
          foundExpected[i] = true;
          isExpected = true;
        }
      }
    }
    if (isExpected)
      continue;

    std::cout << "compile " << filename << ": an unexpected error occurred:\n";
    // pretty print error
    ppErrorMan.logError(error.message, error.loc, error.type);

    failed = 1;
  }

  for (size_t i = 0; i < expectedErrors.size(); i++) {
    auto &expected = expectedErrors[i];
    if (!foundExpected[i]) {
      std::cout << "an annotated error did not occur in test\nexpected error ";
      if (expected.type != ErrorType::NONE) {
        std::cout << errorTypeToString(expected.type);
      } else {
        std::cout << "\"" << expected.message << "\"";
      }

      std::cout << " to occur on line " << expected.loc.row
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
    auto path = entry.path().string();
    if (!path.compare(path.size() - 4, 4, ".ovd"))
      numTests++;
  }

  std::cout << "         " << numTests << " tests to run\n\n";

  for (auto &entry : std::filesystem::directory_iterator(dirPath)) {
    auto path = entry.path().string();
    if (path.compare(path.size() - 4, 4, ".ovd"))
      continue;

    std::cout << "\x1b[1m[ .... ]\x1b[m " << entry.path().string()
              << ": beginning test\n";

    auto tester = TesterInstance(path);
    auto res = tester.run();
    if (res == 0) {
      std::cout << "\x1b[1m[  \x1b[32mOK\x1b[0;1m  ]\x1b[m";
    } else {
      failed++;
      std::cout << "\x1b[1m[ \x1b[31mFAIL\x1b[0;1m ]\x1b[m";
    }

    std::cout << " " << path << ": test " << ((res == 0) ? "passed" : "failed")
              << "\n\n";
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