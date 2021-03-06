#include "tester.hpp"
#include "ast.hpp"
#include "ast_printer.hpp"
#include "error.hpp"
#include "escape_analysis.hpp"
#include "generics_pass.hpp"
#include "ir_printer.hpp"
#include "llvm_codegen.hpp"
#include "parser.hpp"
#include "resolve_pass.hpp"
#include "tokenizer.hpp"
#include "type_check.hpp"
#include <algorithm>
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
      {":UseOfPrivateType", ErrorType::UseOfPrivateType},
      {":TypeError", ErrorType::TypeError},
      {":NarrowingConversion", ErrorType::NarrowingConversion},
      {":InternalError", ErrorType::InternalError},
      {":NameConvention", ErrorType::NameConvention}};
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
      {ErrorType::UseOfPrivateType, ":UseOfPrivateType"},
      {ErrorType::TypeError, ":TypeError"},
      {ErrorType::NarrowingConversion, ":NarrowingConversion"},
      {ErrorType::InternalError, ":InternalError"},
      {ErrorType::NameConvention, ":NameConvention"}};

  return types[type];
}

void TesterInstance::doError(const std::string &message) {
  std::cout << "Ovid compiler test framework:\n";
  std::cout << "A pre-compile error occurred in file " << filename << " before "
            << line << ":" << pos_in_line << ":\n\n";
  std::cout << message << "\n\n";
  std::cout << "Hint: this is probably due to a test program with malformed "
               "test annotation comments.\n";
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
    if (arg == "parse") {
      modes.insert(TestMode::Parse);
    } else if (arg == "type_check") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::TypeCheck);
    } else if (arg == "compile") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::TypeCheck);
      modes.insert(TestMode::Compile);
    } else if (arg == "run") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::TypeCheck);
      modes.insert(TestMode::Compile);
      modes.insert(TestMode::Run);
    } else if (arg == "run_check_output") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::TypeCheck);
      modes.insert(TestMode::Compile);
      modes.insert(TestMode::Run);
      modes.insert(TestMode::RunCheckOutput);
    } else if (arg == "check_ast") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::CheckAST);
    } else if (arg == "check_ir") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::TypeCheck);
      modes.insert(TestMode::CheckIR);
    } else if (arg == "check_escape") {
      modes.insert(TestMode::Parse);
      modes.insert(TestMode::TypeCheck);
      modes.insert(TestMode::CheckEscape);
    } else {
      doError("invalid test mode (expected parse, compile, run, "
              "run_check_output, "
              "check_ast, check_ir, and/or check_escape)");
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

    if (desc.size() >= 2 && desc[0] == '_' && desc[1] == '_' &&
        desc != "__error:") {
      doError("invalid comment annotation type (expected __error: __)");
      return;
    } else if (desc != "__error:") {
      continue;
    }
    std::string token;
    while (!(token = readToken()).empty()) {
      if (token[0] == ':') {
        expectedErrors.emplace_back(TestErrorRecord(
            errorStringSpecifierToErrorType(token), "",
            SourceLocation(&filename, line, 0, line, 0, &file)));
      } else {
        expectedErrors.emplace_back(TestErrorRecord(
            ErrorType::NONE, token,
            SourceLocation(&filename, line, 0, line, 0, &file)));
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

int TesterInstance::runParse(ErrorManager &errorMan, ast::StatementList &astRes,
                             ScopesRoot &scopes_root, ActiveScopes &scopes) {
  // Parse
  auto lexer = Tokenizer(&filename, &file, errorMan);
  auto parser = Parser(lexer, errorMan, scopes, scopes_root, packageName);
  astRes = parser.parseProgram();
  parser.removePushedPackageScope();

  // ResolvePass
  auto resolvePass = ast::ResolvePass(scopes, errorMan, packageName);
  resolvePass.visitNodes(astRes, ast::ResolvePassState());
  resolvePass.removePushedPackageScope();

  if (errorMan.criticalErrorOccurred())
    return 1;

  return 0;
}

int TesterInstance::runCheckAST(ErrorManager &errorMan,
                                const ast::StatementList &ast) {
  // print ast to string
  std::ostringstream ast_out;
  auto printer = ast::ASTPrinter(ast_out);
  printer.visitNodes(ast, ast::ASTPrinterState());
  // read in expected ast
  auto ast_filename = filename + ".expect.ast";
  auto ast_file = std::ifstream(ast_filename);
  if (!ast_file.is_open()) {
    doError(string_format("failed to open file %s, expected by mode check_ast",
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
    return 1;
  }

  return 0;
}

int TesterInstance::runCheckIR(ErrorManager &errorMan,
                               const ir::InstructionList &ir) {
  // print ir to string
  std::ostringstream ir_out;
  auto printer = ir::IRPrinter(ir_out);
  printer.visitInstructions(ir, ast::ASTPrinterState());
  // read in expected ast
  auto ir_filename = filename + ".expect.ir";
  auto ir_file = std::ifstream(ir_filename);
  if (!ir_file.is_open()) {
    doError(string_format("failed to open file %s, expected by mode check_ir",
                          ir_filename.c_str()));
  }
  std::string expected_ir(std::istreambuf_iterator<char>(ir_file), {});

  // compare parsed ast to expected
  if (ir_out.str() != expected_ir) {
    std::cout << "check_ir " << filename
              << ": generated ir doesn't match ir in file " << ir_filename
              << "\n";
    std::cout << "------------ expected ir ------------\n"
              << expected_ir << "\n";
    std::cout << "------------  generated ir  ------------\n"
              << ir_out.str() << "\n";
    return 1;
  }

  return 0;
}

int TesterInstance::runCheckEscape(ErrorManager &errorMan,
                                   const ir::InstructionList &ir) {
  // run escape analysis and print escaping flows to string
  std::ostringstream escape_out;
  // only check escapes
  ir::runEscapeAnalysis(ir, false, true, false, escape_out);
  // read in expected escape info
  auto escape_filename = filename + ".expect.escape";
  auto escape_file = std::ifstream(escape_filename);
  if (!escape_file.is_open()) {
    doError(
        string_format("failed to open file %s, expected by mode check_escape",
                      escape_filename.c_str()));
    return 1;
  }
  std::string expected_escape(std::istreambuf_iterator<char>(escape_file), {});

  // compare parsed to expected
  if (escape_out.str() != expected_escape) {
    std::cout << "check_escape " << filename
              << ": escapes don't match expected escapes in file "
              << escape_filename << "\n";
    std::cout << "------------ expected escapes ------------\n"
              << expected_escape << "\n";
    std::cout << "------------  generated escapes  ------------\n"
              << escape_out.str() << "\n";
    return 1;
  }

  return 0;
}

int TesterInstance::runLLVMCodegen(ErrorManager &errorMan,
                                   ir::LLVMCodegenPass &codegen_pass,
                                   ScopesRoot &root_scopes,
                                   const std::vector<std::string> &package) {
  /* if run mode is included, require main function */
  bool run_mode = modes.count(TestMode::Run) > 0;

  std::vector<llvm::PassBuilder::OptimizationLevel> optsToRun = {
      llvm::PassBuilder::OptimizationLevel::Oz,
      llvm::PassBuilder::OptimizationLevel::Os,
      llvm::PassBuilder::OptimizationLevel::O0,
      llvm::PassBuilder::OptimizationLevel::O1,
      llvm::PassBuilder::OptimizationLevel::O2,
      llvm::PassBuilder::OptimizationLevel::O3};

  std::string main_name = "main";

  for (auto optLevel : optsToRun) {
    codegen_pass.optAndEmit(
        optLevel, "ovidc_test_out_tmp.o", ir::CodegenOutputType::OBJ, run_mode,
        root_scopes.names->getScopeTable(package), &main_name);

    if (run_mode) {
      const char *link_path = std::getenv("OVIDC_TESTSUITE_LINK_PATH");
      /* link object code */
      int ld_res =
          system(string_format("cc -o ovidc_test_out_tmp ovidc_test_out_tmp.o "
                               "-lgc -lm -lovidruntime -L%s",
                               link_path != nullptr ? link_path : ".")
                     .c_str());
      if (ld_res != 0) {
        std::cout << "\x1b[1;31mlinking llvm output failed\x1b[m\n";
        return 1;
      }
      /* execute code */
      int run_res = system("./ovidc_test_out_tmp > ovidc_test_output_tmp");
      if (run_res != 0) {
        std::cout << "\x1b[1;31mrunning output returned non 0 result code "
                  << run_res << "\x1b[m\n";
        return 1;
      }

      if (modes.count(TestMode::RunCheckOutput) > 0) {
        // open expected output file
        auto expected_out_filename = filename + ".expect.out";
        auto expected_out_file = std::ifstream(expected_out_filename);
        if (!expected_out_file.is_open()) {
          doError(string_format(
              "failed to open file %s, expected by mode run_check_output",
              expected_out_filename.c_str()));
          return 1;
        }
        std::string expected_out(
            std::istreambuf_iterator<char>(expected_out_file), {});
        // read generated output
        auto generated_out_file = std::ifstream("ovidc_test_output_tmp");
        if (!generated_out_file.is_open()) {
          doError("failed to open file ovidc_test_output_tmp");
          return 1;
        }
        std::string generated_out(
            std::istreambuf_iterator<char>(generated_out_file), {});

        if (expected_out != generated_out) {
          std::cout << "run_check_output " << filename
                    << ": output don't match expected output in file "
                    << expected_out_filename << "\n";
          std::cout << "------------ expected output ------------\n"
                    << expected_out << "\n";
          std::cout << "------------  real output  ------------\n"
                    << generated_out << "\n";
          return 1;
        }
      }
    }
  }

  return 0;
}

int TesterInstance::run() {
  readHeader();

  int failed = 0;
  rewind();

  /* setup compilation */
  auto errorMan = ovid::TestErrorManager();
  ir::reset_id();
  ast::reset_id();

  ScopesRoot root_scopes;
  auto scopes = ActiveScopes(packageName, -1, root_scopes.names.get(),
                             root_scopes.types.get());

  if (modes.count(TestMode::Parse) > 0) {
    ast::StatementList ast;
    auto parseDidError = runParse(errorMan, ast, root_scopes, scopes);

    if (modes.count(TestMode::CheckAST) > 0) {
      if (parseDidError || runCheckAST(errorMan, ast))
        failed = 1;
    }

    if (modes.count(TestMode::TypeCheck) > 0) {
      if (parseDidError) {
        std::cout << "\x1b[1;31mparse pass raised errors, cannot run "
                     "type_check\x1b[m\n";
        failed = 1;
      } else {
        // generate ir
        auto ir = ast::TypeCheck::produceIR(errorMan, packageName, root_scopes,
                                            scopes, ast);

        // run check_ir
        if (modes.count(TestMode::CheckIR) > 0) {
          if (errorMan.criticalErrorOccurred() || runCheckIR(errorMan, ir))
            failed = 1;
        }

        if (modes.count(TestMode::Compile) > 0 ||
            modes.count(TestMode::CheckEscape) > 0) {
          // run generics pass
          ir = ir::GenericsPass::produceIR(scopes, root_scopes, errorMan, ir);
          // run escape analysis
          if (modes.count(TestMode::CheckEscape) > 0) {
            if (runCheckEscape(errorMan, ir))
              failed = 1;
          } else {
            ir::runEscapeAnalysis(ir, false, false, false, std::cout);
          }

          if (modes.count(TestMode::Compile) > 0) {
            /* run codegen pass */
            auto codegen = ir::LLVMCodegenPass(filename, errorMan);
            codegen.visitInstructions(ir, ir::LLVMCodegenPassState());

            if (runLLVMCodegen(errorMan, codegen, root_scopes, packageName))
              failed = 1;
          }
        }
      }
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
} // namespace ovid::tester

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "usage: " << argv[0] << " test.ovd\n";
    return 1;
  }

  auto tester = ovid::tester::TesterInstance(std::string(argv[1]));
  return tester.run();
}
