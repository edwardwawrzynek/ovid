#ifndef H_TESTER_INCL
#define H_TESTER_INCL

#include "ast.hpp"
#include "error.hpp"
#include "ir.hpp"
#include "llvm_codegen.hpp"
#include <fstream>
#include <iostream>

/** Test framework for the ovid compiler:
 * the framework allows the compiler's behavior to be verified on test ovid
 * programs
 *
 * Each program in the test framework must star with the header:
 * //__ovid_compiler_test
 * //__mode: xxx (TestMode)
 * //__ignore_errors: x y z ... (a list of error types, such as :ParseError,
 * etc)
 *
 *
 * Expecting errors:
 * tests can be annotated to indicate expected errors, such as:
 *      var := a + b // __error: :UndeclaredIdentifier :UndeclaredIdentifier
 *  or:
 *      var := a + n // __error: "use of undeclared identifier `a`" "use of
 * undeclared identifier `b`:
 */

namespace ovid::tester {
/**
 * Types of test to perform
 * Parse - run the parser and resolve pass
 * TypeCheck - parse + type check output
 * Compile - just compile the program and verify errors
 * Run - Compile + run output and make sure it doesn't abort
 * RunCheckOutput - Run + check output from run matches expected output in file
 * CheckAST - Parse + Resolve + check ast output against expected
 * CheckIR - Compile + check ir output against expected
 * CheckEscape - Compile + check escape output against expected
 */
enum class TestMode {
  Parse,
  TypeCheck,
  Compile,
  Run,
  RunCheckOutput,
  CheckAST,
  CheckIR,
  CheckEscape
};

class TesterInstance {

  /* convert an error type string (eg :ParseError) to the appropriate ErrorType
  enum */
  ErrorType errorStringSpecifierToErrorType(const std::string &str);
  std::string errorTypeToString(ErrorType type);

  std::string filename;
  std::ifstream file;

  std::set<TestMode> modes;
  int line, pos_in_line;
  int pline, ppos_in_line;

  std::vector<TestErrorRecord> expectedErrors;
  std::vector<bool> foundExpected;

  std::vector<ErrorType> ignoredErrors;

  std::vector<std::string> packageName;

  // read a char, and maintain line, pos_in_line
  int read();

  // reset position in the file
  void rewind();

  void putback(int c);

  // read a token (section of characters, followed by space, or section in
  // quotes) won't read across newlines
  std::string readToken();

  // read to a comment (//), and stop after it
  // return 1 if a comment was reached, 0 otherwise
  int readToComment();

  /* emit a fatal error (not a test failure, something else -- deformed config,
   * etc) */
  void doError(const std::string &message);

  // read in header information (check magic header, read mode)
  void readHeader();
  void readInErrors();

  // run methods:
  // run parse and generate an ast
  int runParse(ErrorManager &errorMan, ast::StatementList &astRes,
               ScopesRoot &scopes_root, ActiveScopes &scopes);
  // check ast against expected
  int runCheckAST(ErrorManager &errorMan, const ast::StatementList &ast);
  // check ir against expected
  int runCheckIR(ErrorManager &errorMan, const ir::InstructionList &ir);
  int runCheckEscape(ErrorManager &errorMan, const ir::InstructionList &ir);

  // run code emission and run and run_check_output modes
  int runLLVMCodegen(ErrorManager &errorMan, ir::LLVMCodegenPass &codegen_pass,
                     ScopesRoot &root_scopes,
                     const std::vector<std::string> &package);

public:
  explicit TesterInstance(const std::string &filename);

  // actually test a program (0 for success, non zero on test failure)
  int run();
};

// run test instances on all files in a directory
int testDirectory(const std::string &dirPath);
} // namespace ovid::tester

#endif