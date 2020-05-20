#include "error.hpp"
#include <iostream>
#include <fstream>

/** Test framework for the ovid compiler:
 * the framework allows the compiler's behavior to be verified on test ovid programs
 *
 * Each program in the test framework must star with the header:
 * //__ovid_compiler_test
 * //__mode: xxx (one of compile, run, run_check_output)
 * //__ignore_errors: x y z ... (a list of error types, such as :all, :ParseError, etc)
 *
 * Modes:
 * compile - just compile the program. If the compiler aborts, test fails. If the compiler raises an error not in the __ignore_errors list, and that error isn't expected (see below), test fails
 * run - same as compile, but makes sure program runs without aborting
 * run_check_output - same as run, but makes sure program output matches output specified in file TODO
 *
 * Expecting errors:
 * tests can be annotated to indicate expected errors, such as:
 *      var := a + b // __error: :UndeclaredIdentifier :UndeclaredIdentifier
 *  or:
 *      var := a + n // __error: "use of undeclared identifier `a`" "use of undeclared identifier `b`:
 */

namespace ovid::tester {
/**
 * Types of test to perform
 * Compile - just compile the program and verify errors
 * Run - Compile + run output and make sure it doesn't abort
 * RunCheckOutput - Run + check output from run matches expected output in file TODO
 */
enum class TestMode {
  Compile, Run, RunCheckOutput
};

class TesterInstance {

  /* convert an error type string (eg :ParseError) to the appropriate ErrorType
  enum */
  ErrorType errorStringSpecifierToErrorType(const std::string &str);

  std::string filename;
  std::ifstream file;

  TestMode mode;
  int line, pos_in_line;

  // read a char, and maintain line, pos_in_line
  int read();

  // reset position in the file
  void rewind();

  // read a token (section of characters, followed by space, or section in quotes)
  std::string readToken();

  // read to a comment (//), and stop after it
  // return 1 if a comment was reached, 0 otherwise
  int readToComment();

public:
  TesterInstance(const std::string & filename);

  void readHeader();
  /* emit a fatal error (not a test failure, something else -- deformed config,
   * etc) */
  void doError(const std::string & message);
};
}