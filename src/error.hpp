#ifndef H_ERROR_INCL
#define H_ERROR_INCL

#include <cstdio>
#include <iostream>
#include <set>
#include <string>

namespace ovid {
/* location in source code */
struct SourceLocation {
  std::string filename;
  int64_t col, row;
  std::istream *file; /* file may be null (if input isn't from a file, or is non
                         seekable (like stdin) */

  SourceLocation(const std::string &filename, int64_t row, int64_t col,
                 std::istream *file)
      : filename(filename), col(col), row(row), file(file){};
};
/**
 * Types of errors and warnings
 * These will be grouped into error levels that can be enabled and disabled
 * Fatal errors can be lumped into broad categories
 * Non fatal errors should be in more specific categories
 */
enum class ErrorType { ParseError, NestedFunctionError };

enum class ErrorPrintLevel { Error, Warning, Note };

class ErrorManager {
public:
  virtual std::nullptr_t logError(const std::string &msg,
                                  SourceLocation location, ErrorType type) = 0;
  virtual bool errorOccurred() = 0;

  virtual ~ErrorManager() = 0;
};

class PrintingErrorManager : public ErrorManager {
private:
  bool didError;

  static ErrorPrintLevel errorTypeToPrintLevel(ErrorType type);

  static std::string errorTypeToCode(ErrorType type);

public:
  PrintingErrorManager() : didError(false){};

  std::nullptr_t logError(const std::string &msg, SourceLocation location,
                          ErrorType type) override;

  bool errorOccurred() override;

  ~PrintingErrorManager() override = default;
};

class TestErrorManager : public ErrorManager {
private:
  std::set<ErrorType> errors;

public:
  std::nullptr_t logError(const std::string &msg, SourceLocation location,
                          ErrorType type) override;
  bool errorOccurred() override;

  bool errorOccurred(ErrorType type);

  ~TestErrorManager() override = default;

  TestErrorManager() : errors(){};
};
} // namespace ovid

#endif