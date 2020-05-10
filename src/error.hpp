#ifndef H_ERROR_INCL
#define H_ERROR_INCL

#include <cstdio>
#include <iostream>
#include <memory>
#include <set>
#include <stdexcept>
#include <string>

namespace ovid {

template <typename... Args>
std::string string_format(const std::string &format, Args... args) {
  size_t size = snprintf(nullptr, 0, format.c_str(), args...) + 1;
  if (size <= 0) {
    throw std::runtime_error("Error during formatting.");
  }
  std::unique_ptr<char[]> buf(new char[size]);
  snprintf(buf.get(), size, format.c_str(), args...);
  return std::string(buf.get(), buf.get() + size - 1);
}

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
enum class ErrorType {
  Note,
  ParseError,
  NestedFunctionError,
  DuplicateVarDeclare,
  VarDeclareShadowed
};

enum class ErrorPrintLevel { Error, Warning, Note };

class ErrorManager {
public:
  virtual std::nullptr_t logError(const std::string &msg,
                                  const SourceLocation &location,
                                  ErrorType type) = 0;
  virtual std::nullptr_t logError(const std::string &msg,
                                  const SourceLocation &location,
                                  ErrorType type, bool emitNewline) = 0;
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

  std::nullptr_t logError(const std::string &msg,
                          const SourceLocation &location,
                          ErrorType type) override;
  std::nullptr_t logError(const std::string &msg,
                          const SourceLocation &location, ErrorType type,
                          bool emitNewline) override;

  bool errorOccurred() override;

  ~PrintingErrorManager() override = default;
};

class TestErrorManager : public ErrorManager {
private:
  std::set<ErrorType> errors;

public:
  std::nullptr_t logError(const std::string &msg,
                          const SourceLocation &location,
                          ErrorType type) override;
  std::nullptr_t logError(const std::string &msg,
                          const SourceLocation &location, ErrorType type,
                          bool emitNewline) override;
  bool errorOccurred() override;

  bool errorOccurred(ErrorType type);

  ~TestErrorManager() override = default;

  TestErrorManager() : errors(){};
};
} // namespace ovid

#endif