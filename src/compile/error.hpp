#ifndef H_ERROR_INCL
#define H_ERROR_INCL

#include <cstdio>
#include <iostream>
#include <memory>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

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
  uint64_t col, row;         // start of error
  uint64_t end_col, end_row; // end of error (inclusive)
  std::istream *file; /* file may be null (if input isn't from a file, or is non
                         seekable (like stdin) */

  SourceLocation(const std::string &filename, uint64_t row, uint64_t col,
                 uint64_t end_row, uint64_t end_col, std::istream *file)
      : filename(filename), col(col), row(row), end_col(end_col),
        end_row(end_row), file(file){};

  // construct a source location beginning here, lasting until before endLoc
  SourceLocation until(const SourceLocation &endLoc);
  // construct a source location beginning here, lasting through endLoc
  SourceLocation through(const SourceLocation &endLoc);

  static SourceLocation nullLocation();
};
/**
 * Types of errors and warnings
 * These will be grouped into error levels that can be enabled and disabled
 * Fatal errors can be lumped into broad categories
 * Non fatal errors should be in more specific categories
 */
enum class ErrorType {
  NONE,
  Note,
  ParseError,
  NestedFunctionError,
  DuplicateVarDeclare,
  VarDeclareShadowed,
  UndeclaredIdentifier,
  MutOnRootOfType,
  PublicSymInPrivateMod,
  PublicSymInFunction,
  TypeDeclInFunction,
  DuplicateTypeDecl,
  TypeDeclShadowed,
  UndeclaredType,
  UseOfPrivateIdentifier,
  UseOfPrivateType,
  ModDeclInFunction,
  TypeError,
  NarrowingConversion,
  InternalError
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
  virtual bool criticalErrorOccurred() = 0;

  virtual bool anyErrorOccurred() = 0;

  virtual ~ErrorManager() = 0;
};

ErrorPrintLevel errorTypeToPrintLevel(ErrorType type);

class PrintingErrorManager : public ErrorManager {
private:
  bool didError;
  bool didCriticalError;

public:
  PrintingErrorManager() : didError(false), didCriticalError(false){};

  std::nullptr_t logError(const std::string &msg,
                          const SourceLocation &location,
                          ErrorType type) override;
  std::nullptr_t logError(const std::string &msg,
                          const SourceLocation &location, ErrorType type,
                          bool emitNewline) override;

  bool criticalErrorOccurred() override;
  bool anyErrorOccurred() override;

  ~PrintingErrorManager() override = default;
};

struct TestErrorRecord {
  ErrorType type;
  std::string message;
  SourceLocation loc;

  TestErrorRecord(ErrorType type, const std::string &message,
                  const SourceLocation &loc)
      : type(type), message(message), loc(loc){};
};

class TestErrorManager : public ErrorManager {
private:
  std::vector<TestErrorRecord> errors;

public:
  std::nullptr_t logError(const std::string &msg,
                          const SourceLocation &location,
                          ErrorType type) override;
  std::nullptr_t logError(const std::string &msg,
                          const SourceLocation &location, ErrorType type,
                          bool emitNewline) override;
  bool criticalErrorOccurred() override;
  bool anyErrorOccurred() override;

  bool errorOccurred(ErrorType type);

  std::vector<TestErrorRecord> getErrors();

  // remove ANSI CSI escape codes from messages
  std::string clearEscapeCodes(const std::string &msg);

  ~TestErrorManager() override = default;

  TestErrorManager() : errors(){};
};

// convert a set of scopes and a name to a printable string
std::string scopesAndNameToString(const std::vector<std::string> &scopes,
                                  const std::string &name);

std::string scopesAndNameToString(const std::vector<std::string> &scopes,
                                  const std::string &name, bool is_global);
} // namespace ovid

#endif