#include "error.hpp"
#include <cassert>
#include <iomanip>
#include <limits>
#include <string>

namespace ovid {

ErrorPrintLevel errorTypeToPrintLevel(ErrorType type) {
  switch (type) {
  case ErrorType::Note:
    return ErrorPrintLevel::Note;
  case ErrorType::VarDeclareShadowed:
  case ErrorType::NarrowingConversion:
  case ErrorType::NameConvention:
    return ErrorPrintLevel::Warning;
  default:
    // all fatal errors
    return ErrorPrintLevel::Error;
  }
}

std::nullptr_t PrintingErrorManager::logError(const std::string &msg,
                                              const SourceLocation &location,
                                              ErrorType type,
                                              bool emitNewline) {
  didError = true;
  auto printType = errorTypeToPrintLevel(type);
  if (printType == ErrorPrintLevel::Error)
    didCriticalError = true;

  std::cout << "\x1b[1m" << location.filename << ":" << location.row << ":"
            << location.col << ": ";

  std::string color_code;
  if (printType == ErrorPrintLevel::Error)
    color_code = "\x1b[1;31m";
  else if (printType == ErrorPrintLevel::Warning)
    color_code = "\x1b[1;33m";
  else if (printType == ErrorPrintLevel::Note)
    color_code = "\x1b[1;36m";

  if (printType == ErrorPrintLevel::Error)
    std::cout << color_code << "error: ";
  else if (printType == ErrorPrintLevel::Warning)
    std::cout << color_code << "warning: ";
  else if (printType == ErrorPrintLevel::Note)
    std::cout << color_code << "note: ";

  std::cout << "\x1b[m" << msg << "\n";

  // print source location
  if (location.file) {
    location.file->clear();
    // save old location
    auto oldLoc = location.file->tellg();
    // seek to proper line
    location.file->seekg(std::ios::beg);
    for (size_t i = 0; i < location.row - 1; ++i) {
      location.file->ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }

    // calculate number of lines to print
    auto numLines = (location.end_row - location.row) + 1;
    std::string line;
    for (size_t l = 0; l < numLines; l++) {
      // print line header
      std::cout << "\x1b[m" << std::setw(5) << location.row + l << " | ";
      // read line
      getline(*location.file, line);
      // print line, and highlight characters in the error
      for (size_t p = 0; p < line.size(); p++) {
        if ((p < location.col - 1 && l == 0) ||
            (p > location.end_col - 1 && l == numLines - 1))
          std::cout << "\x1b[m";
        else
          std::cout << color_code;
        std::cout << line[p];
      }
      std::cout << "\n";
    }

    // if the error was only one line, print underline bar
    if (numLines == 1) {
      std::cout << "\x1b[m      | ";
      size_t i;
      for (i = 0; i < location.col - 1; i++) {
        if (isspace(line[i]))
          std::cout << line[i];
        else
          std::cout << " ";
      }
      std::cout << color_code;
      std::cout << "^";
      i++;
      while (i < location.end_col) {
        std::cout << "~";
        i++;
      }
    }
    std::cout << "\n";

    location.file->seekg(oldLoc);

  } else {
    std::cout << "[ Can't print source location ]";
  }

  std::cout << "\x1b[m";

  if (emitNewline)
    std::cout << "\n";

  return nullptr;
}

std::nullptr_t PrintingErrorManager::logError(const std::string &msg,
                                              const SourceLocation &location,
                                              ErrorType type) {
  return logError(msg, location, type, true);
}

bool PrintingErrorManager::anyErrorOccurred() { return didError; }

bool PrintingErrorManager::criticalErrorOccurred() { return didCriticalError; }

ErrorManager::~ErrorManager() {}

std::nullptr_t TestErrorManager::logError(const std::string &msg,
                                          const SourceLocation &location,
                                          ErrorType type) {
  return logError(msg, location, type, true);
}
std::nullptr_t TestErrorManager::logError(const std::string &msg,
                                          const SourceLocation &location,
                                          ErrorType type, bool emitNewline) {
  errors.emplace_back(type, msg, location);
  return nullptr;
}

bool TestErrorManager::anyErrorOccurred() { return !errors.empty(); }

bool TestErrorManager::criticalErrorOccurred() {
  for (auto &error : errors) {
    if (errorTypeToPrintLevel(error.type) == ErrorPrintLevel::Error)
      return true;
  }
  return false;
}

bool TestErrorManager::errorOccurred(ErrorType type) {
  for (auto &e : errors) {
    if (e.type == type)
      return true;
  }

  return false;
}

std::vector<TestErrorRecord> TestErrorManager::getErrors() { return errors; }

std::string TestErrorManager::clearEscapeCodes(const std::string &msg) {
  std::string res;

  bool in_code = false;
  for (auto &c : msg) {
    if (c == '\x1b') {
      in_code = true;
    }

    if (!in_code) {
      res.push_back(c);
    }

    if (in_code && c == 'm') {
      in_code = false;
    }
  }

  return res;
}

// convert a set of scopes and a name to a printable string
std::string scopesAndNameToString(const std::vector<std::string> &scopes,
                                  const std::string &name, bool is_global) {
  if (!is_global)
    return name;

  std::string res;

  for (auto &scope : scopes) {
    res.append(scope);
    res.push_back(':');
  }
  res.append(name);

  return res;
}

std::string scopesAndNameToString(const std::vector<std::string> &scopes,
                                  const std::string &name) {
  return scopesAndNameToString(scopes, name, true);
}

SourceLocation SourceLocation::through(const SourceLocation &endLoc) {
  assert(file == endLoc.file);
  assert(filename == endLoc.filename);

  return SourceLocation(filename, row, col, endLoc.end_row, endLoc.end_col,
                        file);
}

SourceLocation SourceLocation::until(const SourceLocation &endLoc) {
  assert(file == endLoc.file);
  assert(filename == endLoc.filename);

  return SourceLocation(filename, row, col, endLoc.row, endLoc.col, file);
}

SourceLocation SourceLocation::nullLocation() {
  return SourceLocation("", 0, 0, 0, 0, nullptr);
}

} // namespace ovid