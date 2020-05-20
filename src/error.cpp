#include "error.hpp"
#include <iomanip>
#include <limits>
#include <string>

namespace ovid {

ErrorPrintLevel PrintingErrorManager::errorTypeToPrintLevel(ErrorType type) {
  switch (type) {
  case ErrorType::Note:
    return ErrorPrintLevel::Note;
  case ErrorType::VarDeclareShadowed:
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
  std::cout << "\x1b[1m" << location.filename << ":" << location.row << ":"
            << location.col << ": ";
  if (printType == ErrorPrintLevel::Error)
    std::cout << "\x1b[1;31merror: ";
  else if (printType == ErrorPrintLevel::Warning)
    std::cout << "\x1b[1;33mwarning: ";
  else if (printType == ErrorPrintLevel::Note)
    std::cout << "\x1b[1;36mnote: ";

  std::cout << "\x1b[m";
  std::cout << msg;
  std::cout << "\n" << std::setw(5) << location.row << " |\x1b[m ";
  location.file->clear();
  if (location.file) {
    /* save location */
    auto oldLoc = location.file->tellg();
    /* seek to line row - 1 */
    location.file->seekg(std::ios::beg);
    for (int i = 0; i < location.row - 1; ++i) {
      location.file->ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }

    std::string line;
    getline(*location.file, line);
    std::cout << line << "\n      | \x1b[m";
    for (int i = 0; i < location.col - 1; i++) {
      if (isspace(line[i]))
        std::cout << line[i];
      else
        std::cout << ' ';
    }
    if (printType == ErrorPrintLevel::Error)
      std::cout << "\x1b[31;1m^";
    else if (printType == ErrorPrintLevel::Warning)
      std::cout << "\x1b[33;1m^";
    else if (printType == ErrorPrintLevel::Note)
      std::cout << "\x1b[36;1m^";

    std::cout << "\x1b[m\n";

    location.file->seekg(oldLoc);

  } else {
    std::cout << "[ Can't print source location ]\n";
  }
  if (emitNewline)
    std::cout << "\n";
  return nullptr;
}

std::nullptr_t PrintingErrorManager::logError(const std::string &msg,
                                              const SourceLocation &location,
                                              ErrorType type) {
  return logError(msg, location, type, true);
}

bool PrintingErrorManager::errorOccurred() { return didError; }

std::string PrintingErrorManager::errorTypeToCode(ErrorType type) {
  switch (type) {
  case ErrorType::ParseError:
    return "parse_error";
  case ErrorType::NestedFunctionError:
    return "nested_functions";
  default:
    return "unidentified error";
  }
}

ErrorManager::~ErrorManager() {}

std::nullptr_t TestErrorManager::logError(const std::string &msg,
                                          const SourceLocation &location,
                                          ErrorType type) {
  return logError(msg, location, type, true);
}
std::nullptr_t TestErrorManager::logError(const std::string &msg,
                                          const SourceLocation &location,
                                          ErrorType type, bool emitNewline) {
  if (type != ErrorType::Note) {
    errors.insert(type);
  }
  return nullptr;
}

bool TestErrorManager::errorOccurred() { return !errors.empty(); }

bool TestErrorManager::errorOccurred(ErrorType type) {
  return errors.count(type) > 0;
}

// convert a set of scopes and a name to a printable string
std::string scopesAndNameToString(const std::vector<std::string> & scopes, const std::string & name) {
  std::string res;

  for(auto &scope: scopes) {
    res.append(scope);
    res.push_back(':');
  }
  res.append(name);

  return res;
}

} // namespace ovid