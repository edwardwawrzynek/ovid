#include "error.hpp"
#include <iomanip>
#include <limits>

namespace ovid {
ErrorPrintLevel PrintingErrorManager::errorTypeToPrintLevel(ErrorType type) {
  switch (type) {
  case ErrorType::ParseError:
    return ErrorPrintLevel::Error;
  default:
    return ErrorPrintLevel::Error;
  }
}

std::nullptr_t PrintingErrorManager::logError(const std::string &msg,
                                              SourceLocation location,
                                              ErrorType type) {
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

  std::cout << msg << "\n\x1b[1;34m" << std::setw(5) << location.row
            << " |\x1b[m ";
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
    std::cout << line << "\n\x1b[1;34m      | \x1b[m";
    for (int i = 0; i < location.col - 1; i++) {
      if (isspace(line[i]))
        std::cout << line[i];
      else
        std::cout << ' ';
    }
    if (printType == ErrorPrintLevel::Error)
      std::cout << "\x1b[31;1m^\x1b[m\n";
    else if (printType == ErrorPrintLevel::Warning)
      std::cout << "\x1b[33;1m^\x1b[m\n";
    else if (printType == ErrorPrintLevel::Note)
      std::cout << "\x1b[36;1m^\x1b[m\n";

    location.file->seekg(oldLoc);

  } else {
    std::cout << "[ Can't print source location ]\n";
  }
  return nullptr;
}

bool PrintingErrorManager::errorOccurred() { return didError; }

ErrorManager::~ErrorManager() {}

std::nullptr_t TestErrorManager::logError(const std::string &msg,
                                          SourceLocation location,
                                          ErrorType type) {
  errors.insert(type);
  return nullptr;
}

bool TestErrorManager::errorOccurred() { return !errors.empty(); }

bool TestErrorManager::errorOccurred(ErrorType type) {
  return errors.count(type) > 0;
}
} // namespace ovid