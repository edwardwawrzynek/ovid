#include "tester.hpp"
#include <iostream>
#include <map>
#include <cstring>

namespace ovid::tester {
/* convert an error type string (eg :ParseError) to the appropriate ErrorType enum */
ErrorType TesterInstance::errorStringSpecifierToErrorType(const std::string& str) {
  if(str[0] != ':') {
    doError(string_format("invalid error type %s (hint: expected error type to being with ':' - eg. :ParseError)", str.c_str()));
  }
  std::map<std::string, ErrorType> types = {
      {":ParseError", ErrorType::ParseError},
      {":NestedFunctionError", ErrorType::NestedFunctionError},
      {":DuplicateVarDeclare", ErrorType::DuplicateVarDeclare},
      {":VarDeclareShadowed", ErrorType::VarDeclareShadowed},
      {":UndeclaredIdentifier", ErrorType::UndeclaredIdentifier}
  };
  if(types.count(str) == 0) {
    doError(string_format("invalid error type %s", str.c_str()));
  }
  return types[str];
}

void TesterInstance::doError(const std::string &message) {
  std::cerr << "While running the ovid compile tester, the following pre-compile error occurred:\n";
  std::cerr << message << "\n";
  std::cerr << "The error occurred in file " << filename << " before " << line << ":" << pos_in_line << "\n";
  std::cerr << "Hint: this is probably an error due to malformed test program comments. This is not an error about the program's contents or behavior itself.\n";
  // TODO: only stop this test instance
  exit(1);
}

TesterInstance::TesterInstance(const std::string &filename): filename(filename), file(filename), mode(), line(0), pos_in_line(0) {}

void TesterInstance::readHeader() {
  // rewind
  rewind();
  if(!readToComment()) {
    doError("expected header comment (//__ovid_compiler_test) to begin file");
    return;
  }

  auto head = readToken();
  if(head != "__ovid_compiler_test") {
    doError("first comment in file doesn't match expected header comment (//__ovid_compiler_test)");
    return;
  }

  if(!readToComment()) {
    doError("expected mode comment (//__mode: __) after header comment");
    return;
  }

  auto mode_begin = readToken();
  if(mode_begin != "__mode:") {
    doError("comment doesn't match expected mode comment (//__mode: __)");
    return;
  }

  auto mode_str = readToken();
  if(mode_str == "compile") {
    mode = TestMode::Compile;
  } else if(mode_str == "run") {
    mode = TestMode::Run;
  } else if(mode_str == "run_check_output") {
    mode = TestMode::RunCheckOutput;
  } else {
    doError("invalid test mode (expected compile, run, or run_check_output)");
    return;
  }
}

int TesterInstance::read() {
  char c = file.get();
  if(file.eof() || file.fail()) return -1;

  if(c == '\n') {
    line++;
    pos_in_line = 0;
  } else {
    pos_in_line++;
  }

  return c;
}

void TesterInstance::rewind() {
  // rewind
  file.seekg(0);
  line = 0;
  pos_in_line = 0;
}

std::string TesterInstance::readToken() {
  int c;
  do {
    c = file.get();
    if (c == EOF) {
      doError("expected token, but found EOF");
      return "";
    }
  } while(isspace(c));

  std::string res;
  res.push_back(c);

  while(true) {
    c = file.get();
    if (c == EOF) return res;
    if (!isspace(c)) res.push_back(c);
    else break;
  }

  file.putback(c);

  return res;
}

int TesterInstance::readToComment() {
  int found_slash = 0;
  while(true) {
    int c = file.get();
    if(c == EOF) return 0;
    if(c == '/') {
      if(found_slash) {
        return 1;
      } else {
        found_slash = 1;
      }
    } else {
      found_slash = 0;
    }
  }
}

}

int main() {
  auto tester = ovid::tester::TesterInstance("../test/tests/test0.ovd");

  tester.readHeader();
}