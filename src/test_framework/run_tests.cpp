#include <cstdlib>
#include <filesystem>
#include <iostream>
#include <string>

// run test instances on all files in a directory
int testDirectory(const std::string &dirPath, const std::string &argv0) {
  int failed = 0;

  std::cout << "\x1b[1m[ .... ]\x1b[m Starting the Ovid Compiler Test "
               "Framework on testsuite "
            << dirPath << "\n";

  int numTests = 0;
  for (auto &entry : std::filesystem::directory_iterator(dirPath)) {
    auto path = entry.path().string();
    if (!path.compare(path.size() - 4, 4, ".ovd"))
      numTests++;
  }

  std::cout << "         " << numTests << " tests to run\n\n";

  for (auto &entry : std::filesystem::directory_iterator(dirPath)) {
    auto path = entry.path().string();
    if (path.compare(path.size() - 4, 4, ".ovd"))
      continue;

    std::cout << "\x1b[1m[ .... ]\x1b[m " << entry.path().string()
              << ": beginning test\n";

    std::string cmd = argv0;
    cmd += "TestCase ";
    cmd += path;
    auto res = system(cmd.c_str());
    if (res == 0) {
      std::cout << "\x1b[1m[  \x1b[32mOK\x1b[0;1m  ]\x1b[m";
    } else {
      failed++;
      std::cout << "\x1b[1m[ \x1b[31mFAIL\x1b[0;1m ]\x1b[m";
    }

    std::cout << " " << path << ": test " << ((res == 0) ? "passed" : "failed")
              << "\n\n";
  }

  if (failed > 0) {
    std::cout << "\x1b[1m[ \x1b[31mFAIL\x1b[0;1m ]\x1b[m " << failed << "/"
              << numTests << " tests failed\n";
  } else {
    std::cout << "\x1b[1m[  \x1b[32mOK\x1b[0;1m  ]\x1b[m " << numTests << "/"
              << numTests << " tests passed\n";
  }

  return failed > 0 ? 1 : 0;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "usage: " << std::string(argv[0]) << " test_directory/\n";
    return 1;
  }
  return testDirectory(std::string(argv[1]), std::string(argv[0]));
}