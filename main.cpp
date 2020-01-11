#include <iostream>
#include <cstdint>
#include "ast.h"

class Test {
public: uint16_t value;
};

int main() {
    auto t = new Test();
    t->value = 2;
    std::cout << "Hello, World!" << std::endl;
    return 0;
}
