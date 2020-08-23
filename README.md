# Ovid Language
![CI](https://github.com/edwardwawrzynek/ovid/workflows/CI/badge.svg)

A toy programming language.

See [`lang.md`](https://github.com/edwardwawrzynek/ovid/blob/master/lang.md) for a description of the language.

The `examples` directory contains a few example ovid programs.

## Building

Building Ovid requires:
- [CMake](https://cmake.org) >= 3.15.3
- A C++-17 compliant compiler
- [LLVM](https://llvm.org/) >= 10.0.0
    - Only the LLVM Core libraries are required
    
Ovid's runtime requires:
- [The Boehm GC](https://github.com/ivmai/bdwgc)

To build and install the main executable (`ovidc`):
```
mkdir build
cd build
cmake ..
cmake --build .
sudo cmake --install .
```

To build and run ovid's test framework:
```
mkdir build
cd build
cmake -DENABLE_TESTING=ON ..
cmake --build .
sudo cmake --install .
./runUnitTests
OVIDC_TESTSUITE_PATH=`pwd`/../tests ./runTests
```

## Usage
```ovidc --help```

#### Examples
* Compiling and running `test.ovd` (`test.ovd` must contain a `pub fn main() -> i32`):

```
ovidc --main -o test.o test.ovd
cc -o test test.o -lm -lgc -lovidruntime
./test
```

* Compiling `lib.ovd` as a library, and dumping debug info:
```
ovidc --no-main -o test.o --dump-ast --dump-ir --dump-escape --dump-llvm test.ovd 
```

## Source Organization
* `src` - Ovid compiler source code
    * `test_framework` - The compiler's custom test framework
    * `tokenizer.cpp, parser.cpp` - Tokenizer and parser, which convert ovid files into abstract syntax trees
    * `ast.hpp, ast_visitor.hpp` - Abstract syntax tree (ast) definitions and utilities
    * `symbols.hpp` - Scope tables and utilities
    * `resolve_pass.cpp` - Identifier + Type Alias resolver -- operates on the ast, and manages the module system's rules
    * `type_check.cpp` - Checks types and converts ast to ir
    * `ir.hpp, ir_visitor.hpp` - Intermediate representation (ir) definitions and utilities. A linear instruction based representation
    * `escape_analysis.cpp` - Escape Analyzer, which determines which allocations are stack and which are heap
    * `llvm_codegen.cpp, name_mangle.cpp` - The llvm backend. Converts ir to llvm output.
    * `error.cpp` - Error printing utilities
    * `*_printer.hpp` - Debugging utilities
* `runtime` - Ovid runtime library (not much currently)
* `tests` - The compiler's testsuite. The tests are all ovid files, but use comment annotations to drive the test harness