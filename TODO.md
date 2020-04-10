# TODO
Basic Prototype Goals
- Tokenizer
    - [x] Tokenization
    - [x] Auto semicolon insertion
- Parsing
    - [x] Function Declarations
    - [x] Variable Declarations
    - [x] Infix Expressions
        - [ ] Function Call Expressions
    - [ ] Prefix and Postfix Expressions
    - [x] Location Information for Errors
    - [ ] If statements
    - [ ] Loop statements
- AST
    - [x] Basic Definition
    - [ ] Location Information
- Backend
    - [ ] Basic Expression Emission
    - [ ] Type Checking
        - [ ] Integer types
        - [ ] Boolean type + implicit casts
    - [ ] Symbol tables
        - [ ] Namespacing
    - [ ] If + Loops
    - [ ] Function overloading
    
Second Prototype Goals
- AST
    - [ ] Easy visitors + passes
    - [ ] Passes for composite constructs
- Backend
    - [ ] Multifile + multimodule compiles
        - [ ] Header file format
            - [ ] Symbol table serialization
            - [ ] Source file headers, module header
        - [ ] function metadata in headers
    - [ ] Structures
        - [ ] Methods
    - [ ] Pointers
Third Goals
- [ ] Interfaces and Enums
- [ ] Pattern matching on interfaces + enums
- [ ] Operator overloading
- [ ] Proper garbage collection, c interloop

    