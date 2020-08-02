# Ovid Compiler Pass Description

The ovid compiler operates in passes, each of which analyses and transforms the input program in some way. The combination of these passes transforms source code into assembly or object code.

## Tokenization (Source Code -> Tokens) (`tokenizer.hpp`, `tokenizer.cpp`)
The tokenizer accepts source code as a stream of characters (typically from a `.ovd` file). It groups characters into tokens, which represent discrete elements in the program (such as a keyword, a opening brace, an integer literal, an identifier, etc).

The tokenizer also inserts semicolons into the token stream. Ovid source code can omit semicolons, but ovid's formal grammar requires them. The tokenizer adds semicolons after newlines where the make sense (it doesn't insert them inside multi-line parenthesized expressions, or after tokens that are non terminals).

Code such as:
```ovid
fn sum(a i32, b i32) -> i32 {
    return a + b
}
```
Is transformed into the token stream:

`T_FN`, `T_IDENT("sum")`, `T_LPAREN`, `T_IDENT("a")`, `T_IDENT("i32")`, `T_COMMA`, `T_IDENT("b")`, `T_IDENT("i32")`, `T_RPAREN`, `T_ARROW`, `T_IDENT("i32")`, `T_LBRK`, `T_RETURN`, `T_IDENT("a")`, `T_PLUS`, `T_IDENT("b")`, `T_SEMICOLON` (added by tokenizer), `T_RBRK`

The tokenizer keeps track of the location of each token in the source program (needed for debug info in the final executable).

The tokenizer also allows for a lookahead of one token.

## Parsing (Tokens -> Abstract Syntax Tree) (`ast.hpp`, `parser.hpp`, `parser.cpp`)
The parser transforms the token stream from the tokenizer into a structured tree. Additionally, it builds the symbol table for module, variable, function, and type alias declarations.

The parser follows ovid's grammar by a recursive descent. It starts with the broadest grammar rule, and descents into more specific rules.
 
 For example, one parse descent might look like: program -> statement -> expression -> binary expression -> unary prefix expression -> unary postfix expression -> identifier.
 
 The binary expression parser uses pratt parsing, and supports a full set of operators with different precedences and associativity.
 
 For the token stream given in the section above, the parser produces this abbstract syntax tree (with source locations in parenthesis):
 ```
(1:1) FunctionDecl: sum
  retType:
    IntType: i32
  argNames: a b
  argTypes:
    IntType: i32
    IntType: i32
  body:
    (2:5) ReturnStatement
        (2:12) FunctionCall
          function:
            (2:14) OperatorSymbol: +
          args:
            (2:12) Identifier: a
            (2:16) Identifier: b

```

## Identifier and Type Resolution Pass (AST -> AST) (`resolve_pass.hpp`, `resolve_pass.cpp`)

The resolution pass visits the abstract syntax tree, and looks up identifier and type aliases in the symbol tables. 

It transforms identifiers and type aliases into symbol table references, and generates warnings for uses of undefined identifiers, as well as enforcing ovid's privacy rules.

The resolution pass also issues warnings for shadowing declarations.

## Type Checking Pass (AST -> Intermediate Representation) (`ir.hpp`, `ir.cpp`, `type_check.hpp`, `type_check.hpp`)

The type checker performs two main functions: making sure that the type system's rules are met, and transforming the program into the intermediate representation (IR) form.

The type checker performs type inference, and checks that the types in assignments, operators, and function calls match what is expect.

The type checker transforms the AST into IR. Opposed to the AST, the IR is a list of instructions, not a tree. All expressions (and sub expressions) are explicitly labelled and their type marked.

The AST in the section above is turned into the following IR (with source locations indicated in parenthesis):
```
(1:1)	%sum = FUNCTIONDECLARE (a i32, b i32) -> i32 {
    (1:1)	BASICBLOCK @0 {
        (1:10)	%a = ALLOCATION i32
        (1:17)	%b = ALLOCATION i32
        (2:14)	%3 = BUILTINOPERATOR (i32, i32) -> i32 +
        (2:12)	%4 = FUNCTIONCALL i32 %3(%a, %b)
        (2:5)	RETURN %4
    }
}
```

## Escape Analysis (IR -> IR) (`ir.hpp`, `escape_analysis.hpp`, `escape_analysis.cpp`)

The escape analyzer performs pointer flow analysis on the program, and determines where allocations (variable creations) need to be on the heap, or can be made on the stack.

Ovid is garbage collected, and doesn't make programs distinguish whether variables are heap or stack allocated. All variables could be heap allocated, but doing so puts unneeded pressure on the garbage collector.
 
 The escape analyzer determines whether pointers to each variable can escape the scope of a function (necessitating heap allocation), or if they remain within the function (allowing stack allocation).
 
 The escape analyzer operates in two stages: first, it generates a list of all potential flows in a function, then unifies them to trace all locations a pointer may escape to.
 
 The current analyzer reorders functions so that it has escape analysis information pre-calculated for all functions called in the body of the current function being analyzed. It can trace arg -> arg and arg -> return flows well. It currently cannot analyzer recursive functions well.
 
 ## LLVM Code Generation (IR -> LLVM) (`llvm_codegen.hpp`, `llvm_codegen.cpp`)
 The LLVM code generator transforms ovid's IR into LLVM IR, which can them be optimized and lowered to assembly and object code.
 
 For the IR given in the sections above, the code generator emits:
 ```llvm
define internal i32 @sum(i32 %a, i32 %b) {
allocs:
  br label %bb0

bb0:
  %0 = add i32 %a, %b
  ret i32 %0
}
``` 

The code generation pass runs llvm's optimizations and emits assembly or object code, optionally supporting LTO and PGO.

## Utilities
### AST and IR Visitor (`ast_visitor.hpp`, `ir_visitor.hpp`)
The ast and ir visitors provide the base classes used by the ast and ir passes to visit and transforms their respective structures.

### AST and IR Printers (`printers/ast_printer.hpp`, `printers/ast_printer.cpp`, `printers/ir_printer.hpp`, `printers/ir_printer.cpp`)
The ast and ir printers allow dumping the ast and ir in a human readable format.

### Name Mangler (`name_mangle.hpp`, `name_mangle.cpp`)
Ovid's module system leads to scoped identifiers (such as `std:io:stdout`), as well as specific forms of generic identifiers (such as `std:collections:list<i32>`). 

The system linker doesn't accept these names, and only allows valid c identifiers. The name mangler transforms ovid identifiers into linker compatable ones.

For example, `std:io:stdout` is transformed to `_I3std2io6stdout`.

### Command Line Driver (`main.cpp`)
The command line driver allows the compiler to be used as a command line program, and for various behaviors to be controlled via flags.

### Test Harness Driver (`test/tester.hpp`, `test/tester.cpp`, `tests/*`)
The test harness runs the ovid compiler on the testsuite in `tests` and allows for testing specific behaviors.

Test programs begin with a header like the following:
```
// __ovid_compiler_test
//__mode: MODES
//__end_header
```

The test harness supports the following modes:

* `parse` - Make sure the program passes the tokenizer and parser without error
* `check_ast` - `parse` + check that the generated AST matches the one in the `.expect.ast` file
* `type_check` - `parse` + make sure the program passes the type_checker
* `check_ir` - `type_check` + make sure the generated ir matches the one in the `.expect.ir` file
* `check_escape` - `type_check` + make sure the generated escape analysis summary matches the one in the `.expect.escape` file
* `compile` - `type_check` + make sure the program passes the llvm code generator, and llvm optimizer
* `run` - `compile` + link and run the resulting program, making sure it runs without error
* `run_check_output` - `run` + make sure the output of the progam matches the expected output in the `.expect.out` file

By default, the test harness expects the compiler to not generate an errors. The test harness can also test that specific errors occur. Errors can be annotated with a `//__error` comment:
```ovid
// __ovid_compiler_test
//__mode: parse
//__end_header
fn test() -> i32 {
    return arg //__error: "use of undeclared identifier `arg`"
}
```