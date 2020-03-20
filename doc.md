# Ovid Overview Documentation

Note: Documentation is *very* much a work in progress. It will change with the language

Ovid is a statically typed compiled language.

Statments are ended by newlines. Blocks are deliminated by `{` and `}`. Otherwise, whitespace is ignored.

## Comments
```ovid
// Single line comment
n := 42 // Another comment
/* 
Multi line
comment /* nested comments are supported */
*/
```

Comments that begin with `///` or `/**` are [documentation comments](#doc-comments).

## Basic Types

keyword | type

--- | ---

`i8` | signed 8 bit integer

`i16` | signed 16 bit integer

`i32` | signed 32 bit integer

`i64` | signed 64 bit integer

`u8` | unsigned 8 bit integer

`u16` | unsigned 16 bit integer

`u32` | unsigned 32 bit integer

`u64` | unsigned 64 bit integer

`f32` | 32 bit floating point value

`f64` | 64 bit floating point value

`bool` | boolean type

`string` | string type

### Literals

Integer literals: `-42`, `0xf3ab`, `0b10110`, `'A'`, `'\n'`

Floating point literals: `3.0`, `-0.034`, `43.5e-5`

- Floating point literals must contain a `.` or an exponent, or else they will be interpreted as a integer

Boolean literals: `true`, `false`

String literals: `"Hello World!\n"`

### Type conversion

The expression `T(v)` is the value `v` converted to type `T`. If the conversion is invalid, a compile time error will be thrown.
```ovid
u32(45678)
f32(23)
```

## Variables

Variables are declared and initialized with the `:=` operator. Variables must be initialized on declaration.

The type of a variable is inferred based on its initial value. To force a variable to be of a specific type, use [type conversion](#type-conversion).

Integer literals are inferred to be of type `i64`. Character integer literals are inferred to be of type `i8`. Floating point literals are inferred to be of type `f64`. 

```ovid
num := 12367
msg := "Hello, World!\n"
short_num := i16(-345)
```

## Tuples
A tuple is a ordered set of unnamed values. Tuples may be nested.
```ovid
// Tuple types:
(i32, string, bool)
(i32, (f32, bool))
// Tuple expressions
(1, "hello", false)
(num * 2, (2.0 + sum, a == 1))
```
### Tuple destructuring
```ovid
a, b, c := (1, 2, 3)
a, (b, c) := ('A', ('B', 'C'))
t = (1, (2, 3))
t[0] // -> 1
t[1] // -> (2, 3)
```

## Structures
A structure is a set of named values.
```ovid
struct Person {
    name string
    age i32
    house House
}
```
Structure creation:
```ovid
test := Person { name: "Test Person", age: 435 }
test := Person { "Test Person", 435 }
test.name // -> "Test Person"
```

## Functions
Functions are declared with the `fn` keyword.
```ovid
fn sum(num0 i32, num1 i32) -> i32 {
    return num0 + num1
}
// Functions can return tuples
fn moving_avg(n0 f64, n1 f64, n2 f64) -> (f64, f64) {
    res := (n0 + n1 / 2.0, n1 + n2 / 2.0)
    return  res
}
a0, a1 := moving_avg(1.0, 2.0, 3.0)
```

### Methods
Methods are functions bound to a receiving type. They allow invocation by the `.` operator.
```ovid
fn (self Person) hash() -> i64 {
    return self.name.hash() ^ self.age ^ self.house.hash()
}
test := Person {"test", 12}
test.hash()
// Methods can be created on existing types:
fn (self bool) hash() -> i64 {
    if self == true {
        return 1
    } else {
        return 0
    }
}
```







