# Ovid Reference

Ovid is a compiled programming language. It has a strong, static type system. It takes inspiration from Rust, C, and Go.

## Comments
Single line comments can be denoted with `//`. Multi-line comments are oppened with `/*` and ended with `*/`. Multi-line comments may be nested within each other.

```ovid
// A single line comment
// another comment
/* multi
   line
   comment
   /* nested */
*/
```

## Builtin Types
### Integral Types
- `i8`, `i16`, `i32`, `i64` - signed integral types of various bit widths
- `u8`, `u16`, `u32`, `u64` - unsigned integral types
- `usize`, `isize` (TODO) - integral types capable of representing the size of an object in memory (useful for list indexes, etc -- anything bounded by pointer size). Equivalent to `size_t` in c.

Integral literals can be written in a few ways:
- Implicit base 10: `42`, `-123`
- Explicit base specifier: `0b01010110`, `0xf5ea`, `0o754`
Intgeral literals default to `i32`.

### Floating Point types
- `f32`, `f64` - 32 and 64 bit floating point numbers (equivalent to `float` and `double` in c)

Floating point literals require a decimal point:
- `12.3`, `-1.0`
Floating point literals default to `f64`

### Boolean Type
- `bool`

Boolean literals:
- `true`, `false`

## Functions
All ovid code (expect globals) must be located inside a function.

### Declaration
Functions are declared with the `fn` keywork, followed by function name, arguments, return type, and body:
```ovid
// declare a function sum that takes two arguments of types i32, and returns an i32
fn sum(a i32, b i32) -> i32 { /* ... body ... */ }
```
If a function doesn't return a value, it has the return type `void`:
```ovid
fn foo(a i32) -> void { /* ... */ }
```
If a function returns void, the return type can be omitted. The previous example could be rewritten as:
```ovid
fn foo(a i32) { /* ... */ }
```

If function can use the `return` keywork inside it's body to return a value:
```ovid
fn get_42() -> i32 {
	return 42
}
```

### Calling Functions
Functions are called using a parenthsized notation:
```ovid
get_42()
foo(5)
sum(1, 2)
sum(1, sum(sum(2, 3), 4))
```

A few builtin functions may be called using prefix and infix notation:
Identifier 	| Function				| Notation 	| Precedence 	| Associativity
-		| -					| -		| -		| -
`!`		| Logical Negation			| Prefix	| 1		| right-to-left
`~`		| Binary Negation			| Prefix	| 1		| right-to-left
`-`		| Arithmetic Negation			| Prefix	| 1		| right-to-left
`*`, `/`, `%`	| Multiplication, Division, Modulo	| Infix		| 2		| left-to-right
`+`, `-`	| Addition, Subtraction			| Infix		| 3		| left-to-right
`<<`, `>>` 	| Bitwise left and right  shifts	| Infix		| 4		| left-to-right
`<`, `<=`, `>`, `>=`, `==`, `!=` | Comparison functions | Infix		| 5		| left-to-right
`&`		| Bitwise and				| Infix		| 6		| left-to-right
`^`		| Bitwise exclusive or (xor)		| Infix		| 7		| left-to-right
`|`		| Bitwise or				| Infix		| 8		| left-to-right
`&&`		| Logical and (short-circuiting)	| Infix		| 9		| left-to-right
`||`		| Logical or (short-circuiting)		| Infix		| 10 		| left-to-right
`=`, `+=`, `-=`, `*=`, `/=`, `%=`, `>>=`, `<<=`, `&=`, `^=`, `|=` | Assignment and compound assignment | Infix | 11 | right-to-left

For example, a function returning the bitwise negation of the sum of it's arguments:
```ovid
fn not_sum(arg0 i32, arg1 i32) -> i32 {
   return ~(arg0 + arg1)
}
```

## Variables
Variables are declared with the `val` or `mut` keyword. `val` declares an immutable variable, while `mut` declares a mutable variable.

All variables must be initialized at their declaration.

By default the type of a variable is inferred from it's initializer.

```ovid
val variable = 34 // variable inferred to be type i32
val foo = !false && true // foo inferred to be type bool
mut bar = sum(1 + 1, 2 + 2)
```

Mutable variables can be assigned values after their declaration, while immutable variables cannot:
```ovid
val var1 = 1
var1 = 2 // error
mut var2 = 3
var2 = 4 // okay
```

The type of a variable can be explicitly specified:
```ovid
val number i32 = 56
```

### Globals
Globals are variables declared outside of a function. Their initializer must be constant -- either a literal or a builtin function call. Additionally, globals must have an explicit type specified:
```ovid
val global1 i32 = 1 * 2 + 5 // okay, constant initializer
val global2 i32 = func(1, 2) // error, non constant initializer
mut global3 bool = false // okay
```

## If statements
If-elsif-else statements allow for conditional control flow. They expect boolean conditions.

If-elsif-else statements don't require parenthesis around their conditions, but require braces around their bodies:

```ovid
fn factorial(n i32) -> i32 {
   if n <= 1 {
      return 1
   } else {
      return n * factorial(n - 1)
   }
}
```

If-elsif-else statements can be nested inside each other:
```ovid
fn func(cond1 bool, cond2 bool) -> i32 {
   mut res = 0

   if cond1 {
      if !cond2 {
         res = 1
      } else {
         res = 2
      }
   } elsif cond2 {
      res = 3
   } else {
      res = 4
   }

   return res
}
```

## Tuple Types
Tuple types allow for the combination of multiple values of different types into one type. 

Tuple types are written using parenthesis: `(T1, T2, ...)`

```ovid
(i32, bool)
(i32, (f64, bool), i32)
```

Tuples can be constructed with parenthesis:
```ovid
val tuple = (65, (5.4, false), 34) // tuple is of type (i32, (f64, bool), i32)
```

Individual fields of a tuple can be accessed with the `.` operator. `tuple.0` accesses the first filed, `tuple.1` the second, etc.

```ovid
fn add_points(p0 (f64, f64), p1 (f64, f64)) -> (f64, f64) {
   val x = p0.0 + p1.0
   val y = p0.1 + p1.1

   return (x, y)
}
```

### Destructuring (TODO)
Tuples can be destructured during variable declaration and assignment:
```ovid
fn foo1() -> (bool, bool) { /* ... */ }
fn foo2() -> ((bool, bool), i32) { /* ... */ }

fn bar() {
   mut a, b = foo1()
   mut c = 1

   (a, b), c = foo2()
}
```

## Pointer Types
A pointer to type `T` is written `*T`. 

`*T` does not allow mutation of the contents of `T`. `*mut T` does allow mutation of the contents of `T`.

Pointers are dereferenced using the prefix `*` operator. The address of a variable can be taken with the prefix `&` operator:

```ovid
fn set(ptr *mut i32, val i32) {
   *ptr = val
}

fn foo() {
   mut a = 1
   set(&a, 5)
   // a is now 5
}
```

The `&` operator produces an address that is valid for use anywhere in the program. 

By default, variables are stack allocated. If the compiler determines that an address of a variable may be in use after a function's stack is destroyed, it is instead allocated on the heap. Heap allocated variables are garbage collected once they are no longer accessible.

```ovid
fn foo() -> *mut i32 {
   mut var = 5
   return &var // the result of foo() is safe to dereference, since var is allocated on the heap
}
```

Ovid does not have a null pointer, so pointers are always safe to dereference.

### Automatic field dereferencing

If the field selection operator (`.`) is used on a pointer to a tuple type, the pointer is implicitly dereferenced.

Thus, this function:
```ovid
fn foo1(a *(i32, i32)) -> i32 {
   return a.0
}
```
is equivalent to the more verbose:
```ovid
fn foo2(a *(i32, i32)) -> i32 {
   return (*a).0
}
```
