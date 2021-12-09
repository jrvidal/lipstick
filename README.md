<div align="center">
  <h1>lipstick 💄🐷</h1>
  <img src="lipstick.svg" width="250"/>

> You _can_ put lipstick on a pig

</div>

## What
`lipstick` compiles a subset of Rust's syntax into C. It's not a "Rust subset" though, since there's not borrow checker or lifetimes. It's simply a Rust-like syntax frontend for C.

Check it out in action in the [playground](https://jrvidal.github.io/lipstick).

## Why
Because it's fun, duh.

Also, it might be a good teaching tool, so you can temporarily "turn off" the borrow checker. You can see how writing in unsafe system languages looks like, without actually having any C or C++ knowledge.

## Usage
```
lipstick 0.3.0
Lipstick for C

Compile Rust-like syntax into C code

USAGE:
    lipstick [FLAGS] [OPTIONS] <INPUT> [-- <COMPILER FLAGS>...]

FLAGS:
    -C, --cc         Compile the output of lipstick by calling your C compiler
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -Z <FEATURE>                 Debugging flags [possible values: syn]
        --compiler <COMPILER>    Specifies which C compiler to invoke [default: cc]
    -o, --out <OUT>              The file to write the output to ("-" for stdout) [default: <INPUT>.c]

ARGS:
    <INPUT>                The file to be compiled to C
    <COMPILER FLAGS>...    Flags for the `cc` compiler
```

## Example

### Input
```rust
fn foo() -> &u32 {
  let x: u32 = 7;
  let y: &u32 = &x;
  y
}
```

### Output
```
lipstick foo.rs
```

```c
#include <stdint.h>
#ifndef u32
#define u32 uint32_t
#endif
u32 * foo() ;
u32 * foo() {
  u32 x = 7;
  u32 *y = &x;
  return y;
}
```
## Reference

### Primitive Types
`u8`-`u64`, `i8`-`i64`, `isize`, `usize`, `bool` and floats are translated equivalent C types. The never type `!` can be used in return position.

### Composite Types
You can define structs and unions, and use arrays with constant length. Order of definition does not matter. Function types can also be used.

### References
References `&x` and the deref operator `*x` can be used freely, without pesky lifetimes bothering you.

### Control Flow
`if - else`, `while` and `loop` work. Labeled loops and `break`/`continue` work. `for` loops can be used with literal ranges `0..n` or `0..=n`.

`match` expressions can be used and will get translated to `switch` statements but the only allowed patterns are literals or variables (except for an optional, final wildcard `_`), blocks are the only allowed bodies.

### Expressions
Implicit returns in functions work.

### #include directives
You can use `include![my_header]` or `include![<sys/elf>]` (note the lack of extension) to generate include directives.

# Type Inference
For locally defined types, the `->` operator is auto-inferred.

### What Does not Work
* No fancy `restrict`, `const` or `volatile` shenanigans.
* No type inference, lifetimes, traits, generics, methods, `impl`s, visibility modifiers, paths, tuples, patterns, attributes, etc.

## TODO
* [ ] Some form of `use` modules
* [ ] Stdlib access
* [ ] Preserve comments
* [ ] char/string semantics
* [ ] `static`s
* [ ] Tuples?
* [ ] Enums
* [ ] Attributes for "annotations" (e.g. `volatile`)
* [ ] Type inference?
* [ ] Macros?
* [ ] Map `cc` errors back to original source?