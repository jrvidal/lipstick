# lipstick 💄🐷

> You _can_ put lipstick on a pig

## What
`lipstick` compiles a subset of Rust's syntax into C. It's not a "Rust subset" though, since there's not borrow checker or lifetimes. It's simply a Rust-like syntax frontend for C.

## Why
Because it's fun, duh.

Also, it might be a good teaching tool, so you can temporarily "turn off" the borrow checker. You can see how writing in unsafe system languages looks like, without actually having any C or C++ knowledge.

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
* `match` &rarr; `switch`
* `loop {...}` &rarr; `while (1) {...}`
* `for i in 0..n {...}` &rarr; `for (uintptr_t i = 0; i < n; i++) {...}` (inclusive ranges too)
* `while`, `loop`, `for i in 0..n {}`, `if-else`.
* Rust-like primitives `u32`, `i32`, `usize`, ... can be used.
* References and deref operator `&x` / `*x` can be used freely, no lifetimes!
* Structs, unions and arrays.
* `include![my_header]` or `include![<my_standard_header>]` for `#include` directives.
* `foo.bar` &rarr; `foo.bar` or `foo->bar` when necessary
* `fn foo() -> !` &rarr; `_Noreturn void foo()`
* No fancy `restrict`, `const` or `volatile` shenanigans.
* No type inference, lifetimes, traits, generics, methods, `impl`s, visibility modifiers, paths, tuples, patterns, attributes, etc.

## TODO
* [ ] Some form of `use` modules
* [ ] Stdlib access
* [ ] Preserve comments
* [ ] char/string semantics
* [ ] `static`s
* [ ] Tuples?
* [ ] Attributes for "annotations" (e.g. `volatile`)
* [ ] Type inference?
* [ ] Macros?
* [ ] Map `cc` errors back to original source?
