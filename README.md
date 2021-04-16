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
  return y;
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
u32* foo() {
  u32 x = 7;
  u32 *y = &x;
  return y;
}
```

## Reference
* No type inference, lifetimes, traits, generics, methods, `impl`s, visibility modifiers, paths, tuples, patterns, attributes, etc.
* `match` &rarr; `switch`
* `while`, `loop`, `for i in 0..n {}`, `if-else`.
* Primitives `u32`, `i32`, `usize`, ...
* References `&x` and `*x`.
* Structs and arrays.
* No fancy `restrict`, `const` or `volatile` shenanigans.

## TODO
* [ ] Remove all the `todo!()`s and panics.
* [ ] Infer `->` operator.
* [ ] Some form of `use` modules
* [ ] `static`s
* [ ] Tuples?
* [ ] Attributes for "annotations" (e.g. `volatile`)
* [ ] Reorder declarations to avoid implicit declaration warnings
