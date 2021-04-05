# lipstick 💄🐷

> You _can_ put lipstick on a pig

## What
`lipstick` compiles a subset of Rust's syntax into C. It's not a "Rust subset" though, since there's not borrow checker or lifetimes. It's simply a Rust-like syntax frontend for C.

## Why
Because it's fun, duh.

Also, it might be a good teaching tool, so you can temporarily "turn off" the borrow checker and see what is to write in unsafe system languages, without actually having any C or C++ knowledge.

### Input
```rust
fn foo() -> &u32 {
  let x: u32 = 7;
  let y: &u32 = &x;
  return y;
}
```

### Output
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