# lipstick 💄🐷

> You _can_ put lipstick on a pig


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
#ifndef i32
#define i32 int32_t
#endif
#ifndef u8
#define u8 uint8_t
#endif
#ifndef i8
#define i8 int8_t
#endif
#ifndef usize
#define usize uintptr_t
#endif
#ifndef isize
#define isize intptr_t
#endif
u32* foo() {
  u32 x = 7;
  u32 *y = &x;
  return y;
}
```