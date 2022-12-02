# `syntactic-for`

[<img alt="crates.io" src="https://img.shields.io/crates/v/syntactic-for?style=for-the-badge" height="20">](https://crates.io/crates/syntactic-for)
[<img alt="docs.rs" src="https://img.shields.io/docsrs/syntactic-for?style=for-the-badge" height="20">](https://docs.rs/syntactic-for)

A syntactic "for" loop Rust macro.

For example, the following takes the sum of the bit-length of four integer
types:
```rust
let sum = syntactic_for!{ ty in [ u8, u16, u32, u64 ] {
    [$( <$ty>::BITS ),*].into_iter().sum::<u32>()
}};
assert_eq!(sum, 120);
```

## Usage

The syntax is as follows:
```rust
syntactic_for!{ IDENTIFIER in [ EXPRESSION, EXPRESSION, ... ] {
    BODY
}}
```
where `BODY` works similarly to `macro_rules!`, that is:
`$($IDENTIFIER)SEPARATOR*` will expand and substitute `IDENTIFIER` with
each `EXPRESSION`, separating the expansions with `SEPARATOR`.

`SEPARATOR` can be any non-`*` punctuation.  Hence, the example from above
could also be written without an iterator:
```rust
$( <$ty>::BITS )+*
```

## Examples

### Loop unrolling

Sum the elements of an array with
[loop unrolling](https://en.wikipedia.org/wiki/Loop_unrolling):
```rust
let array = b"oh my, I am getting summed!";
let mut acc = 0u32;
let mut i = 0;
while i <= array.len()-4 {
    syntactic_for!{ offset in [ 0, 1, 2, 3 ] {$(
        acc += array[i + $offset] as u32;
    )*}}
    i += 4;
}
for j in i..array.len() {
    acc += array[j] as u32;
}
assert_eq!(acc, 2366);
```

### Matching

Find the maximum value of an integer type of the given bit size:
```rust
let max_size = syntactic_for!{ ty in [ u8, u16, u32, u64, u128 ] {
    match bit_size {
        $(<$ty>::BITS => <$ty>::MAX as u128,)*
        other => panic!("No integer of size {other}"),
    }
}};
```

### `impl` blocks

Implement a trait for a set of types:
```rust
syntactic_for!{ ty in [ u8, u16, u32, u64, u128 ] {$(
    impl MyTrait for $ty {
        // snip.
    }
)*}}
```

### Custom syntactic loop

A useful design pattern is to define a custom macro that expands to a
syntactic loop over a given set of expressions:
```rust
#[doc(hidden)]
pub extern crate syntactic_for;

#[macro_export]
macro_rules! for_each_custom_type {
    ($ident:ident { $($tt:tt)* }) => {
        $crate::syntactic_for::syntactic_for! { $ident in [
            $crate::CustomType1,
            $crate::CustomType2,
            // etc.
        ] { $($tt)* } }
    }
}
```

For example, a library could expose `for_each_custom_type` as a way of
letting its users write syntactic loops over a set of types defined in the
library.  Then, it becomes possible to add types to that loop inside the
library, whithout requiring any change on the user's end:

```rust
// Try and parse each library type in succession, stopping at the first
// success:
fn can_parse(input: &str) -> bool {
    my_library::for_each_custom_type! { ty {
        $(if let Ok(parsed) = <$ty>::parse(input) {
            return true;
        })*
    }}
    return false;
}
```
