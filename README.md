[![crates.io](https://img.shields.io/crates/d/sleef.svg)](https://crates.io/crates/sleef)
[![crates.io](https://img.shields.io/crates/v/sleef.svg)](https://crates.io/crates/sleef)
[![Released API docs](https://docs.rs/sleef/badge.svg)](https://docs.rs/sleef)
[![CI](https://github.com/burrbull/sleef-rs/workflows/CI/badge.svg?branch=master)](https://github.com/burrbull/sleef-rs)

# sleef-rs

Rust port of [Sleef] math library based on [Portable SIMD Vectors][core_simd] a.k.a. `core::simd`

## Usage

Requires nightly feature `portable_simd`.

You can call math functions directly:
```rust
#![feature(portable_simd)]

use core::simd::f64x2;

fn main() {
    let input = f64x2::from_array([1.43, 0.57]);
    let output = sleef::f64x::sin_u10(input);
    println!("sin(α) = {:?}", output);
}
```

or use `Sleef` trait:
```rust
#![feature(portable_simd)]

use core::simd::f64x2;
use sleef::Sleef;

fn main() {
    let input = f64x2::from_array([1.43, 0.57]);
    let output = input.sin();
    println!("sin(α) = {:?}", output);
}
```

[Sleef]: https://github.com/shibatch/sleef/
[core_simd]: https://github.com/rust-lang/portable-simd