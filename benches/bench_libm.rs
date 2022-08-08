#![feature(test)]
#![feature(portable_simd)]
#![feature(concat_idents)]

extern crate test;
use core_simd::{f32x16 as c_f32x16, f32x4 as c_f32x4, f64x4 as c_f64x4, f64x8 as c_f64x8};
use sleef::Sleef;
use sleef::{f32 as s_f32, f32x as s_f32x, f64 as s_f64, f64x as s_f64x};
use std_float::StdLibm;

use test::{black_box, Bencher};

use core::simd::{f32x16, f32x4, f64x4, f64x8};
use std::simd::StdFloat;

const N: usize = 1024;
const VAL_F: f32 = 0.5;
const VAL_D: f64 = 0.5;

fn init_f32x4() -> Vec<f32x4> {
    vec![f32x4::splat(black_box(VAL_F)); N / 4]
}

fn c_init_f32x4() -> Vec<c_f32x4> {
    vec![c_f32x4::splat(black_box(VAL_F)); N / 4]
}

fn init_f32x16() -> Vec<f32x16> {
    vec![f32x16::splat(black_box(VAL_F)); N / 16]
}

fn c_init_f32x16() -> Vec<c_f32x16> {
    vec![c_f32x16::splat(black_box(VAL_F)); N / 16]
}

fn init_f32() -> Vec<f32> {
    vec![black_box(VAL_F); N]
}

fn init_f64x4() -> Vec<f64x4> {
    vec![f64x4::splat(black_box(VAL_D)); N / 4]
}
fn c_init_f64x4() -> Vec<c_f64x4> {
    vec![c_f64x4::splat(black_box(VAL_D)); N / 4]
}

fn init_f64x8() -> Vec<f64x8> {
    vec![f64x8::splat(black_box(VAL_D)); N / 8]
}

fn c_init_f64x8() -> Vec<c_f64x8> {
    vec![c_f64x8::splat(black_box(VAL_D)); N / 8]
}

fn init_f64() -> Vec<f64> {
    vec![black_box(VAL_D); N]
}

// These fuctions are not inlined to make it easier to check the asm.
//
// Build with:
//
// RUSTFLAGS="-C target-cpu=native --emit asm" cargo bench

macro_rules! benchmark_libm {
    (
        functions ($(
            $names : ident,
            $functions : expr,
            $init : expr
        )*)
    ) => {

        $(
            #[bench]
            #[inline(never)]
            fn $names(b: &mut Bencher) {
                let x = $init;
                let mut y = $init;
                b.iter(|| {
                    for (x, y) in x.iter().zip(y.iter_mut()) {
                        *y = ($functions)(*x);
                    }
                })
            }
        )*
    }
}

benchmark_libm! {
    functions (
        sqrt_f32_std,    f32::sqrt,    init_f32()
        sqrt_f32_libm,    libm::sqrtf,    init_f32()
        sqrt_f32_u35,    s_f32::sqrt_u35,    init_f32()
        sqrt_f32_u05,    s_f32::sqrt_u05,    init_f32()
        sqrt_f32x4_std,  StdFloat::sqrt,  init_f32x4()
        sqrt_f32x4_u35,  s_f32x::sqrt_u35,  init_f32x4()
        sqrt_f32x4_u05,  s_f32x::sqrt_u05,  init_f32x4()

        sqrt_f64_std,    f64::sqrt,    init_f64()
        sqrt_f64_libm,    libm::sqrt,    init_f64()
        sqrt_f64x4_std,  StdFloat::sqrt,  init_f64x4()
        sqrt_f64_u35,    s_f64::sqrt_u35,    init_f64()
        sqrt_f64_u05,    s_f64::sqrt_u05,    init_f64()
        sqrt_f64x4_u35,  s_f64x::sqrt_u35,  init_f64x4()
        sqrt_f64x4_u05,  s_f64x::sqrt_u05,  init_f64x4()
    )
}

benchmark_libm! {
    functions (
        cos_f32_std,    f32::cos,    init_f32()
        cos_f32_libm,    libm::cosf,    init_f32()
        cos_f32_fast,   s_f32::cos_fast,   init_f32()
        cos_f32_u35,    s_f32::cos_u35,    init_f32()
        cos_f32_u10,    s_f32::cos_u10,    init_f32()

        cos_f32x4_fast, s_f32x::cos_fast, init_f32x4()
        cos_f32x4_u35,  s_f32x::cos_u35,  init_f32x4()
        cos_f32x4_u35d, s_f32x::cos_u35_deterministic,  init_f32x4()
        cos_f32x4_u10,  s_f32x::cos_u10,  init_f32x4()
        cos_f32x4_u10d, s_f32x::cos_u10_deterministic,  init_f32x4()
        cos_f32x4_core, c_f32x4::cos,  c_init_f32x4()

        cos_f32x16_fast, s_f32x::cos_fast, init_f32x16()
        cos_f32x16_u35,  s_f32x::cos_u35,  init_f32x16()
        cos_f32x16_u35d, s_f32x::cos_u35_deterministic,  init_f32x16()
        cos_f32x16_u10,  s_f32x::cos_u10,  init_f32x16()
        cos_f32x16_u10d, s_f32x::cos_u10_deterministic,  init_f32x16()
        cos_f32x16_core, c_f32x16::cos,  c_init_f32x16()

        cos_f64_std,    f64::cos,    init_f64()
        cos_f64_libm,    libm::cos,    init_f64()
        cos_f64_u35,    s_f64::cos_u35,    init_f64()
        cos_f64_u10,    s_f64::cos_u10,    init_f64()

        cos_f64x4_u35,  s_f64x::cos_u35,  init_f64x4()
        cos_f64x4_u35d, s_f64x::cos_u35_deterministic,  init_f64x4()
        cos_f64x4_u10,  s_f64x::cos_u10,  init_f64x4()
        cos_f64x4_u10d, s_f64x::cos_u10_deterministic,  init_f64x4()
        cos_f64x4_core, c_f64x4::cos,  c_init_f64x4()

        cos_f64x8_u35,  s_f64x::cos_u35,  init_f64x8()
        cos_f64x8_u35d, s_f64x::cos_u35_deterministic,  init_f64x8()
        cos_f64x8_u10,  s_f64x::cos_u10,  init_f64x8()
        cos_f64x8_u10d, s_f64x::cos_u10_deterministic,  init_f64x8()
        cos_f64x8_core, c_f64x8::cos,  c_init_f64x8()
    )
}

benchmark_libm! {
    functions (
        tan_f32x4,  |x : f32x4| x.tan(),  init_f32x4()
        tan_f32x16, |x : f32x16| x.tan(), init_f32x16()
        tan_f32,    |x : f32| x.tan(),    init_f32()
        tan_f64x4,  |x : f64x4| x.tan(),  init_f64x4()
        tan_f64x8,  |x : f64x8| x.tan(),  init_f64x8()
        tan_f64,    |x : f64| x.tan(),    init_f64()
    )
}

benchmark_libm! {
    functions (
        asin_f32_std,    f32::asin,    init_f32()
        asin_f32_libm,    libm::asinf,    init_f32()
        asin_f32_u35,    s_f32::asin_u35,    init_f32()
        asin_f32_u10,    s_f32::asin_u10,    init_f32()

        asin_f32x4_u35,  s_f32x::asin_u35,  init_f32x4()
        asin_f32x4_u10,  s_f32x::asin_u10,  init_f32x4()
        asin_f32x4_core,  c_f32x4::asin,  c_init_f32x4()

        asin_f32x16_u35,  s_f32x::asin_u35,  init_f32x16()
        asin_f32x16_u10,  s_f32x::asin_u10,  init_f32x16()
        asin_f32x16_core,  c_f32x16::asin,  c_init_f32x16()

        asin_f64_std,    f64::asin,    init_f64()
        asin_f64_libm,    libm::asin,    init_f64()
        asin_f64_u35,    s_f64::asin_u35,    init_f64()
        asin_f64_u10,    s_f64::asin_u10,    init_f64()

        asin_f64x4_u35,  s_f64x::asin_u35,  init_f64x4()
        asin_f64x4_u10,  s_f64x::asin_u10,  init_f64x4()
        asin_f64x4_core,  c_f64x4::asin,  c_init_f64x4()

        asin_f64x8_u35,  s_f64x::asin_u35,  init_f64x8()
        asin_f64x8_u10,  s_f64x::asin_u10,  init_f64x8()
        asin_f64x8_core,  c_f64x8::asin,  c_init_f64x8()
    )
}

benchmark_libm! {
    functions (
        atan_f32_std,    f32::atan,    init_f32()
        atan_f32_libm,    libm::atanf,    init_f32()
        atan_f32_u35,    s_f32::atan_u35,    init_f32()
        atan_f32_u10,    s_f32::atan_u10,    init_f32()

        atan_f32x4_u35,  s_f32x::atan_u35,  init_f32x4()
        atan_f32x4_u10,  s_f32x::atan_u10,  init_f32x4()
        atan_f32x4_core,  c_f32x4::atan,  c_init_f32x4()

        atan_f32x16_u35,  s_f32x::atan_u35,  init_f32x16()
        atan_f32x16_u10,  s_f32x::atan_u10,  init_f32x16()
        atan_f32x16_core,  c_f32x16::atan,  c_init_f32x16()

        atan_f64_std,    f64::atan,    init_f64()
        atan_f64_libm,    libm::atan,    init_f64()
        atan_f64_u35,    s_f64::atan_u35,    init_f64()
        atan_f64_u10,    s_f64::atan_u10,    init_f64()

        atan_f64x4_u35,  s_f64x::atan_u35,  init_f64x4()
        atan_f64x4_u10,  s_f64x::atan_u10,  init_f64x4()
        atan_f64x4_core,  c_f64x4::atan,  c_init_f64x4()

        atan_f64x8_u35,  s_f64x::atan_u35,  init_f64x8()
        atan_f64x8_u10,  s_f64x::atan_u10,  init_f64x8()
        atan_f64x8_core,  c_f64x8::atan,  c_init_f64x8()
    )
}

benchmark_libm! {
    functions (
        exp2_f32_std,    f32::exp2,    init_f32()
        exp2_f32_libm,    libm::exp2f,    init_f32()
        exp2_f32_u10,    s_f32::exp2_u10,    init_f32()

        exp2_f32x4_u10,  s_f32x::exp2_u10,  init_f32x4()
        exp2_f32x4_core,  c_f32x4::exp2,  c_init_f32x4()

        exp2_f32x16_u10,  s_f32x::exp2_u10,  init_f32x16()
        exp2_f32x16_core,  c_f32x16::exp2,  c_init_f32x16()

        exp2_f64_std,    f64::exp2,    init_f64()
        exp2_f64_libm,    libm::exp2,    init_f64()
        exp2_f64_u10,    s_f64::exp2_u10,    init_f64()

        exp2_f64x4_u10,  s_f64x::exp2_u10,  init_f64x4()
        exp2_f64x4_core,  c_f64x4::exp2,  c_init_f64x4()

        exp2_f64x8_u10,  s_f64x::exp2_u10,  init_f64x8()
        exp2_f64x8_core,  c_f64x8::exp2,  c_init_f64x8()
    )
}

benchmark_libm! {
    functions (
        exp_f32_std,    f32::exp,    init_f32()
        exp_f32_libm,    libm::expf,    init_f32()
        exp_f32_u10,    s_f32::exp_u10,    init_f32()

        exp_f32x4_u10,  s_f32x::exp_u10,  init_f32x4()
        exp_f32x4_core,  c_f32x4::exp,  c_init_f32x4()

        exp_f32x16_u10,  s_f32x::exp_u10,  init_f32x16()
        exp_f32x16_core,  c_f32x16::exp,  c_init_f32x16()

        exp_f64_std,    f64::exp,    init_f64()
        exp_f64_libm,    libm::exp,    init_f64()
        exp_f64_u10,    s_f64::exp_u10,    init_f64()

        exp_f64x4_u10,  s_f64x::exp_u10,  init_f64x4()
        exp_f64x4_core,  c_f64x4::exp,  c_init_f64x4()

        exp_f64x8_u10,  s_f64x::exp_u10,  init_f64x8()
        exp_f64x8_core,  c_f64x8::exp,  c_init_f64x8()
    )
}

benchmark_libm! {
    functions (
        log2_f32x4_u35,  s_f32x::log2_u10,  init_f32x4()
        log2_f32x4_u10,  s_f32x::log2_u10,  init_f32x4()
        log2_f32x4_core,  c_f32x4::log2,  c_init_f32x4()
        log2_f32x16, |x : f32x16| x.log2(), init_f32x16()
        log2_f32,    |x : f32| x.log2(),    init_f32()
        log2_f64x4,  |x : f64x4| x.log2(),  init_f64x4()
        log2_f64x8,  |x : f64x8| x.log2(),  init_f64x8()
        log2_f64,    |x : f64| x.log2(),    init_f64()
    )
}

benchmark_libm! {
    functions (
        ln_f32_std,    f32::ln,    init_f32()
        ln_f32_libm,    libm::logf,    init_f32()
        ln_f32_u35,    s_f32::log_u35,    init_f32()
        ln_f32_u10,    s_f32::log_u10,    init_f32()

        ln_f32x4_u35,  s_f32x::log_u35,  init_f32x4()
        ln_f32x4_u10,  s_f32x::log_u10,  init_f32x4()
        ln_f32x4_core,  c_f32x4::ln,  c_init_f32x4()

        ln_f32x16_u35,  s_f32x::log_u35,  init_f32x16()
        ln_f32x16_u10,  s_f32x::log_u10,  init_f32x16()
        ln_f32x16_core,  c_f32x16::ln,  c_init_f32x16()

        ln_f64_std,    f64::ln,    init_f64()
        ln_f64_libm,    libm::log,    init_f64()
        ln_f64_u35,    s_f64::log_u35,    init_f64()
        ln_f64_u10,    s_f64::log_u10,    init_f64()

        ln_f64x4_u35,  s_f64x::log_u35,  init_f64x4()
        ln_f64x4_u10,  s_f64x::log_u10,  init_f64x4()
        ln_f64x4_core,  c_f64x4::ln,  c_init_f64x4()

        ln_f64x8_u35,  s_f64x::log_u35,  init_f64x8()
        ln_f64x8_u10,  s_f64x::log_u10,  init_f64x8()
        ln_f64x8_core,  c_f64x8::ln,  c_init_f64x8()
    )
}
