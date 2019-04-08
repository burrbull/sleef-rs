#![deny(warnings)]
#![allow(dead_code)] // temporary
#![cfg_attr(not(feature = "std"), no_std)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::mistyped_literal_suffixes)]
#![allow(clippy::excessive_precision)]
#![allow(clippy::approx_constant)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::float_cmp)]
#![allow(clippy::cyclomatic_complexity)]

mod common;
mod tables;

pub mod f32;
pub mod f64;

#[macro_use]
mod f32x;
#[macro_use]
mod f64x;

pub mod f32x2 {
    use packed_simd::*;

    impl_math_f32!(2);
}

pub mod f32x4 {
    use packed_simd::*;

    // SSE2
    // AVX2
    // AArch64 AdvSIMD
    // Arm Neon
    impl_math_f32!(4);
}

pub mod f32x8 {
    use packed_simd::*;
    // AVX2
    impl_math_f32!(8);
    // AVX
    //impl_math_f32!(f32x8, u32x8, m32x8, i32x8s);
}

pub mod f32x16 {
    use packed_simd::*;
    // AVX512
    impl_math_f32!(16);
}

pub mod f64x2 {
    use packed_simd::*;

    // SSE2
    // AVX2
    // AArch64 AdvSIMD
    impl_math_f64!(2, u32, i32, m32);
}

pub mod f64x4 {
    use packed_simd::*;

    // AVX2
    impl_math_f64!(4, u32, i32, m32);
    // AVX
    //impl_mathf32!(f64x4, u64x4, m64x4, i64x4s, u32x4, m32x4, i32x4);
}

pub mod f64x8 {
    use packed_simd::*;

    // AVX5122
    impl_math_f64!(8, u32, i32, m32);
}

#[cfg(test)]
#[cfg(feature = "fasttest")]
const TEST_REPEAT: usize = 1_000;

#[cfg(test)]
#[cfg(not(feature = "fasttest"))]
const TEST_REPEAT: usize = 100_000;
