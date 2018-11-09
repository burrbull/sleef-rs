#![deny(warnings)]
#![no_std]

mod common;
mod consts;

pub mod f32;
pub mod f64;

#[macro_use]
mod f32x;
#[macro_use]
mod f64x;

pub mod f32x2 {
    use packed_simd::*;

    impl_math_f32!(f32x2, u32x2, m32x2, i32x2);
}

pub mod f32x4 {
    use packed_simd::*;

    // SSE2
    // AVX2
    // AArch64 AdvSIMD
    // Arm Neon
    impl_math_f32!(f32x4, u32x4, m32x4, i32x4);
}

pub mod f32x8 {
    use packed_simd::*;
    // AVX2
    impl_math_f32!(f32x8, u32x8, m32x8, i32x8);
    // AVX
    //impl_math_f32!(f32x8, u32x8, m32x8, i32x8s);
}

pub mod f32x16 {
    use packed_simd::*;
    // AVX512
    impl_math_f32!(f32x16, u32x16, m32x16, i32x16);
}

pub mod f64x2 {
    use packed_simd::*;

    // SSE2
    // AVX2
    // AArch64 AdvSIMD
    impl_math_f64!(f64x2, u64x2, m64x2, i64x2, u32x2, m32x2, i32x2);
}

pub mod f64x4 {
    use packed_simd::*;

    // AVX2
    impl_math_f64!(f64x4, u64x4, m64x4, i64x4, u32x4, m32x4, i32x4);
    // AVX
    //impl_mathf32!(f64x4, u64x4, m64x4, i64x4s, u32x4, m32x4, i32x4);
}

pub mod f64x8 {
    use packed_simd::*;

    // AVX5122
    impl_math_f64!(f64x8, u64x8, m64x8, i64x8, u32x8, m32x8, i32x8);
}
