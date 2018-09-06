#![feature(extern_prelude)]// delete

extern crate packed_simd;

mod common;
mod consts;

pub mod double;
pub mod float;

#[macro_use]
mod f2;
//mod d2;

#[macro_use]
mod simdfloat;
//pub mod simddouble;


pub mod f32x4 {
    use packed_simd::*;
    impl_f2_f32!(f32x4, u32x4, m32x4);
    
    // SSE2
    impl_math_f32!(f32x4, u32x4, m32x4, i32x4);
    // AVX2
    //impl_math_f32!(f32x4, u32x4, m32x4, i32x4);
    /*
    // AArch64 AdvSIMD
    impl_math_f32!(f32x4, u32x4, m32x4, i32x4);
    */
    // Arm Neon
    //impl_math_f32!(f32x4, u32x4, m32x4, i32x4);
}

pub mod f32x8 {
    use packed_simd::*;
    impl_f2_f32!(f32x8, u32x8, m32x8);
    // AVX2
    impl_math_f32!(f32x8, u32x8, m32x8, i32x8);
    // AVX
    //impl_math_f32!(f32x8, u32x8, m32x8, i32x8s);
}

pub mod f32x16 {
    use packed_simd::*;
    impl_f2_f32!(f32x16, u32x16, m32x16);
    // AVX512
    impl_math_f32!(f32x16, u32x16, m32x16, i32x16);
}
