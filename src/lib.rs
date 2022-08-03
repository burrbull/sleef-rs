#![deny(warnings)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::mistyped_literal_suffixes)]
#![allow(clippy::excessive_precision)]
#![allow(clippy::approx_constant)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::float_cmp)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::wrong_self_convention)]
#![feature(maybe_uninit_uninit_array)]
#![feature(maybe_uninit_array_assume_init)]

mod common;
mod tables;

/// Math functions for `f32`
pub mod f32;
/// Math functions for `f64`
pub mod f64;

#[macro_use]
mod f32x;
#[macro_use]
mod f64x;

/// Math functions for `f32x2`
pub mod f32x2 {
    impl_math_f32!(2, f32x2, u32x2, i32x2, m32x2);
}

/// Math functions for `f32x4`
pub mod f32x4 {
    impl_math_f32!(4, f32x4, u32x4, i32x4, m32x4);
}

/// Math functions for `f32x8`
pub mod f32x8 {
    impl_math_f32!(8, f32x8, u32x8, i32x8, m32x8);
}

/// Math functions for `f32x16`
pub mod f32x16 {
    impl_math_f32!(16, f32x16, u32x16, i32x16, m32x16);
}

/// Math functions for `f64x2`
pub mod f64x2 {
    impl_math_f64!(2, f64x2, u64x2, i64x2, m64x2, u32x2, i32x2, m32x2);
}

/// Math functions for `f64x4`
pub mod f64x4 {
    impl_math_f64!(4, f64x4, u64x4, i64x4, m64x4, u32x4, i32x4, m32x4);
}

/// Math functions for `f64x8`
pub mod f64x8 {
    impl_math_f64!(8, f64x8, u64x8, i64x8, m64x8, u32x8, i32x8, m32x8);
}

#[cfg(test)]
const TEST_REPEAT_FAST: usize = 10_000;
#[cfg(test)]
const TEST_REPEAT: usize = 100_000;

pub trait Sleef: Sized {
    type Int;
    fn sin(self) -> Self;
    fn cos(self) -> Self;
    fn sin_cos(self) -> (Self, Self);
    fn tan(self) -> Self;
    fn asin(self) -> Self;
    fn acos(self) -> Self;
    fn atan(self) -> Self;
    fn atan2(self, other: Self) -> Self;
    fn ln(self) -> Self;
    fn cbrt(self) -> Self;
    fn exp(self) -> Self;
    fn pow(self, other: Self) -> Self;
    fn sinh(self) -> Self;
    fn cosh(self) -> Self;
    fn tanh(self) -> Self;
    fn asinh(self) -> Self;
    fn acosh(self) -> Self;
    fn atanh(self) -> Self;
    fn exp2(self) -> Self;
    fn exp10(self) -> Self;
    fn exp_m1(self) -> Self;
    fn log10(self) -> Self;
    fn log2(self) -> Self;
    fn log_1p(self) -> Self;
    fn ldexp(self, other: Self::Int) -> Self;
    fn ilogb(self) -> Self::Int;
    fn fma(self, y: Self, z: Self) -> Self;
    fn sqrt(self) -> Self;
    fn abs(self) -> Self;
    fn copy_sign(self, other: Self) -> Self;
    fn max(self, other: Self) -> Self;
    fn min(self, other: Self) -> Self;
    fn fdim(self, other: Self) -> Self;
    fn truncate(self) -> Self;
    fn round(self) -> Self;
    fn next_after(self, other: Self) -> Self;
    fn frfrexp(self) -> Self;
    fn expfrexp(self) -> Self::Int;
    fn fmod(self, other: Self) -> Self;
    fn modf(self) -> (Self, Self);
    fn sin_cos_pi(self) -> (Self, Self);
    fn sin_pi(self) -> Self;
    fn cos_pi(self) -> Self;
    fn hypot(self, other: Self) -> Self;
    fn lgamma(self) -> Self;
    fn erf(self) -> Self;
    fn erfc(self) -> Self;
}
