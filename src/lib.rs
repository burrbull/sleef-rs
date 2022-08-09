#![deny(warnings)]
#![deny(missing_docs)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::mistyped_literal_suffixes)]
#![allow(clippy::excessive_precision)]
#![allow(clippy::approx_constant)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::float_cmp)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::wrong_self_convention)]
#![feature(portable_simd)]

//! SLEEF stands for SIMD Library for Evaluating Elementary Functions.
//! It implements vectorized versions of all C99 real floating point math functions.
//! It can utilize SIMD instructions that are available on modern processors.
//! SLEEF is designed to effciently perform computation with SIMD instruction
//! by reducing the use of conditional branches and scatter/gather memory access.

mod common;
mod tables;

/// Math functions for `f32`
pub mod f32;
/// Math functions for `f64`
pub mod f64;

/// Math functions for `Simd<f32, N>`
pub mod f32x;

/// Math functions for `Simd<f64, N>`
pub mod f64x;

#[cfg(test)]
const TEST_REPEAT_FAST: usize = 10_000;
#[cfg(test)]
const TEST_REPEAT: usize = 100_000;

/// Generic trait for floating point & SIMD numbers
pub trait Sleef: Sized {
    /// Signed integer type or SIMD vector of integers
    type Int;

    /// Sine function
    fn sin(self) -> Self;

    /// Cosine function
    fn cos(self) -> Self;

    /// Evaluate sine and cosine functions simultaneously
    fn sin_cos(self) -> (Self, Self);

    /// Tangent function
    fn tan(self) -> Self;

    /// Arc sine function
    fn asin(self) -> Self;

    /// Arc cosine function
    fn acos(self) -> Self;

    /// Arc tangent function
    fn atan(self) -> Self;

    /// Arc tangent function of two variables
    fn atan2(self, other: Self) -> Self;

    /// Natural logarithmic function
    fn ln(self) -> Self;

    /// Cube root function
    fn cbrt(self) -> Self;

    /// Base-*e* exponential function
    fn exp(self) -> Self;

    /// Power function
    fn pow(self, other: Self) -> Self;

    /// Hyperbolic sine function
    fn sinh(self) -> Self;

    /// Hyperbolic cosine function
    fn cosh(self) -> Self;

    /// Hyperbolic tangent function
    fn tanh(self) -> Self;

    /// Inverse hyperbolic sine function
    fn asinh(self) -> Self;

    /// Inverse hyperbolic cosine function
    fn acosh(self) -> Self;

    /// Inverse hyperbolic tangent function
    fn atanh(self) -> Self;

    /// Base-2 exponential function
    fn exp2(self) -> Self;

    /// Base-10 exponential function
    fn exp10(self) -> Self;

    /// Base-*e* exponential function minus 1
    fn exp_m1(self) -> Self;

    /// Base-10 logarithmic function
    fn log10(self) -> Self;

    /// Base-2 logarithmic function
    fn log2(self) -> Self;

    /// Logarithm of one plus argument
    fn log_1p(self) -> Self;

    /// Multiply by integral power of `2`
    fn ldexp(self, other: Self::Int) -> Self;

    /// Integer exponent of an FP number
    fn ilogb(self) -> Self::Int;

    /// Fused multiply and accumulate
    fn fma(self, y: Self, z: Self) -> Self;

    /// Square root function
    fn sqrt(self) -> Self;

    /// Absolute value
    fn abs(self) -> Self;

    /// Copy sign of a number
    fn copy_sign(self, other: Self) -> Self;

    /// Maximum of two numbers
    fn max(self, other: Self) -> Self;

    /// Minimum of two numbers
    fn min(self, other: Self) -> Self;

    /// Positive difference
    fn fdim(self, other: Self) -> Self;

    /// Round to integer towards zero
    fn truncate(self) -> Self;

    /// Round to integer towards minus infinity
    fn floor(self) -> Self;

    /// Round to integer towards plus infinity
    fn ceil(self) -> Self;

    /// Round to integer, ties round to even
    fn round(self) -> Self;

    /// Find the next representable FP value
    fn next_after(self, other: Self) -> Self;

    /// Fractional component of an FP number
    fn frfrexp(self) -> Self;

    /// Exponent of an FP number
    fn expfrexp(self) -> Self::Int;

    /// FP remainder
    fn fmod(self, other: Self) -> Self;

    /// FP remainder
    fn remainder(self, other: Self) -> Self;

    /// Integral and fractional value of FP number
    fn modf(self) -> (Self, Self);

    /// Evaluate sin( π**a** ) and cos( π**a** ) for given **a** simultaneously
    fn sin_cos_pi(self) -> (Self, Self);

    /// Evaluate sin( π***a*** ) for given ***a***
    fn sin_pi(self) -> Self;

    /// Evaluate cos( π***a*** ) for given ***a***
    fn cos_pi(self) -> Self;

    /// 2D Euclidian distance function
    fn hypot(self, other: Self) -> Self;

    /// Gamma function
    fn gamma(self) -> Self;

    /// Log gamma function
    fn lgamma(self) -> Self;

    /// Error function
    fn erf(self) -> Self;

    /// Complementary error function
    fn erfc(self) -> Self;
}
