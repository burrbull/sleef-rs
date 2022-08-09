#[cfg(test)]
mod tst;
#[cfg(test)]
pub(crate) use tst::*;
mod constants;
pub(crate) use constants::*;

/// Fast functions with 350.0 ULP error bound
mod fast;
pub use fast::{cosf as cos_fast, powf as pow_fast, sinf as sin_fast};

/// Functions with 0.5 ULP error bound
mod u05;
pub use u05::{
    cospif as cospi_u05, hypotf as hypot_u05, sincospif as sincospi_u05, sinpif as sinpi_u05,
    sqrtf as sqrt_u05,
};

/// Functions with 1.0 ULP error bound
mod u10;
pub use u10::{
    acosf as acos_u10, acoshf as acosh_u10, asinf as asin_u10, asinhf as asinh_u10,
    atan2f as atan2_u10, atanf as atan_u10, atanhf as atanh_u10, cbrtf as cbrt_u10,
    cosf as cos_u10, cosf_deterministic as cos_u10_deterministic, coshf as cosh_u10,
    erff as erf_u10, exp10f as exp10_u10, exp2f as exp2_u10, expf as exp_u10, expm1f as expm1_u10,
    lgammaf as lgamma_u10, log10f as log10_u10, log1pf as log1p_u10, log2f as log2_u10,
    logf as log_u10, powf as pow_u10, sincosf as sincos_u10,
    sincosf_deterministic as sincos_u10_deterministic, sinf as sin_u10,
    sinf_deterministic as sin_u10_deterministic, sinhf as sinh_u10, tanf as tan_u10,
    tanf_deterministic as tan_u10_deterministic, tanhf as tanh_u10, tgammaf as tgamma_u10,
};

/// Functions with 1.5 ULP error bound
mod u15;
pub use u15::erfcf as erfc_u15;

/// Functions with 3.5 ULP error bound
mod u35;
pub use u35::{
    acosf as acos_u35, asinf as asin_u35, atan2f as atan2_u35, atanf as atan_u35,
    cbrtf as cbrt_u35, cosf as cos_u35, cosf_deterministic as cos_u35_deterministic,
    coshf as cosh_u35, exp10f as exp10_u35, exp2f as exp2_u35, hypotf as hypot_u35,
    log2f as log2_u35, logf as log_u35, sincosf as sincos_u35,
    sincosf_deterministic as sincos_u35_deterministic, sincospif as sincospi_u35, sinf as sin_u35,
    sinf_deterministic as sin_u35_deterministic, sinhf as sinh_u35, sqrtf as sqrt_u35,
    tanf as tan_u35, tanf_deterministic as tan_u35_deterministic, tanhf as tanh_u35,
};

use crate::common::*;
use doubled::*;

use core::simd::{
    LaneCount, Mask, Simd, SimdFloat, SimdPartialEq, SimdPartialOrd, SupportedLaneCount,
};

type F32x<const N: usize> = Simd<f32, N>;
type U32x<const N: usize> = Simd<u32, N>;
type I32x<const N: usize> = Simd<i32, N>;
type M32x<const N: usize> = Mask<i32, N>;

impl<const N: usize> MaskType for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Mask = M32x<N>;
}

impl<const N: usize> BitsType for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Bits = U32x<N>;
}

impl<const N: usize> MaskType for Doubled<F32x<N>>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Mask = M32x<N>;
}

impl<const N: usize> crate::Sleef for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Int = I32x<N>;
    #[inline]
    fn sin(self) -> Self {
        u35::sinf(self)
    }
    #[inline]
    fn cos(self) -> Self {
        u35::cosf(self)
    }
    #[inline]
    fn sin_cos(self) -> (Self, Self) {
        u35::sincosf(self)
    }
    #[inline]
    fn tan(self) -> Self {
        u35::tanf(self)
    }
    #[inline]
    fn asin(self) -> Self {
        u35::asinf(self)
    }
    #[inline]
    fn acos(self) -> Self {
        u35::acosf(self)
    }
    #[inline]
    fn atan(self) -> Self {
        u35::atanf(self)
    }
    #[inline]
    fn atan2(self, other: Self) -> Self {
        u35::atan2f(self, other)
    }
    #[inline]
    fn ln(self) -> Self {
        u35::logf(self)
    }
    #[inline]
    fn cbrt(self) -> Self {
        u35::cbrtf(self)
    }
    #[inline]
    fn exp(self) -> Self {
        u10::expf(self)
    }
    #[inline]
    fn pow(self, other: Self) -> Self {
        u10::powf(self, other)
    }
    #[inline]
    fn sinh(self) -> Self {
        u10::sinhf(self)
    }
    #[inline]
    fn cosh(self) -> Self {
        u10::coshf(self)
    }
    #[inline]
    fn tanh(self) -> Self {
        u10::tanhf(self)
    }
    #[inline]
    fn asinh(self) -> Self {
        u10::asinhf(self)
    }
    #[inline]
    fn acosh(self) -> Self {
        u10::acoshf(self)
    }
    #[inline]
    fn atanh(self) -> Self {
        u10::atanhf(self)
    }
    #[inline]
    fn exp2(self) -> Self {
        u10::exp2f(self)
    }
    #[inline]
    fn exp10(self) -> Self {
        u10::exp10f(self)
    }
    #[inline]
    fn exp_m1(self) -> Self {
        u10::expm1f(self)
    }
    #[inline]
    fn log10(self) -> Self {
        u10::log10f(self)
    }
    #[inline]
    fn log2(self) -> Self {
        u10::log2f(self)
    }
    #[inline]
    fn log_1p(self) -> Self {
        u10::log1pf(self)
    }
    #[inline]
    fn ldexp(self, other: Self::Int) -> Self {
        ldexpf(self, other)
    }
    #[inline]
    fn ilogb(self) -> Self::Int {
        ilogbf(self)
    }
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
        fmaf(self, y, z)
    }
    #[inline]
    fn sqrt(self) -> Self {
        sqrtf(self)
    }
    #[inline]
    fn abs(self) -> Self {
        fabsf(self)
    }
    #[inline]
    fn copy_sign(self, other: Self) -> Self {
        copysignf(self, other)
    }
    #[inline]
    fn max(self, other: Self) -> Self {
        fmaxf(self, other)
    }
    #[inline]
    fn min(self, other: Self) -> Self {
        fminf(self, other)
    }
    #[inline]
    fn fdim(self, other: Self) -> Self {
        fdimf(self, other)
    }
    #[inline]
    fn truncate(self) -> Self {
        truncf(self)
    }
    #[inline]
    fn floor(self) -> Self {
        floorf(self)
    }
    #[inline]
    fn ceil(self) -> Self {
        ceilf(self)
    }
    #[inline]
    fn round(self) -> Self {
        rintf(self)
    }
    #[inline]
    fn next_after(self, other: Self) -> Self {
        nextafterf(self, other)
    }
    #[inline]
    fn frfrexp(self) -> Self {
        frfrexpf(self)
    }
    #[inline]
    fn expfrexp(self) -> Self::Int {
        expfrexpf(self)
    }
    #[inline]
    fn fmod(self, other: Self) -> Self {
        fmodf(self, other)
    }
    #[inline]
    fn remainder(self, other: Self) -> Self {
        remainderf(self, other)
    }
    #[inline]
    fn modf(self) -> (Self, Self) {
        modff(self)
    }
    #[inline]
    fn sin_cos_pi(self) -> (Self, Self) {
        u35::sincospif(self)
    }
    #[inline]
    fn sin_pi(self) -> Self {
        u05::sinpif(self)
    }
    #[inline]
    fn cos_pi(self) -> Self {
        u05::cospif(self)
    }
    #[inline]
    fn hypot(self, other: Self) -> Self {
        u35::hypotf(self, other)
    }
    #[inline]
    fn gamma(self) -> Self {
        u10::tgammaf(self)
    }
    #[inline]
    fn lgamma(self) -> Self {
        u10::lgammaf(self)
    }
    #[inline]
    fn erf(self) -> Self {
        u10::erff(self)
    }
    #[inline]
    fn erfc(self) -> Self {
        u15::erfcf(self)
    }
}

impl<const N: usize> Sqrt for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn sqrt(self) -> Self {
        use std::simd::StdFloat;
        <Self as StdFloat>::sqrt(self)
    }
}

impl<const N: usize> SqrtAsDoubled for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn sqrt_as_doubled(self) -> Doubled<Self> {
        let t = self.sqrt();
        ((self + t.mul_as_doubled(t)) * t.recip_as_doubled()).scale(Self::splat(0.5))
    }
}

impl<const N: usize> Round for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Int = I32x<N>;
    #[inline]
    fn trunc(self) -> Self {
        self.trunci().cast()
    }
    #[inline]
    fn trunci(self) -> Self::Int {
        self.cast()
    }
    #[inline]
    fn round(self) -> Self {
        rintf(self)
    }
    #[inline]
    fn roundi(self) -> Self::Int {
        self.round().cast()
    }
}

impl<const N: usize> MulAdd for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        if cfg!(target_feature = "fma") {
            use std::simd::StdFloat;
            self.mul_add(y, z)
        } else {
            self * y + z
        }
    }
}

impl<const N: usize> MulSub for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn mul_sub(self, y: Self, z: Self) -> Self {
        if cfg!(target_feature = "fma") {
            use std::simd::StdFloat;
            self.mul_add(y, -z)
        } else {
            self * y - z
        }
    }
}

impl<const N: usize> NegMulAdd for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn neg_mul_add(self, y: Self, z: Self) -> Self {
        if cfg!(target_feature = "fma") {
            use std::simd::StdFloat;
            (-self).mul_add(y, z)
        } else {
            -self * y + z
        }
    }
}

impl<const N: usize> VectorizedSelect<f32> for M32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Output = F32x<N>;
    fn select_splat(self, l: f32, r: f32) -> Self::Output {
        self.select(Self::Output::splat(l), Self::Output::splat(r))
    }
}

impl<const N: usize> DoubledSelect<F32x<N>> for M32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    fn select_doubled(self, l: Doubled<F32x<N>>, r: Doubled<F32x<N>>) -> Doubled<F32x<N>> {
        Doubled::new(self.select(l.0, r.0), self.select(l.1, r.1))
    }
}

impl<const N: usize> SelectSeveral<f32> for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn select3(o0: Self::Mask, o1: Self::Mask, d0: f32, d1: f32, d2: f32) -> Self {
        o0.select(Self::splat(d0), o1.select_splat(d1, d2))
    }
    fn select4(
        o0: Self::Mask,
        o1: Self::Mask,
        o2: Self::Mask,
        d0: f32,
        d1: f32,
        d2: f32,
        d3: f32,
    ) -> Self {
        o0.select(
            Self::splat(d0),
            o1.select(Self::splat(d1), o2.select_splat(d2, d3)),
        )
    }
}

impl<const N: usize> SelectSeveral<f64> for Doubled<F32x<N>>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn select3(o0: Self::Mask, o1: Self::Mask, d0: f64, d1: f64, d2: f64) -> Self {
        o0.select_doubled(
            Doubled::from(d0),
            o1.select_doubled(Doubled::from(d1), Doubled::from(d2)),
        )
    }
    fn select4(
        o0: Self::Mask,
        o1: Self::Mask,
        o2: Self::Mask,
        d0: f64,
        d1: f64,
        d2: f64,
        d3: f64,
    ) -> Self {
        o0.select_doubled(
            Doubled::from(d0),
            o1.select_doubled(
                Doubled::from(d1),
                o2.select_doubled(Doubled::from(d2), Doubled::from(d3)),
            ),
        )
    }
}

impl<const N: usize> Poly<f32> for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    fn c2v(c: f32) -> Self {
        F32x::splat(c)
    }
}

impl<const N: usize> Poly<Self> for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    fn c2v(c: Self) -> Self {
        c
    }
}

impl<const N: usize> Sign for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn sign_bit(self) -> Self::Bits {
        self.to_bits() & neg_zero().to_bits()
    }
    #[inline]
    fn sign(self) -> Self {
        Self::from_bits(one().to_bits() | (self.sign_bit()))
    }
    #[inline]
    fn mul_sign(self, other: Self) -> Self {
        Self::from_bits(self.to_bits() ^ other.sign_bit())
    }
    #[inline]
    fn or_sign(self, other: Self) -> Self {
        Self::from_bits(self.to_bits() | other.sign_bit())
    }
    #[inline]
    fn copy_sign(self, other: Self) -> Self {
        Self::from_bits((!neg_zero().to_bits() & self.to_bits()) ^ (other.sign_bit()))
    }
}

impl<const N: usize> IsNegZero for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn is_neg_zero(self) -> Self::Mask {
        self.to_bits().simd_eq(neg_zero().to_bits())
    }
}

impl<const N: usize> IsInt for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn is_integer(self) -> Self::Mask {
        self.trunc().simd_eq(self)
    }
}

#[inline]
pub(crate) fn from_slice_offset<const N: usize>(ptr: &[f32], vi: I32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    //F32x::gather_or_default(ptr, vi.cast()) // Failes to compile on release
    let ar: [f32; N] = core::array::from_fn(|i| ptr[vi[i] as usize]);
    F32x::from_array(ar)
}

/*#[cfg(
    all(not(feature = "enable_avx512f"),
    not(feature = "enable_avx512fnofma")
))]*/
#[inline]
pub(crate) fn ilogbkf<const N: usize>(mut d: F32x<N>) -> I32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let o = d.simd_lt(F32x::splat(5.421_010_862_427_522_e-20));
    d = o.select(F32x::splat(1.844_674_407_370_955_2_e19) * d, d);
    let q = (d.to_bits() >> U32x::splat(23)).cast() & I32x::splat(0xff);
    q - o.select(I32x::splat(64 + 0x7f), I32x::splat(0x7f))
}

/*#[cfg(
    all(not(feature = "enable_avx512f"),
    not(feature = "enable_avx512fnofma")
))]*/
#[inline]
pub(crate) fn ilogb2kf<const N: usize>(d: F32x<N>) -> I32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let q = d.to_bits();
    let mut q = (q >> U32x::splat(23)).cast();
    q &= I32x::splat(0xff);
    q - I32x::splat(0x7f)
}

/// Integer exponent of an FP number
pub fn ilogbf<const N: usize>(d: F32x<N>) -> I32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut e = ilogbkf(d.abs());
    e = d.simd_eq(zero()).select(sleef_fp_ilogb0(), e);
    e = d.is_nan().select(sleef_fp_ilogbnan(), e);
    d.is_infinite().select(I32x::splat(i32::MAX), e)
}
#[inline]
pub(crate) fn pow2if<const N: usize>(q: I32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    F32x::from_bits(((q + I32x::splat(0x7f)) << I32x::splat(23)).cast())
}

#[inline]
pub(crate) fn ldexpkf<const N: usize>(mut x: F32x<N>, mut q: I32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut m = q >> I32x::splat(31);
    m = (((m + q) >> I32x::splat(6)) - m) << I32x::splat(4);
    q -= m << I32x::splat(2);
    m += I32x::splat(0x7f);
    m = m.simd_gt(I32x::splat(0)).to_int() & m;
    let n = m.simd_gt(I32x::splat(0xff)).to_int();
    m = (!n & m) | (n & I32x::splat(0xff));
    let u = F32x::from_bits((m << I32x::splat(23)).cast());
    x *= u * u * u * u;
    let u = F32x::from_bits(((q + I32x::splat(0x7f)) << I32x::splat(23)).cast());
    x * u
}

#[inline]
pub(crate) fn ldexp2kf<const N: usize>(d: F32x<N>, e: I32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let e1 = e >> I32x::splat(1);
    d * pow2if(e1) * pow2if(e - e1)
}

#[inline]
pub(crate) fn ldexp3kf<const N: usize>(d: F32x<N>, q: I32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    F32x::from_bits((d.to_bits().cast() + (q << I32x::splat(23))).cast())
}

/// Multiply by integral power of `2`
///
/// These functions return the result of multiplying ***m*** by `2` raised to the power ***x***.
pub fn ldexpf<const N: usize>(x: F32x<N>, q: I32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    ldexpkf(x, q)
}

#[inline]
pub(crate) fn rempisubf<const N: usize>(x: F32x<N>) -> (F32x<N>, I32x<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    if cfg!(feature = "full_fp_rounding") {
        let y = (x * F32x::splat(4.)).round();
        let vi = (y - x.round() * F32x::splat(4.)).trunci();
        (x - y * F32x::splat(0.25), vi)
    } else {
        let c = f1_23x().mul_sign(x);
        let rint4x = (F32x::splat(4.) * x).abs().simd_gt(f1_23x()).select(
            F32x::splat(4.) * x,
            (F32x::splat(4.).mla(x, c) - c).or_sign(x),
        );
        let rintx = x
            .abs()
            .simd_gt(f1_23x())
            .select(x, ((x + c) - c).or_sign(x));

        let fr = F32x::splat(-0.25).mla(rint4x, x);
        let vi = F32x::splat(-4.).mla(rintx, rint4x).trunci();
        (fr, vi)
    }
}

#[inline]
pub(crate) fn rempif<const N: usize>(mut a: F32x<N>) -> (Doubled<F32x<N>>, I32x<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut ex = ilogb2kf(a);
    /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
        ex = !(ex >> 31) & ex;
        ex = ex & I32x::splat(127);
    }*/
    ex -= I32x::splat(25);
    let q = ex.simd_gt(I32x::splat(90 - 25)).to_int() & I32x::splat(-64);
    a = ldexp3kf(a, q);
    ex = !(ex >> I32x::splat(31)) & ex;
    ex <<= I32x::splat(2);
    let mut x = a.mul_as_doubled(from_slice_offset(&crate::tables::REMPITABSP, ex));
    let (did, mut q) = rempisubf(x.0);
    x.0 = did;
    x = x.normalize();
    let y = a.mul_as_doubled(from_slice_offset(&crate::tables::REMPITABSP[1..], ex));
    x += y;
    let (did, dii) = rempisubf(x.0);
    q += dii;
    x.0 = did;
    x = x.normalize();
    let mut y = Doubled::new(
        from_slice_offset(&crate::tables::REMPITABSP[2..], ex),
        from_slice_offset(&crate::tables::REMPITABSP[3..], ex),
    );
    y *= a;
    x += y;
    x = x.normalize();
    x *= Doubled::<F32x<N>>::splat(Doubled::new(
        crate::f32::D_PI.0 * 2.,
        crate::f32::D_PI.1 * 2.,
    ));
    x = a
        .abs()
        .simd_lt(F32x::splat(0.7))
        .select_doubled(Doubled::from(a), x);
    (x, q)
}

/// Integral and fractional value of FP number
pub fn modff<const N: usize>(x: F32x<N>) -> (F32x<N>, F32x<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    let fr = x - x.trunci().cast();
    let fr = x.abs().simd_gt(f1_23x()).select(zero(), fr);
    (fr.copy_sign(x), (x - fr).copy_sign(x))
}

#[inline]
pub(crate) fn visinf2_vf_vf_vf<const N: usize>(d: F32x<N>, m: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    F32x::from_bits(d.is_infinite().to_int().cast() & (d.sign_bit() | m.to_bits()))
}

#[inline]
pub(crate) fn expk2f<const N: usize>(d: Doubled<F32x<N>>) -> Doubled<F32x<N>>
where
    LaneCount<N>: SupportedLaneCount,
{
    let u = F32x::from(d) * r_ln2_f();
    let q = u.roundi();

    let mut s = d + q.cast::<f32>() * (-l2u_f());
    s += q.cast::<f32>() * (-l2l_f());

    let u = F32x::splat(0.198_096_022_4_e-3)
        .mla(s.0, F32x::splat(0.139_425_648_4_e-2))
        .mla(s.0, F32x::splat(0.833_345_670_3_e-2))
        .mla(s.0, F32x::splat(0.416_663_736_1_e-1));

    let mut t = s * u + F32x::splat(0.166_666_659_414_234_244_790_680_580_464);
    t = s * t + half();
    t = s + s.square() * t;

    t = one().add_checked(t);

    t = Doubled::new(ldexp2kf(t.0, q), ldexp2kf(t.1, q));

    t = Doubled::new(
        F32x::from_bits(!d.0.simd_lt(F32x::splat(-104.)).to_int().cast::<u32>() & t.0.to_bits()),
        F32x::from_bits(!d.0.simd_lt(F32x::splat(-104.)).to_int().cast::<u32>() & t.1.to_bits()),
    );

    t
}

/// Absolute value
pub fn fabsf<const N: usize>(x: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    x.abs()
}

/// Copy sign of a number
pub fn copysignf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    x.copy_sign(y)
}

/// Maximum of two numbers
pub fn fmaxf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    if cfg!(target_arch = "x86_64") || cfg!(target_arch = "x86")
    /*    && !cfg!(feature = "enable_vecext")
    && !cfg!(feature = "enable_purec")*/
    {
        y.is_nan().select(x, x.simd_max(y))
    } else {
        y.is_nan().select(x, x.simd_gt(y).select(x, y))
    }
}

/// Minimum of two numbers
pub fn fminf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    if cfg!(target_arch = "x86_64") || cfg!(target_arch = "x86")
    /*    && !cfg!(feature = "enable_vecext")
    && !cfg!(feature = "enable_purec")*/
    {
        y.is_nan().select(x, x.simd_min(y))
    } else {
        y.is_nan().select(x, y.simd_gt(x).select(x, y))
    }
}

/// Positive difference
pub fn fdimf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let ret = x - y;
    (ret.simd_lt(zero()) | x.simd_eq(y)).select(zero(), ret)
}

/// Round to integer towards zero
pub fn truncf<const N: usize>(x: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    /*
    #ifdef FULL_FP_ROUNDING
        return vtruncate_vf_vf(x);
    #else
    */
    let fr = x - x.trunci().cast();
    (x.is_infinite() | x.abs().simd_ge(f1_23x())).select(x, (x - fr).copy_sign(x))
    // #endif
}

/// Round to integer towards minus infinity
pub fn floorf<const N: usize>(x: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let fr = x - x.trunci().cast();
    let fr = fr.simd_lt(zero()).select(fr + one(), fr);
    (x.is_infinite() | x.abs().simd_ge(f1_23x())).select(x, (x - fr).copy_sign(x))
}

/// Round to integer towards plus infinity
pub fn ceilf<const N: usize>(x: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let fr = x - x.trunci().cast();
    let fr = fr.simd_le(zero()).select(fr, fr - one());
    (x.is_infinite() | x.abs().simd_ge(f1_23x())).select(x, (x - fr).copy_sign(x))
}

/// Round to integer away from zero
pub fn roundf<const N: usize>(d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut x = d + half();
    let fr = x - x.trunci().cast();
    x = (x.simd_le(zero()) & fr.simd_eq(zero())).select(x - one(), x);
    let fr = fr.simd_lt(zero()).select(fr + one(), fr);
    x = d
        .simd_eq(F32x::splat(0.499_999_970_197_677_612_3))
        .select(zero(), x);
    (d.is_infinite() | d.abs().simd_ge(f1_23x())).select(d, (x - fr).copy_sign(d))
}

/// Round to integer, ties round to even
pub fn rintf<const N: usize>(d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    /* #ifdef FULL_FP_ROUNDING
        return vrint_vf_vf(d);
    #else */
    let c = f1_23x().mul_sign(d);
    d.abs()
        .simd_gt(f1_23x())
        .select(d, ((d + c) - c).or_sign(d))
    // #endif
}

/// Fused multiply and accumulate
///
/// This function compute (***x*** × ***y*** + ***z***) without rounding, and then return the rounded value of the result.
/// This function may return infinity with a correct sign if the absolute value of the correct return value is greater than `1e+33`.
/// The error bounds of the returned value is `max(0.500_01 ULP, f32::MIN_POSITIVE)`.
pub fn fmaf<const N: usize>(mut x: F32x<N>, mut y: F32x<N>, mut z: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    if cfg!(target_feature = "fma") {
        x.mla(y, z)
    } else {
        let h2 = x * y + z;
        let mut q = one();
        let o = h2.abs().simd_lt(F32x::splat(1e-38));
        let c1 = f1_25x() * f1_25x();
        let c2 = c1 * c1;
        {
            x = o.select(x * c1, x);
            y = o.select(y * c1, y);
            z = o.select(z * c2, z);
            q = o.select(one() / c2, q);
        }
        let o = h2.abs().simd_gt(F32x::splat(1e+38));
        {
            x = o.select(x * (one() / c1), x);
            y = o.select(y * (one() / c1), y);
            z = o.select(z * (one() / c2), z);
            q = o.select(c2, q);
        }
        let d = x.mul_as_doubled(y) + z;
        let ret = (x.simd_eq(zero()) | y.simd_eq(zero())).select(z, F32x::from(d));
        let mut o = z.is_infinite();
        o = !x.is_infinite() & o;
        o = !x.is_nan() & o;
        o = !y.is_infinite() & o;
        o = !y.is_nan() & o;
        let h2 = o.select(z, h2);

        o = h2.is_infinite() | h2.is_nan();

        o.select(h2, ret * q)
    }
}

/// Square root function
///
/// The error bound of the returned value is `0.5001 ULP`
pub fn sqrtf<const N: usize>(d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    //   if cfg!(feature = "accurate_sqrt") {
    d.sqrt()
    /*    } else {
        // fall back to approximation if ACCURATE_SQRT is undefined
        u05::sqrtf(d)
    }*/
}

/// Find the next representable FP value
pub fn nextafterf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let x = x.simd_eq(zero()).select(zero().mul_sign(y), x);
    let mut xi2: I32x<N> = x.to_bits().cast();
    let c = x.is_sign_negative() ^ y.simd_ge(x);

    xi2 = c.select(I32x::splat(0) - (xi2 ^ I32x::splat(i32::MIN)), xi2);

    xi2 = x.simd_ne(y).select(xi2 - I32x::splat(1), xi2);

    xi2 = c.select(I32x::splat(0) - (xi2 ^ I32x::splat(i32::MIN)), xi2);

    let mut ret = F32x::from_bits(xi2.cast());

    ret = (ret.simd_eq(zero()) & x.simd_ne(zero())).select(zero().mul_sign(x), ret);

    ret = (x.simd_eq(zero()) & y.simd_eq(zero())).select(y, ret);

    (x.is_nan() | y.is_nan()).select(nan(), ret)
}

#[test]
fn test_nextafterf() {
    crate::f32x::test_ff_f::<2>(
        nextafterf,
        |mut f, t| {
            let prec = f.prec();
            f.set_prec(24);
            f.next_toward(&t);
            f.set_prec(prec);
            f
        },
        f32::MIN..=f32::MAX,
        f32::MIN..=f32::MAX,
        0.1,
    );
}

/// Fractional component of an FP number
pub fn frfrexpf<const N: usize>(x: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let x = x
        .abs()
        .simd_lt(F32x::splat(f32::MIN_POSITIVE))
        .select(x * f1_32x(), x);

    let mut xm = x.to_bits();
    xm &= U32x::splat(!0x_7f80_0000_u32);
    xm |= U32x::splat(0x_3f00_0000_u32);

    let ret = F32x::from_bits(xm);

    let ret = x.is_infinite().select(infinity().mul_sign(x), ret);
    x.simd_eq(zero()).select(x, ret)
}

/// Exponent of an FP number
pub fn expfrexpf<const N: usize>(_x: F32x<N>) -> I32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    /*
      x = x.abs().simd_lt(F32x::splat(f32::MIN_POSITIVE)).select(x * F1_63X, x);

      let mut ret = I32x::from_cast($ix::from_bits(x);
      ret = (vsrl_vi_vi_i(ret, 20) & $ix::splat(0x7ff)) - $ix::splat(0x3fe);

      (x.simd_eq(zero()) | x.is_nan() | x.is_infinite()).select($ix::splat(0), ret)
    */
    I32x::splat(0)
}

/// FP remainder
pub fn fmodf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn toward0<const N: usize>(x: F32x<N>) -> F32x<N>
    where
        LaneCount<N>: SupportedLaneCount,
    {
        let t = F32x::from_bits((x.to_bits().cast() - I32x::splat(1)).cast());
        x.simd_eq(zero()).select(zero(), t)
    }
    #[inline]
    fn trunc_positive<const N: usize>(x: F32x<N>) -> F32x<N>
    where
        LaneCount<N>: SupportedLaneCount,
    {
        if cfg!(feature = "full_fp_rounding") {
            x.trunc()
        } else {
            let fr = x - x.trunci().cast();
            x.abs().simd_ge(f1_23x()).select(x, x - fr)
        }
    }

    let nu = x.abs();
    let de = y.abs();
    let s = one();
    let o = de.simd_lt(F32x::splat(f32::MIN_POSITIVE));
    let nu = o.select(nu * f1_25x(), nu);
    let de = o.select(de * f1_25x(), de);
    let s = o.select(s * (one() / f1_25x()), s);
    let rde = toward0(de.recip());
    #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
    {
        let rde = toward0(rde);
    }
    let mut r = Doubled::from(nu);

    for _ in 0..8 {
        // ceil(log2(FLT_MAX) / 22)+1
        let mut q = trunc_positive(toward0(r.0) * rde);
        q = ((F32x::splat(3.) * de).simd_gt(r.0) & r.0.simd_ge(de)).select(F32x::splat(2.), q);
        q = ((F32x::splat(2.) * de).simd_gt(r.0) & r.0.simd_ge(de)).select(one(), q);
        r = (r + trunc_positive(q).mul_as_doubled(-de)).normalize();
        if r.0.simd_lt(de).all() {
            break;
        }
    }

    let r = F32x::from(r);
    let mut ret = r * s;
    ret = r.simd_eq(de).select(zero(), ret);

    ret = ret.mul_sign(x);

    ret = nu.simd_lt(de).select(x, ret);
    de.simd_eq(zero()).select(nan(), ret)
}

// TODO: add test for fmodf

#[inline]
pub(crate) fn rintfk2<const N: usize>(d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    /*#ifdef FULL_FP_ROUNDING
        return vrint_vf_vf(d);
    #else*/
    let c = f1_23x().mul_sign(d);
    d.abs()
        .simd_gt(f1_23x())
        .select(d, ((d + c) - c).or_sign(d))
    //#endif
}

/// FP remainder
pub fn remainderf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut n = x.abs();
    let mut d = y.abs();
    let mut s = one();
    let o = d.simd_lt(F32x::splat(f32::MIN_POSITIVE * 2.));
    n = o.select(n * f1_25x(), n);
    d = o.select(d * f1_25x(), d);
    s = o.select(s * F32x::splat(1. / crate::f32::F1_25), s);
    let mut r = Doubled::from(n);
    let rd = d.recip();
    let mut qisodd = M32x::splat(false);

    for _ in 0..8 {
        // ceil(log2(FLT_MAX) / 22)+1
        let mut q = rintfk2(r.0 * rd);
        q =
            r.0.abs()
                .simd_lt(d * F32x::splat(1.5))
                .select(one().mul_sign(r.0), q);
        q = (r.0.abs().simd_lt(d * half()) | (!qisodd & r.0.abs().simd_eq(d * half())))
            .select(zero(), q);
        if q.simd_eq(zero()).all() {
            break;
        }
        q = (q * (-d))
            .is_infinite()
            .select(q + F32x::splat(-1.).mul_sign(r.0), q);
        qisodd ^= (q.trunci() & I32x::splat(1)).simd_eq(I32x::splat(1)) & q.abs().simd_lt(f1_24x());
        r = (r + q.mul_as_doubled(-d)).normalize();
    }

    let mut ret = F32x::from(r) * s;
    ret = ret.mul_sign(x);
    ret = y
        .is_infinite()
        .select(x.is_infinite().select(nan(), x), ret);
    d.simd_eq(zero()).select(nan(), ret)
}

#[test]
fn test_remainderf() {
    crate::f32x::test_ff_f::<2>(
        remainderf,
        rug::Float::remainder,
        f32::MIN..=f32::MAX,
        f32::MIN..=f32::MAX,
        0.5,
    );
}

#[inline]
pub(crate) fn sinpifk<const N: usize>(d: F32x<N>) -> Doubled<F32x<N>>
where
    LaneCount<N>: SupportedLaneCount,
{
    let u = d * F32x::splat(4.);
    let q = u.trunci();
    let q = (q + ((q.cast() >> U32x::splat(31)).cast() ^ I32x::splat(1))) & I32x::splat(!1);
    let o = (q & I32x::splat(2)).simd_eq(I32x::splat(2));

    let s = u - q.cast();
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = o
        .select_splat(-0.243_061_180_1_e-7, 0.309_384_205_4_e-6)
        .mla(s, o.select_splat(0.359_057_708_e-5, -0.365_730_738_8_e-4))
        .mla(s, o.select_splat(-0.325_991_772_1_e-3, 0.249_039_358_5_e-2));
    let mut x = u * s
        + o.select_doubled(
            Doubled::new(
                F32x::splat(0.015_854_343_771_934_509_277),
                F32x::splat(4.494_005_135_403_224_281_1_e-10),
            ),
            Doubled::new(
                F32x::splat(-0.080_745_510_756_969_451_904),
                F32x::splat(-1.337_366_533_907_693_625_8_e-9),
            ),
        );
    x = s2 * x
        + o.select_doubled(
            Doubled::new(
                F32x::splat(-0.308_425_128_459_930_419_92),
                F32x::splat(-9.072_833_903_073_392_227_7_e-9),
            ),
            Doubled::new(
                F32x::splat(0.785_398_185_253_143_310_55),
                F32x::splat(-2.185_733_861_756_648_485_5_e-8),
            ),
        );

    x *= o.select_doubled(s2, Doubled::from(t));
    x = o.select_doubled(x + one(), x);

    let o = (q & I32x::splat(4)).simd_eq(I32x::splat(4));
    x = Doubled::new(
        F32x::from_bits((o.to_int().cast() & neg_zero().to_bits()) ^ x.0.to_bits()),
        F32x::from_bits((o.to_int().cast() & neg_zero().to_bits()) ^ x.1.to_bits()),
    );

    x
}

#[inline]
pub(crate) fn cospifk<const N: usize>(d: F32x<N>) -> Doubled<F32x<N>>
where
    LaneCount<N>: SupportedLaneCount,
{
    let u = d * F32x::splat(4.);
    let q = u.trunci();
    let q = (q + ((q.cast() >> U32x::splat(31)).cast() ^ I32x::splat(1))) & I32x::splat(!1);
    let o = (q & I32x::splat(2)).simd_eq(I32x::splat(0));

    let s = u - q.cast();
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = o
        .select_splat(-0.243_061_180_1_e-7, 0.309_384_205_4_e-6)
        .mla(s, o.select_splat(0.359_057_708_e-5, -0.365_730_738_8_e-4))
        .mla(s, o.select_splat(-0.325_991_772_1_e-3, 0.249_039_358_5_e-2));
    let mut x = u * s
        + o.select_doubled(
            Doubled::new(
                F32x::splat(0.015_854_343_771_934_509_277),
                F32x::splat(4.494_005_135_403_224_281_1_e-10),
            ),
            Doubled::new(
                F32x::splat(-0.080_745_510_756_969_451_904),
                F32x::splat(-1.337_366_533_907_693_625_8_e-9),
            ),
        );
    x = s2 * x
        + o.select_doubled(
            Doubled::new(
                F32x::splat(-0.308_425_128_459_930_419_92),
                F32x::splat(-9.072_833_903_073_392_227_7_e-9),
            ),
            Doubled::new(
                F32x::splat(0.785_398_185_253_143_310_55),
                F32x::splat(-2.185_733_861_756_648_485_5_e-8),
            ),
        );

    x *= o.select_doubled(s2, Doubled::from(t));
    x = o.select_doubled(x + one(), x);

    let o = ((q + I32x::splat(2)) & I32x::splat(4)).simd_eq(I32x::splat(4));
    x = Doubled::new(
        F32x::from_bits((o.to_int().cast() & neg_zero().to_bits()) ^ x.0.to_bits()),
        F32x::from_bits((o.to_int().cast() & neg_zero().to_bits()) ^ x.1.to_bits()),
    );

    x
}

#[inline]
pub(crate) fn expm1fk<const N: usize>(d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let q = (d * r_ln2_f()).roundi();
    let s = q.cast::<f32>().mla(-l2u_f(), d);
    let s = q.cast::<f32>().mla(-l2l_f(), s);

    let s2 = s * s;
    let s4 = s2 * s2;

    let u = F32x::poly6(
        s,
        s2,
        s4,
        0.000_198_527_617_612_853_646_278_381,
        0.001_393_043_552_525_341_510_772_71,
        0.008_333_360_776_305_198_669_433_59,
        0.041_666_485_369_205_474_853_515_6,
        0.166_666_671_633_720_397_949_219,
        0.5,
    );

    let u = (s * s).mla(u, s);

    q.simd_eq(I32x::splat(0))
        .select(u, ldexp2kf(u + one(), q) - one())
}
