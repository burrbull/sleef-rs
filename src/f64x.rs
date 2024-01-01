#[cfg(test)]
mod tst;
#[cfg(test)]
pub(crate) use tst::*;
mod constants;
pub(crate) use constants::*;

use core::simd::prelude::*;

/// Functions with 0.5 ULP error bound
mod u05;
pub use u05::{
    cospi as cospi_u05, hypot as hypot_u05, sincospi as sincospi_u05, sinpi as sinpi_u05,
    sqrt as sqrt_u05,
};

/// Functions with 1.0 ULP error bound
mod u10;
pub use u10::{
    acos as acos_u10, acosh as acosh_u10, asin as asin_u10, asinh as asinh_u10, atan as atan_u10,
    atan2 as atan2_u10, atanh as atanh_u10, cbrt as cbrt_u10, cos as cos_u10,
    cos_deterministic as cos_u10_deterministic, cosh as cosh_u10, erf as erf_u10, exp as exp_u10,
    exp10 as exp10_u10, exp2 as exp2_u10, expm1 as expm1_u10, lgamma as lgamma_u10, log as log_u10,
    log10 as log10_u10, log1p as log1p_u10, log2 as log2_u10, pow as pow_u10, sin as sin_u10,
    sin_deterministic as sin_u10_deterministic, sincos as sincos_u10,
    sincos_deterministic as sincos_u10_deterministic, sinh as sinh_u10, tan as tan_u10,
    tan_deterministic as tan_u10_deterministic, tanh as tanh_u10, tgamma as tgamma_u10,
};

/// Functions with 1.5 ULP error bound
mod u15;
pub use u15::erfc as erfc_u15;

/// Functions with 3.5 ULP error bound
mod u35;
pub use u35::{
    acos as acos_u35, asin as asin_u35, atan as atan_u35, atan2 as atan2_u35, cbrt as cbrt_u35,
    cos as cos_u35, cos_deterministic as cos_u35_deterministic, cosh as cosh_u35,
    exp10 as exp10_u35, exp2 as exp2_u35, hypot as hypot_u35, log as log_u35, log2 as log2_u35,
    sin as sin_u35, sin_deterministic as sin_u35_deterministic, sincos as sincos_u35,
    sincos_deterministic as sincos_u35_deterministic, sincospi as sincospi_u35, sinh as sinh_u35,
    sqrt as sqrt_u35, tan as tan_u35, tan_deterministic as tan_u35_deterministic, tanh as tanh_u35,
};

use crate::common::*;
use core::simd::{
    LaneCount, Mask, Simd, SupportedLaneCount,
};
use doubled::*;

type F64x<const N: usize> = Simd<f64, N>;
type U64x<const N: usize> = Simd<u64, N>;
type I64x<const N: usize> = Simd<i64, N>;
type M64x<const N: usize> = Mask<i64, N>;
type Ux<const N: usize> = Simd<u32, N>;
type Ix<const N: usize> = Simd<i32, N>;

impl<const N: usize> MaskType for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Mask = M64x<N>;
}

impl<const N: usize> BitsType for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Bits = U64x<N>;
}

impl<const N: usize> MaskType for Doubled<F64x<N>>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Mask = M64x<N>;
}

impl<const N: usize> crate::Sleef for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Int = Ix<N>;
    #[inline]
    fn sin(self) -> Self {
        u35::sin(self)
    }
    #[inline]
    fn cos(self) -> Self {
        u35::cos(self)
    }
    #[inline]
    fn sin_cos(self) -> (Self, Self) {
        u35::sincos(self)
    }
    #[inline]
    fn tan(self) -> Self {
        u35::tan(self)
    }
    #[inline]
    fn asin(self) -> Self {
        u35::asin(self)
    }
    #[inline]
    fn acos(self) -> Self {
        u35::acos(self)
    }
    #[inline]
    fn atan(self) -> Self {
        u35::atan(self)
    }
    #[inline]
    fn atan2(self, other: Self) -> Self {
        u35::atan2(self, other)
    }
    #[inline]
    fn ln(self) -> Self {
        u35::log(self)
    }
    #[inline]
    fn cbrt(self) -> Self {
        u35::cbrt(self)
    }
    #[inline]
    fn exp(self) -> Self {
        u10::exp(self)
    }
    #[inline]
    fn pow(self, other: Self) -> Self {
        u10::pow(self, other)
    }
    #[inline]
    fn sinh(self) -> Self {
        u10::sinh(self)
    }
    #[inline]
    fn cosh(self) -> Self {
        u10::cosh(self)
    }
    #[inline]
    fn tanh(self) -> Self {
        u10::tanh(self)
    }
    #[inline]
    fn asinh(self) -> Self {
        u10::asinh(self)
    }
    #[inline]
    fn acosh(self) -> Self {
        u10::acosh(self)
    }
    #[inline]
    fn atanh(self) -> Self {
        u10::atanh(self)
    }
    #[inline]
    fn exp2(self) -> Self {
        u10::exp2(self)
    }
    #[inline]
    fn exp10(self) -> Self {
        u10::exp10(self)
    }
    #[inline]
    fn exp_m1(self) -> Self {
        u10::expm1(self)
    }
    #[inline]
    fn log10(self) -> Self {
        u10::log10(self)
    }
    #[inline]
    fn log2(self) -> Self {
        u10::log2(self)
    }
    #[inline]
    fn log_1p(self) -> Self {
        u10::log1p(self)
    }
    #[inline]
    fn ldexp(self, other: Self::Int) -> Self {
        ldexp(self, other)
    }
    #[inline]
    fn ilogb(self) -> Self::Int {
        ilogb(self)
    }
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
        fma(self, y, z)
    }
    #[inline]
    fn sqrt(self) -> Self {
        sqrt(self)
    }
    #[inline]
    fn abs(self) -> Self {
        fabs(self)
    }
    #[inline]
    fn copy_sign(self, other: Self) -> Self {
        copysign(self, other)
    }
    #[inline]
    fn max(self, other: Self) -> Self {
        fmax(self, other)
    }
    #[inline]
    fn min(self, other: Self) -> Self {
        fmin(self, other)
    }
    #[inline]
    fn fdim(self, other: Self) -> Self {
        fdim(self, other)
    }
    #[inline]
    fn truncate(self) -> Self {
        trunc(self)
    }
    #[inline]
    fn floor(self) -> Self {
        floor(self)
    }
    #[inline]
    fn ceil(self) -> Self {
        ceil(self)
    }
    #[inline]
    fn round(self) -> Self {
        rint(self)
    }
    #[inline]
    fn next_after(self, other: Self) -> Self {
        nextafter(self, other)
    }
    #[inline]
    fn frfrexp(self) -> Self {
        frfrexp(self)
    }
    #[inline]
    fn expfrexp(self) -> Self::Int {
        expfrexp(self)
    }
    #[inline]
    fn fmod(self, other: Self) -> Self {
        fmod(self, other)
    }
    #[inline]
    fn remainder(self, other: Self) -> Self {
        remainder(self, other)
    }
    #[inline]
    fn modf(self) -> (Self, Self) {
        modf(self)
    }
    #[inline]
    fn sin_cos_pi(self) -> (Self, Self) {
        u35::sincospi(self)
    }
    #[inline]
    fn sin_pi(self) -> Self {
        u05::sinpi(self)
    }
    #[inline]
    fn cos_pi(self) -> Self {
        u05::cospi(self)
    }
    #[inline]
    fn hypot(self, other: Self) -> Self {
        u35::hypot(self, other)
    }
    #[inline]
    fn gamma(self) -> Self {
        u10::tgamma(self)
    }
    #[inline]
    fn lgamma(self) -> Self {
        u10::lgamma(self)
    }
    #[inline]
    fn erf(self) -> Self {
        u10::erf(self)
    }
    #[inline]
    fn erfc(self) -> Self {
        u15::erfc(self)
    }
}

#[inline]
fn from_slice_offset<const N: usize>(ptr: &[f64], vi: Ix<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    //F64x::gather_or_default(ptr, vi.cast())
    let ar: [f64; N] = core::array::from_fn(|i| ptr[vi[i] as usize]);
    F64x::from_array(ar)
}

#[inline]
fn swap_upper_lower<const N: usize>(i: I64x<N>) -> I64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    //        i.rotate_left(I64x::splat(32))
    let mut ar = i.to_array();
    for v in &mut ar {
        *v = v.rotate_left(32);
    }
    I64x::from_array(ar)
}

impl<const N: usize> Round for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Int = Ix<N>;
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
        rint(self)
    }
    #[inline]
    fn roundi(self) -> Self::Int {
        self.round().cast()
    }
}

impl<const N: usize> MulAdd for F64x<N>
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

impl<const N: usize> MulSub for F64x<N>
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

impl<const N: usize> NegMulAdd for F64x<N>
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

impl<const N: usize> Sqrt for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn sqrt(self) -> Self {
        use std::simd::StdFloat;
        <Self as StdFloat>::sqrt(self)
    }
}

impl<const N: usize> SqrtAsDoubled for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn sqrt_as_doubled(self) -> Doubled<Self> {
        let t = self.sqrt();
        ((self + t.mul_as_doubled(t)) * t.recip_as_doubled()).scale(Self::splat(0.5))
    }
}

impl<const N: usize> VectorizedSelect<f64> for M64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    type Output = F64x<N>;
    fn select_splat(self, l: f64, r: f64) -> Self::Output {
        self.select(Self::Output::splat(l), Self::Output::splat(r))
    }
}
impl<const N: usize> DoubledSelect<F64x<N>> for M64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    fn select_doubled(self, l: Doubled<F64x<N>>, r: Doubled<F64x<N>>) -> Doubled<F64x<N>> {
        Doubled::new(self.select(l.0, r.0), self.select(l.1, r.1))
    }
}

impl<const N: usize> SelectSeveral<f64> for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn select3(o0: Self::Mask, o1: Self::Mask, d0: f64, d1: f64, d2: f64) -> Self {
        o0.select(Self::splat(d0), o1.select_splat(d1, d2))
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
        o0.select(
            Self::splat(d0),
            o1.select(Self::splat(d1), o2.select_splat(d2, d3)),
        )
    }
}

impl<const N: usize> Poly<f64> for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    fn c2v(c: f64) -> Self {
        F64x::splat(c)
    }
}

impl<const N: usize> Poly<Self> for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    fn c2v(c: Self) -> Self {
        c
    }
}

impl<const N: usize> Sign for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn sign_bit(self) -> Self::Bits {
        self.to_bits() & F64x::NEG_ZERO.to_bits()
    }
    #[inline]
    fn sign(self) -> Self {
        F64x::ONE.mul_sign(self)
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
        Self::from_bits((!F64x::NEG_ZERO.to_bits() & self.to_bits()) ^ other.sign_bit())
    }
}

impl<const N: usize> IsNegZero for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn is_neg_zero(self) -> Self::Mask {
        self.to_bits().simd_eq(F64x::NEG_ZERO.to_bits())
    }
}

impl<const N: usize> IsInt for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn is_integer(self) -> Self::Mask {
        if cfg!(feature = "full_fp_rounding") {
            self.trunc().simd_eq(self)
        } else {
            let mut x = (self * (F64x::ONE / F64x::D1_31)).trunc();
            x = (-F64x::D1_31).mla(x, self);
            x.trunc().simd_eq(x) | self.abs().simd_gt(F64x::D1_53)
        }
    }
}

#[inline]
fn cast_into_upper<const N: usize>(q: Ix<N>) -> I64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let q64 = q.cast();
    q64 << I64x::splat(32)
}

#[inline]
fn cast_from_upper<const N: usize>(q: U64x<N>) -> Ix<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    (q >> U64x::splat(32)).cast()
}

#[inline]
fn pow2i<const N: usize>(q: Ix<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let q = Ix::splat(0x3ff) + q;
    let r = cast_into_upper(q);
    F64x::from_bits((r << I64x::splat(20)).cast())
}
#[inline]
fn ldexp2k<const N: usize>(d: F64x<N>, e: Ix<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let e1 = e >> Ix::splat(1);
    d * pow2i(e1) * pow2i(e - (e1))
}
#[inline]
fn ldexp3k<const N: usize>(d: F64x<N>, q: Ix<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    F64x::from_bits((d.to_bits().cast() + (cast_into_upper(q) << I64x::splat(20))).cast())
}

/*#[cfg(all(
    not(feature = "enable_avx512f"),
    not(feature = "enable_avx512fnofma")
))]*/
#[inline]
fn ilogbk<const N: usize>(mut d: F64x<N>) -> Ix<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let o = d.simd_lt(F64x::splat(4.909_093_465_297_726_6_e-91));
    d = o.select(F64x::splat(2.037_035_976_334_486_e90) * d, d);
    let mut q = cast_from_upper(d.to_bits());
    q &= Ix::splat((((1u32 << 12) - 1) << 20) as _);
    q = (q.cast() >> Ux::splat(20)).cast();
    q - o.cast().select(Ix::splat(300 + 0x3ff), Ix::splat(0x3ff))
}
/*#[cfg(all(
    not(feature = "enable_avx512f"),
    not(feature = "enable_avx512fnofma")
))]*/
#[inline]
fn ilogb2k<const N: usize>(d: F64x<N>) -> Ix<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut q = cast_from_upper(d.to_bits());
    q = (q.cast() >> Ux::splat(20)).cast();
    q &= Ix::splat(0x7ff);
    q - Ix::splat(0x3ff)
}

impl<const N: usize> IsOdd for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn is_odd(self) -> Self::Mask {
        if cfg!(feature = "full_fp_rounding") {
            let x = self * F64x::HALF;
            x.trunc().simd_ne(x)
        } else {
            let mut x = (self * (F64x::ONE / F64x::D1_31)).trunc();
            x = (-F64x::D1_31).mla(x, self);

            (x.trunci() & Ix::splat(1))
                .simd_eq(Ix::splat(1))
                .cast::<i64>()
                & self.abs().simd_lt(F64x::D1_53)
        }
    }
}

#[inline]
fn ldexpk<const N: usize>(x: F64x<N>, q: Ix<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut m = q >> Ix::splat(31);
    m = (((m + q) >> Ix::splat(9)) - m) << Ix::splat(7);
    let q = q - (m << Ix::splat(2));
    m = Ix::splat(0x3ff) + m;
    m = !Ix::splat(0).simd_gt(m).to_int() & m;
    m = m.simd_gt(Ix::splat(0x7ff)).select(Ix::splat(0x7ff), m);
    let r = cast_into_upper(m);
    let y = F64x::from_bits((r << I64x::splat(20)).cast());
    x * y * y * y * y * pow2i(q)
}

/// Multiply by integral power of `2`
///
/// These functions return the result of multiplying ***m*** by `2` raised to the power ***x***.
pub fn ldexp<const N: usize>(x: F64x<N>, q: Ix<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    ldexpk(x, q)
}

/// Integer exponent of an FP number
pub fn ilogb<const N: usize>(d: F64x<N>) -> Ix<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut e = ilogbk(d.abs()).cast::<f64>();
    e = d
        .simd_eq(F64x::ZERO)
        .select(F64x::splat(crate::f64::SLEEF_FP_ILOGB0 as f64), e);
    e = d
        .is_nan()
        .select(F64x::splat(crate::f64::SLEEF_FP_ILOGBNAN as f64), e);
    e = d.is_infinite().select(F64x::splat(f64::MAX), e);
    e.roundi()
}

#[inline]
fn rempisub<const N: usize>(x: F64x<N>) -> (F64x<N>, Ix<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    if cfg!(feature = "full_fp_rounding") {
        let y = (x * F64x::splat(4.)).round();
        let vi = (y - x.round() * F64x::splat(4.)).trunci();
        (x - y * F64x::splat(0.25), vi)
    } else {
        let c = F64x::D1_52.mul_sign(x);
        let rint4x = (F64x::splat(4.) * x).abs().simd_gt(F64x::D1_52).select(
            F64x::splat(4.) * x,
            (F64x::splat(4.).mla(x, c) - c).or_sign(x),
        );
        let rintx = x
            .abs()
            .simd_gt(F64x::D1_52)
            .select(x, ((x + c) - c).or_sign(x));

        let fr = F64x::splat(-0.25).mla(rint4x, x);
        let vi = F64x::splat(-4.).mla(rintx, rint4x).trunci();
        (fr, vi)
    }
}
#[inline]
fn rempi<const N: usize>(mut a: F64x<N>) -> (Doubled<F64x<N>>, Ix<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut ex = ilogb2k(a);
    /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
        ex = !(ex >> 31) & ex;
        ex = ex & Ix::splat(1023);
    }*/
    ex -= Ix::splat(55);
    let mut q = ex.simd_gt(Ix::splat(700 - 55)).to_int() & Ix::splat(-64);
    a = ldexp3k(a, q);
    ex = !(ex >> Ix::splat(31)) & ex;
    ex <<= Ix::splat(2);
    let mut x = a.mul_as_doubled(from_slice_offset(&crate::tables::REMPITABDP, ex));
    let (did, dii) = rempisub(x.0);
    q = dii;
    x.0 = did;
    x = x.normalize();
    let mut y = a.mul_as_doubled(from_slice_offset(&crate::tables::REMPITABDP[1..], ex));
    x += y;
    let (did, dii) = rempisub(x.0);
    q += dii;
    x.0 = did;
    x = x.normalize();
    y = Doubled::new(
        from_slice_offset(&crate::tables::REMPITABDP[2..], ex),
        from_slice_offset(&crate::tables::REMPITABDP[3..], ex),
    );
    y *= a;
    x += y;
    x = x.normalize();
    x *= Doubled::<F64x<N>>::splat(Doubled::new(
        crate::f64::D_PI.0 * 2.,
        crate::f64::D_PI.1 * 2.,
    ));
    let o = a.abs().simd_lt(F64x::splat(0.7));
    x = Doubled::new(
        o.select(a, x.0),
        F64x::from_bits(!o.to_int().cast::<u64>() & x.1.to_bits()),
    );
    (x, q)
}

#[inline]
fn visinf2_vd_vd_vd<const N: usize>(d: F64x<N>, m: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    F64x::from_bits(
        d.is_infinite().to_int().cast() & ((d.to_bits() & F64x::NEG_ZERO.to_bits()) | m.to_bits()),
    )
}

#[inline]
fn expk2<const N: usize>(d: Doubled<F64x<N>>) -> Doubled<F64x<N>>
where
    LaneCount<N>: SupportedLaneCount,
{
    let u = F64x::from(d) * F64x::R_LN2;
    let dq = u.round();
    let q = dq.roundi();

    let s = d + dq * (-F64x::L2_U) + dq * (-F64x::L2_L);

    let s2 = s.square();
    let s4 = s2.square();
    let s8 = s4.0 * s4.0;

    let u = F64x::poly10(
        s.0,
        s2.0,
        s4.0,
        s8,
        0.160_247_221_970_993_207_2_e-9,
        0.209_225_518_356_315_700_7_e-8,
        0.250_523_002_378_264_446_5_e-7,
        0.275_572_480_090_213_530_3_e-6,
        0.275_573_189_238_604_437_3_e-5,
        0.248_015_873_560_581_506_5_e-4,
        0.198_412_698_414_807_185_8_e-3,
        0.138_888_888_888_676_325_5_e-2,
        0.833_333_333_333_334_709_5_e-2,
        0.416_666_666_666_666_990_5_e-1,
    );

    let mut t = F64x::HALF.add_checked(s * F64x::splat(0.166_666_666_666_666_657_4));
    t = F64x::ONE.add_checked(t * s);
    t = F64x::ONE.add_checked(t * s);
    t = t.add_checked(s4 * u);

    t = Doubled::new(ldexp2k(t.0, q), ldexp2k(t.1, q));

    t = Doubled::new(
        F64x::from_bits(!d.0.simd_lt(F64x::splat(-1000.)).to_int().cast::<u64>() & t.0.to_bits()),
        F64x::from_bits(!d.0.simd_lt(F64x::splat(-1000.)).to_int().cast::<u64>() & t.1.to_bits()),
    );
    t
}

/// Absolute value
#[inline]
pub fn fabs<const N: usize>(x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    x.abs()
}

/// Copy sign of a number
#[inline]
pub fn copysign<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    x.copy_sign(y)
}

/// Maximum of two numbers
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))] //  && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
pub fn fmax<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    y.is_nan().select(x, x.simd_max(y))
}

/// Maximum of two numbers
#[cfg(all(not(target_arch = "x86"), not(target_arch = "x86_64")))]
pub fn fmax<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    y.is_nan().select(x, x.simd_gt(y).select(x, y))
}

/// Minimum of two numbers
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))] //  && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
pub fn fmin<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    y.is_nan().select(x, x.simd_min(y))
}

/// Minimum of two numbers
#[cfg(all(not(target_arch = "x86"), not(target_arch = "x86_64")))]
pub fn fmin<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    y.is_nan().select(x, y.simd_gt(x).select(x, y))
}

/// Positive difference
pub fn fdim<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let ret = x - y;
    (ret.simd_lt(F64x::ZERO) | x.simd_eq(y)).select(F64x::ZERO, ret)
}

/// Round to integer towards zero
pub fn trunc<const N: usize>(x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    /*
    #ifdef FULL_FP_ROUNDING
    return vtruncate_vd_vd(x);
    #else
    */
    let mut fr = x - F64x::D1_31 * (x * (F64x::ONE / F64x::D1_31)).trunci().cast();
    fr -= fr.trunci().cast();
    (x.is_infinite() | x.abs().simd_ge(F64x::D1_52)).select(x, (x - fr).copy_sign(x))
    // #endif
}

/// Round to integer towards minus infinity
pub fn floor<const N: usize>(x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut fr = x - F64x::D1_31 * (x * (F64x::ONE / F64x::D1_31)).trunci().cast();
    fr -= fr.trunci().cast();
    fr = fr.simd_lt(F64x::ZERO).select(fr + F64x::ONE, fr);
    (x.is_infinite() | x.abs().simd_ge(F64x::D1_52)).select(x, (x - fr).copy_sign(x))
}

/// Round to integer towards plus infinity
pub fn ceil<const N: usize>(x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut fr = x - F64x::D1_31 * (x * (F64x::ONE / F64x::D1_31)).trunci().cast();
    fr -= fr.trunci().cast();
    fr = fr.simd_le(F64x::ZERO).select(fr, fr - F64x::ONE);
    (x.is_infinite() | x.abs().simd_ge(F64x::D1_52)).select(x, (x - fr).copy_sign(x))
}

/// Round to integer away from zero
pub fn round<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut x = d + F64x::HALF;
    let mut fr = x - F64x::D1_31 * (x * (F64x::ONE / F64x::D1_31)).trunci().cast();
    fr -= fr.trunci().cast();
    x = (x.simd_le(F64x::ZERO) & fr.simd_eq(F64x::ZERO)).select(x - F64x::ONE, x);
    fr = fr.simd_lt(F64x::ZERO).select(fr + F64x::ONE, fr);
    x = d
        .simd_eq(F64x::splat(0.499_999_999_999_999_944_49))
        .select(F64x::ZERO, x);
    (d.is_infinite() | d.abs().simd_ge(F64x::D1_52)).select(d, (x - fr).copy_sign(d))
}

/// Round to integer, ties round to even
pub fn rint<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    /*
    #ifdef FULL_FP_ROUNDING
    return vrint_vd_vd(d);
    #else
    */
    let c = F64x::D1_52.mul_sign(d);
    d.abs()
        .simd_gt(F64x::D1_52)
        .select(d, ((d + c) - c).or_sign(d))
    //#endif
}

/// Find the next representable FP value
pub fn nextafter<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let x = x.simd_eq(F64x::ZERO).select(F64x::ZERO.mul_sign(y), x);
    let mut xi2 = x.to_bits().cast::<i64>();
    let c = x.is_sign_negative() ^ y.simd_ge(x);

    let mut t = (xi2 ^ I64x::splat(0x_7fff_ffff_ffff_ffff_u64 as _)) + I64x::splat(1);
    t += swap_upper_lower(
        I64x::splat(1)
            & t.simd_eq(I64x::splat(0x_ffff_ffff_0000_0000_u64 as _))
                .to_int(),
    );
    xi2 = c
        .select(F64x::from_bits(t.cast()), F64x::from_bits(xi2.cast()))
        .to_bits()
        .cast();

    xi2 -= (x.simd_ne(y).to_int().cast() & U64x::splat(1)).cast();

    xi2 = x
        .simd_ne(y)
        .select(
            F64x::from_bits(
                (xi2 + swap_upper_lower(
                    I64x::splat(0x_ffff_ffff_u64 as _)
                        & xi2.simd_eq(I64x::splat(0x_ffff_ffff_u64 as _)).to_int(),
                ))
                .cast(),
            ),
            F64x::from_bits(xi2.cast()),
        )
        .to_bits()
        .cast();

    let mut t = (xi2 ^ I64x::splat(0x_7fff_ffff_ffff_ffff_u64 as _)) + I64x::splat(1);
    t += swap_upper_lower(
        I64x::splat(1)
            & t.simd_eq(I64x::splat(0x_ffff_ffff_0000_0000_u64 as _))
                .to_int(),
    );
    xi2 = c
        .select(F64x::from_bits(t.cast()), F64x::from_bits(xi2.cast()))
        .to_bits()
        .cast();

    let mut ret = F64x::from_bits(xi2.cast());

    ret = (ret.simd_eq(F64x::ZERO) & x.simd_ne(F64x::ZERO)).select(F64x::ZERO.mul_sign(x), ret);

    ret = (x.simd_eq(F64x::ZERO) & y.simd_eq(F64x::ZERO)).select(y, ret);

    (x.is_nan() | y.is_nan()).select(F64x::NAN, ret)
}

#[test]
fn test_nextafter() {
    test_ff_f::<2>(
        nextafter,
        |mut f, t| {
            let prec = f.prec();
            f.set_prec(53);
            f.next_toward(&t);
            f.set_prec(prec);
            f
        },
        f64::MIN..=f64::MAX,
        f64::MIN..=f64::MAX,
        0.1,
    );
}

/// Fractional component of an FP number
pub fn frfrexp<const N: usize>(x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let x = x
        .abs()
        .simd_lt(F64x::splat(f64::MIN_POSITIVE))
        .select(x * F64x::D1_63, x);

    let mut xm = x.to_bits();
    xm &= U64x::splat(0x_800f_ffff_ffff_ffff);
    xm |= U64x::splat(0x_3fe0_0000 << 32);

    let ret = F64x::from_bits(xm);

    let ret = x.is_infinite().select(F64x::INFINITY.mul_sign(x), ret);
    x.simd_eq(F64x::ZERO).select(x, ret)
}

/// Exponent of an FP number
pub fn expfrexp<const N: usize>(x: F64x<N>) -> Ix<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let x = x
        .abs()
        .simd_lt(F64x::splat(f64::MIN_POSITIVE))
        .select(x * F64x::D1_63, x);

    let mut ret = cast_from_upper(x.to_bits());
    ret = ((ret.cast() >> Ux::splat(20)).cast() & Ix::splat(0x7ff)) - Ix::splat(0x3fe);

    (x.simd_eq(F64x::ZERO) | x.is_nan() | x.is_infinite())
        .cast()
        .select(Ix::splat(0), ret)
}

/// Fused multiply and accumulate
///
/// This function compute (***x*** × ***y*** + ***z***) without rounding, and then return the rounded value of the result.
/// This function may return infinity with a correct sign if the absolute value of the correct return value is greater than `1e+300`.
/// The error bounds of the returned value is `max(0.500_01 ULP, f64::MIN_POSITIVE)`.
pub fn fma<const N: usize>(mut x: F64x<N>, mut y: F64x<N>, mut z: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    if cfg!(target_feature = "fma") {
        x.mla(y, z)
    } else {
        let mut h2 = x * y + z;
        let mut q = F64x::ONE;
        let c1 = F64x::D1_54 * F64x::D1_54;
        let c2 = c1 * c1;
        let o = h2.abs().simd_lt(F64x::splat(1e-300));
        {
            x = o.select(x * c1, x);
            y = o.select(y * c1, y);
            z = o.select(z * c2, z);
            q = o.select(F64x::ONE / c2, q);
        }
        let o = h2.abs().simd_gt(F64x::splat(1e+300));
        {
            x = o.select(x * (F64x::ONE / c1), x);
            y = o.select(y * (F64x::ONE / c1), y);
            z = o.select(z * (F64x::ONE / c2), z);
            q = o.select(c2, q);
        }
        let d = x.mul_as_doubled(y) + z;
        let ret = (x.simd_eq(F64x::ZERO) | y.simd_eq(F64x::ZERO)).select(z, d.0 + d.1);
        let mut o = z.is_infinite();
        o = !x.is_infinite() & o;
        o = !x.is_nan() & o;
        o = !y.is_infinite() & o;
        o = !y.is_nan() & o;
        h2 = o.select(z, h2);

        let o = h2.is_infinite() | h2.is_nan();

        o.select(h2, ret * q)
    }
}

/// Square root function
///
/// The error bound of the returned value is `0.5001 ULP`
//#[cfg(feature = "accurate_sqrt")]
pub fn sqrt<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    d.sqrt()
}
// fall back to approximation if ACCURATE_SQRT is undefined
/*#[cfg(not(feature = "accurate_sqrt"))]
pub fn xsqrt<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    u05::sqrt(d)
}*/

/// FP remainder
/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn fmod<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    #[inline]
    fn toward0<const N: usize>(x: F64x<N>) -> F64x<N>
    where
        LaneCount<N>: SupportedLaneCount,
    {
        // returns nextafter(x, 0)
        let t = F64x::from_bits(x.to_bits() + I64x::splat(-1).cast());
        x.simd_eq(F64x::ZERO).select(F64x::ZERO, t)
    }

    #[cfg(feature = "full_fp_rounding")]
    #[inline]
    fn trunc_positive<const N: usize>(x: F64x<N>) -> F64x<N>
    where
        LaneCount<N>: SupportedLaneCount,
    {
        // round to integer toward 0, positive argument only
        x.trunc()
    }
    #[cfg(not(feature = "full_fp_rounding"))]
    #[inline]
    fn trunc_positive<const N: usize>(x: F64x<N>) -> F64x<N>
    where
        LaneCount<N>: SupportedLaneCount,
    {
        let mut fr = (-F64x::D1_31).mla((x * (F64x::ONE / F64x::D1_31)).trunci().cast(), x);
        fr -= fr.trunci().cast();
        x.abs().simd_ge(F64x::D1_52).select(x, x - fr)
    }

    let n = x.abs();
    let d = y.abs();
    let s = F64x::ONE;
    let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE));
    let n = o.select(n * F64x::D1_54, n);
    let d = o.select(d * F64x::D1_54, d);
    let s = o.select(s * (F64x::ONE / F64x::D1_54), s);
    let rd = toward0(d.recip());
    let mut r = Doubled::from(n);

    for _ in 0..21 {
        // ceil(log2(DBL_MAX) / 52)
        let mut q = trunc_positive(toward0(r.0) * rd);
        if cfg!(target_feature = "fma") {
            q = F64x::from_bits(q.to_bits() & U64x::splat(0xffff_ffff_ffff_fffe));
        }
        q = ((F64x::splat(3.) * d).simd_gt(r.0) & r.0.simd_ge(d)).select(F64x::splat(2.), q);
        q = ((d + d).simd_gt(r.0) & r.0.simd_ge(d)).select(F64x::ONE, q);
        r = (r + q.mul_as_doubled(-d)).normalize();
        if r.0.simd_lt(d).all() {
            break;
        }
    }

    let mut ret = r.0 * s;
    ret = F64x::from(r).simd_eq(d).select(F64x::ZERO, ret);

    ret = ret.mul_sign(x);

    ret = n.simd_lt(d).select(x, ret);
    d.simd_eq(F64x::ZERO).select(F64x::NAN, ret)
}

// TODO: add test for fmodf

#[inline]
fn rintk2<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    if cfg!(feature = "full_fp_rounding") {
        rint(d)
    } else {
        let c = F64x::D1_52.mul_sign(d);
        d.abs()
            .simd_gt(F64x::D1_52)
            .select(d, ((d + c) - c).or_sign(d))
    }
}

/// FP remainder
pub fn remainder<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut n = x.abs();
    let mut d = y.abs();
    let mut s = F64x::ONE;
    let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE * 2.));
    n = o.select(n * F64x::D1_54, n);
    d = o.select(d * F64x::D1_54, d);
    s = o.select(s * F64x::splat(1. / crate::f64::D1_54), s);
    let rd = d.recip();
    let mut r = Doubled::from(n);
    let mut qisodd = M64x::splat(false);

    for _ in 0..21 {
        // ceil(log2(DBL_MAX) / 52)
        let mut q = rintk2(r.0 * rd);
        if cfg!(target_feature = "fma") {
            q = F64x::from_bits(q.to_bits() & U64x::splat(0xffff_ffff_ffff_fffe));
        }
        q =
            r.0.abs()
                .simd_lt(d * F64x::splat(1.5))
                .select(F64x::ONE.mul_sign(r.0), q);
        q = (r.0.abs().simd_lt(d * F64x::HALF) | (!qisodd & r.0.abs().simd_eq(d * F64x::HALF)))
            .select(F64x::ZERO, q);
        if q.simd_eq(F64x::ZERO).all() {
            break;
        }
        q = (q * (-d))
            .is_infinite()
            .select(q + F64x::splat(-1.).mul_sign(r.0), q);
        qisodd ^= q.is_odd();
        r = (r + q.mul_as_doubled(-d)).normalize();
    }

    let mut ret = r.0 * s;
    ret = ret.mul_sign(x);
    ret = y
        .is_infinite()
        .select(x.is_infinite().select(F64x::NAN, x), ret);
    d.simd_eq(F64x::ZERO).select(F64x::NAN, ret)
}

#[test]
fn test_remainder() {
    test_ff_f::<2>(
        remainder,
        rug::Float::remainder,
        f64::MIN..=f64::MAX,
        f64::MIN..=f64::MAX,
        0.5,
    );
}

#[inline]
fn sinpik<const N: usize>(d: F64x<N>) -> Doubled<F64x<N>>
where
    LaneCount<N>: SupportedLaneCount,
{
    let u = d * F64x::splat(4.);
    let mut q = u.trunci();
    q = (q + ((q.cast() >> Ux::splat(31)).cast() ^ Ix::splat(1))) & Ix::splat(!1);
    let o = (q & Ix::splat(2)).simd_eq(Ix::splat(2)).cast::<i64>();

    let s = u - q.cast();
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = o
        .select_splat(
            9.944_803_876_268_437_740_902_08_e-16,
            -2.024_611_207_851_823_992_958_68_e-14,
        )
        .mla(
            s,
            o.select_splat(
                -3.897_962_260_629_327_991_640_47_e-13,
                6.948_218_305_801_794_613_277_84_e-12,
            ),
        )
        .mla(
            s,
            o.select_splat(
                1.150_115_825_399_960_352_669_01_e-10,
                -1.757_247_499_528_531_799_526_64_e-9,
            ),
        )
        .mla(
            s,
            o.select_splat(
                -2.461_136_950_104_469_749_535_9_e-8,
                3.133_616_889_668_683_928_784_22_e-7,
            ),
        )
        .mla(
            s,
            o.select_splat(
                3.590_860_448_590_527_540_050_62_e-6,
                -3.657_620_418_216_155_192_036_1_e-5,
            ),
        )
        .mla(
            s,
            o.select_splat(
                -0.000_325_991_886_927_389_905_997_954,
                0.002_490_394_570_192_718_502_743_56,
            ),
        );
    let mut x = u * s
        + o.select_doubled(
            Doubled::new(
                F64x::splat(0.015_854_344_243_815_501_891_425_9),
                F64x::splat(-1.046_932_722_806_315_219_088_45_e-18),
            ),
            Doubled::new(
                F64x::splat(-0.080_745_512_188_280_785_248_473_1),
                F64x::splat(3.618_524_750_670_371_048_499_87_e-18),
            ),
        );
    x = s2 * x
        + o.select_doubled(
            Doubled::new(
                F64x::splat(-0.308_425_137_534_042_437_259_529),
                F64x::splat(-1.956_984_921_336_335_503_383_45_e-17),
            ),
            Doubled::new(
                F64x::splat(0.785_398_163_397_448_278_999_491),
                F64x::splat(3.062_871_137_271_550_026_071_05_e-17),
            ),
        );

    x *= o.select_doubled(s2, Doubled::from(t));
    x = o.select_doubled(x + F64x::ONE, x);

    let o = (q & Ix::splat(4)).simd_eq(Ix::splat(4)).cast::<i64>();
    x.0 = F64x::from_bits((o.to_int().cast() & F64x::NEG_ZERO.to_bits()) ^ x.0.to_bits());
    x.1 = F64x::from_bits((o.to_int().cast() & F64x::NEG_ZERO.to_bits()) ^ x.1.to_bits());

    x
}

/// Integral and fractional value of FP number
pub fn modf<const N: usize>(x: F64x<N>) -> (F64x<N>, F64x<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut fr = x - F64x::D1_31 * (x * (F64x::ONE / F64x::D1_31)).trunci().cast();
    fr -= fr.trunci().cast();
    fr = x.abs().simd_gt(F64x::D1_52).select(F64x::ZERO, fr);

    (fr.copy_sign(x), (x - fr).copy_sign(x))
}
