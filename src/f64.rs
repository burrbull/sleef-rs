#[cfg(test)]
mod tst;
#[cfg(test)]
pub(crate) use tst::*;

use core::f64::consts::{FRAC_1_PI, FRAC_2_PI, FRAC_PI_2, FRAC_PI_4, PI};

use crate::common::*;
use doubled::*;

pub(crate) const D1_63: f64 = (1u64 << 63) as f64;
pub(crate) const D1_60: f64 = (1u64 << 60) as f64;
pub(crate) const D1_54: f64 = (1u64 << 54) as f64;
pub(crate) const D1_53: f64 = (1u64 << 53) as f64;
pub(crate) const D1_52: f64 = (1u64 << 52) as f64;
pub(crate) const D1_32: f64 = (1u64 << 32) as f64;
pub(crate) const D1_31: f64 = (1u64 << 31) as f64;
pub(crate) const D1_28: f64 = (1u64 << 28) as f64;
pub(crate) const D1_24: f64 = (1u64 << 24) as f64;
pub(crate) const D1_23: f64 = (1u64 << 23) as f64;

pub(crate) const SLEEF_FP_ILOGB0: i32 = -2_147_483_648;
pub(crate) const SLEEF_FP_ILOGBNAN: i32 = 2_147_483_647;
pub(crate) const SQRT_DBL_MAX: f64 = 1.340_780_792_994_259_635_5_e+154;
pub(crate) const TRIGRANGEMAX3: f64 = 1e+9;
pub(crate) const L2_U: f64 = 0.693_147_180_559_662_956_511_601_805_686_950_683_593_75;
pub(crate) const L2_L: f64 = 0.282_352_905_630_315_771_225_884_481_750_134_360_255_254_120_68_e-12;
pub(crate) const R_LN2: f64 =
    1.442_695_040_888_963_407_359_924_681_001_892_137_426_645_954_152_985_934_135_449_406_931;
pub(crate) const L10_U: f64 = 0.301_029_995_663_839_144_98;
pub(crate) const L10_L: f64 = 1.420_502_322_726_609_941_8_e-13; // log 2 / log 10
pub(crate) const LOG10_2: f64 = 3.321_928_094_887_362_347_870_319_429_489_390_175_864_831_393;

pub(crate) const M_2_PI: Doubled<f64> = Doubled::new(
    0.636_619_772_367_581_382_43,
    -3.935_735_335_036_497_176_4_e-17,
);
/*
 PI_A to PI_D are constants that satisfy the following two conditions.

 * For PI_A, PI_B and PI_C, the last 28 bits are zero.
 * PI_A + PI_B + PI_C + PI_D is close to PI as much as possible.

 The argument of a trig function is multiplied by 1/PI, and the
 integral part is divided into two parts, each has at most 28
 bits. So, the maximum argument that could be correctly reduced
 should be 2^(28*2-1) PI = 1.1e+17. However, due to internal
 double precision calculation, the actual maximum argument that can
 be correctly reduced is around 2^47.
*/
pub(crate) const PI_A: f64 = 3.141_592_621_803_283_691_4;
pub(crate) const PI_B: f64 = 3.178_650_942_459_171_346_9_e-8;
pub(crate) const PI_C: f64 = 1.224_646_786_410_718_850_2_e-16;
pub(crate) const PI_D: f64 = 1.273_663_432_702_189_981_6_e-24;
pub(crate) const TRIGRANGEMAX: f64 = 1e+14;

/*
 PI_A2 and PI_B2 are constants that satisfy the following two conditions.

 * The last 3 bits of PI_A2 are zero.
 * PI_A2 + PI_B2 is close to PI as much as possible.

 The argument of a trig function is multiplied by 1/PI, and the
 integral part is multiplied by PI_A2. So, the maximum argument that
 could be correctly reduced should be 2^(3-1) PI = 12.6. By testing,
 we confirmed that it correctly reduces the argument up to around 15.
*/
pub(crate) const PI_A2: f64 = 3.141_592_653_589_793_116;
pub(crate) const PI_B2: f64 = 1.224_646_799_147_353_207_2_e-16;

pub(crate) const D_PI: Doubled<f64> =
    Doubled::new(3.141_592_653_589_793_116, 1.224_646_799_147_353_207_2_e-16);
pub(crate) const D_LN2: Doubled<f64> = Doubled::new(
    0.693_147_180_559_945_286_226_764,
    2.319_046_813_846_299_558_417_771_e-17,
);

pub(crate) const TRIGRANGEMAX2: f64 = 15.;

mod u05;
#[rustfmt::skip]
pub use u05::{
    sincospi as sincospi_u05,
    sqrt as sqrt_u05,
    hypot as hypot_u05,
    sinpi as sinpi_u05,
    cospi as cospi_u05,
};
mod u10;
#[rustfmt::skip]
pub use u10::{
    sin as sin_u10,
    cos as cos_u10,
    sincos as sincos_u10,
    tan as tan_u10,
    atan2 as atan2_u10,
    asin as asin_u10,
    acos as acos_u10,
    atan as atan_u10,
    exp as exp_u10,
    cbrt as cbrt_u10,
    log as log_u10,
    pow as pow_u10,
    sinh as sinh_u10,
    cosh as cosh_u10,
    tanh as tanh_u10,
    asinh as asinh_u10,
    acosh as acosh_u10,
    atanh as atanh_u10,
    exp10 as exp10_u10,
    expm1 as expm1_u10,
    log10 as log10_u10,
    log2 as log2_u10,
    tgamma as tgamma_u10,
    lgamma as lgamma_u10,
    erf as erf_u10,
    log1p as log1p_u10,
    exp2 as exp2_u10,
};

mod u15;
pub use u15::erfc as erfc_u15;
mod u35;
#[rustfmt::skip]
pub use u35::{
    sin as sin_u35,
    cos as cos_u35,
    tan as tan_u35,
    sincos as sincos_u35,
    sincospi as sincospi_u35,
    atan as atan_u35,
    atan2 as atan2_u35,
    asin as asin_u35,
    acos as acos_u35,
    log as log_u35,
    sqrt as sqrt_u35,
    cbrt as cbrt_u35,
    sinh as sinh_u35,
    cosh as cosh_u35,
    tanh as tanh_u35,
    hypot as hypot_u35,
    exp2 as exp2_u35,
    exp10 as exp10_u35,
    log2 as log2_u35,
};

impl crate::Sleef for f64 {
    type Int = i32;
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
        u35::sqrt(self)
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

impl MaskType for f64 {
    type Mask = bool;
}

impl BitsType for f64 {
    type Bits = u64;
}

impl MulAdd for f64 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        if cfg!(target_feature = "fma") {
            self.mul_add(y, z)
        } else {
            self * y + z
        }
    }
}

impl Poly<f64> for f64 {
    fn c2v(c: f64) -> Self {
        c
    }
}

impl Sign for f64 {
    #[inline]
    fn sign_bit(self) -> Self::Bits {
        self.to_bits() & (1 << 63)
    }
    #[inline]
    fn sign(self) -> Self {
        mulsign(1., self)
    }
    #[inline]
    fn mul_sign(self, other: Self) -> Self {
        mulsign(self, other)
    }
    #[inline]
    fn or_sign(self, other: Self) -> Self {
        Self::from_bits(self.to_bits() | other.sign_bit())
    }
    #[inline]
    fn copy_sign(self, other: Self) -> Self {
        copysignk(self, other)
    }
}

impl IsNegZero for f64 {
    #[inline]
    fn is_neg_zero(self) -> Self::Mask {
        self.to_bits() == (-0_f64).to_bits()
    }
}

impl IsInt for f64 {
    #[inline]
    fn is_integer(self) -> Self::Mask {
        let x = self - D1_31 * ((self * (1. / D1_31)) as i64 as f64);
        (x == x as i64 as f64) || (fabsk(self) >= D1_53)
    }
}

impl IsOdd for f64 {
    #[inline]
    fn is_odd(self) -> Self::Mask {
        let x = self - D1_31 * ((self * (1. / D1_31)) as i64 as f64);
        ((1 & (x as i64)) != 0) && (fabsk(self) < D1_53)
    }
}

/// Multiply left value with sign of right value
#[inline]
pub fn mulsign(x: f64, y: f64) -> f64 {
    f64::from_bits(x.to_bits() ^ (y.to_bits() & (1 << 63)))
}

#[inline]
fn copysignk(x: f64, y: f64) -> f64 {
    f64::from_bits((x.to_bits() & !(1 << 63)) ^ (y.to_bits() & (1 << 63)))
}

/// Sign of a number
#[inline]
pub fn sign(d: f64) -> f64 {
    mulsign(1., d)
}

#[inline]
fn fabsk(x: f64) -> f64 {
    f64::from_bits(0x_7fff_ffff_ffff_ffff & x.to_bits())
}

#[inline]
fn rintk(x: f64) -> f64 {
    (if x < 0. { x - 0.5 } else { x + 0.5 }) as isize as f64
}

#[inline]
fn ceilk(x: f64) -> isize {
    (x as isize) + (if x < 0. { 0 } else { 1 })
}

#[inline]
fn trunck(x: f64) -> f64 {
    x as isize as f64
}

#[inline]
fn pow2i(q: i32) -> f64 {
    f64::from_bits(((q + 0x3ff) as u64) << 52)
}

#[inline]
fn ldexp2k(d: f64, e: i32) -> f64 {
    // faster than ldexpk, short reach
    d * pow2i(e >> 1) * pow2i(e - (e >> 1))
}

#[inline]
fn ldexp3k(d: f64, e: i32) -> f64 {
    // very fast, no denormal
    f64::from_bits(((d.to_bits() as i64) + ((e as i64) << 52)) as u64)
}

#[inline]
fn ilogbk(mut d: f64) -> i32 {
    let m = d < 4.909_093_465_297_726_6_e-91;
    d = if m { 2.037_035_976_334_486_e90 * d } else { d };
    let q = ((d.to_bits() >> 52) & 0x7ff) as i32;
    if m {
        q - (300 + 0x03ff)
    } else {
        q - 0x03ff
    }
}

// ilogb2k is similar to ilogbk, but the argument has to be a
// normalized FP value.
#[inline]
fn ilogb2k(d: f64) -> i32 {
    (((d.to_bits() >> 52) & 0x7ff) as i32) - 0x3ff
}

fn rempisub(x: f64) -> (f64, i32) {
    // This function is equivalent to :
    // ( x - rint(4 * x) * 0.25, (rint(4 * x) - rint(x) * 4) as i32 );
    let c = D1_52.mul_sign(x);
    let rint4x = if fabsk(4.0 * x) > D1_52 {
        4.0 * x
    } else {
        (4.0.mla(x, c) - c).or_sign(x)
    };
    let rintx = if fabsk(x) > D1_52 {
        x
    } else {
        (x + c - c).or_sign(x)
    };
    let retd = (-0.25).mla(rint4x, x);
    let reti = (-4_f64).mla(rintx, rint4x) as i32;
    (retd, reti)
}

// Payne-Hanek like argument reduction
fn rempi(a: f64) -> (Doubled<f64>, i32) {
    let mut ex = ilogb2k(a) - 55;
    let q = if ex > (700 - 55) { -64 } else { 0 };
    let a = ldexp3k(a, q);
    if ex < 0 {
        ex = 0;
    }
    let ex = (ex * 4) as usize;
    let mut x = a.mul_as_doubled(crate::tables::REMPITABDP[ex]);
    let (did, dii) = rempisub(x.0);
    let mut q = dii;
    x.0 = did;
    x = x.normalize();
    let mut y = a.mul_as_doubled(crate::tables::REMPITABDP[ex + 1]);
    x += y;
    let (did, dii) = rempisub(x.0);
    q += dii;
    x.0 = did;
    x = x.normalize();
    y = Doubled::new(
        crate::tables::REMPITABDP[ex + 2],
        crate::tables::REMPITABDP[ex + 3],
    ) * a;
    x += y;
    x = x.normalize() * Doubled::new(D_PI.0 * 2., D_PI.1 * 2.);
    (if fabsk(a) < 0.7 { Doubled::from(a) } else { x }, q)
}

#[inline]
fn sinpik(d: f64) -> Doubled<f64> {
    let u = d * 4.;
    let q = ceilk(u) & !1;
    let o = (q & 2) != 0;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (if o {
        9.944_803_876_268_437_740_902_08_e-16_f64
    } else {
        -2.024_611_207_851_823_992_958_68_e-14
    })
    .mla(
        s,
        if o {
            -3.897_962_260_629_327_991_640_47_e-13
        } else {
            6.948_218_305_801_794_613_277_84_e-12
        },
    )
    .mla(
        s,
        if o {
            1.150_115_825_399_960_352_669_01_e-10
        } else {
            -1.757_247_499_528_531_799_526_64_e-9
        },
    )
    .mla(
        s,
        if o {
            -2.461_136_950_104_469_749_535_9_e-8
        } else {
            3.133_616_889_668_683_928_784_22_e-7
        },
    )
    .mla(
        s,
        if o {
            3.590_860_448_590_527_540_050_62_e-6
        } else {
            -3.657_620_418_216_155_192_036_1_e-5
        },
    )
    .mla(
        s,
        if o {
            -0.000_325_991_886_927_389_905_997_954
        } else {
            0.002_490_394_570_192_718_502_743_56
        },
    );
    let mut x = u * s
        + (if o {
            Doubled::new(
                0.015_854_344_243_815_501_891_425_9,
                -1.046_932_722_806_315_219_088_45_e-18,
            )
        } else {
            Doubled::new(
                -0.080_745_512_188_280_785_248_473_1,
                3.618_524_750_670_371_048_499_87_e-18,
            )
        });
    x = s2 * x
        + (if o {
            Doubled::new(
                -0.308_425_137_534_042_437_259_529,
                -1.956_984_921_336_335_503_383_45_e-17,
            )
        } else {
            Doubled::new(
                0.785_398_163_397_448_278_999_491,
                3.062_871_137_271_550_026_071_05_e-17,
            )
        });

    x *= if o { s2 } else { Doubled::from(t) };
    x = if o { x + 1. } else { x };

    //

    if (q & 4) != 0 {
        x = -x;
    }

    x
}

#[inline]
fn expk2(d: Doubled<f64>) -> Doubled<f64> {
    let qf = rintk(f64::from(d) * R_LN2);
    let q = qf as i32;

    let s = d + qf * (-L2_U) + qf * (-L2_L);

    let u = 0.160_247_221_970_993_207_2_e-9_f64
        .mla(s.0, 0.209_225_518_356_315_700_7_e-8)
        .mla(s.0, 0.250_523_002_378_264_446_5_e-7)
        .mla(s.0, 0.275_572_480_090_213_530_3_e-6)
        .mla(s.0, 0.275_573_189_238_604_437_3_e-5)
        .mla(s.0, 0.248_015_873_560_581_506_5_e-4)
        .mla(s.0, 0.198_412_698_414_807_185_8_e-3)
        .mla(s.0, 0.138_888_888_888_676_325_5_e-2)
        .mla(s.0, 0.833_333_333_333_334_709_5_e-2)
        .mla(s.0, 0.416_666_666_666_666_990_5_e-1);

    let mut t = s * u + 0.166_666_666_666_666_657_4;
    t = s * t + 0.5;
    t = s + s.square() * t;

    t = 1. + t;

    t = Doubled::new(ldexp2k(t.0, q), ldexp2k(t.1, q));

    if d.0 < -1000. {
        Doubled::from(0.)
    } else {
        t
    }
}

#[inline]
fn ldexpk(x: f64, mut q: i32) -> f64 {
    let mut m = q >> 31;
    m = (((m + q) >> 9) - m) << 7;
    q -= m << 2;
    m += 0x3ff;
    m = if m < 0 { 0 } else { m };
    m = if m > 0x7ff { 0x7ff } else { m };
    let u = f64::from_bits((m as u64) << 52);
    let x = x * u * u * u * u;
    let u = f64::from_bits(((q + 0x3ff) as u64) << 52);
    x * u
}

/// Multiply by integral power of `2`
///
/// These functions return the result of multiplying ***m*** by `2` raised to the power ***x***.
pub fn ldexp(x: f64, mut exp: i32) -> f64 {
    if exp > 2100 {
        exp = 2100;
    }
    if exp < -2100 {
        exp = -2100;
    }

    let mut e0 = exp >> 2;
    if exp < 0 {
        e0 += 1;
    }
    if (-100 < exp) && (exp < 100) {
        e0 = 0;
    }
    let e1 = exp - (e0 << 2);

    let p = pow2i(e0);
    x * pow2i(e1) * p * p * p * p
}

/// Integer exponent of an FP number
pub fn ilogb(d: f64) -> i32 {
    let mut e = ilogbk(fabsk(d));
    e = if d == 0. { SLEEF_FP_ILOGB0 } else { e };
    e = if d.is_nan() { SLEEF_FP_ILOGBNAN } else { e };
    if d.is_infinite() {
        i32::MAX
    } else {
        e
    }
}

/// Fused multiply and accumulate
///
/// This function compute (***x*** × ***y*** + ***z***) without rounding, and then return the rounded value of the result.
/// This function may return infinity with a correct sign if the absolute value of the correct return value is greater than `1e+300`.
/// The error bounds of the returned value is `max(0.500_01 ULP, f64::MIN_POSITIVE)`.
pub fn fma(mut x: f64, mut y: f64, mut z: f64) -> f64 {
    let mut h2 = x * y + z;
    const C0: f64 = D1_54;
    const C1: f64 = C0 * C0;
    const C2: f64 = C1 * C1;

    let q = if fabsk(h2) < 1e-300 {
        x *= C1;
        y *= C1;
        z *= C2;
        1. / C2
    } else if fabsk(h2) > 1e+299 {
        x *= 1. / C1;
        y *= 1. / C1;
        z *= 1. / C2;
        C2
    } else {
        1.
    };

    let d = x.mul_as_doubled(y) + z;
    let ret = if (x == 0.) || (y == 0.) {
        z
    } else {
        f64::from(d)
    };
    if z.is_infinite() && !x.is_infinite() && !x.is_nan() && !y.is_infinite() && !y.is_nan() {
        h2 = z;
    }
    if h2.is_infinite() || h2.is_nan() {
        h2
    } else {
        ret * q
    }
}

/// Absolute value
pub fn fabs(x: f64) -> f64 {
    fabsk(x)
}

/// Copy sign of a number
pub fn copysign(x: f64, y: f64) -> f64 {
    copysignk(x, y)
}

/// Maximum of two numbers
pub fn fmax(x: f64, y: f64) -> f64 {
    if y.is_nan() || (x > y) {
        x
    } else {
        y
    }
}

/// Minimum of two numbers
pub fn fmin(x: f64, y: f64) -> f64 {
    if y.is_nan() || (x < y) {
        x
    } else {
        y
    }
}

/// Positive difference
pub fn fdim(x: f64, y: f64) -> f64 {
    let ret = x - y;
    if (ret < 0.) || (x == y) {
        0.
    } else {
        ret
    }
}

/// Round to integer towards zero
pub fn trunc(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        (x - fr).copy_sign(x)
    }
}

/// Round to integer towards minus infinity
pub fn floor(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    fr = if fr < 0. { fr + 1. } else { fr };
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        (x - fr).copy_sign(x)
    }
}

/// Round to integer towards plus infinity
pub fn ceil(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    fr = if fr <= 0. { fr } else { fr - 1. };
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        (x - fr).copy_sign(x)
    }
}

/// Round to integer away from zero
pub fn round(d: f64) -> f64 {
    let mut x = d + 0.5;
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    if (fr == 0.) && (x <= 0.) {
        x -= 1.;
    }
    fr = if fr < 0. { fr + 1. } else { fr };
    let x = if d == 0.499_999_999_999_999_944_49 {
        0.
    } else {
        x
    }; // nextafter(0.5, 0)
    if d.is_infinite() || (fabsk(d) >= D1_52) {
        d
    } else {
        (x - fr).copy_sign(d)
    }
}

/// Round to integer, ties round to even
pub fn rint(d: f64) -> f64 {
    let c = D1_52.mul_sign(d);
    if fabsk(d) > D1_52 {
        d
    } else {
        (d + c - c).or_sign(d)
    }
}

/// Find the next representable FP value
pub fn nextafter(x: f64, y: f64) -> f64 {
    let x = if x == 0. { 0.0.mul_sign(y) } else { x };
    let mut cxi = x.to_bits() as i64;
    let c = (cxi < 0) == (y < x);
    if c {
        cxi = -(cxi ^ i64::MIN);
    }

    if x != y {
        cxi -= 1;
    }

    if c {
        cxi = -(cxi ^ i64::MIN);
    }

    let cxf = f64::from_bits(cxi as u64);
    if x.is_nan() || y.is_nan() {
        f64::NAN
    } else if (x == 0.) && (y == 0.) {
        y
    } else if (cxf == 0.) && (x != 0.) {
        mulsign(0., x)
    } else {
        cxf
    }
}

#[test]
fn test_nextafter() {
    test_ff_f(
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
pub fn frfrexp(mut x: f64) -> f64 {
    if fabsk(x) < f64::MIN_POSITIVE {
        x *= D1_63;
    }

    let mut cxu = x.to_bits();
    cxu &= !0x_7ff0_0000_0000_0000_u64;
    cxu |= 0x_3fe0_0000_0000_0000_u64;

    if x == 0. {
        x
    } else if x.is_infinite() {
        f64::INFINITY.mul_sign(x)
    } else {
        f64::from_bits(cxu)
    }
}

/// Exponent of an FP number
pub fn expfrexp(mut x: f64) -> i32 {
    let mut ret = if fabsk(x) < f64::MIN_POSITIVE {
        x *= D1_63;
        -63
    } else {
        0
    };

    let cxu = x.to_bits();
    ret += (((cxu >> 52) & 0x7ff) as i32) - 0x3fe;

    if x == 0. || x.is_nan() || x.is_infinite() {
        0
    } else {
        ret
    }
}

#[inline]
fn removelsb(d: f64) -> f64 {
    f64::from_bits(d.to_bits() & 0x_ffff_ffff_ffff_fffe)
}

/// FP remainder
pub fn fmod(x: f64, y: f64) -> f64 {
    #[inline]
    fn toward0(d: f64) -> f64 {
        if d == 0. {
            0.
        } else {
            f64::from_bits(d.to_bits() - 1)
        }
    }

    #[inline]
    fn trunc_positive(x: f64) -> f64 {
        let fr = (-D1_31).mla((x * (1. / D1_31)) as i32 as f64, x);
        if fabsk(x) >= D1_52 {
            x
        } else {
            x - (fr - (fr as i32 as f64))
        }
    }

    let mut n = fabsk(x);
    let mut d = fabsk(y);
    let s = if d < f64::MIN_POSITIVE {
        n *= D1_54;
        d *= D1_54;
        1. / D1_54
    } else {
        1.
    };
    let mut r = Doubled::from(n);
    let rd = toward0(1. / d);

    for _ in 0..21 {
        // ceil(log2(DBL_MAX) / 52)
        let mut q = removelsb(trunc_positive(toward0(r.0) * rd));
        q = if (3. * d > r.0) && (r.0 > d) { 2. } else { q };
        q = if (2. * d > r.0) && (r.0 > d) { 1. } else { q };
        q = if r.0 == d {
            if r.1 >= 0. {
                1.
            } else {
                0.
            }
        } else {
            q
        };
        r = (r + q.mul_as_doubled(-d)).normalize();
        if r.0 < d {
            break;
        }
    }

    let mut ret = if f64::from(r) == d { 0. } else { r.0 * s };
    ret = ret.mul_sign(x);
    if d == 0. {
        f64::NAN
    } else if n < d {
        x
    } else {
        ret
    }
}

// TODO: add test for fmod

#[inline]
fn rintk2(d: f64) -> f64 {
    let c = D1_52.mul_sign(d);
    if fabsk(d) > D1_52 {
        d
    } else {
        (d + c - c).or_sign(d)
    }
}

/// FP remainder
pub fn remainder(x: f64, y: f64) -> f64 {
    let mut n = fabsk(x);
    let mut d = fabsk(y);
    let mut s = 1.;
    if d < f64::MIN_POSITIVE * 2. {
        n *= D1_54;
        d *= D1_54;
        s = 1. / D1_54;
    }
    let rd = 1. / d;
    let mut r = Doubled::from(n);
    let mut qisodd = false;

    for _ in 0..21 {
        // ceil(log2(DBL_MAX) / 52)
        let mut q = removelsb(rintk2(r.0 * rd));
        if fabsk(r.0) < 1.5 * d {
            q = if r.0 < 0. { -1. } else { 1. };
        }
        if fabsk(r.0) < 0.5 * d || (fabsk(r.0) == 0.5 * d && !qisodd) {
            q = 0.;
        }
        if q == 0. {
            break;
        }
        if (q * -d).is_infinite() {
            q += (-1.0).mul_sign(r.0);
        }
        qisodd ^= q.is_odd();
        r = (r + q.mul_as_doubled(-d)).normalize();
    }

    let mut ret = r.0 * s;
    ret = ret.mul_sign(x);
    if y.is_infinite() {
        ret = if x.is_infinite() { f64::NAN } else { x };
    }
    if d == 0. {
        f64::NAN
    } else {
        ret
    }
}

#[test]
fn test_remainder() {
    test_ff_f(
        remainder,
        rug::Float::remainder,
        f64::MIN..=f64::MAX,
        f64::MIN..=f64::MAX,
        0.5,
    );
}

/// Integral and fractional value of FP number
pub fn modf(x: f64) -> (f64, f64) {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    fr = if fabsk(x) >= D1_52 { 0. } else { fr };
    (fr.copy_sign(x), (x - fr).copy_sign(x))
}

/*
/// Square root function
///
/// The error bound of the returned value is `0.5001 ULP`
pub fn sqrt(d: f64) -> f64 {
    SQRT(d)
}
*/
