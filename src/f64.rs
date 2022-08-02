use core::f64::consts::{FRAC_1_PI, FRAC_2_PI, FRAC_PI_2, FRAC_PI_4, PI};

use crate::common::*;
use doubled::*;

const D1_63: f64 = (1u64 << 63) as f64;
const D1_60: f64 = (1u64 << 60) as f64;
const D1_54: f64 = (1u64 << 54) as f64;
const D1_53: f64 = (1u64 << 53) as f64;
const D1_52: f64 = (1u64 << 52) as f64;
const D1_32: f64 = (1u64 << 32) as f64;
const D1_31: f64 = (1u64 << 31) as f64;
const D1_28: f64 = (1u64 << 28) as f64;
const D1_24: f64 = (1u64 << 24) as f64;
const D1_23: f64 = (1u64 << 23) as f64;

const SLEEF_FP_ILOGB0: i32 = -2_147_483_648;
const SLEEF_FP_ILOGBNAN: i32 = 2_147_483_647;
pub(crate) const SQRT_DBL_MAX: f64 = 1.340_780_792_994_259_635_5_e+154;
const M_2_PI_H: f64 = 0.636_619_772_367_581_382_43;
const M_2_PI_L: f64 = -3.935_735_335_036_497_176_4_e-17;
const TRIGRANGEMAX3: f64 = 1e+9;
const L2U: f64 = 0.693_147_180_559_662_956_511_601_805_686_950_683_593_75;
const L2L: f64 = 0.282_352_905_630_315_771_225_884_481_750_134_360_255_254_120_68_e-12;
const R_LN2: f64 =
    1.442_695_040_888_963_407_359_924_681_001_892_137_426_645_954_152_985_934_135_449_406_931;
const L10U: f64 = 0.301_029_995_663_839_144_98; // log 2 / log 10
const L10L: f64 = 1.420_502_322_726_609_941_8_e-13;
const LOG10_2: f64 = 3.321_928_094_887_362_347_870_319_429_489_390_175_864_831_393;

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
const PI_A: f64 = 3.141_592_621_803_283_691_4;
const PI_B: f64 = 3.178_650_942_459_171_346_9_e-8;
const PI_C: f64 = 1.224_646_786_410_718_850_2_e-16;
const PI_D: f64 = 1.273_663_432_702_189_981_6_e-24;
const TRIGRANGEMAX: f64 = 1e+14;

/*
 PI_A2 and PI_B2 are constants that satisfy the following two conditions.

 * The last 3 bits of PI_A2 are zero.
 * PI_A2 + PI_B2 is close to PI as much as possible.

 The argument of a trig function is multiplied by 1/PI, and the
 integral part is multiplied by PI_A2. So, the maximum argument that
 could be correctly reduced should be 2^(3-1) PI = 12.6. By testing,
 we confirmed that it correctly reduces the argument up to around 15.
*/
const PI_A2: f64 = 3.141_592_653_589_793_116;
const PI_B2: f64 = 1.224_646_799_147_353_207_2_e-16;
const TRIGRANGEMAX2: f64 = 15.;

#[inline]
const fn dd(h: f64, l: f64) -> Doubled<f64> {
    Doubled::new(h, l)
}

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
#[rustfmt::skip]
pub use u15::{
    erfc as erfc_u15,
};
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

#[cfg(test)]
use rug::{Assign, Float};
#[cfg(test)]
pub(crate) const PRECF64: u32 = 128;

#[cfg(test)]
pub(crate) fn count_ulp(d: f64, c: &Float) -> f64 {
    let c2 = c.to_f64();
    if (c2 == 0.) && (d != 0.) {
        return 10000.;
    }

    if (c2 == 0. || c2.is_subnormal()) && (d == 0. || d.is_subnormal()) {
        return 0.;
    }

    if c2.is_infinite() && d.is_infinite() {
        return 0.;
    }

    let prec = c.prec();

    let mut fry = Float::with_val(prec, d);

    let mut frw = Float::new(prec);

    let (_, e) = c.to_f64_exp();

    frw.assign(Float::u_exp(1, e - 53_i32));

    fry -= c;
    fry /= &frw;
    let u = fabs(fry.to_f64());

    u
}

#[cfg(test)]
pub(crate) fn gen_input(
    rng: &mut rand::rngs::ThreadRng,
    range: core::ops::RangeInclusive<f64>,
) -> f64 {
    use rand::Rng;
    let mut start = *range.start();
    if start == f64::MIN {
        start = -1e306;
    }
    let mut end = *range.end();
    if end == f64::MAX {
        end = 1e306;
    }
    rng.gen_range(start..=end)
}

#[cfg(test)]
#[allow(warnings)]
fn test_f_f(
    fun_fx: fn(f64) -> f64,
    fun_f: fn(Float) -> Float,
    range: core::ops::RangeInclusive<f64>,
    ulp_ex: f64,
) {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input = gen_input(&mut rng, range.clone());
        let output = fun_fx(input);
        let expected = fun_f(Float::with_val(PRECF64, input));
        if expected.is_nan() && output.is_nan() {
            continue;
        }
        let ulp = count_ulp(output, &expected);
        assert!(
            ulp <= ulp_ex,
            "Iteration: {n}, Input: {input:e}, Output: {output}, Expected: {expected}, ULP: {ulp} > {}",
            ulp_ex
        );
    }
}

#[cfg(test)]
#[allow(warnings)]
fn test_f_ff(
    fun_fx: fn(f64) -> (f64, f64),
    fun_f: fn(Float) -> (Float, Float),
    range: core::ops::RangeInclusive<f64>,
    ulp_ex: f64,
) {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input = gen_input(&mut rng, range.clone());
        let (output1, output2) = fun_fx(input);
        let (expected1, expected2) = fun_f(Float::with_val(PRECF64, input));
        if (expected1.is_nan() && output1.is_nan()) || (expected2.is_nan() && output2.is_nan()) {
            continue;
        }
        let ulp1 = count_ulp(output1, &expected1);
        let ulp2 = count_ulp(output2, &expected2);
        assert!(
            ulp1 <= ulp_ex && ulp2 <= ulp_ex,
                "Iteration: {n}, Input: {input:e}, Output: ({output1}, {output2}), Expected: ({expected1}, {expected2}), ULP: ({ulp1}, {ulp2}) > {}",
                ulp_ex
        );
    }
}

#[cfg(test)]
#[allow(warnings)]
fn test_ff_f(
    fun_fx: fn(f64, f64) -> f64,
    fun_f: fn(Float, &Float) -> Float,
    range1: core::ops::RangeInclusive<f64>,
    range2: core::ops::RangeInclusive<f64>,
    ulp_ex: f64,
) {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input1 = gen_input(&mut rng, range1.clone());
        let input2 = gen_input(&mut rng, range2.clone());
        let output = fun_fx(input1, input2);
        let expected = fun_f(
            Float::with_val(PRECF64, input1),
            &Float::with_val(PRECF64, input2),
        );
        if expected.is_nan() && output.is_nan() {
            continue;
        }
        let ulp = count_ulp(output, &expected);
        assert!(
            ulp <= ulp_ex,
            "Iteration: {n}, Input: ({input1:e}, {input2:e}), Output: {output}, Expected: {expected}, ULP: {ulp} > {}",
            ulp_ex
        );
    }
}

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

impl MulAdd for f64 {
    #[inline]
    fn mul_add(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}

impl Poly<f64> for f64 {
    fn c2v(c: f64) -> Self {
        c
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

#[inline]
pub fn mulsign(x: f64, y: f64) -> f64 {
    f64::from_bits(x.to_bits() ^ (y.to_bits() & (1 << 63)))
}

#[inline]
pub fn copysignk(x: f64, y: f64) -> f64 {
    f64::from_bits((x.to_bits() & !(1 << 63)) ^ (y.to_bits() & (1 << 63)))
}

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

#[inline]
fn atan2k(mut y: f64, mut x: f64) -> f64 {
    let mut q = if x < 0. {
        x = -x;
        -2
    } else {
        0
    };

    if y > x {
        let t = x;
        x = y;
        y = -t;
        q += 1;
    }

    let s = y / x;
    let t = s * s;

    let t2 = t * t;
    let t4 = t2 * t2;
    let t8 = t4 * t4;
    let t16 = t8 * t8;

    let u = f64::poly19(
        t,
        t2,
        t4,
        t8,
        t16,
        -1.887_960_084_630_734_965_637_46_e-5_f64,
        0.000_209_850_076_645_816_976_906_797,
        -0.001_106_118_314_866_724_825_634_71,
        0.003_700_267_441_887_131_192_324_03,
        -0.008_898_961_958_876_554_917_408_09,
        0.016_599_329_773_529_201_970_117,
        -0.025_451_762_493_231_264_161_686_1,
        0.033_785_258_000_135_306_999_389_7,
        -0.040_762_919_127_683_650_000_193_4,
        0.046_666_715_007_784_062_563_267_5,
        -0.052_367_485_230_348_245_761_611_3,
        0.058_766_639_292_667_358_085_431_3,
        -0.066_657_357_936_108_052_598_456_2,
        0.076_921_953_831_176_961_835_502_9,
        -0.090_908_995_008_245_008_229_153,
        0.111_111_105_648_261_418_443_745,
        -0.142_857_142_667_713_293_837_65,
        0.199_999_999_996_591_265_594_148,
        -0.333_333_333_333_311_110_369_124,
    );

    let t = u * t * s + s;
    (q as f64) * f64::consts::FRAC_PI_2 + t
}

fn rempisub(x: f64) -> (f64, i32) {
    // This function is equivalent to :
    // ( x - round(4 * x) * 0.25, (round(4 * x) - round(x) * 4) as i32 );
    let mut fr = x - D1_28 * ((x * (1.0 / D1_28)) as i32 as f64);
    let mut reti = ((7 & ((if x > 0. { 4 } else { 3 }) + ((fr * 8.) as i32))) - 3) >> 1;
    fr = fr - 0.25 * ((fr * 4. + mulsign(0.5, x)) as i32 as f64);
    fr = if fabsk(fr) > 0.25 {
        fr - mulsign(0.5, x)
    } else {
        fr
    };
    fr = if fabsk(fr) > 1e+10 { 0. } else { fr };
    if fabsk(x) == 0.124_999_999_999_999_986_12 {
        fr = x;
        reti = 0;
    }
    (fr, reti)
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
    y = dd(
        crate::tables::REMPITABDP[ex + 2],
        crate::tables::REMPITABDP[ex + 3],
    ) * a;
    x += y;
    x = x.normalize()
        * dd(
            3.141_592_653_589_793_116 * 2.,
            1.224_646_799_147_353_207_2_e-16 * 2.,
        );
    (if fabsk(a) < 0.7 { dd(a, 0.) } else { x }, q)
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
    .mul_add(
        s,
        if o {
            -3.897_962_260_629_327_991_640_47_e-13
        } else {
            6.948_218_305_801_794_613_277_84_e-12
        },
    )
    .mul_add(
        s,
        if o {
            1.150_115_825_399_960_352_669_01_e-10
        } else {
            -1.757_247_499_528_531_799_526_64_e-9
        },
    )
    .mul_add(
        s,
        if o {
            -2.461_136_950_104_469_749_535_9_e-8
        } else {
            3.133_616_889_668_683_928_784_22_e-7
        },
    )
    .mul_add(
        s,
        if o {
            3.590_860_448_590_527_540_050_62_e-6
        } else {
            -3.657_620_418_216_155_192_036_1_e-5
        },
    )
    .mul_add(
        s,
        if o {
            -0.000_325_991_886_927_389_905_997_954
        } else {
            0.002_490_394_570_192_718_502_743_56
        },
    );
    let mut x = u * s
        + (if o {
            dd(
                0.015_854_344_243_815_501_891_425_9,
                -1.046_932_722_806_315_219_088_45_e-18,
            )
        } else {
            dd(
                -0.080_745_512_188_280_785_248_473_1,
                3.618_524_750_670_371_048_499_87_e-18,
            )
        });
    x = s2 * x
        + (if o {
            dd(
                -0.308_425_137_534_042_437_259_529,
                -1.956_984_921_336_335_503_383_45_e-17,
            )
        } else {
            dd(
                0.785_398_163_397_448_278_999_491,
                3.062_871_137_271_550_026_071_05_e-17,
            )
        });

    x *= if o { s2 } else { dd(t, 0.) };
    x = if o { x + 1. } else { x };

    //

    if (q & 4) != 0 {
        x.0 = -x.0;
        x.1 = -x.1;
    }

    x
}

#[inline]
fn cospik(d: f64) -> Doubled<f64> {
    let u = d * 4.;
    let q = ceilk(u) & !1;
    let o = (q & 2) == 0;

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
    .mul_add(
        s,
        if o {
            -3.897_962_260_629_327_991_640_47_e-13
        } else {
            6.948_218_305_801_794_613_277_84_e-12
        },
    )
    .mul_add(
        s,
        if o {
            1.150_115_825_399_960_352_669_01_e-10
        } else {
            -1.757_247_499_528_531_799_526_64_e-9
        },
    )
    .mul_add(
        s,
        if o {
            -2.461_136_950_104_469_749_535_9_e-8
        } else {
            3.133_616_889_668_683_928_784_22_e-7
        },
    )
    .mul_add(
        s,
        if o {
            3.590_860_448_590_527_540_050_62_e-6
        } else {
            -3.657_620_418_216_155_192_036_1_e-5
        },
    )
    .mul_add(
        s,
        if o {
            -0.000_325_991_886_927_389_905_997_954
        } else {
            0.002_490_394_570_192_718_502_743_56
        },
    );
    let mut x = u * s
        + (if o {
            dd(
                0.015_854_344_243_815_501_891_425_9,
                -1.046_932_722_806_315_219_088_45_e-18,
            )
        } else {
            dd(
                -0.080_745_512_188_280_785_248_473_1,
                3.618_524_750_670_371_048_499_87_e-18,
            )
        });
    x = s2 * x
        + (if o {
            dd(
                -0.308_425_137_534_042_437_259_529,
                -1.956_984_921_336_335_503_383_45_e-17,
            )
        } else {
            dd(
                0.785_398_163_397_448_278_999_491,
                3.062_871_137_271_550_026_071_05_e-17,
            )
        });

    x *= if o { s2 } else { dd(t, 0.) };
    x = if o { x + 1. } else { x };

    //

    if ((q + 2) & 4) != 0 {
        x.0 = -x.0;
        x.1 = -x.1;
    }

    x
}

#[inline]
fn expm1k(d: f64) -> f64 {
    let q = rintk(d * R_LN2);

    let s = q.mul_add(-L2U, d);
    let s = q.mul_add(-L2L, s);

    let s2 = s * s;
    let s4 = s2 * s2;
    let s8 = s4 * s4;

    let mut u = f64::poly10(
        s,
        s2,
        s4,
        s8,
        2.088_606_211_072_836_875_363_41_e-9,
        2.511_129_308_928_765_186_106_61_e-8,
        2.755_739_112_349_004_718_933_38_e-7,
        2.755_723_629_119_288_276_294_23_e-6,
        2.480_158_715_923_547_299_879_1_e-5,
        0.000_198_412_698_960_509_205_564_975,
        0.001_388_888_888_897_744_922_079_62,
        0.008_333_333_333_316_527_216_649_84,
        0.041_666_666_666_666_504_759_142_2,
        0.166_666_666_666_666_851_703_837,
    );

    u = s2.mul_add(0.5, s2 * s * u) + s;

    let q = q as i32;
    if q != 0 {
        ldexp2k(u + 1., q) - 1.
    } else {
        u
    }
}

#[inline]
fn logk(mut d: f64) -> Doubled<f64> {
    let o = d < f64::MIN_POSITIVE;
    if o {
        d *= D1_32 * D1_32
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let mut x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.square();

    let x4 = x2.0 * x2.0;
    let x8 = x4 * x4;
    let x16 = x8 * x8;

    let t = f64::poly9(
        x2.0,
        x4,
        x8,
        x16,
        0.116_255_524_079_935_043_668_677,
        0.103_239_680_901_072_952_701_192,
        0.117_754_809_412_463_995_466_069,
        0.133_329_810_868_462_739_215_09,
        0.153_846_227_114_512_262_845_736,
        0.181_818_180_850_050_775_676_507,
        0.222_222_222_230_083_560_345_903,
        0.285_714_285_714_249_172_087_875,
        0.400_000_000_000_000_077_715_612,
    );
    let c = dd(
        0.666_666_666_666_666_629_659_233,
        3.805_549_625_424_120_563_366_16_e-17,
    );

    let mut s = (dd(
        0.693_147_180_559_945_286_226_764,
        2.319_046_813_846_299_558_417_771_e-17,
    ) * (e as f64))
        .add_checked(x.scale(2.));
    x = x2 * x;
    s = s.add_checked(x * c);
    x = x2 * x;
    s.add_checked(x * t)
}

#[inline]
fn expk(d: Doubled<f64>) -> f64 {
    let q = rintk((d.0 + d.1) * R_LN2);

    let s = d + q * (-L2U) + q * (-L2L);

    let s = s.normalize();

    let s2 = s.0 * s.0;
    let s4 = s2 * s2;
    let s8 = s4 * s4;

    let u = f64::poly10(
        s.0,
        s2,
        s4,
        s8,
        2.510_696_834_209_504_195_271_39_e-8,
        2.762_861_667_702_706_491_168_55_e-7,
        2.755_724_967_250_235_741_438_64_e-6,
        2.480_149_739_898_197_941_141_53_e-5,
        0.000_198_412_698_809_069_797_676_111,
        0.001_388_888_893_997_712_896_052_9,
        0.008_333_333_333_323_714_176_010_81,
        0.041_666_666_666_540_952_412_844_9,
        0.166_666_666_666_666_740_681_535,
        0.500_000_000_000_000_999_200_722,
    );

    //    let mut t = (1.).add_checked(s);
    let mut t = 1. + s;
    t = t.add_checked(s.square() * u);

    if d.0 < -1000. {
        0.
    } else {
        ldexpk(t.0 + t.1, q as i32)
    }
}

#[inline]
fn expk2(d: Doubled<f64>) -> Doubled<f64> {
    let qf = rintk((d.0 + d.1) * R_LN2);
    let q = qf as i32;

    let s = d + qf * (-L2U) + qf * (-L2L);

    let u = 0.160_247_221_970_993_207_2_e-9_f64
        .mul_add(s.0, 0.209_225_518_356_315_700_7_e-8)
        .mul_add(s.0, 0.250_523_002_378_264_446_5_e-7)
        .mul_add(s.0, 0.275_572_480_090_213_530_3_e-6)
        .mul_add(s.0, 0.275_573_189_238_604_437_3_e-5)
        .mul_add(s.0, 0.248_015_873_560_581_506_5_e-4)
        .mul_add(s.0, 0.198_412_698_414_807_185_8_e-3)
        .mul_add(s.0, 0.138_888_888_888_676_325_5_e-2)
        .mul_add(s.0, 0.833_333_333_333_334_709_5_e-2)
        .mul_add(s.0, 0.416_666_666_666_666_990_5_e-1);

    let mut t = s * u + 0.166_666_666_666_666_657_4;
    t = s * t + 0.5;
    t = s + s.square() * t;

    t = 1. + t;

    t = Doubled::new(ldexp2k(t.0, q), ldexp2k(t.1, q));

    if d.0 < -1000. {
        dd(0., 0.)
    } else {
        t
    }
}

#[inline]
fn logk2(d: Doubled<f64>) -> Doubled<f64> {
    let e = ilogbk(d.0 * (1. / 0.75));

    let m = Doubled::new(ldexp2k(d.0, -e), ldexp2k(d.1, -e));

    let x = (m + (-1.)) / (m + 1.);
    let x2 = x.square();

    let x4 = x2.0 * x2.0;
    let x8 = x4 * x4;

    let t = f64::poly7(
        x2.0,
        x4,
        x8,
        0.138_604_363_904_671_679_108_56,
        0.131_699_838_841_615_374_240_845,
        0.153_914_168_346_271_945_653_214,
        0.181_816_523_941_564_611_721_589,
        0.222_222_246_326_620_354_039_96,
        0.285_714_285_511_134_091_777_308,
        0.400_000_000_000_914_013_309_483,
    )
    .mul_add(x2.0, 0.666_666_666_666_664_853_302_393);

    (dd(
        0.693_147_180_559_945_286_226_764,
        2.319_046_813_846_299_558_417_771_e-17,
    ) * (e as f64))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * t)
}

fn gammak(a: f64) -> (Doubled<f64>, Doubled<f64>) {
    let mut clln = dd(1., 0.);
    let mut clld = dd(1., 0.);

    let otiny = fabsk(a) < 1e-306;
    let oref = a < 0.5;

    let mut x = if otiny {
        dd(0., 0.)
    } else if oref {
        (1.).add_as_doubled(-a)
    } else {
        dd(a, 0.)
    };

    let o0 = (0.5 <= x.0) && (x.0 <= 1.1);
    let o2 = 2.3 < x.0;

    let mut y = ((x + 1.) * x).normalize();
    y = ((x + 2.) * y).normalize();
    y = ((x + 3.) * y).normalize();
    y = ((x + 4.) * y).normalize();

    clln = if o2 && (x.0 <= 7.) { y } else { clln };

    x = if o2 && (x.0 <= 7.) { x + 5. } else { x };
    let t = if o2 {
        1. / x.0
    } else {
        (x + (if o0 { -1. } else { -2. })).normalize().0
    };

    let u = (if o2 {
        -156.801_412_704_022_726_379_848_862_f64
    } else if o0 {
        0.294_791_677_282_761_419_6_e+2
    } else {
        0.707_481_600_086_460_927_9_e-7
    })
    .mul_add(
        t,
        if o2 {
            1.120_804_464_289_911_606_838_558_160_000
        } else if o0 {
            0.128_145_969_182_782_010_9_e+3
        } else {
            0.400_924_433_300_873_044_3_e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            13.397_985_455_142_589_218_333_060_200_00
        } else if o0 {
            0.261_754_402_578_451_504_3_e+3
        } else {
            0.104_011_464_162_824_694_6_e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.116_546_276_599_463_200_848_033_357_000
        } else if o0 {
            0.328_702_285_568_579_043_2_e+3
        } else {
            0.150_834_915_073_332_916_7_e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            -1.391_801_093_265_337_481_495_562_410_000
        } else if o0 {
            0.281_814_586_773_034_818_6_e+3
        } else {
            0.128_814_307_493_390_102_e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            0.015_056_113_040_026_424_412_918_973_400
        } else if o0 {
            0.172_867_041_467_355_960_5_e+3
        } else {
            0.474_416_774_988_499_393_7_e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            0.179_540_117_061_234_856_098_844_714_000
        } else if o0 {
            0.774_873_576_403_041_681_7_e+2
        } else {
            -0.655_481_630_654_248_990_2_e-7
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.002_481_743_600_264_997_730_942_489_280
        } else if o0 {
            0.251_285_664_308_093_075_2_e+2
        } else {
            -0.318_925_247_145_259_984_4_e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.029_527_880_945_699_120_504_851_034_100
        } else if o0 {
            0.576_679_210_614_007_686_8_e+1
        } else {
            0.135_888_382_147_035_537_7_e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            0.000_540_164_767_892_604_515_196_325_186
        } else if o0 {
            0.727_027_547_399_618_057_1
        } else {
            -0.434_393_127_715_733_604_e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            0.006_403_362_833_808_069_794_787_256_200
        } else if o0 {
            0.839_670_912_457_914_780_9_e-1
        } else {
            0.972_478_589_740_677_955_5_e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000_162_516_262_783_915_816_896_611_252
        } else if o0 {
            -0.821_155_866_974_680_459_5_e-1
        } else {
            -0.203_688_605_722_596_601_1_e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.001_914_438_498_565_477_526_465_972_390
        } else if o0 {
            0.682_883_182_834_188_445_8_e-1
        } else {
            0.437_336_314_181_972_581_5_e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            7.204_895_416_020_010_558_983_115_17_e-5
        } else if o0 {
            -0.771_248_133_996_167_151_1_e-1
        } else {
            -0.943_995_126_830_400_867_7_e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            0.000_839_498_720_672_087_279_971_000_786
        } else if o0 {
            0.833_749_202_301_731_495_7_e-1
        } else {
            0.205_072_703_037_638_980_4_e-4
        },
    )
    .mul_add(
        t,
        if o2 {
            -5.171_790_908_260_592_193_293_944_22_e-5
        } else if o0 {
            -0.909_496_493_145_624_251_8_e-1
        } else {
            -0.449_262_018_343_118_401_8_e-4
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000_592_166_437_353_693_882_857_342_347
        } else if o0 {
            0.100_099_631_357_592_935_8
        } else {
            0.994_575_123_607_187_593_1_e-4
        },
    )
    .mul_add(
        t,
        if o2 {
            6.972_813_758_365_857_774_037_435_39_e-5
        } else if o0 {
            -0.111_334_286_154_420_772_4
        } else {
            -0.223_154_759_903_498_319_6_e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            0.000_784_039_221_720_066_627_493_314_301
        } else if o0 {
            0.125_509_667_321_302_087_5
        } else {
            0.509_669_524_710_196_762_2_e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000_229_472_093_621_399_176_949_318_732
        } else if o0 {
            -0.144_049_896_784_305_436_8
        } else {
            -0.119_275_391_166_788_697_1_e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.002_681_327_160_493_827_160_473_958_490
        } else if o0 {
            0.169_557_177_004_194_981_1
        } else {
            0.289_051_033_074_221_031_e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            0.003_472_222_222_222_222_222_175_164_840
        } else if o0 {
            -0.207_385_551_028_409_276_2
        } else {
            -0.738_555_102_867_446_185_8_e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            0.083_333_333_333_333_333_335_592_087_900
        } else if o0 {
            0.270_580_808_427_781_593_9
        } else {
            0.205_808_084_277_845_533_5_e-1
        },
    );

    y = (x + (-0.5)) * logk2(x);
    y += -x;
    y += dd(
        0.918_938_533_204_672_780_56,
        -3.878_294_158_067_241_449_8_e-17,
    ); // 0.5*log(2*M_PI)

    let mut z = u.mul_as_doubled(t)
        + (if o0 {
            -0.400_685_634_386_531_486_2
        } else {
            -0.673_523_010_531_981_020_1_e-1
        });
    z = z * t
        + (if o0 {
            0.822_467_033_424_113_203_0
        } else {
            0.322_467_033_424_113_203_0
        });
    z = z * t
        + (if o0 {
            -0.577_215_664_901_532_865_5
        } else {
            0.422_784_335_098_467_134_5
        });
    z *= t;

    let mut clc = if o2 { y } else { z };

    clld = if o2 { u.mul_as_doubled(t) + 1. } else { clld };

    y = clln;

    clc = if otiny {
        dd(
            83.177_661_667_193_433_459_033_3,
            3.671_034_596_315_685_072_218_78_e-15,
        ) // log(2^120)
    } else if oref {
        dd(1.144_729_885_849_400_163_9, 1.026_595_116_270_782_638_e-17) + (-clc)
    } else {
        clc
    }; // log(M_PI)
    let clln = if otiny {
        dd(1., 0.)
    } else if oref {
        clln
    } else {
        clld
    };

    if oref {
        x = clld * sinpik(a - D1_28 * ((a * (1. / D1_28)) as i32 as f64));
    }

    clld = if otiny {
        dd(a * (D1_60 * D1_60), 0.)
    } else if oref {
        x
    } else {
        y
    };

    (clc, clln / clld)
}

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

pub fn ilogb(d: f64) -> i32 {
    let mut e = ilogbk(fabsk(d));
    e = if d == 0. { SLEEF_FP_ILOGB0 as i32 } else { e };
    e = if d.is_nan() {
        SLEEF_FP_ILOGBNAN as i32
    } else {
        e
    };
    if d.is_infinite() {
        i32::MAX
    } else {
        e
    }
}

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
    let ret = if (x == 0.) || (y == 0.) { z } else { d.0 + d.1 };
    if z.is_infinite() && !x.is_infinite() && !x.is_nan() && !y.is_infinite() && !y.is_nan() {
        h2 = z;
    }
    if h2.is_infinite() || h2.is_nan() {
        h2
    } else {
        ret * q
    }
}

pub fn fabs(x: f64) -> f64 {
    fabsk(x)
}

pub fn copysign(x: f64, y: f64) -> f64 {
    copysignk(x, y)
}

pub fn fmax(x: f64, y: f64) -> f64 {
    if y.is_nan() || (x > y) {
        x
    } else {
        y
    }
}

pub fn fmin(x: f64, y: f64) -> f64 {
    if y.is_nan() || (x < y) {
        x
    } else {
        y
    }
}

pub fn fdim(x: f64, y: f64) -> f64 {
    let ret = x - y;
    if (ret < 0.) || (x == y) {
        0.
    } else {
        ret
    }
}

pub fn trunc(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

pub fn floor(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    fr = if fr < 0. { fr + 1. } else { fr };
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

pub fn ceil(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    fr = if fr <= 0. { fr } else { fr - 1. };
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

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
        copysignk(x - fr, d)
    }
}

pub fn rint(d: f64) -> f64 {
    let x = d + 0.5;
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    let isodd = (1 & (fr as i32)) != 0;
    fr = fr - (fr as i32 as f64);
    fr = if (fr < 0.) || ((fr == 0.) && isodd) {
        fr + 1.
    } else {
        fr
    };
    let x = if d == 0.500_000_000_000_000_111_02 {
        0.
    } else {
        x
    }; // nextafter(0.5, 1)
    if d.is_infinite() || (fabsk(d) >= D1_52) {
        d
    } else {
        copysignk(x - fr, d)
    }
}

pub fn nextafter(x: f64, y: f64) -> f64 {
    let x = if x == 0. { mulsign(0., y) } else { x };
    let mut cxi = x.to_bits() as i64;
    let c = (cxi < 0) == (y < x);
    if c {
        cxi = -(cxi ^ (1 << 63));
    }

    if x != y {
        cxi -= 1;
    }

    if c {
        cxi = -(((cxi as u64) ^ (1u64 << 63)) as i64);
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
        mulsign(f64::INFINITY, x)
    } else {
        f64::from_bits(cxu)
    }
}

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
    fn removelsb(d: f64) -> f64 {
        f64::from_bits(d.to_bits() & 0x_ffff_ffff_ffff_fffe)
    }

    #[inline]
    fn trunc_positive(x: f64) -> f64 {
        let fr = (-D1_31).mul_add((x * (1. / D1_31)) as i32 as f64, x);
        if fabsk(x) >= D1_52 {
            x
        } else {
            x - (fr - (fr as i32 as f64))
        }
    }

    let mut nu = fabsk(x);
    let mut de = fabsk(y);
    let s = if de < f64::MIN_POSITIVE {
        nu *= D1_54;
        de *= D1_54;
        1. / D1_54
    } else {
        1.
    };
    let mut r = dd(nu, 0.);
    let rde = toward0(1. / de);

    for _ in 0..21 {
        // ceil(log2(DBL_MAX) / 51) + 1
        let q = if (de + de > r.0) && (r.0 >= de) {
            1.
        } else {
            toward0(r.0) * rde
        };
        r = (r + removelsb(trunc_positive(q)).mul_as_doubled(-de)).normalize();
        if r.0 < de {
            break;
        }
    }

    let mut ret = if r.0 + r.1 == de { 0. } else { r.0 * s };
    ret = mulsign(ret, x);
    if de == 0. {
        f64::NAN
    } else if nu < de {
        x
    } else {
        ret
    }
}

pub fn modf(x: f64) -> (f64, f64) {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr -= fr as i32 as f64;
    fr = if fabsk(x) >= D1_52 { 0. } else { fr };
    (copysignk(fr, x), copysignk(x - fr, x))
}

/*
pub fn sqrt(d: f64) -> f64 {
    SQRT(d)
}
*/
