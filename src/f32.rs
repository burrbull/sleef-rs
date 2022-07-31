use crate::common::*;
use core::f32::consts::{FRAC_1_PI, FRAC_2_PI, FRAC_PI_2, FRAC_PI_4, PI};
use doubled::*;

const F1_32: f32 = (1u64 << 32) as f32;
const F1_30: f32 = (1u32 << 30) as f32;
const F1_25: f32 = (1u32 << 25) as f32;
const F1_24: f32 = (1u32 << 24) as f32;
const F1_23: f32 = (1u32 << 23) as f32;
const F1_12: f32 = (1u32 << 12) as f32;
const F1_10: f32 = (1u32 << 10) as f32;

const PI_A_F: f32 = 3.140_625;
const PI_B_F: f32 = 0.000_967_025_756_835_937_5;
const PI_C_F: f32 = 6.277_114_152_908_325_195_3_e-7;
const PI_D_F: f32 = 1.215_420_125_655_342_076_2_e-10;
const TRIGRANGEMAX_F: f32 = 39000.;

const PI_A2_F: f32 = 3.141_479_492_187_5;
const PI_B2_F: f32 = 0.000_113_159_418_106_079_101_56;
const PI_C2_F: f32 = 1.984_187_258_941_005_893_6_e-9;
const TRIGRANGEMAX2_F: f32 = 125.0;

const SLEEF_FP_ILOGB0: i32 = -2_147_483_648;
const SLEEF_FP_ILOGBNAN: i32 = 2_147_483_647;
const SQRT_FLT_MAX: f32 = 18_446_743_523_953_729_536.;
const L10U_F: f32 = 0.301_025_390_6;
const L10L_F: f32 = 4.605_038_981_e-6;
const TRIGRANGEMAX4_F: f32 = 8e+6;
const L2U_F: f32 = 0.693_145_751_953_125;
const L2L_F: f32 = 1.428_606_765_330_187_045_e-6;
const R_LN2_F: f32 =
    1.442_695_040_888_963_407_359_924_681_001_892_137_426_645_954_152_985_934_135_449_406_931;
const LOG10_2_F: f32 = 3.321_928_094_887_362_347_870_319_429_489_390_175_864_831_393;

#[inline]
const fn df(h: f32, l: f32) -> Doubled<f32> {
    Doubled::new(h, l)
}

mod u05;
#[rustfmt::skip]
pub use u05::{
    sincospif as sincospi_u05,
    sqrtf as sqrt_u05,
    hypotf as hypot_u05,
    sinpif as sinpi_u05,
    cospif as cospi_u05,
};

mod u10;
#[rustfmt::skip]
pub use u10::{
    sinf as sin_u10,
    cosf as cos_u10,
    sincosf as sincos_u10,
    tanf as tan_u10,
    atan2f as atan2_u10,
    asinf as asin_u10,
    acosf as acos_u10,
    atanf as atan_u10,
    expf as exp_u10,
    cbrtf as cbrt_u10,
    logf as log_u10,
    powf as pow_u10,
    sinhf as sinh_u10,
    coshf as cosh_u10,
    tanhf as tanh_u10,
    asinhf as asinh_u10,
    acoshf as acosh_u10,
    atanhf as atanh_u10,
    exp10f as exp10_u10,
    expm1f as expm1_u10,
    log10f as log10_u10,
    log2f as log2_u10,
    tgammaf as tgamma_u10,
    lgammaf as lgamma_u10,
    erff as erf_u10,
    log1pf as log1p_u10,
    exp2f as exp2_u10,
};

mod u15;
#[rustfmt::skip]
pub use u15::{
    erfcf as erfc_u15,
};

mod u35;
#[rustfmt::skip]
pub use u35::{
    sinf as sin_u35,
    cosf as cos_u35,
    tanf as tan_u35,
    sincosf as sincos_u35,
    sincospif as sincospi_u35,
    atanf as atan_u35,
    atan2f as atan2_u35,
    asinf as asin_u35,
    acosf as acos_u35,
    logf as log_u35,
    sqrtf as sqrt_u35,
    cbrtf as cbrt_u35,
    sinhf as sinh_u35,
    coshf as cosh_u35,
    tanhf as tanh_u35,
    hypotf as hypot_u35,
    exp2f as exp2_u35,
    exp10f as exp10_u35,
    log2f as log2_u35,
};
mod fast;
#[rustfmt::skip]
pub use fast::{
    sinf as sin_fast,
    cosf as cos_fast,
    powf as pow_fast,
};

#[cfg(test)]
use rug::{Assign, Float};
#[cfg(test)]
pub(crate) const PRECF32: u32 = 32;

#[cfg(test)]
pub(crate) fn count_ulp(d: f32, c: &Float) -> f32 {
    let c2 = c.to_f32();
    if (c2 == 0.) && (d != 0.) {
        return 10000.;
    }

    if (c2 == 0.) && (d == 0.) {
        return 0.;
    }

    if c2.is_infinite() && d.is_infinite() {
        return 0.;
    }

    let prec = c.prec();

    let mut fry = Float::with_val(prec, d);

    let mut frw = Float::new(prec);

    let (_, e) = c.to_f32_exp();

    frw.assign(Float::u_exp(1, e - 24_i32));

    fry -= c;
    fry /= &frw;
    let u = fabsf(fry.to_f32());

    u
}

#[cfg(test)]
fn gen_input(rng: &mut rand::rngs::ThreadRng, range: core::ops::RangeInclusive<f32>) -> f32 {
    use rand::Rng;
    loop {
        let input = rng.gen();
        if !range.contains(&input) {
            continue;
        }
        break input;
    }
}

#[cfg(test)]
fn test_f_f(
    fun_fx: fn(f32) -> f32,
    fun_f: fn(Float) -> Float,
    range: core::ops::RangeInclusive<f32>,
    ulp_ex: f32,
) {
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input = gen_input(&mut rng, range.clone());
        let output = fun_fx(input);
        let expected = fun_f(Float::with_val(PRECF32, input));
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
fn test_f_ff(
    fun_fx: fn(f32) -> (f32, f32),
    fun_f: fn(Float) -> (Float, Float),
    range: core::ops::RangeInclusive<f32>,
    ulp_ex: f32,
) {
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input = gen_input(&mut rng, range.clone());
        let (output1, output2) = fun_fx(input);
        let (expected1, expected2) = fun_f(Float::with_val(PRECF32, input));
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
fn test_ff_f(
    fun_fx: fn(f32, f32) -> f32,
    fun_f: fn(Float, &Float) -> Float,
    range: core::ops::RangeInclusive<f32>,
    ulp_ex: f32,
) {
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input1 = gen_input(&mut rng, range.clone());
        let input2 = gen_input(&mut rng, range.clone());
        let output = fun_fx(input1, input2);
        let expected = fun_f(
            Float::with_val(PRECF32, input1),
            &Float::with_val(PRECF32, input2),
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

impl crate::Sleef for f32 {
    type Int = i32;
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
        u35::sqrtf(self)
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

impl BaseType for f32 {
    type Base = Self;
}

impl MaskType for f32 {
    type Mask = bool;
}

impl MulAdd for f32 {
    #[inline]
    fn mul_add(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}

impl Poly for f32 {
    fn c2v(c: Self::Base) -> Self {
        c
    }
}

impl IsInt for f32 {
    #[inline]
    fn is_integer(self) -> Self::Mask {
        self == (self as i32 as f32)
    }
}

impl IsNegZero for f32 {
    #[inline]
    fn is_neg_zero(self) -> Self::Mask {
        self.to_bits() == (-0f32).to_bits()
    }
}

#[inline]
pub fn mulsignf(x: f32, y: f32) -> f32 {
    f32::from_bits(x.to_bits() ^ (y.to_bits() & (1 << 31)))
}

#[inline]
pub fn copysignfk(x: f32, y: f32) -> f32 {
    f32::from_bits((x.to_bits() & !(1 << 31)) ^ (y.to_bits() & (1 << 31)))
}

#[inline]
pub fn signf(d: f32) -> f32 {
    mulsignf(1., d)
}

#[inline]
fn fabsfk(x: f32) -> f32 {
    f32::from_bits(0x_7fff_ffff & x.to_bits())
}

#[inline]
fn rintfk(x: f32) -> f32 {
    (if x < 0. { x - 0.5 } else { x + 0.5 }) as i32 as f32
}

#[inline]
fn ceilfk(x: f32) -> i32 {
    (x as i32) + (if x < 0. { 0 } else { 1 })
}

#[inline]
fn ilogbkf(mut d: f32) -> i32 {
    let m = d < 5.421_010_862_427_522E-20;
    d = if m { 1.844_674_407_370_955_2E19 * d } else { d };
    let q = ((d.to_bits() >> 23) & 0xff) as i32;
    if m {
        q - (64 + 0x7f)
    } else {
        q - 0x7f
    }
}

// vilogb2kf is similar to ilogbkf, but the argument has to be a
// normalized FP value.
#[inline]
fn ilogb2kf(d: f32) -> i32 {
    ((d.to_bits() >> 23) & 0xff) as i32 - 0x7f
}

#[inline]
fn pow2if(q: i32) -> f32 {
    f32::from_bits(((q + 0x7f) as u32) << 23)
}

#[inline]
fn ldexpkf(mut x: f32, mut q: i32) -> f32 {
    let mut m = q >> 31;
    m = (((m + q) >> 6) - m) << 4;
    q -= m << 2;
    m += 127;
    m = if m < 0 { 0 } else { m };
    m = if m > 255 { 255 } else { m };
    let mut u = f32::from_bits((m as u32) << 23);
    x = x * u * u * u * u;
    u = f32::from_bits(((q + 0x7f) as u32) << 23);
    x * u
}

#[inline]
fn ldexp2kf(d: f32, e: i32) -> f32 {
    // faster than ldexpkf, short reach
    d * pow2if(e >> 1) * pow2if(e - (e >> 1))
}

#[inline]
fn ldexp3kf(d: f32, e: i32) -> f32 {
    // very fast, no denormal
    f32::from_bits(((d.to_bits() as i32) + (e << 23)) as u32)
}

fn rempisubf(x: f32) -> (f32, i32) {
    let mut fr = x - F1_10 * ((x * (1. / F1_10)) as i32 as f32);
    let mut reti = ((7 & ((if x > 0. { 4 } else { 3 }) + ((fr * 8.) as i32))) - 3) >> 1;
    fr = fr - 0.25 * ((fr * 4. + mulsignf(0.5, x)) as i32 as f32);
    fr = if fabsfk(fr) > 0.125 {
        fr - mulsignf(0.5, x)
    } else {
        fr
    };
    fr = if fabsfk(fr) > 1e+10 { 0. } else { fr };
    if fabsfk(x) == 0.124_999_992_549_419_403_08 {
        fr = x;
        reti = 0;
    }
    (fr, reti)
}

fn rempif(a: f32) -> (Doubled<f32>, i32) {
    let mut ex = ilogb2kf(a) - 25;
    let mut q = if ex > (90 - 25) { -64 } else { 0 };
    let a = ldexp3kf(a, q);
    if ex < 0 {
        ex = 0;
    }
    let ex = (ex * 4) as usize;
    let mut x = a.mul_as_doubled(crate::tables::REMPITABSP[ex]);
    let (did, dii) = rempisubf(x.0);
    q = dii;
    x.0 = did;
    x = x.normalize();
    let mut y = a.mul_as_doubled(crate::tables::REMPITABSP[ex + 1]);
    x += y;
    let (did, dii) = rempisubf(x.0);
    q += dii;
    x.0 = did;
    x = x.normalize();
    y = df(
        crate::tables::REMPITABSP[ex + 2],
        crate::tables::REMPITABSP[ex + 3],
    ) * a;
    x += y;
    x = x.normalize();
    x *= df(
        3.141_592_741_012_573_242_2 * 2.,
        -8.742_277_657_347_585_773_1_e-8 * 2.,
    );
    (if fabsfk(a) < 0.7 { df(a, 0.) } else { x }, q)
}

#[inline]
fn atan2kf(mut y: f32, mut x: f32) -> f32 {
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
    let mut t = s * s;
    let t2 = t * t;
    let t4 = t2 * t2;

    let u = f32::poly8(
        t,
        t2,
        t4,
        0.002_823_638_962_581_753_730_773_93,
        -0.015_956_902_876_496_315_002_441_4,
        0.042_504_988_610_744_476_318_359_4,
        -0.074_890_092_015_266_418_457_031_2,
        0.106_347_933_411_598_205_566_406,
        -0.142_027_363_181_114_196_777_344,
        0.199_926_957_488_059_997_558_594,
        -0.333_331_018_686_294_555_664_062,
    );

    t = u * t * s + s;
    (q as f32) * f32::consts::FRAC_PI_2 + t
}

#[inline]
fn expkf(d: Doubled<f32>) -> f32 {
    let qf = rintfk((d.0 + d.1) * R_LN2_F);

    let q = qf as i32;
    let mut s = d + qf * -L2U_F;
    s += qf * -L2L_F;

    s = s.normalize();

    let u = 0.001_363_246_468_827_128_410_339_36_f32
        .mul_add(s.0, 0.008_365_969_173_610_210_418_701_17)
        .mul_add(s.0, 0.041_671_082_377_433_776_855_468_8)
        .mul_add(s.0, 0.166_665_524_244_308_471_679_688)
        .mul_add(s.0, 0.499_999_850_988_388_061_523_438);

    let mut t = s.add_checked(s.square() * u);

    t = (1.).add_checked(t);

    if d.0 < -104. {
        0.
    } else {
        ldexpkf(t.0 + t.1, q)
    }
}

#[inline]
fn expm1kf(d: f32) -> f32 {
    let qf = rintfk(d * R_LN2_F);

    let q = qf as i32;
    let s = qf.mul_add(-L2U_F, d);
    let s = qf.mul_add(-L2L_F, s);

    let s2 = s * s;
    let s4 = s2 * s2;

    let mut u = f32::poly6(
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
    u = s * s * u + s;

    if q != 0 {
        ldexp2kf(u + 1., q) - 1.
    } else {
        u
    }
}

#[inline]
fn logkf(mut d: f32) -> Doubled<f32> {
    let o = d < f32::MIN_POSITIVE;
    if o {
        d *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(d * (1. / 0.75));
    let m = ldexp3kf(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.square();

    let t = 0.240_320_354_700_088_500_976_562_f32
        .mul_add(x2.0, 0.285_112_679_004_669_189_453_125)
        .mul_add(x2.0, 0.400_007_992_982_864_379_882_812);
    let c = df(
        0.666_666_626_930_236_816_406_25,
        3.691_838_612_596_143_320_843_11_e-9,
    );

    (df(0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * (x2 * t + c))
}

#[inline]
fn expk2f(d: Doubled<f32>) -> Doubled<f32> {
    let qf = rintfk((d.0 + d.1) * R_LN2_F);

    let q = qf as i32;
    let mut s = d + qf * -L2U_F;
    s += qf * -L2L_F;

    let u = 0.198_096_022_4_e-3_f32
        .mul_add(s.0, 0.139_425_648_4_e-2)
        .mul_add(s.0, 0.833_345_670_3_e-2)
        .mul_add(s.0, 0.416_663_736_1_e-1);

    let mut t = s * u + 0.166_666_659_414_234_244_790_680_580_464;
    t = s * t + 0.5;
    t = s + s.square() * t;

    t = 1. + t;

    t.0 = ldexp2kf(t.0, q);
    t.1 = ldexp2kf(t.1, q);

    if d.0 < -104. {
        df(0., 0.)
    } else {
        t
    }
}

#[inline]
fn logk2f(d: Doubled<f32>) -> Doubled<f32> {
    let e = ilogbkf(d.0 * (1. / 0.75));
    let m = d.scale(pow2if(-e));

    let x = (m + (-1.)) / (m + 1.);
    let x2 = x.square();

    let t = 0.239_282_846_450_805_664_062_5_f32
        .mul_add(x2.0, 0.285_182_118_415_832_519_531_25)
        .mul_add(x2.0, 0.400_005_877_017_974_853_515_625)
        .mul_add(x2.0, 0.666_666_686_534_881_591_796_875);

    (df(0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * t)
}

#[inline]
fn sinpifk(d: f32) -> Doubled<f32> {
    let u = d * 4.;
    let q = ceilfk(u) & !1;
    let o = (q & 2) != 0;

    let mut s = u - (q as f32);
    let t = s;
    s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (if o {
        -0.243_061_180_1_e-7_f32
    } else {
        0.309_384_205_4_e-6
    })
    .mul_add(
        s,
        if o {
            0.359_057_708_e-5
        } else {
            -0.365_730_738_8_e-4
        },
    )
    .mul_add(
        s,
        if o {
            -0.325_991_772_1_e-3
        } else {
            0.249_039_358_5_e-2
        },
    );
    let mut x = u * s
        + (if o {
            df(
                0.015_854_343_771_934_509_277,
                4.494_005_135_403_224_281_1_e-10,
            )
        } else {
            df(
                -0.080_745_510_756_969_451_904,
                -1.337_366_533_907_693_625_8_e-9,
            )
        });
    x = s2 * x
        + (if o {
            df(
                -0.308_425_128_459_930_419_92,
                -9.072_833_903_073_392_227_7_e-9,
            )
        } else {
            df(
                0.785_398_185_253_143_310_55,
                -2.185_733_861_756_648_485_5_e-8,
            )
        });

    x *= if o { s2 } else { df(t, 0.) };
    x = if o { x + 1. } else { x };

    //

    if (q & 4) != 0 {
        x = -x;
    }
    x
}

#[inline]
fn cospifk(d: f32) -> Doubled<f32> {
    let u = d * 4.;
    let q = ceilfk(u) & !1;
    let o = (q & 2) == 0;

    let mut s = u - (q as f32);
    let t = s;
    s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (if o {
        -0.243_061_180_1_e-7_f32
    } else {
        0.309_384_205_4_e-6
    })
    .mul_add(
        s,
        if o {
            0.359_057_708_e-5
        } else {
            -0.365_730_738_8_e-4
        },
    )
    .mul_add(
        s,
        if o {
            -0.325_991_772_1_e-3
        } else {
            0.249_039_358_5_e-2
        },
    );
    let mut x = u * s
        + (if o {
            df(
                0.015_854_343_771_934_509_277,
                4.494_005_135_403_224_281_1_e-10,
            )
        } else {
            df(
                -0.080_745_510_756_969_451_904,
                -1.337_366_533_907_693_625_8_e-9,
            )
        });
    x = s2 * x
        + (if o {
            df(
                -0.308_425_128_459_930_419_92,
                -9.072_833_903_073_392_227_7_e-9,
            )
        } else {
            df(
                0.785_398_185_253_143_310_55,
                -2.185_733_861_756_648_485_5_e-8,
            )
        });

    x *= if o { s2 } else { df(t, 0.) };
    x = if o { x + 1. } else { x };

    //

    if ((q + 2) & 4) != 0 {
        x = -x;
    }
    x
}

fn gammafk(a: f32) -> (Doubled<f32>, Doubled<f32>) {
    let otiny = fabsfk(a) < 1e-30;
    let oref = a < 0.5;

    let mut x = if otiny {
        df(0., 0.)
    } else if oref {
        (1.).add_as_doubled(-a)
    } else {
        df(a, 0.)
    };

    let o0 = (0.5 <= x.0) && (x.0 <= 1.2);
    let o2 = 2.3 < x.0;

    let mut y = ((x + 1.) * x).normalize();
    y = ((x + 2.) * y).normalize();

    let mut clln = if o2 && (x.0 <= 7.) { y } else { df(1., 0.) };

    x = if o2 && (x.0 <= 7.) { x + 3. } else { x };
    let t = if o2 {
        1. / x.0
    } else {
        (x + (if o0 { -1. } else { -2. })).normalize().0
    };

    let u = (if o2 {
        0.000_839_498_720_672_087_279_971_000_786_f32
    } else if o0 {
        0.943_515_777_6
    } else {
        0.110_248_955_e-3
    })
    .mul_add(
        t,
        if o2 {
            -5.171_790_908_260_592_193_293_944_22_e-5
        } else if o0 {
            0.867_006_361_5
        } else {
            0.816_001_993_4_e-4
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000_592_166_437_353_693_882_857_342_347
        } else if o0 {
            0.482_670_247_6
        } else {
            0.152_846_885_6_e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            6.972_813_758_365_857_774_037_435_39_e-5
        } else if o0 {
            -0.885_512_977_8_e-1
        } else {
            -0.235_506_871_8_e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            0.000_784_039_221_720_066_627_493_314_301
        } else if o0 {
            0.101_382_523_8
        } else {
            0.496_224_209_2_e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000_229_472_093_621_399_176_949_318_732
        } else if o0 {
            -0.149_340_897_8
        } else {
            -0.119_348_801_7_e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.002_681_327_160_493_827_160_473_958_490
        } else if o0 {
            0.169_750_914_0
        } else {
            0.289_159_943_3_e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            0.003_472_222_222_222_222_222_175_164_840
        } else if o0 {
            -0.207_245_454_2
        } else {
            -0.738_545_181_2_e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            0.083_333_333_333_333_333_335_592_087_900
        } else if o0 {
            0.270_587_235_7
        } else {
            0.205_807_704_5_e-1
        },
    );

    y = (x + (-0.5)) * logk2f(x);
    y += -x;
    y += df(0.918_938_533_204_672_780_56, 0.); // 0.5*log(2*M_PI)

    let mut z = u.mul_as_doubled(t)
        + (if o0 {
            -0.400_686_534_596_170_958_447_352_690_395
        } else {
            -0.673_523_028_297_382_446_749_257_758_235_e-1
        });
    z = z * t
        + (if o0 {
            0.822_466_960_142_643_054_450_325_495_997
        } else {
            0.322_467_033_928_981_157_743_538_726_901
        });
    z = z * t
        + (if o0 {
            -0.577_215_665_946_766_039_837_398_973_297
        } else {
            0.422_784_335_087_484_338_986_941_629_852
        });
    z *= t;

    let mut clc = if o2 { y } else { z };

    let mut clld = if o2 {
        u.mul_as_doubled(t) + 1.
    } else {
        df(1., 0.)
    };

    y = clln;

    clc = if otiny {
        df(41.588_830_833_596_718_565_03, 0.) // log(2^60)
    } else if oref {
        df(1.144_729_885_849_400_163_9, 0.) + (-clc)
    } else {
        clc
    }; // log(M_PI)
    clln = if otiny {
        df(1., 0.)
    } else if oref {
        clln
    } else {
        clld
    };

    if oref {
        x = clld * sinpifk(a - F1_12 * ((a * (1. / F1_12)) as i32 as f32));
    }

    clld = if otiny {
        df(a * (F1_30 * F1_30), 0.)
    } else if oref {
        x
    } else {
        y
    };

    (clc, clln / clld)
}

/// Integer exponent of an FP number
pub fn ilogbf(d: f32) -> i32 {
    let mut e = ilogbkf(fabsfk(d));
    e = if d == 0. { SLEEF_FP_ILOGB0 } else { e };
    e = if d.is_nan() { SLEEF_FP_ILOGBNAN } else { e };
    if d.is_infinite() {
        i32::MAX
    } else {
        e
    }
}

/// Absolute value
pub fn fabsf(x: f32) -> f32 {
    fabsfk(x)
}

/// Copy sign of a number
pub fn copysignf(x: f32, y: f32) -> f32 {
    copysignfk(x, y)
}

/// Maximum of two numbers
pub fn fmaxf(x: f32, y: f32) -> f32 {
    if y.is_nan() || (x > y) {
        x
    } else {
        y
    }
}

/// Minimum of two numbers
pub fn fminf(x: f32, y: f32) -> f32 {
    if y.is_nan() || (x < y) {
        x
    } else {
        y
    }
}

/// Positive difference
pub fn fdimf(x: f32, y: f32) -> f32 {
    let ret = x - y;
    if (ret < 0.) || (x == y) {
        0.
    } else {
        ret
    }
}

/// Round to integer towards zero
pub fn truncf(x: f32) -> f32 {
    let fr = x - (x as i32 as f32);
    if x.is_infinite() || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

/// Round to integer towards minus infinity
pub fn floorf(x: f32) -> f32 {
    let mut fr = x - (x as i32 as f32);
    fr = if fr < 0. { fr + 1. } else { fr };
    if x.is_infinite() || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

/// Round to integer towards plus infinity
pub fn ceilf(x: f32) -> f32 {
    let mut fr = x - (x as i32 as f32);
    fr = if fr <= 0. { fr } else { fr - 1. };
    if x.is_infinite() || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

/// Round to integer away from zero
pub fn roundf(d: f32) -> f32 {
    let mut x = d + 0.5;
    let mut fr = x - (x as i32 as f32);
    if (fr == 0.) && (x <= 0.) {
        x -= 1.
    };
    fr = if fr < 0. { fr + 1. } else { fr };
    x = if d == 0.499_999_970_197_677_612_3 {
        0.
    } else {
        x
    }; // nextafterf(0.5, 0)
    if d.is_infinite() || (fabsfk(d) >= F1_23) {
        d
    } else {
        copysignfk(x - fr, d)
    }
}

/// Round to integer, ties round to even
pub fn rintf(d: f32) -> f32 {
    let mut x = d + 0.5;
    let isodd = (1 & (x as i32)) != 0;
    let mut fr = x - (x as i32 as f32);
    fr = if (fr < 0.) || ((fr == 0.) && isodd) {
        fr + 1.
    } else {
        fr
    };
    x = if d == 0.500_000_059_604_644_775_39 {
        0.
    } else {
        x
    }; // nextafterf(0.5, 1)
    if d.is_infinite() || (fabsfk(d) >= F1_23) {
        d
    } else {
        copysignfk(x - fr, d)
    }
}

/// Integral and fractional value of FP number
pub fn modff(x: f32) -> (f32, f32) {
    let mut fr = x - (x as i32 as f32);
    fr = if fabsfk(x) > F1_23 { 0. } else { fr };
    (copysignfk(fr, x), copysignfk(x - fr, x))
}

/// Multiply by integral power of 2
///
/// These functions return the result of multiplying ***m*** by 2 raised to the power ***x***.
pub fn ldexpf(x: f32, mut exp: i32) -> f32 {
    if exp > 300 {
        exp = 300;
    }
    if exp < -300 {
        exp = -300;
    }

    let mut e0 = exp >> 2;
    if exp < 0 {
        e0 += 1;
    }
    if (-50 < exp) && (exp < 50) {
        e0 = 0;
    }
    let e1 = exp - (e0 << 2);

    let p = pow2if(e0);
    x * pow2if(e1) * p * p * p * p
}

/// Find the next representable FP value
pub fn nextafterf(x: f32, y: f32) -> f32 {
    let mut cxi = (if x == 0. { mulsignf(0., y) } else { x }).to_bits() as i32;
    let c = (cxi < 0) == (y < x);
    if c {
        cxi = -(cxi ^ (1 << 31));
    }

    if x != y {
        cxi -= 1;
    }

    if c {
        cxi = -(cxi ^ (1 << 31));
    }

    let cxf = f32::from_bits(cxi as u32);

    if x.is_nan() || y.is_nan() {
        f32::NAN
    } else if (x == 0.) && (y == 0.) {
        y
    } else if (cxf == 0.) && (x != 0.) {
        mulsignf(0., x)
    } else {
        cxf
    }
}

/// Fractional component of an FP number
pub fn frfrexpf(mut x: f32) -> f32 {
    if fabsfk(x) < f32::MIN_POSITIVE {
        x *= F1_30;
    }

    let mut cxu = x.to_bits();
    cxu &= !0x_7f80_0000_u32;
    cxu |= 0x_3f00_0000_u32;

    if x == 0. {
        x
    } else if x.is_infinite() {
        mulsignf(f32::INFINITY, x)
    } else {
        f32::from_bits(cxu)
    }
}

/// Exponent of an FP number
pub fn expfrexpf(mut x: f32) -> i32 {
    let mut ret = if fabsfk(x) < f32::MIN_POSITIVE {
        x *= F1_30;
        -30
    } else {
        0
    };

    ret += (((x.to_bits() >> 23) & 0xff) as i32) - 0x7e;

    if (x == 0.) || x.is_nan() || x.is_infinite() {
        0
    } else {
        ret
    }
}

/// FP remainder
pub fn fmodf(x: f32, y: f32) -> f32 {
    #[inline]
    fn toward0(d: f32) -> f32 {
        if d == 0. {
            0.
        } else {
            f32::from_bits(d.to_bits() - 1)
        }
    }

    #[inline]
    fn trunc_positive(x: f32) -> f32 {
        if fabsfk(x) >= F1_23 {
            x
        } else {
            x - (x - (x as i32 as f32))
        }
    }

    let mut nu = fabsfk(x);
    let mut de = fabsfk(y);
    let s = if de < f32::MIN_POSITIVE {
        nu *= F1_25;
        de *= F1_25;
        1. / F1_25
    } else {
        1.
    };

    let mut r = df(nu, 0.);
    let rde = toward0(1. / de);

    for _ in 0..8 {
        // ceil(log2(FLT_MAX) / 22)+1
        let q = if (de + de > r.0) && (r.0 >= de) {
            1.
        } else {
            toward0(r.0) * rde
        };
        r = (r + trunc_positive(q).mul_as_doubled(-de)).normalize();
        if r.0 < de {
            break;
        }
    }

    let mut ret = if r.0 + r.1 == de { 0. } else { (r.0 + r.1) * s };

    ret = mulsignf(ret, x);
    if de == 0. {
        f32::NAN
    } else if nu < de {
        x
    } else {
        ret
    }
}

/// Fused multiply and accumulate
///
/// This function compute (***x*** × ***y*** + ***z***) without rounding, and then return the rounded value of the result.
/// This function may return infinity with a correct sign if the absolute value of the correct return value is greater than 1e+33.
/// The error bounds of the returned value is max(0.500_01 ULP, f32::MIN_POSITIVE).
pub fn fmaf(mut x: f32, mut y: f32, mut z: f32) -> f32 {
    const C0: f32 = F1_25;
    const C1: f32 = C0 * C0;
    const C2: f32 = C1 * C1;

    let mut h2 = x * y + z;
    let q = if fabsfk(h2) < 1e-38 {
        x *= C1;
        y *= C1;
        z *= C2;
        1. / C2
    } else if fabsfk(h2) > 1e+38 {
        x *= 1. / C1;
        y *= 1. / C1;
        z *= 1. / C2;
        C2
    } else {
        1.
    };

    let mut d = x.mul_as_doubled(y);
    d += z;
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

/*
pub fn sqrtf(d: f32) -> f32 {
    SQRTF(d)
}
*/

#[inline]
fn logk3f(mut d: f32) -> f32 {
    let o = d < f32::MIN_POSITIVE;
    if o {
        d *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(d * (1. / 0.75));
    let m = ldexp3kf(d, -e);

    if o {
        e -= 64;
    }

    let x = (m - 1.) / (m + 1.);
    let x2 = x * x;

    let t = 0.239_282_846_450_805_664_062_5
        .mul_add(x2, 0.285_182_118_415_832_519_531_25)
        .mul_add(x2, 0.400_005_877_017_974_853_515_625)
        .mul_add(x2, 0.666_666_686_534_881_591_796_875)
        .mul_add(x2, 2.);

    x.mul_add(t, 0.693_147_180_559_945_286_226_764 * (e as f32))
}

#[inline]
fn expk3f(d: f32) -> f32 {
    let q = rintfk(d * R_LN2_F);

    let mut s = q.mul_add(-L2U_F, d);
    s = q.mul_add(-L2L_F, s);

    let mut u = 0.000_198_527_617_612_853_646_278_381
        .mul_add(s, 0.001_393_043_552_525_341_510_772_71)
        .mul_add(s, 0.008_333_360_776_305_198_669_433_59)
        .mul_add(s, 0.041_666_485_369_205_474_853_515_6)
        .mul_add(s, 0.166_666_671_633_720_397_949_219)
        .mul_add(s, 0.5);

    u = (s * s).mul_add(u, s + 1.);
    u = ldexpkf(u, q as i32);

    if d < -104. {
        0.
    } else {
        u
    }
}
