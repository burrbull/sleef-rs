//! Functions with 3.5 ULP error bound

use super::*;

/// Arc tangent function of two variables
///
/// These functions evaluates the arc tangent function of (***y*** / ***x***).
/// The quadrant of the result is determined according to the signs of ***x*** and ***y***.
/// The error bound of the returned value is 3.5 ULP.
pub fn atan2f(y: f32, x: f32) -> f32 {
    let mut r = atan2kf(fabsfk(y), x);

    r = if x.is_infinite() || (x == 0.) {
        FRAC_PI_2
            - (if x.is_infinite() {
                signf(x) * FRAC_PI_2
            } else {
                0.
            })
    } else if y.is_infinite() {
        FRAC_PI_2
            - (if x.is_infinite() {
                signf(x) * FRAC_PI_4
            } else {
                0.
            })
    } else if y == 0. {
        if signf(x) == -1. {
            PI
        } else {
            0.
        }
    } else {
        mulsignf(r, x)
    };

    if x.is_nan() || y.is_nan() {
        f32::NAN
    } else {
        mulsignf(r, y)
    }
}

/// Natural logarithmic function
///
/// These functions return the natural logarithm of ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn logf(mut d: f32) -> f32 {
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

    let t = 0.239_282_846_450_805_664_062_5_f32
        .mul_add(x2, 0.285_182_118_415_832_519_531_25)
        .mul_add(x2, 0.400_005_877_017_974_853_515_625)
        .mul_add(x2, 0.666_666_686_534_881_591_796_875)
        .mul_add(x2, 2.);

    if d == 0. {
        f32::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f32::NAN
    } else if d.is_infinite() {
        f32::INFINITY
    } else {
        x * t + 0.693_147_180_559_945_286_226_764 * (e as f32)
    }
}

/// Evaluate sin( π**a** ) and cos( π**a** ) for given **a** simultaneously
///
/// Evaluates the sine and cosine functions of πa at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// The error bound of the returned values is 3.5 ULP if ***a*** is in [-1e+7, 1e+7].
/// If a is a finite value out of this range, an arbitrary value within [-1, 1] is returned.
/// If a is a NaN or infinity, a NaN is returned.
pub fn sincospif(d: f32) -> (f32, f32) {
    let u = d * 4.;
    let q = ceilfk(u) & !1_i32;

    let s = u - (q as f32);
    let t = s;
    let s = s * s;

    let mut rsin = (-0.360_092_526_5_e-4_f32)
        .mul_add(s, 0.249_008_811_1_e-2)
        .mul_add(s, -0.807_455_107_6_e-1)
        .mul_add(s, 0.785_398_185_3)
        * t;

    let mut rcos = 0.353_981_522_5_e-5_f32
        .mul_add(s, -0.325_957_400_5_e-3)
        .mul_add(s, 0.158_543_158_3_e-1)
        .mul_add(s, -0.308_425_128_5)
        .mul_add(s, 1.);

    if (q & 2) != 0 {
        core::mem::swap(&mut rcos, &mut rsin);
    }
    if (q & 4) != 0 {
        rsin = -rsin;
    }
    if ((q + 2) & 4) != 0 {
        rcos = -rcos;
    }

    if fabsfk(d) > 1e+7 {
        rsin = 0.;
        rcos = 1.;
    }
    if d.is_infinite() {
        rsin = f32::NAN;
        rcos = f32::NAN;
    }

    (rsin, rcos)
}

/// Hyperbolic sine function
///
/// These functions evaluates the hyperbolic sine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP if ***a*** is in [-88, 88].
/// If ***a*** is a finite value out of this range, infinity with a correct sign
/// or a correct value with 3.5 ULP error bound is returned.
pub fn sinhf(x: f32) -> f32 {
    let e = expm1kf(fabsfk(x));
    let mut y = (e + 2.) / (e + 1.) * (0.5 * e);

    y = if fabsfk(x) > 88. { f32::INFINITY } else { y };
    y = if y.is_nan() { f32::INFINITY } else { y };
    y = mulsignf(y, x);
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

/// Hyperbolic cosine function
///
/// These functions evaluates the hyperbolic cosine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP if a is in [-88, 88].
/// If ***a*** is a finite value out of this range, infinity with a correct sign
/// or a correct value with 3.5 ULP error bound is returned.
pub fn coshf(x: f32) -> f32 {
    let e = u10::expf(fabsfk(x));
    let mut y = 0.5 * e + 0.5 / e;

    y = if fabsfk(x) > 88. { f32::INFINITY } else { y };
    y = if y.is_nan() { f32::INFINITY } else { y };
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

/// Hyperbolic tangent function
///
/// These functions evaluates the hyperbolic tangent function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP for the double-precision
/// function or 3.5 ULP for the single-precision function.
pub fn tanhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let d = expm1kf(2. * y);
    y = d / (d + 2.);

    y = if fabsfk(x) > 18.714_973_875 { 1. } else { y };
    y = if y.is_nan() { 1. } else { y };
    y = mulsignf(y, x);
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

/// 2D Euclidian distance function
///
/// The error bound of the returned value is 3.5 ULP.
pub fn hypotf(mut x: f32, mut y: f32) -> f32 {
    x = fabsfk(x);
    y = fabsfk(y);
    let min = x.min(y);
    let max = x.max(y);

    let t = min / max;
    if (x == f32::INFINITY) || (y == f32::INFINITY) {
        f32::INFINITY
    } else if x.is_nan() || y.is_nan() {
        f32::NAN
    } else if min == 0. {
        max
    } else {
        max * (1. + t * t).sqrt()
    }
}

/// Square root function
///
/// The error bound of the returned value is 3.5 ULP.
pub fn sqrtf(mut d: f32) -> f32 {
    let mut q = 1.;

    d = if d < 0. { f32::NAN } else { d };

    if d < 5.293_955_920_339_377_e-23 {
        d *= 1.888_946_593_147_858_e+22;
        q = 7.275_957_614_183_426_e-12;
    }

    if d > 1.844_674_407_370_955_2_e+19 {
        d *= 5.421_010_862_427_522_e-20;
        q = 4_294_967_296.;
    }

    // http://en.wikipedia.org/wiki/Fast_inverse_square_root
    let mut x = f32::from_bits(0x_5f37_5a86 - ((d + 1e-45).to_bits() >> 1));

    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;

    if d == f32::INFINITY {
        f32::INFINITY
    } else {
        x * d * q
    }
}

/// Sine function
///
/// These functions evaluates the sine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn sinf(mut d: f32) -> f32 {
    let q: i32;
    let t = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * FRAC_1_PI);
        q = qf as i32;
        d = qf.mul_add(-PI_A2_F, d);
        d = qf.mul_add(-PI_B2_F, d);
        d = qf.mul_add(-PI_C2_F, d);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * FRAC_1_PI);
        q = qf as i32;
        d = qf.mul_add(-PI_A_F, d);
        d = qf.mul_add(-PI_B_F, d);
        d = qf.mul_add(-PI_C_F, d);
        d = qf.mul_add(-PI_D_F, d);
    } else {
        let (mut dfidf, dfii) = rempif(t);
        q = ((dfii & 3) * 2 + ((dfidf.0 > 0.) as i32) + 1) >> 2;
        if (dfii & 1) != 0 {
            dfidf += df(
                mulsignf(3.141_592_741_012_573_242_2 * -0.5, dfidf.0),
                mulsignf(-8.742_277_657_347_585_773_1_e-8 * -0.5, dfidf.0),
            );
        }
        d = dfidf.0 + dfidf.1;
        if t.is_infinite() || t.is_nan() {
            d = f32::NAN;
        }
    }

    let s = d * d;

    if (q & 1) != 0 {
        d = -d;
    }

    let u = 2.608_315_980_978_659_354_150_3_e-6_f32
        .mul_add(s, -0.000_198_106_907_191_686_332_225_8)
        .mul_add(s, 0.008_333_078_585_565_090_179_443_36)
        .mul_add(s, -0.166_666_597_127_914_428_710_938);

    if t.is_neg_zero() {
        -0.
    } else {
        s.mul_add(u * d, d)
    }
}

/// Cosine function
///
/// These functions evaluates the cosine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn cosf(mut d: f32) -> f32 {
    let q: i32;
    let t = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        q = 1 + 2 * (rintfk(d * FRAC_1_PI - 0.5) as i32);
        let qf = q as f32;
        d = qf.mul_add(-PI_A2_F * 0.5, d);
        d = qf.mul_add(-PI_B2_F * 0.5, d);
        d = qf.mul_add(-PI_C2_F * 0.5, d);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        q = 1 + 2 * (rintfk(d * FRAC_1_PI - 0.5) as i32);
        let qf = q as f32;
        d = qf.mul_add(-PI_A_F * 0.5, d);
        d = qf.mul_add(-PI_B_F * 0.5, d);
        d = qf.mul_add(-PI_C_F * 0.5, d);
        d = qf.mul_add(-PI_D_F * 0.5, d);
    } else {
        let (mut dfidf, dfii) = rempif(t);
        q = ((dfii & 3) * 2 + ((dfidf.0 > 0.) as i32) + 7) >> 1;
        if (dfii & 1) == 0 {
            dfidf += df(
                mulsignf(
                    3.141_592_741_012_573_242_2 * -0.5,
                    if dfidf.0 > 0. { 1. } else { -1. },
                ),
                mulsignf(
                    -8.742_277_657_347_585_773_1_e-8 * -0.5,
                    if dfidf.0 > 0. { 1. } else { -1. },
                ),
            );
        }
        d = dfidf.0 + dfidf.1;
        if t.is_infinite() || t.is_nan() {
            d = f32::NAN;
        }
    }

    let s = d * d;

    if (q & 2) == 0 {
        d = -d;
    }

    let u = 2.608_315_980_978_659_354_150_3_e-6_f32
        .mul_add(s, -0.000_198_106_907_191_686_332_225_8)
        .mul_add(s, 0.008_333_078_585_565_090_179_443_36)
        .mul_add(s, -0.166_666_597_127_914_428_710_938);

    s.mul_add(u * d, d)
}

/// Evaluate sine and cosine function simultaneously
///
/// Evaluates the sine and cosine functions of a value in ***a*** at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// The error bound of the returned values is 3.5 ULP.
/// If ***a*** is a NaN or infinity, a NaN is returned.
pub fn sincosf(d: f32) -> (f32, f32) {
    let q: i32;

    let mut s = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * FRAC_2_PI);
        q = qf as i32;
        s = qf.mul_add(-PI_A2_F * 0.5, s);
        s = qf.mul_add(-PI_B2_F * 0.5, s);
        s = qf.mul_add(-PI_C2_F * 0.5, s);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * FRAC_2_PI);
        q = qf as i32;
        s = qf.mul_add(-PI_A_F * 0.5, s);
        s = qf.mul_add(-PI_B_F * 0.5, s);
        s = qf.mul_add(-PI_C_F * 0.5, s);
        s = qf.mul_add(-PI_D_F * 0.5, s);
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        s = dfidf.0 + dfidf.1;
        if d.is_infinite() || d.is_nan() {
            s = f32::NAN;
        }
    }

    let t = s;

    s = s * s;

    let mut u = (-0.000_195_169_282_960_705_459_117_889_f32)
        .mul_add(s, 0.008_332_157_507_538_795_471_191_41)
        .mul_add(s, -0.166_666_537_523_269_653_320_312);
    u = u * s * t;

    let mut rsin = if d.is_neg_zero() { -0. } else { t + u };

    u = (-2.718_118_423_672_422_068_193_55_e-7_f32)
        .mul_add(s, 2.479_904_469_510_074_704_885_48_e-5)
        .mul_add(s, -0.001_388_887_874_782_085_418_701_17)
        .mul_add(s, 0.041_666_664_183_139_801_025_390_6)
        .mul_add(s, -0.5);

    let mut rcos = u * s + 1.;

    if (q & 1) != 0 {
        core::mem::swap(&mut rcos, &mut rsin);
    }
    if (q & 2) != 0 {
        rsin = -rsin;
    }
    if ((q + 1) & 2) != 0 {
        rcos = -rcos;
    }

    (rsin, rcos)
}

/// Tangent function
///
/// These functions evaluates the tangent function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn tanf(d: f32) -> f32 {
    let q;

    let mut x = d;

    if fabsfk(d) < TRIGRANGEMAX2_F * 0.5 {
        let qf = rintfk(d * FRAC_2_PI);
        q = qf as i32;
        x = qf.mul_add(-PI_A2_F * 0.5, x);
        x = qf.mul_add(-PI_B2_F * 0.5, x);
        x = qf.mul_add(-PI_C2_F * 0.5, x);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * FRAC_2_PI);
        q = qf as i32;
        x = qf.mul_add(-PI_A_F * 0.5, x);
        x = qf.mul_add(-PI_B_F * 0.5, x);
        x = qf.mul_add(-PI_C_F * 0.5, x);
        x = qf.mul_add(-PI_D_F * 0.5, x);
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        x = dfidf.0 + dfidf.1;
        if d.is_infinite() || d.is_nan() {
            x = f32::NAN;
        }
    }

    let s = x * x;

    if (q & 1) != 0 {
        x = -x;
    }

    let s2 = s * s;
    let s4 = s2 * s2;

    let mut u = f32::poly6(
        s,
        s2,
        s4,
        0.009_272_458_031_773_567_199_707_03,
        0.003_319_849_958_643_317_222_595_21,
        0.024_299_807_846_546_173_095_703_1,
        0.053_449_530_154_466_629_028_320_3,
        0.133_383_005_857_467_651_367_188,
        0.333_331_853_151_321_411_132_812,
    );

    u = s.mul_add(u * x, x);

    if (q & 1) != 0 {
        1. / u
    } else {
        u
    }
}

/// Arc tangent function
///
/// These functions evaluates the arc tangent function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn atanf(mut s: f32) -> f32 {
    let mut q = if signf(s) == -1. {
        s = -s;
        2
    } else {
        0
    };

    if s > 1. {
        s = 1. / s;
        q |= 1;
    }

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

    t = s + s * (t * u);

    if (q & 1) != 0 {
        t = 1.570_796_326_794_896_557_998_982 - t;
    }
    if (q & 2) != 0 {
        -t
    } else {
        t
    }
}

/// Arc sine function
///
/// These functions evaluates the arc sine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn asinf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let x = if o { fabsfk(d) } else { x2.sqrt() };

    let u = 0.419_745_482_5_e-1_f32
        .mul_add(x2, 0.242_404_602_5_e-1)
        .mul_add(x2, 0.454_742_386_9_e-1)
        .mul_add(x2, 0.749_502_927_1_e-1)
        .mul_add(x2, 0.166_667_729_6)
        .mul_add(x * x2, x);

    let r = if o { u } else { FRAC_PI_2 - 2. * u };
    mulsignf(r, d)
}

/// Arc cosine function
///
/// These functions evaluates the arc cosine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn acosf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let mut x = if o { fabsfk(d) } else { x2.sqrt() };
    x = if fabsfk(d) == 1. { 0. } else { x };

    let mut u = 0.419_745_482_5_e-1_f32
        .mul_add(x2, 0.242_404_602_5_e-1)
        .mul_add(x2, 0.454_742_386_9_e-1)
        .mul_add(x2, 0.749_502_927_1_e-1)
        .mul_add(x2, 0.166_667_729_6);

    u *= x * x2;

    let y = 3.141_592_653_589_793_2 / 2. - (mulsignf(x, d) + mulsignf(u, d));
    x += u;
    let r = if o { y } else { x * 2. };
    if !o && (d < 0.) {
        df(
            3.141_592_741_012_573_242_2,
            -8.742_277_657_347_585_773_1_e-8,
        )
        .add_checked(-r)
        .0
    } else {
        r
    }
}

/// Cube root function
///
/// These functions return the real cube root of ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn cbrtf(mut d: f32) -> f32 {
    let e = ilogbkf(fabsfk(d)) + 1;
    d = ldexp2kf(d, -e);
    let r = (e + 6144) % 3;
    let mut q = if r == 1 {
        1.259_921_049_894_873_164_767_210_6
    } else {
        1.
    };
    q = if r == 2 {
        1.587_401_051_968_199_474_751_705_6
    } else {
        q
    };
    q = ldexp2kf(q, (e + 6144) / 3 - 2048);

    q = mulsignf(q, d);
    d = fabsfk(d);

    let x = (-0.601_564_466_953_277_587_890_625_f32)
        .mul_add(d, 2.820_889_234_542_846_679_687_5)
        .mul_add(d, -5.532_182_216_644_287_109_375)
        .mul_add(d, 5.898_262_500_762_939_453_125)
        .mul_add(d, -3.809_541_702_270_507_812_5)
        .mul_add(d, 2.224_125_623_703_002_929_687_5);

    let y = d * x * x;
    (y - (2. / 3.) * y * (y * x - 1.)) * q
}

pub fn exp2f(d: f32) -> f32 {
    let q = rintfk(d);

    let s = d - q;

    let mut u = 0.153_592_089_2_e-3
        .mul_add(s, 0.133_926_270_1_e-2)
        .mul_add(s, 0.961_838_476_4_e-2)
        .mul_add(s, 0.555_034_726_9_e-1)
        .mul_add(s, 0.240_226_447_6)
        .mul_add(s, 0.693_147_182_5)
        .mul_add(s, 0.1_e+1);

    u = ldexp2kf(u, q as i32);

    if d < -150. {
        0.
    } else if d >= 128. {
        f32::INFINITY
    } else {
        u
    }
}

pub fn exp10f(d: f32) -> f32 {
    let q = rintfk(d * LOG10_2_F);

    let mut s = q.mul_add(-L10U_F, d);
    s = q.mul_add(-L10L_F, s);

    let mut u = 0.206_400_498_7
        .mul_add(s, 0.541_787_743_6)
        .mul_add(s, 0.117_128_682_1_e+1)
        .mul_add(s, 0.203_465_604_8_e+1)
        .mul_add(s, 0.265_094_876_3_e+1)
        .mul_add(s, 0.230_258_512_5_e+1)
        .mul_add(s, 0.1_e+1);

    u = ldexp2kf(u, q as i32);

    if d < -50. {
        0.
    } else if d > 38.531_839_419_103_623_894_138_7 {
        f32::INFINITY // log10(FLT_MAX)
    } else {
        u
    }
}

pub fn log2f(mut d: f32) -> f32 {
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

    let t = 0.437_408_834_7
        .mul_add(x2, 0.576_484_382_2)
        .mul_add(x2, 0.961_802_423);

    let r = (x2 * x).mul_add(t, x.mul_add(0.288_539_004_3_e+1, e as f32));

    if d == 0. {
        f32::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f32::NAN
    } else if d.is_infinite() {
        f32::INFINITY
    } else {
        r
    }
}
