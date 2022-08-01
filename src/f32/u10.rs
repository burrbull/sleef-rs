//! Functions with 1.0 ULP error bound

use super::*;

/// Base-2 exponential function
///
/// This function returns 2 raised to ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn exp2f(d: f32) -> f32 {
    let qf = rintfk(d);
    let q = qf as i32;
    let s = d - qf;

    let mut u = 0.153_592_089_2_e-3_f32
        .mul_add(s, 0.133_926_270_1_e-2)
        .mul_add(s, 0.961_838_476_4_e-2)
        .mul_add(s, 0.555_034_726_9_e-1)
        .mul_add(s, 0.240_226_447_6)
        .mul_add(s, 0.693_147_182_5);
    u = (1.).add_checked(u.mul_as_doubled(s)).normalize().0;

    if d >= 128. {
        f32::INFINITY
    } else if d < -150. {
        0.
    } else {
        ldexp2kf(u, q)
    }
}

#[test]
fn test_exp2f() {
    test_f_f(exp2f, rug::Float::exp2, -150.0..=128.0, 1.);
}

/// Sine function
///
/// This function evaluates the sine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn sinf(d: f32) -> f32 {
    let q: i32;
    let mut s: Doubled<f32>;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * FRAC_1_PI);
        q = qf as i32;
        let u = qf.mul_add(-PI_A2_F, d);
        s = u.add_as_doubled(qf * (-PI_B2_F));
        s.add_checked_assign(qf * (-PI_C2_F));
    } else {
        let (mut dfidf, dfii) = rempif(d);
        q = ((dfii & 3) * 2 + ((dfidf.0 > 0.) as i32) + 1) >> 2;
        if (dfii & 1) != 0 {
            dfidf += df(
                mulsignf(3.141_592_741_012_573_242_2 * -0.5, dfidf.0),
                mulsignf(-8.742_277_657_347_585_773_1_e-8 * -0.5, dfidf.0),
            );
        }
        s = dfidf.normalize();
        if d.is_infinite() || d.is_nan() {
            s.0 = f32::NAN;
        }
    }

    let t = s;
    s = s.square();

    let mut u = 2.608_315_980_978_659_354_150_3_e-6_f32
        .mul_add(s.0, -0.000_198_106_907_191_686_332_225_8)
        .mul_add(s.0, 0.008_333_078_585_565_090_179_443_36);

    let x =
        (1.).add_checked((-0.166_666_597_127_914_428_710_938).add_checked_as_doubled(u * s.0) * s);

    u = t.mul_as_f(x);

    if (q & 1) != 0 {
        u = -u;
    }
    if d.is_neg_zero() {
        d
    } else {
        u
    }
}

#[test]
fn test_sinf() {
    test_f_f(sinf, rug::Float::sin, f32::MIN..=f32::MAX, 1.);
}

/// Cosine function
///
/// This function evaluates the cosine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn cosf(mut d: f32) -> f32 {
    let mut s: Doubled<f32>;
    let q: i32;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        d = fabsfk(d);
        let dq = rintfk(d * FRAC_1_PI - 0.5).mul_add(2., 1.);
        q = dq as i32;
        s = d.add_as_doubled(dq * (-PI_A2_F * 0.5));
        s += dq * (-PI_B2_F * 0.5);
        s += dq * (-PI_C2_F * 0.5);
    } else {
        let (mut dfidf, dfii) = rempif(d);
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
        s = dfidf.normalize();
        if d.is_infinite() || d.is_nan() {
            s.0 = f32::NAN;
        }
    }

    let t = s;
    s = s.square();

    let mut u = 2.608_315_980_978_659_354_150_3_e-6_f32
        .mul_add(s.0, -0.000_198_106_907_191_686_332_225_8)
        .mul_add(s.0, 0.008_333_078_585_565_090_179_443_36);

    let x =
        (1.).add_checked((-0.166_666_597_127_914_428_710_938).add_checked_as_doubled(u * s.0) * s);

    u = t.mul_as_f(x);

    if (q & 2) == 0 {
        -u
    } else {
        u
    }
}

#[test]
fn test_cosf() {
    test_f_f(cosf, rug::Float::cos, f32::MIN..=f32::MAX, 1.);
}

/// Evaluate sine and cosine functions simultaneously
///
/// Evaluates the sine and cosine functions of a value in a at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// returned value, respectively.
/// The error bound of the returned values is 1.0 ULP.
/// If ***a*** is a NaN or infinity, a NaN is returned.
pub fn sincosf(d: f32) -> (f32, f32) {
    let q: i32;
    let mut s: Doubled<f32>;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * FRAC_2_PI);
        q = qf as i32;
        let u = qf.mul_add(-PI_A2_F * 0.5, d);
        s = u.add_as_doubled(qf * (-PI_B2_F * 0.5));
        s.add_checked_assign(qf * (-PI_C2_F * 0.5));
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        s = dfidf;
        if d.is_infinite() || d.is_nan() {
            s.0 = f32::NAN;
        }
    }

    let t = s;
    s.0 = s.square_as_f();

    let u = (-0.000_195_169_282_960_705_459_117_889_f32)
        .mul_add(s.0, 0.008_332_157_507_538_795_471_191_41)
        .mul_add(s.0, -0.166_666_537_523_269_653_320_312)
        * s.0
        * t.0;

    let mut x = t.add_checked(u);
    let mut rsin = if d.is_neg_zero() { -0. } else { x.0 + x.1 };

    let u = (-2.718_118_423_672_422_068_193_55_e-7_f32)
        .mul_add(s.0, 2.479_904_469_510_074_704_885_48_e-5)
        .mul_add(s.0, -0.001_388_887_874_782_085_418_701_17)
        .mul_add(s.0, 0.041_666_664_183_139_801_025_390_6)
        .mul_add(s.0, -0.5);

    x = (1.).add_checked(s.0.mul_as_doubled(u));
    let mut rcos = x.0 + x.1;

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

#[test]
fn test_sincosf() {
    test_f_ff(
        sincosf,
        |in1| {
            let prec = in1.prec();
            in1.sin_cos(Float::new(prec))
        },
        f32::MIN..=f32::MAX,
        1.,
    );
}

/// Tangent function
///
/// This function evaluates the tangent function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn tanf(d: f32) -> f32 {
    let q: i32;
    let mut s: Doubled<f32>;
    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * FRAC_2_PI);
        q = qf as i32;
        let u = qf.mul_add(-PI_A2_F * 0.5, d);
        s = u.add_as_doubled(qf * (-PI_B2_F * 0.5));
        s.add_checked_assign(qf * (-PI_C2_F * 0.5));
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        s = dfidf;
        if d.is_infinite() || d.is_nan() {
            s.0 = f32::NAN;
        }
    }

    if (q & 1) != 0 {
        s = -s;
    }

    let t = s;
    s = s.square().normalize();

    let u = 0.004_466_364_625_841_379_165_649_41_f32
        .mul_add(s.0, -8.392_018_207_814_544_439_315_8_e-5)
        .mul_add(s.0, 0.010_963_924_229_145_050_048_828_1)
        .mul_add(s.0, 0.021_236_030_384_898_185_729_980_5)
        .mul_add(s.0, 0.054_068_714_380_264_282_226_562_5);

    let mut x = (0.133_325_666_189_193_725_585_938).add_checked_as_doubled(u * s.0);
    x = (1.).add_checked((0.333_333_611_488_342_285_156_25).add_checked(s * x) * s);
    x = t * x;

    if (q & 1) != 0 {
        x = x.recpre();
    }

    if d.is_neg_zero() {
        -0.
    } else {
        x.0 + x.1
    }
}

#[test]
fn test_tanf() {
    test_f_f(tanf, rug::Float::tan, f32::MIN..=f32::MAX, 1.);
}

fn atan2kf_u1(mut y: Doubled<f32>, mut x: Doubled<f32>) -> Doubled<f32> {
    let mut q = if x.0 < 0. {
        x.0 = -x.0;
        x.1 = -x.1;
        -2
    } else {
        0
    };

    if y.0 > x.0 {
        let t = x;
        x = y;
        y.0 = -t.0;
        y.1 = -t.1;
        q += 1;
    }

    let s = y / x;
    let mut t = s.square().normalize();

    let u = (-0.001_763_979_089_446_365_833_282_47_f32)
        .mul_add(t.0, 0.010_790_090_076_625_347_137_451_2)
        .mul_add(t.0, -0.030_956_460_162_997_245_788_574_2)
        .mul_add(t.0, 0.057_736_508_548_259_735_107_421_9)
        .mul_add(t.0, -0.083_895_072_340_965_270_996_093_8)
        .mul_add(t.0, 0.109_463_557_600_975_036_621_094)
        .mul_add(t.0, -0.142_626_821_994_781_494_140_625)
        .mul_add(t.0, 0.199_983_194_470_405_578_613_281);

    t = t * (-0.333_332_866_430_282_592_773_438).add_checked_as_doubled(u * t.0);
    t = s * (1.).add_checked(t);
    df(
        1.570_796_370_506_286_621_1,
        -4.371_138_828_673_792_886_5_e-8,
    ) * (q as f32)
        + t
}

/// Arc tangent function of two variables
///
/// This function evaluates the arc tangent function of (***y*** / ***x***).
/// The quadrant of the result is determined according to the signs
/// of ***x*** and ***y***.
/// The error bound of the returned values is `max(1.0 ULP, f32::MIN_POSITIVE)`.
pub fn atan2f(mut y: f32, mut x: f32) -> f32 {
    if fabsfk(x) < 2.938_737_278_354_183_094_7_e-39 {
        y *= F1_24;
        x *= F1_24;
    } // nexttowardf((1. / FLT_MAX), 1)
    let d = atan2kf_u1(df(fabsfk(y), 0.), df(x, 0.));
    let mut r = d.0 + d.1;

    r = mulsignf(r, x);
    r = if y == 0. {
        if signf(x) == -1. {
            PI
        } else {
            0.
        }
    } else if y.is_infinite() {
        FRAC_PI_2
            - (if x.is_infinite() {
                signf(x) * FRAC_PI_4
            } else {
                0.
            })
    } else if x.is_infinite() || (x == 0.) {
        FRAC_PI_2
            - (if x.is_infinite() {
                signf(x) * FRAC_PI_2
            } else {
                0.
            })
    } else {
        r
    };

    if x.is_nan() || y.is_nan() {
        f32::NAN
    } else {
        mulsignf(r, y)
    }
}

#[test]
fn test_atan2f() {
    test_ff_f(
        atan2f,
        rug::Float::atan2,
        f32::MIN..=f32::MAX,
        f32::MIN..=f32::MAX,
        1.,
    );
}

/// Arc sine function
///
/// This function evaluates the arc sine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn asinf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let mut x = if o {
        df(fabsfk(d), 0.)
    } else {
        x2.sqrt_as_doubled()
    };
    x = if fabsfk(d) == 1. { df(0., 0.) } else { x };

    let u = 0.419_745_482_5_e-1_f32
        .mul_add(x2, 0.242_404_602_5_e-1)
        .mul_add(x2, 0.454_742_386_9_e-1)
        .mul_add(x2, 0.749_502_927_1_e-1)
        .mul_add(x2, 0.166_667_729_6)
        * x2
        * x.0;

    let y = (df(
        3.141_592_741_012_573_242_2 / 4.,
        -8.742_277_657_347_585_773_1_e-8 / 4.,
    )
    .sub_checked(x))
    .add_checked(-u);
    let r = if o { u + x.0 } else { (y.0 + y.1) * 2. };
    mulsignf(r, d)
}

#[test]
fn test_asinf() {
    test_f_f(asinf, rug::Float::asin, -1.0..=1.0, 1.);
}

/// Arc cosine function
///
/// This function evaluates the arc cosine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn acosf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let mut x = if o {
        df(fabsfk(d), 0.)
    } else {
        x2.sqrt_as_doubled()
    };
    x = if fabsfk(d) == 1. { df(0., 0.) } else { x };

    let u = 0.419_745_482_5_e-1_f32
        .mul_add(x2, 0.242_404_602_5_e-1)
        .mul_add(x2, 0.454_742_386_9_e-1)
        .mul_add(x2, 0.749_502_927_1_e-1)
        .mul_add(x2, 0.166_667_729_6)
        * x.0
        * x2;

    let mut y = df(
        3.141_592_741_012_573_242_2 / 2.,
        -8.742_277_657_347_585_773_1_e-8 / 2.,
    )
    .sub_checked(mulsignf(x.0, d).add_checked_as_doubled(mulsignf(u, d)));
    x.add_checked_assign(u);
    y = if o { y } else { x.scale(2.) };
    if !o && (d < 0.) {
        y = df(
            3.141_592_741_012_573_242_2,
            -8.742_277_657_347_585_773_1_e-8,
        )
        .sub_checked(y);
    }

    y.0 + y.1
}

#[test]
fn test_acosf() {
    test_f_f(acosf, rug::Float::acos, -1.0..=1.0, 1.);
}

/// Arc tangent function
///
/// This function evaluates the arc tangent function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn atanf(d: f32) -> f32 {
    let d2 = atan2kf_u1(df(fabsfk(d), 0.), df(1., 0.));
    let r = if d.is_infinite() {
        1.570_796_326_794_896_557_998_982
    } else {
        d2.0 + d2.1
    };
    mulsignf(r, d)
}

#[test]
fn test_atanf() {
    test_f_f(atanf, rug::Float::atan, f32::MIN..=f32::MAX, 1.);
}

/// Natural logarithmic function
///
/// This function returns the natural logarithm of ***a***.
/// The error bound of the returned value is 1.0 ULP.
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

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let t = 0.302_729_487_4_f32
        .mul_add(x2, 0.399_610_817_4)
        .mul_add(x2, 0.666_669_488);

    let s = (df(0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x.0 * t);

    if d == 0. {
        f32::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f32::NAN
    } else if d.is_infinite() {
        f32::INFINITY
    } else {
        s.0 + s.1
    }
}

#[test]
fn test_logf() {
    test_f_f(logf, rug::Float::ln, 0.0..=f32::MAX, 1.);
}

/// Cube root function
///
/// This function returns the real cube root of ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn cbrtf(mut d: f32) -> f32 {
    let e = ilogbkf(fabsfk(d)) + 1;
    d = ldexp2kf(d, -e);
    let r = (e + 6144) % 3;
    let mut q2 = if r == 1 {
        df(
            1.259_921_073_913_574_218_8,
            -2.401_870_169_421_727_041_5_e-8,
        )
    } else {
        df(1., 0.)
    };
    q2 = if r == 2 {
        df(1.587_401_032_447_814_941_4, 1.952_038_530_816_935_235_6_e-8)
    } else {
        q2
    };

    q2.0 = mulsignf(q2.0, d);
    q2.1 = mulsignf(q2.1, d);
    d = fabsfk(d);

    let mut x = (-0.601_564_466_953_277_587_890_625_f32)
        .mul_add(d, 2.820_889_234_542_846_679_687_5)
        .mul_add(d, -5.532_182_216_644_287_109_375)
        .mul_add(d, 5.898_262_500_762_939_453_125)
        .mul_add(d, -3.809_541_702_270_507_812_5)
        .mul_add(d, 2.224_125_623_703_002_929_687_5);

    let mut y = x * x;
    y = y * y;
    x -= (d * y - x) * (1. / 3.);

    let z = x;

    let mut u = x.mul_as_doubled(x);
    u = u * u * d + (-x);
    y = u.0 + u.1;

    y = -2. / 3. * y * z;
    let v = (z.mul_as_doubled(z) + y) * d * q2;

    if d == 0. {
        mulsignf(0., q2.0)
    } else if d.is_infinite() {
        mulsignf(f32::INFINITY, q2.0)
    } else {
        ldexp2kf(v.0 + v.1, (e + 6144) / 3 - 2048)
    }
}

#[test]
fn test_cbrtf() {
    test_f_f(cbrtf, rug::Float::cbrt, f32::MIN..=f32::MAX, 1.);
}

/// Gamma function
///
/// The error bound of the returned value is 1.0 ULP.
pub fn tgammaf(a: f32) -> f32 {
    let (da, db) = gammafk(a);
    let y = expk2f(da) * db;
    let mut r = y.0 + y.1;
    r = if ((a == f32::NEG_INFINITY) || ((a < 0.) && a.is_integer()))
        || (a.is_finite() && (a < 0.) && r.is_nan())
    {
        f32::NAN
    } else {
        r
    };
    if ((a == f32::INFINITY) || a.is_finite())
        && (a >= -f32::MIN_POSITIVE)
        && ((a == 0.) || (a > 36.) || r.is_nan())
    {
        mulsignf(f32::INFINITY, a)
    } else {
        r
    }
}

#[test]
fn test_tgammaf() {
    test_f_f(tgammaf, rug::Float::gamma, f32::MIN..=f32::MAX, 1.0);
}

/// Log gamma function
///
/// The error bound of the returned value is 1.0 ULP if the argument is positive.
/// If the argument is larger than 4e+36, it may return infinity instead of the correct value.
/// The error bound is `max(1 ULP and 1e-8)`, if the argument is negative.
pub fn lgammaf(a: f32) -> f32 {
    let (da, db) = gammafk(a);
    let y = da + logk2f(db.abs());
    let r = y.0 + y.1;
    if a.is_infinite() || (a <= 0. && a.is_integer()) || (a.is_finite() && r.is_nan()) {
        f32::INFINITY
    } else {
        r
    }
}

#[test]
fn test_lgammaf() {
    test_f_f(lgammaf, rug::Float::ln_gamma, 0.0..=4e36, 1.0);
}

/// Error function
///
/// The error bound of the returned value is 1.0 ULP.
pub fn erff(mut a: f32) -> f32 {
    let s = a;

    a = fabsfk(a);
    let o0 = a < 1.1;
    let o1 = a < 2.4;
    let o2 = a < 4.0;
    let mut u = if o0 { a * a } else { a };

    let t = if o0 {
        0.708_929_219_4_e-4_f32
    } else if o1 {
        -0.179_266_789_9_e-4
    } else {
        -0.949_575_769_5_e-5
    }
    .mul_add(
        u,
        if o0 {
            -0.776_831_118_9_e-3
        } else if o1 {
            0.393_763_301_e-3
        } else {
            0.248_146_592_6_e-3
        },
    )
    .mul_add(
        u,
        if o0 {
            0.515_946_373_3_e-2
        } else if o1 {
            -0.394_918_117_7_e-2
        } else {
            -0.291_817_681_9_e-2
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.268_378_127_4_e-1
        } else if o1 {
            0.244_547_464_e-1
        } else {
            0.205_970_667_3_e-1
        },
    )
    .mul_add(
        u,
        if o0 {
            0.112_831_801_2
        } else if o1 {
            -0.107_099_615_0
        } else {
            -0.990_189_984_4_e-1
        },
    );
    let mut d = t.mul_as_doubled(u);
    d += if o0 {
        df(-0.376_125_876_000_657_465_175_213_237_214, 0.)
    } else if o1 {
        df(-0.634_588_905_908_410_389_971_210_809_210, 0.)
    } else {
        df(-0.643_598_050_547_891_613_081_201_721_633, 0.)
    };
    d *= u;
    d += if o0 {
        df(0.112_837_916_021_059_138_255_978_217_023_e+1, 0.)
    } else if o1 {
        df(-0.112_879_855_826_694_507_209_862_753_992_e+1, 0.)
    } else {
        df(-0.112_461_487_742_845_562_801_052_956_293_e+1, 0.)
    };
    d *= a;
    d = if o0 { d } else { (1.).add_checked(-expk2f(d)) };
    u = mulsignf(if o2 { d.0 + d.1 } else { 1. }, s);
    if a.is_nan() {
        f32::NAN
    } else {
        u
    }
}

#[test]
fn test_erff() {
    test_f_f(erff, rug::Float::erf, f32::MIN..=f32::MAX, 0.75);
}

/// Base-*e* exponential function
///
/// This function returns the value of *e* raised to ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn expf(d: f32) -> f32 {
    let qf = rintfk(d * R_LN2_F);
    let q = qf as i32;
    let s = qf.mul_add(-L2U_F, d);
    let s = qf.mul_add(-L2L_F, s);

    let mut u = 0.000_198_527_617_612_853_646_278_381_f32
        .mul_add(s, 0.001_393_043_552_525_341_510_772_71)
        .mul_add(s, 0.008_333_360_776_305_198_669_433_59)
        .mul_add(s, 0.041_666_485_369_205_474_853_515_6)
        .mul_add(s, 0.166_666_671_633_720_397_949_219)
        .mul_add(s, 0.5);

    u = s * s * u + s + 1.;

    if d < -104. {
        0.
    } else if d > 104. {
        f32::INFINITY
    } else {
        ldexp2kf(u, q)
    }
}

#[test]
fn test_expf() {
    test_f_f(expf, rug::Float::exp, -104.0..=100.0, 1.);
}

/// Power function
///
/// This function returns the value of ***x*** raised to the power of ***y***.
/// The error bound of the returned value is 1.0 ULP.
pub fn powf(x: f32, y: f32) -> f32 {
    let yisint = (y == (y as i32 as f32)) || (fabsfk(y) >= F1_24);
    let yisodd = ((1 & (y as i32)) != 0) && yisint && (fabsfk(y) < F1_24);

    let mut result = expkf(logkf(fabsfk(x)) * y);

    result = if result.is_nan() {
        f32::INFINITY
    } else {
        result
    };
    result *= if x >= 0. {
        1.
    } else if !yisint {
        f32::NAN
    } else if yisodd {
        -1.
    } else {
        1.
    };

    let efx = mulsignf(fabsfk(x) - 1., y);
    if (y == 0.) || (x == 1.) {
        1.
    } else if x.is_nan() || y.is_nan() {
        f32::NAN
    } else if x.is_infinite() || (x == 0.) {
        (if yisodd { signf(x) } else { 1. })
            * (if (if x == 0. { -y } else { y }) < 0. {
                0.
            } else {
                f32::INFINITY
            })
    } else if y.is_infinite() {
        if efx < 0. {
            0.
        } else if efx == 0. {
            1.
        } else {
            f32::INFINITY
        }
    } else {
        result
    }
}

#[test]
fn test_powf() {
    use rug::{ops::Pow, Float};
    test_ff_f(
        powf,
        |in1, in2| Float::with_val(in1.prec(), in1.pow(in2)),
        f32::MIN..=f32::MAX,
        f32::MIN..=f32::MAX,
        1.,
    );
}

/// Hyperbolic sine function
///
/// This function evaluates the hyperbolic sine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP if ***a*** is in
/// `[-709, 709]` for the double-precision function or `[-88.5, 88.5]`
/// for the single-precision function.
/// If ***a*** is a finite value out of this range, infinity with a correct
/// sign or a correct value with 1.0 ULP error bound is returned.
pub fn sinhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    d = d.sub_checked(d.recpre());
    y = (d.0 + d.1) * 0.5;

    y = if fabsfk(x) > 89. { f32::INFINITY } else { y };
    y = if y.is_nan() { f32::INFINITY } else { y };
    y = mulsignf(y, x);
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

#[test]
fn test_sinhf() {
    test_f_f(sinhf, rug::Float::sinh, -88.5..=88.5, 1.);
}

/// Hyperbolic cosine function
///
/// This function evaluates the hyperbolic cosine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP if ***a** is in
/// `[-709, 709]` for the double-precision function or `[-88.5, 88.5]`
/// for the single-precision function.
/// If a is a finite value out of this range, infinity with a correct
/// sign or a correct value with 1.0 ULP error bound is returned.
pub fn coshf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    d = d.add_checked(d.recpre());
    y = (d.0 + d.1) * 0.5;

    y = if fabsfk(x) > 89. { f32::INFINITY } else { y };
    y = if y.is_nan() { f32::INFINITY } else { y };
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

#[test]
fn test_coshf() {
    test_f_f(coshf, rug::Float::cosh, -88.5..=88.5, 1.);
}

/// Hyperbolic tangent function
///
/// This function evaluates the hyperbolic tangent function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP for the double-precision
/// function or 1.0001 ULP for the single-precision function.
pub fn tanhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    let e = d.recpre();
    d = d.sub_checked(e) / d.add_checked(e);
    y = d.0 + d.1;

    y = if fabsfk(x) > 18.714_973_875 { 1. } else { y }; // TODO: check
    y = if y.is_nan() { 1. } else { y };
    y = mulsignf(y, x);
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

#[test]
fn test_tanhf() {
    test_f_f(tanhf, rug::Float::tanh, -8.7..=8.7, 1.0001);
}

/// Inverse hyperbolic sine function
///
/// This function evaluates the inverse hyperbolic sine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP if a is in
/// `[-1.34e+154, 1.34e+154]` for the double-precision function or 1.001 ULP
/// if ***a*** is in `[-1.84e+19, 1.84e+19]` for the single-precision function.
/// If ***a*** is a finite value out of this range, infinity with a correct
/// sign or a correct value with 1.0 ULP error bound is returned.
pub fn asinhf(x: f32) -> f32 {
    let mut y = fabsfk(x);

    let mut d = if y > 1. { x.recpre() } else { df(y, 0.) };
    d = (d.square() + 1.).sqrt();
    d = if y > 1. { d * y } else { d };

    d = logk2f(d.add_checked(x).normalize());
    y = d.0 + d.1;

    y = if fabsfk(x) > SQRT_FLT_MAX || y.is_nan() {
        mulsignf(f32::INFINITY, x)
    } else {
        y
    };
    y = if x.is_nan() { f32::NAN } else { y };
    if x.is_neg_zero() {
        -0.
    } else {
        y
    }
}

#[test]
fn test_asinhf() {
    test_f_f(
        asinhf,
        rug::Float::asinh,
        -SQRT_FLT_MAX..=SQRT_FLT_MAX,
        1.0001,
    );
}

/// Inverse hyperbolic cosine function
///
/// This function evaluates the inverse hyperbolic cosine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP if a is in
/// `[-1.34e+154, 1.34e+154]` for the double-precision function or 1.001 ULP
/// if ***a*** is in `[-1.84e+19, 1.84e+19]` for the single-precision function.
/// If ***a*** is a finite value out of this range, infinity with a correct
/// sign or a correct value with 1.0 ULP error bound is returned.
pub fn acoshf(x: f32) -> f32 {
    let d = logk2f((x.add_as_doubled(1.)).sqrt() * (x.add_as_doubled(-1.)).sqrt() + x);
    let mut y = d.0 + d.1;

    y = if (x > SQRT_FLT_MAX) || y.is_nan() {
        f32::INFINITY
    } else {
        y
    };
    y = if x == 1. { 0. } else { y };
    y = if x < 1. { f32::NAN } else { y };
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

#[test]
fn test_acoshf() {
    test_f_f(
        acoshf,
        rug::Float::acosh,
        -SQRT_FLT_MAX..=SQRT_FLT_MAX,
        1.0001,
    );
}

/// Inverse hyperbolic tangent function
///
/// This function evaluates the inverse hyperbolic tangent function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP for the double-precision
/// function or 1.0001 ULP for the single-precision function.
pub fn atanhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let d = logk2f((1.).add_as_doubled(y) / (1.).add_as_doubled(-y));
    y = if y > 1. {
        f32::NAN
    } else if y == 1. {
        f32::INFINITY
    } else {
        (d.0 + d.1) * 0.5
    };

    y = if x.is_infinite() || y.is_nan() {
        f32::NAN
    } else {
        y
    };
    y = mulsignf(y, x);
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

#[test]
fn test_atanhf() {
    test_f_f(atanhf, rug::Float::atanh, f32::MIN..=f32::MAX, 1.0001);
}

/// Base-10 exponential function
///
/// This function returns 10 raised to ***a***.
/// The error bound of the returned value is 1.09 ULP.
pub fn exp10f(d: f32) -> f32 {
    let qf = rintfk(d * LOG10_2_F);

    let q = qf as i32;
    let s = qf.mul_add(-L10U_F, d);
    let s = qf.mul_add(-L10L_F, s);

    let mut u = 0.680_255_591_9_e-1
        .mul_add(s, 0.207_808_032_6)
        .mul_add(s, 0.539_390_385_2)
        .mul_add(s, 0.117_124_533_7_e+1)
        .mul_add(s, 0.203_467_869_8_e+1)
        .mul_add(s, 0.265_094_900_1_e+1);
    let x = df(2.3025851249694824219, -3.1705172516493593157e-08).add_checked(u * s);
    u = (1.).add_checked(x * s).normalize().0;

    if d > 38.531_839_419_103_623_894_138_7 {
        f32::INFINITY // log10(FLT_MAX)
    } else if d < -50. {
        0.
    } else {
        ldexp2kf(u, q)
    }
}

#[test]
fn test_exp10f() {
    test_f_f(exp10f, rug::Float::exp10, -50.0..=38.54, 1.);
}

/// Base-*e* exponential function minus 1
///
/// This function returns the value one less than *e* raised to ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn expm1f(a: f32) -> f32 {
    let d = expk2f(df(a, 0.)) + (-1.);
    if a.is_neg_zero() {
        -0.
    } else if a < -16.635_532_333_438_687_426_013_570 {
        -1.
    } else if a > 88.722_831_726_074_218_75 {
        f32::INFINITY
    } else {
        d.0 + d.1
    }
}

#[test]
fn test_expm1f() {
    test_f_f(expm1f, rug::Float::exp_m1, -16.64..=88.73, 1.);
}

/// Base-10 logarithmic function
///
/// This function returns the base-10 logarithm of ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn log10f(mut d: f32) -> f32 {
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
    let x2 = x.0 * x.0;

    let t = 0.131_428_986_8_f32
        .mul_add(x2, 0.173_549_354_1)
        .mul_add(x2, 0.289_530_962_7);

    let s = (df(0.301_030_01, -1.432_098_889_e-8) * (e as f32))
        .add_checked(x * df(0.868_588_984, -2.170_757_285_e-8))
        .add_checked(x2 * x.0 * t);

    if d == 0. {
        f32::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f32::NAN
    } else if d.is_infinite() {
        f32::INFINITY
    } else {
        s.0 + s.1
    }
}

#[test]
fn test_log10f() {
    test_f_f(log10f, rug::Float::log10, 0.0..=f32::MAX, 1.);
}

/// Base-2 logarithmic function
///
/// This function returns the base-2 logarithm of ***a***.
/// The error bound of the returned value is 1.0 ULP.
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

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let t = 0.437_455_028_3_f32
        .mul_add(x2, 0.576_479_017_7)
        .mul_add(x2, 0.961_801_290_512);

    let mut s = (e as f32) + x * df(2.885_390_043_258_666_992_2, 3.273_447_448_356_848_861_6_e-8);
    s += x2 * x.0 * t;

    if d == 0. {
        f32::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f32::NAN
    } else if d.is_infinite() {
        f32::INFINITY
    } else {
        s.0 + s.1
    }
}

#[test]
fn test_log2f() {
    test_f_f(log2f, rug::Float::log2, 0.0..=f32::MAX, 1.);
}

/// Logarithm of one plus argument
///
/// This function returns the natural logarithm of (1+***a***).
/// The error bound of the returned value is 1.0 ULP.
pub fn log1pf(d: f32) -> f32 {
    let mut dp1 = d + 1.;

    let o = dp1 < f32::MIN_POSITIVE;
    if o {
        dp1 *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(dp1 * (1. / 0.75));

    let t = ldexp3kf(1., -e);
    let m = d.mul_add(t, t - 1.);

    if o {
        e -= 64;
    }

    let x = df(m, 0.) / (2.).add_checked_as_doubled(m);
    let x2 = x.0 * x.0;

    let t = 0.302_729_487_4_f32
        .mul_add(x2, 0.399_610_817_4)
        .mul_add(x2, 0.666_669_488);

    let s = (df(0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x.0 * t);

    if d.is_neg_zero() {
        -0.
    } else if d == -1. {
        f32::NEG_INFINITY
    } else if d < -1. {
        f32::NAN
    } else if d > 1e+38 {
        f32::INFINITY
    } else {
        s.0 + s.1
    }
}

#[test]
fn test_log1pf() {
    test_f_f(log1pf, rug::Float::ln_1p, -1.0..=1e+38, 1.);
}
