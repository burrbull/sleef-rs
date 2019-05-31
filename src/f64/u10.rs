//! Functions with 1.0 ULP error bound

use super::*;

pub fn acos(d: f64) -> f64 {
    let o = fabsk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
    let mut x = if o {
        dd(fabsk(d), 0.)
    } else {
        x2.sqrt_as_doubled()
    };
    x = if fabsk(d) == 1. { dd(0., 0.) } else { x };

    let x4 = x2 * x2;
    let x8 = x4 * x4;
    let x16 = x8 * x8;

    let u = f64::poly12(
        x2,
        x4,
        x8,
        x16,
        0.316_158_765_065_393_462_8_e-1,
        -0.158_191_824_332_999_664_3_e-1,
        0.192_904_547_726_791_067_4_e-1,
        0.660_607_747_627_717_061_e-2,
        0.121_536_052_557_737_733_1_e-1,
        0.138_871_518_450_160_921_8_e-1,
        0.173_595_699_122_361_460_4_e-1,
        0.223_717_618_193_204_834_1_e-1,
        0.303_819_592_803_813_223_7_e-1,
        0.446_428_568_137_710_243_8_e-1,
        0.750_000_000_037_858_161_1_e-1,
        0.166_666_666_666_649_754_3,
    ) * (x.0 * x2);

    let mut y = dd(
        3.141_592_653_589_793_116 / 2.,
        1.224_646_799_147_353_207_2_e-16 / 2.,
    )
    .sub_checked(mulsign(x.0, d).add_checked_as_doubled(mulsign(u, d)));
    x.add_checked_assign(u);
    y = if o { y } else { x.scale(2.) };
    if !o && (d < 0.) {
        y = dd(3.141_592_653_589_793_116, 1.224_646_799_147_353_207_2_e-16).sub_checked(y)
    };
    y.0 + y.1
}

pub fn atan(d: f64) -> f64 {
    let d2 = atan2k_u1(dd(fabsk(d), 0.), dd(1., 0.));
    let r = if d.is_infinite() {
        1.570_796_326_794_896_557_998_982
    } else {
        d2.0 + d2.1
    };
    mulsign(r, d)
}

fn atan2k_u1(mut y: Doubled<f64>, mut x: Doubled<f64>) -> Doubled<f64> {
    let mut q: isize = if x.0 < 0. {
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
    let mut t = s.square();
    t = t.normalize();

    let t2 = t.0 * t.0;
    let t4 = t2 * t2;
    let t8 = t4 * t4;

    let u = f64::poly16(
        t.0,
        t2,
        t4,
        t8,
        1.062_984_841_914_487_466_074_15_e-5,
        -0.000_125_620_649_967_286_867_384_336,
        0.000_705_576_642_963_934_123_897_74,
        -0.002_518_656_144_987_133_603_529_99,
        0.006_462_628_990_369_911_723_135_04,
        -0.012_828_133_366_339_903_101_427_4,
        0.020_802_479_992_414_579_790_249_7,
        -0.028_900_234_478_474_031_568_628_9,
        0.035_978_500_503_510_459_085_365_6,
        -0.041_848_579_703_592_507_506_027,
        0.047_084_301_165_328_398_819_376_3,
        -0.052_491_421_058_844_842_106_871_9,
        0.058_794_659_096_958_100_386_043_4,
        -0.066_662_088_477_879_549_719_418_2,
        0.076_922_533_029_620_376_865_409_5,
        -0.090_909_044_277_338_757_478_190_7,
    )
    .mul_add(t.0, 0.111_111_108_376_896_236_538_123)
    .mul_add(t.0, -0.142_857_142_756_268_568_062_339)
    .mul_add(t.0, 0.199_999_999_997_977_351_284_817)
    .mul_add(t.0, -0.333_333_333_333_317_605_173_818);

    t *= u;
    t = s * (1.).add_checked(t);
    if fabsk(s.0) < 1e-200 {
        t = s;
    }
    dd(
        1.570_796_326_794_896_557_998_982,
        6.123_233_995_736_766_035_868_82_e-17,
    ) * (q as f64)
        + t
}

pub fn atan2(mut y: f64, mut x: f64) -> f64 {
    if fabsk(x) < 5.562_684_646_268_008_398_4_e-309 {
        y *= D1_53;
        x *= D1_53;
    } // nexttoward((1.0 / DBL_MAX), 1)
    let d = atan2k_u1(dd(fabsk(y), 0.), dd(x, 0.));
    let mut r = d.0 + d.1;

    r = if y == 0. {
        if sign(x) == -1. {
            PI
        } else {
            0.
        }
    } else if y.is_infinite() {
        FRAC_PI_2
            - (if x.is_infinite() {
                sign(x) * FRAC_PI_4
            } else {
                0.
            })
    } else if x.is_infinite() || (x == 0.) {
        FRAC_PI_2
            - (if x.is_infinite() {
                sign(x) * FRAC_PI_2
            } else {
                0.
            })
    } else {
        mulsign(r, x)
    };
    if x.is_nan() || y.is_nan() {
        f64::NAN
    } else {
        mulsign(r, y)
    }
}

pub fn asin(d: f64) -> f64 {
    let o = fabsk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
    let mut x = if o {
        dd(fabsk(d), 0.)
    } else {
        x2.sqrt_as_doubled()
    };
    x = if fabsk(d) == 1.0 { dd(0., 0.) } else { x };

    let x4 = x2 * x2;
    let x8 = x4 * x4;
    let x16 = x8 * x8;

    let u = f64::poly12(
        x2,
        x4,
        x8,
        x16,
        0.316_158_765_065_393_462_8_e-1,
        -0.158_191_824_332_999_664_3_e-1,
        0.192_904_547_726_791_067_4_e-1,
        0.660_607_747_627_717_061_e-2,
        0.121_536_052_557_737_733_1_e-1,
        0.138_871_518_450_160_921_8_e-1,
        0.173_595_699_122_361_460_4_e-1,
        0.223_717_618_193_204_834_1_e-1,
        0.303_819_592_803_813_223_7_e-1,
        0.446_428_568_137_710_243_8_e-1,
        0.750_000_000_037_858_161_1_e-1,
        0.166_666_666_666_649_754_3,
    ) * (x2 * x.0);

    let y = dd(
        3.141_592_653_589_793_116 / 4.,
        1.224_646_799_147_353_207_2_e-16 / 4.,
    )
    .sub_checked(x)
    .add_checked(-u);
    let r = if o { u + x.0 } else { (y.0 + y.1) * 2. };
    mulsign(r, d)
}

pub fn sin(d: f64) -> f64 {
    let mut s: Doubled<f64>;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(d * FRAC_1_PI);
        ql = qlf as isize;
        s = qlf.mul_add(-PI_A2, d).add_checked_as_doubled(qlf * -PI_B2);
    } else if fabsk(d) < TRIGRANGEMAX {
        let dqh = trunck(d * (FRAC_1_PI / D1_24)) * D1_24;
        let qlf = rintk(d.mul_add(FRAC_1_PI, -dqh));
        ql = qlf as isize;

        s = dqh.mul_add(-PI_A, d).add_checked_as_doubled(qlf * -PI_A);
        s += dqh * -PI_B;
        s += qlf * -PI_B;
        s += dqh * -PI_C;
        s += qlf * -PI_C;
        s = s.add_checked((dqh + qlf) * -PI_D);
    } else {
        let (mut ddidd, ddii) = rempi(d);
        ql = (((ddii & 3) * 2 + ((ddidd.0 > 0.) as i32) + 1) >> 2) as isize;
        if (ddii & 1) != 0 {
            ddidd = ddidd
                + dd(
                    mulsign(3.141_592_653_589_793_116 * -0.5, ddidd.0),
                    mulsign(1.224_646_799_147_353_207_2_e-16 * -0.5, ddidd.0),
                );
        }
        s = ddidd.normalize();
        if d.is_infinite() || d.is_nan() {
            s.0 = f64::NAN;
        }
    }

    let t = s;
    s = s.square();

    let s2 = s.0 * s.0;
    let s4 = s2 * s2;

    let u = f64::poly6(
        s.0,
        s2,
        s4,
        2.720_524_161_385_295_679_179_83_e-15,
        -7.642_925_941_139_544_719_002_3_e-13,
        1.605_893_701_172_778_962_116_23_e-10,
        -2.505_210_681_484_312_335_936_8_e-8,
        2.755_731_921_044_282_247_773_79_e-6,
        -0.000_198_412_698_412_046_454_654_947,
    )
    .mul_add(s.0, 0.008_333_333_333_333_180_562_019_22);

    let x =
        (1.).add_checked((-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0) * s);

    let u = t.mul_as_f(x);

    if d.is_neg_zero() {
        d
    } else if (ql & 1) != 0 {
        -u
    } else {
        u
    }
}

#[test]
fn test_sin() {
    test_f_f(
        sin,
        if cfg!(feature = "std") {
            f64::sin
        } else {
            libm::sin
        },
        f64::MIN,
        f64::MAX,
        1.,
    );
}

pub fn cos(d: f64) -> f64 {
    let mut s: Doubled<f64>;
    let ql: isize;

    let d = fabsk(d);

    if d < TRIGRANGEMAX2 {
        ql = (2_f64).mul_add(rintk(d * FRAC_1_PI - 0.5), 1.) as isize;
        let qlf = ql as f64;
        s = d
            .add_as_doubled(qlf * (-PI_A2 * 0.5))
            .add_checked(qlf * (-PI_B2 * 0.5));
    } else if d < TRIGRANGEMAX {
        let mut dqh = trunck(d * (FRAC_1_PI / D1_23) - 0.5 * (FRAC_1_PI / D1_23));
        let qlf = 2. * rintk(d * FRAC_1_PI - 0.5 - dqh * D1_23) + 1.;
        ql = qlf as isize;
        dqh *= D1_24;

        let u = dqh.mul_add(-PI_A * 0.5, d);
        s = u.add_as_doubled(qlf * (-PI_A * 0.5));
        s += dqh * (-PI_B * 0.5);
        s += qlf * (-PI_B * 0.5);
        s += dqh * (-PI_C * 0.5);
        s = (s + qlf * (-PI_C * 0.5)).add_checked((dqh + qlf) * (-PI_D * 0.5));
    } else {
        let (mut ddidd, ddii) = rempi(d);
        ql = (((ddii & 3) * 2 + ((ddidd.0 > 0.) as i32) + 7) >> 1) as isize;
        if (ddii & 1) == 0 {
            ddidd = ddidd
                + dd(
                    mulsign(
                        3.141_592_653_589_793_116 * -0.5,
                        if ddidd.0 > 0. { 1. } else { -1. },
                    ),
                    mulsign(
                        1.224_646_799_147_353_207_2_e-16 * -0.5,
                        if ddidd.0 > 0. { 1. } else { -1. },
                    ),
                );
        }
        s = ddidd.normalize();
        if d.is_infinite() || d.is_nan() {
            s.0 = f64::NAN;
        }
    }

    let t = s;
    s = s.square();

    let s2 = s.0 * s.0;
    let s4 = s2 * s2;

    let u = f64::poly6(
        s.0,
        s2,
        s4,
        2.720_524_161_385_295_679_179_83_e-15,
        -7.642_925_941_139_544_719_002_3_e-13,
        1.605_893_701_172_778_962_116_23_e-10,
        -2.505_210_681_484_312_335_936_8_e-8,
        2.755_731_921_044_282_247_773_79_e-6,
        -0.000_198_412_698_412_046_454_654_947,
    )
    .mul_add(s.0, 0.008_333_333_333_333_180_562_019_22);

    let x =
        (1.).add_checked((-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0) * s);

    let u = t.mul_as_f(x);

    if ((ql as isize) & 2) == 0 {
        -u
    } else {
        u
    }
}

pub fn sincos(d: f64) -> (f64, f64) {
    let mut s: Doubled<f64>;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(d * (FRAC_2_PI));
        ql = qlf as isize;
        s = qlf
            .mul_add(-PI_A2 * 0.5, d)
            .add_checked_as_doubled(qlf * (-PI_B2 * 0.5));
    } else if fabsk(d) < TRIGRANGEMAX {
        let dqh = trunck(d * ((FRAC_2_PI) / D1_24)) * D1_24;
        let qlf = rintk(d * (FRAC_2_PI) - dqh);
        ql = qlf as isize;

        s = dqh
            .mul_add(-PI_A * 0.5, d)
            .add_checked_as_doubled(qlf * (-PI_A * 0.5));
        s += dqh * (-PI_B * 0.5);
        s += qlf * (-PI_B * 0.5);
        s += dqh * (-PI_C * 0.5);
        s = (s + qlf * (-PI_C * 0.5)).add_checked((dqh + qlf) * (-PI_D * 0.5));
    } else {
        let (ddidd, ddii) = rempi(d);
        ql = ddii as isize;
        s = ddidd;
        if d.is_infinite() || d.is_nan() {
            s = dd(f64::NAN, f64::NAN);
        }
    }

    let t = s;

    s.0 = s.square_as_f();

    let u = 1.589_383_072_832_289_373_285_11_e-10_f64
        .mul_add(s.0, -2.505_069_435_025_397_733_493_18_e-8)
        .mul_add(s.0, 2.755_731_317_768_463_605_125_47_e-6)
        .mul_add(s.0, -0.000_198_412_698_278_911_770_864_914)
        .mul_add(s.0, 0.008_333_333_333_319_184_596_174_6)
        .mul_add(s.0, -0.166_666_666_666_666_130_709_393)
        * s.0
        * t.0;

    let x = t.add_checked(u);
    let mut rsin = if d.is_neg_zero() { -0. } else { x.0 + x.1 };

    let u = (-1.136_153_502_390_974_295_315_23_e-11_f64)
        .mul_add(s.0, 2.087_574_712_070_400_554_793_66_e-9)
        .mul_add(s.0, -2.755_731_440_288_475_674_985_67_e-7)
        .mul_add(s.0, 2.480_158_728_900_018_673_119_15_e-5)
        .mul_add(s.0, -0.001_388_888_888_887_140_192_823_29)
        .mul_add(s.0, 0.041_666_666_666_666_551_959_206_2)
        .mul_add(s.0, -0.5);

    let x = (1.).add_checked(s.0.mul_as_doubled(u));
    let mut rcos = x.0 + x.1;

    if (ql & 1) != 0 {
        core::mem::swap(&mut rcos, &mut rsin);
    }
    if (ql & 2) != 0 {
        rsin = -rsin;
    }
    if ((ql + 1) & 2) != 0 {
        rcos = -rcos;
    }

    (rsin, rcos)
}

#[test]
fn test_sincos() {
    test_f_ff(
        sincos,
        if cfg!(feature = "std") {
            f64::sin_cos
        } else {
            libm::sincos
        },
        f64::MIN,
        f64::MAX,
        1.,
    );
}

pub fn tan(d: f64) -> f64 {
    let mut s: Doubled<f64>;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(d * (2. * FRAC_1_PI));
        ql = qlf as isize;
        s = qlf
            .mul_add(-PI_A2 * 0.5, d)
            .add_checked_as_doubled(qlf * (-PI_B2 * 0.5));
    } else if fabsk(d) < TRIGRANGEMAX {
        let dqh = trunck(d * (FRAC_2_PI / D1_24)) * D1_24;
        s = dd(M_2_PI_H, M_2_PI_L) * d + ((if d < 0. { -0.5 } else { 0.5 }) - dqh);
        ql = (s.0 + s.1) as isize;

        let qlf = ql as f64;

        s = dqh
            .mul_add(-PI_A * 0.5, d)
            .add_checked_as_doubled(qlf * (-PI_A * 0.5));
        s += dqh * (-PI_B * 0.5);
        s += qlf * (-PI_B * 0.5);
        s += dqh * (-PI_C * 0.5);
        s = (s + qlf * (-PI_C * 0.5)).add_checked((dqh + qlf) * (-PI_D * 0.5));
    } else {
        let (ddidd, ddii) = rempi(d);
        ql = ddii as isize;
        s = ddidd;
        if d.is_infinite() || d.is_nan() {
            s.0 = f64::NAN;
        }
    }

    let t = s.scale(0.5);
    s = t.square();

    let s2 = s.0 * s.0;
    let s4 = s2 * s2;

    let u = f64::poly8(
        s.0,
        s2,
        s4,
        0.324_509_882_663_927_631_6_e-3,
        0.561_921_973_811_432_373_5_e-3,
        0.146_078_150_240_278_449_4_e-2,
        0.359_161_154_079_249_951_9_e-2,
        0.886_326_840_956_311_312_6_e-2,
        0.218_694_872_818_553_549_8_e-1,
        0.539_682_539_951_727_297_e-1,
        0.133_333_333_333_050_058_1,
    )
    .mul_add(s.0, 0.333_333_333_333_334_369_5);

    let mut x = t.add_checked(s * t * u);

    let mut y = (-1.).add_checked(x.square());
    x = x.scale(-2.);

    if (ql & 1) != 0 {
        let t = x;
        x = y;
        y = -t;
    }

    x = x / y;

    if d.is_neg_zero() {
        d
    } else {
        x.0 + x.1
    }
}

pub fn log(mut d: f64) -> f64 {
    let o = d < f64::MIN_POSITIVE;
    if o {
        d *= D1_32 * D1_32;
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let x4 = x2 * x2;
    let x8 = x4 * x4;

    let t = f64::poly7(
        x2,
        x4,
        x8,
        0.153_207_698_850_270_135_3,
        0.152_562_905_100_342_871_6,
        0.181_860_593_293_778_599_6,
        0.222_221_451_983_938_000_9,
        0.285_714_293_279_429_931_7,
        0.399_999_999_963_525_199,
        0.666_666_666_666_733_354_1,
    );

    let s = (dd(
        0.693_147_180_559_945_286_226_764,
        2.319_046_813_846_299_558_417_771_e-17,
    ) * (e as f64))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x.0 * t);

    if d == 0. {
        f64::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f64::NAN
    } else if d.is_infinite() {
        f64::INFINITY
    } else {
        s.0 + s.1
    }
}

pub fn cbrt(d: f64) -> f64 {
    let mut q2 = dd(1., 0.);

    let e = ilogbk(fabsk(d)) + 1;
    let d = ldexp2k(d, -e);
    let r = (e + 6144) % 3;
    q2 = if r == 1 {
        dd(
            1.259_921_049_894_873_190_7,
            -2.589_933_375_300_506_917_7_e-17,
        )
    } else {
        q2
    };
    q2 = if r == 2 {
        dd(
            1.587_401_051_968_199_583_4,
            -1.086_900_819_419_782_298_6_e-16,
        )
    } else {
        q2
    };

    q2.0 = mulsign(q2.0, d);
    q2.1 = mulsign(q2.1, d);
    let d = fabsk(d);

    let mut x = (-0.640_245_898_480_692_909_870_982_f64)
        .mul_add(d, 2.961_551_030_200_395_118_185_95)
        .mul_add(d, -5.733_530_609_229_478_436_361_66)
        .mul_add(d, 6.039_903_689_894_587_479_614_07)
        .mul_add(d, -3.858_419_355_104_449_888_216_32)
        .mul_add(d, 2.230_727_530_249_660_972_572_2);

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
        mulsign(0., q2.0)
    } else if d.is_infinite() {
        mulsign(f64::INFINITY, q2.0)
    } else {
        ldexp2k(v.0 + v.1, (e + 6144) / 3 - 2048)
    }
}

pub fn tgamma(a: f64) -> f64 {
    let (da, db) = gammak(a);
    let y = expk2(da) * db;
    let r = y.0 + y.1;
    let r = if (a == f64::NEG_INFINITY)
        || ((a < 0.) && a.is_integer())
        || (a.is_finite() && (a < 0.) && r.is_nan())
    {
        f64::NAN
    } else {
        r
    };
    if ((a == f64::INFINITY) || a.is_finite())
        && a >= -f64::MIN_POSITIVE
        && ((a == 0.) || (a > 200.) || r.is_nan())
    {
        mulsign(f64::INFINITY, a)
    } else {
        r
    }
}

pub fn lgamma(a: f64) -> f64 {
    let (da, db) = gammak(a);
    let y = da + logk2(db.abs());
    let r = y.0 + y.1;
    if a.is_infinite() || ((a <= 0.) && a.is_integer()) || (a.is_finite() && r.is_nan()) {
        f64::INFINITY
    } else {
        r
    }
}

pub fn erf(a: f64) -> f64 {
    let s = a;

    let a = fabsk(a);
    let o0 = a < 1.;
    let o1 = a < 3.7;
    let o2 = a < 6.;
    let u = if o0 { a * a } else { a };

    let t = (if o0 {
        0.680_107_240_139_539_215_7_e-20_f64
    } else if o1 {
        0.283_095_452_208_771_766_e-13
    } else {
        -0.584_675_040_426_961_049_3_e-17
    })
    .mul_add(
        u,
        if o0 {
            -0.216_176_624_757_005_639_1_e-18
        } else if o1 {
            -0.150_949_194_617_948_194_e-11
        } else {
            0.607_669_104_881_260_789_8_e-15
        },
    )
    .mul_add(
        u,
        if o0 {
            0.469_591_917_330_159_875_2_e-17
        } else if o1 {
            0.382_785_717_780_717_315_2_e-10
        } else {
            -0.300_751_860_960_489_383_1_e-13
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.904_914_041_988_801_081_9_e-16
        } else if o1 {
            -0.613_973_392_155_898_724_1_e-9
        } else {
            0.942_790_626_082_464_606_3_e-12
        },
    )
    .mul_add(
        u,
        if o0 {
            0.163_401_890_355_741_151_7_e-14
        } else if o1 {
            0.698_538_793_460_803_882_4_e-8
        } else {
            -0.210_011_090_826_939_362_9_e-10
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.278_348_578_633_345_521_6_e-13
        } else if o1 {
            -0.598_822_451_303_437_147_4_e-7
        } else {
            0.353_463_952_346_122_347_3_e-9
        },
    )
    .mul_add(
        u,
        if o0 {
            0.446_322_127_678_641_272_2_e-12
        } else if o1 {
            0.400_571_695_235_534_664_e-6
        } else {
            -0.466_496_772_828_539_592_6_e-8
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.671_136_662_285_013_898_7_e-11
        } else if o1 {
            -0.213_219_010_457_578_44_e-5
        } else {
            0.494_382_328_376_900_053_2_e-7
        },
    )
    .mul_add(
        u,
        if o0 {
            0.942_275_905_023_265_834_6_e-10
        } else if o1 {
            0.909_246_130_404_263_032_5_e-5
        } else {
            -0.427_120_339_476_114_825_4_e-6
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.122_905_553_010_022_847_7_e-8
        } else if o1 {
            -0.307_918_808_096_620_545_7_e-4
        } else {
            0.303_406_767_740_491_589_5_e-5
        },
    )
    .mul_add(
        u,
        if o0 {
            0.148_071_928_158_508_502_3_e-7
        } else if o1 {
            0.797_141_344_308_237_076_2_e-4
        } else {
            -0.177_629_528_906_687_113_5_e-4
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.163_658_446_912_340_271_4_e-6
        } else if o1 {
            -0.138_785_321_522_544_286_4_e-3
        } else {
            0.852_454_763_055_950_505_e-4
        },
    )
    .mul_add(
        u,
        if o0 {
            0.164_621_143_658_892_336_3_e-5
        } else if o1 {
            0.646_967_802_625_759_096_5_e-4
        } else {
            -0.329_058_294_496_178_439_8_e-3
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.149_256_503_584_062_486_6_e-4
        } else if o1 {
            0.499_664_528_037_294_586_e-3
        } else {
            0.969_696_606_878_910_115_7_e-3
        },
    )
    .mul_add(
        u,
        if o0 {
            0.120_553_329_817_896_649_6_e-3
        } else if o1 {
            -0.162_280_248_284_252_053_5_e-2
        } else {
            -0.181_252_762_804_698_613_7_e-2
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.854_832_702_345_085_116_6_e-3
        } else if o1 {
            0.161_532_055_704_937_717_1_e-3
        } else {
            -0.472_540_982_812_361_901_7_e-3
        },
    )
    .mul_add(
        u,
        if o0 {
            0.522_397_762_544_218_879_9_e-2
        } else if o1 {
            0.191_526_232_557_487_560_7_e-1
        } else {
            0.209_031_542_792_422_926_6_e-1
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.268_661_706_451_312_556_9_e-1
        } else if o1 {
            -0.102_781_829_848_603_345_5
        } else {
            -0.105_204_192_184_277_664_5
        },
    )
    .mul_add(
        u,
        if o0 {
            0.112_837_916_709_551_275_3
        } else if o1 {
            -0.636_617_281_984_250_382_7
        } else {
            -0.634_535_180_876_656_834_7
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.376_126_389_031_837_538_0
        } else if o1 {
            -0.112_837_959_064_891_046_9_e+1
        } else {
            -0.112_944_292_910_352_439_6_e+1
        },
    );
    let mut d = t.mul_as_doubled(u);
    d += if o0 {
        dd(
            1.128_379_167_095_512_558_6,
            1.533_545_961_316_582_267_4_e-17,
        )
    } else if o1 {
        dd(
            3.411_064_473_619_613_758_7_e-8,
            -2.487_565_070_832_329_424_6_e-24,
        )
    } else {
        dd(
            0.000_249_630_356_905_264_382_85,
            -5.436_266_503_485_625_979_5_e-21,
        )
    };
    d = if o0 {
        d * a
    } else {
        (1.).add_checked(-expk2(d))
    };
    if a.is_nan() {
        f64::NAN
    } else {
        mulsign(if o2 { d.0 + d.1 } else { 1. }, s)
    }
}

pub fn exp(d: f64) -> f64 {
    let qf = rintk(d * R_LN2);
    let q = qf as i32;

    let s = qf.mul_add(-L2U, d);
    let s = qf.mul_add(-L2L, s);

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
    )
    .mul_add(s, 0.5);

    u = s * s * u + s + 1.;

    if d > 709.782_711_149_557_429_092_172_174_26 {
        f64::INFINITY
    } else if d < -1000. {
        0.
    } else {
        ldexp2k(u, q)
    }
}

pub fn pow(x: f64, y: f64) -> f64 {
    let yisint = y.is_integer();
    let yisodd = yisint && y.is_odd();

    let d = logk(fabsk(x)) * y;
    let mut result = if d.0 > 709.782_711_149_557_429_092_172_174_26 {
        f64::INFINITY
    } else {
        expk(d)
    };

    result = if result.is_nan() {
        f64::INFINITY
    } else {
        result
    };
    result *= if x > 0. {
        1.
    } else if !yisint {
        f64::NAN
    } else if yisodd {
        -1.
    } else {
        1.
    };

    let efx = mulsign(fabsk(x) - 1., y);
    if (y == 0.) || (x == 1.) {
        1.
    } else if y.is_infinite() {
        if efx < 0. {
            0.
        } else if efx == 0. {
            1.
        } else {
            f64::INFINITY
        }
    } else if x.is_infinite() || (x == 0.) {
        (if yisodd { sign(x) } else { 1. })
            * (if (if x == 0. { -y } else { y }) < 0. {
                0.
            } else {
                f64::INFINITY
            })
    } else if x.is_nan() || y.is_nan() {
        f64::NAN
    } else {
        result
    }
}

#[test]
fn test_pow() {
    test_ff_f(
        pow,
        if cfg!(feature = "std") {
            f64::powf
        } else {
            libm::pow
        },
        f64::MIN,
        f64::MAX,
        1.,
    );
}

pub fn sinh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let mut d = expk2(dd(y, 0.));
    d = d.sub_checked(d.recpre());
    y = (d.0 + d.1) * 0.5;

    y = if fabsk(x) > 710. { f64::INFINITY } else { y };
    y = if y.is_nan() { f64::INFINITY } else { y };
    y = mulsign(y, x);
    if x.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn cosh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let mut d = expk2(dd(y, 0.));
    d = d.add_checked(d.recpre());
    y = (d.0 + d.1) * 0.5;

    y = if fabsk(x) > 710. { f64::INFINITY } else { y };
    y = if y.is_nan() { f64::INFINITY } else { y };
    if x.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn asinh(x: f64) -> f64 {
    let mut y = fabsk(x);

    let mut d = if y > 1. { x.recpre() } else { dd(y, 0.) };
    d = (d.square() + 1.).sqrt();
    d = if y > 1. { d * y } else { d };

    d = logk2(d.add_checked(x).normalize());
    y = d.0 + d.1;

    y = if fabsk(x) > SQRT_DBL_MAX || y.is_nan() {
        mulsign(f64::INFINITY, x)
    } else {
        y
    };
    y = if x.is_nan() { f64::NAN } else { y };
    if x.is_neg_zero() {
        -0.
    } else {
        y
    }
}

pub fn acosh(x: f64) -> f64 {
    let d = logk2(x.add_as_doubled(1.).sqrt() * x.add_as_doubled(-1.).sqrt() + x);
    let mut y = d.0 + d.1;

    y = if (x > SQRT_DBL_MAX) || y.is_nan() {
        f64::INFINITY
    } else {
        y
    };
    y = if x == 1. { 0. } else { y };
    y = if x < 1. { f64::NAN } else { y };
    if x.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn atanh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let d = logk2((1.).add_as_doubled(y) / (1.).add_as_doubled(-y));
    y = if y > 1.0 {
        f64::NAN
    } else if y == 1.0 {
        f64::INFINITY
    } else {
        (d.0 + d.1) * 0.5
    };

    y = mulsign(y, x);
    if x.is_infinite() || y.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn exp2(d: f64) -> f64 {
    let q = rintk(d) as i32;

    let s = d - (q as f64);

    let s2 = s * s;
    let s4 = s2 * s2;
    let s8 = s4 * s4;

    let mut u = f64::poly10(
        s,
        s2,
        s4,
        s8,
        0.443_435_908_292_652_945_4_e-9,
        0.707_316_459_808_570_742_5_e-8,
        0.101_781_926_092_176_045_1_e-6,
        0.132_154_387_251_132_761_5_e-5,
        0.152_527_335_351_758_473_e-4,
        0.154_035_304_510_114_780_8_e-3,
        0.133_335_581_467_049_907_3_e-2,
        0.961_812_910_759_760_053_6_e-2,
        0.555_041_086_648_204_659_6_e-1,
        0.240_226_506_959_101_221_4,
    )
    .mul_add(s, 0.693_147_180_559_945_286_2);

    u = (1.).add_checked(u.mul_as_doubled(s)).normalize().0;

    u = ldexp2k(u, q);

    if d >= 1024. {
        f64::INFINITY
    } else if d < -2000. {
        0.
    } else {
        u
    }
}

pub fn exp10(d: f64) -> f64 {
    let q = rintk(d * LOG10_2) as i32;
    let qf = q as f64;
    let s = qf.mul_add(-L10U, d);
    let s = qf.mul_add(-L10L, s);

    let mut u = 0.241_146_349_833_426_765_2_e-3_f64
        .mul_add(s, 0.115_748_841_521_718_737_5_e-2)
        .mul_add(s, 0.501_397_554_678_973_365_9_e-2)
        .mul_add(s, 0.195_976_232_072_053_308_e-1)
        .mul_add(s, 0.680_893_639_944_678_413_8_e-1)
        .mul_add(s, 0.206_995_849_472_267_623_4)
        .mul_add(s, 0.539_382_929_205_853_622_9)
        .mul_add(s, 0.117_125_514_890_854_165_5_e+1)
        .mul_add(s, 0.203_467_859_229_343_295_3_e+1)
        .mul_add(s, 0.265_094_905_523_920_587_6_e+1)
        .mul_add(s, 0.230_258_509_299_404_590_1_e+1);
    u = (1.).add_checked(u.mul_as_doubled(s)).normalize().0;

    if d > 308.254_715_559_916_71 {
        f64::INFINITY // log10(DBL_MAX)
    } else if d < -350. {
        0.
    } else {
        ldexp2k(u, q)
    }
}

pub fn expm1(a: f64) -> f64 {
    let d = expk2(dd(a, 0.)) + (-1.0);
    if a.is_neg_zero() {
        -0.
    } else if a > 709.782_712_893_383_996_732_223 {
        f64::INFINITY // log(DBL_MAX)
    } else if a < -36.736_800_569_677_101_399_113_302_437 {
        -1. // log(1 - nexttoward(1, 0))
    } else {
        d.0 + d.1
    }
}

pub fn log2(mut d: f64) -> f64 {
    let o = d < f64::MIN_POSITIVE;
    if o {
        d *= D1_32 * D1_32;
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let x4 = x2 * x2;
    let x8 = x4 * x4;

    let t = f64::poly7(
        x2,
        x4,
        x8,
        0.221_194_175_045_608_149,
        0.220_076_869_315_227_768_9,
        0.262_370_805_748_851_465_6,
        0.320_597_747_794_449_550_2,
        0.412_198_594_548_532_470_9,
        0.577_078_016_299_705_898_2,
        0.961_796_693_926_080_914_49,
    );

    let s = (e as f64)
        + x * dd(2.885_390_081_777_926_774, 6.056_160_499_551_673_643_4_e-18)
        + x2 * x.0 * t;

    if d == 0. {
        f64::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f64::NAN
    } else if d.is_infinite() {
        f64::INFINITY
    } else {
        s.0 + s.1
    }
}

pub fn log1p(d: f64) -> f64 {
    let mut dp1 = d + 1.;

    let o = dp1 < f64::MIN_POSITIVE;
    if o {
        dp1 *= D1_32 * D1_32
    };

    let mut e = ilogb2k(dp1 * (1. / 0.75));

    let t = ldexp3k(1., -e);
    let m = d.mul_add(t, t - 1.);

    if o {
        e -= 64;
    }

    let x = dd(m, 0.) / (2.).add_checked_as_doubled(m);
    let x2 = x.0 * x.0;

    let x4 = x2 * x2;
    let x8 = x4 * x4;

    let t = f64::poly7(
        x2,
        x4,
        x8,
        0.153_207_698_850_270_135_3,
        0.152_562_905_100_342_871_6,
        0.181_860_593_293_778_599_6,
        0.222_221_451_983_938_000_9,
        0.285_714_293_279_429_931_7,
        0.399_999_999_963_525_199,
        0.666_666_666_666_733_354_1,
    );

    let s = (dd(
        0.693_147_180_559_945_286_226_764,
        2.319_046_813_846_299_558_417_771_e-17,
    ) * (e as f64))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x.0 * t);

    if d.is_neg_zero() {
        -0.
    } else if d == -1. {
        f64::NEG_INFINITY
    } else if (d < -1.) || d.is_nan() {
        f64::NAN
    } else if d > 1_e307 {
        f64::INFINITY
    } else {
        s.0 + s.1
    }
}

pub fn tanh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let mut d = expk2(dd(y, 0.));
    let e = d.recpre();
    d = d.sub_checked(e) / d.add_checked(e);
    y = d.0 + d.1;

    y = if fabsk(x) > 18.714_973_875 { 1. } else { y };
    y = if y.is_nan() { 1. } else { y };
    y = mulsign(y, x);
    if x.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn log10(mut d: f64) -> f64 {
    let o = d < f64::MIN_POSITIVE;
    if o {
        d *= D1_32 * D1_32;
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let x4 = x2 * x2;
    let x8 = x4 * x4;

    let t = f64::poly7(
        x2,
        x4,
        x8,
        0.665_372_581_957_675_846_e-1,
        0.662_572_278_282_083_371_2_e-1,
        0.789_810_521_431_394_407_8_e-1,
        0.965_095_503_571_527_513_2_e-1,
        0.124_084_140_972_144_499_3,
        0.173_717_792_745_460_508_6,
        0.289_529_654_602_197_261_7,
    );

    let s = (dd(
        0.301_029_995_663_981_198_02,
        -2.803_728_127_785_170_339_e-18,
    ) * (e as f64))
        .add_checked(
            x * dd(
                0.868_588_963_806_503_633_34,
                1.143_005_969_409_638_931_1_e-17,
            ),
        )
        .add_checked(x2 * x.0 * t);

    if d.is_infinite() {
        f64::INFINITY
    } else if (d < 0.) || d.is_nan() {
        f64::NAN
    } else if d == 0. {
        f64::NEG_INFINITY
    } else {
        s.0 + s.1
    }
}
