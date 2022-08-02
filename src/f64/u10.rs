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

#[test]
fn test_acos() {
    test_f_f(acos, rug::Float::acos, -1.0..=1.0, 1.);
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

#[test]
fn test_atan() {
    test_f_f(atan, rug::Float::atan, f64::MIN..=f64::MAX, 1.);
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

#[test]
fn test_atan2() {
    test_ff_f(
        atan2,
        rug::Float::atan2,
        f64::MIN..=f64::MAX,
        f64::MIN..=f64::MAX,
        1.,
    );
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

#[test]
fn test_asin() {
    test_f_f(asin, rug::Float::asin, -1.0..=1.0, 1.);
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
    super::test_f_f(sin, rug::Float::sin, f64::MIN..=f64::MAX, 1.);
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

#[test]
fn test_cos() {
    test_f_f(cos, rug::Float::cos, f64::MIN..=f64::MAX, 1.);
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
        |in1| {
            let prec = in1.prec();
            in1.sin_cos(Float::new(prec))
        },
        f64::MIN..=f64::MAX,
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

#[test]
fn test_tan() {
    test_f_f(tan, rug::Float::tan, f64::MIN..=f64::MAX, 1.);
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

#[test]
fn test_log() {
    test_f_f(log, rug::Float::ln, 0.0..=f64::MAX, 1.);
}

#[test]
fn test_log2() {
    test_f_f(log2, rug::Float::log2, 0.0..=f64::MAX, 1.);
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

#[test]
fn test_cbrt() {
    test_f_f(cbrt, rug::Float::cbrt, f64::MIN..=f64::MAX, 1.);
}

#[test]
fn test_log1p() {
    test_f_f(log1p, rug::Float::ln_1p, -1.0..=1e+307, 1.);
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

#[test]
fn test_tgamma() {
    test_f_f(tgamma, rug::Float::gamma, f64::MIN..=f64::MAX, 1.);
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

#[test]
fn test_lgamma() {
    test_f_f(lgamma, rug::Float::ln_gamma, 0.0..=2e305, 1.);
}

fn ddmla(x: f64, y: Doubled<f64>, z: Doubled<f64>) -> Doubled<f64> {
    z + (y * x)
}
fn poly2dd_b(x: f64, c1: Doubled<f64>, c0: Doubled<f64>) -> Doubled<f64> {
    ddmla(x, c1, c0)
}
fn poly2dd(x: f64, c1: f64, c0: Doubled<f64>) -> Doubled<f64> {
    ddmla(x, dd(c1, 0.), c0)
}
fn poly4dd(x: f64, c3: f64, c2: Doubled<f64>, c1: Doubled<f64>, c0: Doubled<f64>) -> Doubled<f64> {
    ddmla(x * x, poly2dd(x, c3, c2), poly2dd_b(x, c1, c0))
}

pub fn erf(a: f64) -> f64 {
    let x = fabsk(a);
    let x2 = x * x;
    let x4 = x2 * x2;
    let x8 = x4 * x4;
    let x16 = x8 * x8;

    let mut t2;
    if x < 2.5 {
        // Abramowitz and Stegun
        let t = f64::poly21(
            x,
            x2,
            x4,
            x8,
            x16,
            -0.208_327_100_252_522_209_7_e-14,
            0.715_190_997_079_089_700_9_e-13,
            -0.116_223_822_011_099_936_4_e-11,
            0.118_647_423_082_158_525_9_e-10,
            -0.849_997_317_835_461_344_0_e-10,
            0.450_764_746_259_884_162_9_e-9,
            -0.180_804_447_428_884_891_5_e-8,
            0.543_508_182_671_621_238_9_e-8,
            -0.114_393_989_575_862_848_4_e-7,
            0.121_544_236_268_088_924_3_e-7,
            0.166_987_875_618_125_035_5_e-7,
            -0.980_807_460_225_519_428_8_e-7,
            0.138_900_055_786_583_720_4_e-6,
            0.294_551_452_998_733_186_6_e-6,
            -0.184_291_827_300_399_828_3_e-5,
            0.341_798_783_611_536_213_6_e-5,
            0.386_023_635_649_312_910_1_e-5,
            -0.330_940_307_274_994_754_6_e-4,
            0.106_086_292_259_757_953_2_e-3,
            0.232_325_315_521_307_617_4_e-3,
            0.149_014_971_914_554_472_9_e-3,
        );
        t2 = poly4dd(
            x,
            t,
            dd(
                0.009_287_795_839_227_560_440_5,
                7.928_755_946_396_110_749_3_e-19,
            ),
            dd(
                0.042_275_531_758_784_692_937_,
                1.378_522_662_050_101_613_8_e-19,
            ),
            dd(
                0.070_523_697_943_469_534_91,
                9.584_662_807_079_209_284_2_e-19,
            ),
        );
        t2 = (1.).add_checked(t2 * x);
        t2 = t2.square();
        t2 = t2.square();
        t2 = t2.square();
        t2 = t2.square();
        t2 = t2.recpre();
    } else if x > 6. {
        t2 = dd(0., 0.);
    } else {
        let t = f64::poly21(
            x,
            x2,
            x4,
            x8,
            x16,
            -0.402_401_513_075_262_193_2_e-18,
            0.384_719_333_281_704_817_2_e-16,
            -0.174_931_624_145_564_408_8_e-14,
            0.502_961_832_287_287_271_5_e-13,
            -0.102_522_146_685_146_316_4_e-11,
            0.157_369_555_933_194_558_3_e-10,
            -0.188_465_855_804_020_370_9_e-9,
            0.179_816_785_303_215_930_9_e-8,
            -0.138_074_534_235_503_314_2_e-7,
            0.852_570_572_646_910_349_9_e-7,
            -0.416_044_805_810_130_340_5_e-6,
            0.151_727_266_000_858_848_5_e-5,
            -0.334_163_412_731_720_169_7_e-5,
            -0.251_502_339_587_972_451_3_e-5,
            0.653_973_126_966_490_755_4_e-4,
            -0.355_106_509_742_838_865_8_e-3,
            0.121_073_609_795_836_886_4_e-2,
            -0.260_556_691_257_999_868_0_e-2,
            0.125_282_320_243_609_319_3_e-2,
            0.182_019_139_526_331_322_2_e-1,
            -0.102_155_715_545_346_595_4,
        );
        t2 = poly4dd(
            x,
            t,
            dd(
                -0.636_910_443_836_417_483_61,
                -2.424_947_752_653_943_183_9_e-17,
            ),
            dd(
                -1.128_292_606_180_396_173_7,
                -6.297_033_886_041_099_650_5_e-17,
            ),
            dd(
                -1.226_131_378_518_480_496_7_e-05,
                -5.532_970_751_449_010_704_4_e-22,
            ),
        );
        t2 = dd(expk(t2), 0.);
    }

    t2 += -1.;

    if x < 1e-8 {
        t2 = dd(-1.128_379_167_095_512_627_562_454_759_59 * x, 0.);
    }
    mulsign(
        if a == 0. {
            0.
        } else if a.is_infinite() {
            1.
        } else {
            -t2.0 - t2.1
        },
        a,
    )
}

#[test]
fn test_erf() {
    test_f_f(erf, rug::Float::erf, f64::MIN..=f64::MAX, 0.75);
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

#[test]
fn test_exp() {
    test_f_f(exp, rug::Float::exp, -1000.0..=710.0, 1.);
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
    use rug::{ops::Pow, Float};
    test_ff_f(
        pow,
        |in1, in2| Float::with_val(in1.prec(), in1.pow(in2)),
        f64::MIN..=f64::MAX,
        f64::MIN..=f64::MAX,
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

#[test]
fn test_sinh() {
    test_f_f(sinh, rug::Float::sinh, -709.0..=709.0, 1.);
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

#[test]
fn test_cosh() {
    test_f_f(cosh, rug::Float::cosh, -709.0..=709.0, 1.);
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

#[test]
fn test_asinh() {
    test_f_f(asinh, rug::Float::asinh, -SQRT_DBL_MAX..=SQRT_DBL_MAX, 1.);
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

#[test]
fn test_acosh() {
    test_f_f(acosh, rug::Float::acosh, -SQRT_DBL_MAX..=SQRT_DBL_MAX, 1.);
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

#[test]
fn test_atanh() {
    test_f_f(atanh, rug::Float::atanh, f64::MIN..=f64::MAX, 1.);
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

#[test]
fn test_exp2() {
    test_f_f(exp2, rug::Float::exp2, -2000.0..=1024.0, 1.);
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

#[test]
fn test_exp10() {
    test_f_f(exp10, rug::Float::exp10, -350.0..=308.26, 1.09);
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

#[test]
fn test_expm1() {
    test_f_f(expm1, rug::Float::exp_m1, -37.0..=710.0, 1.);
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

#[test]
fn test_tanh() {
    test_f_f(tanh, rug::Float::tanh, -19.0..=19.0, 1.);
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

#[test]
fn test_log10() {
    test_f_f(log10, rug::Float::log10, 0.0..=f64::MAX, 1.);
}
