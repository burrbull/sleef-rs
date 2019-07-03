//! Functions with 3.5 ULP error bound
use super::*;

pub fn sincospi(d: f64) -> (f64, f64) {
    let u = d * 4.;
    let q = ceilk(u) & !1_isize;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;

    //

    let u = 0.688_063_889_476_606_013_6_e-11_f64
        .mul_add(s, -0.175_715_956_454_231_019_9_e-8)
        .mul_add(s, 0.313_361_632_725_786_731_1_e-6)
        .mul_add(s, -0.365_762_041_638_848_645_2_e-4)
        .mul_add(s, 0.249_039_457_018_993_210_3_e-2)
        .mul_add(s, -0.807_455_121_882_805_632_e-1)
        .mul_add(s, 0.785_398_163_397_448_279);

    let mut rsin = u * t;

    //

    let u = (-0.386_014_121_368_379_435_2_e-12_f64)
        .mul_add(s, 0.115_005_788_802_968_141_5_e-9)
        .mul_add(s, -0.246_113_649_300_666_355_3_e-7)
        .mul_add(s, 0.359_086_044_662_351_671_3_e-5)
        .mul_add(s, -0.325_991_886_926_943_594_2_e-3)
        .mul_add(s, 0.158_543_442_438_154_116_9_e-1)
        .mul_add(s, -0.308_425_137_534_042_437_3)
        .mul_add(s, 1.);

    let mut rcos = u;

    //

    if (q & 2) != 0 {
        core::mem::swap(&mut rcos, &mut rsin);
    }
    if (q & 4) != 0 {
        rsin = -rsin;
    }
    if ((q + 2) & 4) != 0 {
        rcos = -rcos;
    }

    if fabsk(d) > TRIGRANGEMAX3 / 4. {
        rsin = 0.;
        rcos = 1.;
    }
    if d.is_infinite() {
        rsin = f64::NAN;
        rcos = f64::NAN;
    }

    (rsin, rcos)
}

pub fn sinh(x: f64) -> f64 {
    let e = expm1k(fabsk(x));
    let mut y = (e + 2.) / (e + 1.) * (0.5 * e);

    y = if fabsk(x) > 709. { f64::INFINITY } else { y };
    y = if y.is_nan() { f64::INFINITY } else { y };
    y = mulsign(y, x);
    if x.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn cosh(x: f64) -> f64 {
    let e = u10::exp(fabsk(x));
    let mut y = 0.5 / e + 0.5 * e;

    y = if fabsk(x) > 709. { f64::INFINITY } else { y };
    y = if y.is_nan() { f64::INFINITY } else { y };
    if x.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn tanh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let d = expm1k(2. * y);
    y = d / (d + 2.);

    y = if fabsk(x) > 18.714_973_875 { 1. } else { y };
    y = if y.is_nan() { 1. } else { y };
    y = mulsign(y, x);
    if x.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn sqrt(d: f64) -> f64 {
    u05::sqrt(d)
}

pub fn hypot(mut x: f64, mut y: f64) -> f64 {
    x = fabsk(x);
    y = fabsk(y);
    let min = x.min(y);
    let max = x.max(y);

    let t = min / max;
    if (x == f64::INFINITY) || (y == f64::INFINITY) {
        f64::INFINITY
    } else if x.is_nan() || y.is_nan() {
        f64::NAN
    } else if min == 0. {
        max
    } else {
        max * (1. + t * t).sqrt()
    }
}

#[test]
fn test_hypot() {
    test_ff_f(
        hypot,
        rug::Float::hypot,
        -1e307..=1e307,
        -1e307..=1e307,
        3.5,
    );
}

pub fn atan2(y: f64, x: f64) -> f64 {
    let mut r = atan2k(fabsk(y), x);

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
    let x = if o { fabsk(d) } else { x2.sqrt() };

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
    )
    .mul_add(x * x2, x);

    let r = if o { u } else { FRAC_PI_2 - 2. * u };
    mulsign(r, d)
}

pub fn acos(d: f64) -> f64 {
    let o = fabsk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
    let mut x = if o { fabsk(d) } else { x2.sqrt() };
    x = if fabsk(d) == 1. { 0. } else { x };

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
    ) * (x * x2);

    let y = 3.141_592_653_589_793_2 / 2. - (mulsign(x, d) + mulsign(u, d));
    x += u;
    let r = if o { y } else { x * 2. };
    if !o && (d < 0.) {
        dd(3.141_592_653_589_793_116, 1.224_646_799_147_353_207_2_e-16)
            .add_checked(-r)
            .0
    } else {
        r
    }
}

pub fn atan(mut s: f64) -> f64 {
    let mut q = if sign(s) == -1. {
        s = -s;
        2
    } else {
        0
    };

    if s > 1. {
        s = 1. / s;
        q |= 1;
    }

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
        -1.887_960_084_630_734_965_637_46_e-5,
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

    let t = s + s * (t * u);

    if (q & 2) != 0 {
        -t
    } else if (q & 1) != 0 {
        1.570_796_326_794_896_557_998_982 - t
    } else {
        t
    }
}

pub fn sin(mut d: f64) -> f64 {
    let t = d;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(d * FRAC_1_PI);
        ql = qlf as isize;
        d = qlf.mul_add(-PI_A2, d);
        d = qlf.mul_add(-PI_B2, d);
    } else if fabsk(d) < TRIGRANGEMAX {
        let dqh = trunck(d * (FRAC_1_PI / D1_24)) * D1_24;
        let qlf = rintk(d.mul_add(FRAC_1_PI, -dqh));
        ql = qlf as isize;

        d = dqh.mul_add(-PI_A, d);
        d = qlf.mul_add(-PI_A, d);
        d = dqh.mul_add(-PI_B, d);
        d = qlf.mul_add(-PI_B, d);
        d = dqh.mul_add(-PI_C, d);
        d = qlf.mul_add(-PI_C, d);
        d = (dqh + qlf).mul_add(-PI_D, d);
    } else {
        let (mut ddidd, ddii) = rempi(t);
        ql = (((ddii & 3) * 2 + ((ddidd.0 > 0.) as i32) + 1) >> 2) as isize;
        if ddii & 1 != 0 {
            ddidd = ddidd
                + dd(
                    mulsign(3.141_592_653_589_793_116 * -0.5, ddidd.0),
                    mulsign(1.224_646_799_147_353_207_2_e-16 * -0.5, ddidd.0),
                );
        }
        d = ddidd.0 + ddidd.1;
        if t.is_infinite() || t.is_nan() {
            d = f64::NAN;
        }
    }

    let s = d * d;

    if (ql & 1) != 0 {
        d = -d;
    }

    let s2 = s * s;
    let s4 = s2 * s2;

    let u = f64::poly8(
        s,
        s2,
        s4,
        -7.972_559_550_090_378_688_919_52_e-18_f64,
        2.810_099_727_108_632_000_912_51_e-15,
        -7.647_122_191_181_588_332_884_84_e-13,
        1.605_904_306_056_645_016_290_54_e-10,
        -2.505_210_837_635_020_458_107_55_e-8,
        2.755_731_922_391_987_476_304_16_e-6,
        -0.000_198_412_698_412_696_162_806_809,
        0.008_333_333_333_333_329_748_238_15,
    )
    .mul_add(s, -0.166_666_666_666_666_657_414_808);

    if t.is_neg_zero() {
        t
    } else {
        s.mul_add(u * d, d)
    }
}

#[test]
fn test_sin() {
    super::test_f_f(sin, rug::Float::sin, f64::MIN..=f64::MAX, 3.5);
}

pub fn cos(mut d: f64) -> f64 {
    let t = d;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = (2_f64).mul_add(rintk(d * FRAC_1_PI - 0.5), 1.);
        ql = qlf as isize;
        d = qlf.mul_add(-PI_A2 * 0.5, d);
        d = qlf.mul_add(-PI_B2 * 0.5, d);
    } else if fabsk(d) < TRIGRANGEMAX {
        let mut dqh = trunck(d * (FRAC_1_PI / D1_23) - 0.5 * (FRAC_1_PI / D1_23));
        let qlf = 2. * rintk(d * FRAC_1_PI - 0.5 - dqh * D1_23) + 1.;
        ql = qlf as isize;
        dqh *= D1_24;

        d = dqh.mul_add(-PI_A * 0.5, d);
        d = qlf.mul_add(-PI_A * 0.5, d);
        d = dqh.mul_add(-PI_B * 0.5, d);
        d = qlf.mul_add(-PI_B * 0.5, d);
        d = dqh.mul_add(-PI_C * 0.5, d);
        d = qlf.mul_add(-PI_C * 0.5, d);
        d = (dqh + qlf).mul_add(-PI_D * 0.5, d);
    } else {
        let (mut ddidd, ddii) = rempi(t);
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
        d = ddidd.0 + ddidd.1;
        if t.is_infinite() || t.is_nan() {
            d = f64::NAN;
        }
    }

    let s = d * d;

    if (ql & 2) == 0 {
        d = -d;
    }

    let s2 = s * s;
    let s4 = s2 * s2;

    let u = f64::poly8(
        s,
        s2,
        s4,
        -7.972_559_550_090_378_688_919_52_e-18_f64,
        2.810_099_727_108_632_000_912_51_e-15,
        -7.647_122_191_181_588_332_884_84_e-13,
        1.605_904_306_056_645_016_290_54_e-10,
        -2.505_210_837_635_020_458_107_55_e-8,
        2.755_731_922_391_987_476_304_16_e-6,
        -0.000_198_412_698_412_696_162_806_809,
        0.008_333_333_333_333_329_748_238_15,
    )
    .mul_add(s, -0.166_666_666_666_666_657_414_808);

    s.mul_add(u * d, d)
}

#[test]
fn test_cos() {
    super::test_f_f(cos, rug::Float::cos, f64::MIN..=f64::MAX, 3.5);
}

pub fn sincos(d: f64) -> (f64, f64) {
    let ql: isize;

    let mut s = d;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(s * FRAC_2_PI);
        ql = qlf as isize;
        s = qlf.mul_add(-PI_A2 * 0.5, s);
        s = qlf.mul_add(-PI_B2 * 0.5, s);
    } else if fabsk(d) < TRIGRANGEMAX {
        let dqh = trunck(d * (FRAC_2_PI / D1_24)) * D1_24;
        let qlf = rintk(d * FRAC_2_PI - dqh);
        ql = qlf as isize;

        s = dqh.mul_add(-PI_A * 0.5, s);
        s = qlf.mul_add(-PI_A * 0.5, s);
        s = dqh.mul_add(-PI_B * 0.5, s);
        s = qlf.mul_add(-PI_B * 0.5, s);
        s = dqh.mul_add(-PI_C * 0.5, s);
        s = qlf.mul_add(-PI_C * 0.5, s);
        s = (dqh + qlf).mul_add(-PI_D * 0.5, s);
    } else {
        let (ddidd, ddii) = rempi(d);
        ql = ddii as isize;
        s = ddidd.0 + ddidd.1;
        if d.is_infinite() || d.is_nan() {
            s = f64::NAN;
        }
    }

    let t = s;

    s = s * s;

    let u = 1.589_383_072_832_289_373_285_11_e-10_f64
        .mul_add(s, -2.505_069_435_025_397_733_493_18_e-8)
        .mul_add(s, 2.755_731_317_768_463_605_125_47_e-6)
        .mul_add(s, -0.000_198_412_698_278_911_770_864_914)
        .mul_add(s, 0.008_333_333_333_319_184_596_174_6)
        .mul_add(s, -0.166_666_666_666_666_130_709_393)
        * s
        * t;

    let mut rsin = if d.is_neg_zero() { -0. } else { t + u };

    let u = (-1.136_153_502_390_974_295_315_23_e-11_f64)
        .mul_add(s, 2.087_574_712_070_400_554_793_66_e-9)
        .mul_add(s, -2.755_731_440_288_475_674_985_67_e-7)
        .mul_add(s, 2.480_158_728_900_018_673_119_15_e-5)
        .mul_add(s, -0.001_388_888_888_887_140_192_823_29)
        .mul_add(s, 0.041_666_666_666_666_551_959_206_2)
        .mul_add(s, -0.5);

    let mut rcos = u * s + 1.;

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

pub fn tan(d: f64) -> f64 {
    let mut x: f64;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(d * FRAC_2_PI);
        ql = qlf as isize;
        x = qlf.mul_add(-PI_A2 * 0.5, d);
        x = qlf.mul_add(-PI_B2 * 0.5, x);
    } else if fabsk(d) < 1e+6 {
        let dqh = trunck(d * (FRAC_2_PI / D1_24)) * D1_24;
        let qlf = rintk(d * FRAC_2_PI - dqh);
        ql = qlf as isize;

        x = dqh.mul_add(-PI_A * 0.5, d);
        x = qlf.mul_add(-PI_A * 0.5, x);
        x = dqh.mul_add(-PI_B * 0.5, x);
        x = qlf.mul_add(-PI_B * 0.5, x);
        x = dqh.mul_add(-PI_C * 0.5, x);
        x = qlf.mul_add(-PI_C * 0.5, x);
        x = (dqh + qlf).mul_add(-PI_D * 0.5, x);
    } else {
        let (ddidd, ddii) = rempi(d);
        ql = ddii as isize;
        x = ddidd.0 + ddidd.1;
        if d.is_infinite() || d.is_nan() {
            x = f64::NAN;
        }
    }

    x *= 0.5;
    let s = x * x;

    let s2 = s * s;
    let s4 = s2 * s2;

    let mut u = f64::poly8(
        s,
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
    .mul_add(s, 0.333_333_333_333_334_369_5);
    u = s.mul_add(u * x, x);

    let mut y = u.mul_add(u, -1.);
    x = -2. * u;

    if (ql & 1) != 0 {
        let t = x;
        x = y;
        y = -t;
    }

    x / y
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

    let x = (m - 1.) / (m + 1.);
    let x2 = x * x;

    let x4 = x2 * x2;
    let x8 = x4 * x4;

    let t = f64::poly7(
        x2,
        x4,
        x8,
        0.153_487_338_491_425_068_243_146,
        0.152_519_917_006_351_951_593_857,
        0.181_863_266_251_982_985_677_316,
        0.222_221_366_518_767_365_905_163,
        0.285_714_294_746_548_025_383_248,
        0.399_999_999_950_799_600_689_777,
        0.666_666_666_666_777_874_006_3,
    );

    let x = x * 2. + 0.693_147_180_559_945_286_226_764 * (e as f64) + x * x2 * t;

    if d == 0. {
        f64::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f64::NAN
    } else if d.is_infinite() {
        f64::INFINITY
    } else {
        x * t + 0.693_147_180_559_945_286_226_764 * (e as f64)
    }
}

pub fn cbrt(mut d: f64) -> f64 {
    // max error : 2 ulps
    let mut q = 1.;
    let e = ilogbk(fabsk(d)) + 1;
    d = ldexp2k(d, -e);
    let r = (e + 6144) % 3;
    q = if r == 1 {
        1.259_921_049_894_873_164_767_210_6
    } else {
        q
    };
    q = if r == 2 {
        1.587_401_051_968_199_474_751_705_6
    } else {
        q
    };
    q = ldexp2k(q, (e + 6144) / 3 - 2048);

    q = mulsign(q, d);
    d = fabsk(d);

    let mut x = (-0.640_245_898_480_692_909_870_982_f64)
        .mul_add(d, 2.961_551_030_200_395_118_185_95)
        .mul_add(d, -5.733_530_609_229_478_436_361_66)
        .mul_add(d, 6.039_903_689_894_587_479_614_07)
        .mul_add(d, -3.858_419_355_104_449_888_216_32)
        .mul_add(d, 2.230_727_530_249_660_972_572_2);

    let mut y = x * x;
    y = y * y;
    x -= (d * y - x) * (1. / 3.);
    y = d * x * x;
    (y - (2. / 3.) * y * (y * x - 1.)) * q
}

pub fn exp2(d: f64) -> f64 {
    let q = rintk(d);

    let s = d - q;

    let mut u = 0.443_435_908_292_652_945_4_e-9
        .mul_add(s, 0.707_316_459_808_570_742_5_e-8)
        .mul_add(s, 0.101_781_926_092_176_045_1_e-6)
        .mul_add(s, 0.132_154_387_251_132_761_5_e-5)
        .mul_add(s, 0.152_527_335_351_758_473_e-4)
        .mul_add(s, 0.154_035_304_510_114_780_8_e-3)
        .mul_add(s, 0.133_335_581_467_049_907_3_e-2)
        .mul_add(s, 0.961_812_910_759_760_053_6_e-2)
        .mul_add(s, 0.555_041_086_648_204_659_6_e-1)
        .mul_add(s, 0.240_226_506_959_101_221_4)
        .mul_add(s, 0.693_147_180_559_945_286_2)
        .mul_add(s, 0.1_e+1);

    u = ldexp2k(u, q as i32);

    if d < -2000. {
        0.
    } else if d >= 1024. {
        f64::INFINITY
    } else {
        u
    }
}

pub fn exp10(d: f64) -> f64 {
    let q = rintk(d * LOG10_2);

    let mut s = q.mul_add(-L10U, d);
    s = q.mul_add(-L10L, s);

    let mut u = 0.241_146_349_833_426_765_2_e-3
        .mul_add(s, 0.115_748_841_521_718_737_5_e-2)
        .mul_add(s, 0.501_397_554_678_973_365_9_e-2)
        .mul_add(s, 0.195_976_232_072_053_308_e-1)
        .mul_add(s, 0.680_893_639_944_678_413_8_e-1)
        .mul_add(s, 0.206_995_849_472_267_623_4)
        .mul_add(s, 0.539_382_929_205_853_622_9)
        .mul_add(s, 0.117_125_514_890_854_165_5_e+1)
        .mul_add(s, 0.203_467_859_229_343_295_3_e+1)
        .mul_add(s, 0.265_094_905_523_920_587_6_e+1)
        .mul_add(s, 0.230_258_509_299_404_590_1_e+1)
        .mul_add(s, 0.1_e+1);

    u = ldexp2k(u, q as i32);

    if d < -350. {
        0.
    } else if d > 308.254_715_559_916_71 {
        f64::INFINITY
    } else {
        u
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

    let x = (m - 1.) / (m + 1.);
    let x2 = x * x;

    let t = 0.221_194_175_045_608_149
        .mul_add(x2, 0.220_076_869_315_227_768_9)
        .mul_add(x2, 0.262_370_805_748_851_465_6)
        .mul_add(x2, 0.320_597_747_794_449_550_2)
        .mul_add(x2, 0.412_198_594_548_532_470_9)
        .mul_add(x2, 0.577_078_016_299_705_898_2)
        .mul_add(x2, 0.961_796_693_926_080_914_49);

    let s = (e as f64).add_checked((2.885_390_081_777_926_774).mul_as_doubled(x));
    let r = t.mul_add(x * x2, s.0 + s.1);

    if d == 0. {
        f64::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f64::NAN
    } else if d.is_infinite() {
        f64::INFINITY
    } else {
        r
    }
}
