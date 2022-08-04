//! Functions with 1.5 ULP error bound

use super::*;

/// Complementary error function
///
/// The error bound of the returned value is `max(1.5 ULP, f64::MIN_POSITIVE)`
/// if the argument is less than `26.2`, and `max(2.5 ULP, f64::MIN_POSITIVE)` otherwise.
pub fn erfc(a: f64) -> f64 {
    let s = a;
    let a = fabsk(a);
    let o0 = a < 1.;
    let o1 = a < 2.2;
    let o2 = a < 4.2;
    let o3 = a < 27.3;
    let u = if o0 {
        a.mul_as_doubled(a)
    } else if o1 {
        Doubled::from(a)
    } else {
        Doubled::<f64>::from(1.) / Doubled::from(a)
    };

    let t = (if o0 {
        0.680_107_240_139_538_613_9_e-20_f64
    } else if o1 {
        0.343_801_034_136_258_530_3_e-12
    } else if o2 {
        -0.575_781_953_642_071_044_9_e+2
    } else {
        0.233_424_972_963_870_131_9_e+5
    })
    .mul_add(
        u.0,
        if o0 {
            -0.216_176_624_757_005_566_9_e-18
        } else if o1 {
            -0.123_702_118_816_059_826_4_e-10
        } else if o2 {
            0.466_928_965_449_810_448_3_e+3
        } else {
            -0.469_566_104_493_310_776_9_e+5
        },
    )
    .mul_add(
        u.0,
        if o0 {
            0.469_591_917_330_159_567_e-17
        } else if o1 {
            0.211_798_583_987_762_785_2_e-9
        } else if o2 {
            -0.179_632_987_946_135_585_8_e+4
        } else {
            0.317_340_310_874_864_335_3_e+5
        },
    )
    .mul_add(
        u.0,
        if o0 {
            -0.904_914_041_988_800_712_2_e-16
        } else if o1 {
            -0.229_056_092_917_736_950_6_e-8
        } else if o2 {
            0.435_589_219_369_957_572_8_e+4
        } else {
            0.324_298_278_695_957_378_7_e+4
        },
    )
    .mul_add(
        u.0,
        if o0 {
            0.163_401_890_355_741_072_8_e-14
        } else if o1 {
            0.174_893_162_169_814_953_8_e-7
        } else if o2 {
            -0.745_625_888_496_576_499_2_e+4
        } else {
            -0.201_471_799_976_034_781_1_e+5
        },
    )
    .mul_add(
        u.0,
        if o0 {
            -0.278_348_578_633_345_174_5_e-13
        } else if o1 {
            -0.995_660_260_662_324_919_5_e-7
        } else if o2 {
            0.955_397_735_816_702_152_1_e+4
        } else {
            0.155_400_697_096_711_828_6_e+5
        },
    )
    .mul_add(
        u.0,
        if o0 {
            0.446_322_127_678_641_575_2_e-12
        } else if o1 {
            0.433_001_024_064_032_708_e-6
        } else if o2 {
            -0.947_001_990_544_422_915_3_e+4
        } else {
            -0.615_087_419_056_355_429_3_e+4
        },
    )
    .mul_add(
        u.0,
        if o0 {
            -0.671_136_662_285_013_656_3_e-11
        } else if o1 {
            -0.143_505_060_099_176_333_1_e-5
        } else if o2 {
            0.738_734_432_184_985_507_8_e+4
        } else {
            0.124_004_776_563_481_573_2_e+4
        },
    )
    .mul_add(
        u.0,
        if o0 {
            0.942_275_905_023_266_222_3_e-10
        } else if o1 {
            0.346_013_947_965_069_566_2_e-5
        } else if o2 {
            -0.455_771_305_416_638_279_e+4
        } else {
            -0.821_032_547_575_269_973_1_e+2
        },
    )
    .mul_add(
        u.0,
        if o0 {
            -0.122_905_553_010_022_909_8_e-8
        } else if o1 {
            -0.498_890_818_063_289_817_3_e-5
        } else if o2 {
            0.220_786_696_735_405_530_5_e+4
        } else {
            0.324_244_388_083_993_087_e+2
        },
    )
    .mul_add(
        u.0,
        if o0 {
            0.148_071_928_158_508_651_2_e-7
        } else if o1 {
            -0.130_877_597_632_635_201_2_e-5
        } else if o2 {
            -0.821_797_565_862_175_474_6_e+3
        } else {
            -0.292_341_886_383_316_058_6_e+2
        },
    )
    .mul_add(
        u.0,
        if o0 {
            -0.163_658_446_912_339_980_3_e-6
        } else if o1 {
            0.282_508_654_085_031_010_3_e-4
        } else if o2 {
            0.226_865_948_350_791_74_e+3
        } else {
            0.345_746_173_281_438_307_1
        },
    )
    .mul_add(
        u.0,
        if o0 {
            0.164_621_143_658_892_357_5_e-5
        } else if o1 {
            -0.639_391_371_306_998_607_1_e-4
        } else if o2 {
            -0.463_336_126_031_856_068_2_e+2
        } else {
            0.548_973_015_595_239_299_8_e+1
        },
    )
    .mul_add(
        u.0,
        if o0 {
            -0.149_256_503_584_062_351_1_e-4
        } else if o1 {
            -0.256_643_651_469_507_892_6_e-4
        } else if o2 {
            0.955_738_012_373_394_596_5_e+1
        } else {
            0.155_993_413_225_129_413_4_e-2
        },
    )
    .mul_add(
        u.0,
        if o0 {
            0.120_553_329_817_896_785_1_e-3
        } else if o1 {
            0.589_579_237_565_944_036_4_e-3
        } else if o2 {
            -0.295_842_933_193_966_128_9_e+1
        } else {
            -0.154_174_156_683_152_063_8_e+1
        },
    )
    .mul_add(
        u.0,
        if o0 {
            -0.854_832_702_345_085_008_1_e-3
        } else if o1 {
            -0.169_571_557_916_358_859_8_e-2
        } else if o2 {
            0.167_032_950_809_276_548_0
        } else {
            0.282_315_223_055_836_418_6_e-5
        },
    )
    .mul_add(
        u.0,
        if o0 {
            0.522_397_762_544_218_793_2_e-2
        } else if o1 {
            0.208_911_643_491_805_514_9_e-3
        } else if o2 {
            0.609_661_568_011_541_921_1
        } else {
            0.624_999_918_419_534_283_8
        },
    )
    .mul_add(
        u.0,
        if o0 {
            -0.268_661_706_451_312_522_2_e-1
        } else if o1 {
            0.191_285_594_958_491_775_3_e-1
        } else if o2 {
            0.105_921_244_319_354_358_5_e-2
        } else {
            0.174_174_941_640_870_128_8_e-8
        },
    );

    let mut d = u * t;
    d += if o0 {
        Doubled::new(
            0.112_837_916_709_551_261_41,
            -4.017_569_162_593_211_848_3_e-18,
        )
    } else if o1 {
        Doubled::new(
            -0.102_772_633_431_476_467_79,
            -6.233_871_408_340_490_022_5_e-18,
        )
    } else if o2 {
        Doubled::new(
            -0.500_051_804_739_990_224_39,
            2.636_214_056_904_199_580_3_e-17,
        )
    } else {
        Doubled::new(
            -0.500_000_000_025_844_437_7,
            -4.007_404_471_238_699_228_1_e-17,
        )
    };
    d *= u;
    d += if o0 {
        Doubled::new(
            -0.376_126_389_031_837_538_02,
            1.339_189_720_604_255_238_7_e-17,
        )
    } else if o1 {
        Doubled::new(
            -0.636_619_767_429_163_596_62,
            7.632_101_915_908_572_466_2_e-18,
        )
    } else if o2 {
        Doubled::new(
            1.601_106_273_924_963_368_e-6,
            1.197_400_185_776_447_677_5_e-23,
        )
    } else {
        Doubled::new(
            2.376_197_313_752_336_479_2_e-13,
            -1.167_007_695_053_102_658_2_e-29,
        )
    };
    d *= u;
    d += if o0 {
        Doubled::new(
            1.128_379_167_095_512_558_6,
            1.533_545_961_316_582_267_4_e-17,
        )
    } else if o1 {
        Doubled::new(
            -1.128_379_167_471_729_616_1,
            8.089_684_775_596_537_719_4_e-17,
        )
    } else if o2 {
        Doubled::new(
            -0.572_364_966_451_454_293_41,
            3.070_455_324_587_202_725_8_e-17,
        )
    } else {
        Doubled::new(
            -0.572_364_942_924_701_081_14,
            -2.398_435_220_805_689_800_3_e-17,
        )
    };

    let mut x = (if o1 { d } else { Doubled::from(-a) }) * a;
    x = if o1 { x } else { x + d };
    x = if o0 {
        Doubled::from(1.).sub_checked(x)
    } else {
        expk2(x)
    };
    x = if o1 { x } else { x * u };

    let mut r = if o3 { f64::from(x) } else { 0. };
    if s < 0. {
        r = 2. - r;
    }
    if s.is_nan() {
        f64::NAN
    } else {
        r
    }
}

#[test]
fn test_erfc() {
    test_f_f(erfc, rug::Float::erfc, f64::MIN..=26.2, 1.5);
    test_f_f(erfc, rug::Float::erfc, 26.2..=f64::MAX, 2.5);
}
