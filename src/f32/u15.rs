//! Functions with 1.5 ULP error bound

use super::*;

/// Complementary error function
///
/// The error bound is `max(1.5 ULP, f32::MIN_POSITIVE)`.
pub fn erfcf(mut a: f32) -> f32 {
    let s = a;
    a = fabsfk(a);
    let o0 = a < 1.;
    let o1 = a < 2.2;
    let o2 = a < 4.3;
    let o3 = a < 10.1;
    let u = if o1 {
        Doubled::from(a)
    } else {
        Doubled::from(1.) / Doubled::from(a)
    };

    let t = if o0 {
        -0.863_804_161_8_e-4_f32
    } else if o1 {
        -0.623_697_724_2_e-5
    } else if o2 {
        -0.386_950_403_5
    } else {
        0.111_534_416_7_e+1
    }
    .mla(
        u.0,
        if o0 {
            0.600_016_617_7_e-3
        } else if o1 {
            0.574_982_150_3_e-4
        } else if o2 {
            0.128_807_723_5_e+1
        } else {
            -0.945_490_419_9
        },
    )
    .mla(
        u.0,
        if o0 {
            -0.166_570_360_3_e-2
        } else if o1 {
            0.600_285_147_8_e-5
        } else if o2 {
            -0.181_680_321_7_e+1
        } else {
            -0.366_725_951_4
        },
    )
    .mla(
        u.0,
        if o0 {
            0.179_515_627_7_e-3
        } else if o1 {
            -0.285_103_637_7_e-2
        } else if o2 {
            0.124_915_087_2_e+1
        } else {
            0.715_566_337_1
        },
    )
    .mla(
        u.0,
        if o0 {
            0.191_410_612_3_e-1
        } else if o1 {
            0.226_051_807_4_e-1
        } else if o2 {
            -0.132_885_798_8
        } else {
            -0.126_294_726_5_e-1
        },
    );

    let mut d = u * t;
    d += if o0 {
        Doubled::new(-0.102_775_359_343_930_288_081_655_368_891, 0.)
    } else if o1 {
        Doubled::new(-0.105_247_583_459_338_632_253_369_014_063, 0.)
    } else if o2 {
        Doubled::new(-0.482_365_310_333_045_318_680_618_892_669, 0.)
    } else {
        Doubled::new(-0.498_961_546_254_537_647_970_305_302_739, 0.)
    };
    d *= u;
    d += if o0 {
        Doubled::new(-0.636_619_483_208_481_931_303_752_546_439, 0.)
    } else if o1 {
        Doubled::new(-0.635_609_463_574_589_034_216_723_775_292, 0.)
    } else if o2 {
        Doubled::new(-0.134_450_203_224_533_979_217_859_332_703_e-2, 0.)
    } else {
        Doubled::new(-0.471_199_543_422_848_492_080_722_832_666_e-4, 0.)
    };
    d *= u;
    d += if o0 {
        Doubled::new(-0.112_837_917_790_537_404_939_545_770_596_e+1, 0.)
    } else if o1 {
        Doubled::new(-0.112_855_987_376_668_622_084_547_028_949_e+1, 0.)
    } else if o2 {
        Doubled::new(-0.572_319_781_150_472_949_561_786_101_080, 0.)
    } else {
        Doubled::new(-0.572_364_030_327_966_044_425_932_623_525, 0.)
    };

    let mut x = (if o1 { d } else { Doubled::from(-a) }) * a;
    x = if o1 { x } else { x + d };

    x = expk2f(x);
    x = if o1 { x } else { x * u };

    let mut r = if o3 { f32::from(x) } else { 0. };
    if s < 0. {
        r = 2. - r;
    }
    if s.is_nan() {
        f32::NAN
    } else {
        r
    }
}

#[test]
fn test_erfcf() {
    test_f_f(erfcf, rug::Float::erfc, f32::MIN..=f32::MAX, 1.5);
}
