//! Functions with 0.5 ULP error bound

use super::*;

pub fn sincospi(d: f64) -> (f64, f64) {
    let u = d * 4.;
    let q = ceilk(u) & !1_isize;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (-2.024_611_207_851_823_992_958_68_e-14_f64)
        .mul_add(s, 6.948_218_305_801_794_613_277_84_e-12)
        .mul_add(s, -1.757_247_499_528_531_799_526_64_e-9)
        .mul_add(s, 3.133_616_889_668_683_928_784_22_e-7)
        .mul_add(s, -3.657_620_418_216_155_192_036_1_e-5)
        .mul_add(s, 0.002_490_394_570_192_718_502_743_56);
    let mut x = u * s
        + dd(
            -0.080_745_512_188_280_785_248_473_1,
            3.618_524_750_670_371_048_499_87_e-18,
        );
    x = s2 * x
        + dd(
            0.785_398_163_397_448_278_999_491,
            3.062_871_137_271_550_026_071_05_e-17,
        );

    x *= t;
    let mut rsin = if xisnegzero(d) { -0. } else { x.0 + x.1 };

    //

    let u = 9.944_803_876_268_437_740_902_08_e-16_f64
        .mul_add(s, -3.897_962_260_629_327_991_640_47_e-13)
        .mul_add(s, 1.150_115_825_399_960_352_669_01_e-10)
        .mul_add(s, -2.461_136_950_104_469_749_535_9_e-8)
        .mul_add(s, 3.590_860_448_590_527_540_050_62_e-6)
        .mul_add(s, -0.000_325_991_886_927_389_905_997_954);
    x = u * s
        + dd(
            0.015_854_344_243_815_501_891_425_9,
            -1.046_932_722_806_315_219_088_45_e-18,
        );
    x = s2 * x
        + dd(
            -0.308_425_137_534_042_437_259_529,
            -1.956_984_921_336_335_503_383_45_e-17,
        );

    x = x * s2 + 1.;
    let mut rcos = x.0 + x.1;

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

pub fn sinpi(d: f64) -> f64 {
    let x = sinpik(d);
    if d.is_infinite() {
        f64::NAN
    } else if fabsk(d) > TRIGRANGEMAX3 / 4. {
        0.
    } else if xisnegzero(d) {
        -0.
    } else {
        x.0 + x.1
    }
}

pub fn cospi(d: f64) -> f64 {
    let x = cospik(d);

    if d.is_infinite() {
        f64::NAN
    } else if fabsk(d) > TRIGRANGEMAX3 / 4. {
        1.
    } else {
        x.0 + x.1
    }
}

pub fn sqrt(mut d: f64) -> f64 {
    let mut q = 0.5;

    d = if d < 0. { f64::NAN } else { d };

    if d < 8.636_168_555_094_445E-78 {
        d *= 1.157_920_892_373_162E77;
        q = 2.938_735_877_055_718_8E-39 * 0.5;
    }

    if d > 1.340_780_792_994_259_7_e+154 {
        d *= 7.458_340_731_200_207_e-155;
        q = 1.157_920_892_373_162_e+77 * 0.5;
    }

    // http://en.wikipedia.org/wiki/Fast_inverse_square_root
    let mut x = f64::from_bits(0x_5fe6_ec85_e7de_30da - ((d + 1e-320).to_bits() >> 1));

    x = x * (1.5 - 0.5 * d * x * x);
    x = x * (1.5 - 0.5 * d * x * x);
    x = x * (1.5 - 0.5 * d * x * x) * d;

    let d2 = (d + x.mul_as_doubled(x)) * x.recpre();

    let ret = (d2.0 + d2.1) * q;

    let ret = if d == f64::INFINITY {
        f64::INFINITY
    } else {
        ret
    };
    if d == 0. {
        d
    } else {
        ret
    }
}

pub fn hypot(mut x: f64, mut y: f64) -> f64 {
    x = fabsk(x);
    y = fabsk(y);
    let min = fmink(x, y);
    let mut n = min;
    let max = fmaxk(x, y);
    let mut d = max;

    if max < f64::MIN {
        n *= D1_54;
        d *= D1_54;
    }
    let mut t = dd(n, 0.) / dd(d, 0.);
    t = (t.square() + 1.).sqrt() * max;
    let ret = t.0 + t.1;

    if (x == f64::INFINITY) || (y == f64::INFINITY) {
        f64::INFINITY
    } else if x.is_nan() || y.is_nan() {
        f64::NAN
    } else if min == 0. {
        max
    } else if ret.is_nan() {
        f64::INFINITY
    } else {
        ret
    }
}
