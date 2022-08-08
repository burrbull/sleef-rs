//! Fast functions with 350 ULP error bound

use super::*;

/// Fast sine function
///
/// The error bounds of the returned value is `min(350 ULP, 2e-6)`.
pub fn sinf(mut d: f32) -> f32 {
    let t = d;

    let q = rintfk(d * FRAC_1_PI);
    d = q.mla(-PI, d);

    let s = d * d;

    let mut u = (-0.188_174_817_6_e-3)
        .mla(s, 0.832_350_272_7_e-2)
        .mla(s, -0.166_665_136_8);
    u = (s * d).mla(u, d);

    if ((q as i32) & 1) != 0 {
        u = -u;
    }

    if fabsfk(t) > 30. {
        return super::u35::sinf(t);
    }

    u
}

#[test]
fn test_sinf() {
    test_c_f_f(sinf, rug::Float::sin, -30.0..=30.0, |ulp, o, e| {
        let ulp_ex = 350.;
        (
            ulp <= ulp_ex || (e.clone() - o).abs() <= 2e-6,
            format!("ULP: {ulp} > {ulp_ex}"),
        )
    });
}

/// Fast cosine function
///
/// The error bounds of the returned value is `min(350 ULP, 2e-6)`.
pub fn cosf(mut d: f32) -> f32 {
    let t = d;

    let q = rintfk(d.mla(FRAC_1_PI, -0.5));
    d = q.mla(-PI, d - FRAC_PI_2);

    let s = d * d;

    let mut u = (-0.188_174_817_6_e-3)
        .mla(s, 0.832_350_272_7_e-2)
        .mla(s, -0.166_665_136_8);
    u = (s * d).mla(u, d);

    if ((q as i32) & 1) == 0 {
        u = -u;
    }

    if fabsfk(t) > 30. {
        return super::u35::cosf(t);
    }

    u
}

#[test]
fn test_cosf() {
    test_c_f_f(cosf, rug::Float::cos, -30.0..=30.0, |ulp, o, e| {
        let ulp_ex = 350.;
        (
            ulp <= ulp_ex || (e.clone() - o).abs() <= 2e-6,
            format!("ULP: {ulp} > {ulp_ex}"),
        )
    });
}

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
        .mla(x2, 0.285_182_118_415_832_519_531_25)
        .mla(x2, 0.400_005_877_017_974_853_515_625)
        .mla(x2, 0.666_666_686_534_881_591_796_875)
        .mla(x2, 2.);

    x.mla(t, 0.693_147_180_559_945_286_226_764 * (e as f32))
}

#[inline]
fn expk3f(d: f32) -> f32 {
    let q = rintfk(d * R_LN2_F);

    let mut s = q.mla(-L2U_F, d);
    s = q.mla(-L2L_F, s);

    let mut u = 0.000_198_527_617_612_853_646_278_381
        .mla(s, 0.001_393_043_552_525_341_510_772_71)
        .mla(s, 0.008_333_360_776_305_198_669_433_59)
        .mla(s, 0.041_666_485_369_205_474_853_515_6)
        .mla(s, 0.166_666_671_633_720_397_949_219)
        .mla(s, 0.5);

    u = (s * s).mla(u, s + 1.);
    u = ldexpkf(u, q as i32);

    if d < -104. {
        0.
    } else {
        u
    }
}

/// Fast power function
///
/// The error bounds of the returned value is `350 ULP`.
pub fn powf(x: f32, y: f32) -> f32 {
    let mut result = expk3f(logk3f(fabsfk(x)) * y);

    let yisint = (y == (y as i32 as f32)) || (fabsfk(y) >= F1_24);
    let yisodd = (1 & (y as i32)) != 0 && yisint && (fabsfk(y) < F1_24);

    result *= if (x < 0.) && yisodd { -1. } else { 1. };

    if y == 0. {
        1.
    } else if x == 0. {
        0.
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
        350.,
    );
}
