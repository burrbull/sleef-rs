//! Fast functions with 350 ULP error bound

use super::*;

/// Fast sine function
///
/// The error bounds of the returned value is `min(350 ULP, 2e-6)`.
pub fn sinf(mut d: f32) -> f32 {
    let t = d;

    let q = rintfk(d * FRAC_1_PI);
    d = q.mul_add(-PI, d);

    let s = d * d;

    let mut u = (-0.188_174_817_6_e-3)
        .mul_add(s, 0.832_350_272_7_e-2)
        .mul_add(s, -0.166_665_136_8);
    u = (s * d).mul_add(u, d);

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

    let q = rintfk(d.mul_add(FRAC_1_PI, -0.5));
    d = q.mul_add(-PI, d - FRAC_PI_2);

    let s = d * d;

    let mut u = (-0.188_174_817_6_e-3)
        .mul_add(s, 0.832_350_272_7_e-2)
        .mul_add(s, -0.166_665_136_8);
    u = (s * d).mul_add(u, d);

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
