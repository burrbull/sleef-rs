//! Functions with 0.5 ULP error bound

use super::*;

/// Evaluate sin( π***a*** ) and cos( π***a*** ) for given ***a*** simultaneously
///
/// Evaluates the sine and cosine functions of π***a*** at a time, and store the two values in a tuple.
/// The error bound of the returned value are `max(0.506 ULP, f32::MIN_POSITIVE)` if `[-1e+7, 1e+7]`.
/// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
/// If ***a*** is a `NaN` or infinity, a `NaN` is returned.
pub fn sincospif(d: f32) -> (f32, f32) {
    let u = d * 4.;
    let q = super::ceilfk(u) & !1_i32;

    let s = u - (q as f32);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = 0.309_384_205_4_e-6_f32
        .mul_add(s, -0.365_730_738_8_e-4)
        .mul_add(s, 0.249_039_358_5_e-2);
    let mut x = u * s
        + df(
            -0.080_745_510_756_969_451_904,
            -1.337_366_533_907_693_625_8_e-9,
        );
    x = s2 * x
        + df(
            0.785_398_185_253_143_310_55,
            -2.185_733_861_756_648_485_5_e-8,
        );

    x *= t;
    let mut rsin = if d.is_neg_zero() { -0. } else { f32::from(x) };

    let u = (-0.243_061_180_1_e-7_f32)
        .mul_add(s, 0.359_057_708_e-5)
        .mul_add(s, -0.325_991_772_1_e-3);
    x = u * s
        + df(
            0.015_854_343_771_934_509_277,
            4.494_005_135_403_224_281_1_e-10,
        );
    x = s2 * x
        + df(
            -0.308_425_128_459_930_419_92,
            -9.072_833_903_073_392_227_7_e-9,
        );

    x = x * s2 + 1.;
    let mut rcos = f32::from(x);

    if (q & 2) != 0 {
        core::mem::swap(&mut rcos, &mut rsin)
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

#[test]
fn test_sincospif() {
    let rangemax2 = 1e+7 / 4.;
    test_f_ff(
        sincospif,
        |mut in1| {
            let prec = in1.prec();
            in1.set_prec(prec * 2);
            (in1 * Float::with_val(prec * 2, rug::float::Constant::Pi)).sin_cos(Float::new(prec))
        },
        -rangemax2..=rangemax2,
        0.505,
    );
}

/// Evaluate sin( π***a*** ) for given ***a***
///
/// This function evaluates the sine function of π***a***.
/// The error bound of the returned value is `max(0.506 ULP, f32::MIN_POSITIVE)`
/// if `[-1e+7, 1e+7]` for the single-precision function.
/// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
/// If ***a*** is a `NaN` or infinity, a NaN is returned.
pub fn sinpif(d: f32) -> f32 {
    let x = super::sinpifk(d);

    if d.is_infinite() {
        f32::NAN
    } else if fabsfk(d) > TRIGRANGEMAX4_F {
        0.
    } else if d.is_neg_zero() {
        -0.
    } else {
        f32::from(x)
    }
}

#[test]
fn test_sinpif() {
    let rangemax2 = 1e+7 / 4.;
    test_f_f(
        sinpif,
        |mut in1| {
            let prec = in1.prec();
            in1.set_prec(prec * 2);
            (in1 * Float::with_val(prec * 2, rug::float::Constant::Pi)).sin()
        },
        -rangemax2..=rangemax2,
        0.506,
    );
}

/// Evaluate cos( π***a*** ) for given ***a***
///
/// This function evaluates the cosine function of π***a***.
/// The error bound of the returned value is `max(0.506 ULP, f32::MIN_POSITIVE)`
/// if `[-1e+7, 1e+7]` for the single-precision function.
/// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
/// If ***a*** is a `NaN` or infinity, a `NaN` is returned.
pub fn cospif(d: f32) -> f32 {
    let x = super::cospifk(d);

    if d.is_infinite() {
        f32::NAN
    } else if fabsfk(d) > TRIGRANGEMAX4_F {
        1.
    } else {
        f32::from(x)
    }
}

#[test]
fn test_cospif() {
    let rangemax2 = 1e+7 / 4.;
    test_f_f(
        cospif,
        |mut in1| {
            let prec = in1.prec();
            in1.set_prec(prec * 2);
            (in1 * Float::with_val(prec * 2, rug::float::Constant::Pi)).cos()
        },
        -rangemax2..=rangemax2,
        0.506,
    );
}

/// Square root function
///
/// The error bound of the returned value is `0.5001 ULP`.
pub fn sqrtf(mut d: f32) -> f32 {
    let mut q = 0.5;

    d = if d < 0. { f32::NAN } else { d };

    if d < 5.293_955_920_339_377_e-23 {
        d *= 1.888_946_593_147_858_e+22;
        q = 7.275_957_614_183_426_e-12 * 0.5;
    }

    if d > 1.844_674_407_370_955_2_e+19 {
        d *= 5.421_010_862_427_522_e-20;
        q = 4_294_967_296. * 0.5;
    }

    // http://en.wikipedia.org/wiki/Fast_inverse_square_root
    let mut x = f32::from_bits(0x_5f37_5a86 - ((d + 1e-45).to_bits() >> 1));

    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;
    x *= (1.5 - 0.5 * d * x * x) * d;

    let d2 = (d + x.mul_as_doubled(x)) * x.recpre();

    if (d == 0.) || (d == f32::INFINITY) {
        d
    } else {
        f32::from(d2) * q
    }
}

#[test]
fn test_sqrtf() {
    test_f_f(sqrtf, rug::Float::sqrt, f32::MIN..=f32::MAX, 0.5);
}

/// 2D Euclidian distance function
///
/// The error bound of the returned value is `0.5001 ULP`.
pub fn hypotf(mut x: f32, mut y: f32) -> f32 {
    x = fabsfk(x);
    y = fabsfk(y);
    let min = x.min(y);
    let mut n = min;
    let max = x.max(y);
    let mut d = max;

    if max < f32::MIN_POSITIVE {
        n *= F1_24;
        d *= F1_24;
    }
    let mut t = df(n, 0.) / df(d, 0.);
    t = (t.square() + 1.).sqrt() * max;

    let ret = f32::from(t);
    if (x == f32::INFINITY) || (y == f32::INFINITY) {
        f32::INFINITY
    } else if x.is_nan() || y.is_nan() {
        f32::NAN
    } else if min == 0. {
        max
    } else if ret.is_nan() {
        f32::INFINITY
    } else {
        ret
    }
}

#[test]
fn test_hypotf() {
    test_ff_f(
        hypotf,
        rug::Float::hypot,
        f32::MIN..=f32::MAX,
        f32::MIN..=f32::MAX,
        0.5001,
    );
}
