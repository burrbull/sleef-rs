//! Functions with 0.5 ULP error bound

use super::*;

/// Square root function
///
/// The error bound of the returned value is 0.5001 ULP.
pub fn sqrtf(mut d: f32) -> f32 {
    let mut q = 0.5;

    d = if d < 0. { f32::NAN } else { d };

    if d < 5.2939559203393770e-23 {
        d *= 1.8889465931478580e+22;
        q = 7.2759576141834260e-12 * 0.5;
    }

    if d > 1.8446744073709552e+19 {
        d *= 5.4210108624275220e-20;
        q = 4294967296. * 0.5;
    }

    // http://en.wikipedia.org/wiki/Fast_inverse_square_root
    let mut x = f32::from_bits(0x5f375a86 - ((d + 1e-45).to_bits() >> 1));

    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;
    x *= (1.5 - 0.5 * d * x * x) * d;

    let d2 = (d + x.mul_as_doubled(x)) * x.recpre();

    if (d == 0.) || (d == f32::INFINITY) {
        d
    } else {
        (d2.0 + d2.1) * q
    }
}

/// Evaluate sin( π***a*** ) and cos( π***a*** ) for given ***a*** simultaneously
///
/// Evaluates the sine and cosine functions of π***a*** at a time, and store the two values in a tuple.
/// The error bound of the returned value are `max(0.506 ULP, f32::MIN)` if [-1e+7, 1e+7].
/// If ***a*** is a finite value out of this range, an arbitrary value within [-1, 1] is returned.
/// If ***a*** is a NaN or infinity, a NaN is returned.
pub fn sincospif(d: f32) -> (f32, f32) {
    let u = d * 4.;
    let q = super::ceilfk(u) & !1_i32;

    let s = u - (q as f32);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = 0.3093842054e-6_f32
        .mul_add(s, -0.3657307388e-4)
        .mul_add(s, 0.2490393585e-2);
    let mut x = u * s + df(-0.080745510756969451904, -1.3373665339076936258e-09);
    x = s2 * x + df(0.78539818525314331055, -2.1857338617566484855e-08);

    x *= t;
    let mut rsin = x.0 + x.1;
    if xisnegzerof(d) {
        rsin = -0.;
    }

    let u = (-0.2430611801e-7_f32)
        .mul_add(s, 0.3590577080e-5)
        .mul_add(s, -0.3259917721e-3);
    x = u * s + df(0.015854343771934509277, 4.4940051354032242811e-10);
    x = s2 * x + df(-0.30842512845993041992, -9.0728339030733922277e-09);

    x = x * s2 + 1.;
    let mut rcos = x.0 + x.1;

    if (q & 2) != 0 {
        let s = rcos;
        rcos = rsin;
        rsin = s;
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

/// 2D Euclidian distance function
///
/// The error bound of the returned value is 0.5001 ULP.
pub fn hypotf(mut x: f32, mut y: f32) -> f32 {
    x = fabsfk(x);
    y = fabsfk(y);
    let min = super::fminfk(x, y);
    let mut n = min;
    let max = super::fmaxfk(x, y);
    let mut d = max;

    if max < f32::MIN {
        n *= F1_24;
        d *= F1_24;
    }
    let mut t = df(n, 0.) / df(d, 0.);
    t = (t.square() + 1.).sqrt() * max;

    let ret = t.0 + t.1;
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

/// Evaluate sin( π***a*** ) for given ***a***
///
/// This function evaluates the sine function of π***a***.
/// The error bound of the returned value is `max(0.506 ULP, f32::MIN)`
/// if [-1e+7, 1e+7] for the single-precision function.
/// If ***a*** is a finite value out of this range, an arbitrary value within [-1, 1] is returned.
/// If ***a*** is a NaN or infinity, a NaN is returned.
pub fn sinpif(d: f32) -> f32 {
    let x = super::sinpifk(d);

    if d.is_infinite() {
        f32::NAN
    } else if fabsfk(d) > TRIGRANGEMAX4_F {
        0.
    } else if xisnegzerof(d) {
        -0.
    } else {
        x.0 + x.1
    }
}

/// Evaluate cos( π***a*** ) for given ***a***
///
/// This function evaluates the cosine function of π***a***.
/// The error bound of the returned value is `max(0.506 ULP, f32::MIN)`
/// if [-1e+7, 1e+7] for the single-precision function.
/// If ***a*** is a finite value out of this range, an arbitrary value within [-1, 1] is returned.
/// If ***a*** is a NaN or infinity, a NaN is returned.
pub fn cospif(d: f32) -> f32 {
    let x = super::cospifk(d);

    if d.is_infinite() {
        f32::NAN
    } else if fabsfk(d) > TRIGRANGEMAX4_F {
        1.
    } else {
        x.0 + x.1
    }
}
