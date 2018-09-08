use super::*;

pub fn sqrtf(mut d: f32) -> f32 {
    let mut q = 0.5;

    d = if d < 0. { SLEEF_NAN_F } else { d };

    if d < 5.2939559203393770e-23 {
        d *= 1.8889465931478580e+22;
        q = 7.2759576141834260e-12 * 0.5;
    }

    if d > 1.8446744073709552e+19 {
        d *= 5.4210108624275220e-20;
        q = 4294967296. * 0.5;
    }

    // http://en.wikipedia.org/wiki/Fast_inverse_square_root
    let mut x = int_bits_to_float(0x5f375a86 - (float_to_raw_int_bits(d + 1e-45) >> 1));

    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;
    x *= (1.5 - 0.5 * d * x * x) * d;

    let d2 = (d + x.mul_as_f2(x)) * x.recpre();

    if (d == 0.) || (d == SLEEF_INFINITY_F) {
        d
    } else {
        (d2.0 + d2.1) * q
    }
}

pub fn sincospif(d: f32) -> (f32, f32) {
    let u = d * 4.;
    let q = super::ceilfk(u) & !1_i32;

    let s = u - (q as f32);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_f2(t);

    //

    let u = 0.3093842054e-6
        .mul_add(s, -0.3657307388e-4)
        .mul_add(s, 0.2490393585e-2);
    let mut x = u * s + df(-0.080745510756969451904, -1.3373665339076936258e-09);
    x = s2 * x + df(0.78539818525314331055, -2.1857338617566484855e-08);

    x *= t;
    let mut rsin = x.0 + x.1;
    if xisnegzerof(d) {
        rsin = -0.;
    }

    let u = (-0.2430611801e-7)
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
    if d.isinf() {
        rsin = SLEEF_NAN_F;
        rcos = SLEEF_NAN_F;
    }

    (rsin, rcos)
}

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
    if (x == SLEEF_INFINITY_F) || (y == SLEEF_INFINITY_F) {
        SLEEF_INFINITY_F
    } else if x.isnan() || y.isnan() {
        SLEEF_NAN_F
    } else if min == 0. {
        max
    } else if ret.isnan() {
        SLEEF_INFINITY_F
    } else {
        ret
    }
}

pub fn sinpif(d: f32) -> f32 {
    let x = super::sinpifk(d);

    if d.isinf() {
        SLEEF_NAN_F
    } else if fabsfk(d) > TRIGRANGEMAX4_F {
        0.
    } else if xisnegzerof(d) {
        -0.
    } else {
        x.0 + x.1
    }
}

pub fn cospif(d: f32) -> f32 {
    let x = super::cospifk(d);

    if d.isinf() {
        SLEEF_NAN_F
    } else if fabsfk(d) > TRIGRANGEMAX4_F {
        1.
    } else {
        x.0 + x.1
    }
}
