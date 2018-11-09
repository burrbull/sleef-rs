//! Functions with 3.5 ULP error bound

use super::*;

/// Arc tangent function of two variables
///
/// These functions evaluates the arc tangent function of (***y*** / ***x***).
/// The quadrant of the result is determined according to the signs of ***x*** and ***y***.
/// The error bound of the returned value is 3.5 ULP.
pub fn atan2f(y: f32, x: f32) -> f32 {
    let mut r = atan2kf(fabsfk(y), x);

    r = if x.isinf() || (x == 0.) {
        M_PI_2_F - (if x.isinf() { signf(x) * M_PI_2_F } else { 0. })
    } else if y.isinf() {
        M_PI_2_F - (if x.isinf() { signf(x) * M_PI_4_F } else { 0. })
    } else if y == 0. {
        (if signf(x) == -1. { M_PI_F } else { 0. })
    } else {
        mulsignf(r, x)
    };

    if x.isnan() || y.isnan() {
        SLEEF_NAN_F
    } else {
        mulsignf(r, y)
    }
}

/// Natural logarithmic function
///
/// These functions return the natural logarithm of ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn logf(mut d: f32) -> f32 {
    let o = d < f32::MIN;
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

    let t = 0.2392828464508056640625_f32
        .mul_add(x2, 0.28518211841583251953125)
        .mul_add(x2, 0.400005877017974853515625)
        .mul_add(x2, 0.666666686534881591796875)
        .mul_add(x2, 2.);

    if d == 0. {
        -SLEEF_INFINITY_F
    } else if (d < 0.) || d.isnan() {
        SLEEF_NAN_F
    } else if d.isinf() {
        SLEEF_INFINITY_F
    } else {
        x * t + 0.693147180559945286226764 * (e as f32)
    }
}

/// Evaluate sin( π**a** ) and cos( π**a** ) for given **a** simultaneously
///
/// Evaluates the sine and cosine functions of πa at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// The error bound of the returned values is 3.5 ULP if ***a*** is in [-1e+7, 1e+7].
/// If a is a finite value out of this range, an arbitrary value within [-1, 1] is returned.
/// If a is a NaN or infinity, a NaN is returned. 
pub fn sincospif(d: f32) -> (f32, f32) {
    let u = d * 4.;
    let q = ceilfk(u) & !1_i32;

    let s = u - (q as f32);
    let t = s;
    let s = s * s;

    let mut rsin = -0.3600925265e-4_f32
        .mul_add(s, 0.2490088111e-2)
        .mul_add(s, -0.8074551076e-1)
        .mul_add(s, 0.7853981853e+0)
        * t;

    let mut rcos = 0.3539815225e-5_f32
        .mul_add(s, -0.3259574005e-3)
        .mul_add(s, 0.1585431583e-1)
        .mul_add(s, -0.3084251285e+0)
        .mul_add(s, 1.);

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

/// Hyperbolic sine function
///
/// These functions evaluates the hyperbolic sine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP if ***a*** is in [-709, 709]
/// for the double-precision function or [-88, 88] for the single-precision function.
/// If ***a*** is a finite value out of this range, infinity with a correct sign
/// or a correct value with 3.5 ULP error bound is returned.
pub fn sinhf(x: f32) -> f32 {
    let e = expm1kf(fabsfk(x));
    let mut y = (e + 2.) / (e + 1.) * (0.5 * e);

    y = if fabsfk(x) > 88. { SLEEF_INFINITY_F } else { y };
    y = if y.isnan() { SLEEF_INFINITY_F } else { y };
    y = mulsignf(y, x);
    if x.isnan() {
        SLEEF_NAN_F
    } else {
        y
    }
}

/// Hyperbolic cosine function
///
/// These functions evaluates the hyperbolic cosine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP if a is in [-709, 709]
/// for the double-precision function or [-88, 88] for the single-precision function.
/// If ***a*** is a finite value out of this range, infinity with a correct sign
/// or a correct value with 3.5 ULP error bound is returned.
pub fn coshf(x: f32) -> f32 {
    let e = u10::expf(fabsfk(x));
    let mut y = 0.5 * e + 0.5 / e;

    y = if fabsfk(x) > 88. { SLEEF_INFINITY_F } else { y };
    y = if y.isnan() { SLEEF_INFINITY_F } else { y };
    if x.isnan() {
        SLEEF_NAN_F
    } else {
        y
    }
}

/// Hyperbolic tangent function
///
/// These functions evaluates the hyperbolic tangent function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP for the double-precision
/// function or 3.5 ULP for the single-precision function.
pub fn tanhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let d = expm1kf(2. * y);
    y = d / (d + 2.);

    y = if fabsfk(x) > 18.714973875 { 1. } else { y };
    y = if y.isnan() { 1. } else { y };
    y = mulsignf(y, x);
    if x.isnan() {
        SLEEF_NAN_F
    } else {
        y
    }
}

/// 2D Euclidian distance function
///
/// The error bound of the returned value is 3.5 ULP.
pub fn hypotf(mut x: f32, mut y: f32) -> f32 {
    x = fabsfk(x);
    y = fabsfk(y);
    let min = fminfk(x, y);
    let max = fmaxfk(x, y);

    let t = min / max;
    if (x == SLEEF_INFINITY_F) || (y == SLEEF_INFINITY_F) {
        SLEEF_INFINITY_F
    } else if x.isnan() || y.isnan() {
        SLEEF_NAN_F
    } else if min == 0. {
        max
    } else {
        max * (1. + t * t).sqrt()
    }
}

/// Square root function
///
/// The error bound of the returned value is 3.5 ULP.
pub fn sqrtf(mut d: f32) -> f32 {
    let mut q = 1.;

    d = if d < 0. { SLEEF_NAN_F } else { d };

    if d < 5.2939559203393770e-23 {
        d *= 1.8889465931478580e+22;
        q = 7.2759576141834260e-12;
    }

    if d > 1.8446744073709552e+19 {
        d *= 5.4210108624275220e-20;
        q = 4294967296.0;
    }

    // http://en.wikipedia.org/wiki/Fast_inverse_square_root
    let mut x = f32::from_bits(0x5f375a86 - ((d + 1e-45).to_bits() >> 1));

    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;
    x *= 1.5 - 0.5 * d * x * x;

    if d == SLEEF_INFINITY_F {
        SLEEF_INFINITY_F
    } else {
        (x * d * q)
    }
}

/// Sine function
///
/// These functions evaluates the sine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn sinf(mut d: f32) -> f32 {
    let q: i32;
    let t = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * M_1_PI_F);
        q = qf as i32;
        d = qf.mul_add(-PI_A2_F, d);
        d = qf.mul_add(-PI_B2_F, d);
        d = qf.mul_add(-PI_C2_F, d);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * M_1_PI_F);
        q = qf as i32;
        d = qf.mul_add(-PI_A_F, d);
        d = qf.mul_add(-PI_B_F, d);
        d = qf.mul_add(-PI_C_F, d);
        d = qf.mul_add(-PI_D_F, d);
    } else {
        let (mut dfidf, dfii) = rempif(t);
        q = ((dfii & 3) * 2 + ((dfidf.0 > 0.) as i32) + 1) >> 2;
        if (dfii & 1) != 0 {
            dfidf += df(
                mulsignf(3.1415927410125732422 * -0.5, dfidf.0),
                mulsignf(-8.7422776573475857731e-08 * -0.5, dfidf.0),
            );
        }
        d = dfidf.0 + dfidf.1;
        if t.isinf() || t.isnan() {
            d = SLEEF_NAN_F;
        }
    }

    let s = d * d;

    if (q & 1) != 0 {
        d = -d;
    }

    let u = 2.6083159809786593541503e-06_f32
        .mul_add(s, -0.0001981069071916863322258)
        .mul_add(s, 0.00833307858556509017944336)
        .mul_add(s, -0.166666597127914428710938);

    if xisnegzerof(t) {
        -0.
    } else {
        s.mul_add(u * d, d)
    }
}

/// Cosine function
///
/// These functions evaluates the cosine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn cosf(mut d: f32) -> f32 {
    let q: i32;
    let t = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        q = 1 + 2 * (rintfk(d * M_1_PI_F - 0.5) as i32);
        let qf = q as f32;
        d = qf.mul_add(-PI_A2_F * 0.5, d);
        d = qf.mul_add(-PI_B2_F * 0.5, d);
        d = qf.mul_add(-PI_C2_F * 0.5, d);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        q = 1 + 2 * (rintfk(d * M_1_PI_F - 0.5) as i32);
        let qf = q as f32;
        d = qf.mul_add(-PI_A_F * 0.5, d);
        d = qf.mul_add(-PI_B_F * 0.5, d);
        d = qf.mul_add(-PI_C_F * 0.5, d);
        d = qf.mul_add(-PI_D_F * 0.5, d);
    } else {
        let (mut dfidf, dfii) = rempif(t);
        q = ((dfii & 3) * 2 + ((dfidf.0 > 0.) as i32) + 7) >> 1;
        if (dfii & 1) == 0 {
            dfidf += df(
                mulsignf(
                    3.1415927410125732422 * -0.5,
                    if dfidf.0 > 0. { 1. } else { -1. },
                ),
                mulsignf(
                    -8.7422776573475857731e-08 * -0.5,
                    if dfidf.0 > 0. { 1. } else { -1. },
                ),
            );
        }
        d = dfidf.0 + dfidf.1;
        if t.isinf() || t.isnan() {
            d = SLEEF_NAN_F;
        }
    }

    let s = d * d;

    if (q & 2) == 0 {
        d = -d;
    }

    let u = 2.6083159809786593541503e-06_f32
        .mul_add(s, -0.0001981069071916863322258)
        .mul_add(s, 0.00833307858556509017944336)
        .mul_add(s, -0.166666597127914428710938);

    s.mul_add(u * d, d)
}

/// Evaluate sine and cosine function simultaneously
///
/// Evaluates the sine and cosine functions of a value in ***a*** at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// The error bound of the returned values is 3.5 ULP.
/// If ***a*** is a NaN or infinity, a NaN is returned.
pub fn sincosf(d: f32) -> (f32, f32) {
    let q: i32;

    let mut s = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * M_2_PI_F);
        q = qf as i32;
        s = qf.mul_add(-PI_A2_F * 0.5, s);
        s = qf.mul_add(-PI_B2_F * 0.5, s);
        s = qf.mul_add(-PI_C2_F * 0.5, s);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * M_2_PI_F);
        q = qf as i32;
        s = qf.mul_add(-PI_A_F * 0.5, s);
        s = qf.mul_add(-PI_B_F * 0.5, s);
        s = qf.mul_add(-PI_C_F * 0.5, s);
        s = qf.mul_add(-PI_D_F * 0.5, s);
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        s = dfidf.0 + dfidf.1;
        if d.isinf() || d.isnan() {
            s = SLEEF_NAN_F;
        }
    }

    let t = s;

    s = s * s;

    let mut u = (-0.000195169282960705459117889_f32)
        .mul_add(s, 0.00833215750753879547119141)
        .mul_add(s, -0.166666537523269653320312);
    u = u * s * t;

    let mut rsin = t + u;

    if xisnegzerof(d) {
        rsin = -0.;
    }

    u = (-2.71811842367242206819355e-07_f32)
        .mul_add(s, 2.47990446951007470488548e-05)
        .mul_add(s, -0.00138888787478208541870117)
        .mul_add(s, 0.0416666641831398010253906)
        .mul_add(s, -0.5);

    let mut rcos = u * s + 1.;

    if (q & 1) != 0 {
        let s = rcos;
        rcos = rsin;
        rsin = s;
    }
    if (q & 2) != 0 {
        rsin = -rsin;
    }
    if ((q + 1) & 2) != 0 {
        rcos = -rcos;
    }

    (rsin, rcos)
}

/// Tangent function
///
/// These functions evaluates the tangent function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn tanf(d: f32) -> f32 {
    let q;

    let mut x = d;

    if fabsfk(d) < TRIGRANGEMAX2_F * 0.5 {
        let qf = rintfk(d * M_2_PI_F);
        q = qf as i32;
        x = qf.mul_add(-PI_A2_F * 0.5, x);
        x = qf.mul_add(-PI_B2_F * 0.5, x);
        x = qf.mul_add(-PI_C2_F * 0.5, x);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * M_2_PI_F);
        q = qf as i32;
        x = qf.mul_add(-PI_A_F * 0.5, x);
        x = qf.mul_add(-PI_B_F * 0.5, x);
        x = qf.mul_add(-PI_C_F * 0.5, x);
        x = qf.mul_add(-PI_D_F * 0.5, x);
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        x = dfidf.0 + dfidf.1;
        if d.isinf() || d.isnan() {
            x = SLEEF_NAN_F;
        }
    }

    let s = x * x;

    if (q & 1) != 0 {
        x = -x;
    }

    let mut u = 0.00927245803177356719970703_f32
        .mul_add(s, 0.00331984995864331722259521)
        .mul_add(s, 0.0242998078465461730957031)
        .mul_add(s, 0.0534495301544666290283203)
        .mul_add(s, 0.133383005857467651367188)
        .mul_add(s, 0.333331853151321411132812);

    u = s.mul_add(u * x, x);

    if (q & 1) != 0 {
        1. / u
    } else {
        u
    }
}

/// Arc tangent function
///
/// These functions evaluates the arc tangent function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn atanf(mut s: f32) -> f32 {
    let mut q = 0;

    if signf(s) == -1. {
        s = -s;
        q = 2;
    }
    if s > 1. {
        s = 1. / s;
        q |= 1;
    }

    let mut t = s * s;

    let u = 0.00282363896258175373077393_f32
        .mul_add(t, -0.0159569028764963150024414)
        .mul_add(t, 0.0425049886107444763183594)
        .mul_add(t, -0.0748900920152664184570312)
        .mul_add(t, 0.106347933411598205566406)
        .mul_add(t, -0.142027363181114196777344)
        .mul_add(t, 0.199926957488059997558594)
        .mul_add(t, -0.333331018686294555664062);

    t = s + s * (t * u);

    if (q & 1) != 0 {
        t = 1.570796326794896557998982 - t;
    }
    if (q & 2) != 0 {
        -t
    } else {
        t
    }
}

/// Arc sine function
///
/// These functions evaluates the arc sine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn asinf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let x = if o { fabsfk(d) } else { x2.sqrt() };

    let u = 0.4197454825e-1_f32
        .mul_add(x2, 0.2424046025e-1)
        .mul_add(x2, 0.4547423869e-1)
        .mul_add(x2, 0.7495029271e-1)
        .mul_add(x2, 0.1666677296e+0)
        .mul_add(x * x2, x);

    let r = if o { u } else { (M_PI_F / 2. - 2. * u) };
    mulsignf(r, d)
}

/// Arc cosine function
///
/// These functions evaluates the arc cosine function of a value in ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn acosf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let mut x = if o { fabsfk(d) } else { x2.sqrt() };
    x = if fabsfk(d) == 1. { 0. } else { x };

    let mut u = 0.4197454825e-1_f32
        .mul_add(x2, 0.2424046025e-1)
        .mul_add(x2, 0.4547423869e-1)
        .mul_add(x2, 0.7495029271e-1)
        .mul_add(x2, 0.1666677296e+0);

    u *= x * x2;

    let y = 3.1415926535897932 / 2. - (mulsignf(x, d) + mulsignf(u, d));
    x += u;
    let r = if o { y } else { x * 2. };
    if !o && (d < 0.) {
        df(3.1415927410125732422, -8.7422776573475857731e-08)
            .add_checked(-r)
            .0
    } else {
        r
    }
}

/// Cube root function
///
/// These functions return the real cube root of ***a***.
/// The error bound of the returned value is 3.5 ULP.
pub fn cbrtf(mut d: f32) -> f32 {
    let e = ilogbkf(fabsfk(d)) + 1;
    d = ldexp2kf(d, -e);
    let r = (e + 6144) % 3;
    let mut q = if r == 1 {
        1.2599210498948731647672106
    } else {
        1.
    };
    q = if r == 2 {
        1.5874010519681994747517056
    } else {
        q
    };
    q = ldexp2kf(q, (e + 6144) / 3 - 2048);

    q = mulsignf(q, d);
    d = fabsfk(d);

    let x = (-0.601564466953277587890625_f32)
        .mul_add(d, 2.8208892345428466796875)
        .mul_add(d, -5.532182216644287109375)
        .mul_add(d, 5.898262500762939453125)
        .mul_add(d, -3.8095417022705078125)
        .mul_add(d, 2.2241256237030029296875);

    let y = d * x * x;
    (y - (2. / 3.) * y * (y * x - 1.)) * q
}
