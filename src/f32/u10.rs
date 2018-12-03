//! Functions with 1.0 ULP error bound

use super::*;

/// Base-2 exponential function
///
/// This function returns 2 raised to ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn exp2f(d: f32) -> f32 {
    let qf = rintfk(d);
    let q = qf as i32;
    let s = d - qf;

    let mut u = 0.1535920892e-3_f32
        .mul_add(s, 0.1339262701e-2)
        .mul_add(s, 0.9618384764e-2)
        .mul_add(s, 0.5550347269e-1)
        .mul_add(s, 0.2402264476e+0)
        .mul_add(s, 0.6931471825e+0);
    u = (1.).add_checked(u.mul_as_doubled(s)).normalize().0;

    if d >= 128. {
        f32::INFINITY
    } else if d < -150. {
        0.
    } else {
        ldexp2kf(u, q)
    }
}

/// Sine function
///
/// This function evaluates the sine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn sinf(d: f32) -> f32 {
    let q: i32;
    let mut s: Doubled<f32>;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * FRAC_1_PI);
        q = qf as i32;
        let u = qf.mul_add(-PI_A2_F, d);
        s = u.add_as_doubled(qf * (-PI_B2_F));
        s.add_checked_assign(qf * (-PI_C2_F));
    } else {
        let (mut dfidf, dfii) = rempif(d);
        q = ((dfii & 3) * 2 + ((dfidf.0 > 0.) as i32) + 1) >> 2;
        if (dfii & 1) != 0 {
            dfidf += df(
                mulsignf(3.1415927410125732422 * -0.5, dfidf.0),
                mulsignf(-8.7422776573475857731e-08 * -0.5, dfidf.0),
            );
        }
        s = dfidf.normalize();
        if d.is_infinite() || d.is_nan() {
            s.0 = f32::NAN;
        }
    }

    let t = s;
    s = s.square();

    let mut u = 2.6083159809786593541503e-06_f32
        .mul_add(s.0, -0.0001981069071916863322258)
        .mul_add(s.0, 0.00833307858556509017944336);

    let x = (1.).add_checked((-0.166666597127914428710938).add_checked_as_doubled(u * s.0) * s);

    u = t.mul_as_f(x);

    if (q & 1) != 0 {
        u = -u;
    }
    if xisnegzerof(d) {
        d
    } else {
        u
    }
}

/// Cosine function
///
/// This function evaluates the cosine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn cosf(mut d: f32) -> f32 {
    let mut s: Doubled<f32>;
    let q: i32;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        d = fabsfk(d);
        let dq = rintfk(d * FRAC_1_PI - 0.5).mul_add(2., 1.);
        q = dq as i32;
        s = d.add_as_doubled(dq * (-PI_A2_F * 0.5));
        s += dq * (-PI_B2_F * 0.5);
        s += dq * (-PI_C2_F * 0.5);
    } else {
        let (mut dfidf, dfii) = rempif(d);
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
        s = dfidf.normalize();
        if d.is_infinite() || d.is_nan() {
            s.0 = f32::NAN;
        }
    }

    let t = s;
    s = s.square();

    let mut u = 2.6083159809786593541503e-06_f32
        .mul_add(s.0, -0.0001981069071916863322258)
        .mul_add(s.0, 0.00833307858556509017944336);

    let x = (1.).add_checked((-0.166666597127914428710938).add_checked_as_doubled(u * s.0) * s);

    u = t.mul_as_f(x);

    if (q & 2) == 0 {
        -u
    } else {
        u
    }
}

/// Evaluate sine and cosine functions simultaneously
///
/// Evaluates the sine and cosine functions of a value in a at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// returned value, respectively.
/// The error bound of the returned values is 1.0 ULP.
/// If ***a*** is a NaN or infinity, a NaN is returned.
pub fn sincosf(d: f32) -> (f32, f32) {
    let q: i32;
    let mut s: Doubled<f32>;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * FRAC_2_PI);
        q = qf as i32;
        let u = qf.mul_add(-PI_A2_F * 0.5, d);
        s = u.add_as_doubled(qf * (-PI_B2_F * 0.5));
        s.add_checked_assign(qf * (-PI_C2_F * 0.5));
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        s = dfidf;
        if d.is_infinite() || d.is_nan() {
            s.0 = f32::NAN;
        }
    }

    let t = s;
    s.0 = s.square_as_f();

    let u = (-0.000195169282960705459117889_f32)
        .mul_add(s.0, 0.00833215750753879547119141)
        .mul_add(s.0, -0.166666537523269653320312)
        * s.0
        * t.0;

    let mut x = t.add_checked(u);
    let mut rsin = x.0 + x.1;
    if xisnegzerof(d) {
        rsin = -0.;
    }

    let u = (-2.71811842367242206819355e-07_f32)
        .mul_add(s.0, 2.47990446951007470488548e-05)
        .mul_add(s.0, -0.00138888787478208541870117)
        .mul_add(s.0, 0.0416666641831398010253906)
        .mul_add(s.0, -0.5);

    x = (1.).add_checked(s.0.mul_as_doubled(u));
    let mut rcos = x.0 + x.1;

    if (q & 1) != 0 {
        let u = rcos;
        rcos = rsin;
        rsin = u;
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
/// This function evaluates the tangent function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn tanf(d: f32) -> f32 {
    let q: i32;
    let mut s: Doubled<f32>;
    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * FRAC_2_PI);
        q = qf as i32;
        let u = qf.mul_add(-PI_A2_F * 0.5, d);
        s = u.add_as_doubled(qf * (-PI_B2_F * 0.5));
        s.add_checked_assign(qf * (-PI_C2_F * 0.5));
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        s = dfidf;
        if d.is_infinite() || d.is_nan() {
            s.0 = f32::NAN;
        }
    }

    if (q & 1) != 0 {
        s = -s;
    }

    let t = s;
    s = s.square().normalize();

    let u = 0.00446636462584137916564941_f32
        .mul_add(s.0, -8.3920182078145444393158e-05)
        .mul_add(s.0, 0.0109639242291450500488281)
        .mul_add(s.0, 0.0212360303848981857299805)
        .mul_add(s.0, 0.0540687143802642822265625);

    let mut x = (0.133325666189193725585938).add_checked_as_doubled(u * s.0);
    x = (1.).add_checked((0.33333361148834228515625).add_checked(s * x) * s);
    x = t * x;

    if (q & 1) != 0 {
        x = x.recpre();
    }

    if xisnegzerof(d) {
        -0.
    } else {
        x.0 + x.1
    }
}

fn atan2kf_u1(mut y: Doubled<f32>, mut x: Doubled<f32>) -> Doubled<f32> {
    let mut q = 0;

    if x.0 < 0. {
        x.0 = -x.0;
        x.1 = -x.1;
        q = -2;
    }
    if y.0 > x.0 {
        let t = x;
        x = y;
        y.0 = -t.0;
        y.1 = -t.1;
        q += 1;
    }

    let s = y / x;
    let mut t = s.square().normalize();

    let u = (-0.00176397908944636583328247_f32)
        .mul_add(t.0, 0.0107900900766253471374512)
        .mul_add(t.0, -0.0309564601629972457885742)
        .mul_add(t.0, 0.0577365085482597351074219)
        .mul_add(t.0, -0.0838950723409652709960938)
        .mul_add(t.0, 0.109463557600975036621094)
        .mul_add(t.0, -0.142626821994781494140625)
        .mul_add(t.0, 0.199983194470405578613281);

    t = t * (-0.333332866430282592773438).add_checked_as_doubled(u * t.0);
    t = s * (1.).add_checked(t);
    df(1.5707963705062866211, -4.3711388286737928865e-08) * (q as f32) + t
}

/// Arc tangent function of two variables
///
/// This function evaluates the arc tangent function of (***y*** / ***x***).
/// The quadrant of the result is determined according to the signs
/// of ***x*** and ***y***.
/// The error bound of the returned values is `max(1.0 ULP, f32::MIN)`.
pub fn atan2f(mut y: f32, mut x: f32) -> f32 {
    if fabsfk(x) < 2.9387372783541830947e-39 {
        y *= F1_24;
        x *= F1_24;
    } // nexttowardf((1. / FLT_MAX), 1)
    let d = atan2kf_u1(df(fabsfk(y), 0.), df(x, 0.));
    let mut r = d.0 + d.1;

    r = mulsignf(r, x);
    r = if y == 0. {
        (if signf(x) == -1. { PI } else { 0. })
    } else if y.is_infinite() {
        FRAC_PI_2
            - (if x.is_infinite() {
                signf(x) * FRAC_PI_4
            } else {
                0.
            })
    } else if x.is_infinite() || (x == 0.) {
        FRAC_PI_2
            - (if x.is_infinite() {
                signf(x) * FRAC_PI_2
            } else {
                0.
            })
    } else {
        r
    };

    if x.is_nan() || y.is_nan() {
        f32::NAN
    } else {
        mulsignf(r, y)
    }
}

/// Arc sine function
///
/// This function evaluates the arc sine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn asinf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let mut x = if o {
        df(fabsfk(d), 0.)
    } else {
        x2.sqrt_as_doubled()
    };
    x = if fabsfk(d) == 1. { df(0., 0.) } else { x };

    let u = 0.4197454825e-1_f32
        .mul_add(x2, 0.2424046025e-1)
        .mul_add(x2, 0.4547423869e-1)
        .mul_add(x2, 0.7495029271e-1)
        .mul_add(x2, 0.1666677296e+0)
        * x2
        * x.0;

    let y = (df(3.1415927410125732422 / 4., -8.7422776573475857731e-08 / 4.).sub_checked(x))
        .add_checked(-u);
    let r = if o { u + x.0 } else { (y.0 + y.1) * 2. };
    mulsignf(r, d)
}

/// Arc cosine function
///
/// This function evaluates the arc cosine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn acosf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let mut x = if o {
        df(fabsfk(d), 0.)
    } else {
        x2.sqrt_as_doubled()
    };
    x = if fabsfk(d) == 1. { df(0., 0.) } else { x };

    let u = 0.4197454825e-1_f32
        .mul_add(x2, 0.2424046025e-1)
        .mul_add(x2, 0.4547423869e-1)
        .mul_add(x2, 0.7495029271e-1)
        .mul_add(x2, 0.1666677296e+0)
        * x.0
        * x2;

    let mut y = df(3.1415927410125732422 / 2., -8.7422776573475857731e-08 / 2.)
        .sub_checked(mulsignf(x.0, d).add_checked_as_doubled(mulsignf(u, d)));
    x.add_checked_assign(u);
    y = if o { y } else { x.scale(2.) };
    if !o && (d < 0.) {
        y = df(3.1415927410125732422, -8.7422776573475857731e-08).sub_checked(y);
    }

    y.0 + y.1
}

/// Arc tangent function
///
/// This function evaluates the arc tangent function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn atanf(d: f32) -> f32 {
    let d2 = atan2kf_u1(df(fabsfk(d), 0.), df(1., 0.));
    let mut r = d2.0 + d2.1;
    if d.is_infinite() {
        r = 1.570796326794896557998982;
    }
    mulsignf(r, d)
}

/// Natural logarithmic function
///
/// This function returns the natural logarithm of ***a***.
/// The error bound of the returned value is 1.0 ULP.
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

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let t = 0.3027294874e+0_f32
        .mul_add(x2, 0.3996108174e+0)
        .mul_add(x2, 0.6666694880e+0);

    let s = (df(0.69314718246459960938, -1.904654323148236017e-09) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x.0 * t);

    if d == 0. {
        f32::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f32::NAN
    } else if d.is_infinite() {
        f32::INFINITY
    } else {
        s.0 + s.1
    }
}

/// Cube root function
///
/// This function returns the real cube root of ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn cbrtf(mut d: f32) -> f32 {
    let e = ilogbkf(fabsfk(d)) + 1;
    d = ldexp2kf(d, -e);
    let r = (e + 6144) % 3;
    let mut q2 = if r == 1 {
        df(1.2599210739135742188, -2.4018701694217270415e-08)
    } else {
        df(1., 0.)
    };
    q2 = if r == 2 {
        df(1.5874010324478149414, 1.9520385308169352356e-08)
    } else {
        q2
    };

    q2.0 = mulsignf(q2.0, d);
    q2.1 = mulsignf(q2.1, d);
    d = fabsfk(d);

    let mut x = (-0.601564466953277587890625_f32)
        .mul_add(d, 2.8208892345428466796875)
        .mul_add(d, -5.532182216644287109375)
        .mul_add(d, 5.898262500762939453125)
        .mul_add(d, -3.8095417022705078125)
        .mul_add(d, 2.2241256237030029296875);

    let mut y = x * x;
    y = y * y;
    x -= (d * y - x) * (1. / 3.);

    let z = x;

    let mut u = x.mul_as_doubled(x);
    u = u * u * d + (-x);
    y = u.0 + u.1;

    y = -2. / 3. * y * z;
    let v = (z.mul_as_doubled(z) + y) * d * q2;

    if d == 0. {
        mulsignf(0., q2.0)
    } else if d.is_infinite() {
        mulsignf(f32::INFINITY, q2.0)
    } else {
        ldexp2kf(v.0 + v.1, (e + 6144) / 3 - 2048)
    }
}

/// Gamma function
///
/// The error bound of the returned value is 1.0 ULP.
pub fn tgammaf(a: f32) -> f32 {
    let (da, db) = gammafk(a);
    let y = expk2f(da) * db;
    let mut r = y.0 + y.1;
    r = if ((a == f32::NEG_INFINITY) || ((a < 0.) && xisintf(a)))
        || (a.is_finite() && (a < 0.) && r.is_nan())
    {
        f32::NAN
    } else {
        r
    };
    if ((a == f32::INFINITY) || a.is_finite())
        && (a >= -f32::MIN)
        && ((a == 0.) || (a > 36.) || r.is_nan())
    {
        mulsignf(f32::INFINITY, a)
    } else {
        r
    }
}

/// Log gamma function
///
/// The error bound of the returned value is 1.0 ULP if the argument is positive.
/// If the argument is larger than 4e+36, it may return infinity instead of the correct value.
/// The error bound is `max(1 ULP and 1e-8)`, if the argument is negative.
pub fn lgammaf(a: f32) -> f32 {
    let (da, db) = gammafk(a);
    let y = da + logk2f(db.abs());
    let r = y.0 + y.1;
    if a.is_infinite() || (a <= 0. && xisintf(a)) || (a.is_finite() && r.is_nan()) {
        f32::INFINITY
    } else {
        r
    }
}

/// Error function
///
/// The error bound of the returned value is 1.0 ULP.
pub fn erff(mut a: f32) -> f32 {
    let s = a;

    a = fabsfk(a);
    let o0 = a < 1.1;
    let o1 = a < 2.4;
    let o2 = a < 4.0;
    let mut u = if o0 { a * a } else { a };

    let t = if o0 {
        0.7089292194e-4_f32
    } else if o1 {
        -0.1792667899e-4
    } else {
        -0.9495757695e-5
    }
    .mul_add(
        u,
        if o0 {
            -0.7768311189e-3
        } else if o1 {
            0.3937633010e-3
        } else {
            0.2481465926e-3
        },
    )
    .mul_add(
        u,
        if o0 {
            0.5159463733e-2
        } else if o1 {
            -0.3949181177e-2
        } else {
            -0.2918176819e-2
        },
    )
    .mul_add(
        u,
        if o0 {
            -0.2683781274e-1
        } else if o1 {
            0.2445474640e-1
        } else {
            0.2059706673e-1
        },
    )
    .mul_add(
        u,
        if o0 {
            0.1128318012e+0
        } else if o1 {
            -0.1070996150e+0
        } else {
            -0.9901899844e-1
        },
    );
    let mut d = t.mul_as_doubled(u);
    d += if o0 {
        df(-0.376125876000657465175213237214e+0, 0.)
    } else if o1 {
        df(-0.634588905908410389971210809210e+0, 0.)
    } else {
        df(-0.643598050547891613081201721633e+0, 0.)
    };
    d *= u;
    d += if o0 {
        df(0.112837916021059138255978217023e+1, 0.)
    } else if o1 {
        df(-0.112879855826694507209862753992e+1, 0.)
    } else {
        df(-0.112461487742845562801052956293e+1, 0.)
    };
    d *= a;
    d = if o0 { d } else { (1.).add_checked(-expk2f(d)) };
    u = mulsignf(if o2 { d.0 + d.1 } else { 1. }, s);
    if a.is_nan() {
        f32::NAN
    } else {
        u
    }
}

/// Base-*e* exponential function
///
/// This function returns the value of *e* raised to ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn expf(d: f32) -> f32 {
    let qf = rintfk(d * R_LN2_F);
    let q = qf as i32;
    let s = qf.mul_add(-L2U_F, d);
    let s = qf.mul_add(-L2L_F, s);

    let mut u = 0.000198527617612853646278381_f32
        .mul_add(s, 0.00139304355252534151077271)
        .mul_add(s, 0.00833336077630519866943359)
        .mul_add(s, 0.0416664853692054748535156)
        .mul_add(s, 0.166666671633720397949219)
        .mul_add(s, 0.5);

    u = s * s * u + s + 1.;

    if d < -104. {
        0.
    } else if d > 104. {
        f32::INFINITY
    } else {
        ldexp2kf(u, q)
    }
}

/// Power function
///
/// This function returns the value of ***x*** raised to the power of ***y***.
/// The error bound of the returned value is 1.0 ULP.
pub fn powf(x: f32, y: f32) -> f32 {
    let yisint = (y == (y as i32 as f32)) || (fabsfk(y) >= F1_24);
    let yisodd = ((1 & (y as i32)) != 0) && yisint && (fabsfk(y) < F1_24);

    let mut result = expkf(logkf(fabsfk(x)) * y);

    result = if result.is_nan() {
        f32::INFINITY
    } else {
        result
    };
    result *= if x >= 0. {
        1.
    } else if !yisint {
        f32::NAN
    } else if yisodd {
        -1.
    } else {
        1.
    };

    let efx = mulsignf(fabsfk(x) - 1., y);
    if (y == 0.) || (x == 1.) {
        1.
    } else if x.is_nan() || y.is_nan() {
        f32::NAN
    } else if x.is_infinite() || (x == 0.) {
        (if yisodd { signf(x) } else { 1. })
            * (if (if x == 0. { -y } else { y }) < 0. {
                0.
            } else {
                f32::INFINITY
            })
    } else if y.is_infinite() {
        if efx < 0. {
            0.
        } else if efx == 0. {
            1.
        } else {
            f32::INFINITY
        }
    } else {
        result
    }
}

/// Hyperbolic sine function
///
/// This function evaluates the hyperbolic sine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP if ***a*** is in
/// `[-709, 709]` for the double-precision function or `[-88.5, 88.5]`
/// for the single-precision function.
/// If ***a*** is a finite value out of this range, infinity with a correct
/// sign or a correct value with 1.0 ULP error bound is returned.
pub fn sinhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    d = d.sub_checked(d.recpre());
    y = (d.0 + d.1) * 0.5;

    y = if fabsfk(x) > 89. { f32::INFINITY } else { y };
    y = if y.is_nan() { f32::INFINITY } else { y };
    y = mulsignf(y, x);
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

/// Hyperbolic cosine function
///
/// This function evaluates the hyperbolic cosine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP if ***a** is in
/// `[-709, 709]` for the double-precision function or `[-88.5, 88.5]`
/// for the single-precision function.
/// If a is a finite value out of this range, infinity with a correct
/// sign or a correct value with 1.0 ULP error bound is returned.
pub fn coshf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    d = d.add_checked(d.recpre());
    y = (d.0 + d.1) * 0.5;

    y = if fabsfk(x) > 89. { f32::INFINITY } else { y };
    y = if y.is_nan() { f32::INFINITY } else { y };
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

/// Hyperbolic tangent function
///
/// This function evaluates the hyperbolic tangent function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP for the double-precision
/// function or 1.0001 ULP for the single-precision function.
pub fn tanhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    let e = d.recpre();
    d = d.sub_checked(e) / d.add_checked(e);
    y = d.0 + d.1;

    y = if fabsfk(x) > 18.714973875 { 1. } else { y };
    y = if y.is_nan() { 1. } else { y };
    y = mulsignf(y, x);
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

/// Inverse hyperbolic sine function
///
/// This function evaluates the inverse hyperbolic sine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP if a is in
/// `[-1.34e+154, 1.34e+154]` for the double-precision function or 1.001 ULP
/// if ***a*** is in `[-1.84e+19, 1.84e+19]` for the single-precision function.
/// If ***a*** is a finite value out of this range, infinity with a correct
/// sign or a correct value with 1.0 ULP error bound is returned.
pub fn asinhf(x: f32) -> f32 {
    let mut y = fabsfk(x);

    let mut d = if y > 1. { x.recpre() } else { df(y, 0.) };
    d = (d.square() + 1.).sqrt();
    d = if y > 1. { d * y } else { d };

    d = logk2f(d.add_checked(x).normalize());
    y = d.0 + d.1;

    y = if fabsfk(x) > SQRT_FLT_MAX || y.is_nan() {
        mulsignf(f32::INFINITY, x)
    } else {
        y
    };
    y = if x.is_nan() { f32::NAN } else { y };
    if xisnegzerof(x) {
        -0.
    } else {
        y
    }
}

/// Inverse hyperbolic cosine function
///
/// This function evaluates the inverse hyperbolic cosine function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP if a is in
/// `[-1.34e+154, 1.34e+154]` for the double-precision function or 1.001 ULP
/// if ***a*** is in `[-1.84e+19, 1.84e+19]` for the single-precision function.
/// If ***a*** is a finite value out of this range, infinity with a correct
/// sign or a correct value with 1.0 ULP error bound is returned.
pub fn acoshf(x: f32) -> f32 {
    let d = logk2f((x.add_as_doubled(1.)).sqrt() * (x.add_as_doubled(-1.)).sqrt() + x);
    let mut y = d.0 + d.1;

    y = if (x > SQRT_FLT_MAX) || y.is_nan() {
        f32::INFINITY
    } else {
        y
    };
    y = if x == 1. { 0. } else { y };
    y = if x < 1. { f32::NAN } else { y };
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

/// Inverse hyperbolic tangent function
///
/// This function evaluates the inverse hyperbolic tangent function of a value in ***a***.
/// The error bound of the returned value is 1.0 ULP for the double-precision
/// function or 1.0001 ULP for the single-precision function.
pub fn atanhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let d = logk2f((1.).add_as_doubled(y) / (1.).add_as_doubled(-y));
    y = if y > 1. {
        f32::NAN
    } else if y == 1. {
        f32::INFINITY
    } else {
        (d.0 + d.1) * 0.5
    };

    y = if x.is_infinite() || y.is_nan() {
        f32::NAN
    } else {
        y
    };
    y = mulsignf(y, x);
    if x.is_nan() {
        f32::NAN
    } else {
        y
    }
}

/// Base-10 exponential function
///
/// This function returns 10 raised to ***a***.
/// The error bound of the returned value is 1.09 ULP.
pub fn exp10f(d: f32) -> f32 {
    let qf = rintfk(d * (LOG10_2 as f32));

    let q = qf as i32;
    let s = qf.mul_add(-L10U_F, d);
    let s = qf.mul_add(-L10L_F, s);

    let mut u = 0.2064004987e+0_f32
        .mul_add(s, 0.5417877436e+0)
        .mul_add(s, 0.1171286821e+1)
        .mul_add(s, 0.2034656048e+1)
        .mul_add(s, 0.2650948763e+1)
        .mul_add(s, 0.2302585125e+1);
    u = (1.).add_checked(u.mul_as_doubled(s)).normalize().0;

    if d > 38.5318394191036238941387 {
        f32::INFINITY // log10(FLT_MAX)
    } else if d < -50. {
        0.
    } else {
        ldexp2kf(u, q)
    }
}

/// Base-*e* exponential function minus 1
///
/// This function returns the value one less than *e* raised to ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn expm1f(a: f32) -> f32 {
    let d = expk2f(df(a, 0.)) + (-1.);
    if xisnegzerof(a) {
        -0.
    } else if a < -16.635532333438687426013570 {
        -1.
    } else if a > 88.72283172607421875 {
        f32::INFINITY
    } else {
        d.0 + d.1
    }
}

/// Base-10 logarithmic function
///
/// This function returns the base-10 logarithm of ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn log10f(mut d: f32) -> f32 {
    let o = d < f32::MIN;
    if o {
        d *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(d * (1. / 0.75));
    let m = ldexp3kf(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let t = 0.1314289868e+0_f32
        .mul_add(x2, 0.1735493541e+0)
        .mul_add(x2, 0.2895309627e+0);

    let s = df(0.30103001, -1.432098889e-08)
        * (e as f32)
            .add_checked(x * df(0.868588984, -2.170757285e-08))
            .add_checked(x2 * x.0 * t);

    if d == 0. {
        f32::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f32::NAN
    } else if d.is_infinite() {
        f32::INFINITY
    } else {
        s.0 + s.1
    }
}

/// Base-2 logarithmic function
///
/// This function returns the base-2 logarithm of ***a***.
/// The error bound of the returned value is 1.0 ULP.
pub fn log2f(mut d: f32) -> f32 {
    let o = d < f32::MIN;
    if o {
        d *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(d * (1. / 0.75));
    let m = ldexp3kf(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let t = 0.4374550283e+0_f32
        .mul_add(x2, 0.5764790177e+0)
        .mul_add(x2, 0.9618012905120);

    let mut s = (e as f32) + x * df(2.8853900432586669922, 3.2734474483568488616e-08);
    s += x2 * x.0 * t;

    if d == 0. {
        f32::NEG_INFINITY
    } else if (d < 0.) || d.is_nan() {
        f32::NAN
    } else if d.is_infinite() {
        f32::INFINITY
    } else {
        s.0 + s.1
    }
}

/// Logarithm of one plus argument
///
/// This function returns the natural logarithm of (1+***a***).
/// The error bound of the returned value is 1.0 ULP.
pub fn log1pf(d: f32) -> f32 {
    let mut dp1 = d + 1.;

    let o = dp1 < f32::MIN;
    if o {
        dp1 *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(dp1 * (1. / 0.75));

    let t = ldexp3kf(1., -e);
    let m = d.mul_add(t, t - 1.);

    if o {
        e -= 64;
    }

    let x = df(m, 0.) / (2.).add_checked_as_doubled(m);
    let x2 = x.0 * x.0;

    let t = 0.3027294874e+0_f32
        .mul_add(x2, 0.3996108174e+0)
        .mul_add(x2, 0.6666694880e+0);

    let s = (df(0.69314718246459960938, -1.904654323148236017e-09) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x.0 * t);

    if xisnegzerof(d) {
        -0.
    } else if d == -1. {
        f32::NEG_INFINITY
    } else if d < -1. {
        f32::NAN
    } else if d > 1e+38 {
        f32::INFINITY
    } else {
        s.0 + s.1
    }
}
