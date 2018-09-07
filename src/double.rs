#![allow(dead_code)]
use std::f64;
use std::isize;

use common::*;
use f2::*;

// ------- Delete !!! Replace with core::simd::f32n2 -------------------

#[inline]
fn rintk(x: f64) -> f64 {
    (if x < 0. { (x - 0.5) } else { (x + 0.5) }) as isize as f64
}
#[inline]
fn ceilk(x: f64) -> isize {
    (x as isize) + (if x < 0. { 0 } else { 1 })
}
#[inline]
fn trunck(x: f64) -> f64 {
    x as isize as f64
}
#[inline]
fn fmink(x: f64, y: f64) -> f64 {
    if x < y {
        x
    } else {
        y
    }
}
#[inline]
fn fmaxk(x: f64, y: f64) -> f64 {
    if x > y {
        x
    } else {
        y
    }
}

#[inline]
fn pow2i(q: isize) -> f64 {
    long_bits_to_double(((q + 0x3ff) as i64) << 52)
}

#[inline]
fn ldexpk(x: f64, mut q: isize) -> f64 {
    let mut m = q >> 31;
    m = (((m + q) >> 9) - m) << 7;
    q = q - (m << 2);
    m += 0x3ff;
    m = if m < 0 { 0 } else { m };
    m = if m > 0x7ff { 0x7ff } else { m };
    let u = long_bits_to_double((m as i64) << 52);
    let x = x * u * u * u * u;
    let u = long_bits_to_double(((q + 0x3ff) as i64) << 52);
    x * u
}

#[inline]
fn ldexp2k(d: f64, e: isize) -> f64 {
    // faster than ldexpk, short reach
    d * pow2i(e >> 1) * pow2i(e - (e >> 1))
}

#[inline]
fn ldexp3k(d: f64, e: isize) -> f64 {
    // very fast, no denormal
    long_bits_to_double(double_to_raw_long_bits(d) + ((e as i64) << 52))
}

pub fn xldexp(x: f64, mut exp: isize) -> f64 {
    if exp > 2100 {
        exp = 2100;
    }
    if exp < -2100 {
        exp = -2100;
    }

    let mut e0 = exp >> 2;
    if exp < 0 {
        e0 += 1;
    }
    if (-100 < exp) && (exp < 100) {
        e0 = 0;
    }
    let e1 = exp - (e0 << 2);

    let p = pow2i(e0);
    x * pow2i(e1) * p * p * p * p
}

#[inline]
fn ilogbk(mut d: f64) -> isize {
    let m = d < 4.9090934652977266E-91;
    d = if m { 2.037035976334486E90 * d } else { d };
    let q = (double_to_raw_long_bits(d) >> 52) & 0x7ff;
    (if m { q - (300 + 0x03ff) } else { q - 0x03ff }) as isize
}

// ilogb2k is similar to ilogbk, but the argument has to be a
// normalized FP value.
#[inline]
fn ilogb2k(d: f64) -> isize {
    (((double_to_raw_long_bits(d) >> 52) & 0x7ff) - 0x3ff) as isize
}

pub fn xilogb(d: f64) -> isize {
    let mut e = ilogbk(fabsk(d));
    e = if d == 0. { SLEEF_FP_ILOGB0 as isize } else { e };
    e = if d.isnan() {
        SLEEF_FP_ILOGBNAN as isize
    } else {
        e
    };
    if d.isinf() {
        isize::MAX
    } else {
        e
    }
}

#[inline]
fn atan2k(mut y: f64, mut x: f64) -> f64 {
    let mut q = 0;

    if x < 0. {
        x = -x;
        q = -2;
    }
    if y > x {
        let t = x;
        x = y;
        y = -t;
        q += 1;
    }

    let s = y / x;
    let t = s * s;

    let u = (-1.88796008463073496563746e-05)
        .mul_add(t, 0.000209850076645816976906797)
        .mul_add(t, -0.00110611831486672482563471)
        .mul_add(t, 0.00370026744188713119232403)
        .mul_add(t, -0.00889896195887655491740809)
        .mul_add(t, 0.016599329773529201970117)
        .mul_add(t, -0.0254517624932312641616861)
        .mul_add(t, 0.0337852580001353069993897)
        .mul_add(t, -0.0407629191276836500001934)
        .mul_add(t, 0.0466667150077840625632675)
        .mul_add(t, -0.0523674852303482457616113)
        .mul_add(t, 0.0587666392926673580854313)
        .mul_add(t, -0.0666573579361080525984562)
        .mul_add(t, 0.0769219538311769618355029)
        .mul_add(t, -0.090908995008245008229153)
        .mul_add(t, 0.111111105648261418443745)
        .mul_add(t, -0.14285714266771329383765)
        .mul_add(t, 0.199999999996591265594148)
        .mul_add(t, -0.333333333333311110369124);

    let t = u * t * s + s;
    (q as f64) * (M_PI / 2.) + t
}

pub fn xatan2(y: f64, x: f64) -> f64 {
    let mut r = atan2k(fabsk(y), x);

    r = if y == 0. {
        (if sign(x) == -1. { M_PI } else { 0. })
    } else if y.isinf() {
        M_PI / 2.
            - (if x.isinf() {
                (sign(x) * (M_PI * 1. / 4.))
            } else {
                0.
            })
    } else if x.isinf() || (x == 0.) {
        M_PI / 2.
            - (if x.isinf() {
                (sign(x) * (M_PI / 2.))
            } else {
                0.
            })
    } else {
        mulsign(r, x)
    };
    if x.isnan() || y.isnan() {
        SLEEF_NAN
    } else {
        mulsign(r, y)
    }
}

pub fn xasin(d: f64) -> f64 {
    let o = fabsk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
    let x = if o { fabsk(d) } else { SQRT(x2) };

    let u = 0.3161587650653934628e-1
        .mul_add(x2, -0.1581918243329996643e-1)
        .mul_add(x2, 0.1929045477267910674e-1)
        .mul_add(x2, 0.6606077476277170610e-2)
        .mul_add(x2, 0.1215360525577377331e-1)
        .mul_add(x2, 0.1388715184501609218e-1)
        .mul_add(x2, 0.1735956991223614604e-1)
        .mul_add(x2, 0.2237176181932048341e-1)
        .mul_add(x2, 0.3038195928038132237e-1)
        .mul_add(x2, 0.4464285681377102438e-1)
        .mul_add(x2, 0.7500000000378581611e-1)
        .mul_add(x2, 0.1666666666666497543e+0)
        .mul_add(x * x2, x);

    let r = if o { u } else { M_PI / 2. - 2. * u };
    mulsign(r, d)
}

pub fn xacos(d: f64) -> f64 {
    let o = fabsk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
    let mut x = if o { fabsk(d) } else { SQRT(x2) };
    x = if fabsk(d) == 1. { 0. } else { x };

    let u = 0.3161587650653934628e-1
        .mul_add(x2, -0.1581918243329996643e-1)
        .mul_add(x2, 0.1929045477267910674e-1)
        .mul_add(x2, 0.6606077476277170610e-2)
        .mul_add(x2, 0.1215360525577377331e-1)
        .mul_add(x2, 0.1388715184501609218e-1)
        .mul_add(x2, 0.1735956991223614604e-1)
        .mul_add(x2, 0.2237176181932048341e-1)
        .mul_add(x2, 0.3038195928038132237e-1)
        .mul_add(x2, 0.4464285681377102438e-1)
        .mul_add(x2, 0.7500000000378581611e-1)
        .mul_add(x2, 0.1666666666666497543e+0)
        * x
        * x2;

    let y = 3.1415926535897932 / 2. - (mulsign(x, d) + mulsign(u, d));
    x += u;
    let r = if o { y } else { x * 2. };
    if !o && (d < 0.) {
        dd(3.141592653589793116, 1.2246467991473532072e-16)
            .add_checked(-r)
            .0
    } else {
        r
    }
}

pub fn xatan(mut s: f64) -> f64 {
    let mut q = 0;

    if sign(s) == -1. {
        s = -s;
        q = 2;
    }
    if s > 1. {
        s = 1. / s;
        q |= 1;
    }

    let t = s * s;

    let u = (-1.88796008463073496563746e-05)
        .mul_add(t, 0.000209850076645816976906797)
        .mul_add(t, -0.00110611831486672482563471)
        .mul_add(t, 0.00370026744188713119232403)
        .mul_add(t, -0.00889896195887655491740809)
        .mul_add(t, 0.016599329773529201970117)
        .mul_add(t, -0.0254517624932312641616861)
        .mul_add(t, 0.0337852580001353069993897)
        .mul_add(t, -0.0407629191276836500001934)
        .mul_add(t, 0.0466667150077840625632675)
        .mul_add(t, -0.0523674852303482457616113)
        .mul_add(t, 0.0587666392926673580854313)
        .mul_add(t, -0.0666573579361080525984562)
        .mul_add(t, 0.0769219538311769618355029)
        .mul_add(t, -0.090908995008245008229153)
        .mul_add(t, 0.111111105648261418443745)
        .mul_add(t, -0.14285714266771329383765)
        .mul_add(t, 0.199999999996591265594148)
        .mul_add(t, -0.333333333333311110369124);

    let t = s + s * (t * u);

    if (q & 2) != 0 {
        -t
    } else if (q & 1) != 0 {
        1.570796326794896557998982 - t
    } else {
        t
    }
}

fn rempisub(x: f64) -> (f64, i32) {
    // This function is equivalent to :
    // ( x - round(4 * x) * 0.25, (round(4 * x) - round(x) * 4) as i32 );
    let mut fr = x - D1_28 * ((x * (1.0 / D1_28)) as i32 as f64);
    let mut reti = ((7 & ((if x > 0. { 4 } else { 3 }) + ((fr * 8.) as i32))) - 3) >> 1;
    fr = fr - 0.25 * ((fr * 4. + mulsign(0.5, x)) as i32 as f64);
    fr = if fabsk(fr) > 0.25 {
        (fr - mulsign(0.5, x))
    } else {
        fr
    };
    fr = if fabsk(fr) > 1e+10 { 0. } else { fr };
    if fabsk(x) == 0.12499999999999998612 {
        fr = x;
        reti = 0;
    }
    (fr, reti)
}

// Payne-Hanek like argument reduction
fn rempi(a: f64) -> (F2<f64>, i32) {
    let mut ex = ilogb2k(a) - 55;
    let q = if ex > (700 - 55) { -64 } else { 0 };
    let a = ldexp3k(a, q);
    if ex < 0 {
        ex = 0;
    }
    let ex = (ex * 4) as usize;
    let mut x = a.mul_as_f2(REMPITABDP[ex]);
    let (did, dii) = rempisub(x.0);
    let mut q = dii;
    x.0 = did;
    x = x.normalize();
    let mut y = a.mul_as_f2(REMPITABDP[ex + 1]);
    x += y;
    let (did, dii) = rempisub(x.0);
    q += dii;
    x.0 = did;
    x = x.normalize();
    y = dd(REMPITABDP[ex + 2], REMPITABDP[ex + 3]) * a;
    x += y;
    x = x.normalize() * dd(3.141592653589793116 * 2., 1.2246467991473532072e-16 * 2.);
    (if fabsk(a) < 0.7 { dd(a, 0.) } else { x }, q)
}

pub fn xsin(mut d: f64) -> f64 {
    let t = d;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(d * M_1_PI);
        ql = qlf as isize;
        d = qlf.mul_add(-PI_A2, d);
        d = qlf.mul_add(-PI_B2, d);
    } else if fabsk(d) < TRIGRANGEMAX {
        let dqh = trunck(d * (M_1_PI / D1_24)) * D1_24;
        let qlf = rintk(d.mul_add(M_1_PI, -dqh));
        ql = qlf as isize;

        d = dqh.mul_add(-PI_A, d);
        d = qlf.mul_add(-PI_A, d);
        d = dqh.mul_add(-PI_B, d);
        d = qlf.mul_add(-PI_B, d);
        d = dqh.mul_add(-PI_C, d);
        d = qlf.mul_add(-PI_C, d);
        d = (dqh + qlf).mul_add(-PI_D, d);
    } else {
        let (mut ddidd, ddii) = rempi(t);
        ql = (((ddii & 3) * 2 + ((ddidd.0 > 0.) as i32) + 1) >> 2) as isize;
        if ddii & 1 != 0 {
            ddidd = ddidd + dd(
                mulsign(3.141592653589793116 * -0.5, ddidd.0),
                mulsign(1.2246467991473532072e-16 * -0.5, ddidd.0),
            );
        }
        d = ddidd.0 + ddidd.1;
        if t.isinf() || t.isnan() {
            d = SLEEF_NAN;
        }
    }

    let s = d * d;

    if (ql & 1) != 0 {
        d = -d;
    }

    let u = (-7.97255955009037868891952e-18)
        .mul_add(s, 2.81009972710863200091251e-15)
        .mul_add(s, -7.64712219118158833288484e-13)
        .mul_add(s, 1.60590430605664501629054e-10)
        .mul_add(s, -2.50521083763502045810755e-08)
        .mul_add(s, 2.75573192239198747630416e-06)
        .mul_add(s, -0.000198412698412696162806809)
        .mul_add(s, 0.00833333333333332974823815)
        .mul_add(s, -0.166666666666666657414808);

    if xisnegzero(t) {
        t
    } else {
        s.mul_add(u * d, d)
    }
}

pub fn xcos(mut d: f64) -> f64 {
    let t = d;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = (2.).mul_add(rintk(d * M_1_PI - 0.5), 1.);
        ql = qlf as isize;
        d = qlf.mul_add(-PI_A2 * 0.5, d);
        d = qlf.mul_add(-PI_B2 * 0.5, d);
    } else if fabsk(d) < TRIGRANGEMAX {
        let mut dqh = trunck(d * (M_1_PI / D1_23) - 0.5 * (M_1_PI / D1_23));
        let qlf = 2. * rintk(d * M_1_PI - 0.5 - dqh * D1_23) + 1.;
        ql = qlf as isize;
        dqh *= D1_24;

        d = dqh.mul_add(-PI_A * 0.5, d);
        d = qlf.mul_add(-PI_A * 0.5, d);
        d = dqh.mul_add(-PI_B * 0.5, d);
        d = qlf.mul_add(-PI_B * 0.5, d);
        d = dqh.mul_add(-PI_C * 0.5, d);
        d = qlf.mul_add(-PI_C * 0.5, d);
        d = (dqh + qlf).mul_add(-PI_D * 0.5, d);
    } else {
        let (mut ddidd, ddii) = rempi(t);
        ql = (((ddii & 3) * 2 + ((ddidd.0 > 0.) as i32) + 7) >> 1) as isize;
        if (ddii & 1) == 0 {
            ddidd = ddidd + dd(
                mulsign(
                    3.141592653589793116 * -0.5,
                    if ddidd.0 > 0. { 1. } else { -1. },
                ),
                mulsign(
                    1.2246467991473532072e-16 * -0.5,
                    if ddidd.0 > 0. { 1. } else { -1. },
                ),
            );
        }
        d = ddidd.0 + ddidd.1;
        if t.isinf() || t.isnan() {
            d = SLEEF_NAN;
        }
    }

    let s = d * d;

    if (ql & 2) == 0 {
        d = -d;
    }

    let u = (-7.97255955009037868891952e-18)
        .mul_add(s, 2.81009972710863200091251e-15)
        .mul_add(s, -7.64712219118158833288484e-13)
        .mul_add(s, 1.60590430605664501629054e-10)
        .mul_add(s, -2.50521083763502045810755e-08)
        .mul_add(s, 2.75573192239198747630416e-06)
        .mul_add(s, -0.000198412698412696162806809)
        .mul_add(s, 0.00833333333333332974823815)
        .mul_add(s, -0.166666666666666657414808);

    s.mul_add(u * d, d)
}

pub fn xsincos(d: f64) -> (f64, f64) {
    let ql: isize;

    let mut s = d;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(s * (2. * M_1_PI));
        ql = qlf as isize;
        s = qlf.mul_add(-PI_A2 * 0.5, s);
        s = qlf.mul_add(-PI_B2 * 0.5, s);
    } else if fabsk(d) < TRIGRANGEMAX {
        let dqh = trunck(d * ((2. * M_1_PI) / D1_24)) * D1_24;
        let qlf = rintk(d * (2. * M_1_PI) - dqh);
        ql = qlf as isize;

        s = dqh.mul_add(-PI_A * 0.5, s);
        s = qlf.mul_add(-PI_A * 0.5, s);
        s = dqh.mul_add(-PI_B * 0.5, s);
        s = qlf.mul_add(-PI_B * 0.5, s);
        s = dqh.mul_add(-PI_C * 0.5, s);
        s = qlf.mul_add(-PI_C * 0.5, s);
        s = (dqh + qlf).mul_add(-PI_D * 0.5, s);
    } else {
        let (ddidd, ddii) = rempi(d);
        ql = ddii as isize;
        s = ddidd.0 + ddidd.1;
        if d.isinf() || d.isnan() {
            s = SLEEF_NAN;
        }
    }

    let t = s;

    s = s * s;

    let u = 1.58938307283228937328511e-10
        .mul_add(s, -2.50506943502539773349318e-08)
        .mul_add(s, 2.75573131776846360512547e-06)
        .mul_add(s, -0.000198412698278911770864914)
        .mul_add(s, 0.0083333333333191845961746)
        .mul_add(s, -0.166666666666666130709393)
        * s
        * t;

    let mut rsin = t + u;

    if xisnegzero(d) {
        rsin = -0.0;
    }

    let u = (-1.13615350239097429531523e-11)
        .mul_add(s, 2.08757471207040055479366e-09)
        .mul_add(s, -2.75573144028847567498567e-07)
        .mul_add(s, 2.48015872890001867311915e-05)
        .mul_add(s, -0.00138888888888714019282329)
        .mul_add(s, 0.0416666666666665519592062)
        .mul_add(s, -0.5);

    let mut rcos = u * s + 1.;

    if (ql & 1) != 0 {
        s = rcos;
        rcos = rsin;
        rsin = s;
    }
    if (ql & 2) != 0 {
        rsin = -rsin;
    }
    if ((ql + 1) & 2) != 0 {
        rcos = -rcos;
    }
    (rsin, rcos)
}

#[inline]
fn sinpik(d: f64) -> F2<f64> {
    let u = d * 4.;
    let q = ceilk(u) & !1;
    let o = (q & 2) != 0;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_f2(t);

    //

    let u = (if o {
        9.94480387626843774090208e-16
    } else {
        -2.02461120785182399295868e-14
    }).mul_add(
        s,
        if o {
            -3.89796226062932799164047e-13
        } else {
            6.94821830580179461327784e-12
        },
    ).mul_add(
        s,
        if o {
            1.15011582539996035266901e-10
        } else {
            -1.75724749952853179952664e-09
        },
    ).mul_add(
        s,
        if o {
            -2.4611369501044697495359e-08
        } else {
            3.13361688966868392878422e-07
        },
    ).mul_add(
        s,
        if o {
            3.59086044859052754005062e-06
        } else {
            -3.6576204182161551920361e-05
        },
    ).mul_add(
        s,
        if o {
            -0.000325991886927389905997954
        } else {
            0.00249039457019271850274356
        },
    );
    let mut x = u * s
        + (if o {
            dd(0.0158543442438155018914259, -1.04693272280631521908845e-18)
        } else {
            dd(-0.0807455121882807852484731, 3.61852475067037104849987e-18)
        });
    x = s2 * x
        + (if o {
            dd(-0.308425137534042437259529, -1.95698492133633550338345e-17)
        } else {
            dd(0.785398163397448278999491, 3.06287113727155002607105e-17)
        });

    x *= if o { s2 } else { dd(t, 0.) };
    x = if o { x + 1. } else { x };

    //

    if (q & 4) != 0 {
        x.0 = -x.0;
        x.1 = -x.1;
    }

    x
}

#[inline]
fn cospik(d: f64) -> F2<f64> {
    let u = d * 4.;
    let q = ceilk(u) & !1;
    let o = (q & 2) == 0;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_f2(t);

    //

    let u = (if o {
        9.94480387626843774090208e-16
    } else {
        -2.02461120785182399295868e-14
    }).mul_add(
        s,
        if o {
            -3.89796226062932799164047e-13
        } else {
            6.94821830580179461327784e-12
        },
    ).mul_add(
        s,
        if o {
            1.15011582539996035266901e-10
        } else {
            -1.75724749952853179952664e-09
        },
    ).mul_add(
        s,
        if o {
            -2.4611369501044697495359e-08
        } else {
            3.13361688966868392878422e-07
        },
    ).mul_add(
        s,
        if o {
            3.59086044859052754005062e-06
        } else {
            -3.6576204182161551920361e-05
        },
    ).mul_add(
        s,
        if o {
            -0.000325991886927389905997954
        } else {
            0.00249039457019271850274356
        },
    );
    let mut x = u * s
        + (if o {
            dd(0.0158543442438155018914259, -1.04693272280631521908845e-18)
        } else {
            dd(-0.0807455121882807852484731, 3.61852475067037104849987e-18)
        });
    x = s2 * x
        + (if o {
            dd(-0.308425137534042437259529, -1.95698492133633550338345e-17)
        } else {
            dd(0.785398163397448278999491, 3.06287113727155002607105e-17)
        });

    x *= if o { s2 } else { dd(t, 0.) };
    x = if o { x + 1. } else { x };

    //

    if ((q + 2) & 4) != 0 {
        x.0 = -x.0;
        x.1 = -x.1;
    }

    x
}

pub fn xtan(d: f64) -> f64 {
    let mut x: f64;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = rintk(d * (2. * M_1_PI));
        ql = qlf as isize;
        x = qlf.mul_add(-PI_A2 * 0.5, d);
        x = qlf.mul_add(-PI_B2 * 0.5, x);
    } else if fabsk(d) < 1e+7 {
        let dqh = trunck(d * ((2. * M_1_PI) / D1_24)) * D1_24;
        let qlf = rintk(d * (2. * M_1_PI) - dqh);
        ql = qlf as isize;

        x = dqh.mul_add(-PI_A * 0.5, d);
        x = qlf.mul_add(-PI_A * 0.5, x);
        x = dqh.mul_add(-PI_B * 0.5, x);
        x = qlf.mul_add(-PI_B * 0.5, x);
        x = dqh.mul_add(-PI_C * 0.5, x);
        x = qlf.mul_add(-PI_C * 0.5, x);
        x = (dqh + qlf).mul_add(-PI_D * 0.5, x);
    } else {
        let (ddidd, ddii) = rempi(d);
        ql = ddii as isize;
        x = ddidd.0 + ddidd.1;
        if d.isinf() || d.isnan() {
            x = SLEEF_NAN;
        }
    }

    let s = x * x;

    if (ql & 1) != 0 {
        x = -x;
    }

    let mut u = 9.99583485362149960784268e-06
        .mul_add(s, -4.31184585467324750724175e-05)
        .mul_add(s, 0.000103573238391744000389851)
        .mul_add(s, -0.000137892809714281708733524)
        .mul_add(s, 0.000157624358465342784274554)
        .mul_add(s, -6.07500301486087879295969e-05)
        .mul_add(s, 0.000148898734751616411290179)
        .mul_add(s, 0.000219040550724571513561967)
        .mul_add(s, 0.000595799595197098359744547)
        .mul_add(s, 0.00145461240472358871965441)
        .mul_add(s, 0.0035923150771440177410343)
        .mul_add(s, 0.00886321546662684547901456)
        .mul_add(s, 0.0218694899718446938985394)
        .mul_add(s, 0.0539682539049961967903002)
        .mul_add(s, 0.133333333334818976423364)
        .mul_add(s, 0.333333333333320047664472);

    u = s.mul_add(u * x, x);

    if (ql & 1) != 0 {
        1. / u
    } else {
        u
    }
}

pub fn xlog(mut d: f64) -> f64 {
    let o = d < f64::MIN;
    if o {
        d *= D1_32 * D1_32;
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (m - 1.) / (m + 1.);
    let x2 = x * x;

    let t = 0.153487338491425068243146
        .mul_add(x2, 0.152519917006351951593857)
        .mul_add(x2, 0.181863266251982985677316)
        .mul_add(x2, 0.222221366518767365905163)
        .mul_add(x2, 0.285714294746548025383248)
        .mul_add(x2, 0.399999999950799600689777)
        .mul_add(x2, 0.6666666666667778740063)
        .mul_add(x2, 2.);

    let x = x * t + 0.693147180559945286226764 * (e as f64);

    if d == 0. {
        -SLEEF_INFINITY
    } else if (d < 0.) || d.isnan() {
        SLEEF_NAN
    } else if d.isinf() {
        SLEEF_INFINITY
    } else {
        x * t + 0.693147180559945286226764 * (e as f64)
    }
}

pub fn xexp(d: f64) -> f64 {
    let qf = rintk(d * R_LN2);
    let q = qf as isize;

    let s = qf.mul_add(-L2U, d);
    let s = qf.mul_add(-L2L, s);

    let mut u = 2.08860621107283687536341e-09
        .mul_add(s, 2.51112930892876518610661e-08)
        .mul_add(s, 2.75573911234900471893338e-07)
        .mul_add(s, 2.75572362911928827629423e-06)
        .mul_add(s, 2.4801587159235472998791e-05)
        .mul_add(s, 0.000198412698960509205564975)
        .mul_add(s, 0.00138888888889774492207962)
        .mul_add(s, 0.00833333333331652721664984)
        .mul_add(s, 0.0416666666666665047591422)
        .mul_add(s, 0.166666666666666851703837)
        .mul_add(s, 0.5);

    u = s * s * u + s + 1.;

    if d > 709.78271114955742909217217426 {
        SLEEF_INFINITY
    } else if d < -1000. {
        0.
    } else {
        ldexp2k(u, q)
    }
}

#[inline]
fn expm1k(d: f64) -> f64 {
    let q = rintk(d * R_LN2);

    let s = q.mul_add(-L2U, d);
    let s = q.mul_add(-L2L, s);

    let mut u = 2.08860621107283687536341e-09
        .mul_add(s, 2.51112930892876518610661e-08)
        .mul_add(s, 2.75573911234900471893338e-07)
        .mul_add(s, 2.75572362911928827629423e-06)
        .mul_add(s, 2.4801587159235472998791e-05)
        .mul_add(s, 0.000198412698960509205564975)
        .mul_add(s, 0.00138888888889774492207962)
        .mul_add(s, 0.00833333333331652721664984)
        .mul_add(s, 0.0416666666666665047591422)
        .mul_add(s, 0.166666666666666851703837)
        .mul_add(s, 0.5);
    u = s * s * u + s;

    let q = q as isize;
    if q != 0 {
        ldexp2k(u + 1., q) - 1.
    } else {
        u
    }
}

#[inline]
fn logk(mut d: f64) -> F2<f64> {
    let o = d < f64::MIN;
    if o {
        d *= D1_32 * D1_32
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_f2(m) / (1.).add_as_f2(m);
    let x2 = x.square();

    let t = 0.116255524079935043668677
        .mul_add(x2.0, 0.103239680901072952701192)
        .mul_add(x2.0, 0.117754809412463995466069)
        .mul_add(x2.0, 0.13332981086846273921509)
        .mul_add(x2.0, 0.153846227114512262845736)
        .mul_add(x2.0, 0.181818180850050775676507)
        .mul_add(x2.0, 0.222222222230083560345903)
        .mul_add(x2.0, 0.285714285714249172087875)
        .mul_add(x2.0, 0.400000000000000077715612);
    let c = dd(0.666666666666666629659233, 3.80554962542412056336616e-17);

    (dd(0.693147180559945286226764, 2.319046813846299558417771e-17) * (e as f64))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * (x2 * t + c))
}

#[inline]
fn expk(d: F2<f64>) -> f64 {
    let q = rintk((d.0 + d.1) * R_LN2);

    let s = d + q * (-L2U) + q * (-L2L);

    let s = s.normalize();

    let u = 2.51069683420950419527139e-08
        .mul_add(s.0, 2.76286166770270649116855e-07)
        .mul_add(s.0, 2.75572496725023574143864e-06)
        .mul_add(s.0, 2.48014973989819794114153e-05)
        .mul_add(s.0, 0.000198412698809069797676111)
        .mul_add(s.0, 0.0013888888939977128960529)
        .mul_add(s.0, 0.00833333333332371417601081)
        .mul_add(s.0, 0.0416666666665409524128449)
        .mul_add(s.0, 0.166666666666666740681535)
        .mul_add(s.0, 0.500000000000000999200722);

    let mut t = s.add_checked(s.square() * u);

    t = (1.).add_checked(t);

    if d.0 < -1000. {
        0.
    } else {
        ldexpk(t.0 + t.1, q as isize)
    }
}

pub fn xpow(x: f64, y: f64) -> f64 {
    let yisint = xisint(y);
    let yisodd = yisint && xisodd(y);

    let d = logk(fabsk(x)) * y;
    let mut result = expk(d);
    if d.0 > 709.78271114955742909217217426 {
        result = SLEEF_INFINITY;
    }

    result = if result.isnan() {
        SLEEF_INFINITY
    } else {
        result
    };
    result *= if x > 0. {
        1.
    } else if !yisint {
        SLEEF_NAN
    } else if yisodd {
        -1.
    } else {
        1.
    };

    let efx = mulsign(fabsk(x) - 1., y);
    if (y == 0.) || (x == 1.) {
        1.
    } else if y.isinf() {
        if efx < 0. {
            0.
        } else if efx == 0. {
            1.
        } else {
            SLEEF_INFINITY
        }
    } else if x.isinf() || (x == 0.) {
        (if yisodd { sign(x) } else { 1. })
            * (if (if x == 0. { -y } else { y }) < 0. {
                0.
            } else {
                SLEEF_INFINITY
            })
    } else if x.isnan() || y.isnan() {
        SLEEF_NAN
    } else {
        result
    }
}

#[inline]
fn expk2(d: F2<f64>) -> F2<f64> {
    let qf = rintk((d.0 + d.1) * R_LN2);
    let q = qf as isize;

    let s = d + qf * (-L2U) + qf * (-L2L);

    let u = 0.1602472219709932072e-9
        .mul_add(s.0, 0.2092255183563157007e-8)
        .mul_add(s.0, 0.2505230023782644465e-7)
        .mul_add(s.0, 0.2755724800902135303e-6)
        .mul_add(s.0, 0.2755731892386044373e-5)
        .mul_add(s.0, 0.2480158735605815065e-4)
        .mul_add(s.0, 0.1984126984148071858e-3)
        .mul_add(s.0, 0.1388888888886763255e-2)
        .mul_add(s.0, 0.8333333333333347095e-2)
        .mul_add(s.0, 0.4166666666666669905e-1);

    let mut t = s * u + 0.1666666666666666574e+0;
    t = s * t + 0.5;
    t = s + s.square() * t;

    t = 1. + t;

    t = F2::new(ldexp2k(t.0, q), ldexp2k(t.1, q));

    if d.0 < -1000. {
        dd(0., 0.)
    } else {
        t
    }
}

pub fn xsinh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let mut d = expk2(dd(y, 0.));
    d = d.sub_checked(d.recpre());
    y = (d.0 + d.1) * 0.5;

    y = if fabsk(x) > 710. { SLEEF_INFINITY } else { y };
    y = if y.isnan() { SLEEF_INFINITY } else { y };
    y = mulsign(y, x);
    if x.isnan() {
        SLEEF_NAN
    } else {
        y
    }
}

pub fn xcosh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let mut d = expk2(dd(y, 0.));
    d = d.add_checked(d.recpre());
    y = (d.0 + d.1) * 0.5;

    y = if fabsk(x) > 710. { SLEEF_INFINITY } else { y };
    y = if y.isnan() { SLEEF_INFINITY } else { y };
    if x.isnan() {
        SLEEF_NAN
    } else {
        y
    }
}

pub fn xtanh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let mut d = expk2(dd(y, 0.));
    let e = d.recpre();
    d = d.sub_checked(e) / d.add_checked(e);
    y = d.0 + d.1;

    y = if fabsk(x) > 18.714973875 { 1. } else { y };
    y = if y.isnan() { 1. } else { y };
    y = mulsign(y, x);
    if x.isnan() {
        SLEEF_NAN
    } else {
        y
    }
}

#[inline]
fn logk2(d: F2<f64>) -> F2<f64> {
    let e = ilogbk(d.0 * (1. / 0.75));

    let m = F2::new(ldexp2k(d.0, -e), ldexp2k(d.1, -e));

    let x = (m + (-1.)) / (m + 1.);
    let x2 = x.square();

    let t = 0.13860436390467167910856
        .mul_add(x2.0, 0.131699838841615374240845)
        .mul_add(x2.0, 0.153914168346271945653214)
        .mul_add(x2.0, 0.181816523941564611721589)
        .mul_add(x2.0, 0.22222224632662035403996)
        .mul_add(x2.0, 0.285714285511134091777308)
        .mul_add(x2.0, 0.400000000000914013309483)
        .mul_add(x2.0, 0.666666666666664853302393);

    (dd(0.693147180559945286226764, 2.319046813846299558417771e-17) * (e as f64))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * t)
}

pub fn xasinh(x: f64) -> f64 {
    let mut y = fabsk(x);

    let mut d = if y > 1. { x.recpre() } else { dd(y, 0.) };
    d = (d.square() + 1.).sqrt();
    d = if y > 1. { d * y } else { d };

    d = logk2(d.add_checked(x).normalize());
    y = d.0 + d.1;

    y = if fabsk(x) > SQRT_DBL_MAX || y.isnan() {
        mulsign(SLEEF_INFINITY, x)
    } else {
        y
    };
    y = if x.isnan() { SLEEF_NAN } else { y };
    if xisnegzero(x) {
        -0.
    } else {
        y
    }
}

pub fn xacosh(x: f64) -> f64 {
    let d = logk2(x.add_as_f2(1.).sqrt() * x.add_as_f2(-1.).sqrt() + x);
    let mut y = d.0 + d.1;

    y = if (x > SQRT_DBL_MAX) || y.isnan() {
        SLEEF_INFINITY
    } else {
        y
    };
    y = if x == 1. { 0. } else { y };
    y = if x < 1. { SLEEF_NAN } else { y };
    if x.isnan() {
        SLEEF_NAN
    } else {
        y
    }
}

pub fn xatanh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let d = logk2((1.).add_as_f2(y) / (1.).add_as_f2(-y));
    y = if y > 1.0 {
        SLEEF_NAN
    } else if y == 1.0 {
        SLEEF_INFINITY
    } else {
        (d.0 + d.1) * 0.5
    };

    y = mulsign(y, x);
    if x.isinf() || y.isnan() {
        SLEEF_NAN
    } else {
        y
    }
}

//

pub fn xcbrt(mut d: f64) -> f64 {
    // max error : 2 ulps
    let mut q = 1.;
    let e = ilogbk(fabsk(d)) + 1;
    d = ldexp2k(d, -e);
    let r = (e + 6144) % 3;
    q = if r == 1 {
        1.2599210498948731647672106
    } else {
        q
    };
    q = if r == 2 {
        1.5874010519681994747517056
    } else {
        q
    };
    q = ldexp2k(q, (e + 6144) / 3 - 2048);

    q = mulsign(q, d);
    d = fabsk(d);

    let mut x = (-0.640245898480692909870982)
        .mul_add(d, 2.96155103020039511818595)
        .mul_add(d, -5.73353060922947843636166)
        .mul_add(d, 6.03990368989458747961407)
        .mul_add(d, -3.85841935510444988821632)
        .mul_add(d, 2.2307275302496609725722);

    let mut y = x * x;
    y = y * y;
    x -= (d * y - x) * (1. / 3.);
    y = d * x * x;
    (y - (2. / 3.) * y * (y * x - 1.)) * q
}

pub fn xexp2(d: f64) -> f64 {
    let q = rintk(d) as isize;

    let s = d - (q as f64);

    let mut u = 0.4434359082926529454e-9
        .mul_add(s, 0.7073164598085707425e-8)
        .mul_add(s, 0.1017819260921760451e-6)
        .mul_add(s, 0.1321543872511327615e-5)
        .mul_add(s, 0.1525273353517584730e-4)
        .mul_add(s, 0.1540353045101147808e-3)
        .mul_add(s, 0.1333355814670499073e-2)
        .mul_add(s, 0.9618129107597600536e-2)
        .mul_add(s, 0.5550410866482046596e-1)
        .mul_add(s, 0.2402265069591012214e+0)
        .mul_add(s, 0.6931471805599452862e+0);
    u = (1.).add_checked(u.mul_as_f2(s)).normalize().0;

    u = ldexp2k(u, q);

    if d >= 1024. {
        SLEEF_INFINITY
    } else if d < -2000. {
        0.
    } else {
        u
    }
}

pub fn xexp10(d: f64) -> f64 {
    let q = rintk(d * LOG10_2) as isize;
    let qf = q as f64;
    let s = qf.mul_add(-L10U, d);
    let s = qf.mul_add(-L10L, s);

    let mut u = 0.2411463498334267652e-3
        .mul_add(s, 0.1157488415217187375e-2)
        .mul_add(s, 0.5013975546789733659e-2)
        .mul_add(s, 0.1959762320720533080e-1)
        .mul_add(s, 0.6808936399446784138e-1)
        .mul_add(s, 0.2069958494722676234e+0)
        .mul_add(s, 0.5393829292058536229e+0)
        .mul_add(s, 0.1171255148908541655e+1)
        .mul_add(s, 0.2034678592293432953e+1)
        .mul_add(s, 0.2650949055239205876e+1)
        .mul_add(s, 0.2302585092994045901e+1);
    u = (1.).add_checked(u.mul_as_f2(s)).normalize().0;

    if d > 308.25471555991671 {
        SLEEF_INFINITY // log10(DBL_MAX)
    } else if d < -350. {
        0.
    } else {
        ldexp2k(u, q)
    }
}

pub fn xexpm1(a: f64) -> f64 {
    let d = expk2(dd(a, 0.)) + (-1.0);
    if xisnegzero(a) {
        -0.
    } else if a > 709.782712893383996732223 {
        SLEEF_INFINITY // log(DBL_MAX)
    } else if a < -36.736800569677101399113302437 {
        -1. // log(1 - nexttoward(1, 0))
    } else {
        d.0 + d.1
    }
}

pub fn xlog10(mut d: f64) -> f64 {
    let o = d < f64::MIN;
    if o {
        d *= D1_32 * D1_32;
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_f2(m) / (1.).add_as_f2(m);
    let x2 = x.0 * x.0;

    let t = 0.6653725819576758460e-1
        .mul_add(x2, 0.6625722782820833712e-1)
        .mul_add(x2, 0.7898105214313944078e-1)
        .mul_add(x2, 0.9650955035715275132e-1)
        .mul_add(x2, 0.1240841409721444993e+0)
        .mul_add(x2, 0.1737177927454605086e+0)
        .mul_add(x2, 0.2895296546021972617e+0);

    let s = (dd(0.30102999566398119802, -2.803728127785170339e-18) * (e as f64))
        .add_checked(x * dd(0.86858896380650363334, 1.1430059694096389311e-17))
        .add_checked(x2 * x.0 * t);

    if d.isinf() {
        SLEEF_INFINITY
    } else if (d < 0.) || d.isnan() {
        SLEEF_NAN
    } else if d == 0. {
        -SLEEF_INFINITY
    } else {
        s.0 + s.1
    }
}

pub fn xlog2(mut d: f64) -> f64 {
    let o = d < f64::MIN;
    if o {
        d *= D1_32 * D1_32;
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_f2(m) / (1.).add_as_f2(m);
    let x2 = x.0 * x.0;

    let t = 0.2211941750456081490e+0
        .mul_add(x2, 0.2200768693152277689e+0)
        .mul_add(x2, 0.2623708057488514656e+0)
        .mul_add(x2, 0.3205977477944495502e+0)
        .mul_add(x2, 0.4121985945485324709e+0)
        .mul_add(x2, 0.5770780162997058982e+0)
        .mul_add(x2, 0.96179669392608091449);
    let s = (e as f64) + x * dd(2.885390081777926774, 6.0561604995516736434e-18) + x2 * x.0 * t;

    if d == 0. {
        -SLEEF_INFINITY
    } else if (d < 0.) || d.isnan() {
        SLEEF_NAN
    } else if d.isinf() {
        SLEEF_INFINITY
    } else {
        s.0 + s.1
    }
}

pub fn xlog1p(d: f64) -> f64 {
    let mut dp1 = d + 1.;

    let o = dp1 < f64::MIN;
    if o {
        dp1 *= D1_32 * D1_32
    };

    let mut e = ilogb2k(dp1 * (1. / 0.75));

    let t = ldexp3k(1., -e);
    let m = d.mul_add(t, t - 1.);

    if o {
        e -= 64;
    }

    let x = dd(m, 0.) / (2.).add_checked_as_f2(m);
    let x2 = x.0 * x.0;

    let t = 0.1532076988502701353e+0
        .mul_add(x2, 0.1525629051003428716e+0)
        .mul_add(x2, 0.1818605932937785996e+0)
        .mul_add(x2, 0.2222214519839380009e+0)
        .mul_add(x2, 0.2857142932794299317e+0)
        .mul_add(x2, 0.3999999999635251990e+0)
        .mul_add(x2, 0.6666666666667333541e+0);

    let s = (dd(0.693147180559945286226764, 2.319046813846299558417771e-17) * (e as f64))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x.0 * t);

    if xisnegzero(d) {
        -0.0
    } else if d == -1. {
        -SLEEF_INFINITY
    } else if (d < -1.) || d.isnan() {
        SLEEF_NAN
    } else if d > 1e+307 {
        SLEEF_INFINITY
    } else {
        s.0 + s.1
    }
}

//

pub fn xfma(mut x: f64, mut y: f64, mut z: f64) -> f64 {
    let mut h2 = x * y + z;
    let mut q = 1.;
    const C0: f64 = D1_54;
    const C1: f64 = C0 * C0;
    const C2: f64 = C1 * C1;
    if fabsk(h2) < 1e-300 {
        x *= C1;
        y *= C1;
        z *= C2;
        q = 1. / C2;
    }
    if fabsk(h2) > 1e+299 {
        x *= 1. / C1;
        y *= 1. / C1;
        z *= 1. / C2;
        q = C2;
    }
    let d = x.mul_as_f2(y) + z;
    let ret = if (x == 0.) || (y == 0.) { z } else { d.0 + d.1 };
    if z.isinf() && !x.isinf() && !x.isnan() && !y.isinf() && !y.isnan() {
        h2 = z;
    }
    if h2.isinf() || h2.isnan() {
        h2
    } else {
        ret * q
    }
}

pub fn xsqrt(d: f64) -> f64 {
    return SQRT(d);
}

pub fn xfabs(x: f64) -> f64 {
    return fabsk(x);
}

pub fn xcopysign(x: f64, y: f64) -> f64 {
    return copysignk(x, y);
}

pub fn xfmax(x: f64, y: f64) -> f64 {
    if y != y {
        x
    } else if x > y {
        x
    } else {
        y
    }
}

pub fn xfmin(x: f64, y: f64) -> f64 {
    if y != y {
        x
    } else if x < y {
        x
    } else {
        y
    }
}

pub fn xfdim(x: f64, y: f64) -> f64 {
    let ret = x - y;
    if (ret < 0.) || (x == y) {
        0.
    } else {
        ret
    }
}

pub fn xtrunc(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    if x.isinf() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

pub fn xfloor(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    fr = if fr < 0. { fr + 1. } else { fr };
    if x.isinf() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

pub fn xceil(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    fr = if fr <= 0. { fr } else { fr - 1. };
    if x.isinf() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

pub fn xround(d: f64) -> f64 {
    let mut x = d + 0.5;
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    if (fr == 0.) && (x <= 0.) {
        x -= 1.;
    }
    fr = if fr < 0. { fr + 1. } else { fr };
    let x = if d == 0.49999999999999994449 { 0. } else { x }; // nextafter(0.5, 0)
    if d.isinf() || (fabsk(d) >= D1_52) {
        d
    } else {
        copysignk(x - fr, d)
    }
}

pub fn xrint(d: f64) -> f64 {
    let x = d + 0.5;
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    let isodd = (1 & (fr as i32)) != 0;
    fr = fr - (fr as i32 as f64);
    fr = if (fr < 0.) || ((fr == 0.) && isodd) {
        fr + 1.
    } else {
        fr
    };
    let x = if d == 0.50000000000000011102 { 0. } else { x }; // nextafter(0.5, 1)
    if d.isinf() || (fabsk(d) >= D1_52) {
        d
    } else {
        copysignk(x - fr, d)
    }
}

pub fn xnextafter(x: f64, y: f64) -> f64 {
    let x = if x == 0. { mulsign(0., y) } else { x };
    let mut cxi = x.to_bits() as i64;
    let c = (cxi < 0) == (y < x);
    if c {
        cxi = -(cxi ^ (1 << 63));
    }

    if x != y {
        cxi -= 1;
    }

    if c {
        cxi = -(((cxi as u64) ^ (1u64 << 63)) as i64);
    }

    let cxf = f64::from_bits(cxi as u64);
    if x.isnan() || y.isnan() {
        SLEEF_NAN
    } else if (x == 0.) && (y == 0.) {
        y
    } else if (cxf == 0.) && (x != 0.) {
        mulsign(0., x)
    } else {
        cxf
    }
}

pub fn xfrfrexp(mut x: f64) -> f64 {
    if fabsk(x) < f64::MIN {
        x *= D1_63;
    }

    let mut cxu = x.to_bits();
    cxu &= !0x7ff0000000000000u64;
    cxu |= 0x3fe0000000000000u64;

    if x == 0. {
        x
    } else if x.isinf() {
        mulsign(SLEEF_INFINITY, x)
    } else {
        f64::from_bits(cxu)
    }
}

pub fn xexpfrexp(mut x: f64) -> i32 {
    let mut ret = 0;

    if fabsk(x) < f64::MIN {
        x *= D1_63;
        ret = -63;
    }

    let cxu = x.to_bits();
    ret += (((cxu >> 52) & 0x7ff) as i32) - 0x3fe;

    if x == 0. || x.isnan() || x.isinf() {
        0
    } else {
        ret
    }
}

#[inline]
fn toward0(d: f64) -> f64 {
    if d == 0. {
        0.
    } else {
        long_bits_to_double(double_to_raw_long_bits(d) - 1)
    }
}

#[inline]
fn removelsb(d: f64) -> f64 {
    long_bits_to_double(double_to_raw_long_bits(d) & 0xfffffffffffffffe)
}

#[inline]
fn ptrunc(x: f64) -> f64 {
    let fr = (-D1_31).mul_add((x * (1. / D1_31)) as i32 as f64, x);
    if fabsk(x) >= D1_52 {
        x
    } else {
        x - (fr - (fr as i32 as f64))
    }
}

pub fn xfmod(x: f64, y: f64) -> f64 {
    let mut nu = fabsk(x);
    let mut de = fabsk(y);
    let mut s = 1.;
    if de < f64::MIN {
        nu *= D1_54;
        de *= D1_54;
        s = 1. / D1_54;
    }
    let mut r = dd(nu, 0.);
    let rde = toward0(1. / de);

    for _ in 0..21 {
        // ceil(log2(DBL_MAX) / 51) + 1
        let q = if (de + de > r.0) && (r.0 >= de) {
            1.
        } else {
            toward0(r.0) * rde
        };
        r = (r + removelsb(ptrunc(q)).mul_as_f2(-de)).normalize();
        if r.0 < de {
            break;
        }
    }

    let mut ret = r.0 * s;
    if r.0 + r.1 == de {
        ret = 0.;
    }
    ret = mulsign(ret, x);
    if de == 0. {
        SLEEF_NAN
    } else if nu < de {
        x
    } else {
        ret
    }
}

pub fn xmodf(x: f64) -> F2<f64> {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    fr = if fabsk(x) >= D1_52 { 0. } else { fr };
    F2::new(copysignk(fr, x), copysignk(x - fr, x))
}

fn gammak(a: f64) -> (F2<f64>, F2<f64>) {
    let mut clln = dd(1., 0.);
    let mut clld = dd(1., 0.);

    let otiny = fabsk(a) < 1e-306;
    let oref = a < 0.5;

    let mut x = if otiny {
        dd(0., 0.)
    } else if oref {
        (1.).add_as_f2(-a)
    } else {
        dd(a, 0.)
    };

    let o0 = (0.5 <= x.0) && (x.0 <= 1.1);
    let o2 = 2.3 < x.0;

    let mut y = ((x + 1.) * x).normalize();
    y = ((x + 2.) * y).normalize();
    y = ((x + 3.) * y).normalize();
    y = ((x + 4.) * y).normalize();

    clln = if o2 && (x.0 <= 7.) { y } else { clln };

    x = if o2 && (x.0 <= 7.) { x + 5. } else { x };
    let t = if o2 {
        1. / x.0
    } else {
        (x + (if o0 { -1. } else { -2. })).normalize().0
    };

    let u = (if o2 {
        -156.801412704022726379848862
    } else if o0 {
        0.2947916772827614196e+2
    } else {
        0.7074816000864609279e-7
    }).mul_add(
        t,
        if o2 {
            1.120804464289911606838558160000
        } else if o0 {
            0.1281459691827820109e+3
        } else {
            0.4009244333008730443e-6
        },
    ).mul_add(
        t,
        if o2 {
            13.39798545514258921833306020000
        } else if o0 {
            0.2617544025784515043e+3
        } else {
            0.1040114641628246946e-5
        },
    ).mul_add(
        t,
        if o2 {
            -0.116546276599463200848033357000
        } else if o0 {
            0.3287022855685790432e+3
        } else {
            0.1508349150733329167e-5
        },
    ).mul_add(
        t,
        if o2 {
            -1.391801093265337481495562410000
        } else if o0 {
            0.2818145867730348186e+3
        } else {
            0.1288143074933901020e-5
        },
    ).mul_add(
        t,
        if o2 {
            0.015056113040026424412918973400
        } else if o0 {
            0.1728670414673559605e+3
        } else {
            0.4744167749884993937e-6
        },
    ).mul_add(
        t,
        if o2 {
            0.179540117061234856098844714000
        } else if o0 {
            0.7748735764030416817e+2
        } else {
            -0.6554816306542489902e-7
        },
    ).mul_add(
        t,
        if o2 {
            -0.002481743600264997730942489280
        } else if o0 {
            0.2512856643080930752e+2
        } else {
            -0.3189252471452599844e-6
        },
    ).mul_add(
        t,
        if o2 {
            -0.029527880945699120504851034100
        } else if o0 {
            0.5766792106140076868e+1
        } else {
            0.1358883821470355377e-6
        },
    ).mul_add(
        t,
        if o2 {
            0.000540164767892604515196325186
        } else if o0 {
            0.7270275473996180571e+0
        } else {
            -0.4343931277157336040e-6
        },
    ).mul_add(
        t,
        if o2 {
            0.006403362833808069794787256200
        } else if o0 {
            0.8396709124579147809e-1
        } else {
            0.9724785897406779555e-6
        },
    ).mul_add(
        t,
        if o2 {
            -0.000162516262783915816896611252
        } else if o0 {
            -0.8211558669746804595e-1
        } else {
            -0.2036886057225966011e-5
        },
    ).mul_add(
        t,
        if o2 {
            -0.001914438498565477526465972390
        } else if o0 {
            0.6828831828341884458e-1
        } else {
            0.4373363141819725815e-5
        },
    ).mul_add(
        t,
        if o2 {
            7.20489541602001055898311517e-05
        } else if o0 {
            -0.7712481339961671511e-1
        } else {
            -0.9439951268304008677e-5
        },
    ).mul_add(
        t,
        if o2 {
            0.000839498720672087279971000786
        } else if o0 {
            0.8337492023017314957e-1
        } else {
            0.2050727030376389804e-4
        },
    ).mul_add(
        t,
        if o2 {
            -5.17179090826059219329394422e-05
        } else if o0 {
            -0.9094964931456242518e-1
        } else {
            -0.4492620183431184018e-4
        },
    ).mul_add(
        t,
        if o2 {
            -0.000592166437353693882857342347
        } else if o0 {
            0.1000996313575929358e+0
        } else {
            0.9945751236071875931e-4
        },
    ).mul_add(
        t,
        if o2 {
            6.97281375836585777403743539e-05
        } else if o0 {
            -0.1113342861544207724e+0
        } else {
            -0.2231547599034983196e-3
        },
    ).mul_add(
        t,
        if o2 {
            0.000784039221720066627493314301
        } else if o0 {
            0.1255096673213020875e+0
        } else {
            0.5096695247101967622e-3
        },
    ).mul_add(
        t,
        if o2 {
            -0.000229472093621399176949318732
        } else if o0 {
            -0.1440498967843054368e+0
        } else {
            -0.1192753911667886971e-2
        },
    ).mul_add(
        t,
        if o2 {
            -0.002681327160493827160473958490
        } else if o0 {
            0.1695571770041949811e+0
        } else {
            0.2890510330742210310e-2
        },
    ).mul_add(
        t,
        if o2 {
            0.003472222222222222222175164840
        } else if o0 {
            -0.2073855510284092762e+0
        } else {
            -0.7385551028674461858e-2
        },
    ).mul_add(
        t,
        if o2 {
            0.083333333333333333335592087900
        } else if o0 {
            0.2705808084277815939e+0
        } else {
            0.2058080842778455335e-1
        },
    );

    y = (x + (-0.5)) * logk2(x);
    y += -x;
    y += dd(0.91893853320467278056, -3.8782941580672414498e-17); // 0.5*log(2*M_PI)

    let mut z = u.mul_as_f2(t)
        + (if o0 {
            -0.4006856343865314862e+0
        } else {
            -0.6735230105319810201e-1
        });
    z = z * t
        + (if o0 {
            0.8224670334241132030e+0
        } else {
            0.3224670334241132030e+0
        });
    z = z * t
        + (if o0 {
            -0.5772156649015328655e+0
        } else {
            0.4227843350984671345e+0
        });
    z *= t;

    let mut clc = if o2 { y } else { z };

    clld = if o2 { u.mul_as_f2(t) + 1. } else { clld };

    y = clln;

    clc = if otiny {
        dd(83.1776616671934334590333, 3.67103459631568507221878e-15) // log(2^120)
    } else if oref {
        dd(1.1447298858494001639, 1.026595116270782638e-17) + (-clc)
    } else {
        clc
    }; // log(M_PI)
    let clln = if otiny {
        dd(1., 0.)
    } else if oref {
        clln
    } else {
        clld
    };

    if oref {
        x = clld * sinpik(a - D1_28 * ((a * (1. / D1_28)) as i32 as f64));
    }

    clld = if otiny {
        dd(a * (D1_60 * D1_60), 0.)
    } else if oref {
        x
    } else {
        y
    };

    (clc, clln / clld)
}

pub mod u1 {
    use super::gammak;
    use super::rempi;
    use super::rintk;
    use super::trunck;
    use common::*;
    use f2::*;

    pub fn xacos(d: f64) -> f64 {
        let o = fabsk(d) < 0.5;
        let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
        let mut x = if o { dd(fabsk(d), 0.) } else { x2.sqrt_as_f2() };
        x = if fabsk(d) == 1. { dd(0., 0.) } else { x };

        let u = 0.3161587650653934628e-1
            .mul_add(x2, -0.1581918243329996643e-1)
            .mul_add(x2, 0.1929045477267910674e-1)
            .mul_add(x2, 0.6606077476277170610e-2)
            .mul_add(x2, 0.1215360525577377331e-1)
            .mul_add(x2, 0.1388715184501609218e-1)
            .mul_add(x2, 0.1735956991223614604e-1)
            .mul_add(x2, 0.2237176181932048341e-1)
            .mul_add(x2, 0.3038195928038132237e-1)
            .mul_add(x2, 0.4464285681377102438e-1)
            .mul_add(x2, 0.7500000000378581611e-1)
            .mul_add(x2, 0.1666666666666497543e+0)
            * x.0
            * x2;

        let mut y = dd(3.141592653589793116 / 2., 1.2246467991473532072e-16 / 2.)
            .sub_checked(mulsign(x.0, d).add_checked_as_f2(mulsign(u, d)));
        x.add_checked_assign(u);
        y = if o { y } else { x.scale(2.) };
        if !o && (d < 0.) {
            y = dd(3.141592653589793116, 1.2246467991473532072e-16).sub_checked(y)
        };
        y.0 + y.1
    }

    pub fn xatan(d: f64) -> f64 {
        let d2 = atan2k(dd(fabsk(d), 0.), dd(1., 0.));
        let mut r = d2.0 + d2.1;
        if d.isinf() {
            r = 1.570796326794896557998982;
        }
        mulsign(r, d)
    }

    fn atan2k(mut y: F2<f64>, mut x: F2<f64>) -> F2<f64> {
        let mut q: isize = 0;
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
        let mut t = s.square();
        t = t.normalize();

        let u = 1.06298484191448746607415e-05
            .mul_add(t.0, -0.000125620649967286867384336)
            .mul_add(t.0, 0.00070557664296393412389774)
            .mul_add(t.0, -0.00251865614498713360352999)
            .mul_add(t.0, 0.00646262899036991172313504)
            .mul_add(t.0, -0.0128281333663399031014274)
            .mul_add(t.0, 0.0208024799924145797902497)
            .mul_add(t.0, -0.0289002344784740315686289)
            .mul_add(t.0, 0.0359785005035104590853656)
            .mul_add(t.0, -0.041848579703592507506027)
            .mul_add(t.0, 0.0470843011653283988193763)
            .mul_add(t.0, -0.0524914210588448421068719)
            .mul_add(t.0, 0.0587946590969581003860434)
            .mul_add(t.0, -0.0666620884778795497194182)
            .mul_add(t.0, 0.0769225330296203768654095)
            .mul_add(t.0, -0.0909090442773387574781907)
            .mul_add(t.0, 0.111111108376896236538123)
            .mul_add(t.0, -0.142857142756268568062339)
            .mul_add(t.0, 0.199999999997977351284817)
            .mul_add(t.0, -0.333333333333317605173818);

        t *= u;
        t = s * (1.).add_checked(t);
        if fabsk(s.0) < 1e-200 {
            t = s;
        }
        dd(1.570796326794896557998982, 6.12323399573676603586882e-17) * (q as f64) + t
    }

    pub fn xatan2(mut y: f64, mut x: f64) -> f64 {
        if fabsk(x) < 5.5626846462680083984e-309 {
            y *= D1_53;
            x *= D1_53;
        } // nexttoward((1.0 / DBL_MAX), 1)
        let d = atan2k(dd(fabsk(y), 0.), dd(x, 0.));
        let mut r = d.0 + d.1;

        r = if y == 0. {
            if sign(x) == -1. {
                M_PI
            } else {
                0.
            }
        } else if y.isinf() {
            M_PI / 2.
                - (if x.isinf() {
                    (sign(x) * (M_PI * 1. / 4.))
                } else {
                    0.
                })
        } else if x.isinf() || (x == 0.) {
            M_PI / 2.
                - (if x.isinf() {
                    (sign(x) * (M_PI / 2.))
                } else {
                    0.
                })
        } else {
            mulsign(r, x)
        };
        if x.isnan() || y.isnan() {
            SLEEF_NAN
        } else {
            mulsign(r, y)
        }
    }

    pub fn xasin(d: f64) -> f64 {
        let o = fabsk(d) < 0.5;
        let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
        let mut x = if o { dd(fabsk(d), 0.) } else { x2.sqrt_as_f2() };
        x = if fabsk(d) == 1.0 { dd(0., 0.) } else { x };

        let u = 0.3161587650653934628e-1
            .mul_add(x2, -0.1581918243329996643e-1)
            .mul_add(x2, 0.1929045477267910674e-1)
            .mul_add(x2, 0.6606077476277170610e-2)
            .mul_add(x2, 0.1215360525577377331e-1)
            .mul_add(x2, 0.1388715184501609218e-1)
            .mul_add(x2, 0.1735956991223614604e-1)
            .mul_add(x2, 0.2237176181932048341e-1)
            .mul_add(x2, 0.3038195928038132237e-1)
            .mul_add(x2, 0.4464285681377102438e-1)
            .mul_add(x2, 0.7500000000378581611e-1)
            .mul_add(x2, 0.1666666666666497543e+0)
            * x2
            * x.0;

        let y = dd(3.141592653589793116 / 4., 1.2246467991473532072e-16 / 4.)
            .sub_checked(x)
            .add_checked(-u);
        let r = if o { u + x.0 } else { (y.0 + y.1) * 2. };
        mulsign(r, d)
    }

    pub fn xsin(d: f64) -> f64 {
        let mut s: F2<f64>;
        let ql: isize;

        if fabsk(d) < TRIGRANGEMAX2 {
            let qlf = rintk(d * M_1_PI);
            ql = qlf as isize;
            s = qlf.mul_add(-PI_A2, d).add_checked_as_f2(qlf * -PI_B2);
        } else if fabsk(d) < TRIGRANGEMAX {
            let dqh = trunck(d * (M_1_PI / D1_24)) * D1_24;
            let qlf = rintk(d.mul_add(M_1_PI, -dqh));
            ql = qlf as isize;

            s = dqh.mul_add(-PI_A, d).add_checked_as_f2(qlf * -PI_A);
            s += dqh * -PI_B;
            s += qlf * -PI_B;
            s += dqh * -PI_C;
            s += qlf * -PI_C;
            s = s.add_checked((dqh + qlf) * -PI_D);
        } else {
            let (mut ddidd, ddii) = rempi(d);
            ql = (((ddii & 3) * 2 + ((ddidd.0 > 0.) as i32) + 1) >> 2) as isize;
            if (ddii & 1) != 0 {
                ddidd = ddidd + dd(
                    mulsign(3.141592653589793116 * -0.5, ddidd.0),
                    mulsign(1.2246467991473532072e-16 * -0.5, ddidd.0),
                );
            }
            s = ddidd.normalize();
            if d.isinf() || d.isnan() {
                s.0 = SLEEF_NAN;
            }
        }

        let t = s;
        s = s.square();

        let u = 2.72052416138529567917983e-15
            .mul_add(s.0, -7.6429259411395447190023e-13)
            .mul_add(s.0, 1.60589370117277896211623e-10)
            .mul_add(s.0, -2.5052106814843123359368e-08)
            .mul_add(s.0, 2.75573192104428224777379e-06)
            .mul_add(s.0, -0.000198412698412046454654947)
            .mul_add(s.0, 0.00833333333333318056201922);

        let x = (1.).add_checked((-0.166666666666666657414808).add_checked_as_f2(u * s.0) * s);

        let u = t.mul_as_f(x);

        if xisnegzero(d) {
            d
        } else if (ql & 1) != 0 {
            -u
        } else {
            u
        }
    }

    pub fn xcos(d: f64) -> f64 {
        let mut s: F2<f64>;
        let ql: isize;

        let d = fabsk(d);

        if d < TRIGRANGEMAX2 {
            ql = (2.).mul_add(rintk(d * M_1_PI - 0.5), 1.) as isize;
            let qlf = ql as f64;
            s = d
                .add_as_f2(qlf * (-PI_A2 * 0.5))
                .add_checked(qlf * (-PI_B2 * 0.5));
        } else if d < TRIGRANGEMAX {
            let mut dqh = trunck(d * (M_1_PI / D1_23) - 0.5 * (M_1_PI / D1_23));
            let qlf = 2. * rintk(d * M_1_PI - 0.5 - dqh * D1_23) + 1.;
            ql = qlf as isize;
            dqh *= D1_24;

            let u = dqh.mul_add(-PI_A * 0.5, d);
            s = u.add_as_f2(qlf * (-PI_A * 0.5));
            s += dqh * (-PI_B * 0.5);
            s += qlf * (-PI_B * 0.5);
            s += dqh * (-PI_C * 0.5);
            s = (s + qlf * (-PI_C * 0.5)).add_checked((dqh + qlf) * (-PI_D * 0.5));
        } else {
            let (mut ddidd, ddii) = rempi(d);
            ql = (((ddii & 3) * 2 + ((ddidd.0 > 0.) as i32) + 7) >> 1) as isize;
            if (ddii & 1) == 0 {
                ddidd = ddidd + dd(
                    mulsign(
                        3.141592653589793116 * -0.5,
                        if ddidd.0 > 0. { 1. } else { -1. },
                    ),
                    mulsign(
                        1.2246467991473532072e-16 * -0.5,
                        if ddidd.0 > 0. { 1. } else { -1. },
                    ),
                );
            }
            s = ddidd.normalize();
            if d.isinf() || d.isnan() {
                s.0 = SLEEF_NAN;
            }
        }

        let t = s;
        s = s.square();

        let u = 2.72052416138529567917983e-15
            .mul_add(s.0, -7.6429259411395447190023e-13)
            .mul_add(s.0, 1.60589370117277896211623e-10)
            .mul_add(s.0, -2.5052106814843123359368e-08)
            .mul_add(s.0, 2.75573192104428224777379e-06)
            .mul_add(s.0, -0.000198412698412046454654947)
            .mul_add(s.0, 0.00833333333333318056201922);

        let x = (1.).add_checked((-0.166666666666666657414808).add_checked_as_f2(u * s.0) * s);

        let u = t.mul_as_f(x);

        if ((ql as isize) & 2) == 0 {
            -u
        } else {
            u
        }
    }

    pub fn xsincos(d: f64) -> (f64, f64) {
        let mut s: F2<f64>;
        let ql: isize;

        if fabsk(d) < TRIGRANGEMAX2 {
            let qlf = rintk(d * (2. * M_1_PI));
            ql = qlf as isize;
            s = qlf
                .mul_add(-PI_A2 * 0.5, d)
                .add_checked_as_f2(qlf * (-PI_B2 * 0.5));
        } else if fabsk(d) < TRIGRANGEMAX {
            let dqh = trunck(d * ((2. * M_1_PI) / D1_24)) * D1_24;
            let qlf = rintk(d * (2. * M_1_PI) - dqh);
            ql = qlf as isize;

            s = dqh
                .mul_add(-PI_A * 0.5, d)
                .add_checked_as_f2(qlf * (-PI_A * 0.5));
            s += dqh * (-PI_B * 0.5);
            s += qlf * (-PI_B * 0.5);
            s += dqh * (-PI_C * 0.5);
            s = (s + qlf * (-PI_C * 0.5)).add_checked((dqh + qlf) * (-PI_D * 0.5));
        } else {
            let (ddidd, ddii) = rempi(d);
            ql = ddii as isize;
            s = ddidd;
            if d.isinf() || d.isnan() {
                s = dd(SLEEF_NAN, SLEEF_NAN);
            }
        }

        let t = s;

        s.0 = s.square_as_f();

        let u = 1.58938307283228937328511e-10
            .mul_add(s.0, -2.50506943502539773349318e-08)
            .mul_add(s.0, 2.75573131776846360512547e-06)
            .mul_add(s.0, -0.000198412698278911770864914)
            .mul_add(s.0, 0.0083333333333191845961746)
            .mul_add(s.0, -0.166666666666666130709393)
            * s.0
            * t.0;

        let x = t.add_checked(u);
        let mut rsin = x.0 + x.1;

        if xisnegzero(d) {
            rsin = -0.;
        }

        let u = (-1.13615350239097429531523e-11)
            .mul_add(s.0, 2.08757471207040055479366e-09)
            .mul_add(s.0, -2.75573144028847567498567e-07)
            .mul_add(s.0, 2.48015872890001867311915e-05)
            .mul_add(s.0, -0.00138888888888714019282329)
            .mul_add(s.0, 0.0416666666666665519592062)
            .mul_add(s.0, -0.5);

        let x = (1.).add_checked(s.0.mul_as_f2(u));
        let mut rcos = x.0 + x.1;

        if (ql & 1) != 0 {
            let u = rcos;
            rcos = rsin;
            rsin = u;
        }
        if (ql & 2) != 0 {
            rsin = -rsin;
        }
        if ((ql + 1) & 2) != 0 {
            rcos = -rcos;
        }

        (rsin, rcos)
    }

    pub fn xtan(d: f64) -> f64 {
        let mut s: F2<f64>;
        let ql: isize;

        if fabsk(d) < TRIGRANGEMAX2 {
            let qlf = rintk(d * (2. * M_1_PI));
            ql = qlf as isize;
            s = qlf
                .mul_add(-PI_A2 * 0.5, d)
                .add_checked_as_f2(qlf * (-PI_B2 * 0.5));
        } else if fabsk(d) < TRIGRANGEMAX {
            let dqh = trunck(d * (M_2_PI / D1_24)) * D1_24;
            s = dd(M_2_PI_H, M_2_PI_L) * d + ((if d < 0. { -0.5 } else { 0.5 }) - dqh);
            ql = (s.0 + s.1) as isize;

            let qlf = ql as f64;

            s = dqh
                .mul_add(-PI_A * 0.5, d)
                .add_checked_as_f2(qlf * (-PI_A * 0.5));
            s += dqh * (-PI_B * 0.5);
            s += qlf * (-PI_B * 0.5);
            s += dqh * (-PI_C * 0.5);
            s = (s + qlf * (-PI_C * 0.5)).add_checked((dqh + qlf) * (-PI_D * 0.5));
        } else {
            let (ddidd, ddii) = rempi(d);
            ql = ddii as isize;
            s = ddidd;
            if d.isinf() || d.isnan() {
                s.0 = SLEEF_NAN;
            }
        }

        if (ql & 1) != 0 {
            s = -s;
        }

        let t = s;
        s = s.square();

        let u = 1.01419718511083373224408e-05
            .mul_add(s.0, -2.59519791585924697698614e-05)
            .mul_add(s.0, 5.23388081915899855325186e-05)
            .mul_add(s.0, -3.05033014433946488225616e-05)
            .mul_add(s.0, 7.14707504084242744267497e-05)
            .mul_add(s.0, 8.09674518280159187045078e-05)
            .mul_add(s.0, 0.000244884931879331847054404)
            .mul_add(s.0, 0.000588505168743587154904506)
            .mul_add(s.0, 0.00145612788922812427978848)
            .mul_add(s.0, 0.00359208743836906619142924)
            .mul_add(s.0, 0.00886323944362401618113356)
            .mul_add(s.0, 0.0218694882853846389592078)
            .mul_add(s.0, 0.0539682539781298417636002)
            .mul_add(s.0, 0.133333333333125941821962);

        let mut x = (1.).add_checked((0.333333333333334980164153).add_checked_as_f2(u * s.0) * s);
        x = t * x;

        if (ql & 1) != 0 {
            x = x.recpre();
        }

        if xisnegzero(d) {
            d
        } else {
            x.0 + x.1
        }
    }

    pub fn xlog(mut d: f64) -> f64 {
        let o = d < f64::MIN;
        if o {
            d *= D1_32 * D1_32;
        }

        let mut e = super::ilogb2k(d * (1. / 0.75));
        let m = super::ldexp3k(d, -e);

        if o {
            e -= 64;
        }

        let x = (-1.).add_as_f2(m) / (1.).add_as_f2(m);
        let x2 = x.0 * x.0;

        let t = 0.1532076988502701353e+0
            .mul_add(x2, 0.1525629051003428716e+0)
            .mul_add(x2, 0.1818605932937785996e+0)
            .mul_add(x2, 0.2222214519839380009e+0)
            .mul_add(x2, 0.2857142932794299317e+0)
            .mul_add(x2, 0.3999999999635251990e+0)
            .mul_add(x2, 0.6666666666667333541e+0);

        let s = (dd(0.693147180559945286226764, 2.319046813846299558417771e-17) * (e as f64))
            .add_checked(x.scale(2.))
            .add_checked(x2 * x.0 * t);

        if d == 0. {
            -SLEEF_INFINITY
        } else if (d < 0.) || d.isnan() {
            SLEEF_NAN
        } else if d.isinf() {
            SLEEF_INFINITY
        } else {
            s.0 + s.1
        }
    }

    pub fn xcbrt(d: f64) -> f64 {
        let mut q2 = dd(1., 0.);

        let e = super::ilogbk(fabsk(d)) + 1;
        let d = super::ldexp2k(d, -e);
        let r = (e + 6144) % 3;
        q2 = if r == 1 {
            dd(1.2599210498948731907, -2.5899333753005069177e-17)
        } else {
            q2
        };
        q2 = if r == 2 {
            dd(1.5874010519681995834, -1.0869008194197822986e-16)
        } else {
            q2
        };

        q2.0 = mulsign(q2.0, d);
        q2.1 = mulsign(q2.1, d);
        let d = fabsk(d);

        let mut x = (-0.640245898480692909870982)
            .mul_add(d, 2.96155103020039511818595)
            .mul_add(d, -5.73353060922947843636166)
            .mul_add(d, 6.03990368989458747961407)
            .mul_add(d, -3.85841935510444988821632)
            .mul_add(d, 2.2307275302496609725722);

        let mut y = x * x;
        y = y * y;
        x -= (d * y - x) * (1. / 3.);

        let z = x;

        let mut u = x.mul_as_f2(x);
        u = u * u * d + (-x);
        y = u.0 + u.1;

        y = -2. / 3. * y * z;
        let v = (z.mul_as_f2(z) + y) * d * q2;

        if d == 0. {
            mulsign(0., q2.0)
        } else if d.isinf() {
            mulsign(SLEEF_INFINITY, q2.0)
        } else {
            super::ldexp2k(v.0 + v.1, (e + 6144) / 3 - 2048)
        }
    }

    pub fn xtgamma(a: f64) -> f64 {
        let (da, db) = gammak(a);
        let y = super::expk2(da) * db;
        let r = y.0 + y.1;
        let r = if (a == -SLEEF_INFINITY)
            || ((a < 0.) && xisint(a))
            || (xisnumber(a) && (a < 0.) && r.isnan())
        {
            SLEEF_NAN
        } else {
            r
        };
        if ((a == SLEEF_INFINITY) || xisnumber(a))
            && a >= -f64::MIN
            && ((a == 0.) || (a > 200.) || r.isnan())
        {
            mulsign(SLEEF_INFINITY, a)
        } else {
            r
        }
    }

    pub fn xlgamma(a: f64) -> f64 {
        let (da, db) = gammak(a);
        let y = da + super::logk2(db.abs());
        let r = y.0 + y.1;
        if a.isinf() || ((a <= 0.) && xisint(a)) || (xisnumber(a) && r.isnan()) {
            SLEEF_INFINITY
        } else {
            r
        }
    }

    pub fn xerf(a: f64) -> f64 {
        let s = a;

        let a = fabsk(a);
        let o0 = a < 1.;
        let o1 = a < 3.7;
        let o2 = a < 6.;
        let u = if o0 { a * a } else { a };

        let t = (if o0 {
            0.6801072401395392157e-20
        } else if o1 {
            0.2830954522087717660e-13
        } else {
            -0.5846750404269610493e-17
        }).mul_add(
            u,
            if o0 {
                -0.2161766247570056391e-18
            } else if o1 {
                -0.1509491946179481940e-11
            } else {
                0.6076691048812607898e-15
            },
        ).mul_add(
            u,
            if o0 {
                0.4695919173301598752e-17
            } else if o1 {
                0.3827857177807173152e-10
            } else {
                -0.3007518609604893831e-13
            },
        ).mul_add(
            u,
            if o0 {
                -0.9049140419888010819e-16
            } else if o1 {
                -0.6139733921558987241e-09
            } else {
                0.9427906260824646063e-12
            },
        ).mul_add(
            u,
            if o0 {
                0.1634018903557411517e-14
            } else if o1 {
                0.6985387934608038824e-08
            } else {
                -0.2100110908269393629e-10
            },
        ).mul_add(
            u,
            if o0 {
                -0.2783485786333455216e-13
            } else if o1 {
                -0.5988224513034371474e-07
            } else {
                0.3534639523461223473e-09
            },
        ).mul_add(
            u,
            if o0 {
                0.4463221276786412722e-12
            } else if o1 {
                0.4005716952355346640e-06
            } else {
                -0.4664967728285395926e-08
            },
        ).mul_add(
            u,
            if o0 {
                -0.6711366622850138987e-11
            } else if o1 {
                -0.2132190104575784400e-05
            } else {
                0.4943823283769000532e-07
            },
        ).mul_add(
            u,
            if o0 {
                0.9422759050232658346e-10
            } else if o1 {
                0.9092461304042630325e-05
            } else {
                -0.4271203394761148254e-06
            },
        ).mul_add(
            u,
            if o0 {
                -0.1229055530100228477e-08
            } else if o1 {
                -0.3079188080966205457e-04
            } else {
                0.3034067677404915895e-05
            },
        ).mul_add(
            u,
            if o0 {
                0.1480719281585085023e-07
            } else if o1 {
                0.7971413443082370762e-04
            } else {
                -0.1776295289066871135e-04
            },
        ).mul_add(
            u,
            if o0 {
                -0.1636584469123402714e-06
            } else if o1 {
                -0.1387853215225442864e-03
            } else {
                0.8524547630559505050e-04
            },
        ).mul_add(
            u,
            if o0 {
                0.1646211436588923363e-05
            } else if o1 {
                0.6469678026257590965e-04
            } else {
                -0.3290582944961784398e-03
            },
        ).mul_add(
            u,
            if o0 {
                -0.1492565035840624866e-04
            } else if o1 {
                0.4996645280372945860e-03
            } else {
                0.9696966068789101157e-03
            },
        ).mul_add(
            u,
            if o0 {
                0.1205533298178966496e-03
            } else if o1 {
                -0.1622802482842520535e-02
            } else {
                -0.1812527628046986137e-02
            },
        ).mul_add(
            u,
            if o0 {
                -0.8548327023450851166e-03
            } else if o1 {
                0.1615320557049377171e-03
            } else {
                -0.4725409828123619017e-03
            },
        ).mul_add(
            u,
            if o0 {
                0.5223977625442188799e-02
            } else if o1 {
                0.1915262325574875607e-01
            } else {
                0.2090315427924229266e-01
            },
        ).mul_add(
            u,
            if o0 {
                -0.2686617064513125569e-01
            } else if o1 {
                -0.1027818298486033455e+00
            } else {
                -0.1052041921842776645e+00
            },
        ).mul_add(
            u,
            if o0 {
                0.1128379167095512753e+00
            } else if o1 {
                -0.6366172819842503827e+00
            } else {
                -0.6345351808766568347e+00
            },
        ).mul_add(
            u,
            if o0 {
                -0.3761263890318375380e+00
            } else if o1 {
                -0.1128379590648910469e+01
            } else {
                -0.1129442929103524396e+01
            },
        );
        let mut d = t.mul_as_f2(u);
        d += if o0 {
            dd(1.1283791670955125586, 1.5335459613165822674e-17)
        } else if o1 {
            dd(3.4110644736196137587e-08, -2.4875650708323294246e-24)
        } else {
            dd(0.00024963035690526438285, -5.4362665034856259795e-21)
        };
        d = if o0 {
            d * a
        } else {
            (1.).add_checked(-super::expk2(d))
        };
        if a.isnan() {
            SLEEF_NAN
        } else {
            mulsign(if o2 { d.0 + d.1 } else { 1. }, s)
        }
    }

}

pub mod u15 {
    use common::*;
    use f2::*;

    pub fn xerfc(a: f64) -> f64 {
        let s = a;
        let a = fabsk(a);
        let o0 = a < 1.;
        let o1 = a < 2.2;
        let o2 = a < 4.2;
        let o3 = a < 27.3;
        let u = if o0 {
            a.mul_as_f2(a)
        } else if o1 {
            dd(a, 0.)
        } else {
            dd(1., 0.) / dd(a, 0.)
        };

        let t = (if o0 {
            0.6801072401395386139e-20
        } else if o1 {
            0.3438010341362585303e-12
        } else if o2 {
            -0.5757819536420710449e+2
        } else {
            0.2334249729638701319e+5
        }).mul_add(
            u.0,
            if o0 {
                -0.2161766247570055669e-18
            } else if o1 {
                -0.1237021188160598264e-10
            } else if o2 {
                0.4669289654498104483e+3
            } else {
                -0.4695661044933107769e+5
            },
        ).mul_add(
            u.0,
            if o0 {
                0.4695919173301595670e-17
            } else if o1 {
                0.2117985839877627852e-09
            } else if o2 {
                -0.1796329879461355858e+4
            } else {
                0.3173403108748643353e+5
            },
        ).mul_add(
            u.0,
            if o0 {
                -0.9049140419888007122e-16
            } else if o1 {
                -0.2290560929177369506e-08
            } else if o2 {
                0.4355892193699575728e+4
            } else {
                0.3242982786959573787e+4
            },
        ).mul_add(
            u.0,
            if o0 {
                0.1634018903557410728e-14
            } else if o1 {
                0.1748931621698149538e-07
            } else if o2 {
                -0.7456258884965764992e+4
            } else {
                -0.2014717999760347811e+5
            },
        ).mul_add(
            u.0,
            if o0 {
                -0.2783485786333451745e-13
            } else if o1 {
                -0.9956602606623249195e-07
            } else if o2 {
                0.9553977358167021521e+4
            } else {
                0.1554006970967118286e+5
            },
        ).mul_add(
            u.0,
            if o0 {
                0.4463221276786415752e-12
            } else if o1 {
                0.4330010240640327080e-06
            } else if o2 {
                -0.9470019905444229153e+4
            } else {
                -0.6150874190563554293e+4
            },
        ).mul_add(
            u.0,
            if o0 {
                -0.6711366622850136563e-11
            } else if o1 {
                -0.1435050600991763331e-05
            } else if o2 {
                0.7387344321849855078e+4
            } else {
                0.1240047765634815732e+4
            },
        ).mul_add(
            u.0,
            if o0 {
                0.9422759050232662223e-10
            } else if o1 {
                0.3460139479650695662e-05
            } else if o2 {
                -0.4557713054166382790e+4
            } else {
                -0.8210325475752699731e+2
            },
        ).mul_add(
            u.0,
            if o0 {
                -0.1229055530100229098e-08
            } else if o1 {
                -0.4988908180632898173e-05
            } else if o2 {
                0.2207866967354055305e+4
            } else {
                0.3242443880839930870e+2
            },
        ).mul_add(
            u.0,
            if o0 {
                0.1480719281585086512e-07
            } else if o1 {
                -0.1308775976326352012e-05
            } else if o2 {
                -0.8217975658621754746e+3
            } else {
                -0.2923418863833160586e+2
            },
        ).mul_add(
            u.0,
            if o0 {
                -0.1636584469123399803e-06
            } else if o1 {
                0.2825086540850310103e-04
            } else if o2 {
                0.2268659483507917400e+3
            } else {
                0.3457461732814383071e+0
            },
        ).mul_add(
            u.0,
            if o0 {
                0.1646211436588923575e-05
            } else if o1 {
                -0.6393913713069986071e-04
            } else if o2 {
                -0.4633361260318560682e+2
            } else {
                0.5489730155952392998e+1
            },
        ).mul_add(
            u.0,
            if o0 {
                -0.1492565035840623511e-04
            } else if o1 {
                -0.2566436514695078926e-04
            } else if o2 {
                0.9557380123733945965e+1
            } else {
                0.1559934132251294134e-2
            },
        ).mul_add(
            u.0,
            if o0 {
                0.1205533298178967851e-03
            } else if o1 {
                0.5895792375659440364e-03
            } else if o2 {
                -0.2958429331939661289e+1
            } else {
                -0.1541741566831520638e+1
            },
        ).mul_add(
            u.0,
            if o0 {
                -0.8548327023450850081e-03
            } else if o1 {
                -0.1695715579163588598e-02
            } else if o2 {
                0.1670329508092765480e+0
            } else {
                0.2823152230558364186e-5
            },
        ).mul_add(
            u.0,
            if o0 {
                0.5223977625442187932e-02
            } else if o1 {
                0.2089116434918055149e-03
            } else if o2 {
                0.6096615680115419211e+0
            } else {
                0.6249999184195342838e+0
            },
        ).mul_add(
            u.0,
            if o0 {
                -0.2686617064513125222e-01
            } else if o1 {
                0.1912855949584917753e-01
            } else if o2 {
                0.1059212443193543585e-2
            } else {
                0.1741749416408701288e-8
            },
        );

        let mut d = u * t;
        d += if o0 {
            dd(0.11283791670955126141, -4.0175691625932118483e-18)
        } else if o1 {
            dd(-0.10277263343147646779, -6.2338714083404900225e-18)
        } else if o2 {
            dd(-0.50005180473999022439, 2.6362140569041995803e-17)
        } else {
            dd(-0.5000000000258444377, -4.0074044712386992281e-17)
        };
        d *= u;
        d += if o0 {
            dd(-0.37612638903183753802, 1.3391897206042552387e-17)
        } else if o1 {
            dd(-0.63661976742916359662, 7.6321019159085724662e-18)
        } else if o2 {
            dd(1.601106273924963368e-06, 1.1974001857764476775e-23)
        } else {
            dd(2.3761973137523364792e-13, -1.1670076950531026582e-29)
        };
        d *= u;
        d += if o0 {
            dd(1.1283791670955125586, 1.5335459613165822674e-17)
        } else if o1 {
            dd(-1.1283791674717296161, 8.0896847755965377194e-17)
        } else if o2 {
            dd(-0.57236496645145429341, 3.0704553245872027258e-17)
        } else {
            dd(-0.57236494292470108114, -2.3984352208056898003e-17)
        };

        let mut x = (if o1 { d } else { dd(-a, 0.) }) * a;
        x = if o1 { x } else { x + d };
        x = if o0 {
            dd(1., 0.).sub_checked(x)
        } else {
            super::expk2(x)
        };
        x = if o1 { x } else { x * u };

        let mut r = if o3 { x.0 + x.1 } else { 0. };
        if s < 0. {
            r = 2. - r;
        }
        if s.isnan() {
            SLEEF_NAN
        } else {
            r
        }
    }

}

pub mod u05 {
    use common::*;
    use f2::*;

    pub fn xsincospi(d: f64) -> (f64, f64) {
        let u = d * 4.;
        let q = super::ceilk(u) & !1_isize;

        let s = u - (q as f64);
        let t = s;
        let s = s * s;
        let s2 = t.mul_as_f2(t);

        //

        let u = (-2.02461120785182399295868e-14)
            .mul_add(s, 6.94821830580179461327784e-12)
            .mul_add(s, -1.75724749952853179952664e-09)
            .mul_add(s, 3.13361688966868392878422e-07)
            .mul_add(s, -3.6576204182161551920361e-05)
            .mul_add(s, 0.00249039457019271850274356);
        let mut x = u * s + dd(-0.0807455121882807852484731, 3.61852475067037104849987e-18);
        x = s2 * x + dd(0.785398163397448278999491, 3.06287113727155002607105e-17);

        x *= t;
        let mut rsin = x.0 + x.1;

        if xisnegzero(d) {
            rsin = -0.;
        }

        //

        let u = 9.94480387626843774090208e-16
            .mul_add(s, -3.89796226062932799164047e-13)
            .mul_add(s, 1.15011582539996035266901e-10)
            .mul_add(s, -2.4611369501044697495359e-08)
            .mul_add(s, 3.59086044859052754005062e-06)
            .mul_add(s, -0.000325991886927389905997954);
        x = u * s + dd(0.0158543442438155018914259, -1.04693272280631521908845e-18);
        x = s2 * x + dd(-0.308425137534042437259529, -1.95698492133633550338345e-17);

        x = x * s2 + 1.;
        let mut rcos = x.0 + x.1;

        //

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

        if fabsk(d) > TRIGRANGEMAX3 / 4. {
            rsin = 0.;
            rcos = 1.;
        }
        if d.isinf() {
            rsin = SLEEF_NAN;
            rcos = SLEEF_NAN;
        }

        (rsin, rcos)
    }

    pub fn xsinpi(d: f64) -> f64 {
        let x = super::sinpik(d);
        if d.isinf() {
            SLEEF_NAN
        } else if fabsk(d) > TRIGRANGEMAX3 / 4. {
            0.
        } else if xisnegzero(d) {
            -0.
        } else {
            x.0 + x.1
        }
    }

    pub fn xcospi(d: f64) -> f64 {
        let x = super::cospik(d);

        if d.isinf() {
            SLEEF_NAN
        } else if fabsk(d) > TRIGRANGEMAX3 / 4. {
            1.
        } else {
            x.0 + x.1
        }
    }

    pub fn xsqrt(mut d: f64) -> f64 {
        let mut q = 0.5;

        d = if d < 0. { SLEEF_NAN } else { d };

        if d < 8.636168555094445E-78 {
            d *= 1.157920892373162E77;
            q = 2.9387358770557188E-39 * 0.5;
        }

        if d > 1.3407807929942597e+154 {
            d *= 7.4583407312002070e-155;
            q = 1.1579208923731620e+77 * 0.5;
        }

        // http://en.wikipedia.org/wiki/Fast_inverse_square_root
        let mut x =
            long_bits_to_double(0x5fe6ec85e7de30da - (double_to_raw_long_bits(d + 1e-320) >> 1));

        x = x * (1.5 - 0.5 * d * x * x);
        x = x * (1.5 - 0.5 * d * x * x);
        x = x * (1.5 - 0.5 * d * x * x) * d;

        let d2 = (d + x.mul_as_f2(x)) * x.recpre();

        let ret = (d2.0 + d2.1) * q;

        let ret = if d == SLEEF_INFINITY {
            SLEEF_INFINITY
        } else {
            ret
        };
        if d == 0. {
            d
        } else {
            ret
        }
    }

    pub fn xhypot(mut x: f64, mut y: f64) -> f64 {
        x = fabsk(x);
        y = fabsk(y);
        let min = super::fmink(x, y);
        let mut n = min;
        let max = super::fmaxk(x, y);
        let mut d = max;

        if max < f64::MIN {
            n *= D1_54;
            d *= D1_54;
        }
        let mut t = dd(n, 0.) / dd(d, 0.);
        t = (t.square() + 1.).sqrt() * max;
        let ret = t.0 + t.1;

        if (x == SLEEF_INFINITY) || (y == SLEEF_INFINITY) {
            SLEEF_INFINITY
        } else if x.isnan() || y.isnan() {
            SLEEF_NAN
        } else if min == 0. {
            max
        } else if ret.isnan() {
            SLEEF_INFINITY
        } else {
            ret
        }
    }

}

pub mod u35 {
    use common::*;

    pub fn xsincospi(d: f64) -> (f64, f64) {
        let u = d * 4.;
        let q = super::ceilk(u) & !1_isize;

        let s = u - (q as f64);
        let t = s;
        let s = s * s;

        //

        let u = 0.6880638894766060136e-11
            .mul_add(s, -0.1757159564542310199e-8)
            .mul_add(s, 0.3133616327257867311e-6)
            .mul_add(s, -0.3657620416388486452e-4)
            .mul_add(s, 0.2490394570189932103e-2)
            .mul_add(s, -0.8074551218828056320e-1)
            .mul_add(s, 0.7853981633974482790e+0);

        let mut rsin = u * t;

        //

        let u = (-0.3860141213683794352e-12)
            .mul_add(s, 0.1150057888029681415e-9)
            .mul_add(s, -0.2461136493006663553e-7)
            .mul_add(s, 0.3590860446623516713e-5)
            .mul_add(s, -0.3259918869269435942e-3)
            .mul_add(s, 0.1585434424381541169e-1)
            .mul_add(s, -0.3084251375340424373e+0)
            .mul_add(s, 1.);

        let mut rcos = u;

        //

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

        if fabsk(d) > TRIGRANGEMAX3 / 4. {
            rsin = 0.;
            rcos = 1.;
        }
        if d.isinf() {
            rsin = SLEEF_NAN;
            rcos = SLEEF_NAN;
        }

        (rsin, rcos)
    }

    pub fn xsinh(x: f64) -> f64 {
        let e = super::expm1k(fabsk(x));
        let mut y = (e + 2.) / (e + 1.) * (0.5 * e);

        y = if fabsk(x) > 709. { SLEEF_INFINITY } else { y };
        y = if y.isnan() { SLEEF_INFINITY } else { y };
        y = mulsign(y, x);
        if x.isnan() {
            SLEEF_NAN
        } else {
            y
        }
    }

    pub fn xcosh(x: f64) -> f64 {
        let e = super::xexp(fabsk(x));
        let mut y = 0.5 / e + 0.5 * e;

        y = if fabsk(x) > 709. { SLEEF_INFINITY } else { y };
        y = if y.isnan() { SLEEF_INFINITY } else { y };
        if x.isnan() {
            SLEEF_NAN
        } else {
            y
        }
    }

    pub fn xtanh(x: f64) -> f64 {
        let mut y = fabsk(x);
        let d = super::expm1k(2. * y);
        y = d / (d + 2.);

        y = if fabsk(x) > 18.714973875 { 1. } else { y };
        y = if y.isnan() { 1. } else { y };
        y = mulsign(y, x);
        if x.isnan() {
            SLEEF_NAN
        } else {
            y
        }
    }

    pub fn xsqrt(d: f64) -> f64 {
        super::u05::xsqrt(d)
    }

    pub fn xhypot(mut x: f64, mut y: f64) -> f64 {
        x = fabsk(x);
        y = fabsk(y);
        let min = super::fmink(x, y);
        let max = super::fmaxk(x, y);

        let t = min / max;
        if (x == SLEEF_INFINITY) || (y == SLEEF_INFINITY) {
            SLEEF_INFINITY
        } else if x.isnan() || y.isnan() {
            SLEEF_NAN
        } else if min == 0. {
            max
        } else {
            max * SQRT(1. + t * t)
        }
    }

}
