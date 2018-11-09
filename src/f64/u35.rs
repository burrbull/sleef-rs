//! Functions with 3.5 ULP error bound

use super::*;

pub fn sincospi(d: f64) -> (f64, f64) {
    let u = d * 4.;
    let q = ceilk(u) & !1_isize;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;

    //

    let u = 0.6880638894766060136e-11_f64
        .mul_add(s, -0.1757159564542310199e-8)
        .mul_add(s, 0.3133616327257867311e-6)
        .mul_add(s, -0.3657620416388486452e-4)
        .mul_add(s, 0.2490394570189932103e-2)
        .mul_add(s, -0.8074551218828056320e-1)
        .mul_add(s, 0.7853981633974482790e+0);

    let mut rsin = u * t;

    //

    let u = (-0.3860141213683794352e-12_f64)
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

pub fn sinh(x: f64) -> f64 {
    let e = expm1k(fabsk(x));
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

pub fn cosh(x: f64) -> f64 {
    let e = u10::exp(fabsk(x));
    let mut y = 0.5 / e + 0.5 * e;

    y = if fabsk(x) > 709. { SLEEF_INFINITY } else { y };
    y = if y.isnan() { SLEEF_INFINITY } else { y };
    if x.isnan() {
        SLEEF_NAN
    } else {
        y
    }
}

pub fn tanh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let d = expm1k(2. * y);
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

pub fn sqrt(d: f64) -> f64 {
    u05::sqrt(d)
}

pub fn hypot(mut x: f64, mut y: f64) -> f64 {
    x = fabsk(x);
    y = fabsk(y);
    let min = fmink(x, y);
    let max = fmaxk(x, y);

    let t = min / max;
    if (x == SLEEF_INFINITY) || (y == SLEEF_INFINITY) {
        SLEEF_INFINITY
    } else if x.isnan() || y.isnan() {
        SLEEF_NAN
    } else if min == 0. {
        max
    } else {
        max * (1. + t * t).sqrt()
    }
}

pub fn atan2(y: f64, x: f64) -> f64 {
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

pub fn asin(d: f64) -> f64 {
    let o = fabsk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
    let x = if o { fabsk(d) } else { x2.sqrt() };

    let u = 0.3161587650653934628e-1_f64
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

pub fn acos(d: f64) -> f64 {
    let o = fabsk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsk(d)) * 0.5 };
    let mut x = if o { fabsk(d) } else { x2.sqrt() };
    x = if fabsk(d) == 1. { 0. } else { x };

    let u = 0.3161587650653934628e-1_f64
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

pub fn atan(mut s: f64) -> f64 {
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

    let u = (-1.88796008463073496563746e-05_f64)
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

pub fn sin(mut d: f64) -> f64 {
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

    let u = (-7.97255955009037868891952e-18_f64)
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

pub fn cos(mut d: f64) -> f64 {
    let t = d;
    let ql: isize;

    if fabsk(d) < TRIGRANGEMAX2 {
        let qlf = (2_f64).mul_add(rintk(d * M_1_PI - 0.5), 1.);
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

    let u = (-7.97255955009037868891952e-18_f64)
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

pub fn sincos(d: f64) -> (f64, f64) {
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

    let u = 1.58938307283228937328511e-10_f64
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

    let u = (-1.13615350239097429531523e-11_f64)
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

pub fn tan(d: f64) -> f64 {
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

    let mut u = 9.99583485362149960784268e-06_f64
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

pub fn log(mut d: f64) -> f64 {
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

    let t = 0.153487338491425068243146_f64
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

pub fn cbrt(mut d: f64) -> f64 {
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

    let mut x = (-0.640245898480692909870982_f64)
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
