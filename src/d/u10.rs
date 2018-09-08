use super::*;

pub fn acos(d: f64) -> f64 {
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

pub fn atan(d: f64) -> f64 {
    let d2 = atan2k_u1(dd(fabsk(d), 0.), dd(1., 0.));
    let mut r = d2.0 + d2.1;
    if d.isinf() {
        r = 1.570796326794896557998982;
    }
    mulsign(r, d)
}

fn atan2k_u1(mut y: F2<f64>, mut x: F2<f64>) -> F2<f64> {
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

pub fn atan2(mut y: f64, mut x: f64) -> f64 {
    if fabsk(x) < 5.5626846462680083984e-309 {
        y *= D1_53;
        x *= D1_53;
    } // nexttoward((1.0 / DBL_MAX), 1)
    let d = atan2k_u1(dd(fabsk(y), 0.), dd(x, 0.));
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

pub fn asin(d: f64) -> f64 {
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

pub fn sin(d: f64) -> f64 {
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

pub fn cos(d: f64) -> f64 {
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

pub fn sincos(d: f64) -> (f64, f64) {
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

pub fn tan(d: f64) -> f64 {
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

pub fn cbrt(d: f64) -> f64 {
    let mut q2 = dd(1., 0.);

    let e = ilogbk(fabsk(d)) + 1;
    let d = ldexp2k(d, -e);
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
        ldexp2k(v.0 + v.1, (e + 6144) / 3 - 2048)
    }
}

pub fn tgamma(a: f64) -> f64 {
    let (da, db) = gammak(a);
    let y = expk2(da) * db;
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

pub fn lgamma(a: f64) -> f64 {
    let (da, db) = gammak(a);
    let y = da + logk2(db.abs());
    let r = y.0 + y.1;
    if a.isinf() || ((a <= 0.) && xisint(a)) || (xisnumber(a) && r.isnan()) {
        SLEEF_INFINITY
    } else {
        r
    }
}

pub fn erf(a: f64) -> f64 {
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
        (1.).add_checked(-expk2(d))
    };
    if a.isnan() {
        SLEEF_NAN
    } else {
        mulsign(if o2 { d.0 + d.1 } else { 1. }, s)
    }
}

pub fn exp(d: f64) -> f64 {
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

pub fn pow(x: f64, y: f64) -> f64 {
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

pub fn sinh(x: f64) -> f64 {
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

pub fn cosh(x: f64) -> f64 {
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

pub fn asinh(x: f64) -> f64 {
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

pub fn acosh(x: f64) -> f64 {
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

pub fn atanh(x: f64) -> f64 {
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

pub fn exp2(d: f64) -> f64 {
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

pub fn exp10(d: f64) -> f64 {
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

pub fn expm1(a: f64) -> f64 {
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

pub fn log2(mut d: f64) -> f64 {
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

pub fn log1p(d: f64) -> f64 {
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
