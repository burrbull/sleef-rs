#![allow(dead_code)]
use core::f64;
use core::f64::consts::{FRAC_1_PI, FRAC_2_PI, FRAC_PI_2, FRAC_PI_4, PI};
use core::isize;

use crate::common::*;
use doubled::*;

const D1_63: f64 = (1u64 << 63) as f64;
const D1_60: f64 = (1u64 << 60) as f64;
const D1_54: f64 = (1u64 << 54) as f64;
const D1_53: f64 = (1u64 << 53) as f64;
const D1_52: f64 = (1u64 << 52) as f64;
const D1_32: f64 = (1u64 << 32) as f64;
const D1_31: f64 = (1u64 << 31) as f64;
const D1_28: f64 = (1u64 << 28) as f64;
const D1_24: f64 = (1u64 << 24) as f64;
const D1_23: f64 = (1u64 << 23) as f64;

#[inline]
pub fn dd(h: f64, l: f64) -> Doubled<f64> {
    Doubled::new(h, l)
}

pub mod u05;
pub mod u10;
pub mod u15;
pub mod u35;

#[inline]
pub fn xisnegzero(x: f64) -> bool {
    x.to_bits() == (-0_f64).to_bits()
}

#[inline]
pub fn xisint(d: f64) -> bool {
    let x = d - D1_31 * ((d * (1. / D1_31)) as i64 as f64);
    (x == x as i64 as f64) || (fabsk(d) >= D1_53)
}

#[inline]
pub fn xisodd(d: f64) -> bool {
    let x = d - D1_31 * ((d * (1. / D1_31)) as i64 as f64);
    ((1 & (x as i64)) != 0) && (fabsk(d) < D1_53)
}

#[inline]
pub fn mulsign(x: f64, y: f64) -> f64 {
    f64::from_bits(x.to_bits() ^ (y.to_bits() & (1 << 63)))
}

#[inline]
pub fn copysignk(x: f64, y: f64) -> f64 {
    f64::from_bits((x.to_bits() & !(1 << 63)) ^ (y.to_bits() & (1 << 63)))
}

#[inline]
pub fn sign(d: f64) -> f64 {
    mulsign(1., d)
}

#[inline]
fn fabsk(x: f64) -> f64 {
    f64::from_bits(0x7fffffffffffffff & x.to_bits())
}

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
    f64::from_bits(((q + 0x3ff) as u64) << 52)
}

#[inline]
fn ldexpk(x: f64, mut q: isize) -> f64 {
    let mut m = q >> 31;
    m = (((m + q) >> 9) - m) << 7;
    q = q - (m << 2);
    m += 0x3ff;
    m = if m < 0 { 0 } else { m };
    m = if m > 0x7ff { 0x7ff } else { m };
    let u = f64::from_bits((m as u64) << 52);
    let x = x * u * u * u * u;
    let u = f64::from_bits(((q + 0x3ff) as u64) << 52);
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
    f64::from_bits(d.to_bits() + ((e as u64) << 52))
}

#[inline]
fn ilogbk(mut d: f64) -> isize {
    let m = d < 4.9090934652977266E-91;
    d = if m { 2.037035976334486E90 * d } else { d };
    let q = (d.to_bits() >> 52) & 0x7ff;
    (if m { q - (300 + 0x03ff) } else { q - 0x03ff }) as isize
}

// ilogb2k is similar to ilogbk, but the argument has to be a
// normalized FP value.
#[inline]
fn ilogb2k(d: f64) -> isize {
    (((d.to_bits() >> 52) & 0x7ff) - 0x3ff) as isize
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

    let t = u * t * s + s;
    (q as f64) * f64::consts::FRAC_PI_2 + t
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
fn rempi(a: f64) -> (Doubled<f64>, i32) {
    let mut ex = ilogb2k(a) - 55;
    let q = if ex > (700 - 55) { -64 } else { 0 };
    let a = ldexp3k(a, q);
    if ex < 0 {
        ex = 0;
    }
    let ex = (ex * 4) as usize;
    let mut x = a.mul_as_doubled(REMPITABDP[ex]);
    let (did, dii) = rempisub(x.0);
    let mut q = dii;
    x.0 = did;
    x = x.normalize();
    let mut y = a.mul_as_doubled(REMPITABDP[ex + 1]);
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

#[inline]
fn sinpik(d: f64) -> Doubled<f64> {
    let u = d * 4.;
    let q = ceilk(u) & !1;
    let o = (q & 2) != 0;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (if o {
        9.94480387626843774090208e-16_f64
    } else {
        -2.02461120785182399295868e-14
    })
    .mul_add(
        s,
        if o {
            -3.89796226062932799164047e-13
        } else {
            6.94821830580179461327784e-12
        },
    )
    .mul_add(
        s,
        if o {
            1.15011582539996035266901e-10
        } else {
            -1.75724749952853179952664e-09
        },
    )
    .mul_add(
        s,
        if o {
            -2.4611369501044697495359e-08
        } else {
            3.13361688966868392878422e-07
        },
    )
    .mul_add(
        s,
        if o {
            3.59086044859052754005062e-06
        } else {
            -3.6576204182161551920361e-05
        },
    )
    .mul_add(
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
fn cospik(d: f64) -> Doubled<f64> {
    let u = d * 4.;
    let q = ceilk(u) & !1;
    let o = (q & 2) == 0;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (if o {
        9.94480387626843774090208e-16_f64
    } else {
        -2.02461120785182399295868e-14
    })
    .mul_add(
        s,
        if o {
            -3.89796226062932799164047e-13
        } else {
            6.94821830580179461327784e-12
        },
    )
    .mul_add(
        s,
        if o {
            1.15011582539996035266901e-10
        } else {
            -1.75724749952853179952664e-09
        },
    )
    .mul_add(
        s,
        if o {
            -2.4611369501044697495359e-08
        } else {
            3.13361688966868392878422e-07
        },
    )
    .mul_add(
        s,
        if o {
            3.59086044859052754005062e-06
        } else {
            -3.6576204182161551920361e-05
        },
    )
    .mul_add(
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

#[inline]
fn expm1k(d: f64) -> f64 {
    let q = rintk(d * R_LN2);

    let s = q.mul_add(-L2U, d);
    let s = q.mul_add(-L2L, s);

    let mut u = 2.08860621107283687536341e-09_f64
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
fn logk(mut d: f64) -> Doubled<f64> {
    let o = d < f64::MIN;
    if o {
        d *= D1_32 * D1_32
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.square();

    let t = 0.116255524079935043668677_f64
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
fn expk(d: Doubled<f64>) -> f64 {
    let q = rintk((d.0 + d.1) * R_LN2);

    let s = d + q * (-L2U) + q * (-L2L);

    let s = s.normalize();

    let u = 2.51069683420950419527139e-08_f64
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

#[inline]
fn expk2(d: Doubled<f64>) -> Doubled<f64> {
    let qf = rintk((d.0 + d.1) * R_LN2);
    let q = qf as isize;

    let s = d + qf * (-L2U) + qf * (-L2L);

    let u = 0.1602472219709932072e-9_f64
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

    t = Doubled::new(ldexp2k(t.0, q), ldexp2k(t.1, q));

    if d.0 < -1000. {
        dd(0., 0.)
    } else {
        t
    }
}

#[inline]
fn logk2(d: Doubled<f64>) -> Doubled<f64> {
    let e = ilogbk(d.0 * (1. / 0.75));

    let m = Doubled::new(ldexp2k(d.0, -e), ldexp2k(d.1, -e));

    let x = (m + (-1.)) / (m + 1.);
    let x2 = x.square();

    let t = 0.13860436390467167910856_f64
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

#[inline]
fn toward0(d: f64) -> f64 {
    if d == 0. {
        0.
    } else {
        f64::from_bits(d.to_bits() - 1)
    }
}

#[inline]
fn removelsb(d: f64) -> f64 {
    f64::from_bits(d.to_bits() & 0xfffffffffffffffe)
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

fn gammak(a: f64) -> (Doubled<f64>, Doubled<f64>) {
    let mut clln = dd(1., 0.);
    let mut clld = dd(1., 0.);

    let otiny = fabsk(a) < 1e-306;
    let oref = a < 0.5;

    let mut x = if otiny {
        dd(0., 0.)
    } else if oref {
        (1.).add_as_doubled(-a)
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
        -156.801412704022726379848862_f64
    } else if o0 {
        0.2947916772827614196e+2
    } else {
        0.7074816000864609279e-7
    })
    .mul_add(
        t,
        if o2 {
            1.120804464289911606838558160000
        } else if o0 {
            0.1281459691827820109e+3
        } else {
            0.4009244333008730443e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            13.39798545514258921833306020000
        } else if o0 {
            0.2617544025784515043e+3
        } else {
            0.1040114641628246946e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.116546276599463200848033357000
        } else if o0 {
            0.3287022855685790432e+3
        } else {
            0.1508349150733329167e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            -1.391801093265337481495562410000
        } else if o0 {
            0.2818145867730348186e+3
        } else {
            0.1288143074933901020e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            0.015056113040026424412918973400
        } else if o0 {
            0.1728670414673559605e+3
        } else {
            0.4744167749884993937e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            0.179540117061234856098844714000
        } else if o0 {
            0.7748735764030416817e+2
        } else {
            -0.6554816306542489902e-7
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.002481743600264997730942489280
        } else if o0 {
            0.2512856643080930752e+2
        } else {
            -0.3189252471452599844e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.029527880945699120504851034100
        } else if o0 {
            0.5766792106140076868e+1
        } else {
            0.1358883821470355377e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            0.000540164767892604515196325186
        } else if o0 {
            0.7270275473996180571e+0
        } else {
            -0.4343931277157336040e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            0.006403362833808069794787256200
        } else if o0 {
            0.8396709124579147809e-1
        } else {
            0.9724785897406779555e-6
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000162516262783915816896611252
        } else if o0 {
            -0.8211558669746804595e-1
        } else {
            -0.2036886057225966011e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.001914438498565477526465972390
        } else if o0 {
            0.6828831828341884458e-1
        } else {
            0.4373363141819725815e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            7.20489541602001055898311517e-05
        } else if o0 {
            -0.7712481339961671511e-1
        } else {
            -0.9439951268304008677e-5
        },
    )
    .mul_add(
        t,
        if o2 {
            0.000839498720672087279971000786
        } else if o0 {
            0.8337492023017314957e-1
        } else {
            0.2050727030376389804e-4
        },
    )
    .mul_add(
        t,
        if o2 {
            -5.17179090826059219329394422e-05
        } else if o0 {
            -0.9094964931456242518e-1
        } else {
            -0.4492620183431184018e-4
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000592166437353693882857342347
        } else if o0 {
            0.1000996313575929358e+0
        } else {
            0.9945751236071875931e-4
        },
    )
    .mul_add(
        t,
        if o2 {
            6.97281375836585777403743539e-05
        } else if o0 {
            -0.1113342861544207724e+0
        } else {
            -0.2231547599034983196e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            0.000784039221720066627493314301
        } else if o0 {
            0.1255096673213020875e+0
        } else {
            0.5096695247101967622e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000229472093621399176949318732
        } else if o0 {
            -0.1440498967843054368e+0
        } else {
            -0.1192753911667886971e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.002681327160493827160473958490
        } else if o0 {
            0.1695571770041949811e+0
        } else {
            0.2890510330742210310e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            0.003472222222222222222175164840
        } else if o0 {
            -0.2073855510284092762e+0
        } else {
            -0.7385551028674461858e-2
        },
    )
    .mul_add(
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

    let mut z = u.mul_as_doubled(t)
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

    clld = if o2 { u.mul_as_doubled(t) + 1. } else { clld };

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

pub fn ldexp(x: f64, mut exp: isize) -> f64 {
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

pub fn ilogb(d: f64) -> isize {
    let mut e = ilogbk(fabsk(d));
    e = if d == 0. { SLEEF_FP_ILOGB0 as isize } else { e };
    e = if d.is_nan() {
        SLEEF_FP_ILOGBNAN as isize
    } else {
        e
    };
    if d.is_infinite() {
        isize::MAX
    } else {
        e
    }
}

pub fn tanh(x: f64) -> f64 {
    let mut y = fabsk(x);
    let mut d = expk2(dd(y, 0.));
    let e = d.recpre();
    d = d.sub_checked(e) / d.add_checked(e);
    y = d.0 + d.1;

    y = if fabsk(x) > 18.714973875 { 1. } else { y };
    y = if y.is_nan() { 1. } else { y };
    y = mulsign(y, x);
    if x.is_nan() {
        f64::NAN
    } else {
        y
    }
}

pub fn log10(mut d: f64) -> f64 {
    let o = d < f64::MIN;
    if o {
        d *= D1_32 * D1_32;
    }

    let mut e = ilogb2k(d * (1. / 0.75));
    let m = ldexp3k(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_doubled(m) / (1.).add_as_doubled(m);
    let x2 = x.0 * x.0;

    let t = 0.6653725819576758460e-1_f64
        .mul_add(x2, 0.6625722782820833712e-1)
        .mul_add(x2, 0.7898105214313944078e-1)
        .mul_add(x2, 0.9650955035715275132e-1)
        .mul_add(x2, 0.1240841409721444993e+0)
        .mul_add(x2, 0.1737177927454605086e+0)
        .mul_add(x2, 0.2895296546021972617e+0);

    let s = (dd(0.30102999566398119802, -2.803728127785170339e-18) * (e as f64))
        .add_checked(x * dd(0.86858896380650363334, 1.1430059694096389311e-17))
        .add_checked(x2 * x.0 * t);

    if d.is_infinite() {
        f64::INFINITY
    } else if (d < 0.) || d.is_nan() {
        f64::NAN
    } else if d == 0. {
        f64::NEG_INFINITY
    } else {
        s.0 + s.1
    }
}

pub fn fma(mut x: f64, mut y: f64, mut z: f64) -> f64 {
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
    let d = x.mul_as_doubled(y) + z;
    let ret = if (x == 0.) || (y == 0.) { z } else { d.0 + d.1 };
    if z.is_infinite() && !x.is_infinite() && !x.is_nan() && !y.is_infinite() && !y.is_nan() {
        h2 = z;
    }
    if h2.is_infinite() || h2.is_nan() {
        h2
    } else {
        ret * q
    }
}

pub fn fabs(x: f64) -> f64 {
    fabsk(x)
}

pub fn copysign(x: f64, y: f64) -> f64 {
    copysignk(x, y)
}

pub fn fmax(x: f64, y: f64) -> f64 {
    if y != y {
        x
    } else if x > y {
        x
    } else {
        y
    }
}

pub fn fmin(x: f64, y: f64) -> f64 {
    if y != y {
        x
    } else if x < y {
        x
    } else {
        y
    }
}

pub fn fdim(x: f64, y: f64) -> f64 {
    let ret = x - y;
    if (ret < 0.) || (x == y) {
        0.
    } else {
        ret
    }
}

pub fn trunc(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

pub fn floor(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    fr = if fr < 0. { fr + 1. } else { fr };
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

pub fn ceil(x: f64) -> f64 {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    fr = if fr <= 0. { fr } else { fr - 1. };
    if x.is_infinite() || (fabsk(x) >= D1_52) {
        x
    } else {
        copysignk(x - fr, x)
    }
}

pub fn round(d: f64) -> f64 {
    let mut x = d + 0.5;
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    if (fr == 0.) && (x <= 0.) {
        x -= 1.;
    }
    fr = if fr < 0. { fr + 1. } else { fr };
    let x = if d == 0.49999999999999994449 { 0. } else { x }; // nextafter(0.5, 0)
    if d.is_infinite() || (fabsk(d) >= D1_52) {
        d
    } else {
        copysignk(x - fr, d)
    }
}

pub fn rint(d: f64) -> f64 {
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
    if d.is_infinite() || (fabsk(d) >= D1_52) {
        d
    } else {
        copysignk(x - fr, d)
    }
}

pub fn nextafter(x: f64, y: f64) -> f64 {
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
    if x.is_nan() || y.is_nan() {
        f64::NAN
    } else if (x == 0.) && (y == 0.) {
        y
    } else if (cxf == 0.) && (x != 0.) {
        mulsign(0., x)
    } else {
        cxf
    }
}

pub fn frfrexp(mut x: f64) -> f64 {
    if fabsk(x) < f64::MIN {
        x *= D1_63;
    }

    let mut cxu = x.to_bits();
    cxu &= !0x7ff0000000000000u64;
    cxu |= 0x3fe0000000000000u64;

    if x == 0. {
        x
    } else if x.is_infinite() {
        mulsign(f64::INFINITY, x)
    } else {
        f64::from_bits(cxu)
    }
}

pub fn expfrexp(mut x: f64) -> i32 {
    let mut ret = 0;

    if fabsk(x) < f64::MIN {
        x *= D1_63;
        ret = -63;
    }

    let cxu = x.to_bits();
    ret += (((cxu >> 52) & 0x7ff) as i32) - 0x3fe;

    if x == 0. || x.is_nan() || x.is_infinite() {
        0
    } else {
        ret
    }
}

pub fn fmod(x: f64, y: f64) -> f64 {
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
        r = (r + removelsb(ptrunc(q)).mul_as_doubled(-de)).normalize();
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
        f64::NAN
    } else if nu < de {
        x
    } else {
        ret
    }
}

pub fn modf(x: f64) -> Doubled<f64> {
    let mut fr = x - D1_31 * ((x * (1. / D1_31)) as i32 as f64);
    fr = fr - (fr as i32 as f64);
    fr = if fabsk(x) >= D1_52 { 0. } else { fr };
    Doubled::new(copysignk(fr, x), copysignk(x - fr, x))
}

/*
pub fn xsqrt(d: f64) -> f64 {
    SQRT(d)
}
*/
