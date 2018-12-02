#![allow(dead_code)]

use crate::common::*;
use doubled::*;

use core::f32;
use core::i32;

#[inline]
pub fn df(h: f32, l: f32) -> Doubled<f32> {
    Doubled::new(h, l)
}

pub mod u05;
pub mod u10;
pub mod u15;
pub mod u35;

#[inline]
pub fn xisintf(x: f32) -> bool {
    x == (x as i32 as f32)
}

#[inline]
pub fn xisnegzerof(x: f32) -> bool {
    x.to_bits() == (-0f32).to_bits()
}

#[inline]
pub fn mulsignf(x: f32, y: f32) -> f32 {
    f32::from_bits(x.to_bits() ^ (y.to_bits() & (1 << 31)))
}

#[inline]
pub fn copysignfk(x: f32, y: f32) -> f32 {
    f32::from_bits((x.to_bits() & !(1 << 31)) ^ (y.to_bits() & (1 << 31)))
}

#[inline]
pub fn signf(d: f32) -> f32 {
    mulsignf(1., d)
}

#[inline]
fn fabsfk(x: f32) -> f32 {
    f32::from_bits(0x7fffffff & x.to_bits())
}

#[inline]
fn rintfk(x: f32) -> f32 {
    (if x < 0. { (x - 0.5) } else { (x + 0.5) }) as i32 as f32
}

#[inline]
fn ceilfk(x: f32) -> i32 {
    (x as i32) + (if x < 0. { 0 } else { 1 })
}

#[inline]
fn fminfk(x: f32, y: f32) -> f32 {
    if x < y {
        x
    } else {
        y
    }
}

#[inline]
fn fmaxfk(x: f32, y: f32) -> f32 {
    if x > y {
        x
    } else {
        y
    }
}

#[inline]
fn ilogbkf(mut d: f32) -> i32 {
    let m = d < 5.421010862427522E-20;
    d = if m { 1.8446744073709552E19 * d } else { d };
    let q = ((d.to_bits() >> 23) & 0xff) as i32;
    if m {
        q - (64 + 0x7f)
    } else {
        q - 0x7f
    }
}

// vilogb2kf is similar to ilogbkf, but the argument has to be a
// normalized FP value.
#[inline]
fn ilogb2kf(d: f32) -> i32 {
    ((d.to_bits() >> 23) & 0xff) as i32 - 0x7f
}

#[inline]
fn pow2if(q: i32) -> f32 {
    f32::from_bits(((q + 0x7f) as u32) << 23)
}

#[inline]
fn ldexpkf(mut x: f32, mut q: i32) -> f32 {
    let mut m = q >> 31;
    m = (((m + q) >> 6) - m) << 4;
    q = q - (m << 2);
    m += 127;
    m = if m < 0 { 0 } else { m };
    m = if m > 255 { 255 } else { m };
    let mut u = f32::from_bits((m as u32) << 23);
    x = x * u * u * u * u;
    u = f32::from_bits(((q + 0x7f) as u32) << 23);
    x * u
}

#[inline]
fn ldexp2kf(d: f32, e: i32) -> f32 {
    // faster than ldexpkf, short reach
    d * pow2if(e >> 1) * pow2if(e - (e >> 1))
}

#[inline]
fn ldexp3kf(d: f32, e: i32) -> f32 {
    // very fast, no denormal
    f32::from_bits(d.to_bits() + (e << 23) as u32)
}

fn rempisubf(x: f32) -> (f32, i32) {
    let mut fr = x - F1_10 * ((x * (1. / F1_10)) as i32 as f32);
    let mut reti = ((7 & ((if x > 0. { 4 } else { 3 }) + ((fr * 8.) as i32))) - 3) >> 1;
    fr = fr - 0.25 * ((fr * 4. + mulsignf(0.5, x)) as i32 as f32);
    fr = if fabsfk(fr) > 0.125 {
        fr - mulsignf(0.5, x)
    } else {
        fr
    };
    fr = if fabsfk(fr) > 1e+10 { 0. } else { fr };
    if fabsfk(x) == 0.12499999254941940308 {
        fr = x;
        reti = 0;
    }
    (fr, reti)
}

fn rempif(a: f32) -> (Doubled<f32>, i32) {
    let mut ex = ilogb2kf(a) - 25;
    let mut q = if ex > (90 - 25) { -64 } else { 0 };
    let a = ldexp3kf(a, q);
    if ex < 0 {
        ex = 0;
    }
    let ex = (ex * 4) as usize;
    let mut x = a.mul_as_doubled(REMPITABSP[ex]);
    let (did, dii) = rempisubf(x.0);
    q = dii;
    x.0 = did;
    x = x.normalize();
    let mut y = a.mul_as_doubled(REMPITABSP[ex + 1]);
    x += y;
    let (did, dii) = rempisubf(x.0);
    q += dii;
    x.0 = did;
    x = x.normalize();
    y = df(REMPITABSP[ex + 2], REMPITABSP[ex + 3]) * a;
    x += y;
    x = x.normalize();
    x *= df(3.1415927410125732422 * 2., -8.7422776573475857731e-08 * 2.);
    (if fabsfk(a) < 0.7 { df(a, 0.) } else { x }, q)
}

#[inline]
fn atan2kf(mut y: f32, mut x: f32) -> f32 {
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
    let mut t = s * s;

    let u = 0.00282363896258175373077393_f32
        .mul_add(t, -0.0159569028764963150024414)
        .mul_add(t, 0.0425049886107444763183594)
        .mul_add(t, -0.0748900920152664184570312)
        .mul_add(t, 0.106347933411598205566406)
        .mul_add(t, -0.142027363181114196777344)
        .mul_add(t, 0.199926957488059997558594)
        .mul_add(t, -0.333331018686294555664062);

    t = u * t * s + s;
    (q as f32) * M_PI_2_F + t
}

#[inline]
fn expkf(d: Doubled<f32>) -> f32 {
    let qf = rintfk((d.0 + d.1) * R_LN2_F);

    let q = qf as i32;
    let mut s = d + qf * -L2U_F;
    s += qf * -L2L_F;

    s = s.normalize();

    let u = 0.00136324646882712841033936_f32
        .mul_add(s.0, 0.00836596917361021041870117)
        .mul_add(s.0, 0.0416710823774337768554688)
        .mul_add(s.0, 0.166665524244308471679688)
        .mul_add(s.0, 0.499999850988388061523438);

    let mut t = s.add_checked(s.square() * u);

    t = (1.).add_checked(t);

    if d.0 < -104. {
        0.
    } else {
        ldexpkf(t.0 + t.1, q)
    }
}

#[inline]
fn expm1kf(d: f32) -> f32 {
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
    u = s * s * u + s;

    if q != 0 {
        ldexp2kf(u + 1., q) - 1.
    } else {
        u
    }
}

#[inline]
fn logkf(mut d: f32) -> Doubled<f32> {
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
    let x2 = x.square();

    let t = 0.240320354700088500976562_f32
        .mul_add(x2.0, 0.285112679004669189453125)
        .mul_add(x2.0, 0.400007992982864379882812);
    let c = df(0.66666662693023681640625, 3.69183861259614332084311e-09);

    (df(0.69314718246459960938, -1.904654323148236017e-09) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * (x2 * t + c))
}

#[inline]
fn expk2f(d: Doubled<f32>) -> Doubled<f32> {
    let qf = rintfk((d.0 + d.1) * R_LN2_F);

    let q = qf as i32;
    let mut s = d + qf * -L2U_F;
    s += qf * -L2L_F;

    let u = 0.1980960224e-3_f32
        .mul_add(s.0, 0.1394256484e-2)
        .mul_add(s.0, 0.8333456703e-2)
        .mul_add(s.0, 0.4166637361e-1);

    let mut t = s * u + 0.166666659414234244790680580464e+0;
    t = s * t + 0.5;
    t = s + s.square() * t;

    t = 1. + t;

    t.0 = ldexp2kf(t.0, q);
    t.1 = ldexp2kf(t.1, q);

    if d.0 < -104. {
        df(0., 0.)
    } else {
        t
    }
}

#[inline]
fn logk2f(d: Doubled<f32>) -> Doubled<f32> {
    let e = ilogbkf(d.0 * (1. / 0.75));
    let m = d.scale(pow2if(-e));

    let x = (m + (-1.)) / (m + 1.);
    let x2 = x.square();

    let t = 0.2392828464508056640625_f32
        .mul_add(x2.0, 0.28518211841583251953125)
        .mul_add(x2.0, 0.400005877017974853515625)
        .mul_add(x2.0, 0.666666686534881591796875);

    (df(0.69314718246459960938, -1.904654323148236017e-09) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * t)
}

#[inline]
fn toward0f(d: f32) -> f32 {
    if d == 0. {
        0.
    } else {
        f32::from_bits(d.to_bits() - 1)
    }
}

#[inline]
fn ptruncf(x: f32) -> f32 {
    if fabsfk(x) >= F1_23 {
        x
    } else {
        (x - (x - (x as i32 as f32)))
    }
}

#[inline]
fn sinpifk(d: f32) -> Doubled<f32> {
    let u = d * 4.;
    let q = ceilfk(u) & !1;
    let o = (q & 2) != 0;

    let mut s = u - (q as f32);
    let t = s;
    s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (if o {
        -0.2430611801e-7_f32
    } else {
        0.3093842054e-6
    })
    .mul_add(s, if o { 0.3590577080e-5 } else { -0.3657307388e-4 })
    .mul_add(s, if o { -0.3259917721e-3 } else { 0.2490393585e-2 });
    let mut x = u * s
        + (if o {
            df(0.015854343771934509277, 4.4940051354032242811e-10)
        } else {
            df(-0.080745510756969451904, -1.3373665339076936258e-09)
        });
    x = s2 * x
        + (if o {
            df(-0.30842512845993041992, -9.0728339030733922277e-09)
        } else {
            df(0.78539818525314331055, -2.1857338617566484855e-08)
        });

    x *= if o { s2 } else { df(t, 0.) };
    x = if o { x + 1. } else { x };

    //

    if (q & 4) != 0 {
        x = -x;
    }
    x
}

#[inline]
fn cospifk(d: f32) -> Doubled<f32> {
    let u = d * 4.;
    let q = ceilfk(u) & !1;
    let o = (q & 2) == 0;

    let mut s = u - (q as f32);
    let t = s;
    s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (if o {
        -0.2430611801e-7_f32
    } else {
        0.3093842054e-6
    })
    .mul_add(s, if o { 0.3590577080e-5 } else { -0.3657307388e-4 })
    .mul_add(s, if o { -0.3259917721e-3 } else { 0.2490393585e-2 });
    let mut x = u * s
        + (if o {
            df(0.015854343771934509277, 4.4940051354032242811e-10)
        } else {
            df(-0.080745510756969451904, -1.3373665339076936258e-09)
        });
    x = s2 * x
        + (if o {
            df(-0.30842512845993041992, -9.0728339030733922277e-09)
        } else {
            df(0.78539818525314331055, -2.1857338617566484855e-08)
        });

    x *= if o { s2 } else { df(t, 0.) };
    x = if o { x + 1. } else { x };

    //

    if ((q + 2) & 4) != 0 {
        x = -x;
    }
    x
}

fn gammafk(a: f32) -> (Doubled<f32>, Doubled<f32>) {
    let otiny = fabsfk(a) < 1e-30;
    let oref = a < 0.5;

    let mut x = if otiny {
        df(0., 0.)
    } else if oref {
        (1.).add_as_doubled(-a)
    } else {
        df(a, 0.)
    };

    let o0 = (0.5 <= x.0) && (x.0 <= 1.2);
    let o2 = 2.3 < x.0;

    let mut y = ((x + 1.) * x).normalize();
    y = ((x + 2.) * y).normalize();

    let mut clln = if o2 && (x.0 <= 7.) { y } else { df(1., 0.) };

    x = if o2 && (x.0 <= 7.) { x + 3. } else { x };
    let t = if o2 {
        1. / x.0
    } else {
        (x + (if o0 { -1. } else { -2. })).normalize().0
    };

    let u = (if o2 {
        0.000839498720672087279971000786_f32
    } else if o0 {
        0.9435157776e+0
    } else {
        0.1102489550e-3
    })
    .mul_add(
        t,
        if o2 {
            -5.17179090826059219329394422e-05
        } else if o0 {
            0.8670063615e+0
        } else {
            0.8160019934e-4
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000592166437353693882857342347
        } else if o0 {
            0.4826702476e+0
        } else {
            0.1528468856e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            6.97281375836585777403743539e-05
        } else if o0 {
            -0.8855129778e-1
        } else {
            -0.2355068718e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            0.000784039221720066627493314301
        } else if o0 {
            0.1013825238e+0
        } else {
            0.4962242092e-3
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.000229472093621399176949318732
        } else if o0 {
            -0.1493408978e+0
        } else {
            -0.1193488017e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            -0.002681327160493827160473958490
        } else if o0 {
            0.1697509140e+0
        } else {
            0.2891599433e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            0.003472222222222222222175164840
        } else if o0 {
            -0.2072454542e+0
        } else {
            -0.7385451812e-2
        },
    )
    .mul_add(
        t,
        if o2 {
            0.083333333333333333335592087900
        } else if o0 {
            0.2705872357e+0
        } else {
            0.2058077045e-1
        },
    );

    y = (x + (-0.5)) * logk2f(x);
    y += -x;
    y += df(0.91893853320467278056, 0.); // 0.5*log(2*M_PI)

    let mut z = u.mul_as_doubled(t)
        + (if o0 {
            -0.400686534596170958447352690395e+0
        } else {
            -0.673523028297382446749257758235e-1
        });
    z = z * t
        + (if o0 {
            0.822466960142643054450325495997e+0
        } else {
            0.322467033928981157743538726901e+0
        });
    z = z * t
        + (if o0 {
            -0.577215665946766039837398973297e+0
        } else {
            0.422784335087484338986941629852e+0
        });
    z *= t;

    let mut clc = if o2 { y } else { z };

    let mut clld = if o2 {
        u.mul_as_doubled(t) + 1.
    } else {
        df(1., 0.)
    };

    y = clln;

    clc = if otiny {
        df(41.58883083359671856503, 0.) // log(2^60)
    } else if oref {
        df(1.1447298858494001639, 0.) + (-clc)
    } else {
        clc
    }; // log(M_PI)
    clln = if otiny {
        df(1., 0.)
    } else if oref {
        clln
    } else {
        clld
    };

    if oref {
        x = clld * sinpifk(a - F1_12 * ((a * (1. / F1_12)) as i32 as f32));
    }

    clld = if otiny {
        df(a * (F1_30 * F1_30), 0.)
    } else if oref {
        x
    } else {
        y
    };

    (clc, clln / clld)
}

/// Integer exponent of an FP number
pub fn ilogbf(d: f32) -> i32 {
    let mut e = ilogbkf(fabsfk(d));
    e = if d == 0. { SLEEF_FP_ILOGB0 } else { e };
    e = if d.is_nan() { SLEEF_FP_ILOGBNAN } else { e };
    if d.is_infinite() {
        i32::MAX
    } else {
        e
    }
}

/// Absolute value
pub fn fabsf(x: f32) -> f32 {
    fabsfk(x)
}

/// Copy sign of a number
pub fn copysignf(x: f32, y: f32) -> f32 {
    copysignfk(x, y)
}

/// Maximum of two numbers
pub fn fmaxf(x: f32, y: f32) -> f32 {
    if y != y {
        x
    } else if x > y {
        x
    } else {
        y
    }
}

/// Minimum of two numbers
pub fn fminf(x: f32, y: f32) -> f32 {
    if y != y {
        x
    } else if x < y {
        x
    } else {
        y
    }
}

/// Positive difference
pub fn fdimf(x: f32, y: f32) -> f32 {
    let ret = x - y;
    if (ret < 0.) || (x == y) {
        0.
    } else {
        ret
    }
}

/// Round to integer towards zero
pub fn truncf(x: f32) -> f32 {
    let fr = x - (x as i32 as f32);
    if x.is_infinite() || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

/// Round to integer towards minus infinity
pub fn floorf(x: f32) -> f32 {
    let mut fr = x - (x as i32 as f32);
    fr = if fr < 0. { fr + 1. } else { fr };
    if x.is_infinite() || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

/// Round to integer towards plus infinity
pub fn ceilf(x: f32) -> f32 {
    let mut fr = x - (x as i32 as f32);
    fr = if fr <= 0. { fr } else { fr - 1. };
    if x.is_infinite() || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

/// Round to integer away from zero
pub fn roundf(d: f32) -> f32 {
    let mut x = d + 0.5;
    let mut fr = x - (x as i32 as f32);
    if (fr == 0.) && (x <= 0.) {
        x -= 1.
    };
    fr = if fr < 0. { fr + 1. } else { fr };
    x = if d == 0.4999999701976776123 { 0. } else { x }; // nextafterf(0.5, 0)
    if d.is_infinite() || (fabsfk(d) >= F1_23) {
        d
    } else {
        copysignfk(x - fr, d)
    }
}

/// Round to integer, ties round to even
pub fn rintf(d: f32) -> f32 {
    let mut x = d + 0.5;
    let isodd = (1 & (x as i32)) != 0;
    let mut fr = x - (x as i32 as f32);
    fr = if (fr < 0.) || ((fr == 0.) && isodd) {
        fr + 1.
    } else {
        fr
    };
    x = if d == 0.50000005960464477539 { 0. } else { x }; // nextafterf(0.5, 1)
    if d.is_infinite() || (fabsfk(d) >= F1_23) {
        d
    } else {
        copysignfk(x - fr, d)
    }
}

/// Integral and fractional value of FP number
pub fn modff(x: f32) -> Doubled<f32> {
    let mut fr = x - (x as i32 as f32);
    fr = if fabsfk(x) > F1_23 { 0. } else { fr };
    Doubled::new(copysignfk(fr, x), copysignfk(x - fr, x))
}

/// Multiply by integral power of 2
///
/// These functions return the result of multiplying ***m*** by 2 raised to the power ***x***.
pub fn ldexpf(x: f32, mut exp: i32) -> f32 {
    if exp > 300 {
        exp = 300;
    }
    if exp < -300 {
        exp = -300;
    }

    let mut e0 = exp >> 2;
    if exp < 0 {
        e0 += 1;
    }
    if (-50 < exp) && (exp < 50) {
        e0 = 0;
    }
    let e1 = exp - (e0 << 2);

    let p = pow2if(e0);
    x * pow2if(e1) * p * p * p * p
}

/// Find the next representable FP value
pub fn nextafterf(x: f32, y: f32) -> f32 {
    let mut cxi = (if x == 0. { mulsignf(0., y) } else { x }).to_bits() as i32;
    let c = (cxi < 0) == (y < x);
    if c {
        cxi = -(cxi ^ (1 << 31));
    }

    if x != y {
        cxi -= 1;
    }

    if c {
        cxi = -(cxi ^ (1 << 31));
    }

    let cxf = f32::from_bits(cxi as u32);

    if x.is_nan() || y.is_nan() {
        f32::NAN
    } else if (x == 0.) && (y == 0.) {
        y
    } else if (cxf == 0.) && (x != 0.) {
        mulsignf(0., x)
    } else {
        cxf
    }
}

/// Fractional component of an FP number
pub fn frfrexpf(mut x: f32) -> f32 {
    if fabsfk(x) < f32::MIN {
        x *= F1_30;
    }

    let mut cxu = x.to_bits();
    cxu &= !0x7f800000u32;
    cxu |= 0x3f000000u32;

    if x == 0. {
        x
    } else if x.is_infinite() {
        mulsignf(f32::INFINITY, x)
    } else {
        f32::from_bits(cxu)
    }
}

/// Exponent of an FP number
pub fn expfrexpf(mut x: f32) -> i32 {
    let mut ret = 0;

    if fabsfk(x) < f32::MIN {
        x *= F1_30;
        ret = -30;
    }

    ret += (((x.to_bits() >> 23) & 0xff) as i32) - 0x7e;

    if (x == 0.) || x.is_nan() || x.is_infinite() {
        0
    } else {
        ret
    }
}

/// FP remainder
pub fn fmodf(x: f32, y: f32) -> f32 {
    let mut nu = fabsfk(x);
    let mut de = fabsfk(y);
    let mut s = 1.;
    if de < f32::MIN {
        nu *= F1_25;
        de *= F1_25;
        s = 1. / F1_25;
    }
    let mut r = df(nu, 0.);
    let rde = toward0f(1. / de);

    for _ in 0..8 {
        // ceil(log2(FLT_MAX) / 22)+1
        let q = if (de + de > r.0) && (r.0 >= de) {
            1.
        } else {
            (toward0f(r.0) * rde)
        };
        r = (r + ptruncf(q).mul_as_doubled(-de)).normalize();
        if r.0 < de {
            break;
        }
    }

    let mut ret = (r.0 + r.1) * s;
    if r.0 + r.1 == de {
        ret = 0.;
    }
    ret = mulsignf(ret, x);
    if de == 0. {
        f32::NAN
    } else if nu < de {
        x
    } else {
        ret
    }
}

/// Fused multiply and accumulate
///
/// This function compute (***x*** × ***y*** + ***z***) without rounding, and then return the rounded value of the result.
/// This function may return infinity with a correct sign if the absolute value of the correct return value is greater than 1e+33.
/// The error bounds of the returned value is max(0.50001 ULP, f32::MIN).
pub fn fmaf(mut x: f32, mut y: f32, mut z: f32) -> f32 {
    const C0: f32 = F1_25;
    const C1: f32 = C0 * C0;
    const C2: f32 = C1 * C1;

    let mut h2 = x * y + z;
    let mut q = 1.;
    if fabsfk(h2) < 1e-38 {
        x *= C1;
        y *= C1;
        z *= C2;
        q = 1. / C2;
    }
    if fabsfk(h2) > 1e+38 {
        x *= 1. / C1;
        y *= 1. / C1;
        z *= 1. / C2;
        q = C2;
    }
    let mut d = x.mul_as_doubled(y);
    d += z;
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

/*
pub fn xsqrtf(d: f32) -> f32 {
    SQRTF(d)
}
*/
