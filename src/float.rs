#![allow(dead_code)]

use super::common::*;
use super::consts::*;


#[inline]
fn rintfk(x: f32) -> f32 {
    (if x < 0. {
        (x - 0.5)
    } else {
        (x + 0.5)
    }) as i32 as f32
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
    let q = (float_to_raw_int_bits(d) >> 23) & 0xff;
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
    ((float_to_raw_int_bits(d) >> 23) & 0xff) - 0x7f
}

pub fn xilogbf(d: f32) -> i32 {
    let mut e = ilogbkf(fabsfk(d));
    e = if d == 0. { SLEEF_FP_ILOGB0 } else { e };
    e = if xisnanf(d) { SLEEF_FP_ILOGBNAN } else { e };
    if xisinff(d) {
        i32::MAX
    } else {
        e
    }
}

#[inline]
fn pow2if(q: i32) -> f32 {
    int_bits_to_float(((q + 0x7f) as i32) << 23)
}

#[inline]
fn ldexpkf(mut x: f32, mut q: i32) -> f32 {
    let mut m = q >> 31;
    m = (((m + q) >> 6) - m) << 4;
    q = q - (m << 2);
    m += 127;
    m = if m < 0 { 0 } else { m };
    m = if m > 255 { 255 } else { m };
    let mut u = int_bits_to_float((m as i32) << 23);
    x = x * u * u * u * u;
    u = int_bits_to_float(((q + 0x7f) as i32) << 23);
    x * u
}

#[inline]
fn ldexp2kf(d: f32, e: i32) -> f32 {
    // faster than ldexpkf, short reach
    return d * pow2if(e >> 1) * pow2if(e - (e >> 1));
}

#[inline]
fn ldexp3kf(d: f32, e: i32) -> f32 {
    // very fast, no denormal
    return int_bits_to_float(float_to_raw_int_bits(d) + (e << 23));
}

//

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

fn rempif(a: f32) -> (f32n2, i32) {
    let mut ex = ilogb2kf(a) - 25;
    let mut q = if ex > (90 - 25) { -64 } else { 0 };
    let a = ldexp3kf(a, q);
    if ex < 0 {
        ex = 0;
    }
    let ex = (ex * 4) as usize;
    let mut x = a.mul_as_f2(REMPITABSP[ex]);
    let (did, dii) = rempisubf(x.0);
    q = dii;
    x.0 = did;
    x = x.normalize();
    let mut y = a.mul_as_f2(REMPITABSP[ex + 1]);
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

pub fn xsinf(mut d: f32) -> f32 {
    let q: i32;
    let t = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * M_1_PI_F);
        q = qf as i32;
        d = mlaf(qf, -PI_A2_F, d);
        d = mlaf(qf, -PI_B2_F, d);
        d = mlaf(qf, -PI_C2_F, d);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * M_1_PI_F);
        q = qf as i32;
        d = mlaf(qf, -PI_A_F, d);
        d = mlaf(qf, -PI_B_F, d);
        d = mlaf(qf, -PI_C_F, d);
        d = mlaf(qf, -PI_D_F, d);
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
        if xisinff(t) || xisnanf(t) {
            d = SLEEF_NAN_F;
        }
    }

    let s = d * d;

    if (q & 1) != 0 {
        d = -d;
    }

    let u = 2.6083159809786593541503e-06
        .mla(s, -0.0001981069071916863322258)
        .mla(s, 0.00833307858556509017944336)
        .mla(s, -0.166666597127914428710938);

    if xisnegzerof(t) {
        -0.
    } else {
        mlaf(s, u * d, d)
    }
}

pub fn xcosf(mut d: f32) -> f32 {
    let q: i32;
    let t = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        q = 1 + 2 * (rintfk(d * M_1_PI_F - 0.5) as i32);
        let qf = q as f32;
        d = mlaf(qf, -PI_A2_F * 0.5, d);
        d = mlaf(qf, -PI_B2_F * 0.5, d);
        d = mlaf(qf, -PI_C2_F * 0.5, d);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        q = 1 + 2 * (rintfk(d * M_1_PI_F - 0.5) as i32);
        let qf = q as f32;
        d = mlaf(qf, -PI_A_F * 0.5, d);
        d = mlaf(qf, -PI_B_F * 0.5, d);
        d = mlaf(qf, -PI_C_F * 0.5, d);
        d = mlaf(qf, -PI_D_F * 0.5, d);
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
        if xisinff(t) || xisnanf(t) {
            d = SLEEF_NAN_F;
        }
    }

    let s = d * d;

    if (q & 2) == 0 {
        d = -d;
    }

    let u = 2.6083159809786593541503e-06
        .mla(s, -0.0001981069071916863322258)
        .mla(s, 0.00833307858556509017944336)
        .mla(s, -0.166666597127914428710938);

    mlaf(s, u * d, d)
}

pub fn xsincosf(d: f32) -> (f32, f32) {
    let q: i32;

    let mut s = d;

    if fabsfk(d) < TRIGRANGEMAX2_F {
        let qf = rintfk(d * M_2_PI_F);
        q = qf as i32;
        s = mlaf(qf, -PI_A2_F * 0.5, s);
        s = mlaf(qf, -PI_B2_F * 0.5, s);
        s = mlaf(qf, -PI_C2_F * 0.5, s);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * M_2_PI_F);
        q = qf as i32;
        s = mlaf(qf, -PI_A_F * 0.5, s);
        s = mlaf(qf, -PI_B_F * 0.5, s);
        s = mlaf(qf, -PI_C_F * 0.5, s);
        s = mlaf(qf, -PI_D_F * 0.5, s);
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        s = dfidf.0 + dfidf.1;
        if xisinff(d) || xisnanf(d) {
            s = SLEEF_NAN_F;
        }
    }

    let t = s;

    s = s * s;

    let mut u = (-0.000195169282960705459117889)
        .mla(s, 0.00833215750753879547119141)
        .mla(s, -0.166666537523269653320312);
    u = u * s * t;

    let mut rsin = t + u;

    if xisnegzerof(d) {
        rsin = -0.;
    }

    u = (-2.71811842367242206819355e-07)
        .mla(s, 2.47990446951007470488548e-05)
        .mla(s, -0.00138888787478208541870117)
        .mla(s, 0.0416666641831398010253906)
        .mla(s, -0.5);

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

pub fn xtanf(d: f32) -> f32 {
    let q;

    let mut x = d;

    if fabsfk(d) < TRIGRANGEMAX2_F * 0.5 {
        let qf = rintfk(d * M_2_PI_F);
        q = qf as i32;
        x = mlaf(qf, -PI_A2_F * 0.5, x);
        x = mlaf(qf, -PI_B2_F * 0.5, x);
        x = mlaf(qf, -PI_C2_F * 0.5, x);
    } else if fabsfk(d) < TRIGRANGEMAX_F {
        let qf = rintfk(d * M_2_PI_F);
        q = qf as i32;
        x = mlaf(qf, -PI_A_F * 0.5, x);
        x = mlaf(qf, -PI_B_F * 0.5, x);
        x = mlaf(qf, -PI_C_F * 0.5, x);
        x = mlaf(qf, -PI_D_F * 0.5, x);
    } else {
        let (dfidf, dfii) = rempif(d);
        q = dfii;
        x = dfidf.0 + dfidf.1;
        if xisinff(d) || xisnanf(d) {
            x = SLEEF_NAN_F;
        }
    }

    let s = x * x;

    if (q & 1) != 0 {
        x = -x;
    }

    let mut u = 0.00927245803177356719970703
        .mla(s, 0.00331984995864331722259521)
        .mla(s, 0.0242998078465461730957031)
        .mla(s, 0.0534495301544666290283203)
        .mla(s, 0.133383005857467651367188)
        .mla(s, 0.333331853151321411132812);

    u = mlaf(s, u * x, x);

    if (q & 1) != 0 {
        1. / u
    } else {
        u
    }
}

pub fn xatanf(mut s: f32) -> f32 {
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

    let u = 0.00282363896258175373077393
        .mla(t, -0.0159569028764963150024414)
        .mla(t, 0.0425049886107444763183594)
        .mla(t, -0.0748900920152664184570312)
        .mla(t, 0.106347933411598205566406)
        .mla(t, -0.142027363181114196777344)
        .mla(t, 0.199926957488059997558594)
        .mla(t, -0.333331018686294555664062);

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

    let u = 0.00282363896258175373077393
        .mla(t, -0.0159569028764963150024414)
        .mla(t, 0.0425049886107444763183594)
        .mla(t, -0.0748900920152664184570312)
        .mla(t, 0.106347933411598205566406)
        .mla(t, -0.142027363181114196777344)
        .mla(t, 0.199926957488059997558594)
        .mla(t, -0.333331018686294555664062);

    t = u * t * s + s;
    (q as f32) * M_PI_2_F + t
}

pub fn xatan2f(y: f32, x: f32) -> f32 {
    let mut r = atan2kf(fabsfk(y), x);

    r = if xisinff(x) || (x == 0.) {
        M_PI_2_F - (if xisinff(x) { signf(x) * M_PI_2_F } else { 0. })
    } else if xisinff(y) {
        M_PI_2_F - (if xisinff(x) { signf(x) * M_PI_4_F } else { 0. })
    } else if y == 0. {
        (if signf(x) == -1. { M_PI_F } else { 0. })
    } else {
        mulsignf(r, x)
    };

    if xisnanf(x) || xisnanf(y) {
        SLEEF_NAN_F
    } else {
        mulsignf(r, y)
    }
}

pub fn xasinf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let x = if o { fabsfk(d) } else { SQRTF(x2) };

    let u = 0.4197454825e-1
        .mla(x2, 0.2424046025e-1)
        .mla(x2, 0.4547423869e-1)
        .mla(x2, 0.7495029271e-1)
        .mla(x2, 0.1666677296e+0)
        .mla(x * x2, x);

    let r = if o { u } else { (M_PI_F / 2. - 2. * u) };
    mulsignf(r, d)
}

pub fn xacosf(d: f32) -> f32 {
    let o = fabsfk(d) < 0.5;
    let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
    let mut x = if o { fabsfk(d) } else { SQRTF(x2) };
    x = if fabsfk(d) == 1. { 0. } else { x };

    let mut u = 0.4197454825e-1
        .mla(x2, 0.2424046025e-1)
        .mla(x2, 0.4547423869e-1)
        .mla(x2, 0.7495029271e-1)
        .mla(x2, 0.1666677296e+0);

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

pub fn xlogf(mut d: f32) -> f32 {
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

    let t = 0.2392828464508056640625
        .mla(x2, 0.28518211841583251953125)
        .mla(x2, 0.400005877017974853515625)
        .mla(x2, 0.666666686534881591796875)
        .mla(x2, 2.);

    if d == 0. {
        -SLEEF_INFINITY_F
    } else if (d < 0.) || xisnanf(d) {
        SLEEF_NAN_F
    } else if xisinff(d) {
        SLEEF_INFINITY_F
    } else {
        x * t + 0.693147180559945286226764 * (e as f32)
    }
}

pub fn xexpf(d: f32) -> f32 {
    let qf = rintfk(d * R_LN2_F);
    let q = qf as i32;
    let s = mlaf(qf, -L2U_F, d);
    let s = mlaf(qf, -L2L_F, s);

    let mut u = 0.000198527617612853646278381
        .mla(s, 0.00139304355252534151077271)
        .mla(s, 0.00833336077630519866943359)
        .mla(s, 0.0416664853692054748535156)
        .mla(s, 0.166666671633720397949219)
        .mla(s, 0.5);

    u = s * s * u + s + 1.;
    u = ldexp2kf(u, q);

    if d < -104. {
        0.
    } else if d > 104. {
        SLEEF_INFINITY_F
    } else {
        ldexp2kf(u, q)
    }
}

#[inline]
fn expkf(d: f32n2) -> f32 {
    let qf = rintfk((d.0 + d.1) * R_LN2_F);

    let q = qf as i32;
    let mut s = d + qf * -L2U_F;
    s += qf * -L2L_F;

    s = s.normalize();

    let u = 0.00136324646882712841033936
        .mla(s.0, 0.00836596917361021041870117)
        .mla(s.0, 0.0416710823774337768554688)
        .mla(s.0, 0.166665524244308471679688)
        .mla(s.0, 0.499999850988388061523438);

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
    let s = mlaf(qf, -L2U_F, d);
    let s = mlaf(qf, -L2L_F, s);

    let mut u = 0.000198527617612853646278381
        .mla(s, 0.00139304355252534151077271)
        .mla(s, 0.00833336077630519866943359)
        .mla(s, 0.0416664853692054748535156)
        .mla(s, 0.166666671633720397949219)
        .mla(s, 0.5);
    u = s * s * u + s;

    if q != 0 {
        ldexp2kf(u + 1., q) - 1.
    } else {
        u
    }
}

#[inline]
fn logkf(mut d: f32) -> f32n2 {
    let o = d < f32::MIN;
    if o {
        d *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(d * (1. / 0.75));
    let m = ldexp3kf(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_f2(m) / (1.).add_as_f2(m);
    let x2 = x.square();

    let t = 0.240320354700088500976562
        .mla(x2.0, 0.285112679004669189453125)
        .mla(x2.0, 0.400007992982864379882812);
    let c = df(0.66666662693023681640625, 3.69183861259614332084311e-09);

    (df(0.69314718246459960938, -1.904654323148236017e-09) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * (x2 * t + c))
}

#[inline]
fn expk2f(d: f32n2) -> f32n2 {
    let qf = rintfk((d.0 + d.1) * R_LN2_F);

    let q = qf as i32;
    let mut s = d + qf * -L2U_F;
    s += qf * -L2L_F;

    let u = 0.1980960224e-3
        .mla(s.0, 0.1394256484e-2)
        .mla(s.0, 0.8333456703e-2)
        .mla(s.0, 0.4166637361e-1);

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

pub fn xpowf(x: f32, y: f32) -> f32 {
    let yisint = (y == (y as i32 as f32)) || (fabsfk(y) >= F1_24);
    let yisodd = ((1 & (y as i32)) != 0) && yisint && (fabsfk(y) < F1_24);

    let mut result = expkf(logkf(fabsfk(x)) * y);

    result = if xisnanf(result) {
        SLEEF_INFINITY_F
    } else {
        result
    };
    result *= if x >= 0. {
        1.
    } else if !yisint {
        SLEEF_NAN_F
    } else if yisodd {
        -1.
    } else {
        1.
    };

    let efx = mulsignf(fabsfk(x) - 1., y);
    if (y == 0.) || (x == 1.) {
        1.
    } else if xisnanf(x) || xisnanf(y) {
        SLEEF_NAN_F
    } else if xisinff(x) || (x == 0.) {
        (if yisodd { signf(x) } else { 1. })
            * (if (if x == 0. { -y } else { y }) < 0. {
                0.
            } else {
                SLEEF_INFINITY_F
            })
    } else if xisinff(y) {
        if efx < 0. {
            0.
        } else if efx == 0. {
            1.
        } else {
            SLEEF_INFINITY_F
        }
    } else {
        result
    }
}

pub fn xsinhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    d = d.sub_checked(d.rec());
    y = (d.0 + d.1) * 0.5;

    y = if fabsfk(x) > 89. { SLEEF_INFINITY_F } else { y };
    y = if xisnanf(y) { SLEEF_INFINITY_F } else { y };
    y = mulsignf(y, x);
    if xisnanf(x) {
        SLEEF_NAN_F
    } else {
        y
    }
}

pub fn xcoshf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    d = d.add_checked(d.rec());
    y = (d.0 + d.1) * 0.5;

    y = if fabsfk(x) > 89. { SLEEF_INFINITY_F } else { y };
    y = if xisnanf(y) { SLEEF_INFINITY_F } else { y };
    if xisnanf(x) {
        SLEEF_NAN_F
    } else {
        y
    }
}

pub fn xtanhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let mut d = expk2f(df(y, 0.));
    let e = d.rec();
    d = d.sub_checked(e) / d.add_checked(e);
    y = d.0 + d.1;

    y = if fabsfk(x) > 18.714973875 { 1. } else { y };
    y = if xisnanf(y) { 1. } else { y };
    y = mulsignf(y, x);
    if xisnanf(x) {
        SLEEF_NAN_F
    } else {
        y
    }
}

#[inline]
fn logk2f(d: f32n2) -> f32n2 {
    let e = ilogbkf(d.0 * (1. / 0.75));
    let m = d.scale(pow2if(-e));

    let x = (m + (-1.)) / (m + 1.);
    let x2 = x.square();

    let t = 0.2392828464508056640625
        .mla(x2.0, 0.28518211841583251953125)
        .mla(x2.0, 0.400005877017974853515625)
        .mla(x2.0, 0.666666686534881591796875);

    (df(0.69314718246459960938, -1.904654323148236017e-09) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x * t)
}

pub fn xasinhf(x: f32) -> f32 {
    let mut y = fabsfk(x);

    let mut d = if y > 1. { x.rec() } else { df(y, 0.) };
    d = (d.square() + 1.).sqrt();
    d = if y > 1. { d * y } else { d };

    d = logk2f(d.add_checked(x).normalize());
    y = d.0 + d.1;

    y = if fabsfk(x) > SQRT_FLT_MAX || xisnanf(y) {
        mulsignf(SLEEF_INFINITY_F, x)
    } else {
        y
    };
    y = if xisnanf(x) { SLEEF_NAN_F } else { y };
    if xisnegzerof(x) {
        -0.
    } else {
        y
    }
}

pub fn xacoshf(x: f32) -> f32 {
    let d = logk2f((x.add_as_f2(1.)).sqrt() * (x.add_as_f2(-1.)).sqrt() + x);
    let mut y = d.0 + d.1;

    y = if (x > SQRT_FLT_MAX) || xisnanf(y) {
        SLEEF_INFINITY_F
    } else {
        y
    };
    y = if x == 1. { 0. } else { y };
    y = if x < 1. { SLEEF_NAN_F } else { y };
    if xisnanf(x) {
        SLEEF_NAN_F
    } else {
        y
    }
}

pub fn xatanhf(x: f32) -> f32 {
    let mut y = fabsfk(x);
    let d = logk2f((1.).add_as_f2(y) / (1.).add_as_f2(-y));
    y = if y > 1. {
        SLEEF_NAN_F
    } else if y == 1. {
        SLEEF_INFINITY_F
    } else {
        (d.0 + d.1) * 0.5
    };

    y = if xisinff(x) || xisnanf(y) {
        SLEEF_NAN_F
    } else {
        y
    };
    y = mulsignf(y, x);
    if xisnanf(x) {
        SLEEF_NAN_F
    } else {
        y
    }
}

pub fn xexp2f(d: f32) -> f32 {
    let qf = rintfk(d);
    let q = qf as i32;
    let s = d - qf;

    let mut u = 0.1535920892e-3
        .mla(s, 0.1339262701e-2)
        .mla(s, 0.9618384764e-2)
        .mla(s, 0.5550347269e-1)
        .mla(s, 0.2402264476e+0)
        .mla(s, 0.6931471825e+0);
    u = (1.).add_checked(u.mul_as_f2(s)).normalize().0;

    if d >= 128. {
        SLEEF_INFINITY_F
    } else if d < -150. {
        0.
    } else {
        ldexp2kf(u, q)
    }
}

pub fn xexp10f(d: f32) -> f32 {
    let qf = rintfk(d * (LOG10_2 as f32));

    let q = qf as i32;
    let s = mlaf(qf, -L10U_F, d);
    let s = mlaf(qf, -L10L_F, s);

    let mut u = 0.2064004987e+0
        .mla(s, 0.5417877436e+0)
        .mla(s, 0.1171286821e+1)
        .mla(s, 0.2034656048e+1)
        .mla(s, 0.2650948763e+1)
        .mla(s, 0.2302585125e+1);
    u = (1.).add_checked(u.mul_as_f2(s)).normalize().0;

    if d > 38.5318394191036238941387 {
        SLEEF_INFINITY_F // log10(FLT_MAX)
    } else if d < -50. {
        0.
    } else {
        ldexp2kf(u, q)
    }
}

pub fn xexpm1f(a: f32) -> f32 {
    let d = expk2f(df(a, 0.)) + (-1.);
    if xisnegzerof(a) {
        -0.
    } else if a < -16.635532333438687426013570 {
        -1.
    } else if a > 88.72283172607421875 {
        SLEEF_INFINITY_F
    } else {
        d.0 + d.1
    }
}

pub fn xlog10f(mut d: f32) -> f32 {
    let o = d < f32::MIN;
    if o {
        d *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(d * (1. / 0.75));
    let m = ldexp3kf(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_f2(m) / (1.).add_as_f2(m);
    let x2 = x.0 * x.0;

    let t = 0.1314289868e+0
        .mla(x2, 0.1735493541e+0)
        .mla(x2, 0.2895309627e+0);

    let s = df(0.30103001, -1.432098889e-08)
        * (e as f32)
            .add_checked(x * df(0.868588984, -2.170757285e-08))
            .add_checked(x2 * x.0 * t);

    if d == 0. {
        -SLEEF_INFINITY_F
    } else if (d < 0.) || xisnanf(d) {
        SLEEF_NAN_F
    } else if xisinff(d) {
        SLEEF_INFINITY_F
    } else {
        s.0 + s.1
    }
}

pub fn xlog2f(mut d: f32) -> f32 {
    let o = d < f32::MIN;
    if o {
        d *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(d * (1. / 0.75));
    let m = ldexp3kf(d, -e);

    if o {
        e -= 64;
    }

    let x = (-1.).add_as_f2(m) / (1.).add_as_f2(m);
    let x2 = x.0 * x.0;

    let t = 0.4374550283e+0
        .mla(x2, 0.5764790177e+0)
        .mla(x2, 0.9618012905120);

    let mut s = (e as f32) + x * df(2.8853900432586669922, 3.2734474483568488616e-08);
    s += x2 * x.0 * t;

    if d == 0. {
        -SLEEF_INFINITY_F
    } else if (d < 0.) || xisnanf(d) {
        SLEEF_NAN_F
    } else if xisinff(d) {
        SLEEF_INFINITY_F
    } else {
        s.0 + s.1
    }
}

pub fn xlog1pf(d: f32) -> f32 {
    let mut dp1 = d + 1.;

    let o = dp1 < f32::MIN;
    if o {
        dp1 *= F1_32 * F1_32;
    }

    let mut e = ilogb2kf(dp1 * (1. / 0.75));

    let t = ldexp3kf(1., -e);
    let m = mlaf(d, t, t - 1.);

    if o {
        e -= 64;
    }

    let x = df(m, 0.) / (2.).add_checked_as_f2(m);
    let x2 = x.0 * x.0;

    let t = 0.3027294874e+0
        .mla(x2, 0.3996108174e+0)
        .mla(x2, 0.6666694880e+0);

    let s = (df(0.69314718246459960938, -1.904654323148236017e-09) * (e as f32))
        .add_checked(x.scale(2.))
        .add_checked(x2 * x.0 * t);

    if xisnegzerof(d) {
        -0.
    } else if d == -1. {
        -SLEEF_INFINITY_F
    } else if d < -1. {
        SLEEF_NAN_F
    } else if d > 1e+38 {
        SLEEF_INFINITY_F
    } else {
        s.0 + s.1
    }
}

pub fn xcbrtf(mut d: f32) -> f32 {
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

    let x = (-0.601564466953277587890625)
        .mla(d, 2.8208892345428466796875)
        .mla(d, -5.532182216644287109375)
        .mla(d, 5.898262500762939453125)
        .mla(d, -3.8095417022705078125)
        .mla(d, 2.2241256237030029296875);

    let y = d * x * x;
    (y - (2. / 3.) * y * (y * x - 1.)) * q
}

//

pub fn xfabsf(x: f32) -> f32 {
    fabsfk(x)
}

pub fn xcopysignf(x: f32, y: f32) -> f32 {
    copysignfk(x, y)
}

pub fn xfmaxf(x: f32, y: f32) -> f32 {
    if y != y {
        x
    } else if x > y {
        x
    } else {
        y
    }
}

pub fn xfminf(x: f32, y: f32) -> f32 {
    if y != y {
        x
    } else if x < y {
        x
    } else {
        y
    }
}

pub fn xfdimf(x: f32, y: f32) -> f32 {
    let ret = x - y;
    if (ret < 0.) || (x == y) {
        0.
    } else {
        ret
    }
}

pub fn xtruncf(x: f32) -> f32 {
    let fr = x - (x as i32 as f32);
    if xisinff(x) || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

pub fn xfloorf(x: f32) -> f32 {
    let mut fr = x - (x as i32 as f32);
    fr = if fr < 0. { fr + 1. } else { fr };
    if xisinff(x) || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

pub fn xceilf(x: f32) -> f32 {
    let mut fr = x - (x as i32 as f32);
    fr = if fr <= 0. { fr } else { fr - 1. };
    if xisinff(x) || (fabsfk(x) >= F1_23) {
        x
    } else {
        copysignfk(x - fr, x)
    }
}

pub fn xroundf(d: f32) -> f32 {
    let mut x = d + 0.5;
    let mut fr = x - (x as i32 as f32);
    if (fr == 0.) && (x <= 0.) {
        x -= 1.
    };
    fr = if fr < 0. { fr + 1. } else { fr };
    x = if d == 0.4999999701976776123 { 0. } else { x }; // nextafterf(0.5, 0)
    if xisinff(d) || (fabsfk(d) >= F1_23) {
        d
    } else {
        copysignfk(x - fr, d)
    }
}

pub fn xrintf(d: f32) -> f32 {
    let mut x = d + 0.5;
    let isodd = (1 & (x as i32)) != 0;
    let mut fr = x - (x as i32 as f32);
    fr = if (fr < 0.) || ((fr == 0.) && isodd) {
        fr + 1.
    } else {
        fr
    };
    x = if d == 0.50000005960464477539 { 0. } else { x }; // nextafterf(0.5, 1)
    if xisinff(d) || (fabsfk(d) >= F1_23) {
        d
    } else {
        copysignfk(x - fr, d)
    }
}

pub fn xmodff(x: f32) -> f32n2 {
    let mut fr = x - (x as i32 as f32);
    fr = if fabsfk(x) > F1_23 { 0. } else { fr };
    f32n2::new(copysignfk(fr, x), copysignfk(x - fr, x))
}

pub fn xldexpf(x: f32, mut exp: i32) -> f32 {
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

pub fn xnextafterf(x: f32, y: f32) -> f32 {
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

    if xisnanf(x) || xisnanf(y) {
        SLEEF_NAN_F
    } else if (x == 0.) && (y == 0.) {
        y
    } else if (cxf == 0.) && (x != 0.) {
        mulsignf(0., x)
    } else {
        cxf
    }
}

pub fn xfrfrexpf(mut x: f32) -> f32 {
    if fabsfk(x) < f32::MIN {
        x *= F1_30;
    }

    let mut cxu = x.to_bits();
    cxu &= !0x7f800000u32;
    cxu |= 0x3f000000u32;

    if x == 0. {
        x
    } else if xisinff(x) {
        mulsignf(SLEEF_INFINITY_F, x)
    } else {
        f32::from_bits(cxu)
    }
}

pub fn xexpfrexpf(mut x: f32) -> i32 {
    let mut ret = 0;

    if fabsfk(x) < f32::MIN {
        x *= F1_30;
        ret = -30;
    }

    ret += (((x.to_bits() >> 23) & 0xff) as i32) - 0x7e;

    if (x == 0.) || xisnanf(x) || xisinff(x) {
        0
    } else {
        ret
    }
}

#[inline]
fn toward0f(d: f32) -> f32 {
    if d == 0. {
        0.
    } else {
        int_bits_to_float(float_to_raw_int_bits(d) - 1)
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

pub fn xfmodf(x: f32, y: f32) -> f32 {
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
        r = (r + ptruncf(q).mul_as_f2(-de)).normalize();
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
        SLEEF_NAN_F
    } else if nu < de {
        x
    } else {
        ret
    }
}

pub fn xsqrtf(d: f32) -> f32 {
    SQRTF(d)
}

pub fn xfmaf(mut x: f32, mut y: f32, mut z: f32) -> f32 {
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
    let mut d = x.mul_as_f2(y);
    d += z;
    let ret = if (x == 0.) || (y == 0.) { z } else { d.0 + d.1 };
    if xisinff(z) && !xisinff(x) && !xisnanf(x) && !xisinff(y) && !xisnanf(y) {
        h2 = z;
    }
    if xisinff(h2) || xisnanf(h2) {
        h2
    } else {
        ret * q
    }
}

//

#[inline]
fn sinpifk(d: f32) -> f32n2 {
    let u = d * 4.;
    let q = ceilfk(u) & !1;
    let o = (q & 2) != 0;

    let mut s = u - (q as f32);
    let t = s;
    s = s * s;
    let s2 = t.mul_as_f2(t);

    //

    let u = (if o { -0.2430611801e-7 } else { 0.3093842054e-6 })
        .mla(s, if o { 0.3590577080e-5 } else { -0.3657307388e-4 })
        .mla(s, if o { -0.3259917721e-3 } else { 0.2490393585e-2 });
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
fn cospifk(d: f32) -> f32n2 {
    let u = d * 4.;
    let q = ceilfk(u) & !1;
    let o = (q & 2) == 0;

    let mut s = u - (q as f32);
    let t = s;
    s = s * s;
    let s2 = t.mul_as_f2(t);

    //

    let u = (if o { -0.2430611801e-7 } else { 0.3093842054e-6 })
        .mla(s, if o { 0.3590577080e-5 } else { -0.3657307388e-4 })
        .mla(s, if o { -0.3259917721e-3 } else { 0.2490393585e-2 });
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

fn gammafk(a: f32) -> (f32n2, f32n2) {
    let otiny = fabsfk(a) < 1e-30;
    let oref = a < 0.5;

    let mut x = if otiny {
        df(0., 0.)
    } else if oref {
        (1.).add_as_f2(-a)
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
        0.000839498720672087279971000786
    } else if o0 {
        0.9435157776e+0
    } else {
        0.1102489550e-3
    }).mla(
        t,
        if o2 {
            -5.17179090826059219329394422e-05
        } else if o0 {
            0.8670063615e+0
        } else {
            0.8160019934e-4
        },
    ).mla(
        t,
        if o2 {
            -0.000592166437353693882857342347
        } else if o0 {
            0.4826702476e+0
        } else {
            0.1528468856e-3
        },
    ).mla(
        t,
        if o2 {
            6.97281375836585777403743539e-05
        } else if o0 {
            -0.8855129778e-1
        } else {
            -0.2355068718e-3
        },
    ).mla(
        t,
        if o2 {
            0.000784039221720066627493314301
        } else if o0 {
            0.1013825238e+0
        } else {
            0.4962242092e-3
        },
    ).mla(
        t,
        if o2 {
            -0.000229472093621399176949318732
        } else if o0 {
            -0.1493408978e+0
        } else {
            -0.1193488017e-2
        },
    ).mla(
        t,
        if o2 {
            -0.002681327160493827160473958490
        } else if o0 {
            0.1697509140e+0
        } else {
            0.2891599433e-2
        },
    ).mla(
        t,
        if o2 {
            0.003472222222222222222175164840
        } else if o0 {
            -0.2072454542e+0
        } else {
            -0.7385451812e-2
        },
    ).mla(
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

    let mut z = u.mul_as_f2(t)
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

    let mut clld = if o2 { u.mul_as_f2(t) + 1. } else { df(1., 0.) };

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

pub mod u35 {
    use common::*;

    pub fn xsincospif(d: f32) -> (f32, f32) {
        let u = d * 4.;
        let q = super::ceilfk(u) & !1_i32;

        let s = u - (q as f32);
        let t = s;
        let s = s * s;

        let mut rsin = -0.3600925265e-4
            .mla(s, 0.2490088111e-2)
            .mla(s, -0.8074551076e-1)
            .mla(s, 0.7853981853e+0)
            * t;

        let mut rcos = 0.3539815225e-5
            .mla(s, -0.3259574005e-3)
            .mla(s, 0.1585431583e-1)
            .mla(s, -0.3084251285e+0)
            .mla(s, 1.);

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
        if xisinff(d) {
            rsin = SLEEF_NAN_F;
            rcos = SLEEF_NAN_F;
        }

        (rsin, rcos)
    }

    pub fn xsinhf(x: f32) -> f32 {
        let e = super::expm1kf(fabsfk(x));
        let mut y = (e + 2.) / (e + 1.) * (0.5 * e);

        y = if fabsfk(x) > 88. { SLEEF_INFINITY_F } else { y };
        y = if xisnanf(y) { SLEEF_INFINITY_F } else { y };
        y = mulsignf(y, x);
        if xisnanf(x) {
            SLEEF_NAN_F
        } else {
            y
        }
    }

    pub fn xcoshf(x: f32) -> f32 {
        let e = super::xexpf(fabsfk(x));
        let mut y = 0.5 * e + 0.5 / e;

        y = if fabsfk(x) > 88. { SLEEF_INFINITY_F } else { y };
        y = if xisnanf(y) { SLEEF_INFINITY_F } else { y };
        if xisnanf(x) {
            SLEEF_NAN_F
        } else {
            y
        }
    }

    pub fn xtanhf(x: f32) -> f32 {
        let mut y = fabsfk(x);
        let d = super::expm1kf(2. * y);
        y = d / (d + 2.);

        y = if fabsfk(x) > 18.714973875 { 1. } else { y };
        y = if xisnanf(y) { 1. } else { y };
        y = mulsignf(y, x);
        if xisnanf(x) {
            SLEEF_NAN_F
        } else {
            y
        }
    }

    pub fn xhypotf(mut x: f32, mut y: f32) -> f32 {
        x = fabsfk(x);
        y = fabsfk(y);
        let min = super::fminfk(x, y);
        let max = super::fmaxfk(x, y);

        let t = min / max;
        if (x == SLEEF_INFINITY_F) || (y == SLEEF_INFINITY_F) {
            SLEEF_INFINITY_F
        } else if xisnanf(x) || xisnanf(y) {
            SLEEF_NAN_F
        } else if min == 0. {
            max
        } else {
            max * SQRTF(1. + t * t)
        }
    }

    pub fn xsqrtf(mut d: f32) -> f32 {
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
        let mut x = int_bits_to_float(0x5f375a86 - (float_to_raw_int_bits(d + 1e-45) >> 1));

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

}

pub mod u1 {
    use common::*;
    use super::rintfk;
    use super::rempif;
    
    pub fn xsinf(d: f32) -> f32 {
        let q: i32;
        let mut s: f32n2;

        if fabsfk(d) < TRIGRANGEMAX2_F {
            let qf = rintfk(d * M_1_PI_F);
            q = qf as i32;
            let u = mlaf(qf, -PI_A2_F, d);
            s = u.add_as_f2(qf * (-PI_B2_F));
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
            if xisinff(d) || xisnanf(d) {
                s.0 = SLEEF_NAN_F;
            }
        }

        let t = s;
        s = s.square();

        let mut u = 2.6083159809786593541503e-06
            .mla(s.0, -0.0001981069071916863322258)
            .mla(s.0, 0.00833307858556509017944336);

        let x = (1.).add_checked((-0.166666597127914428710938).add_checked_as_f2(u * s.0) * s);

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

    pub fn xcosf(mut d: f32) -> f32 {
        let mut s: f32n2;
        let q: i32;

        if fabsfk(d) < TRIGRANGEMAX2_F {
            d = fabsfk(d);
            let dq = mlaf(rintfk(d * M_1_PI_F - 0.5), 2., 1.);
            q = dq as i32;
            s = d.add_as_f2(dq * (-PI_A2_F * 0.5));
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
            if xisinff(d) || xisnanf(d) {
                s.0 = SLEEF_NAN_F;
            }
        }

        let t = s;
        s = s.square();

        let mut u = 2.6083159809786593541503e-06
            .mla(s.0, -0.0001981069071916863322258)
            .mla(s.0, 0.00833307858556509017944336);

        let x = (1.).add_checked((-0.166666597127914428710938).add_checked_as_f2(u * s.0) * s);

        u = t.mul_as_f(x);

        if (q & 2) == 0 {
            -u
        } else {
            u
        }
    }

    pub fn xsincosf(d: f32) -> (f32, f32) {
        let q: i32;
        let mut s: f32n2;

        if fabsfk(d) < TRIGRANGEMAX2_F {
            let qf = rintfk(d * M_2_PI_F);
            q = qf as i32;
            let u = mlaf(qf, -PI_A2_F * 0.5, d);
            s = u.add_as_f2(qf * (-PI_B2_F * 0.5));
            s.add_checked_assign(qf * (-PI_C2_F * 0.5));
        } else {
            let (dfidf, dfii) = rempif(d);
            q = dfii;
            s = dfidf;
            if xisinff(d) || xisnanf(d) {
                s.0 = SLEEF_NAN_F;
            }
        }

        let t = s;
        s.0 = s.square_as_f();

        let u = (-0.000195169282960705459117889)
            .mla(s.0, 0.00833215750753879547119141)
            .mla(s.0, -0.166666537523269653320312)
            * s.0
            * t.0;

        let mut x = t.add_checked(u);
        let mut rsin = x.0 + x.1;
        if xisnegzerof(d) {
            rsin = -0.;
        }

        let u = (-2.71811842367242206819355e-07)
            .mla(s.0, 2.47990446951007470488548e-05)
            .mla(s.0, -0.00138888787478208541870117)
            .mla(s.0, 0.0416666641831398010253906)
            .mla(s.0, -0.5);

        x = (1.).add_checked(s.0.mul_as_f2(u));
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

    pub fn xtanf(d: f32) -> f32 {
        let q: i32;
        let mut s: f32n2;
        if fabsfk(d) < TRIGRANGEMAX2_F {
            let qf = rintfk(d * M_2_PI_F);
            q = qf as i32;
            let u = mlaf(qf, -PI_A2_F * 0.5, d);
            s = u.add_as_f2(qf * (-PI_B2_F * 0.5));
            s.add_checked_assign(qf * (-PI_C2_F * 0.5));
        } else {
            let (dfidf, dfii) = rempif(d);
            q = dfii;
            s = dfidf;
            if xisinff(d) || xisnanf(d) {
                s.0 = SLEEF_NAN_F;
            }
        }

        if (q & 1) != 0 {
            s = -s;
        }

        let t = s;
        s = s.square().normalize();

        let u = 0.00446636462584137916564941
            .mla(s.0, -8.3920182078145444393158e-05)
            .mla(s.0, 0.0109639242291450500488281)
            .mla(s.0, 0.0212360303848981857299805)
            .mla(s.0, 0.0540687143802642822265625);

        let mut x = (0.133325666189193725585938).add_checked_as_f2(u * s.0);
        x = (1.).add_checked((0.33333361148834228515625).add_checked(s * x) * s);
        x = t * x;

        if (q & 1) != 0 {
            x = x.rec();
        }

        if xisnegzerof(d) {
            -0.
        } else {
            x.0 + x.1
        }
    }

    fn atan2kf(mut y: f32n2, mut x: f32n2) -> f32n2 {
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

        let u = (-0.00176397908944636583328247)
            .mla(t.0, 0.0107900900766253471374512)
            .mla(t.0, -0.0309564601629972457885742)
            .mla(t.0, 0.0577365085482597351074219)
            .mla(t.0, -0.0838950723409652709960938)
            .mla(t.0, 0.109463557600975036621094)
            .mla(t.0, -0.142626821994781494140625)
            .mla(t.0, 0.199983194470405578613281);

        t = t * (-0.333332866430282592773438).add_checked_as_f2(u * t.0);
        t = s * (1.).add_checked(t);
        df(1.5707963705062866211, -4.3711388286737928865e-08) * (q as f32) + t
    }

    pub fn xatan2f(mut y: f32, mut x: f32) -> f32 {
        if fabsfk(x) < 2.9387372783541830947e-39 {
            y *= F1_24;
            x *= F1_24;
        } // nexttowardf((1. / FLT_MAX), 1)
        let d = atan2kf(df(fabsfk(y), 0.), df(x, 0.));
        let mut r = d.0 + d.1;

        r = mulsignf(r, x);
        r = if y == 0. {
            (if signf(x) == -1. { M_PI_F } else { 0. })
        } else if xisinff(y) {
            M_PI_2_F - (if xisinff(x) { signf(x) * M_PI_4_F } else { 0. })
        } else if xisinff(x) || (x == 0.) {
            M_PI_2_F - (if xisinff(x) { signf(x) * M_PI_2_F } else { 0. })
        } else {
            r
        };

        if xisnanf(x) || xisnanf(y) {
            SLEEF_NAN_F
        } else {
            mulsignf(r, y)
        }
    }

    pub fn xasinf(d: f32) -> f32 {
        let o = fabsfk(d) < 0.5;
        let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
        let mut x = if o {
            df(fabsfk(d), 0.)
        } else {
            x2.sqrt_as_f2()
        };
        x = if fabsfk(d) == 1. { df(0., 0.) } else { x };

        let u = 0.4197454825e-1
            .mla(x2, 0.2424046025e-1)
            .mla(x2, 0.4547423869e-1)
            .mla(x2, 0.7495029271e-1)
            .mla(x2, 0.1666677296e+0)
            * x2
            * x.0;

        let y = (df(3.1415927410125732422 / 4., -8.7422776573475857731e-08 / 4.).sub_checked(x))
            .add_checked(-u);
        let r = if o { u + x.0 } else { (y.0 + y.1) * 2. };
        mulsignf(r, d)
    }

    pub fn xacosf(d: f32) -> f32 {
        let o = fabsfk(d) < 0.5;
        let x2 = if o { d * d } else { (1. - fabsfk(d)) * 0.5 };
        let mut x = if o {
            df(fabsfk(d), 0.)
        } else {
            x2.sqrt_as_f2()
        };
        x = if fabsfk(d) == 1. { df(0., 0.) } else { x };

        let u = 0.4197454825e-1
            .mla(x2, 0.2424046025e-1)
            .mla(x2, 0.4547423869e-1)
            .mla(x2, 0.7495029271e-1)
            .mla(x2, 0.1666677296e+0)
            * x.0
            * x2;

        let mut y = df(3.1415927410125732422 / 2., -8.7422776573475857731e-08 / 2.)
            .sub_checked(mulsignf(x.0, d).add_checked_as_f2(mulsignf(u, d)));
        x.add_checked_assign(u);
        y = if o { y } else { x.scale(2.) };
        if !o && (d < 0.) {
            y = df(3.1415927410125732422, -8.7422776573475857731e-08).sub_checked(y);
        }

        y.0 + y.1
    }

    pub fn xatanf(d: f32) -> f32 {
        let d2 = atan2kf(df(fabsfk(d), 0.), df(1., 0.));
        let mut r = d2.0 + d2.1;
        if xisinff(d) {
            r = 1.570796326794896557998982;
        }
        mulsignf(r, d)
    }

    pub fn xlogf(mut d: f32) -> f32 {
        let o = d < f32::MIN;
        if o {
            d *= F1_32 * F1_32;
        }

        let mut e = super::ilogb2kf(d * (1. / 0.75));
        let m = super::ldexp3kf(d, -e);

        if o {
            e -= 64;
        }

        let x = (-1.).add_as_f2(m) / (1.).add_as_f2(m);
        let x2 = x.0 * x.0;

        let t = 0.3027294874e+0
            .mla(x2, 0.3996108174e+0)
            .mla(x2, 0.6666694880e+0);

        let s = (df(0.69314718246459960938, -1.904654323148236017e-09) * (e as f32))
            .add_checked(x.scale(2.))
            .add_checked(x2 * x.0 * t);

        if d == 0. {
            -SLEEF_INFINITY_F
        } else if (d < 0.) || xisnanf(d) {
            SLEEF_NAN_F
        } else if xisinff(d) {
            SLEEF_INFINITY_F
        } else {
            s.0 + s.1
        }
    }

    pub fn xcbrtf(mut d: f32) -> f32 {
        let e = super::ilogbkf(fabsfk(d)) + 1;
        d = super::ldexp2kf(d, -e);
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

        let mut x = (-0.601564466953277587890625)
            .mla(d, 2.8208892345428466796875)
            .mla(d, -5.532182216644287109375)
            .mla(d, 5.898262500762939453125)
            .mla(d, -3.8095417022705078125)
            .mla(d, 2.2241256237030029296875);

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
            mulsignf(0., q2.0)
        } else if xisinff(d) {
            mulsignf(SLEEF_INFINITY_F, q2.0)
        } else {
            super::ldexp2kf(v.0 + v.1, (e + 6144) / 3 - 2048)
        }
    }

    pub fn xtgammaf(a: f32) -> f32 {
        let (da, db) = super::gammafk(a);
        let y = super::expk2f(da) * db;
        let mut r = y.0 + y.1;
        r = if ((a == -SLEEF_INFINITY_F) || ((a < 0.) && xisintf(a)))
            || (xisnumberf(a) && (a < 0.) && xisnanf(r))
        {
            SLEEF_NAN_F
        } else {
            r
        };
        if ((a == SLEEF_INFINITY_F) || xisnumberf(a))
            && (a >= -f32::MIN)
            && ((a == 0.) || (a > 36.) || xisnanf(r))
        {
            mulsignf(SLEEF_INFINITY_F, a)
        } else {
            r
        }
    }

    pub fn xlgammaf(a: f32) -> f32 {
        let (da, db) = super::gammafk(a);
        let y = da + super::logk2f(db.abs());
        let r = y.0 + y.1;
        if xisinff(a) || (a <= 0. && xisintf(a)) || (xisnumberf(a) && xisnanf(r)) {
            SLEEF_INFINITY_F
        } else {
            r
        }
    }

    pub fn xerff(mut a: f32) -> f32 {
        let s = a;

        a = fabsfk(a);
        let o0 = a < 1.1;
        let o1 = a < 2.4;
        let o2 = a < 4.0;
        let mut u = if o0 { a * a } else { a };

        let t = if o0 {
            0.7089292194e-4
        } else if o1 {
            -0.1792667899e-4
        } else {
            -0.9495757695e-5
        }.mla(
            u,
            if o0 {
                -0.7768311189e-3
            } else if o1 {
                0.3937633010e-3
            } else {
                0.2481465926e-3
            },
        ).mla(
            u,
            if o0 {
                0.5159463733e-2
            } else if o1 {
                -0.3949181177e-2
            } else {
                -0.2918176819e-2
            },
        ).mla(
            u,
            if o0 {
                -0.2683781274e-1
            } else if o1 {
                0.2445474640e-1
            } else {
                0.2059706673e-1
            },
        ).mla(
            u,
            if o0 {
                0.1128318012e+0
            } else if o1 {
                -0.1070996150e+0
            } else {
                -0.9901899844e-1
            },
        );
        let mut d = t.mul_as_f2(u);
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
        d = if o0 { d } else { (1.).add_checked(-super::expk2f(d)) };
        u = mulsignf(if o2 { d.0 + d.1 } else { 1. }, s);
        if xisnanf(a) {
            SLEEF_NAN_F
        } else {
            u
        }
    }

}

pub mod u15 {
    use common::*;

    pub fn xerfcf(mut a: f32) -> f32 {
        let s = a;
        a = fabsfk(a);
        let o0 = a < 1.;
        let o1 = a < 2.2;
        let o2 = a < 4.3;
        let o3 = a < 10.1;
        let u = if o1 {
            df(a, 0.)
        } else {
            df(1., 0.) / df(a, 0.)
        };

        let t = if o0 {
            -0.8638041618e-4
        } else if o1 {
            -0.6236977242e-5
        } else if o2 {
            -0.3869504035e+0
        } else {
            0.1115344167e+1
        }.mla(
            u.0,
            if o0 {
                0.6000166177e-3
            } else if o1 {
                0.5749821503e-4
            } else if o2 {
                0.1288077235e+1
            } else {
                -0.9454904199e+0
            },
        ).mla(
            u.0,
            if o0 {
                -0.1665703603e-2
            } else if o1 {
                0.6002851478e-5
            } else if o2 {
                -0.1816803217e+1
            } else {
                -0.3667259514e+0
            },
        ).mla(
            u.0,
            if o0 {
                0.1795156277e-3
            } else if o1 {
                -0.2851036377e-2
            } else if o2 {
                0.1249150872e+1
            } else {
                0.7155663371e+0
            },
        ).mla(
            u.0,
            if o0 {
                0.1914106123e-1
            } else if o1 {
                0.2260518074e-1
            } else if o2 {
                -0.1328857988e+0
            } else {
                -0.1262947265e-1
            },
        );

        let mut d = u * t;
        d += if o0 {
            df(-0.102775359343930288081655368891e+0, 0.)
        } else if o1 {
            df(-0.105247583459338632253369014063e+0, 0.)
        } else if o2 {
            df(-0.482365310333045318680618892669e+0, 0.)
        } else {
            df(-0.498961546254537647970305302739e+0, 0.)
        };
        d *= u;
        d += if o0 {
            df(-0.636619483208481931303752546439e+0, 0.)
        } else if o1 {
            df(-0.635609463574589034216723775292e+0, 0.)
        } else if o2 {
            df(-0.134450203224533979217859332703e-2, 0.)
        } else {
            df(-0.471199543422848492080722832666e-4, 0.)
        };
        d *= u;
        d += if o0 {
            df(-0.112837917790537404939545770596e+1, 0.)
        } else if o1 {
            df(-0.112855987376668622084547028949e+1, 0.)
        } else if o2 {
            df(-0.572319781150472949561786101080e+0, 0.)
        } else {
            df(-0.572364030327966044425932623525e+0, 0.)
        };

        let mut x = (if o1 { d } else { df(-a, 0.) }) * a;
        x = if o1 { x } else { x + d };

        x = super::expk2f(x);
        x = if o1 { x } else { x * u };

        let mut r = if o3 { x.0 + x.1 } else { 0. };
        if s < 0. {
            r = 2. - r;
        }
        if xisnanf(s) {
            SLEEF_NAN_F
        } else {
            r
        }
    }

}

pub mod u05 {
    use common::*;

    pub fn xsqrtf(mut d: f32) -> f32 {
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

        let d2 = (d + x.mul_as_f2(x)) * x.rec();

        if (d == 0.) || (d == SLEEF_INFINITY_F) {
            d
        } else {
            (d2.0 + d2.1) * q
        }
    }

    pub fn xsincospif(d: f32) -> (f32, f32) {
        let u = d * 4.;
        let q = super::ceilfk(u) & !1_i32;

        let s = u - (q as f32);
        let t = s;
        let s = s * s;
        let s2 = t.mul_as_f2(t);

        //

        let u = 0.3093842054e-6
            .mla(s, -0.3657307388e-4)
            .mla(s, 0.2490393585e-2);
        let mut x = u * s + df(-0.080745510756969451904, -1.3373665339076936258e-09);
        x = s2 * x + df(0.78539818525314331055, -2.1857338617566484855e-08);

        x *= t;
        let mut rsin = x.0 + x.1;
        if xisnegzerof(d) {
            rsin = -0.;
        }

        let u = (-0.2430611801e-7)
            .mla(s, 0.3590577080e-5)
            .mla(s, -0.3259917721e-3);
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
        if xisinff(d) {
            rsin = SLEEF_NAN_F;
            rcos = SLEEF_NAN_F;
        }

        (rsin, rcos)
    }

    pub fn xhypotf(mut x: f32, mut y: f32) -> f32 {
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
        } else if xisnanf(x) || xisnanf(y) {
            SLEEF_NAN_F
        } else if min == 0. {
            max
        } else if xisnanf(ret) {
            SLEEF_INFINITY_F
        } else {
            ret
        }
    }

    pub fn xsinpif(d: f32) -> f32 {
        let x = super::sinpifk(d);

        if xisinff(d) {
            SLEEF_NAN_F
        } else if fabsfk(d) > TRIGRANGEMAX4_F {
            0.
        } else if xisnegzerof(d) {
            -0.
        } else {
            x.0 + x.1
        }
    }

    pub fn xcospif(d: f32) -> f32 {
        let x = super::cospifk(d);

        if xisinff(d) {
            SLEEF_NAN_F
        } else if fabsfk(d) > TRIGRANGEMAX4_F {
            1.
        } else {
            x.0 + x.1
        }
    }

}
