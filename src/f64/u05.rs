//! Functions with 0.5 ULP error bound

use super::*;

pub fn sincospi(d: f64) -> (f64, f64) {
    let u = d * 4.;
    let q = ceilk(u) & !1_isize;

    let s = u - (q as f64);
    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    //

    let u = (-2.02461120785182399295868e-14_f64)
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

    let u = 9.94480387626843774090208e-16_f64
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
    if d.is_infinite() {
        rsin = f64::NAN;
        rcos = f64::NAN;
    }

    (rsin, rcos)
}

pub fn sinpi(d: f64) -> f64 {
    let x = sinpik(d);
    if d.is_infinite() {
        f64::NAN
    } else if fabsk(d) > TRIGRANGEMAX3 / 4. {
        0.
    } else if xisnegzero(d) {
        -0.
    } else {
        x.0 + x.1
    }
}

pub fn cospi(d: f64) -> f64 {
    let x = cospik(d);

    if d.is_infinite() {
        f64::NAN
    } else if fabsk(d) > TRIGRANGEMAX3 / 4. {
        1.
    } else {
        x.0 + x.1
    }
}

pub fn sqrt(mut d: f64) -> f64 {
    let mut q = 0.5;

    d = if d < 0. { f64::NAN } else { d };

    if d < 8.636168555094445E-78 {
        d *= 1.157920892373162E77;
        q = 2.9387358770557188E-39 * 0.5;
    }

    if d > 1.3407807929942597e+154 {
        d *= 7.4583407312002070e-155;
        q = 1.1579208923731620e+77 * 0.5;
    }

    // http://en.wikipedia.org/wiki/Fast_inverse_square_root
    let mut x = f64::from_bits(0x5fe6ec85e7de30da - ((d + 1e-320).to_bits() >> 1));

    x = x * (1.5 - 0.5 * d * x * x);
    x = x * (1.5 - 0.5 * d * x * x);
    x = x * (1.5 - 0.5 * d * x * x) * d;

    let d2 = (d + x.mul_as_doubled(x)) * x.recpre();

    let ret = (d2.0 + d2.1) * q;

    let ret = if d == f64::INFINITY {
        f64::INFINITY
    } else {
        ret
    };
    if d == 0. {
        d
    } else {
        ret
    }
}

pub fn hypot(mut x: f64, mut y: f64) -> f64 {
    x = fabsk(x);
    y = fabsk(y);
    let min = fmink(x, y);
    let mut n = min;
    let max = fmaxk(x, y);
    let mut d = max;

    if max < f64::MIN {
        n *= D1_54;
        d *= D1_54;
    }
    let mut t = dd(n, 0.) / dd(d, 0.);
    t = (t.square() + 1.).sqrt() * max;
    let ret = t.0 + t.1;

    if (x == f64::INFINITY) || (y == f64::INFINITY) {
        f64::INFINITY
    } else if x.is_nan() || y.is_nan() {
        f64::NAN
    } else if min == 0. {
        max
    } else if ret.is_nan() {
        f64::INFINITY
    } else {
        ret
    }
}
