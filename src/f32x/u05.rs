use super::*;

/// Evaluate sin( π***a*** ) and cos( π***a*** ) for given ***a*** simultaneously
///
/// Evaluates the sine and cosine functions of π***a*** at a time, and store the two values in a tuple.
/// The error bound of the returned value are `max(0.506 ULP, f32::MIN_POSITIVE)` if `[-1e+7, 1e+7]`.
/// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
/// If ***a*** is a `NaN` or infinity, a `NaN` is returned.
pub fn sincospif<const N: usize>(d: F32x<N>) -> (F32x<N>, F32x<N>)
{
    let u = d * F32x::splat(4.);
    let q = u.trunci();
    let q = (q + ((q.cast() >> U32x::splat(31)).cast() ^ I32x::splat(1))) & I32x::splat(!1);
    let s = u - q.cast();

    let t = s;
    let s = s * s;
    let s2 = t.mul_as_doubled(t);

    let u = F32x::splat(0.309_384_205_4_e-6)
        .mla(s, F32x::splat(-0.365_730_738_8_e-4))
        .mla(s, F32x::splat(0.249_039_358_5_e-2));
    let mut x = u * s
        + Doubled::new(
            F32x::splat(-0.080_745_510_756_969_451_904),
            F32x::splat(-1.337_366_533_907_693_625_8_e-9),
        );
    x = s2 * x
        + Doubled::new(
            F32x::splat(0.785_398_185_253_143_310_55),
            F32x::splat(-2.185_733_861_756_648_485_5_e-8),
        );

    x *= t;
    let rx = F32x::from(x);

    let rx = d.is_neg_zero().select(F32x::NEG_ZERO, rx);

    let u = F32x::splat(-0.243_061_180_1_e-7)
        .mla(s, F32x::splat(0.359_057_708_e-5))
        .mla(s, F32x::splat(-0.325_991_772_1_e-3));
    x = u * s
        + Doubled::new(
            F32x::splat(0.015_854_343_771_934_509_277),
            F32x::splat(4.494_005_135_403_224_281_1_e-10),
        );
    x = s2 * x
        + Doubled::new(
            F32x::splat(-0.308_425_128_459_930_419_92),
            F32x::splat(-9.072_833_903_073_392_227_7_e-9),
        );

    x = x * s2 + F32x::ONE;
    let ry = F32x::from(x);

    let o = (q & I32x::splat(2)).simd_eq(I32x::splat(0));
    let mut rsin = o.select(rx, ry);
    let mut rcos = o.select(ry, rx);

    let o = (q & I32x::splat(4)).simd_eq(I32x::splat(4));
    rsin = F32x::from_bits((o.to_int().cast() & F32x::NEG_ZERO.to_bits()) ^ rsin.to_bits());

    let o = ((q + I32x::splat(2)) & I32x::splat(4)).simd_eq(I32x::splat(4));
    rcos = F32x::from_bits((o.to_int().cast() & F32x::NEG_ZERO.to_bits()) ^ rcos.to_bits());

    let o = d.abs().simd_gt(F32x::splat(1e+7));
    rsin = F32x::from_bits(!o.to_int().cast::<u32>() & rsin.to_bits());
    rcos = F32x::from_bits(!o.to_int().cast::<u32>() & rcos.to_bits());

    let o = d.is_infinite();
    rsin = F32x::from_bits(o.to_int().cast() | rsin.to_bits());
    rcos = F32x::from_bits(o.to_int().cast() | rcos.to_bits());

    (rsin, rcos)
}

#[test]
fn test_sincospif() {
    use rug::{float::Constant, Float};
    let rangemax2 = 1e+7 / 4.;
    test_f_ff::<2>(
        sincospif,
        |mut in1| {
            let prec = in1.prec();
            in1.set_prec(prec * 2);
            (in1 * Float::with_val(prec * 2, Constant::Pi)).sin_cos(Float::new(prec))
        },
        -rangemax2..=rangemax2,
        0.505,
    );
}

/// Square root function
///
/// The error bound of the returned value is `0.5001 ULP`.
pub fn sqrtf<const N: usize>(d: F32x<N>) -> F32x<N>
{
    if cfg!(target_feature = "fma") {
        let d = d.simd_lt(F32x::ZERO).select(F32x::NAN, d);

        let o = d.simd_lt(F32x::splat(5.293_955_920_339_377_e-23));
        let d = o.select(d * F32x::splat(1.888_946_593_147_858_e+22), d);
        let q = o.select(F32x::splat(7.275_957_614_183_426_e-12), F32x::splat(1.));

        let mut y = F32x::from_bits(
            (I32x::splat(0x5f3759df) - (d.to_bits().cast::<i32>() >> I32x::splat(1))).cast(),
        );

        let mut x = d * y;
        let mut w = F32x::HALF * y;
        y = x.neg_mul_add(w, F32x::HALF);
        x = x.mla(y, x);
        w = w.mla(y, w);
        y = x.neg_mul_add(w, F32x::HALF);
        x = x.mla(y, x);
        w = w.mla(y, w);

        y = x.neg_mul_add(w, F32x::splat(1.5));
        w += w;
        w *= y;
        x = w * d;
        y = w.mul_sub(d, x);
        let mut z = w.neg_mul_add(x, F32x::ONE);

        z = w.neg_mul_add(y, z);
        w = F32x::HALF * x;
        w = w.mla(z, y);
        w += x;

        w *= q;

        w = (d.simd_eq(F32x::ZERO) | d.simd_eq(F32x::INFINITY)).select(d, w);

        d.simd_lt(F32x::ZERO).select(F32x::NAN, w)
    } else {
        let d = d.simd_lt(F32x::ZERO).select(F32x::NAN, d);

        let o = d.simd_lt(F32x::splat(5.293_955_920_339_377_e-23));
        let d = o.select(d * F32x::splat(1.888_946_593_147_858_e+22), d);
        let q = o.select(F32x::splat(7.275_957_614_183_426_e-12 * 0.5), F32x::HALF);

        let o = d.simd_gt(F32x::splat(1.844_674_407_370_955_2_e+19));
        let d = o.select(d * F32x::splat(5.421_010_862_427_522_e-20), d);
        let q = o.select(F32x::splat(4_294_967_296.0 * 0.5), q);

        let mut x = F32x::from_bits(
            (I32x::splat(0x_5f37_5a86)
                - ((d + F32x::splat(1e-45)).to_bits().cast() >> I32x::splat(1)))
            .cast(),
        );

        x *= F32x::splat(1.5) - F32x::HALF * d * x * x;
        x *= F32x::splat(1.5) - F32x::HALF * d * x * x;
        x *= F32x::splat(1.5) - F32x::HALF * d * x * x;
        x *= d;

        let d2 = (d + x.mul_as_doubled(x)) * x.recip_as_doubled();

        x = F32x::from(d2) * q;

        x = d.simd_eq(F32x::INFINITY).select(F32x::INFINITY, x);
        d.simd_eq(F32x::ZERO).select(d, x)
    }
}

#[test]
fn test_sqrtf() {
    test_f_f::<2>(sqrtf, rug::Float::sqrt, f32::MIN..=f32::MAX, 0.5);
}

/// 2D Euclidian distance function
///
/// The error bound of the returned value is `0.5001 ULP`.
pub fn hypotf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
{
    let x = x.abs();
    let y = y.abs();
    let min = x.simd_min(y);
    let n = min;
    let max = x.simd_max(y);
    let d = max;

    let o = max.simd_lt(F32x::splat(f32::MIN_POSITIVE));
    let n = o.select(n * F32x::F1_24, n);
    let d = o.select(d * F32x::F1_24, d);

    let t = Doubled::from(n) / Doubled::from(d);
    let t = (t.square() + F32x::ONE).sqrt() * max;
    let mut ret = F32x::from(t);
    ret = ret.is_nan().select(F32x::INFINITY, ret);
    ret = min.simd_eq(F32x::ZERO).select(max, ret);
    ret = (x.is_nan() | y.is_nan()).select(F32x::NAN, ret);
    (x.simd_eq(F32x::INFINITY) | y.simd_eq(F32x::INFINITY)).select(F32x::INFINITY, ret)
}

#[test]
fn test_hypotf() {
    test_ff_f::<2>(
        hypotf,
        rug::Float::hypot,
        f32::MIN..=f32::MAX,
        f32::MIN..=f32::MAX,
        0.5001,
    );
}

/// Evaluate sin( π***a*** ) for given ***a***
///
/// This function evaluates the sine function of π***a***.
/// The error bound of the returned value is `max(0.506 ULP, f32::MIN_POSITIVE)`
/// if `[-1e+7, 1e+7]` for the single-precision function.
/// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
/// If ***a*** is a `NaN` or infinity, a NaN is returned.
pub fn sinpif<const N: usize>(d: F32x<N>) -> F32x<N>
{
    let x = sinpifk(d);
    let mut r = F32x::from(x);

    r = d.is_neg_zero().select(F32x::NEG_ZERO, r);
    r = F32x::from_bits(!d.abs().simd_gt(F32x::TRIGRANGEMAX4).to_int().cast::<u32>() & r.to_bits());
    F32x::from_bits(d.is_infinite().to_int().cast() | r.to_bits())
}

#[test]
fn test_sinpif() {
    use rug::{float::Constant, Float};
    let rangemax2 = 1e+7 / 4.;
    test_f_f::<2>(
        sinpif,
        |mut in1| {
            let prec = in1.prec();
            in1.set_prec(prec * 2);
            (in1 * Float::with_val(prec * 2, Constant::Pi)).sin()
        },
        -rangemax2..=rangemax2,
        0.506,
    );
}

/// Evaluate cos( π***a*** ) for given ***a***
///
/// This function evaluates the cosine function of π***a***.
/// The error bound of the returned value is `max(0.506 ULP, f32::MIN_POSITIVE)`
/// if `[-1e+7, 1e+7]` for the single-precision function.
/// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
/// If ***a*** is a `NaN` or infinity, a `NaN` is returned.
pub fn cospif<const N: usize>(d: F32x<N>) -> F32x<N>
{
    let x = cospifk(d);
    let r = F32x::from(x);

    let r = d.abs().simd_gt(F32x::TRIGRANGEMAX4).select(F32x::ONE, r);
    F32x::from_bits(d.is_infinite().to_int().cast() | r.to_bits())
}

#[test]
fn test_cospif() {
    use rug::{float::Constant, Float};
    let rangemax2 = 1e+7 / 4.;
    test_f_f::<2>(
        cospif,
        |mut in1| {
            let prec = in1.prec();
            in1.set_prec(prec * 2);
            (in1 * Float::with_val(prec * 2, Constant::Pi)).cos()
        },
        -rangemax2..=rangemax2,
        0.506,
    );
}
