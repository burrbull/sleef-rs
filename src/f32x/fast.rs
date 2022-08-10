use super::*;

/// Fast sine function
///
/// The error bounds of the returned value is `min(350 ULP, 2e-6)`.
pub fn sinf<const N: usize>(mut d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let t = d;

    let s = d * frac_1_pi();
    let mut u = s.round();
    let q = s.roundi();
    d = u.mla(-pi(), d);

    let s = d * d;

    u = F32x::splat(-0.188_174_817_6_e-3)
        .mla(s, F32x::splat(0.832_350_272_7_e-2))
        .mla(s, F32x::splat(-0.166_665_136_8));
    u = (s * d).mla(u, d);

    u = F32x::from_bits(
        ((q & I32x::splat(1)).simd_eq(I32x::splat(1)).to_int().cast() & (-zero()).to_bits())
            ^ u.to_bits(),
    );

    let g = t.abs().simd_lt(F32x::splat(30.));
    if !g.all() {
        return g.select(u, super::u35::sinf(t));
    } // !!!!???????????

    u
}

#[test]
fn test_sinf() {
    test_c_f_f::<2>(sinf, rug::Float::sin, -30.0..=30.0, |ulp, o, e| {
        let ulp_ex = 350.;
        (
            ulp <= ulp_ex || (e.clone() - o).abs() <= 2e-6,
            format!("ULP: {ulp} > {ulp_ex}"),
        )
    });
}

/// Fast sine function
///
/// The error bounds of the returned value is `min(350 ULP, 2e-6)`.
pub fn cosf<const N: usize>(mut d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let t = d;

    let s = d.mla(frac_1_pi(), -half());
    let mut u = s.round();
    let q = s.roundi();
    d = u.mla(-pi(), d - frac_pi_2());

    let s = d * d;

    u = F32x::splat(-0.188_174_817_6_e-3)
        .mla(s, F32x::splat(0.832_350_272_7_e-2))
        .mla(s, F32x::splat(-0.166_665_136_8));
    u = (s * d).mla(u, d);

    u = F32x::from_bits(
        ((q & I32x::splat(1)).simd_eq(I32x::splat(0)).to_int().cast() & (-zero()).to_bits())
            ^ u.to_bits(),
    );

    let g = t.abs().simd_lt(F32x::splat(30.));
    if !g.all() {
        return g.select(u, super::u35::cosf(t));
    }

    u
}

#[test]
fn test_cosf() {
    test_c_f_f::<2>(cosf, rug::Float::cos, -30.0..=30.0, |ulp, o, e| {
        let ulp_ex = 350.;
        (
            ulp <= ulp_ex || (e.clone() - o).abs() <= 2e-6,
            format!("ULP: {ulp} > {ulp_ex}"),
        )
    });
}

#[inline]
fn logk3f<const N: usize>(mut d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let (m, e) = //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")
    {
        let o = d.simd_lt(F32x::splat(f32::MIN_POSITIVE));
        d = o.select(d * (f1_32x() * f1_32x()), d);
        let e = ilogb2kf(d * F32x::splat(1./0.75));
        (ldexp3kf(d, -e), o.select(e - I32x::splat(64), e))
    /*} else {
        let mut e = vgetexp_vf_vf(d * F32x::splat(1./0.75));
        (vgetmant_vf_vf(d), e.simd_eq(infinity()).select(F32x::splat(128.), e))
    */};

    let x = (m - one()) / (one() + m);
    let x2 = x * x;

    let t = F32x::splat(0.239_282_846_450_805_664_062_5)
        .mla(x2, F32x::splat(0.285_182_118_415_832_519_531_25))
        .mla(x2, F32x::splat(0.400_005_877_017_974_853_515_625))
        .mla(x2, F32x::splat(0.666_666_686_534_881_591_796_875))
        .mla(x2, F32x::splat(2.));

    //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {
    x.mla(t, F32x::splat(0.693_147_180_559_945_286_226_764) * e.cast())
    /* } else {
        x.mla(t, F32x::splat(0.693_147_180_559_945_286_226_764) * e)
    }*/
}

#[inline]
fn expk3f<const N: usize>(d: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let q = (d * r_ln2_f()).roundi();

    let mut s = q.cast::<f32>().mla(-l2u_f(), d);
    s = q.cast::<f32>().mla(-l2l_f(), s);

    let mut u = F32x::splat(0.000_198_527_617_612_853_646_278_381)
        .mla(s, F32x::splat(0.001_393_043_552_525_341_510_772_71))
        .mla(s, F32x::splat(0.008_333_360_776_305_198_669_433_59))
        .mla(s, F32x::splat(0.041_666_485_369_205_474_853_515_6))
        .mla(s, F32x::splat(0.166_666_671_633_720_397_949_219))
        .mla(s, half());

    u = (s * s).mla(u, s + one());
    u = ldexp2kf(u, q);

    F32x::from_bits(!d.simd_lt(F32x::splat(-104.)).to_int().cast::<u32>() & u.to_bits())
}

/// Fast power function
///
/// The error bounds of the returned value is `350 ULP`.
pub fn powf<const N: usize>(x: F32x<N>, y: F32x<N>) -> F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut result = expk3f(logk3f(x.abs()) * y);
    let yisint = y.trunc().simd_eq(y) | y.abs().simd_gt(f1_24x());
    let yisodd =
        (y.trunci() & I32x::splat(1)).simd_eq(I32x::splat(1)) & yisint & y.abs().simd_lt(f1_24x());

    result = (x.is_sign_negative() & yisodd).select(-result, result);

    result = x.simd_eq(zero()).select(zero(), result);
    y.simd_eq(zero()).select(one(), result)
}

#[test]
fn test_powf() {
    use rug::{ops::Pow, Float};
    test_ff_f::<2>(
        powf,
        |in1, in2| Float::with_val(in1.prec(), in1.pow(in2)),
        f32::MIN..=f32::MAX,
        f32::MIN..=f32::MAX,
        350.,
    );
}
