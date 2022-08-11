use super::*;

/// Sine function
///
/// These functions evaluates the sine function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn sin<const N: usize>(mut d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let r = d;
    let mut ql;

    if d.abs().simd_lt(F64x::TRIGRANGEMAX2).all() {
        let dql = (d * F64x::FRAC_1_PI).round();
        ql = dql.roundi();
        d = dql.mla(-F64x::PI_A2, d);
        d = dql.mla(-F64x::PI_B2, d);
    } else if d.abs().simd_lt(F64x::TRIGRANGEMAX).all() {
        let dqh = (d * (F64x::FRAC_1_PI / F64x::D1_24)).trunc();
        let dqh = dqh * F64x::D1_24;
        let dql = d.mul_sub(F64x::FRAC_1_PI, dqh).round();
        ql = dql.roundi();

        d = dqh.mla(-F64x::PI_A, d);
        d = dql.mla(-F64x::PI_A, d);
        d = dqh.mla(-F64x::PI_B, d);
        d = dql.mla(-F64x::PI_B, d);
        d = dqh.mla(-F64x::PI_C, d);
        d = dql.mla(-F64x::PI_C, d);
        d = (dqh + dql).mla(-F64x::PI_D, d);
    } else {
        let (mut ddidd, ddii) = rempi(d);
        ql = ddii & Ix::splat(3);
        ql = ql
            + ql
            + ddidd
                .0
                .simd_gt(F64x::ZERO)
                .cast()
                .select(Ix::splat(2), Ix::splat(1));
        ql >>= Ix::splat(2);
        let o = (ddii & Ix::splat(1)).simd_eq(Ix::splat(1));
        let mut x = Doubled::new(
            F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(ddidd.0),
            F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(ddidd.0),
        );
        x = ddidd + x;
        ddidd = o.cast().select_doubled(x, ddidd);
        d = F64x::from(ddidd);
        d = F64x::from_bits((r.is_infinite() | r.is_nan()).to_int().cast() | d.to_bits());
    }

    let s = d * d;

    d = F64x::from_bits(
        ((ql & Ix::splat(1)).simd_eq(Ix::splat(1)).to_int().cast() & F64x::NEG_ZERO.to_bits())
            ^ d.to_bits(),
    );

    let s2 = s * s;
    let s4 = s2 * s2;

    let mut u = F64x::poly8(
        s,
        s2,
        s4,
        -7.972_559_550_090_378_688_919_52_e-18,
        2.810_099_727_108_632_000_912_51_e-15,
        -7.647_122_191_181_588_332_884_84_e-13,
        1.605_904_306_056_645_016_290_54_e-10,
        -2.505_210_837_635_020_458_107_55_e-8,
        2.755_731_922_391_987_476_304_16_e-6,
        -0.000_198_412_698_412_696_162_806_809,
        0.008_333_333_333_333_329_748_238_15,
    )
    .mla(s, F64x::splat(-0.166_666_666_666_666_657_414_808));

    u = s * (u * d) + d;

    r.is_neg_zero().select(r, u)
}

/// Sine function
///
/// These functions evaluates the sine function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
///
/// NOTE: This version is slower, but SIMD lanes are independent
pub fn sin_deterministic<const N: usize>(mut d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    // This is the deterministic implementation of sin function. Returned
    // values from deterministic functions are bitwise consistent across
    // all platforms.
    let r = d;

    let dql = (d * F64x::FRAC_1_PI).round();
    let mut ql = dql.roundi();
    d = dql.mla(-F64x::PI_A2, d);
    d = dql.mla(-F64x::PI_B2, d);
    let g = r.abs().simd_lt(F64x::TRIGRANGEMAX2);

    if !g.all() {
        let mut dqh = (r * F64x::FRAC_1_PI / F64x::D1_24).trunc();
        dqh *= F64x::D1_24;
        let dql = r.mul_sub(F64x::FRAC_1_PI, dqh).round();

        let mut u = dqh.mla(-F64x::PI_A, r);
        u = dql.mla(-F64x::PI_A, u);
        u = dqh.mla(-F64x::PI_B, u);
        u = dql.mla(-F64x::PI_B, u);
        u = dqh.mla(-F64x::PI_C, u);
        u = dql.mla(-F64x::PI_C, u);
        u = (dqh + dql).mla(-F64x::PI_D, u);

        ql = g.cast().select(ql, dql.roundi());
        d = g.select(d, u);
        let g = r.abs().simd_lt(F64x::TRIGRANGEMAX);

        if !g.all() {
            let (mut ddidd, ddii) = rempi(r);
            let mut ql2 = ddii & Ix::splat(3);
            ql2 = ql2
                + ql2
                + ddidd
                    .0
                    .simd_gt(F64x::ZERO)
                    .cast()
                    .select(Ix::splat(2), Ix::splat(1));
            ql2 >>= Ix::splat(2);
            let o = (ddii & Ix::splat(1)).simd_eq(Ix::splat(1));
            let mut x = Doubled::new(
                F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(ddidd.0),
                F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(ddidd.0),
            );
            x = ddidd + x;
            ddidd = o.cast().select_doubled(x, ddidd);
            let u = F64x::from(ddidd);
            ql = g.cast().select(ql, ql2);
            d = g.select(d, u);
            d = F64x::from_bits((r.is_infinite() | r.is_nan()).to_int().cast() | d.to_bits());
        }
    }

    let s = d * d;

    d = F64x::from_bits(
        ((ql & Ix::splat(1)).simd_eq(Ix::splat(1)).to_int().cast() & F64x::NEG_ZERO.to_bits())
            ^ d.to_bits(),
    );

    let s2 = s * s;
    let s4 = s2 * s2;

    let mut u = F64x::poly8(
        s,
        s2,
        s4,
        -7.972_559_550_090_378_688_919_52_e-18,
        2.810_099_727_108_632_000_912_51_e-15,
        -7.647_122_191_181_588_332_884_84_e-13,
        1.605_904_306_056_645_016_290_54_e-10,
        -2.505_210_837_635_020_458_107_55_e-8,
        2.755_731_922_391_987_476_304_16_e-6,
        -0.000_198_412_698_412_696_162_806_809,
        0.008_333_333_333_333_329_748_238_15,
    )
    .mla(s, F64x::splat(-0.166_666_666_666_666_657_414_808));

    u = s * (u * d) + d;

    r.is_neg_zero().select(r, u)
}

#[test]
fn test_sin() {
    test_f_f::<2>(sin, rug::Float::sin, f64::MIN..=f64::MAX, 3.5);
    test_f_f::<2>(sin_deterministic, rug::Float::sin, f64::MIN..=f64::MAX, 3.5);
}

/// Cosine function
///
/// These functions evaluates the cosine function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn cos<const N: usize>(mut d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let r = d;
    let mut ql;

    if d.abs().simd_lt(F64x::TRIGRANGEMAX2).all() {
        let dql = F64x::splat(2.).mla(d.mla(F64x::FRAC_1_PI, F64x::splat(-0.5)).round(), F64x::ONE);
        ql = dql.roundi();
        d = dql.mla(-F64x::PI_A2 * F64x::HALF, d);
        d = dql.mla(-F64x::PI_B2 * F64x::HALF, d);
    } else if d.abs().simd_lt(F64x::TRIGRANGEMAX).all() {
        let dqh = d
            .mla(
                F64x::FRAC_1_PI / F64x::D1_23,
                -F64x::FRAC_1_PI / F64x::D1_24,
            )
            .trunc();
        ql = (d * F64x::FRAC_1_PI + dqh.mla(-F64x::D1_23, F64x::splat(-0.5))).roundi();
        let dqh = dqh * F64x::D1_24;
        ql = ql + ql + Ix::splat(1);
        let dql = ql.cast::<f64>();

        d = dqh.mla(-F64x::PI_A * F64x::HALF, d);
        d = dql.mla(-F64x::PI_A * F64x::HALF, d);
        d = dqh.mla(-F64x::PI_B * F64x::HALF, d);
        d = dql.mla(-F64x::PI_B * F64x::HALF, d);
        d = dqh.mla(-F64x::PI_C * F64x::HALF, d);
        d = dql.mla(-F64x::PI_C * F64x::HALF, d);
        d = (dqh + dql).mla(-F64x::PI_D * F64x::HALF, d);
    } else {
        let (mut ddidd, ddii) = rempi(d);
        ql = ddii & Ix::splat(3);
        ql = ql
            + ql
            + ddidd
                .0
                .simd_gt(F64x::ZERO)
                .cast()
                .select(Ix::splat(8), Ix::splat(7));
        ql >>= Ix::splat(1);
        let o = (ddii & Ix::splat(1)).simd_eq(Ix::splat(0));
        let y = ddidd
            .0
            .simd_gt(F64x::ZERO)
            .select(F64x::ZERO, F64x::splat(-1.));
        let mut x = Doubled::new(
            F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(y),
            F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(y),
        );
        x = ddidd + x;
        ddidd = o.cast().select_doubled(x, ddidd);
        d = F64x::from(ddidd);
        d = F64x::from_bits((r.is_infinite() | r.is_nan()).to_int().cast() | d.to_bits());
    }

    let s = d * d;

    d = F64x::from_bits(
        ((ql & Ix::splat(2)).simd_eq(Ix::splat(0)).to_int().cast() & F64x::NEG_ZERO.to_bits())
            ^ d.to_bits(),
    );

    let s2 = s * s;
    let s4 = s2 * s2;

    let u = F64x::poly8(
        s,
        s2,
        s4,
        -7.972_559_550_090_378_688_919_52_e-18,
        2.810_099_727_108_632_000_912_51_e-15,
        -7.647_122_191_181_588_332_884_84_e-13,
        1.605_904_306_056_645_016_290_54_e-10,
        -2.505_210_837_635_020_458_107_55_e-8,
        2.755_731_922_391_987_476_304_16_e-6,
        -0.000_198_412_698_412_696_162_806_809,
        0.008_333_333_333_333_329_748_238_15,
    )
    .mla(s, F64x::splat(-0.166_666_666_666_666_657_414_808));

    s * (u * d) + d
}

/// Cosine function
///
/// These functions evaluates the cosine function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
///
/// NOTE: This version is slower, but SIMD lanes are independent
pub fn cos_deterministic<const N: usize>(mut d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let r = d;

    let g = d.abs().simd_lt(F64x::TRIGRANGEMAX2);
    let dql = F64x::splat(2.).mla(
        (d.mla(F64x::FRAC_1_PI, F64x::splat(-0.5))).round(),
        F64x::ONE,
    );
    let mut ql = dql.roundi();
    d = dql.mla(-F64x::PI_A2 * F64x::HALF, d);
    d = dql.mla(-F64x::PI_B2 * F64x::HALF, d);

    if !g.all() {
        let mut dqh = (r.mla(
            F64x::FRAC_1_PI / F64x::D1_23,
            -F64x::FRAC_1_PI / F64x::D1_24,
        ))
        .trunc();
        let mut ql2 = (r * F64x::FRAC_1_PI + dqh.mla(-F64x::D1_23, F64x::splat(-0.5))).roundi();
        dqh *= F64x::D1_24;
        ql2 = ql2 + ql2 + Ix::splat(1);
        let dql = ql2.cast::<f64>();

        let mut u = dqh.mla(-F64x::PI_A * F64x::HALF, r);
        u = dql.mla(-F64x::PI_A * F64x::HALF, u);
        u = dqh.mla(-F64x::PI_B * F64x::HALF, u);
        u = dql.mla(-F64x::PI_B * F64x::HALF, u);
        u = dqh.mla(-F64x::PI_C * F64x::HALF, u);
        u = dql.mla(-F64x::PI_C * F64x::HALF, u);
        u = (dqh + dql).mla(-F64x::PI_D * F64x::HALF, u);

        ql = g.cast().select(ql, ql2);
        d = g.select(d, u);
        let g = r.abs().simd_lt(F64x::TRIGRANGEMAX);

        if !g.all() {
            let (mut ddidd, ddii) = rempi(r);
            let mut ql2 = ddii & Ix::splat(3);
            ql2 = ql2
                + ql2
                + ddidd
                    .0
                    .simd_gt(F64x::ZERO)
                    .cast()
                    .select(Ix::splat(8), Ix::splat(7));
            ql2 >>= Ix::splat(1);
            let o = (ddii & Ix::splat(1)).simd_eq(Ix::splat(0));
            let y = ddidd
                .0
                .simd_gt(F64x::ZERO)
                .select(F64x::ZERO, F64x::splat(-1.));
            let mut x = Doubled::new(
                F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(y),
                F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(y),
            );
            x = ddidd + x;
            ddidd = o.cast().select_doubled(x, ddidd);
            let u = F64x::from(ddidd);
            ql = g.cast().select(ql, ql2);
            d = g.select(d, u);
            d = F64x::from_bits((r.is_infinite() | r.is_nan()).to_int().cast() | d.to_bits());
        }
    }

    let s = d * d;

    d = F64x::from_bits(
        ((ql & Ix::splat(2)).simd_eq(Ix::splat(0)).to_int().cast() & F64x::NEG_ZERO.to_bits())
            ^ d.to_bits(),
    );

    let s2 = s * s;
    let s4 = s2 * s2;

    let u = F64x::poly8(
        s,
        s2,
        s4,
        -7.972_559_550_090_378_688_919_52_e-18,
        2.810_099_727_108_632_000_912_51_e-15,
        -7.647_122_191_181_588_332_884_84_e-13,
        1.605_904_306_056_645_016_290_54_e-10,
        -2.505_210_837_635_020_458_107_55_e-8,
        2.755_731_922_391_987_476_304_16_e-6,
        -0.000_198_412_698_412_696_162_806_809,
        0.008_333_333_333_333_329_748_238_15,
    )
    .mla(s, F64x::splat(-0.166_666_666_666_666_657_414_808));

    s * (u * d) + d
}

#[test]
fn test_cos() {
    test_f_f::<2>(cos, rug::Float::cos, f64::MIN..=f64::MAX, 3.5);
    test_f_f::<2>(cos_deterministic, rug::Float::cos, f64::MIN..=f64::MAX, 3.5);
}

/// Evaluate sine and cosine function simultaneously
///
/// Evaluates the sine and cosine functions of a value in ***a*** at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// The error bound of the returned values is `3.5 ULP`.
/// If ***a*** is a `NaN` or `infinity`, a `NaN` is returned.
pub fn sincos<const N: usize>(d: F64x<N>) -> (F64x<N>, F64x<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut s;
    let ql;

    if d.abs().simd_lt(F64x::TRIGRANGEMAX2).all() {
        let dql = (d * F64x::FRAC_2_PI).round();
        ql = dql.roundi();
        s = dql.mla(-F64x::PI_A2 * F64x::HALF, d);
        s = dql.mla(-F64x::PI_B2 * F64x::HALF, s);
    } else if d.abs().simd_lt(F64x::TRIGRANGEMAX).all() {
        let dqh = (d * (F64x::FRAC_2_PI / F64x::D1_24)).trunc();
        let dqh = dqh * F64x::D1_24;
        let dql = (d * F64x::FRAC_2_PI - dqh).round();
        ql = dql.roundi();

        s = dqh.mla(-F64x::PI_A * F64x::HALF, d);
        s = dql.mla(-F64x::PI_A * F64x::HALF, s);
        s = dqh.mla(-F64x::PI_B * F64x::HALF, s);
        s = dql.mla(-F64x::PI_B * F64x::HALF, s);
        s = dqh.mla(-F64x::PI_C * F64x::HALF, s);
        s = dql.mla(-F64x::PI_C * F64x::HALF, s);
        s = (dqh + dql).mla(-F64x::PI_D * F64x::HALF, s);
    } else {
        let (ddidd, ddii) = rempi(d);
        ql = ddii;
        s = F64x::from(ddidd);
        s = F64x::from_bits((d.is_infinite() | d.is_nan()).to_int().cast() | s.to_bits());
    }

    let t = s;

    s = s * s;

    let u = F64x::splat(1.589_383_072_832_289_373_285_11_e-10)
        .mla(s, F64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
        .mla(s, F64x::splat(2.755_731_317_768_463_605_125_47_e-6))
        .mla(s, F64x::splat(-0.000_198_412_698_278_911_770_864_914))
        .mla(s, F64x::splat(0.008_333_333_333_319_184_596_174_6))
        .mla(s, F64x::splat(-0.166_666_666_666_666_130_709_393));

    let rx = (u * s).mla(t, t);
    let rx = d.is_neg_zero().select(F64x::NEG_ZERO, rx);

    let u = F64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
        .mla(s, F64x::splat(2.087_574_712_070_400_554_793_66_e-9))
        .mla(s, F64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
        .mla(s, F64x::splat(2.480_158_728_900_018_673_119_15_e-5))
        .mla(s, F64x::splat(-0.001_388_888_888_887_140_192_823_29))
        .mla(s, F64x::splat(0.041_666_666_666_666_551_959_206_2))
        .mla(s, F64x::splat(-0.5));

    let ry = s.mla(u, F64x::ONE);

    let o = (ql & Ix::splat(1)).simd_eq(Ix::splat(0)).cast();
    let mut rsin = o.select(rx, ry);
    let mut rcos = o.select(ry, rx);

    let o = (ql & Ix::splat(2)).simd_eq(Ix::splat(2)).cast::<i64>();
    rsin = F64x::from_bits((o.to_int().cast() & F64x::NEG_ZERO.to_bits()) ^ rsin.to_bits());

    let o = ((ql + Ix::splat(1)) & Ix::splat(2))
        .simd_eq(Ix::splat(2))
        .cast::<i64>();
    rcos = F64x::from_bits((o.to_int().cast() & F64x::NEG_ZERO.to_bits()) ^ rcos.to_bits());

    (rsin, rcos)
}

/// Evaluate sine and cosine function simultaneously
///
/// Evaluates the sine and cosine functions of a value in ***a*** at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// The error bound of the returned values is `3.5 ULP`.
/// If ***a*** is a `NaN` or `infinity`, a `NaN` is returned.
///
/// NOTE: This version is slower, but SIMD lanes are independent
pub fn sincos_deterministic<const N: usize>(d: F64x<N>) -> (F64x<N>, F64x<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut s = d;

    let dql = (s * F64x::FRAC_2_PI).round();
    let mut ql = dql.roundi();
    s = dql.mla(-F64x::PI_A2 * F64x::HALF, s);
    s = dql.mla(-F64x::PI_B2 * F64x::HALF, s);
    let g = d.abs().simd_lt(F64x::TRIGRANGEMAX2);

    if !g.all() {
        let mut dqh = (d * F64x::FRAC_2_PI / F64x::D1_24).trunc();
        dqh *= F64x::D1_24;
        let dql = (d * F64x::FRAC_2_PI - dqh).round();

        let mut u = dqh.mla(-F64x::PI_A * F64x::HALF, d);
        u = dql.mla(-F64x::PI_A * F64x::HALF, u);
        u = dqh.mla(-F64x::PI_B * F64x::HALF, u);
        u = dql.mla(-F64x::PI_B * F64x::HALF, u);
        u = dqh.mla(-F64x::PI_C * F64x::HALF, u);
        u = dql.mla(-F64x::PI_C * F64x::HALF, u);
        u = (dqh + dql).mla(-F64x::PI_D * F64x::HALF, u);

        ql = g.cast().select(ql, dql.roundi());
        s = g.select(s, u);
        let g = d.abs().simd_lt(F64x::TRIGRANGEMAX);

        if !g.all() {
            let (ddidd, ddii) = rempi(d);
            let mut u = F64x::from(ddidd);
            u = F64x::from_bits((d.is_infinite() | d.is_nan()).to_int().cast() | u.to_bits());

            ql = g.cast().select(ql, ddii);
            s = g.select(s, u);
        }
    }

    let t = s;

    s = s * s;

    let u = F64x::splat(1.589_383_072_832_289_373_285_11_e-10)
        .mla(s, F64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
        .mla(s, F64x::splat(2.755_731_317_768_463_605_125_47_e-6))
        .mla(s, F64x::splat(-0.000_198_412_698_278_911_770_864_914))
        .mla(s, F64x::splat(0.008_333_333_333_319_184_596_174_6))
        .mla(s, F64x::splat(-0.166_666_666_666_666_130_709_393));

    let mut rx = (u * s).mla(t, t);
    rx = d.is_neg_zero().select(F64x::NEG_ZERO, rx);

    let u = F64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
        .mla(s, F64x::splat(2.087_574_712_070_400_554_793_66_e-9))
        .mla(s, F64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
        .mla(s, F64x::splat(2.480_158_728_900_018_673_119_15_e-5))
        .mla(s, F64x::splat(-0.001_388_888_888_887_140_192_823_29))
        .mla(s, F64x::splat(0.041_666_666_666_666_551_959_206_2))
        .mla(s, F64x::splat(-0.5));

    let ry = s.mla(u, F64x::ONE);

    let o = (ql & Ix::splat(1)).simd_eq(Ix::splat(0)).cast();
    let mut rsin = o.select(rx, ry);
    let mut rcos = o.select(ry, rx);

    let o = (ql & Ix::splat(2)).simd_eq(Ix::splat(2)).cast::<i64>();
    rsin = F64x::from_bits((o.to_int().cast() & F64x::NEG_ZERO.to_bits()) ^ rsin.to_bits());

    let o = ((ql + Ix::splat(1)) & Ix::splat(2))
        .simd_eq(Ix::splat(2))
        .cast::<i64>();
    rcos = F64x::from_bits((o.to_int().cast() & F64x::NEG_ZERO.to_bits()) ^ rcos.to_bits());

    (rsin, rcos)
}

#[test]
fn test_sincos() {
    test_f_ff::<2>(
        sincos,
        |in1| {
            let prec = in1.prec();
            in1.sin_cos(rug::Float::new(prec))
        },
        f64::MIN..=f64::MAX,
        3.5,
    );
    test_f_ff::<2>(
        sincos_deterministic,
        |in1| {
            let prec = in1.prec();
            in1.sin_cos(rug::Float::new(prec))
        },
        f64::MIN..=f64::MAX,
        3.5,
    );
}

/// Tangent function
///
/// These functions evaluates the tangent function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn tan<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let ql;

    let mut x;

    if d.abs().simd_lt(F64x::TRIGRANGEMAX2).all() {
        let dql = (d * F64x::FRAC_2_PI).round();
        ql = dql.roundi();
        x = dql.mla(-F64x::PI_A2 * F64x::HALF, d);
        x = dql.mla(-F64x::PI_B2 * F64x::HALF, x);
    } else if d.abs().simd_lt(F64x::splat(1e+6)).all() {
        let dqh = (d * (F64x::FRAC_2_PI / F64x::D1_24)).trunc();
        let dqh = dqh * F64x::D1_24;
        let dql = (d * F64x::FRAC_2_PI - dqh).round();
        ql = dql.roundi();

        x = dqh.mla(-F64x::PI_A * F64x::HALF, d);
        x = dql.mla(-F64x::PI_A * F64x::HALF, x);
        x = dqh.mla(-F64x::PI_B * F64x::HALF, x);
        x = dql.mla(-F64x::PI_B * F64x::HALF, x);
        x = dqh.mla(-F64x::PI_C * F64x::HALF, x);
        x = dql.mla(-F64x::PI_C * F64x::HALF, x);
        x = (dqh + dql).mla(-F64x::PI_D * F64x::HALF, x);
    } else {
        let (ddidd, ddii) = rempi(d);
        ql = ddii;
        x = F64x::from(ddidd);
        x = F64x::from_bits(d.is_infinite().to_int().cast() | x.to_bits());
        x = F64x::from_bits((d.is_infinite() | d.is_nan()).to_int().cast() | x.to_bits());
    }

    x *= F64x::HALF;
    let s = x * x;

    let s2 = s * s;
    let s4 = s2 * s2;

    let mut u = F64x::poly8(
        s,
        s2,
        s4,
        0.324_509_882_663_927_631_6_e-3,
        0.561_921_973_811_432_373_5_e-3,
        0.146_078_150_240_278_449_4_e-2,
        0.359_161_154_079_249_951_9_e-2,
        0.886_326_840_956_311_312_6_e-2,
        0.218_694_872_818_553_549_8_e-1,
        0.539_682_539_951_727_297_e-1,
        0.133_333_333_333_050_058_1,
    )
    .mla(s, F64x::splat(0.333_333_333_333_334_369_5));
    u = s.mla(u * x, x);

    let y = u.mla(u, -F64x::ONE);
    x = u * F64x::splat(-2.);

    let o = (ql & Ix::splat(1)).simd_eq(Ix::splat(1)).cast();

    u = o.select(-y, x) / o.select(x, y);
    d.simd_eq(F64x::ZERO).select(d, u)
}

/// Tangent function
///
/// These functions evaluates the tangent function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
///
/// NOTE: This version is slower, but SIMD lanes are independent
pub fn tan_deterministic<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let dql = (d * F64x::FRAC_2_PI).round();
    let mut ql = dql.roundi();
    let mut s = dql.mla(-F64x::PI_A2 * F64x::HALF, d);
    s = dql.mla(-F64x::PI_B2 * F64x::HALF, s);
    let g = d.abs().simd_lt(F64x::TRIGRANGEMAX2);

    if !g.all() {
        let mut dqh = (d * F64x::FRAC_2_PI / F64x::D1_24).trunc();
        dqh *= F64x::D1_24;
        let dql = (d * F64x::FRAC_2_PI - dqh).round();

        let mut u = dqh.mla(-F64x::PI_A * F64x::HALF, d);
        u = dql.mla(-F64x::PI_A * F64x::HALF, u);
        u = dqh.mla(-F64x::PI_B * F64x::HALF, u);
        u = dql.mla(-F64x::PI_B * F64x::HALF, u);
        u = dqh.mla(-F64x::PI_C * F64x::HALF, u);
        u = dql.mla(-F64x::PI_C * F64x::HALF, u);
        u = (dqh + dql).mla(-F64x::PI_D * F64x::HALF, u);

        ql = g.cast().select(ql, dql.roundi());
        s = g.select(s, u);
        let g = d.abs().simd_lt(F64x::splat(1e+6));

        if !g.all() {
            let (ddidd, ddii) = rempi(d);
            let ql2 = ddii;
            let mut u = F64x::from(ddidd);
            u = F64x::from_bits((d.is_infinite() | d.is_nan()).to_int().cast() | u.to_bits());

            ql = g.cast().select(ql, ql2);
            s = g.select(s, u);
        }
    }

    let x = s * F64x::HALF;
    let s = x * x;

    let s2 = s * s;
    let s4 = s2 * s2;

    let mut u = F64x::poly8(
        s,
        s2,
        s4,
        0.324_509_882_663_927_631_6_e-3,
        0.561_921_973_811_432_373_5_e-3,
        0.146_078_150_240_278_449_4_e-2,
        0.359_161_154_079_249_951_9_e-2,
        0.886_326_840_956_311_312_6_e-2,
        0.218_694_872_818_553_549_8_e-1,
        0.539_682_539_951_727_297_e-1,
        0.133_333_333_333_050_058_1,
    )
    .mla(s, F64x::splat(0.333_333_333_333_334_369_5));
    u = s.mla(u * x, x);

    let y = u.mla(u, -F64x::ONE);
    let x = u * F64x::splat(-2.);

    let o = (ql & Ix::splat(1)).simd_eq(Ix::splat(1)).cast();
    u = o.select(-y, x) / o.select(x, y);
    d.simd_eq(F64x::ZERO).select(d, u)
}

#[test]
fn test_tan() {
    test_f_f::<2>(tan, rug::Float::tan, f64::MIN..=f64::MAX, 3.5);
    test_f_f::<2>(tan_deterministic, rug::Float::tan, f64::MIN..=f64::MAX, 3.5);
}

/// Evaluate sin( π**a** ) and cos( π**a** ) for given **a** simultaneously
///
/// Evaluates the sine and cosine functions of π**a** at a time,
/// and store the two values in *first* and *second* position in the returned value, respectively.
/// The error bound of the returned values is `3.5 ULP` if ***a*** is in `[-1e+7, 1e+7]`.
/// If a is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
/// If a is a `NaN` or `infinity`, a `NaN` is returned.
pub fn sincospi<const N: usize>(d: F64x<N>) -> (F64x<N>, F64x<N>)
where
    LaneCount<N>: SupportedLaneCount,
{
    let u = d * F64x::splat(4.);
    let mut q = u.trunci();
    q = (q + ((q.cast() >> Ux::splat(31)).cast() ^ Ix::splat(1))) & Ix::splat(!1);
    let s = u - q.cast();

    let t = s;
    let s = s * s;

    //

    let u = F64x::splat(0.688_063_889_476_606_013_6_e-11)
        .mla(s, F64x::splat(-0.175_715_956_454_231_019_9_e-8))
        .mla(s, F64x::splat(0.313_361_632_725_786_731_1_e-6))
        .mla(s, F64x::splat(-0.365_762_041_638_848_645_2_e-4))
        .mla(s, F64x::splat(0.249_039_457_018_993_210_3_e-2))
        .mla(s, F64x::splat(-0.807_455_121_882_805_632_e-1))
        .mla(s, F64x::splat(0.785_398_163_397_448_279));

    let rx = u * t;

    let u = F64x::splat(-0.386_014_121_368_379_435_2_e-12)
        .mla(s, F64x::splat(0.115_005_788_802_968_141_5_e-9))
        .mla(s, F64x::splat(-0.246_113_649_300_666_355_3_e-7))
        .mla(s, F64x::splat(0.359_086_044_662_351_671_3_e-5))
        .mla(s, F64x::splat(-0.325_991_886_926_943_594_2_e-3))
        .mla(s, F64x::splat(0.158_543_442_438_154_116_9_e-1))
        .mla(s, F64x::splat(-0.308_425_137_534_042_437_3))
        .mla(s, F64x::ONE);

    let ry = u;

    let o = (q & Ix::splat(2)).simd_eq(Ix::splat(0)).cast();
    let mut rsin = o.select(rx, ry);
    let mut rcos = o.select(ry, rx);

    let o = (q & Ix::splat(4)).simd_eq(Ix::splat(4)).cast::<i64>();
    rsin = F64x::from_bits((o.to_int().cast() & F64x::NEG_ZERO.to_bits()) ^ rsin.to_bits());

    let o = ((q + Ix::splat(2)) & Ix::splat(4))
        .simd_eq(Ix::splat(4))
        .cast::<i64>();
    rcos = F64x::from_bits((o.to_int().cast() & F64x::NEG_ZERO.to_bits()) ^ rcos.to_bits());

    let o = d.abs().simd_gt(F64x::TRIGRANGEMAX3 / F64x::splat(4.));
    rsin = F64x::from_bits(!o.to_int().cast::<u64>() & rsin.to_bits());
    rcos = F64x::from_bits(!o.to_int().cast::<u64>() & rcos.to_bits());

    let o = d.is_infinite();
    rsin = F64x::from_bits(o.to_int().cast() | rsin.to_bits());
    rcos = F64x::from_bits(o.to_int().cast() | rcos.to_bits());

    (rsin, rcos)
}

#[test]
fn test_sincospi() {
    use rug::{float::Constant, Float};
    let rangemax2 = 1e+9 / 4.;
    test_f_ff::<2>(
        sincospi,
        |mut in1| {
            let prec = in1.prec();
            in1.set_prec(prec * 2);
            (in1 * Float::with_val(prec * 2, Constant::Pi)).sin_cos(Float::new(prec))
        },
        -rangemax2..=rangemax2,
        1.5,
    );
}

#[inline]
fn atan2k<const N: usize>(y: F64x<N>, x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let q = x.is_sign_negative().cast().to_int() & Ix::splat(-2);
    let x = x.abs();

    let q = x.simd_lt(y).cast().select(q + Ix::splat(1), q);
    let p = x.simd_lt(y);
    let s = p.select(-x, y);
    let mut t = x.simd_max(y);

    let s = s / t;
    t = s * s;

    let t2 = t * t;
    let t4 = t2 * t2;
    let t8 = t4 * t4;
    let t16 = t8 * t8;

    let u = F64x::poly19(
        t,
        t2,
        t4,
        t8,
        t16,
        -1.887_960_084_630_734_965_637_46_e-05,
        0.000_209_850_076_645_816_976_906_797,
        -0.001_106_118_314_866_724_825_634_71,
        0.003_700_267_441_887_131_192_324_03,
        -0.008_898_961_958_876_554_917_408_09,
        0.016_599_329_773_529_201_970_117,
        -0.025_451_762_493_231_264_161_686_1,
        0.033_785_258_000_135_306_999_389_7,
        -0.040_762_919_127_683_650_000_193_4,
        0.046_666_715_007_784_062_563_267_5,
        -0.052_367_485_230_348_245_761_611_3,
        0.058_766_639_292_667_358_085_431_3,
        -0.066_657_357_936_108_052_598_456_2,
        0.076_921_953_831_176_961_835_502_9,
        -0.090_908_995_008_245_008_229_153,
        0.111_111_105_648_261_418_443_745,
        -0.142_857_142_667_713_293_837_65,
        0.199_999_999_996_591_265_594_148,
        -0.333_333_333_333_311_110_369_124,
    );

    t = s.mla(t * u, s);
    q.cast::<f64>().mla(F64x::FRAC_PI_2, t)
}

/// Arc tangent function of two variables
///
/// These functions evaluates the arc tangent function of (***y*** / ***x***).
/// The quadrant of the result is determined according to the signs of ***x*** and ***y***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn atan2<const N: usize>(y: F64x<N>, x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut r = atan2k(y.abs(), x);

    r = r.mul_sign(x);
    r = (x.is_infinite() | x.simd_eq(F64x::ZERO)).select(
        F64x::FRAC_PI_2 - visinf2_vd_vd_vd(x, F64x::FRAC_PI_2.mul_sign(x)),
        r,
    );
    r = y.is_infinite().select(
        F64x::FRAC_PI_2 - visinf2_vd_vd_vd(x, F64x::FRAC_PI_4.mul_sign(x)),
        r,
    );
    r = y.simd_eq(F64x::ZERO).select(
        F64x::from_bits(x.is_sign_negative().to_int().cast() & F64x::PI.to_bits()),
        r,
    );

    F64x::from_bits((x.is_nan() | y.is_nan()).to_int().cast() | r.mul_sign(y).to_bits())
}

#[test]
fn test_atan2() {
    test_ff_f::<2>(
        atan2,
        rug::Float::atan2,
        f64::MIN..=f64::MAX,
        f64::MIN..=f64::MAX,
        3.5,
    );
}

/// Arc sine function
///
/// These functions evaluates the arc sine function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn asin<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let o = d.abs().simd_lt(F64x::HALF);
    let x2 = o.select(d * d, (F64x::ONE - d.abs()) * F64x::HALF);
    let x = o.select(d.abs(), x2.sqrt());

    let x4 = x2 * x2;
    let x8 = x4 * x4;
    let x16 = x8 * x8;

    let mut u = F64x::poly12(
        x2,
        x4,
        x8,
        x16,
        0.316_158_765_065_393_462_8_e-1,
        -0.158_191_824_332_999_664_3_e-1,
        0.192_904_547_726_791_067_4_e-1,
        0.660_607_747_627_717_061_e-2,
        0.121_536_052_557_737_733_1_e-1,
        0.138_871_518_450_160_921_8_e-1,
        0.173_595_699_122_361_460_4_e-1,
        0.223_717_618_193_204_834_1_e-1,
        0.303_819_592_803_813_223_7_e-1,
        0.446_428_568_137_710_243_8_e-1,
        0.750_000_000_037_858_161_1_e-1,
        0.166_666_666_666_649_754_3,
    );

    u = u.mla(x * x2, x);

    let r = o.select(u, u.mla(F64x::splat(-2.), F64x::FRAC_PI_2));
    r.mul_sign(d)
}

#[test]
fn test_asin() {
    test_f_f::<2>(asin, rug::Float::asin, -1.0..=1.0, 3.5);
}

/// Arc cosine function
///
/// These functions evaluates the arc cosine function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn acos<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let o = d.abs().simd_lt(F64x::HALF);
    let x2 = o.select(d * d, (F64x::ONE - d.abs()) * F64x::HALF);
    let mut x = o.select(d.abs(), x2.sqrt());
    x = d.abs().simd_eq(F64x::ONE).select(F64x::ZERO, x);

    let x4 = x2 * x2;
    let x8 = x4 * x4;
    let x16 = x8 * x8;

    let mut u = F64x::poly12(
        x2,
        x4,
        x8,
        x16,
        0.316_158_765_065_393_462_8_e-1,
        -0.158_191_824_332_999_664_3_e-1,
        0.192_904_547_726_791_067_4_e-1,
        0.660_607_747_627_717_061_e-2,
        0.121_536_052_557_737_733_1_e-1,
        0.138_871_518_450_160_921_8_e-1,
        0.173_595_699_122_361_460_4_e-1,
        0.223_717_618_193_204_834_1_e-1,
        0.303_819_592_803_813_223_7_e-1,
        0.446_428_568_137_710_243_8_e-1,
        0.750_000_000_037_858_161_1_e-1,
        0.166_666_666_666_649_754_3,
    );

    u *= x2 * x;

    let y = F64x::FRAC_PI_2 - (x.mul_sign(d) + u.mul_sign(d));
    x += u;
    let r = o.select(y, x * F64x::splat(2.));
    (!o & d.simd_lt(F64x::ZERO)).select(
        Doubled::<F64x<N>>::splat(crate::f64::D_PI)
            .add_checked(-r)
            .0,
        r,
    )
}

#[test]
fn test_acos() {
    test_f_f::<2>(acos, rug::Float::acos, -1.0..=1.0, 3.5);
}

/// Arc tangent function
///
/// These functions evaluates the arc tangent function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn atan<const N: usize>(mut s: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    /*if cfg!(feature = "__intel_compiler") {
        // && defined(ENABLE_PURECFMA_SCALAR)
        let w = s;
    }*/

    let q = s.is_sign_negative().cast().to_int() & Ix::splat(2);
    s = s.abs();

    let q = F64x::ONE.simd_lt(s).cast().select(q + Ix::splat(1), q);
    s = F64x::ONE.simd_lt(s).select(s.recip(), s);

    let mut t = s * s;

    let t2 = t * t;
    let t4 = t2 * t2;
    let t8 = t4 * t4;
    let t16 = t8 * t8;

    let u = F64x::poly19(
        t,
        t2,
        t4,
        t8,
        t16,
        -1.887_960_084_630_734_965_637_46_e-5,
        0.000_209_850_076_645_816_976_906_797,
        -0.001_106_118_314_866_724_825_634_71,
        0.003_700_267_441_887_131_192_324_03,
        -0.008_898_961_958_876_554_917_408_09,
        0.016_599_329_773_529_201_970_117,
        -0.025_451_762_493_231_264_161_686_1,
        0.033_785_258_000_135_306_999_389_7,
        -0.040_762_919_127_683_650_000_193_4,
        0.046_666_715_007_784_062_563_267_5,
        -0.052_367_485_230_348_245_761_611_3,
        0.058_766_639_292_667_358_085_431_3,
        -0.066_657_357_936_108_052_598_456_2,
        0.076_921_953_831_176_961_835_502_9,
        -0.090_908_995_008_245_008_229_153,
        0.111_111_105_648_261_418_443_745,
        -0.142_857_142_667_713_293_837_65,
        0.199_999_999_996_591_265_594_148,
        -0.333_333_333_333_311_110_369_124,
    );

    t = s.mla(t * u, s);

    t = (q & Ix::splat(1))
        .simd_eq(Ix::splat(1))
        .cast()
        .select(F64x::FRAC_PI_2 - t, t);
    t = F64x::from_bits(
        ((q & Ix::splat(2)).simd_eq(Ix::splat(2)).to_int().cast() & F64x::NEG_ZERO.to_bits())
            ^ t.to_bits(),
    );

    /*if cfg!(feature = "__intel_compiler") {
        // && defined(ENABLE_PURECFMA_SCALAR)
        t = w.simd_eq(F64x::ZERO).select(w, t);
    }*/
    t
}

#[test]
fn test_atan() {
    test_f_f::<2>(atan, rug::Float::atan, f64::MIN..=f64::MAX, 3.5);
}

#[inline]
fn expm1k<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut u = (d * F64x::R_LN2).round();
    let q = u.roundi();

    let s = u.mla(-F64x::L2_U, d);
    let s = u.mla(-F64x::L2_L, s);

    let s2 = s * s;
    let s4 = s2 * s2;
    let s8 = s4 * s4;

    u = F64x::poly10(
        s,
        s2,
        s4,
        s8,
        2.088_606_211_072_836_875_363_41_e-9,
        2.511_129_308_928_765_186_106_61_e-8,
        2.755_739_112_349_004_718_933_38_e-7,
        2.755_723_629_119_288_276_294_23_e-6,
        2.480_158_715_923_547_299_879_1_e-5,
        0.000_198_412_698_960_509_205_564_975,
        0.001_388_888_888_897_744_922_079_62,
        0.008_333_333_333_316_527_216_649_84,
        0.041_666_666_666_666_504_759_142_2,
        0.166_666_666_666_666_851_703_837,
    );

    u = s2.mla(F64x::HALF, s2 * s * u) + s;

    q.simd_eq(Ix::splat(0))
        .cast()
        .select(u, ldexp2k(u + F64x::ONE, q) - F64x::ONE)
}

/// Hyperbolic sine function
///
/// These functions evaluates the hyperbolic sine function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP` if ***a*** is in `[-709, 709]`.
/// If ***a*** is a finite value out of this range, infinity with a correct sign
/// or a correct value with `3.5 ULP` error bound is returned.
pub fn sinh<const N: usize>(x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let e = expm1k(x.abs());

    let mut y = (e + F64x::splat(2.)) / (e + F64x::ONE);
    y *= F64x::HALF * e;

    y = (x.abs().simd_gt(F64x::splat(709.)) | y.is_nan()).select(F64x::INFINITY, y);
    y = y.mul_sign(x);
    F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
}

#[test]
fn test_sinh() {
    test_f_f::<2>(sinh, rug::Float::sinh, -709.0..=709.0, 3.5);
}

/// Hyperbolic cosine function
///
/// These functions evaluates the hyperbolic cosine function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP` if a is in `[-709, 709]`.
/// If ***a*** is a finite value out of this range, infinity with a correct sign
/// or a correct value with `3.5 ULP` error bound is returned.
pub fn cosh<const N: usize>(x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let e = u10::exp(x.abs());
    let mut y = F64x::HALF.mla(e, F64x::HALF / e);

    y = (x.abs().simd_gt(F64x::splat(709.)) | y.is_nan()).select(F64x::INFINITY, y);
    F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
}

#[test]
fn test_cosh() {
    test_f_f::<2>(cosh, rug::Float::cosh, -709.0..=709.0, 3.5);
}

/// Hyperbolic tangent function
///
/// These functions evaluates the hyperbolic tangent function of a value in ***a***.
/// The error bound of the returned value is `3.5 ULP` for the double-precision
/// function or `3.5 ULP` for the single-precision function.
pub fn tanh<const N: usize>(x: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let d = expm1k(F64x::splat(2.) * x.abs());
    let mut y = d / (F64x::splat(2.) + d);

    y = (x.abs().simd_gt(F64x::splat(18.714_973_875)) | y.is_nan()).select(F64x::ONE, y);
    y = y.mul_sign(x);
    F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
}

#[test]
fn test_tanh() {
    test_f_f::<2>(tanh, rug::Float::tanh, -19.0..=19.0, 3.5);
}

/// Natural logarithmic function
///
/// These functions return the natural logarithm of ***a***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn log<const N: usize>(mut d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let m;

    let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE));
                d = o.select(d * (F64x::D1_32 * F64x::D1_32), d);
                let mut e = ilogb2k(d * F64x::splat(1. / 0.75));
                m = ldexp3k(d, -e);
                e = o.cast().select(e - Ix::splat(64), e);
                e.cast()
            }/* else {
                let mut e = vgetexp_vd_vd(d * F64x::splat(1. / 0.75));
                e = e.simd_eq(F64x::INFINITY).select(F64x::splat(1024.), e);
                m = vgetmant_vd_vd(d);
                e
            }*/;

    let mut x = (m - F64x::ONE) / (F64x::ONE + m);
    let x2 = x * x;

    let x4 = x2 * x2;
    let x8 = x4 * x4;
    let x3 = x * x2;

    let t = F64x::poly7(
        x2,
        x4,
        x8,
        0.153_487_338_491_425_068_243_146,
        0.152_519_917_006_351_951_593_857,
        0.181_863_266_251_982_985_677_316,
        0.222_221_366_518_767_365_905_163,
        0.285_714_294_746_548_025_383_248,
        0.399_999_999_950_799_600_689_777,
        0.666_666_666_666_777_874_006_3,
    );

    /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
    x = x.mla(
        F64x::splat(2.),
        F64x::splat(0.693_147_180_559_945_286_226_764) * ef,
    );
    x = x3.mla(t, x);

    x = d.simd_eq(F64x::INFINITY).select(F64x::INFINITY, x);
    x = (d.simd_lt(F64x::ZERO) | d.is_nan()).select(F64x::NAN, x);
    d.simd_eq(F64x::ZERO).select(F64x::NEG_INFINITY, x)
    /* } else {
        x = x.mla(F64x::splat(2.), F64x::splat(0.693_147_180_559_945_286_226_764) * ef);
        x = x3.mla(t, x);
        vfixup_vd_vd_vd_vi2_i(x, d, I64x::splat((5 << (5 * 4))), 0)
    }*/
}

#[test]
fn test_log() {
    test_f_f::<2>(log, rug::Float::ln, 0.0..=f64::MAX, 3.5);
}

/// Base-10 logarithmic function
///
/// This function returns the base-10 logarithm of ***a***.
pub fn log2<const N: usize>(mut d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let (m, e) = //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")
    {
        let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE));
        d = o.select(d * (F64x::D1_32 * F64x::D1_32), d);
        let e = ilogb2k(d * F64x::splat(1./0.75));
        (ldexp3k(d, -e), o.cast().select(e - Ix::splat(64), e))
    /*} else {
        vdouble e = vgetexp_vd_vd(d * F64x::splat(1./0.75));
        (vgetmant_vd_vd(d), e.simd_eq(F64x::INFINITY).select(F64x::splat(1024.), e))
    */};

    let x = (m - F64x::ONE) / (m + F64x::ONE);
    let x2 = x * x;

    let t = F64x::splat(0.221_194_175_045_608_149)
        .mla(x2, F64x::splat(0.220_076_869_315_227_768_9))
        .mla(x2, F64x::splat(0.262_370_805_748_851_465_6))
        .mla(x2, F64x::splat(0.320_597_747_794_449_550_2))
        .mla(x2, F64x::splat(0.412_198_594_548_532_470_9))
        .mla(x2, F64x::splat(0.577_078_016_299_705_898_2))
        .mla(x2, F64x::splat(0.961_796_693_926_080_914_49));

    let s = //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")
    {
        e.cast::<f64>().add_checked(x.mul_as_doubled(F64x::splat(2.885_390_081_777_926_774)))
    /* } else {
        e.add_checked(x.mul_as_doubled(F64x::splat(2.885_390_081_777_926_774)))
    */ };

    let mut r = t.mla(x * x2, F64x::from(s));

    //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {
    r = d.simd_eq(F64x::INFINITY).select(F64x::INFINITY, r);
    r = (d.simd_lt(F64x::ZERO) | d.is_nan()).select(F64x::NAN, r);
    r = d.simd_eq(F64x::ZERO).select(F64x::NEG_INFINITY, r);
    /* } else {
        r = vfixup_vd_vd_vd_vi2_i(r, d, I32::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
    }*/

    r
}

#[test]
fn test_log2() {
    test_f_f::<2>(log2, rug::Float::log2, 0.0..=f64::MAX, 3.5);
}

/// Base-10 exponential function
///
/// This function returns 10 raised to ***a***.
pub fn exp10<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut u = (d * F64x::LOG10_2).round();
    let q = u.roundi();

    let mut s = u.mla(-F64x::L10_U, d);
    s = u.mla(-F64x::L10_L, s);

    let s2 = s * s;
    let s4 = s2 * s2;
    let s8 = s4 * s4;
    u = F64x::poly11(
        s,
        s2,
        s4,
        s8,
        0.241_146_349_833_426_765_2_e-3,
        0.115_748_841_521_718_737_5_e-2,
        0.501_397_554_678_973_365_9_e-2,
        0.195_976_232_072_053_308_e-1,
        0.680_893_639_944_678_413_8_e-1,
        0.206_995_849_472_267_623_4,
        0.539_382_929_205_853_622_9,
        0.117_125_514_890_854_165_5_e+1,
        0.203_467_859_229_343_295_3_e+1,
        0.265_094_905_523_920_587_6_e+1,
        0.230_258_509_299_404_590_1_e+1,
    );

    u = u.mla(s, F64x::ONE);

    u = ldexp2k(u, q);

    u = d
        .simd_gt(F64x::splat(308.254_715_559_916_71))
        .select(F64x::INFINITY, u);
    F64x::from_bits(!d.simd_lt(F64x::splat(-350.)).to_int().cast::<u64>() & u.to_bits())
}

#[test]
fn test_exp10() {
    test_f_f::<2>(exp10, rug::Float::exp10, -350.0..=308.26, 3.5);
}

/// Base-2 exponential function
///
/// This function returns `2` raised to ***a***.
pub fn exp2<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut u = d.round();
    let q = u.roundi();

    let s = d - u;

    let s2 = s * s;
    let s4 = s2 * s2;
    let s8 = s4 * s4;
    u = F64x::poly10(
        s,
        s2,
        s4,
        s8,
        0.443_435_908_292_652_945_4_e-9,
        0.707_316_459_808_570_742_5_e-8,
        0.101_781_926_092_176_045_1_e-6,
        0.132_154_387_251_132_761_5_e-5,
        0.152_527_335_351_758_473_e-4,
        0.154_035_304_510_114_780_8_e-3,
        0.133_335_581_467_049_907_3_e-2,
        0.961_812_910_759_760_053_6_e-2,
        0.555_041_086_648_204_659_6_e-1,
        0.240_226_506_959_101_221_4,
    );
    u = u.mla(s, F64x::splat(0.693_147_180_559_945_286_2));

    u = u.mla(s, F64x::ONE);

    u = ldexp2k(u, q);

    u = d.simd_ge(F64x::splat(1024.)).select(F64x::INFINITY, u);
    F64x::from_bits(!d.simd_lt(F64x::splat(-2000.)).to_int().cast::<u64>() & u.to_bits())
}

#[test]
fn test_exp2() {
    test_f_f::<2>(exp2, rug::Float::exp2, -2000.0..=1024.0, 3.5);
}

/// Square root function
///
/// The error bound of the returned value is `3.5 ULP`.
pub fn sqrt<const N: usize>(d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    u05::sqrt(d)
}

/// Cube root function
///
/// These functions return the real cube root of ***a***.
/// The error bound of the returned value is `3.5 ULP`.
pub fn cbrt<const N: usize>(mut d: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let mut q = F64x::ONE;
    /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
        let s = d;
    }*/
    let e = ilogbk(d.abs()) + Ix::splat(1);
    d = ldexp2k(d, -e);

    let t = e.cast::<f64>() + F64x::splat(6144.);
    let qu = (t * F64x::splat(1. / 3.)).trunci();
    let re = (t - (qu.cast::<f64>() * F64x::splat(3.))).trunci();

    q = re
        .simd_eq(Ix::splat(1))
        .cast()
        .select(F64x::splat(1.259_921_049_894_873_164_767_210_6), q);
    q = re
        .simd_eq(Ix::splat(2))
        .cast()
        .select(F64x::splat(1.587_401_051_968_199_474_751_705_6), q);
    q = ldexp2k(q, qu - Ix::splat(2048));

    q = q.mul_sign(d);

    d = d.abs();

    let mut x = F64x::splat(-0.640_245_898_480_692_909_870_982)
        .mla(d, F64x::splat(2.961_551_030_200_395_118_185_95))
        .mla(d, F64x::splat(-5.733_530_609_229_478_436_361_66))
        .mla(d, F64x::splat(6.039_903_689_894_587_479_614_07))
        .mla(d, F64x::splat(-3.858_419_355_104_449_888_216_32))
        .mla(d, F64x::splat(2.230_727_530_249_660_972_572_2));

    let mut y = x * x;
    y = y * y;
    x -= d.mul_sub(y, x) * F64x::splat(1. / 3.);
    y = d * x * x;
    y = (y - F64x::splat(2. / 3.) * y * y.mla(x, F64x::splat(-1.))) * q;

    /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
        y = s.is_infinite().select(F64x::INFINITY.mul_sign(s), y);
        y = s
            .simd_eq(F64x::ZERO)
            .select(F64x::ZERO.mul_sign(s), y);
    }*/

    y
}

#[test]
fn test_cbrt() {
    test_f_f::<2>(cbrt, rug::Float::cbrt, f64::MIN..=f64::MAX, 3.5);
}

/// 2D Euclidian distance function
///
/// The error bound of the returned value is `3.5 ULP`.
pub fn hypot<const N: usize>(x: F64x<N>, y: F64x<N>) -> F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    let x = x.abs();
    let y = y.abs();
    let min = x.simd_min(y);
    let max = x.simd_max(y);

    let t = min / max;
    let mut ret = max * t.mla(t, F64x::ONE).sqrt();
    ret = min.simd_eq(F64x::ZERO).select(max, ret);
    ret = (x.is_nan() | y.is_nan()).select(F64x::NAN, ret);
    (x.simd_eq(F64x::INFINITY) | y.simd_eq(F64x::INFINITY)).select(F64x::INFINITY, ret)
}

#[test]
fn test_hypot() {
    test_ff_f::<2>(
        hypot,
        rug::Float::hypot,
        -1e307..=1e307,
        -1e307..=1e307,
        3.5,
    );
}
