macro_rules! impl_math_f64_u35 {
    () => {
        use super::*;

        /// Sine function
        ///
        /// These functions evaluates the sine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        #[cfg(not(feature = "deterministic"))]
        pub fn sin(mut d: F64x) -> F64x {
            let r = d;
            let mut ql;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * F64x::FRAC_1_PI).round();
                ql = dql.roundi();
                d = dql.mul_add(-PI_A2, d);
                d = dql.mul_add(-PI_B2, d);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = (d * (F64x::FRAC_1_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = d.mul_sub(F64x::FRAC_1_PI, dqh).round();
                ql = dql.roundi();

                d = dqh.mul_add(-PI_A, d);
                d = dql.mul_add(-PI_A, d);
                d = dqh.mul_add(-PI_B, d);
                d = dql.mul_add(-PI_B, d);
                d = dqh.mul_add(-PI_C, d);
                d = dql.mul_add(-PI_C, d);
                d = (dqh + dql).mul_add(-PI_D, d);
            } else {
                let (mut ddidd, ddii) = rempi(d);
                ql = ddii & Ix::splat(3);
                ql = ql + ql + Mx::from_cast(ddidd.0.gt(ZERO)).select(Ix::splat(2), Ix::splat(1));
                ql >>= 2;
                let o = (ddii & Ix::splat(1)).eq(Ix::splat(1));
                let mut x = Doubled::new(
                    F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(ddidd.0),
                    F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(ddidd.0),
                );
                x = ddidd + x;
                ddidd = M64x::from_cast(o).select_doubled(x, ddidd);
                d = F64x::from(ddidd);
                d = F64x::from_bits(U64x::from_bits(r.is_infinite() | r.is_nan()) | U64x::from_bits(d));
            }

            let s = d * d;

            d = F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(1))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(d),
            );

            let s2 = s * s;
            let s4 = s2 * s2;

            let mut u = F64x::poly8(s, s2, s4,
                -7.972_559_550_090_378_688_919_52_e-18,
                2.810_099_727_108_632_000_912_51_e-15,
                -7.647_122_191_181_588_332_884_84_e-13,
                1.605_904_306_056_645_016_290_54_e-10,
                -2.505_210_837_635_020_458_107_55_e-8,
                2.755_731_922_391_987_476_304_16_e-6,
                -0.000_198_412_698_412_696_162_806_809,
                0.008_333_333_333_333_329_748_238_15)
                .mul_add(s, F64x::splat(-0.166_666_666_666_666_657_414_808));

            u = s * (u * d) + d;

            r.is_neg_zero().select(r, u)
        }

        /// Sine function
        ///
        /// These functions evaluates the sine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        #[cfg(feature = "deterministic")]
        pub fn sin(mut d: F64x) -> F64x {
            // This is the deterministic implementation of sin function. Returned
            // values from deterministic functions are bitwise consistent across
            // all platforms. The function name xsin will be renamed to
            // Sleef_cinz_sind2_u35sse2 with renamesse2.h, for example. The
            // renaming by rename*.h is switched according to DETERMINISTIC macro.
            let r = d;

            let dql = (d * F64x::FRAC_1_PI).round();
            let mut ql = dql.roundi();
            d = dql.mul_add(-PI_A2, d);
            d = dql.mul_add(-PI_B2, d);
            let g = r.abs().lt(TRIGRANGEMAX2);

            if !g.all() {
                let mut dqh = (r * F64x::FRAC_1_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let dql = r.mul_sub(F64x::FRAC_1_PI, dqh).round();

                let mut u = dqh.mul_add(-PI_A, r);
                u = dql.mul_add(-PI_A, u);
                u = dqh.mul_add(-PI_B, u);
                u = dql.mul_add(-PI_B, u);
                u = dqh.mul_add(-PI_C, u);
                u = dql.mul_add(-PI_C, u);
                u = (dqh + dql).mul_add(-PI_D, u);

                ql = Mx::from_cast(g).select(ql, dql.roundi());
                d = g.select(d, u);
                let g = r.abs().lt(TRIGRANGEMAX);

                if !g.all() {
                    let (mut ddidd, ddii) = rempi(r);
                    let mut ql2 = ddii & Ix::splat(3);
                    ql2 = ql2 + ql2 + Mx::from_cast(ddidd.0.gt(ZERO)).select(Ix::splat(2), Ix::splat(1));
                    ql2 >>= 2;
                    let o = (ddii & Ix::splat(1)).eq(Ix::splat(1));
                    let mut x = Doubled::new(
                        F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(ddidd.0),
                        F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(ddidd.0),
                    );
                    x = ddidd + x;
                    ddidd = M64x::from_cast(o).select_doubled(x, ddidd);
                    let u = F64x::from(ddidd);
                    ql = Mx::from_cast(g).select(ql, ql2);
                    d = g.select(d, u);
                    d = F64x::from_bits(U64x::from_bits(r.is_infinite() | r.is_nan()) | U64x::from_bits(d));
                }
            }

            let s = d * d;

            d = F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(1))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(d),
            );

            let s2 = s * s;
            let s4 = s2 * s2;

            let mut u = F64x::poly8(s, s2, s4,
                -7.972_559_550_090_378_688_919_52_e-18,
                2.810_099_727_108_632_000_912_51_e-15,
                -7.647_122_191_181_588_332_884_84_e-13,
                1.605_904_306_056_645_016_290_54_e-10,
                -2.505_210_837_635_020_458_107_55_e-8,
                2.755_731_922_391_987_476_304_16_e-6,
                -0.000_198_412_698_412_696_162_806_809,
                0.008_333_333_333_333_329_748_238_15)
                .mul_add(s, F64x::splat(-0.166_666_666_666_666_657_414_808));

            u = s * (u * d) + d;

            r.is_neg_zero().select(r, u)
        }

        #[test]
        fn test_sin() {
            test_f_f(
                sin,
                rug::Float::sin,
                f64::MIN..=f64::MAX,
                3.5,
            );
        }

        /// Cosine function
        ///
        /// These functions evaluates the cosine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        #[cfg(not(feature = "deterministic"))]
        pub fn cos(mut d: F64x) -> F64x {
            let r = d;
            let mut ql;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql =
                    F64x::splat(2.).mul_add(d.mul_add(F64x::FRAC_1_PI, F64x::splat(-0.5)).round(), ONE);
                ql = dql.roundi();
                d = dql.mul_add(-PI_A2 * HALF, d);
                d = dql.mul_add(-PI_B2 * HALF, d);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = d
                    .mul_add(F64x::FRAC_1_PI / D1_23X, -F64x::FRAC_1_PI / D1_24X)
                    .trunc();
                ql = (d * F64x::FRAC_1_PI + dqh.mul_add(-D1_23X, F64x::splat(-0.5))).roundi();
                let dqh = dqh * D1_24X;
                ql = ql + ql + Ix::splat(1);
                let dql = F64x::from_cast(ql);

                d = dqh.mul_add(-PI_A * HALF, d);
                d = dql.mul_add(-PI_A * HALF, d);
                d = dqh.mul_add(-PI_B * HALF, d);
                d = dql.mul_add(-PI_B * HALF, d);
                d = dqh.mul_add(-PI_C * HALF, d);
                d = dql.mul_add(-PI_C * HALF, d);
                d = (dqh + dql).mul_add(-PI_D * HALF, d);
            } else {
                let (mut ddidd, ddii) = rempi(d);
                ql = ddii & Ix::splat(3);
                ql = ql + ql + Mx::from_cast(ddidd.0.gt(ZERO)).select(Ix::splat(8), Ix::splat(7));
                ql >>= 1;
                let o = (ddii & Ix::splat(1)).eq(Ix::splat(0));
                let y = ddidd.0.gt(ZERO).select(ZERO, F64x::splat(-1.));
                let mut x = Doubled::new(
                    F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(y),
                    F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(y),
                );
                x = ddidd + x;
                ddidd = M64x::from_cast(o).select_doubled(x, ddidd);
                d = F64x::from(ddidd);
                d = F64x::from_bits(U64x::from_bits(r.is_infinite() | r.is_nan()) | U64x::from_bits(d));
            }

            let s = d * d;

            d = F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((ql & Ix::splat(2)).eq(Ix::splat(0))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(d),
            );

            let s2 = s * s;
            let s4 = s2 * s2;

            let u = F64x::poly8(s, s2, s4,
                -7.972_559_550_090_378_688_919_52_e-18,
                2.810_099_727_108_632_000_912_51_e-15,
                -7.647_122_191_181_588_332_884_84_e-13,
                1.605_904_306_056_645_016_290_54_e-10,
                -2.505_210_837_635_020_458_107_55_e-8,
                2.755_731_922_391_987_476_304_16_e-6,
                -0.000_198_412_698_412_696_162_806_809,
                0.008_333_333_333_333_329_748_238_15)
                .mul_add(s, F64x::splat(-0.166_666_666_666_666_657_414_808));

            s * (u * d) + d
        }

        /// Cosine function
        ///
        /// These functions evaluates the cosine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        #[cfg(feature = "deterministic")]
        pub fn cos(mut d: F64x) -> F64x {
            let r = d;

            let g = d.abs().lt(TRIGRANGEMAX2);
            let dql = F64x::splat(2.).mul_add((d.mul_add(F64x::FRAC_1_PI, F64x::splat(-0.5))).round(), ONE);
            let mut ql = dql.roundi();
            d = dql.mul_add(-PI_A2 * HALF, d);
            d = dql.mul_add(-PI_B2 * HALF, d);

            if !g.all() {
                let mut dqh = (r.mul_add(F64x::FRAC_1_PI / D1_23X, -F64x::FRAC_1_PI / D1_24X)).trunc();
                let mut ql2 = (r * F64x::FRAC_1_PI + dqh.mul_add(-D1_23X, F64x::splat(-0.5))).roundi();
                dqh *= D1_24X;
                ql2 = ql2 + ql2 + Ix::splat(1);
                let dql = F64x::from_cast(ql2);

                let mut u = dqh.mul_add(-PI_A * HALF, r);
                u = dql.mul_add(-PI_A * HALF, u);
                u = dqh.mul_add(-PI_B * HALF, u);
                u = dql.mul_add(-PI_B * HALF, u);
                u = dqh.mul_add(-PI_C * HALF, u);
                u = dql.mul_add(-PI_C * HALF, u);
                u = (dqh + dql).mul_add(-PI_D * HALF, u);

                ql = Mx::from_cast(g).select(ql, ql2);
                d = g.select(d, u);
                let g = r.abs().lt(TRIGRANGEMAX);

                if !g.all() {
                    let (mut ddidd, ddii) = rempi(r);
                    let mut ql2 = ddii & Ix::splat(3);
                    ql2 = ql2 + ql2 + Mx::from_cast(ddidd.0.gt(ZERO)).select(Ix::splat(8), Ix::splat(7));
                    ql2 >>= 1;
                    let o = (ddii & Ix::splat(1)).eq(Ix::splat(0));
                    let y = ddidd.0.gt(ZERO).select(ZERO, F64x::splat(-1.));
                    let mut x = Doubled::new(
                        F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(y),
                        F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(y),
                    );
                    x = ddidd + x;
                    ddidd = M64x::from_cast(o).select_doubled(x, ddidd);
                    let u = F64x::from(ddidd);
                    ql = Mx::from_cast(g).select(ql, ql2);
                    d = g.select(d, u);
                    d = F64x::from_bits(U64x::from_bits(r.is_infinite() | r.is_nan()) | U64x::from_bits(d));
                }
            }

            let s = d * d;

            d = F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((ql & Ix::splat(2)).eq(Ix::splat(0))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(d),
            );

            let s2 = s * s;
            let s4 = s2 * s2;

            let u = F64x::poly8(s, s2, s4,
                -7.972_559_550_090_378_688_919_52_e-18,
                2.810_099_727_108_632_000_912_51_e-15,
                -7.647_122_191_181_588_332_884_84_e-13,
                1.605_904_306_056_645_016_290_54_e-10,
                -2.505_210_837_635_020_458_107_55_e-8,
                2.755_731_922_391_987_476_304_16_e-6,
                -0.000_198_412_698_412_696_162_806_809,
                0.008_333_333_333_333_329_748_238_15)
                .mul_add(s, F64x::splat(-0.166_666_666_666_666_657_414_808));

            s * (u * d) + d
        }

        #[test]
        fn test_cos() {
            test_f_f(
                cos,
                rug::Float::cos,
                f64::MIN..=f64::MAX,
                3.5,
            );
        }

        /// Evaluate sine and cosine function simultaneously
        ///
        /// Evaluates the sine and cosine functions of a value in ***a*** at a time,
        /// and store the two values in *first* and *second* position in the returned value, respectively.
        /// The error bound of the returned values is `3.5 ULP`.
        /// If ***a*** is a `NaN` or `infinity`, a `NaN` is returned.
        #[cfg(not(feature = "deterministic"))]
        pub fn sincos(d: F64x) -> (F64x, F64x) {
            let mut s: F64x;
            let ql: Ix;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * F64x::FRAC_2_PI).round();
                ql = dql.roundi();
                s = dql.mul_add(-PI_A2 * HALF, d);
                s = dql.mul_add(-PI_B2 * HALF, s);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = (d * (F64x::FRAC_2_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = (d * F64x::FRAC_2_PI - dqh).round();
                ql = dql.roundi();

                s = dqh.mul_add(-PI_A * HALF, d);
                s = dql.mul_add(-PI_A * HALF, s);
                s = dqh.mul_add(-PI_B * HALF, s);
                s = dql.mul_add(-PI_B * HALF, s);
                s = dqh.mul_add(-PI_C * HALF, s);
                s = dql.mul_add(-PI_C * HALF, s);
                s = (dqh + dql).mul_add(-PI_D * HALF, s);
            } else {
                let (ddidd, ddii) = rempi(d);
                ql = ddii;
                s = F64x::from(ddidd);
                s = F64x::from_bits(U64x::from_bits(d.is_infinite() | d.is_nan()) | U64x::from_bits(s));
            }

            let t = s;

            s = s * s;

            let u = F64x::splat(1.589_383_072_832_289_373_285_11_e-10)
                .mul_add(s, F64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
                .mul_add(s, F64x::splat(2.755_731_317_768_463_605_125_47_e-6))
                .mul_add(s, F64x::splat(-0.000_198_412_698_278_911_770_864_914))
                .mul_add(s, F64x::splat(0.008_333_333_333_319_184_596_174_6))
                .mul_add(s, F64x::splat(-0.166_666_666_666_666_130_709_393));

            let rx = (u * s).mul_add(t, t);
            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
                .mul_add(s, F64x::splat(2.087_574_712_070_400_554_793_66_e-9))
                .mul_add(s, F64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
                .mul_add(s, F64x::splat(2.480_158_728_900_018_673_119_15_e-5))
                .mul_add(s, F64x::splat(-0.001_388_888_888_887_140_192_823_29))
                .mul_add(s, F64x::splat(0.041_666_666_666_666_551_959_206_2))
                .mul_add(s, F64x::splat(-0.5));

            let ry = s.mul_add(u, ONE);

            let o = M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(0)));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = M64x::from_cast((ql & Ix::splat(2)).eq(Ix::splat(2)));
            rsin =
                F64x::from_bits((U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rsin));

            let o = M64x::from_cast(((ql + Ix::splat(1)) & Ix::splat(2)).eq(Ix::splat(2)));
            rcos =
                F64x::from_bits((U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rcos));

            (rsin, rcos)
        }

        /// Evaluate sine and cosine function simultaneously
        ///
        /// Evaluates the sine and cosine functions of a value in ***a*** at a time,
        /// and store the two values in *first* and *second* position in the returned value, respectively.
        /// The error bound of the returned values is `3.5 ULP`.
        /// If ***a*** is a `NaN` or `infinity`, a `NaN` is returned.
        #[cfg(feature = "deterministic")]
        pub fn sincos(d: F64x) -> (F64x, F64x) {
            let mut s = d;

            let dql = (s * F64x::FRAC_2_PI).round();
            let mut ql = dql.roundi();
            s = dql.mul_add(-PI_A2 * HALF, s);
            s = dql.mul_add(-PI_B2 * HALF, s);
            let g = d.abs().lt(TRIGRANGEMAX2);

            if !g.all() {
                let mut dqh = (d * F64x::FRAC_2_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let dql = (d * F64x::FRAC_2_PI - dqh).round();

                let mut u = dqh.mul_add(-PI_A * HALF, d);
                u = dql.mul_add(-PI_A * HALF, u);
                u = dqh.mul_add(-PI_B * HALF, u);
                u = dql.mul_add(-PI_B * HALF, u);
                u = dqh.mul_add(-PI_C * HALF, u);
                u = dql.mul_add(-PI_C * HALF, u);
                u = (dqh + dql).mul_add(-PI_D * HALF, u);

                ql = Mx::from_cast(g).select(ql, dql.roundi());
                s = g.select(s, u);
                let g = d.abs().lt(TRIGRANGEMAX);

                if !g.all() {
                    let (ddidd, ddii) = rempi(d);
                    let mut u = F64x::from(ddidd);
                    u = F64x::from_bits(U64x::from_bits(d.is_infinite() | d.is_nan()) | U64x::from_bits(u));

                    ql = Mx::from_cast(g).select(ql, ddii);
                    s = g.select(s, u);
                }
            }

            let t = s;

            s = s * s;

            let u = F64x::splat(1.589_383_072_832_289_373_285_11_e-10)
                .mul_add(s, F64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
                .mul_add(s, F64x::splat(2.755_731_317_768_463_605_125_47_e-6))
                .mul_add(s, F64x::splat(-0.000_198_412_698_278_911_770_864_914))
                .mul_add(s, F64x::splat(0.008_333_333_333_319_184_596_174_6))
                .mul_add(s, F64x::splat(-0.166_666_666_666_666_130_709_393));

            let mut rx = (u * s).mul_add(t, t);
            rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
                .mul_add(s, F64x::splat(2.087_574_712_070_400_554_793_66_e-9))
                .mul_add(s, F64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
                .mul_add(s, F64x::splat(2.480_158_728_900_018_673_119_15_e-5))
                .mul_add(s, F64x::splat(-0.001_388_888_888_887_140_192_823_29))
                .mul_add(s, F64x::splat(0.041_666_666_666_666_551_959_206_2))
                .mul_add(s, F64x::splat(-0.5));

            let ry = s.mul_add(u, ONE);

            let o = M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(0)));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = M64x::from_cast((ql & Ix::splat(2)).eq(Ix::splat(2)));
            rsin =
                F64x::from_bits((U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rsin));

            let o = M64x::from_cast(((ql + Ix::splat(1)) & Ix::splat(2)).eq(Ix::splat(2)));
            rcos =
                F64x::from_bits((U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rcos));

            (rsin, rcos)
        }

        #[test]
        fn test_sincos() {
            test_f_ff(
                sincos,
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
        #[cfg(not(feature = "deterministic"))]
        pub fn tan(d: F64x) -> F64x {
            let ql: Ix;

            let mut x: F64x;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * F64x::FRAC_2_PI).round();
                ql = dql.roundi();
                x = dql.mul_add(-PI_A2 * HALF, d);
                x = dql.mul_add(-PI_B2 * HALF, x);
            } else if d.abs().lt(F64x::splat(1e+6)).all() {
                let dqh = (d * (F64x::FRAC_2_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = (d * F64x::FRAC_2_PI - dqh).round();
                ql = dql.roundi();

                x = dqh.mul_add(-PI_A * HALF, d);
                x = dql.mul_add(-PI_A * HALF, x);
                x = dqh.mul_add(-PI_B * HALF, x);
                x = dql.mul_add(-PI_B * HALF, x);
                x = dqh.mul_add(-PI_C * HALF, x);
                x = dql.mul_add(-PI_C * HALF, x);
                x = (dqh + dql).mul_add(-PI_D * HALF, x);
            } else {
                let (ddidd, ddii) = rempi(d);
                ql = ddii;
                x = F64x::from(ddidd);
                x = F64x::from_bits(U64x::from_bits(d.is_infinite()) | U64x::from_bits(x));
                x = F64x::from_bits(U64x::from_bits(d.is_infinite() | d.is_nan()) | U64x::from_bits(x));
            }

            x *= HALF;
            let s = x * x;

            let s2 = s * s;
            let s4 = s2 * s2;

            let mut u = F64x::poly8(s, s2, s4,
                0.324_509_882_663_927_631_6_e-3,
                0.561_921_973_811_432_373_5_e-3,
                0.146_078_150_240_278_449_4_e-2,
                0.359_161_154_079_249_951_9_e-2,
                0.886_326_840_956_311_312_6_e-2,
                0.218_694_872_818_553_549_8_e-1,
                0.539_682_539_951_727_297_e-1,
                0.133_333_333_333_050_058_1)
                .mul_add(s, F64x::splat(0.333_333_333_333_334_369_5));
            u = s.mul_add(u * x, x);

            let y = u.mul_add(u, -ONE);
            x = u * F64x::splat(-2.);

            let o = M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(1)));

            u = o.select(-y, x) / o.select(x, y);
            d.eq(ZERO).select(d, u)
        }

        /// Tangent function
        ///
        /// These functions evaluates the tangent function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        #[cfg(feature = "deterministic")]
        pub fn tan(d: F64x) -> F64x {
            let dql = (d * F64x::FRAC_2_PI).round();
            let mut ql = dql.roundi();
            let mut s = dql.mul_add(-PI_A2 * HALF, d);
            s = dql.mul_add(-PI_B2 * HALF, s);
            let g = d.abs().lt(TRIGRANGEMAX2);

            if !g.all() {
                let mut dqh = (d * F64x::FRAC_2_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let dql = (d * F64x::FRAC_2_PI - dqh).round();

                let mut u = dqh.mul_add(-PI_A * HALF, d);
                u = dql.mul_add(-PI_A * HALF, u);
                u = dqh.mul_add(-PI_B * HALF, u);
                u = dql.mul_add(-PI_B * HALF, u);
                u = dqh.mul_add(-PI_C * HALF, u);
                u = dql.mul_add(-PI_C * HALF, u);
                u = (dqh + dql).mul_add(-PI_D * HALF, u);

                ql = Mx::from_cast(g).select(ql, dql.roundi());
                s = g.select(s, u);
                let g = d.abs().lt(F64x::splat(1e+6));

                if !g.all() {
                    let (ddidd, ddii) = rempi(d);
                    let ql2 = ddii;
                    let mut u = F64x::from(ddidd);
                    u = F64x::from_bits(U64x::from_bits(d.is_infinite() | d.is_nan()) | U64x::from_bits(u));

                    ql = Mx::from_cast(g).select(ql, ql2);
                    s = g.select(s, u);
                }
            }

            let x = s * HALF;
            let s = x * x;

            let s2 = s * s;
            let s4 = s2 * s2;

            let mut u = F64x::poly8(s, s2, s4,
                0.324_509_882_663_927_631_6_e-3,
                0.561_921_973_811_432_373_5_e-3,
                0.146_078_150_240_278_449_4_e-2,
                0.359_161_154_079_249_951_9_e-2,
                0.886_326_840_956_311_312_6_e-2,
                0.218_694_872_818_553_549_8_e-1,
                0.539_682_539_951_727_297_e-1,
                0.133_333_333_333_050_058_1)
                .mul_add(s, F64x::splat(0.333_333_333_333_334_369_5));
            u = s.mul_add(u * x, x);

            let y = u.mul_add(u, -ONE);
            let x = u * F64x::splat(-2.);

            let o = M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(1)));
            u = o.select(-y, x) / o.select(x, y);
            d.eq(ZERO).select(d, u)
        }

        #[test]
        fn test_tan() {
            test_f_f(
                tan,
                rug::Float::tan,
                f64::MIN..=f64::MAX,
                3.5,
            );
        }

        /// Evaluate sin( π**a** ) and cos( π**a** ) for given **a** simultaneously
        ///
        /// Evaluates the sine and cosine functions of π**a** at a time,
        /// and store the two values in *first* and *second* position in the returned value, respectively.
        /// The error bound of the returned values is `3.5 ULP` if ***a*** is in `[-1e+7, 1e+7]`.
        /// If a is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
        /// If a is a `NaN` or `infinity`, a `NaN` is returned.
        pub fn sincospi(d: F64x) -> (F64x, F64x) {
            let u = d * F64x::splat(4.);
            let mut q = u.trunci();
            q = (q + (Ix::from_bits(Ux::from_bits(q) >> 31) ^ Ix::splat(1))) & Ix::splat(!1);
            let s = u - F64x::from_cast(q);

            let t = s;
            let s = s * s;

            //

            let u = F64x::splat(0.688_063_889_476_606_013_6_e-11)
                .mul_add(s, F64x::splat(-0.175_715_956_454_231_019_9_e-8))
                .mul_add(s, F64x::splat(0.313_361_632_725_786_731_1_e-6))
                .mul_add(s, F64x::splat(-0.365_762_041_638_848_645_2_e-4))
                .mul_add(s, F64x::splat(0.249_039_457_018_993_210_3_e-2))
                .mul_add(s, F64x::splat(-0.807_455_121_882_805_632_e-1))
                .mul_add(s, F64x::splat(0.785_398_163_397_448_279));

            let rx = u * t;

            let u = F64x::splat(-0.386_014_121_368_379_435_2_e-12)
                .mul_add(s, F64x::splat(0.115_005_788_802_968_141_5_e-9))
                .mul_add(s, F64x::splat(-0.246_113_649_300_666_355_3_e-7))
                .mul_add(s, F64x::splat(0.359_086_044_662_351_671_3_e-5))
                .mul_add(s, F64x::splat(-0.325_991_886_926_943_594_2_e-3))
                .mul_add(s, F64x::splat(0.158_543_442_438_154_116_9_e-1))
                .mul_add(s, F64x::splat(-0.308_425_137_534_042_437_3))
                .mul_add(s, ONE);

            let ry = u;

            let o = (q & Ix::splat(2)).eq(Ix::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = M64x::from_cast((q & Ix::splat(4)).eq(Ix::splat(4)));
            rsin =
                F64x::from_bits((U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rsin));

            let o = M64x::from_cast(((q + Ix::splat(2)) & Ix::splat(4)).eq(Ix::splat(4)));
            rcos =
                F64x::from_bits((U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rcos));

            let o = d.abs().gt(TRIGRANGEMAX3 / F64x::splat(4.));
            rsin = F64x::from_bits(!U64x::from_bits(o) & U64x::from_bits(rsin));
            rcos = F64x::from_bits(!U64x::from_bits(o) & U64x::from_bits(rcos));

            let o = d.is_infinite();
            rsin = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(rsin));
            rcos = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(rcos));

            (rsin, rcos)
        }

        #[test]
        fn test_sincospi() {
            use rug::{float::Constant, Float};
            let rangemax2 = 1e+9 / 4.;
            test_f_ff(
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

        /// Arc tangent function of two variables
        ///
        /// These functions evaluates the arc tangent function of (***y*** / ***x***).
        /// The quadrant of the result is determined according to the signs of ***x*** and ***y***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn atan2(y: F64x, x: F64x) -> F64x {
            let mut r = atan2k(y.abs(), x);

            r = r.mul_sign(x);
            r = (x.is_infinite() | x.eq(ZERO)).select(
                F64x::FRAC_PI_2 - visinf2_vd_vd_vd(x, F64x::FRAC_PI_2.mul_sign(x)),
                r,
            );
            r = y.is_infinite().select(
                F64x::FRAC_PI_2 - visinf2_vd_vd_vd(x, F64x::FRAC_PI_4.mul_sign(x)),
                r,
            );
            r = y.eq(ZERO).select(
                F64x::from_bits(U64x::from_bits(x.is_sign_negative()) & U64x::from_bits(F64x::PI)),
                r,
            );

            F64x::from_bits(U64x::from_bits(x.is_nan() | y.is_nan()) | U64x::from_bits(r.mul_sign(y)))
        }

        #[test]
        fn test_atan2() {
            test_ff_f(
                atan2,
                rug::Float::atan2,
                f64::MIN..=f64::MAX,
                f64::MIN..=f64::MAX,
                3.5
            );
        }

        /// Arc sine function
        ///
        /// These functions evaluates the arc sine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn asin(d: F64x) -> F64x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let x = o.select(d.abs(), x2.sqrt());

            let x4 = x2 * x2;
            let x8 = x4 * x4;
            let x16 = x8 * x8;

            let mut u = F64x::poly12(x2, x4, x8, x16,
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
                0.166_666_666_666_649_754_3);

            u = u.mul_add(x * x2, x);

            let r = o.select(u, u.mul_add(F64x::splat(-2.), F64x::FRAC_PI_2));
            r.mul_sign(d)
        }

        #[test]
        fn test_asin() {
            test_f_f(
                asin,
                rug::Float::asin,
                -1.0..=1.0,
                3.5,
            );
        }

        /// Arc cosine function
        ///
        /// These functions evaluates the arc cosine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn acos(d: F64x) -> F64x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select(d.abs(), x2.sqrt());
            x = d.abs().eq(ONE).select(ZERO, x);

            let x4 = x2 * x2;
            let x8 = x4 * x4;
            let x16 = x8 * x8;

            let mut u = F64x::poly12(x2, x4, x8, x16,
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
                0.166_666_666_666_649_754_3);

            u *= x2 * x;

            let y = F64x::FRAC_PI_2 - (x.mul_sign(d) + u.mul_sign(d));
            x += u;
            let r = o.select(y, x * F64x::splat(2.));
            (!o & d.lt(ZERO)).select(
                Doubled::<F64x>::splat(crate::f64::D_PI).add_checked(-r).0,
                r,
            )
        }

        #[test]
        fn test_acos() {
            test_f_f(
                acos,
                rug::Float::acos,
                -1.0..=1.0,
                3.5,
            );
        }

        /// Arc tangent function
        ///
        /// These functions evaluates the arc tangent function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn atan(mut s: F64x) -> F64x {
            /*if cfg!(feature = "__intel_compiler") {
                // && defined(ENABLE_PURECFMA_SCALAR)
                let w = s;
            }*/

            let q = vsel_vi_vd_vi(s, Ix::splat(2));
            s = s.abs();

            let q = vsel_vi_vd_vd_vi_vi(ONE, s, q + Ix::splat(1), q);
            s = ONE.lt(s).select(s.recpre(), s);

            let mut t = s * s;

            let t2 = t * t;
            let t4 = t2 * t2;
            let t8 = t4 * t4;
            let t16 = t8 * t8;

            let u = F64x::poly19(t, t2, t4, t8, t16,
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
                -0.333_333_333_333_311_110_369_124);

            t = s.mul_add(t * u, s);

            t = M64x::from_cast((q & Ix::splat(1)).eq(Ix::splat(1))).select(F64x::FRAC_PI_2 - t, t);
            t = F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((q & Ix::splat(2)).eq(Ix::splat(2))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(t),
            );

            /*if cfg!(feature = "__intel_compiler") {
                // && defined(ENABLE_PURECFMA_SCALAR)
                t = w.eq(ZERO).select(w, t);
            }*/
            t
        }

        #[test]
        fn test_atan() {
            test_f_f(
                atan,
                rug::Float::atan,
                f64::MIN..=f64::MAX,
                3.5,
            );
        }

        /// Hyperbolic sine function
        ///
        /// These functions evaluates the hyperbolic sine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP` if ***a*** is in `[-709, 709]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct sign
        /// or a correct value with `3.5 ULP` error bound is returned.
        pub fn sinh(x: F64x) -> F64x {
            let e = expm1k(x.abs());

            let mut y = (e + F64x::splat(2.)) / (e + ONE);
            y *= HALF * e;

            y = (x.abs().gt(F64x::splat(709.)) | y.is_nan()).select(F64x::INFINITY, y);
            y = y.mul_sign(x);
            F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y))
        }

        #[test]
        fn test_sinh() {
            test_f_f(
                sinh,
                rug::Float::sinh,
                -709.0..=709.0,
                3.5
            );
        }

        /// Hyperbolic cosine function
        ///
        /// These functions evaluates the hyperbolic cosine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP` if a is in `[-709, 709]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct sign
        /// or a correct value with `3.5 ULP` error bound is returned.
        pub fn cosh(x: F64x) -> F64x {
            let e = u10::exp(x.abs());
            let mut y = HALF.mul_add(e, HALF / e);

            y = (x.abs().gt(F64x::splat(709.)) | y.is_nan()).select(F64x::INFINITY, y);
            F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y))
        }

        #[test]
        fn test_cosh() {
            test_f_f(
                cosh,
                rug::Float::cosh,
                -709.0..=709.0,
                3.5
            );
        }

        /// Hyperbolic tangent function
        ///
        /// These functions evaluates the hyperbolic tangent function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP` for the double-precision
        /// function or `3.5 ULP` for the single-precision function.
        pub fn tanh(x: F64x) -> F64x {
            let d = expm1k(F64x::splat(2.) * x.abs());
            let mut y = d / (F64x::splat(2.) + d);

            y = (x.abs().gt(F64x::splat(18.714_973_875)) | y.is_nan()).select(ONE, y);
            y = y.mul_sign(x);
            F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y))
        }

        #[test]
        fn test_tanh() {
            test_f_f(
                tanh,
                rug::Float::tanh,
                -19.0..=19.0,
                3.5
            );
        }

        /// Natural logarithmic function
        ///
        /// These functions return the natural logarithm of ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn log(mut d: F64x) -> F64x {
            let m: F64x;

            let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
                    {
                        let o = d.lt(F64x::splat(f64::MIN_POSITIVE));
                        d = o.select(d * (D1_32X * D1_32X), d);
                        let mut e = ilogb2k(d * F64x::splat(1. / 0.75));
                        m = ldexp3k(d, -e);
                        e = Mx::from_cast(o).select(e - Ix::splat(64), e);
                        F64x::from_cast(e)
                    }/* else {
                        let mut e = vgetexp_vd_vd(d * F64x::splat(1. / 0.75));
                        e = e.eq(F64x::INFINITY).select(F64x::splat(1024.), e);
                        m = vgetmant_vd_vd(d);
                        e
                    }*/;

            let mut x = (m - ONE) / (ONE + m);
            let x2 = x * x;

            let x4 = x2 * x2;
            let x8 = x4 * x4;
            let x3 = x * x2;

            let t = F64x::poly7(x2, x4, x8,
                0.153_487_338_491_425_068_243_146,
                0.152_519_917_006_351_951_593_857,
                0.181_863_266_251_982_985_677_316,
                0.222_221_366_518_767_365_905_163,
                0.285_714_294_746_548_025_383_248,
                0.399_999_999_950_799_600_689_777,
                0.666_666_666_666_777_874_006_3);

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            x = x.mul_add(F64x::splat(2.), F64x::splat(0.693_147_180_559_945_286_226_764) * ef);
            x = x3.mul_add(t, x);

            x = d.eq(F64x::INFINITY).select(F64x::INFINITY, x);
            x = (d.lt(ZERO) | d.is_nan()).select(F64x::NAN, x);
            d.eq(ZERO).select(F64x::NEG_INFINITY, x)
            /* } else {
                x = x.mul_add(F64x::splat(2.), F64x::splat(0.693_147_180_559_945_286_226_764) * ef);
                x = x3.mul_add(t, x);
                vfixup_vd_vd_vd_vi2_i(x, d, I64x::splat((5 << (5 * 4))), 0)
            }*/
        }

        #[test]
        fn test_log() {
            test_f_f(
                log,
                rug::Float::ln,
                0.0..=f64::MAX,
                3.5,
            );
        }

        /// Base-10 logarithmic function
        ///
        /// This function returns the base-10 logarithm of ***a***.
        pub fn log2(mut d: F64x) -> F64x {
            let (m, e) = //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")
            {
                let o = d.lt(F64x::splat(f64::MIN_POSITIVE));
                d = o.select(d * (D1_32X * D1_32X), d);
                let e = ilogb2k(d * F64x::splat(1./0.75));
                (ldexp3k(d, -e), Mx::from_cast(o).select(e - Ix::splat(64), e))
            /*} else {
                vdouble e = vgetexp_vd_vd(d * F64x::splat(1./0.75));
                (vgetmant_vd_vd(d), e.eq(F64x::INFINITY).select(F64x::splat(1024.), e))
            */};

            let x = (m - ONE) / (m + ONE);
            let x2 = x * x;

            let t = F64x::splat(0.221_194_175_045_608_149)
                .mul_add(x2, F64x::splat(0.220_076_869_315_227_768_9))
                .mul_add(x2, F64x::splat(0.262_370_805_748_851_465_6))
                .mul_add(x2, F64x::splat(0.320_597_747_794_449_550_2))
                .mul_add(x2, F64x::splat(0.412_198_594_548_532_470_9))
                .mul_add(x2, F64x::splat(0.577_078_016_299_705_898_2))
                .mul_add(x2, F64x::splat(0.961_796_693_926_080_914_49));

            let s = //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")
            {
              F64x::from_cast(e).add_checked(x.mul_as_doubled(F64x::splat(2.885_390_081_777_926_774)))
            /* } else {
                e.add_checked(x.mul_as_doubled(F64x::splat(2.885_390_081_777_926_774)))
            */ };

            let mut r = t.mul_add(x * x2, F64x::from(s));

            //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {
            r = d.eq(F64x::INFINITY).select(F64x::INFINITY, r);
            r = (d.lt(ZERO) | d.is_nan()).select(F64x::NAN, r);
            r = d.eq(ZERO).select(F64x::NEG_INFINITY, r);
            /* } else {
                r = vfixup_vd_vd_vd_vi2_i(r, d, I32::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
            }*/

            r
        }

        #[test]
        fn test_log2() {
            test_f_f(
                log2,
                rug::Float::log2,
                0.0..=f64::MAX,
                3.5
            );
        }

        /// Base-10 exponential function
        ///
        /// This function returns 10 raised to ***a***.
        pub fn exp10(d: F64x) -> F64x {
            let mut u = (d * LOG10_2).round();
            let q = u.roundi();

            let mut s = u.mul_add(-L10.0, d);
            s = u.mul_add(-L10.1, s);

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

            u = u.mul_add(s, ONE);

            u = ldexp2k(u, q);

            u = d
                .gt(F64x::splat(308.254_715_559_916_71))
                .select(F64x::INFINITY, u);
            F64x::from_bits(!U64x::from_bits(d.lt(F64x::splat(-350.))) & U64x::from_bits(u))
        }

        #[test]
        fn test_exp10() {
            test_f_f(
                exp10,
                rug::Float::exp10,
                -350.0..=308.26,
                3.5
            );
        }

        /// Base-2 exponential function
        ///
        /// This function returns `2` raised to ***a***.
        pub fn exp2(d: F64x) -> F64x {
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
            u = u.mul_add(s, F64x::splat(0.693_147_180_559_945_286_2));

            u = u.mul_add(s, ONE);

            u = ldexp2k(u, q);

            u = d.ge(F64x::splat(1024.)).select(F64x::INFINITY, u);
            F64x::from_bits(!U64x::from_bits(d.lt(F64x::splat(-2000.))) & U64x::from_bits(u))
        }

        #[test]
        fn test_exp2() {
            test_f_f(
                exp2,
                rug::Float::exp2,
                -2000.0..=1024.0,
                3.5
            );
        }

        /// Square root function
        ///
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn sqrt(d: F64x) -> F64x {
            return u05::sqrt(d);
        }

        /// Cube root function
        ///
        /// These functions return the real cube root of ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn cbrt(mut d: F64x) -> F64x {
            let mut q = ONE;
            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                let s = d;
            }*/
            let e = ilogbk(d.abs()) + Ix::splat(1);
            d = ldexp2k(d, -e);

            let t = F64x::from_cast(e) + F64x::splat(6144.);
            let qu = (t * F64x::splat(1. / 3.)).trunci();
            let re = (t - (F64x::from_cast(qu) * F64x::splat(3.))).trunci();

            q = M64x::from_cast(re.eq(Ix::splat(1)))
                .select(F64x::splat(1.259_921_049_894_873_164_767_210_6), q);
            q = M64x::from_cast(re.eq(Ix::splat(2)))
                .select(F64x::splat(1.587_401_051_968_199_474_751_705_6), q);
            q = ldexp2k(q, qu - Ix::splat(2048));

            q = q.mul_sign(d);

            d = d.abs();

            let mut x = F64x::splat(-0.640_245_898_480_692_909_870_982)
                .mul_add(d, F64x::splat(2.961_551_030_200_395_118_185_95))
                .mul_add(d, F64x::splat(-5.733_530_609_229_478_436_361_66))
                .mul_add(d, F64x::splat(6.039_903_689_894_587_479_614_07))
                .mul_add(d, F64x::splat(-3.858_419_355_104_449_888_216_32))
                .mul_add(d, F64x::splat(2.230_727_530_249_660_972_572_2));

            let mut y = x * x;
            y = y * y;
            x -= d.mul_sub(y, x) * F64x::splat(1. / 3.);
            y = d * x * x;
            y = (y - F64x::splat(2. / 3.) * y * y.mul_add(x, F64x::splat(-1.))) * q;

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                y = s.is_infinite().select(F64x::INFINITY.mul_sign(s), y);
                y = s
                    .eq(ZERO)
                    .select(ZERO.mul_sign(s), y);
            }*/

            y
        }

        #[test]
        fn test_cbrt() {
            test_f_f(
                cbrt,
                rug::Float::cbrt,
                f64::MIN..=f64::MAX,
                3.5
            );
        }

        /// 2D Euclidian distance function
        ///
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn hypot(x: F64x, y: F64x) -> F64x {
            let x = x.abs();
            let y = y.abs();
            let min = x.min(y);
            let max = x.max(y);

            let t = min / max;
            let mut ret = max * t.mul_add(t, ONE).sqrt();
            ret = min.eq(ZERO).select(max, ret);
            ret = (x.is_nan() | y.is_nan()).select(F64x::NAN, ret);
            (x.eq(F64x::INFINITY) | y.eq(F64x::INFINITY)).select(F64x::INFINITY, ret)
        }

        #[test]
        fn test_hypot() {
            test_ff_f(
                hypot,
                rug::Float::hypot,
                -1e307..=1e307,
                -1e307..=1e307,
                3.5,
            );
        }
    };
}
