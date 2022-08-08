macro_rules! impl_math_f64_u10 {
    () => {
        use super::*;

        /// Sine function
        ///
        /// This function evaluates the sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn sin(d: F64x) -> F64x {
            let mut s;
            let mut ql;

            if d.abs().simd_lt(TRIGRANGEMAX2).all() {
                let dql = (d * FRAC_1_PI).round();
                ql = dql.roundi();
                let u = dql.mla(-PI_A2, d);
                s = u.add_checked_as_doubled(dql * (-PI_B2));
            } else if d.abs().simd_lt(TRIGRANGEMAX).all() {
                let dqh = (d * (FRAC_1_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = (d.mul_sub(FRAC_1_PI, dqh)).round();
                ql = dql.roundi();

                let u = dqh.mla(-PI_A, d);
                s = u.add_checked_as_doubled(dql * (-PI_A));
                s += dqh * (-PI_B);
                s += dql * (-PI_B);
                s += dqh * (-PI_C);
                s += dql * (-PI_C);
                s += (dqh + dql) * (-PI_D);
            } else {
                let (mut ddidd, ddii) = rempi(d);
                ql = ddii & Ix::splat(3);
                ql = ql + ql + ddidd.0.simd_gt(ZERO).cast().select(Ix::splat(2), Ix::splat(1));
                ql >>= Ix::splat(2);
                let o = (ddii & Ix::splat(1)).simd_eq(Ix::splat(1));
                let mut x = Doubled::new(
                    F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(ddidd.0),
                    F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(ddidd.0),
                );
                x = ddidd + x;
                ddidd = o.cast().select_doubled(x, ddidd);
                s = ddidd.normalize();
                s.0 = F64x::from_bits(
                    (d.is_infinite() | d.is_nan()).to_int().cast() | s.0.to_bits(),
                );
            }

            let t = s;
            s = s.square();

            let s2 = s.0 * s.0;
            let s4 = s2 * s2;

            let mut u = F64x::poly6(s.0, s2, s4,
                2.720_524_161_385_295_679_179_83_e-15,
                -7.642_925_941_139_544_719_002_3_e-13,
                1.605_893_701_172_778_962_116_23_e-10,
                -2.505_210_681_484_312_335_936_8_e-8,
                2.755_731_921_044_282_247_773_79_e-6,
                -0.000_198_412_698_412_046_454_654_947)
                .mla(s.0, F64x::splat(0.008_333_333_333_333_180_562_019_22));

            let x = ONE.add_checked(
                (F64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0)) * s,
            );

            u = t.mul_as_f(x);

            u = F64x::from_bits(
                ((ql & Ix::splat(1)).simd_eq(Ix::splat(1)).to_int().cast()
                    & NEG_ZERO.to_bits()
                ) ^ u.to_bits(),
            );
            d.simd_eq(ZERO).select(d, u)
        }

        /// Sine function
        ///
        /// This function evaluates the sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn sin_deterministic(d: F64x) -> F64x {
            let mut s;
            let mut ql;

            let g = d.abs().simd_lt(TRIGRANGEMAX2);
            let dql = (d * FRAC_1_PI).round();
            ql = dql.roundi();
            let u = dql.mla(-PI_A2, d);
            let mut x = u.add_checked_as_doubled(dql * (-PI_B2));

            if !g.all() {
                let mut dqh = (d * FRAC_1_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let dql = d.mul_sub(FRAC_1_PI, dqh).round();

                let u = dqh.mla(-PI_A, d);
                s = u.add_checked_as_doubled(dql * (-PI_A));
                s += dqh * (-PI_B);
                s += dql * (-PI_B);
                s += dqh * (-PI_C);
                s += dql * (-PI_C);
                s = s.add_checked((dqh + dql) * (-PI_D));

                ql = g.cast().select(ql, dql.roundi());
                x = g.select_doubled(x, s);
                let g = d.abs().simd_lt(TRIGRANGEMAX);

                if !g.all() {
                    let (mut ddidd, ddii) = rempi(d);
                    let mut ql2 = ddii & Ix::splat(3);
                    ql2 = ql2 + ql2 + ddidd.0.simd_gt(ZERO).cast().select(Ix::splat(2), Ix::splat(1));
                    ql2 >>= Ix::splat(2);
                    let o = (ddii & Ix::splat(1)).simd_eq(Ix::splat(1));
                    let mut t = Doubled::new(
                        F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(ddidd.0),
                        F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(ddidd.0),
                    );
                    t = ddidd + t;
                    ddidd = o.cast().select_doubled(t, ddidd);
                    s = ddidd.normalize();
                    ql = g.cast().select(ql, ql2);
                    x = g.select_doubled(x, s);
                    x.0 = F64x::from_bits(
                        (d.is_infinite() | d.is_nan()).to_int().cast() | x.0.to_bits(),
                    );
                }
            }

            let t = x;
            s = x.square();

            let s2 = s.0 * s.0;
            let s4 = s2 * s2;

            let mut u = F64x::poly6(s.0, s2, s4,
                2.720_524_161_385_295_679_179_83_e-15,
                -7.642_925_941_139_544_719_002_3_e-13,
                1.605_893_701_172_778_962_116_23_e-10,
                -2.505_210_681_484_312_335_936_8_e-8,
                2.755_731_921_044_282_247_773_79_e-6,
                -0.000_198_412_698_412_046_454_654_947)
                .mla(s.0, F64x::splat(0.008_333_333_333_333_180_562_019_22));

            x = ONE.add_checked(
                F64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0) * s,
            );

            u = t.mul_as_f(x);

            u = F64x::from_bits(
                ((ql & Ix::splat(1)).simd_eq(Ix::splat(1)).to_int().cast()
                    & NEG_ZERO.to_bits()
                ) ^ u.to_bits(),
            );

            d.simd_eq(ZERO).select(d, u)
        }

        #[test]
        fn test_sin() {
            test_f_f(
                sin,
                rug::Float::sin,
                f64::MIN..=f64::MAX,
                1.
            );
            test_f_f(
                sin_deterministic,
                rug::Float::sin,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /// Cosine function
        ///
        /// This function evaluates the cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn cos(d: F64x) -> F64x {
            let mut s;
            let mut ql;

            if d.abs().simd_lt(TRIGRANGEMAX2).all() {
                let dql = d.mla(FRAC_1_PI, F64x::splat(-0.5)).round();
                let dql = F64x::splat(2.).mla(dql, ONE);
                ql = dql.roundi();
                s = d.add_as_doubled(dql * (-PI_A2) * HALF);
                s = s.add_checked(dql * (-PI_B2) * HALF);
            } else if d.abs().simd_lt(TRIGRANGEMAX).all() {
                let dqh = d
                    .mla(FRAC_1_PI / D1_23X, -FRAC_1_PI / D1_24X)
                    .trunc();
                ql = (d * FRAC_1_PI + dqh.mla(-D1_23X, F64x::splat(-0.5))).roundi();
                let dqh = dqh * D1_24X;
                ql = ql + ql + Ix::splat(1);
                let dql: F64x = ql.cast();

                let u = dqh.mla(-PI_A * HALF, d);
                s = u.add_as_doubled(dql * -PI_A * HALF);
                s += dqh * (-PI_B) * HALF;
                s += dql * (-PI_B) * HALF;
                s += dqh * (-PI_C) * HALF;
                s += dql * (-PI_C) * HALF;
                s = s.add_checked((dqh + dql) * (-PI_D) * HALF);
            } else {
                let (mut ddidd, ddii) = rempi(d);
                ql = ddii & Ix::splat(3);
                ql = ql + ql + ddidd.0.simd_gt(ZERO).cast().select(Ix::splat(8), Ix::splat(7));
                ql >>= Ix::splat(1);
                let o = (ddii & Ix::splat(1)).simd_eq(Ix::splat(0));
                let y = ddidd.0.simd_gt(ZERO).select(ZERO, F64x::splat(-1.));
                let mut x = Doubled::new(
                    F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(y),
                    F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(y),
                );
                x = ddidd + x;
                ddidd = o.cast().select_doubled(x, ddidd);
                s = ddidd.normalize();
                s.0 = F64x::from_bits(
                    (d.is_infinite() | d.is_nan()).to_int().cast() | s.0.to_bits(),
                );
            }

            let t = s;
            s = s.square();

            let s2 = s.0 * s.0;
            let s4 = s2 * s2;

            let u = F64x::poly6(s.0, s2, s4,
                2.720_524_161_385_295_679_179_83_e-15,
                -7.642_925_941_139_544_719_002_3_e-13,
                1.605_893_701_172_778_962_116_23_e-10,
                -2.505_210_681_484_312_335_936_8_e-8,
                2.755_731_921_044_282_247_773_79_e-6,
                -0.000_198_412_698_412_046_454_654_947)
                .mla(s.0, F64x::splat(0.008_333_333_333_333_180_562_019_22));

            let x = ONE.add_checked(
                (F64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0)) * s,
            );

            let u = t.mul_as_f(x);

            F64x::from_bits(
                ((ql & Ix::splat(2)).simd_eq(Ix::splat(0)).to_int().cast()
                    & NEG_ZERO.to_bits()
                ) ^ u.to_bits(),
            )
        }

        /// Cosine function
        ///
        /// This function evaluates the cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn cos_deterministic(d: F64x) -> F64x {
            let g = d.abs().simd_lt(TRIGRANGEMAX2);
            let mut dql = d.mla(FRAC_1_PI, F64x::splat(-0.5)).round();
            dql = F64x::splat(2.).mla(dql, ONE);
            let mut ql = dql.roundi();
            let mut x = d.add_as_doubled(dql * (-PI_A2 * HALF));
            x = x.add_checked(dql * (-PI_B2 * HALF));

            if !g.all() {
                let mut dqh = (d.mla(FRAC_1_PI / D1_23X, -FRAC_1_PI / D1_24X)).trunc();
                let mut ql2 = (d * FRAC_1_PI + dqh.mla(-D1_23X, F64x::splat(-0.5))).roundi();
                dqh *= D1_24X;
                ql2 = ql2 + ql2 + Ix::splat(1);
                let dql: F64x = ql2.cast();

                let u = dqh.mla(-PI_A * HALF, d);
                let mut s = u.add_as_doubled(dql * (-PI_A * HALF));
                s += dqh * (-PI_B * HALF);
                s += dql * (-PI_B * HALF);
                s += dqh * (-PI_C * HALF);
                s += dql * (-PI_C * HALF);
                s = s.add_checked((dqh + dql) * (-PI_D * HALF));

                ql = g.cast().select(ql, ql2);
                x = g.select_doubled(x, s);
                let g = d.abs().simd_lt(TRIGRANGEMAX);

                if !g.all() {
                    let (mut ddidd, ddii) = rempi(d);
                    let mut ql2 = ddii & Ix::splat(3);
                    ql2 = ql2 + ql2 + ddidd.0.simd_gt(ZERO).cast().select(Ix::splat(8), Ix::splat(7));
                    ql2 >>= Ix::splat(1);
                    let o = (ddii & Ix::splat(1)).simd_eq(Ix::splat(0));
                    let y = ddidd.0.simd_gt(ZERO).select(ZERO, F64x::splat(-1.));
                    let mut t = Doubled::new(
                        F64x::splat(-crate::f64::D_PI.0 * 0.5).mul_sign(y),
                        F64x::splat(-crate::f64::D_PI.1 * 0.5).mul_sign(y),
                    );
                    t = ddidd + t;
                    ddidd = o.cast().select_doubled(t, ddidd);
                    s = ddidd.normalize();
                    ql = g.cast().select(ql, ql2);
                    x = g.select_doubled(x, s);
                    x.0 = F64x::from_bits(
                        (d.is_infinite() | d.is_nan()).to_int().cast() | x.0.to_bits(),
                    );
                }
            }

            let t = x;
            let s = x.square();

            let s2 = s.0 * s.0;
            let s4 = s2 * s2;

            let u = F64x::poly6(s.0, s2, s4,
                2.720_524_161_385_295_679_179_83_e-15,
                -7.642_925_941_139_544_719_002_3_e-13,
                1.605_893_701_172_778_962_116_23_e-10,
                -2.505_210_681_484_312_335_936_8_e-8,
                2.755_731_921_044_282_247_773_79_e-6,
                -0.000_198_412_698_412_046_454_654_947)
                .mla(s.0, F64x::splat(0.008_333_333_333_333_180_562_019_22));

            x = ONE.add_checked(
                F64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0) * s,
            );

            let u = t.mul_as_f(x);

            F64x::from_bits(
                ((ql & Ix::splat(2)).simd_eq(Ix::splat(0)).to_int().cast()
                    & NEG_ZERO.to_bits()
                ) ^ u.to_bits(),
            )
        }

        #[test]
        fn test_cos() {
            test_f_f(
                cos,
                rug::Float::cos,
                f64::MIN..=f64::MAX,
                1.
            );
            test_f_f(
                cos_deterministic,
                rug::Float::cos,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /// Evaluate sine and cosine functions simultaneously
        ///
        /// Evaluates the sine and cosine functions of a value in a at a time,
        /// and store the two values in *first* and *second* position in the returned value, respectively.
        /// returned value, respectively.
        /// The error bound of the returned values is `1.0 ULP`.
        /// If ***a*** is a `NaN` or `infinity`, a `NaN` is returned.
        pub fn sincos(d: F64x) -> (F64x, F64x) {
            let mut s;
            let ql;

            if d.abs().simd_lt(TRIGRANGEMAX2).all() {
                let dql = (d * FRAC_2_PI).round();
                ql = dql.roundi();
                let u = dql.mla(-PI_A2 * HALF, d);
                s = u.add_checked_as_doubled(dql * (-PI_B2) * HALF);
            } else if d.abs().simd_lt(TRIGRANGEMAX).all() {
                let dqh = (d * (FRAC_2_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = (d * FRAC_2_PI - dqh).round();
                ql = dql.roundi();

                let u = dqh.mla(-PI_A * HALF, d);
                s = u.add_checked_as_doubled(dql * (-PI_A) * HALF);
                s += dqh * (-PI_B) * HALF;
                s += dql * (-PI_B) * HALF;
                s += dqh * (-PI_C) * HALF;
                s += dql * (-PI_C) * HALF;
                s += (dqh + dql) * (-PI_D) * HALF;
            } else {
                let (ddidd, ddii) = rempi(d);
                ql = ddii;
                s = ddidd;
                let o = d.is_infinite() | d.is_nan();
                s = Doubled::new(
                    F64x::from_bits(o.to_int().cast() | s.0.to_bits()),
                    F64x::from_bits(o.to_int().cast() | s.1.to_bits())
                );
            }

            let t = s;

            s.0 = s.square_as_f();

            let u = F64x::splat(1.589_383_072_832_289_373_285_11_e-10)
                .mla(s.0, F64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
                .mla(s.0, F64x::splat(2.755_731_317_768_463_605_125_47_e-6))
                .mla(s.0, F64x::splat(-0.000_198_412_698_278_911_770_864_914))
                .mla(s.0, F64x::splat(0.008_333_333_333_319_184_596_174_6))
                .mla(s.0, F64x::splat(-0.166_666_666_666_666_130_709_393))
                * (s.0 * t.0);

            let x = t.add_checked(u);
            let rx = F64x::from(x);

            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
                .mla(s.0, F64x::splat(2.087_574_712_070_400_554_793_66_e-9))
                .mla(s.0, F64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
                .mla(s.0, F64x::splat(2.480_158_728_900_018_673_119_15_e-5))
                .mla(s.0, F64x::splat(-0.001_388_888_888_887_140_192_823_29))
                .mla(s.0, F64x::splat(0.041_666_666_666_666_551_959_206_2))
                .mla(s.0, F64x::splat(-0.5));

            let x = ONE.add_checked(s.0.mul_as_doubled(u));
            let ry = F64x::from(x);

            let o = (ql & Ix::splat(1)).simd_eq(Ix::splat(0)).cast();
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o: M64x = (ql & Ix::splat(2)).simd_eq(Ix::splat(2)).cast();
            rsin = F64x::from_bits(
                (o.to_int().cast() & NEG_ZERO.to_bits()) ^ rsin.to_bits(),
            );

            let o: M64x = ((ql + Ix::splat(1)) & Ix::splat(2)).simd_eq(Ix::splat(2)).cast();
            rcos = F64x::from_bits(
                (o.to_int().cast() & NEG_ZERO.to_bits()) ^ rcos.to_bits(),
            );

            (rsin, rcos)
        }

        /// Evaluate sine and cosine functions simultaneously
        ///
        /// Evaluates the sine and cosine functions of a value in a at a time,
        /// and store the two values in *first* and *second* position in the returned value, respectively.
        /// returned value, respectively.
        /// The error bound of the returned values is `1.0 ULP`.
        /// If ***a*** is a `NaN` or `infinity`, a `NaN` is returned.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn sincos_deterministic(d: F64x) -> (F64x, F64x) {
            let dql = (d * FRAC_2_PI).round();
            let mut ql = dql.roundi();
            let u = dql.mla(-PI_A2 * HALF, d);
            let mut s = u.add_checked_as_doubled(dql * (-PI_B2 * HALF));
            let g = d.abs().simd_lt(TRIGRANGEMAX2);

            if !g.all() {
                let mut dqh = (d * FRAC_2_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let dql = (d * FRAC_2_PI - dqh).round();

                let u = dqh.mla(-PI_A * HALF, d);
                let mut x = u.add_checked_as_doubled(dql * (-PI_A * HALF));
                x += dqh * (-PI_B * HALF);
                x += dql * (-PI_B * HALF);
                x += dqh * (-PI_C * HALF);
                x += dql * (-PI_C * HALF);
                x = x.add_checked((dqh + dql) * (-PI_D * HALF));

                ql = g.cast().select(ql, dql.roundi());
                s = g.select_doubled(s, x);
                let g = d.abs().simd_lt(TRIGRANGEMAX);

                if !g.all() {
                    let (ddidd, ddii) = rempi(d);
                    x = ddidd;
                    let o = d.is_infinite() | d.is_nan();
                    x = Doubled::new(
                        F64x::from_bits(o.to_int().cast() | x.0.to_bits()),
                        F64x::from_bits(o.to_int().cast() | x.1.to_bits())
                    );

                    ql = g.cast().select(ql, ddii);
                    s = g.select_doubled(s, x);
                }
            }

            let t = s;

            s.0 = s.square_as_f();

            let u = F64x::splat(1.589_383_072_832_289_373_285_11_e-10)
                .mla(s.0, F64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
                .mla(s.0, F64x::splat(2.755_731_317_768_463_605_125_47_e-6))
                .mla(s.0, F64x::splat(-0.000_198_412_698_278_911_770_864_914))
                .mla(s.0, F64x::splat(0.008_333_333_333_319_184_596_174_6))
                .mla(s.0, F64x::splat(-0.166_666_666_666_666_130_709_393))
                * (s.0 * t.0);

            let x = t.add_checked(u);
            let mut rx = F64x::from(x);

            rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
                .mla(s.0, F64x::splat(2.087_574_712_070_400_554_793_66_e-9))
                .mla(s.0, F64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
                .mla(s.0, F64x::splat(2.480_158_728_900_018_673_119_15_e-5))
                .mla(s.0, F64x::splat(-0.001_388_888_888_887_140_192_823_29))
                .mla(s.0, F64x::splat(0.041_666_666_666_666_551_959_206_2))
                .mla(s.0, F64x::splat(-0.5));

            let x = ONE.add_checked(s.0.mul_as_doubled(u));
            let ry = F64x::from(x);

            let o = (ql & Ix::splat(1)).simd_eq(Ix::splat(0)).cast();
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o: M64x = (ql & Ix::splat(2)).simd_eq(Ix::splat(2)).cast();
            rsin = F64x::from_bits(
                (o.to_int().cast() & NEG_ZERO.to_bits()) ^ rsin.to_bits(),
            );

            let o: M64x = ((ql + Ix::splat(1)) & Ix::splat(2)).simd_eq(Ix::splat(2)).cast();
            rcos = F64x::from_bits(
                (o.to_int().cast() & NEG_ZERO.to_bits()) ^ rcos.to_bits(),
            );

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
                1.
            );
            test_f_ff(
                sincos_deterministic,
                |in1| {
                    let prec = in1.prec();
                    in1.sin_cos(rug::Float::new(prec))
                },
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /// Tangent function
        ///
        /// This function evaluates the tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn tan(d: F64x) -> F64x {
            let mut s;
            let ql;

            if d.abs().simd_lt(TRIGRANGEMAX2).all() {
                let dql = (d * FRAC_2_PI).round();
                ql = dql.roundi();
                let u = dql.mla(-PI_A2 * HALF, d);
                s = u.add_checked_as_doubled(dql * (-PI_B2) * HALF);
            } else if d.abs().simd_lt(TRIGRANGEMAX).all() {
                let dqh = (d * (FRAC_2_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                s = Doubled::new(M_2_PI_H, M_2_PI_L) * d
                    + (d.simd_lt(ZERO).select(F64x::splat(-0.5), HALF) - dqh);
                let dql = F64x::from(s).trunc();
                ql = dql.roundi();

                let u = dqh.mla(-PI_A * HALF, d);
                s = u.add_checked_as_doubled(dql * (-PI_A) * HALF);
                s += dqh * (-PI_B) * HALF;
                s += dql * (-PI_B) * HALF;
                s += dqh * (-PI_C) * HALF;
                s += dql * (-PI_C) * HALF;
                s += (dqh + dql) * (-PI_D) * HALF;
            } else {
                let (ddidd, ddii) = rempi(d);
                ql = ddii;
                s = ddidd;
                let o = d.is_infinite() | d.is_nan();
                s = Doubled::new(
                    F64x::from_bits(o.to_int().cast() | s.0.to_bits()),
                    F64x::from_bits(o.to_int().cast() | s.1.to_bits())
                );
            }

            let t = s.scale(HALF);
            s = t.square();

            let s2 = s.0 * s.0;
            let s4 = s2 * s2;
            let u = F64x::poly8(s.0, s2, s4,
                0.324_509_882_663_927_631_6_e-3,
                0.561_921_973_811_432_373_5_e-3,
                0.146_078_150_240_278_449_4_e-2,
                0.359_161_154_079_249_951_9_e-2,
                0.886_326_840_956_311_312_6_e-2,
                0.218_694_872_818_553_549_8_e-1,
                0.539_682_539_951_727_297_e-1,
                0.133_333_333_333_050_058_1)
                .mla(s.0, F64x::splat(0.333_333_333_333_334_369_5));
            let mut x = t.add_checked(s * t * u);

            let y = (-ONE).add_checked(x.square());
            x = x.scale(F64x::splat(-2.));

            let o = (ql & Ix::splat(1)).simd_eq(Ix::splat(1)).cast();

            x = o.select_doubled(-y, x) / o.select_doubled(x, y);

            let u = F64x::from(x);

            d.simd_eq(ZERO).select(d, u)
        }

        /// Tangent function
        ///
        /// This function evaluates the tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn tan_deterministic(d: F64x) -> F64x {
            let dql = (d * FRAC_2_PI).round();
            let mut ql = dql.roundi();
            let u = dql.mla(-PI_A2 * HALF, d);
            let mut s = u.add_checked_as_doubled(dql * (-PI_B2 * HALF));
            let g = d.abs().simd_lt(TRIGRANGEMAX2);

            if !g.all() {
                let mut dqh = (d * FRAC_2_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let mut x = Doubled::new(M_2_PI_H, M_2_PI_L) * d
                    + (d.simd_lt(ZERO).select(F64x::splat(-0.5), HALF) - dqh);
                let dql = F64x::from(x).trunc();

                let u = dqh.mla(-PI_A * HALF, d);
                x = u.add_checked_as_doubled(dql * (-PI_A * HALF));
                x += dqh * (-PI_B * HALF);
                x += dql * (-PI_B * HALF);
                x += dqh * (-PI_C * HALF);
                x += dql * (-PI_C * HALF);
                x = x.add_checked((dqh + dql) * (-PI_D * HALF));

                ql = g.cast().select(ql, dql.roundi());
                s = g.select_doubled(s, x);
                let g = d.abs().simd_lt(TRIGRANGEMAX);

                if !g.all() {
                    let (ddidd, ddii) = rempi(d);
                    x = ddidd;
                    let o = d.is_infinite() | d.is_nan();
                    x = Doubled::new(
                        F64x::from_bits(o.to_int().cast() | x.0.to_bits()),
                        F64x::from_bits(o.to_int().cast() | x.1.to_bits())
                    );

                    ql = g.cast().select(ql, ddii);
                    s = g.select_doubled(s, x);
                }
            }

            let t = s.scale(HALF);
            s = t.square();

            let s2 = s.0 * s.0;
            let s4 = s2 * s2;
            let u = F64x::poly8(s.0, s2, s4,
                0.324_509_882_663_927_631_6_e-3,
                0.561_921_973_811_432_373_5_e-3,
                0.146_078_150_240_278_449_4_e-2,
                0.359_161_154_079_249_951_9_e-2,
                0.886_326_840_956_311_312_6_e-2,
                0.218_694_872_818_553_549_8_e-1,
                0.539_682_539_951_727_297_e-1,
                0.133_333_333_333_050_058_1)
                .mla(s.0, F64x::splat(0.333_333_333_333_334_369_5));
            let mut x = t.add_checked(s * t * u);

            let y = (-ONE).add_checked(x.square());
            x = x.scale(F64x::splat(-2.));

            let o = (ql & Ix::splat(1)).simd_eq(Ix::splat(1)).cast();

            x = o.select_doubled(-y, x) / o.select_doubled(x, y);

            let u = F64x::from(x);

            d.simd_eq(ZERO).select(d, u)
        }

        #[test]
        fn test_tan() {
            test_f_f(
                tan,
                rug::Float::tan,
                f64::MIN..=f64::MAX,
                1.
            );
            test_f_f(
                tan_deterministic,
                rug::Float::tan,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        #[inline]
        fn atan2k_u1(y: Doubled<F64x>, mut x: Doubled<F64x>) -> Doubled<F64x> {
            let q = x.0.is_sign_negative().cast().to_int() & Ix::splat(-2);
            let p = x.0.simd_lt(ZERO);
            let b = p.to_int().cast() & NEG_ZERO.to_bits();
            x = Doubled::new(
                F64x::from_bits(b ^ x.0.to_bits()),
                F64x::from_bits(b ^ x.1.to_bits())
            );

            let q = x.0.simd_lt(y.0).cast().select(q + Ix::splat(1), q);
            let p = x.0.simd_lt(y.0);
            let s = p.select_doubled(-x, y);
            let mut t = p.select_doubled(y, x);

            let s = s / t;
            t = s.square();
            t = t.normalize();

            let t2 = t.0 * t.0;
            let t4 = t2 * t2;
            let t8 = t4 * t4;

            let u = F64x::poly16(t.0, t2, t4, t8,
                1.062_984_841_914_487_466_074_15_e-05,
                -0.000_125_620_649_967_286_867_384_336,
                0.000_705_576_642_963_934_123_897_74,
                -0.002_518_656_144_987_133_603_529_99,
                0.006_462_628_990_369_911_723_135_04,
                -0.012_828_133_366_339_903_101_427_4,
                0.020_802_479_992_414_579_790_249_7,
                -0.028_900_234_478_474_031_568_628_9,
                0.035_978_500_503_510_459_085_365_6,
                -0.041_848_579_703_592_507_506_027,
                0.047_084_301_165_328_398_819_376_3,
                -0.052_491_421_058_844_842_106_871_9,
                0.058_794_659_096_958_100_386_043_4,
                -0.066_662_088_477_879_549_719_418_2,
                0.076_922_533_029_620_376_865_409_5,
                -0.090_909_044_277_338_757_478_190_7)
                .mla(t.0, F64x::splat(0.111_111_108_376_896_236_538_123))
                .mla(t.0, F64x::splat(-0.142_857_142_756_268_568_062_339))
                .mla(t.0, F64x::splat(0.199_999_999_997_977_351_284_817))
                .mla(t.0, F64x::splat(-0.333_333_333_333_317_605_173_818));

            t = s.add_checked(s * t * u);
            (Doubled::new(
                F64x::splat(1.570_796_326_794_896_557_998_982),
                F64x::splat(6.123_233_995_736_766_035_868_82_e-17),
            ) * q.cast())
            .add_checked(t)
        }

        /// Arc tangent function of two variables
        ///
        /// This function evaluates the arc tangent function of (***y*** / ***x***).
        /// The quadrant of the result is determined according to the signs
        /// of ***x*** and ***y***.
        /// The error bound of the returned values is `max(1.0 ULP, f64::MIN_POSITIVE)`.
        pub fn atan2(y: F64x, x: F64x) -> F64x {
            let o = x.abs().simd_lt(F64x::splat(5.562_684_646_268_008_398_4_e-309)); // nexttoward((1.0 / DBL_MAX), 1)
            let x = o.select(x * D1_53X, x);
            let y = o.select(y * D1_23X, y);

            let d = atan2k_u1(Doubled::from(y.abs()), Doubled::from(x));
            let mut r = F64x::from(d);

            r = r.mul_sign(x);
            r = (x.is_infinite() | x.simd_eq(ZERO)).select(
                FRAC_PI_2 - visinf2_vd_vd_vd(x, FRAC_PI_2.mul_sign(x)),
                r,
            );
            r = y.is_infinite().select(
                FRAC_PI_2 - visinf2_vd_vd_vd(x, FRAC_PI_4.mul_sign(x)),
                r,
            );
            r = y.simd_eq(ZERO).select(
                F64x::from_bits(x.is_sign_negative().to_int().cast() & PI.to_bits()),
                r,
            );

            F64x::from_bits((x.is_nan() | y.is_nan()).to_int().cast() | r.mul_sign(y).to_bits())
        }

        #[test]
        fn test_atan2() {
            test_ff_f(
                atan2,
                rug::Float::atan2,
                f64::MIN..=f64::MAX,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /// Arc sine function
        ///
        /// This function evaluates the arc sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn asin(d: F64x) -> F64x {
            let o = d.abs().simd_lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select_doubled(Doubled::from(d.abs()), x2.sqrt_as_doubled());
            x = d.abs().simd_eq(ONE).select_doubled(Doubled::from(ZERO), x);

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

            u *= x2 * x.0;

            let y = Doubled::<F64x>::splat(Doubled::new(
                crate::f64::D_PI.0 / 4.,
                crate::f64::D_PI.1 / 4.,
            ))
            .sub_checked(x)
            .sub_checked(u);

            let r = o.select(u + x.0, F64x::from(y) * F64x::splat(2.));
            r.mul_sign(d)
        }

        #[test]
        fn test_asin() {
            test_f_f(
                asin,
                rug::Float::asin,
                -1.0..=1.0,
                1.
            );
        }

        /// Arc cosine function
        ///
        /// This function evaluates the arc cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn acos(d: F64x) -> F64x {
            let o = d.abs().simd_lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select_doubled(Doubled::from(d.abs()), x2.sqrt_as_doubled());
            x = d.abs().simd_eq(ONE).select_doubled(Doubled::from(ZERO), x);

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

            u *= x2 * x.0;

            let mut y = Doubled::<F64x>::splat(Doubled::new(
                crate::f64::D_PI.0 / 2.,
                crate::f64::D_PI.1 / 2.,
            ))
            .sub_checked(x.0.mul_sign(d).add_checked_as_doubled(u.mul_sign(d)));
            x = x.add_checked(u);

            y = o.select_doubled(y, x.scale(F64x::splat(2.)));

            y = (!o & d.simd_lt(ZERO)).select_doubled(
                Doubled::<F64x>::splat(crate::f64::D_PI).sub_checked(y),
                y,
            );

            y.into()
        }

        #[test]
        fn test_acos() {
            test_f_f(
                acos,
                rug::Float::acos,
                -1.0..=1.0,
                1.
            );
        }

        /// Arc tangent function
        ///
        /// This function evaluates the arc tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn atan(d: F64x) -> F64x {
            let d2 = atan2k_u1(Doubled::from(d.abs()), Doubled::from(ONE));
            let mut r = F64x::from(d2);
            r = d
                .is_infinite()
                .select(F64x::splat(1.570_796_326_794_896_557_998_982), r);
            r.mul_sign(d)
        }

        #[test]
        fn test_atan() {
            test_f_f(
                atan,
                rug::Float::atan,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /// Hyperbolic sine function
        ///
        /// This function evaluates the hyperbolic sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP` if ***a*** is in `[-709, 709]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct
        /// sign or a correct value with `1.0 ULP` error bound is returned.
        pub fn sinh(x: F64x) -> F64x {
            let mut y = x.abs();
            let mut d = expk2(Doubled::from(y));
            d = d.sub_checked(d.recip());
            y = F64x::from(d) * HALF;

            y = (x.abs().simd_gt(F64x::splat(710.)) | y.is_nan()).select(INFINITY, y);
            y = y.mul_sign(x);
            F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_sinh() {
            test_f_f(
                sinh,
                rug::Float::sinh,
                -709.0..=709.0,
                1.
            );
        }

        /// Hyperbolic cosine function
        ///
        /// This function evaluates the hyperbolic cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP` if ***a** is in `[-709, 709]`.
        /// If a is a finite value out of this range, infinity with a correct
        /// sign or a correct value with `1.0 ULP` error bound is returned.
        pub fn cosh(x: F64x) -> F64x {
            let mut y = x.abs();
            let mut d = expk2(Doubled::from(y));
            d = d.add_checked(d.recip());
            y = F64x::from(d) * HALF;

            y = (x.abs().simd_gt(F64x::splat(710.)) | y.is_nan()).select(INFINITY, y);
            F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_cosh() {
            test_f_f(
                cosh,
                rug::Float::cosh,
                -709.0..=709.0,
                1.
            );
        }

        /// Hyperbolic tangent function
        ///
        /// This function evaluates the hyperbolic tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn tanh(x: F64x) -> F64x {
            let mut y = x.abs();
            let mut d = expk2(Doubled::from(y));
            let e = d.recip();
            d = (d + (-e)) / (d + e);
            y = F64x::from(d);

            y = (x.abs().simd_gt(F64x::splat(18.714_973_875)) | y.is_nan()).select(ONE, y);
            y = y.mul_sign(x);
            F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_tanh() {
            test_f_f(
                tanh,
                rug::Float::tanh,
                -19.0..=19.0,
                1.
            );
        }

        #[inline]
        fn logk2(d: Doubled<F64x>) -> Doubled<F64x> {
            let e = ilogbk(d.0 * F64x::splat(1. / 0.75));

            let m = Doubled::new(ldexp2k(d.0, -e), ldexp2k(d.1, -e));

            let x = (m + F64x::splat(-1.)) / (m + ONE);
            let x2 = x.square();

            let x4 = x2.0 * x2.0;
            let x8 = x4 * x4;

            let t = F64x::poly7(
                x2.0,
                x4,
                x8,
                0.138_604_363_904_671_679_108_56,
                0.131_699_838_841_615_374_240_845,
                0.153_914_168_346_271_945_653_214,
                0.181_816_523_941_564_611_721_589,
                0.222_222_246_326_620_354_039_96,
                0.285_714_285_511_134_091_777_308,
                0.400_000_000_000_914_013_309_483,
            )
            .mla(x2.0, F64x::splat(0.666_666_666_666_664_853_302_393));

            let mut s = Doubled::<F64x>::splat(crate::f64::D_LN2) * e.cast();
            s = s.add_checked(x.scale(F64x::splat(2.)));
            s.add_checked(x2 * x * t)
        }

        /// Inverse hyperbolic sine function
        ///
        /// This function evaluates the inverse hyperbolic sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP` if ***a*** is in `[-1.34e+154, 1.34e+154]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct
        /// sign or a correct value with `1.0 ULP` error bound is returned.
        pub fn asinh(x: F64x) -> F64x {
            let mut y = x.abs();
            let o = y.simd_gt(ONE);

            let mut d = o.select_doubled(x.recip_as_doubled(), Doubled::from(y));
            d = (d.square() + ONE).sqrt();
            d = o.select_doubled(d * y, d);

            d = logk2((d + x).normalize());
            y = F64x::from(d);

            y = (x.abs().simd_gt(SQRT_DBL_MAX) | y.is_nan()).select(INFINITY.mul_sign(x), y);

            y = F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits());
            x.is_neg_zero().select(NEG_ZERO, y)
        }

        #[test]
        fn test_asinh() {
            test_f_f(
                asinh,
                rug::Float::asinh,
                -crate::f64::SQRT_DBL_MAX..=crate::f64::SQRT_DBL_MAX,
                1.
            );
        }

        /// Inverse hyperbolic cosine function
        ///
        /// This function evaluates the inverse hyperbolic cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP` if ***a*** is in `[-1.34e+154, 1.34e+154]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct
        /// sign or a correct value with `1.0 ULP` error bound is returned.
        pub fn acosh(x: F64x) -> F64x {
            let d = logk2(x.add_as_doubled(ONE).sqrt() * x.add_as_doubled(F64x::splat(-1.)).sqrt() + x);
            let mut y = F64x::from(d);

            y = (x.abs().simd_gt(SQRT_DBL_MAX) | y.is_nan()).select(INFINITY, y);
            y = F64x::from_bits(!x.simd_eq(ONE).to_int().cast::<u64>() & y.to_bits());

            y = F64x::from_bits(x.simd_lt(ONE).to_int().cast() | y.to_bits());
            F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_acosh() {
            test_f_f(
                acosh,
                rug::Float::acosh,
                -crate::f64::SQRT_DBL_MAX..=crate::f64::SQRT_DBL_MAX,
                1.
            );
        }

        /// Inverse hyperbolic tangent function
        ///
        /// This function evaluates the inverse hyperbolic tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn atanh(x: F64x) -> F64x {
            let mut y = x.abs();
            let d = logk2(ONE.add_as_doubled(y) / ONE.add_as_doubled(-y));
            y = F64x::from_bits(
                y.simd_gt(ONE).to_int().cast()
                    | y.simd_eq(ONE).select(INFINITY, (d.0 + d.1) * HALF).to_bits(),
            );

            y = y.mul_sign(x);
            y = F64x::from_bits((x.is_infinite() | y.is_nan()).to_int().cast() | y.to_bits());
            F64x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_atanh() {
            test_f_f(
                atanh,
                rug::Float::atanh,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /// Natural logarithmic function
        ///
        /// This function returns the natural logarithm of ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn log(mut d: F64x) -> F64x {
            let m: F64x;
            let mut s =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE));
                    d = o.select(d * (D1_32X * D1_32X), d);
                    let mut e = ilogb2k(d * F64x::splat(1. / 0.75));
                    m = ldexp3k(d, -e);
                    e = o.cast().select(e - Ix::splat(64), e);
                    Doubled::<F64x>::splat(crate::f64::D_LN2) * e.cast()
                }/* else {
                    let mut e = vgetexp_vd_vd(d * F64x::splat(1. / 0.75));
                    e = e.simd_eq(INFINITY).select(F64x::splat(1024.), e);
                    m = vgetmant_vd_vd(d);
                    Doubled::<F64x>::splat(crate::f64::D_LN2) * e
                }*/;

            let x = F64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F64x::splat(0.153_207_698_850_270_135_3)
                .mla(x2, F64x::splat(0.152_562_905_100_342_871_6))
                .mla(x2, F64x::splat(0.181_860_593_293_778_599_6))
                .mla(x2, F64x::splat(0.222_221_451_983_938_000_9))
                .mla(x2, F64x::splat(0.285_714_293_279_429_931_7))
                .mla(x2, F64x::splat(0.399_999_999_963_525_199))
                .mla(x2, F64x::splat(0.666_666_666_666_733_354_1));

            s = s.add_checked(x.scale(F64x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let r = F64x::from(s);

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            let r = d.simd_eq(INFINITY).select(INFINITY, r);
            let r = (d.simd_lt(ZERO) | d.is_nan()).select(NAN, r);
            d.simd_eq(ZERO).select(NEG_INFINITY, r)
            /*} else {
                vfixup_vd_vd_vd_vi2_i(
                    r,
                    d,
                    I64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[test]
        fn test_log() {
            test_f_f(
                log,
                rug::Float::ln,
                0.0..=f64::MAX,
                1.
            );
        }

        /// Base-10 logarithmic function
        ///
        /// This function returns the base-10 logarithm of ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn log10(mut d: F64x) -> F64x {
            let m: F64x;

            let mut s = /*if !cfg!(feature = "enable_avx512f")
                        && !cfg!(feature = "enable_avx512fnofma")*/
                    {
                        let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE));
                        d = o.select(d * (D1_32X * D1_32X), d);
                        let mut e = ilogb2k(d * F64x::splat(1. / 0.75));
                        m = ldexp3k(d, -e);
                        e = o.cast().select(e - Ix::splat(64), e);
                        Doubled::new(
                            F64x::splat(0.301_029_995_663_981_198_02),
                            F64x::splat(-2.803_728_127_785_170_339_e-18)
                        ) * e.cast()
                    }/* else {
                        let mut e = vgetexp_vd_vd(d * F64x::splat(1. / 0.75));
                        e = e.simd_eq(INFINITY).select(F64x::splat(1024.), e);
                        m = vgetmant_vd_vd(d);
                        Doubled::new(
                            F64x::splat(0.301_029_995_663_981_198_02),
                            F64x::splat(-2.803_728_127_785_170_339_e-18)
                        ) * e
                    }*/;

            let x = F64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;
            let x4 = x2 * x2;
            let x8 = x4 * x4;

            let t = F64x::poly7(x2, x4, x8,
                0.665_372_581_957_675_846_e-1,
                0.662_572_278_282_083_371_2_e-1,
                0.789_810_521_431_394_407_8_e-1,
                0.965_095_503_571_527_513_2_e-1,
                0.124_084_140_972_144_499_3,
                0.173_717_792_745_460_508_6,
                0.289_529_654_602_197_261_7);

            s = s.add_checked(
                x * Doubled::new(
                    F64x::splat(0.868_588_963_806_503_633_34),
                    F64x::splat(1.143_005_969_409_638_931_1_e-17),
                ),
            );
            s = s.add_checked(x2 * x.0 * t);

            let r = F64x::from(s);

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            let r = d.simd_eq(INFINITY).select(INFINITY, r);
            let r = (d.simd_lt(ZERO) | d.is_nan()).select(NAN, r);
            d.simd_eq(ZERO).select(NEG_INFINITY, r)
            /*} else {
                vfixup_vd_vd_vd_vi2_i(
                    r,
                    d,
                    I64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[test]
        fn test_log10() {
            test_f_f(
                log10,
                rug::Float::log10,
                0.0..=f64::MAX,
                1.
            );
        }

        /// Base-2 logarithmic function
        ///
        /// This function returns the base-2 logarithm of ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn log2(mut d: F64x) -> F64x {
            let m: F64x;
            let ef =
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE));
                d = o.select(d * (D1_32X * D1_32X), d);
                let mut e = ilogb2k(d * F64x::splat(1. / 0.75));
                m = ldexp3k(d, -e);
                e = o.cast().select(e - Ix::splat(64), e);
                e.cast()
            }/* else {
                let e = vgetexp_vd_vd(d * F64x::splat(1.0 / 0.75));
                e = e.simd_eq(INFINITY).select(F64x::splat(1024.), e);
                m = vgetmant_vd_vd(d);
                e
            }*/;

            let x = F64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;
            let x4 = x2 * x2;
            let x8 = x4 * x4;

            let t = F64x::poly7(x2, x4, x8,
                0.221_194_175_045_608_149,
                0.220_076_869_315_227_768_9,
                0.262_370_805_748_851_465_6,
                0.320_597_747_794_449_550_2,
                0.412_198_594_548_532_470_9,
                0.577_078_016_299_705_898_2,
                0.961_796_693_926_080_914_49);

            let mut s =
                ef + x * Doubled::new(
                    F64x::splat(2.885_390_081_777_926_774),
                    F64x::splat(6.056_160_499_551_673_643_4_e-18)
                );
            s += x2 * x.0 * t;

            let r = F64x::from(s);

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            let r = d.simd_eq(INFINITY).select(INFINITY, r);
            let r = (d.simd_lt(ZERO) | d.is_nan()).select(NAN, r);
            d.simd_eq(ZERO).select(NEG_INFINITY, r)
            /*} else {
                vfixup_vd_vd_vd_vi2_i(
                    r,
                    d,
                    I64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[test]
        fn test_log2() {
            test_f_f(
                log2,
                rug::Float::log2,
                0.0..=f64::MAX,
                1.
            );
        }

        /// Logarithm of one plus argument
        ///
        /// This function returns the natural logarithm of (1+***a***).
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn log1p(d: F64x) -> F64x {
            let m: F64x;

            let mut dp1 = d + ONE;

            let mut s =
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                let o = dp1.simd_lt(F64x::splat(f64::MIN_POSITIVE));
                dp1 = o.select(dp1 * (D1_32X * D1_32X), dp1);
                let mut e = ilogb2k(dp1 * F64x::splat(1. / 0.75));
                let t = ldexp3k(ONE, -e);
                m = d.mla(t, t - ONE);
                e = o.cast().select(e - Ix::splat(64), e);
                Doubled::<F64x>::splat(crate::f64::D_LN2) * e.cast()
            }/* else {
                let e = vgetexp_vd_vd(dp1, F64x::splat(1. / 0.75));
                e = e.simd_eq(INFINITY).select(F64x::splat(1024.), e);
                let t = ldexp3k(ONE, -e.roundi());
                m = d.mla(t, t - ONE);
                Doubled::<F64x>::splat(crate::f64::D_LN2) * e
            }*/;

            let x = Doubled::from(m) / F64x::splat(2.).add_checked_as_doubled(m);
            let x2 = x.0 * x.0;
            let x4 = x2 * x2;
            let x8 = x4 * x4;

            let t = F64x::poly7(x2, x4, x8,
                0.153_207_698_850_270_135_3,
                0.152_562_905_100_342_871_6,
                0.181_860_593_293_778_599_6,
                0.222_221_451_983_938_000_9,
                0.285_714_293_279_429_931_7,
                0.399_999_999_963_525_199,
                0.666_666_666_666_733_354_1);

            s = s.add_checked(x.scale(F64x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = F64x::from(s);

            r = d.simd_gt(F64x::splat(1e+307)).select(INFINITY, r);
            r = (d.simd_lt(F64x::splat(-1.)) | d.is_nan()).select(NAN, r);
            r = d.simd_eq(F64x::splat(-1.)).select(NEG_INFINITY, r);
            d.is_neg_zero().select(NEG_ZERO, r)
        }

        #[test]
        fn test_log1p() {
            test_f_f(
                log1p,
                rug::Float::ln_1p,
                -1.0..=1e+307,
                1.
            );
        }

        /// Base-*e* exponential function
        ///
        /// This function returns the value of *e* raised to ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn exp(d: F64x) -> F64x {
            let mut u = (d * R_LN2).round();
            let q = u.roundi();

            let s = u.mla(-L2_U, d);
            let s = u.mla(-L2_L, s);

            if cfg!(target_feature = "fma") {
                let s2 = s * s;
                let s4 = s2 * s2;
                let s8 = s4 * s4;
                u = F64x::poly10(s, s2, s4, s8,
                    0.208_127_637_823_716_445_7_e-8,
                    0.251_121_070_304_228_802_2_e-7,
                    0.275_576_262_816_949_119_2_e-6,
                    0.275_572_340_202_538_823_9_e-5,
                    0.248_015_868_747_968_626_4_e-4,
                    0.198_412_698_985_586_585_e-3,
                    0.138_888_888_891_449_779_7_e-2,
                    0.833_333_333_331_493_821_e-2,
                    0.416_666_666_666_660_259_8_e-1,
                    0.166_666_666_666_666_907_2)
                    .mla(s, HALF)
                    .mla(s, ONE)
                    .mla(s, ONE);

            } else {
                let s2 = s * s;
                let s4 = s2 * s2;
                let s8 = s4 * s4;

                u = F64x::poly10(s, s2, s4, s8,
                  2.088_606_211_072_836_875_363_41_e-9,
                    2.511_129_308_928_765_186_106_61_e-8,
                    2.755_739_112_349_004_718_933_38_e-7,
                    2.755_723_629_119_288_276_294_23_e-6,
                    2.480_158_715_923_547_299_879_1_e-5,
                    0.000_198_412_698_960_509_205_564_975,
                    0.001_388_888_888_897_744_922_079_62,
                    0.008_333_333_333_316_527_216_649_84,
                    0.041_666_666_666_666_504_759_142_2,
                    0.166_666_666_666_666_851_703_837)
                    .mla(s, HALF);

                u = ONE + (s * s).mla(u, s);
            }

            u = ldexp2k(u, q);

            u = d
                .simd_gt(F64x::splat(709.782_711_149_557_429_092_172_174_26))
                .select(INFINITY, u);
            F64x::from_bits(!d.simd_lt(F64x::splat(-1000.)).to_int().cast::<u64>() & u.to_bits())
        }

        #[test]
        fn test_exp() {
            test_f_f(
                exp,
                rug::Float::exp,
                -1000.0..=710.0,
                1.
            );
        }

        /// Base-10 exponential function
        ///
        /// This function returns 10 raised to ***a***.
        /// The error bound of the returned value is `1.09 ULP`.
        pub fn exp10(d: F64x) -> F64x {
            let mut u = (d * LOG10_2).round();
            let q = u.roundi();

            let s = u.mla(-L10_U, d);
            let s = u.mla(-L10_L, s);

            u = F64x::splat(0.241_146_349_833_426_765_2_e-3)
                .mla(s, F64x::splat(0.115_748_841_521_718_737_5_e-2))
                .mla(s, F64x::splat(0.501_397_554_678_973_365_9_e-2))
                .mla(s, F64x::splat(0.195_976_232_072_053_308_e-1))
                .mla(s, F64x::splat(0.680_893_639_944_678_413_8_e-1))
                .mla(s, F64x::splat(0.206_995_849_472_267_623_4))
                .mla(s, F64x::splat(0.539_382_929_205_853_622_9))
                .mla(s, F64x::splat(0.117_125_514_890_854_165_5_e+1))
                .mla(s, F64x::splat(0.203_467_859_229_343_295_3_e+1))
                .mla(s, F64x::splat(0.265_094_905_523_920_587_6_e+1))
                .mla(s, F64x::splat(0.230_258_509_299_404_590_1_e+1));

            if cfg!(target_feature = "fma") {
                u = u.mla(s, ONE);
            } else {
                u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
            }

            u = ldexp2k(u, q);

            u = d
                .simd_gt(F64x::splat(308.254_715_559_916_71))
                .select(INFINITY, u);
            F64x::from_bits(!d.simd_lt(F64x::splat(-350.)).to_int().cast::<u64>() & u.to_bits())
        }

        #[test]
        fn test_exp10() {
            test_f_f(
                exp10,
                rug::Float::exp10,
                -350.0..=308.26,
                1.09
            );
        }

        /// Base-*e* exponential function minus 1
        ///
        /// This function returns the value one less than *e* raised to ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn expm1(a: F64x) -> F64x {
            let d = expk2(Doubled::from(a)) + F64x::splat(-1.);
            let mut x = F64x::from(d);
            x = a
                .simd_gt(F64x::splat(709.782_712_893_383_996_732_223))
                .select(INFINITY, x);
            x = a
                .simd_lt(F64x::splat(-36.736_800_569_677_101_399_113_302_437))
                .select(F64x::splat(-1.), x);
            a.is_neg_zero().select(NEG_ZERO, x)
        }

        #[test]
        fn test_expm1() {
            test_f_f(
                expm1,
                rug::Float::exp_m1,
                -37.0..=710.0,
                1.
            );
        }

        /// Base-2 exponential function
        ///
        /// This function returns `2` raised to ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn exp2(d: F64x) -> F64x {
            let mut u = d.round();
            let q = u.roundi();

            let s = d - u;

            let s2 = s * s;
            let s4 = s2 * s2;
            let s8 = s4 * s4;

            u = F64x::poly10(s, s2, s4, s8,
                0.443_435_908_292_652_945_4_e-9,
                0.707_316_459_808_570_742_5_e-8,
                0.101_781_926_092_176_045_1_e-6,
                0.132_154_387_251_132_761_5_e-5,
                0.152_527_335_351_758_473_e-4,
                0.154_035_304_510_114_780_8_e-3,
                0.133_335_581_467_049_907_3_e-2,
                0.961_812_910_759_760_053_6_e-2,
                0.555_041_086_648_204_659_6_e-1,
                0.240_226_506_959_101_221_4)
                .mla(s, F64x::splat(0.693_147_180_559_945_286_2));

            if cfg!(target_feature = "fma") {
                u = u.mla(s, ONE);
            } else {
                u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
            }

            u = ldexp2k(u, q);

            u = d.simd_ge(F64x::splat(1024.)).select(INFINITY, u);
            F64x::from_bits(!d.simd_lt(F64x::splat(-2000.)).to_int().cast::<u64>() & u.to_bits())
        }

        #[test]
        fn test_exp2() {
            test_f_f(
                exp2,
                rug::Float::exp2,
                -2000.0..=1024.0,
                1.
            );
        }

        #[inline]
        fn expk(d: Doubled<F64x>) -> F64x {
            let mut u = F64x::from(d) * R_LN2;
            let dq = u.round();
            let q = dq.roundi();

            let mut s = d + dq * (-L2_U);
            s += dq * (-L2_L);

            s = s.normalize();

            let s2 = s.0 * s.0;
            let s4 = s2 * s2;
            let s8 = s4 * s4;

            u = F64x::poly10(
                s.0,
                s2,
                s4,
                s8,
                2.510_696_834_209_504_195_271_39_e-8,
                2.762_861_667_702_706_491_168_55_e-7,
                2.755_724_967_250_235_741_438_64_e-6,
                2.480_149_739_898_197_941_141_53_e-5,
                0.000_198_412_698_809_069_797_676_111,
                0.001_388_888_893_997_712_896_052_9,
                0.008_333_333_333_323_714_176_010_81,
                0.041_666_666_666_540_952_412_844_9,
                0.166_666_666_666_666_740_681_535,
                0.500_000_000_000_000_999_200_722,
            );

            let mut t = ONE.add_checked(s);
            t = t.add_checked(s.square() * u);

            u = F64x::from(t);
            u = ldexp2k(u, q);

            F64x::from_bits(!d.0.simd_lt(F64x::splat(-1000.)).to_int().cast::<u64>() & u.to_bits())
        }

        #[inline]
        fn logk(mut d: F64x) -> Doubled<F64x> {
            let m: F64x;

            let mut s =
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE));
                d = o.select(d * (D1_32X * D1_32X), d);
                let mut e = ilogb2k(d * F64x::splat(1. / 0.75));
                m = ldexp3k(d, -e);
                e = o.cast().select(e - Ix::splat(64), e);
                Doubled::<F64x>::splat(crate::f64::D_LN2) * e.cast()
            }/* else {
                let mut e = vgetexp_vd_vd(d * F64x::splat(1. / 0.75));
                e = e.simd_eq(INFINITY).select(F64x::splat(1024.), e);
                m = vgetmant_vd_vd(d);
                Doubled::<F64x>::splat(D_LN2) * e
            }*/;

            let x = F64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.square();

            let x4 = x2.0 * x2.0;
            let x8 = x4 * x4;
            let x16 = x8 * x8;

            let t = F64x::poly9(
                x2.0,
                x4,
                x8,
                x16,
                0.116_255_524_079_935_043_668_677,
                0.103_239_680_901_072_952_701_192,
                0.117_754_809_412_463_995_466_069,
                0.133_329_810_868_462_739_215_09,
                0.153_846_227_114_512_262_845_736,
                0.181_818_180_850_050_775_676_507,
                0.222_222_222_230_083_560_345_903,
                0.285_714_285_714_249_172_087_875,
                0.400_000_000_000_000_077_715_612,
            );

            let c = Doubled::new(
                F64x::splat(0.666_666_666_666_666_629_659_233),
                F64x::splat(3.805_549_625_424_120_563_366_16_e-17),
            );

            s = s.add_checked(x.scale(F64x::splat(2.)));
            s.add_checked(x2 * x * (x2 * t + c))
        }

        /// Power function
        ///
        /// This function returns the value of ***x*** raised to the power of ***y***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn pow(x: F64x, y: F64x) -> F64x {
            if true {
                let yisint = y.is_integer();
                let yisodd = y.is_odd() & yisint;

                let d = logk(x.abs()) * y;
                let mut result = expk(d);
                result =
                    d.0.simd_gt(F64x::splat(709.782_711_149_557_429_092_172_174_26))
                        .select(INFINITY, result);

                result *= x.simd_gt(ZERO).select(
                    ONE,
                    yisint.select(yisodd.select(F64x::splat(-1.), ONE), NAN),
                );

                let efx = (x.abs() - ONE).mul_sign(y);

                result = y.is_infinite().select(
                    F64x::from_bits(
                        !efx.simd_lt(ZERO).to_int().cast::<u64>()
                            & efx.simd_eq(ZERO).select(ONE, INFINITY).to_bits(),
                    ),
                    result,
                );

                result = (x.is_infinite() | x.simd_eq(ZERO)).select(
                    yisodd.select(x.sign(), ONE)
                        * F64x::from_bits(
                            !x.simd_eq(ZERO).select(-y, y).simd_lt(ZERO).to_int().cast::<u64>()
                                & INFINITY.to_bits(),
                        ),
                    result,
                );

                result =
                    F64x::from_bits((x.is_nan() | y.is_nan()).to_int().cast() | result.to_bits());

                (y.simd_eq(ZERO) | x.simd_eq(ONE)).select(ONE, result)
            } else {
                expk(logk(x) * y)
            }
        }

        #[test]
        fn test_pow() {
            use rug::{ops::Pow, Float};
            test_ff_f(
                pow,
                |in1, in2| Float::with_val(in1.prec(), in1.pow(in2)),
                f64::MIN..=f64::MAX,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /// Cube root function
        ///
        /// This function returns the real cube root of ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn cbrt(mut d: F64x) -> F64x {
            let mut q2 = Doubled::from(ONE);

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                let s = d;
            }*/
            let e = ilogbk(d.abs()) + Ix::splat(1);
            d = ldexp2k(d, -e);

            let t = e.cast::<f64>() + F64x::splat(6144.);
            let qu = (t * F64x::splat(1. / 3.)).trunci();
            let re = (t - qu.cast::<f64>() * F64x::splat(3.)).trunci();

            q2 = re.simd_eq(Ix::splat(1)).cast().select_doubled(
                Doubled::new(
                    F64x::splat(1.259_921_049_894_873_190_7),
                    F64x::splat(-2.589_933_375_300_506_917_7_e-17),
                ),
                q2,
            );
            q2 = re.simd_eq(Ix::splat(2)).cast().select_doubled(
                Doubled::new(
                    F64x::splat(1.587_401_051_968_199_583_4),
                    F64x::splat(-1.086_900_819_419_782_298_6_e-16),
                ),
                q2,
            );

            q2.0 = q2.0.mul_sign(d);
            q2.1 = q2.1.mul_sign(d);
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

            let mut z = x;

            let mut u = x.mul_as_doubled(x);
            u = u * u;
            u *= d;
            u += -x;
            y = F64x::from(u);

            y = F64x::splat(-2. / 3.) * y * z;
            let mut v = z.mul_as_doubled(z) + y;
            v *= d;
            v *= q2;
            z = ldexp2k(F64x::from(v), qu - Ix::splat(2048));

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            z = d.is_infinite().select(INFINITY.mul_sign(q2.0), z);
            d.simd_eq(ZERO).select(F64x::from_bits(q2.0.sign_bit()), z)
            /*} else {
                z = s.is_infinite().select(INFINITY.mul_sign(s), z);
                s.simd_eq(ZERO)
                    .select(ZERO.mul_sign(s), z)
            }*/
        }

        #[test]
        fn test_cbrt() {
            test_f_f(
                cbrt,
                rug::Float::cbrt,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        fn gammak(a: F64x) -> (Doubled<F64x>, Doubled<F64x>) {
            let mut clln = Doubled::from(ONE);
            let mut clld = Doubled::from(ONE);

            let otiny = a.abs().simd_lt(F64x::splat(1e-306));
            let oref = a.simd_lt(HALF);

            let mut x = otiny.select_doubled(
                Doubled::from(ZERO),
                oref.select_doubled(ONE.add_as_doubled(-a), Doubled::from(a)),
            );

            let o0 = HALF.simd_le(x.0) & x.0.simd_le(F64x::splat(1.1));
            let o2 = F64x::splat(2.3).simd_le(x.0);

            let mut y = ((x + ONE) * x).normalize();
            y = ((x + F64x::splat(2.)) * y).normalize();
            y = ((x + F64x::splat(3.)) * y).normalize();
            y = ((x + F64x::splat(4.)) * y).normalize();

            let o = o2 & x.0.simd_le(F64x::splat(7.));
            clln = o.select_doubled(y, clln);

            x = o.select_doubled(x + F64x::splat(5.), x);

            let t = o2.select(x.0.recip(), (x + o0.select_splat(-1., -2.)).normalize().0);

            let u = F64x::select3(
                o2,
                o0,
                -156.801_412_704_022_726_379_848_862,
                0.294_791_677_282_761_419_6_e+2,
                0.707_481_600_086_460_927_9_e-7,
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    1.120_804_464_289_911_606_838_558_16,
                    0.128_145_969_182_782_010_9_e+3,
                    0.400_924_433_300_873_044_3_e-6,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    13.397_985_455_142_589_218_333_060_2,
                    0.261_754_402_578_451_504_3_e+3,
                    0.104_011_464_162_824_694_6_e-5,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -0.116_546_276_599_463_200_848_033_357,
                    0.328_702_285_568_579_043_2_e+3,
                    0.150_834_915_073_332_916_7_e-5,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -1.391_801_093_265_337_481_495_562_41,
                    0.281_814_586_773_034_818_6_e+3,
                    0.128_814_307_493_390_102_e-5,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    0.015_056_113_040_026_424_412_918_973_4,
                    0.172_867_041_467_355_960_5_e+3,
                    0.474_416_774_988_499_393_7_e-6,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    0.179_540_117_061_234_856_098_844_714,
                    0.774_873_576_403_041_681_7_e+2,
                    -0.655_481_630_654_248_990_2_e-7,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -0.002_481_743_600_264_997_730_942_489_28,
                    0.251_285_664_308_093_075_2_e+2,
                    -0.318_925_247_145_259_984_4_e-6,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -0.029_527_880_945_699_120_504_851_034_1,
                    0.576_679_210_614_007_686_8_e+1,
                    0.135_888_382_147_035_537_7_e-6,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    0.000_540_164_767_892_604_515_196_325_186,
                    0.727_027_547_399_618_057_1,
                    -0.434_393_127_715_733_604_e-6,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    0.006_403_362_833_808_069_794_787_256_2,
                    0.839_670_912_457_914_780_9_e-1,
                    0.972_478_589_740_677_955_5_e-6,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -0.000_162_516_262_783_915_816_896_611_252,
                    -0.821_155_866_974_680_459_5_e-1,
                    -0.203_688_605_722_596_601_1_e-5,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -0.001_914_438_498_565_477_526_465_972_39,
                    0.682_883_182_834_188_445_8_e-1,
                    0.437_336_314_181_972_581_5_e-5,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    7.204_895_416_020_010_558_983_115_17_e-5,
                    -0.771_248_133_996_167_151_1_e-1,
                    -0.943_995_126_830_400_867_7_e-5,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    0.000_839_498_720_672_087_279_971_000_786,
                    0.833_749_202_301_731_495_7_e-1,
                    0.205_072_703_037_638_980_4_e-4,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -5.171_790_908_260_592_193_293_944_22_e-5,
                    -0.909_496_493_145_624_251_8_e-1,
                    -0.449_262_018_343_118_401_8_e-4,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -0.000_592_166_437_353_693_882_857_342_347,
                    0.100_099_631_357_592_935_8,
                    0.994_575_123_607_187_593_1_e-4,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    6.972_813_758_365_857_774_037_435_39_e-5,
                    -0.111_334_286_154_420_772_4,
                    -0.223_154_759_903_498_319_6_e-3,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    0.000_784_039_221_720_066_627_493_314_301,
                    0.125_509_667_321_302_087_5,
                    0.509_669_524_710_196_762_2_e-3,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -0.000_229_472_093_621_399_176_949_318_732,
                    -0.144_049_896_784_305_436_8,
                    -0.119_275_391_166_788_697_1_e-2,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    -0.002_681_327_160_493_827_160_473_958_490,
                    0.169_557_177_004_194_981_1,
                    0.289_051_033_074_221_031_e-2,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    0.003_472_222_222_222_222_222_175_164_840,
                    -0.207_385_551_028_409_276_2,
                    -0.738_555_102_867_446_185_8_e-2,
                ),
            )
            .mla(
                t,
                F64x::select3(
                    o2,
                    o0,
                    0.083_333_333_333_333_333_335_592_087_900,
                    0.270_580_808_427_781_593_9,
                    0.205_808_084_277_845_533_5_e-1,
                ),
            );

            let mut y = (x + F64x::splat(-0.5)) * logk2(x);
            y += -x;
            y += Doubled::new(
                F64x::splat(0.918_938_533_204_672_780_56),
                F64x::splat(-3.878_294_158_067_241_449_8_e-17),
            ); // 0.5*log(2*M_PI)

            let mut z = u.mul_as_doubled(t)
                + o0.select_splat(
                    -0.400_685_634_386_531_486_2,
                    -0.673_523_010_531_981_020_1_e-1,
                );
            z = z * t + o0.select_splat(0.822_467_033_424_113_203, 0.322_467_033_424_113_203);
            z = z * t + o0.select_splat(-0.577_215_664_901_532_865_5, 0.422_784_335_098_467_134_5);
            z *= t;

            let mut clc = o2.select_doubled(y, z);

            clld = o2.select_doubled(u.mul_as_doubled(t) + ONE, clld);

            y = clln;

            clc = otiny.select_doubled(
                Doubled::new(
                    F64x::splat(83.177_661_667_193_433_459_033_3),
                    F64x::splat(3.671_034_596_315_685_072_218_78_e-15),
                ), // log(2^120)
                oref.select_doubled(
                    Doubled::new(
                        F64x::splat(1.144_729_885_849_400_163_9),
                        F64x::splat(1.026_595_116_270_782_638_e-17)
                    ) + (-clc),
                    clc,
                ),
            ); // log(M_PI)
            clln = otiny.select_doubled(Doubled::from(ONE), oref.select_doubled(clln, clld));

            if !(!oref).all() {
                let t = a - D1_28X * (a * (ONE / D1_28X)).trunci().cast();
                x = clld * sinpik(t);
            }

            clld = otiny.select_doubled(
                Doubled::from(a * (D1_60X * D1_60X)),
                oref.select_doubled(x, y),
            );

            (clc, clln / clld)
        }

        /// Gamma function
        ///
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn tgamma(a: F64x) -> F64x {
            let (da, db) = gammak(a);
            let y = expk2(da) * db;
            let r = F64x::from(y);
            let o = a.simd_eq(NEG_INFINITY)
                | (a.simd_lt(ZERO) & a.is_integer())
                | (a.is_finite() & a.simd_lt(ZERO) & r.is_nan());
            let r = o.select(NAN, r);

            let o = ((a.simd_eq(INFINITY) | a.is_finite()) & a.simd_ge(F64x::splat(-f64::MIN_POSITIVE)))
                & (a.simd_eq(ZERO) | a.simd_gt(F64x::splat(200.)) | r.is_nan());
            o.select(INFINITY.mul_sign(a), r)
        }

        #[test]
        fn test_tgamma() {
            test_f_f(
                tgamma,
                rug::Float::gamma,
                f64::MIN..=f64::MAX,
                1.
            );
        }

        /// Log gamma function
        ///
        /// The error bound of the returned value is `1.0 ULP` if the argument is positive.
        /// If the argument is larger than `2e+305`, it may return infinity instead of the correct value.
        /// The error bound is `max(1 ULP and 1e-15)`, if the argument is negative.
        pub fn lgamma(a: F64x) -> F64x {
            let (da, db) = gammak(a);
            let y = da + logk2(db.abs());
            let r = F64x::from(y);

            let o = a.is_infinite() | (a.simd_le(ZERO) & a.is_integer()) | (a.is_finite() & r.is_nan());
            o.select(INFINITY, r)
        }

        #[test]
        fn test_lgamma() {
            test_f_f(
                lgamma,
                rug::Float::ln_gamma,
                0.0..=2e305,
                1.
            );
        }

        fn ddmla(x: F64x, y: Doubled<F64x>, z: Doubled<F64x>) -> Doubled<F64x> {
            z + (y * x)
        }
        fn poly2dd_b(x: F64x, c1: Doubled<F64x>, c0: Doubled<F64x>) -> Doubled<F64x> {
            ddmla(x, c1, c0)
        }
        fn poly2dd(x: F64x, c1: F64x, c0: Doubled<F64x>) -> Doubled<F64x> {
            ddmla(x, Doubled::from(c1), c0)
        }
        fn poly4dd(x: F64x, c3: F64x, c2: Doubled<F64x>, c1: Doubled<F64x>, c0: Doubled<F64x>) -> Doubled<F64x> {
            ddmla(x*x, poly2dd(x, c3, c2), poly2dd_b(x, c1, c0))
        }

        /// Error function
        ///
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn erf(a: F64x) -> F64x {
            let x = a.abs();
            let x2 = x * x;
            let x4 = x2 * x2;
            let x8 = x4 * x4;
            let x16 = x8 * x8;
            let o25 = x.simd_le(F64x::splat(2.5));

            let mut t2;
            if o25.all() {
                // Abramowitz and Stegun
                let t = F64x::poly21(x, x2, x4, x8, x16,
                    -0.208_327_100_252_522_209_7_e-14,
                    0.715_190_997_079_089_700_9_e-13,
                    -0.116_223_822_011_099_936_4_e-11,
                    0.118_647_423_082_158_525_9_e-10,
                    -0.849_997_317_835_461_344_0_e-10,
                    0.450_764_746_259_884_162_9_e-9,
                    -0.180_804_447_428_884_891_5_e-8,
                    0.543_508_182_671_621_238_9_e-8,
                    -0.114_393_989_575_862_848_4_e-7,
                    0.121_544_236_268_088_924_3_e-7,
                    0.166_987_875_618_125_035_5_e-7,
                    -0.980_807_460_225_519_428_8_e-7,
                    0.138_900_055_786_583_720_4_e-6,
                    0.294_551_452_998_733_186_6_e-6,
                    -0.184_291_827_300_399_828_3_e-5,
                    0.341_798_783_611_536_213_6_e-5,
                    0.386_023_635_649_312_910_1_e-5,
                    -0.330_940_307_274_994_754_6_e-4,
                    0.106_086_292_259_757_953_2_e-3,
                    0.232_325_315_521_307_617_4_e-3,
                    0.149_014_971_914_554_472_9_e-3);
                t2 = poly4dd(x, t,
                    Doubled::new(
                        F64x::splat(0.009_287_795_839_227_560_440_5),
                        F64x::splat(7.928_755_946_396_110_749_3_e-19)
                    ),
                    Doubled::new(
                        F64x::splat(0.042_275_531_758_784_692_937),
                        F64x::splat(1.378_522_662_050_101_613_8_e-19)
                    ),
                    Doubled::new(
                        F64x::splat(0.070_523_697_943_469_534_91),
                        F64x::splat(9.584_662_807_079_209_284_2_e-19)
                    )
                );
                t2 = ONE.add_checked(t2 * x);
                t2 = t2.square();
                t2 = t2.square();
                t2 = t2.square();
                t2 = t2.square();
                t2 = t2.recip();
            } else {
                let t = F64x::poly21(x, x2, x4, x8, x16,
                    o25.select_splat(-0.208_327_100_252_522_209_7_e-14, -0.402_401_513_075_262_193_2_e-18),
                    o25.select_splat(0.715_190_997_079_089_700_9_e-13, 0.384_719_333_281_704_817_2_e-16),
                    o25.select_splat(-0.116_223_822_011_099_936_4_e-11, -0.174_931_624_145_564_408_8_e-14),
                    o25.select_splat(0.118_647_423_082_158_525_9_e-10, 0.502_961_832_287_287_271_5_e-13),
                    o25.select_splat(-0.849_997_317_835_461_344_0_e-10, -0.102_522_146_685_146_316_4_e-11),
                    o25.select_splat(0.450_764_746_259_884_162_9_e-9, 0.157_369_555_933_194_558_3_e-10),
                    o25.select_splat(-0.180_804_447_428_884_891_5_e-8, -0.188_465_855_804_020_370_9_e-9),
                    o25.select_splat(0.543_508_182_671_621_238_9_e-8, 0.179_816_785_303_215_930_9_e-8),
                    o25.select_splat(-0.114_393_989_575_862_848_4_e-7, -0.138_074_534_235_503_314_2_e-7),
                    o25.select_splat(0.121_544_236_268_088_924_3_e-7, 0.852_570_572_646_910_349_9_e-7),
                    o25.select_splat(0.166_987_875_618_125_035_5_e-7, -0.416_044_805_810_130_340_5_e-6),
                    o25.select_splat(-0.980_807_460_225_519_428_8_e-7, 0.151_727_266_000_858_848_5_e-5),
                    o25.select_splat(0.138_900_055_786_583_720_4_e-6, -0.334_163_412_731_720_169_7_e-5),
                    o25.select_splat(0.294_551_452_998_733_186_6_e-6, -0.251_502_339_587_972_451_3_e-5),
                    o25.select_splat(-0.184_291_827_300_399_828_3_e-5, 0.653_973_126_966_490_755_4_e-4),
                    o25.select_splat(0.341_798_783_611_536_213_6_e-5, -0.355_106_509_742_838_865_8_e-3),
                    o25.select_splat(0.386_023_635_649_312_910_1_e-5, 0.121_073_609_795_836_886_4_e-2),
                    o25.select_splat(-0.330_940_307_274_994_754_6_e-4, -0.260_556_691_257_999_868_e-2),
                    o25.select_splat(0.106_086_292_259_757_953_2_e-3, 0.125_282_320_243_609_319_3_e-2),
                    o25.select_splat(0.232_325_315_521_307_617_4_e-3, 0.182_019_139_526_331_322_2_e-1),
                    o25.select_splat(0.149_014_971_914_554_472_9_e-3, -0.102_155_715_545_346_595_4));
                t2 = poly4dd(x, t,
                    o25.select_doubled(
                        Doubled::new(
                            F64x::splat(0.009_287_795_839_227_560_440_5),
                            F64x::splat(7.928_755_946_396_110_749_3_e-19)
                        ),
                        Doubled::new(
                            F64x::splat(-0.636_910_443_836_417_483_61),
                            F64x::splat(-2.424_947_752_653_943_183_9_e-17)
                        )
                    ),
                    o25.select_doubled(
                        Doubled::new(
                            F64x::splat(0.042_275_531_758_784_692_937),
                            F64x::splat(1.378_522_662_050_101_613_8_e-19)
                        ),
                        Doubled::new(
                            F64x::splat(-1.128_292_606_180_396_173_7),
                            F64x::splat(-6.297_033_886_041_099_650_5_e-17)
                        )
                    ),
                    o25.select_doubled(
                        Doubled::new(
                            F64x::splat(0.070_523_697_943_469_534_91),
                            F64x::splat(9.584_662_807_079_209_284_2_e-19)
                        ),
                        Doubled::new(
                            F64x::splat(-1.226_131_378_518_480_496_7_e-05),
                            F64x::splat(-5.532_970_751_449_010_704_4_e-22)
                        )
                    )
                );
                let mut s2 = ONE.add_checked(t2 * x);
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.recip();
                t2 = o25.select_doubled(s2, Doubled::from(expk(t2)));
            }

            t2 += F64x::splat(-1.);

            let mut z = -F64x::from(t2);
            z = x.simd_lt(F64x::splat(1e-8)).select(x * F64x::splat(1.128_379_167_095_512_627_562_454_759_59), z);
            z = x.simd_ge(F64x::splat(6.)).select(ONE, z);
            z = a.is_infinite().select(ONE, z);
            z = a.simd_eq(ZERO).select(ZERO, z);
            z.mul_sign(a)
        }

        #[test]
        fn test_erf() {
            test_f_f(
                erf,
                rug::Float::erf,
                f64::MIN..=f64::MAX,
                0.75
            );
        }
    };
}
