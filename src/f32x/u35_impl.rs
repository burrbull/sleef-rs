macro_rules! impl_math_f32_u35 {
    () => {
        use super::*;

        /// Sine function
        ///
        /// These functions evaluates the sine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn sinf(mut d: F32x) -> F32x {
            let mut q: I32x;
            let u: F32x;
            let r = d;

            if d.abs().simd_lt(TRIGRANGEMAX2_F).all() {
                q = (d * FRAC_1_PI).roundi();
                u = q.cast();
                d = u.mul_add(-PI_A2_F, d);
                d = u.mul_add(-PI_B2_F, d);
                d = u.mul_add(-PI_C2_F, d);
            } else if d.abs().simd_lt(TRIGRANGEMAX_F).all() {
                q = (d * FRAC_1_PI).roundi();
                u = q.cast();
                d = u.mul_add(-PI_A_F, d);
                d = u.mul_add(-PI_B_F, d);
                d = u.mul_add(-PI_C_F, d);
                d = u.mul_add(-PI_D_F, d);
            } else {
                let (mut dfidf, dfii) = rempif(d);
                q = dfii & I32x::splat(3);
                q = q + q + dfidf.0.simd_gt(ZERO).select(I32x::splat(2), I32x::splat(1));
                q >>= I32x::splat(2);
                let o = (dfii & I32x::splat(1)).simd_eq(I32x::splat(1));
                let mut x = Doubled::new(
                    F32x::splat(crate::f32::D_PI.0 * -0.5).mul_sign(dfidf.0),
                    F32x::splat(crate::f32::D_PI.1 * -0.5).mul_sign(dfidf.0),
                );
                x = dfidf + x;
                dfidf = o.select_doubled(x, dfidf);
                d = F32x::from(dfidf);

                d = F32x::from_bits((r.is_infinite() | r.is_nan()).to_int().cast() | d.to_bits());
            }

            let s = d * d;

            d = F32x::from_bits(
                ((q & I32x::splat(1)).simd_eq(I32x::splat(1)).to_int().cast() & NEG_ZERO.to_bits())
                    ^ d.to_bits(),
            );

            let mut u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s, F32x::splat(0.008_333_078_585_565_090_179_443_36))
                .mul_add(s, F32x::splat(-0.166_666_597_127_914_428_710_938));

            u = s * (u * d) + d;

            r.is_neg_zero().select(r, u)
        }

        /// Sine function
        ///
        /// These functions evaluates the sine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn sinf_deterministic(mut d: F32x) -> F32x {
            let r = d;

            let mut q = (d * FRAC_1_PI).roundi();
            let u: F32x = q.cast();
            d = u.mul_add(-PI_A2_F, d);
            d = u.mul_add(-PI_B2_F, d);
            d = u.mul_add(-PI_C2_F, d);
            let g = r.abs().simd_lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let s: F32x = q.cast();
                let mut u = s.mul_add(-PI_A_F, r);
                u = s.mul_add(-PI_B_F, u);
                u = s.mul_add(-PI_C_F, u);
                u = s.mul_add(-PI_D_F, u);

                d = g.select(d, u);
                let g = r.abs().simd_lt(TRIGRANGEMAX_F);

                if !g.all() {
                    let (mut dfidf, dfii) = rempif(d);
                    let mut q2 = dfii & I32x::splat(3);
                    q2 = q2 + q2 + dfidf.0.simd_gt(ZERO).select(I32x::splat(2), I32x::splat(1));
                    q2 >>= I32x::splat(2);
                    let o = (dfii & I32x::splat(1)).simd_eq(I32x::splat(1));
                    let mut x = Doubled::new(
                        F32x::splat(crate::f32::D_PI.0 * -0.5).mul_sign(dfidf.0),
                        F32x::splat(crate::f32::D_PI.1 * -0.5).mul_sign(dfidf.0),
                    );
                    x = dfidf + x;
                    dfidf = o.select_doubled(x, dfidf);
                    u = F32x::from(dfidf);

                    u = F32x::from_bits((r.is_infinite() | r.is_nan()).to_int().cast() | u.to_bits());

                    q = g.select(q, q2);
                    d = g.select(d, u);
                }
            }

            let s = d * d;

            d = F32x::from_bits(
                ((q & I32x::splat(1)).simd_eq(I32x::splat(1)).to_int().cast() & NEG_ZERO.to_bits())
                    ^ d.to_bits(),
            );

            let mut u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s, F32x::splat(0.008_333_078_585_565_090_179_443_36))
                .mul_add(s, F32x::splat(-0.166_666_597_127_914_428_710_938));

            u = s * (u * d) + d;

            r.is_neg_zero().select(r, u)
        }

        #[test]
        fn test_sinf() {
            test_f_f(
                sinf,
                rug::Float::sin,
                f32::MIN..=f32::MAX,
                3.5,
            );
            test_f_f(
                sinf_deterministic,
                rug::Float::sin,
                f32::MIN..=f32::MAX,
                3.5,
            );
        }

        /// Cosine function
        ///
        /// These functions evaluates the cosine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn cosf(mut d: F32x) -> F32x {
            let mut q: I32x;
            let r = d;

            if d.abs().simd_lt(TRIGRANGEMAX2_F).all() {
                q = (d * FRAC_1_PI - HALF).roundi();
                q = q + q + I32x::splat(1);

                let u: F32x = q.cast();
                d = u.mul_add(-PI_A2_F * HALF, d);
                d = u.mul_add(-PI_B2_F * HALF, d);
                d = u.mul_add(-PI_C2_F * HALF, d);
            } else if d.abs().simd_lt(TRIGRANGEMAX_F).all() {
                q = (d * FRAC_1_PI - HALF).roundi();
                q = q + q + I32x::splat(1);

                let u: F32x = q.cast();
                d = u.mul_add(-PI_A_F * HALF, d);
                d = u.mul_add(-PI_B_F * HALF, d);
                d = u.mul_add(-PI_C_F * HALF, d);
                d = u.mul_add(-PI_D_F * HALF, d);
            } else {
                let (mut dfidf, dfii) = rempif(d);
                q = dfii & I32x::splat(3);
                q = q + q + dfidf.0.simd_gt(ZERO).select(I32x::splat(8), I32x::splat(7));
                q >>= I32x::splat(1);
                let o = (dfii & I32x::splat(1)).simd_eq(I32x::splat(0));
                let y = dfidf.0.simd_gt(ZERO).select(ZERO, F32x::splat(-1.));
                let mut x = Doubled::new(
                    F32x::splat(crate::f32::D_PI.0 * -0.5).mul_sign(y),
                    F32x::splat(crate::f32::D_PI.1 * -0.5).mul_sign(y),
                );
                x = dfidf + x;
                dfidf = o.select_doubled(x, dfidf);
                d = F32x::from(dfidf);

                d = F32x::from_bits((r.is_infinite() | r.is_nan()).to_int().cast() | d.to_bits());
            }

            let s = d * d;

            d = F32x::from_bits(
                ((q & I32x::splat(2)).simd_eq(I32x::splat(0)).to_int().cast() & NEG_ZERO.to_bits())
                    ^ d.to_bits(),
            );

            let u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s, F32x::splat(0.008_333_078_585_565_090_179_443_36))
                .mul_add(s, F32x::splat(-0.166_666_597_127_914_428_710_938));

            s * (u * d) + d
        }

        /// Cosine function
        ///
        /// These functions evaluates the cosine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn cosf_deterministic(mut d: F32x) -> F32x {
            let r = d;

            let mut q = (d * FRAC_1_PI - HALF).roundi();
            q = q + q + I32x::splat(1);
            let u: F32x = q.cast();
            d = u.mul_add(-PI_A2_F * HALF, d);
            d = u.mul_add(-PI_B2_F * HALF, d);
            d = u.mul_add(-PI_C2_F * HALF, d);
            let g = r.abs().simd_lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let s: F32x = q.cast();
                let mut u = s.mul_add(-PI_A_F * HALF, r);
                u = s.mul_add(-PI_B_F * HALF, u);
                u = s.mul_add(-PI_C_F * HALF, u);
                u = s.mul_add(-PI_D_F * HALF, u);

                d = g.select(d, u);
                let g = r.abs().simd_lt(TRIGRANGEMAX_F);

                if !g.all() {
                    let (mut dfidf, dfii) = rempif(d);
                    let mut q2 = dfii & I32x::splat(3);
                    q2 = q2 + q2 + dfidf.0.simd_gt(ZERO).select(I32x::splat(8), I32x::splat(7));
                    q2 >>= I32x::splat(1);
                    let o = (dfii & I32x::splat(1)).simd_eq(I32x::splat(0));
                    let y = dfidf.0.simd_gt(ZERO).select(ZERO, F32x::splat(-1.));
                    let mut x = Doubled::new(
                        F32x::splat(crate::f32::D_PI.0 * -0.5).mul_sign(y),
                        F32x::splat(crate::f32::D_PI.1 * -0.5).mul_sign(y),
                    );
                    x = dfidf + x;
                    dfidf = o.select_doubled(x, dfidf);
                    u = F32x::from(dfidf);

                    u = F32x::from_bits((r.is_infinite() | r.is_nan()).to_int().cast() | u.to_bits());

                    q = g.select(q, q2);
                    d = g.select(d, u);
                }
            }

            let s = d * d;

            d = F32x::from_bits(
                ((q & I32x::splat(2)).simd_eq(I32x::splat(0)).to_int().cast() & NEG_ZERO.to_bits())
                    ^ d.to_bits(),
            );

            let u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s, F32x::splat(0.008_333_078_585_565_090_179_443_36))
                .mul_add(s, F32x::splat(-0.166_666_597_127_914_428_710_938));

            s * (u * d) + d
        }

        #[test]
        fn test_cosf() {
            test_f_f(
                cosf,
                rug::Float::cos,
                f32::MIN..=f32::MAX,
                3.5,
            );
            test_f_f(
                cosf_deterministic,
                rug::Float::cos,
                f32::MIN..=f32::MAX,
                3.5,
            );
        }

        /// Evaluate sine and cosine function simultaneously
        ///
        /// Evaluates the sine and cosine functions of a value in ***a*** at a time,
        /// and store the two values in *first* and *second* position in the returned value, respectively.
        /// The error bound of the returned values is `3.5 ULP`.
        /// If ***a*** is a `NaN` or `infinity`, a `NaN` is returned.
        pub fn sincosf(d: F32x) -> (F32x, F32x) {
            let q: I32x;
            let mut s = d;

            if d.abs().simd_lt(TRIGRANGEMAX2_F).all() {
                q = (d * FRAC_2_PI).roundi();
                let u: F32x = q.cast();
                s = u.mul_add(-PI_A2_F * HALF, s);
                s = u.mul_add(-PI_B2_F * HALF, s);
                s = u.mul_add(-PI_C2_F * HALF, s);
            } else if d.abs().simd_lt(TRIGRANGEMAX_F).all() {
                q = (d * FRAC_2_PI).roundi();
                let u: F32x = q.cast();
                s = u.mul_add(-PI_A_F * HALF, s);
                s = u.mul_add(-PI_B_F * HALF, s);
                s = u.mul_add(-PI_C_F * HALF, s);
                s = u.mul_add(-PI_D_F * HALF, s);
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                s = F32x::from(dfidf);
                s = F32x::from_bits((d.is_infinite() | d.is_nan()).to_int().cast() | s.to_bits());
            }

            let t = s;

            s = s * s;

            let u = F32x::splat(-0.000_195_169_282_960_705_459_117_889)
                .mul_add(s, F32x::splat(0.008_332_157_507_538_795_471_191_41))
                .mul_add(s, F32x::splat(-0.166_666_537_523_269_653_320_312));

            let rx = (u * s).mul_add(t, t);
            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F32x::splat(-2.718_118_423_672_422_068_193_55_e-7)
                .mul_add(s, F32x::splat(2.479_904_469_510_074_704_885_48_e-5))
                .mul_add(s, F32x::splat(-0.001_388_887_874_782_085_418_701_17))
                .mul_add(s, F32x::splat(0.041_666_664_183_139_801_025_390_6))
                .mul_add(s, F32x::splat(-0.5));

            let ry = s.mul_add(u, ONE);

            let o = (q & I32x::splat(1)).simd_eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(2)).simd_eq(I32x::splat(2));
            rsin =
                F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ rsin.to_bits());

            let o = ((q + I32x::splat(1)) & I32x::splat(2)).simd_eq(I32x::splat(2));
            rcos =
                F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ rcos.to_bits());

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
        pub fn sincosf_deterministic(d: F32x) -> (F32x, F32x) {
            let mut q = (d * FRAC_2_PI).roundi();
            let u: F32x = q.cast();
            let mut s = u.mul_add(-PI_A2_F * HALF, d);
            s = u.mul_add(-PI_B2_F * HALF, s);
            s = u.mul_add(-PI_C2_F * HALF, s);
            let g = d.abs().simd_lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let q2 = (d * FRAC_2_PI).roundi();
                let u: F32x = q2.cast();
                let mut t = u.mul_add(-PI_A_F * HALF, d);
                t = u.mul_add(-PI_B_F * HALF, t);
                t = u.mul_add(-PI_C_F * HALF, t);
                t = u.mul_add(-PI_D_F * HALF, t);

                q = g.select(q, q2);
                s = g.select(s, t);
                let g = d.abs().simd_lt(TRIGRANGEMAX_F);

                if !g.all() {
                    let (dfidf, dfii) = rempif(d);
                    let mut t = F32x::from(dfidf);
                    t = F32x::from_bits((d.is_infinite() | d.is_nan()).to_int().cast() | t.to_bits());

                    q = g.select(q, dfii);
                    s = g.select(s, t);
                }
            }

            let t = s;

            s = s * s;

            let u = F32x::splat(-0.000_195_169_282_960_705_459_117_889)
                .mul_add(s, F32x::splat(0.008_332_157_507_538_795_471_191_41))
                .mul_add(s, F32x::splat(-0.166_666_537_523_269_653_320_312));

            let mut rx = (u * s).mul_add(t, t);
            rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F32x::splat(-2.718_118_423_672_422_068_193_55_e-7)
                .mul_add(s, F32x::splat(2.479_904_469_510_074_704_885_48_e-5))
                .mul_add(s, F32x::splat(-0.001_388_887_874_782_085_418_701_17))
                .mul_add(s, F32x::splat(0.041_666_664_183_139_801_025_390_6))
                .mul_add(s, F32x::splat(-0.5));

            let ry = s.mul_add(u, ONE);

            let o = (q & I32x::splat(1)).simd_eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(2)).simd_eq(I32x::splat(2));
            rsin =
                F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ rsin.to_bits());

            let o = ((q + I32x::splat(1)) & I32x::splat(2)).simd_eq(I32x::splat(2));
            rcos =
                F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ rcos.to_bits());

            (rsin, rcos)
        }

        #[test]
        fn test_sincosf() {
            test_f_ff(
                sincosf,
                |in1| {
                    let prec = in1.prec();
                    in1.sin_cos(rug::Float::new(prec))
                },
                f32::MIN..=f32::MAX,
                3.5,
            );
            test_f_ff(
                sincosf_deterministic,
                |in1| {
                    let prec = in1.prec();
                    in1.sin_cos(rug::Float::new(prec))
                },
                f32::MIN..=f32::MAX,
                3.5,
            );
        }

        /// Tangent function
        ///
        /// These functions evaluates the tangent function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn tanf(d: F32x) -> F32x {
            let q: I32x;

            let mut x = d;

            if d.abs().simd_lt(TRIGRANGEMAX2_F * HALF).all() {
                q = (d * FRAC_2_PI).roundi();
                let u: F32x = q.cast();
                x = u.mul_add(-PI_A2_F * HALF, x);
                x = u.mul_add(-PI_B2_F * HALF, x);
                x = u.mul_add(-PI_C2_F * HALF, x);
            } else if d.abs().simd_lt(TRIGRANGEMAX_F).all() {
                q = (d * (F32x::splat(2.) * FRAC_1_PI)).roundi();
                let u: F32x = q.cast();
                x = u.mul_add(-PI_A_F * HALF, x);
                x = u.mul_add(-PI_B_F * HALF, x);
                x = u.mul_add(-PI_C_F * HALF, x);
                x = u.mul_add(-PI_D_F * HALF, x);
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                x = F32x::from(dfidf);
                x = F32x::from_bits((d.is_infinite() | d.is_nan()).to_int().cast() | x.to_bits());
                x = d.is_neg_zero().select(d, x);
            }

            let s = x * x;

            let o = (q & I32x::splat(1)).simd_eq(I32x::splat(1));
            x = F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ x.to_bits());

            let mut u = if cfg!(feature = "enable_neon32") {
                F32x::splat(0.009_272_458_031_773_567_199_707_03)
                    .mul_add(s, F32x::splat(0.003_319_849_958_643_317_222_595_21))
                    .mul_add(s, F32x::splat(0.024_299_807_846_546_173_095_703_1))
                    .mul_add(s, F32x::splat(0.053_449_530_154_466_629_028_320_3))
                    .mul_add(s, F32x::splat(0.133_383_005_857_467_651_367_188))
                    .mul_add(s, F32x::splat(0.333_331_853_151_321_411_132_812))
            } else {
                let s2 = s * s;
                let s4 = s2 * s2;

                F32x::poly6(s, s2, s4,
                    0.009_272_458_031_773_567_199_707_03,
                    0.003_319_849_958_643_317_222_595_21,
                    0.024_299_807_846_546_173_095_703_1,
                    0.053_449_530_154_466_629_028_320_3,
                    0.133_383_005_857_467_651_367_188,
                    0.333_331_853_151_321_411_132_812)
            };

            u = s.mul_add(u * x, x);

            o.select(u.recip(), u)
        }

        /// Tangent function
        ///
        /// These functions evaluates the tangent function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn tanf_deterministic(d: F32x) -> F32x {
            let mut q = (d * FRAC_2_PI).roundi();
            let u: F32x = q.cast();
            let mut x = u.mul_add(-PI_A2_F * HALF, d);
            x = u.mul_add(-PI_B2_F * HALF, x);
            x = u.mul_add(-PI_C2_F * HALF, x);
            let g = d.abs().simd_lt(TRIGRANGEMAX2_F * HALF);

            if !g.all() {
                let q2 = (d * FRAC_2_PI).roundi();
                let s: F32x = q.cast();
                let mut u = s.mul_add(-PI_A_F * HALF, d);
                u = s.mul_add(-PI_B_F * HALF, u);
                u = s.mul_add(-PI_C_F * HALF, u);
                u = s.mul_add(-PI_D_F * HALF, u);

                q = g.select(q, q2);
                x = g.select(x, u);
                let g = d.abs().simd_lt(TRIGRANGEMAX_F);

                if !g.all() {
                    let (dfidf, dfii) = rempif(d);
                    u = F32x::from(dfidf);
                    u = F32x::from_bits((d.is_infinite() | d.is_nan()).to_int().cast() | u.to_bits());
                    u = d.is_neg_zero().select(d, u);
                    q = g.select(q, dfii);
                    x = g.select(x, u);
                }
            }

            let s = x * x;

            let o = (q & I32x::splat(1)).simd_eq(I32x::splat(1));
            x = F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ x.to_bits());

            let mut u = if cfg!(feature = "enable_neon32") {
                F32x::splat(0.009_272_458_031_773_567_199_707_03)
                    .mul_add(s, F32x::splat(0.003_319_849_958_643_317_222_595_21))
                    .mul_add(s, F32x::splat(0.024_299_807_846_546_173_095_703_1))
                    .mul_add(s, F32x::splat(0.053_449_530_154_466_629_028_320_3))
                    .mul_add(s, F32x::splat(0.133_383_005_857_467_651_367_188))
                    .mul_add(s, F32x::splat(0.333_331_853_151_321_411_132_812))
            } else {
                let s2 = s * s;
                let s4 = s2 * s2;

                F32x::poly6(s, s2, s4,
                    0.009_272_458_031_773_567_199_707_03,
                    0.003_319_849_958_643_317_222_595_21,
                    0.024_299_807_846_546_173_095_703_1,
                    0.053_449_530_154_466_629_028_320_3,
                    0.133_383_005_857_467_651_367_188,
                    0.333_331_853_151_321_411_132_812)
            };

            u = s.mul_add(u * x, x);

            o.select(u.recip(), u)
        }

        #[test]
        fn test_tanf() {
            test_f_f(
                tanf,
                rug::Float::tan,
                f32::MIN..=f32::MAX,
                3.5,
            );
            test_f_f(
                tanf_deterministic,
                rug::Float::tan,
                f32::MIN..=f32::MAX,
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
        pub fn sincospif(d: F32x) -> (F32x, F32x) {
            let u = d * F32x::splat(4.);
            let q = u.trunci();
            let q = (q + ((q.cast() >> U32x::splat(31)).cast() ^ I32x::splat(1))) & I32x::splat(!1);
            let s = u - q.cast();

            let t = s;
            let s = s * s;

            //

            let u = F32x::splat(-0.360_092_526_5_e-4)
                .mul_add(s, F32x::splat(0.249_008_811_1_e-2))
                .mul_add(s, F32x::splat(-0.807_455_107_6_e-1))
                .mul_add(s, F32x::splat(0.785_398_185_3));

            let rx = u * t;

            //

            let u = F32x::splat(0.353_981_522_5_e-5)
                .mul_add(s, F32x::splat(-0.325_957_400_5_e-3))
                .mul_add(s, F32x::splat(0.158_543_158_3_e-1))
                .mul_add(s, F32x::splat(-0.308_425_128_5))
                .mul_add(s, ONE);

            let ry = u;

            //

            let o = (q & I32x::splat(2)).simd_eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(4)).simd_eq(I32x::splat(4));
            rsin =
                F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ rsin.to_bits());

            let o = ((q + I32x::splat(2)) & I32x::splat(4)).simd_eq(I32x::splat(4));
            rcos =
                F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ rcos.to_bits());

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
            test_f_ff(
                sincospif,
                |mut in1| {
                    let prec = in1.prec();
                    in1.set_prec(prec * 2);
                    (in1 * Float::with_val(prec * 2, Constant::Pi)).sin_cos(Float::new(prec))
                },
                -rangemax2..=rangemax2,
                2.,
            );
        }

        /// Arc tangent function of two variables
        ///
        /// These functions evaluates the arc tangent function of (***y*** / ***x***).
        /// The quadrant of the result is determined according to the signs of ***x*** and ***y***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn atan2f(y: F32x, x: F32x) -> F32x {
            let mut r = atan2kf(y.abs(), x);

            r = r.mul_sign(x);
            r = (x.is_infinite() | x.simd_eq(ZERO)).select(
                FRAC_PI_2 - visinf2_vf_vf_vf(x, FRAC_PI_2.mul_sign(x)),
                r,
            );
            r = y.is_infinite().select(
                FRAC_PI_2 - visinf2_vf_vf_vf(x, FRAC_PI_4.mul_sign(x)),
                r,
            );

            r = y.simd_eq(ZERO).select(
                F32x::from_bits(x.is_sign_negative().to_int().cast() & PI.to_bits()),
                r,
            );

            F32x::from_bits((x.is_nan() | y.is_nan()).to_int().cast() | r.mul_sign(y).to_bits())
        }

        #[test]
        fn test_atan2f() {
            test_ff_f(
                atan2f,
                rug::Float::atan2,
                f32::MIN..=f32::MAX,
                f32::MIN..=f32::MAX,
                3.5
            );
        }

        /// Arc sine function
        ///
        /// These functions evaluates the arc sine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn asinf(d: F32x) -> F32x {
            let o = d.abs().simd_lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let x = o.select(d.abs(), x2.sqrt());

            let u = F32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, F32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, F32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, F32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, F32x::splat(0.166_667_729_6))
                .mul_add(x * x2, x);

            let r = o.select(u, u.mul_add(F32x::splat(-2.), FRAC_PI_2));
            r.mul_sign(d)
        }

        #[test]
        fn test_asinf() {
            test_f_f(
                asinf,
                rug::Float::asin,
                -1.0..=1.0,
                3.5,
            );
        }

        /// Arc cosine function
        ///
        /// These functions evaluates the arc cosine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn acosf(d: F32x) -> F32x {
            let o = d.abs().simd_lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select(d.abs(), x2.sqrt());
            x = d.abs().simd_eq(ONE).select(ZERO, x);

            let u = F32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, F32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, F32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, F32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, F32x::splat(0.166_667_729_6))
                * (x2 * x);

            let y = F32x::splat(core::f32::consts::FRAC_PI_2) - (x.mul_sign(d) + u.mul_sign(d));
            x += u;
            let r = o.select(y, x * F32x::splat(2.));
            (!o & d.simd_lt(ZERO)).select(
                Doubled::<F32x>::splat(crate::f32::D_PI).add_checked(-r).0,
                r,
            )
        }

        #[test]
        fn test_acosf() {
            test_f_f(
                acosf,
                rug::Float::acos,
                -1.0..=1.0,
                3.5,
            );
        }

        /// Arc tangent function
        ///
        /// These functions evaluates the arc tangent function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn atanf(d: F32x) -> F32x {
            let q = vsel_vi2_vf_vi2(d, I32x::splat(2));
            let s = d.abs();

            let q = vsel_vi2_vf_vf_vi2_vi2(ONE, s, q + I32x::splat(1), q);
            let s = ONE.simd_lt(s).select(s.recip(), s);

            let mut t = s * s;

            let t2 = t * t;
            let t4 = t2 * t2;


            let u = F32x::poly8(t, t2, t4,
                0.002_823_638_962_581_753_730_773_93,
                -0.015_956_902_876_496_315_002_441_4,
                0.042_504_988_610_744_476_318_359_4,
                -0.074_890_092_015_266_418_457_031_2,
                0.106_347_933_411_598_205_566_406,
                -0.142_027_363_181_114_196_777_344,
                0.199_926_957_488_059_997_558_594,
                -0.333_331_018_686_294_555_664_062);

            t = s.mul_add(t * u, s);

            t = (q & I32x::splat(1))
                .simd_eq(I32x::splat(1))
                .select(FRAC_PI_2 - t, t);

            t = F32x::from_bits(
                ((q & I32x::splat(2)).simd_eq(I32x::splat(2)).to_int().cast() & NEG_ZERO.to_bits())
                    ^ t.to_bits(),
            );

            if cfg!(feature = "enable_neon32") || cfg!(feature = "enable_neon32vfpv4") {
                t = d.is_infinite().select(
                    F32x::splat(1.587_401_051_968_199_474_751_705_6).mul_sign(d),
                    t,
                );
            }

            t
        }

        #[test]
        fn test_atanf() {
            test_f_f(
                atanf,
                rug::Float::atan,
                f32::MIN..=f32::MAX,
                3.5,
            );
        }

        /// Hyperbolic sine function
        ///
        /// These functions evaluates the hyperbolic sine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP` if ***a*** is in `[-88, 88]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct sign
        /// or a correct value with `3.5 ULP` error bound is returned.
        pub fn sinhf(x: F32x) -> F32x {
            let e = expm1fk(x.abs());
            let mut y = (e + F32x::splat(2.)) / (e + ONE);
            y *= HALF * e;

            y = (x.abs().simd_gt(F32x::splat(88.)) | y.is_nan()).select(INFINITY, y);
            y = y.mul_sign(x);
            F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_sinhf() {
            test_f_f(
                sinhf,
                rug::Float::sinh,
                -88.0..=88.0,
                3.5,
            );
        }

        /// Hyperbolic cosine function
        ///
        /// These functions evaluates the hyperbolic cosine function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP` if a is in `[-88, 88]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct sign
        /// or a correct value with `3.5 ULP` error bound is returned.
        pub fn coshf(x: F32x) -> F32x {
            let e = u10::expf(x.abs());
            let mut y = HALF.mul_add(e, HALF / e);

            y = (x.abs().simd_gt(F32x::splat(88.)) | y.is_nan()).select(INFINITY, y);
            F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_coshf() {
            test_f_f(
                coshf,
                rug::Float::cosh,
                -88.0..=88.0,
                3.5,
            );
        }

        /// Hyperbolic tangent function
        ///
        /// These functions evaluates the hyperbolic tangent function of a value in ***a***.
        /// The error bound of the returned value is `3.5 ULP` for the double-precision
        /// function or `3.5 ULP` for the single-precision function.
        pub fn tanhf(x: F32x) -> F32x {
            let d = expm1fk(F32x::splat(2.) * x.abs());
            let mut y = d / (F32x::splat(2.) + d);

            y = (x.abs().simd_gt(F32x::splat(8.664_339_742)) | y.is_nan()).select(ONE, y);
            y = y.mul_sign(x);
            F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_tanhf() {
            test_f_f(
                tanhf,
                rug::Float::tanh,
                -8.7..=8.7,
                3.5,
            );
        }

        /// Natural logarithmic function
        ///
        /// These functions return the natural logarithm of ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn logf(mut d: F32x) -> F32x {
            let m: F32x;

            let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.simd_lt(F32x::splat(f32::MIN_POSITIVE));
                d = o.select(d * (F1_32X * F1_32X), d);
                let mut e = ilogb2kf(d * F32x::splat(1. / 0.75));
                m = ldexp3kf(d, -e);
                e = o.select(e - I32x::splat(64), e);
                e.cast()
            }/* else {
                let mut e = vgetexp_vf_vf(d * F32x::splat(1. / 0.75));
                e = e.simd_eq(INFINITY).select(F32x::splat(128.), e);
                m = vgetmant_vf_vf(d);
                e
            }*/;

            let mut x = (m - ONE) / (ONE + m);
            let x2 = x * x;

            let t = F32x::splat(0.239_282_846_450_805_664_062_5)
                .mul_add(x2, F32x::splat(0.285_182_118_415_832_519_531_25))
                .mul_add(x2, F32x::splat(0.400_005_877_017_974_853_515_625))
                .mul_add(x2, F32x::splat(0.666_666_686_534_881_591_796_875))
                .mul_add(x2, F32x::splat(2.));

            x = x.mul_add(t, F32x::splat(0.693_147_180_559_945_286_226_764) * ef);
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            x = d.simd_eq(INFINITY).select(INFINITY, x);
            x = (d.simd_lt(ZERO) | d.is_nan()).select(NAN, x);
            d.simd_eq(ZERO).select(NEG_INFINITY, x)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(x, d, I32x::splat(5 << (5 * 4)), 0)
            }*/
        }

        #[test]
        fn test_logf() {
            test_f_f(
                logf,
                rug::Float::ln,
                0.0..=f32::MAX,
                3.5,
            );
        }

        /// Base-10 logarithmic function
        ///
        /// This function returns the base-10 logarithm of ***a***.
        pub fn log2f(mut d: F32x) -> F32x {
            let (m, e) = //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")
            {
                let o = d.simd_lt(F32x::splat(f32::MIN_POSITIVE));
                d = o.select(d * (F1_32X * F1_32X), d);
                let e = ilogb2kf(d * F32x::splat(1./0.75));
                (ldexp3kf(d, -e), o.select(e - I32x::splat(64), e))
            /*} else {
                let e = vgetexp_vf_vf(d * F32x::splat(1./0.75));
                (vgetmant_vf_vf(d), e.simd_eq(INFINITY).select(F32x::splat(128.), e))
            */};

            let x = (m - ONE) / (m + ONE);
            let x2 = x * x;

            let t = F32x::splat(0.437_408_834_7)
                .mul_add(x2, F32x::splat(0.576_484_382_2))
                .mul_add(x2, F32x::splat(0.961_802_423));

            //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")
            {
                let mut r = (x2 * x).mul_add(t, x.mul_add(F32x::splat(0.288_539_004_3_e+1), e.cast()));

                r = d.simd_eq(INFINITY).select(INFINITY, r);
                r = (d.simd_lt(ZERO) | d.is_nan()).select(NAN, r);
                d.simd_eq(ZERO).select(NEG_INFINITY, r)
            /*} else {
                let r = (x2 * x).mul_add(t, x.mul_add(F32x::splat(0.288_539_004_3_e+1), e));

                vfixup_vf_vf_vf_vi2_i(r, d, I32::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0)
            */
            }
        }

        #[test]
        fn test_log2f() {
            test_f_f(
                log2f,
                rug::Float::log2,
                0.0..=f32::MAX,
                3.5,
            );
        }

        /// Base-10 exponential function
        ///
        /// This function returns 10 raised to ***a***.
        pub fn exp10f(d: F32x) -> F32x {
            let mut u = (d * LOG10_2_F).round();
            let q = u.roundi();

            let mut s = u.mul_add(-L10U_F, d);
            s = u.mul_add(-L10L_F, s);

            u = F32x::splat(0.206_400_498_7)
                .mul_add(s, F32x::splat(0.541_787_743_6))
                .mul_add(s, F32x::splat(0.117_128_682_1_e+1))
                .mul_add(s, F32x::splat(0.203_465_604_8_e+1))
                .mul_add(s, F32x::splat(0.265_094_876_3_e+1))
                .mul_add(s, F32x::splat(0.230_258_512_5_e+1))
                .mul_add(s, F32x::splat(0.1_e+1));

            u = ldexp2kf(u, q);

            u = d
                .simd_gt(F32x::splat(38.531_839_419_103_623_894_138_7))
                .select(INFINITY, u);
            F32x::from_bits(!d.simd_lt(F32x::splat(-50.)).to_int().cast::<u32>() & u.to_bits())
        }

        #[test]
        fn test_exp10f() {
            test_f_f(
                exp10f,
                rug::Float::exp10,
                -38.54..=38.54,
                3.5,
            );
        }

        /// Base-2 exponential function
        ///
        /// This function returns `2` raised to ***a***.
        pub fn exp2f(d: F32x) -> F32x {
            let mut u = d.round();
            let q = u.roundi();

            let s = d - u;

            u = F32x::splat(0.153_592_089_2_e-3)
                .mul_add(s, F32x::splat(0.133_926_270_1_e-2))
                .mul_add(s, F32x::splat(0.961_838_476_4_e-2))
                .mul_add(s, F32x::splat(0.555_034_726_9_e-1))
                .mul_add(s, F32x::splat(0.240_226_447_6))
                .mul_add(s, F32x::splat(0.693_147_182_5))
                .mul_add(s, F32x::splat(0.1_e+1));

            u = ldexp2kf(u, q);

            u = d.simd_ge(F32x::splat(128.)).select(INFINITY, u);
            F32x::from_bits(!d.simd_lt(F32x::splat(-150.)).to_int().cast::<u32>() & u.to_bits())
        }

        #[test]
        fn test_exp2f() {
            test_f_f(
                exp2f,
                rug::Float::exp2,
                -150.0..=128.0,
                3.5,
            );
        }

        /*#[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
        pub fn sqrtf(d: F32x) -> F32x {
            let e = F32x::from_bits(
                U32x::splat(0x_2000_0000)
                    + (U32x::splat(0x_7f00_0000) & (d.to_bits() >> U32x::splat(1))),
            );
            let m = F32x::from_bits(
                I32x::splat(0x_3f00_0000) + (I32x::splat(0x_01ff_ffff) & I32x::from_bits(d)),
            );
            let mut x = vrsqrteq_f32(m);
            x = vmulq_f32(x, vrsqrtsq_f32(m, vmulq_f32(x, x)));
            let mut u = vmulq_f32(x, m);
            u = vmlaq_f32(u, vmlsq_f32(m, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
            e = F32x::from_bits(
                !d.simd_eq(ZERO).to_int().cast() &
                e.to_bits()
            );
            u = e * u;

            u = d.is_infinite().select(INFINITY, u);
            u = F32x::from_bits(
                (d.is_nan() | d.simd_lt(ZERO)).to_int().cast() |
                u.to_bits()
            );
            u.mul_sign(d)
        }*/
        /*#[cfg(feature = "enable_vecext")]
        pub fn xsqrtf_u35(d: F32x) -> F32x {
            let mut q = d.sqrt();
            q = d.is_neg_zero().select(NEG_ZERO, q);
            d.simd_eq(INFINITY).select(INFINITY, q)
        }*/
        /// Square root function
        ///
        /// The error bound of the returned value is `3.5 ULP`.
        #[cfg(all(
                    not(feature = "enable_neon32"),
                    not(feature = "enable_neon32vfpv4"),
                //    not(feature = "enable_vecext")
                ))]
        pub fn sqrtf(d: F32x) -> F32x {
            d.sqrt()
        }

        #[test]
        fn test_sqrtf() {
            test_f_f(
                sqrtf,
                rug::Float::sqrt,
                0.0..=f32::MAX,
                3.5,
            );
        }

        /// Cube root function
        ///
        /// These functions return the real cube root of ***a***.
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn cbrtf(mut d: F32x) -> F32x {
            let mut q = ONE;

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                let s = d;
            }*/
            let e = ilogbkf(d.abs()) + I32x::splat(1);
            d = ldexp2kf(d, -e);

            let t = e.cast::<f32>() + F32x::splat(6144.);
            let qu = (t * F32x::splat(1. / 3.)).trunci();
            let re = (t - qu.cast::<f32>() * F32x::splat(3.)).trunci();

            q = re
                .simd_eq(I32x::splat(1))
                .select(F32x::splat(1.259_921_049_894_873_164_767_210_6), q);
            q = re
                .simd_eq(I32x::splat(2))
                .select(F32x::splat(1.587_401_051_968_199_474_751_705_6), q);
            q = ldexp2kf(q, qu - I32x::splat(2048));

            q = q.mul_sign(d);
            d = d.abs();

            let x = F32x::splat(-0.601_564_466_953_277_587_890_625)
                .mul_add(d, F32x::splat(2.820_889_234_542_846_679_687_5))
                .mul_add(d, F32x::splat(-5.532_182_216_644_287_109_375))
                .mul_add(d, F32x::splat(5.898_262_500_762_939_453_125))
                .mul_add(d, F32x::splat(-3.809_541_702_270_507_812_5))
                .mul_add(d, F32x::splat(2.224_125_623_703_002_929_687_5));

            let mut y = d * x * x;
            y = (y - F32x::splat(2. / 3.) * y * y.mul_add(x, F32x::splat(-1.))) * q;

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                y = s.is_infinite().select(INFINITY.mul_sign(s), y);
                y = s
                    .simd_eq(ZERO)
                    .select(ZERO.mul_sign(s), y);
            }*/

            y
        }

        #[test]
        fn test_cbrtf() {
            test_f_f(
                cbrtf,
                rug::Float::cbrt,
                f32::MIN..=f32::MAX,
                3.5,
            );
        }

        /// 2D Euclidian distance function
        ///
        /// The error bound of the returned value is `3.5 ULP`.
        pub fn hypotf(x: F32x, y: F32x) -> F32x {
            let x = x.abs();
            let y = y.abs();
            let min = x.simd_min(y);
            let max = x.simd_max(y);

            let t = min / max;
            let mut ret = max * t.mul_add(t, ONE).sqrt();
            ret = min.simd_eq(ZERO).select(max, ret);
            ret = (x.is_nan() | y.is_nan()).select(NAN, ret);
            (x.simd_eq(INFINITY) | y.simd_eq(INFINITY)).select(INFINITY, ret)
        }

        #[test]
        fn test_hypotf() {
            test_ff_f(
                hypotf,
                rug::Float::hypot,
                f32::MIN..=f32::MAX,
                f32::MIN..=f32::MAX,
                3.5,
            );
        }
    };
}
