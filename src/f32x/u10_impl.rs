macro_rules! impl_math_f32_u10 {
    () => {
        use super::*;

        /// Sine function
        ///
        /// This function evaluates the sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn sinf(d: F32x) -> F32x {
            let mut q: I32x;
            let mut s: Doubled<F32x>;

            if d.abs().simd_lt(TRIGRANGEMAX2_F).all() {
                let u = (d * FRAC_1_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F, d);
                s = v.add_as_doubled(u * (-PI_B2_F));
                s = s.add_checked(u * (-PI_C2_F));
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
                s = dfidf.normalize();

                s.0 = F32x::from_bits(
                    (d.is_infinite() | d.is_nan()).to_int().cast() | s.0.to_bits(),
                );
            }

            let t = s;
            let s = s.square();

            let mut u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s.0, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s.0, F32x::splat(0.008_333_078_585_565_090_179_443_36));

            let x = ONE.add_checked(
                F32x::splat(-0.166_666_597_127_914_428_710_938).add_checked_as_doubled(u * s.0) * s,
            );

            u = t.mul_as_f(x);

            u = F32x::from_bits(
                ((q & I32x::splat(1)).simd_eq(I32x::splat(1)).to_int().cast()
                    & NEG_ZERO.to_bits()
                ) ^ u.to_bits(),
            );

            d.is_neg_zero().select(d, u)
        }

        /// Sine function
        ///
        /// This function evaluates the sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn sinf_deterministic(d: F32x) -> F32x {
            let u = (d * FRAC_1_PI).round();
            let mut q = u.roundi();
            let v = u.mul_add((-PI_A2_F), d);
            let mut s = v.add_as_doubled(u * (-PI_B2_F));
            s = s.add_checked(u * (-PI_C2_F));
            let g = d.abs().simd_lt(TRIGRANGEMAX2_F);

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
                let mut t = dfidf.normalize();

                t.0 = F32x::from_bits(
                    (d.is_infinite() | d.is_nan()).to_int().cast() | t.0.to_bits(),
                );

                q = g.select(q, q2);
                s = g.select_doubled(s, t);
            }

            let t = s;
            s = s.square();

            let mut u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s.0, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s.0, F32x::splat(0.008_333_078_585_565_090_179_443_36));

            let x = ONE.add_checked(
                F32x::splat(-0.166_666_597_127_914_428_710_938).add_checked_as_doubled(u * s.0) * s,
            );

            u = t.mul_as_f(x);

            u = F32x::from_bits(
                ((q & I32x::splat(1)).simd_eq(I32x::splat(1)).to_int().cast()
                    & NEG_ZERO.to_bits()
                ) ^ u.to_bits(),
            );

            d.is_neg_zero().select(d, u)
        }

        #[test]
        fn test_sinf() {
            test_f_f(
                sinf,
                rug::Float::sin,
                f32::MIN..=f32::MAX,
                1.
            );
            test_f_f(
                sinf_deterministic,
                rug::Float::sin,
                f32::MIN..=f32::MAX,
                1.
            );
        }

        /// Cosine function
        ///
        /// This function evaluates the cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn cosf(d: F32x) -> F32x {
            let mut q: I32x;
            let mut s: Doubled<F32x>;

            if d.abs().simd_lt(TRIGRANGEMAX2_F).all() {
                let dq = (d.mul_add(FRAC_1_PI, F32x::splat(-0.5)))
                    .round()
                    .mul_add(F32x::splat(2.), ONE);
                q = dq.roundi();
                s = d.add_as_doubled(dq * (-PI_A2_F) * HALF);
                s += dq * (-PI_B2_F) * HALF;
                s += dq * (-PI_C2_F) * HALF;
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
                s = dfidf.normalize();

                s.0 = F32x::from_bits(
                    (d.is_infinite() | d.is_nan()).to_int().cast() | s.0.to_bits(),
                );
            }

            let t = s;
            s = s.square();

            let u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s.0, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s.0, F32x::splat(0.008_333_078_585_565_090_179_443_36));

            let x = ONE.add_checked(
                F32x::splat(-0.166_666_597_127_914_428_710_938).add_checked_as_doubled(u * s.0) * s,
            );

            let u = t.mul_as_f(x);

            F32x::from_bits(
                ((q & I32x::splat(2)).simd_eq(I32x::splat(0)).to_int().cast()
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
        pub fn cosf_deterministic(d: F32x) -> F32x {
            let dq = (d.mul_add(FRAC_1_PI, F32x::splat(-0.5)))
                .round()
                .mul_add(F32x::splat(2.), ONE);
            let mut q = dq.roundi();
            let mut s = d.add_as_doubled(dq * (-PI_A2_F * HALF));
            s += dq * (-PI_B2_F * HALF);
            s += dq * (-PI_C2_F * HALF);
            let g = d.abs().simd_lt(TRIGRANGEMAX2_F);

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
                let mut t = dfidf.normalize();

                t.0 = F32x::from_bits(
                    (d.is_infinite() | d.is_nan()).to_int().cast() | t.0.to_bits(),
                );

                q = g.select(q, q2);
                s = g.select_doubled(s, t);
            }

            let t = s;
            s = s.square();

            let u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s.0, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s.0, F32x::splat(0.008_333_078_585_565_090_179_443_36));

            let x = ONE.add_checked(
                F32x::splat(-0.166_666_597_127_914_428_710_938).add_checked_as_doubled(u * s.0) * s,
            );

            let u = t.mul_as_f(x);

            F32x::from_bits(
                ((q & I32x::splat(2)).simd_eq(I32x::splat(0)).to_int().cast()
                    & NEG_ZERO.to_bits()
                ) ^ u.to_bits(),
            )
        }

        #[test]
        fn test_cosf() {
            test_f_f(
                cosf,
                rug::Float::cos,
                f32::MIN..=f32::MAX,
                1.
            );
            test_f_f(
                cosf_deterministic,
                rug::Float::cos,
                f32::MIN..=f32::MAX,
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
        pub fn sincosf(d: F32x) -> (F32x, F32x) {
            let q: I32x;
            let mut s: Doubled<F32x>;

            if d.abs().simd_lt(TRIGRANGEMAX2_F).all() {
                let u = (d * FRAC_2_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F * HALF, d);
                s = v.add_as_doubled(u * (-PI_B2_F) * HALF);
                s = s.add_checked(u * (-PI_C2_F) * HALF);
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                s = dfidf;
                let o = d.is_infinite() | d.is_nan();
                s.0 = F32x::from_bits(o.to_int().cast() | s.0.to_bits());
            }

            let t = s;

            s.0 = s.square_as_f();

            let u = F32x::splat(-0.000_195_169_282_960_705_459_117_889)
                .mul_add(s.0, F32x::splat(0.008_332_157_507_538_795_471_191_41))
                .mul_add(s.0, F32x::splat(-0.166_666_537_523_269_653_320_312))
                * (s.0 * t.0);

            let x = t.add_checked(u);
            let rx = F32x::from(x);

            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F32x::splat(-2.718_118_423_672_422_068_193_55_e-7)
                .mul_add(s.0, F32x::splat(2.479_904_469_510_074_704_885_48_e-5))
                .mul_add(s.0, F32x::splat(-0.001_388_887_874_782_085_418_701_17))
                .mul_add(s.0, F32x::splat(0.041_666_664_183_139_801_025_390_6))
                .mul_add(s.0, F32x::splat(-0.5));

            let x = ONE.add_checked(s.0.mul_as_doubled(u));
            let ry = F32x::from(x);

            let o = (q & I32x::splat(1)).simd_eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(2)).simd_eq(I32x::splat(2));
            rsin = F32x::from_bits(
                (o.to_int().cast() & NEG_ZERO.to_bits()) ^ rsin.to_bits(),
            );

            let o = ((q + I32x::splat(1)) & I32x::splat(2)).simd_eq(I32x::splat(2));
            rcos = F32x::from_bits(
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
        pub fn sincosf_deterministic(d: F32x) -> (F32x, F32x) {
            let u = (d * FRAC_2_PI).round();
            let mut q = u.roundi();
            let v = u.mul_add(-PI_A2_F * HALF, d);
            let mut s = v.add_as_doubled(u * (-PI_B2_F * HALF));
            s = s.add_checked(u * (-PI_C2_F * HALF));
            let g = d.abs().simd_lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let (dfidf, dfii) = rempif(d);
                let mut t = dfidf;
                let o = d.is_infinite() | d.is_nan();
                t.0 = F32x::from_bits(o.to_int().cast() | t.0.to_bits());
                q = g.select(q, dfii);
                s = g.select_doubled(s, t);
            }

            let t = s;

            s.0 = s.square_as_f();

            let u = F32x::splat(-0.000_195_169_282_960_705_459_117_889)
                .mul_add(s.0, F32x::splat(0.008_332_157_507_538_795_471_191_41))
                .mul_add(s.0, F32x::splat(-0.166_666_537_523_269_653_320_312))
                * (s.0 * t.0);

            let x = t.add_checked(u);
            let mut rx = F32x::from(x);

            rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F32x::splat(-2.718_118_423_672_422_068_193_55_e-7)
                .mul_add(s.0, F32x::splat(2.479_904_469_510_074_704_885_48_e-5))
                .mul_add(s.0, F32x::splat(-0.001_388_887_874_782_085_418_701_17))
                .mul_add(s.0, F32x::splat(0.041_666_664_183_139_801_025_390_6))
                .mul_add(s.0, F32x::splat(-0.5));

            let x = ONE.add_checked(s.0.mul_as_doubled(u));
            let ry = F32x::from(x);

            let o = (q & I32x::splat(1)).simd_eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(2)).simd_eq(I32x::splat(2));
            rsin = F32x::from_bits(
                (o.to_int().cast() & NEG_ZERO.to_bits()) ^ rsin.to_bits(),
            );

            let o = ((q + I32x::splat(1)) & I32x::splat(2)).simd_eq(I32x::splat(2));
            rcos = F32x::from_bits(
                (o.to_int().cast() & NEG_ZERO.to_bits()) ^ rcos.to_bits(),
            );

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
                1.
            );
            test_f_ff(
                sincosf_deterministic,
                |in1| {
                    let prec = in1.prec();
                    in1.sin_cos(rug::Float::new(prec))
                },
                f32::MIN..=f32::MAX,
                1.
            );
        }

        /// Tangent function
        ///
        /// This function evaluates the tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn tanf(d: F32x) -> F32x {
            let q: I32x;

            let mut s = if d.abs().simd_lt(TRIGRANGEMAX2_F).all() {
                let u = (d * FRAC_2_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F * HALF, d);
                v.add_as_doubled(u * (-PI_B2_F) * HALF)
                    .add_checked(u * (-PI_C2_F) * HALF)
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                let o = d.is_infinite() | d.is_nan();
                Doubled::new(
                    F32x::from_bits(o.to_int().cast() | dfidf.0.to_bits()),
                    F32x::from_bits(o.to_int().cast() | dfidf.1.to_bits()),
                )
            };

            let o = (q & I32x::splat(1)).simd_eq(I32x::splat(1));
            let n = o.to_int().cast() & NEG_ZERO.to_bits();
            s = Doubled::new(
                F32x::from_bits(s.0.to_bits() ^ n),
                F32x::from_bits(s.1.to_bits() ^ n)
            );

            let t = s;
            s = s.square();
            s = s.normalize();

            let u = F32x::splat(0.004_466_364_625_841_379_165_649_41)
                .mul_add(s.0, F32x::splat(-8.392_018_207_814_544_439_315_8_e-5))
                .mul_add(s.0, F32x::splat(0.010_963_924_229_145_050_048_828_1))
                .mul_add(s.0, F32x::splat(0.021_236_030_384_898_185_729_980_5))
                .mul_add(s.0, F32x::splat(0.054_068_714_380_264_282_226_562_5));

            let mut x =
                F32x::splat(0.133_325_666_189_193_725_585_938).add_checked_as_doubled(u * s.0);
            x = ONE
                .add_checked(F32x::splat(0.333_333_611_488_342_285_156_25).add_checked(s * x) * s);
            x = t * x;

            x = o.select_doubled(x.recpre(), x);

            let u = F32x::from(x);

            d.is_neg_zero().select(d, u)
        }

        /// Tangent function
        ///
        /// This function evaluates the tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        ///
        /// NOTE: This version is slower, but SIMD lanes are independent
        pub fn tanf_deterministic(d: F32x) -> F32x {
            let u = (d * FRAC_2_PI).round();
            let mut q = u.roundi();
            let v = u.mul_add(-PI_A2_F * HALF, d);
            let mut s = v.add_as_doubled(u * (-PI_B2_F * HALF));
            s = s.add_checked(u * (-PI_C2_F * HALF));
            let g = d.abs().simd_lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let (dfidf, dfii) = rempif(d);
                let mut t = dfidf;
                let o = d.is_infinite() | d.is_nan();
                t = Doubled::new(
                    F32x::from_bits(o.to_int().cast() | t.0.to_bits()),
                    F32x::from_bits(o.to_int().cast() | t.1.to_bits())
                );
                q = g.select(q, dfii);
                s = g.select_doubled(s, t);
            }

            let o = (q & I32x::splat(1)).simd_eq(I32x::splat(1));
            let n = o.to_int().cast() & NEG_ZERO.to_bits();
            s = Doubled::new(
                F32x::from_bits(s.0.to_bits() ^ n),
                F32x::from_bits(s.1.to_bits() ^ n)
            );

            let t = s;
            s = s.square();
            s = s.normalize();

            let u = F32x::splat(0.004_466_364_625_841_379_165_649_41)
                .mul_add(s.0, F32x::splat(-8.392_018_207_814_544_439_315_8_e-5))
                .mul_add(s.0, F32x::splat(0.010_963_924_229_145_050_048_828_1))
                .mul_add(s.0, F32x::splat(0.021_236_030_384_898_185_729_980_5))
                .mul_add(s.0, F32x::splat(0.054_068_714_380_264_282_226_562_5));

            let mut x =
                F32x::splat(0.133_325_666_189_193_725_585_938).add_checked_as_doubled(u * s.0);
            x = ONE
                .add_checked(F32x::splat(0.333_333_611_488_342_285_156_25).add_checked(s * x) * s);
            x = t * x;

            x = o.select_doubled(x.recpre(), x);

            let u = F32x::from(x);

            d.is_neg_zero().select(d, u)
        }

        #[test]
        fn test_tanf() {
            test_f_f(
                tanf,
                rug::Float::tan,
                f32::MIN..=f32::MAX,
                1.
            );
            test_f_f(
                tanf_deterministic,
                rug::Float::tan,
                f32::MIN..=f32::MAX,
                1.
            );
        }

        #[inline]
        fn atan2kf_u1(y: Doubled<F32x>, mut x: Doubled<F32x>) -> Doubled<F32x> {
            let q = vsel_vi2_vf_vf_vi2_vi2(x.0, ZERO, I32x::splat(-2), I32x::splat(0));
            let p = x.0.simd_lt(ZERO);
            let r = p.to_int().cast() & NEG_ZERO.to_bits();
            x = Doubled::new(
                F32x::from_bits(x.0.to_bits() ^ r),
                F32x::from_bits(x.1.to_bits() ^ r)
            );

            let q = vsel_vi2_vf_vf_vi2_vi2(x.0, y.0, q + I32x::splat(1), q);
            let p = x.0.simd_lt(y.0);
            let s = p.select_doubled(-x, y);
            let mut t = p.select_doubled(y, x);

            let s = s / t;
            t = s.square();
            t = t.normalize();

            let u = F32x::splat(-0.001_763_979_089_446_365_833_282_47)
                .mul_add(t.0, F32x::splat(0.010_790_090_076_625_347_137_451_2))
                .mul_add(t.0, F32x::splat(-0.030_956_460_162_997_245_788_574_2))
                .mul_add(t.0, F32x::splat(0.057_736_508_548_259_735_107_421_9))
                .mul_add(t.0, F32x::splat(-0.083_895_072_340_965_270_996_093_8))
                .mul_add(t.0, F32x::splat(0.109_463_557_600_975_036_621_094))
                .mul_add(t.0, F32x::splat(-0.142_626_821_994_781_494_140_625))
                .mul_add(t.0, F32x::splat(0.199_983_194_470_405_578_613_281));

            t *= F32x::splat(-0.333_332_866_430_282_592_773_438).add_checked_as_doubled(u * t.0);
            t = s * ONE.add_checked(t);
            (Doubled::new(
                F32x::splat(1.570_796_370_506_286_621_1),
                F32x::splat(-4.371_138_828_673_792_886_5_e-8),
            ) * q.cast())
            .add_checked(t)
        }

        /// Arc tangent function of two variables
        ///
        /// This function evaluates the arc tangent function of (***y*** / ***x***).
        /// The quadrant of the result is determined according to the signs
        /// of ***x*** and ***y***.
        /// The error bound of the returned values is `max(1.0 ULP, f32::MIN_POSITIVE)`.
        pub fn atan2f(mut y: F32x, mut x: F32x) -> F32x {
            let o = x.abs().simd_lt(F32x::splat(2.938_737_278_354_183_094_7_e-39)); // nexttowardf((1.0 / FLT_MAX), 1)
            x = o.select(x * F1_24X, x);
            y = o.select(y * F1_24X, y);

            let d = atan2kf_u1(Doubled::from(y.abs()), Doubled::from(x));
            let mut r = F32x::from(d);

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

            F32x::from_bits(
                (x.is_nan() | y.is_nan()).to_int().cast() | r.mul_sign(y).to_bits(),
            )
        }

        #[test]
        fn test_atan2f() {
            test_ff_f(
                atan2f,
                rug::Float::atan2,
                f32::MIN..=f32::MAX,
                f32::MIN..=f32::MAX,
                1.
            );
        }

        /// Arc sine function
        ///
        /// This function evaluates the arc sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn asinf(d: F32x) -> F32x {
            let o = d.abs().simd_lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select_doubled(Doubled::from(d.abs()), x2.sqrt_as_doubled());
            x = d.abs().simd_eq(ONE).select_doubled(Doubled::from(ZERO), x);

            let u = F32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, F32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, F32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, F32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, F32x::splat(0.166_667_729_6))
                * (x2 * x.0);

            let y = Doubled::new(
                F32x::splat(crate::f32::D_PI.0 / 4.),
                F32x::splat(crate::f32::D_PI.1 / 4.),
            )
            .sub_checked(x)
            .sub_checked(u);

            let r = o.select(u + x.0, F32x::from(y) * F32x::splat(2.));
            r.mul_sign(d)
        }

        #[test]
        fn test_asinf() {
            test_f_f(
                asinf,
                rug::Float::asin,
                -1.0..=1.0,
                1.
            );
        }

        /// Arc cosine function
        ///
        /// This function evaluates the arc cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn acosf(d: F32x) -> F32x {
            let o = d.abs().simd_lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);

            let mut x = o.select_doubled(Doubled::from(d.abs()), x2.sqrt_as_doubled());
            x = d.abs().simd_eq(ONE).select_doubled(Doubled::from(ZERO), x);

            let u = F32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, F32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, F32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, F32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, F32x::splat(0.166_667_729_6))
                * (x2 * x.0);

            let mut y = Doubled::new(
                F32x::splat(crate::f32::D_PI.0 / 2.),
                F32x::splat(crate::f32::D_PI.1 / 2.),
            )
            .sub_checked(x.0.mul_sign(d).add_checked_as_doubled(u.mul_sign(d)));
            x = x.add_checked(u);

            y = o.select_doubled(y, x.scale(F32x::splat(2.)));

            y = (!o & d.simd_lt(ZERO)).select_doubled(
                Doubled::<F32x>::splat(crate::f32::D_PI)
                .sub_checked(y),
                y,
            );

            y.into()
        }

        #[test]
        fn test_acosf() {
            test_f_f(
                acosf,
                rug::Float::acos,
                -1.0..=1.0,
                1.
            );
        }

        /// Arc tangent function
        ///
        /// This function evaluates the arc tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn atanf(d: F32x) -> F32x {
            let d2 = atan2kf_u1(Doubled::from(d.abs()), Doubled::from(ONE));
            let mut r = F32x::from(d2);
            r = d
                .is_infinite()
                .select(F32x::splat(1.570_796_326_794_896_557_998_982), r);
            r.mul_sign(d)
        }

        #[test]
        fn test_atanf() {
            test_f_f(
                atanf,
                rug::Float::atan,
                f32::MIN..=f32::MAX,
                1.
            );
        }

        /// Hyperbolic sine function
        ///
        /// This function evaluates the hyperbolic sine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP` if ***a*** is in `[-88.5, 88.5]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct
        /// sign or a correct value with `1.0 ULP` error bound is returned.
        pub fn sinhf(x: F32x) -> F32x {
            let mut y = x.abs();
            let d = expk2f(Doubled::from(y));
            let d = d.sub_checked(d.recpre());
            y = F32x::from(d) * HALF;

            y = (x.abs().simd_gt(F32x::splat(89.)) | y.is_nan()).select(INFINITY, y);
            y = y.mul_sign(x);
            F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_sinhf() {
            test_f_f(
                sinhf,
                rug::Float::sinh,
                -88.5..=88.5,
                1.
            );
        }

        /// Hyperbolic cosine function
        ///
        /// This function evaluates the hyperbolic cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.0 ULP` if ***a** is in `[-88.5, 88.5]`.
        /// If a is a finite value out of this range, infinity with a correct
        /// sign or a correct value with `1.0 ULP` error bound is returned.
        pub fn coshf(x: F32x) -> F32x {
            let mut y = x.abs();
            let d = expk2f(Doubled::from(y));
            let d = d.add_checked(d.recpre());
            y = F32x::from(d) * HALF;

            y = (x.abs().simd_gt(F32x::splat(89.)) | y.is_nan()).select(INFINITY, y);
            F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_coshf() {
            test_f_f(
                coshf,
                rug::Float::cosh,
                -88.5..=88.5,
                1.
            );
        }

        /// Hyperbolic tangent function
        ///
        /// This function evaluates the hyperbolic tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0001 ULP`.
        pub fn tanhf(x: F32x) -> F32x {
            let mut y = x.abs();
            let d = expk2f(Doubled::from(y));
            let e = d.recpre();
            let d = d.add_checked(-e) / d.add_checked(e);
            y = F32x::from(d);

            y = (x.abs().simd_gt(F32x::splat(8.664_339_742)) | y.is_nan()).select(ONE, y); // TODO: check
            y = y.mul_sign(x);
            F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_tanhf() {
            test_f_f(
                tanhf,
                rug::Float::tanh,
                -8.7..=8.7,
                1.0001
            );
        }

        #[inline]
        fn logk2f(d: Doubled<F32x>) -> Doubled<F32x> {
            let e = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                        ilogbkf(d.0 * F32x::splat(1. / 0.75))
                    /*} else {
                        vgetexp_vf_vf(d.0 * F32x::splat(1. / 0.75)).roundi()
                    }*/;
            let m = d.scale(pow2if(-e));

            let x = (m + F32x::splat(-1.)) / (m + ONE);
            let x2 = x.square();

            let t = F32x::splat(0.239_282_846_450_805_664_062_5)
                .mul_add(x2.0, F32x::splat(0.285_182_118_415_832_519_531_25))
                .mul_add(x2.0, F32x::splat(0.400_005_877_017_974_853_515_625))
                .mul_add(x2.0, F32x::splat(0.666_666_686_534_881_591_796_875));

            let mut s = Doubled::<F32x>::splat(crate::f32::D_LN2) * e.cast();
            s = s.add_checked(x.scale(F32x::splat(2.)));
            s.add_checked(x2 * x * t)
        }

        /// Inverse hyperbolic sine function
        ///
        /// This function evaluates the inverse hyperbolic sine function of a value in ***a***.
        /// The error bound of the returned value is `1.001 ULP` if ***a*** is in `[-1.84e+19, 1.84e+19]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct
        /// sign or a correct value with `1.0 ULP` error bound is returned.
        pub fn asinhf(x: F32x) -> F32x {
            let mut y = x.abs();
            let o = y.simd_gt(ONE);

            let mut d = o.select_doubled(x.recpre_as_doubled(), Doubled::from(y));
            d = (d.square() + ONE).sqrt();
            d = o.select_doubled(d * y, d);

            d = logk2f((d + x).normalize());
            y = F32x::from(d);

            y = (x.abs().simd_gt(SQRT_FLT_MAX) | y.is_nan())
                .select(INFINITY.mul_sign(x), y);
            y = F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits());
            x.is_neg_zero().select(NEG_ZERO, y)
        }

        #[test]
        fn test_asinhf() {
            test_f_f(
                asinhf,
                rug::Float::asinh,
                -crate::f32::SQRT_FLT_MAX..=crate::f32::SQRT_FLT_MAX,
                1.0001
            );
        }

        /// Inverse hyperbolic cosine function
        ///
        /// This function evaluates the inverse hyperbolic cosine function of a value in ***a***.
        /// The error bound of the returned value is `1.001 ULP` if ***a*** is in `[-1.84e+19, 1.84e+19]`.
        /// If ***a*** is a finite value out of this range, infinity with a correct
        /// sign or a correct value with `1.0 ULP` error bound is returned.
        pub fn acoshf(x: F32x) -> F32x {
            let d = logk2f(
                x.add_as_doubled(ONE).sqrt() * x.add_as_doubled(F32x::splat(-1.)).sqrt() + x,
            );
            let mut y = F32x::from(d);

            y = (x.abs().simd_gt(SQRT_FLT_MAX) | y.is_nan()).select(INFINITY, y);

            y = F32x::from_bits(!x.simd_eq(ONE).to_int().cast::<u32>() & y.to_bits());

            y = F32x::from_bits(x.simd_lt(ONE).to_int().cast() | y.to_bits());
            F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_acoshf() {
            test_f_f(
                acoshf,
                rug::Float::acosh,
                -crate::f32::SQRT_FLT_MAX..=crate::f32::SQRT_FLT_MAX,
                1.0001
            );
        }

        /// Inverse hyperbolic tangent function
        ///
        /// This function evaluates the inverse hyperbolic tangent function of a value in ***a***.
        /// The error bound of the returned value is `1.0001 ULP`.
        pub fn atanhf(x: F32x) -> F32x {
            let mut y = x.abs();
            let d = logk2f(ONE.add_as_doubled(y) / ONE.add_as_doubled(-y));
            y = F32x::from_bits(
                y.simd_gt(ONE).to_int().cast()
                    | y.simd_eq(ONE).select(INFINITY, F32x::from(d) * HALF).to_bits(),
            );

            y = F32x::from_bits((x.is_infinite() | y.is_nan()).to_int().cast() | y.to_bits());
            y = y.mul_sign(x);
            F32x::from_bits(x.is_nan().to_int().cast() | y.to_bits())
        }

        #[test]
        fn test_atanhf() {
            test_f_f(
                atanhf,
                rug::Float::atanh,
                f32::MIN..=f32::MAX,
                1.0001
            );
        }

        /// Natural logarithmic function
        ///
        /// This function returns the natural logarithm of ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn logf(mut d: F32x) -> F32x {
            let m: F32x;

            let mut s = /*if !cfg!(feature = "enable_avx512f")
                && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.simd_lt(F32x::splat(f32::MIN_POSITIVE));
                d = o.select(d * (F1_32X * F1_32X), d);
                let mut e = ilogb2kf(d * F32x::splat(1. / 0.75));
                m = ldexp3kf(d, -e);
                e = o.select(e - I32x::splat(64), e);
                Doubled::<F32x>::splat(crate::f32::D_LN2) * e.cast()
            }/* else {
                let mut e = vgetexp_vf_vf(d * F32x::splat(1. / 0.75));
                e = e.simd_eq(INFINITY).select(F32x::splat(128.), e);
                m = vgetmant_vf_vf(d);
                Doubled::F32x::splat(crate::f32::D_LN2) * e
            }*/;

            let x = F32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F32x::splat(0.302_729_487_4)
                .mul_add(x2, F32x::splat(0.399_610_817_4))
                .mul_add(x2, F32x::splat(0.666_669_488));

            s = s.add_checked(x.scale(F32x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let r = F32x::from(s);

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            let r = d.simd_eq(INFINITY).select(INFINITY, r);
            let r = (d.simd_lt(ZERO) | d.is_nan()).select(NAN, r);
            d.simd_eq(ZERO).select(NEG_INFINITY, r)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(
                    r,
                    d,
                    I32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[test]
        fn test_logf() {
            test_f_f(
                logf,
                rug::Float::ln,
                0.0..=f32::MAX,
                1.
            );
        }

        /// Base-10 logarithmic function
        ///
        /// This function returns the base-10 logarithm of ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn log10f(mut d: F32x) -> F32x {
            let m: F32x;

            let mut s =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = d.simd_lt(F32x::splat(f32::MIN_POSITIVE));
                    d = o.select(d * (F1_32X * F1_32X), d);
                    let mut e = ilogb2kf(d * F32x::splat(1. / 0.75));
                    m = ldexp3kf(d, -e);
                    e = o.select(e - I32x::splat(64), e);
                    Doubled::new(
                        F32x::splat(0.301_030_01),
                        F32x::splat(-1.432_098_889_e-8)
                    ) * e.cast()
                }/* else {
                    let mut e = vgetexp_vf_vf(d * F32x::splat(1. / 0.75));
                    e = e.simd_eq(INFINITY).select(F32x::splat(128.), e);
                    m = vgetmant_vf_vf(d);
                    Doubled::new(
                        F32x::splat(0.301_030_01),
                        F32x::splat(-1.432_098_889_e-8)
                    ) * e
                }*/;

            let x = F32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F32x::splat(0.131_428_986_8)
                .mul_add(x2, F32x::splat(0.173_549_354_1))
                .mul_add(x2, F32x::splat(0.289_530_962_7));

            s = s.add_checked(x * Doubled::new(
                F32x::splat(0.868_588_984),
                F32x::splat(-2.170_757_285_e-8)
            ));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = F32x::from(s);

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            r = d.simd_eq(INFINITY).select(INFINITY, r);
            r = (d.simd_lt(ZERO) | d.is_nan()).select(NAN, r);
            d.simd_eq(ZERO).select(NEG_INFINITY, r)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(
                    r,
                    d,
                    I32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[test]
        fn test_log10f() {
            test_f_f(
                log10f,
                rug::Float::log10,
                0.0..=f32::MAX,
                1.
            );
        }

        /// Base-2 logarithmic function
        ///
        /// This function returns the base-2 logarithm of ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn log2f(mut d: F32x) -> F32x {
            let m: F32x;

            let ef =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
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

            let x = F32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F32x::splat(0.437_455_028_3)
                .mul_add(x2, F32x::splat(0.576_479_017_7))
                .mul_add(x2, F32x::splat(0.961_801_290_512));
            let mut s = ef
                + x * Doubled::new(
                    F32x::splat(2.885_390_043_258_666_992_2),
                    F32x::splat(3.273_447_448_356_848_861_6_e-8)
                );
            s += x2 * x.0 * t;

            let mut r = F32x::from(s);

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            r = d.simd_eq(INFINITY).select(INFINITY, r);
            r = (d.simd_lt(ZERO) | d.is_nan()).select(NAN, r);
            d.simd_eq(ZERO).select(NEG_INFINITY, r)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(
                    r,
                    d,
                    I32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[test]
        fn test_log2f() {
            test_f_f(
                log2f,
                rug::Float::log2,
                0.0..=f32::MAX,
                1.
            );
        }

        /// Logarithm of one plus argument
        ///
        /// This function returns the natural logarithm of (1+***a***).
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn log1pf(d: F32x) -> F32x {
            let m: F32x;

            let dp1 = d + ONE;

            let mut s =
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                let o = dp1.simd_lt(F32x::splat(f32::MIN_POSITIVE));
                let dp1 = o.select(dp1 * (F1_32X * F1_32X), dp1);
                let e = ilogb2kf(dp1 * F32x::splat(1. / 0.75));
                let t = ldexp3kf(ONE, -e);
                m = d.mul_add(t, t - ONE);
                let e = o.select(e - I32x::splat(64), e);
                Doubled::<F32x>::splat(crate::f32::D_LN2) * e.cast()
            }/* else {
                let e = vgetexp_vf_vf(dp1, F32x::splat(1. / 0.75));
                let e = e.simd_eq(INFINITY).select(F32x::splat(128.), e);
                let t = ldexp3kf(ONE, -e.roundi());
                m = d.mul_add(t, t - ONE);
                Doubled::<F32x>::splat(crate::f32::D_LN2) * e
            }*/;

            let x = Doubled::from(m) / F32x::splat(2.).add_checked_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F32x::splat(0.302_729_487_4)
                .mul_add(x2, F32x::splat(0.399_610_817_4))
                .mul_add(x2, F32x::splat(0.666_669_488));

            s = s.add_checked(x.scale(F32x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = F32x::from(s);

            r = d.simd_gt(F32x::splat(1e+38)).select(INFINITY, r);
            r = F32x::from_bits(F32x::splat(-1.).simd_gt(d).to_int().cast() | r.to_bits());
            r = d.simd_eq(F32x::splat(-1.)).select(NEG_INFINITY, r);
            d.is_neg_zero().select(NEG_ZERO, r)
        }

        #[test]
        fn test_log1pf() {
            test_f_f(
                log1pf,
                rug::Float::ln_1p,
                -1.0..=1e+38,
                1.
            );
        }

        /// Base-*e* exponential function
        ///
        /// This function returns the value of *e* raised to ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn expf(d: F32x) -> F32x {
            let q = (d * R_LN2_F).roundi();

            let s = q.cast::<f32>().mul_add(-L2U_F, d);
            let s = q.cast::<f32>().mul_add(-L2L_F, s);

            let mut u = F32x::splat(0.000_198_527_617_612_853_646_278_381)
                .mul_add(s, F32x::splat(0.001_393_043_552_525_341_510_772_71))
                .mul_add(s, F32x::splat(0.008_333_360_776_305_198_669_433_59))
                .mul_add(s, F32x::splat(0.041_666_485_369_205_474_853_515_6))
                .mul_add(s, F32x::splat(0.166_666_671_633_720_397_949_219))
                .mul_add(s, HALF);

            u = ONE + (s * s).mul_add(u, s);

            u = ldexp2kf(u, q);

            u = F32x::from_bits(!d.simd_lt(F32x::splat(-104.)).to_int().cast::<u32>() & u.to_bits());
            F32x::splat(100.).simd_lt(d).select(INFINITY, u)
        }

        #[test]
        fn test_expf() {
            test_f_f(
                expf,
                rug::Float::exp,
                -104.0..=100.0,
                1.
            );
        }

        /// Base-10 exponential function
        ///
        /// This function returns 10 raised to ***a***.
        /// The error bound of the returned value is `1.09 ULP`.
        pub fn exp10f(d: F32x) -> F32x {
            let mut u = (d * LOG10_2_F).round();
            let q = u.roundi();

            let s = u.mul_add(-L10U_F, d);
            let s = u.mul_add(-L10L_F, s);

            u = F32x::splat(0.680_255_591_9_e-1)
                .mul_add(s, F32x::splat(0.207_808_032_6))
                .mul_add(s, F32x::splat(0.539_390_385_2))
                .mul_add(s, F32x::splat(0.117_124_533_7_e+1))
                .mul_add(s, F32x::splat(0.203_467_869_8_e+1))
                .mul_add(s, F32x::splat(0.265_094_900_1_e+1));
            let x = Doubled::new(
                F32x::splat(2.302_585_124_969_482_421_9),
                F32x::splat(-3.170_517_251_649_359_315_7_e-08)
            ).add_checked(u * s);
            u = ONE.add_checked(x * s).normalize().0;

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
                -50.0..=38.54,
                1.
            );
        }

        /// Base-*e* exponential function minus 1
        ///
        /// This function returns the value one less than *e* raised to ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn expm1f(a: F32x) -> F32x {
            let d = expk2f(Doubled::from(a)) + F32x::splat(-1.);
            let mut x = F32x::from(d);
            x = a
                .simd_gt(F32x::splat(88.722_831_726_074_218_75))
                .select(INFINITY, x);
            x = a
                .simd_lt(F32x::splat(-16.635_532_333_438_687_426_013_570))
                .select(F32x::splat(-1.), x);
            a.is_neg_zero().select(NEG_ZERO, x)
        }

        #[test]
        fn test_expm1f() {
            test_f_f(
                expm1f,
                rug::Float::exp_m1,
                -16.64..=88.73,
                1.
            );
        }

        /// Base-2 exponential function
        ///
        /// This function returns `2` raised to ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn exp2f(d: F32x) -> F32x {
            let mut u = d.round();
            let q = u.roundi();

            let s = d - u;

            u = F32x::splat(0.153_592_089_2_e-3)
                .mul_add(s, F32x::splat(0.133_926_270_1_e-2))
                .mul_add(s, F32x::splat(0.961_838_476_4_e-2))
                .mul_add(s, F32x::splat(0.555_034_726_9_e-1))
                .mul_add(s, F32x::splat(0.240_226_447_6))
                .mul_add(s, F32x::splat(0.693_147_182_5));

            if cfg!(target_feature = "fma") {
                u = u.mul_add(s, ONE);
            } else {
                u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
            }

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
                1.
            );
        }

        #[inline]
        fn logkf(mut d: F32x) -> Doubled<F32x> {
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

            let x = F32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.square();

            let t = F32x::splat(0.240_320_354_700_088_500_976_562)
                .mul_add(x2.0, F32x::splat(0.285_112_679_004_669_189_453_125))
                .mul_add(x2.0, F32x::splat(0.400_007_992_982_864_379_882_812));
            let c = Doubled::new(
                F32x::splat(0.666_666_626_930_236_816_406_25),
                F32x::splat(3.691_838_612_596_143_320_843_11_e-9),
            );

            let mut s = Doubled::<F32x>::splat(crate::f32::D_LN2) * ef;

            s = s.add_checked(x.scale(F32x::splat(2.)));
            s.add_checked(x2 * x * (x2 * t + c))
        }

        #[inline]
        fn expkf(d: Doubled<F32x>) -> F32x {
            let u = <F32x>::from(d) * R_LN2_F;
            let q = u.roundi();

            let mut s = d + q.cast::<f32>() * (-L2U_F);
            s += q.cast::<f32>() * (-L2L_F);

            s = s.normalize();

            let mut u = F32x::splat(0.001_363_246_468_827_128_410_339_36)
                .mul_add(s.0, F32x::splat(0.008_365_969_173_610_210_418_701_17))
                .mul_add(s.0, F32x::splat(0.041_671_082_377_433_776_855_468_8))
                .mul_add(s.0, F32x::splat(0.166_665_524_244_308_471_679_688))
                .mul_add(s.0, F32x::splat(0.499_999_850_988_388_061_523_438));

            let mut t = s.add_checked(s.square() * u);

            t = ONE.add_checked(t);
            u = F32x::from(t);
            u = ldexpkf(u, q);

            F32x::from_bits(!d.0.simd_lt(F32x::splat(-104.)).to_int().cast::<u32>() & u.to_bits())
        }

        /// Power function
        ///
        /// This function returns the value of ***x*** raised to the power of ***y***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn powf(x: F32x, y: F32x) -> F32x {
            if true {
                let yisint = y.trunc().simd_eq(y) | y.abs().simd_gt(F1_24X);
                let yisodd =
                    (y.trunci() & I32x::splat(1)).simd_eq(I32x::splat(1)) & yisint & y.abs().simd_lt(F1_24X);

                #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
                {
                    let yisodd = !y.is_infinite().to_int().cast() & yisodd;
                }

                let mut result = expkf(logkf(x.abs()) * y);

                result = result.is_nan().select(INFINITY, result);

                result *= x.simd_gt(ZERO).select(
                    ONE,
                    yisint.select(yisodd.select(F32x::splat(-1.), ONE), NAN),
                );

                let efx = (x.abs() - ONE).mul_sign(y);

                result = y.is_infinite().select(
                    F32x::from_bits(
                        !efx.simd_lt(ZERO).to_int().cast::<u32>()
                            & efx.simd_eq(ZERO).select(ONE, INFINITY).to_bits(),
                    ),
                    result,
                );

                result = (x.is_infinite() | x.simd_eq(ZERO)).select(
                    yisodd.select(x.sign(), ONE)
                        * F32x::from_bits(
                            !x.simd_eq(ZERO).select(-y, y).simd_lt(ZERO).to_int().cast::<u32>()
                                & INFINITY.to_bits(),
                        ),
                    result,
                );

                result = F32x::from_bits(
                    (x.is_nan() | y.is_nan()).to_int().cast() | result.to_bits(),
                );

                (y.simd_eq(ZERO) | x.simd_eq(ONE)).select(ONE, result)
            } else {
                expkf(logkf(x) * y)
            }
        }

        #[test]
        fn test_powf() {
            use rug::{ops::Pow, Float};
            test_ff_f(
                powf,
                |in1, in2| Float::with_val(in1.prec(), in1.pow(in2)),
                f32::MIN..=f32::MAX,
                f32::MIN..=f32::MAX,
                1.
            );
        }

        /// Cube root function
        ///
        /// This function returns the real cube root of ***a***.
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn cbrtf(mut d: F32x) -> F32x {
            let mut q2 = Doubled::from(ONE);

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                let s = d;
            }*/
            let e = ilogbkf(d.abs()) + I32x::splat(1);
            d = ldexp2kf(d, -e);

            let t = e.cast::<f32>() + F32x::splat(6144.);
            let qu = (t * F32x::splat(1. / 3.)).trunci();
            let re = (t - qu.cast::<f32>() * F32x::splat(3.)).trunci();

            q2 = re.simd_eq(I32x::splat(1)).select_doubled(
                Doubled::new(
                    F32x::splat(1.259_921_073_913_574_218_8),
                    F32x::splat(-2.401_870_169_421_727_041_5_e-8),
                ),
                q2,
            );
            q2 = re.simd_eq(I32x::splat(2)).select_doubled(
                Doubled::new(
                    F32x::splat(1.587_401_032_447_814_941_4),
                    F32x::splat(1.952_038_530_816_935_235_6_e-8)
                ),
                q2,
            );

            q2.0 = q2.0.mul_sign(d);
            q2.1 = q2.1.mul_sign(d);
            d = d.abs();

            let mut x = F32x::splat(-0.601_564_466_953_277_587_890_625)
                .mul_add(d, F32x::splat(2.820_889_234_542_846_679_687_5))
                .mul_add(d, F32x::splat(-5.532_182_216_644_287_109_375))
                .mul_add(d, F32x::splat(5.898_262_500_762_939_453_125))
                .mul_add(d, F32x::splat(-3.809_541_702_270_507_812_5))
                .mul_add(d, F32x::splat(2.224_125_623_703_002_929_687_5));

            let mut y = x * x;
            y = y * y;
            x -= d.neg_mul_add(y, x) * F32x::splat(-1. / 3.);

            let mut z = x;

            let mut u = x.mul_as_doubled(x);
            u = u * u;
            u *= d;
            u += -x;
            y = F32x::from(u);

            y = F32x::splat(-2. / 3.) * y * z;
            let mut v = z.mul_as_doubled(z) + y;
            v *= d;
            v *= q2;
            z = ldexp2kf(F32x::from(v), qu - I32x::splat(2048));

            z = d.is_infinite().select(INFINITY.mul_sign(q2.0), z);
            z = d.simd_eq(ZERO).select(F32x::from_bits(q2.0.sign_bit()), z);

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                z = s.is_infinite().select(INFINITY.mul_sign(s), z);
                z = s
                    .simd_eq(ZERO)
                    .select((ZERO, s).mul_sign(z);
            }*/

            z
        }

        #[test]
        fn test_cbrtf() {
            test_f_f(
                cbrtf,
                rug::Float::cbrt,
                f32::MIN..=f32::MAX,
                1.
            );
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        fn gammafk(a: F32x) -> (Doubled<F32x>, Doubled<F32x>) {
            let mut clln = Doubled::from(ONE);
            let mut clld = Doubled::from(ONE);

            let otiny = a.abs().simd_lt(F32x::splat(1e-30));
            let oref = a.simd_lt(HALF);

            let x = otiny.select_doubled(
                Doubled::from(ZERO),
                oref.select_doubled(ONE.add_as_doubled(-a), Doubled::from(a)),
            );

            let o0 = HALF.simd_le(x.0) & x.0.simd_le(F32x::splat(1.2));
            let o2 = F32x::splat(2.3).simd_le(x.0);

            let mut y = ((x + ONE) * x).normalize();
            y = ((x + F32x::splat(2.)) * y).normalize();

            let o = o2 & x.0.simd_le(F32x::splat(7.));
            clln = o.select_doubled(y, clln);

            let mut x = o.select_doubled(x + F32x::splat(3.), x);
            let t = o2.select(x.0.recip(), (x + o0.select_splat(-1., -2.)).normalize().0);

            let u = F32x::select3(
                o2,
                o0,
                0.000_839_498_720_672_087_279_971_000_786,
                0.943_515_777_6,
                0.110_248_955_e-3,
            )
            .mul_add(
                t,
                F32x::select3(
                    o2,
                    o0,
                    -5.171_790_908_260_592_193_293_944_22_e-5,
                    0.867_006_361_5,
                    0.816_001_993_4_e-4,
                ),
            )
            .mul_add(
                t,
                F32x::select3(
                    o2,
                    o0,
                    -0.000_592_166_437_353_693_882_857_342_347,
                    0.482_670_247_6,
                    0.152_846_885_6_e-3,
                ),
            )
            .mul_add(
                t,
                F32x::select3(
                    o2,
                    o0,
                    6.972_813_758_365_857_774_037_435_39_e-5,
                    -0.885_512_977_8_e-1,
                    -0.235_506_871_8_e-3,
                ),
            )
            .mul_add(
                t,
                F32x::select3(
                    o2,
                    o0,
                    0.000_784_039_221_720_066_627_493_314_301,
                    0.101_382_523_8,
                    0.496_224_209_2_e-3,
                ),
            )
            .mul_add(
                t,
                F32x::select3(
                    o2,
                    o0,
                    -0.000_229_472_093_621_399_176_949_318_732,
                    -0.149_340_897_8,
                    -0.119_348_801_7_e-2,
                ),
            )
            .mul_add(
                t,
                F32x::select3(
                    o2,
                    o0,
                    -0.002_681_327_160_493_827_160_473_958_490,
                    0.169_750_914,
                    0.289_159_943_3_e-2,
                ),
            )
            .mul_add(
                t,
                F32x::select3(
                    o2,
                    o0,
                    0.003_472_222_222_222_222_222_175_164_840,
                    -0.207_245_454_2,
                    -0.738_545_181_2_e-2,
                ),
            )
            .mul_add(
                t,
                F32x::select3(
                    o2,
                    o0,
                    0.083_333_333_333_333_333_335_592_087_900,
                    0.270_587_235_7,
                    0.205_807_704_5_e-1,
                ),
            );

            y = (x + F32x::splat(-0.5)) * logk2f(x);
            y += -x;
            y += Doubled::from(0.918_938_533_204_672_780_56_f64); // 0.5*log(2*M_PI)

            let mut z = u.mul_as_doubled(t)
                + o0.select_splat(
                    -0.400_686_534_596_170_958_447_352_690_395,
                    -0.673_523_028_297_382_446_749_257_758_235_e-1,
                );
            z = z * t
                + o0.select_splat(
                    0.822_466_960_142_643_054_450_325_495_997,
                    0.322_467_033_928_981_157_743_538_726_901,
                );
            z = z * t
                + o0.select_splat(
                    -0.577_215_665_946_766_039_837_398_973_297,
                    0.422_784_335_087_484_338_986_941_629_852,
                );
            z *= t;

            let mut clc = o2.select_doubled(y, z);

            clld = o2.select_doubled(u.mul_as_doubled(t) + ONE, clld);

            y = clln;

            clc = otiny.select_doubled(
                Doubled::from(41.588_830_833_596_718_565_03_f64), // log(2^60)
                oref.select_doubled(
                    Doubled::<F32x>::from(1.144_729_885_849_400_163_9_f64) + (-clc),
                    clc,
                ),
            ); // log(M_PI)
            clln = otiny.select_doubled(Doubled::from(ONE), oref.select_doubled(clln, clld));

            if !(!oref).all() {
                let t = a - F1_12X * (a * (ONE / F1_12X)).trunci().cast();
                x = clld * sinpifk(t);
            }

            clld = otiny.select_doubled(
                Doubled::from(a * (F1_30X * F1_30X)),
                oref.select_doubled(x, y),
            );

            (clc, clln / clld)
        }

        /// Gamma function
        ///
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn tgammaf(a: F32x) -> F32x {
            let (da, db) = gammafk(a);
            let y = expk2f(da) * db;
            let r = F32x::from(y);

            let o = a.simd_eq(NEG_INFINITY)
                | (a.simd_lt(ZERO) & a.is_integer())
                | (a.is_finite() & a.simd_lt(ZERO) & r.is_nan());
            let r = o.select(NAN, r);

            let o = (a.simd_eq(INFINITY) | a.is_finite())
                & a.simd_ge(F32x::splat(-f32::MIN_POSITIVE))
                & (a.simd_eq(ZERO) | a.simd_gt(F32x::splat(36.)) | r.is_nan());
            o.select(INFINITY.mul_sign(a), r)
        }

        #[test]
        fn test_tgammaf() {
            test_f_f(
                tgammaf,
                rug::Float::gamma,
                f32::MIN..=f32::MAX,
                1.0
            );
        }

        /// Log gamma function
        ///
        /// The error bound of the returned value is `1.0 ULP` if the argument is positive.
        /// If the argument is larger than `4e+36`, it may return infinity instead of the correct value.
        /// The error bound is `max(1 ULP and 1e-8)`, if the argument is negative.
        pub fn lgammaf(a: F32x) -> F32x {
            let (da, db) = gammafk(a);
            let y = da + logk2f(db.abs());
            let r = F32x::from(y);

            let o =
                a.is_infinite() | ((a.simd_le(ZERO) & a.is_integer()) | (a.is_finite() & r.is_nan()));
            o.select(INFINITY, r)
        }

        #[test]
        fn test_lgammaf() {
            test_f_f(
                lgammaf,
                rug::Float::ln_gamma,
                0.0..=4e36,
                1.0
            );
        }

        fn dfmla(x: F32x, y: Doubled<F32x>, z: Doubled<F32x>) -> Doubled<F32x> {
            z + (y * x)
        }
        fn poly2df_b(x: F32x, c1: Doubled<F32x>, c0: Doubled<F32x>) -> Doubled<F32x> {
            dfmla(x, c1, c0)
        }
        fn poly2df(x: F32x, c1: F32x, c0: Doubled<F32x>) -> Doubled<F32x> {
            dfmla(x, Doubled::from(c1), c0)
        }
        fn poly4df(x: F32x, c3: F32x, c2: Doubled<F32x>, c1: Doubled<F32x>, c0: Doubled<F32x>) -> Doubled<F32x> {
            dfmla(x*x, poly2df(x, c3, c2), poly2df_b(x, c1, c0))
        }

        /// Error function
        ///
        /// The error bound of the returned value is `1.0 ULP`.
        pub fn erff(a: F32x) -> F32x {
            let x = a.abs();
            let x2 = x * x;
            let x4 = x2 * x2;
            let o25 = x.simd_le(F32x::splat(2.5));

            let mut t2;
            if o25.all() {
                // Abramowitz and Stegun
                let t = F32x::poly6(x, x2, x4,
                    -0.436_044_700_8_e-6,
                    0.686_751_536_7_e-5,
                    -0.304_515_67_e-4,
                    0.980_853_656_1_e-4,
                    0.239_552_391_6_e-3,
                    0.145_990_154_1_e-3);
                t2 = poly4df(x, t,
                    Doubled::new(
                        F32x::splat(0.009_288_344_532_251_358_032_2),
                        F32x::splat(-2.786_374_589_702_533_075_5_e-11)
                    ),
                    Doubled::new(
                        F32x::splat(0.042_275_499_552_488_327_026),
                        F32x::splat(1.346_139_928_998_810_605_7_e-09)
                    ),
                    Doubled::new(
                        F32x::splat(0.070_523_701_608_180_999_756),
                        F32x::splat(-3.661_630_931_870_736_516_3_e-09)
                    )
                );
                t2 = ONE.add_checked(t2 * x);
                t2 = t2.square();
                t2 = t2.square();
                t2 = t2.square();
                t2 = t2.square();
                t2 = t2.recpre();
            } else {
                let t = F32x::poly6(x, x2, x4,
                        o25.select_splat(-0.436_044_700_8_e-6, -0.113_001_284_8_e-6),
                        o25.select_splat(0.686_751_536_7_e-5, 0.411_527_298_6_e-5),
                        o25.select_splat(-0.304_515_67_e-4, -0.692_830_435_6_e-4),
                        o25.select_splat(0.980_853_656_1_e-4, 0.717_269_256_7_e-3),
                        o25.select_splat(0.239_552_391_6_e-3, -0.513_104_535_6_e-2),
                        o25.select_splat(0.145_990_154_1_e-3, 0.270_863_715_6_e-1)
                );
                t2 = poly4df(x, t,
                    o25.select_doubled(
                        Doubled::new(
                            F32x::splat(0.009_288_344_532_251_358_032_2),
                            F32x::splat(-2.786_374_589_702_533_075_5_e-11)
                        ),
                        Doubled::new(
                            F32x::splat(-0.110_643_193_125_724_792_48),
                            F32x::splat(3.705_045_277_722_528_300_7_e-09))
                    ),
                    o25.select_doubled(
                        Doubled::new(
                            F32x::splat(0.042_275_499_552_488_327_026),
                            F32x::splat(1.346_139_928_998_810_605_7_e-09)
                        ),
                        Doubled::new(
                            F32x::splat(-0.631_922_304_630_279_541_02),
                            F32x::splat(-2.020_043_258_507_317_785_9_e-08)
                        )
                    ),
                    o25.select_doubled(
                        Doubled::new(
                            F32x::splat(0.070_523_701_608_180_999_756),
                            F32x::splat(-3.661_630_931_870_736_516_3_e-09)
                        ),
                        Doubled::new(
                            F32x::splat(-1.129_663_825_035_095_214_8),
                            F32x::splat(2.551_512_019_645_325_925_2_e-08)
                        )
                    )
                );
                t2 *= x;
                let mut s2 = ONE.add_checked(t2);
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.recpre();
                t2 = o25.select_doubled(s2, Doubled::from(expkf(t2)));
            }

            t2 += F32x::splat(-1.);
            t2 = x.simd_lt(F32x::splat(1e-4)).select_doubled(Doubled::new(
                F32x::splat(-1.128_379_225_730_895_996_1),
                F32x::splat(5.863_538_342_219_759_109_7_e-08)
            ) * x, t2);

            let mut z = -F32x::from(t2);
            z = x.simd_ge(F32x::splat(6.)).select(ONE, z);
            z = a.is_infinite().select(ONE, z);
            z = a.simd_eq(ZERO).select(ZERO, z);
            z.mul_sign(a)
        }

        #[test]
        fn test_erff() {
            test_f_f(
                erff,
                rug::Float::erf,
                f32::MIN..=f32::MAX,
                0.75
            );
        }
    };
}
