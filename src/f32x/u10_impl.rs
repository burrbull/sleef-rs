macro_rules! impl_math_f32_u10 {
    () => {
        use super::*;

        #[cfg(not(feature = "deterministic"))]
        pub fn sinf(d: F32x) -> F32x {
            let mut q: I32x;
            let mut s: Doubled<F32x>;

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                let u = (d * F32x::FRAC_1_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F, d);
                s = v.add_as_doubled(u * (-PI_B2_F));
                s = s.add_checked(u * (-PI_C2_F));
            } else {
                let (mut dfidf, dfii) = rempif(d);
                q = dfii & I32x::splat(3);
                q = q + q + dfidf.0.gt(ZERO).select(I32x::splat(2), I32x::splat(1));
                q >>= 2;
                let o = (dfii & I32x::splat(1)).eq(I32x::splat(1));
                let mut x = Doubled::new(
                    F32x::splat(3.141_592_741_012_573_242_2 * -0.5).mul_sign(dfidf.0),
                    F32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5).mul_sign(dfidf.0),
                );
                x = dfidf + x;
                dfidf = o.select_doubled(x, dfidf);
                s = dfidf.normalize();

                s.0 = F32x::from_bits(
                    U32x::from_bits(d.is_infinite() | d.is_nan()) | U32x::from_bits(s.0),
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
                (U32x::from_bits((q & I32x::splat(1)).eq(I32x::splat(1)))
                    & U32x::from_bits(NEG_ZERO))
                    ^ U32x::from_bits(u),
            );

            d.is_neg_zero().select(d, u)
        }

        #[cfg(feature = "deterministic")]
        pub fn sinf(d: F32x) -> F32x {
            let u = (d * F32x::FRAC_1_PI).round();
            let mut q = u.roundi();
            let v = u.mul_add((-PI_A2_F), d);
            let mut s = v.add_as_doubled(u * (-PI_B2_F));
            s = s.add_checked(u * (-PI_C2_F));
            let g = d.abs().lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let (mut dfidf, dfii) = rempif(d);
                let mut q2 = dfii & I32x::splat(3);
                q2 = q2 + q2 + dfidf.0.gt(ZERO).select(I32x::splat(2), I32x::splat(1));
                q2 >>= 2;
                let o = (dfii & I32x::splat(1)).eq(I32x::splat(1));
                let mut x = Doubled::new(
                    F32x::splat(3.141_592_741_012_573_242_2 * -0.5).mul_sign(dfidf.0),
                    F32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5).mul_sign(dfidf.0),
                );
                x = dfidf + x;
                dfidf = o.select_doubled(x, dfidf);
                let mut t = dfidf.normalize();

                t.0 = F32x::from_bits(
                    U32x::from_bits(d.is_infinite() | d.is_nan()) | U32x::from_bits(t.0),
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
                (U32x::from_bits((q & I32x::splat(1)).eq(I32x::splat(1)))
                    & U32x::from_bits(NEG_ZERO))
                    ^ U32x::from_bits(u),
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
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn cosf(d: F32x) -> F32x {
            let mut q: I32x;
            let mut s: Doubled<F32x>;

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                let dq = (d.mul_add(F32x::FRAC_1_PI, F32x::splat(-0.5)))
                    .round()
                    .mul_add(F32x::splat(2.), ONE);
                q = dq.roundi();
                s = d.add_as_doubled(dq * (-PI_A2_F) * HALF);
                s += dq * (-PI_B2_F) * HALF;
                s += dq * (-PI_C2_F) * HALF;
            } else {
                let (mut dfidf, dfii) = rempif(d);
                q = dfii & I32x::splat(3);
                q = q + q + dfidf.0.gt(ZERO).select(I32x::splat(8), I32x::splat(7));
                q >>= 1;
                let o = (dfii & I32x::splat(1)).eq(I32x::splat(0));
                let y = dfidf.0.gt(ZERO).select(ZERO, F32x::splat(-1.));
                let mut x = Doubled::new(
                    F32x::splat(3.141_592_741_012_573_242_2 * -0.5).mul_sign(y),
                    F32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5).mul_sign(y),
                );
                x = dfidf + x;
                dfidf = o.select_doubled(x, dfidf);
                s = dfidf.normalize();

                s.0 = F32x::from_bits(
                    U32x::from_bits(d.is_infinite() | d.is_nan()) | U32x::from_bits(s.0),
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
                (U32x::from_bits((q & I32x::splat(2)).eq(I32x::splat(0)))
                    & U32x::from_bits(NEG_ZERO))
                    ^ U32x::from_bits(u),
            )
        }

        #[cfg(feature = "deterministic")]
        pub fn cosf(d: F32x) -> F32x {
            let dq = (d.mul_add(F32x::FRAC_1_PI, F32x::splat(-0.5)))
                .round()
                .mul_add(F32x::splat(2.), ONE);
            let mut q = dq.roundi();
            let mut s = d.add_as_doubled(dq * (-PI_A2_F * HALF));
            s += dq * (-PI_B2_F * HALF);
            s += dq * (-PI_C2_F * HALF);
            let g = d.abs().lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let (mut dfidf, dfii) = rempif(d);
                let mut q2 = dfii & I32x::splat(3);
                q2 = q2 + q2 + dfidf.0.gt(ZERO).select(I32x::splat(8), I32x::splat(7));
                q2 >>= 1;
                let o = (dfii & I32x::splat(1)).eq(I32x::splat(0));
                let y = dfidf.0.gt(ZERO).select(ZERO, F32x::splat(-1.));
                let mut x = Doubled::new(
                    F32x::splat(3.141_592_741_012_573_242_2 * -0.5).mul_sign(y),
                    F32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5).mul_sign(y),
                );
                x = dfidf + x;
                dfidf = o.select_doubled(x, dfidf);
                let mut t = dfidf.normalize();

                t.0 = F32x::from_bits(
                    U32x::from_bits(d.is_infinite() | d.is_nan()) | U32x::from_bits(t.0),
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
                (U32x::from_bits((q & I32x::splat(2)).eq(I32x::splat(0)))
                    & U32x::from_bits(NEG_ZERO))
                    ^ U32x::from_bits(u),
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
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn sincosf(d: F32x) -> (F32x, F32x) {
            let q: I32x;
            let mut s: Doubled<F32x>;

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                let u = (d * F32x::FRAC_2_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F * HALF, d);
                s = v.add_as_doubled(u * (-PI_B2_F) * HALF);
                s = s.add_checked(u * (-PI_C2_F) * HALF);
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                s = dfidf;
                let o = d.is_infinite() | d.is_nan();
                s.0 = F32x::from_bits(U32x::from_bits(o) | U32x::from_bits(s.0));
            }

            let t = s;

            s.0 = s.square_as_f();

            let u = F32x::splat(-0.000_195_169_282_960_705_459_117_889)
                .mul_add(s.0, F32x::splat(0.008_332_157_507_538_795_471_191_41))
                .mul_add(s.0, F32x::splat(-0.166_666_537_523_269_653_320_312))
                * (s.0 * t.0);

            let x = t.add_checked(u);
            let rx = x.0 + x.1;

            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F32x::splat(-2.718_118_423_672_422_068_193_55_e-7)
                .mul_add(s.0, F32x::splat(2.479_904_469_510_074_704_885_48_e-5))
                .mul_add(s.0, F32x::splat(-0.001_388_887_874_782_085_418_701_17))
                .mul_add(s.0, F32x::splat(0.041_666_664_183_139_801_025_390_6))
                .mul_add(s.0, F32x::splat(-0.5));

            let x = ONE.add_checked(s.0.mul_as_doubled(u));
            let ry = x.0 + x.1;

            let o = (q & I32x::splat(1)).eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(2)).eq(I32x::splat(2));
            rsin = F32x::from_bits(
                (U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(rsin),
            );

            let o = ((q + I32x::splat(1)) & I32x::splat(2)).eq(I32x::splat(2));
            rcos = F32x::from_bits(
                (U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(rcos),
            );

            (rsin, rcos)
        }

        #[cfg(feature = "deterministic")]
        pub fn sincosf(d: F32x) -> (F32x, F32x) {
            let u = (d * F32x::FRAC_2_PI).round();
            let mut q = u.roundi();
            let v = u.mul_add(-PI_A2_F * HALF, d);
            let mut s = v.add_as_doubled(u * (-PI_B2_F * HALF));
            s = s.add_checked(u * (-PI_C2_F * HALF));
            let g = d.abs().lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let (dfidf, dfii) = rempif(d);
                let mut t = dfidf;
                let o = d.is_infinite() | d.is_nan();
                t.0 = F32x::from_bits(U32x::from_bits(o) | U32x::from_bits(t.0));
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
            let mut rx = x.0 + x.1;

            rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F32x::splat(-2.718_118_423_672_422_068_193_55_e-7)
                .mul_add(s.0, F32x::splat(2.479_904_469_510_074_704_885_48_e-5))
                .mul_add(s.0, F32x::splat(-0.001_388_887_874_782_085_418_701_17))
                .mul_add(s.0, F32x::splat(0.041_666_664_183_139_801_025_390_6))
                .mul_add(s.0, F32x::splat(-0.5));

            let x = ONE.add_checked(s.0.mul_as_doubled(u));
            let ry = x.0 + x.1;

            let o = (q & I32x::splat(1)).eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(2)).eq(I32x::splat(2));
            rsin = F32x::from_bits(
                (U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(rsin),
            );

            let o = ((q + I32x::splat(1)) & I32x::splat(2)).eq(I32x::splat(2));
            rcos = F32x::from_bits(
                (U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(rcos),
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
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn tanf(d: F32x) -> F32x {
            let q: I32x;

            let mut s = if d.abs().lt(TRIGRANGEMAX2_F).all() {
                let u = (d * F32x::FRAC_2_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F * HALF, d);
                v.add_as_doubled(u * (-PI_B2_F) * HALF)
                    .add_checked(u * (-PI_C2_F) * HALF)
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                let o = d.is_infinite() | d.is_nan();
                Doubled::new(
                    F32x::from_bits(U32x::from_bits(o) | U32x::from_bits(dfidf.0)),
                    F32x::from_bits(U32x::from_bits(o) | U32x::from_bits(dfidf.1)),
                )
            };

            let o = (q & I32x::splat(1)).eq(I32x::splat(1));
            let n = U32x::from_bits(o) & U32x::from_bits(NEG_ZERO);
            s.0 = F32x::from_bits(U32x::from_bits(s.0) ^ n);
            s.1 = F32x::from_bits(U32x::from_bits(s.1) ^ n);

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

            let u = x.0 + x.1;

            d.is_neg_zero().select(d, u)
        }

        #[cfg(feature = "deterministic")]
        pub fn tanf(d: F32x) -> F32x {
            let u = (d * F32x::FRAC_2_PI).round();
            let mut q = u.roundi();
            let v = u.mul_add(-PI_A2_F * HALF, d);
            let mut s = v.add_as_doubled(u * (-PI_B2_F * HALF));
            s = s.add_checked(u * (-PI_C2_F * HALF));
            let g = d.abs().lt(TRIGRANGEMAX2_F);

            if !g.all() {
                let (dfidf, dfii) = rempif(d);
                let mut t = dfidf;
                let o = d.is_infinite() | d.is_nan();
                t.0 = F32x::from_bits(U32x::from_bits(o) | U32x::from_bits(t.0));
                t.1 = F32x::from_bits(U32x::from_bits(o) | U32x::from_bits(t.1));
                q = g.select(q, dfii);
                s = g.select_doubled(s, t);
            }

            let o = (q & I32x::splat(1)).eq(I32x::splat(1));
            let n = U32x::from_bits(o) & U32x::from_bits(NEG_ZERO);
            s.0 = F32x::from_bits(U32x::from_bits(s.0) ^ n);
            s.1 = F32x::from_bits(U32x::from_bits(s.1) ^ n);

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

            let u = x.0 + x.1;

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
        }

        #[inline]
        fn atan2kf_u1(y: Doubled<F32x>, mut x: Doubled<F32x>) -> Doubled<F32x> {
            let q = vsel_vi2_vf_vf_vi2_vi2(x.0, ZERO, I32x::splat(-2), I32x::splat(0));
            let p = x.0.lt(ZERO);
            let r = U32x::from_bits(p) & U32x::from_bits(NEG_ZERO);
            x.0 = F32x::from_bits(U32x::from_bits(x.0) ^ r);
            x.1 = F32x::from_bits(U32x::from_bits(x.1) ^ r);

            let q = vsel_vi2_vf_vf_vi2_vi2(x.0, y.0, q + I32x::splat(1), q);
            let p = x.0.lt(y.0);
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
            (Doubled::from((
                1.570_796_370_506_286_621_1,
                -4.371_138_828_673_792_886_5_e-8,
            )) * F32x::from_cast(q))
            .add_checked(t)
        }

        pub fn atan2f(mut y: F32x, mut x: F32x) -> F32x {
            let o = x.abs().lt(F32x::splat(2.938_737_278_354_183_094_7_e-39)); // nexttowardf((1.0 / FLT_MAX), 1)
            x = o.select(x * F1_24X, x);
            y = o.select(y * F1_24X, y);

            let d = atan2kf_u1(Doubled::new(y.abs(), ZERO), Doubled::new(x, ZERO));
            let mut r = d.0 + d.1;

            r = r.mul_sign(x);
            r = (x.is_infinite() | x.eq(ZERO)).select(
                F32x::FRAC_PI_2 - visinf2_vf_vf_vf(x, F32x::FRAC_PI_2.mul_sign(x)),
                r,
            );
            r = y.is_infinite().select(
                F32x::FRAC_PI_2 - visinf2_vf_vf_vf(x, F32x::FRAC_PI_4.mul_sign(x)),
                r,
            );
            r = y.eq(ZERO).select(
                F32x::from_bits(U32x::from_bits(x.is_sign_negative()) & U32x::from_bits(F32x::PI)),
                r,
            );

            F32x::from_bits(
                U32x::from_bits(x.is_nan() | y.is_nan()) | U32x::from_bits(r.mul_sign(y)),
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

        pub fn asinf(d: F32x) -> F32x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
            x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

            let u = F32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, F32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, F32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, F32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, F32x::splat(0.166_667_729_6))
                * (x2 * x.0);

            let y = Doubled::from((
                3.141_592_741_012_573_242_2 / 4.,
                -8.742_277_657_347_585_773_1_e-8 / 4.,
            ))
            .sub_checked(x)
            .sub_checked(u);

            let r = o.select(u + x.0, (y.0 + y.1) * F32x::splat(2.));
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

        pub fn acosf(d: F32x) -> F32x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);

            let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
            x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

            let u = F32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, F32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, F32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, F32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, F32x::splat(0.166_667_729_6))
                * (x2 * x.0);

            let mut y = Doubled::from((
                3.141_592_741_012_573_242_2 / 2.,
                -8.742_277_657_347_585_773_1_e-8 / 2.,
            ))
            .sub_checked(x.0.mul_sign(d).add_checked_as_doubled(u.mul_sign(d)));
            x = x.add_checked(u);

            y = o.select_doubled(y, x.scale(F32x::splat(2.)));

            y = (!o & d.lt(ZERO)).select_doubled(
                Doubled::from((
                    3.141_592_741_012_573_242_2,
                    -8.742_277_657_347_585_773_1_e-8,
                ))
                .sub_checked(y),
                y,
            );

            y.0 + y.1
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

        pub fn atanf(d: F32x) -> F32x {
            let d2 = atan2kf_u1(Doubled::new(d.abs(), ZERO), Doubled::from((1., 0.)));
            let mut r = d2.0 + d2.1;
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

        pub fn expf(d: F32x) -> F32x {
            let q = (d * R_LN2_F).roundi();

            let s = F32x::from_cast(q).mul_add(-L2U_F, d);
            let s = F32x::from_cast(q).mul_add(-L2L_F, s);

            let mut u = F32x::splat(0.000_198_527_617_612_853_646_278_381)
                .mul_add(s, F32x::splat(0.001_393_043_552_525_341_510_772_71))
                .mul_add(s, F32x::splat(0.008_333_360_776_305_198_669_433_59))
                .mul_add(s, F32x::splat(0.041_666_485_369_205_474_853_515_6))
                .mul_add(s, F32x::splat(0.166_666_671_633_720_397_949_219))
                .mul_add(s, HALF);

            u = ONE + (s * s).mul_add(u, s);

            u = ldexp2kf(u, q);

            u = F32x::from_bits(!U32x::from_bits(d.lt(F32x::splat(-104.))) & U32x::from_bits(u));
            F32x::splat(100.).lt(d).select(F32x::INFINITY, u)
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

        pub fn cbrtf(mut d: F32x) -> F32x {
            let mut q2 = Doubled::from((1., 0.));

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                let s = d;
            }*/
            let e = ilogbkf(d.abs()) + I32x::splat(1);
            d = ldexp2kf(d, -e);

            let t = F32x::from_cast(e) + F32x::splat(6144.);
            let qu = (t * F32x::splat(1. / 3.)).trunci();
            let re = (t - F32x::from_cast(qu) * F32x::splat(3.)).trunci();

            q2 = re.eq(I32x::splat(1)).select_doubled(
                Doubled::from((
                    1.259_921_073_913_574_218_8,
                    -2.401_870_169_421_727_041_5_e-8,
                )),
                q2,
            );
            q2 = re.eq(I32x::splat(2)).select_doubled(
                Doubled::from((1.587_401_032_447_814_941_4, 1.952_038_530_816_935_235_6_e-8)),
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
            y = u.0 + u.1;

            y = F32x::splat(-2. / 3.) * y * z;
            let mut v = z.mul_as_doubled(z) + y;
            v *= d;
            v *= q2;
            z = ldexp2kf(v.0 + v.1, qu - I32x::splat(2048));

            z = d.is_infinite().select(F32x::INFINITY.mul_sign(q2.0), z);
            z = d.eq(ZERO).select(F32x::from_bits(q2.0.sign_bit()), z);

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                z = s.is_infinite().select(F32x::INFINITY.mul_sign(s), z);
                z = s
                    .eq(ZERO)
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

        pub fn logf(mut d: F32x) -> F32x {
            let m: F32x;

            let mut s = /*if !cfg!(feature = "enable_avx512f")
                && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.lt(F32x::splat(f32::MIN_POSITIVE));
                d = o.select(d * (F1_32X * F1_32X), d);
                let mut e = ilogb2kf(d * F32x::splat(1. / 0.75));
                m = ldexp3kf(d, -e);
                e = o.select(e - I32x::splat(64), e);
                Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * F32x::from_cast(e)
            }/* else {
                let mut e = vgetexp_vf_vf(d * F32x::splat(1. / 0.75));
                e = e.eq(F32x::INFINITY).select(F32x::splat(128.), e);
                m = vgetmant_vf_vf(d);
                Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * e
            }*/;

            let x = F32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F32x::splat(0.302_729_487_4)
                .mul_add(x2, F32x::splat(0.399_610_817_4))
                .mul_add(x2, F32x::splat(0.666_669_488));

            s = s.add_checked(x.scale(F32x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            let r = d.eq(F32x::INFINITY).select(F32x::INFINITY, r);
            let r = (d.lt(ZERO) | d.is_nan()).select(F32x::NAN, r);
            d.eq(ZERO).select(F32x::NEG_INFINITY, r)
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

        pub fn powf(x: F32x, y: F32x) -> F32x {
            if true {
                let yisint = y.trunc().eq(y) | y.abs().gt(F1_24X);
                let yisodd =
                    (y.trunci() & I32x::splat(1)).eq(I32x::splat(1)) & yisint & y.abs().lt(F1_24X);

                #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
                {
                    let yisodd = !U32x::from_bits(y.is_infinite()) & yisodd;
                }

                let mut result = expkf(logkf(x.abs()) * y);

                result = result.is_nan().select(F32x::INFINITY, result);

                result *= x.gt(ZERO).select(
                    ONE,
                    yisint.select(yisodd.select(F32x::splat(-1.), ONE), F32x::NAN),
                );

                let efx = (x.abs() - ONE).mul_sign(y);

                result = y.is_infinite().select(
                    F32x::from_bits(
                        !U32x::from_bits(efx.lt(ZERO))
                            & U32x::from_bits(efx.eq(ZERO).select(ONE, F32x::INFINITY)),
                    ),
                    result,
                );

                result = (x.is_infinite() | x.eq(ZERO)).select(
                    yisodd.select(x.sign(), ONE)
                        * F32x::from_bits(
                            !U32x::from_bits(x.eq(ZERO).select(-y, y).lt(ZERO))
                                & U32x::from_bits(F32x::INFINITY),
                        ),
                    result,
                );

                result = F32x::from_bits(
                    U32x::from_bits(x.is_nan() | y.is_nan()) | U32x::from_bits(result),
                );

                (y.eq(ZERO) | x.eq(ONE)).select(ONE, result)
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

        pub fn sinhf(x: F32x) -> F32x {
            let mut y = x.abs();
            let d = expk2f(Doubled::new(y, ZERO));
            let d = d.sub_checked(d.recpre());
            y = (d.0 + d.1) * HALF;

            y = (x.abs().gt(F32x::splat(89.)) | y.is_nan()).select(F32x::INFINITY, y);
            y = y.mul_sign(x);
            F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y))
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

        pub fn coshf(x: F32x) -> F32x {
            let mut y = x.abs();
            let d = expk2f(Doubled::new(y, ZERO));
            let d = d.add_checked(d.recpre());
            y = (d.0 + d.1) * HALF;

            y = (x.abs().gt(F32x::splat(89.)) | y.is_nan()).select(F32x::INFINITY, y);
            F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y))
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

        pub fn tanhf(x: F32x) -> F32x {
            let mut y = x.abs();
            let d = expk2f(Doubled::new(y, ZERO));
            let e = d.recpre();
            let d = d.add_checked(-e) / d.add_checked(e);
            y = d.0 + d.1;

            y = (x.abs().gt(F32x::splat(8.664_339_742)) | y.is_nan()).select(ONE, y); // TODO: check
            y = y.mul_sign(x);
            F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y))
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

        pub fn asinhf(x: F32x) -> F32x {
            let mut y = x.abs();
            let o = y.gt(ONE);

            let mut d = o.select_doubled(x.recpre_as_doubled(), Doubled::new(y, ZERO));
            d = (d.square() + ONE).sqrt();
            d = o.select_doubled(d * y, d);

            d = logk2f((d + x).normalize());
            y = d.0 + d.1;

            y = (x.abs().gt(SQRT_FLT_MAX) | y.is_nan())
                .select(F32x::INFINITY.mul_sign(x), y);
            y = F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y));
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

        pub fn acoshf(x: F32x) -> F32x {
            let d = logk2f(
                x.add_as_doubled(ONE).sqrt() * x.add_as_doubled(F32x::splat(-1.)).sqrt() + x,
            );
            let mut y = d.0 + d.1;

            y = (x.abs().gt(SQRT_FLT_MAX) | y.is_nan()).select(F32x::INFINITY, y);

            y = F32x::from_bits(!U32x::from_bits(x.eq(ONE)) & U32x::from_bits(y));

            y = F32x::from_bits(U32x::from_bits(x.lt(ONE)) | U32x::from_bits(y));
            F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y))
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

        pub fn atanhf(x: F32x) -> F32x {
            let mut y = x.abs();
            let d = logk2f(ONE.add_as_doubled(y) / ONE.add_as_doubled(-y));
            y = F32x::from_bits(
                U32x::from_bits(y.gt(ONE))
                    | U32x::from_bits(y.eq(ONE).select(F32x::INFINITY, (d.0 + d.1) * HALF)),
            );

            y = F32x::from_bits(U32x::from_bits(x.is_infinite() | y.is_nan()) | U32x::from_bits(y));
            y = y.mul_sign(x);
            F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y))
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
                .gt(F32x::splat(38.531_839_419_103_623_894_138_7))
                .select(F32x::INFINITY, u);
            F32x::from_bits(!U32x::from_bits(d.lt(F32x::splat(-50.))) & U32x::from_bits(u))
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

        pub fn expm1f(a: F32x) -> F32x {
            let d = expk2f(Doubled::new(a, ZERO)) + F32x::splat(-1.);
            let mut x = d.0 + d.1;
            x = a
                .gt(F32x::splat(88.722_831_726_074_218_75))
                .select(F32x::INFINITY, x);
            x = a
                .lt(F32x::splat(-16.635_532_333_438_687_426_013_570))
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

        pub fn log10f(mut d: F32x) -> F32x {
            let m: F32x;

            let mut s =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = d.lt(F32x::splat(f32::MIN_POSITIVE));
                    d = o.select(d * (F1_32X * F1_32X), d);
                    let mut e = ilogb2kf(d * F32x::splat(1. / 0.75));
                    m = ldexp3kf(d, -e);
                    e = o.select(e - I32x::splat(64), e);
                    Doubled::from((0.301_030_01, -1.432_098_889_e-8)) * F32x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vf_vf(d * F32x::splat(1. / 0.75));
                    e = e.eq(F32x::INFINITY).select(F32x::splat(128.), e);
                    m = vgetmant_vf_vf(d);
                    Doubled::from((0.301_030_01, -1.432_098_889_e-8)) * e
                }*/;

            let x = F32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F32x::splat(0.131_428_986_8)
                .mul_add(x2, F32x::splat(0.173_549_354_1))
                .mul_add(x2, F32x::splat(0.289_530_962_7));

            s = s.add_checked(x * Doubled::from((0.868_588_984, -2.170_757_285_e-8)));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            r = d.eq(F32x::INFINITY).select(F32x::INFINITY, r);
            r = (d.lt(ZERO) | d.is_nan()).select(F32x::NAN, r);
            d.eq(ZERO).select(F32x::NEG_INFINITY, r)
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

        pub fn log2f(mut d: F32x) -> F32x {
            let m: F32x;

            let ef =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = d.lt(F32x::splat(f32::MIN_POSITIVE));
                    d = o.select(d * (F1_32X * F1_32X), d);
                    let mut e = ilogb2kf(d * F32x::splat(1. / 0.75));
                    m = ldexp3kf(d, -e);
                    e = o.select(e - I32x::splat(64), e);
                    F32x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vf_vf(d * F32x::splat(1. / 0.75));
                    e = e.eq(F32x::INFINITY).select(F32x::splat(128.), e);
                    m = vgetmant_vf_vf(d);
                    e
                }*/;

            let x = F32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F32x::splat(0.437_455_028_3)
                .mul_add(x2, F32x::splat(0.576_479_017_7))
                .mul_add(x2, F32x::splat(0.961_801_290_512));
            let mut s = ef
                + x * Doubled::from((2.885_390_043_258_666_992_2, 3.273_447_448_356_848_861_6_e-8));
            s += x2 * x.0 * t;

            let mut r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            r = d.eq(F32x::INFINITY).select(F32x::INFINITY, r);
            r = (d.lt(ZERO) | d.is_nan()).select(F32x::NAN, r);
            d.eq(ZERO).select(F32x::NEG_INFINITY, r)
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

        pub fn tgammaf(a: F32x) -> F32x {
            let (da, db) = gammafk(a);
            let y = expk2f(da) * db;
            let r = y.0 + y.1;

            let o = a.eq(F32x::NEG_INFINITY)
                | (a.lt(ZERO) & a.is_integer())
                | (a.is_finite() & a.lt(ZERO) & r.is_nan());
            let r = o.select(F32x::NAN, r);

            let o = (a.eq(F32x::INFINITY) | a.is_finite())
                & a.ge(F32x::splat(-f32::MIN_POSITIVE))
                & (a.eq(ZERO) | a.gt(F32x::splat(36.)) | r.is_nan());
            o.select(F32x::INFINITY, a).mul_sign(r)
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

        pub fn lgammaf(a: F32x) -> F32x {
            let (da, db) = gammafk(a);
            let y = da + logk2f(db.abs());
            let r = y.0 + y.1;

            let o =
                a.is_infinite() | ((a.le(ZERO) & a.is_integer()) | (a.is_finite() & r.is_nan()));
            o.select(F32x::INFINITY, r)
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
            dfmla(x, Doubled::new(c1, ZERO), c0)
        }
        fn poly4df(x: F32x, c3: F32x, c2: Doubled<F32x>, c1: Doubled<F32x>, c0: Doubled<F32x>) -> Doubled<F32x> {
            dfmla(x*x, poly2df(x, c3, c2), poly2df_b(x, c1, c0))
        }

        pub fn erff(a: F32x) -> F32x {
            let x = a.abs();
            let x2 = x * x;
            let x4 = x2 * x2;
            let o25 = x.le(F32x::splat(2.5));

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
                    Doubled::from((0.009_288_344_532_251_358_032_2, -2.786_374_589_702_533_075_5_e-11)),
                    Doubled::from((0.042_275_499_552_488_327_026, 1.346_139_928_998_810_605_7_e-09)),
                    Doubled::from((0.070_523_701_608_180_999_756, -3.661_630_931_870_736_516_3_e-09))
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
                        o25.select_splat(0.145_990_154_1_e-3, 0.270_863_715_6_e-1));
                t2 = poly4df(x, t,
                    o25.select_doubled(
                        Doubled::from((0.009_288_344_532_251_358_032_2, -2.786_374_589_702_533_075_5_e-11)),
                        Doubled::from((-0.110_643_193_125_724_792_48, 3.705_045_277_722_528_300_7_e-09))
                            ),
                    o25.select_doubled(
                        Doubled::from((0.042_275_499_552_488_327_026, 1.346_139_928_998_810_605_7_e-09)),
                        Doubled::from((-0.631_922_304_630_279_541_02, -2.020_043_258_507_317_785_9_e-08))
                            ),
                    o25.select_doubled(
                        Doubled::from((0.070_523_701_608_180_999_756, -3.661_630_931_870_736_516_3_e-09)),
                        Doubled::from((-1.129_663_825_035_095_214_8, 2.551_512_019_645_325_925_2_e-08))
                            ));
                t2 *= x;
                let mut s2 = ONE.add_checked(t2);
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.square();
                s2 = s2.recpre();
                t2 = o25.select_doubled(s2, Doubled::new(expkf(t2), ZERO));
            }

            t2 += F32x::splat(-1.);
            t2 = x.lt(F32x::splat(1e-4)).select_doubled(Doubled::from((-1.128_379_225_730_895_996_1, 5.863_538_342_219_759_109_7_e-08)) * x, t2);

            let mut z = -(t2.0 + t2.1);
            z = x.ge(F32x::splat(6.)).select(ONE, z);
            z = a.is_infinite().select(ONE, z);
            z = a.eq(ZERO).select(ZERO, z);
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

        pub fn log1pf(d: F32x) -> F32x {
            let m: F32x;

            let dp1 = d + ONE;

            let mut s =
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                let o = dp1.lt(F32x::splat(f32::MIN_POSITIVE));
                let dp1 = o.select(dp1 * (F1_32X * F1_32X), dp1);
                let e = ilogb2kf(dp1 * F32x::splat(1. / 0.75));
                let t = ldexp3kf(ONE, -e);
                m = d.mul_add(t, t - ONE);
                let e = o.select(e - I32x::splat(64), e);
                Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * F32x::from_cast(e)
            }/* else {
                let e = vgetexp_vf_vf(dp1, F32x::splat(1. / 0.75));
                let e = e.eq(F32x::INFINITY).select(F32x::splat(128.), e);
                let t = ldexp3kf(ONE, -e.roundi());
                m = d.mul_add(t, t - ONE);
                Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * e
            }*/;

            let x = Doubled::new(m, ZERO) / F32x::splat(2.).add_checked_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F32x::splat(0.302_729_487_4)
                .mul_add(x2, F32x::splat(0.399_610_817_4))
                .mul_add(x2, F32x::splat(0.666_669_488));

            s = s.add_checked(x.scale(F32x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = s.0 + s.1;

            r = d.gt(F32x::splat(1e+38)).select(F32x::INFINITY, r);
            r = F32x::from_bits(U32x::from_bits(F32x::splat(-1.).gt(d)) | U32x::from_bits(r));
            r = d.eq(F32x::splat(-1.)).select(F32x::NEG_INFINITY, r);
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

            if !cfg!(target_feature = "fma") {
                u = u.mul_adde(s, ONE);
            } else {
                u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
            }

            u = ldexp2kf(u, q);

            u = d.ge(F32x::splat(128.)).select(F32x::INFINITY, u);
            F32x::from_bits(!U32x::from_bits(d.lt(F32x::splat(-150.))) & U32x::from_bits(u))
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
    };
}
