macro_rules! impl_math_f32_u35 {
    () => {
        use super::*;

        pub fn sinf(mut d: F32x) -> F32x {
            let mut q: I32x;
            let u: F32x;
            let r = d;

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                q = (d * F32x::FRAC_1_PI).roundi();
                u = F32x::from_cast(q);
                d = u.mul_add(-PI_A2_F, d);
                d = u.mul_add(-PI_B2_F, d);
                d = u.mul_add(-PI_C2_F, d);
            } else if d.abs().lt(TRIGRANGEMAX_F).all() {
                q = (d * F32x::FRAC_1_PI).roundi();
                u = F32x::from_cast(q);
                d = u.mul_add(-PI_A_F, d);
                d = u.mul_add(-PI_B_F, d);
                d = u.mul_add(-PI_C_F, d);
                d = u.mul_add(-PI_D_F, d);
            } else {
                let (mut dfidf, dfii) = rempif(d);
                q = dfii & I32x::splat(3);
                q = q + q + dfidf
                    .0
                    .gt(ZERO)
                    .select(I32x::splat(2), I32x::splat(1));
                q >>= 2;
                let o = (dfii & I32x::splat(1)).eq(I32x::splat(1));
                let mut x = Doubled::new(
                    F32x::splat(3.141_592_741_012_573_242_2 * -0.5).mul_sign(dfidf.0),
                    F32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5).mul_sign(dfidf.0),
                );
                x = dfidf + x;
                dfidf = o.select_doubled(x, dfidf);
                d = dfidf.0 + dfidf.1;

                d = F32x::from_bits(
                    U32x::from_bits(r.is_infinite() | r.is_nan()) |
                    U32x::from_bits(d)
                );
            }

            let s = d * d;

            d = F32x::from_bits(
                (
                    U32x::from_bits((q & I32x::splat(1)).eq(I32x::splat(1))) &
                    U32x::from_bits(NEG_ZERO)
                ) ^ U32x::from_bits(d),
            );

            let mut u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s, F32x::splat(0.008_333_078_585_565_090_179_443_36))
                .mul_add(s, F32x::splat(-0.166_666_597_127_914_428_710_938));

            u = s * (u * d) + d;

            r.is_neg_zero().select(r, u)
        }

        pub fn cosf(mut d: F32x) -> F32x {
            let mut q: I32x;
            let r = d;

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                q = (d * F32x::FRAC_1_PI - HALF).roundi();
                q = q + q + I32x::splat(1);

                let u = F32x::from_cast(q);
                d = u.mul_add(-PI_A2_F * HALF, d);
                d = u.mul_add(-PI_B2_F * HALF, d);
                d = u.mul_add(-PI_C2_F * HALF, d);
            } else if d.abs().lt(TRIGRANGEMAX_F).all() {
                q = (d * F32x::FRAC_1_PI - HALF).roundi();
                q = q + q + I32x::splat(1);

                let u = F32x::from_cast(q);
                d = u.mul_add(-PI_A_F * HALF, d);
                d = u.mul_add(-PI_B_F * HALF, d);
                d = u.mul_add(-PI_C_F * HALF, d);
                d = u.mul_add(-PI_D_F * HALF, d);
            } else {
                let (mut dfidf, dfii) = rempif(d);
                q = dfii & I32x::splat(3);
                q = q + q + dfidf
                    .0
                    .gt(ZERO)
                    .select(I32x::splat(8), I32x::splat(7));
                q >>= 1;
                let o = (dfii & I32x::splat(1)).eq(I32x::splat(0));
                let y = dfidf
                    .0
                    .gt(ZERO)
                    .select(ZERO, F32x::splat(-1.));
                let mut x = Doubled::new(
                    F32x::splat(3.141_592_741_012_573_242_2 * -0.5).mul_sign(y),
                    F32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5).mul_sign(y),
                );
                x = dfidf + x;
                dfidf = o.select_doubled(x, dfidf);
                d = dfidf.0 + dfidf.1;

                d = F32x::from_bits(
                    U32x::from_bits(r.is_infinite() | r.is_nan()) |
                    U32x::from_bits(d)
                );
            }

            let s = d * d;

            d = F32x::from_bits(
                (
                    U32x::from_bits((q & I32x::splat(2)).eq(I32x::splat(0))) &
                    U32x::from_bits(NEG_ZERO)
                ) ^ U32x::from_bits(d),
            );

            let u = F32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                .mul_add(s, F32x::splat(-0.000_198_106_907_191_686_332_225_8))
                .mul_add(s, F32x::splat(0.008_333_078_585_565_090_179_443_36))
                .mul_add(s, F32x::splat(-0.166_666_597_127_914_428_710_938));

            s * (u * d) + d
        }

        pub fn tanf(d: F32x) -> F32x {
            let q: I32x;

            let mut x = d;

            if d.abs().lt(TRIGRANGEMAX2_F * HALF).all() {
                q = (d * F32x::FRAC_2_PI).roundi();
                let u = F32x::from_cast(q);
                x = u.mul_add(-PI_A2_F * HALF, x);
                x = u.mul_add(-PI_B2_F * HALF, x);
                x = u.mul_add(-PI_C2_F * HALF, x);
            } else if d.abs().lt(TRIGRANGEMAX_F).all() {
                q = (d * (2. * F32x::FRAC_1_PI)).roundi();
                let u = F32x::from_cast(q);
                x = u.mul_add(-PI_A_F * HALF, x);
                x = u.mul_add(-PI_B_F * HALF, x);
                x = u.mul_add(-PI_C_F * HALF, x);
                x = u.mul_add(-PI_D_F * HALF, x);
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                x = dfidf.0 + dfidf.1;
                x = F32x::from_bits(
                    U32x::from_bits(d.is_infinite() | d.is_nan()) |
                    U32x::from_bits(x)
                );
                x = d.is_neg_zero().select(d, x);
            }

            let s = x * x;

            let o = (q & I32x::splat(1)).eq(I32x::splat(1));
            x = F32x::from_bits(
                (U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(x),
            );

            let mut u = F32x::splat(0.009_272_458_031_773_567_199_707_03)
                .mul_add(s, F32x::splat(0.003_319_849_958_643_317_222_595_21))
                .mul_add(s, F32x::splat(0.024_299_807_846_546_173_095_703_1))
                .mul_add(s, F32x::splat(0.053_449_530_154_466_629_028_320_3))
                .mul_add(s, F32x::splat(0.133_383_005_857_467_651_367_188))
                .mul_add(s, F32x::splat(0.333_331_853_151_321_411_132_812));

            u = s.mul_add(u * x, x);

            o.select(u.recpre(), u)
        }

        pub fn sincosf(d: F32x) -> (F32x, F32x) {
            let q: I32x;
            let mut s = d;

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                q = (d * F32x::FRAC_2_PI).roundi();
                let u = F32x::from_cast(q);
                s = u.mul_add(-PI_A2_F * HALF, s);
                s = u.mul_add(-PI_B2_F * HALF, s);
                s = u.mul_add(-PI_C2_F * HALF, s);
            } else if d.abs().lt(TRIGRANGEMAX_F).all() {
                q = (d * F32x::FRAC_2_PI).roundi();
                let u = F32x::from_cast(q);
                s = u.mul_add(-PI_A_F * HALF, s);
                s = u.mul_add(-PI_B_F * HALF, s);
                s = u.mul_add(-PI_C_F * HALF, s);
                s = u.mul_add(-PI_D_F * HALF, s);
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                s = dfidf.0 + dfidf.1;
                s = F32x::from_bits(
                    U32x::from_bits(d.is_infinite() | d.is_nan()) |
                    U32x::from_bits(s)
                );
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

        pub fn sincospif(d: F32x) -> (F32x, F32x) {
            let u = d * F32x::splat(4.);
            let q = u.trunci();
            let q = (q + (I32x::from_bits(U32x::from_bits(q) >> 31) ^ I32x::splat(1)))
                & I32x::splat(!1);
            let s = u - F32x::from_cast(q);

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

            let o = (q & I32x::splat(2)).eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(4)).eq(I32x::splat(4));
            rsin = F32x::from_bits(
                (U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(rsin),
            );

            let o = ((q + I32x::splat(2)) & I32x::splat(4)).eq(I32x::splat(4));
            rcos = F32x::from_bits(
                (U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(rcos),
            );

            let o = d.abs().gt(F32x::splat(1e+7));
            rsin = F32x::from_bits(!U32x::from_bits(o) & U32x::from_bits(rsin));
            rcos = F32x::from_bits(!U32x::from_bits(o) & U32x::from_bits(rcos));

            let o = d.is_infinite();
            rsin = F32x::from_bits(U32x::from_bits(o) | U32x::from_bits(rsin));
            rcos = F32x::from_bits(U32x::from_bits(o) | U32x::from_bits(rcos));

            (rsin, rcos)
        }

        pub fn atanf(d: F32x) -> F32x {
            let q = vsel_vi2_vf_vi2(d, I32x::splat(2));
            let s = d.abs();

            let q = vsel_vi2_vf_vf_vi2_vi2(ONE, s, q + I32x::splat(1), q);
            let s = ONE.lt(s).select(s.recpre(), s);

            let mut t = s * s;

            let u = F32x::splat(0.002_823_638_962_581_753_730_773_93)
                .mul_add(t, F32x::splat(-0.015_956_902_876_496_315_002_441_4))
                .mul_add(t, F32x::splat(0.042_504_988_610_744_476_318_359_4))
                .mul_add(t, F32x::splat(-0.074_890_092_015_266_418_457_031_2))
                .mul_add(t, F32x::splat(0.106_347_933_411_598_205_566_406))
                .mul_add(t, F32x::splat(-0.142_027_363_181_114_196_777_344))
                .mul_add(t, F32x::splat(0.199_926_957_488_059_997_558_594))
                .mul_add(t, F32x::splat(-0.333_331_018_686_294_555_664_062));

            t = s.mul_add(t * u, s);

            t = (q & I32x::splat(1))
                .eq(I32x::splat(1))
                .select(F32x::FRAC_PI_2 - t, t);

            t = F32x::from_bits(
                (U32x::from_bits((q & I32x::splat(2)).eq(I32x::splat(2))) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(t),
            );

            if cfg!(feature = "enable_neon32") || cfg!(feature = "enable_neon32vfpv4") {
                t = d.is_infinite().select(
                    F32x::splat(1.587_401_051_968_199_474_751_705_6).mul_sign(d),
                    t,
                );
            }

            t
        }

        pub fn atan2f(y: F32x, x: F32x) -> F32x {
            let mut r = atan2kf(y.abs(), x);

            r = r.mul_sign(x);
            r = (x.is_infinite() | x.eq(ZERO)).select(
                F32x::FRAC_PI_2
                    - visinf2_vf_vf_vf(x, F32x::FRAC_PI_2.mul_sign(x)),
                r,
            );
            r = y.is_infinite().select(
                F32x::FRAC_PI_2
                    - visinf2_vf_vf_vf(x, F32x::FRAC_PI_4.mul_sign(x)),
                r,
            );

            r = y.eq(ZERO).select(
                F32x::from_bits(
                    U32x::from_bits(x.is_sign_negative()) &
                    U32x::from_bits(F32x::PI)
                ),
                r,
            );

            F32x::from_bits(
                U32x::from_bits(x.is_nan() | y.is_nan()) |
                U32x::from_bits(r.mul_sign(y))
            )
        }

        pub fn asinf(d: F32x) -> F32x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let x = o.select(d.abs(), x2.sqrt());

            let u = F32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, F32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, F32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, F32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, F32x::splat(0.166_667_729_6))
                .mul_add(x * x2, x);

            let r = o.select(u, u.mul_add(F32x::splat(-2.), F32x::FRAC_PI_2));
            r.mul_sign(d)
        }

        pub fn acosf(d: F32x) -> F32x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select(d.abs(), x2.sqrt());
            x = d.abs().eq(ONE).select(ZERO, x);

            let u = F32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, F32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, F32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, F32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, F32x::splat(0.166_667_729_6))
                * (x2 * x);

            let y = F32x::splat(3.141_592_653_589_793_2 / 2.)
                - (x.mul_sign(d) + u.mul_sign(d));
            x += u;
            let r = o.select(y, x * F32x::splat(2.));
            (!o & d.lt(ZERO)).select(
                Doubled::from((3.141_592_741_012_573_242_2, -8.742_277_657_347_585_773_1_e-8))
                    .add_checked(-r)
                    .0,
                r,
            )
        }

        pub fn logf(mut d: F32x) -> F32x {
            let m: F32x;

            let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.lt(F32x::splat(f32::MIN));
                d = o.select(d * (F1_32X * F1_32X), d);
                let mut e = vilogb2k_vi2_vf(d * F32x::splat(1. / 0.75));
                m = vldexp3_vf_vf_vi2(d, -e);
                e = o.select(e - I32x::splat(64), e);
                F32x::from_cast(e)
            }/* else {
                let mut e = vgetexp_vf_vf(d * F32x::splat(1. / 0.75));
                e = e.eq(F32x::INFINITY).select(F32x::splat(128.), e);
                m = vgetmant_vf_vf(d);
                e
            }*/;

            let mut x = (F32x::splat(-1.) + m) / (ONE + m);
            let x2 = x * x;

            let t = F32x::splat(0.239_282_846_450_805_664_062_5)
                .mul_add(x2, F32x::splat(0.285_182_118_415_832_519_531_25))
                .mul_add(x2, F32x::splat(0.400_005_877_017_974_853_515_625))
                .mul_add(x2, F32x::splat(0.666_666_686_534_881_591_796_875))
                .mul_add(x2, F32x::splat(2.));

            x = x.mul_add(t, F32x::splat(0.693_147_180_559_945_286_226_764) * ef);
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                x = d.eq(F32x::INFINITY).select(F32x::INFINITY, x);
                x = (d.lt(ZERO) | d.is_nan()).select(F32x::NAN, x);
                d.eq(ZERO)
                    .select(F32x::NEG_INFINITY, x)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(x, d, I32x::splat(5 << (5 * 4)), 0)
            }*/
        }

        #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
        pub fn sqrtf(d: F32x) -> F32x {
            let e = F32x::from_bits(
                U32x::splat(0x_2000_0000)
                    + (U32x::splat(0x_7f00_0000) & (U32x::from_bits(d) >> 1)),
            );
            let m = F32x::from_bits(
                I32x::splat(0x_3f00_0000) + (I32x::splat(0x_01ff_ffff) & I32x::from_bits(d)),
            );
            let mut x = vrsqrteq_f32(m);
            x = vmulq_f32(x, vrsqrtsq_f32(m, vmulq_f32(x, x)));
            let mut u = vmulq_f32(x, m);
            u = vmlaq_f32(u, vmlsq_f32(m, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
            e = F32x::from_bits(
                !U32x::from_bits(d.eq(ZERO)) &
                U32x::from_bits(e)
            );
            u = e * u;

            u = d.is_infinite().select(F32x::INFINITY, u);
            u = F32x::from_bits(
                U32x::from_bits(d.is_nan() | d.lt(ZERO)) |
                U32x::from_bits(u)
            );
            u.mul_sign(d)
        }
        /*#[cfg(feature = "enable_vecext")]
                pub fn xsqrtf_u35(d: F32x) -> F32x {
                    let mut q = d.sqrt();
                    q = d.is_neg_zero().select(NEG_ZERO, q);
                    d.eq(F32x::INFINITY).select(F32x::INFINITY, q)
                }*/
        #[cfg(all(
                    not(feature = "enable_neon32"),
                    not(feature = "enable_neon32vfpv4"),
                //    not(feature = "enable_vecext")
                ))]
        pub fn sqrtf(d: F32x) -> F32x {
            d.sqrt()
        }

        pub fn cbrtf(mut d: F32x) -> F32x {
            let mut q = ONE;

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                let s = d;
            }*/
            let e = vilogbk_vi2_vf(d.abs()) + I32x::splat(1);
            d = vldexp2_vf_vf_vi2(d, -e);

            let t = F32x::from_cast(e) + F32x::splat(6144.);
            let qu = (t * F32x::splat(1. / 3.)).trunci();
            let re = (t - F32x::from_cast(qu) * F32x::splat(3.)).trunci();

            q = re
                .eq(I32x::splat(1))
                .select(F32x::splat(1.259_921_049_894_873_164_767_210_6), q);
            q = re
                .eq(I32x::splat(2))
                .select(F32x::splat(1.587_401_051_968_199_474_751_705_6), q);
            q = vldexp2_vf_vf_vi2(q, qu - I32x::splat(2048));

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
                y = s.is_infinite().select(F32x::INFINITY.mul_sign(s), y);
                y = s
                    .eq(ZERO)
                    .select(ZERO.mul_sign(s), y);
            }*/

            y
        }


        pub fn sinhf(x: F32x) -> F32x {
            let e = expm1fk(x.abs());
            let mut y = (e + F32x::splat(2.)) / (e + ONE);
            y *= HALF * e;

            y = (x.abs().gt(F32x::splat(88.)) | y.is_nan())
                .select(F32x::INFINITY, y);
            y = y.mul_sign(x);
            F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y))
        }

        pub fn coshf(x: F32x) -> F32x {
            let e = u10::expf(x.abs());
            let mut y = HALF.mul_add(e, HALF / e);

            y = (x.abs().gt(F32x::splat(88.)) | y.is_nan())
                .select(F32x::INFINITY, y);
            F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y))
        }

        pub fn tanhf(x: F32x) -> F32x {
            let d = expm1fk(F32x::splat(2.) * x.abs());
            let mut y = d / (F32x::splat(2.) + d);

            y = (x.abs().gt(F32x::splat(8.664_339_742)) | y.is_nan())
                .select(ONE, y);
            y = y.mul_sign(x);
            F32x::from_bits(U32x::from_bits(x.is_nan()) | U32x::from_bits(y))
        }


        pub fn hypotf(x: F32x, y: F32x) -> F32x {
            let x = x.abs();
            let y = y.abs();
            let min = x.min(y);
            let max = x.max(y);

            let t = min / max;
            let mut ret = max * t.mul_add(t, ONE).sqrt();
            ret = min.eq(ZERO).select(max, ret);
            ret = (x.is_nan() | y.is_nan()).select(F32x::NAN, ret);
            (x.eq(F32x::INFINITY) | y.eq(F32x::INFINITY))
                .select(F32x::INFINITY, ret)
        }

    };
}
