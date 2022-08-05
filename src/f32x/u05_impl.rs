macro_rules! impl_math_f32_u05 {
    () => {
        use super::*;

        /// Evaluate sin( π***a*** ) and cos( π***a*** ) for given ***a*** simultaneously
        ///
        /// Evaluates the sine and cosine functions of π***a*** at a time, and store the two values in a tuple.
        /// The error bound of the returned value are `max(0.506 ULP, f32::MIN_POSITIVE)` if `[-1e+7, 1e+7]`.
        /// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
        /// If ***a*** is a `NaN` or infinity, a `NaN` is returned.
        pub fn sincospif(d: F32x) -> (F32x, F32x) {
            let u = d * F32x::splat(4.);
            let q = u.trunci();
            let q = (q + ((q.cast() >> U32x::splat(31)).cast() ^ I32x::splat(1))) & I32x::splat(!1);
            let s = u - q.cast();

            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            let u = F32x::splat(0.309_384_205_4_e-6)
                .mul_add(s, F32x::splat(-0.365_730_738_8_e-4))
                .mul_add(s, F32x::splat(0.249_039_358_5_e-2));
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

            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F32x::splat(-0.243_061_180_1_e-7)
                .mul_add(s, F32x::splat(0.359_057_708_e-5))
                .mul_add(s, F32x::splat(-0.325_991_772_1_e-3));
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

            x = x * s2 + ONE;
            let ry = F32x::from(x);

            let o = (q & I32x::splat(2)).simd_eq(I32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & I32x::splat(4)).simd_eq(I32x::splat(4));
            rsin = F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ rsin.to_bits());

            let o = ((q + I32x::splat(2)) & I32x::splat(4)).simd_eq(I32x::splat(4));
            rcos = F32x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ rcos.to_bits());

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
                0.505,
            );
        }

        /// Square root function
        ///
        /// The error bound of the returned value is `0.5001 ULP`.
        pub fn sqrtf(d: F32x) -> F32x {
            /*
            #if defined(ENABLE_FMA_SP)
              vfloat q, w, x, y, z;

              d = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(d, vcast_vf_f(0)), vcast_vf_f(SLEEF_NANf), d);

              vopmask o = vlt_vo_vf_vf(d, vcast_vf_f(5.2939559203393770e-23f));
              d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, vcast_vf_f(1.8889465931478580e+22f)), d);
              q = vsel_vf_vo_vf_vf(o, vcast_vf_f(7.2759576141834260e-12f), vcast_vf_f(1.0f));

              y = vreinterpret_vf_vi2(vsub_vi2_vi2_vi2(vcast_vi2_i(0x5f3759df), vsrl_vi2_vi2_i(vreinterpret_vi2_vf(d), 1)));

              x = vmul_vf_vf_vf(d, y);         w = vmul_vf_vf_vf(vcast_vf_f(0.5), y);
              y = vfmanp_vf_vf_vf_vf(x, w, vcast_vf_f(0.5));
              x = vfma_vf_vf_vf_vf(x, y, x);   w = vfma_vf_vf_vf_vf(w, y, w);
              y = vfmanp_vf_vf_vf_vf(x, w, vcast_vf_f(0.5));
              x = vfma_vf_vf_vf_vf(x, y, x);   w = vfma_vf_vf_vf_vf(w, y, w);

              y = vfmanp_vf_vf_vf_vf(x, w, vcast_vf_f(1.5));  w = vadd_vf_vf_vf(w, w);
              w = vmul_vf_vf_vf(w, y);
              x = vmul_vf_vf_vf(w, d);
              y = vfmapn_vf_vf_vf_vf(w, d, x); z = vfmanp_vf_vf_vf_vf(w, x, vcast_vf_f(1));

              z = vfmanp_vf_vf_vf_vf(w, y, z); w = vmul_vf_vf_vf(vcast_vf_f(0.5), x);
              w = vfma_vf_vf_vf_vf(w, z, y);
              w = vadd_vf_vf_vf(w, x);

              w = vmul_vf_vf_vf(w, q);

              w = vsel_vf_vo_vf_vf(vor_vo_vo_vo(veq_vo_vf_vf(d, vcast_vf_f(0)),
                                veq_vo_vf_vf(d, vcast_vf_f(SLEEF_INFINITYf))), d, w);

              w = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(d, vcast_vf_f(0)), vcast_vf_f(SLEEF_NANf), w);

              return w;
            #else
            */

            let d = d.simd_lt(ZERO).select(NAN, d);

            let o = d.simd_lt(F32x::splat(5.293_955_920_339_377_e-23));
            let d = o.select(d * F32x::splat(1.888_946_593_147_858_e+22), d);
            let q = o.select(F32x::splat(7.275_957_614_183_426_e-12 * 0.5), HALF);

            let o = d.simd_gt(F32x::splat(1.844_674_407_370_955_2_e+19));
            let d = o.select(d * F32x::splat(5.421_010_862_427_522_e-20), d);
            let q = o.select(F32x::splat(4_294_967_296.0 * 0.5), q);

            let mut x = F32x::from_bits(
                (I32x::splat(0x_5f37_5a86)
                    - ((d + F32x::splat(1e-45)).to_bits() >> U32x::splat(1)).cast())
                .cast(),
            );

            x *= F32x::splat(1.5) - HALF * d * x * x;
            x *= F32x::splat(1.5) - HALF * d * x * x;
            x *= F32x::splat(1.5) - HALF * d * x * x;
            x *= d;

            let d2 = (d + x.mul_as_doubled(x)) * x.recpre_as_doubled();

            x = F32x::from(d2) * q;

            x = d.simd_eq(INFINITY).select(INFINITY, x);
            d.simd_eq(ZERO).select(d, x)
            // #endif
        }

        #[test]
        fn test_sqrtf() {
            test_f_f(sqrtf, rug::Float::sqrt, f32::MIN..=f32::MAX, 0.5);
        }

        /// 2D Euclidian distance function
        ///
        /// The error bound of the returned value is `0.5001 ULP`.
        pub fn hypotf(x: F32x, y: F32x) -> F32x {
            let x = x.abs();
            let y = y.abs();
            let min = x.simd_min(y);
            let n = min;
            let max = x.simd_max(y);
            let d = max;

            let o = max.simd_lt(F32x::splat(f32::MIN_POSITIVE));
            let n = o.select(n * F1_24X, n);
            let d = o.select(d * F1_24X, d);

            let t = Doubled::from(n) / Doubled::from(d);
            let t = (t.square() + ONE).sqrt() * max;
            let mut ret = F32x::from(t);
            ret = ret.is_nan().select(INFINITY, ret);
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
        pub fn sinpif(d: F32x) -> F32x {
            let x = sinpifk(d);
            let mut r = F32x::from(x);

            r = d.is_neg_zero().select(NEG_ZERO, r);
            r = F32x::from_bits(
                !d.abs().simd_gt(TRIGRANGEMAX4_F).to_int().cast::<u32>() & r.to_bits(),
            );
            F32x::from_bits(d.is_infinite().to_int().cast() | r.to_bits())
        }

        #[test]
        fn test_sinpif() {
            use rug::{float::Constant, Float};
            let rangemax2 = 1e+7 / 4.;
            test_f_f(
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
        pub fn cospif(d: F32x) -> F32x {
            let x = cospifk(d);
            let r = F32x::from(x);

            let r = d.abs().simd_gt(TRIGRANGEMAX4_F).select(ONE, r);
            F32x::from_bits(d.is_infinite().to_int().cast() | r.to_bits())
        }

        #[test]
        fn test_cospif() {
            use rug::{float::Constant, Float};
            let rangemax2 = 1e+7 / 4.;
            test_f_f(
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
    };
}
