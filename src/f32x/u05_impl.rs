macro_rules! impl_math_f32_u05 {
    () => {
        use super::*;

        pub fn sincospif(d: F32x) -> (F32x, F32x) {
            let u = d * F32x::splat(4.);
            let q = u.trunci();
            let q = (q + (I32x::from_bits(U32x::from_bits(q) >> 31) ^ I32x::splat(1)))
                & I32x::splat(!1);
            let s = u - F32x::from_cast(q);

            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            let u = F32x::splat(0.309_384_205_4_e-6)
                .mul_add(s, F32x::splat(-0.365_730_738_8_e-4))
                .mul_add(s, F32x::splat(0.249_039_358_5_e-2));
            let mut x = u * s
                + Doubled::from((
                    -0.080_745_510_756_969_451_904,
                    -1.337_366_533_907_693_625_8_e-9,
                ));
            x = s2 * x
                + Doubled::from((
                    0.785_398_185_253_143_310_55,
                    -2.185_733_861_756_648_485_5_e-8,
                ));

            x *= t;
            let rx = x.0 + x.1;

            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F32x::splat(-0.243_061_180_1_e-7)
                .mul_add(s, F32x::splat(0.359_057_708_e-5))
                .mul_add(s, F32x::splat(-0.325_991_772_1_e-3));
            x = u * s
                + Doubled::from((
                    0.015_854_343_771_934_509_277,
                    4.494_005_135_403_224_281_1_e-10,
                ));
            x = s2 * x
                + Doubled::from((
                    -0.308_425_128_459_930_419_92,
                    -9.072_833_903_073_392_227_7_e-9,
                ));

            x = x * s2 + ONE;
            let ry = x.0 + x.1;

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

        pub fn sqrtf(d: F32x) -> F32x {
            let d = d.lt(ZERO).select(F32x::NAN, d);

            let o = d.lt(F32x::splat(5.293_955_920_339_377_e-23));
            let d = o.select(d * F32x::splat(1.888_946_593_147_858_e+22), d);
            let q = o.select(F32x::splat(7.275_957_614_183_426_e-12 * 0.5), HALF);

            let o = d.gt(F32x::splat(1.844_674_407_370_955_2_e+19));
            let d = o.select(d * F32x::splat(5.421_010_862_427_522_e-20), d);
            let q = o.select(F32x::splat(4_294_967_296.0 * 0.5), q);

            let mut x = F32x::from_bits(
                I32x::splat(0x_5f37_5a86)
                    - I32x::from_bits(U32x::from_bits(d + F32x::splat(1e-45)) >> 1),
            );

            x *= F32x::splat(1.5) - HALF * d * x * x;
            x *= F32x::splat(1.5) - HALF * d * x * x;
            x *= F32x::splat(1.5) - HALF * d * x * x;
            x *= d;

            let d2 = (d + x.mul_as_doubled(x)) * x.recpre_as_doubled();

            x = (d2.0 + d2.1) * q;

            x = d.eq(F32x::INFINITY).select(F32x::INFINITY, x);
            d.eq(ZERO).select(d, x)
        }

        #[test]
        fn test_sqrtf() {
            test_f_f(sqrtf, rug::Float::sqrt, f32::MIN..=f32::MAX, 0.5);
        }

        pub fn hypotf(x: F32x, y: F32x) -> F32x {
            let x = x.abs();
            let y = y.abs();
            let min = x.min(y);
            let n = min;
            let max = x.max(y);
            let d = max;

            let o = max.lt(F32x::splat(f32::MIN_POSITIVE));
            let n = o.select(n * F1_24X, n);
            let d = o.select(d * F1_24X, d);

            let t = Doubled::new(n, ZERO) / Doubled::new(d, ZERO);
            let t = (t.square() + ONE).sqrt() * max;
            let mut ret = t.0 + t.1;
            ret = ret.is_nan().select(F32x::INFINITY, ret);
            ret = min.eq(ZERO).select(max, ret);
            ret = (x.is_nan() | y.is_nan()).select(F32x::NAN, ret);
            (x.eq(F32x::INFINITY) | y.eq(F32x::INFINITY)).select(F32x::INFINITY, ret)
        }

        #[test]
        fn test_hypotf() {
            test_ff_f(hypotf, rug::Float::hypot, f32::MIN..=f32::MAX, 0.5001);
        }

        pub fn sinpif(d: F32x) -> F32x {
            let x = sinpifk(d);
            let mut r = x.0 + x.1;

            r = d.is_neg_zero().select(NEG_ZERO, r);
            r = F32x::from_bits(!U32x::from_bits(d.abs().gt(TRIGRANGEMAX4_F)) & U32x::from_bits(r));
            F32x::from_bits(U32x::from_bits(d.is_infinite()) | U32x::from_bits(r))
        }

        pub fn cospif(d: F32x) -> F32x {
            let x = cospifk(d);
            let r = x.0 + x.1;

            let r = d.abs().gt(TRIGRANGEMAX4_F).select(ONE, r);
            F32x::from_bits(U32x::from_bits(d.is_infinite()) | U32x::from_bits(r))
        }
    };
}
