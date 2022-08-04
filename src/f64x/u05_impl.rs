macro_rules! impl_math_f64_u05 {
    () => {
        use super::*;

        /// Evaluate sin( π***a*** ) and cos( π***a*** ) for given ***a*** simultaneously
        ///
        /// Evaluates the sine and cosine functions of π***a*** at a time, and store the two values in a tuple.
        /// The error bound of the returned value are `max(0.506 ULP, f64::MIN_POSITIVE)` if `[-1e+9, 1e+9]`.
        /// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
        /// If ***a*** is a `NaN` or infinity, a `NaN` is returned.
        pub fn sincospi(d: F64x) -> (F64x, F64x) {
            let u = d * F64x::splat(4.);
            let mut q = u.trunci();
            q = (q + (Ix::from_bits(Ux::from_bits(q) >> 31) ^ Ix::splat(1))) & Ix::splat(!1);
            let s = u - F64x::from_cast(q);

            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            //

            let u = F64x::splat(-2.024_611_207_851_823_992_958_68_e-14)
                .mul_add(s, F64x::splat(6.948_218_305_801_794_613_277_84_e-12))
                .mul_add(s, F64x::splat(-1.757_247_499_528_531_799_526_64_e-9))
                .mul_add(s, F64x::splat(3.133_616_889_668_683_928_784_22_e-7))
                .mul_add(s, F64x::splat(-3.657_620_418_216_155_192_036_1_e-5))
                .mul_add(s, F64x::splat(0.002_490_394_570_192_718_502_743_56));
            let mut x = u * s
                + Doubled::new(
                    F64x::splat(-0.080_745_512_188_280_785_248_473_1),
                    F64x::splat(3.618_524_750_670_371_048_499_87_e-18),
                );
            x = s2 * x
                + Doubled::new(
                    F64x::splat(0.785_398_163_397_448_278_999_491),
                    F64x::splat(3.062_871_137_271_550_026_071_05_e-17),
                );

            x *= t;
            let rx = F64x::from(x);

            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            //

            let u = F64x::splat(9.944_803_876_268_437_740_902_08_e-16)
                .mul_add(s, F64x::splat(-3.897_962_260_629_327_991_640_47_e-13))
                .mul_add(s, F64x::splat(1.150_115_825_399_960_352_669_01_e-10))
                .mul_add(s, F64x::splat(-2.461_136_950_104_469_749_535_9_e-8))
                .mul_add(s, F64x::splat(3.590_860_448_590_527_540_050_62_e-6))
                .mul_add(s, F64x::splat(-0.000_325_991_886_927_389_905_997_954));
            let mut x = u * s
                + Doubled::new(
                    F64x::splat(0.015_854_344_243_815_501_891_425_9),
                    F64x::splat(-1.046_932_722_806_315_219_088_45_e-18),
                );
            x = s2 * x
                + Doubled::new(
                    F64x::splat(-0.308_425_137_534_042_437_259_529),
                    F64x::splat(-1.956_984_921_336_335_503_383_45_e-17),
                );

            x = x * s2 + ONE;
            let ry = F64x::from(x);

            //

            let o = M64x::from_cast((q & Ix::splat(2)).eq(Ix::splat(0)));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = M64x::from_cast((q & Ix::splat(4)).eq(Ix::splat(4)));
            rsin = F64x::from_bits(
                (U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rsin),
            );

            let o = M64x::from_cast(((q + Ix::splat(2)) & Ix::splat(4)).eq(Ix::splat(4)));
            rcos = F64x::from_bits(
                (U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rcos),
            );

            let o = d.abs().gt(TRIGRANGEMAX3 / F64x::splat(4.));
            rsin = F64x::from_bits(!U64x::from_bits(o) & U64x::from_bits(rsin));
            rcos = o.select(ONE, rcos);

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
                0.506,
            );
        }

        /// Evaluate sin( π***a*** ) for given ***a***
        ///
        /// This function evaluates the sine function of π***a***.
        /// The error bound of the returned value is `max(0.506 ULP, f64::MIN_POSITIVE)`
        /// if `[-1e+9, 1e+9]` for the single-precision function.
        /// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
        /// If ***a*** is a `NaN` or infinity, a NaN is returned.
        pub fn sinpi(d: F64x) -> F64x {
            let x = sinpik(d);
            let mut r = F64x::from(x);

            r = d.is_neg_zero().select(NEG_ZERO, r);
            r = F64x::from_bits(
                !U64x::from_bits(d.abs().gt(TRIGRANGEMAX3 / F64x::splat(4.))) & U64x::from_bits(r),
            );
            F64x::from_bits(U64x::from_bits(d.is_infinite()) | U64x::from_bits(r))
        }

        #[test]
        fn test_sinpi() {
            use rug::{float::Constant, Float};
            let rangemax2 = 1e+9 / 4.;
            test_f_f(
                sinpi,
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
        /// The error bound of the returned value is `max(0.506 ULP, f64::MIN_POSITIVE)`
        /// if `[-1e+9, 1e+9]` for the single-precision function.
        /// If ***a*** is a finite value out of this range, an arbitrary value within `[-1, 1]` is returned.
        /// If ***a*** is a `NaN` or infinity, a `NaN` is returned.
        pub fn cospi(d: F64x) -> F64x {
            let x = cospik(d);
            let r = F64x::from(x);

            let r = d.abs().gt(TRIGRANGEMAX3 / F64x::splat(4.)).select(ONE, r);
            F64x::from_bits(U64x::from_bits(d.is_infinite()) | U64x::from_bits(r))
        }

        #[test]
        fn test_cospi() {
            use rug::{float::Constant, Float};
            let rangemax2 = 1e+9 / 4.;
            test_f_f(
                cospi,
                |mut in1| {
                    let prec = in1.prec();
                    in1.set_prec(prec * 2);
                    (in1 * Float::with_val(prec * 2, Constant::Pi)).cos()
                },
                -rangemax2..=rangemax2,
                0.506,
            );
        }

        /// Square root function
        ///
        /// The error bound of the returned value is `0.5001 ULP`.
        pub fn sqrt(d: F64x) -> F64x {
            let d = d.lt(ZERO).select(F64x::NAN, d);

            let o = d.lt(F64x::splat(8.636_168_555_094_445_e-78));
            let d = o.select(d * F64x::splat(1.157_920_892_373_162_e77), d);
            let q = o.select(F64x::splat(2.938_735_877_055_718_8_e-39 * 0.5), HALF);

            let o = d.gt(F64x::splat(1.340_780_792_994_259_7_e+154));
            let d = o.select(d * F64x::splat(7.458_340_731_200_207_e-155), d);
            let q = o.select(F64x::splat(1.157_920_892_373_162_e+77 * 0.5), q);

            let mut x = F64x::from_bits(
                splat2i(0x_5fe6_ec86, 0)
                    - I64x::from_bits(U64x::from_bits(d + F64x::splat(1e-320)) >> 1),
            );

            x *= F64x::splat(1.5) - HALF * d * x * x;
            x *= F64x::splat(1.5) - HALF * d * x * x;
            x *= F64x::splat(1.5) - HALF * d * x * x;
            x *= d;

            let d2 = (d + x.mul_as_doubled(x)) * x.recpre_as_doubled();

            x = F64x::from(d2) * q;

            x = d.eq(F64x::INFINITY).select(F64x::INFINITY, x);
            d.eq(ZERO).select(d, x)
        }

        #[test]
        fn test_sqrt() {
            test_f_f(sqrt, rug::Float::sqrt, f64::MIN..=f64::MAX, 0.50001);
        }

        /// 2D Euclidian distance function
        ///
        /// The error bound of the returned value is `0.5001 ULP`.
        pub fn hypot(x: F64x, y: F64x) -> F64x {
            let x = x.abs();
            let y = y.abs();
            let min = x.min(y);
            let n = min;
            let max = x.max(y);
            let d = max;

            let o = max.lt(F64x::splat(f64::MIN_POSITIVE));
            let n = o.select(n * D1_54X, n);
            let d = o.select(d * D1_54X, d);

            let t = Doubled::from(n) / Doubled::from(d);
            let t = (t.square() + ONE).sqrt() * max;
            let mut ret = F64x::from(t);
            ret = ret.is_nan().select(F64x::INFINITY, ret);
            ret = min.eq(ZERO).select(max, ret);
            ret = (x.is_nan() | y.is_nan()).select(F64x::NAN, ret);
            (x.eq(F64x::INFINITY) | y.eq(F64x::INFINITY)).select(F64x::INFINITY, ret)
        }

        #[test]
        fn test_hypot() {
            test_ff_f(
                hypot,
                rug::Float::hypot,
                f64::MIN..=f64::MAX,
                f64::MIN..=f64::MAX,
                0.5,
            );
        }
    };
}
