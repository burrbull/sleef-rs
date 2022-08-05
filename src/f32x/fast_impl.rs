macro_rules! impl_math_f32_fast {
    () => {
        use super::*;

        /// Fast sine function
        ///
        /// The error bounds of the returned value is `min(350 ULP, 2e-6)`.
        pub fn sinf(mut d: F32x) -> F32x {
            let t = d;

            let s = d * FRAC_1_PI;
            let mut u = s.round();
            let q = s.roundi();
            d = u.mul_add(-PI, d);

            let s = d * d;

            u = F32x::splat(-0.188_174_817_6_e-3)
                .mul_add(s, F32x::splat(0.832_350_272_7_e-2))
                .mul_add(s, F32x::splat(-0.166_665_136_8));
            u = (s * d).mul_add(u, d);

            u = F32x::from_bits(
                ((q & I32x::splat(1)).simd_eq(I32x::splat(1)).to_int().cast() & (-ZERO).to_bits())
                    ^ u.to_bits(),
            );

            let g = t.abs().simd_lt(F32x::splat(30.));
            if !g.all() {
                return g.select(u, super::u35::sinf(t));
            } // !!!!???????????

            u
        }

        #[test]
        fn test_sinf() {
            test_c_f_f(sinf, rug::Float::sin, -30.0..=30.0, |ulp, o, e| {
                let ulp_ex = 350.;
                (
                    ulp <= ulp_ex || (e.clone() - o).abs() <= 2e-6,
                    format!("ULP: {ulp} > {ulp_ex}"),
                )
            });
        }

        /// Fast sine function
        ///
        /// The error bounds of the returned value is `min(350 ULP, 2e-6)`.
        pub fn cosf(mut d: F32x) -> F32x {
            let t = d;

            let s = d.mul_add(FRAC_1_PI, -HALF);
            let mut u = s.round();
            let q = s.roundi();
            d = u.mul_add(-PI, d - FRAC_PI_2);

            let s = d * d;

            u = F32x::splat(-0.188_174_817_6_e-3)
                .mul_add(s, F32x::splat(0.832_350_272_7_e-2))
                .mul_add(s, F32x::splat(-0.166_665_136_8));
            u = (s * d).mul_add(u, d);

            u = F32x::from_bits(
                ((q & I32x::splat(1)).simd_eq(I32x::splat(0)).to_int().cast() & (-ZERO).to_bits())
                    ^ u.to_bits(),
            );

            let g = t.abs().simd_lt(F32x::splat(30.));
            if !g.all() {
                return g.select(u, super::u35::cosf(t));
            }

            u
        }

        #[test]
        fn test_cosf() {
            test_c_f_f(cosf, rug::Float::cos, -30.0..=30.0, |ulp, o, e| {
                let ulp_ex = 350.;
                (
                    ulp <= ulp_ex || (e.clone() - o).abs() <= 2e-6,
                    format!("ULP: {ulp} > {ulp_ex}"),
                )
            });
        }

        /// Fast power function
        ///
        /// The error bounds of the returned value is `350 ULP`.
        pub fn powf(x: F32x, y: F32x) -> F32x {
            let mut result = expk3f(logk3f(x.abs()) * y);
            let yisint = y.trunc().simd_eq(y) | y.abs().simd_gt(F1_24X);
            let yisodd = (y.trunci() & I32x::splat(1)).simd_eq(I32x::splat(1))
                & yisint
                & y.abs().simd_lt(F1_24X);

            result = (x.is_sign_negative() & yisodd).select(-result, result);

            result = x.simd_eq(ZERO).select(ZERO, result);
            y.simd_eq(ZERO).select(ONE, result)
        }

        #[test]
        fn test_powf() {
            use rug::{ops::Pow, Float};
            test_ff_f(
                powf,
                |in1, in2| Float::with_val(in1.prec(), in1.pow(in2)),
                f32::MIN..=f32::MAX,
                f32::MIN..=f32::MAX,
                350.,
            );
        }
    };
}
