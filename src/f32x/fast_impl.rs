macro_rules! impl_math_f32_fast {
    () => {
        use super::*;

        pub fn sinf(mut d: F32x) -> F32x {
            let t = d;

            let s = d * F32x::FRAC_1_PI;
            let mut u = s.round();
            let q = s.roundi();
            d = u.mul_add(-F32x::PI, d);

            let s = d * d;

            u = F32x::splat(-0.188_174_817_6_e-3)
                .mul_add(s, F32x::splat(0.832_350_272_7_e-2))
                .mul_add(s, F32x::splat(-0.166_665_136_8));
            u = (s * d).mul_add(u, d);

            u = F32x::from_bits(
                (U32x::from_bits((q & I32x::splat(1)).eq(I32x::splat(1))) & U32x::from_bits(-ZERO))
                    ^ U32x::from_bits(u),
            );

            let g = t.abs().lt(F32x::splat(30.));
            if !g.all() {
                return g.select(u, super::u35::sinf(t));
            } // !!!!???????????

            u
        }

        pub fn cosf(mut d: F32x) -> F32x {
            let t = d;

            let s = d.mul_add(F32x::FRAC_1_PI, -HALF);
            let mut u = s.round();
            let q = s.roundi();
            d = u.mul_add(-F32x::PI, d - F32x::FRAC_PI_2);

            let s = d * d;

            u = F32x::splat(-0.188_174_817_6_e-3)
                .mul_add(s, F32x::splat(0.832_350_272_7_e-2))
                .mul_add(s, F32x::splat(-0.166_665_136_8));
            u = (s * d).mul_add(u, d);

            u = F32x::from_bits(
                (U32x::from_bits((q & I32x::splat(1)).eq(I32x::splat(0))) & U32x::from_bits(-ZERO))
                    ^ U32x::from_bits(u),
            );

            let g = t.abs().lt(F32x::splat(30.));
            if !g.all() {
                return g.select(u, super::u35::cosf(t));
            }

            u
        }

        pub fn powf(x: F32x, y: F32x) -> F32x {
            let mut result = expk3f(logk3f(x.abs()) * y);
            let yisint = y.trunc().eq(y) | y.abs().gt(F1_24X);
            let yisodd =
                (y.trunci() & I32x::splat(1)).eq(I32x::splat(1)) & yisint & y.abs().lt(F1_24X);

            result = (x.is_sign_negative() & yisodd).select(-result, result);

            result = x.eq(ZERO).select(ZERO, result);
            y.eq(ZERO).select(ONE, result)
        }
    };
}
