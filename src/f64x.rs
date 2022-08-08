#[macro_use]
mod u05_impl;

#[macro_use]
mod u10_impl;

#[macro_use]
mod u15_impl;

#[macro_use]
mod u35_impl;

macro_rules! impl_math_f64 {
    ($size:literal, $f64x:ident, $u64x:ident, $i64x:ident, $m64x:ident, $u32x:ident, $i32x:ident, $m32x:ident) => {
        use core::simd::{SimdFloat, SimdPartialEq, SimdPartialOrd};
        use crate::common::*;
        use doubled::*;

        pub use core::simd::$f64x;
        pub use core::simd::$u64x;
        pub use core::simd::$i64x;
        pub use core::simd::$m64x;
        pub use core::simd::$u32x;
        pub use core::simd::$i32x;
        pub use core::simd::$m32x;
        type F64x = $f64x;
        type U64x = $u64x;
        type I64x = $i64x;
        type M64x = $m64x;
        type Ux = $u32x;
        type Ix = $i32x;

        impl MaskType for F64x {
            type Mask = M64x;
        }

        impl BitsType for F64x {
            type Bits = U64x;
        }

        impl MaskType for Doubled<F64x> {
            type Mask = M64x;
        }

        impl crate::Sleef for F64x {
            type Int = Ix;
            #[inline]
            fn sin(self) -> Self {
                u35::sin(self)
            }
            #[inline]
            fn cos(self) -> Self {
                u35::cos(self)
            }
            #[inline]
            fn sin_cos(self) -> (Self, Self) {
                u35::sincos(self)
            }
            #[inline]
            fn tan(self) -> Self {
                u35::tan(self)
            }
            #[inline]
            fn asin(self) -> Self {
                u35::asin(self)
            }
            #[inline]
            fn acos(self) -> Self {
                u35::acos(self)
            }
            #[inline]
            fn atan(self) -> Self {
                u35::atan(self)
            }
            #[inline]
            fn atan2(self, other: Self) -> Self {
                u35::atan2(self, other)
            }
            #[inline]
            fn ln(self) -> Self {
                u35::log(self)
            }
            #[inline]
            fn cbrt(self) -> Self {
                u35::cbrt(self)
            }
            #[inline]
            fn exp(self) -> Self {
                u10::exp(self)
            }
            #[inline]
            fn pow(self, other: Self) -> Self {
                u10::pow(self, other)
            }
            #[inline]
            fn sinh(self) -> Self {
                u10::sinh(self)
            }
            #[inline]
            fn cosh(self) -> Self {
                u10::cosh(self)
            }
            #[inline]
            fn tanh(self) -> Self {
                u10::tanh(self)
            }
            #[inline]
            fn asinh(self) -> Self {
                u10::asinh(self)
            }
            #[inline]
            fn acosh(self) -> Self {
                u10::acosh(self)
            }
            #[inline]
            fn atanh(self) -> Self {
                u10::atanh(self)
            }
            #[inline]
            fn exp2(self) -> Self {
                u10::exp2(self)
            }
            #[inline]
            fn exp10(self) -> Self {
                u10::exp10(self)
            }
            #[inline]
            fn exp_m1(self) -> Self {
                u10::expm1(self)
            }
            #[inline]
            fn log10(self) -> Self {
                u10::log10(self)
            }
            #[inline]
            fn log2(self) -> Self {
                u10::log2(self)
            }
            #[inline]
            fn log_1p(self) -> Self {
                u10::log1p(self)
            }
            #[inline]
            fn ldexp(self, other: Self::Int) -> Self {
                ldexp(self, other)
            }
            #[inline]
            fn ilogb(self) -> Self::Int {
                ilogb(self)
            }
            #[inline]
            fn fma(self, y: Self, z: Self) -> Self {
                fma(self, y, z)
            }
            #[inline]
            fn sqrt(self) -> Self {
                sqrt(self)
            }
            #[inline]
            fn abs(self) -> Self {
                fabs(self)
            }
            #[inline]
            fn copy_sign(self, other: Self) -> Self {
                copysign(self, other)
            }
            #[inline]
            fn max(self, other: Self) -> Self {
                fmax(self, other)
            }
            #[inline]
            fn min(self, other: Self) -> Self {
                fmin(self, other)
            }
            #[inline]
            fn fdim(self, other: Self) -> Self {
                fdim(self, other)
            }
            #[inline]
            fn truncate(self) -> Self {
                trunc(self)
            }
            #[inline]
            fn floor(self) -> Self {
                floor(self)
            }
            #[inline]
            fn ceil(self) -> Self {
                ceil(self)
            }
            #[inline]
            fn round(self) -> Self {
                rint(self)
            }
            #[inline]
            fn next_after(self, other: Self) -> Self {
                nextafter(self, other)
            }
            #[inline]
            fn frfrexp(self) -> Self {
                frfrexp(self)
            }
            #[inline]
            fn expfrexp(self) -> Self::Int {
                expfrexp(self)
            }
            #[inline]
            fn fmod(self, other: Self) -> Self {
                fmod(self, other)
            }
            #[inline]
            fn remainder(self, other: Self) -> Self {
                remainder(self, other)
            }
            #[inline]
            fn modf(self) -> (Self, Self) {
                modf(self)
            }
            #[inline]
            fn sin_cos_pi(self) -> (Self, Self) {
                u35::sincospi(self)
            }
            #[inline]
            fn sin_pi(self) -> Self {
                u05::sinpi(self)
            }
            #[inline]
            fn cos_pi(self) -> Self {
                u05::cospi(self)
            }
            #[inline]
            fn hypot(self, other: Self) -> Self {
                u35::hypot(self, other)
            }
            #[inline]
            fn gamma(self) -> Self {
                u10::tgamma(self)
            }
            #[inline]
            fn lgamma(self) -> Self {
                u10::lgamma(self)
            }
            #[inline]
            fn erf(self) -> Self {
                u10::erf(self)
            }
            #[inline]
            fn erfc(self) -> Self {
                u15::erfc(self)
            }
        }

        const fn splat(value: f64) -> F64x {
            F64x::from_array([value; F64x::LANES])
        }

        const PI: F64x = splat(core::f64::consts::PI);
        const FRAC_1_PI: F64x = splat(core::f64::consts::FRAC_1_PI);
        const FRAC_2_PI: F64x = splat(core::f64::consts::FRAC_2_PI);
        const FRAC_PI_2: F64x = splat(core::f64::consts::FRAC_PI_2);
        const FRAC_PI_4: F64x = splat(core::f64::consts::FRAC_PI_4);
        const NAN: F64x = splat(f64::NAN);
        const INFINITY: F64x = splat(f64::INFINITY);
        const NEG_INFINITY: F64x = splat(f64::NEG_INFINITY);

        const ZERO: F64x = splat(0.);
        const NEG_ZERO: F64x = splat(-0.);
        const ONE: F64x = splat(1.);
        const HALF: F64x = splat(0.5);
        const D1_63X: F64x = splat(crate::f64::D1_63);
        const D1_60X: F64x = splat(crate::f64::D1_60);
        const D1_54X: F64x = splat(crate::f64::D1_54);
        const D1_53X: F64x = splat(crate::f64::D1_53);
        const D1_52X: F64x = splat(crate::f64::D1_52);
        const D1_32X: F64x = splat(crate::f64::D1_32);
        const D1_31X: F64x = splat(crate::f64::D1_31);
        const D1_28X: F64x = splat(crate::f64::D1_28);
        const D1_24X: F64x = splat(crate::f64::D1_24);
        const D1_23X: F64x = splat(crate::f64::D1_23);

        const PI_A: F64x = splat(crate::f64::PI_A);
        const PI_B: F64x = splat(crate::f64::PI_B);
        const PI_C: F64x = splat(crate::f64::PI_C);
        const PI_D: F64x = splat(crate::f64::PI_D);
        const TRIGRANGEMAX: F64x = splat(crate::f64::TRIGRANGEMAX);

        const PI_A2: F64x = splat(crate::f64::PI_A2);
        const PI_B2: F64x = splat(crate::f64::PI_B2);
        const TRIGRANGEMAX2: F64x = splat(crate::f64::TRIGRANGEMAX2);

        const SLEEF_FP_ILOGB0: F64x = splat(crate::f64::SLEEF_FP_ILOGB0 as f64);
        const SLEEF_FP_ILOGBNAN: F64x = splat(crate::f64::SLEEF_FP_ILOGBNAN as f64);
        const SQRT_DBL_MAX: F64x = splat(crate::f64::SQRT_DBL_MAX);
        const M_2_PI_H: F64x = splat(crate::f64::M_2_PI_H);
        const M_2_PI_L: F64x = splat(crate::f64::M_2_PI_L);
        const TRIGRANGEMAX3: F64x = splat(crate::f64::TRIGRANGEMAX3);
        const L2_U: F64x = splat(crate::f64::L2_U);
        const L2_L: F64x = splat(crate::f64::L2_L);
        const R_LN2: F64x = splat(crate::f64::R_LN2);
        const L10_U: F64x = splat(crate::f64::L10_U);
        const L10_L: F64x = splat(crate::f64::L10_L);
        const LOG10_2: F64x = splat(crate::f64::LOG10_2);

        mod u05 {
            //! Functions with 0.5 ULP error bound
            impl_math_f64_u05!();
        }
        pub use u05::{
            sincospi as sincospi_u05,
            sqrt as sqrt_u05,
            hypot as hypot_u05,
            sinpi as sinpi_u05,
            cospi as cospi_u05,
        };

        mod u10 {
            //! Functions with 1.0 ULP error bound
            impl_math_f64_u10!();
        }
        pub use u10::{
            sin as sin_u10,
            sin_deterministic as sin_u10_deterministic,
            cos as cos_u10,
            cos_deterministic as cos_u10_deterministic,
            sincos as sincos_u10,
            sincos_deterministic as sincos_u10_deterministic,
            tan as tan_u10,
            tan_deterministic as tan_u10_deterministic,
            atan2 as atan2_u10,
            asin as asin_u10,
            acos as acos_u10,
            atan as atan_u10,
            exp as exp_u10,
            cbrt as cbrt_u10,
            log as log_u10,
            pow as pow_u10,
            sinh as sinh_u10,
            cosh as cosh_u10,
            tanh as tanh_u10,
            asinh as asinh_u10,
            acosh as acosh_u10,
            atanh as atanh_u10,
            exp10 as exp10_u10,
            expm1 as expm1_u10,
            log10 as log10_u10,
            log2 as log2_u10,
            tgamma as tgamma_u10,
            lgamma as lgamma_u10,
            erf as erf_u10,
            log1p as log1p_u10,
            exp2 as exp2_u10,
        };

        mod u15 {
            //! Functions with 1.5 ULP error bound
            impl_math_f64_u15!();
        }
        pub use u15::{
            erfc as erfc_u15,
        };

        mod u35 {
            //! Functions with 3.5 ULP error bound
            impl_math_f64_u35!();
        }
        pub use u35::{
            sin as sin_u35,
            sin_deterministic as sin_u35_deterministic,
            cos as cos_u35,
            cos_deterministic as cos_u35_deterministic,
            sincos as sincos_u35,
            sincos_deterministic as sincos_u35_deterministic,
            tan as tan_u35,
            tan_deterministic as tan_u35_deterministic,
            sincospi as sincospi_u35,
            atan as atan_u35,
            atan2 as atan2_u35,
            asin as asin_u35,
            acos as acos_u35,
            log as log_u35,
            sqrt as sqrt_u35,
            cbrt as cbrt_u35,
            sinh as sinh_u35,
            cosh as cosh_u35,
            tanh as tanh_u35,
            hypot as hypot_u35,
            exp2 as exp2_u35,
            exp10 as exp10_u35,
            log2 as log2_u35,
        };

        #[cfg(test)]
        fn gen_input(rng: &mut rand::rngs::ThreadRng, range: core::ops::RangeInclusive<f64>) -> F64x {
            let mut arr = [0.; $size];
            for i in 0..$size {
                arr[i] = crate::f64::gen_input(rng, range.clone());
            }
            arr.into()
        }

        #[cfg(test)]
        fn test_f_f(fun_fx: fn(F64x) -> F64x, fun_f: fn(rug::Float) -> rug::Float, range: core::ops::RangeInclusive<f64>, ulp_ex: f64) {
            let mut rng = rand::thread_rng();
            for n in 0..crate::TEST_REPEAT_FAST {
                let in_fx = gen_input(&mut rng, range.clone());
                let out_fx = fun_fx(in_fx);
                for i in 0..$size {
                    let input = in_fx[i];
                    let output = out_fx[i];
                    let expected = fun_f(rug::Float::with_val(crate::f64::PRECF64, input));
                    if expected.is_nan() && output.is_nan() {
                        continue;
                    }
                    let ulp = crate::f64::count_ulp(output, &expected);
                    assert!(
                        ulp <= ulp_ex,
                        "Iteration: {n}, Position: {i}, Input: {input:e}, Output: {output}, Expected: {expected}, ULP: {ulp} > {}",
                        ulp_ex
                    );
                }
            }
        }

        #[cfg(test)]
        fn test_f_ff(
            fun_fx: fn(F64x) -> (F64x, F64x),
            fun_f: fn(rug::Float) -> (rug::Float, rug::Float),
            range: core::ops::RangeInclusive<f64>,
            ulp_ex: f64,
        ) {
            let mut rng = rand::thread_rng();
            for n in 0..crate::TEST_REPEAT_FAST {
                let in_fx = gen_input(&mut rng, range.clone());
                let (out_fx1, out_fx2) = fun_fx(in_fx);
                for i in 0..$size {
                    let input = in_fx[i];
                    let output1 = out_fx1[i];
                    let output2 = out_fx2[i];
                    let (expected1, expected2) = fun_f(rug::Float::with_val(crate::f64::PRECF64, input));
                    if (expected1.is_nan() && output1.is_nan()) || (expected2.is_nan() && output2.is_nan())
                    {
                        continue;
                    }
                    let ulp1 = crate::f64::count_ulp(output1, &expected1);
                    let ulp2 = crate::f64::count_ulp(output2, &expected2);
                    assert!(
                        ulp1 <= ulp_ex && ulp2 <= ulp_ex,
                            "Iteration: {n}, Position: {i}, Input: {input:e}, Output: ({output1}, {output2}), Expected: ({expected1}, {expected2}), ULP: ({ulp1}, {ulp2}) > {}",
                            ulp_ex
                    );
                }
            }
        }

        #[cfg(test)]
        fn test_ff_f(
            fun_fx: fn(F64x, F64x) -> F64x,
            fun_f: fn(rug::Float, &rug::Float) -> rug::Float,
            range1: core::ops::RangeInclusive<f64>,
            range2: core::ops::RangeInclusive<f64>,
            ulp_ex: f64,
        ) {
            let mut rng = rand::thread_rng();
            for n in 0..crate::TEST_REPEAT_FAST {
                let in_fx1 = gen_input(&mut rng, range1.clone());
                let in_fx2 = gen_input(&mut rng, range2.clone());
                let out_fx = fun_fx(in_fx1, in_fx2);
                for i in 0..$size {
                    let input1 = in_fx1[i];
                    let input2 = in_fx2[i];
                    let output = out_fx[i];
                    let expected = fun_f(
                        rug::Float::with_val(crate::f64::PRECF64, input1),
                        &rug::Float::with_val(crate::f64::PRECF64, input2),
                    );
                    if expected.is_nan() && output.is_nan() {
                        continue;
                    }
                    let ulp = crate::f64::count_ulp(output, &expected);
                    assert!(
                        ulp <= ulp_ex,
                        "Iteration: {n}, Position: {i}, Input: ({input1:e}, {input2:e}), Output: {output}, Expected: {expected}, ULP: {ulp} > {}",
                        ulp_ex
                    );
                }
            }
        }

        #[inline]
        fn from_slice_offset(ptr: &[f64], vi: Ix) -> F64x {
            //F64x::gather_or_default(ptr, vi.cast())
            let ar: [f64; $size] = core::array::from_fn(|i| ptr[vi[i] as usize]);
            F64x::from_array(ar)
        }

        #[inline]
        fn swap_upper_lower(i: I64x) -> I64x {
    //        i.rotate_left(I64x::splat(32))
            let mut ar = i.to_array();
            for v in &mut ar {
                *v = v.rotate_left(32);
            }
            I64x::from_array(ar)
        }

        impl Round for F64x {
            type Int = Ix;
            #[inline]
            fn trunc(self) -> Self {
                self.trunci().cast()
            }
            #[inline]
            fn trunci(self) -> Self::Int {
                self.cast()
            }
            #[inline]
            fn round(self) -> Self {
                rint(self)
            }
            #[inline]
            fn roundi(self) -> Self::Int {
                self.round().cast()
            }
        }

        impl MulAdd for F64x {
            #[inline]
            fn mul_add(self, y: Self, z: Self) -> Self {
                if cfg!(target_feature = "fma") {
                    use std::simd::{StdFloat};
                    <Self as StdFloat>::mul_add(self, y, z)
                } else {
                    self * y + z
                }
            }
        }

        impl MulSub for F64x {
            #[inline]
            fn mul_sub(self, y: Self, z: Self) -> Self {
                if cfg!(target_feature = "fma") {
                    use std::simd::{StdFloat};
                    <Self as StdFloat>::mul_add(self, y, -z)
                } else {
                    self * y - z
                }
            }
        }

        impl NegMulAdd for F64x {
            #[inline]
            fn neg_mul_add(self, y: Self, z: Self) -> Self {
                if cfg!(target_feature = "fma") {
                    use std::simd::{StdFloat};
                    <Self as StdFloat>::mul_add(-self, y, z)
                } else {
                    -self * y + z
                }
            }
        }

        impl Sqrt for F64x {
            #[inline]
            fn sqrt(self) -> Self {
                use std::simd::{StdFloat};
                <Self as StdFloat>::sqrt(self)
            }
        }

        impl SqrtAsDoubled for F64x {
            #[inline]
            fn sqrt_as_doubled(self) -> Doubled<Self> {
                let t = self.sqrt();
                ((self + t.mul_as_doubled(t)) * t.recip_as_doubled()).scale(Self::splat(0.5))
            }
        }

        impl VectorizedSelect<f64> for M64x {
            type Output = F64x;
            fn select_splat(self, l: f64, r: f64) -> Self::Output {
                self.select(Self::Output::splat(l), Self::Output::splat(r))
            }
        }
        impl DoubledSelect<F64x> for M64x {
            fn select_doubled(self, l: Doubled<F64x>, r: Doubled<F64x>) -> Doubled<F64x> {
                Doubled::new(self.select(l.0, r.0), self.select(l.1, r.1))
            }
        }

        impl SelectSeveral<f64> for F64x {
            #[inline]
            fn select3(o0: Self::Mask, o1: Self::Mask, d0: f64, d1: f64, d2: f64) -> Self {
                o0.select(Self::splat(d0), o1.select_splat(d1, d2))
            }
            fn select4(
                o0: Self::Mask,
                o1: Self::Mask,
                o2: Self::Mask,
                d0: f64,
                d1: f64,
                d2: f64,
                d3: f64,
            ) -> Self {
                o0.select(
                    Self::splat(d0),
                    o1.select(Self::splat(d1), o2.select_splat(d2, d3)),
                )
            }
        }

        impl Poly<f64> for F64x {
            fn c2v(c: f64) -> Self {
                F64x::splat(c)
            }
        }

        impl Poly<Self> for F64x {
            fn c2v(c: Self) -> Self {
                c
            }
        }

        impl Sign for F64x {
            #[inline]
            fn sign_bit(self) -> Self::Bits {
                self.to_bits() & NEG_ZERO.to_bits()
            }
            #[inline]
            fn sign(self) -> Self {
                ONE.mul_sign(self)
            }
            #[inline]
            fn mul_sign(self, other: Self) -> Self {
                Self::from_bits(self.to_bits() ^ other.sign_bit())
            }
            #[inline]
            fn or_sign(self, other: Self) -> Self {
                Self::from_bits(self.to_bits() | other.sign_bit())
            }
            #[inline]
            fn copy_sign(self, other: Self) -> Self {
                Self::from_bits((!NEG_ZERO.to_bits() & self.to_bits()) ^ other.sign_bit())
            }
        }

        impl IsNegZero for F64x {
            #[inline]
            fn is_neg_zero(self) -> Self::Mask {
                self.to_bits().simd_eq(NEG_ZERO.to_bits())
            }
        }

        impl IsInt for F64x {
            #[inline]
            fn is_integer(self) -> Self::Mask {
                if cfg!(feature = "full_fp_rounding") {
                    self.trunc().simd_eq(self)
                } else {
                    let mut x = (self * (ONE / D1_31X)).trunc();
                    x = (-D1_31X).mul_add(x, self);
                    x.trunc().simd_eq(x) | self.abs().simd_gt(D1_53X)
                }
            }
        }

        #[inline]
        fn cast_into_upper(q: Ix) -> I64x {
            let q64: I64x = q.cast();
            q64 << I64x::splat(32)
        }

        #[inline]
        fn cast_from_upper(q: U64x) -> Ix {
            (q >> U64x::splat(32)).cast()
        }

        #[inline]
        fn pow2i(q: Ix) -> F64x {
            let q = Ix::splat(0x3ff) + q;
            let r = cast_into_upper(q);
            F64x::from_bits((r << I64x::splat(20)).cast())
        }
        #[inline]
        fn ldexp2k(d: F64x, e: Ix) -> F64x {
            let e1 = e >> Ix::splat(1);
            d * pow2i(e1) * pow2i(e - (e1))
        }
        #[inline]
        fn ldexp3k(d: F64x, q: Ix) -> F64x {
            F64x::from_bits((d.to_bits().cast() + (cast_into_upper(q) << I64x::splat(20))).cast())
        }

        /*#[cfg(all(
            not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn ilogbk(mut d: F64x) -> Ix {
            let o = d.simd_lt(F64x::splat(4.909_093_465_297_726_6_e-91));
            d = o.select(F64x::splat(2.037_035_976_334_486_e90) * d, d);
            let mut q = cast_from_upper(d.to_bits());
            q &= Ix::splat((((1u32 << 12) - 1) << 20) as _);
            q = (q.cast() >> Ux::splat(20)).cast();
            q - o.cast().select(Ix::splat(300 + 0x3ff), Ix::splat(0x3ff))
        }
        /*#[cfg(all(
            not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn ilogb2k(d: F64x) -> Ix {
            let mut q = cast_from_upper(d.to_bits());
            q = (q.cast() >> Ux::splat(20)).cast();
            q &= Ix::splat(0x7ff);
            q - Ix::splat(0x3ff)
        }

        impl IsOdd for F64x {
            #[inline]
            fn is_odd(self) -> Self::Mask {
                if cfg!(feature = "full_fp_rounding") {
                    let x = self * HALF;
                    x.trunc().simd_ne(x)
                } else {
                    let mut x = (self * (ONE / D1_31X)).trunc();
                    x = (-D1_31X).mul_add(x, self);

                    (x.trunci() & Ix::splat(1)).simd_eq(Ix::splat(1)).cast::<i64>() & self.abs().simd_lt(D1_53X)
                }
            }
        }

        #[inline]
        fn ldexpk(x: F64x, q: Ix) -> F64x {
            let mut m = q >> Ix::splat(31);
            m = (((m + q) >> Ix::splat(9)) - m) << Ix::splat(7);
            let q = q - (m << Ix::splat(2));
            m = Ix::splat(0x3ff) + m;
            m = !Ix::splat(0).simd_gt(m).to_int() & m;
            m = m.simd_gt(Ix::splat(0x7ff)).select(Ix::splat(0x7ff), m);
            let r = cast_into_upper(m);
            let y = F64x::from_bits((r << I64x::splat(20)).cast());
            x * y * y * y * y * pow2i(q)
        }

        /// Multiply by integral power of `2`
        ///
        /// These functions return the result of multiplying ***m*** by `2` raised to the power ***x***.
        pub fn ldexp(x: F64x, q: Ix) -> F64x {
            ldexpk(x, q)
        }

        /// Integer exponent of an FP number
        pub fn ilogb(d: F64x) -> Ix {
            let mut e: F64x = ilogbk(d.abs()).cast();
            e = d.simd_eq(ZERO).select(SLEEF_FP_ILOGB0, e);
            e = d.is_nan().select(SLEEF_FP_ILOGBNAN, e);
            e = d.is_infinite().select(F64x::splat(f64::MAX), e);
            e.roundi()
        }

        #[inline]
        fn rempisub(x: F64x) -> (F64x, Ix) {
            if cfg!(feature = "full_fp_rounding") {
                let y = (x * F64x::splat(4.)).round();
                let vi = (y - x.round() * F64x::splat(4.)).trunci();
                (x - y * F64x::splat(0.25), vi)
            } else {
                let c = D1_52X.mul_sign(x);
                let rint4x = (F64x::splat(4.) * x).abs().simd_gt(D1_52X).select(
                    (F64x::splat(4.) * x),
                    (F64x::splat(4.).mul_add(x, c) - c).or_sign(x)
                );
                let rintx  = x.abs().simd_gt(D1_52X).select(x, ((x + c) - c).or_sign(x));

                let fr = F64x::splat(-0.25).mul_add(rint4x, x);
                let vi = F64x::splat(-4.).mul_add(rintx, rint4x).trunci();
                (fr, vi)
            }
        }
        #[inline]
        fn rempi(mut a: F64x) -> (Doubled<F64x>, Ix) {
            let mut ex = ilogb2k(a);
            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                ex = !(ex >> 31) & ex;
                ex = ex & Ix::splat(1023);
            }*/
            ex -= Ix::splat(55);
            let mut q = ex.simd_gt(Ix::splat(700 - 55)).to_int() & Ix::splat(-64);
            a = ldexp3k(a, q);
            ex = !(ex >> Ix::splat(31)) & ex;
            ex <<= Ix::splat(2);
            let mut x = a.mul_as_doubled(from_slice_offset(&crate::tables::REMPITABDP, ex));
            let (did, dii) = rempisub(x.0);
            q = dii;
            x.0 = did;
            x = x.normalize();
            let mut y = a.mul_as_doubled(from_slice_offset(&crate::tables::REMPITABDP[1..], ex));
            x += y;
            let (did, dii) = rempisub(x.0);
            q += dii;
            x.0 = did;
            x = x.normalize();
            y = Doubled::new(
                from_slice_offset(&crate::tables::REMPITABDP[2..], ex),
                from_slice_offset(&crate::tables::REMPITABDP[3..], ex),
            );
            y *= a;
            x += y;
            x = x.normalize();
            x *= Doubled::<F64x>::splat(Doubled::new(
                crate::f64::D_PI.0 * 2.,
                crate::f64::D_PI.1 * 2.
            ));
            let o = a.abs().simd_lt(F64x::splat(0.7));
            x = Doubled::new(
                o.select(a, x.0),
                F64x::from_bits(!o.to_int().cast::<u64>() & x.1.to_bits())
            );
            (x, q)
        }

        #[inline]
        fn visinf2_vd_vd_vd(d: F64x, m: F64x) -> F64x {
            F64x::from_bits(
                d.is_infinite().to_int().cast()
                    & ((d.to_bits() & NEG_ZERO.to_bits()) | m.to_bits()),
            )
        }

        #[inline]
        fn expk2(d: Doubled<F64x>) -> Doubled<F64x> {
            let u = F64x::from(d) * R_LN2;
            let dq = u.round();
            let q = dq.roundi();

            let s = d + dq * (-L2_U) + dq * (-L2_L);

            let s2 = s.square();
            let s4 = s2.square();
            let s8 = s4.0 * s4.0;

            let u = F64x::poly10(
                s.0,
                s2.0,
                s4.0,
                s8,
                0.160_247_221_970_993_207_2_e-9,
                0.209_225_518_356_315_700_7_e-8,
                0.250_523_002_378_264_446_5_e-7,
                0.275_572_480_090_213_530_3_e-6,
                0.275_573_189_238_604_437_3_e-5,
                0.248_015_873_560_581_506_5_e-4,
                0.198_412_698_414_807_185_8_e-3,
                0.138_888_888_888_676_325_5_e-2,
                0.833_333_333_333_334_709_5_e-2,
                0.416_666_666_666_666_990_5_e-1,
            );

            let mut t = HALF.add_checked(s * F64x::splat(0.166_666_666_666_666_657_4));
            t = ONE.add_checked(t * s);
            t = ONE.add_checked(t * s);
            t = t.add_checked(s4 * u);

            t = Doubled::new(
                ldexp2k(t.0, q),
                ldexp2k(t.1, q)
            );

            t = Doubled::new(
                F64x::from_bits(!d.0.simd_lt(F64x::splat(-1000.)).to_int().cast::<u64>() & t.0.to_bits()),
                F64x::from_bits(!d.0.simd_lt(F64x::splat(-1000.)).to_int().cast::<u64>() & t.1.to_bits())
            );
            t
        }

        #[inline]
        fn splat2i(i0: i32, i1: i32) -> I64x {
            I64x::splat(((i0 as i64) << 32) + (i1 as i64))
        }

        #[inline]
        fn splat2u(u0: u32, u1: u32) -> I64x {
            I64x::splat((((u0 as u64) << 32) + (u1 as u64)) as i64)
        }

        #[inline]
        fn splat2uu(u0: u32, u1: u32) -> U64x {
            U64x::splat(((u0 as u64) << 32) + (u1 as u64))
        }

        /// Absolute value
        #[inline]
        pub fn fabs(x: F64x) -> F64x {
            x.abs()
        }

        /// Copy sign of a number
        #[inline]
        pub fn copysign(x: F64x, y: F64x) -> F64x {
            x.copy_sign(y)
        }

        /// Maximum of two numbers
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))] //  && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
        pub fn fmax(x: F64x, y: F64x) -> F64x {
            y.is_nan().select(x, x.simd_max(y))
        }

        /// Maximum of two numbers
        #[cfg(all(not(target_arch = "x86"), not(target_arch = "x86_64")))]
        pub fn fmax(x: F64x, y: F64x) -> F64x {
            y.is_nan().select(x, x.simd_gt(y).select(x, y))
        }

        /// Minimum of two numbers
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))] //  && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
        pub fn fmin(x: F64x, y: F64x) -> F64x {
            y.is_nan().select(x, x.simd_min(y))
        }

        /// Minimum of two numbers
        #[cfg(all(not(target_arch = "x86"), not(target_arch = "x86_64")))]
        pub fn fmin(x: F64x, y: F64x) -> F64x {
            y.is_nan().select(x, y.simd_gt(x).select(x, y))
        }

        /// Positive difference
        pub fn fdim(x: F64x, y: F64x) -> F64x {
            let ret = x - y;
            (ret.simd_lt(ZERO) | x.simd_eq(y)).select(ZERO, ret)
        }

        /// Round to integer towards zero
        pub fn trunc(x: F64x) -> F64x {
            /*
            #ifdef FULL_FP_ROUNDING
            return vtruncate_vd_vd(x);
            #else
            */
            let mut fr = x - D1_31X * (x * (ONE / D1_31X)).trunci().cast();
            fr -= fr.trunci().cast();
            (x.is_infinite() | x.abs().simd_ge(D1_52X)).select(x, (x - fr).copy_sign(x))
        // #endif
        }

        /// Round to integer towards minus infinity
        pub fn floor(x: F64x) -> F64x {
            let mut fr = x - D1_31X * (x * (ONE / D1_31X)).trunci().cast();
            fr -= fr.trunci().cast();
            fr = fr.simd_lt(ZERO).select(fr + ONE, fr);
            (x.is_infinite() | x.abs().simd_ge(D1_52X)).select(x, (x - fr).copy_sign(x))
        }

        /// Round to integer towards plus infinity
        pub fn ceil(x: F64x) -> F64x {
            let mut fr = x - D1_31X * (x * (ONE / D1_31X)).trunci().cast();
            fr -= fr.trunci().cast();
            fr = fr.simd_le(ZERO).select(fr, fr - ONE);
            (x.is_infinite() | x.abs().simd_ge(D1_52X)).select(x, (x - fr).copy_sign(x))
        }

        /// Round to integer away from zero
        pub fn round(d: F64x) -> F64x {
            let mut x = d + HALF;
            let mut fr = x - D1_31X * (x * (ONE / D1_31X)).trunci().cast();
            fr -= fr.trunci().cast();
            x = (x.simd_le(ZERO) & fr.simd_eq(ZERO)).select(x - ONE, x);
            fr = fr.simd_lt(ZERO).select(fr + ONE, fr);
            x = d
                .simd_eq(F64x::splat(0.499_999_999_999_999_944_49))
                .select(ZERO, x);
            (d.is_infinite() | d.abs().simd_ge(D1_52X)).select(d, (x - fr).copy_sign(d))
        }

        /// Round to integer, ties round to even
        pub fn rint(d: F64x) -> F64x {
            /*
            #ifdef FULL_FP_ROUNDING
            return vrint_vd_vd(d);
            #else
            */
            let c = D1_52X.mul_sign(d);
            d.abs().simd_gt(D1_52X).select(d, ((d + c) - c).or_sign(d))
            //#endif
        }

        /// Find the next representable FP value
        pub fn nextafter(x: F64x, y: F64x) -> F64x {
            let x = x.simd_eq(ZERO).select(ZERO.mul_sign(y), x);
            let mut xi2: I64x = x.to_bits().cast();
            let c = x.is_sign_negative() ^ y.simd_ge(x);

            let mut t = (xi2 ^ splat2u(0x_7fff_ffff, 0x_ffff_ffff)) + splat2i(0, 1);
            t += swap_upper_lower(splat2i(0, 1) & t.simd_eq(splat2i(-1, 0)).to_int());
            xi2 = c.select(F64x::from_bits(t.cast()), F64x::from_bits(xi2.cast())).to_bits().cast();

            xi2 -= (x.simd_ne(y).to_int().cast() & splat2uu(0, 1)).cast();

            xi2 = x.simd_ne(y).select(
                F64x::from_bits((
                    xi2 + swap_upper_lower(splat2i(0, -1) & xi2.simd_eq(splat2i(0, -1)).to_int())
                ).cast()),
                F64x::from_bits(xi2.cast()),
            ).to_bits().cast();

            let mut t = (xi2 ^ splat2u(0x_7fff_ffff, 0x_ffff_ffff)) + splat2i(0, 1);
            t += swap_upper_lower(splat2i(0, 1) & t.simd_eq(splat2i(-1, 0)).to_int());
            xi2 = c.select(F64x::from_bits(t.cast()), F64x::from_bits(xi2.cast())).to_bits().cast();

            let mut ret = F64x::from_bits(xi2.cast());

            ret = (ret.simd_eq(ZERO) & x.simd_ne(ZERO)).select(ZERO.mul_sign(x), ret);

            ret = (x.simd_eq(ZERO) & y.simd_eq(ZERO)).select(y, ret);

            (x.is_nan() | y.is_nan()).select(NAN, ret)
        }

        #[test]
        fn test_nextafter() {
            test_ff_f(
                nextafter,
                |mut f, t| {
                    let prec = f.prec();
                    f.set_prec(53);
                    f.next_toward(&t);
                    f.set_prec(prec);
                    f
                },
                f64::MIN..=f64::MAX,
                f64::MIN..=f64::MAX,
                0.1,
            );
        }

        /// Fractional component of an FP number
        pub fn frfrexp(x: F64x) -> F64x {
            let x = x
                .abs()
                .simd_lt(F64x::splat(f64::MIN_POSITIVE))
                .select(x * D1_63X, x);

            let mut xm = x.to_bits();
            xm &= splat2uu(!0x_7ff0_0000, !0);
            xm |= splat2uu(0x_3fe0_0000, 0);

            let ret = F64x::from_bits(xm);

            let ret = x.is_infinite().select(INFINITY.mul_sign(x), ret);
            x.simd_eq(ZERO).select(x, ret)
        }

        /// Exponent of an FP number
        pub fn expfrexp(x: F64x) -> Ix {
            let x = x
                .abs()
                .simd_lt(F64x::splat(f64::MIN_POSITIVE))
                .select(x * D1_63X, x);

            let mut ret = cast_from_upper(x.to_bits());
            ret = ((ret.cast() >> Ux::splat(20)).cast() & Ix::splat(0x7ff)) - Ix::splat(0x3fe);

            (x.simd_eq(ZERO) | x.is_nan() | x.is_infinite()).cast().select(Ix::splat(0), ret)
        }

        /// Fused multiply and accumulate
        ///
        /// This function compute (***x*** × ***y*** + ***z***) without rounding, and then return the rounded value of the result.
        /// This function may return infinity with a correct sign if the absolute value of the correct return value is greater than `1e+300`.
        /// The error bounds of the returned value is `max(0.500_01 ULP, f64::MIN_POSITIVE)`.
        pub fn fma(mut x: F64x, mut y: F64x, mut z: F64x) -> F64x {
            /*
            #ifdef ENABLE_FMA_DP
            return vfma_vd_vd_vd_vd(x, y, z);
            #else
            */
            let mut h2 = x * y + z;
            let mut q = ONE;
            const C0: F64x = D1_54X;
            let c1: F64x = C0 * C0;
            let c2: F64x = c1 * c1;
            let o = h2.abs().simd_lt(F64x::splat(1e-300));
            {
                x = o.select(x * c1, x);
                y = o.select(y * c1, y);
                z = o.select(z * c2, z);
                q = o.select(ONE / c2, q);
            }
            let o = h2.abs().simd_gt(F64x::splat(1e+300));
            {
                x = o.select(x * (ONE / c1), x);
                y = o.select(y * (ONE / c1), y);
                z = o.select(z * (ONE / c2), z);
                q = o.select(c2, q);
            }
            let d = x.mul_as_doubled(y) + z;
            let ret = (x.simd_eq(ZERO) | y.simd_eq(ZERO)).select(z, d.0 + d.1);
            let mut o = z.is_infinite();
            o = !x.is_infinite() & o;
            o = !x.is_nan() & o;
            o = !y.is_infinite() & o;
            o = !y.is_nan() & o;
            h2 = o.select(z, h2);

            let o = h2.is_infinite() | h2.is_nan();

            o.select(h2, ret * q)
// #endif
        }

        /// Square root function
        ///
        /// The error bound of the returned value is `0.5001 ULP`
        //#[cfg(feature = "accurate_sqrt")]
        pub fn sqrt(d: F64x) -> F64x {
            d.sqrt()
        }
        // fall back to approximation if ACCURATE_SQRT is undefined
        /*#[cfg(not(feature = "accurate_sqrt"))]
        pub fn xsqrt(d: F64x) -> F64x {
            u05::sqrt(d)
        }*/

        /// FP remainder
        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        pub fn fmod(x: F64x, y: F64x) -> F64x {
            #[inline]
            fn toward0(x: F64x) -> F64x {
                // returns nextafter(x, 0)
                let t = F64x::from_bits(x.to_bits() + splat2i(-1, -1).cast());
                x.simd_eq(ZERO).select(ZERO, t)
            }

            #[cfg(feature = "full_fp_rounding")]
            #[inline]
            fn trunc_positive(x: F64x) -> F64x {
                // round to integer toward 0, positive argument only
                x.trunc()
            }
            #[cfg(not(feature = "full_fp_rounding"))]
            #[inline]
            fn trunc_positive(x: F64x) -> F64x {
                let mut fr = (-D1_31X).mul_add((x * (ONE / D1_31X)).trunci().cast(), x);
                fr -= fr.trunci().cast();
                x.abs().simd_ge(D1_52X).select(x, x - fr)
            }

            let n = x.abs();
            let d = y.abs();
            let s = ONE;
            let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE));
            let n = o.select(n * D1_54X, n);
            let d = o.select(d * D1_54X, d);
            let s = o.select(s * (ONE / D1_54X), s);
            let rd = toward0(d.recip());
            let mut r = Doubled::from(n);

            for _ in 0..21 {
                // ceil(log2(DBL_MAX) / 52)
                let mut q = trunc_positive(toward0(r.0) * rd);
                /* #ifndef ENABLE_FMA_DP
                q = vreinterpret_vd_vm(vand_vm_vm_vm(vreinterpret_vm_vd(q), vcast_vm_i_i(0xffffffff, 0xfffffffe)));
                #endif */
                q = ((F64x::splat(3.) * d).simd_gt(r.0) & r.0.simd_ge(d)).select(F64x::splat(2.), q);
                q = ((d + d).simd_gt(r.0) & r.0.simd_ge(d)).select( ONE, q);
                r = (r + q.mul_as_doubled(-d)).normalize();
                if r.0.simd_lt(d).all() {
                    break;
                }
            }

            let mut ret = r.0 * s;
            ret = F64x::from(r).simd_eq(d).select(ZERO, ret);

            ret = ret.mul_sign(x);

            ret = n.simd_lt(d).select(x, ret);
            d.simd_eq(ZERO).select(NAN, ret)
        }

        // TODO: add test for fmodf

        #[inline]
        fn rintk2(d: F64x) -> F64x {
            if cfg!(feature = "full_fp_rounding") {
                rint(d)
            } else {
                let c = D1_52X.mul_sign(d);
                d.abs().simd_gt(D1_52X).select(d, ((d + c) - c).or_sign(d))
            }
        }

        /// FP remainder
        pub fn remainder(x: F64x, y: F64x) -> F64x {
            let mut n = x.abs();
            let mut d = y.abs();
            let mut s = ONE;
            let o = d.simd_lt(F64x::splat(f64::MIN_POSITIVE*2.));
            n = o.select(n * D1_54X, n);
            d = o.select(d * D1_54X, d);
            s  = o.select(s * F64x::splat(1. / crate::f64::D1_54), s);
            let rd = d.recip();
            let mut r = Doubled::from(n);
            let mut qisodd = M64x::splat(false);

            for _ in 0..21 { // ceil(log2(DBL_MAX) / 52)
                let mut q = rintk2(r.0 * rd);
            /*#ifndef ENABLE_FMA_DP
                q = vreinterpret_vd_vm(vand_vm_vm_vm(vreinterpret_vm_vd(q), vcast_vm_u64(UINT64_C(0xfffffffffffffffe))));
            #endif*/
                q = r.0.abs().simd_lt(d * F64x::splat(1.5)).select(ONE.mul_sign(r.0), q);
                q = (r.0.abs().simd_lt(d * HALF) | (!qisodd & r.0.abs().simd_eq(d * HALF)))
                    .select(ZERO, q);
                if q.simd_eq(ZERO).all() {
                    break;
                }
                q = (q * (-d)).is_infinite().select((q + F64x::splat(-1.).mul_sign(r.0)), q);
                qisodd ^= q.is_odd();
                r = (r + q.mul_as_doubled(-d)).normalize();
            }

            let mut ret = r.0 * s;
            ret = ret.mul_sign(x);
            ret = y.is_infinite().select(x.is_infinite().select(NAN, x), ret);
            d.simd_eq(ZERO).select(NAN, ret)
        }

        #[test]
        fn test_remainder() {
            test_ff_f(
                remainder,
                rug::Float::remainder,
                f64::MIN..=f64::MAX,
                f64::MIN..=f64::MAX,
                0.5,
            );
        }

        #[inline]
        fn sinpik(d: F64x) -> Doubled<F64x> {
            let u = d * F64x::splat(4.);
            let mut q = u.trunci();
            q = (q + ((q.cast() >> Ux::splat(31)).cast() ^ Ix::splat(1))) & Ix::splat(!1);
            let o: M64x = (q & Ix::splat(2)).simd_eq(Ix::splat(2)).cast();

            let s = u - q.cast();
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            //

            let u = o
                .select_splat(
                    9.944_803_876_268_437_740_902_08_e-16,
                    -2.024_611_207_851_823_992_958_68_e-14,
                )
                .mul_add(
                    s,
                    o.select_splat(
                        -3.897_962_260_629_327_991_640_47_e-13,
                        6.948_218_305_801_794_613_277_84_e-12,
                    ),
                )
                .mul_add(
                    s,
                    o.select_splat(
                        1.150_115_825_399_960_352_669_01_e-10,
                        -1.757_247_499_528_531_799_526_64_e-9,
                    ),
                )
                .mul_add(
                    s,
                    o.select_splat(
                        -2.461_136_950_104_469_749_535_9_e-8,
                        3.133_616_889_668_683_928_784_22_e-7,
                    ),
                )
                .mul_add(
                    s,
                    o.select_splat(
                        3.590_860_448_590_527_540_050_62_e-6,
                        -3.657_620_418_216_155_192_036_1_e-5,
                    ),
                )
                .mul_add(
                    s,
                    o.select_splat(
                        -0.000_325_991_886_927_389_905_997_954,
                        0.002_490_394_570_192_718_502_743_56,
                    ),
                );
            let mut x = u * s
                + o.select_doubled(
                    Doubled::new(
                        F64x::splat(0.015_854_344_243_815_501_891_425_9),
                        F64x::splat(-1.046_932_722_806_315_219_088_45_e-18),
                    ),
                    Doubled::new(
                        F64x::splat(-0.080_745_512_188_280_785_248_473_1),
                        F64x::splat(3.618_524_750_670_371_048_499_87_e-18),
                    ),
                );
            x = s2 * x
                + o.select_doubled(
                    Doubled::new(
                        F64x::splat(-0.308_425_137_534_042_437_259_529),
                        F64x::splat(-1.956_984_921_336_335_503_383_45_e-17),
                    ),
                    Doubled::new(
                        F64x::splat(0.785_398_163_397_448_278_999_491),
                        F64x::splat(3.062_871_137_271_550_026_071_05_e-17),
                    ),
                );

            x *= o.select_doubled(s2, Doubled::from(t));
            x = o.select_doubled(x + ONE, x);

            let o: M64x = (q & Ix::splat(4)).simd_eq(Ix::splat(4)).cast();
            x.0 = F64x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ x.0.to_bits());
            x.1 = F64x::from_bits((o.to_int().cast() & NEG_ZERO.to_bits()) ^ x.1.to_bits());

            x
        }

        /// Integral and fractional value of FP number
        pub fn modf(x: F64x) -> (F64x, F64x) {
            let mut fr = x - D1_31X * (x * (ONE / D1_31X)).trunci().cast();
            fr -= fr.trunci().cast();
            fr = x.abs().simd_gt(D1_52X).select(ZERO, fr);

            (fr.copy_sign(x), (x - fr).copy_sign(x))
        }

    };
}
