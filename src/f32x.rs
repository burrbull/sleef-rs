#[macro_use]
mod u05_impl;

#[macro_use]
mod u10_impl;

#[cfg(not(feature = "deterministic"))]
#[macro_use]
mod u15_impl;

#[macro_use]
mod u35_impl;

#[macro_use]
mod fast_impl;

macro_rules! impl_math_f32 {
    ($size:expr) => {
        use crate::common::*;
        use doubled::*;

        type F32x = packed_simd::Simd<[f32; $size]>;
        type U32x = packed_simd::Simd<[u32; $size]>;
        type I32x = packed_simd::Simd<[i32; $size]>;
        type M32x = packed_simd::Simd<[packed_simd::m32; $size]>;

        impl BaseType for F32x {
            type Base = f32;
        }

        impl BaseType for U32x {
            type Base = u32;
        }

        impl BaseType for I32x {
            type Base = i32;
        }
        /*
        impl BaseType for M32x {
            type Base = m32;
        }
        */
        impl MaskType for F32x {
            type Mask = M32x;
        }

        impl BitsType for F32x {
            type Bits = U32x;
        }

        impl MaskType for Doubled<F32x> {
            type Mask = M32x;
        }

        const ZERO: F32x = F32x::splat(0.);
        const NEG_ZERO: F32x = F32x::splat(-0.);
        const ONE: F32x = F32x::splat(1.);
        const HALF: F32x = F32x::splat(0.5);
        const F1_32X: F32x = F32x::splat((1u64 << 32) as f32);
        const F1_30X: F32x = F32x::splat((1u32 << 30) as f32);
        const F1_25X: F32x = F32x::splat((1u32 << 25) as f32);
        const F1_24X: F32x = F32x::splat((1u32 << 24) as f32);
        const F1_23X: F32x = F32x::splat((1u32 << 23) as f32);
        const F1_12X: F32x = F32x::splat((1u32 << 12) as f32);
        const F1_10X: F32x = F32x::splat((1u32 << 10) as f32);

        const PI_A_F: F32x = F32x::splat(3.140_625);
        const PI_B_F: F32x = F32x::splat(0.000_967_025_756_835_937_5);
        const PI_C_F: F32x = F32x::splat(6.277_114_152_908_325_195_3_e-7);
        const PI_D_F: F32x = F32x::splat(1.215_420_125_655_342_076_2_e-10);
        const TRIGRANGEMAX_F: F32x = F32x::splat(39000.);

        const PI_A2_F: F32x = F32x::splat(3.141_479_492_187_5);
        const PI_B2_F: F32x = F32x::splat(0.000_113_159_418_106_079_101_56);
        const PI_C2_F: F32x = F32x::splat(1.984_187_258_941_005_893_6_e-9);
        const TRIGRANGEMAX2_F: F32x = F32x::splat(125.0);

        const SLEEF_FP_ILOGB0: I32x = I32x::splat(-2_147_483_328);
        const SLEEF_FP_ILOGBNAN: I32x = I32x::splat(2_147_483_327);
        const SQRT_FLT_MAX: F32x = F32x::splat(18_446_743_523_953_729_536.);
        const L10U_F: F32x = F32x::splat(0.301_025_390_6);
        const L10L_F: F32x = F32x::splat(4.605_038_981_e-6);
        const TRIGRANGEMAX4_F: F32x = F32x::splat(8e+6);
        const L2U_F: F32x = F32x::splat(0.693_145_751_953_125);
        const L2L_F: F32x = F32x::splat(1.428_606_765_330_187_045_e-6);
        const R_LN2_F: F32x = F32x::splat(
            1.442_695_040_888_963_407_359_924_681_001_892_137_426_645_954_152_985_934_135_449_406_931,
        );
        const LOG10_2_F: F32x = F32x::splat(3.321_928_094_887_362_347_870_319_429_489_390_175_864_831_393);

        pub mod u05 {
            //! Functions with 0.5 ULP error bound
            impl_math_f32_u05!();
        }

        pub mod u10 {
            //! Functions with 1.0 ULP error bound
            impl_math_f32_u10!();
        }

        #[cfg(not(feature = "deterministic"))]
        pub mod u15 {
            //! Functions with 1.5 ULP error bound
            impl_math_f32_u15!();
        }

        pub mod u35 {
            //! Functions with 3.5 ULP error bound
            impl_math_f32_u35!();
        }

        pub mod fast {
            //! Fast functions with 3500 ULP error bound
            impl_math_f32_fast!();
        }

        #[cfg(test)]
        fn test_libm_f_f(fun_fx: fn(F32x) -> F32x, fun_f: fn(f32) -> f32, mn: f32, mx: f32, ulp: f32) {
            use rand::Rng;
            let mut rng = rand::thread_rng();
            for _ in 0..crate::TEST_REPEAT {
                let mut in_f = [0_f32; $size];
                for v in in_f.iter_mut() {
                    *v = rng.gen_range(mn, mx);
                }
                let in_fx = F32x::from_slice_unaligned(&in_f);
                let out_fx = fun_fx(in_fx);
                for i in 0..$size {
                    let input = in_f[i];
                    let expected = fun_f(input);
                    let output = out_fx.extract(i);
                    if expected.is_nan() && output.is_nan() {
                        continue;
                    }
                    let diff = (expected.to_bits() as i32).wrapping_sub(output.to_bits() as i32) as f32;
                    #[cfg(not(feature = "std"))]
                    assert!(libm::fabsf(diff) <= 1. + ulp); // WARN!!!
                    #[cfg(feature = "std")]
                    assert!(
                        diff.abs() <= 1. + ulp,
                        format!(
                            "Position: {}, Input: {:e}, Output: {}, Expected: {}",
                            i, input, output, expected
                        )
                    );
                }
            }
        }

        #[cfg(test)]
        fn test_libm_f_ff(
            fun_fx: fn(F32x) -> (F32x, F32x),
            fun_f: fn(f32) -> (f32, f32),
            mn: f32,
            mx: f32,
            ulp: f32,
        ) {
            use rand::Rng;
            let mut rng = rand::thread_rng();
            for _ in 0..crate::TEST_REPEAT {
                let mut in_f = [0_f32; $size];
                for v in in_f.iter_mut() {
                    *v = rng.gen_range(mn, mx);
                }
                let in_fx = F32x::from_slice_unaligned(&in_f);
                let (out_fx1, out_fx2) = fun_fx(in_fx);
                for i in 0..$size {
                    let input = in_f[i];
                    let (expected1, expected2) = fun_f(input);
                    let output1 = out_fx1.extract(i);
                    let output2 = out_fx2.extract(i);
                    if (expected1.is_nan() && output1.is_nan()) || (expected2.is_nan() && output2.is_nan())
                    {
                        continue;
                    }
                    let diff1 = (expected1.to_bits() as i32).wrapping_sub(output1.to_bits() as i32) as f32;
                    let diff2 = (expected2.to_bits() as i32).wrapping_sub(output2.to_bits() as i32) as f32;
                    #[cfg(not(feature = "std"))]
                    assert!(libm::fabsf(diff1) <= 1. + ulp && libm::fabsf(diff2) <= 1. + ulp); // WARN!!!
                    #[cfg(feature = "std")]
                    assert!(
                        diff1.abs() <= 1. + ulp && diff2.abs() <= 1. + ulp,
                        format!(
                            "Position: {}, Input: {:e}, Output: ({}, {}), Expected: ({}, {})",
                            i, input, output1, output2, expected1, expected2
                        )
                    );
                }
            }
        }

        #[cfg(test)]
        fn test_libm_ff_f(
            fun_fx: fn(F32x, F32x) -> (F32x),
            fun_f: fn(f32, f32) -> f32,
            mn: f32,
            mx: f32,
            ulp: f32,
        ) {
            use rand::Rng;
            let mut rng = rand::thread_rng();
            for _ in 0..crate::TEST_REPEAT {
                let mut in_f1 = [0_f32; $size];
                let mut in_f2 = [0_f32; $size];
                for v in in_f1.iter_mut() {
                    *v = rng.gen_range(mn, mx);
                }
                for v in in_f2.iter_mut() {
                    *v = rng.gen_range(mn, mx);
                }
                let in_fx1 = F32x::from_slice_unaligned(&in_f1);
                let in_fx2 = F32x::from_slice_unaligned(&in_f2);
                let out_fx = fun_fx(in_fx1, in_fx2);
                for i in 0..$size {
                    let input1 = in_f1[i];
                    let input2 = in_f2[i];
                    let expected = fun_f(input1, input2);
                    let output = out_fx.extract(i);
                    if expected.is_nan() && output.is_nan() {
                        continue;
                    }
                    let diff = (expected.to_bits() as i32).wrapping_sub(output.to_bits() as i32) as f32;
                    #[cfg(not(feature = "std"))]
                    assert!(libm::fabsf(diff) <= 1. + ulp); // WARN!!!
                    #[cfg(feature = "std")]
                    assert!(
                        diff.abs() <= 1. + ulp,
                        format!(
                            "Position: {}, Input: ({:e}, {:e}), Output: {}, Expected: {}",
                            i, input1, input2, output, expected
                        )
                    );
                }
            }
        }

        #[inline]
        fn vgather_vf_p_vi2(ptr: &[f32], vi: I32x) -> F32x {
            let mut ar = [0_f32; F32x::lanes()];
            for i in 0..F32x::lanes() {
                ar[i] = ptr[vi.extract(i) as usize];
            }
            F32x::from_slice_unaligned(&ar)
        }

        impl SqrtAsDoubled for F32x {
            #[inline]
            fn sqrt_as_doubled(self) -> Doubled<Self> {
                let t = self.sqrt();
                ((self + t.mul_as_doubled(t)) * t.recpre()).scale(Self::splat(0.5))
            }
        }

        impl Round for F32x {
            type Int = I32x;
            #[inline]
            fn trunc(self) -> Self {
                Self::from_cast(self.trunci())
            }
            #[inline]
            fn trunci(self) -> Self::Int {
                Self::Int::from_cast(self)
            }
            #[inline]
            fn round(self) -> Self {
                rintf(self)
            }
            #[inline]
            fn roundi(self) -> Self::Int {
                Self::Int::from_cast(self.round())
            }
        }

        impl MulAdd for F32x {
            #[inline]
            fn mul_add(self, y: Self, z: Self) -> Self {
                self.mul_add(y, z)
            }
        }

        impl MulSub for F32x {
            #[inline]
            fn mul_sub(self, y: Self, z: Self) -> Self {
                self.mul_add(y, -z)
            }
        }

        impl NegMulAdd for F32x {
            #[inline]
            fn neg_mul_add(self, y: Self, z: Self) -> Self {
                (-self).mul_add(y, z)
            }
        }

        impl VectorizedSelect<f32> for M32x {
            type Output = F32x;
            fn select_splat(self, l: f32, r: f32) -> Self::Output {
                self.select(Self::Output::splat(l), Self::Output::splat(r))
            }
        }
        impl DoubledSelect<F32x> for M32x {
            fn select_doubled(self, l: Doubled<F32x>, r: Doubled<F32x>) -> Doubled<F32x> {
                Doubled::new(self.select(l.0, r.0), self.select(l.1, r.1))
            }
        }

        impl SelectSeveral<f32> for F32x {
            #[inline]
            fn select3(o0: Self::Mask, o1: Self::Mask, d0: f32, d1: f32, d2: f32) -> Self {
                o0.select(Self::splat(d0), o1.select_splat(d1, d2))
            }
            fn select4(
                o0: Self::Mask,
                o1: Self::Mask,
                o2: Self::Mask,
                d0: f32,
                d1: f32,
                d2: f32,
                d3: f32,
            ) -> Self {
                o0.select(
                    Self::splat(d0),
                    o1.select(Self::splat(d1), o2.select_splat(d2, d3)),
                )
            }
        }

        impl SelectSeveral<f64> for Doubled<F32x> {
            #[inline]
            fn select3(o0: Self::Mask, o1: Self::Mask, d0: f64, d1: f64, d2: f64) -> Self {
                o0.select_doubled(
                    Doubled::from(d0),
                    o1.select_doubled(Doubled::from(d1), Doubled::from(d2)),
                )
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
                o0.select_doubled(
                    Doubled::from(d0),
                    o1.select_doubled(
                        Doubled::from(d1),
                        o2.select_doubled(Doubled::from(d2), Doubled::from(d3)),
                    ),
                )
            }
        }

        impl Poly for F32x {
            fn c2v(c: Self::Base) -> Self {
                F32x::splat(c)
            }
        }

        #[inline]
        fn vsel_vf2_vo_f_f_f_f(o: M32x, x1: (f32, f32), x0: (f32, f32)) -> Doubled<F32x> {
            Doubled::new(o.select_splat(x1.0, x0.0), o.select_splat(x1.1, x0.1))
        }

        #[inline]
        fn vsel_vi2_vf_vf_vi2_vi2(f0: F32x, f1: F32x, x: I32x, y: I32x) -> I32x {
            f0.lt(f1).select(x, y)
        }

        #[inline]
        fn vsel_vi2_vf_vi2(d: F32x, x: I32x) -> I32x {
            I32x::from_bits(d.is_sign_negative()) & x
        }

        impl Sign for F32x {
            #[inline]
            fn is_sign_negative(self) -> Self::Mask {
                self.sign_bit().ne(Self::Bits::splat(0))
            }
            #[inline]
            fn is_sign_positive(self) -> Self::Mask {
                !self.is_sign_negative()
            }
            #[inline]
            fn sign_bit(self) -> Self::Bits {
                Self::Bits::from_bits(self) & Self::Bits::from_bits(NEG_ZERO)
            }
            #[inline]
            fn sign(self) -> Self {
                Self::from_bits(Self::Bits::from_bits(ONE) | (self.sign_bit()))
            }
            #[inline]
            fn mul_sign(self, other: Self) -> Self {
                Self::from_bits(Self::Bits::from_bits(self) ^ other.sign_bit())
            }
            #[inline]
            fn copy_sign(self, other: Self) -> Self {
                Self::from_bits(
                    (!Self::Bits::from_bits(NEG_ZERO) & Self::Bits::from_bits(self)) ^ (other.sign_bit()),
                )
            }
        }

        impl NegZero for F32x {
            #[inline]
            fn is_neg_zero(self) -> Self::Mask {
                Self::Bits::from_bits(self).eq(Self::Bits::from_bits(NEG_ZERO))
            }
        }

        impl IsInt for F32x {
            #[inline]
            fn is_integer(self) -> Self::Mask {
                self.trunc().eq(self)
            }
        }

        /*#[cfg(
            all(not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn vilogbk_vi2_vf(mut d: F32x) -> I32x {
            let o = d.lt(F32x::splat(5.421_010_862_427_522_e-20));
            d = o.select(F32x::splat(1.844_674_407_370_955_2_e19) * d, d);
            let q = I32x::from_cast(U32x::from_bits(d) >> 23) & I32x::splat(0xff);
            q - o.select(I32x::splat(64 + 0x7f), I32x::splat(0x7f))
        }
        /*#[cfg(
            all(not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn vilogb2k_vi2_vf(d: F32x) -> I32x {
            let q = U32x::from_bits(d);
            let mut q = I32x::from_bits(q >> 23);
            q &= I32x::splat(0xff);
            q - I32x::splat(0x7f)
        }

        //

        pub fn ilogbf(d: F32x) -> I32x {
            let mut e = vilogbk_vi2_vf(d.abs());
            e = d.eq(ZERO).select(SLEEF_FP_ILOGB0, e);
            e = d.is_nan().select(SLEEF_FP_ILOGBNAN, e);
            d.is_infinite().select(I32x::splat(i32::MAX), e)
        }
        #[inline]
        fn vpow2i_vf_vi2(q: I32x) -> F32x {
            F32x::from_bits(U32x::from_bits((q + I32x::splat(0x7f)) << 23))
        }

        #[inline]
        fn vldexp_vf_vf_vi2(mut x: F32x, mut q: I32x) -> F32x {
            let mut m = q >> 31;
            m = (((m + q) >> 6) - m) << 4;
            q -= m << 2;
            m += I32x::splat(0x7f);
            m = I32x::from_bits(m.gt(I32x::splat(0))) & m;
            let n = I32x::from_bits(m.gt(I32x::splat(0xff)));
            m = (!n & m) | (n & I32x::splat(0xff));
            let u = F32x::from_bits(U32x::from_bits(m << 23));
            x *= u * u * u * u;
            let u = F32x::from_bits(U32x::from_bits((q + I32x::splat(0x7f)) << 23));
            x * u
        }
        #[inline]
        fn vldexp2_vf_vf_vi2(d: F32x, e: I32x) -> F32x {
            d * vpow2i_vf_vi2(e >> 1) * vpow2i_vf_vi2(e - (e >> 1))
        }
        #[inline]
        fn vldexp3_vf_vf_vi2(d: F32x, q: I32x) -> F32x {
            F32x::from_bits(I32x::from_bits(d) + (q << 23))
        }

        pub fn ldexpf(x: F32x, q: I32x) -> F32x {
            vldexp_vf_vf_vi2(x, q)
        }

        #[inline]
        fn rempisubf(x: F32x) -> (F32x, I32x) {
            if cfg!(feature = "full_fp_rounding") {
                let y = (x * F32x::splat(4.)).round();
                let vi = (y - x.round() * F32x::splat(4.)).trunci();
                (x - y * F32x::splat(0.25), vi)
            } else {
                let mut fr = x - F1_10X * (x * (ONE / F1_10X)).trunc();
                let mut vi =
                    x.gt(ZERO).select(I32x::splat(4), I32x::splat(3)) + (fr * F32x::splat(8.)).trunci();
                vi = ((I32x::splat(7) & vi) - I32x::splat(3)) >> 1;
                fr -= F32x::splat(0.25) * (fr.mul_add(F32x::splat(4.), HALF.mul_sign(x))).trunc();
                fr = fr
                    .abs()
                    .gt(F32x::splat(0.25))
                    .select(fr - HALF.mul_sign(x), fr);
                fr = fr.abs().gt(F32x::splat(1e+10)).select(ZERO, fr);
                let o = x.abs().eq(F32x::splat(0.124_999_992_549_419_403_08));
                fr = o.select(x, fr);
                vi = o.select(I32x::splat(0), vi);
                (fr, vi)
            }
        }
        #[inline]
        fn rempif(mut a: F32x) -> (Doubled<F32x>, I32x) {
            let mut ex = vilogb2k_vi2_vf(a);
            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                ex = !(ex >> 31) & ex;
                ex = ex & I32x::splat(127);
            }*/
            ex -= I32x::splat(25);
            let q = I32x::from_bits(ex.gt(I32x::splat(90 - 25))) & I32x::splat(-64);
            a = vldexp3_vf_vf_vi2(a, q);
            ex = !(ex >> 31) & ex;
            ex <<= 2;
            let mut x = a.mul_as_doubled(vgather_vf_p_vi2(&crate::tables::REMPITABSP, ex));
            let (did, mut q) = rempisubf(x.0);
            x.0 = did;
            x = x.normalize();
            let y = a.mul_as_doubled(vgather_vf_p_vi2(&crate::tables::REMPITABSP[1..], ex));
            x += y;
            let (did, dii) = rempisubf(x.0);
            q += dii;
            x.0 = did;
            x = x.normalize();
            let mut y = Doubled::new(
                vgather_vf_p_vi2(&crate::tables::REMPITABSP[2..], ex),
                vgather_vf_p_vi2(&crate::tables::REMPITABSP[3..], ex),
            );
            y *= a;
            x += y;
            x = x.normalize();
            x *= Doubled::from((
                3.141_592_741_012_573_242_2 * 2.,
                -8.742_277_657_347_585_773_1_e-8 * 2.,
            ));
            x = a
                .abs()
                .lt(F32x::splat(0.7))
                .select_doubled(Doubled::new(a, ZERO), x);
            (x, q)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn modff(x: F32x) -> (F32x, F32x) {
            let fr = x - F32x::from_cast(x.trunci());
            let fr = x.abs().gt(F1_23X).select(ZERO, fr);
            (fr.copy_sign(x), (x - fr).copy_sign(x))
        }

        #[inline]
        fn atan2kf(y: F32x, x: F32x) -> F32x {
            let q = vsel_vi2_vf_vi2(x, I32x::splat(-2));
            let x = x.abs();

            let q = vsel_vi2_vf_vf_vi2_vi2(x, y, q + I32x::splat(1), q);
            let p = x.lt(y);
            let s = p.select(-x, y);
            let mut t = x.max(y);

            let s = s / t;
            t = s * s;

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

            let t = s.mul_add(t * u, s);
            F32x::from_cast(q).mul_add(F32x::FRAC_PI_2, t)
        }
        #[inline]
        fn visinf2_vf_vf_vf(d: F32x, m: F32x) -> F32x {
            F32x::from_bits(U32x::from_bits(d.is_infinite()) & (d.sign_bit() | U32x::from_bits(m)))
        }

        #[inline]
        fn logkf(mut d: F32x) -> Doubled<F32x> {
            let m: F32x;

            let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
                    {
                        let o = d.lt(F32x::splat(f32::MIN_POSITIVE));
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

            let x = F32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.square();

            let t = F32x::splat(0.240_320_354_700_088_500_976_562)
                .mul_add(x2.0, F32x::splat(0.285_112_679_004_669_189_453_125))
                .mul_add(x2.0, F32x::splat(0.400_007_992_982_864_379_882_812));
            let c = Doubled::from((
                0.666_666_626_930_236_816_406_25,
                3.691_838_612_596_143_320_843_11_e-9,
            ));

            let mut s = Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * ef;

            s = s.add_checked(x.scale(F32x::splat(2.)));
            s.add_checked(x2 * x * (x2 * t + c))
        }

        #[inline]
        fn expkf(d: Doubled<F32x>) -> F32x {
            let u = <F32x>::from(d) * R_LN2_F;
            let q = u.roundi();

            let mut s = d + F32x::from_cast(q) * (-L2U_F);
            s += F32x::from_cast(q) * (-L2L_F);

            s = s.normalize();

            let mut u = F32x::splat(0.001_363_246_468_827_128_410_339_36)
                .mul_add(s.0, F32x::splat(0.008_365_969_173_610_210_418_701_17))
                .mul_add(s.0, F32x::splat(0.041_671_082_377_433_776_855_468_8))
                .mul_add(s.0, F32x::splat(0.166_665_524_244_308_471_679_688))
                .mul_add(s.0, F32x::splat(0.499_999_850_988_388_061_523_438));

            let mut t = s.add_checked(s.square() * u);

            t = ONE.add_checked(t);
            u = t.0 + t.1;
            u = vldexp_vf_vf_vi2(u, q);

            F32x::from_bits(!U32x::from_bits(d.0.lt(F32x::splat(-104.))) & U32x::from_bits(u))
        }

        #[inline]
        fn expk2f(d: Doubled<F32x>) -> Doubled<F32x> {
            let u = (d.0 + d.1) * R_LN2_F;
            let q = u.roundi();

            let mut s = d + F32x::from_cast(q) * (-L2U_F);
            s += F32x::from_cast(q) * (-L2L_F);

            let u = F32x::splat(0.198_096_022_4_e-3)
                .mul_add(s.0, F32x::splat(0.139_425_648_4_e-2))
                .mul_add(s.0, F32x::splat(0.833_345_670_3_e-2))
                .mul_add(s.0, F32x::splat(0.416_663_736_1_e-1));

            let mut t = s * u + F32x::splat(0.166_666_659_414_234_244_790_680_580_464);
            t = s * t + HALF;
            t = s + s.square() * t;

            t = ONE.add_checked(t);

            t.0 = vldexp2_vf_vf_vi2(t.0, q);
            t.1 = vldexp2_vf_vf_vi2(t.1, q);

            t.0 = F32x::from_bits(!U32x::from_bits(d.0.lt(F32x::splat(-104.))) & U32x::from_bits(t.0));
            t.1 = F32x::from_bits(!U32x::from_bits(d.0.lt(F32x::splat(-104.))) & U32x::from_bits(t.1));

            t
        }

        #[inline]
        fn logk2f(d: Doubled<F32x>) -> Doubled<F32x> {
            let e = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                        vilogbk_vi2_vf(d.0 * F32x::splat(1. / 0.75))
                    /*} else {
                        vgetexp_vf_vf(d.0 * F32x::splat(1. / 0.75)).roundi()
                    }*/;
            let m = d.scale(vpow2i_vf_vi2(-e));

            let x = (m + F32x::splat(-1.)) / (m + ONE);
            let x2 = x.square();

            let t = F32x::splat(0.239_282_846_450_805_664_062_5)
                .mul_add(x2.0, F32x::splat(0.285_182_118_415_832_519_531_25))
                .mul_add(x2.0, F32x::splat(0.400_005_877_017_974_853_515_625))
                .mul_add(x2.0, F32x::splat(0.666_666_686_534_881_591_796_875));

            let mut s = Doubled::new(
                F32x::splat(0.693_147_182_464_599_609_38),
                F32x::splat(-1.904_654_323_148_236_017_e-9),
            ) * F32x::from_cast(e);
            s = s.add_checked(x.scale(F32x::splat(2.)));
            s.add_checked(x2 * x * t)
        }

        #[cfg(not(feature = "deterministic"))]
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

            u = vldexp2_vf_vf_vi2(u, q);

            u = d.ge(F32x::splat(128.)).select(F32x::INFINITY, u);
            F32x::from_bits(!U32x::from_bits(d.lt(F32x::splat(-150.))) & U32x::from_bits(u))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn log1pf(d: F32x) -> F32x {
            let m: F32x;

            let dp1 = d + ONE;

            let mut s =
                        /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                            let o = dp1.lt(F32x::splat(f32::MIN_POSITIVE));
                            let dp1 = o.select(dp1 * (F1_32X * F1_32X), dp1);
                            let e = vilogb2k_vi2_vf(dp1 * F32x::splat(1. / 0.75));
                            let t = vldexp3_vf_vf_vi2(ONE, -e);
                            m = d.mul_add(t, t - ONE);
                            let e = o.select(e - I32x::splat(64), e);
                            Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * F32x::from_cast(e)
                        }/* else {
                            let e = vgetexp_vf_vf(dp1, F32x::splat(1. / 0.75));
                            let e = e.eq(F32x::INFINITY).select(F32x::splat(128.), e);
                            let t = vldexp3_vf_vf_vi2(ONE, -e.roundi());
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

        //

        #[cfg(not(feature = "deterministic"))]
        pub fn fabsf(x: F32x) -> F32x {
            x.abs()
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn copysignf(x: F32x, y: F32x) -> F32x {
            x.copy_sign(y)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn fmaxf(x: F32x, y: F32x) -> F32x {
            if cfg!(target_arch = "x86_64") || cfg!(target_arch = "x86")
            /*    && !cfg!(feature = "enable_vecext")
            && !cfg!(feature = "enable_purec")*/
            {
                y.is_nan().select(x, x.max(y))
            } else {
                y.is_nan().select(x, x.gt(y).select(x, y))
            }
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn fminf(x: F32x, y: F32x) -> F32x {
            if cfg!(target_arch = "x86_64") || cfg!(target_arch = "x86")
            /*    && !cfg!(feature = "enable_vecext")
            && !cfg!(feature = "enable_purec")*/
            {
                y.is_nan().select(x, x.min(y))
            } else {
                y.is_nan().select(x, y.gt(x).select(x, y))
            }
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn fdimf(x: F32x, y: F32x) -> F32x {
            let ret = x - y;
            (ret.lt(ZERO) | x.eq(y)).select(ZERO, ret)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn truncf(x: F32x) -> F32x {
            let fr = x - F32x::from_cast(x.trunci());
            (x.is_infinite() | x.abs().ge(F1_23X)).select(x, (x - fr).copy_sign(x))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn floorf(x: F32x) -> F32x {
            let fr = x - F32x::from_cast(x.trunci());
            let fr = fr.lt(ZERO).select(fr + ONE, fr);
            (x.is_infinite() | x.abs().ge(F1_23X)).select(x, (x - fr).copy_sign(x))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn ceilf(x: F32x) -> F32x {
            let fr = x - F32x::from_cast(x.trunci());
            let fr = fr.le(ZERO).select(fr, fr - ONE);
            (x.is_infinite() | x.abs().ge(F1_23X)).select(x, (x - fr).copy_sign(x))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn roundf(d: F32x) -> F32x {
            let mut x = d + HALF;
            let fr = x - F32x::from_cast(x.trunci());
            x = (x.le(ZERO) & fr.eq(ZERO)).select(x - ONE, x);
            let fr = fr.lt(ZERO).select(fr + ONE, fr);
            x = d
                .eq(F32x::splat(0.499_999_970_197_677_612_3))
                .select(ZERO, x);
            (d.is_infinite() | d.abs().ge(F1_23X)).select(d, (x - fr).copy_sign(d))
        }

        //#[cfg(not(feature="deterministic"))]
        pub fn rintf(d: F32x) -> F32x {
            let mut x = d + HALF;
            let isodd = (I32x::splat(1) & x.trunci()).eq(I32x::splat(1));
            let mut fr = x - F32x::from_cast(x.trunci());
            fr = (fr.lt(ZERO) | (fr.eq(ZERO) & isodd)).select(fr + ONE, fr);
            x = d
                .eq(F32x::splat(0.500_000_059_604_644_775_39))
                .select(ZERO, x);
            (d.is_infinite() | d.abs().ge(F1_23X)).select(d, (x - fr).copy_sign(d))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn fmaf(mut x: F32x, mut y: F32x, mut z: F32x) -> F32x {
            let h2 = x * y + z;
            let mut q = ONE;
            let o = h2.abs().lt(F32x::splat(1e-38));
            const C0: F32x = F1_25X;
            let c1: F32x = C0 * C0;
            let c2: F32x = c1 * c1;
            {
                x = o.select(x * c1, x);
                y = o.select(y * c1, y);
                z = o.select(z * c2, z);
                q = o.select(ONE / c2, q);
            }
            let o = h2.abs().gt(F32x::splat(1e+38));
            {
                x = o.select(x * (ONE / c1), x);
                y = o.select(y * (ONE / c1), y);
                z = o.select(z * (ONE / c2), z);
                q = o.select(c2, q);
            }
            let d = x.mul_as_doubled(y) + z;
            let ret = (x.eq(ZERO) | y.eq(ZERO)).select(z, d.0 + d.1);
            let mut o = z.is_infinite();
            o = !x.is_infinite() & o;
            o = !x.is_nan() & o;
            o = !y.is_infinite() & o;
            o = !y.is_nan() & o;
            let h2 = o.select(z, h2);

            o = h2.is_infinite() | h2.is_nan();

            o.select(h2, ret * q)
        }

        pub fn sqrtf(d: F32x) -> F32x {
            //   if cfg!(feature = "accurate_sqrt") {
            d.sqrt()
            /*    } else {
                // fall back to approximation if ACCURATE_SQRT is undefined
                u05::sqrtf(d)
            }*/
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn nextafterf(x: F32x, y: F32x) -> F32x {
            let x = x.eq(ZERO).select(ZERO.mul_sign(y), x);
            let mut xi2 = I32x::from_bits(x);
            let c = x.is_sign_negative() ^ y.ge(x);

            xi2 = c.select(I32x::splat(0) - (xi2 ^ I32x::splat(1 << 31)), xi2);

            xi2 = x.ne(y).select(xi2 - I32x::splat(1), xi2);

            xi2 = c.select(I32x::splat(0) - (xi2 ^ I32x::splat(1 << 31)), xi2);

            let mut ret = F32x::from_bits(xi2);

            ret = (ret.eq(ZERO) & x.ne(ZERO)).select(ZERO.mul_sign(x), ret);

            ret = (x.eq(ZERO) & y.eq(ZERO)).select(y, ret);

            (x.is_nan() | y.is_nan()).select(F32x::NAN, ret)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn frfrexpf(x: F32x) -> F32x {
            let x = x
                .abs()
                .lt(F32x::splat(f32::MIN_POSITIVE))
                .select(x * F1_32X, x);

            let mut xm = U32x::from_bits(x);
            xm &= U32x::splat(!0x_7f80_0000_u32);
            xm |= U32x::splat(0x_3f00_0000_u32);

            let ret = F32x::from_bits(xm);

            let ret = x.is_infinite().select(F32x::INFINITY.mul_sign(x), ret);
            x.eq(ZERO).select(x, ret)
        }

        pub fn expfrexpf(_x: F32x) -> I32x {
            /*
              x = x.abs().lt(F32x::splat(f32::MIN_POSITIVE)).select(x * F1_63X, x);

              let mut ret = I32x::from_cast($ix::from_bits(x);
              ret = (vsrl_vi_vi_i(ret, 20) & $ix::splat(0x7ff)) - $ix::splat(0x3fe);

              (x.eq(ZERO) | x.is_nan() | x.is_infinite()).select($ix::splat(0), ret)
            */
            I32x::splat(0)
        }
        #[inline]
        fn vtoward0f(x: F32x) -> F32x {
            let t = F32x::from_bits(I32x::from_bits(x) - I32x::splat(1));
            x.eq(ZERO).select(ZERO, t)
        }
        #[inline]
        fn vptruncf(x: F32x) -> F32x {
            if cfg!(feature = "full_fp_rounding") {
                x.trunc()
            } else {
                let fr = x - F32x::from_cast(x.trunci());
                x.abs().ge(F1_23X).select(x, x - fr)
            }
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn fmodf(x: F32x, y: F32x) -> F32x {
            let nu = x.abs();
            let de = y.abs();
            let s = ONE;
            let o = de.lt(F32x::splat(f32::MIN_POSITIVE));
            let nu = o.select(nu * F1_25X, nu);
            let de = o.select(de * F1_25X, de);
            let s = o.select(s * (ONE / F1_25X), s);
            let rde = vtoward0f(de.recpre());
            #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
            {
                let rde = vtoward0f(rde);
            }
            let mut r = Doubled::new(nu, ZERO);

            for _ in 0..8 {
                // ceil(log2(FLT_MAX) / 22)+1
                let q = ((de + de).gt(r.0) & r.0.ge(de)).select(ONE, vtoward0f(r.0) * rde);
                r = (r + vptruncf(q).mul_as_doubled(-de)).normalize();
                if r.0.lt(de).all() {
                    break;
                }
            }

            let mut ret = (r.0 + r.1) * s;
            ret = (r.0 + r.1).eq(de).select(ZERO, ret);

            ret = ret.mul_sign(x);

            ret = nu.lt(de).select(x, ret);
            de.eq(ZERO).select(F32x::NAN, ret)
        }

        //
        #[inline]
        fn sinpifk(d: F32x) -> Doubled<F32x> {
            let u = d * F32x::splat(4.);
            let q = u.trunci();
            let q = (q + (I32x::from_bits(U32x::from_bits(q) >> 31) ^ I32x::splat(1))) & I32x::splat(!1);
            let o = (q & I32x::splat(2)).eq(I32x::splat(2));

            let s = u - F32x::from_cast(q);
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            //

            let u = o
                .select_splat(-0.243_061_180_1_e-7, 0.309_384_205_4_e-6)
                .mul_add(s, o.select_splat(0.359_057_708_e-5, -0.365_730_738_8_e-4))
                .mul_add(s, o.select_splat(-0.325_991_772_1_e-3, 0.249_039_358_5_e-2));
            let mut x = u * s
                + vsel_vf2_vo_f_f_f_f(
                    o,
                    (
                        0.015_854_343_771_934_509_277,
                        4.494_005_135_403_224_281_1_e-10,
                    ),
                    (
                        -0.080_745_510_756_969_451_904,
                        -1.337_366_533_907_693_625_8_e-9,
                    ),
                );
            x = s2 * x
                + vsel_vf2_vo_f_f_f_f(
                    o,
                    (
                        -0.308_425_128_459_930_419_92,
                        -9.072_833_903_073_392_227_7_e-9,
                    ),
                    (
                        0.785_398_185_253_143_310_55,
                        -2.185_733_861_756_648_485_5_e-8,
                    ),
                );

            x *= o.select_doubled(s2, Doubled::new(t, ZERO));
            x = o.select_doubled(x + ONE, x);

            let o = (q & I32x::splat(4)).eq(I32x::splat(4));
            x.0 = F32x::from_bits((U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(x.0));
            x.1 = F32x::from_bits((U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(x.1));

            x
        }

        #[inline]
        fn cospifk(d: F32x) -> Doubled<F32x> {
            let u = d * F32x::splat(4.);
            let q = u.trunci();
            let q = (q + (I32x::from_bits(U32x::from_bits(q) >> 31) ^ I32x::splat(1))) & I32x::splat(!1);
            let o = (q & I32x::splat(2)).eq(I32x::splat(0));

            let s = u - F32x::from_cast(q);
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            //

            let u = o
                .select_splat(-0.243_061_180_1_e-7, 0.309_384_205_4_e-6)
                .mul_add(s, o.select_splat(0.359_057_708_e-5, -0.365_730_738_8_e-4))
                .mul_add(s, o.select_splat(-0.325_991_772_1_e-3, 0.249_039_358_5_e-2));
            let mut x = u * s
                + vsel_vf2_vo_f_f_f_f(
                    o,
                    (
                        0.015_854_343_771_934_509_277,
                        4.494_005_135_403_224_281_1_e-10,
                    ),
                    (
                        -0.080_745_510_756_969_451_904,
                        -1.337_366_533_907_693_625_8_e-9,
                    ),
                );
            x = s2 * x
                + vsel_vf2_vo_f_f_f_f(
                    o,
                    (
                        -0.308_425_128_459_930_419_92,
                        -9.072_833_903_073_392_227_7_e-9,
                    ),
                    (
                        0.785_398_185_253_143_310_55,
                        -2.185_733_861_756_648_485_5_e-8,
                    ),
                );

            x *= o.select_doubled(s2, Doubled::new(t, ZERO));
            x = o.select_doubled(x + ONE, x);

            let o = ((q + I32x::splat(2)) & I32x::splat(4)).eq(I32x::splat(4));
            x.0 = F32x::from_bits((U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(x.0));
            x.1 = F32x::from_bits((U32x::from_bits(o) & U32x::from_bits(NEG_ZERO)) ^ U32x::from_bits(x.1));

            x
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        fn gammafk(a: F32x) -> (Doubled<F32x>, Doubled<F32x>) {
            let mut clln = Doubled::from((1., 0.));
            let mut clld = Doubled::from((1., 0.));

            let otiny = a.abs().lt(F32x::splat(1e-30));
            let oref = a.lt(HALF);

            let x = otiny.select_doubled(
                Doubled::from((0., 0.)),
                oref.select_doubled(ONE.add_as_doubled(-a), Doubled::new(a, ZERO)),
            );

            let o0 = HALF.le(x.0) & x.0.le(F32x::splat(1.2));
            let o2 = F32x::splat(2.3).le(x.0);

            let mut y = ((x + ONE) * x).normalize();
            y = ((x + F32x::splat(2.)) * y).normalize();

            let o = o2 & x.0.le(F32x::splat(7.));
            clln = o.select_doubled(y, clln);

            let mut x = o.select_doubled(x + F32x::splat(3.), x);
            let t = o2.select(x.0.recpre(), (x + o0.select_splat(-1., -2.)).normalize().0);

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
            clln = otiny.select_doubled(Doubled::from((1., 0.)), oref.select_doubled(clln, clld));

            if !(!oref).all() {
                let t = a - F1_12X * F32x::from_cast((a * (ONE / F1_12X)).trunci());
                x = clld * sinpifk(t);
            }

            clld = otiny.select_doubled(
                Doubled::new(a * (F1_30X * F1_30X), ZERO),
                oref.select_doubled(x, y),
            );

            (clc, clln / clld)
        }

        #[inline]
        fn expm1fk(d: F32x) -> F32x {
            let q = (d * R_LN2_F).roundi();
            let s = F32x::from_cast(q).mul_add(-L2U_F, d);
            let s = F32x::from_cast(q).mul_add(-L2L_F, s);

            let s2 = s * s;
            let s4 = s2 * s2;

            let u = F32x::poly6(s, s2, s4,
                0.000_198_527_617_612_853_646_278_381,
                0.001_393_043_552_525_341_510_772_71,
                0.008_333_360_776_305_198_669_433_59,
                0.041_666_485_369_205_474_853_515_6,
                0.166_666_671_633_720_397_949_219,
                0.5);

            let u = (s * s).mul_add(u, s);

            q.eq(I32x::splat(0))
                .select(u, vldexp2_vf_vf_vi2(u + ONE, q) - ONE)
        }

        #[inline]
        fn logk3f(mut d: F32x) -> F32x {
            let (m, e) = //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")
            {
                let o = d.lt(F32x::splat(f32::MIN_POSITIVE));
                d = o.select(d * (F1_32X * F1_32X), d);
                let e = vilogb2k_vi2_vf(d * F32x::splat(1./0.75));
                (vldexp3_vf_vf_vi2(d, -e), o.select(e - I32x::splat(64), e))
            /*} else {
              let mut e = vgetexp_vf_vf(d * F32x::splat(1./0.75));
              (vgetmant_vf_vf(d), e.eq(F32x::INFINITY).select(F32x::splat(128.), e))
            */};

            let x = (m - ONE) / (ONE + m);
            let x2 = x * x;

            let t = F32x::splat(0.239_282_846_450_805_664_062_5)
                .mul_add(x2, F32x::splat(0.285_182_118_415_832_519_531_25))
                .mul_add(x2, F32x::splat(0.400_005_877_017_974_853_515_625))
                .mul_add(x2, F32x::splat(0.666_666_686_534_881_591_796_875))
                .mul_add(x2, F32x::splat(2.));

            //if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {
            x.mul_add(
                t,
                F32x::splat(0.693_147_180_559_945_286_226_764) * F32x::from_cast(e),
            )
            /* } else {
              x.mul_add(t, F32x::splat(0.693_147_180_559_945_286_226_764) * e)
            }*/
        }

        #[inline]
        fn expk3f(d: F32x) -> F32x {
            let q = (d * R_LN2_F).roundi();

            let mut s = F32x::from_cast(q).mul_add(-L2U_F, d);
            s = F32x::from_cast(q).mul_add(-L2L_F, s);

            let mut u = F32x::splat(0.000_198_527_617_612_853_646_278_381)
                .mul_add(s, F32x::splat(0.001_393_043_552_525_341_510_772_71))
                .mul_add(s, F32x::splat(0.008_333_360_776_305_198_669_433_59))
                .mul_add(s, F32x::splat(0.041_666_485_369_205_474_853_515_6))
                .mul_add(s, F32x::splat(0.166_666_671_633_720_397_949_219))
                .mul_add(s, F32x::splat(0.5));

            u = (s * s).mul_add(u, s + ONE);
            u = vldexp2_vf_vf_vi2(u, q);

            F32x::from_bits(!U32x::from_bits(d.lt(F32x::splat(-104.))) & U32x::from_bits(u))
        }

    };
}
