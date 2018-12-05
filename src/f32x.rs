#[macro_use]
mod u05_impl;
#[macro_use]
mod u10_impl;
#[macro_use]
mod u15_impl;
#[macro_use]
mod u35_impl;

macro_rules! impl_math_f32 {
    ($f32x:ident, $u32x:ident, $m32x:ident, $i32x:ident) => {
        use crate::common::*;
        use doubled::*;

        const ZERO: $f32x = $f32x::splat(0.);
        const ONE: $f32x = $f32x::splat(1.);
        const HALF: $f32x = $f32x::splat(0.5);
        const F1_32X: $f32x = $f32x::splat((1u64 << 32) as f32);
        const F1_30X: $f32x = $f32x::splat((1u32 << 30) as f32);
        const F1_25X: $f32x = $f32x::splat((1u32 << 25) as f32);
        const F1_24X: $f32x = $f32x::splat((1u32 << 24) as f32);
        const F1_23X: $f32x = $f32x::splat((1u32 << 23) as f32);
        const F1_12X: $f32x = $f32x::splat((1u32 << 12) as f32);
        const F1_10X: $f32x = $f32x::splat((1u32 << 10) as f32);

        const PI_A_F: $f32x = $f32x::splat(3.140_625);
        const PI_B_F: $f32x = $f32x::splat(0.000_967_025_756_835_937_5);
        const PI_C_F: $f32x = $f32x::splat(6.277_114_152_908_325_195_3_e-7);
        const PI_D_F: $f32x = $f32x::splat(1.215_420_125_655_342_076_2_e-10);
        const TRIGRANGEMAX_F: $f32x = $f32x::splat(39000.);

        const PI_A2_F: $f32x = $f32x::splat(3.141_479_492_187_5);
        const PI_B2_F: $f32x = $f32x::splat(0.000_113_159_418_106_079_101_56);
        const PI_C2_F: $f32x = $f32x::splat(1.984_187_258_941_005_893_6_e-9);
        const TRIGRANGEMAX2_F: $f32x = $f32x::splat(125.0);

        const SLEEF_FP_ILOGB0: $i32x = $i32x::splat(-2_147_483_328);
        const SLEEF_FP_ILOGBNAN: $i32x = $i32x::splat(2_147_483_327);
        const SQRT_FLT_MAX: $f32x = $f32x::splat(18_446_743_523_953_729_536.);
        const L10U_F: $f32x = $f32x::splat(0.301_025_390_6);
        const L10L_F: $f32x = $f32x::splat(4.605_038_981_e-6);
        const TRIGRANGEMAX4_F: $f32x = $f32x::splat(8e+6);
        const L2U_F: $f32x = $f32x::splat(0.693_145_751_953_125);
        const L2L_F: $f32x = $f32x::splat(1.428_606_765_330_187_045_e-6);
        const R_LN2_F: $f32x = $f32x::splat(1.442_695_040_888_963_407_359_924_681_001_892_137_426_645_954_152_985_934_135_449_406_931);
        const LOG10_2_F: $f32x = $f32x::splat(3.321_928_094_887_362_347_870_319_429_489_390_175_864_831_393);

        pub mod u05 {
            //! Functions with 0.5 ULP error bound
            impl_math_f32_u05!($f32x, $u32x, $m32x, $i32x);
        }

        pub mod u10 {
            //! Functions with 1.0 ULP error bound
            impl_math_f32_u10!($f32x, $u32x, $m32x, $i32x);
        }

        pub mod u15 {
            //! Functions with 1.5 ULP error bound
            impl_math_f32_u15!($f32x, $u32x, $m32x, $i32x);
        }

        pub mod u35 {
            //! Functions with 3.5 ULP error bound
            impl_math_f32_u35!($f32x, $u32x, $m32x, $i32x);
        }

        //---------???????
        //--------- Naive implementation ???????
        #[inline]
        fn vandnot_vm_vm_vm(x: $u32x, y: $u32x) -> $u32x { y & !x }

        #[inline]
        fn vandnot_vo_vo_vo(x: $m32x, y: $m32x) -> $m32x { y & !x }

        #[inline]
        fn vand_vm_vo32_vm(x: $m32x, y: $u32x) -> $u32x { $u32x::from_bits(x) & y }
        #[inline]
        fn vor_vm_vo32_vm(x: $m32x, y: $u32x) -> $u32x {  $u32x::from_bits(x) | y }
        #[inline]
        fn vandnot_vm_vo32_vm(x: $m32x, y: $u32x) -> $u32x { y & !$u32x::from_bits(x) }

        #[inline]
        fn vandnot_vi2_vi2_vi2(x: $i32x, y: $i32x) -> $i32x { y & !x }

        #[inline]
        fn vand_vi2_vo_vi2(x: $m32x, y: $i32x) -> $i32x { $i32x::from_bits(x) & y }

        #[inline]
        fn vgt_vi2_vi2_vi2(x: $i32x, y: $i32x) -> $i32x { $i32x::from_bits(x.gt(y)) }


        impl SqrtAsDoubled for $f32x {
            #[inline]
            fn sqrt_as_doubled(self) -> Doubled<Self> {
                let t = self.sqrt();
                ((self + t.mul_as_doubled(t)) * t.recpre()).scale(Self::splat(0.5))
            }
        }

        impl Round for $f32x {
            type Int = $i32x;
            #[inline]
            fn truncate(self) -> Self {
                Self::from_cast(self.truncatei())
            }
            #[inline]
            fn truncatei(self) -> Self::Int {
                Self::Int::from_cast(self)
            }
            #[inline]
            fn rint(self) -> Self {
                rintf(self)
            }
            #[inline]
            fn rinti(self) -> Self::Int {
                Self::Int::from_cast(self.rint())
            }
        }

        #[inline]
        fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { z - x * y }


        #[inline]
        fn vgather_vf_p_vi2(ptr: &[f32], vi: $i32x) -> $f32x {
          let mut ar = [0_f32; $f32x::lanes()];
          for i in 0..$f32x::lanes() {
              ar[i] = ptr[vi.extract(i) as usize];
          }
          $f32x::from_slice_aligned(&ar)
        }


        //----------???????
        //----------???????

        //-------------------

        //----------------------
        impl VectorizedSelect<f32> for $m32x {
            type Output = $f32x;
            fn select_splat(self, l: f32, r: f32) -> Self::Output {
                self.select(Self::Output::splat(l), Self::Output::splat(r))
            }
        }
        impl DoubledSelect<$f32x> for $m32x {
            fn select_doubled(self, l: Doubled<$f32x>, r: Doubled<$f32x>) -> Doubled<$f32x> {
                Doubled::new(self.select(l.0, r.0), self.select(l.1, r.1))
            }
        }

        #[inline]
        fn vsel_vf_vo_vo_f_f_f(o0: $m32x, o1: $m32x, d0: f32, d1: f32, d2: f32) -> $f32x {
          o0.select($f32x::splat(d0), o1.select_splat(d1, d2))
        }

        #[inline]
        fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $m32x, o1: $m32x, o2: $m32x, d0: f32, d1: f32, d2: f32, d3: f32) -> $f32x {
          o0.select($f32x::splat(d0), o1.select($f32x::splat(d1), o2.select_splat(d2, d3)))
        }

        #[inline]
        fn vsel_vf2_vo_f_f_f_f(o: $m32x, x1: f32, y1: f32, x0: f32, y0: f32) -> Doubled<$f32x> {
            Doubled::new(o.select_splat(x1, x0), o.select_splat(y1, y0))
        }

        #[inline]
        fn vsel_vf2_vo_vo_d_d_d(o0: $m32x, o1: $m32x, d0: f64, d1: f64, d2: f64) -> Doubled<$f32x> {
            o0.select_doubled(Doubled::from(d0),
                o1.select_doubled(Doubled::from(d1), Doubled::from(d2)),
            )
        }

        #[inline]
        fn vsel_vf2_vo_vo_vo_d_d_d_d(
            o0: $m32x,
            o1: $m32x,
            o2: $m32x,
            d0: f64,
            d1: f64,
            d2: f64,
            d3: f64,
        ) -> Doubled<$f32x>  {
            o0.select_doubled(Doubled::from(d0),
                o1.select_doubled(Doubled::from(d1),
                    o2.select_doubled(Doubled::from(d2), Doubled::from(d3)),
                ),
            )
        }

        //---------------------

        #[inline]
        fn visnegzero_vo_vf(d: $f32x) -> $m32x {
            $i32x::from_bits(d).eq($i32x::from_bits($f32x::splat(-0.)))
        }

        #[inline]
        fn vsignbit_vm_vf(f: $f32x) -> $u32x {
            $u32x::from_bits(f) & $u32x::from_bits($f32x::splat(-0.))
        }
        #[inline]
        fn vmulsign_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x {
            $f32x::from_bits($u32x::from_bits(x) ^ vsignbit_vm_vf(y))
        }
        #[inline]
        fn vcopysign_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x {
            $f32x::from_bits(
                vandnot_vm_vm_vm($u32x::from_bits($f32x::splat(-0.)), $u32x::from_bits(x))
                    ^ ($u32x::from_bits($f32x::splat(-0.)) & $u32x::from_bits(y)),
            )
        }
        #[inline]
        fn vsign_vf_vf(f: $f32x) -> $f32x {
            $f32x::from_bits(
                $u32x::from_bits(ONE)
                    | ($u32x::from_bits($f32x::splat(-0.)) & $u32x::from_bits(f)),
            )
        }

        impl Sign for $f32x {
            type Mask = $m32x;
            #[inline]
            fn is_sign_negative(self) -> Self::Mask {
                ($u32x::from_bits(self) & $u32x::splat(((-0.) as f32).to_bits()))
                    .ne($u32x::splat(0))
            }
            #[inline]
            fn is_sign_positive(self) -> Self::Mask {
                !self.is_sign_negative()
            }
        }

        #[inline]
        fn vsel_vi2_vf_vf_vi2_vi2(f0: $f32x, f1: $f32x, x: $i32x, y: $i32x) -> $i32x {
            f0.lt(f1).select(x, y)
        }
        #[inline]
        fn vsel_vi2_vf_vi2(d: $f32x, x: $i32x) -> $i32x {
            vand_vi2_vo_vi2(d.is_sign_negative(), x)
        }

        impl IsInt for $f32x {
            type Mask = $m32x;
            #[inline]
            fn is_integer(self) -> Self::Mask {
                self.truncate().eq(self)
            }
        }

        /*#[cfg(
            all(not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn vilogbk_vi2_vf(mut d: $f32x) -> $i32x {
            let o = d.lt($f32x::splat(5.421_010_862_427_522_e-20));
            d = o.select($f32x::splat(1.844_674_407_370_955_2_e19) * d, d);
            let q = $i32x::from_cast($u32x::from_bits(d) >> 23) & $i32x::splat(0xff);
            q - o.select($i32x::splat(64 + 0x7f), $i32x::splat(0x7f))
        }
        /*#[cfg(
            all(not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn vilogb2k_vi2_vf(d: $f32x) -> $i32x {
            let q = $u32x::from_bits(d);
            let mut q = $i32x::from_bits(q >> 23);
            q &= $i32x::splat(0xff);
            q - $i32x::splat(0x7f)
        }

        //

        pub fn ilogbf(d: $f32x) -> $i32x {
            let mut e = vilogbk_vi2_vf(d.abs());
            e = d
                .eq(ZERO)
                .select(SLEEF_FP_ILOGB0, e);
            e = d.is_nan().select(SLEEF_FP_ILOGBNAN, e);
            d.is_infinite().select($i32x::splat(i32::MAX), e)
        }
        #[inline]
        fn vpow2i_vf_vi2(q: $i32x) -> $f32x {
            $f32x::from_bits($u32x::from_bits((q + $i32x::splat(0x7f)) << 23))
        }
        #[inline]
        fn vldexp_vf_vf_vi2(mut x: $f32x, mut q: $i32x) -> $f32x {
            let mut m = q >> 31;
            m = (((m + q) >> 6) - m) << 4;
            q -= m << 2;
            m += $i32x::splat(0x7f);
            m = vgt_vi2_vi2_vi2(m, $i32x::splat(0)) & m;
            let n = vgt_vi2_vi2_vi2(m, $i32x::splat(0xff));
            m = vandnot_vi2_vi2_vi2(n, m) | (n & $i32x::splat(0xff));
            let u = $f32x::from_bits($u32x::from_bits(m << 23));
            x *= u * u * u * u;
            let u = $f32x::from_bits($u32x::from_bits((q + $i32x::splat(0x7f)) << 23));
            x * u
        }
        #[inline]
        fn vldexp2_vf_vf_vi2(d: $f32x, e: $i32x) -> $f32x {
            d * vpow2i_vf_vi2(e >> 1) * vpow2i_vf_vi2(e - (e >> 1))
        }
        #[inline]
        fn vldexp3_vf_vf_vi2(d: $f32x, q: $i32x) -> $f32x {
            $f32x::from_bits($i32x::from_bits(d) + (q << 23))
        }

        pub fn ldexpf(x: $f32x, q: $i32x) -> $f32x {
            vldexp_vf_vf_vi2(x, q)
        }

        #[inline]
        fn rempisubf(x: $f32x) -> ($f32x, $i32x) {
            if cfg!(feature = "full_fp_rounding") {
                let y = (x * $f32x::splat(4.)).rint();
                let vi = (y - x.rint() * $f32x::splat(4.)).truncatei();
                (x - y * $f32x::splat(0.25), vi)
            } else {
                let mut fr = x - F1_10X * (x * (ONE / F1_10X)).truncate();
                let mut vi = x
                    .gt(ZERO)
                    .select($i32x::splat(4), $i32x::splat(3))
                    + (fr * $f32x::splat(8.)).truncatei();
                vi = (($i32x::splat(7) & vi) - $i32x::splat(3)) >> 1;
                fr -= $f32x::splat(0.25)
                    * (fr.mul_add($f32x::splat(4.), vmulsign_vf_vf_vf(HALF, x)))
                        .truncate();
                fr = fr
                    .abs()
                    .gt($f32x::splat(0.25))
                    .select(fr - vmulsign_vf_vf_vf(HALF, x), fr);
                fr = fr.abs().gt($f32x::splat(1e+10)).select(ZERO, fr);
                let o = x.abs().eq($f32x::splat(0.124_999_992_549_419_403_08));
                fr = o.select(x, fr);
                vi = o.select($i32x::splat(0), vi);
                (fr, vi)
            }
        }
        #[inline]
        fn rempif(mut a: $f32x) -> (Doubled<$f32x>, $i32x) {
            let mut ex = vilogb2k_vi2_vf(a);
            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                ex = vandnot_vi2_vi2_vi2(ex >> 31, ex);
                ex = ex & $i32x::splat(127);
            }*/
            ex -= $i32x::splat(25);
            let q = vand_vi2_vo_vi2(ex.gt($i32x::splat(90 - 25)), $i32x::splat(-64));
            a = vldexp3_vf_vf_vi2(a, q);
            ex = vandnot_vi2_vi2_vi2(ex >> 31, ex);
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
            x *= Doubled::from((3.141_592_741_012_573_242_2 * 2., -8.742_277_657_347_585_773_1_e-8 * 2.));
            x = a.abs().lt($f32x::splat(0.7)).select_doubled(Doubled::new(a, ZERO),
                x,
            );
            (x, q)
        }

        pub fn modff(x: $f32x) -> ($f32x, $f32x) {
            let fr = x - $f32x::from_cast(x.truncatei());
            let fr = x.abs().gt(F1_23X).select(ZERO, fr);
            (vcopysign_vf_vf_vf(fr, x), vcopysign_vf_vf_vf(x - fr, x))
        }

        #[inline]
        fn atan2kf(y: $f32x, x: $f32x) -> $f32x {
            let q = vsel_vi2_vf_vi2(x, $i32x::splat(-2));
            let x = x.abs();

            let q = vsel_vi2_vf_vf_vi2_vi2(x, y, q + $i32x::splat(1), q);
            let p = x.lt(y);
            let s = p.select(-x, y);
            let mut t = x.max(y);

            let s = s / t;
            t = s * s;

            let u = $f32x::splat(0.002_823_638_962_581_753_730_773_93)
                .mul_add(t, $f32x::splat(-0.015_956_902_876_496_315_002_441_4))
                .mul_add(t, $f32x::splat(0.042_504_988_610_744_476_318_359_4))
                .mul_add(t, $f32x::splat(-0.074_890_092_015_266_418_457_031_2))
                .mul_add(t, $f32x::splat(0.106_347_933_411_598_205_566_406))
                .mul_add(t, $f32x::splat(-0.142_027_363_181_114_196_777_344))
                .mul_add(t, $f32x::splat(0.199_926_957_488_059_997_558_594))
                .mul_add(t, $f32x::splat(-0.333_331_018_686_294_555_664_062));

            let t = s.mul_add(t * u, s);
            $f32x::from_cast(q).mul_add($f32x::FRAC_PI_2, t)
        }
        #[inline]
        fn visinf2_vf_vf_vf(d: $f32x, m: $f32x) -> $f32x {
            $f32x::from_bits(vand_vm_vo32_vm(
                d.is_infinite(),
                vsignbit_vm_vf(d) | $u32x::from_bits(m),
            ))
        }

        #[inline]
        fn logkf(mut d: $f32x) -> Doubled<$f32x> {
            let m: $f32x;

            let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.lt($f32x::splat(f32::MIN));
                d = o.select(d * (F1_32X * F1_32X), d);
                let mut e = vilogb2k_vi2_vf(d * $f32x::splat(1. / 0.75));
                m = vldexp3_vf_vf_vi2(d, -e);
                e = o.select(e - $i32x::splat(64), e);
                $f32x::from_cast(e)
            }/* else {
                let mut e = vgetexp_vf_vf(d * $f32x::splat(1. / 0.75));
                e = e.eq($f32x::INFINITY).select($f32x::splat(128.), e);
                m = vgetmant_vf_vf(d);
                e
            }*/;

            let x = $f32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.square();

            let t = $f32x::splat(0.240_320_354_700_088_500_976_562)
                .mul_add(x2.0, $f32x::splat(0.285_112_679_004_669_189_453_125))
                .mul_add(x2.0, $f32x::splat(0.400_007_992_982_864_379_882_812));
            let c = Doubled::from((0.666_666_626_930_236_816_406_25, 3.691_838_612_596_143_320_843_11_e-9));

            let mut s = Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * ef;

            s = s.add_checked(x.scale($f32x::splat(2.)));
            s.add_checked(x2 * x * (x2 * t + c))
        }

        #[inline]
        fn expkf(d: Doubled<$f32x>) -> $f32x {
            let u = (d.0 + d.1) * R_LN2_F;
            let q = u.rinti();

            let mut s = d + $f32x::from_cast(q) * (-L2U_F);
            s += $f32x::from_cast(q) * (-L2L_F);

            s = s.normalize();

            let mut u = $f32x::splat(0.001_363_246_468_827_128_410_339_36)
                .mul_add(s.0, $f32x::splat(0.008_365_969_173_610_210_418_701_17))
                .mul_add(s.0, $f32x::splat(0.041_671_082_377_433_776_855_468_8))
                .mul_add(s.0, $f32x::splat(0.166_665_524_244_308_471_679_688))
                .mul_add(s.0, $f32x::splat(0.499_999_850_988_388_061_523_438));

            let mut t = s.add_checked(s.square() * u);

            t = ONE.add_checked(t);
            u = t.0 + t.1;
            u = vldexp_vf_vf_vi2(u, q);

            $f32x::from_bits(vandnot_vm_vo32_vm(
                d.0.lt($f32x::splat(-104.)),
                $u32x::from_bits(u),
            ))
        }

        #[inline]
        fn expk2f(d: Doubled<$f32x>) -> Doubled<$f32x> {
            let u = (d.0 + d.1) * R_LN2_F;
            let q = u.rinti();

            let mut s = d + $f32x::from_cast(q) * (-L2U_F);
            s += $f32x::from_cast(q) * (-L2L_F);

            let u = $f32x::splat(0.198_096_022_4_e-3)
                .mul_add(s.0, $f32x::splat(0.139_425_648_4_e-2))
                .mul_add(s.0, $f32x::splat(0.833_345_670_3_e-2))
                .mul_add(s.0, $f32x::splat(0.416_663_736_1_e-1));

            let mut t = s * u + $f32x::splat(0.166_666_659_414_234_244_790_680_580_464);
            t = s * t + HALF;
            t = s + s.square() * t;

            t = ONE.add_checked(t);

            t.0 = vldexp2_vf_vf_vi2(t.0, q);
            t.1 = vldexp2_vf_vf_vi2(t.1, q);

            t.0 = $f32x::from_bits(vandnot_vm_vo32_vm(
                d.0.lt($f32x::splat(-104.)),
                $u32x::from_bits(t.0),
            ));
            t.1 = $f32x::from_bits(vandnot_vm_vo32_vm(
                d.0.lt($f32x::splat(-104.)),
                $u32x::from_bits(t.1),
            ));

            t
        }

        #[inline]
        fn logk2f(d: Doubled<$f32x>) -> Doubled<$f32x> {
            let e = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                vilogbk_vi2_vf(d.0 * $f32x::splat(1. / 0.75))
            /*} else {
                vgetexp_vf_vf(d.0 * $f32x::splat(1. / 0.75)).rinti()
            }*/;
            let m = d.scale(vpow2i_vf_vi2(-e));

            let x = (m + $f32x::splat(-1.)) / (m + ONE);
            let x2 = x.square();

            let t = $f32x::splat(0.239_282_846_450_805_664_062_5)
                .mul_add(x2.0, $f32x::splat(0.285_182_118_415_832_519_531_25))
                .mul_add(x2.0, $f32x::splat(0.400_005_877_017_974_853_515_625))
                .mul_add(x2.0, $f32x::splat(0.666_666_686_534_881_591_796_875));

            let mut s = Doubled::new(
                $f32x::splat(0.693_147_182_464_599_609_38),
                $f32x::splat(-1.904_654_323_148_236_017_e-9),
            ) * $f32x::from_cast(e);
            s = s.add_checked(x.scale($f32x::splat(2.)));
            s.add_checked(x2 * x * t)
        }

        pub fn exp2f(d: $f32x) -> $f32x {
            let mut u = d.rint();
            let q = u.rinti();

            let s = d - u;

            u = $f32x::splat(0.153_592_089_2_e-3)
                .mul_add(s, $f32x::splat(0.133_926_270_1_e-2))
                .mul_add(s, $f32x::splat(0.961_838_476_4_e-2))
                .mul_add(s, $f32x::splat(0.555_034_726_9_e-1))
                .mul_add(s, $f32x::splat(0.240_226_447_6))
                .mul_add(s, $f32x::splat(0.693_147_182_5));

            if !cfg!(target_feature = "fma") {
                u = u.mul_adde(s, ONE);
            } else {
                u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
            }

            u = vldexp2_vf_vf_vi2(u, q);

            u = d
                .ge($f32x::splat(128.))
                .select($f32x::INFINITY, u);
            $f32x::from_bits(vandnot_vm_vo32_vm(
                d.lt($f32x::splat(-150.)),
                $u32x::from_bits(u),
            ))
        }

        pub fn log1pf(d: $f32x) -> $f32x {
            let m: $f32x;

            let dp1 = d + ONE;

            let mut s =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = dp1.lt($f32x::splat(f32::MIN));
                    let dp1 = o.select(dp1 * (F1_32X * F1_32X), dp1);
                    let e = vilogb2k_vi2_vf(dp1 * $f32x::splat(1. / 0.75));
                    let t = vldexp3_vf_vf_vi2(ONE, -e);
                    m = d.mul_add(t, t - ONE);
                    let e = o.select(e - $i32x::splat(64), e);
                    Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * $f32x::from_cast(e)
                }/* else {
                    let e = vgetexp_vf_vf(dp1, $f32x::splat(1. / 0.75));
                    let e = e.eq($f32x::INFINITY).select($f32x::splat(128.), e);
                    let t = vldexp3_vf_vf_vi2(ONE, -e.rinti());
                    m = d.mul_add(t, t - ONE);
                    Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * e
                }*/;

            let x = Doubled::new(m, ZERO) / $f32x::splat(2.).add_checked_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = $f32x::splat(0.302_729_487_4)
                .mul_add(x2, $f32x::splat(0.399_610_817_4))
                .mul_add(x2, $f32x::splat(0.666_669_488));

            s = s.add_checked(x.scale($f32x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = s.0 + s.1;

            r = d
                .gt($f32x::splat(1e+38))
                .select($f32x::INFINITY, r);
            r = $f32x::from_bits(vor_vm_vo32_vm($f32x::splat(-1.).gt(d), $u32x::from_bits(r)));
            r = d
                .eq($f32x::splat(-1.))
                .select($f32x::NEG_INFINITY, r);
            visnegzero_vo_vf(d).select($f32x::splat(-0.), r)
        }

        //

        pub fn fabsf(x: $f32x) -> $f32x {
            x.abs()
        }

        pub fn copysignf(x: $f32x, y: $f32x) -> $f32x {
            vcopysign_vf_vf_vf(x, y)
        }

        pub fn fmaxf(x: $f32x, y: $f32x) -> $f32x {
            if cfg!(target_arch = "x86_64") || cfg!(target_arch = "x86")
            /*    && !cfg!(feature = "enable_vecext")
                        && !cfg!(feature = "enable_purec")*/
            {
                y.is_nan().select(x, x.max(y))
            } else {
                y.is_nan().select(x, x.gt(y).select(x, y))
            }
        }

        pub fn fminf(x: $f32x, y: $f32x) -> $f32x {
            if cfg!(target_arch = "x86_64") || cfg!(target_arch = "x86")
            /*    && !cfg!(feature = "enable_vecext")
                        && !cfg!(feature = "enable_purec")*/
            {
                y.is_nan().select(x, x.min(y))
            } else {
                y.is_nan().select(x, y.gt(x).select(x, y))
            }
        }

        pub fn fdimf(x: $f32x, y: $f32x) -> $f32x {
            let ret = x - y;
            (ret.lt(ZERO) | x.eq(y)).select(ZERO, ret)
        }

        pub fn truncf(x: $f32x) -> $f32x {
            let fr = x - $f32x::from_cast(x.truncatei());
            (x.is_infinite() | x.abs().ge(F1_23X))
                .select(x, vcopysign_vf_vf_vf(x - fr, x))
        }

        pub fn floorf(x: $f32x) -> $f32x {
            let fr = x - $f32x::from_cast(x.truncatei());
            let fr = fr.lt(ZERO).select(fr + ONE, fr);
            (x.is_infinite() | x.abs().ge(F1_23X))
                .select(x, vcopysign_vf_vf_vf(x - fr, x))
        }

        pub fn ceilf(x: $f32x) -> $f32x {
            let fr = x - $f32x::from_cast(x.truncatei());
            let fr = fr.le(ZERO).select(fr, fr - ONE);
            (x.is_infinite() | x.abs().ge(F1_23X))
                .select(x, vcopysign_vf_vf_vf(x - fr, x))
        }

        pub fn roundf(d: $f32x) -> $f32x {
            let mut x = d + HALF;
            let fr = x - $f32x::from_cast(x.truncatei());
            x = (x.le(ZERO) & fr.eq(ZERO)).select(x - ONE, x);
            let fr = fr.lt(ZERO).select(fr + ONE, fr);
            x = d
                .eq($f32x::splat(0.499_999_970_197_677_612_3))
                .select(ZERO, x);
            (d.is_infinite() | d.abs().ge(F1_23X))
                .select(d, vcopysign_vf_vf_vf(x - fr, d))
        }

        pub fn rintf(d: $f32x) -> $f32x {
            let mut x = d + HALF;
            let isodd = ($i32x::splat(1) & x.truncatei()).eq($i32x::splat(1));
            let mut fr = x - $f32x::from_cast(x.truncatei());
            fr = (fr.lt(ZERO) | (fr.eq(ZERO) & isodd))
                .select(fr + ONE, fr);
            x = d
                .eq($f32x::splat(0.500_000_059_604_644_775_39))
                .select(ZERO, x);
            (d.is_infinite() | d.abs().ge(F1_23X))
                .select(d, vcopysign_vf_vf_vf(x - fr, d))
        }

        pub fn fmaf(mut x: $f32x, mut y: $f32x, mut z: $f32x) -> $f32x {
            let h2 = x * y + z;
            let mut q = ONE;
            let o = h2.abs().lt($f32x::splat(1e-38));
            const C0: $f32x = F1_25X;
            let c1: $f32x = C0 * C0;
            let c2: $f32x = c1 * c1;
            {
                x = o.select(x * c1, x);
                y = o.select(y * c1, y);
                z = o.select(z * c2, z);
                q = o.select(ONE / c2, q);
            }
            let o = h2.abs().gt($f32x::splat(1e+38));
            {
                x = o.select(x * (ONE / c1), x);
                y = o.select(y * (ONE / c1), y);
                z = o.select(z * (ONE / c2), z);
                q = o.select(c2, q);
            }
            let d = x.mul_as_doubled(y) + z;
            let ret = (x.eq(ZERO) | y.eq(ZERO)).select(z, d.0 + d.1);
            let mut o = z.is_infinite();
            o = vandnot_vo_vo_vo(x.is_infinite(), o);
            o = vandnot_vo_vo_vo(x.is_nan(), o);
            o = vandnot_vo_vo_vo(y.is_infinite(), o);
            o = vandnot_vo_vo_vo(y.is_nan(), o);
            let h2 = o.select(z, h2);

            o = h2.is_infinite() | h2.is_nan();

            o.select(h2, ret * q)
        }

        pub fn sqrtf(d: $f32x) -> $f32x {
        //   if cfg!(feature = "accurate_sqrt") {
                d.sqrt()
        /*    } else {
                // fall back to approximation if ACCURATE_SQRT is undefined
                u05::sqrtf(d)
            }*/
        }

        pub fn nextafterf(x: $f32x, y: $f32x) -> $f32x {
            let x = x
                .eq(ZERO)
                .select(vmulsign_vf_vf_vf(ZERO, y), x);
            let mut xi2 = $i32x::from_bits(x);
            let c = x.is_sign_negative() ^ y.ge(x);

            xi2 = c.select($i32x::splat(0) - (xi2 ^ $i32x::splat(1 << 31)), xi2);

            xi2 = x.ne(y).select(xi2 - $i32x::splat(1), xi2);

            xi2 = c.select($i32x::splat(0) - (xi2 ^ $i32x::splat(1 << 31)), xi2);

            let mut ret = $f32x::from_bits(xi2);

            ret = (ret.eq(ZERO) & x.ne(ZERO))
                .select(vmulsign_vf_vf_vf(ZERO, x), ret);

            ret = (x.eq(ZERO) & y.eq(ZERO)).select(y, ret);

            (x.is_nan() | y.is_nan()).select($f32x::NAN, ret)
        }

        pub fn frfrexpf(x: $f32x) -> $f32x {
            let x = x
                .abs()
                .lt($f32x::splat(f32::MIN))
                .select(x * F1_32X, x);

            let mut xm = $u32x::from_bits(x);
            xm &= $u32x::splat(!0x_7f80_0000_u32);
            xm |= $u32x::splat(0x_3f00_0000_u32);

            let ret = $f32x::from_bits(xm);

            let ret =
                x.is_infinite().select(vmulsign_vf_vf_vf($f32x::INFINITY, x), ret);
            x.eq(ZERO).select(x, ret)
        }

        pub fn expfrexpf(_x: $f32x) -> $i32x {
            /*
                                  x = x.abs().lt($f32x::splat(f32::MIN)).select(x * F1_63X, x);

                                  let mut ret = $i32x::from_cast($ix::from_bits(x);
                                  ret = (vsrl_vi_vi_i(ret, 20) & $ix::splat(0x7ff)) - $ix::splat(0x3fe);

                                  (x.eq(ZERO) | x.is_nan() | x.is_infinite()).select($ix::splat(0), ret)
                                  */
            $i32x::splat(0)
        }
        #[inline]
        fn vtoward0f(x: $f32x) -> $f32x {
            let t = $f32x::from_bits($i32x::from_bits(x) - $i32x::splat(1));
            x.eq(ZERO).select(ZERO, t)
        }
        #[inline]
        fn vptruncf(x: $f32x) -> $f32x {
            if cfg!(feature = "full_fp_rounding") {
                x.truncate()
            } else {
                let fr = x - $f32x::from_cast(x.truncatei());
                x.abs().ge(F1_23X).select(x, x - fr)
            }
        }

        pub fn fmodf(x: $f32x, y: $f32x) -> $f32x {
            let nu = x.abs();
            let de = y.abs();
            let s = ONE;
            let o = de.lt($f32x::splat(f32::MIN));
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
                let q =
                    ((de + de).gt(r.0) & r.0.ge(de)).select(ONE, vtoward0f(r.0) * rde);
                r = (r + vptruncf(q).mul_as_doubled(-de)).normalize();
                if r.0.lt(de).all() {
                    break;
                }
            }

            let mut ret = (r.0 + r.1) * s;
            ret = (r.0 + r.1).eq(de).select(ZERO, ret);

            ret = vmulsign_vf_vf_vf(ret, x);

            ret = nu.lt(de).select(x, ret);
            de.eq(ZERO)
                .select($f32x::NAN, ret)
        }

        //
        #[inline]
        fn sinpifk(d: $f32x) -> Doubled<$f32x> {
            let u = d * $f32x::splat(4.);
            let q = u.truncatei();
            let q = (q + ($i32x::from_bits($u32x::from_bits(q) >> 31) ^ $i32x::splat(1)))
                & $i32x::splat(!1);
            let o = (q & $i32x::splat(2)).eq($i32x::splat(2));

            let s = u - $f32x::from_cast(q);
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            //

            let u = o.select_splat(-0.243_061_180_1_e-7, 0.309_384_205_4_e-6)
                .mul_add(s, o.select_splat(0.359_057_708_e-5, -0.365_730_738_8_e-4))
                .mul_add(s, o.select_splat(-0.325_991_772_1_e-3, 0.249_039_358_5_e-2));
            let mut x = u * s + vsel_vf2_vo_f_f_f_f(
                o,
                0.015_854_343_771_934_509_277,
                4.494_005_135_403_224_281_1_e-10,
                -0.080_745_510_756_969_451_904,
                -1.337_366_533_907_693_625_8_e-9,
            );
            x = s2 * x + vsel_vf2_vo_f_f_f_f(
                o,
                -0.308_425_128_459_930_419_92,
                -9.072_833_903_073_392_227_7_e-9,
                0.785_398_185_253_143_310_55,
                -2.185_733_861_756_648_485_5_e-8,
            );

            x *= o.select_doubled(s2, Doubled::new(t, ZERO));
            x = o.select_doubled(x + ONE, x);

            let o = (q & $i32x::splat(4)).eq($i32x::splat(4));
            x.0 = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x.0),
            );
            x.1 = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x.1),
            );

            x
        }

        #[inline]
        fn cospifk(d: $f32x) -> Doubled<$f32x> {
            let u = d * $f32x::splat(4.);
            let q = u.truncatei();
            let q = (q + ($i32x::from_bits($u32x::from_bits(q) >> 31) ^ $i32x::splat(1)))
                & $i32x::splat(!1);
            let o = (q & $i32x::splat(2)).eq($i32x::splat(0));

            let s = u - $f32x::from_cast(q);
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            //

            let u = o.select_splat(-0.243_061_180_1_e-7, 0.309_384_205_4_e-6)
                .mul_add(s, o.select_splat(0.359_057_708_e-5, -0.365_730_738_8_e-4))
                .mul_add(s, o.select_splat(-0.325_991_772_1_e-3, 0.249_039_358_5_e-2));
            let mut x = u * s + vsel_vf2_vo_f_f_f_f(
                o,
                0.015_854_343_771_934_509_277,
                4.494_005_135_403_224_281_1_e-10,
                -0.080_745_510_756_969_451_904,
                -1.337_366_533_907_693_625_8_e-9,
            );
            x = s2 * x + vsel_vf2_vo_f_f_f_f(
                o,
                -0.308_425_128_459_930_419_92,
                -9.072_833_903_073_392_227_7_e-9,
                0.785_398_185_253_143_310_55,
                -2.185_733_861_756_648_485_5_e-8,
            );

            x *= o.select_doubled(s2, Doubled::new(t, ZERO));
            x = o.select_doubled(x + ONE, x);

            let o = ((q + $i32x::splat(2)) & $i32x::splat(4)).eq($i32x::splat(4));
            x.0 = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x.0),
            );
            x.1 = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x.1),
            );

            x
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        fn gammafk(a: $f32x) -> (Doubled<$f32x>, Doubled<$f32x>) {
            let mut clln = Doubled::from((1., 0.));
            let mut clld = Doubled::from((1., 0.));

            let otiny = a.abs().lt($f32x::splat(1e-30));
            let oref = a.lt(HALF);

            let x = otiny.select_doubled(Doubled::from((0., 0.)),
                oref.select_doubled(ONE.add_as_doubled(-a),
                    Doubled::new(a, ZERO),
                ),
            );

            let o0 = HALF.le(x.0) & x.0.le($f32x::splat(1.2));
            let o2 = $f32x::splat(2.3).le(x.0);

            let mut y = ((x + ONE) * x).normalize();
            y = ((x + $f32x::splat(2.)) * y).normalize();

            let o = o2 & x.0.le($f32x::splat(7.));
            clln = o.select_doubled(y, clln);

            let mut x = o.select_doubled(x + $f32x::splat(3.), x);
            let t = o2.select(x.0.recpre(), (x + o0.select_splat(-1., -2.)).normalize().0);

            let u = vsel_vf_vo_vo_f_f_f(
                o2,
                o0,
                0.000_839_498_720_672_087_279_971_000_786,
                0.943_515_777_6,
                0.110_248_955_e-3,
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    -5.171_790_908_260_592_193_293_944_22_e-5,
                    0.867_006_361_5,
                    0.816_001_993_4_e-4,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    -0.000_592_166_437_353_693_882_857_342_347,
                    0.482_670_247_6,
                    0.152_846_885_6_e-3,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    6.972_813_758_365_857_774_037_435_39_e-5,
                    -0.885_512_977_8_e-1,
                    -0.235_506_871_8_e-3,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    0.000_784_039_221_720_066_627_493_314_301,
                    0.101_382_523_8,
                    0.496_224_209_2_e-3,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    -0.000_229_472_093_621_399_176_949_318_732,
                    -0.149_340_897_8,
                    -0.119_348_801_7_e-2,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    -0.002_681_327_160_493_827_160_473_958_490,
                    0.169_750_914,
                    0.289_159_943_3_e-2,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    0.003_472_222_222_222_222_222_175_164_840,
                    -0.207_245_454_2,
                    -0.738_545_181_2_e-2,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    0.083_333_333_333_333_333_335_592_087_900,
                    0.270_587_235_7,
                    0.205_807_704_5_e-1,
                ),
            );

            y = (x + $f32x::splat(-0.5)) * logk2f(x);
            y += -x;
            y += Doubled::from(0.918_938_533_204_672_780_56_f64); // 0.5*log(2*M_PI)

            let mut z = u.mul_as_doubled(t) + o0.select_splat(-0.400_686_534_596_170_958_447_352_690_395,
                -0.673_523_028_297_382_446_749_257_758_235_e-1,
            );
            z = z * t + o0.select_splat(0.822_466_960_142_643_054_450_325_495_997,
                0.322_467_033_928_981_157_743_538_726_901,
            );
            z = z * t + o0.select_splat(-0.577_215_665_946_766_039_837_398_973_297,
                0.422_784_335_087_484_338_986_941_629_852,
            );
            z *= t;

            let mut clc = o2.select_doubled(y, z);

            clld = o2.select_doubled(u.mul_as_doubled(t) + ONE, clld);

            y = clln;

            clc = otiny.select_doubled(Doubled::from(41.588_830_833_596_718_565_03_f64), // log(2^60)
                oref.select_doubled(Doubled::<$f32x>::from(1.144_729_885_849_400_163_9_f64) + (-clc), clc),
            ); // log(M_PI)
            clln = otiny.select_doubled(Doubled::from((1., 0.)),
                oref.select_doubled(clln, clld),
            );

            if !(!oref).all() {
                let t = a
                    - F1_12X * $f32x::from_cast((a * (ONE / F1_12X)).truncatei());
                x = clld * sinpifk(t);
            }

            clld = otiny.select_doubled(Doubled::new(a * (F1_30X * F1_30X), ZERO),
                oref.select_doubled(x, y),
            );

            (clc, clln / clld)
        }

        #[inline]
        fn expm1fk(d: $f32x) -> $f32x {
            let q = (d * R_LN2_F).rinti();
            let s = $f32x::from_cast(q).mul_add(-L2U_F, d);
            let s = $f32x::from_cast(q).mul_add(-L2L_F, s);

            let u = $f32x::splat(0.000_198_527_617_612_853_646_278_381)
                .mul_add(s, $f32x::splat(0.001_393_043_552_525_341_510_772_71))
                .mul_add(s, $f32x::splat(0.008_333_360_776_305_198_669_433_59))
                .mul_add(s, $f32x::splat(0.041_666_485_369_205_474_853_515_6))
                .mul_add(s, $f32x::splat(0.166_666_671_633_720_397_949_219))
                .mul_add(s, HALF);

            let u = (s * s).mul_add(u, s);

            q.eq($i32x::splat(0)).select(
                u,
                vldexp2_vf_vf_vi2(u + ONE, q) - ONE,
            )
        }

    };
}
