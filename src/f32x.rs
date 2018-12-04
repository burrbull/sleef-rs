macro_rules! impl_math_f32 {
    ($f32x:ident, $u32x:ident, $m32x:ident, $i32x:ident) => {
        use crate::common::*;
        use doubled::*;

        const ZERO: $f32x = $f32x::splat(0.);
        const ONE: $f32x = $f32x::splat(1.);
        const F1_32X: $f32x = $f32x::splat((1u64 << 32) as f32);
        const F1_30X: $f32x = $f32x::splat((1u32 << 30) as f32);
        const F1_25X: $f32x = $f32x::splat((1u32 << 25) as f32);
        const F1_24X: $f32x = $f32x::splat((1u32 << 24) as f32);
        const F1_23X: $f32x = $f32x::splat((1u32 << 23) as f32);
        const F1_12X: $f32x = $f32x::splat((1u32 << 12) as f32);
        const F1_10X: $f32x = $f32x::splat((1u32 << 10) as f32);

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
                .select($i32x::splat(SLEEF_FP_ILOGB0), e);
            e = d.is_nan().select($i32x::splat(SLEEF_FP_ILOGBNAN), e);
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
                    * (fr.mul_add($f32x::splat(4.), vmulsign_vf_vf_vf($f32x::splat(0.5), x)))
                        .truncate();
                fr = fr
                    .abs()
                    .gt($f32x::splat(0.25))
                    .select(fr - vmulsign_vf_vf_vf($f32x::splat(0.5), x), fr);
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
            let mut x = a.mul_as_doubled(vgather_vf_p_vi2(&REMPITABSP, ex));
            let (did, mut q) = rempisubf(x.0);
            x.0 = did;
            x = x.normalize();
            let y = a.mul_as_doubled(vgather_vf_p_vi2(&REMPITABSP[1..], ex));
            x += y;
            let (did, dii) = rempisubf(x.0);
            q += dii;
            x.0 = did;
            x = x.normalize();
            let mut y = Doubled::new(
                vgather_vf_p_vi2(&REMPITABSP[2..], ex),
                vgather_vf_p_vi2(&REMPITABSP[3..], ex),
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
            let u = (d.0 + d.1) * $f32x::splat(R_LN2_F);
            let q = u.rinti();

            let mut s = d + $f32x::from_cast(q) * $f32x::splat(-L2U_F);
            s += $f32x::from_cast(q) * $f32x::splat(-L2L_F);

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
            let u = (d.0 + d.1) * $f32x::splat(R_LN2_F);
            let q = u.rinti();

            let mut s = d + $f32x::from_cast(q) * $f32x::splat(-L2U_F);
            s += $f32x::from_cast(q) * $f32x::splat(-L2L_F);

            let u = $f32x::splat(0.198_096_022_4_e-3)
                .mul_add(s.0, $f32x::splat(0.139_425_648_4_e-2))
                .mul_add(s.0, $f32x::splat(0.833_345_670_3_e-2))
                .mul_add(s.0, $f32x::splat(0.416_663_736_1_e-1));

            let mut t = s * u + $f32x::splat(0.166_666_659_414_234_244_790_680_580_464);
            t = s * t + $f32x::splat(0.5);
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
            let mut x = d + $f32x::splat(0.5);
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
            let mut x = d + $f32x::splat(0.5);
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
            let oref = a.lt($f32x::splat(0.5));

            let x = otiny.select_doubled(Doubled::from((0., 0.)),
                oref.select_doubled(ONE.add_as_doubled(-a),
                    Doubled::new(a, ZERO),
                ),
            );

            let o0 = $f32x::splat(0.5).le(x.0) & x.0.le($f32x::splat(1.2));
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
            let q = (d * $f32x::splat(R_LN2_F)).rinti();
            let s = $f32x::from_cast(q).mul_add($f32x::splat(-L2U_F), d);
            let s = $f32x::from_cast(q).mul_add($f32x::splat(-L2L_F), s);

            let u = $f32x::splat(0.000_198_527_617_612_853_646_278_381)
                .mul_add(s, $f32x::splat(0.001_393_043_552_525_341_510_772_71))
                .mul_add(s, $f32x::splat(0.008_333_360_776_305_198_669_433_59))
                .mul_add(s, $f32x::splat(0.041_666_485_369_205_474_853_515_6))
                .mul_add(s, $f32x::splat(0.166_666_671_633_720_397_949_219))
                .mul_add(s, $f32x::splat(0.5));

            let u = (s * s).mul_add(u, s);

            q.eq($i32x::splat(0)).select(
                u,
                vldexp2_vf_vf_vi2(u + ONE, q) - ONE,
            )
        }

        pub mod u05 {
            //! Functions with 0.5 ULP error bound
            use super::*;

            pub fn sincospif(d: $f32x) -> ($f32x, $f32x) {
                let u = d * $f32x::splat(4.);
                let q = u.truncatei();
                let q = (q + ($i32x::from_bits($u32x::from_bits(q) >> 31) ^ $i32x::splat(1)))
                    & $i32x::splat(!1);
                let s = u - $f32x::from_cast(q);

                let t = s;
                let s = s * s;
                let s2 = t.mul_as_doubled(t);

                //

                let u = $f32x::splat(0.309_384_205_4_e-6)
                    .mul_add(s, $f32x::splat(-0.365_730_738_8_e-4))
                    .mul_add(s, $f32x::splat(0.249_039_358_5_e-2));
                let mut x = u * s + Doubled::from((-0.080_745_510_756_969_451_904, -1.337_366_533_907_693_625_8_e-9));
                x = s2 * x + Doubled::from((0.785_398_185_253_143_310_55, -2.185_733_861_756_648_485_5_e-8));

                x *= t;
                let rx = x.0 + x.1;

                let rx = visnegzero_vo_vf(d).select($f32x::splat(-0.), rx);

                //

                let u = $f32x::splat(-0.243_061_180_1_e-7)
                    .mul_add(s, $f32x::splat(0.359_057_708_e-5))
                    .mul_add(s, $f32x::splat(-0.325_991_772_1_e-3));
                x = u * s + Doubled::from((0.015_854_343_771_934_509_277, 4.494_005_135_403_224_281_1_e-10));
                x = s2 * x + Doubled::from((-0.308_425_128_459_930_419_92, -9.072_833_903_073_392_227_7_e-9));

                x = x * s2 + ONE;
                let ry = x.0 + x.1;

                //

                let o = (q & $i32x::splat(2)).eq($i32x::splat(0));
                let mut rsin = o.select(rx, ry);
                let mut rcos = o.select(ry, rx);

                let o = (q & $i32x::splat(4)).eq($i32x::splat(4));
                rsin = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(rsin),
                );

                let o = ((q + $i32x::splat(2)) & $i32x::splat(4)).eq($i32x::splat(4));
                rcos = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(rcos),
                );

                let o = d.abs().gt($f32x::splat(1e+7));
                rsin = $f32x::from_bits(vandnot_vm_vo32_vm(o, $u32x::from_bits(rsin)));
                rcos = $f32x::from_bits(vandnot_vm_vo32_vm(o, $u32x::from_bits(rcos)));

                let o = d.is_infinite();
                rsin = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rsin)));
                rcos = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rcos)));

                (rsin, rcos)
            }

            pub fn sqrtf(d: $f32x) -> $f32x {
                let d = d.lt(ZERO).select($f32x::NAN, d);

                let o = d.lt($f32x::splat(5.293_955_920_339_377_e-23));
                let d = o.select(d * $f32x::splat(1.888_946_593_147_858_e+22), d);
                let q = o.select(
                    $f32x::splat(7.275_957_614_183_426_e-12 * 0.5),
                    $f32x::splat(0.5),
                );

                let o = d.gt($f32x::splat(1.844_674_407_370_955_2_e+19));
                let d = o.select(d * $f32x::splat(5.421_010_862_427_522_e-20), d);
                let q = o.select($f32x::splat(4_294_967_296.0 * 0.5), q);

                let mut x = $f32x::from_bits(
                    $i32x::splat(0x_5f37_5a86)
                        - $i32x::from_bits(
                            $u32x::from_bits(d + $f32x::splat(1e-45)) >> 1,
                        ),
                );

                x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
                x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
                x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
                x *= d;

                let d2 = (d + x.mul_as_doubled(x)) * x.recpre_as_doubled();

                x = (d2.0 + d2.1) * q;

                x = d.eq($f32x::INFINITY).select($f32x::INFINITY, x);
                d.eq(ZERO).select(d, x)
            }

            pub fn hypotf(x: $f32x, y: $f32x) -> $f32x {
                let x = x.abs();
                let y = y.abs();
                let min = x.min(y);
                let n = min;
                let max = x.max(y);
                let d = max;

                let o = max.lt($f32x::splat(f32::MIN));
                let n = o.select(n * F1_24X, n);
                let d = o.select(d * F1_24X, d);

                let t = Doubled::new(n, ZERO) / Doubled::new(d, ZERO);
                let t = (t.square() + ONE).sqrt() * max;
                let mut ret = t.0 + t.1;
                ret = ret.is_nan().select($f32x::INFINITY, ret);
                ret = min.eq(ZERO).select(max, ret);
                ret = (x.is_nan() | y.is_nan()).select($f32x::NAN, ret);
                (x.eq($f32x::INFINITY) | y.eq($f32x::INFINITY))
                    .select($f32x::INFINITY, ret)
            }

            pub fn xsinpif(d: $f32x) -> $f32x {
                let x = sinpifk(d);
                let mut r = x.0 + x.1;

                r = visnegzero_vo_vf(d).select($f32x::splat(-0.), r);
                r = $f32x::from_bits(vandnot_vm_vo32_vm(
                    d.abs().gt($f32x::splat(TRIGRANGEMAX4_F)),
                    $u32x::from_bits(r),
                ));
                $f32x::from_bits(vor_vm_vo32_vm(d.is_infinite(), $u32x::from_bits(r)))
            }

            pub fn cospif(d: $f32x) -> $f32x {
                let x = cospifk(d);
                let r = x.0 + x.1;

                let r = d
                    .abs()
                    .gt($f32x::splat(TRIGRANGEMAX4_F))
                    .select(ONE, r);
                $f32x::from_bits(vor_vm_vo32_vm(d.is_infinite(), $u32x::from_bits(r)))
            }

        }

        pub mod u10 {
            //! Functions with 1.0 ULP error bound
            use super::*;

            pub fn sinf(d: $f32x) -> $f32x {
                let mut q: $i32x;
                let mut s: Doubled<$f32x>;

                if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                    let u = (d * $f32x::FRAC_1_PI).rint();
                    q = u.rinti();
                    let v = u.mul_add($f32x::splat(-PI_A2_F), d);
                    s = v.add_as_doubled(u * $f32x::splat(-PI_B2_F));
                    s = s.add_checked(u * $f32x::splat(-PI_C2_F));
                } else {
                    let (mut dfidf, dfii) = rempif(d);
                    q = dfii & $i32x::splat(3);
                    q = q + q + dfidf
                        .0
                        .gt(ZERO)
                        .select($i32x::splat(2), $i32x::splat(1));
                    q >>= 2;
                    let o = (dfii & $i32x::splat(1)).eq($i32x::splat(1));
                    let mut x = Doubled::new(
                        vmulsign_vf_vf_vf($f32x::splat(3.141_592_741_012_573_242_2 * -0.5), dfidf.0),
                        vmulsign_vf_vf_vf($f32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5), dfidf.0),
                    );
                    x = dfidf + x;
                    dfidf = o.select_doubled(x, dfidf);
                    s = dfidf.normalize();

                    s.0 = $f32x::from_bits(vor_vm_vo32_vm(
                        d.is_infinite() | d.is_nan(),
                        $u32x::from_bits(s.0),
                    ));
                }

                let t = s;
                let s = s.square();

                let mut u = $f32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                    .mul_add(s.0, $f32x::splat(-0.000_198_106_907_191_686_332_225_8))
                    .mul_add(s.0, $f32x::splat(0.008_333_078_585_565_090_179_443_36));

                let x = ONE.add_checked(
                    $f32x::splat(-0.166_666_597_127_914_428_710_938).add_checked_as_doubled(u * s.0) * s,
                );

                u = t.mul_as_f(x);

                u = $f32x::from_bits(
                    vand_vm_vo32_vm(
                        (q & $i32x::splat(1)).eq($i32x::splat(1)),
                        $u32x::from_bits($f32x::splat(-0.)),
                    ) ^ $u32x::from_bits(u),
                );

                visnegzero_vo_vf(d).select(d, u)
            }

            pub fn cosf(d: $f32x) -> $f32x {
                let mut q: $i32x;
                let mut s: Doubled<$f32x>;

                if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                    let dq = (d.mul_add($f32x::FRAC_1_PI, $f32x::splat(-0.5)))
                        .rint()
                        .mul_add($f32x::splat(2.), ONE);
                    q = dq.rinti();
                    s = d.add_as_doubled(dq * $f32x::splat(-PI_A2_F * 0.5));
                    s += dq * $f32x::splat(-PI_B2_F * 0.5);
                    s += dq * $f32x::splat(-PI_C2_F * 0.5);
                } else {
                    let (mut dfidf, dfii) = rempif(d);
                    q = dfii & $i32x::splat(3);
                    q = q + q + dfidf
                        .0
                        .gt(ZERO)
                        .select($i32x::splat(8), $i32x::splat(7));
                    q >>= 1;
                    let o = (dfii & $i32x::splat(1)).eq($i32x::splat(0));
                    let y = dfidf
                        .0
                        .gt(ZERO)
                        .select(ZERO, $f32x::splat(-1.));
                    let mut x = Doubled::new(
                        vmulsign_vf_vf_vf($f32x::splat(3.141_592_741_012_573_242_2 * -0.5), y),
                        vmulsign_vf_vf_vf($f32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5), y),
                    );
                    x = dfidf + x;
                    dfidf = o.select_doubled(x, dfidf);
                    s = dfidf.normalize();

                    s.0 = $f32x::from_bits(vor_vm_vo32_vm(
                        d.is_infinite() | d.is_nan(),
                        $u32x::from_bits(s.0),
                    ));
                }

                let t = s;
                s = s.square();

                let u = $f32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                    .mul_add(s.0, $f32x::splat(-0.000_198_106_907_191_686_332_225_8))
                    .mul_add(s.0, $f32x::splat(0.008_333_078_585_565_090_179_443_36));

                let x = ONE.add_checked(
                    $f32x::splat(-0.166_666_597_127_914_428_710_938).add_checked_as_doubled(u * s.0) * s,
                );

                let u = t.mul_as_f(x);

                $f32x::from_bits(
                    vand_vm_vo32_vm(
                        (q & $i32x::splat(2)).eq($i32x::splat(0)),
                        $u32x::from_bits($f32x::splat(-0.)),
                    ) ^ $u32x::from_bits(u),
                )
            }

            pub fn sincosf(d: $f32x) -> ($f32x, $f32x) {
                let q: $i32x;
                let mut s: Doubled<$f32x>;

                if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                    let u = (d * $f32x::FRAC_2_PI).rint();
                    q = u.rinti();
                    let v = u.mul_add($f32x::splat(-PI_A2_F * 0.5), d);
                    s = v.add_as_doubled(u * $f32x::splat(-PI_B2_F * 0.5));
                    s = s.add_checked(u * $f32x::splat(-PI_C2_F * 0.5));
                } else {
                    let (dfidf, dfii) = rempif(d);
                    q = dfii;
                    s = dfidf;
                    let o = d.is_infinite() | d.is_nan();
                    s.0 = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(s.0)));
                }

                let t = s;

                s.0 = s.square_as_f();

                let u = $f32x::splat(-0.000_195_169_282_960_705_459_117_889)
                    .mul_add(s.0, $f32x::splat(0.008_332_157_507_538_795_471_191_41))
                    .mul_add(s.0, $f32x::splat(-0.166_666_537_523_269_653_320_312))
                    * (s.0 * t.0);

                let x = t.add_checked(u);
                let rx = x.0 + x.1;

                let rx = visnegzero_vo_vf(d).select($f32x::splat(-0.), rx);

                let u = $f32x::splat(-2.718_118_423_672_422_068_193_55_e-7)
                    .mul_add(s.0, $f32x::splat(2.479_904_469_510_074_704_885_48_e-5))
                    .mul_add(s.0, $f32x::splat(-0.001_388_887_874_782_085_418_701_17))
                    .mul_add(s.0, $f32x::splat(0.041_666_664_183_139_801_025_390_6))
                    .mul_add(s.0, $f32x::splat(-0.5));

                let x = ONE.add_checked(s.0.mul_as_doubled(u));
                let ry = x.0 + x.1;

                let o = (q & $i32x::splat(1)).eq($i32x::splat(0));
                let mut rsin = o.select(rx, ry);
                let mut rcos = o.select(ry, rx);

                let o = (q & $i32x::splat(2)).eq($i32x::splat(2));
                rsin = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(rsin),
                );

                let o = ((q + $i32x::splat(1)) & $i32x::splat(2)).eq($i32x::splat(2));
                rcos = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(rcos),
                );

                (rsin, rcos)
            }


            pub fn tanf(d: $f32x) -> $f32x {
                let q: $i32x;

                let mut s = if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                    let u = (d * $f32x::FRAC_2_PI).rint();
                    q = u.rinti();
                    let v = u.mul_add($f32x::splat(-PI_A2_F * 0.5), d);
                    v.add_as_doubled(u * $f32x::splat(-PI_B2_F * 0.5))
                        .add_checked(u * $f32x::splat(-PI_C2_F * 0.5))
                } else {
                    let (dfidf, dfii) = rempif(d);
                    q = dfii;
                    let o = d.is_infinite() | d.is_nan();
                    Doubled::new(
                        $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(dfidf.0))),
                        $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(dfidf.1))),
                    )
                };

                let o = (q & $i32x::splat(1)).eq($i32x::splat(1));
                let n = vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.)));
                s.0 = $f32x::from_bits($u32x::from_bits(s.0) ^ n);
                s.1 = $f32x::from_bits($u32x::from_bits(s.1) ^ n);

                let t = s;
                s = s.square();
                s = s.normalize();

                let u = $f32x::splat(0.004_466_364_625_841_379_165_649_41)
                    .mul_add(s.0, $f32x::splat(-8.392_018_207_814_544_439_315_8_e-5))
                    .mul_add(s.0, $f32x::splat(0.010_963_924_229_145_050_048_828_1))
                    .mul_add(s.0, $f32x::splat(0.021_236_030_384_898_185_729_980_5))
                    .mul_add(s.0, $f32x::splat(0.054_068_714_380_264_282_226_562_5));

                let mut x = $f32x::splat(0.133_325_666_189_193_725_585_938).add_checked_as_doubled(u * s.0);
                x = ONE
                    .add_checked($f32x::splat(0.333_333_611_488_342_285_156_25).add_checked(s * x) * s);
                x = t * x;

                x = o.select_doubled(x.recpre(), x);

                let u = x.0 + x.1;

                visnegzero_vo_vf(d).select(d, u)
            }


            //
            #[inline]
            fn atan2kf_u1(y: Doubled<$f32x>, mut x: Doubled<$f32x>) -> Doubled<$f32x> {
                let q =
                    vsel_vi2_vf_vf_vi2_vi2(x.0, ZERO, $i32x::splat(-2), $i32x::splat(0));
                let p = x.0.lt(ZERO);
                let r = vand_vm_vo32_vm(p, $u32x::from_bits($f32x::splat(-0.)));
                x.0 = $f32x::from_bits($u32x::from_bits(x.0) ^ r);
                x.1 = $f32x::from_bits($u32x::from_bits(x.1) ^ r);

                let q = vsel_vi2_vf_vf_vi2_vi2(x.0, y.0, q + $i32x::splat(1), q);
                let p = x.0.lt(y.0);
                let s = p.select_doubled(-x, y);
                let mut t = p.select_doubled(y, x);

                let s = s / t;
                t = s.square();
                t = t.normalize();

                let u = $f32x::splat(-0.001_763_979_089_446_365_833_282_47)
                    .mul_add(t.0, $f32x::splat(0.010_790_090_076_625_347_137_451_2))
                    .mul_add(t.0, $f32x::splat(-0.030_956_460_162_997_245_788_574_2))
                    .mul_add(t.0, $f32x::splat(0.057_736_508_548_259_735_107_421_9))
                    .mul_add(t.0, $f32x::splat(-0.083_895_072_340_965_270_996_093_8))
                    .mul_add(t.0, $f32x::splat(0.109_463_557_600_975_036_621_094))
                    .mul_add(t.0, $f32x::splat(-0.142_626_821_994_781_494_140_625))
                    .mul_add(t.0, $f32x::splat(0.199_983_194_470_405_578_613_281));

                t *= $f32x::splat(-0.333_332_866_430_282_592_773_438).add_checked_as_doubled(u * t.0);
                t = s * ONE.add_checked(t);
                (Doubled::from((1.570_796_370_506_286_621_1, -4.371_138_828_673_792_886_5_e-8)) * $f32x::from_cast(q))
                    .add_checked(t)
            }

            pub fn atan2f(mut y: $f32x, mut x: $f32x) -> $f32x {
                let o = x.abs().lt($f32x::splat(2.938_737_278_354_183_094_7_e-39)); // nexttowardf((1.0 / FLT_MAX), 1)
                x = o.select(x * F1_24X, x);
                y = o.select(y * F1_24X, y);

                let d = atan2kf_u1(
                    Doubled::new(y.abs(), ZERO),
                    Doubled::new(x, ZERO),
                );
                let mut r = d.0 + d.1;

                r = vmulsign_vf_vf_vf(r, x);
                r = (x.is_infinite() | x.eq(ZERO)).select(
                    $f32x::FRAC_PI_2
                        - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::FRAC_PI_2, x)),
                    r,
                );
                r = y.is_infinite().select(
                    $f32x::FRAC_PI_2
                        - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::FRAC_PI_4, x)),
                    r,
                );
                r = y.eq(ZERO).select(
                    $f32x::from_bits(vand_vm_vo32_vm(
                        x.is_sign_negative(),
                        $u32x::from_bits($f32x::PI),
                    )),
                    r,
                );

                $f32x::from_bits(vor_vm_vo32_vm(
                    x.is_nan() | y.is_nan(),
                    $u32x::from_bits(vmulsign_vf_vf_vf(r, y)),
                ))
            }

            pub fn asinf(d: $f32x) -> $f32x {
                let o = d.abs().lt($f32x::splat(0.5));
                let x2 = o.select(d * d, (ONE - d.abs()) * $f32x::splat(0.5));
                let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
                x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

                let u = $f32x::splat(0.419_745_482_5_e-1)
                    .mul_add(x2, $f32x::splat(0.242_404_602_5_e-1))
                    .mul_add(x2, $f32x::splat(0.454_742_386_9_e-1))
                    .mul_add(x2, $f32x::splat(0.749_502_927_1_e-1))
                    .mul_add(x2, $f32x::splat(0.166_667_729_6))
                    * (x2 * x.0);

                let y = Doubled::from((3.141_592_741_012_573_242_2 / 4., -8.742_277_657_347_585_773_1_e-8 / 4.))
                    .sub_checked(x)
                    .sub_checked(u);

                let r = o.select(u + x.0, (y.0 + y.1) * $f32x::splat(2.));
                vmulsign_vf_vf_vf(r, d)
            }

            pub fn acosf(d: $f32x) -> $f32x {
                let o = d.abs().lt($f32x::splat(0.5));
                let x2 = o.select(d * d, (ONE - d.abs()) * $f32x::splat(0.5));

                let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
                x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

                let u = $f32x::splat(0.419_745_482_5_e-1)
                    .mul_add(x2, $f32x::splat(0.242_404_602_5_e-1))
                    .mul_add(x2, $f32x::splat(0.454_742_386_9_e-1))
                    .mul_add(x2, $f32x::splat(0.749_502_927_1_e-1))
                    .mul_add(x2, $f32x::splat(0.166_667_729_6))
                    * (x2 * x.0);

                let mut y = Doubled::from((3.141_592_741_012_573_242_2 / 2., -8.742_277_657_347_585_773_1_e-8 / 2.))
                    .sub_checked(vmulsign_vf_vf_vf(x.0, d).add_checked_as_doubled(vmulsign_vf_vf_vf(u, d)));
                x = x.add_checked(u);

                y = o.select_doubled(y, x.scale($f32x::splat(2.)));

                y = vandnot_vo_vo_vo(o, d.lt(ZERO)).select_doubled(Doubled::from((3.141_592_741_012_573_242_2, -8.742_277_657_347_585_773_1_e-8)).sub_checked(y),
                    y,
                );

                y.0 + y.1
            }

            pub fn atanf(d: $f32x) -> $f32x {
                let d2 = atan2kf_u1(Doubled::new(d.abs(), ZERO), Doubled::from((1., 0.)));
                let mut r = d2.0 + d2.1;
                r = d.is_infinite().select($f32x::splat(1.570_796_326_794_896_557_998_982), r);
                vmulsign_vf_vf_vf(r, d)
            }


            pub fn expf(d: $f32x) -> $f32x {
                let q = (d * $f32x::splat(R_LN2_F)).rinti();

                let s = $f32x::from_cast(q).mul_add($f32x::splat(-L2U_F), d);
                let s = $f32x::from_cast(q).mul_add($f32x::splat(-L2L_F), s);

                let mut u = $f32x::splat(0.000_198_527_617_612_853_646_278_381)
                    .mul_add(s, $f32x::splat(0.001_393_043_552_525_341_510_772_71))
                    .mul_add(s, $f32x::splat(0.008_333_360_776_305_198_669_433_59))
                    .mul_add(s, $f32x::splat(0.041_666_485_369_205_474_853_515_6))
                    .mul_add(s, $f32x::splat(0.166_666_671_633_720_397_949_219))
                    .mul_add(s, $f32x::splat(0.5));

                u = ONE + (s * s).mul_add(u, s);

                u = vldexp2_vf_vf_vi2(u, q);

                u = $f32x::from_bits(vandnot_vm_vo32_vm(
                    d.lt($f32x::splat(-104.)),
                    $u32x::from_bits(u),
                ));
                $f32x::splat(100.)
                    .lt(d)
                    .select($f32x::INFINITY, u)
            }

            pub fn cbrtf(mut d: $f32x) -> $f32x {
                let mut q2 = Doubled::from((1., 0.));

                /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                    let s = d;
                }*/
                let e = vilogbk_vi2_vf(d.abs()) + $i32x::splat(1);
                d = vldexp2_vf_vf_vi2(d, -e);

                let t = $f32x::from_cast(e) + $f32x::splat(6144.);
                let qu = (t * $f32x::splat(1. / 3.)).truncatei();
                let re = (t - $f32x::from_cast(qu) * $f32x::splat(3.)).truncatei();

                q2 = re.eq($i32x::splat(1)).select_doubled(Doubled::from((1.259_921_073_913_574_218_8, -2.401_870_169_421_727_041_5_e-8)),
                    q2,
                );
                q2 = re.eq($i32x::splat(2)).select_doubled(Doubled::from((1.587_401_032_447_814_941_4, 1.952_038_530_816_935_235_6_e-8)),
                    q2,
                );

                q2.0 = vmulsign_vf_vf_vf(q2.0, d);
                q2.1 = vmulsign_vf_vf_vf(q2.1, d);
                d = d.abs();

                let mut x = $f32x::splat(-0.601_564_466_953_277_587_890_625)
                    .mul_add(d, $f32x::splat(2.820_889_234_542_846_679_687_5))
                    .mul_add(d, $f32x::splat(-5.532_182_216_644_287_109_375))
                    .mul_add(d, $f32x::splat(5.898_262_500_762_939_453_125))
                    .mul_add(d, $f32x::splat(-3.809_541_702_270_507_812_5))
                    .mul_add(d, $f32x::splat(2.224_125_623_703_002_929_687_5));

                let mut y = x * x;
                y = y * y;
                x -= vmlanp_vf_vf_vf_vf(d, y, x) * $f32x::splat(-1. / 3.);

                let mut z = x;

                let mut u = x.mul_as_doubled(x);
                u = u * u;
                u *= d;
                u += -x;
                y = u.0 + u.1;

                y = $f32x::splat(-2. / 3.) * y * z;
                let mut v = z.mul_as_doubled(z) + y;
                v *= d;
                v *= q2;
                z = vldexp2_vf_vf_vi2(v.0 + v.1, qu - $i32x::splat(2048));

                z = d.is_infinite().select(vmulsign_vf_vf_vf($f32x::INFINITY, q2.0), z);
                z = d
                    .eq(ZERO)
                    .select($f32x::from_bits(vsignbit_vm_vf(q2.0)), z);

                /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                    z = s.is_infinite().select(vmulsign_vf_vf_vf($f32x::INFINITY, s), z);
                    z = s
                        .eq(ZERO)
                        .select(vmulsign_vf_vf_vf(ZERO, s), z);
                }*/

                z
            }


            pub fn logf(mut d: $f32x) -> $f32x {
                let m: $f32x;

                let mut s = /*if !cfg!(feature = "enable_avx512f")
                    && !cfg!(feature = "enable_avx512fnofma")*/
                {
                    let o = d.lt($f32x::splat(f32::MIN));
                    d = o.select(d * (F1_32X * F1_32X), d);
                    let mut e = vilogb2k_vi2_vf(d * $f32x::splat(1. / 0.75));
                    m = vldexp3_vf_vf_vi2(d, -e);
                    e = o.select(e - $i32x::splat(64), e);
                    Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * $f32x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vf_vf(d * $f32x::splat(1. / 0.75));
                    e = e.eq($f32x::INFINITY).select($f32x::splat(128.), e);
                    m = vgetmant_vf_vf(d);
                    Doubled::from((0.693_147_182_464_599_609_38, -1.904_654_323_148_236_017_e-9)) * e
                }*/;

                let x = $f32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f32x::splat(0.302_729_487_4)
                    .mul_add(x2, $f32x::splat(0.399_610_817_4))
                    .mul_add(x2, $f32x::splat(0.666_669_488));

                s = s.add_checked(x.scale($f32x::splat(2.)));
                s = s.add_checked(x2 * x.0 * t);

                let r = s.0 + s.1;

                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    let r = d.eq($f32x::INFINITY).select($f32x::INFINITY, r);
                    let r =
                        (d.lt(ZERO) | d.is_nan()).select($f32x::NAN, r);
                    d.eq(ZERO)
                        .select($f32x::NEG_INFINITY, r)
                /*} else {
                    vfixup_vf_vf_vf_vi2_i(
                        r,
                        d,
                        $i32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                        0,
                    )
                }*/
            }


            pub fn powf(x: $f32x, y: $f32x) -> $f32x {
                if true {
                    let yisint = y.truncate().eq(y) | y.abs().gt(F1_24X);
                    let yisodd = (y.truncatei() & $i32x::splat(1)).eq($i32x::splat(1))
                        & yisint
                        & y.abs().lt(F1_24X);

                    #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
                    {
                        let yisodd = vandnot_vm_vo32_vm(y.is_infinite(), yisodd);
                    }

                    let mut result = expkf(logkf(x.abs()) * y);

                    result = result.is_nan().select($f32x::INFINITY, result);

                    result *= x.gt(ZERO).select(
                        ONE,
                        yisint.select(
                            yisodd.select($f32x::splat(-1.), ONE),
                            $f32x::NAN,
                        ),
                    );

                    let efx = vmulsign_vf_vf_vf(x.abs() - ONE, y);

                    result = y.is_infinite().select(
                        $f32x::from_bits(vandnot_vm_vo32_vm(
                            efx.lt(ZERO),
                            $u32x::from_bits(
                                efx.eq(ZERO)
                                    .select(ONE, $f32x::INFINITY),
                            ),
                        )),
                        result,
                    );

                    result = (x.is_infinite() | x.eq(ZERO)).select(
                        yisodd.select(vsign_vf_vf(x), ONE) * $f32x::from_bits(
                            vandnot_vm_vo32_vm(
                                x.eq(ZERO).select(-y, y).lt(ZERO),
                                $u32x::from_bits($f32x::INFINITY),
                            ),
                        ),
                        result,
                    );

                    result = $f32x::from_bits(vor_vm_vo32_vm(
                        x.is_nan() | y.is_nan(),
                        $u32x::from_bits(result),
                    ));

                    (y.eq(ZERO) | x.eq(ONE))
                        .select(ONE, result)
                } else {
                    expkf(logkf(x) * y)
                }
            }


            pub fn sinhf(x: $f32x) -> $f32x {
                let mut y = x.abs();
                let d = expk2f(Doubled::new(y, ZERO));
                let d = d.sub_checked(d.recpre());
                y = (d.0 + d.1) * $f32x::splat(0.5);

                y = (x.abs().gt($f32x::splat(89.)) | y.is_nan())
                    .select($f32x::INFINITY, y);
                y = vmulsign_vf_vf_vf(y, x);
                $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
            }

            pub fn coshf(x: $f32x) -> $f32x {
                let mut y = x.abs();
                let d = expk2f(Doubled::new(y, ZERO));
                let d = d.add_checked(d.recpre());
                y = (d.0 + d.1) * $f32x::splat(0.5);

                y = (x.abs().gt($f32x::splat(89.)) | y.is_nan())
                    .select($f32x::INFINITY, y);
                $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
            }

            pub fn tanhf(x: $f32x) -> $f32x {
                let mut y = x.abs();
                let d = expk2f(Doubled::new(y, ZERO));
                let e = d.recpre();
                let d = d.add_checked(-e) / d.add_checked(e);
                y = d.0 + d.1;

                y = (x.abs().gt($f32x::splat(8.664_339_742)) | y.is_nan())
                    .select(ONE, y);
                y = vmulsign_vf_vf_vf(y, x);
                $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
            }


            pub fn asinhf(x: $f32x) -> $f32x {
                let mut y = x.abs();
                let o = y.gt(ONE);

                let mut d = o.select_doubled(x.recpre_as_doubled(), Doubled::new(y, ZERO));
                d = (d.square() + ONE).sqrt();
                d = o.select_doubled(d * y, d);

                d = logk2f((d + x).normalize());
                y = d.0 + d.1;

                y = (x.abs().gt($f32x::splat(SQRT_FLT_MAX)) | y.is_nan())
                    .select(vmulsign_vf_vf_vf($f32x::INFINITY, x), y);
                y = $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)));
                visnegzero_vo_vf(x).select($f32x::splat(-0.), y)
            }

            pub fn acoshf(x: $f32x) -> $f32x {
                let d = logk2f(
                    x.add_as_doubled(ONE).sqrt() * x.add_as_doubled($f32x::splat(-1.)).sqrt() + x,
                );
                let mut y = d.0 + d.1;

                y = (x.abs().gt($f32x::splat(SQRT_FLT_MAX)) | y.is_nan())
                    .select($f32x::INFINITY, y);

                y = $f32x::from_bits(vandnot_vm_vo32_vm(
                    x.eq(ONE),
                    $u32x::from_bits(y),
                ));

                y = $f32x::from_bits(vor_vm_vo32_vm(x.lt(ONE), $u32x::from_bits(y)));
                $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
            }

            pub fn atanhf(x: $f32x) -> $f32x {
                let mut y = x.abs();
                let d = logk2f(ONE.add_as_doubled(y) / ONE.add_as_doubled(-y));
                y = $f32x::from_bits(vor_vm_vo32_vm(
                    y.gt(ONE),
                    $u32x::from_bits(y.eq(ONE).select(
                        $f32x::INFINITY,
                        (d.0 + d.1) * $f32x::splat(0.5),
                    )),
                ));

                y = $f32x::from_bits(vor_vm_vo32_vm(
                    x.is_infinite() | y.is_nan(),
                    $u32x::from_bits(y),
                ));
                y = vmulsign_vf_vf_vf(y, x);
                $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
            }


            pub fn exp10f(d: $f32x) -> $f32x {
                let mut u = (d * $f32x::splat(LOG10_2_F)).rint();
                let q = u.rinti();

                let s = u.mul_add($f32x::splat(-L10U_F), d);
                let s = u.mul_add($f32x::splat(-L10L_F), s);

                u = $f32x::splat(0.206_400_498_7)
                    .mul_add(s, $f32x::splat(0.541_787_743_6))
                    .mul_add(s, $f32x::splat(0.117_128_682_1_e+1))
                    .mul_add(s, $f32x::splat(0.203_465_604_8_e+1))
                    .mul_add(s, $f32x::splat(0.265_094_876_3_e+1))
                    .mul_add(s, $f32x::splat(0.230_258_512_5_e+1));

                if !cfg!(target_feature = "fma") {
                    u = u.mul_adde(s, ONE);
                } else {
                    u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
                }

                u = vldexp2_vf_vf_vi2(u, q);

                u = d
                    .gt($f32x::splat(38.531_839_419_103_623_894_138_7))
                    .select($f32x::INFINITY, u);
                $f32x::from_bits(vandnot_vm_vo32_vm(
                    d.lt($f32x::splat(-50.)),
                    $u32x::from_bits(u),
                ))
            }

            pub fn expm1f(a: $f32x) -> $f32x {
                let d = expk2f(Doubled::new(a, ZERO)) + $f32x::splat(-1.);
                let mut x = d.0 + d.1;
                x = a
                    .gt($f32x::splat(88.722_831_726_074_218_75))
                    .select($f32x::INFINITY, x);
                x = a
                    .lt($f32x::splat(-16.635_532_333_438_687_426_013_570))
                    .select($f32x::splat(-1.), x);
                visnegzero_vo_vf(a).select($f32x::splat(-0.), x)
            }

            pub fn log10f(mut d: $f32x) -> $f32x {
                let m: $f32x;

                let mut s =
                    /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                        let o = d.lt($f32x::splat(f32::MIN));
                        d = o.select(d * (F1_32X * F1_32X), d);
                        let mut e = vilogb2k_vi2_vf(d * $f32x::splat(1. / 0.75));
                        m = vldexp3_vf_vf_vi2(d, -e);
                        e = o.select(e - $i32x::splat(64), e);
                        Doubled::from((0.301_030_01, -1.432_098_889_e-8)) * $f32x::from_cast(e)
                    }/* else {
                        let mut e = vgetexp_vf_vf(d * $f32x::splat(1. / 0.75));
                        e = e.eq($f32x::INFINITY).select($f32x::splat(128.), e);
                        m = vgetmant_vf_vf(d);
                        Doubled::from((0.301_030_01, -1.432_098_889_e-8)) * e
                    }*/;

                let x = $f32x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f32x::splat(0.131_428_986_8)
                    .mul_add(x2, $f32x::splat(0.173_549_354_1))
                    .mul_add(x2, $f32x::splat(0.289_530_962_7));

                s = s.add_checked(x * Doubled::from((0.868_588_984, -2.170_757_285_e-8)));
                s = s.add_checked(x2 * x.0 * t);

                let mut r = s.0 + s.1;

                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    r = d.eq($f32x::INFINITY).select($f32x::INFINITY, r);
                    r = (d.lt(ZERO) | d.is_nan()).select($f32x::NAN, r);
                    d.eq(ZERO)
                        .select($f32x::NEG_INFINITY, r)
                /*} else {
                    vfixup_vf_vf_vf_vi2_i(
                        r,
                        d,
                        $i32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                        0,
                    )
                }*/
            }

            pub fn log2f(mut d: $f32x) -> $f32x {
                let m: $f32x;

                let ef =
                    /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
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
                let x2 = x.0 * x.0;

                let t = $f32x::splat(0.437_455_028_3)
                    .mul_add(x2, $f32x::splat(0.576_479_017_7))
                    .mul_add(x2, $f32x::splat(0.961_801_290_512));
                let mut s = ef + x * Doubled::from((2.885_390_043_258_666_992_2, 3.273_447_448_356_848_861_6_e-8));
                s += x2 * x.0 * t;

                let mut r = s.0 + s.1;

                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    r = d.eq($f32x::INFINITY).select($f32x::INFINITY, r);
                    r = (d.lt(ZERO) | d.is_nan()).select($f32x::NAN, r);
                    d.eq(ZERO)
                        .select($f32x::NEG_INFINITY, r)
                /*} else {
                    vfixup_vf_vf_vf_vi2_i(
                        r,
                        d,
                        $i32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                        0,
                    )
                }*/
            }



            pub fn tgammaf(a: $f32x) -> $f32x {
                let (da, db) = gammafk(a);
                let y = expk2f(da) * db;
                let r = y.0 + y.1;

                let o = a.eq($f32x::NEG_INFINITY)
                    | (a.lt(ZERO) & a.is_integer())
                    | (a.is_finite() & a.lt(ZERO) & r.is_nan());
                let r = o.select($f32x::NAN, r);

                let o = (a.eq($f32x::INFINITY) | a.is_finite())
                    & a.ge($f32x::splat(-f32::MIN))
                    & (a.eq(ZERO) | a.gt($f32x::splat(36.)) | r.is_nan());
                o.select(vmulsign_vf_vf_vf($f32x::INFINITY, a), r)
            }

            pub fn lgammaf(a: $f32x) -> $f32x {
                let (da, db) = gammafk(a);
                let y = da + logk2f(db.abs());
                let r = y.0 + y.1;

                let o = a.is_infinite()
                    | ((a.le(ZERO) & a.is_integer())
                        | (a.is_finite() & r.is_nan()));
                o.select($f32x::INFINITY, r)
            }

            /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
            pub fn erff(a: $f32x) -> $f32x {
                let s = a;

                let a = a.abs();
                let o0 = a.lt($f32x::splat(1.1));
                let o1 = a.lt($f32x::splat(2.4));
                let o2 = a.lt($f32x::splat(4.));
                let u = o0.select(a * a, a);

                let t =
                    vsel_vf_vo_vo_f_f_f(o0, o1, 0.708_929_219_4_e-4, -0.179_266_789_9_e-4, -0.949_575_769_5_e-5)
                        .mul_add(
                            u,
                            vsel_vf_vo_vo_f_f_f(
                                o0,
                                o1,
                                -0.776_831_118_9_e-3,
                                0.393_763_301_e-3,
                                0.248_146_592_6_e-3,
                            ),
                        ).mul_add(
                            u,
                            vsel_vf_vo_vo_f_f_f(
                                o0,
                                o1,
                                0.515_946_373_3_e-2,
                                -0.394_918_117_7_e-2,
                                -0.291_817_681_9_e-2,
                            ),
                        ).mul_add(
                            u,
                            vsel_vf_vo_vo_f_f_f(
                                o0,
                                o1,
                                -0.268_378_127_4_e-1,
                                0.244_547_464_e-1,
                                0.205_970_667_3_e-1,
                            ),
                        ).mul_add(
                            u,
                            vsel_vf_vo_vo_f_f_f(
                                o0,
                                o1,
                                0.112_831_801_2,
                                -0.107_099_615,
                                -0.990_189_984_4_e-1,
                            ),
                        );
                let mut d = t.mul_as_doubled(u);
                d += vsel_vf2_vo_vo_d_d_d(
                    o0,
                    o1,
                    -0.376_125_876_000_657_465_175_213_237_214,
                    -0.634_588_905_908_410_389_971_210_809_21,
                    -0.643_598_050_547_891_613_081_201_721_633,
                );
                d *= u;
                d += vsel_vf2_vo_vo_d_d_d(
                    o0,
                    o1,
                    0.112_837_916_021_059_138_255_978_217_023_e+1,
                    -0.112_879_855_826_694_507_209_862_753_992_e+1,
                    -0.112_461_487_742_845_562_801_052_956_293_e+1,
                );
                d *= a;
                d = o0.select_doubled(d, ONE.add_checked(-expk2f(d)));
                let u = vmulsign_vf_vf_vf(o2.select(d.0 + d.1, ONE), s);
                a.is_nan().select($f32x::NAN, u)
            }

        }

        pub mod u15 {
            //! Functions with 1.5 ULP error bound
            use super::*;

            /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
            pub fn erfcf(a: $f32x) -> $f32x {
                let s = a;
                let a = a.abs();
                let o0 = a.lt(ONE);
                let o1 = a.lt($f32x::splat(2.2));
                let o2 = a.lt($f32x::splat(4.3));
                let o3 = a.lt($f32x::splat(10.1));

                let u = o1.select_doubled(Doubled::new(a, ZERO),
                    Doubled::from((1., 0.)) / Doubled::new(a, ZERO),
                );

                let t = vsel_vf_vo_vo_vo_f_f_f_f(
                    o0,
                    o1,
                    o2,
                    -0.863_804_161_8_e-4,
                    -0.623_697_724_2_e-5,
                    -0.386_950_403_5,
                    0.111_534_416_7_e+1,
                ).mul_add(
                    u.0,
                    vsel_vf_vo_vo_vo_f_f_f_f(
                        o0,
                        o1,
                        o2,
                        0.600_016_617_7_e-3,
                        0.574_982_150_3_e-4,
                        0.128_807_723_5_e+1,
                        -0.945_490_419_9,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vf_vo_vo_vo_f_f_f_f(
                        o0,
                        o1,
                        o2,
                        -0.166_570_360_3_e-2,
                        0.600_285_147_8_e-5,
                        -0.181_680_321_7_e+1,
                        -0.366_725_951_4,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vf_vo_vo_vo_f_f_f_f(
                        o0,
                        o1,
                        o2,
                        0.179_515_627_7_e-3,
                        -0.285_103_637_7_e-2,
                        0.124_915_087_2_e+1,
                        0.715_566_337_1,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vf_vo_vo_vo_f_f_f_f(
                        o0,
                        o1,
                        o2,
                        0.191_410_612_3_e-1,
                        0.226_051_807_4_e-1,
                        -0.132_885_798_8,
                        -0.126_294_726_5_e-1,
                    ),
                );

                let mut d = u * t;
                d += vsel_vf2_vo_vo_vo_d_d_d_d(
                    o0,
                    o1,
                    o2,
                    -0.102_775_359_343_930_288_081_655_368_891,
                    -0.105_247_583_459_338_632_253_369_014_063,
                    -0.482_365_310_333_045_318_680_618_892_669,
                    -0.498_961_546_254_537_647_970_305_302_739,
                );
                d *= u;
                d += vsel_vf2_vo_vo_vo_d_d_d_d(
                    o0,
                    o1,
                    o2,
                    -0.636_619_483_208_481_931_303_752_546_439,
                    -0.635_609_463_574_589_034_216_723_775_292,
                    -0.134_450_203_224_533_979_217_859_332_703_e-2,
                    -0.471_199_543_422_848_492_080_722_832_666_e-4,
                );
                d *= u;
                d += vsel_vf2_vo_vo_vo_d_d_d_d(
                    o0,
                    o1,
                    o2,
                    -0.112_837_917_790_537_404_939_545_770_596_e+1,
                    -0.112_855_987_376_668_622_084_547_028_949_e+1,
                    -0.572_319_781_150_472_949_561_786_101_080,
                    -0.572_364_030_327_966_044_425_932_623_525,
                );

                let mut x = o1.select_doubled(d, Doubled::new(-a, ZERO)) * a;
                x = o1.select_doubled(x, x + d);

                x = expk2f(x);
                x = o1.select_doubled(x, x * u);

                let mut r = o3.select(x.0 + x.1, ZERO);
                r = s.is_sign_negative().select($f32x::splat(2.) - r, r);
                s.is_nan().select($f32x::NAN, r)
            }

        }

        pub mod u35 {
            //! Functions with 3.5 ULP error bound
            use super::*;

            pub fn sinf(mut d: $f32x) -> $f32x {
                let mut q: $i32x;
                let u: $f32x;
                let r = d;

                if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                    q = (d * $f32x::FRAC_1_PI).rinti();
                    u = $f32x::from_cast(q);
                    d = u.mul_add($f32x::splat(-PI_A2_F), d);
                    d = u.mul_add($f32x::splat(-PI_B2_F), d);
                    d = u.mul_add($f32x::splat(-PI_C2_F), d);
                } else if d.abs().lt($f32x::splat(TRIGRANGEMAX_F)).all() {
                    q = (d * $f32x::FRAC_1_PI).rinti();
                    u = $f32x::from_cast(q);
                    d = u.mul_add($f32x::splat(-PI_A_F), d);
                    d = u.mul_add($f32x::splat(-PI_B_F), d);
                    d = u.mul_add($f32x::splat(-PI_C_F), d);
                    d = u.mul_add($f32x::splat(-PI_D_F), d);
                } else {
                    let (mut dfidf, dfii) = rempif(d);
                    q = dfii & $i32x::splat(3);
                    q = q + q + dfidf
                        .0
                        .gt(ZERO)
                        .select($i32x::splat(2), $i32x::splat(1));
                    q >>= 2;
                    let o = (dfii & $i32x::splat(1)).eq($i32x::splat(1));
                    let mut x = Doubled::new(
                        vmulsign_vf_vf_vf($f32x::splat(3.141_592_741_012_573_242_2 * -0.5), dfidf.0),
                        vmulsign_vf_vf_vf($f32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5), dfidf.0),
                    );
                    x = dfidf + x;
                    dfidf = o.select_doubled(x, dfidf);
                    d = dfidf.0 + dfidf.1;

                    d = $f32x::from_bits(vor_vm_vo32_vm(
                        r.is_infinite() | r.is_nan(),
                        $u32x::from_bits(d),
                    ));
                }

                let s = d * d;

                d = $f32x::from_bits(
                    vand_vm_vo32_vm(
                        (q & $i32x::splat(1)).eq($i32x::splat(1)),
                        $u32x::from_bits($f32x::splat(-0.)),
                    ) ^ $u32x::from_bits(d),
                );

                let mut u = $f32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                    .mul_add(s, $f32x::splat(-0.000_198_106_907_191_686_332_225_8))
                    .mul_add(s, $f32x::splat(0.008_333_078_585_565_090_179_443_36))
                    .mul_add(s, $f32x::splat(-0.166_666_597_127_914_428_710_938));

                u = s * (u * d) + d;

                visnegzero_vo_vf(r).select(r, u)
            }

            pub fn cosf(mut d: $f32x) -> $f32x {
                let mut q: $i32x;
                let r = d;

                if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                    q = (d * $f32x::FRAC_1_PI - $f32x::splat(0.5)).rinti();
                    q = q + q + $i32x::splat(1);

                    let u = $f32x::from_cast(q);
                    d = u.mul_add($f32x::splat(-PI_A2_F * 0.5), d);
                    d = u.mul_add($f32x::splat(-PI_B2_F * 0.5), d);
                    d = u.mul_add($f32x::splat(-PI_C2_F * 0.5), d);
                } else if d.abs().lt($f32x::splat(TRIGRANGEMAX_F)).all() {
                    q = (d * $f32x::FRAC_1_PI - $f32x::splat(0.5)).rinti();
                    q = q + q + $i32x::splat(1);

                    let u = $f32x::from_cast(q);
                    d = u.mul_add($f32x::splat(-PI_A_F * 0.5), d);
                    d = u.mul_add($f32x::splat(-PI_B_F * 0.5), d);
                    d = u.mul_add($f32x::splat(-PI_C_F * 0.5), d);
                    d = u.mul_add($f32x::splat(-PI_D_F * 0.5), d);
                } else {
                    let (mut dfidf, dfii) = rempif(d);
                    q = dfii & $i32x::splat(3);
                    q = q + q + dfidf
                        .0
                        .gt(ZERO)
                        .select($i32x::splat(8), $i32x::splat(7));
                    q >>= 1;
                    let o = (dfii & $i32x::splat(1)).eq($i32x::splat(0));
                    let y = dfidf
                        .0
                        .gt(ZERO)
                        .select(ZERO, $f32x::splat(-1.));
                    let mut x = Doubled::new(
                        vmulsign_vf_vf_vf($f32x::splat(3.141_592_741_012_573_242_2 * -0.5), y),
                        vmulsign_vf_vf_vf($f32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5), y),
                    );
                    x = dfidf + x;
                    dfidf = o.select_doubled(x, dfidf);
                    d = dfidf.0 + dfidf.1;

                    d = $f32x::from_bits(vor_vm_vo32_vm(
                        r.is_infinite() | r.is_nan(),
                        $u32x::from_bits(d),
                    ));
                }

                let s = d * d;

                d = $f32x::from_bits(
                    vand_vm_vo32_vm(
                        (q & $i32x::splat(2)).eq($i32x::splat(0)),
                        $u32x::from_bits($f32x::splat(-0.)),
                    ) ^ $u32x::from_bits(d),
                );

                let u = $f32x::splat(2.608_315_980_978_659_354_150_3_e-6)
                    .mul_add(s, $f32x::splat(-0.000_198_106_907_191_686_332_225_8))
                    .mul_add(s, $f32x::splat(0.008_333_078_585_565_090_179_443_36))
                    .mul_add(s, $f32x::splat(-0.166_666_597_127_914_428_710_938));

                s * (u * d) + d
            }

            pub fn tanf(d: $f32x) -> $f32x {
                let q: $i32x;

                let mut x = d;

                if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F * 0.5)).all() {
                    q = (d * $f32x::FRAC_2_PI).rinti();
                    let u = $f32x::from_cast(q);
                    x = u.mul_add($f32x::splat(-PI_A2_F * 0.5), x);
                    x = u.mul_add($f32x::splat(-PI_B2_F * 0.5), x);
                    x = u.mul_add($f32x::splat(-PI_C2_F * 0.5), x);
                } else if d.abs().lt($f32x::splat(TRIGRANGEMAX_F)).all() {
                    q = (d * (2. * $f32x::FRAC_1_PI)).rinti();
                    let u = $f32x::from_cast(q);
                    x = u.mul_add($f32x::splat(-PI_A_F * 0.5), x);
                    x = u.mul_add($f32x::splat(-PI_B_F * 0.5), x);
                    x = u.mul_add($f32x::splat(-PI_C_F * 0.5), x);
                    x = u.mul_add($f32x::splat(-PI_D_F * 0.5), x);
                } else {
                    let (dfidf, dfii) = rempif(d);
                    q = dfii;
                    x = dfidf.0 + dfidf.1;
                    x = $f32x::from_bits(vor_vm_vo32_vm(
                        d.is_infinite() | d.is_nan(),
                        $u32x::from_bits(x),
                    ));
                    x = visnegzero_vo_vf(d).select(d, x);
                }

                let s = x * x;

                let o = (q & $i32x::splat(1)).eq($i32x::splat(1));
                x = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x),
                );

                let mut u = $f32x::splat(0.009_272_458_031_773_567_199_707_03)
                    .mul_add(s, $f32x::splat(0.003_319_849_958_643_317_222_595_21))
                    .mul_add(s, $f32x::splat(0.024_299_807_846_546_173_095_703_1))
                    .mul_add(s, $f32x::splat(0.053_449_530_154_466_629_028_320_3))
                    .mul_add(s, $f32x::splat(0.133_383_005_857_467_651_367_188))
                    .mul_add(s, $f32x::splat(0.333_331_853_151_321_411_132_812));

                u = s.mul_add(u * x, x);

                o.select(u.recpre(), u)
            }

            pub fn sincosf(d: $f32x) -> ($f32x, $f32x) {
                let q: $i32x;
                let mut s = d;

                if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                    q = (d * $f32x::FRAC_2_PI).rinti();
                    let u = $f32x::from_cast(q);
                    s = u.mul_add($f32x::splat(-PI_A2_F * 0.5), s);
                    s = u.mul_add($f32x::splat(-PI_B2_F * 0.5), s);
                    s = u.mul_add($f32x::splat(-PI_C2_F * 0.5), s);
                } else if d.abs().lt($f32x::splat(TRIGRANGEMAX_F)).all() {
                    q = (d * $f32x::FRAC_2_PI).rinti();
                    let u = $f32x::from_cast(q);
                    s = u.mul_add($f32x::splat(-PI_A_F * 0.5), s);
                    s = u.mul_add($f32x::splat(-PI_B_F * 0.5), s);
                    s = u.mul_add($f32x::splat(-PI_C_F * 0.5), s);
                    s = u.mul_add($f32x::splat(-PI_D_F * 0.5), s);
                } else {
                    let (dfidf, dfii) = rempif(d);
                    q = dfii;
                    s = dfidf.0 + dfidf.1;
                    s = $f32x::from_bits(vor_vm_vo32_vm(
                        d.is_infinite() | d.is_nan(),
                        $u32x::from_bits(s),
                    ));
                }

                let t = s;

                s = s * s;

                let u = $f32x::splat(-0.000_195_169_282_960_705_459_117_889)
                    .mul_add(s, $f32x::splat(0.008_332_157_507_538_795_471_191_41))
                    .mul_add(s, $f32x::splat(-0.166_666_537_523_269_653_320_312));

                let rx = (u * s).mul_add(t, t);
                let rx = visnegzero_vo_vf(d).select($f32x::splat(-0.), rx);

                let u = $f32x::splat(-2.718_118_423_672_422_068_193_55_e-7)
                    .mul_add(s, $f32x::splat(2.479_904_469_510_074_704_885_48_e-5))
                    .mul_add(s, $f32x::splat(-0.001_388_887_874_782_085_418_701_17))
                    .mul_add(s, $f32x::splat(0.041_666_664_183_139_801_025_390_6))
                    .mul_add(s, $f32x::splat(-0.5));

                let ry = s.mul_add(u, ONE);

                let o = (q & $i32x::splat(1)).eq($i32x::splat(0));
                let mut rsin = o.select(rx, ry);
                let mut rcos = o.select(ry, rx);

                let o = (q & $i32x::splat(2)).eq($i32x::splat(2));
                rsin = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(rsin),
                );

                let o = ((q + $i32x::splat(1)) & $i32x::splat(2)).eq($i32x::splat(2));
                rcos = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(rcos),
                );

                (rsin, rcos)
            }

            pub fn sincospif(d: $f32x) -> ($f32x, $f32x) {
                let u = d * $f32x::splat(4.);
                let q = u.truncatei();
                let q = (q + ($i32x::from_bits($u32x::from_bits(q) >> 31) ^ $i32x::splat(1)))
                    & $i32x::splat(!1);
                let s = u - $f32x::from_cast(q);

                let t = s;
                let s = s * s;

                //

                let u = $f32x::splat(-0.360_092_526_5_e-4)
                    .mul_add(s, $f32x::splat(0.249_008_811_1_e-2))
                    .mul_add(s, $f32x::splat(-0.807_455_107_6_e-1))
                    .mul_add(s, $f32x::splat(0.785_398_185_3));

                let rx = u * t;

                //

                let u = $f32x::splat(0.353_981_522_5_e-5)
                    .mul_add(s, $f32x::splat(-0.325_957_400_5_e-3))
                    .mul_add(s, $f32x::splat(0.158_543_158_3_e-1))
                    .mul_add(s, $f32x::splat(-0.308_425_128_5))
                    .mul_add(s, ONE);

                let ry = u;

                //

                let o = (q & $i32x::splat(2)).eq($i32x::splat(0));
                let mut rsin = o.select(rx, ry);
                let mut rcos = o.select(ry, rx);

                let o = (q & $i32x::splat(4)).eq($i32x::splat(4));
                rsin = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(rsin),
                );

                let o = ((q + $i32x::splat(2)) & $i32x::splat(4)).eq($i32x::splat(4));
                rcos = $f32x::from_bits(
                    vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(rcos),
                );

                let o = d.abs().gt($f32x::splat(1e+7));
                rsin = $f32x::from_bits(vandnot_vm_vo32_vm(o, $u32x::from_bits(rsin)));
                rcos = $f32x::from_bits(vandnot_vm_vo32_vm(o, $u32x::from_bits(rcos)));

                let o = d.is_infinite();
                rsin = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rsin)));
                rcos = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rcos)));

                (rsin, rcos)
            }

            pub fn atanf(d: $f32x) -> $f32x {
                let q = vsel_vi2_vf_vi2(d, $i32x::splat(2));
                let s = d.abs();

                let q = vsel_vi2_vf_vf_vi2_vi2(ONE, s, q + $i32x::splat(1), q);
                let s = ONE.lt(s).select(s.recpre(), s);

                let mut t = s * s;

                let u = $f32x::splat(0.002_823_638_962_581_753_730_773_93)
                    .mul_add(t, $f32x::splat(-0.015_956_902_876_496_315_002_441_4))
                    .mul_add(t, $f32x::splat(0.042_504_988_610_744_476_318_359_4))
                    .mul_add(t, $f32x::splat(-0.074_890_092_015_266_418_457_031_2))
                    .mul_add(t, $f32x::splat(0.106_347_933_411_598_205_566_406))
                    .mul_add(t, $f32x::splat(-0.142_027_363_181_114_196_777_344))
                    .mul_add(t, $f32x::splat(0.199_926_957_488_059_997_558_594))
                    .mul_add(t, $f32x::splat(-0.333_331_018_686_294_555_664_062));

                t = s.mul_add(t * u, s);

                t = (q & $i32x::splat(1))
                    .eq($i32x::splat(1))
                    .select($f32x::FRAC_PI_2 - t, t);

                t = $f32x::from_bits(
                    vand_vm_vo32_vm(
                        (q & $i32x::splat(2)).eq($i32x::splat(2)),
                        $u32x::from_bits($f32x::splat(-0.)),
                    ) ^ $u32x::from_bits(t),
                );

                if cfg!(feature = "enable_neon32") || cfg!(feature = "enable_neon32vfpv4") {
                    t = d.is_infinite().select(
                        vmulsign_vf_vf_vf($f32x::splat(1.587_401_051_968_199_474_751_705_6), d),
                        t,
                    );
                }

                t
            }

            pub fn atan2f(y: $f32x, x: $f32x) -> $f32x {
                let mut r = atan2kf(y.abs(), x);

                r = vmulsign_vf_vf_vf(r, x);
                r = (x.is_infinite() | x.eq(ZERO)).select(
                    $f32x::FRAC_PI_2
                        - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::FRAC_PI_2, x)),
                    r,
                );
                r = y.is_infinite().select(
                    $f32x::FRAC_PI_2
                        - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::FRAC_PI_4, x)),
                    r,
                );

                r = y.eq(ZERO).select(
                    $f32x::from_bits(vand_vm_vo32_vm(
                        x.is_sign_negative(),
                        $u32x::from_bits($f32x::PI),
                    )),
                    r,
                );

                $f32x::from_bits(vor_vm_vo32_vm(
                    x.is_nan() | y.is_nan(),
                    $u32x::from_bits(vmulsign_vf_vf_vf(r, y)),
                ))
            }

            pub fn asinf(d: $f32x) -> $f32x {
                let o = d.abs().lt($f32x::splat(0.5));
                let x2 = o.select(d * d, (ONE - d.abs()) * $f32x::splat(0.5));
                let x = o.select(d.abs(), x2.sqrt());

                let u = $f32x::splat(0.419_745_482_5_e-1)
                    .mul_add(x2, $f32x::splat(0.242_404_602_5_e-1))
                    .mul_add(x2, $f32x::splat(0.454_742_386_9_e-1))
                    .mul_add(x2, $f32x::splat(0.749_502_927_1_e-1))
                    .mul_add(x2, $f32x::splat(0.166_667_729_6))
                    .mul_add(x * x2, x);

                let r = o.select(u, u.mul_add($f32x::splat(-2.), $f32x::FRAC_PI_2));
                vmulsign_vf_vf_vf(r, d)
            }

            pub fn acosf(d: $f32x) -> $f32x {
                let o = d.abs().lt($f32x::splat(0.5));
                let x2 = o.select(d * d, (ONE - d.abs()) * $f32x::splat(0.5));
                let mut x = o.select(d.abs(), x2.sqrt());
                x = d.abs().eq(ONE).select(ZERO, x);

                let u = $f32x::splat(0.419_745_482_5_e-1)
                    .mul_add(x2, $f32x::splat(0.242_404_602_5_e-1))
                    .mul_add(x2, $f32x::splat(0.454_742_386_9_e-1))
                    .mul_add(x2, $f32x::splat(0.749_502_927_1_e-1))
                    .mul_add(x2, $f32x::splat(0.166_667_729_6))
                    * (x2 * x);

                let y = $f32x::splat(3.141_592_653_589_793_2 / 2.)
                    - (vmulsign_vf_vf_vf(x, d) + vmulsign_vf_vf_vf(u, d));
                x += u;
                let r = o.select(y, x * $f32x::splat(2.));
                vandnot_vo_vo_vo(o, d.lt(ZERO)).select(
                    Doubled::from((3.141_592_741_012_573_242_2, -8.742_277_657_347_585_773_1_e-8))
                        .add_checked(-r)
                        .0,
                    r,
                )
            }

            pub fn logf(mut d: $f32x) -> $f32x {
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

                let mut x = ($f32x::splat(-1.) + m) / (ONE + m);
                let x2 = x * x;

                let t = $f32x::splat(0.239_282_846_450_805_664_062_5)
                    .mul_add(x2, $f32x::splat(0.285_182_118_415_832_519_531_25))
                    .mul_add(x2, $f32x::splat(0.400_005_877_017_974_853_515_625))
                    .mul_add(x2, $f32x::splat(0.666_666_686_534_881_591_796_875))
                    .mul_add(x2, $f32x::splat(2.));

                x = x.mul_add(t, $f32x::splat(0.693_147_180_559_945_286_226_764) * ef);
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    x = d.eq($f32x::INFINITY).select($f32x::INFINITY, x);
                    x = (d.lt(ZERO) | d.is_nan()).select($f32x::NAN, x);
                    d.eq(ZERO)
                        .select($f32x::NEG_INFINITY, x)
                /*} else {
                    vfixup_vf_vf_vf_vi2_i(x, d, $i32x::splat(5 << (5 * 4)), 0)
                }*/
            }

            #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
            pub fn sqrtf(d: $f32x) -> $f32x {
                let e = $f32x::from_bits(
                    $u32x::splat(0x_2000_0000)
                        + ($u32x::splat(0x_7f00_0000) & ($u32x::from_bits(d) >> 1)),
                );
                let m = $f32x::from_bits(
                    $i32x::splat(0x_3f00_0000) + ($i32x::splat(0x_01ff_ffff) & $i32x::from_bits(d)),
                );
                let mut x = vrsqrteq_f32(m);
                x = vmulq_f32(x, vrsqrtsq_f32(m, vmulq_f32(x, x)));
                let mut u = vmulq_f32(x, m);
                u = vmlaq_f32(u, vmlsq_f32(m, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
                e = $f32x::from_bits(vandnot_vm_vo32_vm(
                    d.eq(ZERO),
                    $u32x::from_bits(e),
                ));
                u = e * u;

                u = d.is_infinite().select($f32x::INFINITY, u);
                u = $f32x::from_bits(vor_vm_vo32_vm(
                    d.is_nan() | d.lt(ZERO),
                    $u32x::from_bits(u),
                ));
                vmulsign_vf_vf_vf(u, d)
            }
            /*#[cfg(feature = "enable_vecext")]
                    pub fn xsqrtf_u35(d: $f32x) -> $f32x {
                        let mut q = d.sqrt();
                        q = visnegzero_vo_vf(d).select($f32x::splat(-0.), q);
                        d.eq($f32x::INFINITY).select($f32x::INFINITY, q)
                    }*/
            #[cfg(all(
                        not(feature = "enable_neon32"),
                        not(feature = "enable_neon32vfpv4"),
                    //    not(feature = "enable_vecext")
                    ))]
            pub fn sqrtf(d: $f32x) -> $f32x {
                d.sqrt()
            }

            pub fn cbrtf(mut d: $f32x) -> $f32x {
                let mut q = ONE;

                /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                    let s = d;
                }*/
                let e = vilogbk_vi2_vf(d.abs()) + $i32x::splat(1);
                d = vldexp2_vf_vf_vi2(d, -e);

                let t = $f32x::from_cast(e) + $f32x::splat(6144.);
                let qu = (t * $f32x::splat(1. / 3.)).truncatei();
                let re = (t - $f32x::from_cast(qu) * $f32x::splat(3.)).truncatei();

                q = re
                    .eq($i32x::splat(1))
                    .select($f32x::splat(1.259_921_049_894_873_164_767_210_6), q);
                q = re
                    .eq($i32x::splat(2))
                    .select($f32x::splat(1.587_401_051_968_199_474_751_705_6), q);
                q = vldexp2_vf_vf_vi2(q, qu - $i32x::splat(2048));

                q = vmulsign_vf_vf_vf(q, d);
                d = d.abs();

                let x = $f32x::splat(-0.601_564_466_953_277_587_890_625)
                    .mul_add(d, $f32x::splat(2.820_889_234_542_846_679_687_5))
                    .mul_add(d, $f32x::splat(-5.532_182_216_644_287_109_375))
                    .mul_add(d, $f32x::splat(5.898_262_500_762_939_453_125))
                    .mul_add(d, $f32x::splat(-3.809_541_702_270_507_812_5))
                    .mul_add(d, $f32x::splat(2.224_125_623_703_002_929_687_5));

                let mut y = d * x * x;
                y = (y - $f32x::splat(2. / 3.) * y * y.mul_add(x, $f32x::splat(-1.))) * q;

                /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                    y = s.is_infinite().select(vmulsign_vf_vf_vf($f32x::INFINITY, s), y);
                    y = s
                        .eq(ZERO)
                        .select(vmulsign_vf_vf_vf(ZERO, s), y);
                }*/

                y
            }


            pub fn sinhf(x: $f32x) -> $f32x {
                let e = expm1fk(x.abs());
                let mut y = (e + $f32x::splat(2.)) / (e + ONE);
                y *= $f32x::splat(0.5) * e;

                y = (x.abs().gt($f32x::splat(88.)) | y.is_nan())
                    .select($f32x::INFINITY, y);
                y = vmulsign_vf_vf_vf(y, x);
                $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
            }

            pub fn coshf(x: $f32x) -> $f32x {
                let e = u10::expf(x.abs());
                let mut y = $f32x::splat(0.5).mul_add(e, $f32x::splat(0.5) / e);

                y = (x.abs().gt($f32x::splat(88.)) | y.is_nan())
                    .select($f32x::INFINITY, y);
                $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
            }

            pub fn tanhf(x: $f32x) -> $f32x {
                let d = expm1fk($f32x::splat(2.) * x.abs());
                let mut y = d / ($f32x::splat(2.) + d);

                y = (x.abs().gt($f32x::splat(8.664_339_742)) | y.is_nan())
                    .select(ONE, y);
                y = vmulsign_vf_vf_vf(y, x);
                $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
            }


            pub fn hypotf(x: $f32x, y: $f32x) -> $f32x {
                let x = x.abs();
                let y = y.abs();
                let min = x.min(y);
                let max = x.max(y);

                let t = min / max;
                let mut ret = max * t.mul_add(t, ONE).sqrt();
                ret = min.eq(ZERO).select(max, ret);
                ret = (x.is_nan() | y.is_nan()).select($f32x::NAN, ret);
                (x.eq($f32x::INFINITY) | y.eq($f32x::INFINITY))
                    .select($f32x::INFINITY, ret)
            }


        }

    };
}
