macro_rules! impl_math_f64 {
    ($f64x:ident, $u64x:ident, $m64x:ident, $i64x:ident, $ux:ident, $mx:ident, $ix:ident) => {
        use crate::common::*;
        use doubled::*;

        const ZERO: $f64x = $f64x::splat(0.);
        const ONE: $f64x = $f64x::splat(1.);
        const D1_63X: $f64x = $f64x::splat((1u64 << 63) as f64);
        const D1_60X: $f64x = $f64x::splat((1u64 << 60) as f64);
        const D1_54X: $f64x = $f64x::splat((1u64 << 54) as f64);
        const D1_53X: $f64x = $f64x::splat((1u64 << 53) as f64);
        const D1_52X: $f64x = $f64x::splat((1u64 << 52) as f64);
        const D1_32X: $f64x = $f64x::splat((1u64 << 32) as f64);
        const D1_31X: $f64x = $f64x::splat((1u64 << 31) as f64);
        const D1_28X: $f64x = $f64x::splat((1u64 << 28) as f64);
        const D1_24X: $f64x = $f64x::splat((1u64 << 24) as f64);
        const D1_23X: $f64x = $f64x::splat((1u64 << 23) as f64);

        //---------???????
        //--------- Naive implementation ???????
        #[inline]
        fn vandnot_vm_vm_vm(x: $u64x, y: $u64x) -> $u64x { y & !x }

        #[inline]
        fn vandnot_vo_vo_vo(x: $m64x, y: $m64x) -> $m64x { y & !x }

        #[inline]
        fn vand_vm_vo64_vm(x: $m64x, y: $u64x) -> $u64x { $u64x::from_bits(x) & y }
        #[inline]
        fn vor_vm_vo64_vm(x: $m64x, y: $u64x) -> $u64x { $u64x::from_bits(x) | y }
        #[inline]
        fn vandnot_vm_vo64_vm(x: $m64x, y: $u64x) -> $u64x { y & !$u64x::from_bits(x) }

        #[inline]
        fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { y & !x }

        #[inline]
        fn vand_vi_vo_vi(x: $mx, y: $ix) -> $ix { $ix::from_bits(x) & y }
        #[inline]
        fn vandnot_vi_vo_vi(x: $mx, y: $ix) -> $ix { $ix::from_bits(x) & !y }

        #[inline]
        fn veq_vi2_vi2_vi2(x: $i64x, y: $i64x) -> $i64x { $i64x::from_bits(x.eq(y)) }

        impl Round for $f64x {
            type Int = $ix;
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
                rint(self)
            }
            #[inline]
            fn rinti(self) -> Self::Int {
                Self::Int::from_cast(self.rint())
            }
        }

        impl MulSub for $f64x {
            #[inline]
            fn mul_sub(self, y: Self, z: Self) -> Self {
                self*y - z
            }
        }

        #[inline]
        fn vgather_vd_p_vi(ptr: &[f64], vi: $ix) -> $f64x {
            let mut ar = [0_f64; $f64x::lanes()];
            for i in 0..$f64x::lanes() {
                ar[i] = ptr[vi.extract(i) as usize];
            }
            $f64x::from_slice_aligned(&ar)
        }

        #[inline]
        fn vrev21_vi2_vi2(i: $i64x) -> $i64x {
            const L : usize = $i64x::lanes()*2;
            let i2 = Simd::<[i32;L]>::from_bits(i);
            let mut r = [0_i32;L];
            i2.write_to_slice_aligned(&mut r);
            for i in 0..L/2 {
                r.swap(i*2, i*2+1);
            }
            $i64x::from_bits(Simd::<[i32;L]>::from_slice_aligned(&r))
        }

        //----------???????
        //----------???????

        // ------------

        impl SqrtAsDoubled for $f64x {
            #[inline]
            fn sqrt_as_doubled(self) -> Doubled<Self> {
                let t = self.sqrt();
                ((self + t.mul_as_doubled(t)) * t.recpre_as_doubled()).scale(Self::splat(0.5))
            }
        }
        // --------------------
        impl VectorizedSelect<f64> for $m64x {
            type Output = $f64x;
            fn select_splat(self, l: f64, r: f64) -> Self::Output {
                self.select(Self::Output::splat(l), Self::Output::splat(r))
            }
        }
        impl DoubledSelect<$f64x> for $m64x {
            fn select_doubled(self, l: Doubled<$f64x>, r: Doubled<$f64x>) -> Doubled<$f64x> {
                Doubled::new(self.select(l.0, r.0), self.select(l.1, r.1))
            }
        }

        #[inline]
        fn vsel_vd_vo_vo_d_d_d(o0: $m64x, o1: $m64x, d0: f64, d1: f64, d2: f64) -> $f64x {
            o0.select($f64x::splat(d0), o1.select_splat(d1, d2))
        }

        #[inline]
        fn vsel_vd_vo_vo_vo_d_d_d_d(
            o0: $m64x,
            o1: $m64x,
            o2: $m64x,
            d0: f64,
            d1: f64,
            d2: f64,
            d3: f64,
        ) -> $f64x {
            o0.select(
                $f64x::splat(d0),
                o1.select($f64x::splat(d1), o2.select_splat(d2, d3)),
            )
        }
        // -------------------

        #[inline]
        fn vsel_vd2_vo_d_d_d_d(o: $m64x, x1: f64, y1: f64, x0: f64, y0: f64) -> Doubled<$f64x> {
            Doubled::new(o.select_splat(x1, x0), o.select_splat(y1, y0))
        }
        // -----------
        impl Sign for $f64x {
            type Mask = $m64x;
            #[inline]
            fn is_sign_negative(self) -> Self::Mask {
                ($u64x::from_bits(self) & $u64x::splat(((-0.) as f64).to_bits()))
                    .ne($u64x::splat(0))
            }
            #[inline]
            fn is_sign_positive(self) -> Self::Mask {
                !self.is_sign_negative()
            }
        }

        // return d0 < d1 ? x : y
        #[inline]
        fn vsel_vi_vd_vd_vi_vi(d0: $f64x, d1: $f64x, x: $ix, y: $ix) -> $ix {
            d0.lt(d1).select(x, y)
        }

        // return d0 < 0 ? x : 0
        #[inline]
        fn vsel_vi_vd_vi(d: $f64x, x: $ix) -> $ix {
            vand_vi_vo_vi($mx::from_cast(d.is_sign_negative()), x)
        }
        #[inline]
        fn visnegzero_vo_vd(d: $f64x) -> $m64x {
            $u64x::from_bits(d).eq($u64x::from_bits($f64x::splat(-0.)))
        }
        #[inline]
        fn vsignbit_vm_vd(d: $f64x) -> $u64x {
            $u64x::from_bits(d) & $u64x::from_bits($f64x::splat(-0.))
        }
        #[inline]
        fn vmulsign_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x {
            $f64x::from_bits($u64x::from_bits(x) ^ vsignbit_vm_vd(y))
        }
        #[inline]
        fn vcopysign_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x {
            $f64x::from_bits(
                vandnot_vm_vm_vm($u64x::from_bits($f64x::splat(-0.)), $u64x::from_bits(x))
                    ^ ($u64x::from_bits($f64x::splat(-0.)) & $u64x::from_bits(y)),
            )
        }
        #[inline]
        fn vsign_vd_vd(d: $f64x) -> $f64x {
            vmulsign_vd_vd_vd(ONE, d)
        }
        #[inline]
        fn vpow2i_vd_vi(q: $ix) -> $f64x {
            let q = $ix::splat(0x3ff) + q;
            let r = $i64x::from_cast(q);
            $f64x::from_bits(r << 20)
        }
        #[inline]
        fn vldexp_vd_vd_vi(x: $f64x, q: $ix) -> $f64x {
            let mut m = q >> 31;
            m = (((m + q) >> 9) - m) << 7;
            let q = q - (m << 2);
            m = $ix::splat(0x3ff) + m;
            m = vandnot_vi_vo_vi($ix::splat(0).gt(m), m);
            m = m.gt($ix::splat(0x7ff)).select($ix::splat(0x7ff), m);
            let r = $i64x::from_cast(m);
            let y = $f64x::from_bits(r << 20);
            x * y * y * y * y * vpow2i_vd_vi(q)
        }
        #[inline]
        fn vldexp2_vd_vd_vi(d: $f64x, e: $ix) -> $f64x {
            d * vpow2i_vd_vi(e >> 1) * vpow2i_vd_vi(e - (e >> 1))
        }
        #[inline]
        fn vldexp3_vd_vd_vi(d: $f64x, q: $ix) -> $f64x {
            $f64x::from_bits($i64x::from_bits(d) + ($i64x::from_cast(q) << 20))
        }

        /*#[cfg(all(
            not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn vilogbk_vi_vd(mut d: $f64x) -> $ix {
            let o = d.lt($f64x::splat(4.909_093_465_297_726_6_e-91));
            d = o.select($f64x::splat(2.037_035_976_334_486_e90) * d, d);
            let mut q = $ix::from_cast($u64x::from_bits(d));
            q &= $ix::splat(((1 << 12) - 1) << 20);
            q = $ix::from_bits($ux::from_bits(q) >> 20);
            q - $mx::from_cast(o).select($ix::splat(300 + 0x3ff), $ix::splat(0x3ff))
        }
        /*#[cfg(all(
            not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn vilogb2k_vi_vd(d: $f64x) -> $ix {
            let mut q = $ix::from_cast($u64x::from_bits(d));
            q = $ix::from_bits($ux::from_bits(q) >> 20);
            q &= $ix::splat(0x7ff);
            q - $ix::splat(0x3ff)
        }

        impl IsInt for $f64x {
            type Mask = $m64x;
            #[inline]
            fn is_integer(self) -> Self::Mask {
                let mut x = (self * (ONE / D1_31X)).truncate();
                x = (-D1_31X).mul_add(x, self);
                x.truncate().eq(x) | self.abs().gt(D1_53X)
            }
        }

        #[inline]
        fn visodd_vo_vd(d: $f64x) -> $m64x {
            let mut x = (d * (ONE / D1_31X)).truncate();
            x = (-D1_31X).mul_add(x, d);

            $m64x::from_cast((x.truncatei() & $ix::splat(1)).eq($ix::splat(1)))
                & d.abs().lt(D1_53X)
        }

        //

        pub fn ldexp(x: $f64x, q: $ix) -> $f64x {
            vldexp_vd_vd_vi(x, q)
        }

        pub fn ilogb(d: $f64x) -> $ix {
            let mut e = $f64x::from_cast(vilogbk_vi_vd(d.abs()));
            e = d
                .eq(ZERO)
                .select($f64x::splat(SLEEF_FP_ILOGB0.into()), e);
            e = d.is_nan().select($f64x::splat(SLEEF_FP_ILOGBNAN.into()), e);
            e = d.is_infinite().select($f64x::splat(f64::MAX), e);
            e.rinti()
        }

        #[inline]
        fn rempisub(x: $f64x) -> ($f64x, $ix) {
            if cfg!(feature = "full_fp_rounding") {
                let y = (x * $f64x::splat(4.)).rint();
                let vi = (y - x.rint() * $f64x::splat(4.)).truncatei();
                (x - y * $f64x::splat(0.25), vi)
            } else {
                let mut fr = x - D1_28X * (x * (ONE / D1_28X)).truncate();
                let mut vi = $mx::from_cast(x.gt(ZERO))
                    .select($ix::splat(4), $ix::splat(3))
                    + (fr * $f64x::splat(8.)).truncatei();
                vi = (($ix::splat(7) & vi) - $ix::splat(3)) >> 1;
                fr = fr - $f64x::splat(0.25) * fr
                    .mul_add($f64x::splat(4.), vmulsign_vd_vd_vd($f64x::splat(0.5), x))
                    .truncate();
                fr = fr
                    .abs()
                    .gt($f64x::splat(0.25))
                    .select(fr - vmulsign_vd_vd_vd($f64x::splat(0.5), x), fr);
                fr = fr.abs().gt($f64x::splat(1e+10)).select(ZERO, fr);
                let o = x.abs().eq($f64x::splat(0.124_999_999_999_999_986_12));
                fr = o.select(x, fr);
                vi = $mx::from_cast(o).select($ix::splat(0), vi);
                (fr, vi)
            }
        }
        #[inline]
        fn rempi(mut a: $f64x) -> (Doubled<$f64x>, $ix) {
            let mut ex = vilogb2k_vi_vd(a);
            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                ex = vandnot_vi_vi_vi(ex >> 31, ex);
                ex = ex & $ix::splat(1023);
            }*/
            ex -= $ix::splat(55);
            let mut q = vand_vi_vo_vi(ex.gt($ix::splat(700 - 55)), $ix::splat(-64));
            a = vldexp3_vd_vd_vi(a, q);
            ex = vandnot_vi_vi_vi(ex >> 31, ex);
            ex <<= 2;
            let mut x = a.mul_as_doubled(vgather_vd_p_vi(&REMPITABDP, ex));
            let (did, dii) = rempisub(x.0);
            q = dii;
            x.0 = did;
            x = x.normalize();
            let mut y = a.mul_as_doubled(vgather_vd_p_vi(&REMPITABDP[1..], ex));
            x += y;
            let (did, dii) = rempisub(x.0);
            q += dii;
            x.0 = did;
            x = x.normalize();
            y = Doubled::new(
                vgather_vd_p_vi(&REMPITABDP[2..], ex),
                vgather_vd_p_vi(&REMPITABDP[3..], ex),
            );
            y *= a;
            x += y;
            x = x.normalize();
            x *= Doubled::from((3.141_592_653_589_793_116 * 2., 1.224_646_799_147_353_207_2_e-16 * 2.));
            let o = a.abs().lt($f64x::splat(0.7));
            x.0 = o.select(a, x.0);
            x.1 = $f64x::from_bits(vandnot_vm_vo64_vm(o, $u64x::from_bits(x.1)));
            (x, q)
        }

        #[inline]
        fn cospik(d: $f64x) -> Doubled<$f64x> {
            let u = d * $f64x::splat(4.);
            let mut q = u.truncatei();
            q = (q + ($ix::from_bits($ux::from_bits(q) >> 31) ^ $ix::splat(1))) & $ix::splat(!1);
            let o = $m64x::from_cast((q & $ix::splat(2)).eq($ix::splat(0)));

            let s = u - $f64x::from_cast(q);
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            //

            let u = o.select_splat(9.944_803_876_268_437_740_902_08_e-16,
                -2.024_611_207_851_823_992_958_68_e-14,
            ).mul_add(
                s,
                o.select_splat(-3.897_962_260_629_327_991_640_47_e-13,
                    6.948_218_305_801_794_613_277_84_e-12,
                ),
            ).mul_add(
                s,
                o.select_splat(1.150_115_825_399_960_352_669_01_e-10,
                    -1.757_247_499_528_531_799_526_64_e-9,
                ),
            ).mul_add(
                s,
                o.select_splat(-2.461_136_950_104_469_749_535_9_e-8,
                    3.133_616_889_668_683_928_784_22_e-7,
                ),
            ).mul_add(
                s,
                o.select_splat(3.590_860_448_590_527_540_050_62_e-6,
                    -3.657_620_418_216_155_192_036_1_e-5,
                ),
            ).mul_add(
                s,
                o.select_splat(-0.000_325_991_886_927_389_905_997_954,
                    0.002_490_394_570_192_718_502_743_560,
                ),
            );
            let mut x = u * s + vsel_vd2_vo_d_d_d_d(
                o,
                0.015_854_344_243_815_501_891_425_9,
                -1.046_932_722_806_315_219_088_45_e-18,
                -0.080_745_512_188_280_785_248_473_1,
                3.618_524_750_670_371_048_499_87_e-18,
            );
            x = s2 * x + vsel_vd2_vo_d_d_d_d(
                o,
                -0.308_425_137_534_042_437_259_529,
                -1.956_984_921_336_335_503_383_45_e-17,
                0.785_398_163_397_448_278_999_491,
                3.062_871_137_271_550_026_071_05_e-17,
            );

            x *= o.select_doubled(s2, Doubled::new(t, ZERO));
            x = o.select_doubled(x + ONE, x);

            let o = $m64x::from_cast((q + $ix::splat(2) & $ix::splat(4)).eq($ix::splat(4)));
            x.0 = $f64x::from_bits(
                vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(x.0),
            );
            x.1 = $f64x::from_bits(
                vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(x.1),
            );

            x
        }

        #[inline]
        fn atan2k(y: $f64x, x: $f64x) -> $f64x {
            let q = vsel_vi_vd_vi(x, $ix::splat(-2));
            let x = x.abs();

            let q = vsel_vi_vd_vd_vi_vi(x, y, q + $ix::splat(1), q);
            let p = x.lt(y);
            let s = p.select(-x, y);
            let mut t = x.max(y);

            let s = s / t;
            t = s * s;

            let mut u: $f64x;
            if cfg!(feature = "split_kernel") {
                let t2 = t * t;

                u = $f64x::splat(-1.887_960_084_630_734_965_637_46_e-5)
                    .mul_add(t2, $f64x::splat(-0.001_106_118_314_866_724_825_634_71))
                    .mul_add(t2, $f64x::splat(-0.008_898_961_958_876_554_917_408_09))
                    .mul_add(t2, $f64x::splat(-0.025_451_762_493_231_264_161_686_1))
                    .mul_add(t2, $f64x::splat(-0.040_762_919_127_683_650_000_193_4))
                    .mul_add(t2, $f64x::splat(-0.052_367_485_230_348_245_761_611_3))
                    .mul_add(t2, $f64x::splat(-0.066_657_357_936_108_052_598_456_2))
                    .mul_add(t2, $f64x::splat(-0.090_908_995_008_245_008_229_153))
                    .mul_add(t2, $f64x::splat(-0.142_857_142_667_713_293_837_65))
                    .mul_add(t2, $f64x::splat(-0.333_333_333_333_311_110_369_124));

                let v = $f64x::splat(0.000_209_850_076_645_816_976_906_797)
                    .mul_add(t2, $f64x::splat(0.003_700_267_441_887_131_192_324_03))
                    .mul_add(t2, $f64x::splat(0.016_599_329_773_529_201_970_117))
                    .mul_add(t2, $f64x::splat(0.033_785_258_000_135_306_999_389_7))
                    .mul_add(t2, $f64x::splat(0.046_666_715_007_784_062_563_267_5))
                    .mul_add(t2, $f64x::splat(0.058_766_639_292_667_358_085_431_3))
                    .mul_add(t2, $f64x::splat(0.076_921_953_831_176_961_835_502_9))
                    .mul_add(t2, $f64x::splat(0.111_111_105_648_261_418_443_745))
                    .mul_add(t2, $f64x::splat(0.199_999_999_996_591_265_594_148));

                u = v.mul_add(t, u);
            } else {
                u = $f64x::splat(-1.887_960_084_630_734_965_637_46_e-5)
                    .mul_add(t, $f64x::splat(0.000_209_850_076_645_816_976_906_797))
                    .mul_add(t, $f64x::splat(-0.001_106_118_314_866_724_825_634_71))
                    .mul_add(t, $f64x::splat(0.003_700_267_441_887_131_192_324_03))
                    .mul_add(t, $f64x::splat(-0.008_898_961_958_876_554_917_408_09))
                    .mul_add(t, $f64x::splat(0.016_599_329_773_529_201_970_117))
                    .mul_add(t, $f64x::splat(-0.025_451_762_493_231_264_161_686_1))
                    .mul_add(t, $f64x::splat(0.033_785_258_000_135_306_999_389_7))
                    .mul_add(t, $f64x::splat(-0.040_762_919_127_683_650_000_193_4))
                    .mul_add(t, $f64x::splat(0.046_666_715_007_784_062_563_267_5))
                    .mul_add(t, $f64x::splat(-0.052_367_485_230_348_245_761_611_3))
                    .mul_add(t, $f64x::splat(0.058_766_639_292_667_358_085_431_3))
                    .mul_add(t, $f64x::splat(-0.066_657_357_936_108_052_598_456_2))
                    .mul_add(t, $f64x::splat(0.076_921_953_831_176_961_835_502_9))
                    .mul_add(t, $f64x::splat(-0.090_908_995_008_245_008_229_153))
                    .mul_add(t, $f64x::splat(0.111_111_105_648_261_418_443_745))
                    .mul_add(t, $f64x::splat(-0.142_857_142_667_713_293_837_65))
                    .mul_add(t, $f64x::splat(0.199_999_999_996_591_265_594_148))
                    .mul_add(t, $f64x::splat(-0.333_333_333_333_311_110_369_124));
            }

            t = s.mul_add(t * u, s);
            $f64x::from_cast(q).mul_add($f64x::FRAC_PI_2, t)
        }
        #[inline]
        fn visinf2_vd_vd_vd(d: $f64x, m: $f64x) -> $f64x {
            $f64x::from_bits(vand_vm_vo64_vm(
                d.is_infinite(),
                ($u64x::from_bits(d) & $u64x::from_bits($f64x::splat(-0.))) | $u64x::from_bits(m),
            ))
        }

        #[inline]
        fn expm1k(d: $f64x) -> $f64x {
            let mut u = (d * $f64x::splat(R_LN2)).rint();
            let q = u.rinti();

            let s = u.mul_add($f64x::splat(-L2U), d);
            let s = u.mul_add($f64x::splat(-L2L), s);

            u = $f64x::splat(2.088_606_211_072_836_875_363_41_e-9)
                .mul_add(s, $f64x::splat(2.511_129_308_928_765_186_106_61_e-8))
                .mul_add(s, $f64x::splat(2.755_739_112_349_004_718_933_38_e-7))
                .mul_add(s, $f64x::splat(2.755_723_629_119_288_276_294_23_e-6))
                .mul_add(s, $f64x::splat(2.480_158_715_923_547_299_879_1_e-5))
                .mul_add(s, $f64x::splat(0.000_198_412_698_960_509_205_564_975))
                .mul_add(s, $f64x::splat(0.001_388_888_888_897_744_922_079_62))
                .mul_add(s, $f64x::splat(0.008_333_333_333_316_527_216_649_84))
                .mul_add(s, $f64x::splat(0.041_666_666_666_666_504_759_142_2))
                .mul_add(s, $f64x::splat(0.166_666_666_666_666_851_703_837))
                .mul_add(s, $f64x::splat(0.5));
            u = (s * s).mul_add(u, s);

            $m64x::from_cast(q.eq($ix::splat(0))).select(
                u,
                vldexp2_vd_vd_vi(u + ONE, q) - ONE,
            )
        }
        #[inline]
        fn logk(mut d: $f64x) -> Doubled<$f64x> {
            let m: $f64x;

            let mut s =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = d.lt($f64x::splat(f64::MIN));
                    d = o.select(d * (D1_32X * D1_32X), d);
                    let mut e = vilogb2k_vi_vd(d * $f64x::splat(1. / 0.75));
                    m = vldexp3_vd_vd_vi(d, -e);
                    e = $mx::from_cast(o).select(e - $ix::splat(64), e);
                    Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17))
                        * $f64x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vd_vd(d * $f64x::splat(1. / 0.75));
                    e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                    m = vgetmant_vd_vd(d);
                    Doubled::new(
                        $f64x::splat(0.693_147_180_559_945_286_226_764),
                        $f64x::splat(2.319_046_813_846_299_558_417_771_e-17),
                    ) * e
                }*/;

            let x = $f64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.square();

            let t = $f64x::splat(0.116_255_524_079_935_043_668_677)
                .mul_add(x2.0, $f64x::splat(0.103_239_680_901_072_952_701_192))
                .mul_add(x2.0, $f64x::splat(0.117_754_809_412_463_995_466_069))
                .mul_add(x2.0, $f64x::splat(0.133_329_810_868_462_739_215_09))
                .mul_add(x2.0, $f64x::splat(0.153_846_227_114_512_262_845_736))
                .mul_add(x2.0, $f64x::splat(0.181_818_180_850_050_775_676_507))
                .mul_add(x2.0, $f64x::splat(0.222_222_222_230_083_560_345_903))
                .mul_add(x2.0, $f64x::splat(0.285_714_285_714_249_172_087_875))
                .mul_add(x2.0, $f64x::splat(0.400_000_000_000_000_077_715_612));
            let c = Doubled::from((0.666_666_666_666_666_629_659_233, 3.805_549_625_424_120_563_366_16_e-17));

            s = s.add_checked(x.scale($f64x::splat(2.)));
            s.add_checked(x2 * x * (x2 * t + c))
        }

        #[inline]
        fn expk(d: Doubled<$f64x>) -> $f64x {
            let mut u = (d.0 + d.1) * $f64x::splat(R_LN2);
            let dq = u.rint();
            let q = dq.rinti();

            let mut s = d + dq * $f64x::splat(-L2U);
            s += dq * $f64x::splat(-L2L);

            s = s.normalize();

            u = $f64x::splat(2.510_696_834_209_504_195_271_39_e-8)
                .mul_add(s.0, $f64x::splat(2.762_861_667_702_706_491_168_55_e-7))
                .mul_add(s.0, $f64x::splat(2.755_724_967_250_235_741_438_64_e-6))
                .mul_add(s.0, $f64x::splat(2.480_149_739_898_197_941_141_53_e-5))
                .mul_add(s.0, $f64x::splat(0.000_198_412_698_809_069_797_676_111))
                .mul_add(s.0, $f64x::splat(0.001_388_888_893_997_712_896_052_9))
                .mul_add(s.0, $f64x::splat(0.008_333_333_333_323_714_176_010_81))
                .mul_add(s.0, $f64x::splat(0.041_666_666_666_540_952_412_844_9))
                .mul_add(s.0, $f64x::splat(0.166_666_666_666_666_740_681_535))
                .mul_add(s.0, $f64x::splat(0.500_000_000_000_000_999_200_722));

            let mut t = s.add_checked(s.square() * u);

            t = ONE.add_checked(t);
            u = t.0 + t.1;
            u = vldexp2_vd_vd_vi(u, q);

            $f64x::from_bits(vandnot_vm_vo64_vm(
                d.0.lt($f64x::splat(-1000.)),
                $u64x::from_bits(u),
            ))
        }

        #[inline]
        fn expk2(d: Doubled<$f64x>) -> Doubled<$f64x> {
            let u = (d.0 + d.1) * $f64x::splat(R_LN2);
            let dq = u.rint();
            let q = dq.rinti();

            let s = d + dq * $f64x::splat(-L2U) + dq * $f64x::splat(-L2L);

            let u = $f64x::splat(0.160_247_221_970_993_207_2_e-9)
                .mul_add(s.0, $f64x::splat(0.209_225_518_356_315_700_7_e-8))
                .mul_add(s.0, $f64x::splat(0.250_523_002_378_264_446_5_e-7))
                .mul_add(s.0, $f64x::splat(0.275_572_480_090_213_530_3_e-6))
                .mul_add(s.0, $f64x::splat(0.275_573_189_238_604_437_3_e-5))
                .mul_add(s.0, $f64x::splat(0.248_015_873_560_581_506_5_e-4))
                .mul_add(s.0, $f64x::splat(0.198_412_698_414_807_185_8_e-3))
                .mul_add(s.0, $f64x::splat(0.138_888_888_888_676_325_5_e-2))
                .mul_add(s.0, $f64x::splat(0.833_333_333_333_334_709_5_e-2))
                .mul_add(s.0, $f64x::splat(0.416_666_666_666_666_990_5_e-1));

            let mut t = s * u + $f64x::splat(0.166_666_666_666_666_657_4);
            t = s * t + $f64x::splat(0.5);
            t = s + s.square() * t;

            t = ONE.add_checked(t);

            t.0 = vldexp2_vd_vd_vi(t.0, q);
            t.1 = vldexp2_vd_vd_vi(t.1, q);

            t.0 = $f64x::from_bits(vandnot_vm_vo64_vm(
                d.0.lt($f64x::splat(-1000.)),
                $u64x::from_bits(t.0),
            ));
            t.1 = $f64x::from_bits(vandnot_vm_vo64_vm(
                d.0.lt($f64x::splat(-1000.)),
                $u64x::from_bits(t.1),
            ));

            t
        }

        #[inline]
        fn logk2(d: Doubled<$f64x>) -> Doubled<$f64x> {
            let e = vilogbk_vi_vd(d.0 * $f64x::splat(1. / 0.75));

            let m = Doubled::new(vldexp2_vd_vd_vi(d.0, -e), vldexp2_vd_vd_vi(d.1, -e));

            let x = (m + $f64x::splat(-1.)) / (m + ONE);
            let x2 = x.square();

            let t = $f64x::splat(0.138_604_363_904_671_679_108_56)
                .mul_add(x2.0, $f64x::splat(0.131_699_838_841_615_374_240_845))
                .mul_add(x2.0, $f64x::splat(0.153_914_168_346_271_945_653_214))
                .mul_add(x2.0, $f64x::splat(0.181_816_523_941_564_611_721_589))
                .mul_add(x2.0, $f64x::splat(0.222_222_246_326_620_354_039_96))
                .mul_add(x2.0, $f64x::splat(0.285_714_285_511_134_091_777_308))
                .mul_add(x2.0, $f64x::splat(0.400_000_000_000_914_013_309_483))
                .mul_add(x2.0, $f64x::splat(0.666_666_666_666_664_853_302_393));

            let mut s = Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17))
                * $f64x::from_cast(e);
            s = s.add_checked(x.scale($f64x::splat(2.)));
            s.add_checked(x2 * x * t)
        }

        //
        #[inline]
        fn vcast_vi2_i_i(i0: i32, i1: i32) -> $i64x {
              const L : usize = $i64x::lanes();
              let mut a = [0_i32;L*2];
              for j in 0..L {
                a[2*j] = i0;
                a[2*j+1] = i1;
          }
          $i64x::from_bits(Simd::<[i32; L*2]>::from_slice_aligned(&a))
        }

        #[inline]
        fn vcast_vi2_u_u(i0: u32, i1: u32) -> $i64x {
              const L : usize = $i64x::lanes();
              let mut a = [0_u32;L*2];
              for j in 0..L {
                a[2*j] = i0;
                a[2*j+1] = i1;
          }
          $i64x::from_bits(Simd::<[u32; L*2]>::from_slice_aligned(&a))
        }

        #[inline]
        pub fn fabs(x: $f64x) -> $f64x {
            x.abs()
        }

        #[inline]
        pub fn copysign(x: $f64x, y: $f64x) -> $f64x {
            vcopysign_vd_vd_vd(x, y)
        }

        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))] //  && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
        pub fn fmax(x: $f64x, y: $f64x) -> $f64x {
            y.is_nan().select(x, x.max(y))
        }
        #[cfg(all(not(target_arch = "x86"), not(target_arch = "x86_64")))]
        pub fn fmax(x: $f64x, y: $f64x) -> $f64x {
            y.is_nan().select(x, x.gt(y).select(x, y))
        }
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))] //  && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
        pub fn fmin(x: $f64x, y: $f64x) -> $f64x {
            y.is_nan().select(x, x.min(y))
        }
        #[cfg(all(not(target_arch = "x86"), not(target_arch = "x86_64")))]
        pub fn fmin(x: $f64x, y: $f64x) -> $f64x {
            y.is_nan().select(x, y.gt(x).select(x, y))
        }

        pub fn fdim(x: $f64x, y: $f64x) -> $f64x {
            let ret = x - y;
            (ret.lt(ZERO) | x.eq(y)).select(ZERO, ret)
        }

        pub fn trunc(x: $f64x) -> $f64x {
            let mut fr = x
                - D1_31X
                    * $f64x::from_cast((x * (ONE / D1_31X)).truncatei());
            fr -= $f64x::from_cast(fr.truncatei());
            (x.is_infinite() | x.abs().ge(D1_52X))
                .select(x, vcopysign_vd_vd_vd(x - fr, x))
        }

        pub fn floor(x: $f64x) -> $f64x {
            let mut fr = x
                - D1_31X
                    * $f64x::from_cast((x * (ONE / D1_31X)).truncatei());
            fr -= $f64x::from_cast(fr.truncatei());
            fr = fr.lt(ZERO).select(fr + ONE, fr);
            (x.is_infinite() | x.abs().ge(D1_52X))
                .select(x, vcopysign_vd_vd_vd(x - fr, x))
        }

        pub fn ceil(x: $f64x) -> $f64x {
            let mut fr = x
                - D1_31X
                    * $f64x::from_cast((x * (ONE / D1_31X)).truncatei());
            fr -= $f64x::from_cast(fr.truncatei());
            fr = fr.le(ZERO).select(fr, fr - ONE);
            (x.is_infinite() | x.abs().ge(D1_52X))
                .select(x, vcopysign_vd_vd_vd(x - fr, x))
        }

        pub fn round(d: $f64x) -> $f64x {
            let mut x = d + $f64x::splat(0.5);
            let mut fr = x
                - D1_31X
                    * $f64x::from_cast((x * (ONE / D1_31X)).truncatei());
            fr -= $f64x::from_cast(fr.truncatei());
            x = (x.le(ZERO) & fr.eq(ZERO)).select(x - ONE, x);
            fr = fr.lt(ZERO).select(fr + ONE, fr);
            x = d
                .eq($f64x::splat(0.499_999_999_999_999_944_49))
                .select(ZERO, x);
            (d.is_infinite() | d.abs().ge(D1_52X))
                .select(d, vcopysign_vd_vd_vd(x - fr, d))
        }

        pub fn rint(d: $f64x) -> $f64x {
            let mut x = d + $f64x::splat(0.5);
            let mut fr = x
                - D1_31X
                    * $f64x::from_cast((x * (ONE / D1_31X)).truncatei());
            let isodd = $m64x::from_cast(($ix::splat(1) & fr.truncatei()).eq($ix::splat(1)));
            fr -= $f64x::from_cast(fr.truncatei());
            fr = (fr.lt(ZERO) | (fr.eq(ZERO) & isodd))
                .select(fr + ONE, fr);
            x = d
                .eq($f64x::splat(0.500_000_000_000_000_111_02))
                .select(ZERO, x);
            (d.is_infinite() | d.abs().ge(D1_52X))
                .select(d, vcopysign_vd_vd_vd(x - fr, d))
        }

        pub fn nextafter(x: $f64x, y: $f64x) -> $f64x {
            let x = x
                .eq(ZERO)
                .select(vmulsign_vd_vd_vd(ZERO, y), x);
            let mut xi2 = $i64x::from_bits(x);
            let c = x.is_sign_negative() ^ y.ge(x);

            let mut t = (xi2 ^ vcast_vi2_u_u(0x_7fff_ffff, 0x_ffff_ffff)) + vcast_vi2_i_i(0, 1);
            t += vrev21_vi2_vi2(vcast_vi2_i_i(0, 1) & veq_vi2_vi2_vi2(t, vcast_vi2_i_i(-1, 0)));
            xi2 = $i64x::from_bits(c.select($f64x::from_bits(t), $f64x::from_bits(xi2)));

            xi2 -= $i64x::from_cast(vand_vm_vo64_vm(x.ne(y), $u64x::from_u32((0, 1))));

            xi2 = $i64x::from_bits(x.ne(y).select(
                $f64x::from_bits(
                    xi2 + vrev21_vi2_vi2(
                        vcast_vi2_i_i(0, -1) & veq_vi2_vi2_vi2(xi2, vcast_vi2_i_i(0, -1)),
                    ),
                ),
                $f64x::from_bits(xi2),
            ));

            let mut t = (xi2 ^ vcast_vi2_u_u(0x_7fff_ffff, 0x_ffff_ffff)) + vcast_vi2_i_i(0, 1);
            t += vrev21_vi2_vi2(vcast_vi2_i_i(0, 1) & veq_vi2_vi2_vi2(t, vcast_vi2_i_i(-1, 0)));
            xi2 = $i64x::from_bits(c.select($f64x::from_bits(t), $f64x::from_bits(xi2)));

            let mut ret = $f64x::from_bits(xi2);

            ret = (ret.eq(ZERO) & x.ne(ZERO))
                .select(vmulsign_vd_vd_vd(ZERO, x), ret);

            ret = (x.eq(ZERO) & y.eq(ZERO)).select(y, ret);

            (x.is_nan() | y.is_nan()).select($f64x::NAN, ret)
        }

        pub fn frfrexp(x: $f64x) -> $f64x {
            let x = x
                .abs()
                .lt($f64x::splat(f64::MIN))
                .select(x * D1_63X, x);

            let mut xm = $u64x::from_bits(x);
            xm &= $u64x::from_u32((!0x_7ff0_0000, !0));
            xm |= $u64x::from_u32((0x_3fe0_0000, 0));

            let ret = $f64x::from_bits(xm);

            let ret =
                x.is_infinite().select(vmulsign_vd_vd_vd($f64x::INFINITY, x), ret);
            x.eq(ZERO).select(x, ret)
        }

        pub fn expfrexp(x: $f64x) -> $ix {
            let x = x
                .abs()
                .lt($f64x::splat(f64::MIN))
                .select(x * D1_63X, x);

            let mut ret = $ix::from_cast($u64x::from_bits(x));
            ret = ($ix::from_bits($ux::from_bits(ret) >> 20) & $ix::splat(0x7ff))
                - $ix::splat(0x3fe);

            (x.eq(ZERO) | x.is_nan() | x.is_infinite()).select($ix::splat(0), ret)
        }

        pub fn fma(mut x: $f64x, mut y: $f64x, mut z: $f64x) -> $f64x {
            let mut h2 = x * y + z;
            let mut q = ONE;
            const C0: $f64x = D1_54X;
            let c1: $f64x = C0 * C0;
            let c2: $f64x = c1 * c1;
            let o = h2.abs().lt($f64x::splat(1e-300));
            {
                x = o.select(x * c1, x);
                y = o.select(y * c1, y);
                z = o.select(z * c2, z);
                q = o.select(ONE / c2, q);
            }
            let o = h2.abs().gt($f64x::splat(1e+300));
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
            h2 = o.select(z, h2);

            let o = h2.is_infinite() | h2.is_nan();

            o.select(h2, ret * q)
        }

        //#[cfg(feature = "accurate_sqrt")]
        pub fn sqrt(d: $f64x) -> $f64x {
            d.sqrt()
        }
        // fall back to approximation if ACCURATE_SQRT is undefined
        /*#[cfg(not(feature = "accurate_sqrt"))]
        pub fn xsqrt(d: $f64x) -> $f64x {
            u05::sqrt(d)
        }*/

        #[inline]
        fn vtoward0(x: $f64x) -> $f64x {
            // returns nextafter(x, 0)
            let t = $f64x::from_bits($u64x::from_bits(x) + $u64x::from_bits(vcast_vi2_i_i(-1, -1)));
            x.eq(ZERO).select(ZERO, t)
        }
        #[cfg(feature = "full_fp_rounding")]
        #[inline]
        fn vptrunc(x: $f64x) -> $f64x {
            // round to integer toward 0, positive argument only
            x.truncate()
        }
        #[cfg(not(feature = "full_fp_rounding"))]
        #[inline]
        fn vptrunc(x: $f64x) -> $f64x {
            let mut fr = (-D1_31X).mul_add(
                $f64x::from_cast((x * (ONE / D1_31X)).truncatei()),
                x,
            );
            fr -= $f64x::from_cast(fr.truncatei());
            x.abs().ge(D1_52X).select(x, x - fr)
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        pub fn fmod(x: $f64x, y: $f64x) -> $f64x {
            let nu = x.abs();
            let de = y.abs();
            let s = ONE;
            let o = de.lt($f64x::splat(f64::MIN));
            let nu = o.select(nu * D1_54X, nu);
            let de = o.select(de * D1_54X, de);
            let s = o.select(s * (ONE / D1_54X), s);
            let rde = vtoward0(de.recpre());
            let mut r = Doubled::new(nu, ZERO);

            for _ in 0..21 {
                // ceil(log2(DBL_MAX) / 51) + 1
                let q =
                    ((de + de).gt(r.0) & r.0.ge(de)).select(ONE, vtoward0(r.0) * rde);
                let q = $f64x::from_bits(
                    $u64x::from_bits(vptrunc(q)) & $u64x::from_u32((0x_ffff_ffff, 0x_ffff_fffe)),
                );
                r = (r + q.mul_as_doubled(-de)).normalize();
                if r.0.lt(de).all() {
                    break;
                }
            }

            let mut ret = r.0 * s;
            ret = (r.0 + r.1).eq(de).select(ZERO, ret);

            ret = vmulsign_vd_vd_vd(ret, x);

            ret = nu.lt(de).select(x, ret);
            de.eq(ZERO).select($f64x::NAN, ret)
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        fn gammak(a: $f64x) -> (Doubled<$f64x>, Doubled<$f64x>) {
            let mut clln = Doubled::from((1., 0.));
            let mut clld = Doubled::from((1., 0.));

            let otiny = a.abs().lt($f64x::splat(1e-306));
            let oref = a.lt($f64x::splat(0.5));

            let mut x = otiny.select_doubled(Doubled::from((0., 0.)),
                oref.select_doubled(ONE.add_as_doubled(-a),
                    Doubled::new(a, ZERO),
                ),
            );

            let o0 = $f64x::splat(0.5).le(x.0) & x.0.le($f64x::splat(1.1));
            let o2 = $f64x::splat(2.3).le(x.0);

            let mut y = ((x + ONE) * x).normalize();
            y = ((x + $f64x::splat(2.)) * y).normalize();
            y = ((x + $f64x::splat(3.)) * y).normalize();
            y = ((x + $f64x::splat(4.)) * y).normalize();

            let o = o2 & x.0.le($f64x::splat(7.));
            clln = o.select_doubled(y, clln);

            x = o.select_doubled(x + $f64x::splat(5.), x);

            let t = o2.select(x.0.recpre(), (x + o0.select_splat(-1., -2.)).normalize().0);

            let u = vsel_vd_vo_vo_d_d_d(
                o2,
                o0,
                -156.801_412_704_022_726_379_848_862,
                0.294_791_677_282_761_419_6_e+2,
                0.707_481_600_086_460_927_9_e-7,
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    1.120_804_464_289_911_606_838_558_16,
                    0.128_145_969_182_782_010_9_e+3,
                    0.400_924_433_300_873_044_3_e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    13.397_985_455_142_589_218_333_060_2,
                    0.261_754_402_578_451_504_3_e+3,
                    0.104_011_464_162_824_694_6_e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.116_546_276_599_463_200_848_033_357,
                    0.328_702_285_568_579_043_2_e+3,
                    0.150_834_915_073_332_916_7_e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -1.391_801_093_265_337_481_495_562_41,
                    0.281_814_586_773_034_818_6_e+3,
                    0.128_814_307_493_390_102_e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.015_056_113_040_026_424_412_918_973_4,
                    0.172_867_041_467_355_960_5_e+3,
                    0.474_416_774_988_499_393_7_e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.179_540_117_061_234_856_098_844_714,
                    0.774_873_576_403_041_681_7_e+2,
                    -0.655_481_630_654_248_990_2_e-7,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.002_481_743_600_264_997_730_942_489_28,
                    0.251_285_664_308_093_075_2_e+2,
                    -0.318_925_247_145_259_984_4_e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.029_527_880_945_699_120_504_851_034_1,
                    0.576_679_210_614_007_686_8_e+1,
                    0.135_888_382_147_035_537_7_e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.000_540_164_767_892_604_515_196_325_186,
                    0.727_027_547_399_618_057_1,
                    -0.434_393_127_715_733_604_e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.006_403_362_833_808_069_794_787_256_2,
                    0.839_670_912_457_914_780_9_e-1,
                    0.972_478_589_740_677_955_5_e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.000_162_516_262_783_915_816_896_611_252,
                    -0.821_155_866_974_680_459_5_e-1,
                    -0.203_688_605_722_596_601_1_e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.001_914_438_498_565_477_526_465_972_39,
                    0.682_883_182_834_188_445_8_e-1,
                    0.437_336_314_181_972_581_5_e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    7.204_895_416_020_010_558_983_115_17_e-5,
                    -0.771_248_133_996_167_151_1_e-1,
                    -0.943_995_126_830_400_867_7_e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.000_839_498_720_672_087_279_971_000_786,
                    0.833_749_202_301_731_495_7_e-1,
                    0.205_072_703_037_638_980_4_e-4,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -5.171_790_908_260_592_193_293_944_22_e-5,
                    -0.909_496_493_145_624_251_8_e-1,
                    -0.449_262_018_343_118_401_8_e-4,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.000_592_166_437_353_693_882_857_342_347,
                    0.100_099_631_357_592_935_8,
                    0.994_575_123_607_187_593_1_e-4,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    6.972_813_758_365_857_774_037_435_39_e-5,
                    -0.111_334_286_154_420_772_4,
                    -0.223_154_759_903_498_319_6_e-3,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.000_784_039_221_720_066_627_493_314_301,
                    0.125_509_667_321_302_087_5,
                    0.509_669_524_710_196_762_2_e-3,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.000_229_472_093_621_399_176_949_318_732,
                    -0.144_049_896_784_305_436_8,
                    -0.119_275_391_166_788_697_1_e-2,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.002_681_327_160_493_827_160_473_958_490,
                    0.169_557_177_004_194_981_1,
                    0.289_051_033_074_221_031_e-2,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.003_472_222_222_222_222_222_175_164_840,
                    -0.207_385_551_028_409_276_2,
                    -0.738_555_102_867_446_185_8_e-2,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.083_333_333_333_333_333_335_592_087_900,
                    0.270_580_808_427_781_593_9,
                    0.205_808_084_277_845_533_5_e-1,
                ),
            );

            let mut y = (x + $f64x::splat(-0.5)) * logk2(x);
            y += -x;
            y += Doubled::from((0.918_938_533_204_672_780_56, -3.878_294_158_067_241_449_8_e-17)); // 0.5*log(2*M_PI)

            let mut z = u.mul_as_doubled(t)
                + o0.select_splat(-0.400_685_634_386_531_486_2, -0.673_523_010_531_981_020_1_e-1);
            z = z * t + o0.select_splat(0.822_467_033_424_113_203, 0.322_467_033_424_113_203);
            z = z * t + o0.select_splat(-0.577_215_664_901_532_865_5, 0.422_784_335_098_467_134_5);
            z *= t;

            let mut clc = o2.select_doubled(y, z);

            clld = o2.select_doubled(u.mul_as_doubled(t) + ONE, clld);

            y = clln;

            clc = otiny.select_doubled(Doubled::from((83.177_661_667_193_433_459_033_3, 3.671_034_596_315_685_072_218_78_e-15)), // log(2^120)
                oref.select_doubled(Doubled::<$f64x>::from((1.144_729_885_849_400_163_9, 1.026_595_116_270_782_638_e-17)) + (-clc),
                    clc,
                ),
            ); // log(M_PI)
            clln = otiny.select_doubled(Doubled::from((1., 0.)),
                oref.select_doubled(clln, clld),
            );

            if !(!oref).all() {
                let t = a - D1_28X * $f64x::from_cast((a * (ONE / D1_28X)).truncatei());
                x = clld * sinpik(t);
            }

            clld = otiny.select_doubled(Doubled::new(a * (D1_60X * D1_60X), ZERO),
                oref.select_doubled(x, y),
            );

            (clc, clln / clld)
        }

        #[inline]
        fn sinpik(d: $f64x) -> Doubled<$f64x> {
            let u = d * $f64x::splat(4.);
            let mut q = u.truncatei();
            q = (q + ($ix::from_bits($ux::from_bits(q) >> 31) ^ $ix::splat(1))) & $ix::splat(!1);
            let o = $m64x::from_cast((q & $ix::splat(2)).eq($ix::splat(2)));

            let s = u - $f64x::from_cast(q);
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_doubled(t);

            //

            let u = o.select_splat(9.944_803_876_268_437_740_902_08_e-16,
                -2.024_611_207_851_823_992_958_68_e-14,
            ).mul_add(
                s,
                o.select_splat(-3.897_962_260_629_327_991_640_47_e-13,
                    6.948_218_305_801_794_613_277_84_e-12,
                ),
            ).mul_add(
                s,
                o.select_splat(1.150_115_825_399_960_352_669_01_e-10,
                    -1.757_247_499_528_531_799_526_64_e-9,
                ),
            ).mul_add(
                s,
                o.select_splat(-2.461_136_950_104_469_749_535_9_e-8,
                    3.133_616_889_668_683_928_784_22_e-7,
                ),
            ).mul_add(
                s,
                o.select_splat(3.590_860_448_590_527_540_050_62_e-6,
                    -3.657_620_418_216_155_192_036_1_e-5,
                ),
            ).mul_add(
                s,
                o.select_splat(-0.000_325_991_886_927_389_905_997_954,
                    0.002_490_394_570_192_718_502_743_56,
                ),
            );
            let mut x = u * s + vsel_vd2_vo_d_d_d_d(
                o,
                0.015_854_344_243_815_501_891_425_9,
                -1.046_932_722_806_315_219_088_45_e-18,
                -0.080_745_512_188_280_785_248_473_1,
                3.618_524_750_670_371_048_499_87_e-18,
            );
            x = s2 * x + vsel_vd2_vo_d_d_d_d(
                o,
                -0.308_425_137_534_042_437_259_529,
                -1.956_984_921_336_335_503_383_45_e-17,
                0.785_398_163_397_448_278_999_491,
                3.062_871_137_271_550_026_071_05_e-17,
            );

            x *= o.select_doubled(s2, Doubled::new(t, ZERO));
            x = o.select_doubled(x + ONE, x);

            let o = $m64x::from_cast((q & $ix::splat(4)).eq($ix::splat(4)));
            x.0 = $f64x::from_bits(
                vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(x.0),
            );
            x.1 = $f64x::from_bits(
                vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(x.1),
            );

            x
        }

        pub fn modf(x: $f64x) -> ($f64x, $f64x) {
            let mut fr = x
                - D1_31X
                    * $f64x::from_cast((x * (ONE / D1_31X)).truncatei());
            fr -= $f64x::from_cast(fr.truncatei());
            fr = x.abs().gt(D1_52X).select(ZERO, fr);

            (vcopysign_vd_vd_vd(fr, x), vcopysign_vd_vd_vd(x - fr, x))
        }

        pub mod u05 {
            //! Functions with 0.5 ULP error bound
            use super::*;

            pub fn sincospi(d: $f64x) -> ($f64x, $f64x) {
                let u = d * $f64x::splat(4.);
                let mut q = u.truncatei();
                q = (q + ($ix::from_bits($ux::from_bits(q) >> 31) ^ $ix::splat(1))) & $ix::splat(!1);
                let s = u - $f64x::from_cast(q);

                let t = s;
                let s = s * s;
                let s2 = t.mul_as_doubled(t);

                //

                let u = $f64x::splat(-2.024_611_207_851_823_992_958_68_e-14)
                    .mul_add(s, $f64x::splat(6.948_218_305_801_794_613_277_84_e-12))
                    .mul_add(s, $f64x::splat(-1.757_247_499_528_531_799_526_64_e-9))
                    .mul_add(s, $f64x::splat(3.133_616_889_668_683_928_784_22_e-7))
                    .mul_add(s, $f64x::splat(-3.657_620_418_216_155_192_036_1_e-5))
                    .mul_add(s, $f64x::splat(0.002_490_394_570_192_718_502_743_56));
                let mut x =
                    u * s + Doubled::from((-0.080_745_512_188_280_785_248_473_1, 3.618_524_750_670_371_048_499_87_e-18));
                x = s2 * x + Doubled::from((0.785_398_163_397_448_278_999_491, 3.062_871_137_271_550_026_071_05_e-17));

                x *= t;
                let rx = x.0 + x.1;

                let rx = visnegzero_vo_vd(d).select($f64x::splat(-0.), rx);

                //

                let u = $f64x::splat(9.944_803_876_268_437_740_902_08_e-16)
                    .mul_add(s, $f64x::splat(-3.897_962_260_629_327_991_640_47_e-13))
                    .mul_add(s, $f64x::splat(1.150_115_825_399_960_352_669_01_e-10))
                    .mul_add(s, $f64x::splat(-2.461_136_950_104_469_749_535_9_e-8))
                    .mul_add(s, $f64x::splat(3.590_860_448_590_527_540_050_62_e-6))
                    .mul_add(s, $f64x::splat(-0.000_325_991_886_927_389_905_997_954));
                let mut x =
                    u * s + Doubled::from((0.015_854_344_243_815_501_891_425_9, -1.046_932_722_806_315_219_088_45_e-18));
                x = s2 * x + Doubled::from((-0.308_425_137_534_042_437_259_529, -1.956_984_921_336_335_503_383_45_e-17));

                x = x * s2 + ONE;
                let ry = x.0 + x.1;

                //

                let o = $m64x::from_cast((q & $ix::splat(2)).eq($ix::splat(0)));
                let mut rsin = o.select(rx, ry);
                let mut rcos = o.select(ry, rx);

                let o = $m64x::from_cast((q & $ix::splat(4)).eq($ix::splat(4)));
                rsin = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(rsin),
                );

                let o = $m64x::from_cast(((q + $ix::splat(2)) & $ix::splat(4)).eq($ix::splat(4)));
                rcos = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(rcos),
                );

                let o = d.abs().gt($f64x::splat(TRIGRANGEMAX3 / 4.));
                rsin = $f64x::from_bits(vandnot_vm_vo64_vm(o, $u64x::from_bits(rsin)));
                rcos = o.select(ONE, rcos);

                let o = d.is_infinite();
                rsin = $f64x::from_bits(vor_vm_vo64_vm(o, $u64x::from_bits(rsin)));
                rcos = $f64x::from_bits(vor_vm_vo64_vm(o, $u64x::from_bits(rcos)));

                (rsin, rcos)
            }

            pub fn sinpi(d: $f64x) -> $f64x {
                let x = sinpik(d);
                let mut r = x.0 + x.1;

                r = visnegzero_vo_vd(d).select($f64x::splat(-0.), r);
                r = $f64x::from_bits(vandnot_vm_vo64_vm(
                    d.abs().gt($f64x::splat(TRIGRANGEMAX3 / 4.)),
                    $u64x::from_bits(r),
                ));
                $f64x::from_bits(vor_vm_vo64_vm(d.is_infinite(), $u64x::from_bits(r)))
            }

            pub fn cospi(d: $f64x) -> $f64x {
                let x = cospik(d);
                let r = x.0 + x.1;

                let r = d
                    .abs()
                    .gt($f64x::splat(TRIGRANGEMAX3 / 4.))
                    .select(ONE, r);
                $f64x::from_bits(vor_vm_vo64_vm(d.is_infinite(), $u64x::from_bits(r)))
            }

            pub fn sqrt(d: $f64x) -> $f64x {
                let d = d.lt(ZERO).select($f64x::NAN, d);

                let o = d.lt($f64x::splat(8.636_168_555_094_445_e-78));
                let d = o.select(d * $f64x::splat(1.157_920_892_373_162_e77), d);
                let q = o.select(
                    $f64x::splat(2.938_735_877_055_718_8_e-39 * 0.5),
                    $f64x::splat(0.5),
                );

                let o = d.gt($f64x::splat(1.340_780_792_994_259_7_e+154));
                let d = o.select(d * $f64x::splat(7.458_340_731_200_207_e-155), d);
                let q = o.select($f64x::splat(1.157_920_892_373_162_e+77 * 0.5), q);

                let mut x = $f64x::from_bits(
                    vcast_vi2_i_i(0x_5fe6_ec86, 0)
                        - $i64x::from_bits($u64x::from_bits(d + $f64x::splat(1e-320)) >> 1),
                );

                x *= $f64x::splat(1.5) - $f64x::splat(0.5) * d * x * x;
                x *= $f64x::splat(1.5) - $f64x::splat(0.5) * d * x * x;
                x *= $f64x::splat(1.5) - $f64x::splat(0.5) * d * x * x;
                x *= d;

                let d2 = (d + x.mul_as_doubled(x)) * x.recpre_as_doubled();

                x = (d2.0 + d2.1) * q;

                x = d.eq($f64x::INFINITY).select($f64x::INFINITY, x);
                d.eq(ZERO).select(d, x)
            }

            pub fn hypot(x: $f64x, y: $f64x) -> $f64x {
                let x = x.abs();
                let y = y.abs();
                let min = x.min(y);
                let n = min;
                let max = x.max(y);
                let d = max;

                let o = max.lt($f64x::splat(f64::MIN));
                let n = o.select(n * D1_54X, n);
                let d = o.select(d * D1_54X, d);

                let t = Doubled::new(n, ZERO) / Doubled::new(d, ZERO);
                let t = (t.square() + ONE).sqrt() * max;
                let mut ret = t.0 + t.1;
                ret = ret.is_nan().select($f64x::INFINITY, ret);
                ret = min.eq(ZERO).select(max, ret);
                ret = (x.is_nan() | y.is_nan()).select($f64x::NAN, ret);
                (x.eq($f64x::INFINITY) | y.eq($f64x::INFINITY))
                    .select($f64x::INFINITY, ret)
            }

        }

        pub mod u10 {
            //! Functions with 1.0 ULP error bound
            use super::*;

            pub fn sin(d: $f64x) -> $f64x {
                let mut s: Doubled<$f64x>;
                let mut ql: $ix;

                if d.abs().lt($f64x::splat(TRIGRANGEMAX2)).all() {
                    let dql = (d * $f64x::FRAC_1_PI).rint();
                    ql = dql.rinti();
                    let u = dql.mul_add($f64x::splat(-PI_A2), d);
                    s = u.add_checked_as_doubled(dql * $f64x::splat(-PI_B2));
                } else if d.abs().lt($f64x::splat(TRIGRANGEMAX)).all() {
                    let dqh = (d * ($f64x::FRAC_1_PI / D1_24X)).truncate();
                    let dqh = dqh * D1_24X;
                    let dql = (d.mul_sub($f64x::FRAC_1_PI, dqh)).rint();
                    ql = dql.rinti();

                    let u = dqh.mul_add($f64x::splat(-PI_A), d);
                    s = u.add_checked_as_doubled(dql * $f64x::splat(-PI_A));
                    s += dqh * $f64x::splat(-PI_B);
                    s += dql * $f64x::splat(-PI_B);
                    s += dqh * $f64x::splat(-PI_C);
                    s += dql * $f64x::splat(-PI_C);
                    s += (dqh + dql) * $f64x::splat(-PI_D);
                } else {
                    let (mut ddidd, ddii) = rempi(d);
                    ql = ddii & $ix::splat(3);
                    ql = ql + ql + $mx::from_cast(ddidd.0.gt(ZERO))
                        .select($ix::splat(2), $ix::splat(1));
                    ql >>= 2;
                    let o = (ddii & $ix::splat(1)).eq($ix::splat(1));
                    let mut x = Doubled::new(
                        vmulsign_vd_vd_vd($f64x::splat(-3.141_592_653_589_793_116 * 0.5), ddidd.0),
                        vmulsign_vd_vd_vd($f64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5), ddidd.0),
                    );
                    x = ddidd + x;
                    ddidd = $m64x::from_cast(o).select_doubled(x, ddidd);
                    s = ddidd.normalize();
                    s.0 = $f64x::from_bits(vor_vm_vo64_vm(
                        d.is_infinite() | d.is_nan(),
                        $u64x::from_bits(s.0),
                    ));
                }

                let t = s;
                s = s.square();

                let mut u = $f64x::splat(2.720_524_161_385_295_679_179_83_e-15)
                    .mul_add(s.0, $f64x::splat(-7.642_925_941_139_544_719_002_3_e-13))
                    .mul_add(s.0, $f64x::splat(1.605_893_701_172_778_962_116_23_e-10))
                    .mul_add(s.0, $f64x::splat(-2.505_210_681_484_312_335_936_8_e-8))
                    .mul_add(s.0, $f64x::splat(2.755_731_921_044_282_247_773_79_e-6))
                    .mul_add(s.0, $f64x::splat(-0.000_198_412_698_412_046_454_654_947))
                    .mul_add(s.0, $f64x::splat(0.008_333_333_333_333_180_562_019_22));

                let x = ONE.add_checked(
                    ($f64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0)) * s,
                );

                u = t.mul_as_f(x);

                u = $f64x::from_bits(
                    vand_vm_vo64_vm(
                        $m64x::from_cast((ql & $ix::splat(1)).eq($ix::splat(1))),
                        $u64x::from_bits($f64x::splat(-0.)),
                    ) ^ $u64x::from_bits(u),
                );
                d.eq(ZERO).select(d, u)
            }

            pub fn cos(d: $f64x) -> $f64x {
                let mut s: Doubled<$f64x>;
                let mut ql: $ix;

                if d.abs().lt($f64x::splat(TRIGRANGEMAX2)).all() {
                    let dql = d.mul_add($f64x::FRAC_1_PI, $f64x::splat(-0.5)).rint();
                    let dql = $f64x::splat(2.).mul_add(dql, ONE);
                    ql = dql.rinti();
                    s = d.add_as_doubled(dql * $f64x::splat(-PI_A2 * 0.5));
                    s = s.add_checked(dql * $f64x::splat(-PI_B2 * 0.5));
                } else if d.abs().lt($f64x::splat(TRIGRANGEMAX)).all() {
                    let dqh = d
                        .mul_add($f64x::FRAC_1_PI / D1_23X, -$f64x::FRAC_1_PI / D1_24X)
                        .truncate();
                    ql = (d * $f64x::FRAC_1_PI
                        + dqh.mul_add(-D1_23X, $f64x::splat(-0.5))).rinti();
                    let dqh = dqh * D1_24X;
                    ql = ql + ql + $ix::splat(1);
                    let dql = $f64x::from_cast(ql);

                    let u = dqh.mul_add($f64x::splat(-PI_A * 0.5), d);
                    s = u.add_as_doubled(dql * $f64x::splat(-PI_A * 0.5));
                    s += dqh * $f64x::splat(-PI_B * 0.5);
                    s += dql * $f64x::splat(-PI_B * 0.5);
                    s += dqh * $f64x::splat(-PI_C * 0.5);
                    s += dql * $f64x::splat(-PI_C * 0.5);
                    s = s.add_checked((dqh + dql) * $f64x::splat(-PI_D * 0.5));
                } else {
                    let (mut ddidd, ddii) = rempi(d);
                    ql = ddii & $ix::splat(3);
                    ql = ql + ql + $mx::from_cast(ddidd.0.gt(ZERO))
                        .select($ix::splat(8), $ix::splat(7));
                    ql >>= 1;
                    let o = (ddii & $ix::splat(1)).eq($ix::splat(0));
                    let y = ddidd
                        .0
                        .gt(ZERO)
                        .select(ZERO, $f64x::splat(-1.));
                    let mut x = Doubled::new(
                        vmulsign_vd_vd_vd($f64x::splat(-3.141_592_653_589_793_116 * 0.5), y),
                        vmulsign_vd_vd_vd($f64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5), y),
                    );
                    x = ddidd + x;
                    ddidd = $m64x::from_cast(o).select_doubled(x, ddidd);
                    s = ddidd.normalize();
                    s.0 = $f64x::from_bits(vor_vm_vo64_vm(
                        d.is_infinite() | d.is_nan(),
                        $u64x::from_bits(s.0),
                    ));
                }

                let t = s;
                s = s.square();

                let u = $f64x::splat(2.720_524_161_385_295_679_179_83_e-15)
                    .mul_add(s.0, $f64x::splat(-7.642_925_941_139_544_719_002_3_e-13))
                    .mul_add(s.0, $f64x::splat(1.605_893_701_172_778_962_116_23_e-10))
                    .mul_add(s.0, $f64x::splat(-2.505_210_681_484_312_335_936_8_e-8))
                    .mul_add(s.0, $f64x::splat(2.755_731_921_044_282_247_773_79_e-6))
                    .mul_add(s.0, $f64x::splat(-0.000_198_412_698_412_046_454_654_947))
                    .mul_add(s.0, $f64x::splat(0.008_333_333_333_333_180_562_019_22));

                let x = ONE.add_checked(
                    ($f64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0)) * s,
                );

                let u = t.mul_as_f(x);

                $f64x::from_bits(
                    vand_vm_vo64_vm(
                        $m64x::from_cast((ql & $ix::splat(2)).eq($ix::splat(0))),
                        $u64x::from_bits($f64x::splat(-0.)),
                    ) ^ $u64x::from_bits(u),
                )
            }

            pub fn sincos(d: $f64x) -> ($f64x, $f64x) {
                let mut s: Doubled<$f64x>;
                let ql: $ix;

                if d.abs().lt($f64x::splat(TRIGRANGEMAX2)).all() {
                    let dql = (d * $f64x::FRAC_2_PI).rint();
                    ql = dql.rinti();
                    let u = dql.mul_add($f64x::splat(-PI_A2 * 0.5), d);
                    s = u.add_checked_as_doubled(dql * $f64x::splat(-PI_B2 * 0.5));
                } else if d.abs().lt($f64x::splat(TRIGRANGEMAX)).all() {
                    let dqh = (d * ($f64x::FRAC_2_PI / D1_24X)).truncate();
                    let dqh = dqh * D1_24X;
                    let dql = (d * $f64x::FRAC_2_PI - dqh).rint();
                    ql = dql.rinti();

                    let u = dqh.mul_add($f64x::splat(-PI_A * 0.5), d);
                    s = u.add_checked_as_doubled(dql * $f64x::splat(-PI_A * 0.5));
                    s += dqh * $f64x::splat(-PI_B * 0.5);
                    s += dql * $f64x::splat(-PI_B * 0.5);
                    s += dqh * $f64x::splat(-PI_C * 0.5);
                    s += dql * $f64x::splat(-PI_C * 0.5);
                    s += (dqh + dql) * $f64x::splat(-PI_D * 0.5);
                } else {
                    let (ddidd, ddii) = rempi(d);
                    ql = ddii;
                    s = ddidd;
                    let o = d.is_infinite() | d.is_nan();
                    s.0 = $f64x::from_bits(vor_vm_vo64_vm(o, $u64x::from_bits(s.0)));
                    s.1 = $f64x::from_bits(vor_vm_vo64_vm(o, $u64x::from_bits(s.1)));
                }

                let t = s;

                s.0 = s.square_as_f();

                let mut u = $f64x::splat(1.589_383_072_832_289_373_285_11_e-10)
                    .mul_add(s.0, $f64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
                    .mul_add(s.0, $f64x::splat(2.755_731_317_768_463_605_125_47_e-6))
                    .mul_add(s.0, $f64x::splat(-0.000_198_412_698_278_911_770_864_914))
                    .mul_add(s.0, $f64x::splat(0.008_333_333_333_319_184_596_174_6))
                    .mul_add(s.0, $f64x::splat(-0.166_666_666_666_666_130_709_393));

                u *= s.0 * t.0;

                let x = t.add_checked(u);
                let rx = x.0 + x.1;

                let rx = visnegzero_vo_vd(d).select($f64x::splat(-0.), rx);

                let u = $f64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
                    .mul_add(s.0, $f64x::splat(2.087_574_712_070_400_554_793_66_e-9))
                    .mul_add(s.0, $f64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
                    .mul_add(s.0, $f64x::splat(2.480_158_728_900_018_673_119_15_e-5))
                    .mul_add(s.0, $f64x::splat(-0.001_388_888_888_887_140_192_823_29))
                    .mul_add(s.0, $f64x::splat(0.041_666_666_666_666_551_959_206_2))
                    .mul_add(s.0, $f64x::splat(-0.5));

                let x = ONE.add_checked(s.0.mul_as_doubled(u));
                let ry = x.0 + x.1;

                let o = $m64x::from_cast((ql & $ix::splat(1)).eq($ix::splat(0)));
                let mut rsin = o.select(rx, ry);
                let mut rcos = o.select(ry, rx);

                let o = $m64x::from_cast((ql & $ix::splat(2)).eq($ix::splat(2)));
                rsin = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(rsin),
                );

                let o = $m64x::from_cast(((ql + $ix::splat(1)) & $ix::splat(2)).eq($ix::splat(2)));
                rcos = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(rcos),
                );

                (rsin, rcos)
            }

            pub fn tan(d: $f64x) -> $f64x {
                let mut s: Doubled<$f64x>;
                let ql: $ix;

                if d.abs().lt($f64x::splat(TRIGRANGEMAX2)).all() {
                    let dql = (d * $f64x::FRAC_2_PI).rint();
                    ql = dql.rinti();
                    let u = dql.mul_add($f64x::splat(-PI_A2 * 0.5), d);
                    s = u.add_checked_as_doubled(dql * $f64x::splat(-PI_B2 * 0.5));
                } else if d.abs().lt($f64x::splat(TRIGRANGEMAX)).all() {
                    let dqh = (d * ($f64x::FRAC_2_PI / D1_24X)).truncate();
                    let dqh = dqh * D1_24X;
                    s = Doubled::from((M_2_PI_H, M_2_PI_L)) * d
                        + (d.lt(ZERO)
                            .select($f64x::splat(-0.5), $f64x::splat(0.5))
                            - dqh);
                    let dql = (s.0 + s.1).truncate();
                    ql = dql.rinti();

                    let u = dqh.mul_add($f64x::splat(-PI_A * 0.5), d);
                    s = u.add_checked_as_doubled(dql * $f64x::splat(-PI_A * 0.5));
                    s += dqh * $f64x::splat(-PI_B * 0.5);
                    s += dql * $f64x::splat(-PI_B * 0.5);
                    s += dqh * $f64x::splat(-PI_C * 0.5);
                    s += dql * $f64x::splat(-PI_C * 0.5);
                    s += (dqh + dql) * $f64x::splat(-PI_D * 0.5);
                } else {
                    let (ddidd, ddii) = rempi(d);
                    ql = ddii;
                    s = ddidd;
                    let o = d.is_infinite() | d.is_nan();
                    s.0 = $f64x::from_bits(vor_vm_vo64_vm(o, $u64x::from_bits(s.0)));
                    s.1 = $f64x::from_bits(vor_vm_vo64_vm(o, $u64x::from_bits(s.1)));
                }

                let o = $m64x::from_cast((ql & $ix::splat(1)).eq($ix::splat(1)));
                let n = vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.)));
                s.0 = $f64x::from_bits($u64x::from_bits(s.0) ^ n);
                s.1 = $f64x::from_bits($u64x::from_bits(s.1) ^ n);

                let t = s;
                s = s.square();

                let mut u: $f64x;
                if cfg!(feature = "split_kernel") {
                    let sx2 = s.0 * s.0;

                    u = $f64x::splat(-2.595_197_915_859_246_976_986_14_e-5)
                        .mul_add(sx2, $f64x::splat(-3.050_330_144_339_464_882_256_16_e-5))
                        .mul_add(sx2, $f64x::splat(8.096_745_182_801_591_870_450_78_e-5))
                        .mul_add(sx2, $f64x::splat(0.000_588_505_168_743_587_154_904_506))
                        .mul_add(sx2, $f64x::splat(0.003_592_087_438_369_066_191_429_24))
                        .mul_add(sx2, $f64x::splat(0.021_869_488_285_384_638_959_207_8))
                        .mul_add(sx2, $f64x::splat(0.133_333_333_333_125_941_821_962));

                    let v = $f64x::splat(1.014_197_185_110_833_732_244_08_e-5)
                        .mul_add(sx2, $f64x::splat(5.233_880_819_158_998_553_251_86_e-5))
                        .mul_add(sx2, $f64x::splat(7.147_075_040_842_427_442_674_97_e-5))
                        .mul_add(sx2, $f64x::splat(0.000_244_884_931_879_331_847_054_404))
                        .mul_add(sx2, $f64x::splat(0.001_456_127_889_228_124_279_788_48))
                        .mul_add(sx2, $f64x::splat(0.008_863_239_443_624_016_181_133_56))
                        .mul_add(sx2, $f64x::splat(0.053_968_253_978_129_841_763_600_2));

                    u = v.mul_add(s.0, u);
                } else {
                    u = $f64x::splat(1.014_197_185_110_833_732_244_08_e-5)
                        .mul_add(s.0, $f64x::splat(-2.595_197_915_859_246_976_986_14_e-5))
                        .mul_add(s.0, $f64x::splat(5.233_880_819_158_998_553_251_86_e-5))
                        .mul_add(s.0, $f64x::splat(-3.050_330_144_339_464_882_256_16_e-5))
                        .mul_add(s.0, $f64x::splat(7.147_075_040_842_427_442_674_97_e-5))
                        .mul_add(s.0, $f64x::splat(8.096_745_182_801_591_870_450_78_e-5))
                        .mul_add(s.0, $f64x::splat(0.000_244_884_931_879_331_847_054_404))
                        .mul_add(s.0, $f64x::splat(0.000_588_505_168_743_587_154_904_506))
                        .mul_add(s.0, $f64x::splat(0.001_456_127_889_228_124_279_788_48))
                        .mul_add(s.0, $f64x::splat(0.003_592_087_438_369_066_191_429_24))
                        .mul_add(s.0, $f64x::splat(0.008_863_239_443_624_016_181_133_56))
                        .mul_add(s.0, $f64x::splat(0.021_869_488_285_384_638_959_207_8))
                        .mul_add(s.0, $f64x::splat(0.053_968_253_978_129_841_763_600_2))
                        .mul_add(s.0, $f64x::splat(0.133_333_333_333_125_941_821_962));
                }

                let mut x = ONE.add_checked(
                    $f64x::splat(0.333_333_333_333_334_980_164_153).add_checked_as_doubled(u * s.0) * s,
                );
                x = t * x;

                x = o.select_doubled(x.recpre(), x);

                let u = x.0 + x.1;

                d.eq(ZERO).select(d, u)
            }

            #[inline]
            fn atan2k_u1(y: Doubled<$f64x>, mut x: Doubled<$f64x>) -> Doubled<$f64x> {
                let q = vsel_vi_vd_vi(x.0, $ix::splat(-2));
                let p = x.0.lt(ZERO);
                let b = vand_vm_vo64_vm(p, $u64x::from_bits($f64x::splat(-0.)));
                x.0 = $f64x::from_bits(b ^ $u64x::from_bits(x.0));
                x.1 = $f64x::from_bits(b ^ $u64x::from_bits(x.1));

                let q = vsel_vi_vd_vd_vi_vi(x.0, y.0, q + $ix::splat(1), q);
                let p = x.0.lt(y.0);
                let s = p.select_doubled(-x, y);
                let mut t = p.select_doubled(y, x);

                let s = s / t;
                t = s.square();
                t = t.normalize();

                let mut u: $f64x;
                if cfg!(feature = "split_kernel") {
                    let tx3 = t.0 * t.0 * t.0;

                    u = $f64x::splat(0.000_705_576_642_963_934_123_897_74)
                        .mul_add(t.0, $f64x::splat(-0.002_518_656_144_987_133_603_529_99))
                        .mul_add(tx3, $f64x::splat(0.020_802_479_992_414_579_790_249_7))
                        .mul_add(t.0, $f64x::splat(-0.028_900_234_478_474_031_568_628_9))
                        .mul_add(tx3, $f64x::splat(0.047_084_301_165_328_398_819_376_3))
                        .mul_add(t.0, $f64x::splat(-0.052_491_421_058_844_842_106_871_9))
                        .mul_add(tx3, $f64x::splat(0.076_922_533_029_620_376_865_409_5))
                        .mul_add(t.0, $f64x::splat(-0.090_909_044_277_338_757_478_190_7))
                        .mul_add(tx3, $f64x::splat(0.199_999_999_997_977_351_284_817))
                        .mul_add(t.0, $f64x::splat(-0.333_333_333_333_317_605_173_818));

                    let v = $f64x::splat(1.062_984_841_914_487_466_074_15_e-5)
                        .mul_add(t.0, $f64x::splat(-0.000_125_620_649_967_286_867_384_336))
                        .mul_add(tx3, $f64x::splat(0.006_462_628_990_369_911_723_135_04))
                        .mul_add(t.0, $f64x::splat(-0.012_828_133_366_339_903_101_427_4))
                        .mul_add(tx3, $f64x::splat(0.035_978_500_503_510_459_085_365_6))
                        .mul_add(t.0, $f64x::splat(-0.041_848_579_703_592_507_506_027))
                        .mul_add(tx3, $f64x::splat(0.058_794_659_096_958_100_386_043_4))
                        .mul_add(t.0, $f64x::splat(-0.066_662_088_477_879_549_719_418_2))
                        .mul_add(tx3, $f64x::splat(0.111_111_108_376_896_236_538_123))
                        .mul_add(t.0, $f64x::splat(-0.142_857_142_756_268_568_062_339));

                    u = v.mul_add(t.0 * t.0, u);
                } else {
                    u = $f64x::splat(1.062_984_841_914_487_466_074_15_e-5)
                        .mul_add(t.0, $f64x::splat(-0.000_125_620_649_967_286_867_384_336))
                        .mul_add(t.0, $f64x::splat(0.000_705_576_642_963_934_123_897_74))
                        .mul_add(t.0, $f64x::splat(-0.002_518_656_144_987_133_603_529_99))
                        .mul_add(t.0, $f64x::splat(0.006_462_628_990_369_911_723_135_04))
                        .mul_add(t.0, $f64x::splat(-0.012_828_133_366_339_903_101_427_4))
                        .mul_add(t.0, $f64x::splat(0.020_802_479_992_414_579_790_249_7))
                        .mul_add(t.0, $f64x::splat(-0.028_900_234_478_474_031_568_628_9))
                        .mul_add(t.0, $f64x::splat(0.035_978_500_503_510_459_085_365_6))
                        .mul_add(t.0, $f64x::splat(-0.041_848_579_703_592_507_506_027))
                        .mul_add(t.0, $f64x::splat(0.047_084_301_165_328_398_819_376_3))
                        .mul_add(t.0, $f64x::splat(-0.052_491_421_058_844_842_106_871_9))
                        .mul_add(t.0, $f64x::splat(0.058_794_659_096_958_100_386_043_4))
                        .mul_add(t.0, $f64x::splat(-0.066_662_088_477_879_549_719_418_2))
                        .mul_add(t.0, $f64x::splat(0.076_922_533_029_620_376_865_409_5))
                        .mul_add(t.0, $f64x::splat(-0.090_909_044_277_338_757_478_190_7))
                        .mul_add(t.0, $f64x::splat(0.111_111_108_376_896_236_538_123))
                        .mul_add(t.0, $f64x::splat(-0.142_857_142_756_268_568_062_339))
                        .mul_add(t.0, $f64x::splat(0.199_999_999_997_977_351_284_817))
                        .mul_add(t.0, $f64x::splat(-0.333_333_333_333_317_605_173_818));
                }

                t *= u;
                t = s * ONE.add_checked(t);
                (Doubled::from((1.570_796_326_794_896_557_998_982, 6.123_233_995_736_766_035_868_82_e-17))
                    * $f64x::from_cast(q)).add_checked(t)
            }

            pub fn atan2(y: $f64x, x: $f64x) -> $f64x {
                let o = x.abs().lt($f64x::splat(5.562_684_646_268_008_398_4_e-309)); // nexttoward((1.0 / DBL_MAX), 1)
                let x = o.select(x * D1_53X, x);
                let y = o.select(y * D1_23X, y);

                let d = atan2k_u1(
                    Doubled::new(y.abs(), ZERO),
                    Doubled::new(x, ZERO),
                );
                let mut r = d.0 + d.1;

                r = vmulsign_vd_vd_vd(r, x);
                r = (x.is_infinite() | x.eq(ZERO)).select(
                    $f64x::FRAC_PI_2
                        - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd($f64x::FRAC_PI_2, x)),
                    r,
                );
                r = y.is_infinite().select(
                    $f64x::FRAC_PI_2
                        - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd($f64x::FRAC_PI_4, x)),
                    r,
                );
                r = y.eq(ZERO).select(
                    $f64x::from_bits(vand_vm_vo64_vm(
                        x.is_sign_negative(),
                        $u64x::from_bits($f64x::PI),
                    )),
                    r,
                );

                $f64x::from_bits(vor_vm_vo64_vm(
                    x.is_nan() | y.is_nan(),
                    $u64x::from_bits(vmulsign_vd_vd_vd(r, y)),
                ))
            }


            pub fn asin(d: $f64x) -> $f64x {
                let o = d.abs().lt($f64x::splat(0.5));
                let x2 = o.select(d * d, (ONE - d.abs()) * $f64x::splat(0.5));
                let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
                x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

                let mut u;

                if cfg!(feature = "split_kernel") {
                    let x4 = x2 * x2;

                    u = $f64x::splat(-0.158_191_824_332_999_664_3_e-1)
                        .mul_add(x4, $f64x::splat(0.660_607_747_627_717_061_e-2))
                        .mul_add(x4, $f64x::splat(0.138_871_518_450_160_921_8_e-1))
                        .mul_add(x4, $f64x::splat(0.223_717_618_193_204_834_1_e-1))
                        .mul_add(x4, $f64x::splat(0.446_428_568_137_710_243_8_e-1))
                        .mul_add(x4, $f64x::splat(0.166_666_666_666_649_754_3));

                    let v = $f64x::splat(0.316_158_765_065_393_462_8_e-1)
                        .mul_add(x4, $f64x::splat(0.192_904_547_726_791_067_4_e-1))
                        .mul_add(x4, $f64x::splat(0.121_536_052_557_737_733_1_e-1))
                        .mul_add(x4, $f64x::splat(0.173_595_699_122_361_460_4_e-1))
                        .mul_add(x4, $f64x::splat(0.303_819_592_803_813_223_7_e-1))
                        .mul_add(x4, $f64x::splat(0.750_000_000_037_858_161_1_e-1));

                    u = v.mul_add(x2, u);
                } else {
                    u = $f64x::splat(0.316_158_765_065_393_462_8_e-1)
                        .mul_add(x2, $f64x::splat(-0.158_191_824_332_999_664_3_e-1))
                        .mul_add(x2, $f64x::splat(0.192_904_547_726_791_067_4_e-1))
                        .mul_add(x2, $f64x::splat(0.660_607_747_627_717_061_e-2))
                        .mul_add(x2, $f64x::splat(0.121_536_052_557_737_733_1_e-1))
                        .mul_add(x2, $f64x::splat(0.138_871_518_450_160_921_8_e-1))
                        .mul_add(x2, $f64x::splat(0.173_595_699_122_361_460_4_e-1))
                        .mul_add(x2, $f64x::splat(0.223_717_618_193_204_834_1_e-1))
                        .mul_add(x2, $f64x::splat(0.303_819_592_803_813_223_7_e-1))
                        .mul_add(x2, $f64x::splat(0.446_428_568_137_710_243_8_e-1))
                        .mul_add(x2, $f64x::splat(0.750_000_000_037_858_161_1_e-1))
                        .mul_add(x2, $f64x::splat(0.166_666_666_666_649_754_3));
                }

                u *= x2 * x.0;

                let y = Doubled::from((3.141_592_653_589_793_116 / 4., 1.224_646_799_147_353_207_2_e-16 / 4.))
                    .sub_checked(x)
                    .sub_checked(u);

                let r = o.select(u + x.0, (y.0 + y.1) * $f64x::splat(2.));
                vmulsign_vd_vd_vd(r, d)
            }


            pub fn acos(d: $f64x) -> $f64x {
                let o = d.abs().lt($f64x::splat(0.5));
                let x2 = o.select(d * d, (ONE - d.abs()) * $f64x::splat(0.5));
                let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
                x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

                let mut u: $f64x;
                if cfg!(feature = "split_kernel") {
                    let x4 = x2 * x2;

                    u = $f64x::splat(-0.158_191_824_332_999_664_3_e-1)
                        .mul_add(x4, $f64x::splat(0.660_607_747_627_717_061_e-2))
                        .mul_add(x4, $f64x::splat(0.138_871_518_450_160_921_8_e-1))
                        .mul_add(x4, $f64x::splat(0.223_717_618_193_204_834_1_e-1))
                        .mul_add(x4, $f64x::splat(0.446_428_568_137_710_243_8_e-1))
                        .mul_add(x4, $f64x::splat(0.166_666_666_666_649_754_3));

                    let v = $f64x::splat(0.316_158_765_065_393_462_8_e-1)
                        .mul_add(x4, $f64x::splat(0.192_904_547_726_791_067_4_e-1))
                        .mul_add(x4, $f64x::splat(0.121_536_052_557_737_733_1_e-1))
                        .mul_add(x4, $f64x::splat(0.173_595_699_122_361_460_4_e-1))
                        .mul_add(x4, $f64x::splat(0.303_819_592_803_813_223_7_e-1))
                        .mul_add(x4, $f64x::splat(0.750_000_000_037_858_161_1_e-1));

                    u = v.mul_add(x2, u);
                } else {
                    u = $f64x::splat(0.316_158_765_065_393_462_8_e-1)
                        .mul_add(x2, $f64x::splat(-0.158_191_824_332_999_664_3_e-1))
                        .mul_add(x2, $f64x::splat(0.192_904_547_726_791_067_4_e-1))
                        .mul_add(x2, $f64x::splat(0.660_607_747_627_717_061_e-2))
                        .mul_add(x2, $f64x::splat(0.121_536_052_557_737_733_1_e-1))
                        .mul_add(x2, $f64x::splat(0.138_871_518_450_160_921_8_e-1))
                        .mul_add(x2, $f64x::splat(0.173_595_699_122_361_460_4_e-1))
                        .mul_add(x2, $f64x::splat(0.223_717_618_193_204_834_1_e-1))
                        .mul_add(x2, $f64x::splat(0.303_819_592_803_813_223_7_e-1))
                        .mul_add(x2, $f64x::splat(0.446_428_568_137_710_243_8_e-1))
                        .mul_add(x2, $f64x::splat(0.750_000_000_037_858_161_1_e-1))
                        .mul_add(x2, $f64x::splat(0.166_666_666_666_649_754_3));
                }

                u *= x2 * x.0;

                let mut y = Doubled::from((3.141_592_653_589_793_116 / 2., 1.224_646_799_147_353_207_2_e-16 / 2.))
                    .sub_checked(vmulsign_vd_vd_vd(x.0, d).add_checked_as_doubled(vmulsign_vd_vd_vd(u, d)));
                x = x.add_checked(u);

                y = o.select_doubled(y, x.scale($f64x::splat(2.)));

                y = vandnot_vo_vo_vo(o, d.lt(ZERO)).select_doubled(Doubled::from((3.141_592_653_589_793_116, 1.224_646_799_147_353_207_2_e-16)).sub_checked(y),
                    y,
                );

                y.0 + y.1
            }

            pub fn atan(d: $f64x) -> $f64x {
                let d2 = atan2k_u1(Doubled::new(d.abs(), ZERO), Doubled::from((1., 0.)));
                let mut r = d2.0 + d2.1;
                r = d.is_infinite().select($f64x::splat(1.570_796_326_794_896_557_998_982), r);
                vmulsign_vd_vd_vd(r, d)
            }


            pub fn exp(d: $f64x) -> $f64x {
                let mut u = (d * $f64x::splat(R_LN2)).rint();
                let q = u.rinti();

                let s = u.mul_add($f64x::splat(-L2U), d);
                let s = u.mul_add($f64x::splat(-L2L), s);

                if cfg!(target_feature = "fma") {
                    if cfg!(feature = "split_kernel") {
                        let s2 = s * s;

                        u = $f64x::splat(0.208_127_637_823_716_445_7_e-8)
                            .mul_adde(s2, $f64x::splat(0.275_576_262_816_949_119_2_e-6))
                            .mul_adde(s2, $f64x::splat(0.248_015_868_747_968_626_4_e-4))
                            .mul_adde(s2, $f64x::splat(0.138_888_888_891_449_779_7_e-2))
                            .mul_adde(s2, $f64x::splat(0.416_666_666_666_660_259_8_e-1))
                            .mul_adde(s2, $f64x::splat(0.5));

                        let v = $f64x::splat(0.251_121_070_304_228_802_2_e-7)
                            .mul_adde(s2, $f64x::splat(0.275_572_340_202_538_823_9_e-5))
                            .mul_adde(s2, $f64x::splat(0.198_412_698_985_586_585_e-3))
                            .mul_adde(s2, $f64x::splat(0.833_333_333_331_493_821_e-2))
                            .mul_adde(s2, $f64x::splat(0.166_666_666_666_666_907_2));

                        u = v
                            .mul_add(s, u)
                            .mul_adde(s, $f64x::splat(1.))
                            .mul_adde(s, $f64x::splat(1.));
                    } else {
                        u = $f64x::splat(0.208_127_637_823_716_445_7_e-8)
                            .mul_adde(s, $f64x::splat(0.251_121_070_304_228_802_2_e-7))
                            .mul_adde(s, $f64x::splat(0.275_576_262_816_949_119_2_e-6))
                            .mul_adde(s, $f64x::splat(0.275_572_340_202_538_823_9_e-5))
                            .mul_adde(s, $f64x::splat(0.248_015_868_747_968_626_4_e-4))
                            .mul_adde(s, $f64x::splat(0.198_412_698_985_586_585_e-3))
                            .mul_adde(s, $f64x::splat(0.138_888_888_891_449_779_7_e-2))
                            .mul_adde(s, $f64x::splat(0.833_333_333_331_493_821_e-2))
                            .mul_adde(s, $f64x::splat(0.416_666_666_666_660_259_8_e-1))
                            .mul_adde(s, $f64x::splat(0.166_666_666_666_666_907_2))
                            .mul_adde(s, $f64x::splat(0.5))
                            .mul_adde(s, $f64x::splat(1.))
                            .mul_adde(s, $f64x::splat(1.));
                    }
                } else {
                    u = $f64x::splat(2.088_606_211_072_836_875_363_41_e-9)
                        .mul_add(s, $f64x::splat(2.511_129_308_928_765_186_106_61_e-8))
                        .mul_add(s, $f64x::splat(2.755_739_112_349_004_718_933_38_e-7))
                        .mul_add(s, $f64x::splat(2.755_723_629_119_288_276_294_23_e-6))
                        .mul_add(s, $f64x::splat(2.480_158_715_923_547_299_879_1_e-5))
                        .mul_add(s, $f64x::splat(0.000_198_412_698_960_509_205_564_975))
                        .mul_add(s, $f64x::splat(0.001_388_888_888_897_744_922_079_62))
                        .mul_add(s, $f64x::splat(0.008_333_333_333_316_527_216_649_84))
                        .mul_add(s, $f64x::splat(0.041_666_666_666_666_504_759_142_2))
                        .mul_add(s, $f64x::splat(0.166_666_666_666_666_851_703_837))
                        .mul_add(s, $f64x::splat(0.5));

                    u = ONE + (s * s).mul_add(u, s);
                }

                u = vldexp2_vd_vd_vi(u, q);

                u = d
                    .gt($f64x::splat(709.782_711_149_557_429_092_172_174_26))
                    .select($f64x::INFINITY, u);
                $f64x::from_bits(vandnot_vm_vo64_vm(
                    d.lt($f64x::splat(-1000.)),
                    $u64x::from_bits(u),
                ))
            }

            pub fn log(mut d: $f64x) -> $f64x {
                let m: $f64x;
                let mut s =
                    /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                        let o = d.lt($f64x::splat(f64::MIN));
                        d = o.select(d * (D1_32X * D1_32X), d);
                        let mut e = vilogb2k_vi_vd(d * $f64x::splat(1. / 0.75));
                        m = vldexp3_vd_vd_vi(d, -e);
                        e = $mx::from_cast(o).select(e - $ix::splat(64), e);
                        Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17))
                            * $f64x::from_cast(e)
                    }/* else {
                        let mut e = vgetexp_vd_vd(d * $f64x::splat(1. / 0.75));
                        e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                        m = vgetmant_vd_vd(d);
                        Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17)) * e
                    }*/;

                let x = $f64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f64x::splat(0.153_207_698_850_270_135_3)
                    .mul_add(x2, $f64x::splat(0.152_562_905_100_342_871_6))
                    .mul_add(x2, $f64x::splat(0.181_860_593_293_778_599_6))
                    .mul_add(x2, $f64x::splat(0.222_221_451_983_938_000_9))
                    .mul_add(x2, $f64x::splat(0.285_714_293_279_429_931_7))
                    .mul_add(x2, $f64x::splat(0.399_999_999_963_525_199))
                    .mul_add(x2, $f64x::splat(0.666_666_666_666_733_354_1));

                s = s.add_checked(x.scale($f64x::splat(2.)));
                s = s.add_checked(x2 * x.0 * t);

                let r = s.0 + s.1;

                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    let r = d.eq($f64x::INFINITY).select($f64x::INFINITY, r);
                    let r =
                        (d.lt(ZERO) | d.is_nan()).select($f64x::NAN, r);
                    d.eq(ZERO)
                        .select($f64x::NEG_INFINITY, r)
                /*} else {
                    vfixup_vd_vd_vd_vi2_i(
                        r,
                        d,
                        $i64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                        0,
                    )
                }*/
            }

            pub fn pow(x: $f64x, y: $f64x) -> $f64x {
                if true {
                    let yisint = y.is_integer();
                    let yisodd = visodd_vo_vd(y) & yisint;

                    let d = logk(x.abs()) * y;
                    let mut result = expk(d);
                    result =
                        d.0.gt($f64x::splat(709.782_711_149_557_429_092_172_174_26))
                            .select($f64x::INFINITY, result);

                    result *= x.gt(ZERO).select(
                        ONE,
                        yisint.select(
                            yisodd.select($f64x::splat(-1.), ONE),
                            $f64x::NAN,
                        ),
                    );

                    let efx = vmulsign_vd_vd_vd(x.abs() - ONE, y);

                    result = y.is_infinite().select(
                        $f64x::from_bits(vandnot_vm_vo64_vm(
                            efx.lt(ZERO),
                            $u64x::from_bits(
                                efx.eq(ZERO)
                                    .select(ONE, $f64x::INFINITY),
                            ),
                        )),
                        result,
                    );

                    result = (x.is_infinite() | x.eq(ZERO)).select(
                        yisodd.select(vsign_vd_vd(x), ONE) * $f64x::from_bits(
                            vandnot_vm_vo64_vm(
                                x.eq(ZERO).select(-y, y).lt(ZERO),
                                $u64x::from_bits($f64x::INFINITY),
                            ),
                        ),
                        result,
                    );

                    result = $f64x::from_bits(vor_vm_vo64_vm(
                        x.is_nan() | y.is_nan(),
                        $u64x::from_bits(result),
                    ));

                    (y.eq(ZERO) | x.eq(ONE)).select(ONE, result)
                } else {
                    expk(logk(x) * y)
                }
            }

            pub fn sinh(x: $f64x) -> $f64x {
                let mut y = x.abs();
                let mut d = expk2(Doubled::new(y, ZERO));
                d = d.sub_checked(d.recpre());
                y = (d.0 + d.1) * $f64x::splat(0.5);

                y = (x.abs().gt($f64x::splat(710.)) | y.is_nan())
                    .select($f64x::INFINITY, y);
                y = vmulsign_vd_vd_vd(y, x);
                $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
            }

            pub fn cosh(x: $f64x) -> $f64x {
                let mut y = x.abs();
                let mut d = expk2(Doubled::new(y, ZERO));
                d = d.add_checked(d.recpre());
                y = (d.0 + d.1) * $f64x::splat(0.5);

                y = (x.abs().gt($f64x::splat(710.)) | y.is_nan())
                    .select($f64x::INFINITY, y);
                $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
            }

            pub fn tanh(x: $f64x) -> $f64x {
                let mut y = x.abs();
                let mut d = expk2(Doubled::new(y, ZERO));
                let e = d.recpre();
                d = (d + (-e)) / (d + e);
                y = d.0 + d.1;

                y = (x.abs().gt($f64x::splat(18.714_973_875)) | y.is_nan())
                    .select(ONE, y);
                y = vmulsign_vd_vd_vd(y, x);
                $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
            }

            pub fn asinh(x: $f64x) -> $f64x {
                let mut y = x.abs();
                let o = y.gt(ONE);

                let mut d = o.select_doubled(x.recpre_as_doubled(), Doubled::new(y, ZERO));
                d = (d.square() + ONE).sqrt();
                d = o.select_doubled(d * y, d);

                d = logk2((d + x).normalize());
                y = d.0 + d.1;

                y = (x.abs().gt($f64x::splat(SQRT_DBL_MAX)) | y.is_nan())
                    .select(vmulsign_vd_vd_vd($f64x::INFINITY, x), y);

                y = $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)));
                visnegzero_vo_vd(x).select($f64x::splat(-0.), y)
            }

            pub fn acosh(x: $f64x) -> $f64x {
                let d = logk2(
                    x.add_as_doubled(ONE).sqrt() * x.add_as_doubled($f64x::splat(-1.)).sqrt() + x,
                );
                let mut y = d.0 + d.1;

                y = (x.abs().gt($f64x::splat(SQRT_DBL_MAX)) | y.is_nan())
                    .select($f64x::INFINITY, y);
                y = $f64x::from_bits(vandnot_vm_vo64_vm(
                    x.eq(ONE),
                    $u64x::from_bits(y),
                ));

                y = $f64x::from_bits(vor_vm_vo64_vm(x.lt(ONE), $u64x::from_bits(y)));
                $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
            }

            pub fn atanh(x: $f64x) -> $f64x {
                let mut y = x.abs();
                let d = logk2(ONE.add_as_doubled(y) / ONE.add_as_doubled(-y));
                y = $f64x::from_bits(vor_vm_vo64_vm(
                    y.gt(ONE),
                    $u64x::from_bits(y.eq(ONE).select(
                        $f64x::INFINITY,
                        (d.0 + d.1) * $f64x::splat(0.5),
                    )),
                ));

                y = vmulsign_vd_vd_vd(y, x);
                y = $f64x::from_bits(vor_vm_vo64_vm(
                    x.is_infinite() | y.is_nan(),
                    $u64x::from_bits(y),
                ));
                $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
            }

            pub fn cbrt(mut d: $f64x) -> $f64x {
                let mut q2 = Doubled::from((1., 0.));

                /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                    let s = d;
                }*/
                let e = vilogbk_vi_vd(d.abs()) + $ix::splat(1);
                d = vldexp2_vd_vd_vi(d, -e);

                let t = $f64x::from_cast(e) + $f64x::splat(6144.);
                let qu = (t * $f64x::splat(1. / 3.)).truncatei();
                let re = (t - $f64x::from_cast(qu) * $f64x::splat(3.)).truncatei();

                q2 = $m64x::from_cast(re.eq($ix::splat(1))).select_doubled(Doubled::from((1.259_921_049_894_873_190_7, -2.589_933_375_300_506_917_7_e-17)),
                    q2,
                );
                q2 = $m64x::from_cast(re.eq($ix::splat(2))).select_doubled(Doubled::from((1.587_401_051_968_199_583_4, -1.086_900_819_419_782_298_6_e-16)),
                    q2,
                );

                q2.0 = vmulsign_vd_vd_vd(q2.0, d);
                q2.1 = vmulsign_vd_vd_vd(q2.1, d);
                d = d.abs();

                let mut x = $f64x::splat(-0.640_245_898_480_692_909_870_982)
                    .mul_add(d, $f64x::splat(2.961_551_030_200_395_118_185_95))
                    .mul_add(d, $f64x::splat(-5.733_530_609_229_478_436_361_66))
                    .mul_add(d, $f64x::splat(6.039_903_689_894_587_479_614_07))
                    .mul_add(d, $f64x::splat(-3.858_419_355_104_449_888_216_32))
                    .mul_add(d, $f64x::splat(2.230_727_530_249_660_972_572_2));

                let mut y = x * x;
                y = y * y;
                x -= d.mul_sub(y, x) * $f64x::splat(1. / 3.);

                let mut z = x;

                let mut u = x.mul_as_doubled(x);
                u = u * u;
                u *= d;
                u += -x;
                y = u.0 + u.1;

                y = $f64x::splat(-2. / 3.) * y * z;
                let mut v = z.mul_as_doubled(z) + y;
                v *= d;
                v *= q2;
                z = vldexp2_vd_vd_vi(v.0 + v.1, qu - $ix::splat(2048));

                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    z = d.is_infinite()
                        .select(vmulsign_vd_vd_vd($f64x::INFINITY, q2.0), z);
                    d.eq(ZERO)
                        .select($f64x::from_bits(vsignbit_vm_vd(q2.0)), z)
                /*} else {
                    z = s.is_infinite().select(vmulsign_vd_vd_vd($f64x::INFINITY, s), z);
                    s.eq(ZERO)
                        .select(vmulsign_vd_vd_vd(ZERO, s), z)
                }*/
            }

            pub fn exp2(d: $f64x) -> $f64x {
                let mut u = d.rint();
                let q = u.rinti();

                let s = d - u;

                if cfg!(feature = "split_kernel") {
                    let s2 = s * s;

                    u = $f64x::splat(0.443_435_908_292_652_945_4_e-9)
                        .mul_add(s2, $f64x::splat(0.101_781_926_092_176_045_1_e-6))
                        .mul_add(s2, $f64x::splat(0.152_527_335_351_758_473_e-4))
                        .mul_add(s2, $f64x::splat(0.133_335_581_467_049_907_3_e-2))
                        .mul_add(s2, $f64x::splat(0.555_041_086_648_204_659_6_e-1));

                    let v = $f64x::splat(0.707_316_459_808_570_742_5_e-8)
                        .mul_add(s2, $f64x::splat(0.132_154_387_251_132_761_5_e-5))
                        .mul_add(s2, $f64x::splat(0.154_035_304_510_114_780_8_e-3))
                        .mul_add(s2, $f64x::splat(0.961_812_910_759_760_053_6_e-2))
                        .mul_add(s2, $f64x::splat(0.240_226_506_959_101_221_4));

                    u = u
                        .mul_add(s, v)
                        .mul_add(s, $f64x::splat(0.693_147_180_559_945_286_2));
                } else {
                    u = $f64x::splat(0.443_435_908_292_652_945_4_e-9)
                        .mul_add(s, $f64x::splat(0.707_316_459_808_570_742_5_e-8))
                        .mul_add(s, $f64x::splat(0.101_781_926_092_176_045_1_e-6))
                        .mul_add(s, $f64x::splat(0.132_154_387_251_132_761_5_e-5))
                        .mul_add(s, $f64x::splat(0.152_527_335_351_758_473_e-4))
                        .mul_add(s, $f64x::splat(0.154_035_304_510_114_780_8_e-3))
                        .mul_add(s, $f64x::splat(0.133_335_581_467_049_907_3_e-2))
                        .mul_add(s, $f64x::splat(0.961_812_910_759_760_053_6_e-2))
                        .mul_add(s, $f64x::splat(0.555_041_086_648_204_659_6_e-1))
                        .mul_add(s, $f64x::splat(0.240_226_506_959_101_221_4))
                        .mul_add(s, $f64x::splat(0.693_147_180_559_945_286_2));
                }

                if cfg!(target_feature = "fma") {
                    u = u.mul_adde(s, ONE);
                } else {
                    u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
                }

                u = vldexp2_vd_vd_vi(u, q);

                u = d
                    .ge($f64x::splat(1024.))
                    .select($f64x::INFINITY, u);
                $f64x::from_bits(vandnot_vm_vo64_vm(
                    d.lt($f64x::splat(-2000.)),
                    $u64x::from_bits(u),
                ))
            }

            pub fn exp10(d: $f64x) -> $f64x {
                let mut u = (d * $f64x::splat(LOG10_2)).rint();
                let q = u.rinti();

                let s = u.mul_add($f64x::splat(-L10U), d);
                let s = u.mul_add($f64x::splat(-L10L), s);
                if cfg!(feature = "split_kernel") {
                    let s2 = s * s;

                    u = $f64x::splat(0.241_146_349_833_426_765_2_e-3)
                        .mul_add(s2, $f64x::splat(0.501_397_554_678_973_365_9_e-2))
                        .mul_add(s2, $f64x::splat(0.680_893_639_944_678_413_8_e-1))
                        .mul_add(s2, $f64x::splat(0.539_382_929_205_853_622_9))
                        .mul_add(s2, $f64x::splat(0.203_467_859_229_343_295_3_e+1));

                    let v = $f64x::splat(0.115_748_841_521_718_737_5_e-2)
                        .mul_add(s2, $f64x::splat(0.195_976_232_072_053_308_e-1))
                        .mul_add(s2, $f64x::splat(0.206_995_849_472_267_623_4))
                        .mul_add(s2, $f64x::splat(0.117_125_514_890_854_165_5_e+1))
                        .mul_add(s2, $f64x::splat(0.265_094_905_523_920_587_6_e+1));

                    u = u
                        .mul_add(s, v)
                        .mul_add(s, $f64x::splat(0.230_258_509_299_404_590_1_e+1));
                } else {
                    u = $f64x::splat(0.241_146_349_833_426_765_2_e-3)
                        .mul_add(s, $f64x::splat(0.115_748_841_521_718_737_5_e-2))
                        .mul_add(s, $f64x::splat(0.501_397_554_678_973_365_9_e-2))
                        .mul_add(s, $f64x::splat(0.195_976_232_072_053_308_e-1))
                        .mul_add(s, $f64x::splat(0.680_893_639_944_678_413_8_e-1))
                        .mul_add(s, $f64x::splat(0.206_995_849_472_267_623_4))
                        .mul_add(s, $f64x::splat(0.539_382_929_205_853_622_9))
                        .mul_add(s, $f64x::splat(0.117_125_514_890_854_165_5_e+1))
                        .mul_add(s, $f64x::splat(0.203_467_859_229_343_295_3_e+1))
                        .mul_add(s, $f64x::splat(0.265_094_905_523_920_587_6_e+1))
                        .mul_add(s, $f64x::splat(0.230_258_509_299_404_590_1_e+1));
                }

                if cfg!(target_feature = "fma") {
                    u = u.mul_adde(s, ONE);
                } else {
                    u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
                }

                u = vldexp2_vd_vd_vi(u, q);

                u = d
                    .gt($f64x::splat(308.254_715_559_916_71))
                    .select($f64x::INFINITY, u);
                $f64x::from_bits(vandnot_vm_vo64_vm(
                    d.lt($f64x::splat(-350.)),
                    $u64x::from_bits(u),
                ))
            }

            pub fn expm1(a: $f64x) -> $f64x {
                let d = expk2(Doubled::new(a, ZERO)) + $f64x::splat(-1.);
                let mut x = d.0 + d.1;
                x = a
                    .gt($f64x::splat(709.782_712_893_383_996_732_223))
                    .select($f64x::INFINITY, x);
                x = a
                    .lt($f64x::splat(-36.736_800_569_677_101_399_113_302_437))
                    .select($f64x::splat(-1.), x);
                visnegzero_vo_vd(a).select($f64x::splat(-0.), x)
            }

            pub fn log10(mut d: $f64x) -> $f64x {
                let m: $f64x;

                let mut s = /*if !cfg!(feature = "enable_avx512f")
                    && !cfg!(feature = "enable_avx512fnofma")*/
                {
                    let o = d.lt($f64x::splat(f64::MIN));
                    d = o.select(d * (D1_32X * D1_32X), d);
                    let mut e = vilogb2k_vi_vd(d * $f64x::splat(1. / 0.75));
                    m = vldexp3_vd_vd_vi(d, -e);
                    e = $mx::from_cast(o).select(e - $ix::splat(64), e);
                    Doubled::from((0.301_029_995_663_981_198_02, -2.803_728_127_785_170_339_e-18)) * $f64x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vd_vd(d * $f64x::splat(1. / 0.75));
                    e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                    m = vgetmant_vd_vd(d);
                    Doubled::from((0.301_029_995_663_981_198_02, -2.803_728_127_785_170_339_e-18)) * e
                }*/;

                let x = $f64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f64x::splat(0.665_372_581_957_675_846_e-1)
                    .mul_add(x2, $f64x::splat(0.662_572_278_282_083_371_2_e-1))
                    .mul_add(x2, $f64x::splat(0.789_810_521_431_394_407_8_e-1))
                    .mul_add(x2, $f64x::splat(0.965_095_503_571_527_513_2_e-1))
                    .mul_add(x2, $f64x::splat(0.124_084_140_972_144_499_3))
                    .mul_add(x2, $f64x::splat(0.173_717_792_745_460_508_6))
                    .mul_add(x2, $f64x::splat(0.289_529_654_602_197_261_7));

                s = s.add_checked(x * Doubled::from((0.868_588_963_806_503_633_34, 1.143_005_969_409_638_931_1_e-17)));
                s = s.add_checked(x2 * x.0 * t);

                let r = s.0 + s.1;

                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    let r = d.eq($f64x::INFINITY).select($f64x::INFINITY, r);
                    let r =
                        (d.lt(ZERO) | d.is_nan()).select($f64x::NAN, r);
                    d.eq(ZERO)
                        .select($f64x::NEG_INFINITY, r)
                /*} else {
                    vfixup_vd_vd_vd_vi2_i(
                        r,
                        d,
                        $i64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                        0,
                    )
                }*/
            }

            pub fn log2(mut d: $f64x) -> $f64x {
                let m: $f64x;
                let ef =
                    /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                        let o = d.lt($f64x::splat(f64::MIN));
                        d = o.select(d * (D1_32X * D1_32X), d);
                        let mut e = vilogb2k_vi_vd(d * $f64x::splat(1. / 0.75));
                        m = vldexp3_vd_vd_vi(d, -e);
                        e = $mx::from_cast(o).select(e - $ix::splat(64), e);
                        $f64x::from_cast(e)
                    }/* else {
                        let e = vgetexp_vd_vd(d * $f64x::splat(1.0 / 0.75));
                        e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                        m = vgetmant_vd_vd(d);
                        e
                    }*/;

                let x = $f64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f64x::splat(0.221_194_175_045_608_149)
                    .mul_add(x2, $f64x::splat(0.220_076_869_315_227_768_9))
                    .mul_add(x2, $f64x::splat(0.262_370_805_748_851_465_6))
                    .mul_add(x2, $f64x::splat(0.320_597_747_794_449_550_2))
                    .mul_add(x2, $f64x::splat(0.412_198_594_548_532_470_9))
                    .mul_add(x2, $f64x::splat(0.577_078_016_299_705_898_2))
                    .mul_add(x2, $f64x::splat(0.961_796_693_926_080_914_49));

                let mut s = ef + x * Doubled::from((2.885_390_081_777_926_774, 6.056_160_499_551_673_643_4_e-18));
                s += x2 * x.0 * t;

                let r = s.0 + s.1;

                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    let r = d.eq($f64x::INFINITY).select($f64x::INFINITY, r);
                    let r =
                        (d.lt(ZERO) | d.is_nan()).select($f64x::NAN, r);
                    d.eq(ZERO)
                        .select($f64x::NEG_INFINITY, r)
                /*} else {
                    vfixup_vd_vd_vd_vi2_i(
                        r,
                        d,
                        $i64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                        0,
                    )
                }*/
            }

            pub fn log1p(d: $f64x) -> $f64x {
                let m: $f64x;

                let mut dp1 = d + ONE;

                let mut s =
                    /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                        let o = dp1.lt($f64x::splat(f64::MIN));
                        dp1 = o.select(dp1 * (D1_32X * D1_32X), dp1);
                        let mut e = vilogb2k_vi_vd(dp1 * $f64x::splat(1. / 0.75));
                        let t = vldexp3_vd_vd_vi(ONE, -e);
                        m = d.mul_add(t, t - ONE);
                        e = $mx::from_cast(o).select(e - $ix::splat(64), e);
                        Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17))
                            * $f64x::from_cast(e)
                    }/* else {
                        let e = vgetexp_vd_vd(dp1, $f64x::splat(1. / 0.75));
                        e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                        let t = vldexp3_vd_vd_vi(ONE, -e.rinti());
                        m = d.mul_add(t, t - ONE);
                        Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17)) * e
                    }*/;

                let x = Doubled::new(m, ZERO) / $f64x::splat(2.).add_checked_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f64x::splat(0.153_207_698_850_270_135_3)
                    .mul_add(x2, $f64x::splat(0.152_562_905_100_342_871_6))
                    .mul_add(x2, $f64x::splat(0.181_860_593_293_778_599_6))
                    .mul_add(x2, $f64x::splat(0.222_221_451_983_938_000_9))
                    .mul_add(x2, $f64x::splat(0.285_714_293_279_429_931_7))
                    .mul_add(x2, $f64x::splat(0.399_999_999_963_525_199))
                    .mul_add(x2, $f64x::splat(0.666_666_666_666_733_354_1));

                s = s.add_checked(x.scale($f64x::splat(2.)));
                s = s.add_checked(x2 * x.0 * t);

                let mut r = s.0 + s.1;

                r = d
                    .gt($f64x::splat(1e+307))
                    .select($f64x::INFINITY, r);
                r = (d.lt($f64x::splat(-1.)) | d.is_nan()).select($f64x::NAN, r);
                r = d
                    .eq($f64x::splat(-1.))
                    .select($f64x::NEG_INFINITY, r);
                visnegzero_vo_vd(d).select($f64x::splat(-0.), r)
            }

            pub fn tgamma(a: $f64x) -> $f64x {
                let (da, db) = gammak(a);
                let y = expk2(da) * db;
                let r = y.0 + y.1;
                let o = a.eq($f64x::NEG_INFINITY)
                    | (a.lt(ZERO) & a.is_integer())
                    | (a.is_finite() & a.lt(ZERO) & r.is_nan());
                let r = o.select($f64x::NAN, r);

                let o = ((a.eq($f64x::INFINITY) | a.is_finite())
                    & a.ge($f64x::splat(-f64::MIN)))
                    & (a.eq(ZERO) | a.gt($f64x::splat(200.)) | r.is_nan());
                o.select(vmulsign_vd_vd_vd($f64x::INFINITY, a), r)
            }

            pub fn lgamma(a: $f64x) -> $f64x {
                let (da, db) = gammak(a);
                let y = da + logk2(db.abs());
                let r = y.0 + y.1;

                let o = a.is_infinite()
                    | (a.le(ZERO) & a.is_integer())
                    | (a.is_finite() & r.is_nan());
                o.select($f64x::INFINITY, r)
            }

            /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
            pub fn erf(a: $f64x) -> $f64x {
                let s = a;

                let a = a.abs();
                let o0 = a.lt(ONE);
                let o1 = a.lt($f64x::splat(3.7));
                let o2 = a.lt($f64x::splat(6.));
                let u = o0.select(a * a, a);

                let t = vsel_vd_vo_vo_d_d_d(
                    o0,
                    o1,
                    0.680_107_240_139_539_215_7_e-20,
                    0.283_095_452_208_771_766_e-13,
                    -0.584_675_040_426_961_049_3_e-17,
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.216_176_624_757_005_639_1_e-18,
                        -0.150_949_194_617_948_194_e-11,
                        0.607_669_104_881_260_789_8_e-15,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.469_591_917_330_159_875_2_e-17,
                        0.382_785_717_780_717_315_2_e-10,
                        -0.300_751_860_960_489_383_1_e-13,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.904_914_041_988_801_081_9_e-16,
                        -0.613_973_392_155_898_724_1_e-9,
                        0.942_790_626_082_464_606_3_e-12,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.163_401_890_355_741_151_7_e-14,
                        0.698_538_793_460_803_882_4_e-8,
                        -0.210_011_090_826_939_362_9_e-10,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.278_348_578_633_345_521_6_e-13,
                        -0.598_822_451_303_437_147_4_e-7,
                        0.353_463_952_346_122_347_3_e-9,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.446_322_127_678_641_272_2_e-12,
                        0.400_571_695_235_534_664_e-6,
                        -0.466_496_772_828_539_592_6_e-8,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.671_136_662_285_013_898_7_e-11,
                        -0.213_219_010_457_578_44_e-5,
                        0.494_382_328_376_900_053_2_e-7,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.942_275_905_023_265_834_6_e-10,
                        0.909_246_130_404_263_032_5_e-5,
                        -0.427_120_339_476_114_825_4_e-6,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.122_905_553_010_022_847_7_e-8,
                        -0.307_918_808_096_620_545_7_e-4,
                        0.303_406_767_740_491_589_5_e-5,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.148_071_928_158_508_502_3_e-7,
                        0.797_141_344_308_237_076_2_e-4,
                        -0.177_629_528_906_687_113_5_e-4,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.163_658_446_912_340_271_4_e-6,
                        -0.138_785_321_522_544_286_4_e-3,
                        0.852_454_763_055_950_505_e-4,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.164_621_143_658_892_336_3_e-5,
                        0.646_967_802_625_759_096_5_e-4,
                        -0.329_058_294_496_178_439_8_e-3,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.149_256_503_584_062_486_6_e-4,
                        0.499_664_528_037_294_586_e-3,
                        0.969_696_606_878_910_115_7_e-3,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.120_553_329_817_896_649_6_e-3,
                        -0.162_280_248_284_252_053_5_e-2,
                        -0.181_252_762_804_698_613_7_e-2,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.854_832_702_345_085_116_6_e-3,
                        0.161_532_055_704_937_717_1_e-3,
                        -0.472_540_982_812_361_901_7_e-3,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.522_397_762_544_218_879_9_e-2,
                        0.191_526_232_557_487_560_7_e-1,
                        0.209_031_542_792_422_926_6_e-1,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.268_661_706_451_312_556_9_e-1,
                        -0.102_781_829_848_603_345_5,
                        -0.105_204_192_184_277_664_5,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.112_837_916_709_551_275_3,
                        -0.636_617_281_984_250_382_7,
                        -0.634_535_180_876_656_834_7,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.376_126_389_031_837_538,
                        -0.112_837_959_064_891_046_9_e+1,
                        -0.112_944_292_910_352_439_6_e+1,
                    ),
                );
                let mut d = t.mul_as_doubled(u);

                d += Doubled::new(
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        1.128_379_167_095_512_558_6,
                        3.411_064_473_619_613_758_7_e-8,
                        0.000_249_630_356_905_264_382_85,
                    ),
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        1.533_545_961_316_582_267_4_e-17,
                        -2.487_565_070_832_329_424_6_e-24,
                        -5.436_266_503_485_625_979_5_e-21,
                    ),
                );
                d = o0.select_doubled(d * a, ONE.add_checked(-expk2(d)));

                let u = vmulsign_vd_vd_vd(o2.select(d.0 + d.1, ONE), s);
                a.is_nan().select($f64x::NAN, u)
            }

        }

        pub mod u15 {
            //! Functions with 1.5 ULP error bound
            use super::*;

            /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
            pub fn xerfc(a: $f64x) -> $f64x {
                let s = a;
                let a = a.abs();
                let o0 = a.lt(ONE);
                let o1 = a.lt($f64x::splat(2.2));
                let o2 = a.lt($f64x::splat(4.2));
                let o3 = a.lt($f64x::splat(27.3));

                let u = o0.select_doubled(a.mul_as_doubled(a),
                    o1.select_doubled(Doubled::new(a, ZERO),
                        Doubled::from((1., 0.)) / Doubled::new(a, ZERO),
                    ),
                );

                let t = vsel_vd_vo_vo_vo_d_d_d_d(
                    o0,
                    o1,
                    o2,
                    0.680_107_240_139_538_613_9_e-20,
                    0.343_801_034_136_258_530_3_e-12,
                    -0.575_781_953_642_071_044_9_e+2,
                    0.233_424_972_963_870_131_9_e+5,
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.216_176_624_757_005_566_9_e-18,
                        -0.123_702_118_816_059_826_4_e-10,
                        0.466_928_965_449_810_448_3_e+3,
                        -0.469_566_104_493_310_776_9_e+5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.469_591_917_330_159_567_e-17,
                        0.211_798_583_987_762_785_2_e-9,
                        -0.179_632_987_946_135_585_8_e+4,
                        0.317_340_310_874_864_335_3_e+5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.904_914_041_988_800_712_2_e-16,
                        -0.229_056_092_917_736_950_6_e-8,
                        0.435_589_219_369_957_572_8_e+4,
                        0.324_298_278_695_957_378_7_e+4,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.163_401_890_355_741_072_8_e-14,
                        0.174_893_162_169_814_953_8_e-7,
                        -0.745_625_888_496_576_499_2_e+4,
                        -0.201_471_799_976_034_781_1_e+5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.278_348_578_633_345_174_5_e-13,
                        -0.995_660_260_662_324_919_5_e-7,
                        0.955_397_735_816_702_152_1_e+4,
                        0.155_400_697_096_711_828_6_e+5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.446_322_127_678_641_575_2_e-12,
                        0.433_001_024_064_032_708_e-6,
                        -0.947_001_990_544_422_915_3_e+4,
                        -0.615_087_419_056_355_429_3_e+4,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.671_136_662_285_013_656_3_e-11,
                        -0.143_505_060_099_176_333_1_e-5,
                        0.738_734_432_184_985_507_8_e+4,
                        0.124_004_776_563_481_573_2_e+4,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.942_275_905_023_266_222_3_e-10,
                        0.346_013_947_965_069_566_2_e-5,
                        -0.455_771_305_416_638_279_e+4,
                        -0.821_032_547_575_269_973_1_e+2,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.122_905_553_010_022_909_8_e-8,
                        -0.498_890_818_063_289_817_3_e-5,
                        0.220_786_696_735_405_530_5_e+4,
                        0.324_244_388_083_993_087_e+2,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.148_071_928_158_508_651_2_e-7,
                        -0.130_877_597_632_635_201_2_e-5,
                        -0.821_797_565_862_175_474_6_e+3,
                        -0.292_341_886_383_316_058_6_e+2,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.163_658_446_912_339_980_3_e-6,
                        0.282_508_654_085_031_010_3_e-4,
                        0.226_865_948_350_791_74_e+3,
                        0.345_746_173_281_438_307_1,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.164_621_143_658_892_357_5_e-5,
                        -0.639_391_371_306_998_607_1_e-4,
                        -0.463_336_126_031_856_068_2_e+2,
                        0.548_973_015_595_239_299_8_e+1,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.149_256_503_584_062_351_1_e-4,
                        -0.256_643_651_469_507_892_6_e-4,
                        0.955_738_012_373_394_596_5_e+1,
                        0.155_993_413_225_129_413_4_e-2,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.120_553_329_817_896_785_1_e-3,
                        0.589_579_237_565_944_036_4_e-3,
                        -0.295_842_933_193_966_128_9_e+1,
                        -0.154_174_156_683_152_063_8_e+1,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.854_832_702_345_085_008_1_e-3,
                        -0.169_571_557_916_358_859_8_e-2,
                        0.167_032_950_809_276_548,
                        0.282_315_223_055_836_418_6_e-5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.522_397_762_544_218_793_2_e-2,
                        0.208_911_643_491_805_514_9_e-3,
                        0.609_661_568_011_541_921_1,
                        0.624_999_918_419_534_283_8,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.268_661_706_451_312_522_2_e-1,
                        0.191_285_594_958_491_775_3_e-1,
                        0.105_921_244_319_354_358_5_e-2,
                        0.174_174_941_640_870_128_8_e-8,
                    ),
                );

                let mut d = u * t;
                d += Doubled::new(
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.112_837_916_709_551_261_41,
                        -0.102_772_633_431_476_467_79,
                        -0.500_051_804_739_990_224_39,
                        -0.500_000_000_025_844_437_7,
                    ),
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -4.017_569_162_593_211_848_3_e-18,
                        -6.233_871_408_340_490_022_5_e-18,
                        2.636_214_056_904_199_580_3_e-17,
                        -4.007_404_471_238_699_228_1_e-17,
                    ),
                );
                d *= u;
                d += Doubled::new(
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.376_126_389_031_837_538_02,
                        -0.636_619_767_429_163_596_62,
                        1.601_106_273_924_963_368_e-6,
                        2.376_197_313_752_336_479_2_e-13,
                    ),
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        1.339_189_720_604_255_238_7_e-17,
                        7.632_101_915_908_572_466_2_e-18,
                        1.197_400_185_776_447_677_5_e-23,
                        -1.167_007_695_053_102_658_2_e-29,
                    ),
                );
                d *= u;
                d += Doubled::new(
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        1.128_379_167_095_512_558_6,
                        -1.128_379_167_471_729_616_1,
                        -0.572_364_966_451_454_293_41,
                        -0.572_364_942_924_701_081_14,
                    ),
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        1.533_545_961_316_582_267_4_e-17,
                        8.089_684_775_596_537_719_4_e-17,
                        3.070_455_324_587_202_725_8_e-17,
                        -2.398_435_220_805_689_800_3_e-17,
                    ),
                );

                let mut x = o1.select_doubled(d, Doubled::new(-a, ZERO)) * a;
                x = o1.select_doubled(x, x + d);
                x = o0.select_doubled(Doubled::from((1., 0.)).sub_checked(x), expk2(x));
                x = o1.select_doubled(x, x * u);

                let mut r = o3.select(x.0 + x.1, ZERO);
                r = s.is_sign_negative().select($f64x::splat(2.) - r, r);
                s.is_nan().select($f64x::NAN, r)
            }

        }

        pub mod u35 {
            //! Functions with 3.5 ULP error bound
            use super::*;

            pub fn sin(mut d: $f64x) -> $f64x {
                let r = d;
                let mut ql: $ix;

                if d.abs().lt($f64x::splat(TRIGRANGEMAX2)).all() {
                    let dql = (d * $f64x::FRAC_1_PI).rint();
                    ql = dql.rinti();
                    d = dql.mul_add($f64x::splat(-PI_A2), d);
                    d = dql.mul_add($f64x::splat(-PI_B2), d);
                } else if d.abs().lt($f64x::splat(TRIGRANGEMAX)).all() {
                    let dqh = (d * ($f64x::FRAC_1_PI / D1_24X)).truncate();
                    let dqh = dqh * D1_24X;
                    let dql = d.mul_sub($f64x::FRAC_1_PI, dqh).rint();
                    ql = dql.rinti();

                    d = dqh.mul_add($f64x::splat(-PI_A), d);
                    d = dql.mul_add($f64x::splat(-PI_A), d);
                    d = dqh.mul_add($f64x::splat(-PI_B), d);
                    d = dql.mul_add($f64x::splat(-PI_B), d);
                    d = dqh.mul_add($f64x::splat(-PI_C), d);
                    d = dql.mul_add($f64x::splat(-PI_C), d);
                    d = (dqh + dql).mul_add($f64x::splat(-PI_D), d);
                } else {
                    let (mut ddidd, ddii) = rempi(d);
                    ql = ddii & $ix::splat(3);
                    ql = ql + ql + $mx::from_cast(ddidd.0.gt(ZERO))
                        .select($ix::splat(2), $ix::splat(1));
                    ql >>= 2;
                    let o = (ddii & $ix::splat(1)).eq($ix::splat(1));
                    let mut x = Doubled::new(
                        vmulsign_vd_vd_vd($f64x::splat(-3.141_592_653_589_793_116 * 0.5), ddidd.0),
                        vmulsign_vd_vd_vd($f64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5), ddidd.0),
                    );
                    x = ddidd + x;
                    ddidd = $m64x::from_cast(o).select_doubled(x, ddidd);
                    d = ddidd.0 + ddidd.1;
                    d = $f64x::from_bits(vor_vm_vo64_vm(
                        r.is_infinite() | r.is_nan(),
                        $u64x::from_bits(d),
                    ));
                }

                let s = d * d;

                d = $f64x::from_bits(
                    vand_vm_vo64_vm(
                        $m64x::from_cast((ql & $ix::splat(1)).eq($ix::splat(1))),
                        $u64x::from_bits($f64x::splat(-0.)),
                    ) ^ $u64x::from_bits(d),
                );

                let mut u = $f64x::splat(-7.972_559_550_090_378_688_919_52_e-18)
                    .mul_add(s, $f64x::splat(2.810_099_727_108_632_000_912_51_e-15))
                    .mul_add(s, $f64x::splat(-7.647_122_191_181_588_332_884_84_e-13))
                    .mul_add(s, $f64x::splat(1.605_904_306_056_645_016_290_54_e-10))
                    .mul_add(s, $f64x::splat(-2.505_210_837_635_020_458_107_55_e-8))
                    .mul_add(s, $f64x::splat(2.755_731_922_391_987_476_304_16_e-6))
                    .mul_add(s, $f64x::splat(-0.000_198_412_698_412_696_162_806_809))
                    .mul_add(s, $f64x::splat(0.008_333_333_333_333_329_748_238_15))
                    .mul_add(s, $f64x::splat(-0.166_666_666_666_666_657_414_808));

                u = s * (u * d) + d;

                visnegzero_vo_vd(r).select(r, u)
            }

            pub fn cos(mut d: $f64x) -> $f64x {
                let r = d;
                let mut ql: $ix;

                if d.abs().lt($f64x::splat(TRIGRANGEMAX2)).all() {
                    let dql = $f64x::splat(2.).mul_add(
                        d.mul_add($f64x::FRAC_1_PI, $f64x::splat(-0.5)).rint(),
                        ONE,
                    );
                    ql = dql.rinti();
                    d = dql.mul_add($f64x::splat(-PI_A2 * 0.5), d);
                    d = dql.mul_add($f64x::splat(-PI_B2 * 0.5), d);
                } else if d.abs().lt($f64x::splat(TRIGRANGEMAX)).all() {
                    let dqh = d
                        .mul_add($f64x::FRAC_1_PI / D1_23X, -$f64x::FRAC_1_PI / D1_24X)
                        .truncate();
                    ql = (d * $f64x::FRAC_1_PI
                        + dqh.mul_add(-D1_23X, $f64x::splat(-0.5))).rinti();
                    let dqh = dqh * D1_24X;
                    ql = ql + ql + $ix::splat(1);
                    let dql = $f64x::from_cast(ql);

                    d = dqh.mul_add($f64x::splat(-PI_A * 0.5), d);
                    d = dql.mul_add($f64x::splat(-PI_A * 0.5), d);
                    d = dqh.mul_add($f64x::splat(-PI_B * 0.5), d);
                    d = dql.mul_add($f64x::splat(-PI_B * 0.5), d);
                    d = dqh.mul_add($f64x::splat(-PI_C * 0.5), d);
                    d = dql.mul_add($f64x::splat(-PI_C * 0.5), d);
                    d = (dqh + dql).mul_add($f64x::splat(-PI_D * 0.5), d);
                } else {
                    let (mut ddidd, ddii) = rempi(d);
                    ql = ddii & $ix::splat(3);
                    ql = ql + ql + $mx::from_cast(ddidd.0.gt(ZERO))
                        .select($ix::splat(8), $ix::splat(7));
                    ql >>= 1;
                    let o = (ddii & $ix::splat(1)).eq($ix::splat(0));
                    let y = ddidd
                        .0
                        .gt(ZERO)
                        .select(ZERO, $f64x::splat(-1.));
                    let mut x = Doubled::new(
                        vmulsign_vd_vd_vd($f64x::splat(-3.141_592_653_589_793_116 * 0.5), y),
                        vmulsign_vd_vd_vd($f64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5), y),
                    );
                    x = ddidd + x;
                    ddidd = $m64x::from_cast(o).select_doubled(x, ddidd);
                    d = ddidd.0 + ddidd.1;
                    d = $f64x::from_bits(vor_vm_vo64_vm(
                        r.is_infinite() | r.is_nan(),
                        $u64x::from_bits(d),
                    ));
                }

                let s = d * d;

                d = $f64x::from_bits(
                    vand_vm_vo64_vm(
                        $m64x::from_cast((ql & $ix::splat(2)).eq($ix::splat(0))),
                        $u64x::from_bits($f64x::splat(-0.)),
                    ) ^ $u64x::from_bits(d),
                );

                let u = $f64x::splat(-7.972_559_550_090_378_688_919_52_e-18)
                    .mul_add(s, $f64x::splat(2.810_099_727_108_632_000_912_51_e-15))
                    .mul_add(s, $f64x::splat(-7.647_122_191_181_588_332_884_84_e-13))
                    .mul_add(s, $f64x::splat(1.605_904_306_056_645_016_290_54_e-10))
                    .mul_add(s, $f64x::splat(-2.505_210_837_635_020_458_107_55_e-8))
                    .mul_add(s, $f64x::splat(2.755_731_922_391_987_476_304_16_e-6))
                    .mul_add(s, $f64x::splat(-0.000_198_412_698_412_696_162_806_809))
                    .mul_add(s, $f64x::splat(0.008_333_333_333_333_329_748_238_15))
                    .mul_add(s, $f64x::splat(-0.166_666_666_666_666_657_414_808));

                s * (u * d) + d
            }

            pub fn sincos(d: $f64x) -> ($f64x, $f64x) {
                let mut s: $f64x;
                let ql: $ix;

                if d.abs().lt($f64x::splat(TRIGRANGEMAX2)).all() {
                    let dql = (d * $f64x::FRAC_2_PI).rint();
                    ql = dql.rinti();
                    s = dql.mul_add($f64x::splat(-PI_A2 * 0.5), d);
                    s = dql.mul_add($f64x::splat(-PI_B2 * 0.5), s);
                } else if d.abs().lt($f64x::splat(TRIGRANGEMAX)).all() {
                    let dqh = (d * ($f64x::FRAC_2_PI / D1_24X)).truncate();
                    let dqh = dqh * D1_24X;
                    let dql = (d * $f64x::FRAC_2_PI - dqh).rint();
                    ql = dql.rinti();

                    s = dqh.mul_add($f64x::splat(-PI_A * 0.5), d);
                    s = dql.mul_add($f64x::splat(-PI_A * 0.5), s);
                    s = dqh.mul_add($f64x::splat(-PI_B * 0.5), s);
                    s = dql.mul_add($f64x::splat(-PI_B * 0.5), s);
                    s = dqh.mul_add($f64x::splat(-PI_C * 0.5), s);
                    s = dql.mul_add($f64x::splat(-PI_C * 0.5), s);
                    s = (dqh + dql).mul_add($f64x::splat(-PI_D * 0.5), s);
                } else {
                    let (ddidd, ddii) = rempi(d);
                    ql = ddii;
                    s = ddidd.0 + ddidd.1;
                    s = $f64x::from_bits(vor_vm_vo64_vm(
                        d.is_infinite() | d.is_nan(),
                        $u64x::from_bits(s),
                    ));
                }

                let t = s;

                s = s * s;

                let u = $f64x::splat(1.589_383_072_832_289_373_285_11_e-10)
                    .mul_add(s, $f64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
                    .mul_add(s, $f64x::splat(2.755_731_317_768_463_605_125_47_e-6))
                    .mul_add(s, $f64x::splat(-0.000_198_412_698_278_911_770_864_914))
                    .mul_add(s, $f64x::splat(0.008_333_333_333_319_184_596_174_6))
                    .mul_add(s, $f64x::splat(-0.166_666_666_666_666_130_709_393));

                let rx = (u * s).mul_add(t, t);
                let rx = visnegzero_vo_vd(d).select($f64x::splat(-0.), rx);

                let u = $f64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
                    .mul_add(s, $f64x::splat(2.087_574_712_070_400_554_793_66_e-9))
                    .mul_add(s, $f64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
                    .mul_add(s, $f64x::splat(2.480_158_728_900_018_673_119_15_e-5))
                    .mul_add(s, $f64x::splat(-0.001_388_888_888_887_140_192_823_29))
                    .mul_add(s, $f64x::splat(0.041_666_666_666_666_551_959_206_2))
                    .mul_add(s, $f64x::splat(-0.5));

                let ry = s.mul_add(u, ONE);

                let o = $m64x::from_cast((ql & $ix::splat(1)).eq($ix::splat(0)));
                let mut rsin = o.select(rx, ry);
                let mut rcos = o.select(ry, rx);

                let o = $m64x::from_cast((ql & $ix::splat(2)).eq($ix::splat(2)));
                rsin = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(rsin),
                );

                let o = $m64x::from_cast(((ql + $ix::splat(1)) & $ix::splat(2)).eq($ix::splat(2)));
                rcos = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(rcos),
                );

                (rsin, rcos)
            }

            pub fn sincospi(d: $f64x) -> ($f64x, $f64x) {
                let u = d * $f64x::splat(4.);
                let mut q = u.truncatei();
                q = (q + ($ix::from_bits($ux::from_bits(q) >> 31) ^ $ix::splat(1))) & $ix::splat(!1);
                let s = u - $f64x::from_cast(q);

                let t = s;
                let s = s * s;

                //

                let u = $f64x::splat(0.688_063_889_476_606_013_6_e-11)
                    .mul_add(s, $f64x::splat(-0.175_715_956_454_231_019_9_e-8))
                    .mul_add(s, $f64x::splat(0.313_361_632_725_786_731_1_e-6))
                    .mul_add(s, $f64x::splat(-0.365_762_041_638_848_645_2_e-4))
                    .mul_add(s, $f64x::splat(0.249_039_457_018_993_210_3_e-2))
                    .mul_add(s, $f64x::splat(-0.807_455_121_882_805_632_e-1))
                    .mul_add(s, $f64x::splat(0.785_398_163_397_448_279));

                let rx = u * t;

                //

                let u = $f64x::splat(-0.386_014_121_368_379_435_2_e-12)
                    .mul_add(s, $f64x::splat(0.115_005_788_802_968_141_5_e-9))
                    .mul_add(s, $f64x::splat(-0.246_113_649_300_666_355_3_e-7))
                    .mul_add(s, $f64x::splat(0.359_086_044_662_351_671_3_e-5))
                    .mul_add(s, $f64x::splat(-0.325_991_886_926_943_594_2_e-3))
                    .mul_add(s, $f64x::splat(0.158_543_442_438_154_116_9_e-1))
                    .mul_add(s, $f64x::splat(-0.308_425_137_534_042_437_3))
                    .mul_add(s, ONE);

                let ry = u;

                //

                let o = (q & $ix::splat(2)).eq($ix::splat(0));
                let mut rsin = o.select(rx, ry);
                let mut rcos = o.select(ry, rx);

                let o = $m64x::from_cast((q & $ix::splat(4)).eq($ix::splat(4)));
                rsin = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(rsin),
                );

                let o = $m64x::from_cast(((q + $ix::splat(2)) & $ix::splat(4)).eq($ix::splat(4)));
                rcos = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(rcos),
                );

                let o = d.abs().gt($f64x::splat(TRIGRANGEMAX3 / 4.));
                rsin = $f64x::from_bits(vandnot_vm_vo64_vm(o, $u64x::from_bits(rsin)));
                rcos = $f64x::from_bits(vandnot_vm_vo64_vm(o, $u64x::from_bits(rcos)));

                let o = d.is_infinite();
                rsin = $f64x::from_bits(vor_vm_vo64_vm(o, $u64x::from_bits(rsin)));
                rcos = $f64x::from_bits(vor_vm_vo64_vm(o, $u64x::from_bits(rcos)));

                (rsin, rcos)
            }
            pub fn tan(d: $f64x) -> $f64x {
                let ql: $ix;

                let mut x: $f64x;

                if d.abs().lt($f64x::splat(TRIGRANGEMAX2)).all() {
                    let dql = (d * $f64x::FRAC_2_PI).rint();
                    ql = dql.rinti();
                    x = dql.mul_add($f64x::splat(-PI_A2 * 0.5), d);
                    x = dql.mul_add($f64x::splat(-PI_B2 * 0.5), x);
                } else if d.abs().lt($f64x::splat(1e+7)).all() {
                    let dqh = (d * ($f64x::FRAC_2_PI / D1_24X)).truncate();
                    let dqh = dqh * D1_24X;
                    let dql = (d * $f64x::FRAC_2_PI - dqh).rint();
                    ql = dql.rinti();

                    x = dqh.mul_add($f64x::splat(-PI_A * 0.5), d);
                    x = dql.mul_add($f64x::splat(-PI_A * 0.5), x);
                    x = dqh.mul_add($f64x::splat(-PI_B * 0.5), x);
                    x = dql.mul_add($f64x::splat(-PI_B * 0.5), x);
                    x = dqh.mul_add($f64x::splat(-PI_C * 0.5), x);
                    x = dql.mul_add($f64x::splat(-PI_C * 0.5), x);
                    x = (dqh + dql).mul_add($f64x::splat(-PI_D * 0.5), x);
                } else {
                    let (ddidd, ddii) = rempi(d);
                    ql = ddii;
                    x = ddidd.0 + ddidd.1;
                    x = $f64x::from_bits(vor_vm_vo64_vm(d.is_infinite(), $u64x::from_bits(x)));
                    x = $f64x::from_bits(vor_vm_vo64_vm(
                        d.is_infinite() | d.is_nan(),
                        $u64x::from_bits(x),
                    ));
                }

                let s = x * x;

                let o = $m64x::from_cast((ql & $ix::splat(1)).eq($ix::splat(1)));
                x = $f64x::from_bits(
                    vand_vm_vo64_vm(o, $u64x::from_bits($f64x::splat(-0.))) ^ $u64x::from_bits(x),
                );

                let mut u: $f64x;
                if cfg!(feature = "split_kernel") {
                    let s2 = s * s;

                    u = $f64x::splat(-4.311_845_854_673_247_507_241_75_e-5)
                        .mul_add(s2, $f64x::splat(-0.000_137_892_809_714_281_708_733_524))
                        .mul_add(s2, $f64x::splat(-6.075_003_014_860_878_792_959_69_e-5))
                        .mul_add(s2, $f64x::splat(0.000_219_040_550_724_571_513_561_967))
                        .mul_add(s2, $f64x::splat(0.001_454_612_404_723_588_719_654_41))
                        .mul_add(s2, $f64x::splat(0.008_863_215_466_626_845_479_014_56))
                        .mul_add(s2, $f64x::splat(0.053_968_253_904_996_196_790_300_2))
                        .mul_add(s2, $f64x::splat(0.333_333_333_333_320_047_664_472));

                    let v = $f64x::splat(9.995_834_853_621_499_607_842_68_e-6)
                        .mul_add(s2, $f64x::splat(0.000_103_573_238_391_744_000_389_851))
                        .mul_add(s2, $f64x::splat(0.000_157_624_358_465_342_784_274_554))
                        .mul_add(s2, $f64x::splat(0.000_148_898_734_751_616_411_290_179))
                        .mul_add(s2, $f64x::splat(0.000_595_799_595_197_098_359_744_547))
                        .mul_add(s2, $f64x::splat(0.003_592_315_077_144_017_741_034_3))
                        .mul_add(s2, $f64x::splat(0.021_869_489_971_844_693_898_539_4))
                        .mul_add(s2, $f64x::splat(0.133_333_333_334_818_976_423_364));

                    u = v.mul_add(s, u);
                } else {
                    u = $f64x::splat(9.995_834_853_621_499_607_842_68_e-6)
                        .mul_add(s, $f64x::splat(-4.311_845_854_673_247_507_241_75_e-5))
                        .mul_add(s, $f64x::splat(0.000_103_573_238_391_744_000_389_851))
                        .mul_add(s, $f64x::splat(-0.000_137_892_809_714_281_708_733_524))
                        .mul_add(s, $f64x::splat(0.000_157_624_358_465_342_784_274_554))
                        .mul_add(s, $f64x::splat(-6.075_003_014_860_878_792_959_69_e-5))
                        .mul_add(s, $f64x::splat(0.000_148_898_734_751_616_411_290_179))
                        .mul_add(s, $f64x::splat(0.000_219_040_550_724_571_513_561_967))
                        .mul_add(s, $f64x::splat(0.000_595_799_595_197_098_359_744_547))
                        .mul_add(s, $f64x::splat(0.001_454_612_404_723_588_719_654_41))
                        .mul_add(s, $f64x::splat(0.003_592_315_077_144_017_741_034_3))
                        .mul_add(s, $f64x::splat(0.008_863_215_466_626_845_479_014_56))
                        .mul_add(s, $f64x::splat(0.021_869_489_971_844_693_898_539_4))
                        .mul_add(s, $f64x::splat(0.053_968_253_904_996_196_790_300_2))
                        .mul_add(s, $f64x::splat(0.133_333_333_334_818_976_423_364))
                        .mul_add(s, $f64x::splat(0.333_333_333_333_320_047_664_472));
                }

                u = s.mul_add(u * x, x);

                u = o.select(u.recpre(), u);
                d.eq(ZERO).select(d, u)
            }

            pub fn atan2(y: $f64x, x: $f64x) -> $f64x {
                let mut r = atan2k(y.abs(), x);

                r = vmulsign_vd_vd_vd(r, x);
                r = (x.is_infinite() | x.eq(ZERO)).select(
                    $f64x::FRAC_PI_2
                        - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd($f64x::FRAC_PI_2, x)),
                    r,
                );
                r = y.is_infinite().select(
                    $f64x::FRAC_PI_2
                        - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd($f64x::FRAC_PI_4, x)),
                    r,
                );
                r = y.eq(ZERO).select(
                    $f64x::from_bits(vand_vm_vo64_vm(
                        x.is_sign_negative(),
                        $u64x::from_bits($f64x::PI),
                    )),
                    r,
                );

                $f64x::from_bits(vor_vm_vo64_vm(
                    x.is_nan() | y.is_nan(),
                    $u64x::from_bits(vmulsign_vd_vd_vd(r, y)),
                ))
            }

            pub fn asin(d: $f64x) -> $f64x {
                let o = d.abs().lt($f64x::splat(0.5));
                let x2 = o.select(d * d, (ONE - d.abs()) * $f64x::splat(0.5));
                let x = o.select(d.abs(), x2.sqrt());

                let mut u;

                if cfg!(feature = "split_kernel") {
                    let x4 = x2 * x2;

                    u = $f64x::splat(-0.158_191_824_332_999_664_3_e-1)
                        .mul_add(x4, $f64x::splat(0.660_607_747_627_717_061_e-2))
                        .mul_add(x4, $f64x::splat(0.138_871_518_450_160_921_8_e-1))
                        .mul_add(x4, $f64x::splat(0.223_717_618_193_204_834_1_e-1))
                        .mul_add(x4, $f64x::splat(0.446_428_568_137_710_243_8_e-1))
                        .mul_add(x4, $f64x::splat(0.166_666_666_666_649_754_3));

                    let v = $f64x::splat(0.316_158_765_065_393_462_8_e-1)
                        .mul_add(x4, $f64x::splat(0.192_904_547_726_791_067_4_e-1))
                        .mul_add(x4, $f64x::splat(0.121_536_052_557_737_733_1_e-1))
                        .mul_add(x4, $f64x::splat(0.173_595_699_122_361_460_4_e-1))
                        .mul_add(x4, $f64x::splat(0.303_819_592_803_813_223_7_e-1))
                        .mul_add(x4, $f64x::splat(0.750_000_000_037_858_161_1_e-1));

                    u = v.mul_add(x2, u);
                } else {
                    u = $f64x::splat(0.316_158_765_065_393_462_8_e-1)
                        .mul_add(x2, $f64x::splat(-0.158_191_824_332_999_664_3_e-1))
                        .mul_add(x2, $f64x::splat(0.192_904_547_726_791_067_4_e-1))
                        .mul_add(x2, $f64x::splat(0.660_607_747_627_717_061_e-2))
                        .mul_add(x2, $f64x::splat(0.121_536_052_557_737_733_1_e-1))
                        .mul_add(x2, $f64x::splat(0.138_871_518_450_160_921_8_e-1))
                        .mul_add(x2, $f64x::splat(0.173_595_699_122_361_460_4_e-1))
                        .mul_add(x2, $f64x::splat(0.223_717_618_193_204_834_1_e-1))
                        .mul_add(x2, $f64x::splat(0.303_819_592_803_813_223_7_e-1))
                        .mul_add(x2, $f64x::splat(0.446_428_568_137_710_243_8_e-1))
                        .mul_add(x2, $f64x::splat(0.750_000_000_037_858_161_1_e-1))
                        .mul_add(x2, $f64x::splat(0.166_666_666_666_649_754_3));
                }

                u = u.mul_add(x * x2, x);

                let r = o.select(u, u.mul_add($f64x::splat(-2.), $f64x::FRAC_PI_2));
                vmulsign_vd_vd_vd(r, d)
            }
            pub fn acos(d: $f64x) -> $f64x {
                let o = d.abs().lt($f64x::splat(0.5));
                let x2 = o.select(d * d, (ONE - d.abs()) * $f64x::splat(0.5));
                let mut x = o.select(d.abs(), x2.sqrt());
                x = d.abs().eq(ONE).select(ZERO, x);

                let mut u;

                if cfg!(feature = "split_kernel") {
                    let x4 = x2 * x2;

                    u = $f64x::splat(-0.158_191_824_332_999_664_3_e-1)
                        .mul_add(x4, $f64x::splat(0.660_607_747_627_717_061_e-2))
                        .mul_add(x4, $f64x::splat(0.138_871_518_450_160_921_8_e-1))
                        .mul_add(x4, $f64x::splat(0.223_717_618_193_204_834_1_e-1))
                        .mul_add(x4, $f64x::splat(0.446_428_568_137_710_243_8_e-1))
                        .mul_add(x4, $f64x::splat(0.166_666_666_666_649_754_3));

                    let v = $f64x::splat(0.316_158_765_065_393_462_8_e-1)
                        .mul_add(x4, $f64x::splat(0.192_904_547_726_791_067_4_e-1))
                        .mul_add(x4, $f64x::splat(0.121_536_052_557_737_733_1_e-1))
                        .mul_add(x4, $f64x::splat(0.173_595_699_122_361_460_4_e-1))
                        .mul_add(x4, $f64x::splat(0.303_819_592_803_813_223_7_e-1))
                        .mul_add(x4, $f64x::splat(0.750_000_000_037_858_161_1_e-1));

                    u = v.mul_add(x2, u);
                } else {
                    u = $f64x::splat(0.316_158_765_065_393_462_8_e-1)
                        .mul_add(x2, $f64x::splat(-0.158_191_824_332_999_664_3_e-1))
                        .mul_add(x2, $f64x::splat(0.192_904_547_726_791_067_4_e-1))
                        .mul_add(x2, $f64x::splat(0.660_607_747_627_717_061_e-2))
                        .mul_add(x2, $f64x::splat(0.121_536_052_557_737_733_1_e-1))
                        .mul_add(x2, $f64x::splat(0.138_871_518_450_160_921_8_e-1))
                        .mul_add(x2, $f64x::splat(0.173_595_699_122_361_460_4_e-1))
                        .mul_add(x2, $f64x::splat(0.223_717_618_193_204_834_1_e-1))
                        .mul_add(x2, $f64x::splat(0.303_819_592_803_813_223_7_e-1))
                        .mul_add(x2, $f64x::splat(0.446_428_568_137_710_243_8_e-1))
                        .mul_add(x2, $f64x::splat(0.750_000_000_037_858_161_1_e-1))
                        .mul_add(x2, $f64x::splat(0.166_666_666_666_649_754_3));
                }

                u *= x2 * x;

                let y = $f64x::FRAC_PI_2 - (vmulsign_vd_vd_vd(x, d) + vmulsign_vd_vd_vd(u, d));
                x += u;
                let r = o.select(y, x * $f64x::splat(2.));
                vandnot_vo_vo_vo(o, d.lt(ZERO)).select(
                    Doubled::from((3.141_592_653_589_793_116, 1.224_646_799_147_353_207_2_e-16))
                        .add_checked(-r)
                        .0,
                    r,
                )
            }
            pub fn atan(mut s: $f64x) -> $f64x {
                let mut u;
                /*if cfg!(feature = "__intel_compiler") {
                    // && defined(ENABLE_PURECFMA_SCALAR)
                    let w = s;
                }*/

                let q = vsel_vi_vd_vi(s, $ix::splat(2));
                s = s.abs();

                let q = vsel_vi_vd_vd_vi_vi(ONE, s, q + $ix::splat(1), q);
                s = ONE.lt(s).select(s.recpre(), s);

                let mut t = s * s;

                if cfg!(feature = "split_kernel") {
                    let t2 = t * t;

                    u = $f64x::splat(-1.887_960_084_630_734_965_637_46_e-5)
                        .mul_add(t2, $f64x::splat(-0.001_106_118_314_866_724_825_634_71))
                        .mul_add(t2, $f64x::splat(-0.008_898_961_958_876_554_917_408_09))
                        .mul_add(t2, $f64x::splat(-0.025_451_762_493_231_264_161_686_1))
                        .mul_add(t2, $f64x::splat(-0.040_762_919_127_683_650_000_193_4))
                        .mul_add(t2, $f64x::splat(-0.052_367_485_230_348_245_761_611_3))
                        .mul_add(t2, $f64x::splat(-0.066_657_357_936_108_052_598_456_2))
                        .mul_add(t2, $f64x::splat(-0.090_908_995_008_245_008_229_153))
                        .mul_add(t2, $f64x::splat(-0.142_857_142_667_713_293_837_65))
                        .mul_add(t2, $f64x::splat(-0.333_333_333_333_311_110_369_124));

                    let v = $f64x::splat(0.000_209_850_076_645_816_976_906_797)
                        .mul_add(t2, $f64x::splat(0.003_700_267_441_887_131_192_324_03))
                        .mul_add(t2, $f64x::splat(0.016_599_329_773_529_201_970_117))
                        .mul_add(t2, $f64x::splat(0.033_785_258_000_135_306_999_389_7))
                        .mul_add(t2, $f64x::splat(0.046_666_715_007_784_062_563_267_5))
                        .mul_add(t2, $f64x::splat(0.058_766_639_292_667_358_085_431_3))
                        .mul_add(t2, $f64x::splat(0.076_921_953_831_176_961_835_502_9))
                        .mul_add(t2, $f64x::splat(0.111_111_105_648_261_418_443_745))
                        .mul_add(t2, $f64x::splat(0.199_999_999_996_591_265_594_148));

                    u = v.mul_add(t, u);
                } else {
                    u = $f64x::splat(-1.887_960_084_630_734_965_637_46_e-5)
                        .mul_add(t, $f64x::splat(0.000_209_850_076_645_816_976_906_797))
                        .mul_add(t, $f64x::splat(-0.001_106_118_314_866_724_825_634_71))
                        .mul_add(t, $f64x::splat(0.003_700_267_441_887_131_192_324_03))
                        .mul_add(t, $f64x::splat(-0.008_898_961_958_876_554_917_408_09))
                        .mul_add(t, $f64x::splat(0.016_599_329_773_529_201_970_117))
                        .mul_add(t, $f64x::splat(-0.025_451_762_493_231_264_161_686_1))
                        .mul_add(t, $f64x::splat(0.033_785_258_000_135_306_999_389_7))
                        .mul_add(t, $f64x::splat(-0.040_762_919_127_683_650_000_193_4))
                        .mul_add(t, $f64x::splat(0.046_666_715_007_784_062_563_267_5))
                        .mul_add(t, $f64x::splat(-0.052_367_485_230_348_245_761_611_3))
                        .mul_add(t, $f64x::splat(0.058_766_639_292_667_358_085_431_3))
                        .mul_add(t, $f64x::splat(-0.066_657_357_936_108_052_598_456_2))
                        .mul_add(t, $f64x::splat(0.076_921_953_831_176_961_835_502_9))
                        .mul_add(t, $f64x::splat(-0.090_908_995_008_245_008_229_153))
                        .mul_add(t, $f64x::splat(0.111_111_105_648_261_418_443_745))
                        .mul_add(t, $f64x::splat(-0.142_857_142_667_713_293_837_65))
                        .mul_add(t, $f64x::splat(0.199_999_999_996_591_265_594_148))
                        .mul_add(t, $f64x::splat(-0.333_333_333_333_311_110_369_124));
                }

                t = s.mul_add(t * u, s);

                t = $m64x::from_cast((q & $ix::splat(1)).eq($ix::splat(1)))
                    .select($f64x::FRAC_PI_2 - t, t);
                t = $f64x::from_bits(
                    vand_vm_vo64_vm(
                        $m64x::from_cast((q & $ix::splat(2)).eq($ix::splat(2))),
                        $u64x::from_bits($f64x::splat(-0.)),
                    ) ^ $u64x::from_bits(t),
                );

                /*if cfg!(feature = "__intel_compiler") {
                    // && defined(ENABLE_PURECFMA_SCALAR)
                    t = w.eq(ZERO).select(w, t);
                }*/
                t
            }

            pub fn log(mut d: $f64x) -> $f64x {
                let m: $f64x;

                let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
                {
                    let o = d.lt($f64x::splat(f64::MIN));
                    d = o.select(d * (D1_32X * D1_32X), d);
                    let mut e = vilogb2k_vi_vd(d * $f64x::splat(1. / 0.75));
                    m = vldexp3_vd_vd_vi(d, -e);
                    e = $mx::from_cast(o).select(e - $ix::splat(64), e);
                    $f64x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vd_vd(d * $f64x::splat(1. / 0.75));
                    e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                    m = vgetmant_vd_vd(d);
                    e
                }*/;

                let mut x = ($f64x::splat(-1.) + m) / (ONE + m);
                let x2 = x * x;

                let t = $f64x::splat(0.153_487_338_491_425_068_243_146)
                    .mul_add(x2, $f64x::splat(0.152_519_917_006_351_951_593_857))
                    .mul_add(x2, $f64x::splat(0.181_863_266_251_982_985_677_316))
                    .mul_add(x2, $f64x::splat(0.222_221_366_518_767_365_905_163))
                    .mul_add(x2, $f64x::splat(0.285_714_294_746_548_025_383_248))
                    .mul_add(x2, $f64x::splat(0.399_999_999_950_799_600_689_777))
                    .mul_add(x2, $f64x::splat(0.666_666_666_666_777_874_006_3))
                    .mul_add(x2, $f64x::splat(2.));

                x = x.mul_add(t, $f64x::splat(0.693_147_180_559_945_286_226_764) * ef);
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                    x = d.eq($f64x::INFINITY).select($f64x::INFINITY, x);
                    x = (d.lt(ZERO) | d.is_nan()).select($f64x::NAN, x);
                    d.eq(ZERO)
                        .select($f64x::NEG_INFINITY, x)
               /* } else {
                    vfixup_vd_vd_vd_vi2_i(x, d, $i64x::splat((5 << (5 * 4))), 0)
                }*/
            }

            pub fn sinh(x: $f64x) -> $f64x {
                let e = expm1k(x.abs());

                let mut y = (e + $f64x::splat(2.)) / (e + ONE);
                y *= $f64x::splat(0.5) * e;

                y = (x.abs().gt($f64x::splat(709.)) | y.is_nan())
                    .select($f64x::INFINITY, y);
                y = vmulsign_vd_vd_vd(y, x);
                $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
            }

            pub fn cosh(x: $f64x) -> $f64x {
                let e = u10::exp(x.abs());
                let mut y = $f64x::splat(0.5).mul_add(e, $f64x::splat(0.5) / e);

                y = (x.abs().gt($f64x::splat(709.)) | y.is_nan())
                    .select($f64x::INFINITY, y);
                $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
            }

            pub fn tanh(x: $f64x) -> $f64x {
                let d = expm1k($f64x::splat(2.) * x.abs());
                let mut y = d / ($f64x::splat(2.) + d);

                y = (x.abs().gt($f64x::splat(18.714_973_875)) | y.is_nan())
                    .select(ONE, y);
                y = vmulsign_vd_vd_vd(y, x);
                $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
            }

            pub fn cbrt(mut d: $f64x) -> $f64x {
                let mut q = ONE;
                /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                    let s = d;
                }*/
                let e = vilogbk_vi_vd(d.abs()) + $ix::splat(1);
                d = vldexp2_vd_vd_vi(d, -e);

                let t = $f64x::from_cast(e) + $f64x::splat(6144.);
                let qu = (t * $f64x::splat(1. / 3.)).truncatei();
                let re = (t * ($f64x::from_cast(qu) * $f64x::splat(3.))).truncatei();

                q = $m64x::from_cast(re.eq($ix::splat(1)))
                    .select($f64x::splat(1.259_921_049_894_873_164_767_210_6), q);
                q = $m64x::from_cast(re.eq($ix::splat(2)))
                    .select($f64x::splat(1.587_401_051_968_199_474_751_705_6), q);
                q = vldexp2_vd_vd_vi(q, qu - $ix::splat(2048));

                q = vmulsign_vd_vd_vd(q, d);

                d = d.abs();

                let mut x = $f64x::splat(-0.640_245_898_480_692_909_870_982)
                    .mul_add(d, $f64x::splat(2.961_551_030_200_395_118_185_95))
                    .mul_add(d, $f64x::splat(-5.733_530_609_229_478_436_361_66))
                    .mul_add(d, $f64x::splat(6.039_903_689_894_587_479_614_07))
                    .mul_add(d, $f64x::splat(-3.858_419_355_104_449_888_216_32))
                    .mul_add(d, $f64x::splat(2.230_727_530_249_660_972_572_2));

                let mut y = x * x;
                y = y * y;
                x -= d.mul_sub(y, x) * $f64x::splat(1. / 3.);
                y = d * x * x;
                y = (y - $f64x::splat(2. / 3.) * y * y.mul_add(x, $f64x::splat(-1.))) * q;

                /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                    y = s.is_infinite().select(vmulsign_vd_vd_vd($f64x::INFINITY, s), y);
                    y = s
                        .eq(ZERO)
                        .select(vmulsign_vd_vd_vd(ZERO, s), y);
                }*/

                y
            }

            pub fn sqrt(d: $f64x) -> $f64x {
                return u05::sqrt(d);
            }

            pub fn hypot(x: $f64x, y: $f64x) -> $f64x {
                let x = x.abs();
                let y = y.abs();
                let min = x.min(y);
                let max = x.max(y);

                let t = min / max;
                let mut ret = max * t.mul_add(t, ONE).sqrt();
                ret = min.eq(ZERO).select(max, ret);
                ret = (x.is_nan() | y.is_nan()).select($f64x::NAN, ret);
                (x.eq($f64x::INFINITY) | y.eq($f64x::INFINITY))
                    .select($f64x::INFINITY, ret)
            }

        }

    };
}
