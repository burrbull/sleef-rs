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
            let v = r[i*2+0];
            r[i*2+0] = r[i*2+1];
            r[i*2+1] = v;
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
        #[inline]
        fn vsignbit_vo_vd(d: $f64x) -> $m64x {
            ($u64x::from_bits(d) & $u64x::from_bits($f64x::splat(-0.)))
                .eq($u64x::from_bits($f64x::splat(-0.)))
        }

        // return d0 < d1 ? x : y
        #[inline]
        fn vsel_vi_vd_vd_vi_vi(d0: $f64x, d1: $f64x, x: $ix, y: $ix) -> $ix {
            d0.lt(d1).select(x, y)
        }

        // return d0 < 0 ? x : 0
        #[inline]
        fn vsel_vi_vd_vi(d: $f64x, x: $ix) -> $ix {
            vand_vi_vo_vi($mx::from_cast(vsignbit_vo_vd(d)), x)
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
            let o = d.lt($f64x::splat(4.9090934652977266e-91));
            d = o.select($f64x::splat(2.037035976334486e90) * d, d);
            let mut q = $ix::from_cast($u64x::from_bits(d));
            q = q & $ix::splat(((1 << 12) - 1) << 20);
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
            q = q & $ix::splat(0x7ff);
            q - $ix::splat(0x3ff)
        }
        #[inline]
        fn visint_vo_vd(d: $f64x) -> $m64x {
            let mut x = (d * (ONE / D1_31X)).truncate();
            x = (-D1_31X).mul_add(x, d);
            x.truncate().eq(x) | d.abs().gt(D1_53X)
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
                let o = x.abs().eq($f64x::splat(0.12499999999999998612));
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
            ex = ex - $ix::splat(55);
            let mut q = vand_vi_vo_vi(ex.gt($ix::splat(700 - 55)), $ix::splat(-64));
            a = vldexp3_vd_vd_vi(a, q);
            ex = vandnot_vi_vi_vi(ex >> 31, ex);
            ex = ex << 2;
            let mut x = a.mul_as_doubled(vgather_vd_p_vi(&REMPITABDP, ex));
            let (did, dii) = rempisub(x.0);
            q = dii;
            x.0 = did;
            x = x.normalize();
            let mut y = a.mul_as_doubled(vgather_vd_p_vi(&REMPITABDP[1..], ex));
            x += y;
            let (did, dii) = rempisub(x.0);
            q = q + dii;
            x.0 = did;
            x = x.normalize();
            y = Doubled::new(
                vgather_vd_p_vi(&REMPITABDP[2..], ex),
                vgather_vd_p_vi(&REMPITABDP[3..], ex),
            );
            y *= a;
            x += y;
            x = x.normalize();
            x *= Doubled::from((3.141592653589793116 * 2., 1.2246467991473532072e-16 * 2.));
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

            let u = o.select_splat(9.94480387626843774090208e-16,
                -2.02461120785182399295868e-14,
            ).mul_add(
                s,
                o.select_splat(-3.89796226062932799164047e-13,
                    6.948218305801794613277840e-12,
                ),
            ).mul_add(
                s,
                o.select_splat(1.150115825399960352669010e-10,
                    -1.75724749952853179952664e-09,
                ),
            ).mul_add(
                s,
                o.select_splat(-2.46113695010446974953590e-08,
                    3.133616889668683928784220e-07,
                ),
            ).mul_add(
                s,
                o.select_splat(3.590860448590527540050620e-06,
                    -3.65762041821615519203610e-05,
                ),
            ).mul_add(
                s,
                o.select_splat(-0.000325991886927389905997954,
                    0.0024903945701927185027435600,
                ),
            );
            let mut x = u * s + vsel_vd2_vo_d_d_d_d(
                o,
                0.0158543442438155018914259,
                -1.04693272280631521908845e-18,
                -0.0807455121882807852484731,
                3.61852475067037104849987e-18,
            );
            x = s2 * x + vsel_vd2_vo_d_d_d_d(
                o,
                -0.308425137534042437259529,
                -1.95698492133633550338345e-17,
                0.785398163397448278999491,
                3.06287113727155002607105e-17,
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

                u = $f64x::splat(-1.88796008463073496563746e-05)
                    .mul_add(t2, $f64x::splat(-0.00110611831486672482563471))
                    .mul_add(t2, $f64x::splat(-0.00889896195887655491740809))
                    .mul_add(t2, $f64x::splat(-0.0254517624932312641616861))
                    .mul_add(t2, $f64x::splat(-0.0407629191276836500001934))
                    .mul_add(t2, $f64x::splat(-0.0523674852303482457616113))
                    .mul_add(t2, $f64x::splat(-0.0666573579361080525984562))
                    .mul_add(t2, $f64x::splat(-0.090908995008245008229153))
                    .mul_add(t2, $f64x::splat(-0.14285714266771329383765))
                    .mul_add(t2, $f64x::splat(-0.333333333333311110369124));

                let v = $f64x::splat(0.000209850076645816976906797)
                    .mul_add(t2, $f64x::splat(0.00370026744188713119232403))
                    .mul_add(t2, $f64x::splat(0.016599329773529201970117))
                    .mul_add(t2, $f64x::splat(0.0337852580001353069993897))
                    .mul_add(t2, $f64x::splat(0.0466667150077840625632675))
                    .mul_add(t2, $f64x::splat(0.0587666392926673580854313))
                    .mul_add(t2, $f64x::splat(0.0769219538311769618355029))
                    .mul_add(t2, $f64x::splat(0.111111105648261418443745))
                    .mul_add(t2, $f64x::splat(0.199999999996591265594148));

                u = v.mul_add(t, u);
            } else {
                u = $f64x::splat(-1.88796008463073496563746e-05)
                    .mul_add(t, $f64x::splat(0.000209850076645816976906797))
                    .mul_add(t, $f64x::splat(-0.00110611831486672482563471))
                    .mul_add(t, $f64x::splat(0.00370026744188713119232403))
                    .mul_add(t, $f64x::splat(-0.00889896195887655491740809))
                    .mul_add(t, $f64x::splat(0.016599329773529201970117))
                    .mul_add(t, $f64x::splat(-0.0254517624932312641616861))
                    .mul_add(t, $f64x::splat(0.0337852580001353069993897))
                    .mul_add(t, $f64x::splat(-0.0407629191276836500001934))
                    .mul_add(t, $f64x::splat(0.0466667150077840625632675))
                    .mul_add(t, $f64x::splat(-0.0523674852303482457616113))
                    .mul_add(t, $f64x::splat(0.0587666392926673580854313))
                    .mul_add(t, $f64x::splat(-0.0666573579361080525984562))
                    .mul_add(t, $f64x::splat(0.0769219538311769618355029))
                    .mul_add(t, $f64x::splat(-0.090908995008245008229153))
                    .mul_add(t, $f64x::splat(0.111111105648261418443745))
                    .mul_add(t, $f64x::splat(-0.14285714266771329383765))
                    .mul_add(t, $f64x::splat(0.199999999996591265594148))
                    .mul_add(t, $f64x::splat(-0.333333333333311110369124));
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

            u = $f64x::splat(2.08860621107283687536341e-09)
                .mul_add(s, $f64x::splat(2.51112930892876518610661e-08))
                .mul_add(s, $f64x::splat(2.75573911234900471893338e-07))
                .mul_add(s, $f64x::splat(2.75572362911928827629423e-06))
                .mul_add(s, $f64x::splat(2.4801587159235472998791e-05))
                .mul_add(s, $f64x::splat(0.000198412698960509205564975))
                .mul_add(s, $f64x::splat(0.00138888888889774492207962))
                .mul_add(s, $f64x::splat(0.00833333333331652721664984))
                .mul_add(s, $f64x::splat(0.0416666666666665047591422))
                .mul_add(s, $f64x::splat(0.166666666666666851703837))
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
                    Doubled::from((0.693147180559945286226764, 2.319046813846299558417771e-17))
                        * $f64x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vd_vd(d * $f64x::splat(1. / 0.75));
                    e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                    m = vgetmant_vd_vd(d);
                    Doubled::new(
                        $f64x::splat(0.693147180559945286226764),
                        $f64x::splat(2.319046813846299558417771e-17),
                    ) * e
                }*/;

            let x = $f64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.square();

            let t = $f64x::splat(0.116255524079935043668677)
                .mul_add(x2.0, $f64x::splat(0.103239680901072952701192))
                .mul_add(x2.0, $f64x::splat(0.117754809412463995466069))
                .mul_add(x2.0, $f64x::splat(0.13332981086846273921509))
                .mul_add(x2.0, $f64x::splat(0.153846227114512262845736))
                .mul_add(x2.0, $f64x::splat(0.181818180850050775676507))
                .mul_add(x2.0, $f64x::splat(0.222222222230083560345903))
                .mul_add(x2.0, $f64x::splat(0.285714285714249172087875))
                .mul_add(x2.0, $f64x::splat(0.400000000000000077715612));
            let c = Doubled::from((0.666666666666666629659233, 3.80554962542412056336616e-17));

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

            u = $f64x::splat(2.51069683420950419527139e-08)
                .mul_add(s.0, $f64x::splat(2.76286166770270649116855e-07))
                .mul_add(s.0, $f64x::splat(2.75572496725023574143864e-06))
                .mul_add(s.0, $f64x::splat(2.48014973989819794114153e-05))
                .mul_add(s.0, $f64x::splat(0.000198412698809069797676111))
                .mul_add(s.0, $f64x::splat(0.0013888888939977128960529))
                .mul_add(s.0, $f64x::splat(0.00833333333332371417601081))
                .mul_add(s.0, $f64x::splat(0.0416666666665409524128449))
                .mul_add(s.0, $f64x::splat(0.166666666666666740681535))
                .mul_add(s.0, $f64x::splat(0.500000000000000999200722));

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

            let u = $f64x::splat(0.1602472219709932072e-9)
                .mul_add(s.0, $f64x::splat(0.2092255183563157007e-8))
                .mul_add(s.0, $f64x::splat(0.2505230023782644465e-7))
                .mul_add(s.0, $f64x::splat(0.2755724800902135303e-6))
                .mul_add(s.0, $f64x::splat(0.2755731892386044373e-5))
                .mul_add(s.0, $f64x::splat(0.2480158735605815065e-4))
                .mul_add(s.0, $f64x::splat(0.1984126984148071858e-3))
                .mul_add(s.0, $f64x::splat(0.1388888888886763255e-2))
                .mul_add(s.0, $f64x::splat(0.8333333333333347095e-2))
                .mul_add(s.0, $f64x::splat(0.4166666666666669905e-1));

            let mut t = s * u + $f64x::splat(0.1666666666666666574e+0);
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

            let t = $f64x::splat(0.13860436390467167910856)
                .mul_add(x2.0, $f64x::splat(0.131699838841615374240845))
                .mul_add(x2.0, $f64x::splat(0.153914168346271945653214))
                .mul_add(x2.0, $f64x::splat(0.181816523941564611721589))
                .mul_add(x2.0, $f64x::splat(0.22222224632662035403996))
                .mul_add(x2.0, $f64x::splat(0.285714285511134091777308))
                .mul_add(x2.0, $f64x::splat(0.400000000000914013309483))
                .mul_add(x2.0, $f64x::splat(0.666666666666664853302393));

            let mut s = Doubled::from((0.693147180559945286226764, 2.319046813846299558417771e-17))
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
                .eq($f64x::splat(0.49999999999999994449))
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
                .eq($f64x::splat(0.50000000000000011102))
                .select(ZERO, x);
            (d.is_infinite() | d.abs().ge(D1_52X))
                .select(d, vcopysign_vd_vd_vd(x - fr, d))
        }

        pub fn nextafter(x: $f64x, y: $f64x) -> $f64x {
            let x = x
                .eq(ZERO)
                .select(vmulsign_vd_vd_vd(ZERO, y), x);
            let mut xi2 = $i64x::from_bits(x);
            let c = vsignbit_vo_vd(x) ^ y.ge(x);

            let mut t = (xi2 ^ vcast_vi2_u_u(0x7fffffff, 0xffffffff)) + vcast_vi2_i_i(0, 1);
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

            let mut t = (xi2 ^ vcast_vi2_u_u(0x7fffffff, 0xffffffff)) + vcast_vi2_i_i(0, 1);
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
            xm = xm & $u64x::from_u32((!0x7ff00000, !0));
            xm = xm | $u64x::from_u32((0x3fe00000, 0));

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
                    $u64x::from_bits(vptrunc(q)) & $u64x::from_u32((0xffffffff, 0xfffffffe)),
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
                -156.801412704022726379848862,
                0.2947916772827614196e+2,
                0.7074816000864609279e-7,
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    1.120804464289911606838558160000,
                    0.1281459691827820109e+3,
                    0.4009244333008730443e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    13.39798545514258921833306020000,
                    0.2617544025784515043e+3,
                    0.1040114641628246946e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.116546276599463200848033357000,
                    0.3287022855685790432e+3,
                    0.1508349150733329167e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -1.391801093265337481495562410000,
                    0.2818145867730348186e+3,
                    0.1288143074933901020e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.015056113040026424412918973400,
                    0.1728670414673559605e+3,
                    0.4744167749884993937e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.179540117061234856098844714000,
                    0.7748735764030416817e+2,
                    -0.6554816306542489902e-7,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.002481743600264997730942489280,
                    0.2512856643080930752e+2,
                    -0.3189252471452599844e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.029527880945699120504851034100,
                    0.5766792106140076868e+1,
                    0.1358883821470355377e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.000540164767892604515196325186,
                    0.7270275473996180571e+0,
                    -0.4343931277157336040e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.006403362833808069794787256200,
                    0.8396709124579147809e-1,
                    0.9724785897406779555e-6,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.000162516262783915816896611252,
                    -0.8211558669746804595e-1,
                    -0.2036886057225966011e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.001914438498565477526465972390,
                    0.6828831828341884458e-1,
                    0.4373363141819725815e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    7.20489541602001055898311517e-05,
                    -0.7712481339961671511e-1,
                    -0.9439951268304008677e-5,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.000839498720672087279971000786,
                    0.8337492023017314957e-1,
                    0.2050727030376389804e-4,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -5.17179090826059219329394422e-05,
                    -0.9094964931456242518e-1,
                    -0.4492620183431184018e-4,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.000592166437353693882857342347,
                    0.1000996313575929358e+0,
                    0.9945751236071875931e-4,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    6.97281375836585777403743539e-05,
                    -0.1113342861544207724e+0,
                    -0.2231547599034983196e-3,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.000784039221720066627493314301,
                    0.1255096673213020875e+0,
                    0.5096695247101967622e-3,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.000229472093621399176949318732,
                    -0.1440498967843054368e+0,
                    -0.1192753911667886971e-2,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    -0.002681327160493827160473958490,
                    0.1695571770041949811e+0,
                    0.2890510330742210310e-2,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.003472222222222222222175164840,
                    -0.2073855510284092762e+0,
                    -0.7385551028674461858e-2,
                ),
            ).mul_add(
                t,
                vsel_vd_vo_vo_d_d_d(
                    o2,
                    o0,
                    0.083333333333333333335592087900,
                    0.2705808084277815939e+0,
                    0.2058080842778455335e-1,
                ),
            );

            let mut y = (x + $f64x::splat(-0.5)) * logk2(x);
            y += -x;
            y += Doubled::from((0.91893853320467278056, -3.8782941580672414498e-17)); // 0.5*log(2*M_PI)

            let mut z = u.mul_as_doubled(t)
                + o0.select_splat(-0.4006856343865314862e+0, -0.6735230105319810201e-1);
            z = z * t + o0.select_splat(0.8224670334241132030e+0, 0.3224670334241132030e+0);
            z = z * t + o0.select_splat(-0.5772156649015328655e+0, 0.4227843350984671345e+0);
            z = z * t;

            let mut clc = o2.select_doubled(y, z);

            clld = o2.select_doubled(u.mul_as_doubled(t) + ONE, clld);

            y = clln;

            clc = otiny.select_doubled(Doubled::from((83.1776616671934334590333, 3.67103459631568507221878e-15)), // log(2^120)
                oref.select_doubled(Doubled::<$f64x>::from((1.1447298858494001639, 1.026595116270782638e-17)) + (-clc),
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

            let u = o.select_splat(9.94480387626843774090208e-16,
                -2.02461120785182399295868e-14,
            ).mul_add(
                s,
                o.select_splat(-3.89796226062932799164047e-13,
                    6.948218305801794613277840e-12,
                ),
            ).mul_add(
                s,
                o.select_splat(1.150115825399960352669010e-10,
                    -1.75724749952853179952664e-09,
                ),
            ).mul_add(
                s,
                o.select_splat(-2.46113695010446974953590e-08,
                    3.133616889668683928784220e-07,
                ),
            ).mul_add(
                s,
                o.select_splat(3.590860448590527540050620e-06,
                    -3.65762041821615519203610e-05,
                ),
            ).mul_add(
                s,
                o.select_splat(-0.000325991886927389905997954,
                    0.0024903945701927185027435600,
                ),
            );
            let mut x = u * s + vsel_vd2_vo_d_d_d_d(
                o,
                0.0158543442438155018914259,
                -1.04693272280631521908845e-18,
                -0.0807455121882807852484731,
                3.61852475067037104849987e-18,
            );
            x = s2 * x + vsel_vd2_vo_d_d_d_d(
                o,
                -0.308425137534042437259529,
                -1.95698492133633550338345e-17,
                0.785398163397448278999491,
                3.06287113727155002607105e-17,
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

                let u = $f64x::splat(-2.02461120785182399295868e-14)
                    .mul_add(s, $f64x::splat(6.94821830580179461327784e-12))
                    .mul_add(s, $f64x::splat(-1.75724749952853179952664e-09))
                    .mul_add(s, $f64x::splat(3.13361688966868392878422e-07))
                    .mul_add(s, $f64x::splat(-3.6576204182161551920361e-05))
                    .mul_add(s, $f64x::splat(0.00249039457019271850274356));
                let mut x =
                    u * s + Doubled::from((-0.0807455121882807852484731, 3.61852475067037104849987e-18));
                x = s2 * x + Doubled::from((0.785398163397448278999491, 3.06287113727155002607105e-17));

                x *= t;
                let rx = x.0 + x.1;

                let rx = visnegzero_vo_vd(d).select($f64x::splat(-0.), rx);

                //

                let u = $f64x::splat(9.94480387626843774090208e-16)
                    .mul_add(s, $f64x::splat(-3.89796226062932799164047e-13))
                    .mul_add(s, $f64x::splat(1.15011582539996035266901e-10))
                    .mul_add(s, $f64x::splat(-2.4611369501044697495359e-08))
                    .mul_add(s, $f64x::splat(3.59086044859052754005062e-06))
                    .mul_add(s, $f64x::splat(-0.000325991886927389905997954));
                let mut x =
                    u * s + Doubled::from((0.0158543442438155018914259, -1.04693272280631521908845e-18));
                x = s2 * x + Doubled::from((-0.308425137534042437259529, -1.95698492133633550338345e-17));

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

                let o = d.lt($f64x::splat(8.636168555094445e-78));
                let d = o.select(d * $f64x::splat(1.157920892373162e77), d);
                let q = o.select(
                    $f64x::splat(2.9387358770557188e-39 * 0.5),
                    $f64x::splat(0.5),
                );

                let o = d.gt($f64x::splat(1.3407807929942597e+154));
                let d = o.select(d * $f64x::splat(7.4583407312002070e-155), d);
                let q = o.select($f64x::splat(1.1579208923731620e+77 * 0.5), q);

                let mut x = $f64x::from_bits(
                    vcast_vi2_i_i(0x5fe6ec86, 0)
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
                    ql = ql >> 2;
                    let o = (ddii & $ix::splat(1)).eq($ix::splat(1));
                    let mut x = Doubled::new(
                        vmulsign_vd_vd_vd($f64x::splat(-3.141592653589793116 * 0.5), ddidd.0),
                        vmulsign_vd_vd_vd($f64x::splat(-1.2246467991473532072e-16 * 0.5), ddidd.0),
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

                let mut u = $f64x::splat(2.72052416138529567917983e-15)
                    .mul_add(s.0, $f64x::splat(-7.6429259411395447190023e-13))
                    .mul_add(s.0, $f64x::splat(1.60589370117277896211623e-10))
                    .mul_add(s.0, $f64x::splat(-2.5052106814843123359368e-08))
                    .mul_add(s.0, $f64x::splat(2.75573192104428224777379e-06))
                    .mul_add(s.0, $f64x::splat(-0.000198412698412046454654947))
                    .mul_add(s.0, $f64x::splat(0.00833333333333318056201922));

                let x = ONE.add_checked(
                    ($f64x::splat(-0.166666666666666657414808).add_checked_as_doubled(u * s.0)) * s,
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
                    ql = ql >> 1;
                    let o = (ddii & $ix::splat(1)).eq($ix::splat(0));
                    let y = ddidd
                        .0
                        .gt(ZERO)
                        .select(ZERO, $f64x::splat(-1.));
                    let mut x = Doubled::new(
                        vmulsign_vd_vd_vd($f64x::splat(-3.141592653589793116 * 0.5), y),
                        vmulsign_vd_vd_vd($f64x::splat(-1.2246467991473532072e-16 * 0.5), y),
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

                let u = $f64x::splat(2.72052416138529567917983e-15)
                    .mul_add(s.0, $f64x::splat(-7.6429259411395447190023e-13))
                    .mul_add(s.0, $f64x::splat(1.60589370117277896211623e-10))
                    .mul_add(s.0, $f64x::splat(-2.5052106814843123359368e-08))
                    .mul_add(s.0, $f64x::splat(2.75573192104428224777379e-06))
                    .mul_add(s.0, $f64x::splat(-0.000198412698412046454654947))
                    .mul_add(s.0, $f64x::splat(0.00833333333333318056201922));

                let x = ONE.add_checked(
                    ($f64x::splat(-0.166666666666666657414808).add_checked_as_doubled(u * s.0)) * s,
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

                let mut u = $f64x::splat(1.58938307283228937328511e-10)
                    .mul_add(s.0, $f64x::splat(-2.50506943502539773349318e-08))
                    .mul_add(s.0, $f64x::splat(2.75573131776846360512547e-06))
                    .mul_add(s.0, $f64x::splat(-0.000198412698278911770864914))
                    .mul_add(s.0, $f64x::splat(0.0083333333333191845961746))
                    .mul_add(s.0, $f64x::splat(-0.166666666666666130709393));

                u = u * (s.0 * t.0);

                let x = t.add_checked(u);
                let rx = x.0 + x.1;

                let rx = visnegzero_vo_vd(d).select($f64x::splat(-0.), rx);

                let u = $f64x::splat(-1.13615350239097429531523e-11)
                    .mul_add(s.0, $f64x::splat(2.08757471207040055479366e-09))
                    .mul_add(s.0, $f64x::splat(-2.75573144028847567498567e-07))
                    .mul_add(s.0, $f64x::splat(2.48015872890001867311915e-05))
                    .mul_add(s.0, $f64x::splat(-0.00138888888888714019282329))
                    .mul_add(s.0, $f64x::splat(0.0416666666666665519592062))
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

                    u = $f64x::splat(-2.59519791585924697698614e-05)
                        .mul_add(sx2, $f64x::splat(-3.05033014433946488225616e-05))
                        .mul_add(sx2, $f64x::splat(8.09674518280159187045078e-05))
                        .mul_add(sx2, $f64x::splat(0.000588505168743587154904506))
                        .mul_add(sx2, $f64x::splat(0.00359208743836906619142924))
                        .mul_add(sx2, $f64x::splat(0.0218694882853846389592078))
                        .mul_add(sx2, $f64x::splat(0.133333333333125941821962));

                    let v = $f64x::splat(1.01419718511083373224408e-05)
                        .mul_add(sx2, $f64x::splat(5.23388081915899855325186e-05))
                        .mul_add(sx2, $f64x::splat(7.14707504084242744267497e-05))
                        .mul_add(sx2, $f64x::splat(0.000244884931879331847054404))
                        .mul_add(sx2, $f64x::splat(0.00145612788922812427978848))
                        .mul_add(sx2, $f64x::splat(0.00886323944362401618113356))
                        .mul_add(sx2, $f64x::splat(0.0539682539781298417636002));

                    u = v.mul_add(s.0, u);
                } else {
                    u = $f64x::splat(1.01419718511083373224408e-05)
                        .mul_add(s.0, $f64x::splat(-2.59519791585924697698614e-05))
                        .mul_add(s.0, $f64x::splat(5.23388081915899855325186e-05))
                        .mul_add(s.0, $f64x::splat(-3.05033014433946488225616e-05))
                        .mul_add(s.0, $f64x::splat(7.14707504084242744267497e-05))
                        .mul_add(s.0, $f64x::splat(8.09674518280159187045078e-05))
                        .mul_add(s.0, $f64x::splat(0.000244884931879331847054404))
                        .mul_add(s.0, $f64x::splat(0.000588505168743587154904506))
                        .mul_add(s.0, $f64x::splat(0.00145612788922812427978848))
                        .mul_add(s.0, $f64x::splat(0.00359208743836906619142924))
                        .mul_add(s.0, $f64x::splat(0.00886323944362401618113356))
                        .mul_add(s.0, $f64x::splat(0.0218694882853846389592078))
                        .mul_add(s.0, $f64x::splat(0.0539682539781298417636002))
                        .mul_add(s.0, $f64x::splat(0.133333333333125941821962));
                }

                let mut x = ONE.add_checked(
                    $f64x::splat(0.333333333333334980164153).add_checked_as_doubled(u * s.0) * s,
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

                    u = $f64x::splat(0.00070557664296393412389774)
                        .mul_add(t.0, $f64x::splat(-0.00251865614498713360352999))
                        .mul_add(tx3, $f64x::splat(0.0208024799924145797902497))
                        .mul_add(t.0, $f64x::splat(-0.0289002344784740315686289))
                        .mul_add(tx3, $f64x::splat(0.0470843011653283988193763))
                        .mul_add(t.0, $f64x::splat(-0.0524914210588448421068719))
                        .mul_add(tx3, $f64x::splat(0.0769225330296203768654095))
                        .mul_add(t.0, $f64x::splat(-0.0909090442773387574781907))
                        .mul_add(tx3, $f64x::splat(0.199999999997977351284817))
                        .mul_add(t.0, $f64x::splat(-0.333333333333317605173818));

                    let v = $f64x::splat(1.06298484191448746607415e-05)
                        .mul_add(t.0, $f64x::splat(-0.000125620649967286867384336))
                        .mul_add(tx3, $f64x::splat(0.00646262899036991172313504))
                        .mul_add(t.0, $f64x::splat(-0.0128281333663399031014274))
                        .mul_add(tx3, $f64x::splat(0.0359785005035104590853656))
                        .mul_add(t.0, $f64x::splat(-0.041848579703592507506027))
                        .mul_add(tx3, $f64x::splat(0.0587946590969581003860434))
                        .mul_add(t.0, $f64x::splat(-0.0666620884778795497194182))
                        .mul_add(tx3, $f64x::splat(0.111111108376896236538123))
                        .mul_add(t.0, $f64x::splat(-0.142857142756268568062339));

                    u = v.mul_add(t.0 * t.0, u);
                } else {
                    u = $f64x::splat(1.06298484191448746607415e-05)
                        .mul_add(t.0, $f64x::splat(-0.000125620649967286867384336))
                        .mul_add(t.0, $f64x::splat(0.00070557664296393412389774))
                        .mul_add(t.0, $f64x::splat(-0.00251865614498713360352999))
                        .mul_add(t.0, $f64x::splat(0.00646262899036991172313504))
                        .mul_add(t.0, $f64x::splat(-0.0128281333663399031014274))
                        .mul_add(t.0, $f64x::splat(0.0208024799924145797902497))
                        .mul_add(t.0, $f64x::splat(-0.0289002344784740315686289))
                        .mul_add(t.0, $f64x::splat(0.0359785005035104590853656))
                        .mul_add(t.0, $f64x::splat(-0.041848579703592507506027))
                        .mul_add(t.0, $f64x::splat(0.0470843011653283988193763))
                        .mul_add(t.0, $f64x::splat(-0.0524914210588448421068719))
                        .mul_add(t.0, $f64x::splat(0.0587946590969581003860434))
                        .mul_add(t.0, $f64x::splat(-0.0666620884778795497194182))
                        .mul_add(t.0, $f64x::splat(0.0769225330296203768654095))
                        .mul_add(t.0, $f64x::splat(-0.0909090442773387574781907))
                        .mul_add(t.0, $f64x::splat(0.111111108376896236538123))
                        .mul_add(t.0, $f64x::splat(-0.142857142756268568062339))
                        .mul_add(t.0, $f64x::splat(0.199999999997977351284817))
                        .mul_add(t.0, $f64x::splat(-0.333333333333317605173818));
                }

                t *= u;
                t = s * ONE.add_checked(t);
                (Doubled::from((1.570796326794896557998982, 6.12323399573676603586882e-17))
                    * $f64x::from_cast(q)).add_checked(t)
            }

            pub fn atan2(y: $f64x, x: $f64x) -> $f64x {
                let o = x.abs().lt($f64x::splat(5.5626846462680083984e-309)); // nexttoward((1.0 / DBL_MAX), 1)
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
                        vsignbit_vo_vd(x),
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

                    u = $f64x::splat(-0.1581918243329996643e-1)
                        .mul_add(x4, $f64x::splat(0.6606077476277170610e-2))
                        .mul_add(x4, $f64x::splat(0.1388715184501609218e-1))
                        .mul_add(x4, $f64x::splat(0.2237176181932048341e-1))
                        .mul_add(x4, $f64x::splat(0.4464285681377102438e-1))
                        .mul_add(x4, $f64x::splat(0.1666666666666497543e+0));

                    let v = $f64x::splat(0.3161587650653934628e-1)
                        .mul_add(x4, $f64x::splat(0.1929045477267910674e-1))
                        .mul_add(x4, $f64x::splat(0.1215360525577377331e-1))
                        .mul_add(x4, $f64x::splat(0.1735956991223614604e-1))
                        .mul_add(x4, $f64x::splat(0.3038195928038132237e-1))
                        .mul_add(x4, $f64x::splat(0.7500000000378581611e-1));

                    u = v.mul_add(x2, u);
                } else {
                    u = $f64x::splat(0.3161587650653934628e-1)
                        .mul_add(x2, $f64x::splat(-0.1581918243329996643e-1))
                        .mul_add(x2, $f64x::splat(0.1929045477267910674e-1))
                        .mul_add(x2, $f64x::splat(0.6606077476277170610e-2))
                        .mul_add(x2, $f64x::splat(0.1215360525577377331e-1))
                        .mul_add(x2, $f64x::splat(0.1388715184501609218e-1))
                        .mul_add(x2, $f64x::splat(0.1735956991223614604e-1))
                        .mul_add(x2, $f64x::splat(0.2237176181932048341e-1))
                        .mul_add(x2, $f64x::splat(0.3038195928038132237e-1))
                        .mul_add(x2, $f64x::splat(0.4464285681377102438e-1))
                        .mul_add(x2, $f64x::splat(0.7500000000378581611e-1))
                        .mul_add(x2, $f64x::splat(0.1666666666666497543e+0));
                }

                u *= x2 * x.0;

                let y = Doubled::from((3.141592653589793116 / 4., 1.2246467991473532072e-16 / 4.))
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

                    u = $f64x::splat(-0.1581918243329996643e-1)
                        .mul_add(x4, $f64x::splat(0.6606077476277170610e-2))
                        .mul_add(x4, $f64x::splat(0.1388715184501609218e-1))
                        .mul_add(x4, $f64x::splat(0.2237176181932048341e-1))
                        .mul_add(x4, $f64x::splat(0.4464285681377102438e-1))
                        .mul_add(x4, $f64x::splat(0.1666666666666497543e+0));

                    let v = $f64x::splat(0.3161587650653934628e-1)
                        .mul_add(x4, $f64x::splat(0.1929045477267910674e-1))
                        .mul_add(x4, $f64x::splat(0.1215360525577377331e-1))
                        .mul_add(x4, $f64x::splat(0.1735956991223614604e-1))
                        .mul_add(x4, $f64x::splat(0.3038195928038132237e-1))
                        .mul_add(x4, $f64x::splat(0.7500000000378581611e-1));

                    u = v.mul_add(x2, u);
                } else {
                    u = $f64x::splat(0.3161587650653934628e-1)
                        .mul_add(x2, $f64x::splat(-0.1581918243329996643e-1))
                        .mul_add(x2, $f64x::splat(0.1929045477267910674e-1))
                        .mul_add(x2, $f64x::splat(0.6606077476277170610e-2))
                        .mul_add(x2, $f64x::splat(0.1215360525577377331e-1))
                        .mul_add(x2, $f64x::splat(0.1388715184501609218e-1))
                        .mul_add(x2, $f64x::splat(0.1735956991223614604e-1))
                        .mul_add(x2, $f64x::splat(0.2237176181932048341e-1))
                        .mul_add(x2, $f64x::splat(0.3038195928038132237e-1))
                        .mul_add(x2, $f64x::splat(0.4464285681377102438e-1))
                        .mul_add(x2, $f64x::splat(0.7500000000378581611e-1))
                        .mul_add(x2, $f64x::splat(0.1666666666666497543e+0));
                }

                u *= x2 * x.0;

                let mut y = Doubled::from((3.141592653589793116 / 2., 1.2246467991473532072e-16 / 2.))
                    .sub_checked(vmulsign_vd_vd_vd(x.0, d).add_checked_as_doubled(vmulsign_vd_vd_vd(u, d)));
                x = x.add_checked(u);

                y = o.select_doubled(y, x.scale($f64x::splat(2.)));

                y = vandnot_vo_vo_vo(o, d.lt(ZERO)).select_doubled(Doubled::from((3.141592653589793116, 1.2246467991473532072e-16)).sub_checked(y),
                    y,
                );

                y.0 + y.1
            }

            pub fn atan(d: $f64x) -> $f64x {
                let d2 = atan2k_u1(Doubled::new(d.abs(), ZERO), Doubled::from((1., 0.)));
                let mut r = d2.0 + d2.1;
                r = d.is_infinite().select($f64x::splat(1.570796326794896557998982), r);
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

                        u = $f64x::splat(0.2081276378237164457e-8)
                            .mul_adde(s2, $f64x::splat(0.2755762628169491192e-6))
                            .mul_adde(s2, $f64x::splat(0.2480158687479686264e-4))
                            .mul_adde(s2, $f64x::splat(0.1388888888914497797e-2))
                            .mul_adde(s2, $f64x::splat(0.4166666666666602598e-1))
                            .mul_adde(s2, $f64x::splat(0.5000000000000000000e+0));

                        let v = $f64x::splat(0.2511210703042288022e-7)
                            .mul_adde(s2, $f64x::splat(0.2755723402025388239e-5))
                            .mul_adde(s2, $f64x::splat(0.1984126989855865850e-3))
                            .mul_adde(s2, $f64x::splat(0.8333333333314938210e-2))
                            .mul_adde(s2, $f64x::splat(0.1666666666666669072e+0));

                        u = v
                            .mul_add(s, u)
                            .mul_adde(s, $f64x::splat(0.1000000000000000000e+1))
                            .mul_adde(s, $f64x::splat(0.1000000000000000000e+1));
                    } else {
                        u = $f64x::splat(0.2081276378237164457e-8)
                            .mul_adde(s, $f64x::splat(0.2511210703042288022e-7))
                            .mul_adde(s, $f64x::splat(0.2755762628169491192e-6))
                            .mul_adde(s, $f64x::splat(0.2755723402025388239e-5))
                            .mul_adde(s, $f64x::splat(0.2480158687479686264e-4))
                            .mul_adde(s, $f64x::splat(0.1984126989855865850e-3))
                            .mul_adde(s, $f64x::splat(0.1388888888914497797e-2))
                            .mul_adde(s, $f64x::splat(0.8333333333314938210e-2))
                            .mul_adde(s, $f64x::splat(0.4166666666666602598e-1))
                            .mul_adde(s, $f64x::splat(0.1666666666666669072e+0))
                            .mul_adde(s, $f64x::splat(0.5000000000000000000e+0))
                            .mul_adde(s, $f64x::splat(0.1000000000000000000e+1))
                            .mul_adde(s, $f64x::splat(0.1000000000000000000e+1));
                    }
                } else {
                    u = $f64x::splat(2.08860621107283687536341e-09)
                        .mul_add(s, $f64x::splat(2.51112930892876518610661e-08))
                        .mul_add(s, $f64x::splat(2.75573911234900471893338e-07))
                        .mul_add(s, $f64x::splat(2.75572362911928827629423e-06))
                        .mul_add(s, $f64x::splat(2.4801587159235472998791e-05))
                        .mul_add(s, $f64x::splat(0.000198412698960509205564975))
                        .mul_add(s, $f64x::splat(0.00138888888889774492207962))
                        .mul_add(s, $f64x::splat(0.00833333333331652721664984))
                        .mul_add(s, $f64x::splat(0.0416666666666665047591422))
                        .mul_add(s, $f64x::splat(0.166666666666666851703837))
                        .mul_add(s, $f64x::splat(0.5));

                    u = ONE + (s * s).mul_add(u, s);
                }

                u = vldexp2_vd_vd_vi(u, q);

                u = d
                    .gt($f64x::splat(709.78271114955742909217217426))
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
                        Doubled::from((0.693147180559945286226764, 2.319046813846299558417771e-17))
                            * $f64x::from_cast(e)
                    }/* else {
                        let mut e = vgetexp_vd_vd(d * $f64x::splat(1. / 0.75));
                        e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                        m = vgetmant_vd_vd(d);
                        Doubled::from((0.693147180559945286226764, 2.319046813846299558417771e-17)) * e
                    }*/;

                let x = $f64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f64x::splat(0.1532076988502701353e+0)
                    .mul_add(x2, $f64x::splat(0.1525629051003428716e+0))
                    .mul_add(x2, $f64x::splat(0.1818605932937785996e+0))
                    .mul_add(x2, $f64x::splat(0.2222214519839380009e+0))
                    .mul_add(x2, $f64x::splat(0.2857142932794299317e+0))
                    .mul_add(x2, $f64x::splat(0.3999999999635251990e+0))
                    .mul_add(x2, $f64x::splat(0.6666666666667333541e+0));

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
                    let yisint = visint_vo_vd(y);
                    let yisodd = visodd_vo_vd(y) & yisint;

                    let d = logk(x.abs()) * y;
                    let mut result = expk(d);
                    result =
                        d.0.gt($f64x::splat(709.78271114955742909217217426))
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

                y = (x.abs().gt($f64x::splat(18.714973875)) | y.is_nan())
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

                q2 = $m64x::from_cast(re.eq($ix::splat(1))).select_doubled(Doubled::from((1.2599210498948731907, -2.5899333753005069177e-17)),
                    q2,
                );
                q2 = $m64x::from_cast(re.eq($ix::splat(2))).select_doubled(Doubled::from((1.5874010519681995834, -1.0869008194197822986e-16)),
                    q2,
                );

                q2.0 = vmulsign_vd_vd_vd(q2.0, d);
                q2.1 = vmulsign_vd_vd_vd(q2.1, d);
                d = d.abs();

                let mut x = $f64x::splat(-0.640245898480692909870982)
                    .mul_add(d, $f64x::splat(2.96155103020039511818595))
                    .mul_add(d, $f64x::splat(-5.73353060922947843636166))
                    .mul_add(d, $f64x::splat(6.03990368989458747961407))
                    .mul_add(d, $f64x::splat(-3.85841935510444988821632))
                    .mul_add(d, $f64x::splat(2.2307275302496609725722));

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

                    u = $f64x::splat(0.4434359082926529454e-9)
                        .mul_add(s2, $f64x::splat(0.1017819260921760451e-6))
                        .mul_add(s2, $f64x::splat(0.1525273353517584730e-4))
                        .mul_add(s2, $f64x::splat(0.1333355814670499073e-2))
                        .mul_add(s2, $f64x::splat(0.5550410866482046596e-1));

                    let v = $f64x::splat(0.7073164598085707425e-8)
                        .mul_add(s2, $f64x::splat(0.1321543872511327615e-5))
                        .mul_add(s2, $f64x::splat(0.1540353045101147808e-3))
                        .mul_add(s2, $f64x::splat(0.9618129107597600536e-2))
                        .mul_add(s2, $f64x::splat(0.2402265069591012214e+0));

                    u = u
                        .mul_add(s, v)
                        .mul_add(s, $f64x::splat(0.6931471805599452862e+0));
                } else {
                    u = $f64x::splat(0.4434359082926529454e-9)
                        .mul_add(s, $f64x::splat(0.7073164598085707425e-8))
                        .mul_add(s, $f64x::splat(0.1017819260921760451e-6))
                        .mul_add(s, $f64x::splat(0.1321543872511327615e-5))
                        .mul_add(s, $f64x::splat(0.1525273353517584730e-4))
                        .mul_add(s, $f64x::splat(0.1540353045101147808e-3))
                        .mul_add(s, $f64x::splat(0.1333355814670499073e-2))
                        .mul_add(s, $f64x::splat(0.9618129107597600536e-2))
                        .mul_add(s, $f64x::splat(0.5550410866482046596e-1))
                        .mul_add(s, $f64x::splat(0.2402265069591012214e+0))
                        .mul_add(s, $f64x::splat(0.6931471805599452862e+0));
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

                    u = $f64x::splat(0.2411463498334267652e-3)
                        .mul_add(s2, $f64x::splat(0.5013975546789733659e-2))
                        .mul_add(s2, $f64x::splat(0.6808936399446784138e-1))
                        .mul_add(s2, $f64x::splat(0.5393829292058536229e+0))
                        .mul_add(s2, $f64x::splat(0.2034678592293432953e+1));

                    let v = $f64x::splat(0.1157488415217187375e-2)
                        .mul_add(s2, $f64x::splat(0.1959762320720533080e-1))
                        .mul_add(s2, $f64x::splat(0.2069958494722676234e+0))
                        .mul_add(s2, $f64x::splat(0.1171255148908541655e+1))
                        .mul_add(s2, $f64x::splat(0.2650949055239205876e+1));

                    u = u
                        .mul_add(s, v)
                        .mul_add(s, $f64x::splat(0.2302585092994045901e+1));
                } else {
                    u = $f64x::splat(0.2411463498334267652e-3)
                        .mul_add(s, $f64x::splat(0.1157488415217187375e-2))
                        .mul_add(s, $f64x::splat(0.5013975546789733659e-2))
                        .mul_add(s, $f64x::splat(0.1959762320720533080e-1))
                        .mul_add(s, $f64x::splat(0.6808936399446784138e-1))
                        .mul_add(s, $f64x::splat(0.2069958494722676234e+0))
                        .mul_add(s, $f64x::splat(0.5393829292058536229e+0))
                        .mul_add(s, $f64x::splat(0.1171255148908541655e+1))
                        .mul_add(s, $f64x::splat(0.2034678592293432953e+1))
                        .mul_add(s, $f64x::splat(0.2650949055239205876e+1))
                        .mul_add(s, $f64x::splat(0.2302585092994045901e+1));
                }

                if cfg!(target_feature = "fma") {
                    u = u.mul_adde(s, ONE);
                } else {
                    u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
                }

                u = vldexp2_vd_vd_vi(u, q);

                u = d
                    .gt($f64x::splat(308.25471555991671))
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
                    .gt($f64x::splat(709.782712893383996732223))
                    .select($f64x::INFINITY, x);
                x = a
                    .lt($f64x::splat(-36.736800569677101399113302437))
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
                    Doubled::from((0.30102999566398119802, -2.803728127785170339e-18)) * $f64x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vd_vd(d * $f64x::splat(1. / 0.75));
                    e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                    m = vgetmant_vd_vd(d);
                    Doubled::from((0.30102999566398119802, -2.803728127785170339e-18)) * e
                }*/;

                let x = $f64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f64x::splat(0.6653725819576758460e-1)
                    .mul_add(x2, $f64x::splat(0.6625722782820833712e-1))
                    .mul_add(x2, $f64x::splat(0.7898105214313944078e-1))
                    .mul_add(x2, $f64x::splat(0.9650955035715275132e-1))
                    .mul_add(x2, $f64x::splat(0.1240841409721444993e+0))
                    .mul_add(x2, $f64x::splat(0.1737177927454605086e+0))
                    .mul_add(x2, $f64x::splat(0.2895296546021972617e+0));

                s = s.add_checked(x * Doubled::from((0.86858896380650363334, 1.1430059694096389311e-17)));
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

                let t = $f64x::splat(0.2211941750456081490e+0)
                    .mul_add(x2, $f64x::splat(0.2200768693152277689e+0))
                    .mul_add(x2, $f64x::splat(0.2623708057488514656e+0))
                    .mul_add(x2, $f64x::splat(0.3205977477944495502e+0))
                    .mul_add(x2, $f64x::splat(0.4121985945485324709e+0))
                    .mul_add(x2, $f64x::splat(0.5770780162997058982e+0))
                    .mul_add(x2, $f64x::splat(0.96179669392608091449));

                let mut s = ef + x * Doubled::from((2.885390081777926774, 6.0561604995516736434e-18));
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
                        Doubled::from((0.693147180559945286226764, 2.319046813846299558417771e-17))
                            * $f64x::from_cast(e)
                    }/* else {
                        let e = vgetexp_vd_vd(dp1, $f64x::splat(1. / 0.75));
                        e = e.eq($f64x::INFINITY).select($f64x::splat(1024.), e);
                        let t = vldexp3_vd_vd_vi(ONE, -e.rinti());
                        m = d.mul_add(t, t - ONE);
                        Doubled::from((0.693147180559945286226764, 2.319046813846299558417771e-17)) * e
                    }*/;

                let x = Doubled::new(m, ZERO) / $f64x::splat(2.).add_checked_as_doubled(m);
                let x2 = x.0 * x.0;

                let t = $f64x::splat(0.1532076988502701353e+0)
                    .mul_add(x2, $f64x::splat(0.1525629051003428716e+0))
                    .mul_add(x2, $f64x::splat(0.1818605932937785996e+0))
                    .mul_add(x2, $f64x::splat(0.2222214519839380009e+0))
                    .mul_add(x2, $f64x::splat(0.2857142932794299317e+0))
                    .mul_add(x2, $f64x::splat(0.3999999999635251990e+0))
                    .mul_add(x2, $f64x::splat(0.6666666666667333541e+0));

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
                    | (a.lt(ZERO) & visint_vo_vd(a))
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
                    | (a.le(ZERO) & visint_vo_vd(a))
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
                    0.6801072401395392157e-20,
                    0.2830954522087717660e-13,
                    -0.5846750404269610493e-17,
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.2161766247570056391e-18,
                        -0.1509491946179481940e-11,
                        0.6076691048812607898e-15,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.4695919173301598752e-17,
                        0.3827857177807173152e-10,
                        -0.3007518609604893831e-13,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.9049140419888010819e-16,
                        -0.6139733921558987241e-09,
                        0.9427906260824646063e-12,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.1634018903557411517e-14,
                        0.6985387934608038824e-08,
                        -0.2100110908269393629e-10,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.2783485786333455216e-13,
                        -0.5988224513034371474e-07,
                        0.3534639523461223473e-09,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.4463221276786412722e-12,
                        0.4005716952355346640e-06,
                        -0.4664967728285395926e-08,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.6711366622850138987e-11,
                        -0.2132190104575784400e-05,
                        0.4943823283769000532e-07,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.9422759050232658346e-10,
                        0.9092461304042630325e-05,
                        -0.4271203394761148254e-06,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.1229055530100228477e-08,
                        -0.3079188080966205457e-04,
                        0.3034067677404915895e-05,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.1480719281585085023e-07,
                        0.7971413443082370762e-04,
                        -0.1776295289066871135e-04,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.1636584469123402714e-06,
                        -0.1387853215225442864e-03,
                        0.8524547630559505050e-04,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.1646211436588923363e-05,
                        0.6469678026257590965e-04,
                        -0.3290582944961784398e-03,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.1492565035840624866e-04,
                        0.4996645280372945860e-03,
                        0.9696966068789101157e-03,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.1205533298178966496e-03,
                        -0.1622802482842520535e-02,
                        -0.1812527628046986137e-02,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.8548327023450851166e-03,
                        0.1615320557049377171e-03,
                        -0.4725409828123619017e-03,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.5223977625442188799e-02,
                        0.1915262325574875607e-01,
                        0.2090315427924229266e-01,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.2686617064513125569e-01,
                        -0.1027818298486033455e+00,
                        -0.1052041921842776645e+00,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        0.1128379167095512753e+00,
                        -0.6366172819842503827e+00,
                        -0.6345351808766568347e+00,
                    ),
                ).mul_add(
                    u,
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        -0.3761263890318375380e+00,
                        -0.1128379590648910469e+01,
                        -0.1129442929103524396e+01,
                    ),
                );
                let mut d = t.mul_as_doubled(u);

                d += Doubled::new(
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        1.1283791670955125586,
                        3.4110644736196137587e-08,
                        0.00024963035690526438285,
                    ),
                    vsel_vd_vo_vo_d_d_d(
                        o0,
                        o1,
                        1.5335459613165822674e-17,
                        -2.4875650708323294246e-24,
                        -5.4362665034856259795e-21,
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
                    0.6801072401395386139e-20,
                    0.3438010341362585303e-12,
                    -0.5757819536420710449e+2,
                    0.2334249729638701319e+5,
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.2161766247570055669e-18,
                        -0.1237021188160598264e-10,
                        0.4669289654498104483e+3,
                        -0.4695661044933107769e+5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.4695919173301595670e-17,
                        0.2117985839877627852e-09,
                        -0.1796329879461355858e+4,
                        0.3173403108748643353e+5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.9049140419888007122e-16,
                        -0.2290560929177369506e-08,
                        0.4355892193699575728e+4,
                        0.3242982786959573787e+4,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.1634018903557410728e-14,
                        0.1748931621698149538e-07,
                        -0.7456258884965764992e+4,
                        -0.2014717999760347811e+5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.2783485786333451745e-13,
                        -0.9956602606623249195e-07,
                        0.9553977358167021521e+4,
                        0.1554006970967118286e+5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.4463221276786415752e-12,
                        0.4330010240640327080e-06,
                        -0.9470019905444229153e+4,
                        -0.6150874190563554293e+4,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.6711366622850136563e-11,
                        -0.1435050600991763331e-05,
                        0.7387344321849855078e+4,
                        0.1240047765634815732e+4,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.9422759050232662223e-10,
                        0.3460139479650695662e-05,
                        -0.4557713054166382790e+4,
                        -0.8210325475752699731e+2,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.1229055530100229098e-08,
                        -0.4988908180632898173e-05,
                        0.2207866967354055305e+4,
                        0.3242443880839930870e+2,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.1480719281585086512e-07,
                        -0.1308775976326352012e-05,
                        -0.8217975658621754746e+3,
                        -0.2923418863833160586e+2,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.1636584469123399803e-06,
                        0.2825086540850310103e-04,
                        0.2268659483507917400e+3,
                        0.3457461732814383071e+0,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.1646211436588923575e-05,
                        -0.6393913713069986071e-04,
                        -0.4633361260318560682e+2,
                        0.5489730155952392998e+1,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.1492565035840623511e-04,
                        -0.2566436514695078926e-04,
                        0.9557380123733945965e+1,
                        0.1559934132251294134e-2,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.1205533298178967851e-03,
                        0.5895792375659440364e-03,
                        -0.2958429331939661289e+1,
                        -0.1541741566831520638e+1,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.8548327023450850081e-03,
                        -0.1695715579163588598e-02,
                        0.1670329508092765480e+0,
                        0.2823152230558364186e-5,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.5223977625442187932e-02,
                        0.2089116434918055149e-03,
                        0.6096615680115419211e+0,
                        0.6249999184195342838e+0,
                    ),
                ).mul_add(
                    u.0,
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.2686617064513125222e-01,
                        0.1912855949584917753e-01,
                        0.1059212443193543585e-2,
                        0.1741749416408701288e-8,
                    ),
                );

                let mut d = u * t;
                d += Doubled::new(
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        0.11283791670955126141,
                        -0.10277263343147646779,
                        -0.50005180473999022439,
                        -0.5000000000258444377,
                    ),
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -4.0175691625932118483e-18,
                        -6.2338714083404900225e-18,
                        2.6362140569041995803e-17,
                        -4.0074044712386992281e-17,
                    ),
                );
                d *= u;
                d += Doubled::new(
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        -0.37612638903183753802,
                        -0.63661976742916359662,
                        1.601106273924963368e-06,
                        2.3761973137523364792e-13,
                    ),
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        1.3391897206042552387e-17,
                        7.6321019159085724662e-18,
                        1.1974001857764476775e-23,
                        -1.1670076950531026582e-29,
                    ),
                );
                d *= u;
                d += Doubled::new(
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        1.1283791670955125586,
                        -1.1283791674717296161,
                        -0.57236496645145429341,
                        -0.57236494292470108114,
                    ),
                    vsel_vd_vo_vo_vo_d_d_d_d(
                        o0,
                        o1,
                        o2,
                        1.5335459613165822674e-17,
                        8.0896847755965377194e-17,
                        3.0704553245872027258e-17,
                        -2.3984352208056898003e-17,
                    ),
                );

                let mut x = o1.select_doubled(d, Doubled::new(-a, ZERO)) * a;
                x = o1.select_doubled(x, x + d);
                x = o0.select_doubled(Doubled::from((1., 0.)).sub_checked(x), expk2(x));
                x = o1.select_doubled(x, x * u);

                let mut r = o3.select(x.0 + x.1, ZERO);
                r = vsignbit_vo_vd(s).select($f64x::splat(2.) - r, r);
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
                    ql = ql >> 2;
                    let o = (ddii & $ix::splat(1)).eq($ix::splat(1));
                    let mut x = Doubled::new(
                        vmulsign_vd_vd_vd($f64x::splat(-3.141592653589793116 * 0.5), ddidd.0),
                        vmulsign_vd_vd_vd($f64x::splat(-1.2246467991473532072e-16 * 0.5), ddidd.0),
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

                let mut u = $f64x::splat(-7.97255955009037868891952e-18)
                    .mul_add(s, $f64x::splat(2.81009972710863200091251e-15))
                    .mul_add(s, $f64x::splat(-7.64712219118158833288484e-13))
                    .mul_add(s, $f64x::splat(1.60590430605664501629054e-10))
                    .mul_add(s, $f64x::splat(-2.50521083763502045810755e-08))
                    .mul_add(s, $f64x::splat(2.75573192239198747630416e-06))
                    .mul_add(s, $f64x::splat(-0.000198412698412696162806809))
                    .mul_add(s, $f64x::splat(0.00833333333333332974823815))
                    .mul_add(s, $f64x::splat(-0.166666666666666657414808));

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
                    ql = ql >> 1;
                    let o = (ddii & $ix::splat(1)).eq($ix::splat(0));
                    let y = ddidd
                        .0
                        .gt(ZERO)
                        .select(ZERO, $f64x::splat(-1.));
                    let mut x = Doubled::new(
                        vmulsign_vd_vd_vd($f64x::splat(-3.141592653589793116 * 0.5), y),
                        vmulsign_vd_vd_vd($f64x::splat(-1.2246467991473532072e-16 * 0.5), y),
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

                let u = $f64x::splat(-7.97255955009037868891952e-18)
                    .mul_add(s, $f64x::splat(2.81009972710863200091251e-15))
                    .mul_add(s, $f64x::splat(-7.64712219118158833288484e-13))
                    .mul_add(s, $f64x::splat(1.60590430605664501629054e-10))
                    .mul_add(s, $f64x::splat(-2.50521083763502045810755e-08))
                    .mul_add(s, $f64x::splat(2.75573192239198747630416e-06))
                    .mul_add(s, $f64x::splat(-0.000198412698412696162806809))
                    .mul_add(s, $f64x::splat(0.00833333333333332974823815))
                    .mul_add(s, $f64x::splat(-0.166666666666666657414808));

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

                let u = $f64x::splat(1.58938307283228937328511e-10)
                    .mul_add(s, $f64x::splat(-2.50506943502539773349318e-08))
                    .mul_add(s, $f64x::splat(2.75573131776846360512547e-06))
                    .mul_add(s, $f64x::splat(-0.000198412698278911770864914))
                    .mul_add(s, $f64x::splat(0.0083333333333191845961746))
                    .mul_add(s, $f64x::splat(-0.166666666666666130709393));

                let rx = (u * s).mul_add(t, t);
                let rx = visnegzero_vo_vd(d).select($f64x::splat(-0.), rx);

                let u = $f64x::splat(-1.13615350239097429531523e-11)
                    .mul_add(s, $f64x::splat(2.08757471207040055479366e-09))
                    .mul_add(s, $f64x::splat(-2.75573144028847567498567e-07))
                    .mul_add(s, $f64x::splat(2.48015872890001867311915e-05))
                    .mul_add(s, $f64x::splat(-0.00138888888888714019282329))
                    .mul_add(s, $f64x::splat(0.0416666666666665519592062))
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

                let u = $f64x::splat(0.6880638894766060136e-11)
                    .mul_add(s, $f64x::splat(-0.1757159564542310199e-8))
                    .mul_add(s, $f64x::splat(0.3133616327257867311e-6))
                    .mul_add(s, $f64x::splat(-0.3657620416388486452e-4))
                    .mul_add(s, $f64x::splat(0.2490394570189932103e-2))
                    .mul_add(s, $f64x::splat(-0.8074551218828056320e-1))
                    .mul_add(s, $f64x::splat(0.7853981633974482790e+0));

                let rx = u * t;

                //

                let u = $f64x::splat(-0.3860141213683794352e-12)
                    .mul_add(s, $f64x::splat(0.1150057888029681415e-9))
                    .mul_add(s, $f64x::splat(-0.2461136493006663553e-7))
                    .mul_add(s, $f64x::splat(0.3590860446623516713e-5))
                    .mul_add(s, $f64x::splat(-0.3259918869269435942e-3))
                    .mul_add(s, $f64x::splat(0.1585434424381541169e-1))
                    .mul_add(s, $f64x::splat(-0.3084251375340424373e+0))
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

                    u = $f64x::splat(-4.31184585467324750724175e-05)
                        .mul_add(s2, $f64x::splat(-0.000137892809714281708733524))
                        .mul_add(s2, $f64x::splat(-6.07500301486087879295969e-05))
                        .mul_add(s2, $f64x::splat(0.000219040550724571513561967))
                        .mul_add(s2, $f64x::splat(0.00145461240472358871965441))
                        .mul_add(s2, $f64x::splat(0.00886321546662684547901456))
                        .mul_add(s2, $f64x::splat(0.0539682539049961967903002))
                        .mul_add(s2, $f64x::splat(0.333333333333320047664472));

                    let v = $f64x::splat(9.99583485362149960784268e-06)
                        .mul_add(s2, $f64x::splat(0.000103573238391744000389851))
                        .mul_add(s2, $f64x::splat(0.000157624358465342784274554))
                        .mul_add(s2, $f64x::splat(0.000148898734751616411290179))
                        .mul_add(s2, $f64x::splat(0.000595799595197098359744547))
                        .mul_add(s2, $f64x::splat(0.0035923150771440177410343))
                        .mul_add(s2, $f64x::splat(0.0218694899718446938985394))
                        .mul_add(s2, $f64x::splat(0.133333333334818976423364));

                    u = v.mul_add(s, u);
                } else {
                    u = $f64x::splat(9.99583485362149960784268e-06)
                        .mul_add(s, $f64x::splat(-4.31184585467324750724175e-05))
                        .mul_add(s, $f64x::splat(0.000103573238391744000389851))
                        .mul_add(s, $f64x::splat(-0.000137892809714281708733524))
                        .mul_add(s, $f64x::splat(0.000157624358465342784274554))
                        .mul_add(s, $f64x::splat(-6.07500301486087879295969e-05))
                        .mul_add(s, $f64x::splat(0.000148898734751616411290179))
                        .mul_add(s, $f64x::splat(0.000219040550724571513561967))
                        .mul_add(s, $f64x::splat(0.000595799595197098359744547))
                        .mul_add(s, $f64x::splat(0.00145461240472358871965441))
                        .mul_add(s, $f64x::splat(0.0035923150771440177410343))
                        .mul_add(s, $f64x::splat(0.00886321546662684547901456))
                        .mul_add(s, $f64x::splat(0.0218694899718446938985394))
                        .mul_add(s, $f64x::splat(0.0539682539049961967903002))
                        .mul_add(s, $f64x::splat(0.133333333334818976423364))
                        .mul_add(s, $f64x::splat(0.333333333333320047664472));
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
                        vsignbit_vo_vd(x),
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

                    u = $f64x::splat(-0.1581918243329996643e-1)
                        .mul_add(x4, $f64x::splat(0.6606077476277170610e-2))
                        .mul_add(x4, $f64x::splat(0.1388715184501609218e-1))
                        .mul_add(x4, $f64x::splat(0.2237176181932048341e-1))
                        .mul_add(x4, $f64x::splat(0.4464285681377102438e-1))
                        .mul_add(x4, $f64x::splat(0.1666666666666497543e+0));

                    let v = $f64x::splat(0.3161587650653934628e-1)
                        .mul_add(x4, $f64x::splat(0.1929045477267910674e-1))
                        .mul_add(x4, $f64x::splat(0.1215360525577377331e-1))
                        .mul_add(x4, $f64x::splat(0.1735956991223614604e-1))
                        .mul_add(x4, $f64x::splat(0.3038195928038132237e-1))
                        .mul_add(x4, $f64x::splat(0.7500000000378581611e-1));

                    u = v.mul_add(x2, u);
                } else {
                    u = $f64x::splat(0.3161587650653934628e-1)
                        .mul_add(x2, $f64x::splat(-0.1581918243329996643e-1))
                        .mul_add(x2, $f64x::splat(0.1929045477267910674e-1))
                        .mul_add(x2, $f64x::splat(0.6606077476277170610e-2))
                        .mul_add(x2, $f64x::splat(0.1215360525577377331e-1))
                        .mul_add(x2, $f64x::splat(0.1388715184501609218e-1))
                        .mul_add(x2, $f64x::splat(0.1735956991223614604e-1))
                        .mul_add(x2, $f64x::splat(0.2237176181932048341e-1))
                        .mul_add(x2, $f64x::splat(0.3038195928038132237e-1))
                        .mul_add(x2, $f64x::splat(0.4464285681377102438e-1))
                        .mul_add(x2, $f64x::splat(0.7500000000378581611e-1))
                        .mul_add(x2, $f64x::splat(0.1666666666666497543e+0));
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

                    u = $f64x::splat(-0.1581918243329996643e-1)
                        .mul_add(x4, $f64x::splat(0.6606077476277170610e-2))
                        .mul_add(x4, $f64x::splat(0.1388715184501609218e-1))
                        .mul_add(x4, $f64x::splat(0.2237176181932048341e-1))
                        .mul_add(x4, $f64x::splat(0.4464285681377102438e-1))
                        .mul_add(x4, $f64x::splat(0.1666666666666497543e+0));

                    let v = $f64x::splat(0.3161587650653934628e-1)
                        .mul_add(x4, $f64x::splat(0.1929045477267910674e-1))
                        .mul_add(x4, $f64x::splat(0.1215360525577377331e-1))
                        .mul_add(x4, $f64x::splat(0.1735956991223614604e-1))
                        .mul_add(x4, $f64x::splat(0.3038195928038132237e-1))
                        .mul_add(x4, $f64x::splat(0.7500000000378581611e-1));

                    u = v.mul_add(x2, u);
                } else {
                    u = $f64x::splat(0.3161587650653934628e-1)
                        .mul_add(x2, $f64x::splat(-0.1581918243329996643e-1))
                        .mul_add(x2, $f64x::splat(0.1929045477267910674e-1))
                        .mul_add(x2, $f64x::splat(0.6606077476277170610e-2))
                        .mul_add(x2, $f64x::splat(0.1215360525577377331e-1))
                        .mul_add(x2, $f64x::splat(0.1388715184501609218e-1))
                        .mul_add(x2, $f64x::splat(0.1735956991223614604e-1))
                        .mul_add(x2, $f64x::splat(0.2237176181932048341e-1))
                        .mul_add(x2, $f64x::splat(0.3038195928038132237e-1))
                        .mul_add(x2, $f64x::splat(0.4464285681377102438e-1))
                        .mul_add(x2, $f64x::splat(0.7500000000378581611e-1))
                        .mul_add(x2, $f64x::splat(0.1666666666666497543e+0));
                }

                u *= x2 * x;

                let y = $f64x::FRAC_PI_2 - (vmulsign_vd_vd_vd(x, d) + vmulsign_vd_vd_vd(u, d));
                x = x + u;
                let r = o.select(y, x * $f64x::splat(2.));
                vandnot_vo_vo_vo(o, d.lt(ZERO)).select(
                    Doubled::from((3.141592653589793116, 1.2246467991473532072e-16))
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

                    u = $f64x::splat(-1.88796008463073496563746e-05)
                        .mul_add(t2, $f64x::splat(-0.00110611831486672482563471))
                        .mul_add(t2, $f64x::splat(-0.00889896195887655491740809))
                        .mul_add(t2, $f64x::splat(-0.0254517624932312641616861))
                        .mul_add(t2, $f64x::splat(-0.0407629191276836500001934))
                        .mul_add(t2, $f64x::splat(-0.0523674852303482457616113))
                        .mul_add(t2, $f64x::splat(-0.0666573579361080525984562))
                        .mul_add(t2, $f64x::splat(-0.090908995008245008229153))
                        .mul_add(t2, $f64x::splat(-0.14285714266771329383765))
                        .mul_add(t2, $f64x::splat(-0.333333333333311110369124));

                    let v = $f64x::splat(0.000209850076645816976906797)
                        .mul_add(t2, $f64x::splat(0.00370026744188713119232403))
                        .mul_add(t2, $f64x::splat(0.016599329773529201970117))
                        .mul_add(t2, $f64x::splat(0.0337852580001353069993897))
                        .mul_add(t2, $f64x::splat(0.0466667150077840625632675))
                        .mul_add(t2, $f64x::splat(0.0587666392926673580854313))
                        .mul_add(t2, $f64x::splat(0.0769219538311769618355029))
                        .mul_add(t2, $f64x::splat(0.111111105648261418443745))
                        .mul_add(t2, $f64x::splat(0.199999999996591265594148));

                    u = v.mul_add(t, u);
                } else {
                    u = $f64x::splat(-1.88796008463073496563746e-05)
                        .mul_add(t, $f64x::splat(0.000209850076645816976906797))
                        .mul_add(t, $f64x::splat(-0.00110611831486672482563471))
                        .mul_add(t, $f64x::splat(0.00370026744188713119232403))
                        .mul_add(t, $f64x::splat(-0.00889896195887655491740809))
                        .mul_add(t, $f64x::splat(0.016599329773529201970117))
                        .mul_add(t, $f64x::splat(-0.0254517624932312641616861))
                        .mul_add(t, $f64x::splat(0.0337852580001353069993897))
                        .mul_add(t, $f64x::splat(-0.0407629191276836500001934))
                        .mul_add(t, $f64x::splat(0.0466667150077840625632675))
                        .mul_add(t, $f64x::splat(-0.0523674852303482457616113))
                        .mul_add(t, $f64x::splat(0.0587666392926673580854313))
                        .mul_add(t, $f64x::splat(-0.0666573579361080525984562))
                        .mul_add(t, $f64x::splat(0.0769219538311769618355029))
                        .mul_add(t, $f64x::splat(-0.090908995008245008229153))
                        .mul_add(t, $f64x::splat(0.111111105648261418443745))
                        .mul_add(t, $f64x::splat(-0.14285714266771329383765))
                        .mul_add(t, $f64x::splat(0.199999999996591265594148))
                        .mul_add(t, $f64x::splat(-0.333333333333311110369124));
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

                let t = $f64x::splat(0.153487338491425068243146)
                    .mul_add(x2, $f64x::splat(0.152519917006351951593857))
                    .mul_add(x2, $f64x::splat(0.181863266251982985677316))
                    .mul_add(x2, $f64x::splat(0.222221366518767365905163))
                    .mul_add(x2, $f64x::splat(0.285714294746548025383248))
                    .mul_add(x2, $f64x::splat(0.399999999950799600689777))
                    .mul_add(x2, $f64x::splat(0.6666666666667778740063))
                    .mul_add(x2, $f64x::splat(2.));

                x = x.mul_add(t, $f64x::splat(0.693147180559945286226764) * ef);
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
                y = y * ($f64x::splat(0.5) * e);

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

                y = (x.abs().gt($f64x::splat(18.714973875)) | y.is_nan())
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
                    .select($f64x::splat(1.2599210498948731647672106), q);
                q = $m64x::from_cast(re.eq($ix::splat(2)))
                    .select($f64x::splat(1.5874010519681994747517056), q);
                q = vldexp2_vd_vd_vi(q, qu - $ix::splat(2048));

                q = vmulsign_vd_vd_vd(q, d);

                d = d.abs();

                let mut x = $f64x::splat(-0.640245898480692909870982)
                    .mul_add(d, $f64x::splat(2.96155103020039511818595))
                    .mul_add(d, $f64x::splat(-5.73353060922947843636166))
                    .mul_add(d, $f64x::splat(6.03990368989458747961407))
                    .mul_add(d, $f64x::splat(-3.85841935510444988821632))
                    .mul_add(d, $f64x::splat(2.2307275302496609725722));

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
