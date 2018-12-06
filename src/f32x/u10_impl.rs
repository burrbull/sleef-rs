macro_rules! impl_math_f32_u10 {
    ($f32x:ident, $u32x:ident, $m32x:ident, $i32x:ident) => {
        use super::*;

        pub fn sinf(d: $f32x) -> $f32x {
            let mut q: $i32x;
            let mut s: Doubled<$f32x>;

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                let u = (d * $f32x::FRAC_1_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F, d);
                s = v.add_as_doubled(u * (-PI_B2_F));
                s = s.add_checked(u * (-PI_C2_F));
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
                    $f32x::splat(3.141_592_741_012_573_242_2 * -0.5).mul_sign(dfidf.0),
                    $f32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5).mul_sign(dfidf.0),
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

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                let dq = (d.mul_add($f32x::FRAC_1_PI, $f32x::splat(-0.5)))
                    .round()
                    .mul_add($f32x::splat(2.), ONE);
                q = dq.roundi();
                s = d.add_as_doubled(dq * (-PI_A2_F) * HALF);
                s += dq * (-PI_B2_F) * HALF;
                s += dq * (-PI_C2_F) * HALF;
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
                    $f32x::splat(3.141_592_741_012_573_242_2 * -0.5).mul_sign(y),
                    $f32x::splat(-8.742_277_657_347_585_773_1_e-8 * -0.5).mul_sign(y),
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

            if d.abs().lt(TRIGRANGEMAX2_F).all() {
                let u = (d * $f32x::FRAC_2_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F * HALF, d);
                s = v.add_as_doubled(u * (-PI_B2_F) * HALF);
                s = s.add_checked(u * (-PI_C2_F) * HALF);
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

            let mut s = if d.abs().lt(TRIGRANGEMAX2_F).all() {
                let u = (d * $f32x::FRAC_2_PI).round();
                q = u.roundi();
                let v = u.mul_add(-PI_A2_F * HALF, d);
                v.add_as_doubled(u * (-PI_B2_F) * HALF)
                    .add_checked(u * (-PI_C2_F) * HALF)
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

            r = r.mul_sign(x);
            r = (x.is_infinite() | x.eq(ZERO)).select(
                $f32x::FRAC_PI_2
                    - visinf2_vf_vf_vf(x, $f32x::FRAC_PI_2.mul_sign(x)),
                r,
            );
            r = y.is_infinite().select(
                $f32x::FRAC_PI_2
                    - visinf2_vf_vf_vf(x, $f32x::FRAC_PI_4.mul_sign(x)),
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
                $u32x::from_bits(r.mul_sign(y)),
            ))
        }

        pub fn asinf(d: $f32x) -> $f32x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
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
            r.mul_sign(d)
        }

        pub fn acosf(d: $f32x) -> $f32x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);

            let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
            x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

            let u = $f32x::splat(0.419_745_482_5_e-1)
                .mul_add(x2, $f32x::splat(0.242_404_602_5_e-1))
                .mul_add(x2, $f32x::splat(0.454_742_386_9_e-1))
                .mul_add(x2, $f32x::splat(0.749_502_927_1_e-1))
                .mul_add(x2, $f32x::splat(0.166_667_729_6))
                * (x2 * x.0);

            let mut y = Doubled::from((3.141_592_741_012_573_242_2 / 2., -8.742_277_657_347_585_773_1_e-8 / 2.))
                .sub_checked(x.0.mul_sign(d).add_checked_as_doubled(u.mul_sign(d)));
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
            r.mul_sign(d)
        }


        pub fn expf(d: $f32x) -> $f32x {
            let q = (d * R_LN2_F).roundi();

            let s = $f32x::from_cast(q).mul_add(-L2U_F, d);
            let s = $f32x::from_cast(q).mul_add(-L2L_F, s);

            let mut u = $f32x::splat(0.000_198_527_617_612_853_646_278_381)
                .mul_add(s, $f32x::splat(0.001_393_043_552_525_341_510_772_71))
                .mul_add(s, $f32x::splat(0.008_333_360_776_305_198_669_433_59))
                .mul_add(s, $f32x::splat(0.041_666_485_369_205_474_853_515_6))
                .mul_add(s, $f32x::splat(0.166_666_671_633_720_397_949_219))
                .mul_add(s, HALF);

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
            let qu = (t * $f32x::splat(1. / 3.)).trunci();
            let re = (t - $f32x::from_cast(qu) * $f32x::splat(3.)).trunci();

            q2 = re.eq($i32x::splat(1)).select_doubled(Doubled::from((1.259_921_073_913_574_218_8, -2.401_870_169_421_727_041_5_e-8)),
                q2,
            );
            q2 = re.eq($i32x::splat(2)).select_doubled(Doubled::from((1.587_401_032_447_814_941_4, 1.952_038_530_816_935_235_6_e-8)),
                q2,
            );

            q2.0 = q2.0.mul_sign(d);
            q2.1 = q2.1.mul_sign(d);
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

            z = d.is_infinite().select($f32x::INFINITY.mul_sign(q2.0), z);
            z = d
                .eq(ZERO)
                .select($f32x::from_bits(q2.0.sign_bit()), z);

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                z = s.is_infinite().select($f32x::INFINITY.mul_sign(s), z);
                z = s
                    .eq(ZERO)
                    .select((ZERO, s).mul_sign(z);
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
                let yisint = y.trunc().eq(y) | y.abs().gt(F1_24X);
                let yisodd = (y.trunci() & $i32x::splat(1)).eq($i32x::splat(1))
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

                let efx = (x.abs() - ONE).mul_sign(y);

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
                    yisodd.select(x.sign(), ONE) * $f32x::from_bits(
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
            y = (d.0 + d.1) * HALF;

            y = (x.abs().gt($f32x::splat(89.)) | y.is_nan())
                .select($f32x::INFINITY, y);
            y = y.mul_sign(x);
            $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
        }

        pub fn coshf(x: $f32x) -> $f32x {
            let mut y = x.abs();
            let d = expk2f(Doubled::new(y, ZERO));
            let d = d.add_checked(d.recpre());
            y = (d.0 + d.1) * HALF;

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
            y = y.mul_sign(x);
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

            y = (x.abs().gt(SQRT_FLT_MAX) | y.is_nan())
                .select($f32x::INFINITY, x).mul_sign(y);
            y = $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)));
            visnegzero_vo_vf(x).select($f32x::splat(-0.), y)
        }

        pub fn acoshf(x: $f32x) -> $f32x {
            let d = logk2f(
                x.add_as_doubled(ONE).sqrt() * x.add_as_doubled($f32x::splat(-1.)).sqrt() + x,
            );
            let mut y = d.0 + d.1;

            y = (x.abs().gt(SQRT_FLT_MAX) | y.is_nan())
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
                    (d.0 + d.1) * HALF,
                )),
            ));

            y = $f32x::from_bits(vor_vm_vo32_vm(
                x.is_infinite() | y.is_nan(),
                $u32x::from_bits(y),
            ));
            y = y.mul_sign(x);
            $f32x::from_bits(vor_vm_vo32_vm(x.is_nan(), $u32x::from_bits(y)))
        }


        pub fn exp10f(d: $f32x) -> $f32x {
            let mut u = (d * LOG10_2_F).round();
            let q = u.roundi();

            let s = u.mul_add(-L10U_F, d);
            let s = u.mul_add(-L10L_F, s);

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
            o.select($f32x::INFINITY, a).mul_sign(r)
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
            let u = o2.select(d.0 + d.1, ONE).mul_sign(s);
            a.is_nan().select($f32x::NAN, u)
        }

    };
}
