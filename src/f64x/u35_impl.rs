macro_rules! impl_math_f64_u35 {
    ($f64x:ident, $u64x:ident, $m64x:ident, $i64x:ident, $ux:ident, $mx:ident, $ix:ident) => {
        use super::*;

        pub fn sin(mut d: $f64x) -> $f64x {
            let r = d;
            let mut ql: $ix;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * $f64x::FRAC_1_PI).round();
                ql = dql.roundi();
                d = dql.mul_add(-PI_A2, d);
                d = dql.mul_add(-PI_B2, d);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = (d * ($f64x::FRAC_1_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = d.mul_sub($f64x::FRAC_1_PI, dqh).round();
                ql = dql.roundi();

                d = dqh.mul_add(-PI_A, d);
                d = dql.mul_add(-PI_A, d);
                d = dqh.mul_add(-PI_B, d);
                d = dql.mul_add(-PI_B, d);
                d = dqh.mul_add(-PI_C, d);
                d = dql.mul_add(-PI_C, d);
                d = (dqh + dql).mul_add(-PI_D, d);
            } else {
                let (mut ddidd, ddii) = rempi(d);
                ql = ddii & $ix::splat(3);
                ql = ql + ql + $mx::from_cast(ddidd.0.gt(ZERO))
                    .select($ix::splat(2), $ix::splat(1));
                ql >>= 2;
                let o = (ddii & $ix::splat(1)).eq($ix::splat(1));
                let mut x = Doubled::new(
                    $f64x::splat(-3.141_592_653_589_793_116 * 0.5).mul_sign(ddidd.0),
                    $f64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5).mul_sign(ddidd.0),
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
                    $u64x::from_bits(NEG_ZERO),
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

            r.is_neg_zero().select(r, u)
        }

        pub fn cos(mut d: $f64x) -> $f64x {
            let r = d;
            let mut ql: $ix;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = $f64x::splat(2.).mul_add(
                    d.mul_add($f64x::FRAC_1_PI, $f64x::splat(-0.5)).round(),
                    ONE,
                );
                ql = dql.roundi();
                d = dql.mul_add(-PI_A2 * HALF, d);
                d = dql.mul_add(-PI_B2 * HALF, d);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = d
                    .mul_add($f64x::FRAC_1_PI / D1_23X, -$f64x::FRAC_1_PI / D1_24X)
                    .trunc();
                ql = (d * $f64x::FRAC_1_PI
                    + dqh.mul_add(-D1_23X, $f64x::splat(-0.5))).roundi();
                let dqh = dqh * D1_24X;
                ql = ql + ql + $ix::splat(1);
                let dql = $f64x::from_cast(ql);

                d = dqh.mul_add(-PI_A * HALF, d);
                d = dql.mul_add(-PI_A * HALF, d);
                d = dqh.mul_add(-PI_B * HALF, d);
                d = dql.mul_add(-PI_B * HALF, d);
                d = dqh.mul_add(-PI_C * HALF, d);
                d = dql.mul_add(-PI_C * HALF, d);
                d = (dqh + dql).mul_add(-PI_D * HALF, d);
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
                    $f64x::splat(-3.141_592_653_589_793_116 * 0.5).mul_sign(y),
                    $f64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5).mul_sign(y),
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
                    $u64x::from_bits(NEG_ZERO),
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

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * $f64x::FRAC_2_PI).round();
                ql = dql.roundi();
                s = dql.mul_add(-PI_A2 * HALF, d);
                s = dql.mul_add(-PI_B2 * HALF, s);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = (d * ($f64x::FRAC_2_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = (d * $f64x::FRAC_2_PI - dqh).round();
                ql = dql.roundi();

                s = dqh.mul_add(-PI_A * HALF, d);
                s = dql.mul_add(-PI_A * HALF, s);
                s = dqh.mul_add(-PI_B * HALF, s);
                s = dql.mul_add(-PI_B * HALF, s);
                s = dqh.mul_add(-PI_C * HALF, s);
                s = dql.mul_add(-PI_C * HALF, s);
                s = (dqh + dql).mul_add(-PI_D * HALF, s);
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
            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

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
                vand_vm_vo64_vm(o, $u64x::from_bits(NEG_ZERO)) ^ $u64x::from_bits(rsin),
            );

            let o = $m64x::from_cast(((ql + $ix::splat(1)) & $ix::splat(2)).eq($ix::splat(2)));
            rcos = $f64x::from_bits(
                vand_vm_vo64_vm(o, $u64x::from_bits(NEG_ZERO)) ^ $u64x::from_bits(rcos),
            );

            (rsin, rcos)
        }

        pub fn sincospi(d: $f64x) -> ($f64x, $f64x) {
            let u = d * $f64x::splat(4.);
            let mut q = u.trunci();
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
                vand_vm_vo64_vm(o, $u64x::from_bits(NEG_ZERO)) ^ $u64x::from_bits(rsin),
            );

            let o = $m64x::from_cast(((q + $ix::splat(2)) & $ix::splat(4)).eq($ix::splat(4)));
            rcos = $f64x::from_bits(
                vand_vm_vo64_vm(o, $u64x::from_bits(NEG_ZERO)) ^ $u64x::from_bits(rcos),
            );

            let o = d.abs().gt(TRIGRANGEMAX3 / $f64x::splat(4.));
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

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * $f64x::FRAC_2_PI).round();
                ql = dql.roundi();
                x = dql.mul_add(-PI_A2 * HALF, d);
                x = dql.mul_add(-PI_B2 * HALF, x);
            } else if d.abs().lt($f64x::splat(1e+7)).all() {
                let dqh = (d * ($f64x::FRAC_2_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = (d * $f64x::FRAC_2_PI - dqh).round();
                ql = dql.roundi();

                x = dqh.mul_add(-PI_A * HALF, d);
                x = dql.mul_add(-PI_A * HALF, x);
                x = dqh.mul_add(-PI_B * HALF, x);
                x = dql.mul_add(-PI_B * HALF, x);
                x = dqh.mul_add(-PI_C * HALF, x);
                x = dql.mul_add(-PI_C * HALF, x);
                x = (dqh + dql).mul_add(-PI_D * HALF, x);
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
                vand_vm_vo64_vm(o, $u64x::from_bits(NEG_ZERO)) ^ $u64x::from_bits(x),
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

            r = r.mul_sign(x);
            r = (x.is_infinite() | x.eq(ZERO)).select(
                $f64x::FRAC_PI_2
                    - visinf2_vd_vd_vd(x, $f64x::FRAC_PI_2.mul_sign(x)),
                r,
            );
            r = y.is_infinite().select(
                $f64x::FRAC_PI_2
                    - visinf2_vd_vd_vd(x, $f64x::FRAC_PI_4.mul_sign(x)),
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
                $u64x::from_bits(r.mul_sign(y)),
            ))
        }

        pub fn asin(d: $f64x) -> $f64x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
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
            r.mul_sign(d)
        }
        pub fn acos(d: $f64x) -> $f64x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
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

            let y = $f64x::FRAC_PI_2 - (x.mul_sign(d) + u.mul_sign(d));
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
                    $u64x::from_bits(NEG_ZERO),
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
            y *= HALF * e;

            y = (x.abs().gt($f64x::splat(709.)) | y.is_nan())
                .select($f64x::INFINITY, y);
            y = y.mul_sign(x);
            $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
        }

        pub fn cosh(x: $f64x) -> $f64x {
            let e = u10::exp(x.abs());
            let mut y = HALF.mul_add(e, HALF / e);

            y = (x.abs().gt($f64x::splat(709.)) | y.is_nan())
                .select($f64x::INFINITY, y);
            $f64x::from_bits(vor_vm_vo64_vm(x.is_nan(), $u64x::from_bits(y)))
        }

        pub fn tanh(x: $f64x) -> $f64x {
            let d = expm1k($f64x::splat(2.) * x.abs());
            let mut y = d / ($f64x::splat(2.) + d);

            y = (x.abs().gt($f64x::splat(18.714_973_875)) | y.is_nan())
                .select(ONE, y);
            y = y.mul_sign(x);
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
            let qu = (t * $f64x::splat(1. / 3.)).trunci();
            let re = (t * ($f64x::from_cast(qu) * $f64x::splat(3.))).trunci();

            q = $m64x::from_cast(re.eq($ix::splat(1)))
                .select($f64x::splat(1.259_921_049_894_873_164_767_210_6), q);
            q = $m64x::from_cast(re.eq($ix::splat(2)))
                .select($f64x::splat(1.587_401_051_968_199_474_751_705_6), q);
            q = vldexp2_vd_vd_vi(q, qu - $ix::splat(2048));

            q = q.mul_sign(d);

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
                y = s.is_infinite().select($f64x::INFINITY.mul_sign(s), y);
                y = s
                    .eq(ZERO)
                    .select(ZERO.mul_sign(s), y);
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

    };
}
