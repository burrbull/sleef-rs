macro_rules! impl_math_f64_u10 {
    () => {
        use super::*;

        #[cfg(not(feature = "deterministic"))]
        pub fn sin(d: F64x) -> F64x {
            let mut s;
            let mut ql;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * F64x::FRAC_1_PI).round();
                ql = dql.roundi();
                let u = dql.mul_add(-PI_A2, d);
                s = u.add_checked_as_doubled(dql * (-PI_B2));
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = (d * (F64x::FRAC_1_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = (d.mul_sub(F64x::FRAC_1_PI, dqh)).round();
                ql = dql.roundi();

                let u = dqh.mul_add(-PI_A, d);
                s = u.add_checked_as_doubled(dql * (-PI_A));
                s += dqh * (-PI_B);
                s += dql * (-PI_B);
                s += dqh * (-PI_C);
                s += dql * (-PI_C);
                s += (dqh + dql) * (-PI_D);
            } else {
                let (mut ddidd, ddii) = rempi(d);
                ql = ddii & Ix::splat(3);
                ql = ql + ql + Mx::from_cast(ddidd.0.gt(ZERO)).select(Ix::splat(2), Ix::splat(1));
                ql >>= 2;
                let o = (ddii & Ix::splat(1)).eq(Ix::splat(1));
                let mut x = Doubled::new(
                    F64x::splat(-3.141_592_653_589_793_116 * 0.5).mul_sign(ddidd.0),
                    F64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5).mul_sign(ddidd.0),
                );
                x = ddidd + x;
                ddidd = M64x::from_cast(o).select_doubled(x, ddidd);
                s = ddidd.normalize();
                s.0 = F64x::from_bits(
                    U64x::from_bits(d.is_infinite() | d.is_nan()) | U64x::from_bits(s.0),
                );
            }

            let t = s;
            s = s.square();

            let mut u = F64x::splat(2.720_524_161_385_295_679_179_83_e-15)
                .mul_add(s.0, F64x::splat(-7.642_925_941_139_544_719_002_3_e-13))
                .mul_add(s.0, F64x::splat(1.605_893_701_172_778_962_116_23_e-10))
                .mul_add(s.0, F64x::splat(-2.505_210_681_484_312_335_936_8_e-8))
                .mul_add(s.0, F64x::splat(2.755_731_921_044_282_247_773_79_e-6))
                .mul_add(s.0, F64x::splat(-0.000_198_412_698_412_046_454_654_947))
                .mul_add(s.0, F64x::splat(0.008_333_333_333_333_180_562_019_22));

            let x = ONE.add_checked(
                (F64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0)) * s,
            );

            u = t.mul_as_f(x);

            u = F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(1))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(u),
            );
            d.eq(ZERO).select(d, u)
        }

        #[cfg(feature = "deterministic")]
        pub fn sin(mut d: F64x) -> F64x {
            let mut s;
            let mut ql;

            let g = d.abs().lt(TRIGRANGEMAX2);
            let dql = (d * F64x::FRAC_1_PI).round();
            ql = dql.roundi();
            let u = dql.mul_add(-PI_A2, d);
            let mut x = u.add_checked_as_doubled(dql * (-PI_B2));

            if !g.all() {
                let mut dqh = (d * F64x::FRAC_1_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let dql = d.mul_sub(F64x::FRAC_1_PI, dqh).round();

                let u = dqh.mul_add(-PI_A, d);
                s = u.add_checked_as_doubled(dql * (-PI_A));
                s += dqh * (-PI_B);
                s += dql * (-PI_B);
                s += dqh * (-PI_C);
                s += dql * (-PI_C);
                s = s.add_checked((dqh + dql) * (-PI_D));

                ql = Mx::from_cast(g).select(ql, dql.roundi());
                x = g.select_doubled(x, s);
                g = d.abs().lt(TRIGRANGEMAX);

                if !g.all() {
                    let (mut ddidd, ddii) = rempi(d);
                    let ql2 = ddii & Ix::splat(3);
                    ql2 = ql2 + ql2 + Mx::from_cast(ddidd.0.gt(ZERO)).select(Ix::splat(2), Ix::splat(1));
                    ql2 >>= 2;
                    let o = (ddii & Ix::splat(1)).eq(Ix::splat(1));
                    let mut t = Doubled::new(
                        F64x::splat(-3.141_592_653_589_793_116 * 0.5).mul_sign(ddidd.0),
                        F64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5).mul_sign(ddidd.0),
                    );
                    t = ddidd + t;
                    ddidd = M64x::from_cast(o).select_doubled(t, ddidd);
                    s = ddidd.normalize();
                    ql = Mx::from_cast(g).select(ql, ql2);
                    x = g.select_doubled(x, s);
                    x.0 = F64x::from_bits(
                        U64x::from_bits(d.is_infinite() | d.is_nan()) | U64x::from_bits(x.0),
                    );
                }
            }

            let t = x;
            s = x.square();

            let mut u = F64x::splat(2.720_524_161_385_295_679_179_83_e-15)
                .mul_add(s.0, F64x::splat(-7.642_925_941_139_544_719_002_3_e-13))
                .mul_add(s.0, F64x::splat(1.605_893_701_172_778_962_116_23_e-10))
                .mul_add(s.0, F64x::splat(-2.505_210_681_484_312_335_936_8_e-8))
                .mul_add(s.0, F64x::splat(2.755_731_921_044_282_247_773_79_e-6))
                .mul_add(s.0, F64x::splat(-0.000_198_412_698_412_046_454_654_947))
                .mul_add(s.0, F64x::splat(0.008_333_333_333_333_180_562_019_22));

            x = ONE.add_checked(
                F64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0) * s,
            );

            u = t.mul_as_f(x);

            u = F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(1))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(u),
            );

            d.eq(ZERO).select(d, u)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn cos(d: F64x) -> F64x {
            let mut s;
            let mut ql;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = d.mul_add(F64x::FRAC_1_PI, F64x::splat(-0.5)).round();
                let dql = F64x::splat(2.).mul_add(dql, ONE);
                ql = dql.roundi();
                s = d.add_as_doubled(dql * (-PI_A2) * HALF);
                s = s.add_checked(dql * (-PI_B2) * HALF);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = d
                    .mul_add(F64x::FRAC_1_PI / D1_23X, -F64x::FRAC_1_PI / D1_24X)
                    .trunc();
                ql = (d * F64x::FRAC_1_PI + dqh.mul_add(-D1_23X, F64x::splat(-0.5))).roundi();
                let dqh = dqh * D1_24X;
                ql = ql + ql + Ix::splat(1);
                let dql = F64x::from_cast(ql);

                let u = dqh.mul_add(-PI_A * HALF, d);
                s = u.add_as_doubled(dql * -PI_A * HALF);
                s += dqh * (-PI_B) * HALF;
                s += dql * (-PI_B) * HALF;
                s += dqh * (-PI_C) * HALF;
                s += dql * (-PI_C) * HALF;
                s = s.add_checked((dqh + dql) * (-PI_D) * HALF);
            } else {
                let (mut ddidd, ddii) = rempi(d);
                ql = ddii & Ix::splat(3);
                ql = ql + ql + Mx::from_cast(ddidd.0.gt(ZERO)).select(Ix::splat(8), Ix::splat(7));
                ql >>= 1;
                let o = (ddii & Ix::splat(1)).eq(Ix::splat(0));
                let y = ddidd.0.gt(ZERO).select(ZERO, F64x::splat(-1.));
                let mut x = Doubled::new(
                    F64x::splat(-3.141_592_653_589_793_116 * 0.5).mul_sign(y),
                    F64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5).mul_sign(y),
                );
                x = ddidd + x;
                ddidd = M64x::from_cast(o).select_doubled(x, ddidd);
                s = ddidd.normalize();
                s.0 = F64x::from_bits(
                    U64x::from_bits(d.is_infinite() | d.is_nan()) | U64x::from_bits(s.0),
                );
            }

            let t = s;
            s = s.square();

            let u = F64x::splat(2.720_524_161_385_295_679_179_83_e-15)
                .mul_add(s.0, F64x::splat(-7.642_925_941_139_544_719_002_3_e-13))
                .mul_add(s.0, F64x::splat(1.605_893_701_172_778_962_116_23_e-10))
                .mul_add(s.0, F64x::splat(-2.505_210_681_484_312_335_936_8_e-8))
                .mul_add(s.0, F64x::splat(2.755_731_921_044_282_247_773_79_e-6))
                .mul_add(s.0, F64x::splat(-0.000_198_412_698_412_046_454_654_947))
                .mul_add(s.0, F64x::splat(0.008_333_333_333_333_180_562_019_22));

            let x = ONE.add_checked(
                (F64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0)) * s,
            );

            let u = t.mul_as_f(x);

            F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((ql & Ix::splat(2)).eq(Ix::splat(0))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(u),
            )
        }

        #[cfg(feature = "deterministic")]
        pub fn cos(mut d: F64x) -> F64x {
            let g = d.abs().lt(TRIGRANGEMAX2);
            let mut dql = d.mul_add(F64x::FRAC_1_PI, F64x::splat(-0.5)).round();
            dql = F64x::splat(2.).mul_add(dql, ONE);
            let mut ql = dql.roundi();
            let mut x = d.add_as_doubled(dql * (-PI_A2 * HALF));
            x = x.add_checked(dql * (-PI_B2 * HALF));

            if !g.all() {
                let mut dqh = (d.mul_add(F64x::FRAC_1_PI / D1_23X, -F64x::FRAC_1_PI / D1_24X)).trunc();
                let mut ql2 = (d * F64x::FRAC_1_PI + dqh.mul_add(-D1_23X, F64x::splat(-0.5))).roundi();
                dqh *= D1_24X;
                ql2 = ql2 + ql2 + Ix::splat(1);
                let dql = F64x::from_cast(ql2);

                let u = dqh.mul_add(-PI_A * HALF, d);
                let mut s = u.add_as_doubled(dql * (-PI_A * HALF));
                s += dqh * (-PI_B * HALF);
                s += dql * (-PI_B * HALF);
                s += dqh * (-PI_C * HALF);
                s += dql * (-PI_C * HALF);
                s = s.add_checked((dqh + dql) * (-PI_D * HALF));

                ql = Mx::from_cast(g).select(ql, ql2);
                x = g.select_doubled(x, s);
                g = d.abs().lt(TRIGRANGEMAX);

                if !g.all() {
                    let (mut ddidd, ddii) = rempi(d);
                    let mut ql2 = ddii & Ix::splat(3);
                    ql2 = ql2 + ql2 + Mx::from_cast(ddidd.0.gt(ZERO)).select(Ix::splat(8), Ix::splat(7));
                    ql2 >>= 1;
                    let o = (ddii & Ix::splat(1)).eq(Ix::splat(0));
                    let y = ddidd.0.gt(ZERO).select(ZERO, F64x::splat(-1.));
                    let mut t = Doubled::new(
                        F64x::splat(-3.141_592_653_589_793_116 * 0.5).mul_sign(y),
                        F64x::splat(-1.224_646_799_147_353_207_2_e-16 * 0.5).mul_sign(y),
                    );
                    t = ddidd + t;
                    ddidd = M64x::from_cast(o).select_doubled(t, ddidd);
                    s = ddidd.normalize();
                    ql = Mx::from_cast(g).select(ql, ql2);
                    x = g.select_doubled(x, s);
                    x.0 = F64x::from_bits(
                        U64x::from_bits(d.is_infinite() | d.is_nan()) | U64x::from_bits(x.0),
                    );
                }
            }

            let t = x;
            let s = x.square();

            let u = F64x::splat(2.720_524_161_385_295_679_179_83_e-15)
                .mul_add(s.0, F64x::splat(-7.642_925_941_139_544_719_002_3_e-13))
                .mul_add(s.0, F64x::splat(1.605_893_701_172_778_962_116_23_e-10))
                .mul_add(s.0, F64x::splat(-2.505_210_681_484_312_335_936_8_e-8))
                .mul_add(s.0, F64x::splat(2.755_731_921_044_282_247_773_79_e-6))
                .mul_add(s.0, F64x::splat(-0.000_198_412_698_412_046_454_654_947))
                .mul_add(s.0, F64x::splat(0.008_333_333_333_333_180_562_019_22));

            x = ONE.add_checked(
                F64x::splat(-0.166_666_666_666_666_657_414_808).add_checked_as_doubled(u * s.0) * s,
            );

            u = t.mul_as_f(x);

            F64x::from_bits(
                (U64x::from_bits(M64x::from_cast((ql & Ix::splat(2)).eq(Ix::splat(0))))
                    & U64x::from_bits(NEG_ZERO))
                    ^ U64x::from_bits(u),
            )
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn sincos(d: F64x) -> (F64x, F64x) {
            let mut s;
            let ql;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * F64x::FRAC_2_PI).round();
                ql = dql.roundi();
                let u = dql.mul_add(-PI_A2 * HALF, d);
                s = u.add_checked_as_doubled(dql * (-PI_B2) * HALF);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = (d * (F64x::FRAC_2_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                let dql = (d * F64x::FRAC_2_PI - dqh).round();
                ql = dql.roundi();

                let u = dqh.mul_add(-PI_A * HALF, d);
                s = u.add_checked_as_doubled(dql * (-PI_A) * HALF);
                s += dqh * (-PI_B) * HALF;
                s += dql * (-PI_B) * HALF;
                s += dqh * (-PI_C) * HALF;
                s += dql * (-PI_C) * HALF;
                s += (dqh + dql) * (-PI_D) * HALF;
            } else {
                let (ddidd, ddii) = rempi(d);
                ql = ddii;
                s = ddidd;
                let o = d.is_infinite() | d.is_nan();
                s.0 = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(s.0));
                s.1 = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(s.1));
            }

            let t = s;

            s.0 = s.square_as_f();

            let u = F64x::splat(1.589_383_072_832_289_373_285_11_e-10)
                .mul_add(s.0, F64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
                .mul_add(s.0, F64x::splat(2.755_731_317_768_463_605_125_47_e-6))
                .mul_add(s.0, F64x::splat(-0.000_198_412_698_278_911_770_864_914))
                .mul_add(s.0, F64x::splat(0.008_333_333_333_319_184_596_174_6))
                .mul_add(s.0, F64x::splat(-0.166_666_666_666_666_130_709_393))
                * (s.0 * t.0);

            let x = t.add_checked(u);
            let rx = x.0 + x.1;

            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
                .mul_add(s.0, F64x::splat(2.087_574_712_070_400_554_793_66_e-9))
                .mul_add(s.0, F64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
                .mul_add(s.0, F64x::splat(2.480_158_728_900_018_673_119_15_e-5))
                .mul_add(s.0, F64x::splat(-0.001_388_888_888_887_140_192_823_29))
                .mul_add(s.0, F64x::splat(0.041_666_666_666_666_551_959_206_2))
                .mul_add(s.0, F64x::splat(-0.5));

            let x = ONE.add_checked(s.0.mul_as_doubled(u));
            let ry = x.0 + x.1;

            let o = M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(0)));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = M64x::from_cast((ql & Ix::splat(2)).eq(Ix::splat(2)));
            rsin = F64x::from_bits(
                (U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rsin),
            );

            let o = M64x::from_cast(((ql + Ix::splat(1)) & Ix::splat(2)).eq(Ix::splat(2)));
            rcos = F64x::from_bits(
                (U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rcos),
            );

            (rsin, rcos)
        }

        #[cfg(feature = "deterministic")]
        pub fn sincos(d: F64x) -> (F64x, F64x) {
            let dql = (d * F64x::FRAC_2_PI).round();
            let mut ql = dql.roundi();
            let u = dql.mul_add(-PI_A2 * HALF, d);
            let mut s = u.add_checked_as_doubled(dql * (-PI_B2 * HALF));
            let g = d.abs().lt(TRIGRANGEMAX2);

            if !g.all() {
                let mut dqh = (d * F64x::FRAC_2_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let dql = (d * F64x::FRAC_2_PI - dqh).round();

                let u = dqh.mul_add(-PI_A * HALF, d);
                let mut x = u.add_checked_as_doubled(dql * (-PI_A * HALF));
                x += dqh * (-PI_B * HALF);
                x += dql * (-PI_B * HALF);
                x += dqh * (-PI_C * HALF);
                x += dql * (-PI_C * HALF);
                x = x.add_checked((dqh + dql) * (-PI_D * HALF));

                ql = Mx::from_cast(g).select(ql, dql.roundi());
                s = g.select_doubled(s, x);
                g = d.abs().lt(TRIGRANGEMAX);

                if !g.all() {
                    let (ddidd, ddii) = rempi(d);
                    x = ddidd;
                    let o = d.is_infinite() | d.is_nan();
                    x.0 = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(x.0));
                    x.1 = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(x.1));

                    ql = Mx::from_cast(g).select(ql, ddii);
                    s = g.select_doubled(s, x);
                }
            }

            let t = s;

            s.0 = s.square_as_f();

            let u = F64x::splat(1.589_383_072_832_289_373_285_11_e-10)
                .mul_add(s.0, F64x::splat(-2.505_069_435_025_397_733_493_18_e-8))
                .mul_add(s.0, F64x::splat(2.755_731_317_768_463_605_125_47_e-6))
                .mul_add(s.0, F64x::splat(-0.000_198_412_698_278_911_770_864_914))
                .mul_add(s.0, F64x::splat(0.008_333_333_333_319_184_596_174_6))
                .mul_add(s.0, F64x::splat(-0.166_666_666_666_666_130_709_393))
                * (s.0 * t.0);

            let x = t.add_checked(u);
            let mut rx = x.0 + x.1;

            rx = d.is_neg_zero().select(NEG_ZERO, rx);

            let u = F64x::splat(-1.136_153_502_390_974_295_315_23_e-11)
                .mul_add(s.0, F64x::splat(2.087_574_712_070_400_554_793_66_e-9))
                .mul_add(s.0, F64x::splat(-2.755_731_440_288_475_674_985_67_e-7))
                .mul_add(s.0, F64x::splat(2.480_158_728_900_018_673_119_15_e-5))
                .mul_add(s.0, F64x::splat(-0.001_388_888_888_887_140_192_823_29))
                .mul_add(s.0, F64x::splat(0.041_666_666_666_666_551_959_206_2))
                .mul_add(s.0, F64x::splat(-0.5));

            let x = ONE.add_checked(s.0.mul_as_doubled(u));
            let ry = x.0 + x.1;

            let o = M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(0)));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = M64x::from_cast((ql & Ix::splat(2)).eq(Ix::splat(2)));
            rsin = F64x::from_bits(
                (U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rsin),
            );

            let o = M64x::from_cast(((ql + Ix::splat(1)) & Ix::splat(2)).eq(Ix::splat(2)));
            rcos = F64x::from_bits(
                (U64x::from_bits(o) & U64x::from_bits(NEG_ZERO)) ^ U64x::from_bits(rcos),
            );

            (rsin, rcos)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn tan(d: F64x) -> F64x {
            let mut s;
            let ql;

            if d.abs().lt(TRIGRANGEMAX2).all() {
                let dql = (d * F64x::FRAC_2_PI).round();
                ql = dql.roundi();
                let u = dql.mul_add(-PI_A2 * HALF, d);
                s = u.add_checked_as_doubled(dql * (-PI_B2) * HALF);
            } else if d.abs().lt(TRIGRANGEMAX).all() {
                let dqh = (d * (F64x::FRAC_2_PI / D1_24X)).trunc();
                let dqh = dqh * D1_24X;
                s = Doubled::new(M_2_PI_H, M_2_PI_L) * d
                    + (d.lt(ZERO).select(F64x::splat(-0.5), HALF) - dqh);
                let dql = (s.0 + s.1).trunc();
                ql = dql.roundi();

                let u = dqh.mul_add(-PI_A * HALF, d);
                s = u.add_checked_as_doubled(dql * (-PI_A) * HALF);
                s += dqh * (-PI_B) * HALF;
                s += dql * (-PI_B) * HALF;
                s += dqh * (-PI_C) * HALF;
                s += dql * (-PI_C) * HALF;
                s += (dqh + dql) * (-PI_D) * HALF;
            } else {
                let (ddidd, ddii) = rempi(d);
                ql = ddii;
                s = ddidd;
                let o = d.is_infinite() | d.is_nan();
                s.0 = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(s.0));
                s.1 = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(s.1));
            }

            let o = M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(1)));
            let n = U64x::from_bits(o) & U64x::from_bits(NEG_ZERO);
            s.0 = F64x::from_bits(U64x::from_bits(s.0) ^ n);
            s.1 = F64x::from_bits(U64x::from_bits(s.1) ^ n);

            let t = s;
            s = s.square();

            let u = if cfg!(feature = "split_kernel") {
                let sx2 = s.0 * s.0;

                let u = F64x::splat(-2.595_197_915_859_246_976_986_14_e-5)
                    .mul_add(sx2, F64x::splat(-3.050_330_144_339_464_882_256_16_e-5))
                    .mul_add(sx2, F64x::splat(8.096_745_182_801_591_870_450_78_e-5))
                    .mul_add(sx2, F64x::splat(0.000_588_505_168_743_587_154_904_506))
                    .mul_add(sx2, F64x::splat(0.003_592_087_438_369_066_191_429_24))
                    .mul_add(sx2, F64x::splat(0.021_869_488_285_384_638_959_207_8))
                    .mul_add(sx2, F64x::splat(0.133_333_333_333_125_941_821_962));

                let v = F64x::splat(1.014_197_185_110_833_732_244_08_e-5)
                    .mul_add(sx2, F64x::splat(5.233_880_819_158_998_553_251_86_e-5))
                    .mul_add(sx2, F64x::splat(7.147_075_040_842_427_442_674_97_e-5))
                    .mul_add(sx2, F64x::splat(0.000_244_884_931_879_331_847_054_404))
                    .mul_add(sx2, F64x::splat(0.001_456_127_889_228_124_279_788_48))
                    .mul_add(sx2, F64x::splat(0.008_863_239_443_624_016_181_133_56))
                    .mul_add(sx2, F64x::splat(0.053_968_253_978_129_841_763_600_2));

                v.mul_add(s.0, u)
            } else {
                F64x::splat(1.014_197_185_110_833_732_244_08_e-5)
                    .mul_add(s.0, F64x::splat(-2.595_197_915_859_246_976_986_14_e-5))
                    .mul_add(s.0, F64x::splat(5.233_880_819_158_998_553_251_86_e-5))
                    .mul_add(s.0, F64x::splat(-3.050_330_144_339_464_882_256_16_e-5))
                    .mul_add(s.0, F64x::splat(7.147_075_040_842_427_442_674_97_e-5))
                    .mul_add(s.0, F64x::splat(8.096_745_182_801_591_870_450_78_e-5))
                    .mul_add(s.0, F64x::splat(0.000_244_884_931_879_331_847_054_404))
                    .mul_add(s.0, F64x::splat(0.000_588_505_168_743_587_154_904_506))
                    .mul_add(s.0, F64x::splat(0.001_456_127_889_228_124_279_788_48))
                    .mul_add(s.0, F64x::splat(0.003_592_087_438_369_066_191_429_24))
                    .mul_add(s.0, F64x::splat(0.008_863_239_443_624_016_181_133_56))
                    .mul_add(s.0, F64x::splat(0.021_869_488_285_384_638_959_207_8))
                    .mul_add(s.0, F64x::splat(0.053_968_253_978_129_841_763_600_2))
                    .mul_add(s.0, F64x::splat(0.133_333_333_333_125_941_821_962))
            };

            let mut x = ONE.add_checked(
                F64x::splat(0.333_333_333_333_334_980_164_153).add_checked_as_doubled(u * s.0) * s,
            );
            x = t * x;

            x = o.select_doubled(x.recpre(), x);

            let u = x.0 + x.1;

            d.eq(ZERO).select(d, u)
        }

        #[cfg(feature = "deterministic")]
        pub fn tan(d: F64x) -> F64x {
            let dql = (d * F64x::FRAC_2_PI).round();
            let mut ql = dql.roundi();
            let u = dql.mul_add(-PI_A2 * HALF, d);
            let mut s = u.add_checked_as_doubled(dql * (-PI_B2 * HALF));
            let g = d.abs().lt(TRIGRANGEMAX2);

            if !g.all() {
                let mut dqh = (d * F64x::FRAC_2_PI / D1_24X).trunc();
                dqh *= D1_24X;
                let mut x = Doubled::new(M_2_PI_H, M_2_PI_L) * d
                    + (d.lt(ZERO).select(F64x::splat(-0.5), HALF) - dqh);
                let dql = (x.0 + x.1).trunc();

                let u = dqh.mul_add(-PI_A * HALF, d);
                x = u.add_checked_as_doubled(dql * (-PI_A * HALF));
                x += dqh * (-PI_B * HALF);
                x += dql * (-PI_B * HALF);
                x += dqh * (-PI_C * HALF);
                x += dql * (-PI_C * HALF);
                x = x.add_checked((dqh + dql) * (-PI_D * HALF));

                ql = Mx::from_cast(g).select(ql, dql.roundi());
                s = g.select_doubled(s, x);
                g = d.abs().lt(TRIGRANGEMAX);

                if !g.all() {
                    let (ddidd, ddii) = rempi(d);
                    x = ddidd;
                    let o = d.is_infinite() | d.is_nan();
                    x.0 = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(x.0));
                    x.1 = F64x::from_bits(U64x::from_bits(o) | U64x::from_bits(x.1));

                    ql = Mx::from_cast(g).select(ql, ddii);
                    s = g.select_doubled(s, x);
                }
            }

            let o = M64x::from_cast((ql & Ix::splat(1)).eq(Ix::splat(1)));
            let n = U64x::from_bits(o) & U64x::from_bits(NEG_ZERO);
            s.0 = F64x::from_bits(U64x::from_bits(s.0) ^ n);
            s.1 = F64x::from_bits(U64x::from_bits(s.1) ^ n);

            let t = s;
            s = s.square();

            let mut u = if cfg!(feature = "split_kernel") {
                let sx2 = s.0 * s.0;

                let u = F64x::splat(-2.595_197_915_859_246_976_986_14_e-5)
                    .mul_add(sx2, F64x::splat(-3.050_330_144_339_464_882_256_16_e-5))
                    .mul_add(sx2, F64x::splat(8.096_745_182_801_591_870_450_78_e-5))
                    .mul_add(sx2, F64x::splat(0.000_588_505_168_743_587_154_904_506))
                    .mul_add(sx2, F64x::splat(0.003_592_087_438_369_066_191_429_24))
                    .mul_add(sx2, F64x::splat(0.021_869_488_285_384_638_959_207_8))
                    .mul_add(sx2, F64x::splat(0.133_333_333_333_125_941_821_962));

                let v = F64x::splat(1.014_197_185_110_833_732_244_08_e-5)
                    .mul_add(sx2, F64x::splat(5.233_880_819_158_998_553_251_86_e-5))
                    .mul_add(sx2, F64x::splat(7.147_075_040_842_427_442_674_97_e-5))
                    .mul_add(sx2, F64x::splat(0.000_244_884_931_879_331_847_054_404))
                    .mul_add(sx2, F64x::splat(0.001_456_127_889_228_124_279_788_48))
                    .mul_add(sx2, F64x::splat(0.008_863_239_443_624_016_181_133_56))
                    .mul_add(sx2, F64x::splat(0.053_968_253_978_129_841_763_600_2));

                v.mul_add(s.0, u)
            } else {
                F64x::splat(1.014_197_185_110_833_732_244_08_e-5)
                    .mul_add(s.0, F64x::splat(-2.595_197_915_859_246_976_986_14_e-5))
                    .mul_add(s.0, F64x::splat(5.233_880_819_158_998_553_251_86_e-5))
                    .mul_add(s.0, F64x::splat(-3.050_330_144_339_464_882_256_16_e-5))
                    .mul_add(s.0, F64x::splat(7.147_075_040_842_427_442_674_97_e-5))
                    .mul_add(s.0, F64x::splat(8.096_745_182_801_591_870_450_78_e-5))
                    .mul_add(s.0, F64x::splat(0.000_244_884_931_879_331_847_054_404))
                    .mul_add(s.0, F64x::splat(0.000_588_505_168_743_587_154_904_506))
                    .mul_add(s.0, F64x::splat(0.001_456_127_889_228_124_279_788_48))
                    .mul_add(s.0, F64x::splat(0.003_592_087_438_369_066_191_429_24))
                    .mul_add(s.0, F64x::splat(0.008_863_239_443_624_016_181_133_56))
                    .mul_add(s.0, F64x::splat(0.021_869_488_285_384_638_959_207_8))
                    .mul_add(s.0, F64x::splat(0.053_968_253_978_129_841_763_600_2))
                    .mul_add(s.0, F64x::splat(0.133_333_333_333_125_941_821_962))
            };

            let mut x = ONE
                .add_checked(F64x::splat(0.333333333333334980164153).add_checked_as_doubled(u * s.0) * s);
            x = t * x;

            x = o.select_doubled(x.recpre(), x);

            u = x.0 + x.1;

            d.eq(ZERO).select(d, u)
        }

        #[inline]
        fn atan2k_u1(y: Doubled<F64x>, mut x: Doubled<F64x>) -> Doubled<F64x> {
            let q = vsel_vi_vd_vi(x.0, Ix::splat(-2));
            let p = x.0.lt(ZERO);
            let b = U64x::from_bits(p) & U64x::from_bits(NEG_ZERO);
            x.0 = F64x::from_bits(b ^ U64x::from_bits(x.0));
            x.1 = F64x::from_bits(b ^ U64x::from_bits(x.1));

            let q = vsel_vi_vd_vd_vi_vi(x.0, y.0, q + Ix::splat(1), q);
            let p = x.0.lt(y.0);
            let s = p.select_doubled(-x, y);
            let mut t = p.select_doubled(y, x);

            let s = s / t;
            t = s.square();
            t = t.normalize();

            let mut u: F64x;
            if cfg!(feature = "split_kernel") {
                let tx3 = t.0 * t.0 * t.0;

                u = F64x::splat(0.000_705_576_642_963_934_123_897_74)
                    .mul_add(t.0, F64x::splat(-0.002_518_656_144_987_133_603_529_99))
                    .mul_add(tx3, F64x::splat(0.020_802_479_992_414_579_790_249_7))
                    .mul_add(t.0, F64x::splat(-0.028_900_234_478_474_031_568_628_9))
                    .mul_add(tx3, F64x::splat(0.047_084_301_165_328_398_819_376_3))
                    .mul_add(t.0, F64x::splat(-0.052_491_421_058_844_842_106_871_9))
                    .mul_add(tx3, F64x::splat(0.076_922_533_029_620_376_865_409_5))
                    .mul_add(t.0, F64x::splat(-0.090_909_044_277_338_757_478_190_7))
                    .mul_add(tx3, F64x::splat(0.199_999_999_997_977_351_284_817))
                    .mul_add(t.0, F64x::splat(-0.333_333_333_333_317_605_173_818));

                let v = F64x::splat(1.062_984_841_914_487_466_074_15_e-5)
                    .mul_add(t.0, F64x::splat(-0.000_125_620_649_967_286_867_384_336))
                    .mul_add(tx3, F64x::splat(0.006_462_628_990_369_911_723_135_04))
                    .mul_add(t.0, F64x::splat(-0.012_828_133_366_339_903_101_427_4))
                    .mul_add(tx3, F64x::splat(0.035_978_500_503_510_459_085_365_6))
                    .mul_add(t.0, F64x::splat(-0.041_848_579_703_592_507_506_027))
                    .mul_add(tx3, F64x::splat(0.058_794_659_096_958_100_386_043_4))
                    .mul_add(t.0, F64x::splat(-0.066_662_088_477_879_549_719_418_2))
                    .mul_add(tx3, F64x::splat(0.111_111_108_376_896_236_538_123))
                    .mul_add(t.0, F64x::splat(-0.142_857_142_756_268_568_062_339));

                u = v.mul_add(t.0 * t.0, u);
            } else {
                u = F64x::splat(1.062_984_841_914_487_466_074_15_e-5)
                    .mul_add(t.0, F64x::splat(-0.000_125_620_649_967_286_867_384_336))
                    .mul_add(t.0, F64x::splat(0.000_705_576_642_963_934_123_897_74))
                    .mul_add(t.0, F64x::splat(-0.002_518_656_144_987_133_603_529_99))
                    .mul_add(t.0, F64x::splat(0.006_462_628_990_369_911_723_135_04))
                    .mul_add(t.0, F64x::splat(-0.012_828_133_366_339_903_101_427_4))
                    .mul_add(t.0, F64x::splat(0.020_802_479_992_414_579_790_249_7))
                    .mul_add(t.0, F64x::splat(-0.028_900_234_478_474_031_568_628_9))
                    .mul_add(t.0, F64x::splat(0.035_978_500_503_510_459_085_365_6))
                    .mul_add(t.0, F64x::splat(-0.041_848_579_703_592_507_506_027))
                    .mul_add(t.0, F64x::splat(0.047_084_301_165_328_398_819_376_3))
                    .mul_add(t.0, F64x::splat(-0.052_491_421_058_844_842_106_871_9))
                    .mul_add(t.0, F64x::splat(0.058_794_659_096_958_100_386_043_4))
                    .mul_add(t.0, F64x::splat(-0.066_662_088_477_879_549_719_418_2))
                    .mul_add(t.0, F64x::splat(0.076_922_533_029_620_376_865_409_5))
                    .mul_add(t.0, F64x::splat(-0.090_909_044_277_338_757_478_190_7))
                    .mul_add(t.0, F64x::splat(0.111_111_108_376_896_236_538_123))
                    .mul_add(t.0, F64x::splat(-0.142_857_142_756_268_568_062_339))
                    .mul_add(t.0, F64x::splat(0.199_999_999_997_977_351_284_817))
                    .mul_add(t.0, F64x::splat(-0.333_333_333_333_317_605_173_818));
            }

            t *= u;
            t = s * ONE.add_checked(t);
            (Doubled::from((
                1.570_796_326_794_896_557_998_982,
                6.123_233_995_736_766_035_868_82_e-17,
            )) * F64x::from_cast(q))
            .add_checked(t)
        }

        pub fn atan2(y: F64x, x: F64x) -> F64x {
            let o = x.abs().lt(F64x::splat(5.562_684_646_268_008_398_4_e-309)); // nexttoward((1.0 / DBL_MAX), 1)
            let x = o.select(x * D1_53X, x);
            let y = o.select(y * D1_23X, y);

            let d = atan2k_u1(Doubled::new(y.abs(), ZERO), Doubled::new(x, ZERO));
            let mut r = d.0 + d.1;

            r = r.mul_sign(x);
            r = (x.is_infinite() | x.eq(ZERO)).select(
                F64x::FRAC_PI_2 - visinf2_vd_vd_vd(x, F64x::FRAC_PI_2.mul_sign(x)),
                r,
            );
            r = y.is_infinite().select(
                F64x::FRAC_PI_2 - visinf2_vd_vd_vd(x, F64x::FRAC_PI_4.mul_sign(x)),
                r,
            );
            r = y.eq(ZERO).select(
                F64x::from_bits(U64x::from_bits(x.is_sign_negative()) & U64x::from_bits(F64x::PI)),
                r,
            );

            F64x::from_bits(U64x::from_bits(x.is_nan() | y.is_nan()) | U64x::from_bits(r.mul_sign(y)))
        }

        pub fn asin(d: F64x) -> F64x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
            x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

            let mut u;

            if cfg!(feature = "split_kernel") {
                let x4 = x2 * x2;

                u = F64x::splat(-0.158_191_824_332_999_664_3_e-1)
                    .mul_add(x4, F64x::splat(0.660_607_747_627_717_061_e-2))
                    .mul_add(x4, F64x::splat(0.138_871_518_450_160_921_8_e-1))
                    .mul_add(x4, F64x::splat(0.223_717_618_193_204_834_1_e-1))
                    .mul_add(x4, F64x::splat(0.446_428_568_137_710_243_8_e-1))
                    .mul_add(x4, F64x::splat(0.166_666_666_666_649_754_3));

                let v = F64x::splat(0.316_158_765_065_393_462_8_e-1)
                    .mul_add(x4, F64x::splat(0.192_904_547_726_791_067_4_e-1))
                    .mul_add(x4, F64x::splat(0.121_536_052_557_737_733_1_e-1))
                    .mul_add(x4, F64x::splat(0.173_595_699_122_361_460_4_e-1))
                    .mul_add(x4, F64x::splat(0.303_819_592_803_813_223_7_e-1))
                    .mul_add(x4, F64x::splat(0.750_000_000_037_858_161_1_e-1));

                u = v.mul_add(x2, u);
            } else {
                u = F64x::splat(0.316_158_765_065_393_462_8_e-1)
                    .mul_add(x2, F64x::splat(-0.158_191_824_332_999_664_3_e-1))
                    .mul_add(x2, F64x::splat(0.192_904_547_726_791_067_4_e-1))
                    .mul_add(x2, F64x::splat(0.660_607_747_627_717_061_e-2))
                    .mul_add(x2, F64x::splat(0.121_536_052_557_737_733_1_e-1))
                    .mul_add(x2, F64x::splat(0.138_871_518_450_160_921_8_e-1))
                    .mul_add(x2, F64x::splat(0.173_595_699_122_361_460_4_e-1))
                    .mul_add(x2, F64x::splat(0.223_717_618_193_204_834_1_e-1))
                    .mul_add(x2, F64x::splat(0.303_819_592_803_813_223_7_e-1))
                    .mul_add(x2, F64x::splat(0.446_428_568_137_710_243_8_e-1))
                    .mul_add(x2, F64x::splat(0.750_000_000_037_858_161_1_e-1))
                    .mul_add(x2, F64x::splat(0.166_666_666_666_649_754_3));
            }

            u *= x2 * x.0;

            let y = Doubled::from((
                3.141_592_653_589_793_116 / 4.,
                1.224_646_799_147_353_207_2_e-16 / 4.,
            ))
            .sub_checked(x)
            .sub_checked(u);

            let r = o.select(u + x.0, (y.0 + y.1) * F64x::splat(2.));
            r.mul_sign(d)
        }

        pub fn acos(d: F64x) -> F64x {
            let o = d.abs().lt(HALF);
            let x2 = o.select(d * d, (ONE - d.abs()) * HALF);
            let mut x = o.select_doubled(Doubled::new(d.abs(), ZERO), x2.sqrt_as_doubled());
            x = d.abs().eq(ONE).select_doubled(Doubled::from((0., 0.)), x);

            let mut u: F64x;
            if cfg!(feature = "split_kernel") {
                let x4 = x2 * x2;

                u = F64x::splat(-0.158_191_824_332_999_664_3_e-1)
                    .mul_add(x4, F64x::splat(0.660_607_747_627_717_061_e-2))
                    .mul_add(x4, F64x::splat(0.138_871_518_450_160_921_8_e-1))
                    .mul_add(x4, F64x::splat(0.223_717_618_193_204_834_1_e-1))
                    .mul_add(x4, F64x::splat(0.446_428_568_137_710_243_8_e-1))
                    .mul_add(x4, F64x::splat(0.166_666_666_666_649_754_3));

                let v = F64x::splat(0.316_158_765_065_393_462_8_e-1)
                    .mul_add(x4, F64x::splat(0.192_904_547_726_791_067_4_e-1))
                    .mul_add(x4, F64x::splat(0.121_536_052_557_737_733_1_e-1))
                    .mul_add(x4, F64x::splat(0.173_595_699_122_361_460_4_e-1))
                    .mul_add(x4, F64x::splat(0.303_819_592_803_813_223_7_e-1))
                    .mul_add(x4, F64x::splat(0.750_000_000_037_858_161_1_e-1));

                u = v.mul_add(x2, u);
            } else {
                u = F64x::splat(0.316_158_765_065_393_462_8_e-1)
                    .mul_add(x2, F64x::splat(-0.158_191_824_332_999_664_3_e-1))
                    .mul_add(x2, F64x::splat(0.192_904_547_726_791_067_4_e-1))
                    .mul_add(x2, F64x::splat(0.660_607_747_627_717_061_e-2))
                    .mul_add(x2, F64x::splat(0.121_536_052_557_737_733_1_e-1))
                    .mul_add(x2, F64x::splat(0.138_871_518_450_160_921_8_e-1))
                    .mul_add(x2, F64x::splat(0.173_595_699_122_361_460_4_e-1))
                    .mul_add(x2, F64x::splat(0.223_717_618_193_204_834_1_e-1))
                    .mul_add(x2, F64x::splat(0.303_819_592_803_813_223_7_e-1))
                    .mul_add(x2, F64x::splat(0.446_428_568_137_710_243_8_e-1))
                    .mul_add(x2, F64x::splat(0.750_000_000_037_858_161_1_e-1))
                    .mul_add(x2, F64x::splat(0.166_666_666_666_649_754_3));
            }

            u *= x2 * x.0;

            let mut y = Doubled::from((
                3.141_592_653_589_793_116 / 2.,
                1.224_646_799_147_353_207_2_e-16 / 2.,
            ))
            .sub_checked(x.0.mul_sign(d).add_checked_as_doubled(u.mul_sign(d)));
            x = x.add_checked(u);

            y = o.select_doubled(y, x.scale(F64x::splat(2.)));

            y = (!o & d.lt(ZERO)).select_doubled(
                Doubled::from((3.141_592_653_589_793_116, 1.224_646_799_147_353_207_2_e-16)).sub_checked(y),
                y,
            );

            y.0 + y.1
        }

        pub fn atan(d: F64x) -> F64x {
            let d2 = atan2k_u1(Doubled::new(d.abs(), ZERO), Doubled::from((1., 0.)));
            let mut r = d2.0 + d2.1;
            r = d
                .is_infinite()
                .select(F64x::splat(1.570_796_326_794_896_557_998_982), r);
            r.mul_sign(d)
        }

        pub fn exp(d: F64x) -> F64x {
            let mut u = (d * R_LN2).round();
            let q = u.roundi();

            let s = u.mul_add(-L2U, d);
            let s = u.mul_add(-L2L, s);

            if cfg!(target_feature = "fma") {
                if cfg!(feature = "split_kernel") {
                    let s2 = s * s;

                    u = F64x::splat(0.208_127_637_823_716_445_7_e-8)
                        .mul_adde(s2, F64x::splat(0.275_576_262_816_949_119_2_e-6))
                        .mul_adde(s2, F64x::splat(0.248_015_868_747_968_626_4_e-4))
                        .mul_adde(s2, F64x::splat(0.138_888_888_891_449_779_7_e-2))
                        .mul_adde(s2, F64x::splat(0.416_666_666_666_660_259_8_e-1))
                        .mul_adde(s2, HALF);

                    let v = F64x::splat(0.251_121_070_304_228_802_2_e-7)
                        .mul_adde(s2, F64x::splat(0.275_572_340_202_538_823_9_e-5))
                        .mul_adde(s2, F64x::splat(0.198_412_698_985_586_585_e-3))
                        .mul_adde(s2, F64x::splat(0.833_333_333_331_493_821_e-2))
                        .mul_adde(s2, F64x::splat(0.166_666_666_666_666_907_2));

                    u = v.mul_add(s, u).mul_adde(s, ONE).mul_adde(s, ONE);
                } else {
                    u = F64x::splat(0.208_127_637_823_716_445_7_e-8)
                        .mul_adde(s, F64x::splat(0.251_121_070_304_228_802_2_e-7))
                        .mul_adde(s, F64x::splat(0.275_576_262_816_949_119_2_e-6))
                        .mul_adde(s, F64x::splat(0.275_572_340_202_538_823_9_e-5))
                        .mul_adde(s, F64x::splat(0.248_015_868_747_968_626_4_e-4))
                        .mul_adde(s, F64x::splat(0.198_412_698_985_586_585_e-3))
                        .mul_adde(s, F64x::splat(0.138_888_888_891_449_779_7_e-2))
                        .mul_adde(s, F64x::splat(0.833_333_333_331_493_821_e-2))
                        .mul_adde(s, F64x::splat(0.416_666_666_666_660_259_8_e-1))
                        .mul_adde(s, F64x::splat(0.166_666_666_666_666_907_2))
                        .mul_adde(s, HALF)
                        .mul_adde(s, ONE)
                        .mul_adde(s, ONE);
                }
            } else {
                u = F64x::splat(2.088_606_211_072_836_875_363_41_e-9)
                    .mul_add(s, F64x::splat(2.511_129_308_928_765_186_106_61_e-8))
                    .mul_add(s, F64x::splat(2.755_739_112_349_004_718_933_38_e-7))
                    .mul_add(s, F64x::splat(2.755_723_629_119_288_276_294_23_e-6))
                    .mul_add(s, F64x::splat(2.480_158_715_923_547_299_879_1_e-5))
                    .mul_add(s, F64x::splat(0.000_198_412_698_960_509_205_564_975))
                    .mul_add(s, F64x::splat(0.001_388_888_888_897_744_922_079_62))
                    .mul_add(s, F64x::splat(0.008_333_333_333_316_527_216_649_84))
                    .mul_add(s, F64x::splat(0.041_666_666_666_666_504_759_142_2))
                    .mul_add(s, F64x::splat(0.166_666_666_666_666_851_703_837))
                    .mul_add(s, HALF);

                u = ONE + (s * s).mul_add(u, s);
            }

            u = vldexp2_vd_vd_vi(u, q);

            u = d
                .gt(F64x::splat(709.782_711_149_557_429_092_172_174_26))
                .select(F64x::INFINITY, u);
            F64x::from_bits(!U64x::from_bits(d.lt(F64x::splat(-1000.))) & U64x::from_bits(u))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn log(mut d: F64x) -> F64x {
            let m: F64x;
            let mut s =
                        /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                            let o = d.lt(F64x::splat(f64::MIN));
                            d = o.select(d * (D1_32X * D1_32X), d);
                            let mut e = vilogb2k_vi_vd(d * F64x::splat(1. / 0.75));
                            m = vldexp3_vd_vd_vi(d, -e);
                            e = Mx::from_cast(o).select(e - Ix::splat(64), e);
                            Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17))
                                * F64x::from_cast(e)
                        }/* else {
                            let mut e = vgetexp_vd_vd(d * F64x::splat(1. / 0.75));
                            e = e.eq(F64x::INFINITY).select(F64x::splat(1024.), e);
                            m = vgetmant_vd_vd(d);
                            Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17)) * e
                        }*/;

            let x = F64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F64x::splat(0.153_207_698_850_270_135_3)
                .mul_add(x2, F64x::splat(0.152_562_905_100_342_871_6))
                .mul_add(x2, F64x::splat(0.181_860_593_293_778_599_6))
                .mul_add(x2, F64x::splat(0.222_221_451_983_938_000_9))
                .mul_add(x2, F64x::splat(0.285_714_293_279_429_931_7))
                .mul_add(x2, F64x::splat(0.399_999_999_963_525_199))
                .mul_add(x2, F64x::splat(0.666_666_666_666_733_354_1));

            s = s.add_checked(x.scale(F64x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            let r = d.eq(F64x::INFINITY).select(F64x::INFINITY, r);
            let r = (d.lt(ZERO) | d.is_nan()).select(F64x::NAN, r);
            d.eq(ZERO).select(F64x::NEG_INFINITY, r)
            /*} else {
                vfixup_vd_vd_vd_vi2_i(
                    r,
                    d,
                    I64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn pow(x: F64x, y: F64x) -> F64x {
            if true {
                let yisint = y.is_integer();
                let yisodd = visodd_vo_vd(y) & yisint;

                let d = logk(x.abs()) * y;
                let mut result = expk(d);
                result =
                    d.0.gt(F64x::splat(709.782_711_149_557_429_092_172_174_26))
                        .select(F64x::INFINITY, result);

                result *= x.gt(ZERO).select(
                    ONE,
                    yisint.select(yisodd.select(F64x::splat(-1.), ONE), F64x::NAN),
                );

                let efx = (x.abs() - ONE).mul_sign(y);

                result = y.is_infinite().select(
                    F64x::from_bits(
                        !U64x::from_bits(efx.lt(ZERO))
                            & U64x::from_bits(efx.eq(ZERO).select(ONE, F64x::INFINITY)),
                    ),
                    result,
                );

                result = (x.is_infinite() | x.eq(ZERO)).select(
                    yisodd.select(x.sign(), ONE)
                        * F64x::from_bits(
                            !U64x::from_bits(x.eq(ZERO).select(-y, y).lt(ZERO))
                                & U64x::from_bits(F64x::INFINITY),
                        ),
                    result,
                );

                result =
                    F64x::from_bits(U64x::from_bits(x.is_nan() | y.is_nan()) | U64x::from_bits(result));

                (y.eq(ZERO) | x.eq(ONE)).select(ONE, result)
            } else {
                expk(logk(x) * y)
            }
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn sinh(x: F64x) -> F64x {
            let mut y = x.abs();
            let mut d = expk2(Doubled::new(y, ZERO));
            d = d.sub_checked(d.recpre());
            y = (d.0 + d.1) * HALF;

            y = (x.abs().gt(F64x::splat(710.)) | y.is_nan()).select(F64x::INFINITY, y);
            y = y.mul_sign(x);
            F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn cosh(x: F64x) -> F64x {
            let mut y = x.abs();
            let mut d = expk2(Doubled::new(y, ZERO));
            d = d.add_checked(d.recpre());
            y = (d.0 + d.1) * HALF;

            y = (x.abs().gt(F64x::splat(710.)) | y.is_nan()).select(F64x::INFINITY, y);
            F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn tanh(x: F64x) -> F64x {
            let mut y = x.abs();
            let mut d = expk2(Doubled::new(y, ZERO));
            let e = d.recpre();
            d = (d + (-e)) / (d + e);
            y = d.0 + d.1;

            y = (x.abs().gt(F64x::splat(18.714_973_875)) | y.is_nan()).select(ONE, y);
            y = y.mul_sign(x);
            F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn asinh(x: F64x) -> F64x {
            let mut y = x.abs();
            let o = y.gt(ONE);

            let mut d = o.select_doubled(x.recpre_as_doubled(), Doubled::new(y, ZERO));
            d = (d.square() + ONE).sqrt();
            d = o.select_doubled(d * y, d);

            d = logk2((d + x).normalize());
            y = d.0 + d.1;

            y = (x.abs().gt(SQRT_DBL_MAX) | y.is_nan()).select(F64x::INFINITY.mul_sign(x), y);

            y = F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y));
            x.is_neg_zero().select(NEG_ZERO, y)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn acosh(x: F64x) -> F64x {
            let d = logk2(x.add_as_doubled(ONE).sqrt() * x.add_as_doubled(F64x::splat(-1.)).sqrt() + x);
            let mut y = d.0 + d.1;

            y = (x.abs().gt(SQRT_DBL_MAX) | y.is_nan()).select(F64x::INFINITY, y);
            y = F64x::from_bits(!U64x::from_bits(x.eq(ONE)) & U64x::from_bits(y));

            y = F64x::from_bits(U64x::from_bits(x.lt(ONE)) | U64x::from_bits(y));
            F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn atanh(x: F64x) -> F64x {
            let mut y = x.abs();
            let d = logk2(ONE.add_as_doubled(y) / ONE.add_as_doubled(-y));
            y = F64x::from_bits(
                U64x::from_bits(y.gt(ONE))
                    | U64x::from_bits(y.eq(ONE).select(F64x::INFINITY, (d.0 + d.1) * HALF)),
            );

            y = y.mul_sign(x);
            y = F64x::from_bits(U64x::from_bits(x.is_infinite() | y.is_nan()) | U64x::from_bits(y));
            F64x::from_bits(U64x::from_bits(x.is_nan()) | U64x::from_bits(y))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn cbrt(mut d: F64x) -> F64x {
            let mut q2 = Doubled::from((1., 0.));

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                let s = d;
            }*/
            let e = vilogbk_vi_vd(d.abs()) + Ix::splat(1);
            d = vldexp2_vd_vd_vi(d, -e);

            let t = F64x::from_cast(e) + F64x::splat(6144.);
            let qu = (t * F64x::splat(1. / 3.)).trunci();
            let re = (t - F64x::from_cast(qu) * F64x::splat(3.)).trunci();

            q2 = M64x::from_cast(re.eq(Ix::splat(1))).select_doubled(
                Doubled::from((
                    1.259_921_049_894_873_190_7,
                    -2.589_933_375_300_506_917_7_e-17,
                )),
                q2,
            );
            q2 = M64x::from_cast(re.eq(Ix::splat(2))).select_doubled(
                Doubled::from((
                    1.587_401_051_968_199_583_4,
                    -1.086_900_819_419_782_298_6_e-16,
                )),
                q2,
            );

            q2.0 = q2.0.mul_sign(d);
            q2.1 = q2.1.mul_sign(d);
            d = d.abs();

            let mut x = F64x::splat(-0.640_245_898_480_692_909_870_982)
                .mul_add(d, F64x::splat(2.961_551_030_200_395_118_185_95))
                .mul_add(d, F64x::splat(-5.733_530_609_229_478_436_361_66))
                .mul_add(d, F64x::splat(6.039_903_689_894_587_479_614_07))
                .mul_add(d, F64x::splat(-3.858_419_355_104_449_888_216_32))
                .mul_add(d, F64x::splat(2.230_727_530_249_660_972_572_2));

            let mut y = x * x;
            y = y * y;
            x -= d.mul_sub(y, x) * F64x::splat(1. / 3.);

            let mut z = x;

            let mut u = x.mul_as_doubled(x);
            u = u * u;
            u *= d;
            u += -x;
            y = u.0 + u.1;

            y = F64x::splat(-2. / 3.) * y * z;
            let mut v = z.mul_as_doubled(z) + y;
            v *= d;
            v *= q2;
            z = vldexp2_vd_vd_vi(v.0 + v.1, qu - Ix::splat(2048));

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            z = d.is_infinite().select(F64x::INFINITY.mul_sign(q2.0), z);
            d.eq(ZERO).select(F64x::from_bits(q2.0.sign_bit()), z)
            /*} else {
                z = s.is_infinite().select(F64x::INFINITY.mul_sign(s), z);
                s.eq(ZERO)
                    .select(ZERO.mul_sign(s), z)
            }*/
        }

        pub fn exp2(d: F64x) -> F64x {
            let mut u = d.round();
            let q = u.roundi();

            let s = d - u;

            if cfg!(feature = "split_kernel") {
                let s2 = s * s;

                u = F64x::splat(0.443_435_908_292_652_945_4_e-9)
                    .mul_add(s2, F64x::splat(0.101_781_926_092_176_045_1_e-6))
                    .mul_add(s2, F64x::splat(0.152_527_335_351_758_473_e-4))
                    .mul_add(s2, F64x::splat(0.133_335_581_467_049_907_3_e-2))
                    .mul_add(s2, F64x::splat(0.555_041_086_648_204_659_6_e-1));

                let v = F64x::splat(0.707_316_459_808_570_742_5_e-8)
                    .mul_add(s2, F64x::splat(0.132_154_387_251_132_761_5_e-5))
                    .mul_add(s2, F64x::splat(0.154_035_304_510_114_780_8_e-3))
                    .mul_add(s2, F64x::splat(0.961_812_910_759_760_053_6_e-2))
                    .mul_add(s2, F64x::splat(0.240_226_506_959_101_221_4));

                u = u
                    .mul_add(s, v)
                    .mul_add(s, F64x::splat(0.693_147_180_559_945_286_2));
            } else {
                u = F64x::splat(0.443_435_908_292_652_945_4_e-9)
                    .mul_add(s, F64x::splat(0.707_316_459_808_570_742_5_e-8))
                    .mul_add(s, F64x::splat(0.101_781_926_092_176_045_1_e-6))
                    .mul_add(s, F64x::splat(0.132_154_387_251_132_761_5_e-5))
                    .mul_add(s, F64x::splat(0.152_527_335_351_758_473_e-4))
                    .mul_add(s, F64x::splat(0.154_035_304_510_114_780_8_e-3))
                    .mul_add(s, F64x::splat(0.133_335_581_467_049_907_3_e-2))
                    .mul_add(s, F64x::splat(0.961_812_910_759_760_053_6_e-2))
                    .mul_add(s, F64x::splat(0.555_041_086_648_204_659_6_e-1))
                    .mul_add(s, F64x::splat(0.240_226_506_959_101_221_4))
                    .mul_add(s, F64x::splat(0.693_147_180_559_945_286_2));
            }

            if cfg!(target_feature = "fma") {
                u = u.mul_adde(s, ONE);
            } else {
                u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
            }

            u = vldexp2_vd_vd_vi(u, q);

            u = d.ge(F64x::splat(1024.)).select(F64x::INFINITY, u);
            F64x::from_bits(!U64x::from_bits(d.lt(F64x::splat(-2000.))) & U64x::from_bits(u))
        }

        pub fn exp10(d: F64x) -> F64x {
            let mut u = (d * LOG10_2).round();
            let q = u.roundi();

            let s = u.mul_add(-L10U, d);
            let s = u.mul_add(-L10L, s);
            if cfg!(feature = "split_kernel") {
                let s2 = s * s;

                u = F64x::splat(0.241_146_349_833_426_765_2_e-3)
                    .mul_add(s2, F64x::splat(0.501_397_554_678_973_365_9_e-2))
                    .mul_add(s2, F64x::splat(0.680_893_639_944_678_413_8_e-1))
                    .mul_add(s2, F64x::splat(0.539_382_929_205_853_622_9))
                    .mul_add(s2, F64x::splat(0.203_467_859_229_343_295_3_e+1));

                let v = F64x::splat(0.115_748_841_521_718_737_5_e-2)
                    .mul_add(s2, F64x::splat(0.195_976_232_072_053_308_e-1))
                    .mul_add(s2, F64x::splat(0.206_995_849_472_267_623_4))
                    .mul_add(s2, F64x::splat(0.117_125_514_890_854_165_5_e+1))
                    .mul_add(s2, F64x::splat(0.265_094_905_523_920_587_6_e+1));

                u = u
                    .mul_add(s, v)
                    .mul_add(s, F64x::splat(0.230_258_509_299_404_590_1_e+1));
            } else {
                u = F64x::splat(0.241_146_349_833_426_765_2_e-3)
                    .mul_add(s, F64x::splat(0.115_748_841_521_718_737_5_e-2))
                    .mul_add(s, F64x::splat(0.501_397_554_678_973_365_9_e-2))
                    .mul_add(s, F64x::splat(0.195_976_232_072_053_308_e-1))
                    .mul_add(s, F64x::splat(0.680_893_639_944_678_413_8_e-1))
                    .mul_add(s, F64x::splat(0.206_995_849_472_267_623_4))
                    .mul_add(s, F64x::splat(0.539_382_929_205_853_622_9))
                    .mul_add(s, F64x::splat(0.117_125_514_890_854_165_5_e+1))
                    .mul_add(s, F64x::splat(0.203_467_859_229_343_295_3_e+1))
                    .mul_add(s, F64x::splat(0.265_094_905_523_920_587_6_e+1))
                    .mul_add(s, F64x::splat(0.230_258_509_299_404_590_1_e+1));
            }

            if cfg!(target_feature = "fma") {
                u = u.mul_adde(s, ONE);
            } else {
                u = ONE.add_checked(u.mul_as_doubled(s)).normalize().0;
            }

            u = vldexp2_vd_vd_vi(u, q);

            u = d
                .gt(F64x::splat(308.254_715_559_916_71))
                .select(F64x::INFINITY, u);
            F64x::from_bits(!U64x::from_bits(d.lt(F64x::splat(-350.))) & U64x::from_bits(u))
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn expm1(a: F64x) -> F64x {
            let d = expk2(Doubled::new(a, ZERO)) + F64x::splat(-1.);
            let mut x = d.0 + d.1;
            x = a
                .gt(F64x::splat(709.782_712_893_383_996_732_223))
                .select(F64x::INFINITY, x);
            x = a
                .lt(F64x::splat(-36.736_800_569_677_101_399_113_302_437))
                .select(F64x::splat(-1.), x);
            a.is_neg_zero().select(NEG_ZERO, x)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn log10(mut d: F64x) -> F64x {
            let m: F64x;

            let mut s = /*if !cfg!(feature = "enable_avx512f")
                        && !cfg!(feature = "enable_avx512fnofma")*/
                    {
                        let o = d.lt(F64x::splat(f64::MIN));
                        d = o.select(d * (D1_32X * D1_32X), d);
                        let mut e = vilogb2k_vi_vd(d * F64x::splat(1. / 0.75));
                        m = vldexp3_vd_vd_vi(d, -e);
                        e = Mx::from_cast(o).select(e - Ix::splat(64), e);
                        Doubled::from((0.301_029_995_663_981_198_02, -2.803_728_127_785_170_339_e-18)) * F64x::from_cast(e)
                    }/* else {
                        let mut e = vgetexp_vd_vd(d * F64x::splat(1. / 0.75));
                        e = e.eq(F64x::INFINITY).select(F64x::splat(1024.), e);
                        m = vgetmant_vd_vd(d);
                        Doubled::from((0.301_029_995_663_981_198_02, -2.803_728_127_785_170_339_e-18)) * e
                    }*/;

            let x = F64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F64x::splat(0.665_372_581_957_675_846_e-1)
                .mul_add(x2, F64x::splat(0.662_572_278_282_083_371_2_e-1))
                .mul_add(x2, F64x::splat(0.789_810_521_431_394_407_8_e-1))
                .mul_add(x2, F64x::splat(0.965_095_503_571_527_513_2_e-1))
                .mul_add(x2, F64x::splat(0.124_084_140_972_144_499_3))
                .mul_add(x2, F64x::splat(0.173_717_792_745_460_508_6))
                .mul_add(x2, F64x::splat(0.289_529_654_602_197_261_7));

            s = s.add_checked(
                x * Doubled::from((
                    0.868_588_963_806_503_633_34,
                    1.143_005_969_409_638_931_1_e-17,
                )),
            );
            s = s.add_checked(x2 * x.0 * t);

            let r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            let r = d.eq(F64x::INFINITY).select(F64x::INFINITY, r);
            let r = (d.lt(ZERO) | d.is_nan()).select(F64x::NAN, r);
            d.eq(ZERO).select(F64x::NEG_INFINITY, r)
            /*} else {
                vfixup_vd_vd_vd_vi2_i(
                    r,
                    d,
                    I64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn log2(mut d: F64x) -> F64x {
            let m: F64x;
            let ef =
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                let o = d.lt(F64x::splat(f64::MIN));
                d = o.select(d * (D1_32X * D1_32X), d);
                let mut e = vilogb2k_vi_vd(d * F64x::splat(1. / 0.75));
                m = vldexp3_vd_vd_vi(d, -e);
                e = Mx::from_cast(o).select(e - Ix::splat(64), e);
                F64x::from_cast(e)
            }/* else {
                let e = vgetexp_vd_vd(d * F64x::splat(1.0 / 0.75));
                e = e.eq(F64x::INFINITY).select(F64x::splat(1024.), e);
                m = vgetmant_vd_vd(d);
                e
            }*/;

            let x = F64x::splat(-1.).add_as_doubled(m) / ONE.add_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F64x::splat(0.221_194_175_045_608_149)
                .mul_add(x2, F64x::splat(0.220_076_869_315_227_768_9))
                .mul_add(x2, F64x::splat(0.262_370_805_748_851_465_6))
                .mul_add(x2, F64x::splat(0.320_597_747_794_449_550_2))
                .mul_add(x2, F64x::splat(0.412_198_594_548_532_470_9))
                .mul_add(x2, F64x::splat(0.577_078_016_299_705_898_2))
                .mul_add(x2, F64x::splat(0.961_796_693_926_080_914_49));

            let mut s =
                ef + x * Doubled::from((2.885_390_081_777_926_774, 6.056_160_499_551_673_643_4_e-18));
            s += x2 * x.0 * t;

            let r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
            let r = d.eq(F64x::INFINITY).select(F64x::INFINITY, r);
            let r = (d.lt(ZERO) | d.is_nan()).select(F64x::NAN, r);
            d.eq(ZERO).select(F64x::NEG_INFINITY, r)
            /*} else {
                vfixup_vd_vd_vd_vi2_i(
                    r,
                    d,
                    I64x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn log1p(d: F64x) -> F64x {
            let m: F64x;

            let mut dp1 = d + ONE;

            let mut s =
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                let o = dp1.lt(F64x::splat(f64::MIN));
                dp1 = o.select(dp1 * (D1_32X * D1_32X), dp1);
                let mut e = vilogb2k_vi_vd(dp1 * F64x::splat(1. / 0.75));
                let t = vldexp3_vd_vd_vi(ONE, -e);
                m = d.mul_add(t, t - ONE);
                e = Mx::from_cast(o).select(e - Ix::splat(64), e);
                Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17))
                    * F64x::from_cast(e)
            }/* else {
                let e = vgetexp_vd_vd(dp1, F64x::splat(1. / 0.75));
                e = e.eq(F64x::INFINITY).select(F64x::splat(1024.), e);
                let t = vldexp3_vd_vd_vi(ONE, -e.roundi());
                m = d.mul_add(t, t - ONE);
                Doubled::from((0.693_147_180_559_945_286_226_764, 2.319_046_813_846_299_558_417_771_e-17)) * e
            }*/;

            let x = Doubled::new(m, ZERO) / F64x::splat(2.).add_checked_as_doubled(m);
            let x2 = x.0 * x.0;

            let t = F64x::splat(0.153_207_698_850_270_135_3)
                .mul_add(x2, F64x::splat(0.152_562_905_100_342_871_6))
                .mul_add(x2, F64x::splat(0.181_860_593_293_778_599_6))
                .mul_add(x2, F64x::splat(0.222_221_451_983_938_000_9))
                .mul_add(x2, F64x::splat(0.285_714_293_279_429_931_7))
                .mul_add(x2, F64x::splat(0.399_999_999_963_525_199))
                .mul_add(x2, F64x::splat(0.666_666_666_666_733_354_1));

            s = s.add_checked(x.scale(F64x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = s.0 + s.1;

            r = d.gt(F64x::splat(1e+307)).select(F64x::INFINITY, r);
            r = (d.lt(F64x::splat(-1.)) | d.is_nan()).select(F64x::NAN, r);
            r = d.eq(F64x::splat(-1.)).select(F64x::NEG_INFINITY, r);
            d.is_neg_zero().select(NEG_ZERO, r)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn tgamma(a: F64x) -> F64x {
            let (da, db) = gammak(a);
            let y = expk2(da) * db;
            let r = y.0 + y.1;
            let o = a.eq(F64x::NEG_INFINITY)
                | (a.lt(ZERO) & a.is_integer())
                | (a.is_finite() & a.lt(ZERO) & r.is_nan());
            let r = o.select(F64x::NAN, r);

            let o = ((a.eq(F64x::INFINITY) | a.is_finite()) & a.ge(F64x::splat(-f64::MIN)))
                & (a.eq(ZERO) | a.gt(F64x::splat(200.)) | r.is_nan());
            o.select(F64x::INFINITY.mul_sign(a), r)
        }

        #[cfg(not(feature = "deterministic"))]
        pub fn lgamma(a: F64x) -> F64x {
            let (da, db) = gammak(a);
            let y = da + logk2(db.abs());
            let r = y.0 + y.1;

            let o = a.is_infinite() | (a.le(ZERO) & a.is_integer()) | (a.is_finite() & r.is_nan());
            o.select(F64x::INFINITY, r)
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        #[cfg(not(feature = "deterministic"))]
        pub fn erf(a: F64x) -> F64x {
            let s = a;

            let a = a.abs();
            let o0 = a.lt(ONE);
            let o1 = a.lt(F64x::splat(3.7));
            let o2 = a.lt(F64x::splat(6.));
            let u = o0.select(a * a, a);

            let t = F64x::select3(
                o0,
                o1,
                0.680_107_240_139_539_215_7_e-20,
                0.283_095_452_208_771_766_e-13,
                -0.584_675_040_426_961_049_3_e-17,
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.216_176_624_757_005_639_1_e-18,
                    -0.150_949_194_617_948_194_e-11,
                    0.607_669_104_881_260_789_8_e-15,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.469_591_917_330_159_875_2_e-17,
                    0.382_785_717_780_717_315_2_e-10,
                    -0.300_751_860_960_489_383_1_e-13,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.904_914_041_988_801_081_9_e-16,
                    -0.613_973_392_155_898_724_1_e-9,
                    0.942_790_626_082_464_606_3_e-12,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.163_401_890_355_741_151_7_e-14,
                    0.698_538_793_460_803_882_4_e-8,
                    -0.210_011_090_826_939_362_9_e-10,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.278_348_578_633_345_521_6_e-13,
                    -0.598_822_451_303_437_147_4_e-7,
                    0.353_463_952_346_122_347_3_e-9,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.446_322_127_678_641_272_2_e-12,
                    0.400_571_695_235_534_664_e-6,
                    -0.466_496_772_828_539_592_6_e-8,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.671_136_662_285_013_898_7_e-11,
                    -0.213_219_010_457_578_44_e-5,
                    0.494_382_328_376_900_053_2_e-7,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.942_275_905_023_265_834_6_e-10,
                    0.909_246_130_404_263_032_5_e-5,
                    -0.427_120_339_476_114_825_4_e-6,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.122_905_553_010_022_847_7_e-8,
                    -0.307_918_808_096_620_545_7_e-4,
                    0.303_406_767_740_491_589_5_e-5,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.148_071_928_158_508_502_3_e-7,
                    0.797_141_344_308_237_076_2_e-4,
                    -0.177_629_528_906_687_113_5_e-4,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.163_658_446_912_340_271_4_e-6,
                    -0.138_785_321_522_544_286_4_e-3,
                    0.852_454_763_055_950_505_e-4,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.164_621_143_658_892_336_3_e-5,
                    0.646_967_802_625_759_096_5_e-4,
                    -0.329_058_294_496_178_439_8_e-3,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.149_256_503_584_062_486_6_e-4,
                    0.499_664_528_037_294_586_e-3,
                    0.969_696_606_878_910_115_7_e-3,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.120_553_329_817_896_649_6_e-3,
                    -0.162_280_248_284_252_053_5_e-2,
                    -0.181_252_762_804_698_613_7_e-2,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.854_832_702_345_085_116_6_e-3,
                    0.161_532_055_704_937_717_1_e-3,
                    -0.472_540_982_812_361_901_7_e-3,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.522_397_762_544_218_879_9_e-2,
                    0.191_526_232_557_487_560_7_e-1,
                    0.209_031_542_792_422_926_6_e-1,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.268_661_706_451_312_556_9_e-1,
                    -0.102_781_829_848_603_345_5,
                    -0.105_204_192_184_277_664_5,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    0.112_837_916_709_551_275_3,
                    -0.636_617_281_984_250_382_7,
                    -0.634_535_180_876_656_834_7,
                ),
            )
            .mul_add(
                u,
                F64x::select3(
                    o0,
                    o1,
                    -0.376_126_389_031_837_538,
                    -0.112_837_959_064_891_046_9_e+1,
                    -0.112_944_292_910_352_439_6_e+1,
                ),
            );
            let mut d = t.mul_as_doubled(u);

            d += Doubled::new(
                F64x::select3(
                    o0,
                    o1,
                    1.128_379_167_095_512_558_6,
                    3.411_064_473_619_613_758_7_e-8,
                    0.000_249_630_356_905_264_382_85,
                ),
                F64x::select3(
                    o0,
                    o1,
                    1.533_545_961_316_582_267_4_e-17,
                    -2.487_565_070_832_329_424_6_e-24,
                    -5.436_266_503_485_625_979_5_e-21,
                ),
            );
            d = o0.select_doubled(d * a, ONE.add_checked(-expk2(d)));

            let u = o2.select(d.0 + d.1, ONE).mul_sign(s);
            a.is_nan().select(F64x::NAN, u)
        }
    };
}
