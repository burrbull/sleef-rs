macro_rules! impl_math_f32_u05 {
    ($f32x:ident, $u32x:ident, $m32x:ident, $i32x:ident) => {
        use super::*;

        pub fn sincospif(d: $f32x) -> ($f32x, $f32x) {
            let u = d * $f32x::splat(4.);
            let q = u.trunci();
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
            let mut x = u * s
                + Doubled::from((
                    -0.080_745_510_756_969_451_904,
                    -1.337_366_533_907_693_625_8_e-9,
                ));
            x = s2 * x
                + Doubled::from((
                    0.785_398_185_253_143_310_55,
                    -2.185_733_861_756_648_485_5_e-8,
                ));

            x *= t;
            let rx = x.0 + x.1;

            let rx = d.is_neg_zero().select(NEG_ZERO, rx);

            //

            let u = $f32x::splat(-0.243_061_180_1_e-7)
                .mul_add(s, $f32x::splat(0.359_057_708_e-5))
                .mul_add(s, $f32x::splat(-0.325_991_772_1_e-3));
            x = u * s
                + Doubled::from((
                    0.015_854_343_771_934_509_277,
                    4.494_005_135_403_224_281_1_e-10,
                ));
            x = s2 * x
                + Doubled::from((
                    -0.308_425_128_459_930_419_92,
                    -9.072_833_903_073_392_227_7_e-9,
                ));

            x = x * s2 + ONE;
            let ry = x.0 + x.1;

            //

            let o = (q & $i32x::splat(2)).eq($i32x::splat(0));
            let mut rsin = o.select(rx, ry);
            let mut rcos = o.select(ry, rx);

            let o = (q & $i32x::splat(4)).eq($i32x::splat(4));
            rsin = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits(NEG_ZERO)) ^ $u32x::from_bits(rsin),
            );

            let o = ((q + $i32x::splat(2)) & $i32x::splat(4)).eq($i32x::splat(4));
            rcos = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits(NEG_ZERO)) ^ $u32x::from_bits(rcos),
            );

            let o = d.abs().gt($f32x::splat(1e+7));
            rsin = $f32x::from_bits(!$u32x::from_bits(o) & $u32x::from_bits(rsin));
            rcos = $f32x::from_bits(!$u32x::from_bits(o) & $u32x::from_bits(rcos));

            let o = d.is_infinite();
            rsin = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rsin)));
            rcos = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rcos)));

            (rsin, rcos)
        }

        pub fn sqrtf(d: $f32x) -> $f32x {
            let d = d.lt(ZERO).select($f32x::NAN, d);

            let o = d.lt($f32x::splat(5.293_955_920_339_377_e-23));
            let d = o.select(d * $f32x::splat(1.888_946_593_147_858_e+22), d);
            let q = o.select($f32x::splat(7.275_957_614_183_426_e-12 * 0.5), HALF);

            let o = d.gt($f32x::splat(1.844_674_407_370_955_2_e+19));
            let d = o.select(d * $f32x::splat(5.421_010_862_427_522_e-20), d);
            let q = o.select($f32x::splat(4_294_967_296.0 * 0.5), q);

            let mut x = $f32x::from_bits(
                $i32x::splat(0x_5f37_5a86)
                    - $i32x::from_bits($u32x::from_bits(d + $f32x::splat(1e-45)) >> 1),
            );

            x *= $f32x::splat(1.5) - HALF * d * x * x;
            x *= $f32x::splat(1.5) - HALF * d * x * x;
            x *= $f32x::splat(1.5) - HALF * d * x * x;
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
            (x.eq($f32x::INFINITY) | y.eq($f32x::INFINITY)).select($f32x::INFINITY, ret)
        }

        pub fn xsinpif(d: $f32x) -> $f32x {
            let x = sinpifk(d);
            let mut r = x.0 + x.1;

            r = d.is_neg_zero().select(NEG_ZERO, r);
            r = $f32x::from_bits(
                !$u32x::from_bits(d.abs().gt(TRIGRANGEMAX4_F)) & $u32x::from_bits(r),
            );
            $f32x::from_bits(vor_vm_vo32_vm(d.is_infinite(), $u32x::from_bits(r)))
        }

        pub fn cospif(d: $f32x) -> $f32x {
            let x = cospifk(d);
            let r = x.0 + x.1;

            let r = d.abs().gt(TRIGRANGEMAX4_F).select(ONE, r);
            $f32x::from_bits(vor_vm_vo32_vm(d.is_infinite(), $u32x::from_bits(r)))
        }
    };
}
