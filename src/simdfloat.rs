//          Copyright Naoki Shibata 2010 - 2018.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// Always use -ffp-contract=off option to compile SLEEF.
/*
#include <stdint.h>
#include <assert.h>
#include <limits.h>
#include <float.h>

#include "misc.h"

extern const float rempitabsp[];

#define __SLEEFSIMDSP_C__

#if (defined(_MSC_VER))
#pragma fp_contract (off)
#endif

#ifdef ENABLE_SSE2
#define CONFIG 2
#include "helpersse2.h"
#ifdef DORENAME
#ifdef ENABLE_GNUABI
#include "renamesse2_gnuabi.h"
#else
#include "renamesse2.h"
#endif
#endif
#endif

#ifdef ENABLE_SSE4
#define CONFIG 4
#include "helpersse2.h"
#ifdef DORENAME
#include "renamesse4.h"
#endif
#endif

#ifdef ENABLE_AVX
#define CONFIG 1
#include "helperavx.h"
#ifdef DORENAME
#ifdef ENABLE_GNUABI
#include "renameavx_gnuabi.h"
#else
#include "renameavx.h"
#endif
#endif
#endif

#ifdef ENABLE_FMA4
#define CONFIG 4
#include "helperavx.h"
#ifdef DORENAME
#ifdef ENABLE_GNUABI
#include "renamefma4_gnuabi.h"
#else
#include "renamefma4.h"
#endif
#endif
#endif

#ifdef ENABLE_AVX2
#define CONFIG 1
#include "helperavx2.h"
#ifdef DORENAME
#ifdef ENABLE_GNUABI
#include "renameavx2_gnuabi.h"
#else
#include "renameavx2.h"
#endif
#endif
#endif

#ifdef ENABLE_AVX2128
#define CONFIG 1
#include "helperavx2_128.h"
#ifdef DORENAME
#include "renameavx2128.h"
#endif
#endif

#ifdef ENABLE_AVX512F
#define CONFIG 1
#include "helperavx512f.h"
#ifdef DORENAME
#ifdef ENABLE_GNUABI
#include "renameavx512f_gnuabi.h"
#else
#include "renameavx512f.h"
#endif
#endif
#endif

#ifdef ENABLE_AVX512FNOFMA
#define CONFIG 2
#include "helperavx512f.h"
#ifdef DORENAME
#include "renameavx512fnofma.h"
#endif
#endif

#ifdef ENABLE_ADVSIMD
#define CONFIG 1
#include "helperadvsimd.h"
#ifdef DORENAME
#ifdef ENABLE_GNUABI
#include "renameadvsimd_gnuabi.h"
#else
#include "renameadvsimd.h"
#endif
#endif
#endif

#ifdef ENABLE_ADVSIMDNOFMA
#define CONFIG 2
#include "helperadvsimd.h"
#ifdef DORENAME
#include "renameadvsimdnofma.h"
#endif
#endif

#ifdef ENABLE_NEON32
#define CONFIG 1
#include "helperneon32.h"
#ifdef DORENAME
#include "renameneon32.h"
#endif
#endif

#ifdef ENABLE_NEON32VFPV4
#define CONFIG 4
#include "helperneon32.h"
#ifdef DORENAME
#include "renameneon32vfpv4.h"
#endif
#endif

#ifdef ENABLE_VSX
#define CONFIG 1
#include "helperpower_128.h"
#ifdef DORENAME
#include "renamevsx.h"
#endif
#endif

#ifdef ENABLE_VSXNOFMA
#define CONFIG 2
#include "helperpower_128.h"
#ifdef DORENAME
#include "renamevsxnofma.h"
#endif
#endif

//

#ifdef ENABLE_VECEXT
#define CONFIG 1
#include "helpervecext.h"
#ifdef DORENAME
#include "renamevecext.h"
#endif
#endif

#ifdef ENABLE_PUREC
#define CONFIG 1
#include "helperpurec.h"
#ifdef DORENAME
#include "renamepurec.h"
#endif
#endif

#ifdef ENABLE_PUREC_SCALAR
#define CONFIG 1
#include "helperpurec_scalar.h"
#ifdef DORENAME
#include "renamepurec_scalar.h"
#endif
#endif

#ifdef ENABLE_PURECFMA_SCALAR
#define CONFIG 2
#include "helperpurec_scalar.h"
#ifdef DORENAME
#include "renamepurecfma_scalar.h"
#endif
#endif

//

#ifdef ENABLE_SVE
#define CONFIG 1
#include "helpersve.h"
#ifdef DORENAME
#ifdef ENABLE_GNUABI
#include "renamesve_gnuabi.h"
#else
#include "renamesve.h"
#endif /* ENABLE_GNUABI */
#endif /* DORENAME */
#endif /* ENABLE_SVE */

#ifdef ENABLE_SVENOFMA
#define CONFIG 2
#include "helpersve.h"
#ifdef DORENAME
#include "renamesvenofma.h"
#endif /* DORENAME */
#endif /* ENABLE_SVE */
*/

macro_rules! impl_math_f32 {
    ($f32x:ident, $u32x:ident, $m32x:ident, $i32x:ident) => {
        //use f2::*;

        //---------???????
        //--------- Naive implementation ???????
        #[inline]
        fn vandnot_vm_vm_vm(x: $u32x, y: $u32x) -> $u32x { x & !y }

        #[inline]
        fn vandnot_vo_vo_vo(x: $m32x, y: $m32x) -> $m32x { x & !y }

        #[inline]
        fn vand_vm_vo32_vm(x: $m32x, y: $u32x) -> $u32x { $u32x::from_bits(x) & y }
        #[inline]
        fn vor_vm_vo32_vm(x: $m32x, y: $u32x) -> $u32x {  $u32x::from_bits(x) | y }
        #[inline]
        fn vandnot_vm_vo32_vm(x: $m32x, y: $u32x) -> $u32x {  $u32x::from_bits(x) & !y }

        #[inline]
        fn vandnot_vi2_vi2_vi2(x: $i32x, y: $i32x) -> $i32x { x & !y }

        #[inline]
        fn vand_vi2_vo_vi2(x: $m32x, y: $i32x) -> $i32x { $i32x::from_bits(x) & y }

        #[inline]
        fn vgt_vi2_vi2_vi2(_x: $i32x, _y: $i32x) -> $i32x { unimplemented!() }


        impl Round for $f32x {
            type Int = $i32x;
            #[inline]
            fn truncate(self) -> Self {
                Self::from_cast(self.truncatei())
            }
            #[inline]
            fn truncatei(self) -> Self::Int {
                unimplemented!()
            }
            #[inline]
            fn rint(self) -> Self {
                Self::from_cast(self.rinti())
            }
            #[inline]
            fn rinti(self) -> Self::Int {
                Self::Int::from_cast(self)
            }
        }

        #[inline]
        fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { z - x * y }


        #[inline]
        fn vgather_vf_p_vi2(_ptr: &[f32], _st: usize, _vi: $i32x) -> $f32x {
          unimplemented!()
        }


        //----------???????
        //----------???????

        //-------------------
        impl IsInf for $f32x {
          type Mask = $m32x;
          #[inline]
          fn isinf(self) -> Self::Mask {
             self.abs().eq(Self::splat(SLEEF_INFINITY_F))
          }
          #[inline]
          fn ispinf(self) -> Self::Mask {
            self.eq(Self::splat(SLEEF_INFINITY_F))
          }
        }
        impl IsNan for $f32x {
          type Mask = $m32x;
          #[inline]
          fn isnan(self) -> Self::Mask {
            self.ne(self)
          }
        }

        #[inline]
        fn visnegzero_vo_vf(d: $f32x) -> $m32x {
            $i32x::from_cast(d).eq($i32x::from_cast($f32x::splat(-0.)))
        }

        #[inline]
        fn vnot_vo32_vo32(x: $m32x) -> $m32x {
            x ^ $i32x::splat(0).eq($i32x::splat(0))
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
                $u32x::from_bits($f32x::splat(1.))
                    | ($u32x::from_bits($f32x::splat(-0.)) & $u32x::from_bits(f)),
            )
        }
        #[inline]
        fn vsignbit_vo_vf(d: $f32x) -> $m32x {
            ($i32x::from_cast(d) & $i32x::splat(0x80000000)).eq($i32x::splat(0x80000000))
        }
        #[inline]
        fn vsel_vi2_vf_vf_vi2_vi2(f0: $f32x, f1: $f32x, x: $i32x, y: $i32x) -> $i32x {
            f0.lt(f1).select(x, y)
        }
        #[inline]
        fn vsel_vi2_vf_vi2(d: $f32x, x: $i32x) -> $i32x {
            vand_vi2_vo_vi2(vsignbit_vo_vf(d), x)
        }
        #[inline]
        fn visint_vo_vf(y: $f32x) -> $m32x {
            y.truncate().eq(y)
        }
        #[inline]
        fn visnumber_vo_vf(x: $f32x) -> $m32x {
            vnot_vo32_vo32(x.isinf() | x.isnan())
        }

        /*#[cfg(
            all(not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn vilogbk_vi2_vf(mut d: $f32x) -> $i32x {
            let o = d.lt($f32x::splat(5.421010862427522e-20));
            d = o.select($f32x::splat(1.8446744073709552e19) * d, d);
            let q = $i32x::from_cast($u32x::from_bits(d) >> 23) & $i32x::splat(0xff);
            q - o.select($i32x::splat(64 + 0x7f), $i32x::splat(0x7f))
        }
        /*#[cfg(
            all(not(feature = "enable_avx512f"),
            not(feature = "enable_avx512fnofma")
        ))]*/
        #[inline]
        fn vilogb2k_vi2_vf(d: $f32x) -> $i32x {
            let mut q = $i32x::from_cast(d);
            q = $i32x::from_bits($u32x::from_bits(q) >> 23);
            q = q & $i32x::splat(0xff);
            q - $i32x::splat(0x7f)
        }

        //

        pub fn xilogbf(d: $f32x) -> $i32x {
            let mut e = vilogbk_vi2_vf(d.abs());
            e = d
                .eq($f32x::splat(0.))
                .select($i32x::splat(SLEEF_FP_ILOGB0), e);
            e = d.isnan().select($i32x::splat(SLEEF_FP_ILOGBNAN), e);
            d.isinf().select($i32x::splat(i32::MAX), e)
        }
        #[inline]
        fn vpow2i_vf_vi2(q: $i32x) -> $f32x {
            $f32x::from_bits($u32x::from_bits((q + $i32x::splat(0x7f)) << 23))
        }
        #[inline]
        fn vldexp_vf_vf_vi2(mut x: $f32x, mut q: $i32x) -> $f32x {
            let mut m = q >> 31;
            m = (((m + q) >> 6) - m) << 4;
            q = q - (m << 2);
            m = m + $i32x::splat(0x7f);
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
            $f32x::from_bits($i32x::from_cast(d) + (q << 23))
        }

        pub fn xldexpf(x: $f32x, q: $i32x) -> $f32x {
            vldexp_vf_vf_vi2(x, q)
        }

        #[inline]
        fn rempisubf(x: $f32x) -> ($f32x, $i32x) {
            if cfg!(feature = "full_fp_rounding") {
                let y = (x * $f32x::splat(4.)).rint();
                let vi = (y - x.rint() * $f32x::splat(4.)).truncatei();
                (x - y * $f32x::splat(0.25), vi)
            } else {
                let mut fr = x - $f32x::splat(F1_10) * (x * $f32x::splat(1. / F1_10)).truncate();
                let mut vi = x
                    .gt($f32x::splat(0.))
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
                fr = fr.abs().gt($f32x::splat(1e+10)).select($f32x::splat(0.), fr);
                let o = x.abs().eq($f32x::splat(0.12499999254941940308));
                fr = o.select(x, fr);
                vi = o.select($i32x::splat(0), vi);
                (fr, vi)
            }
        }
        #[inline]
        fn rempif(mut a: $f32x) -> (F2<$f32x>, $i32x) {
            let mut ex = vilogb2k_vi2_vf(a);
            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                ex = vandnot_vi2_vi2_vi2(ex >> 31, ex);
                ex = ex & $i32x::splat(127);
            }*/
            ex -= $i32x::splat(25);
            let q = vand_vi2_vo_vi2(ex.gt($i32x::splat(90 - 25)), $i32x::splat(-64));
            a = vldexp3_vf_vf_vi2(a, q);
            ex = vandnot_vi2_vi2_vi2(ex >> 31, ex);
            ex = ex << 2;
            let mut x = a.mul_as_f2(vgather_vf_p_vi2(&REMPITABSP, 0, ex));
            let (did, mut q) = rempisubf(x.0);
            x.0 = did;
            x = x.normalize();
            let y = a.mul_as_f2(vgather_vf_p_vi2(&REMPITABSP, 1, ex));
            x += y;
            let (did, dii) = rempisubf(x.0);
            q = q + dii;
            x.0 = did;
            x = x.normalize();
            let mut y = F2::new(
                vgather_vf_p_vi2(&REMPITABSP, 2, ex),
                vgather_vf_p_vi2(&REMPITABSP, 3, ex),
            );
            y *= a;
            x += y;
            x = x.normalize();
            x *= F2::from((3.1415927410125732422 * 2., -8.7422776573475857731e-08 * 2.));
            x = vsel_vf2_vo_vf2_vf2(
                a.abs().lt($f32x::splat(0.7)),
                F2::new(a, $f32x::splat(0.)),
                x,
            );
            (x, q)
        }

        pub fn xsinf(mut d: $f32x) -> $f32x {
            let mut q: $i32x;
            let u: $f32x;
            let r = d;

            if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                q = (d * $f32x::splat(M_1_PI_F)).rinti();
                u = $f32x::from_cast(q);
                d = u.mul_add($f32x::splat(-PI_A2_F), d);
                d = u.mul_add($f32x::splat(-PI_B2_F), d);
                d = u.mul_add($f32x::splat(-PI_C2_F), d);
            } else if d.abs().lt($f32x::splat(TRIGRANGEMAX_F)).all() {
                q = (d * $f32x::splat(M_1_PI_F)).rinti();
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
                    .gt($f32x::splat(0.))
                    .select($i32x::splat(2), $i32x::splat(1));
                q = q >> 2;
                let o = (dfii & $i32x::splat(1)).eq($i32x::splat(1));
                let mut x = F2::new(
                    vmulsign_vf_vf_vf($f32x::splat(3.1415927410125732422 * -0.5), dfidf.0),
                    vmulsign_vf_vf_vf($f32x::splat(-8.7422776573475857731e-08 * -0.5), dfidf.0),
                );
                x = dfidf + x;
                dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
                d = dfidf.0 + dfidf.1;

                d = $f32x::from_bits(vor_vm_vo32_vm(
                    r.isinf() | r.isnan(),
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

            let mut u = $f32x::splat(2.6083159809786593541503e-06)
                .mul_add(s, $f32x::splat(-0.0001981069071916863322258))
                .mul_add(s, $f32x::splat(0.00833307858556509017944336))
                .mul_add(s, $f32x::splat(-0.166666597127914428710938));

            u = s * (u * d) + d;

            visnegzero_vo_vf(r).select(r, u)
        }

        pub fn xcosf(mut d: $f32x) -> $f32x {
            let mut q: $i32x;
            let r = d;

            if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                q = (d * $f32x::splat(M_1_PI_F) - $f32x::splat(0.5)).rinti();
                q = q + q + $i32x::splat(1);

                let u = $f32x::from_cast(q);
                d = u.mul_add($f32x::splat(-PI_A2_F * 0.5), d);
                d = u.mul_add($f32x::splat(-PI_B2_F * 0.5), d);
                d = u.mul_add($f32x::splat(-PI_C2_F * 0.5), d);
            } else if d.abs().lt($f32x::splat(TRIGRANGEMAX_F)).all() {
                q = (d * $f32x::splat(M_1_PI_F) - $f32x::splat(0.5)).rinti();
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
                    .gt($f32x::splat(0.))
                    .select($i32x::splat(8), $i32x::splat(7));
                q = q >> 1;
                let o = (dfii & $i32x::splat(1)).eq($i32x::splat(0));
                let y = dfidf
                    .0
                    .gt($f32x::splat(0.))
                    .select($f32x::splat(0.), $f32x::splat(-1.));
                let mut x = F2::new(
                    vmulsign_vf_vf_vf($f32x::splat(3.1415927410125732422 * -0.5), y),
                    vmulsign_vf_vf_vf($f32x::splat(-8.7422776573475857731e-08 * -0.5), y),
                );
                x = dfidf + x;
                dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
                d = dfidf.0 + dfidf.1;

                d = $f32x::from_bits(vor_vm_vo32_vm(
                    r.isinf() | r.isnan(),
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

            let u = $f32x::splat(2.6083159809786593541503e-06)
                .mul_add(s, $f32x::splat(-0.0001981069071916863322258))
                .mul_add(s, $f32x::splat(0.00833307858556509017944336))
                .mul_add(s, $f32x::splat(-0.166666597127914428710938));

            s * (u * d) + d
        }

        pub fn xtanf(d: $f32x) -> $f32x {
            let q: $i32x;

            let mut x = d;

            if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F * 0.5)).all() {
                q = (d * $f32x::splat(2. * M_1_PI_F)).rinti();
                let u = $f32x::from_cast(q);
                x = u.mul_add($f32x::splat(-PI_A2_F * 0.5), x);
                x = u.mul_add($f32x::splat(-PI_B2_F * 0.5), x);
                x = u.mul_add($f32x::splat(-PI_C2_F * 0.5), x);
            } else if d.abs().lt($f32x::splat(TRIGRANGEMAX_F)).all() {
                q = (d * (2. * $f32x::splat(M_1_PI_F))).rinti();
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
                    d.isinf() | d.isnan(),
                    $u32x::from_bits(x),
                ));
                x = visnegzero_vo_vf(d).select(d, x);
            }

            let s = x * x;

            let o = (q & $i32x::splat(1)).eq($i32x::splat(1));
            x = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x),
            );

            let mut u = $f32x::splat(0.00927245803177356719970703)
                .mul_add(s, $f32x::splat(0.00331984995864331722259521))
                .mul_add(s, $f32x::splat(0.0242998078465461730957031))
                .mul_add(s, $f32x::splat(0.0534495301544666290283203))
                .mul_add(s, $f32x::splat(0.133383005857467651367188))
                .mul_add(s, $f32x::splat(0.333331853151321411132812));

            u = s.mul_add(u * x, x);

            o.select(u.recpre(), u)
        }

        pub fn xsinf_u1(d: $f32x) -> $f32x {
            let mut q: $i32x;
            let mut s: F2<$f32x>;

            if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                let u = (d * $f32x::splat(M_1_PI_F)).rint();
                q = u.rinti();
                let v = u.mul_add($f32x::splat(-PI_A2_F), d);
                s = v.add_as_f2(u * $f32x::splat(-PI_B2_F));
                s = s.add_checked(u * $f32x::splat(-PI_C2_F));
            } else {
                let (mut dfidf, dfii) = rempif(d);
                q = dfii & $i32x::splat(3);
                q = q + q + dfidf
                    .0
                    .gt($f32x::splat(0.))
                    .select($i32x::splat(2), $i32x::splat(1));
                q = q >> 2;
                let o = (dfii & $i32x::splat(1)).eq($i32x::splat(1));
                let mut x = F2::new(
                    vmulsign_vf_vf_vf($f32x::splat(3.1415927410125732422 * -0.5), dfidf.0),
                    vmulsign_vf_vf_vf($f32x::splat(-8.7422776573475857731e-08 * -0.5), dfidf.0),
                );
                x = dfidf + x;
                dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
                s = dfidf.normalize();

                s.0 = $f32x::from_bits(vor_vm_vo32_vm(
                    d.isinf() | d.isnan(),
                    $u32x::from_bits(s.0),
                ));
            }

            let t = s;
            let s = s.square();

            let mut u = $f32x::splat(2.6083159809786593541503e-06)
                .mul_add(s.0, $f32x::splat(-0.0001981069071916863322258))
                .mul_add(s.0, $f32x::splat(0.00833307858556509017944336));

            let x = $f32x::splat(1.).add_checked(
                $f32x::splat(-0.166666597127914428710938).add_checked_as_f2(u * s.0) * s,
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

        pub fn xcosf_u1(d: $f32x) -> $f32x {
            let mut q: $i32x;
            let mut s: F2<$f32x>;

            if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                let dq = (d.mul_add($f32x::splat(M_1_PI_F), $f32x::splat(-0.5)))
                    .rint()
                    .mul_add($f32x::splat(2.), $f32x::splat(1.));
                q = dq.rinti();
                s = d.add_as_f2(dq * $f32x::splat(-PI_A2_F * 0.5));
                s += dq * $f32x::splat(-PI_B2_F * 0.5);
                s += dq * $f32x::splat(-PI_C2_F * 0.5);
            } else {
                let (mut dfidf, dfii) = rempif(d);
                q = dfii & $i32x::splat(3);
                q = q + q + dfidf
                    .0
                    .gt($f32x::splat(0.))
                    .select($i32x::splat(8), $i32x::splat(7));
                q = q >> 1;
                let o = (dfii & $i32x::splat(1)).eq($i32x::splat(0));
                let y = dfidf
                    .0
                    .gt($f32x::splat(0.))
                    .select($f32x::splat(0.), $f32x::splat(-1.));
                let mut x = F2::new(
                    vmulsign_vf_vf_vf($f32x::splat(3.1415927410125732422 * -0.5), y),
                    vmulsign_vf_vf_vf($f32x::splat(-8.7422776573475857731e-08 * -0.5), y),
                );
                x = dfidf + x;
                dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
                s = dfidf.normalize();

                s.0 = $f32x::from_bits(vor_vm_vo32_vm(
                    d.isinf() | d.isnan(),
                    $u32x::from_bits(s.0),
                ));
            }

            let t = s;
            s = s.square();

            let u = $f32x::splat(2.6083159809786593541503e-06)
                .mul_add(s.0, $f32x::splat(-0.0001981069071916863322258))
                .mul_add(s.0, $f32x::splat(0.00833307858556509017944336));

            let x = $f32x::splat(1.).add_checked(
                $f32x::splat(-0.166666597127914428710938).add_checked_as_f2(u * s.0) * s,
            );

            let u = t.mul_as_f(x);

            $f32x::from_bits(
                vand_vm_vo32_vm(
                    (q & $i32x::splat(2)).eq($i32x::splat(0)),
                    $u32x::from_bits($f32x::splat(-0.)),
                ) ^ $u32x::from_bits(u),
            )
        }

        pub fn xsincosf(d: $f32x) -> ($f32x, $f32x) {
            let q: $i32x;
            let mut s = d;

            if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                q = (d * $f32x::splat(M_2_PI_F)).rinti();
                let u = $f32x::from_cast(q);
                s = u.mul_add($f32x::splat(-PI_A2_F * 0.5), s);
                s = u.mul_add($f32x::splat(-PI_B2_F * 0.5), s);
                s = u.mul_add($f32x::splat(-PI_C2_F * 0.5), s);
            } else if d.abs().lt($f32x::splat(TRIGRANGEMAX_F)).all() {
                q = (d * $f32x::splat(M_2_PI_F)).rinti();
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
                    d.isinf() | d.isnan(),
                    $u32x::from_bits(s),
                ));
            }

            let t = s;

            s = s * s;

            let u = $f32x::splat(-0.000195169282960705459117889)
                .mul_add(s, $f32x::splat(0.00833215750753879547119141))
                .mul_add(s, $f32x::splat(-0.166666537523269653320312));

            let rx = (u * s).mul_add(t, t);
            let rx = visnegzero_vo_vf(d).select($f32x::splat(-0.), rx);

            let u = $f32x::splat(-2.71811842367242206819355e-07)
                .mul_add(s, $f32x::splat(2.47990446951007470488548e-05))
                .mul_add(s, $f32x::splat(-0.00138888787478208541870117))
                .mul_add(s, $f32x::splat(0.0416666641831398010253906))
                .mul_add(s, $f32x::splat(-0.5));

            let ry = s.mul_add(u, $f32x::splat(1.));

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

        pub fn xsincosf_u1(d: $f32x) -> ($f32x, $f32x) {
            let q: $i32x;
            let mut s: F2<$f32x>;

            if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                let u = (d * $f32x::splat(2. * M_1_PI_F)).rint();
                q = u.rinti();
                let v = u.mul_add($f32x::splat(-PI_A2_F * 0.5), d);
                s = v.add_as_f2(u * $f32x::splat(-PI_B2_F * 0.5));
                s = s.add_checked(u * $f32x::splat(-PI_C2_F * 0.5));
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                s = dfidf;
                let o = d.isinf() | d.isnan();
                s.0 = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(s.0)));
            }

            let t = s;

            s.0 = s.square_as_f();

            let u = $f32x::splat(-0.000195169282960705459117889)
                .mul_add(s.0, $f32x::splat(0.00833215750753879547119141))
                .mul_add(s.0, $f32x::splat(-0.166666537523269653320312))
                * (s.0 * t.0);

            let x = t.add_checked(u);
            let rx = x.0 + x.1;

            let rx = visnegzero_vo_vf(d).select($f32x::splat(-0.), rx);

            let u = $f32x::splat(-2.71811842367242206819355e-07)
                .mul_add(s.0, $f32x::splat(2.47990446951007470488548e-05))
                .mul_add(s.0, $f32x::splat(-0.00138888787478208541870117))
                .mul_add(s.0, $f32x::splat(0.0416666641831398010253906))
                .mul_add(s.0, $f32x::splat(-0.5));

            let x = $f32x::splat(1.).add_checked(s.0.mul_as_f2(u));
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

        pub fn xsincospif_u05(d: $f32x) -> ($f32x, $f32x) {
            let u = d * $f32x::splat(4.);
            let q = u.truncatei();
            let q = (q + ($i32x::from_bits($u32x::from_bits(q) >> 31) ^ $i32x::splat(1)))
                & $i32x::splat(!1);
            let s = u - $f32x::from_cast(q);

            let t = s;
            let s = s * s;
            let s2 = t.mul_as_f2(t);

            //

            let u = $f32x::splat(0.3093842054e-6)
                .mul_add(s, $f32x::splat(-0.3657307388e-4))
                .mul_add(s, $f32x::splat(0.2490393585e-2));
            let mut x = u * s + F2::from((-0.080745510756969451904, -1.3373665339076936258e-09));
            x = s2 * x + F2::from((0.78539818525314331055, -2.1857338617566484855e-08));

            x *= t;
            let rx = x.0 + x.1;

            let rx = visnegzero_vo_vf(d).select($f32x::splat(-0.), rx);

            //

            let u = $f32x::splat(-0.2430611801e-7)
                .mul_add(s, $f32x::splat(0.3590577080e-5))
                .mul_add(s, $f32x::splat(-0.3259917721e-3));
            x = u * s + F2::from((0.015854343771934509277, 4.4940051354032242811e-10));
            x = s2 * x + F2::from((-0.30842512845993041992, -9.0728339030733922277e-09));

            x = x * s2 + $f32x::splat(1.);
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

            let o = d.isinf();
            rsin = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rsin)));
            rcos = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rcos)));

            (rsin, rcos)
        }

        pub fn xsincospif_u35(d: $f32x) -> ($f32x, $f32x) {
            let u = d * $f32x::splat(4.);
            let q = u.truncatei();
            let q = (q + ($i32x::from_bits($u32x::from_bits(q) >> 31) ^ $i32x::splat(1)))
                & $i32x::splat(!1);
            let s = u - $f32x::from_cast(q);

            let t = s;
            let s = s * s;

            //

            let u = $f32x::splat(-0.3600925265e-4)
                .mul_add(s, $f32x::splat(0.2490088111e-2))
                .mul_add(s, $f32x::splat(-0.8074551076e-1))
                .mul_add(s, $f32x::splat(0.7853981853e+0));

            let rx = u * t;

            //

            let u = $f32x::splat(0.3539815225e-5)
                .mul_add(s, $f32x::splat(-0.3259574005e-3))
                .mul_add(s, $f32x::splat(0.1585431583e-1))
                .mul_add(s, $f32x::splat(-0.3084251285e+0))
                .mul_add(s, $f32x::splat(1.));

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

            let o = d.isinf();
            rsin = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rsin)));
            rcos = $f32x::from_bits(vor_vm_vo32_vm(o, $u32x::from_bits(rcos)));

            (rsin, rcos)
        }

        pub fn xmodff(x: $f32x) -> ($f32x, $f32x) {
            let fr = x - $f32x::from_cast(x.truncatei());
            let fr = x.abs().gt($f32x::splat(F1_23)).select($f32x::splat(0.), fr);
            (vcopysign_vf_vf_vf(fr, x), vcopysign_vf_vf_vf(x - fr, x))
        }

        pub fn xtanf_u1(d: $f32x) -> $f32x {
            let q: $i32x;

            let mut s = if d.abs().lt($f32x::splat(TRIGRANGEMAX2_F)).all() {
                let u = (d * $f32x::splat(2. * M_1_PI_F)).rint();
                q = u.rinti();
                let v = u.mul_add($f32x::splat(-PI_A2_F * 0.5), d);
                v.add_as_f2(u * $f32x::splat(-PI_B2_F * 0.5))
                    .add_checked(u * $f32x::splat(-PI_C2_F * 0.5))
            } else {
                let (dfidf, dfii) = rempif(d);
                q = dfii;
                let o = d.isinf() | d.isnan();
                F2::new(
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

            let u = $f32x::splat(0.00446636462584137916564941)
                .mul_add(s.0, $f32x::splat(-8.3920182078145444393158e-05))
                .mul_add(s.0, $f32x::splat(0.0109639242291450500488281))
                .mul_add(s.0, $f32x::splat(0.0212360303848981857299805))
                .mul_add(s.0, $f32x::splat(0.0540687143802642822265625));

            let mut x = $f32x::splat(0.133325666189193725585938).add_checked_as_f2(u * s.0);
            x = $f32x::splat(1.)
                .add_checked($f32x::splat(0.33333361148834228515625).add_checked(s * x) * s);
            x = t * x;

            x = vsel_vf2_vo_vf2_vf2(o, x.recpre(), x);

            let u = x.0 + x.1;

            visnegzero_vo_vf(d).select(d, u)
        }

        pub fn xatanf(d: $f32x) -> $f32x {
            let q = vsel_vi2_vf_vi2(d, $i32x::splat(2));
            let s = d.abs();

            let q = vsel_vi2_vf_vf_vi2_vi2($f32x::splat(1.), s, q + $i32x::splat(1), q);
            let s = $f32x::splat(1.).lt(s).select(s.recpre(), s);

            let mut t = s * s;

            let u = $f32x::splat(0.00282363896258175373077393)
                .mul_add(t, $f32x::splat(-0.0159569028764963150024414))
                .mul_add(t, $f32x::splat(0.0425049886107444763183594))
                .mul_add(t, $f32x::splat(-0.0748900920152664184570312))
                .mul_add(t, $f32x::splat(0.106347933411598205566406))
                .mul_add(t, $f32x::splat(-0.142027363181114196777344))
                .mul_add(t, $f32x::splat(0.199926957488059997558594))
                .mul_add(t, $f32x::splat(-0.333331018686294555664062));

            t = s.mul_add(t * u, s);

            t = (q & $i32x::splat(1))
                .eq($i32x::splat(1))
                .select($f32x::splat(M_PI_F / 2.) - t, t);

            t = $f32x::from_bits(
                vand_vm_vo32_vm(
                    (q & $i32x::splat(2)).eq($i32x::splat(2)),
                    $u32x::from_bits($f32x::splat(-0.)),
                ) ^ $u32x::from_bits(t),
            );

            if cfg!(feature = "enable_neon32") || cfg!(feature = "enable_neon32vfpv4") {
                t = d.isinf().select(
                    vmulsign_vf_vf_vf($f32x::splat(1.5874010519681994747517056), d),
                    t,
                );
            }

            t
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

            let u = $f32x::splat(0.00282363896258175373077393)
                .mul_add(t, $f32x::splat(-0.0159569028764963150024414))
                .mul_add(t, $f32x::splat(0.0425049886107444763183594))
                .mul_add(t, $f32x::splat(-0.0748900920152664184570312))
                .mul_add(t, $f32x::splat(0.106347933411598205566406))
                .mul_add(t, $f32x::splat(-0.142027363181114196777344))
                .mul_add(t, $f32x::splat(0.199926957488059997558594))
                .mul_add(t, $f32x::splat(-0.333331018686294555664062));

            let t = s.mul_add(t * u, s);
            $f32x::from_cast(q).mul_add($f32x::splat(M_PI_F / 2.), t)
        }
        #[inline]
        fn visinf2_vf_vf_vf(d: $f32x, m: $f32x) -> $f32x {
            $f32x::from_bits(vand_vm_vo32_vm(
                d.isinf(),
                vsignbit_vm_vf(d) | $u32x::from_bits(m),
            ))
        }

        pub fn xatan2f(y: $f32x, x: $f32x) -> $f32x {
            let mut r = atan2kf(y.abs(), x);

            r = vmulsign_vf_vf_vf(r, x);
            r = (x.isinf() | x.eq($f32x::splat(0.))).select(
                $f32x::splat(M_PI_F / 2.)
                    - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::splat(M_PI_F / 2.), x)),
                r,
            );
            r = y.isinf().select(
                $f32x::splat(M_PI_F / 2.)
                    - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::splat(M_PI_F / 4.), x)),
                r,
            );

            r = y.eq($f32x::splat(0.)).select(
                $f32x::from_bits(vand_vm_vo32_vm(
                    vsignbit_vo_vf(x),
                    $u32x::from_bits($f32x::splat(M_PI_F)),
                )),
                r,
            );

            $f32x::from_bits(vor_vm_vo32_vm(
                x.isnan() | y.isnan(),
                $u32x::from_bits(vmulsign_vf_vf_vf(r, y)),
            ))
        }

        pub fn xasinf(d: $f32x) -> $f32x {
            let o = d.abs().lt($f32x::splat(0.5));
            let x2 = o.select(d * d, ($f32x::splat(1.) - d.abs()) * $f32x::splat(0.5));
            let x = o.select(d.abs(), x2.sqrt());

            let u = $f32x::splat(0.4197454825e-1)
                .mul_add(x2, $f32x::splat(0.2424046025e-1))
                .mul_add(x2, $f32x::splat(0.4547423869e-1))
                .mul_add(x2, $f32x::splat(0.7495029271e-1))
                .mul_add(x2, $f32x::splat(0.1666677296e+0))
                .mul_add(x * x2, x);

            let r = o.select(u, u.mul_add($f32x::splat(-2.), $f32x::splat(M_PI_F / 2.)));
            vmulsign_vf_vf_vf(r, d)
        }

        pub fn xacosf(d: $f32x) -> $f32x {
            let o = d.abs().lt($f32x::splat(0.5));
            let x2 = o.select(d * d, ($f32x::splat(1.) - d.abs()) * $f32x::splat(0.5));
            let mut x = o.select(d.abs(), x2.sqrt());
            x = d.abs().eq($f32x::splat(1.)).select($f32x::splat(0.), x);

            let u = $f32x::splat(0.4197454825e-1)
                .mul_add(x2, $f32x::splat(0.2424046025e-1))
                .mul_add(x2, $f32x::splat(0.4547423869e-1))
                .mul_add(x2, $f32x::splat(0.7495029271e-1))
                .mul_add(x2, $f32x::splat(0.1666677296e+0))
                * (x2 * x);

            let y = $f32x::splat(3.1415926535897932 / 2.)
                - (vmulsign_vf_vf_vf(x, d) + vmulsign_vf_vf_vf(u, d));
            x = x + u;
            let r = o.select(y, x * $f32x::splat(2.));
            vandnot_vo_vo_vo(o, d.lt($f32x::splat(0.))).select(
                F2::from((3.1415927410125732422, -8.7422776573475857731e-08))
                    .add_checked(-r)
                    .0,
                r,
            )
        }

        //
        #[inline]
        fn atan2kf_u1(y: F2<$f32x>, mut x: F2<$f32x>) -> F2<$f32x> {
            let q =
                vsel_vi2_vf_vf_vi2_vi2(x.0, $f32x::splat(0.), $i32x::splat(-2), $i32x::splat(0));
            let p = x.0.lt($f32x::splat(0.));
            let r = vand_vm_vo32_vm(p, $u32x::from_bits($f32x::splat(-0.)));
            x.0 = $f32x::from_bits($u32x::from_bits(x.0) ^ r);
            x.1 = $f32x::from_bits($u32x::from_bits(x.1) ^ r);

            let q = vsel_vi2_vf_vf_vi2_vi2(x.0, y.0, q + $i32x::splat(1), q);
            let p = x.0.lt(y.0);
            let s = vsel_vf2_vo_vf2_vf2(p, -x, y);
            let mut t = vsel_vf2_vo_vf2_vf2(p, y, x);

            let s = s / t;
            t = s.square();
            t = t.normalize();

            let u = $f32x::splat(-0.00176397908944636583328247)
                .mul_add(t.0, $f32x::splat(0.0107900900766253471374512))
                .mul_add(t.0, $f32x::splat(-0.0309564601629972457885742))
                .mul_add(t.0, $f32x::splat(0.0577365085482597351074219))
                .mul_add(t.0, $f32x::splat(-0.0838950723409652709960938))
                .mul_add(t.0, $f32x::splat(0.109463557600975036621094))
                .mul_add(t.0, $f32x::splat(-0.142626821994781494140625))
                .mul_add(t.0, $f32x::splat(0.199983194470405578613281));

            t *= $f32x::splat(-0.333332866430282592773438).add_checked_as_f2(u * t.0);
            t = s * $f32x::splat(1.).add_checked(t);
            (F2::from((1.5707963705062866211, -4.3711388286737928865e-08)) * $f32x::from_cast(q))
                .add_checked(t)
        }

        pub fn xatan2f_u1(mut y: $f32x, mut x: $f32x) -> $f32x {
            let o = x.abs().lt($f32x::splat(2.9387372783541830947e-39)); // nexttowardf((1.0 / FLT_MAX), 1)
            x = o.select(x * $f32x::splat(F1_24), x);
            y = o.select(y * $f32x::splat(F1_24), y);

            let d = atan2kf_u1(
                F2::new(y.abs(), $f32x::splat(0.)),
                F2::new(x, $f32x::splat(0.)),
            );
            let mut r = d.0 + d.1;

            r = vmulsign_vf_vf_vf(r, x);
            r = (x.isinf() | x.eq($f32x::splat(0.))).select(
                $f32x::splat(M_PI_F / 2.)
                    - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::splat(M_PI_F / 2.), x)),
                r,
            );
            r = y.isinf().select(
                $f32x::splat(M_PI_F / 2.)
                    - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::splat(M_PI_F / 4.), x)),
                r,
            );
            r = y.eq($f32x::splat(0.)).select(
                $f32x::from_bits(vand_vm_vo32_vm(
                    vsignbit_vo_vf(x),
                    $u32x::from_bits($f32x::splat(M_PI_F)),
                )),
                r,
            );

            $f32x::from_bits(vor_vm_vo32_vm(
                x.isnan() | y.isnan(),
                $u32x::from_bits(vmulsign_vf_vf_vf(r, y)),
            ))
        }

        pub fn xasinf_u1(d: $f32x) -> $f32x {
            let o = d.abs().lt($f32x::splat(0.5));
            let x2 = o.select(d * d, ($f32x::splat(1.) - d.abs()) * $f32x::splat(0.5));
            let mut x = vsel_vf2_vo_vf2_vf2(o, F2::new(d.abs(), $f32x::splat(0.)), x2.sqrt_as_f2());
            x = vsel_vf2_vo_vf2_vf2(d.abs().eq($f32x::splat(1.)), F2::from((0., 0.)), x);

            let u = $f32x::splat(0.4197454825e-1)
                .mul_add(x2, $f32x::splat(0.2424046025e-1))
                .mul_add(x2, $f32x::splat(0.4547423869e-1))
                .mul_add(x2, $f32x::splat(0.7495029271e-1))
                .mul_add(x2, $f32x::splat(0.1666677296e+0))
                * (x2 * x.0);

            let y = F2::from((3.1415927410125732422 / 4., -8.7422776573475857731e-08 / 4.))
                .sub_checked(x)
                .sub_checked(u);

            let r = o.select(u + x.0, (y.0 + y.1) * $f32x::splat(2.));
            vmulsign_vf_vf_vf(r, d)
        }

        pub fn xacosf_u1(d: $f32x) -> $f32x {
            let o = d.abs().lt($f32x::splat(0.5));
            let x2 = o.select(d * d, ($f32x::splat(1.) - d.abs()) * $f32x::splat(0.5));

            let mut x = vsel_vf2_vo_vf2_vf2(o, F2::new(d.abs(), $f32x::splat(0.)), x2.sqrt_as_f2());
            x = vsel_vf2_vo_vf2_vf2(d.abs().eq($f32x::splat(1.)), F2::from((0., 0.)), x);

            let u = $f32x::splat(0.4197454825e-1)
                .mul_add(x2, $f32x::splat(0.2424046025e-1))
                .mul_add(x2, $f32x::splat(0.4547423869e-1))
                .mul_add(x2, $f32x::splat(0.7495029271e-1))
                .mul_add(x2, $f32x::splat(0.1666677296e+0))
                * (x2 * x.0);

            let mut y = F2::from((3.1415927410125732422 / 2., -8.7422776573475857731e-08 / 2.))
                .sub_checked(vmulsign_vf_vf_vf(x.0, d).add_checked_as_f2(vmulsign_vf_vf_vf(u, d)));
            x = x.add_checked(u);

            y = vsel_vf2_vo_vf2_vf2(o, y, x.scale($f32x::splat(2.)));

            y = vsel_vf2_vo_vf2_vf2(
                vandnot_vo_vo_vo(o, d.lt($f32x::splat(0.))),
                F2::from((3.1415927410125732422, -8.7422776573475857731e-08)).sub_checked(y),
                y,
            );

            y.0 + y.1
        }

        pub fn xatanf_u1(d: $f32x) -> $f32x {
            let d2 = atan2kf_u1(F2::new(d.abs(), $f32x::splat(0.)), F2::from((1., 0.)));
            let mut r = d2.0 + d2.1;
            r = d.isinf().select($f32x::splat(1.570796326794896557998982), r);
            vmulsign_vf_vf_vf(r, d)
        }

        //

        pub fn xlogf(mut d: $f32x) -> $f32x {
            let m: $f32x;

            let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.lt($f32x::splat(f32::MIN));
                d = o.select(d * $f32x::splat(F1_32 * F1_32), d);
                let mut e = vilogb2k_vi2_vf(d * $f32x::splat(1. / 0.75));
                m = vldexp3_vf_vf_vi2(d, -e);
                e = o.select(e - $i32x::splat(64), e);
                $f32x::from_cast(e)
            }/* else {
                let mut e = vgetexp_vf_vf(d * $f32x::splat(1. / 0.75));
                e = e.ispinf().select($f32x::splat(128.), e);
                m = vgetmant_vf_vf(d);
                e
            }*/;

            let mut x = ($f32x::splat(-1.) + m) / ($f32x::splat(1.) + m);
            let x2 = x * x;

            let t = $f32x::splat(0.2392828464508056640625)
                .mul_add(x2, $f32x::splat(0.28518211841583251953125))
                .mul_add(x2, $f32x::splat(0.400005877017974853515625))
                .mul_add(x2, $f32x::splat(0.666666686534881591796875))
                .mul_add(x2, $f32x::splat(2.));

            x = x.mul_add(t, $f32x::splat(0.693147180559945286226764) * ef);
            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                x = d.ispinf().select($f32x::splat(SLEEF_INFINITY_F), x);
                x = (d.lt($f32x::splat(0.)) | d.isnan()).select($f32x::splat(SLEEF_NAN_F), x);
                d.eq($f32x::splat(0.))
                    .select($f32x::splat(-SLEEF_INFINITY_F), x)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(x, d, $i32x::splat(5 << (5 * 4)), 0)
            }*/
        }

        pub fn xexpf(d: $f32x) -> $f32x {
            let q = (d * $f32x::splat(R_LN2_F)).rinti();

            let s = $f32x::from_cast(q).mul_add($f32x::splat(-L2U_F), d);
            let s = $f32x::from_cast(q).mul_add($f32x::splat(-L2L_F), s);

            let mut u = $f32x::splat(0.000198527617612853646278381)
                .mul_add(s, $f32x::splat(0.00139304355252534151077271))
                .mul_add(s, $f32x::splat(0.00833336077630519866943359))
                .mul_add(s, $f32x::splat(0.0416664853692054748535156))
                .mul_add(s, $f32x::splat(0.166666671633720397949219))
                .mul_add(s, $f32x::splat(0.5));

            u = $f32x::splat(1.) + (s * s).mul_add(u, s);

            u = vldexp2_vf_vf_vi2(u, q);

            u = $f32x::from_bits(vandnot_vm_vo32_vm(
                d.lt($f32x::splat(-104.)),
                $u32x::from_bits(u),
            ));
            $f32x::splat(100.)
                .lt(d)
                .select($f32x::splat(SLEEF_INFINITY_F), u)
        }
        #[inline]
        fn expm1fk(d: $f32x) -> $f32x {
            let q = (d * $f32x::splat(R_LN2_F)).rinti();
            let s = $f32x::from_cast(q).mul_add($f32x::splat(-L2U_F), d);
            let s = $f32x::from_cast(q).mul_add($f32x::splat(-L2L_F), s);

            let u = $f32x::splat(0.000198527617612853646278381)
                .mul_add(s, $f32x::splat(0.00139304355252534151077271))
                .mul_add(s, $f32x::splat(0.00833336077630519866943359))
                .mul_add(s, $f32x::splat(0.0416664853692054748535156))
                .mul_add(s, $f32x::splat(0.166666671633720397949219))
                .mul_add(s, $f32x::splat(0.5));

            let u = (s * s).mul_add(u, s);

            q.eq($i32x::splat(0)).select(
                u,
                vldexp2_vf_vf_vi2(u + $f32x::splat(1.), q) - $f32x::splat(1.),
            )
        }

        #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
        pub fn xsqrtf_u35(d: $f32x) -> $f32x {
            let e = $f32x::from_bits(
                $u32x::splat(0x20000000)
                    + ($u32x::splat(0x7f000000) & ($u32x::from_bits($i32x::from_cast(d)) >> 1)),
            );
            let m = $f32x::from_bits(
                $i32x::splat(0x3f000000) + ($i32x::splat(0x01ffffff) & $i32x::from_cast(d)),
            );
            let mut x = vrsqrteq_f32(m);
            x = vmulq_f32(x, vrsqrtsq_f32(m, vmulq_f32(x, x)));
            let mut u = vmulq_f32(x, m);
            u = vmlaq_f32(u, vmlsq_f32(m, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
            e = $f32x::from_bits(vandnot_vm_vo32_vm(
                d.eq($f32x::splat(0.)),
                $u32x::from_bits(e),
            ));
            u = e * u;

            u = d.isinf().select($f32x::splat(SLEEF_INFINITY_F), u);
            u = $f32x::from_bits(vor_vm_vo32_vm(
                d.isnan() | d.lt($f32x::splat(0.)),
                $u32x::from_bits(u),
            ));
            vmulsign_vf_vf_vf(u, d)
        }
        /*#[cfg(feature = "enable_vecext")]
                pub fn xsqrtf_u35(d: $f32x) -> $f32x {
                    let mut q = d.sqrt();
                    q = visnegzero_vo_vf(d).select($f32x::splat(-0.), q);
                    d.ispinf().select($f32x::splat(SLEEF_INFINITY_F), q)
                }*/
        #[cfg(all(
                    not(feature = "enable_neon32"),
                    not(feature = "enable_neon32vfpv4"),
                //    not(feature = "enable_vecext")
                ))]
        pub fn xsqrtf_u35(d: $f32x) -> $f32x {
            d.sqrt()
        }

        pub fn xcbrtf(mut d: $f32x) -> $f32x {
            let mut q = $f32x::splat(1.);

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
                .select($f32x::splat(1.2599210498948731647672106), q);
            q = re
                .eq($i32x::splat(2))
                .select($f32x::splat(1.5874010519681994747517056), q);
            q = vldexp2_vf_vf_vi2(q, qu - $i32x::splat(2048));

            q = vmulsign_vf_vf_vf(q, d);
            d = d.abs();

            let x = $f32x::splat(-0.601564466953277587890625)
                .mul_add(d, $f32x::splat(2.8208892345428466796875))
                .mul_add(d, $f32x::splat(-5.532182216644287109375))
                .mul_add(d, $f32x::splat(5.898262500762939453125))
                .mul_add(d, $f32x::splat(-3.8095417022705078125))
                .mul_add(d, $f32x::splat(2.2241256237030029296875));

            let mut y = d * x * x;
            y = (y - $f32x::splat(2. / 3.) * y * y.mul_add(x, $f32x::splat(-1.))) * q;

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                y = s.isinf().select(vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), s), y);
                y = s
                    .eq($f32x::splat(0.))
                    .select(vmulsign_vf_vf_vf($f32x::splat(0.), s), y);
            }*/

            y
        }

        pub fn xcbrtf_u1(mut d: $f32x) -> $f32x {
            let mut q2 = F2::from((1., 0.));

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                let s = d;
            }*/
            let e = vilogbk_vi2_vf(d.abs()) + $i32x::splat(1);
            d = vldexp2_vf_vf_vi2(d, -e);

            let t = $f32x::from_cast(e) + $f32x::splat(6144.);
            let qu = (t * $f32x::splat(1. / 3.)).truncatei();
            let re = (t - $f32x::from_cast(qu) * $f32x::splat(3.)).truncatei();

            q2 = vsel_vf2_vo_vf2_vf2(
                re.eq($i32x::splat(1)),
                F2::from((1.2599210739135742188, -2.4018701694217270415e-08)),
                q2,
            );
            q2 = vsel_vf2_vo_vf2_vf2(
                re.eq($i32x::splat(2)),
                F2::from((1.5874010324478149414, 1.9520385308169352356e-08)),
                q2,
            );

            q2.0 = vmulsign_vf_vf_vf(q2.0, d);
            q2.1 = vmulsign_vf_vf_vf(q2.1, d);
            d = d.abs();

            let mut x = $f32x::splat(-0.601564466953277587890625)
                .mul_add(d, $f32x::splat(2.8208892345428466796875))
                .mul_add(d, $f32x::splat(-5.532182216644287109375))
                .mul_add(d, $f32x::splat(5.898262500762939453125))
                .mul_add(d, $f32x::splat(-3.8095417022705078125))
                .mul_add(d, $f32x::splat(2.2241256237030029296875));

            let mut y = x * x;
            y = y * y;
            x -= vmlanp_vf_vf_vf_vf(d, y, x) * $f32x::splat(-1. / 3.);

            let mut z = x;

            let mut u = x.mul_as_f2(x);
            u = u * u;
            u *= d;
            u += -x;
            y = u.0 + u.1;

            y = $f32x::splat(-2. / 3.) * y * z;
            let mut v = z.mul_as_f2(z) + y;
            v *= d;
            v *= q2;
            z = vldexp2_vf_vf_vi2(v.0 + v.1, qu - $i32x::splat(2048));

            z = d.isinf().select(vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), q2.0), z);
            z = d
                .eq($f32x::splat(0.))
                .select($f32x::from_bits(vsignbit_vm_vf(q2.0)), z);

            /*if cfg!(feature = "enable_avx512f") || cfg!(feature = "enable_avx512fnofma") {
                z = s.isinf().select(vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), s), z);
                z = s
                    .eq($f32x::splat(0.))
                    .select(vmulsign_vf_vf_vf($f32x::splat(0.), s), z);
            }*/

            z
        }
        #[inline]
        fn logkf(mut d: $f32x) -> F2<$f32x> {
            let m: $f32x;

            let ef = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.lt($f32x::splat(f32::MIN));
                d = o.select(d * $f32x::splat(F1_32 * F1_32), d);
                let mut e = vilogb2k_vi2_vf(d * $f32x::splat(1. / 0.75));
                m = vldexp3_vf_vf_vi2(d, -e);
                e = o.select(e - $i32x::splat(64), e);
                $f32x::from_cast(e)
            }/* else {
                let mut e = vgetexp_vf_vf(d * $f32x::splat(1. / 0.75));
                e = e.ispinf().select($f32x::splat(128.), e);
                m = vgetmant_vf_vf(d);
                e
            }*/;

            let x = $f32x::splat(-1.).add_as_f2(m) / $f32x::splat(1.).add_as_f2(m);
            let x2 = x.square();

            let t = $f32x::splat(0.240320354700088500976562)
                .mul_add(x2.0, $f32x::splat(0.285112679004669189453125))
                .mul_add(x2.0, $f32x::splat(0.400007992982864379882812));
            let c = F2::from((0.66666662693023681640625, 3.69183861259614332084311e-09));

            let mut s = F2::from((0.69314718246459960938, -1.904654323148236017e-09)) * ef;

            s = s.add_checked(x.scale($f32x::splat(2.)));
            s.add_checked(x2 * x * (x2 * t + c))
        }

        pub fn xlogf_u1(mut d: $f32x) -> $f32x {
            let m: $f32x;

            let mut s = /*if !cfg!(feature = "enable_avx512f")
                && !cfg!(feature = "enable_avx512fnofma")*/
            {
                let o = d.lt($f32x::splat(f32::MIN));
                d = o.select(d * $f32x::splat(F1_32 * F1_32), d);
                let mut e = vilogb2k_vi2_vf(d * $f32x::splat(1. / 0.75));
                m = vldexp3_vf_vf_vi2(d, -e);
                e = o.select(e - $i32x::splat(64), e);
                F2::from((0.69314718246459960938, -1.904654323148236017e-09)) * $f32x::from_cast(e)
            }/* else {
                let mut e = vgetexp_vf_vf(d * $f32x::splat(1. / 0.75));
                e = e.ispinf().select($f32x::splat(128.), e);
                m = vgetmant_vf_vf(d);
                F2::from((0.69314718246459960938, -1.904654323148236017e-09)) * e
            }*/;

            let x = $f32x::splat(-1.).add_as_f2(m) / $f32x::splat(1.).add_as_f2(m);
            let x2 = x.0 * x.0;

            let t = $f32x::splat(0.3027294874e+0)
                .mul_add(x2, $f32x::splat(0.3996108174e+0))
                .mul_add(x2, $f32x::splat(0.6666694880e+0));

            s = s.add_checked(x.scale($f32x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                let r = d.ispinf().select($f32x::splat(SLEEF_INFINITY_F), r);
                let r =
                    (d.lt($f32x::splat(0.)) | d.isnan()).select($f32x::splat(SLEEF_NAN_F), r);
                d.eq($f32x::splat(0.))
                    .select($f32x::splat(-SLEEF_INFINITY_F), r)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(
                    r,
                    d,
                    $i32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }
        #[inline]
        fn expkf(d: F2<$f32x>) -> $f32x {
            let u = (d.0 + d.1) * $f32x::splat(R_LN2_F);
            let q = u.rinti();

            let mut s = d + $f32x::from_cast(q) * $f32x::splat(-L2U_F);
            s += $f32x::from_cast(q) * $f32x::splat(-L2L_F);

            s = s.normalize();

            let mut u = $f32x::splat(0.00136324646882712841033936)
                .mul_add(s.0, $f32x::splat(0.00836596917361021041870117))
                .mul_add(s.0, $f32x::splat(0.0416710823774337768554688))
                .mul_add(s.0, $f32x::splat(0.166665524244308471679688))
                .mul_add(s.0, $f32x::splat(0.499999850988388061523438));

            let mut t = s.add_checked(s.square() * u);

            t = $f32x::splat(1.).add_checked(t);
            u = t.0 + t.1;
            u = vldexp_vf_vf_vi2(u, q);

            $f32x::from_bits(vandnot_vm_vo32_vm(
                d.0.lt($f32x::splat(-104.)),
                $u32x::from_bits(u),
            ))
        }

        pub fn xpowf(x: $f32x, y: $f32x) -> $f32x {
            if true {
                let yisint = y.truncate().eq(y) | y.abs().gt($f32x::splat(F1_24));
                let yisodd = (y.truncatei() & $i32x::splat(1)).eq($i32x::splat(1))
                    & yisint
                    & y.abs().lt($f32x::splat(F1_24));

                #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
                {
                    let yisodd = vandnot_vm_vo32_vm(y.isinf(), yisodd);
                }

                let mut result = expkf(logkf(x.abs()) * y);

                result = result.isnan().select($f32x::splat(SLEEF_INFINITY_F), result);

                result *= x.gt($f32x::splat(0.)).select(
                    $f32x::splat(1.),
                    yisint.select(
                        yisodd.select($f32x::splat(-1.), $f32x::splat(1.)),
                        $f32x::splat(SLEEF_NAN_F),
                    ),
                );

                let efx = vmulsign_vf_vf_vf(x.abs() - $f32x::splat(1.), y);

                result = y.isinf().select(
                    $f32x::from_bits(vandnot_vm_vo32_vm(
                        efx.lt($f32x::splat(0.)),
                        $u32x::from_bits(
                            efx.eq($f32x::splat(0.))
                                .select($f32x::splat(1.), $f32x::splat(SLEEF_INFINITY_F)),
                        ),
                    )),
                    result,
                );

                result = (x.isinf() | x.eq($f32x::splat(0.))).select(
                    yisodd.select(vsign_vf_vf(x), $f32x::splat(1.)) * $f32x::from_bits(
                        vandnot_vm_vo32_vm(
                            x.eq($f32x::splat(0.)).select(-y, y).lt($f32x::splat(0.)),
                            $u32x::from_bits($f32x::splat(SLEEF_INFINITY_F)),
                        ),
                    ),
                    result,
                );

                result = $f32x::from_bits(vor_vm_vo32_vm(
                    x.isnan() | y.isnan(),
                    $u32x::from_bits(result),
                ));

                (y.eq($f32x::splat(0.)) | x.eq($f32x::splat(1.)))
                    .select($f32x::splat(1.), result)
            } else {
                expkf(logkf(x) * y)
            }
        }
        #[inline]
        fn expk2f(d: F2<$f32x>) -> F2<$f32x> {
            let u = (d.0 + d.1) * $f32x::splat(R_LN2_F);
            let q = u.rinti();

            let mut s = d + $f32x::from_cast(q) * $f32x::splat(-L2U_F);
            s += $f32x::from_cast(q) * $f32x::splat(-L2L_F);

            let u = $f32x::splat(0.1980960224e-3)
                .mul_add(s.0, $f32x::splat(0.1394256484e-2))
                .mul_add(s.0, $f32x::splat(0.8333456703e-2))
                .mul_add(s.0, $f32x::splat(0.4166637361e-1));

            let mut t = s * u + $f32x::splat(0.166666659414234244790680580464e+0);
            t = s * t + $f32x::splat(0.5);
            t = s + s.square() * t;

            t = $f32x::splat(1.).add_checked(t);

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

        pub fn xsinhf(x: $f32x) -> $f32x {
            let mut y = x.abs();
            let d = expk2f(F2::new(y, $f32x::splat(0.)));
            let d = d.sub_checked(d.recpre());
            y = (d.0 + d.1) * $f32x::splat(0.5);

            y = (x.abs().gt($f32x::splat(89.)) | y.isnan())
                .select($f32x::splat(SLEEF_INFINITY_F), y);
            y = vmulsign_vf_vf_vf(y, x);
            $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)))
        }

        pub fn xcoshf(x: $f32x) -> $f32x {
            let mut y = x.abs();
            let d = expk2f(F2::new(y, $f32x::splat(0.)));
            let d = d.add_checked(d.recpre());
            y = (d.0 + d.1) * $f32x::splat(0.5);

            y = (x.abs().gt($f32x::splat(89.)) | y.isnan())
                .select($f32x::splat(SLEEF_INFINITY_F), y);
            $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)))
        }

        pub fn xtanhf(x: $f32x) -> $f32x {
            let mut y = x.abs();
            let d = expk2f(F2::new(y, $f32x::splat(0.)));
            let e = d.recpre();
            let d = d.add_checked(-e) / d.add_checked(e);
            y = d.0 + d.1;

            y = (x.abs().gt($f32x::splat(8.664339742)) | y.isnan())
                .select($f32x::splat(1.), y);
            y = vmulsign_vf_vf_vf(y, x);
            $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)))
        }

        pub fn xsinhf_u35(x: $f32x) -> $f32x {
            let e = expm1fk(x.abs());
            let mut y = (e + $f32x::splat(2.)) / (e + $f32x::splat(1.));
            y *= $f32x::splat(0.5) * e;

            y = (x.abs().gt($f32x::splat(88.)) | y.isnan())
                .select($f32x::splat(SLEEF_INFINITY_F), y);
            y = vmulsign_vf_vf_vf(y, x);
            $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)))
        }

        pub fn xcoshf_u35(x: $f32x) -> $f32x {
            let e = xexpf(x.abs());
            let mut y = $f32x::splat(0.5).mul_add(e, $f32x::splat(0.5) / e);

            y = (x.abs().gt($f32x::splat(88.)) | y.isnan())
                .select($f32x::splat(SLEEF_INFINITY_F), y);
            $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)))
        }

        pub fn xtanhf_u35(x: $f32x) -> $f32x {
            let d = expm1fk($f32x::splat(2.) * x.abs());
            let mut y = d / ($f32x::splat(2.) + d);

            y = (x.abs().gt($f32x::splat(8.664339742)) | y.isnan())
                .select($f32x::splat(1.), y);
            y = vmulsign_vf_vf_vf(y, x);
            $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)))
        }
        #[inline]
        fn logk2f(d: F2<$f32x>) -> F2<$f32x> {
            let e = /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                vilogbk_vi2_vf(d.0 * $f32x::splat(1. / 0.75))
            /*} else {
                vgetexp_vf_vf(d.0 * $f32x::splat(1. / 0.75)).rinti()
            }*/;
            let m = d.scale(vpow2i_vf_vi2(-e));

            let x = (m + $f32x::splat(-1.)) / (m + $f32x::splat(1.));
            let x2 = x.square();

            let t = $f32x::splat(0.2392828464508056640625)
                .mul_add(x2.0, $f32x::splat(0.28518211841583251953125))
                .mul_add(x2.0, $f32x::splat(0.400005877017974853515625))
                .mul_add(x2.0, $f32x::splat(0.666666686534881591796875));

            let mut s = F2::new(
                $f32x::splat(0.69314718246459960938),
                $f32x::splat(-1.904654323148236017e-09),
            ) * $f32x::from_cast(e);
            s = s.add_checked(x.scale($f32x::splat(2.)));
            s.add_checked(x2 * x * t)
        }

        pub fn xasinhf(x: $f32x) -> $f32x {
            let mut y = x.abs();
            let o = y.gt($f32x::splat(1.));

            let mut d = vsel_vf2_vo_vf2_vf2(o, x.recpre_as_f2(), F2::new(y, $f32x::splat(0.)));
            d = (d.square() + $f32x::splat(1.)).sqrt();
            d = vsel_vf2_vo_vf2_vf2(o, d * y, d);

            d = logk2f((d + x).normalize());
            y = d.0 + d.1;

            y = (x.abs().gt($f32x::splat(SQRT_FLT_MAX)) | y.isnan())
                .select(vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), x), y);
            y = $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)));
            visnegzero_vo_vf(x).select($f32x::splat(-0.), y)
        }

        pub fn xacoshf(x: $f32x) -> $f32x {
            let d = logk2f(
                x.add_as_f2($f32x::splat(1.)).sqrt() * x.add_as_f2($f32x::splat(-1.)).sqrt() + x,
            );
            let mut y = d.0 + d.1;

            y = (x.abs().gt($f32x::splat(SQRT_FLT_MAX)) | y.isnan())
                .select($f32x::splat(SLEEF_INFINITY_F), y);

            y = $f32x::from_bits(vandnot_vm_vo32_vm(
                x.eq($f32x::splat(1.)),
                $u32x::from_bits(y),
            ));

            y = $f32x::from_bits(vor_vm_vo32_vm(x.lt($f32x::splat(1.)), $u32x::from_bits(y)));
            $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)))
        }

        pub fn xatanhf(x: $f32x) -> $f32x {
            let mut y = x.abs();
            let d = logk2f($f32x::splat(1.).add_as_f2(y) / $f32x::splat(1.).add_as_f2(-y));
            y = $f32x::from_bits(vor_vm_vo32_vm(
                y.gt($f32x::splat(1.)),
                $u32x::from_bits(y.eq($f32x::splat(1.)).select(
                    $f32x::splat(SLEEF_INFINITY_F),
                    (d.0 + d.1) * $f32x::splat(0.5),
                )),
            ));

            y = $f32x::from_bits(vor_vm_vo32_vm(
                x.isinf() | y.isnan(),
                $u32x::from_bits(y),
            ));
            y = vmulsign_vf_vf_vf(y, x);
            $f32x::from_bits(vor_vm_vo32_vm(x.isnan(), $u32x::from_bits(y)))
        }

        pub fn xexp2f(d: $f32x) -> $f32x {
            let mut u = d.rint();
            let q = u.rinti();

            let s = d - u;

            u = $f32x::splat(0.1535920892e-3)
                .mul_add(s, $f32x::splat(0.1339262701e-2))
                .mul_add(s, $f32x::splat(0.9618384764e-2))
                .mul_add(s, $f32x::splat(0.5550347269e-1))
                .mul_add(s, $f32x::splat(0.2402264476e+0))
                .mul_add(s, $f32x::splat(0.6931471825e+0));

            if !cfg!(target_feature = "fma") {
                u = u.mul_adde(s, $f32x::splat(1.));
            } else {
                u = $f32x::splat(1.).add_checked(u.mul_as_f2(s)).normalize().0;
            }

            u = vldexp2_vf_vf_vi2(u, q);

            u = d
                .ge($f32x::splat(128.))
                .select($f32x::splat(SLEEF_INFINITY_F), u);
            $f32x::from_bits(vandnot_vm_vo32_vm(
                d.lt($f32x::splat(-150.)),
                $u32x::from_bits(u),
            ))
        }

        pub fn xexp10f(d: $f32x) -> $f32x {
            let mut u = (d * $f32x::splat(LOG10_2_F)).rint();
            let q = u.rinti();

            let s = u.mul_add($f32x::splat(-L10U_F), d);
            let s = u.mul_add($f32x::splat(-L10L_F), s);

            u = $f32x::splat(0.2064004987e+0)
                .mul_add(s, $f32x::splat(0.5417877436e+0))
                .mul_add(s, $f32x::splat(0.1171286821e+1))
                .mul_add(s, $f32x::splat(0.2034656048e+1))
                .mul_add(s, $f32x::splat(0.2650948763e+1))
                .mul_add(s, $f32x::splat(0.2302585125e+1));

            if !cfg!(target_feature = "fma") {
                u = u.mul_adde(s, $f32x::splat(1.));
            } else {
                u = $f32x::splat(1.).add_checked(u.mul_as_f2(s)).normalize().0;
            }

            u = vldexp2_vf_vf_vi2(u, q);

            u = d
                .gt($f32x::splat(38.5318394191036238941387))
                .select($f32x::splat(SLEEF_INFINITY_F), u);
            $f32x::from_bits(vandnot_vm_vo32_vm(
                d.lt($f32x::splat(-50.)),
                $u32x::from_bits(u),
            ))
        }

        pub fn xexpm1f(a: $f32x) -> $f32x {
            let d = expk2f(F2::new(a, $f32x::splat(0.))) + $f32x::splat(-1.);
            let mut x = d.0 + d.1;
            x = a
                .gt($f32x::splat(88.72283172607421875))
                .select($f32x::splat(SLEEF_INFINITY_F), x);
            x = a
                .lt($f32x::splat(-16.635532333438687426013570))
                .select($f32x::splat(-1.), x);
            visnegzero_vo_vf(a).select($f32x::splat(-0.), x)
        }

        pub fn xlog10f(mut d: $f32x) -> $f32x {
            let m: $f32x;

            let mut s =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = d.lt($f32x::splat(f32::MIN));
                    d = o.select(d * $f32x::splat(F1_32 * F1_32), d);
                    let mut e = vilogb2k_vi2_vf(d * $f32x::splat(1. / 0.75));
                    m = vldexp3_vf_vf_vi2(d, -e);
                    e = o.select(e - $i32x::splat(64), e);
                    F2::from((0.30103001, -1.432098889e-08)) * $f32x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vf_vf(d * $f32x::splat(1. / 0.75));
                    e = e.ispinf().select($f32x::splat(128.), e);
                    m = vgetmant_vf_vf(d);
                    F2::from((0.30103001, -1.432098889e-08)) * e
                }*/;

            let x = $f32x::splat(-1.).add_as_f2(m) / $f32x::splat(1.).add_as_f2(m);
            let x2 = x.0 * x.0;

            let t = $f32x::splat(0.1314289868e+0)
                .mul_add(x2, $f32x::splat(0.1735493541e+0))
                .mul_add(x2, $f32x::splat(0.2895309627e+0));

            s = s.add_checked(x * F2::from((0.868588984, -2.170757285e-08)));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                r = d.ispinf().select($f32x::splat(SLEEF_INFINITY_F), r);
                r = (d.lt($f32x::splat(0.)) | d.isnan()).select($f32x::splat(SLEEF_NAN_F), r);
                d.eq($f32x::splat(0.))
                    .select($f32x::splat(-SLEEF_INFINITY_F), r)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(
                    r,
                    d,
                    $i32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        pub fn xlog2f(mut d: $f32x) -> $f32x {
            let m: $f32x;

            let ef =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = d.lt($f32x::splat(f32::MIN));
                    d = o.select(d * $f32x::splat(F1_32 * F1_32), d);
                    let mut e = vilogb2k_vi2_vf(d * $f32x::splat(1. / 0.75));
                    m = vldexp3_vf_vf_vi2(d, -e);
                    e = o.select(e - $i32x::splat(64), e);
                    $f32x::from_cast(e)
                }/* else {
                    let mut e = vgetexp_vf_vf(d * $f32x::splat(1. / 0.75));
                    e = e.ispinf().select($f32x::splat(128.), e);
                    m = vgetmant_vf_vf(d);
                    e
                }*/;

            let x = $f32x::splat(-1.).add_as_f2(m) / $f32x::splat(1.).add_as_f2(m);
            let x2 = x.0 * x.0;

            let t = $f32x::splat(0.4374550283e+0)
                .mul_add(x2, $f32x::splat(0.5764790177e+0))
                .mul_add(x2, $f32x::splat(0.9618012905120));
            let mut s = ef + x * F2::from((2.8853900432586669922, 3.2734474483568488616e-08));
            s += x2 * x.0 * t;

            let mut r = s.0 + s.1;

            /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma") {*/
                r = d.ispinf().select($f32x::splat(SLEEF_INFINITY_F), r);
                r = (d.lt($f32x::splat(0.)) | d.isnan()).select($f32x::splat(SLEEF_NAN_F), r);
                d.eq($f32x::splat(0.))
                    .select($f32x::splat(-SLEEF_INFINITY_F), r)
            /*} else {
                vfixup_vf_vf_vf_vi2_i(
                    r,
                    d,
                    $i32x::splat((4 << (2 * 4)) | (3 << (4 * 4)) | (5 << (5 * 4)) | (2 << (6 * 4))),
                    0,
                )
            }*/
        }

        pub fn xlog1pf(d: $f32x) -> $f32x {
            let m: $f32x;

            let dp1 = d + $f32x::splat(1.);

            let mut s =
                /*if !cfg!(feature = "enable_avx512f") && !cfg!(feature = "enable_avx512fnofma")*/ {
                    let o = dp1.lt($f32x::splat(f32::MIN));
                    let dp1 = o.select(dp1 * $f32x::splat(F1_32 * F1_32), dp1);
                    let e = vilogb2k_vi2_vf(dp1 * $f32x::splat(1. / 0.75));
                    let t = vldexp3_vf_vf_vi2($f32x::splat(1.), -e);
                    m = d.mul_add(t, t - $f32x::splat(1.));
                    let e = o.select(e - $i32x::splat(64), e);
                    F2::from((0.69314718246459960938, -1.904654323148236017e-09)) * $f32x::from_cast(e)
                }/* else {
                    let e = vgetexp_vf_vf(dp1, $f32x::splat(1. / 0.75));
                    let e = e.ispinf().select($f32x::splat(128.), e);
                    let t = vldexp3_vf_vf_vi2($f32x::splat(1.), -e.rinti());
                    m = d.mul_add(t, t - $f32x::splat(1.));
                    F2::from((0.69314718246459960938, -1.904654323148236017e-09)) * e
                }*/;

            let x = F2::new(m, $f32x::splat(0.)) / $f32x::splat(2.).add_checked_as_f2(m);
            let x2 = x.0 * x.0;

            let t = $f32x::splat(0.3027294874e+0)
                .mul_add(x2, $f32x::splat(0.3996108174e+0))
                .mul_add(x2, $f32x::splat(0.6666694880e+0));

            s = s.add_checked(x.scale($f32x::splat(2.)));
            s = s.add_checked(x2 * x.0 * t);

            let mut r = s.0 + s.1;

            r = d
                .gt($f32x::splat(1e+38))
                .select($f32x::splat(SLEEF_INFINITY_F), r);
            r = $f32x::from_bits(vor_vm_vo32_vm($f32x::splat(-1.).gt(d), $u32x::from_bits(r)));
            r = d
                .eq($f32x::splat(-1.))
                .select($f32x::splat(-SLEEF_INFINITY_F), r);
            visnegzero_vo_vf(d).select($f32x::splat(-0.), r)
        }

        //

        pub fn xfabsf(x: $f32x) -> $f32x {
            x.abs()
        }

        pub fn xcopysignf(x: $f32x, y: $f32x) -> $f32x {
            vcopysign_vf_vf_vf(x, y)
        }

        pub fn xfmaxf(x: $f32x, y: $f32x) -> $f32x {
            if cfg!(target_arch = "x86_64") || cfg!(target_arch = "x86")
            /*    && !cfg!(feature = "enable_vecext")
                        && !cfg!(feature = "enable_purec")*/
            {
                y.isnan().select(x, x.max(y))
            } else {
                y.isnan().select(x, x.gt(y).select(x, y))
            }
        }

        pub fn xfminf(x: $f32x, y: $f32x) -> $f32x {
            if cfg!(target_arch = "x86_64") || cfg!(target_arch = "x86")
            /*    && !cfg!(feature = "enable_vecext")
                        && !cfg!(feature = "enable_purec")*/
            {
                y.isnan().select(x, x.min(y))
            } else {
                y.isnan().select(x, y.gt(x).select(x, y))
            }
        }

        pub fn xfdimf(x: $f32x, y: $f32x) -> $f32x {
            let ret = x - y;
            (ret.lt($f32x::splat(0.)) | x.eq(y)).select($f32x::splat(0.), ret)
        }

        pub fn xtruncf(x: $f32x) -> $f32x {
            let fr = x - $f32x::from_cast(x.truncatei());
            (x.isinf() | x.abs().ge($f32x::splat(F1_23)))
                .select(x, vcopysign_vf_vf_vf(x - fr, x))
        }

        pub fn xfloorf(x: $f32x) -> $f32x {
            let fr = x - $f32x::from_cast(x.truncatei());
            let fr = fr.lt($f32x::splat(0.)).select(fr + $f32x::splat(1.), fr);
            (x.isinf() | x.abs().ge($f32x::splat(F1_23)))
                .select(x, vcopysign_vf_vf_vf(x - fr, x))
        }

        pub fn xceilf(x: $f32x) -> $f32x {
            let fr = x - $f32x::from_cast(x.truncatei());
            let fr = fr.le($f32x::splat(0.)).select(fr, fr - $f32x::splat(1.));
            (x.isinf() | x.abs().ge($f32x::splat(F1_23)))
                .select(x, vcopysign_vf_vf_vf(x - fr, x))
        }

        pub fn xroundf(d: $f32x) -> $f32x {
            let mut x = d + $f32x::splat(0.5);
            let fr = x - $f32x::from_cast(x.truncatei());
            x = (x.le($f32x::splat(0.)) & fr.eq($f32x::splat(0.))).select(x - $f32x::splat(1.), x);
            let fr = fr.lt($f32x::splat(0.)).select(fr + $f32x::splat(1.), fr);
            x = d
                .eq($f32x::splat(0.4999999701976776123))
                .select($f32x::splat(0.), x);
            (d.isinf() | d.abs().ge($f32x::splat(F1_23)))
                .select(d, vcopysign_vf_vf_vf(x - fr, d))
        }

        pub fn xrintf(d: $f32x) -> $f32x {
            let mut x = d + $f32x::splat(0.5);
            let isodd = ($i32x::splat(1) & x.truncatei()).eq($i32x::splat(1));
            let mut fr = x - $f32x::from_cast(x.truncatei());
            fr = (fr.lt($f32x::splat(0.)) | (fr.eq($f32x::splat(0.)) & isodd))
                .select(fr + $f32x::splat(1.), fr);
            x = d
                .eq($f32x::splat(0.50000005960464477539))
                .select($f32x::splat(0.), x);
            (d.isinf() | d.abs().ge($f32x::splat(F1_23)))
                .select(d, vcopysign_vf_vf_vf(x - fr, d))
        }

        pub fn xfmaf(mut x: $f32x, mut y: $f32x, mut z: $f32x) -> $f32x {
            let h2 = x * y + z;
            let mut q = $f32x::splat(1.);
            let o = h2.abs().lt($f32x::splat(1e-38));
            const C0: f32 = F1_25;
            const C1: f32 = C0 * C0;
            const C2: f32 = C1 * C1;
            {
                x = o.select(x * $f32x::splat(C1), x);
                y = o.select(y * $f32x::splat(C1), y);
                z = o.select(z * $f32x::splat(C2), z);
                q = o.select($f32x::splat(1. / C2), q);
            }
            let o = h2.abs().gt($f32x::splat(1e+38));
            {
                x = o.select(x * $f32x::splat(1. / C1), x);
                y = o.select(y * $f32x::splat(1. / C1), y);
                z = o.select(z * $f32x::splat(1. / C2), z);
                q = o.select($f32x::splat(C2), q);
            }
            let d = x.mul_as_f2(y) + z;
            let ret = (x.eq($f32x::splat(0.)) | y.eq($f32x::splat(0.))).select(z, d.0 + d.1);
            let mut o = z.isinf();
            o = vandnot_vo_vo_vo(x.isinf(), o);
            o = vandnot_vo_vo_vo(x.isnan(), o);
            o = vandnot_vo_vo_vo(y.isinf(), o);
            o = vandnot_vo_vo_vo(y.isnan(), o);
            let h2 = o.select(z, h2);

            o = h2.isinf() | h2.isnan();

            o.select(h2, ret * q)
        }

        fn xsqrtf_u05(d: $f32x) -> $f32x {
            let d = d.lt($f32x::splat(0.)).select($f32x::splat(SLEEF_NAN_F), d);

            let o = d.lt($f32x::splat(5.2939559203393770e-23));
            let d = o.select(d * $f32x::splat(1.8889465931478580e+22), d);
            let q = o.select(
                $f32x::splat(7.2759576141834260e-12 * 0.5),
                $f32x::splat(0.5),
            );

            let o = d.gt($f32x::splat(1.8446744073709552e+19));
            let d = o.select(d * $f32x::splat(5.4210108624275220e-20), d);
            let q = o.select($f32x::splat(4294967296.0 * 0.5), q);

            let mut x = $f32x::from_bits(
                $i32x::splat(0x5f375a86)
                    - $i32x::from_bits(
                        $u32x::from_bits($i32x::from_cast(d + $f32x::splat(1e-45))) >> 1,
                    ),
            );

            x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
            x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
            x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
            x *= d;

            let d2 = (d + x.mul_as_f2(x)) * x.recpre_as_f2();

            x = (d2.0 + d2.1) * q;

            x = d.ispinf().select($f32x::splat(SLEEF_INFINITY_F), x);
            d.eq($f32x::splat(0.)).select(d, x)
        }

        pub fn xsqrtf(d: $f32x) -> $f32x {
        //   if cfg!(feature = "accurate_sqrt") {
                d.sqrt()
        /*    } else {
                // fall back to approximation if ACCURATE_SQRT is undefined
                xsqrtf_u05(d)
            }*/
        }

        pub fn xhypotf_u05(x: $f32x, y: $f32x) -> $f32x {
            let x = x.abs();
            let y = y.abs();
            let min = x.min(y);
            let n = min;
            let max = x.max(y);
            let d = max;

            let o = max.lt($f32x::splat(f32::MIN));
            let n = o.select(n * $f32x::splat(F1_24), n);
            let d = o.select(d * $f32x::splat(F1_24), d);

            let t = F2::new(n, $f32x::splat(0.)) / F2::new(d, $f32x::splat(0.));
            let t = (t.square() + $f32x::splat(1.)).sqrt() * max;
            let mut ret = t.0 + t.1;
            ret = ret.isnan().select($f32x::splat(SLEEF_INFINITY_F), ret);
            ret = min.eq($f32x::splat(0.)).select(max, ret);
            ret = (x.isnan() | y.isnan()).select($f32x::splat(SLEEF_NAN_F), ret);
            (x.eq($f32x::splat(SLEEF_INFINITY_F)) | y.eq($f32x::splat(SLEEF_INFINITY_F)))
                .select($f32x::splat(SLEEF_INFINITY_F), ret)
        }

        pub fn xhypotf_u35(x: $f32x, y: $f32x) -> $f32x {
            let x = x.abs();
            let y = y.abs();
            let min = x.min(y);
            let max = x.max(y);

            let t = min / max;
            let mut ret = max * t.mul_add(t, $f32x::splat(1.)).sqrt();
            ret = min.eq($f32x::splat(0.)).select(max, ret);
            ret = (x.isnan() | y.isnan()).select($f32x::splat(SLEEF_NAN_F), ret);
            (x.eq($f32x::splat(SLEEF_INFINITY_F)) | y.eq($f32x::splat(SLEEF_INFINITY_F)))
                .select($f32x::splat(SLEEF_INFINITY_F), ret)
        }

        pub fn xnextafterf(x: $f32x, y: $f32x) -> $f32x {
            let x = x
                .eq($f32x::splat(0.))
                .select(vmulsign_vf_vf_vf($f32x::splat(0.), y), x);
            let mut xi2 = $i32x::from_cast(x);
            let c = vsignbit_vo_vf(x) ^ y.ge(x);

            xi2 = c.select($i32x::splat(0) - (xi2 ^ $i32x::splat(1 << 31)), xi2);

            xi2 = x.ne(y).select(xi2 - $i32x::splat(1), xi2);

            xi2 = c.select($i32x::splat(0) - (xi2 ^ $i32x::splat(1 << 31)), xi2);

            let mut ret = $f32x::from_bits(xi2);

            ret = (ret.eq($f32x::splat(0.)) & x.ne($f32x::splat(0.)))
                .select(vmulsign_vf_vf_vf($f32x::splat(0.), x), ret);

            ret = (x.eq($f32x::splat(0.)) & y.eq($f32x::splat(0.))).select(y, ret);

            (x.isnan() | y.isnan()).select($f32x::splat(SLEEF_NAN_F), ret)
        }

        pub fn xfrfrexpf(x: $f32x) -> $f32x {
            let x = x
                .abs()
                .lt($f32x::splat(f32::MIN))
                .select(x * $f32x::splat(F1_32), x);

            let mut xm = $u32x::from_bits(x);
            xm &= $u32x::splat(!0x7f800000u32);
            xm |= $u32x::splat(0x3f000000u32);

            let ret = $f32x::from_bits(xm);

            let ret =
                x.isinf().select(vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), x), ret);
            x.eq($f32x::splat(0.)).select(x, ret)
        }

        pub fn xexpfrexpf(_x: $f32x) -> $i32x {
            /*
                                  x = x.abs().lt($f32x::splat(f32::MIN)).select(x * $f32x::splat(F1_63), x);

                                  let mut ret = $i32x::from_cast($ix::splat(x);
                                  ret = (vsrl_vi_vi_i(ret, 20) & $ix::splat(0x7ff)) - $ix::splat(0x3fe);

                                  (x.eq($f32x::splat(0.)) | x.isnan() | x.isinf()).select($ix::splat(0), ret)
                                  */
            $i32x::splat(0)
        }
        #[inline]
        fn vtoward0f(x: $f32x) -> $f32x {
            let t = $f32x::from_bits($i32x::from_cast(x) - $i32x::splat(1));
            x.eq($f32x::splat(0.)).select($f32x::splat(0.), t)
        }
        #[inline]
        fn vptruncf(x: $f32x) -> $f32x {
            if cfg!(feature = "full_fp_rounding") {
                x.truncate()
            } else {
                let fr = x - $f32x::from_cast(x.truncatei());
                x.abs().ge($f32x::splat(F1_23)).select(x, x - fr)
            }
        }

        pub fn xfmodf(x: $f32x, y: $f32x) -> $f32x {
            let nu = x.abs();
            let de = y.abs();
            let s = $f32x::splat(1.);
            let o = de.lt($f32x::splat(f32::MIN));
            let nu = o.select(nu * $f32x::splat(F1_25), nu);
            let de = o.select(de * $f32x::splat(F1_25), de);
            let s = o.select(s * $f32x::splat(1. / F1_25), s);
            let rde = vtoward0f(de.recpre());
            #[cfg(any(feature = "enable_neon32", feature = "enable_neon32vfpv4"))]
            {
                let rde = vtoward0f(rde);
            }
            let mut r = F2::new(nu, $f32x::splat(0.));

            for _ in 0..8 {
                // ceil(log2(FLT_MAX) / 22)+1
                let q =
                    ((de + de).gt(r.0) & r.0.ge(de)).select($f32x::splat(1.), vtoward0f(r.0) * rde);
                r = (r + vptruncf(q).mul_as_f2(-de)).normalize();
                if r.0.lt(de).all() {
                    break;
                }
            }

            let mut ret = (r.0 + r.1) * s;
            ret = (r.0 + r.1).eq(de).select($f32x::splat(0.), ret);

            ret = vmulsign_vf_vf_vf(ret, x);

            ret = nu.lt(de).select(x, ret);
            de.eq($f32x::splat(0.))
                .select($f32x::splat(SLEEF_NAN_F), ret)
        }

        //
        #[inline]
        fn sinpifk(d: $f32x) -> F2<$f32x> {
            let u = d * $f32x::splat(4.);
            let q = u.truncatei();
            let q = (q + ($i32x::from_bits($u32x::from_bits(q) >> 31) ^ $i32x::splat(1)))
                & $i32x::splat(!1);
            let o = (q & $i32x::splat(2)).eq($i32x::splat(2));

            let s = u - $f32x::from_cast(q);
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_f2(t);

            //

            let u = vsel_vf_vo_f_f(o, -0.2430611801e-7, 0.3093842054e-6)
                .mul_add(s, vsel_vf_vo_f_f(o, 0.3590577080e-5, -0.3657307388e-4))
                .mul_add(s, vsel_vf_vo_f_f(o, -0.3259917721e-3, 0.2490393585e-2));
            let mut x = u * s + vsel_vf2_vo_f_f_f_f(
                o,
                0.015854343771934509277,
                4.4940051354032242811e-10,
                -0.080745510756969451904,
                -1.3373665339076936258e-09,
            );
            x = s2 * x + vsel_vf2_vo_f_f_f_f(
                o,
                -0.30842512845993041992,
                -9.0728339030733922277e-09,
                0.78539818525314331055,
                -2.1857338617566484855e-08,
            );

            x *= vsel_vf2_vo_vf2_vf2(o, s2, F2::new(t, $f32x::splat(0.)));
            x = vsel_vf2_vo_vf2_vf2(o, x + $f32x::splat(1.), x);

            let o = (q & $i32x::splat(4)).eq($i32x::splat(4));
            x.0 = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x.0),
            );
            x.1 = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x.1),
            );

            x
        }

        pub fn xsinpif_u05(d: $f32x) -> $f32x {
            let x = sinpifk(d);
            let mut r = x.0 + x.1;

            r = visnegzero_vo_vf(d).select($f32x::splat(-0.), r);
            r = $f32x::from_bits(vandnot_vm_vo32_vm(
                d.abs().gt($f32x::splat(TRIGRANGEMAX4_F)),
                $u32x::from_bits(r),
            ));
            $f32x::from_bits(vor_vm_vo32_vm(d.isinf(), $u32x::from_bits(r)))
        }
        #[inline]
        fn cospifk(d: $f32x) -> F2<$f32x> {
            let u = d * $f32x::splat(4.);
            let q = u.truncatei();
            let q = (q + ($i32x::from_bits($u32x::from_bits(q) >> 31) ^ $i32x::splat(1)))
                & $i32x::splat(!1);
            let o = (q & $i32x::splat(2)).eq($i32x::splat(0));

            let s = u - $f32x::from_cast(q);
            let t = s;
            let s = s * s;
            let s2 = t.mul_as_f2(t);

            //

            let u = vsel_vf_vo_f_f(o, -0.2430611801e-7, 0.3093842054e-6)
                .mul_add(s, vsel_vf_vo_f_f(o, 0.3590577080e-5, -0.3657307388e-4))
                .mul_add(s, vsel_vf_vo_f_f(o, -0.3259917721e-3, 0.2490393585e-2));
            let mut x = u * s + vsel_vf2_vo_f_f_f_f(
                o,
                0.015854343771934509277,
                4.4940051354032242811e-10,
                -0.080745510756969451904,
                -1.3373665339076936258e-09,
            );
            x = s2 * x + vsel_vf2_vo_f_f_f_f(
                o,
                -0.30842512845993041992,
                -9.0728339030733922277e-09,
                0.78539818525314331055,
                -2.1857338617566484855e-08,
            );

            x *= vsel_vf2_vo_vf2_vf2(o, s2, F2::new(t, $f32x::splat(0.)));
            x = vsel_vf2_vo_vf2_vf2(o, x + $f32x::splat(1.), x);

            let o = ((q + $i32x::splat(2)) & $i32x::splat(4)).eq($i32x::splat(4));
            x.0 = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x.0),
            );
            x.1 = $f32x::from_bits(
                vand_vm_vo32_vm(o, $u32x::from_bits($f32x::splat(-0.))) ^ $u32x::from_bits(x.1),
            );

            x
        }

        pub fn xcospif_u05(d: $f32x) -> $f32x {
            let x = cospifk(d);
            let r = x.0 + x.1;

            let r = d
                .abs()
                .gt($f32x::splat(TRIGRANGEMAX4_F))
                .select($f32x::splat(1.), r);
            $f32x::from_bits(vor_vm_vo32_vm(d.isinf(), $u32x::from_bits(r)))
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        fn gammafk(a: $f32x) -> (F2<$f32x>, F2<$f32x>) {
            let mut clln = F2::from((1., 0.));
            let mut clld = F2::from((1., 0.));

            let otiny = a.abs().lt($f32x::splat(1e-30));
            let oref = a.lt($f32x::splat(0.5));

            let x = vsel_vf2_vo_vf2_vf2(
                otiny,
                F2::from((0., 0.)),
                vsel_vf2_vo_vf2_vf2(
                    oref,
                    $f32x::splat(1.).add_as_f2(-a),
                    F2::new(a, $f32x::splat(0.)),
                ),
            );

            let o0 = $f32x::splat(0.5).le(x.0) & x.0.le($f32x::splat(1.2));
            let o2 = $f32x::splat(2.3).le(x.0);

            let mut y = ((x + $f32x::splat(1.)) * x).normalize();
            y = ((x + $f32x::splat(2.)) * y).normalize();

            let o = o2 & x.0.le($f32x::splat(7.));
            clln = vsel_vf2_vo_vf2_vf2(o, y, clln);

            let mut x = vsel_vf2_vo_vf2_vf2(o, x + $f32x::splat(3.), x);
            let t = o2.select(x.0.recpre(), (x + vsel_vf_vo_f_f(o0, -1., -2.)).normalize().0);

            let u = vsel_vf_vo_vo_f_f_f(
                o2,
                o0,
                0.000839498720672087279971000786,
                0.9435157776e+0,
                0.1102489550e-3,
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    -5.17179090826059219329394422e-05,
                    0.8670063615e+0,
                    0.8160019934e-4,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    -0.000592166437353693882857342347,
                    0.4826702476e+0,
                    0.1528468856e-3,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    6.97281375836585777403743539e-05,
                    -0.8855129778e-1,
                    -0.2355068718e-3,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    0.000784039221720066627493314301,
                    0.1013825238e+0,
                    0.4962242092e-3,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    -0.000229472093621399176949318732,
                    -0.1493408978e+0,
                    -0.1193488017e-2,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    -0.002681327160493827160473958490,
                    0.1697509140e+0,
                    0.2891599433e-2,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    0.003472222222222222222175164840,
                    -0.2072454542e+0,
                    -0.7385451812e-2,
                ),
            ).mul_add(
                t,
                vsel_vf_vo_vo_f_f_f(
                    o2,
                    o0,
                    0.083333333333333333335592087900,
                    0.2705872357e+0,
                    0.2058077045e-1,
                ),
            );

            y = (x + $f32x::splat(-0.5)) * logk2f(x);
            y += -x;
            y += F2::from(0.91893853320467278056_f64); // 0.5*log(2*M_PI)

            let mut z = u.mul_as_f2(t) + vsel_vf_vo_f_f(
                o0,
                -0.400686534596170958447352690395e+0,
                -0.673523028297382446749257758235e-1,
            );
            z = z * t + vsel_vf_vo_f_f(
                o0,
                0.822466960142643054450325495997e+0,
                0.322467033928981157743538726901e+0,
            );
            z = z * t + vsel_vf_vo_f_f(
                o0,
                -0.577215665946766039837398973297e+0,
                0.422784335087484338986941629852e+0,
            );
            z = z * t;

            let mut clc = vsel_vf2_vo_vf2_vf2(o2, y, z);

            clld = vsel_vf2_vo_vf2_vf2(o2, u.mul_as_f2(t) + $f32x::splat(1.), clld);

            y = clln;

            clc = vsel_vf2_vo_vf2_vf2(
                otiny,
                F2::from(41.58883083359671856503_f64), // log(2^60)
                vsel_vf2_vo_vf2_vf2(oref, F2::from(1.1447298858494001639_f64) + (-clc), clc),
            ); // log(M_PI)
            clln = vsel_vf2_vo_vf2_vf2(
                otiny,
                F2::from((1., 0.)),
                vsel_vf2_vo_vf2_vf2(oref, clln, clld),
            );

            if !vnot_vo32_vo32(oref).all() {
                let t = a
                    - $f32x::splat(F1_12)
                        * $f32x::from_cast((a * $f32x::splat(1. / F1_12)).truncatei());
                x = clld * sinpifk(t);
            }

            clld = vsel_vf2_vo_vf2_vf2(
                otiny,
                F2::new(a * $f32x::splat(F1_30 * F1_30), $f32x::splat(0.)),
                vsel_vf2_vo_vf2_vf2(oref, x, y),
            );

            (clc, clln / clld)
        }

        pub fn xtgammaf_u1(a: $f32x) -> $f32x {
            let (da, db) = gammafk(a);
            let y = expk2f(da) * db;
            let r = y.0 + y.1;

            let o = a.eq($f32x::splat(-SLEEF_INFINITY_F))
                | (a.lt($f32x::splat(0.)) & visint_vo_vf(a))
                | (visnumber_vo_vf(a) & a.lt($f32x::splat(0.)) & r.isnan());
            let r = o.select($f32x::splat(SLEEF_NAN_F), r);

            let o = (a.eq($f32x::splat(SLEEF_INFINITY_F)) | visnumber_vo_vf(a))
                & a.ge($f32x::splat(-f32::MIN))
                & (a.eq($f32x::splat(0.)) | a.gt($f32x::splat(36.)) | r.isnan());
            o.select(vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), a), r)
        }

        pub fn xlgammaf_u1(a: $f32x) -> $f32x {
            let (da, db) = gammafk(a);
            let y = da + logk2f(db.abs());
            let r = y.0 + y.1;

            let o = a.isinf()
                | ((a.le($f32x::splat(0.)) & visint_vo_vf(a))
                    | (visnumber_vo_vf(a) & r.isnan()));
            o.select($f32x::splat(SLEEF_INFINITY_F), r)
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        pub fn xerff_u1(a: $f32x) -> $f32x {
            let s = a;

            let a = a.abs();
            let o0 = a.lt($f32x::splat(1.1));
            let o1 = a.lt($f32x::splat(2.4));
            let o2 = a.lt($f32x::splat(4.));
            let u = o0.select(a * a, a);

            let t =
                vsel_vf_vo_vo_f_f_f(o0, o1, 0.7089292194e-4, -0.1792667899e-4, -0.9495757695e-5)
                    .mul_add(
                        u,
                        vsel_vf_vo_vo_f_f_f(
                            o0,
                            o1,
                            -0.7768311189e-3,
                            0.3937633010e-3,
                            0.2481465926e-3,
                        ),
                    ).mul_add(
                        u,
                        vsel_vf_vo_vo_f_f_f(
                            o0,
                            o1,
                            0.5159463733e-2,
                            -0.3949181177e-2,
                            -0.2918176819e-2,
                        ),
                    ).mul_add(
                        u,
                        vsel_vf_vo_vo_f_f_f(
                            o0,
                            o1,
                            -0.2683781274e-1,
                            0.2445474640e-1,
                            0.2059706673e-1,
                        ),
                    ).mul_add(
                        u,
                        vsel_vf_vo_vo_f_f_f(
                            o0,
                            o1,
                            0.1128318012e+0,
                            -0.1070996150e+0,
                            -0.9901899844e-1,
                        ),
                    );
            let mut d = t.mul_as_f2(u);
            d += vsel_vf2_vo_vo_d_d_d(
                o0,
                o1,
                -0.376125876000657465175213237214e+0,
                -0.634588905908410389971210809210e+0,
                -0.643598050547891613081201721633e+0,
            );
            d *= u;
            d += vsel_vf2_vo_vo_d_d_d(
                o0,
                o1,
                0.112837916021059138255978217023e+1,
                -0.112879855826694507209862753992e+1,
                -0.112461487742845562801052956293e+1,
            );
            d *= a;
            d = vsel_vf2_vo_vf2_vf2(o0, d, $f32x::splat(1.).add_checked(-expk2f(d)));
            let u = vmulsign_vf_vf_vf(o2.select(d.0 + d.1, $f32x::splat(1.)), s);
            a.isnan().select($f32x::splat(SLEEF_NAN_F), u)
        }

        /* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
        pub fn xerfcf_u15(a: $f32x) -> $f32x {
            let s = a;
            let a = a.abs();
            let o0 = a.lt($f32x::splat(1.));
            let o1 = a.lt($f32x::splat(2.2));
            let o2 = a.lt($f32x::splat(4.3));
            let o3 = a.lt($f32x::splat(10.1));

            let u = vsel_vf2_vo_vf2_vf2(
                o1,
                F2::new(a, $f32x::splat(0.)),
                F2::from((1., 0.)) / F2::new(a, $f32x::splat(0.)),
            );

            let t = vsel_vf_vo_vo_vo_f_f_f_f(
                o0,
                o1,
                o2,
                -0.8638041618e-4,
                -0.6236977242e-5,
                -0.3869504035e+0,
                0.1115344167e+1,
            ).mul_add(
                u.0,
                vsel_vf_vo_vo_vo_f_f_f_f(
                    o0,
                    o1,
                    o2,
                    0.6000166177e-3,
                    0.5749821503e-4,
                    0.1288077235e+1,
                    -0.9454904199e+0,
                ),
            ).mul_add(
                u.0,
                vsel_vf_vo_vo_vo_f_f_f_f(
                    o0,
                    o1,
                    o2,
                    -0.1665703603e-2,
                    0.6002851478e-5,
                    -0.1816803217e+1,
                    -0.3667259514e+0,
                ),
            ).mul_add(
                u.0,
                vsel_vf_vo_vo_vo_f_f_f_f(
                    o0,
                    o1,
                    o2,
                    0.1795156277e-3,
                    -0.2851036377e-2,
                    0.1249150872e+1,
                    0.7155663371e+0,
                ),
            ).mul_add(
                u.0,
                vsel_vf_vo_vo_vo_f_f_f_f(
                    o0,
                    o1,
                    o2,
                    0.1914106123e-1,
                    0.2260518074e-1,
                    -0.1328857988e+0,
                    -0.1262947265e-1,
                ),
            );

            let mut d = u * t;
            d += vsel_vf2_vo_vo_vo_d_d_d_d(
                o0,
                o1,
                o2,
                -0.102775359343930288081655368891e+0,
                -0.105247583459338632253369014063e+0,
                -0.482365310333045318680618892669e+0,
                -0.498961546254537647970305302739e+0,
            );
            d *= u;
            d += vsel_vf2_vo_vo_vo_d_d_d_d(
                o0,
                o1,
                o2,
                -0.636619483208481931303752546439e+0,
                -0.635609463574589034216723775292e+0,
                -0.134450203224533979217859332703e-2,
                -0.471199543422848492080722832666e-4,
            );
            d *= u;
            d += vsel_vf2_vo_vo_vo_d_d_d_d(
                o0,
                o1,
                o2,
                -0.112837917790537404939545770596e+1,
                -0.112855987376668622084547028949e+1,
                -0.572319781150472949561786101080e+0,
                -0.572364030327966044425932623525e+0,
            );

            let mut x = vsel_vf2_vo_vf2_vf2(o1, d, F2::new(-a, $f32x::splat(0.))) * a;
            x = vsel_vf2_vo_vf2_vf2(o1, x, x + d);

            x = expk2f(x);
            x = vsel_vf2_vo_vf2_vf2(o1, x, x * u);

            let mut r = o3.select(x.0 + x.1, $f32x::splat(0.));
            r = vsignbit_vo_vf(s).select($f32x::splat(2.) - r, r);
            s.isnan().select($f32x::splat(SLEEF_NAN_F), r)
        }

    };
}
