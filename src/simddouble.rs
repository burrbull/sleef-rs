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

extern const double rempitabdp[];

#define __SLEEFSIMDDP_C__

#if (defined(_MSC_VER))
#pragma fp_contract (off)
#endif
*/

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

//

#include "dd.h"

//

#[inline]
fn $mx vnot_vo64_vo64($mx x) {
  return x ^ veq64_vo_vm_vm(vcast_vm_i_i(0, 0), vcast_vm_i_i(0, 0));
}
#[inline]
fn vsignbit_vo_vd(d: $f64x) -> $mx {
  return veq64_vo_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd($f64x::splat(-0.)));
}

// return d0 < d1 ? x : y
#[inline]
fn vsel_vi_vd_vd_vi_vi($f64x d0, $f64x d1, $ix x, $ix y) -> $ix { return vsel_vi_vo_vi_vi(vcast_vo32_vo64(d0.lt(d1)), x, y); } 

// return d0 < 0 ? x : 0
#[inline]
fn vsel_vi_vd_vi($f64x d, $ix x) -> $ix { return vand_vi_vo_vi(vcast_vo32_vo64(vsignbit_vo_vd(d)), x); }
#[inline]
fn visnegzero_vo_vd(d: $f64x) -> $mx {
  return veq64_vo_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd($f64x::splat(-0.)));
}
#[inline]
fn visnumber_vo_vd(x: $f64x) -> $mx {
  return vandnot_vo_vo_vo(visinf_vo_vd(x), x.eq(x));
}
#[inline]
fn vsignbit_vm_vd(d: $f64x) -> vmask {
  return vand_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd($f64x::splat(-0.)));
}
#[inline]
fn vmulsign_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x {
  return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(x), vsignbit_vm_vd(y)));
}
#[inline]
fn vcopysign_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x {
  return vreinterpret_vd_vm(vxor_vm_vm_vm(vandnot_vm_vm_vm(vreinterpret_vm_vd($f64x::splat(-0.)), vreinterpret_vm_vd(x)), 
					  vand_vm_vm_vm   (vreinterpret_vm_vd($f64x::splat(-0.)), vreinterpret_vm_vd(y))));
}
#[inline]
fn vsign_vd_vd(d: $f64x) -> $f64x {
  return vmulsign_vd_vd_vd($f64x::splat(1.), d);
}
#[inline]
fn vpow2i_vd_vi(q: $ix) -> $f64x {
  q = $ix::splat(0x3ff) + q;
  let r = $ix2::from(q);
  return vreinterpret_vd_vi2(vsll_vi2_vi2_i(r, 20));
}
#[inline]
fn vldexp_vd_vd_vi(x: $f64x, q: $ix) -> $f64x {
  $ix m = vsra_vi_vi_i(q, 31);
  m = vsll_vi_vi_i(vsra_vi_vi_i(m + q, 9) - m, 7);
  q = q - vsll_vi_vi_i(m, 2);
  m = $ix::splat(0x3ff) + m;
  m = vandnot_vi_vo_vi(vgt_vo_vi_vi($ix::splat(0), m), m);
  m = vsel_vi_vo_vi_vi(vgt_vo_vi_vi(m, $ix::splat(0x7ff)), $ix::splat(0x7ff), m);
  let r = $ix2::from(m);
  $f64x y = vreinterpret_vd_vi2(vsll_vi2_vi2_i(r, 20));
  return x * y * y * y * y * vpow2i_vd_vi(q);
}
#[inline]
fn vldexp2_vd_vd_vi(d: $f64x, e: $ix) -> $f64x {
  return d * vpow2i_vd_vi(vsra_vi_vi_i(e, 1)) * vpow2i_vd_vi(e - vsra_vi_vi_i(e, 1));
}
#[inline]
fn vldexp3_vd_vd_vi(d: $f64x, q: $ix) -> $f64x {
  return vreinterpret_vd_vi2(vadd_vi2_vi2_vi2(vreinterpret_vi2_vd(d), vsll_vi2_vi2_i($ix2::from(q), 20)));
}

#[cfg(all(not(feature="enable_avx512f"), not(feature="enable_avx512fnofma")))]#[inline]
fn vilogbk_vi_vd(d: $f64x) -> $ix {
  $mx o = d.lt($f64x::splat(4.9090934652977266e-91));
  d = vsel_vd_vo_vd_vd(o, $f64x::splat(2.037035976334486e90) * d, d);
  let q = $ix::splat(vreinterpret_vi2_vd(d));
  q = vand_vi_vi_vi(q, $ix::splat(((1 << 12)-1) << 20));
  q = vsrl_vi_vi_i(q, 20);
  q - vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), $ix::splat(300 + 0x3ff), $ix::splat(0x3ff))
}
#[inline]
fn vilogb2k_vi_vd(d: $f64x) -> $ix {
  let q = $ix::splat(vreinterpret_vi2_vd(d));
  q = vsrl_vi_vi_i(q, 20);
  q = vand_vi_vi_vi(q, $ix::splat(0x7ff));
  q - $ix::splat(0x3ff)
}
#endif
#[inline]
fn visint_vo_vd(d: $f64x) -> $mx {
  $f64x x = vtruncate_vd_vd(d * $f64x::splat(1. / D1_31));
  x = $f64x::splat(-D1_31).mla(x, d);
  vtruncate_vd_vd(x).eq(x) | vabs_vd_vd(d).gt($f64x::splat(D1_53))
}
#[inline]
fn visodd_vo_vd(d: $f64x) -> $mx {
  $f64x x = vtruncate_vd_vd(d * $f64x::splat(1. / D1_31));
  x = $f64x::splat(-D1_31).mla(x, d);

  vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(vtruncate_vi_vd(x), $ix::splat(1)), $ix::splat(1))) &
		       vabs_vd_vd(d).lt($f64x::splat(D1_53))
}

//

pub fn xldexp(x: $f64x, q: $ix) -> $f64x { return vldexp_vd_vd_vi(x, q); }

pub fn xilogb(d: $f64x) -> $ix {
  $f64x e = $f64x::from(vilogbk_vi_vd(vabs_vd_vd(d)));
  e = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), $f64x::splat(SLEEF_FP_ILOGB0), e);
  e = vsel_vd_vo_vd_vd(visnan_vo_vd(d), $f64x::splat(SLEEF_FP_ILOGBNAN), e);
  e = vsel_vd_vo_vd_vd(visinf_vo_vd(d), $f64x::splat(INT_MAX), e);
  return vrint_vi_vd(e);
}

#[inline]
fn rempisub(x: $f64x) -> ($f64x, $ix) {
   if cfg!(feature="full_fp_rounding") {
    $f64x y = vrint_vd_vd(x * $f64x::splat(4.));
    $ix vi = vtruncate_vi_vd(y - vrint_vd_vd(x) * $f64x::splat(4.));
    ( x - y * $f64x::splat(0.25), vi )
   } else {
    $f64x fr = x - $f64x::splat(D1_28) * vtruncate_vd_vd(x * $f64x::splat(1. / D1_28));
    $ix vi = vsel_vi_vo_vi_vi(vcast_vo32_vo64(x.gt($f64x::splat(0.))), $ix::splat(4), $ix::splat(3)) + vtruncate_vi_vd(fr * $f64x::splat(8.));
    vi = vsra_vi_vi_i(vand_vi_vi_vi($ix::splat(7), vi) - $ix::splat(3), 1);
    fr = fr - $f64x::splat(0.25) * vtruncate_vd_vd(fr.mla($f64x::splat(4.), vmulsign_vd_vd_vd($f64x::splat(0.5), x)));
    fr = vsel_vd_vo_vd_vd(vabs_vd_vd(fr).gt($f64x::splat(0.25)), fr - vmulsign_vd_vd_vd($f64x::splat(0.5), x), fr);
    fr = vsel_vd_vo_vd_vd(vabs_vd_vd(fr).gt($f64x::splat(1e+10)), $f64x::splat(0), fr);
    $mx o = vabs_vd_vd(x).eq($f64x::splat(0.12499999999999998612));
    fr = vsel_vd_vo_vd_vd(o, x, fr);
    vi = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), $ix::splat(0), vi);
    ( fr, vi )
  }
}
#[inline]
fn rempi(a: $f64x) -> (D2, $ix) {
  D2 x, y, z;
  $ix ex = vilogb2k_vi_vd(a);
#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  ex = vandnot_vi_vi_vi(vsra_vi_vi_i(ex, 31), ex);
  ex = vand_vi_vi_vi(ex, $ix::splat(1023));
#endif
  ex = ex - $ix::splat(55);
  $ix q = vand_vi_vo_vi(vgt_vo_vi_vi(ex, $ix::splat(700-55)), $ix::splat(-64));
  a = vldexp3_vd_vd_vi(a, q);
  ex = vandnot_vi_vi_vi(vsra_vi_vi_i(ex, 31), ex);
  ex = vsll_vi_vi_i(ex, 2);
  x = a.mul_as_d2(vgather_vd_p_vi(rempitabdp, ex));
  let (dii, did) = rempisub(x.0);
  q = dii;
  x.0 = did;
  x = x.normalize();
  y = a.mul_as_d2(vgather_vd_p_vi(rempitabdp+1, ex));
  x += y;
  let (dii, did) = rempisub(x.0);
  q = q + dii;
  x.0 = did;
  x = x.normalize();
  y = D2::new(vgather_vd_p_vi(rempitabdp+2, ex), vgather_vd_p_vi(rempitabdp+3, ex));
  y *= a;
  x += y;
  x = x.normalize();
  x *= D2::from((3.141592653589793116*2, 1.2246467991473532072e-16*2));
  $mx o = vabs_vd_vd(a).lt($f64x::splat(0.7));
  x.0 = vsel_vd_vo_vd_vd(o, a, x.0);
  x.1 = vreinterpret_vd_vm(vandnot_vm_vo64_vm(o, vreinterpret_vm_vd(x.1)));
  ( x, q )
}

pub fn xsin(d: $f64x) -> $f64x {
  $f64x u, s, r = d;
  $ix ql;

  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX2))))) {
    $f64x dql = vrint_vd_vd(d * $f64x::splat(M_1_PI));
    ql = vrint_vi_vd(dql);
    d = dql.mla($f64x::splat(-PI_A2), d);
    d = dql.mla($f64x::splat(-PI_B2), d);
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX))))) {
    $f64x dqh = vtruncate_vd_vd(d * $f64x::splat(M_1_PI / D1_24));
    dqh = dqh * $f64x::splat(D1_24);
    $f64x dql = vrint_vd_vd(vmlapn_vd_vd_vd_vd(d, $f64x::splat(M_1_PI), dqh));
    ql = vrint_vi_vd(dql);

    d = dqh.mla($f64x::splat(-PI_A), d);
    d = dql.mla($f64x::splat(-PI_A), d);
    d = dqh.mla($f64x::splat(-PI_B), d);
    d = dql.mla($f64x::splat(-PI_B), d);
    d = dqh.mla($f64x::splat(-PI_C), d);
    d = dql.mla($f64x::splat(-PI_C), d);
    d = (dqh + dql).mla($f64x::splat(-PI_D), d);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = vand_vi_vi_vi(ddii, $ix::splat(3));
    ql = ql + ql + vsel_vi_vo_vi_vi(vcast_vo32_vo64(ddidd.0.gt($f64x::splat(0.))), $ix::splat(2), $ix::splat(1));
    ql = vsra_vi_vi_i(ql, 2);
    $mx o = veq_vo_vi_vi(vand_vi_vi_vi(ddii, $ix::splat(1)), $ix::splat(1));
    D2 x = D2::new(vmulsign_vd_vd_vd($f64x::splat(-3.141592653589793116 * 0.5), ddidd.0), 
				 vmulsign_vd_vd_vd($f64x::splat(-1.2246467991473532072e-16 * 0.5), ddidd.0));
    x = ddidd + x;
    ddidd = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(o), x, ddidd);
    d = ddidd.0 + ddidd.1;
    d = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(r) | visnan_vo_vd(r), vreinterpret_vm_vd(d)));
  }

  s = d * d;

  d = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(1)), $ix::splat(1))), vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(d)));

  u = $f64x::splat(-7.97255955009037868891952e-18)
      .mla(s, $f64x::splat(2.81009972710863200091251e-15))
      .mla(s, $f64x::splat(-7.64712219118158833288484e-13))
      .mla(s, $f64x::splat(1.60590430605664501629054e-10))
      .mla(s, $f64x::splat(-2.50521083763502045810755e-08))
      .mla(s, $f64x::splat(2.75573192239198747630416e-06))
      .mla(s, $f64x::splat(-0.000198412698412696162806809))
      .mla(s, $f64x::splat(0.00833333333333332974823815))
      .mla(s, $f64x::splat(-0.166666666666666657414808));

  u = s*(u*d) + d;

  u = vsel_vd_vo_vd_vd(visnegzero_vo_vd(r), r, u);
  
  return u;
}

pub fn xsin_u1(d: $f64x) -> $f64x {
  $f64x u;
  D2 s, t, x;
  $ix ql;
  
  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX2))))) {
    const $f64x dql = vrint_vd_vd(d*$f64x::splat(M_1_PI));
    ql = vrint_vi_vd(dql);
    u = dql.mla($f64x::splat(-PI_A2), d);
    s = u.add_checked_as_d2(dql*$f64x::splat(-PI_B2));
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX))))) {
    $f64x dqh = vtruncate_vd_vd(d*$f64x::splat(M_1_PI / D1_24));
    dqh = dqh*$f64x::splat(D1_24);
    const $f64x dql = vrint_vd_vd(vmlapn_vd_vd_vd_vd(d, $f64x::splat(M_1_PI), dqh));
    ql = vrint_vi_vd(dql);

    u = dqh.mla($f64x::splat(-PI_A), d);
    s = u.add_checked_as_d2(dql*$f64x::splat(-PI_A));
    s += dqh*$f64x::splat(-PI_B);
    s += dql*$f64x::splat(-PI_B);
    s += dqh*$f64x::splat(-PI_C);
    s += dql*$f64x::splat(-PI_C));
    s += (dqh + dql)*$f64x::splat(-PI_D);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = vand_vi_vi_vi(ddii, $ix::splat(3));
    ql = ql + ql + vsel_vi_vo_vi_vi(vcast_vo32_vo64(ddidd.0.gt($f64x::splat(0.))), $ix::splat(2), $ix::splat(1));
    ql = vsra_vi_vi_i(ql, 2);
    $mx o = veq_vo_vi_vi(vand_vi_vi_vi(ddii, $ix::splat(1)), $ix::splat(1));
    D2 x = D2::new(vmulsign_vd_vd_vd($f64x::splat(-3.141592653589793116 * 0.5), ddidd.0), 
				 vmulsign_vd_vd_vd($f64x::splat(-1.2246467991473532072e-16 * 0.5), ddidd.0));
    x = ddidd + x;
    ddidd = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(o), x, ddidd);
    s = ddidd.normalize();
    s.0 = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d) | visnan_vo_vd(d), vreinterpret_vm_vd(s.0)));
  }
  
  t = s;
  s = s.square();

  u = $f64x::splat(2.72052416138529567917983e-15)
      .mla(s.0, $f64x::splat(-7.6429259411395447190023e-13))
      .mla(s.0, $f64x::splat(1.60589370117277896211623e-10))
      .mla(s.0, $f64x::splat(-2.5052106814843123359368e-08))
      .mla(s.0, $f64x::splat(2.75573192104428224777379e-06))
      .mla(s.0, $f64x::splat(-0.000198412698412046454654947))
      .mla(s.0, $f64x::splat(0.00833333333333318056201922));

  x = $f64x::splat(1.).add_checked(($f64x::splat(-0.166666666666666657414808).add_checked_as_d2(u*s.0)) * s);

  u = t.mul_as_d(x);
  
  u = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(1)), $ix::splat(1))),
						       vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(u)));
  u = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), d, u);
  
  return u;
}

pub fn xcos(d: $f64x) -> $f64x {
  $f64x u, s, r = d;
  $ix ql;

  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX2))))) {
    let dql = $f64x::splat(2.).mla(
            vrint_vd_vd(d.mla($f64x::splat(M_1_PI), $f64x::splat(-0.5))),
				   $f64x::splat(1.)
    );
    ql = vrint_vi_vd(dql);
    d = dql.mla($f64x::splat(-PI_A2 * 0.5), d);
    d = dql.mla($f64x::splat(-PI_B2 * 0.5), d);
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX))))) {
    $f64x dqh = vtruncate_vd_vd(d.mla($f64x::splat(M_1_PI / D1_23), $f64x::splat(-M_1_PI / D1_24)));
    ql = vrint_vi_vd(d*$f64x::splat(M_1_PI) +
				   dqh.mla($f64x::splat(-D1_23), $f64x::splat(-0.5)));
    dqh = dqh*$f64x::splat(D1_24);
    ql = ql + ql + $ix::splat(1);
    $f64x dql = $f64x::from(ql);

    d = dqh.mla($f64x::splat(-PI_A * 0.5), d);
    d = dql.mla($f64x::splat(-PI_A * 0.5), d);
    d = dqh.mla($f64x::splat(-PI_B * 0.5), d);
    d = dql.mla($f64x::splat(-PI_B * 0.5), d);
    d = dqh.mla($f64x::splat(-PI_C * 0.5), d);
    d = dql.mla($f64x::splat(-PI_C * 0.5), d);
    d = (dqh + dql).mla($f64x::splat(-PI_D * 0.5), d);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = vand_vi_vi_vi(ddii, $ix::splat(3));
    ql = ql + ql + vsel_vi_vo_vi_vi(vcast_vo32_vo64(ddidd.0.gt($f64x::splat(0.))), $ix::splat(8), $ix::splat(7));
    ql = vsra_vi_vi_i(ql, 1);
    $mx o = veq_vo_vi_vi(vand_vi_vi_vi(ddii, $ix::splat(1)), $ix::splat(0));
    $f64x y = vsel_vd_vo_vd_vd(ddidd.0.gt($f64x::splat(0.)), $f64x::splat(0.), $f64x::splat(-1.));
    D2 x = D2::new(vmulsign_vd_vd_vd($f64x::splat(-3.141592653589793116 * 0.5), y), 
				 vmulsign_vd_vd_vd($f64x::splat(-1.2246467991473532072e-16 * 0.5), y));
    x = ddidd + x;
    ddidd = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(o), x, ddidd);
    d = ddidd.0 + ddidd.1;
    d = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(r) | visnan_vo_vd(r), vreinterpret_vm_vd(d)));
  }

  s = d*d;

  d = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(2)), $ix::splat(0))), vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(d)));

  u = $f64x::splat(-7.97255955009037868891952e-18)
      .mla(s, $f64x::splat(2.81009972710863200091251e-15))
      .mla(s, $f64x::splat(-7.64712219118158833288484e-13))
      .mla(s, $f64x::splat(1.60590430605664501629054e-10))
      .mla(s, $f64x::splat(-2.50521083763502045810755e-08))
      .mla(s, $f64x::splat(2.75573192239198747630416e-06))
      .mla(s, $f64x::splat(-0.000198412698412696162806809))
      .mla(s, $f64x::splat(0.00833333333333332974823815))
      .mla(s, $f64x::splat(-0.166666666666666657414808));

  u = s*(u*d) + d;
  
  return u;
}

pub fn xcos_u1(d: $f64x) -> $f64x {
  $f64x u;
  D2 s, t, x;
  $ix ql;
  
  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX2))))) {
    $f64x dql = vrint_vd_vd(d.mla($f64x::splat(M_1_PI), $f64x::splat(-0.5)));
    dql = $f64x::splat(2.).mla(dql, $f64x::splat(1.));
    ql = vrint_vi_vd(dql);
    s = d.add_as_d2(dql*$f64x::splat(-PI_A2*0.5));
    s = s.add_checked(dql*$f64x::splat(-PI_B2*0.5));
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX))))) {
    $f64x dqh = vtruncate_vd_vd(d.mla($f64x::splat(M_1_PI / D1_23), $f64x::splat(-M_1_PI / D1_24)));
    ql = vrint_vi_vd(d*$f64x::splat(M_1_PI) +
					dqh.mla($f64x::splat(-D1_23), $f64x::splat(-0.5)));
    dqh = dqh*$f64x::splat(D1_24);
    ql = ql + ql + $ix::splat(1);
    const $f64x dql = $f64x::from(ql);

    u = dqh.mla($f64x::splat(-PI_A * 0.5), d);
    s = u.add_as_d2(dql*$f64x::splat(-PI_A*0.5));
    s += dqh*$f64x::splat(-PI_B*0.5);
    s += dql*$f64x::splat(-PI_B*0.5);
    s += dqh*$f64x::splat(-PI_C*0.5);
    s += dql*$f64x::splat(-PI_C*0.5);
    s = s.add_checked((dqh + dql)*$f64x::splat(-PI_D*0.5));
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = vand_vi_vi_vi(ddii, $ix::splat(3));
    ql = ql + ql + vsel_vi_vo_vi_vi(vcast_vo32_vo64(ddidd.0.gt($f64x::splat(0.))), $ix::splat(8), $ix::splat(7));
    ql = vsra_vi_vi_i(ql, 1);
    $mx o = veq_vo_vi_vi(vand_vi_vi_vi(ddii, $ix::splat(1)), $ix::splat(0));
    $f64x y = vsel_vd_vo_vd_vd(ddidd.0.gt($f64x::splat(0.)), $f64x::splat(0.), $f64x::splat(-1.));
    D2 x = D2::new(vmulsign_vd_vd_vd($f64x::splat(-3.141592653589793116 * 0.5), y), 
				 vmulsign_vd_vd_vd($f64x::splat(-1.2246467991473532072e-16 * 0.5), y));
    x = ddidd + x;
    ddidd = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(o), x, ddidd);
    s = ddidd.normalize();
    s.0 = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d) | visnan_vo_vd(d), vreinterpret_vm_vd(s.0)));
  }
  
  t = s;
  s = s.square();

  u = $f64x::splat(2.72052416138529567917983e-15)
      .mla(s.0, $f64x::splat(-7.6429259411395447190023e-13))
      .mla(s.0, $f64x::splat(1.60589370117277896211623e-10))
      .mla(s.0, $f64x::splat(-2.5052106814843123359368e-08))
      .mla(s.0, $f64x::splat(2.75573192104428224777379e-06))
      .mla(s.0, $f64x::splat(-0.000198412698412046454654947))
      .mla(s.0, $f64x::splat(0.00833333333333318056201922));

  x = $f64x::splat(1.).add_checked(($f64x::splat(-0.166666666666666657414808).add_checked_as_d2(u*s.0)) * s);

  u = t.mul_as_d(x);
  
  u = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(2)), $ix::splat(0))), vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(u)));
  
  return u;
}


pub fn xsincos(d: $f64x) -> ($f64x, $f64x) {
  $mx o;
  $f64x u, t, rx, ry, s;
  D2 r;
  $ix ql;

  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX2))))) {
    $f64x dql = vrint_vd_vd(d*$f64x::splat(2 * M_1_PI));
    ql = vrint_vi_vd(dql);
    s = dql.mla($f64x::splat(-PI_A2 * 0.5), d);
    s = dql.mla($f64x::splat(-PI_B2 * 0.5), s);
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX))))) {
    $f64x dqh = vtruncate_vd_vd(d*$f64x::splat(2*M_1_PI / D1_24));
    dqh = dqh*$f64x::splat(D1_24);
    $f64x dql = vrint_vd_vd(d*$f64x::splat(2*M_1_PI) - dqh);
    ql = vrint_vi_vd(dql);

    s = dqh.mla($f64x::splat(-PI_A * 0.5), d);
    s = dql.mla($f64x::splat(-PI_A * 0.5), s);
    s = dqh.mla($f64x::splat(-PI_B * 0.5), s);
    s = dql.mla($f64x::splat(-PI_B * 0.5), s);
    s = dqh.mla($f64x::splat(-PI_C * 0.5), s);
    s = dql.mla($f64x::splat(-PI_C * 0.5), s);
    s = (dqh + dql).mla($f64x::splat(-PI_D * 0.5), s);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = ddii;
    s = ddidd.0 + ddidd.1;
    s = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d) | visnan_vo_vd(d), vreinterpret_vm_vd(s)));
  }
  
  t = s;

  s = s*s;

  u = $f64x::splat(1.58938307283228937328511e-10)
      .mla(s, $f64x::splat(-2.50506943502539773349318e-08))
      .mla(s, $f64x::splat(2.75573131776846360512547e-06))
      .mla(s, $f64x::splat(-0.000198412698278911770864914))
      .mla(s, $f64x::splat(0.0083333333333191845961746))
      .mla(s, $f64x::splat(-0.166666666666666130709393));

  rx = (u*s).mla(t, t);
  rx = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), $f64x::splat(-0.), rx);

  u = $f64x::splat(-1.13615350239097429531523e-11)
      .mla(s, $f64x::splat(2.08757471207040055479366e-09))
      .mla(s, $f64x::splat(-2.75573144028847567498567e-07))
      .mla(s, $f64x::splat(2.48015872890001867311915e-05))
      .mla(s, $f64x::splat(-0.00138888888888714019282329))
      .mla(s, $f64x::splat(0.0416666666666665519592062))
      .mla(s, $f64x::splat(-0.5));

  ry = s.mla(u, $f64x::splat(1.));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(1)), $ix::splat(0)));
  rsin = vsel_vd_vo_vd_vd(o, rx, ry);
  rcos = vsel_vd_vo_vd_vd(o, ry, rx);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(2)), $ix::splat(2)));
  rsin = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(rsin)));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql + $ix::splat(1), $ix::splat(2)), $ix::splat(2)));
  rcos = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(rcos)));
  
  (rsin, rcos)
}

pub fn xsincos_u1(d: $f64x) -> ($f64x, $f64x) {
  $mx o;
  $f64x u, rx, ry;
  D2 r, s, t, x;
  $ix ql;
  
  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX2))))) {
    const $f64x dql = vrint_vd_vd(d*$f64x::splat(2 * M_1_PI));
    ql = vrint_vi_vd(dql);
    u = dql.mla($f64x::splat(-PI_A2*0.5), d);
    s = u.add_checked_as_d2(dql*$f64x::splat(-PI_B2*0.5));
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX))))) {
    $f64x dqh = vtruncate_vd_vd(d*$f64x::splat(2*M_1_PI / D1_24));
    dqh = dqh*$f64x::splat(D1_24);
    const $f64x dql = vrint_vd_vd(d*$f64x::splat(2*M_1_PI) - dqh);
    ql = vrint_vi_vd(dql);
    
    u = dqh.mla($f64x::splat(-PI_A * 0.5), d);
    s = u.add_checked_as_d2(dql*$f64x::splat(-PI_A*0.5));
    s += dqh*$f64x::splat(-PI_B*0.5);
    s += dql*$f64x::splat(-PI_B*0.5);
    s += dqh*$f64x::splat(-PI_C*0.5);
    s += dql*$f64x::splat(-PI_C*0.5);
    s += (dqh + dql)*$f64x::splat(-PI_D*0.5);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = ddii;
    s = ddidd;
    o = visinf_vo_vd(d) | visnan_vo_vd(d);
    s.0 = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(s.0)));
    s.1 = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(s.1)));
  }
  
  t = s;

  s.0 = s.square_as_d();
  
  u = $f64x::splat(1.58938307283228937328511e-10)
      .mla(s.0, $f64x::splat(-2.50506943502539773349318e-08))
      .mla(s.0, $f64x::splat(2.75573131776846360512547e-06))
      .mla(s.0, $f64x::splat(-0.000198412698278911770864914))
      .mla(s.0, $f64x::splat(0.0083333333333191845961746))
      .mla(s.0, $f64x::splat(-0.166666666666666130709393));

  u = u*(s.0*t.0);

  x = t.add_checked(u);
  rx = x.0 + x.1;

  rx = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), $f64x::splat(-0.), rx);
  
  u = $f64x::splat(-1.13615350239097429531523e-11)
      .mla(s.0, $f64x::splat(2.08757471207040055479366e-09))
      .mla(s.0, $f64x::splat(-2.75573144028847567498567e-07))
      .mla(s.0, $f64x::splat(2.48015872890001867311915e-05))
      .mla(s.0, $f64x::splat(-0.00138888888888714019282329))
      .mla(s.0, $f64x::splat(0.0416666666666665519592062))
      .mla(s.0, $f64x::splat(-0.5));

  x = $f64x::splat(1.).add_checked(s.0.mul_as_d2(u));
  ry = x.0 + x.1;

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(1)), $ix::splat(0)));
  rsin = vsel_vd_vo_vd_vd(o, rx, ry);
  rcos = vsel_vd_vo_vd_vd(o, ry, rx);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(2)), $ix::splat(2)));
  rsin = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(rsin)));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql + $ix::splat(1), $ix::splat(2)), $ix::splat(2)));
  rcos = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(rcos)));

  (rsin, rcos)
}

pub fn xsincospi_u05(d: $f64x) -> ($f64x, $f64x) {
  $mx o;
  $f64x u, s, t, rx, ry;
  D2 r, x, s2;

  u = d*$f64x::splat(4.);
  $ix q = vtruncate_vi_vd(u);
  q = vand_vi_vi_vi(q + vxor_vi_vi_vi(vsrl_vi_vi_i(q, 31), $ix::splat(1)), $ix::splat(!1));
  s = u - $f64x::from(q);
  
  t = s;
  s = s*s;
  s2 = t*t;
  
  //

  u = $f64x::splat(-2.02461120785182399295868e-14)
      .mla(s, $f64x::splat(6.94821830580179461327784e-12))
      .mla(s, $f64x::splat(-1.75724749952853179952664e-09))
      .mla(s, $f64x::splat(3.13361688966868392878422e-07))
      .mla(s, $f64x::splat(-3.6576204182161551920361e-05))
      .mla(s, $f64x::splat(0.00249039457019271850274356));
  x = u*s + D2::from((-0.0807455121882807852484731, 3.61852475067037104849987e-18));
  x = s2 * x + D2::from((0.785398163397448278999491, 3.06287113727155002607105e-17));

  x *= t;
  rx = x.0 + x.1;

  rx = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), $f64x::splat(-0.), rx);
  
  //
  
  u = $f64x::splat(9.94480387626843774090208e-16)
      .mla(s, $f64x::splat(-3.89796226062932799164047e-13))
      .mla(s, $f64x::splat(1.15011582539996035266901e-10))
      .mla(s, $f64x::splat(-2.4611369501044697495359e-08))
      .mla(s, $f64x::splat(3.59086044859052754005062e-06))
      .mla(s, $f64x::splat(-0.000325991886927389905997954));
  x = u*s + D2::from((0.0158543442438155018914259, -1.04693272280631521908845e-18));
  x = s2 * x + D2::from((-0.308425137534042437259529, -1.95698492133633550338345e-17));

  x = x * s2 + $f64x::splat(1.);
  ry = x.0 + x.1;

  //

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(2)), $ix::splat(0)));
  rsin = vsel_vd_vo_vd_vd(o, rx, ry);
  rcos = vsel_vd_vo_vd_vd(o, ry, rx);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(4)), $ix::splat(4)));
  rsin = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(rsin)));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q + $ix::splat(2), $ix::splat(4)), $ix::splat(4)));
  rcos = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(rcos)));

  o = vabs_vd_vd(d).gt($f64x::splat(TRIGRANGEMAX3/4.));
  rsin = vreinterpret_vd_vm(vandnot_vm_vo64_vm(o, vreinterpret_vm_vd(rsin)));
  rcos = vsel_vd_vo_vd_vd(o, $f64x::splat(1.), rcos);

  o = visinf_vo_vd(d);
  rsin = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(rsin)));
  rcos = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(rcos)));

  (rsin, rcos)
}

pub fn xsincospi_u35(d: $f64x) -> ($f64x, $f64x) {
  $mx o;
  $f64x u, s, t, rx, ry;
  D2 r;

  u = d*$f64x::splat(4.);
  $ix q = vtruncate_vi_vd(u);
  q = vand_vi_vi_vi(q + vxor_vi_vi_vi(vsrl_vi_vi_i(q, 31), $ix::splat(1)), $ix::splat(!1));
  s = u - $f64x::from(q);

  t = s;
  s = s*s;
  
  //

  u = $f64x::splat(+0.6880638894766060136e-11)
      .mla(s, $f64x::splat(-0.1757159564542310199e-8))
      .mla(s, $f64x::splat(+0.3133616327257867311e-6))
      .mla(s, $f64x::splat(-0.3657620416388486452e-4))
      .mla(s, $f64x::splat(+0.2490394570189932103e-2))
      .mla(s, $f64x::splat(-0.8074551218828056320e-1))
      .mla(s, $f64x::splat(+0.7853981633974482790e+0));

  rx = u*t;

  //
  
  u = $f64x::splat(-0.3860141213683794352e-12)
      .mla(s, $f64x::splat(+0.1150057888029681415e-9))
      .mla(s, $f64x::splat(-0.2461136493006663553e-7))
      .mla(s, $f64x::splat(+0.3590860446623516713e-5))
      .mla(s, $f64x::splat(-0.3259918869269435942e-3))
      .mla(s, $f64x::splat(+0.1585434424381541169e-1))
      .mla(s, $f64x::splat(-0.3084251375340424373e+0))
      .mla(s, $f64x::splat(1.));

  ry = u;

  //

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(2)), $ix::splat(0)));
  rsin = vsel_vd_vo_vd_vd(o, rx, ry);
  rcos = vsel_vd_vo_vd_vd(o, ry, rx);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(4)), $ix::splat(4)));
  rsin = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(rsin)));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q + $ix::splat(2), $ix::splat(4)), $ix::splat(4)));
  rcos = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(rcos)));

  o = vabs_vd_vd(d).gt($f64x::splat(TRIGRANGEMAX3/4.));
  rsin = vreinterpret_vd_vm(vandnot_vm_vo64_vm(o, vreinterpret_vm_vd(rsin)));
  rcos = vreinterpret_vd_vm(vandnot_vm_vo64_vm(o, vreinterpret_vm_vd(rcos)));

  o = visinf_vo_vd(d);
  rsin = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(rsin)));
  rcos = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(rcos)));

  (rsin, rcos)
}

pub fn xmodf(x: $f64x) -> ($f64x, $f64x) {
  $f64x fr = x - $f64x::splat(D1_31)*$f64x::from(vtruncate_vi_vd(x*$f64x::splat(1. / D1_31)));
  fr -= $f64x::from(vtruncate_vi_vd(fr));
  fr = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(D1_52)), $f64x::splat(0.), fr);

  D2 ret;

  retx = vcopysign_vd_vd_vd(fr, x);
  rety = vcopysign_vd_vd_vd(x - fr, x);

  (retx, rety)
}

#[inline]
fn sinpik(d: $f64x) -> D2 {
  $mx o;
  $f64x u, s, t;
  D2 x, s2;

  u = d*$f64x::splat(4.);
  $ix q = vtruncate_vi_vd(u);
  q = vand_vi_vi_vi(q + vxor_vi_vi_vi(vsrl_vi_vi_i(q, 31), $ix::splat(1)), $ix::splat(!1));
  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(2)), $ix::splat(2)));

  s = u - $f64x::from(q);
  t = s;
  s = s*s;
  s2 = t*t;

  //

  u = vsel_vd_vo_d_d(o, 9.94480387626843774090208e-16, -2.02461120785182399295868e-14)
      .mla(s, vsel_vd_vo_d_d(o, -3.89796226062932799164047e-13, 6.948218305801794613277840e-12))
      .mla(s, vsel_vd_vo_d_d(o, 1.150115825399960352669010e-10, -1.75724749952853179952664e-09))
      .mla(s, vsel_vd_vo_d_d(o, -2.46113695010446974953590e-08, 3.133616889668683928784220e-07))
      .mla(s, vsel_vd_vo_d_d(o, 3.590860448590527540050620e-06, -3.65762041821615519203610e-05))
      .mla(s, vsel_vd_vo_d_d(o, -0.000325991886927389905997954, 0.0024903945701927185027435600));
  x = u*s + vsel_vd2_vo_d_d_d_d(o, 0.0158543442438155018914259, -1.04693272280631521908845e-18,
					    -0.0807455121882807852484731, 3.61852475067037104849987e-18);
  x = s2 * x + vsel_vd2_vo_d_d_d_d(o, -0.308425137534042437259529, -1.95698492133633550338345e-17,
					     0.785398163397448278999491, 3.06287113727155002607105e-17);

  x *= vsel_vd2_vo_vd2_vd2(o, s2, D2::new(t, $f64x::splat(0.)));
  x = vsel_vd2_vo_vd2_vd2(o, x + $f64x::splat(1.), x);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(4)), $ix::splat(4)));
  x.0 = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(x.0)));
  x.1 = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(x.1)));

  return x;
}

pub fn xsinpi_u05(d: $f64x) -> $f64x {
  D2 x = sinpik(d);
  $f64x r = x.0 + x.1;

  r = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), $f64x::splat(-0.), r);
  r = vreinterpret_vd_vm(vandnot_vm_vo64_vm(vabs_vd_vd(d).gt($f64x::splat(TRIGRANGEMAX3/4.)), vreinterpret_vm_vd(r)));
  r = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d), vreinterpret_vm_vd(r)));
  
  return r;
}
#[inline]
fn cospik(d: $f64x) -> D2 {
  $mx o;
  $f64x u, s, t;
  D2 x, s2;

  u = d*$f64x::splat(4.);
  $ix q = vtruncate_vi_vd(u);
  q = vand_vi_vi_vi(q + vxor_vi_vi_vi(vsrl_vi_vi_i(q, 31), $ix::splat(1)), $ix::splat(!1));
  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(2)), $ix::splat(0)));

  s = u - $f64x::from(q);
  t = s;
  s = s*s;
  s2 = t.mul_as_d2(t);
  
  //

  u = vsel_vd_vo_d_d(o, 9.94480387626843774090208e-16, -2.02461120785182399295868e-14)
      .mla(s, vsel_vd_vo_d_d(o, -3.89796226062932799164047e-13, 6.948218305801794613277840e-12))
      .mla(s, vsel_vd_vo_d_d(o, 1.150115825399960352669010e-10, -1.75724749952853179952664e-09))
      .mla(s, vsel_vd_vo_d_d(o, -2.46113695010446974953590e-08, 3.133616889668683928784220e-07))
      .mla(s, vsel_vd_vo_d_d(o, 3.590860448590527540050620e-06, -3.65762041821615519203610e-05))
      .mla(s, vsel_vd_vo_d_d(o, -0.000325991886927389905997954, 0.0024903945701927185027435600));
  x = u*s + vsel_vd2_vo_d_d_d_d(o, 0.0158543442438155018914259, -1.04693272280631521908845e-18,
					    -0.0807455121882807852484731, 3.61852475067037104849987e-18);
  x = s2 * x +
			 vsel_vd2_vo_d_d_d_d(o, -0.308425137534042437259529, -1.95698492133633550338345e-17,
					     0.785398163397448278999491, 3.06287113727155002607105e-17);

  x *= vsel_vd2_vo_vd2_vd2(o, s2, D2::new(t, $f64x::splat(0.)));
  x = vsel_vd2_vo_vd2_vd2(o, x + $f64x::splat(1.), x);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q + $ix::splat(2), $ix::splat(4)), $ix::splat(4)));
  x.0 = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(x.0)));
  x.1 = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(x.1)));

  return x;
}

pub fn xcospi_u05(d: $f64x) -> $f64x {
  D2 x = cospik(d);
  $f64x r = x.0 + x.1;

  r = vsel_vd_vo_vd_vd(vabs_vd_vd(d).gt($f64x::splat(TRIGRANGEMAX3/4.)), $f64x::splat(1.), r);
  r = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d), vreinterpret_vm_vd(r)));
  
  return r;
}

pub fn xtan(d: $f64x) -> $f64x {
  $f64x u, s, x;
  $mx o;
  $ix ql;

  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX2))))) {
    $f64x dql = vrint_vd_vd(d*$f64x::splat(2 * M_1_PI));
    ql = vrint_vi_vd(dql);
    x = dql.mla($f64x::splat(-PI_A2 * 0.5), d);
    x = dql.mla($f64x::splat(-PI_B2 * 0.5), x);
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(1e+7))))) {
    $f64x dqh = vtruncate_vd_vd(d*$f64x::splat(2*M_1_PI / D1_24));
    dqh = dqh*$f64x::splat(D1_24);
    $f64x dql = vrint_vd_vd(d*$f64x::splat(2*M_1_PI) - dqh);
    ql = vrint_vi_vd(dql);

    x = dqh.mla($f64x::splat(-PI_A * 0.5), d);
    x = dql.mla($f64x::splat(-PI_A * 0.5), x);
    x = dqh.mla($f64x::splat(-PI_B * 0.5), x);
    x = dql.mla($f64x::splat(-PI_B * 0.5), x);
    x = dqh.mla($f64x::splat(-PI_C * 0.5), x);
    x = dql.mla($f64x::splat(-PI_C * 0.5), x);
    x = (dqh + dql).mla($f64x::splat(-PI_D * 0.5), x);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = ddii;
    x = ddidd.0 + ddidd.1;
    x = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d), vreinterpret_vm_vd(x)));
    x = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d) | visnan_vo_vd(d), vreinterpret_vm_vd(x)));
  }

  s = x*x;

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(1)), $ix::splat(1)));
  x = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(x)));

#ifdef SPLIT_KERNEL
  $f64x s2 = s*s;
  v;

  u = $f64x::splat(-4.31184585467324750724175e-05)
      .mla(s2, $f64x::splat(-0.000137892809714281708733524))
      .mla(s2, $f64x::splat(-6.07500301486087879295969e-05))
      .mla(s2, $f64x::splat(0.000219040550724571513561967))
      .mla(s2, $f64x::splat(0.00145461240472358871965441))
      .mla(s2, $f64x::splat(0.00886321546662684547901456))
      .mla(s2, $f64x::splat(0.0539682539049961967903002))
      .mla(s2, $f64x::splat(0.333333333333320047664472));

  v = $f64x::splat(9.99583485362149960784268e-06)
      .mla(s2, $f64x::splat(0.000103573238391744000389851))
      .mla(s2, $f64x::splat(0.000157624358465342784274554))
      .mla(s2, $f64x::splat(0.000148898734751616411290179))
      .mla(s2, $f64x::splat(0.000595799595197098359744547))
      .mla(s2, $f64x::splat(0.0035923150771440177410343))
      .mla(s2, $f64x::splat(0.0218694899718446938985394))
      .mla(s2, $f64x::splat(0.133333333334818976423364));
  
  u = v.mla(s, u);
#else
  u = $f64x::splat(9.99583485362149960784268e-06)
      .mla(s, $f64x::splat(-4.31184585467324750724175e-05))
      .mla(s, $f64x::splat(0.000103573238391744000389851))
      .mla(s, $f64x::splat(-0.000137892809714281708733524))
      .mla(s, $f64x::splat(0.000157624358465342784274554))
      .mla(s, $f64x::splat(-6.07500301486087879295969e-05))
      .mla(s, $f64x::splat(0.000148898734751616411290179))
      .mla(s, $f64x::splat(0.000219040550724571513561967))
      .mla(s, $f64x::splat(0.000595799595197098359744547))
      .mla(s, $f64x::splat(0.00145461240472358871965441))
      .mla(s, $f64x::splat(0.0035923150771440177410343))
      .mla(s, $f64x::splat(0.00886321546662684547901456))
      .mla(s, $f64x::splat(0.0218694899718446938985394))
      .mla(s, $f64x::splat(0.0539682539049961967903002))
      .mla(s, $f64x::splat(0.133333333334818976423364))
      .mla(s, $f64x::splat(0.333333333333320047664472));
#endif
  
  u = s.mla(u*x, x);

  u = vsel_vd_vo_vd_vd(o, vrec_vd_vd(u), u);
  u = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), d, u);
  
  return u;
}

pub fn xtan_u1(d: $f64x) -> $f64x {
  $f64x u;
  D2 s, t, x;
  $mx o;
  $ix ql;
  
  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX2))))) {
    $f64x dql = vrint_vd_vd(d*$f64x::splat(2 * M_1_PI));
    ql = vrint_vi_vd(dql);
    u = dql.mla($f64x::splat(-PI_A2*0.5), d);
    s = u.add_checked_as_d2(dql*$f64x::splat(-PI_B2*0.5));
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt($f64x::splat(TRIGRANGEMAX))))) {
    $f64x dqh = vtruncate_vd_vd(d*$f64x::splat(2*M_1_PI / D1_24));
    dqh = dqh*$f64x::splat(D1_24);
    s = D2::from((M_2_PI_H, M_2_PI_L)) * d +
			  (vsel_vd_vo_vd_vd(d.lt($f64x::splat(0.)),
							 $f64x::splat(-0.5), $f64x::splat(0.5)) - dqh);
    const $f64x dql = vtruncate_vd_vd(s.0 + s.1);
    ql = vrint_vi_vd(dql);

    u = dqh.mla($f64x::splat(-PI_A * 0.5), d);
    s = u.add_checked_as_d2(dql*$f64x::splat(-PI_A*0.5));
    s += dqh*$f64x::splat(-PI_B*0.5);
    s += dql*$f64x::splat(-PI_B*0.5);
    s += dqh*$f64x::splat(-PI_C*0.5);
    s += dql*$f64x::splat(-PI_C*0.5);
    s += (dqh + dql)*$f64x::splat(-PI_D*0.5);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = ddii;
    s = ddidd;
    o = visinf_vo_vd(d) | visnan_vo_vd(d);
    s.0 = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(s.0)));
    s.1 = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(s.1)));
  }
  
  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, $ix::splat(1)), $ix::splat(1)));
  vmask n = vand_vm_vo64_vm(o, vreinterpret_vm_vd($f64x::splat(-0.)));
  s.0 = vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(s.0), n));
  s.1 = vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(s.1), n));

  t = s;
  s = s.square();

#ifdef SPLIT_KERNEL
  $f64x sx2 = s.0*s.0;
  v;

  u = $f64x::splat(-2.59519791585924697698614e-05)
      .mla(sx2, $f64x::splat(-3.05033014433946488225616e-05))
      .mla(sx2, $f64x::splat(8.09674518280159187045078e-05))
      .mla(sx2, $f64x::splat(0.000588505168743587154904506))
      .mla(sx2, $f64x::splat(0.00359208743836906619142924))
      .mla(sx2, $f64x::splat(0.0218694882853846389592078))
      .mla(sx2, $f64x::splat(0.133333333333125941821962));

  v = $f64x::splat(1.01419718511083373224408e-05)
      .mla(sx2, $f64x::splat(5.23388081915899855325186e-05))
      .mla(sx2, $f64x::splat(7.14707504084242744267497e-05))
      .mla(sx2, $f64x::splat(0.000244884931879331847054404))
      .mla(sx2, $f64x::splat(0.00145612788922812427978848))
      .mla(sx2, $f64x::splat(0.00886323944362401618113356))
      .mla(sx2, $f64x::splat(0.0539682539781298417636002));

  u = v.mla(s.0, u);
#else
  u = $f64x::splat(1.01419718511083373224408e-05)
      .mla(s.0, $f64x::splat(-2.59519791585924697698614e-05))
      .mla(s.0, $f64x::splat(5.23388081915899855325186e-05))
      .mla(s.0, $f64x::splat(-3.05033014433946488225616e-05))
      .mla(s.0, $f64x::splat(7.14707504084242744267497e-05))
      .mla(s.0, $f64x::splat(8.09674518280159187045078e-05))
      .mla(s.0, $f64x::splat(0.000244884931879331847054404))
      .mla(s.0, $f64x::splat(0.000588505168743587154904506))
      .mla(s.0, $f64x::splat(0.00145612788922812427978848))
      .mla(s.0, $f64x::splat(0.00359208743836906619142924))
      .mla(s.0, $f64x::splat(0.00886323944362401618113356))
      .mla(s.0, $f64x::splat(0.0218694882853846389592078))
      .mla(s.0, $f64x::splat(0.0539682539781298417636002))
      .mla(s.0, $f64x::splat(0.133333333333125941821962));
#endif
  
  x = $f64x::splat(1.).add_checked($f64x::splat(0.333333333333334980164153).add_checked_as_d2(u*s.0) * s);
  x = t * x;

  x = vsel_vd2_vo_vd2_vd2(o, x.rec(), x);

  u = x.0 + x.1;

  u = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), d, u);

  return u;
}
#[inline]
fn atan2k(y: $f64x, x: $f64x) -> $f64x {
  $f64x s, t, u;
  $ix q;
  $mx p;

  q = vsel_vi_vd_vi(x, $ix::splat(-2));
  x = vabs_vd_vd(x);

  q = vsel_vi_vd_vd_vi_vi(x, y, q + $ix::splat(1), q);
  p = x.lt(y);
  s = vsel_vd_vo_vd_vd(p, -x, y);
  t = x.max(y);

  s = s / t;
  t = s*s;

#ifdef SPLIT_KERNEL
  $f64x t2 = t*t;
  v;

  u = $f64x::splat(-1.88796008463073496563746e-05)
      .mla(t2, $f64x::splat(-0.00110611831486672482563471))
      .mla(t2, $f64x::splat(-0.00889896195887655491740809))
      .mla(t2, $f64x::splat(-0.0254517624932312641616861))
      .mla(t2, $f64x::splat(-0.0407629191276836500001934))
      .mla(t2, $f64x::splat(-0.0523674852303482457616113))
      .mla(t2, $f64x::splat(-0.0666573579361080525984562))
      .mla(t2, $f64x::splat(-0.090908995008245008229153))
      .mla(t2, $f64x::splat(-0.14285714266771329383765))
      .mla(t2, $f64x::splat(-0.333333333333311110369124));

  v = $f64x::splat(0.000209850076645816976906797)
      .mla(t2, $f64x::splat(0.00370026744188713119232403))
      .mla(t2, $f64x::splat(0.016599329773529201970117))
      .mla(t2, $f64x::splat(0.0337852580001353069993897))
      .mla(t2, $f64x::splat(0.0466667150077840625632675))
      .mla(t2, $f64x::splat(0.0587666392926673580854313))
      .mla(t2, $f64x::splat(0.0769219538311769618355029))
      .mla(t2, $f64x::splat(0.111111105648261418443745))
      .mla(t2, $f64x::splat(0.199999999996591265594148));

  u = v.mla(t, u);
#else
  u = $f64x::splat(-1.88796008463073496563746e-05)
      .mla(t, $f64x::splat(0.000209850076645816976906797))
      .mla(t, $f64x::splat(-0.00110611831486672482563471))
      .mla(t, $f64x::splat(0.00370026744188713119232403))
      .mla(t, $f64x::splat(-0.00889896195887655491740809))
      .mla(t, $f64x::splat(0.016599329773529201970117))
      .mla(t, $f64x::splat(-0.0254517624932312641616861))
      .mla(t, $f64x::splat(0.0337852580001353069993897))
      .mla(t, $f64x::splat(-0.0407629191276836500001934))
      .mla(t, $f64x::splat(0.0466667150077840625632675))
      .mla(t, $f64x::splat(-0.0523674852303482457616113))
      .mla(t, $f64x::splat(0.0587666392926673580854313))
      .mla(t, $f64x::splat(-0.0666573579361080525984562))
      .mla(t, $f64x::splat(0.0769219538311769618355029))
      .mla(t, $f64x::splat(-0.090908995008245008229153))
      .mla(t, $f64x::splat(0.111111105648261418443745))
      .mla(t, $f64x::splat(-0.14285714266771329383765))
      .mla(t, $f64x::splat(0.199999999996591265594148))
      .mla(t, $f64x::splat(-0.333333333333311110369124));
#endif
  
  t = s.mla(t*u, s);
  t = $f64x::from(q).mla($f64x::splat(M_PI/2.), t);

  return t;
}
#[inline]
fn atan2k_u1(D2 y, D2 x) -> D2 {
  $f64x u;
  D2 s, t;
  $ix q;
  $mx p;

  q = vsel_vi_vd_vi(x.0, $ix::splat(-2));
  p = x.0.lt($f64x::splat(0.));
  vmask b = vand_vm_vo64_vm(p, vreinterpret_vm_vd($f64x::splat(-0.)));
  x.0 = vreinterpret_vd_vm(vxor_vm_vm_vm(b, vreinterpret_vm_vd(x.0)));
  x.1 = vreinterpret_vd_vm(vxor_vm_vm_vm(b, vreinterpret_vm_vd(x.1)));

  q = vsel_vi_vd_vd_vi_vi(x.0, y.0, q + $ix::splat(1), q);
  p = x.0.lt(y.0);
  s = vsel_vd2_vo_vd2_vd2(p, -x, y);
  t = vsel_vd2_vo_vd2_vd2(p, y, x);

  s = s / t;
  t = s.square();
  t = t.normalize();

#ifdef SPLIT_KERNEL
  $f64x tx3 = t.0*t.0*t.0;
  v;

  u = $f64x::splat(0.00070557664296393412389774)
      .mla(t.0, $f64x::splat(-0.00251865614498713360352999))
      .mla(tx3, $f64x::splat(0.0208024799924145797902497))
      .mla(t.0, $f64x::splat(-0.0289002344784740315686289))
      .mla(tx3, $f64x::splat(0.0470843011653283988193763))
      .mla(t.0, $f64x::splat(-0.0524914210588448421068719))
      .mla(tx3, $f64x::splat(0.0769225330296203768654095))
      .mla(t.0, $f64x::splat(-0.0909090442773387574781907))
      .mla(tx3, $f64x::splat(0.199999999997977351284817))
      .mla(t.0, $f64x::splat(-0.333333333333317605173818));
  
  v = $f64x::splat(1.06298484191448746607415e-05)
      .mla(t.0, $f64x::splat(-0.000125620649967286867384336))
      .mla(tx3, $f64x::splat(0.00646262899036991172313504))
      .mla(t.0, $f64x::splat(-0.0128281333663399031014274))
      .mla(tx3, $f64x::splat(0.0359785005035104590853656))
      .mla(t.0, $f64x::splat(-0.041848579703592507506027))
      .mla(tx3, $f64x::splat(0.0587946590969581003860434))
      .mla(t.0, $f64x::splat(-0.0666620884778795497194182))
      .mla(tx3, $f64x::splat(0.111111108376896236538123))
      .mla(t.0, $f64x::splat(-0.142857142756268568062339));
  
  u = v.mla(t.0*t.0, u);
#else
  u = $f64x::splat(1.06298484191448746607415e-05)
      .mla(t.0, $f64x::splat(-0.000125620649967286867384336))
      .mla(t.0, $f64x::splat(0.00070557664296393412389774))
      .mla(t.0, $f64x::splat(-0.00251865614498713360352999))
      .mla(t.0, $f64x::splat(0.00646262899036991172313504))
      .mla(t.0, $f64x::splat(-0.0128281333663399031014274))
      .mla(t.0, $f64x::splat(0.0208024799924145797902497))
      .mla(t.0, $f64x::splat(-0.0289002344784740315686289))
      .mla(t.0, $f64x::splat(0.0359785005035104590853656))
      .mla(t.0, $f64x::splat(-0.041848579703592507506027))
      .mla(t.0, $f64x::splat(0.0470843011653283988193763))
      .mla(t.0, $f64x::splat(-0.0524914210588448421068719))
      .mla(t.0, $f64x::splat(0.0587946590969581003860434))
      .mla(t.0, $f64x::splat(-0.0666620884778795497194182))
      .mla(t.0, $f64x::splat(0.0769225330296203768654095))
      .mla(t.0, $f64x::splat(-0.0909090442773387574781907))
      .mla(t.0, $f64x::splat(0.111111108376896236538123))
      .mla(t.0, $f64x::splat(-0.142857142756268568062339))
      .mla(t.0, $f64x::splat(0.199999999997977351284817))
      .mla(t.0, $f64x::splat(-0.333333333333317605173818));
#endif
  
  t *= u;
  t = s * $f64x::splat(1.).add_checked(t);
  t = (D2::from((1.570796326794896557998982, 6.12323399573676603586882e-17)) * $f64x::from(q)).add_checked(t);

  return t;
}
#[inline]
fn visinf2_vd_vd_vd(d: $f64x, m: $f64x) -> $f64x {
  return vreinterpret_vd_vm(vand_vm_vo64_vm(visinf_vo_vd(d), vor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(m))));
}

pub fn xatan2(y: $f64x, x: $f64x) -> $f64x {
  $f64x r = atan2k(vabs_vd_vd(y), x);

  r = vmulsign_vd_vd_vd(r, x);
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(x) | x.eq($f64x::splat(0.)), $f64x::splat(M_PI/2.) - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd($f64x::splat(M_PI/2.), x)), r);
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(y), $f64x::splat(M_PI/2.) - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd($f64x::splat(M_PI/4.), x)), r);
  r = vsel_vd_vo_vd_vd(y.eq($f64x::splat(0.)), vreinterpret_vd_vm(vand_vm_vo64_vm(vsignbit_vo_vd(x), vreinterpret_vm_vd($f64x::splat(M_PI)))), r);

  r = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x) | visnan_vo_vd(y), vreinterpret_vm_vd(vmulsign_vd_vd_vd(r, y))));
  return r;
}

pub fn xatan2_u1(y: $f64x, x: $f64x) -> $f64x {
  $mx o = vabs_vd_vd(x).lt($f64x::splat(5.5626846462680083984e-309)); // nexttoward((1.0 / DBL_MAX), 1)
  x = vsel_vd_vo_vd_vd(o, x*$f64x::splat(D1_53), x);
  y = vsel_vd_vo_vd_vd(o, y*$f64x::splat(D1_23), y);

  D2 d = atan2k_u1(D2::new(vabs_vd_vd(y), $f64x::splat(0.)), D2::new(x, $f64x::splat(0.)));
  $f64x r = d.0 + d.1;

  r = vmulsign_vd_vd_vd(r, x);
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(x) | x.eq($f64x::splat(0.)), $f64x::splat(M_PI/2.) - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd($f64x::splat(M_PI/2.), x)), r);
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(y), $f64x::splat(M_PI/2.) - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd($f64x::splat(M_PI/4.), x)), r);
  r = vsel_vd_vo_vd_vd(y.eq($f64x::splat(0.)), vreinterpret_vd_vm(vand_vm_vo64_vm(vsignbit_vo_vd(x), vreinterpret_vm_vd($f64x::splat(M_PI)))), r);

  r = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x) | visnan_vo_vd(y), vreinterpret_vm_vd(vmulsign_vd_vd_vd(r, y))));
  return r;
}

pub fn xasin(d: $f64x) -> $f64x {
  $mx o = vabs_vd_vd(d).lt($f64x::splat(0.5));
  $f64x x2 = vsel_vd_vo_vd_vd(o, d*d, ($f64x::splat(1.) - vabs_vd_vd(d))*$f64x::splat(0.5));
  $f64x x = vsel_vd_vo_vd_vd(o, vabs_vd_vd(d), vsqrt_vd_vd(x2)), u;

#ifdef SPLIT_KERNEL
  $f64x x4 = x2*x2
  v;

  u = $f64x::splat(-0.1581918243329996643e-1)
      .mla(x4, $f64x::splat(+0.6606077476277170610e-2))
      .mla(x4, $f64x::splat(+0.1388715184501609218e-1))
      .mla(x4, $f64x::splat(+0.2237176181932048341e-1))
      .mla(x4, $f64x::splat(+0.4464285681377102438e-1))
      .mla(x4, $f64x::splat(+0.1666666666666497543e+0));
  
  v = $f64x::splat(+0.3161587650653934628e-1)
      .mla(x4, $f64x::splat(+0.1929045477267910674e-1))
      .mla(x4, $f64x::splat(+0.1215360525577377331e-1))
      .mla(x4, $f64x::splat(+0.1735956991223614604e-1))
      .mla(x4, $f64x::splat(+0.3038195928038132237e-1))
      .mla(x4, $f64x::splat(+0.7500000000378581611e-1));

  u = v.mla(x2, u);
#else
  u = $f64x::splat(+0.3161587650653934628e-1)
      .mla(x2, $f64x::splat(-0.1581918243329996643e-1))
      .mla(x2, $f64x::splat(+0.1929045477267910674e-1))
      .mla(x2, $f64x::splat(+0.6606077476277170610e-2))
      .mla(x2, $f64x::splat(+0.1215360525577377331e-1))
      .mla(x2, $f64x::splat(+0.1388715184501609218e-1))
      .mla(x2, $f64x::splat(+0.1735956991223614604e-1))
      .mla(x2, $f64x::splat(+0.2237176181932048341e-1))
      .mla(x2, $f64x::splat(+0.3038195928038132237e-1))
      .mla(x2, $f64x::splat(+0.4464285681377102438e-1))
      .mla(x2, $f64x::splat(+0.7500000000378581611e-1))
      .mla(x2, $f64x::splat(+0.1666666666666497543e+0));
#endif

  u = u.mla(x*x2, x);
  
  $f64x r = vsel_vd_vo_vd_vd(o, u, u.mla($f64x::splat(-2.), $f64x::splat(M_PI/2.)));
  return vmulsign_vd_vd_vd(r, d);
}

pub fn xasin_u1(d: $f64x) -> $f64x {
  $mx o = vabs_vd_vd(d).lt($f64x::splat(0.5));
  $f64x x2 = vsel_vd_vo_vd_vd(o, d*d, ($f64x::splat(1.)-vabs_vd_vd(d))*$f64x::splat(0.5));
  u;
  D2 x = vsel_vd2_vo_vd2_vd2(o, D2::new(vabs_vd_vd(d), $f64x::splat(0.)), x2.sqrt_as_d2());
  x = vsel_vd2_vo_vd2_vd2(vabs_vd_vd(d).eq($f64x::splat(1.)), D2::from((0., 0.)), x);

#ifdef SPLIT_KERNEL
  $f64x x4 = x2*x2;
  v;

  u = $f64x::splat(-0.1581918243329996643e-1)
      .mla(x4, $f64x::splat(+0.6606077476277170610e-2))
      .mla(x4, $f64x::splat(+0.1388715184501609218e-1))
      .mla(x4, $f64x::splat(+0.2237176181932048341e-1))
      .mla(x4, $f64x::splat(+0.4464285681377102438e-1))
      .mla(x4, $f64x::splat(+0.1666666666666497543e+0));
  
  v = $f64x::splat(+0.3161587650653934628e-1)
      .mla(x4, $f64x::splat(+0.1929045477267910674e-1))
      .mla(x4, $f64x::splat(+0.1215360525577377331e-1))
      .mla(x4, $f64x::splat(+0.1735956991223614604e-1))
      .mla(x4, $f64x::splat(+0.3038195928038132237e-1))
      .mla(x4, $f64x::splat(+0.7500000000378581611e-1));

  u = v.mla(x2, u);
#else
  u = $f64x::splat(+0.3161587650653934628e-1)
      .mla(x2, $f64x::splat(-0.1581918243329996643e-1))
      .mla(x2, $f64x::splat(+0.1929045477267910674e-1))
      .mla(x2, $f64x::splat(+0.6606077476277170610e-2))
      .mla(x2, $f64x::splat(+0.1215360525577377331e-1))
      .mla(x2, $f64x::splat(+0.1388715184501609218e-1))
      .mla(x2, $f64x::splat(+0.1735956991223614604e-1))
      .mla(x2, $f64x::splat(+0.2237176181932048341e-1))
      .mla(x2, $f64x::splat(+0.3038195928038132237e-1))
      .mla(x2, $f64x::splat(+0.4464285681377102438e-1))
      .mla(x2, $f64x::splat(+0.7500000000378581611e-1))
      .mla(x2, $f64x::splat(+0.1666666666666497543e+0));
#endif

  u *= (x2*x.0);

  D2 y = D2::from((3.141592653589793116/4., 1.2246467991473532072e-16/4.)).sub_checked(x).sub_checked(u);
  
  $f64x r = vsel_vd_vo_vd_vd(o, u + x.0,
			       (y.0 + y.1)*$f64x::splat(2.));
  return vmulsign_vd_vd_vd(r, d);
}

pub fn xacos(d: $f64x) -> $f64x {
  $mx o = vabs_vd_vd(d).lt($f64x::splat(0.5));
  $f64x x2 = vsel_vd_vo_vd_vd(o, d*d,
				($f64x::splat(1.) - vabs_vd_vd(d))*$f64x::splat(0.5)), u;
  $f64x x = vsel_vd_vo_vd_vd(o, vabs_vd_vd(d), vsqrt_vd_vd(x2));
  x = vsel_vd_vo_vd_vd(vabs_vd_vd(d).eq($f64x::splat(1.)), $f64x::splat(0.), x);

#ifdef SPLIT_KERNEL
  $f64x x4 = x2*x2;
  v;

  u = $f64x::splat(-0.1581918243329996643e-1)
      .mla(x4, $f64x::splat(+0.6606077476277170610e-2))
      .mla(x4, $f64x::splat(+0.1388715184501609218e-1))
      .mla(x4, $f64x::splat(+0.2237176181932048341e-1))
      .mla(x4, $f64x::splat(+0.4464285681377102438e-1))
      .mla(x4, $f64x::splat(+0.1666666666666497543e+0));
  
  v = $f64x::splat(+0.3161587650653934628e-1)
      .mla(x4, $f64x::splat(+0.1929045477267910674e-1))
      .mla(x4, $f64x::splat(+0.1215360525577377331e-1))
      .mla(x4, $f64x::splat(+0.1735956991223614604e-1))
      .mla(x4, $f64x::splat(+0.3038195928038132237e-1))
      .mla(x4, $f64x::splat(+0.7500000000378581611e-1));

  u = v.mla(x2, u);
#else
  u = $f64x::splat(+0.3161587650653934628e-1)
      .mla(x2, $f64x::splat(-0.1581918243329996643e-1))
      .mla(x2, $f64x::splat(+0.1929045477267910674e-1))
      .mla(x2, $f64x::splat(+0.6606077476277170610e-2))
      .mla(x2, $f64x::splat(+0.1215360525577377331e-1))
      .mla(x2, $f64x::splat(+0.1388715184501609218e-1))
      .mla(x2, $f64x::splat(+0.1735956991223614604e-1))
      .mla(x2, $f64x::splat(+0.2237176181932048341e-1))
      .mla(x2, $f64x::splat(+0.3038195928038132237e-1))
      .mla(x2, $f64x::splat(+0.4464285681377102438e-1))
      .mla(x2, $f64x::splat(+0.7500000000378581611e-1))
      .mla(x2, $f64x::splat(+0.1666666666666497543e+0));
#endif

  u *= x2*x;

  $f64x y = $f64x::splat(M_PI/2.) - (vmulsign_vd_vd_vd(x, d) + vmulsign_vd_vd_vd(u, d));
  x = x + u;
  $f64x r = vsel_vd_vo_vd_vd(o, y, x*$f64x::splat(2.));
  return vsel_vd_vo_vd_vd(vandnot_vo_vo_vo(o, d.lt($f64x::splat(0.))),
			  D2::from((3.141592653589793116, 1.2246467991473532072e-16)).add_checked(-r).0, r);
}

pub fn xacos_u1(d: $f64x) -> $f64x {
  $mx o = vabs_vd_vd(d).lt($f64x::splat(0.5));
  $f64x x2 = vsel_vd_vo_vd_vd(o, d*d, ($f64x::splat(1.) - vabs_vd_vd(d))*$f64x::splat(0.5)), u;
  D2 x = vsel_vd2_vo_vd2_vd2(o, D2::new(vabs_vd_vd(d), $f64x::splat(0.)), x2.sqrt_as_d2());
  x = vsel_vd2_vo_vd2_vd2(vabs_vd_vd(d).eq($f64x::splat(1.)), D2::from((0., 0.)), x);

#ifdef SPLIT_KERNEL
  $f64x x4 = x2*x2;
  v;

  u = $f64x::splat(-0.1581918243329996643e-1)
      .mla(x4, $f64x::splat(+0.6606077476277170610e-2))
      .mla(x4, $f64x::splat(+0.1388715184501609218e-1))
      .mla(x4, $f64x::splat(+0.2237176181932048341e-1))
      .mla(x4, $f64x::splat(+0.4464285681377102438e-1))
      .mla(x4, $f64x::splat(+0.1666666666666497543e+0));
  
  v = $f64x::splat(+0.3161587650653934628e-1)
      .mla(x4, $f64x::splat(+0.1929045477267910674e-1))
      .mla(x4, $f64x::splat(+0.1215360525577377331e-1))
      .mla(x4, $f64x::splat(+0.1735956991223614604e-1))
      .mla(x4, $f64x::splat(+0.3038195928038132237e-1))
      .mla(x4, $f64x::splat(+0.7500000000378581611e-1));

  u = v.mla(x2, u);
#else
  u = $f64x::splat(+0.3161587650653934628e-1)
      .mla(x2, $f64x::splat(-0.1581918243329996643e-1))
      .mla(x2, $f64x::splat(+0.1929045477267910674e-1))
      .mla(x2, $f64x::splat(+0.6606077476277170610e-2))
      .mla(x2, $f64x::splat(+0.1215360525577377331e-1))
      .mla(x2, $f64x::splat(+0.1388715184501609218e-1))
      .mla(x2, $f64x::splat(+0.1735956991223614604e-1))
      .mla(x2, $f64x::splat(+0.2237176181932048341e-1))
      .mla(x2, $f64x::splat(+0.3038195928038132237e-1))
      .mla(x2, $f64x::splat(+0.4464285681377102438e-1))
      .mla(x2, $f64x::splat(+0.7500000000378581611e-1))
      .mla(x2, $f64x::splat(+0.1666666666666497543e+0));
#endif

  u *= (x2*x.0);

  D2 y = D2::from((3.141592653589793116/2., 1.2246467991473532072e-16/2.))
        .sub_checked(vmulsign_vd_vd_vd(x.0, d).add_checked_as_d2(vmulsign_vd_vd_vd(u, d)));
  x = x.add_checked(u);
  
  y = vsel_vd2_vo_vd2_vd2(o, y, x.scale($f64x::splat(2.)));
  
  y = vsel_vd2_vo_vd2_vd2(vandnot_vo_vo_vo(o, d.lt($f64x::splat(0.))),
			  D2::from((3.141592653589793116, 1.2246467991473532072e-16)).sub_checked(y), y);

  return y.0 + y.1;
}

pub fn xatan_u1(d: $f64x) -> $f64x {
  D2 d2 = atan2k_u1(D2::new(vabs_vd_vd(d), $f64x::splat(0.)), D2::from((1., 0.)));
  $f64x r = d2.0 + d2.1;
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(d), $f64x::splat(1.570796326794896557998982), r);
  return vmulsign_vd_vd_vd(r, d);
}

pub fn xatan(s: $f64x) -> $f64x {
  $f64x t, u;
  $ix q;
#if defined(__INTEL_COMPILER) && defined(ENABLE_PURECFMA_SCALAR)
  $f64x w = s;
#endif

  q = vsel_vi_vd_vi(s, $ix::splat(2));
  s = vabs_vd_vd(s);

  q = vsel_vi_vd_vd_vi_vi($f64x::splat(1.), s, q + $ix::splat(1), q);
  s = vsel_vd_vo_vd_vd($f64x::splat(1.).lt(s), vrec_vd_vd(s), s);

  t = s*s;

#ifdef SPLIT_KERNEL
  $f64x t2 = t*t;
  v;

  u = $f64x::splat(-1.88796008463073496563746e-05)
      .mla(t2, $f64x::splat(-0.00110611831486672482563471))
      .mla(t2, $f64x::splat(-0.00889896195887655491740809))
      .mla(t2, $f64x::splat(-0.0254517624932312641616861))
      .mla(t2, $f64x::splat(-0.0407629191276836500001934))
      .mla(t2, $f64x::splat(-0.0523674852303482457616113))
      .mla(t2, $f64x::splat(-0.0666573579361080525984562))
      .mla(t2, $f64x::splat(-0.090908995008245008229153))
      .mla(t2, $f64x::splat(-0.14285714266771329383765))
      .mla(t2, $f64x::splat(-0.333333333333311110369124));

  v = $f64x::splat(0.000209850076645816976906797)
      .mla(t2, $f64x::splat(0.00370026744188713119232403))
      .mla(t2, $f64x::splat(0.016599329773529201970117))
      .mla(t2, $f64x::splat(0.0337852580001353069993897))
      .mla(t2, $f64x::splat(0.0466667150077840625632675))
      .mla(t2, $f64x::splat(0.0587666392926673580854313))
      .mla(t2, $f64x::splat(0.0769219538311769618355029))
      .mla(t2, $f64x::splat(0.111111105648261418443745))
      .mla(t2, $f64x::splat(0.199999999996591265594148));

  u = v.mla(t, u);
#else
  u = $f64x::splat(-1.88796008463073496563746e-05)
      .mla(t, $f64x::splat(0.000209850076645816976906797))
      .mla(t, $f64x::splat(-0.00110611831486672482563471))
      .mla(t, $f64x::splat(0.00370026744188713119232403))
      .mla(t, $f64x::splat(-0.00889896195887655491740809))
      .mla(t, $f64x::splat(0.016599329773529201970117))
      .mla(t, $f64x::splat(-0.0254517624932312641616861))
      .mla(t, $f64x::splat(0.0337852580001353069993897))
      .mla(t, $f64x::splat(-0.0407629191276836500001934))
      .mla(t, $f64x::splat(0.0466667150077840625632675))
      .mla(t, $f64x::splat(-0.0523674852303482457616113))
      .mla(t, $f64x::splat(0.0587666392926673580854313))
      .mla(t, $f64x::splat(-0.0666573579361080525984562))
      .mla(t, $f64x::splat(0.0769219538311769618355029))
      .mla(t, $f64x::splat(-0.090908995008245008229153))
      .mla(t, $f64x::splat(0.111111105648261418443745))
      .mla(t, $f64x::splat(-0.14285714266771329383765))
      .mla(t, $f64x::splat(0.199999999996591265594148))
      .mla(t, $f64x::splat(-0.333333333333311110369124));
#endif
  
  t = s.mla(t*u, s);

  t = vsel_vd_vo_vd_vd(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(1)), $ix::splat(1))), $f64x::splat(M_PI/2.) - t, t);
  t = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, $ix::splat(2)), $ix::splat(2))), vreinterpret_vm_vd($f64x::splat(-0.))), vreinterpret_vm_vd(t)));

#if defined(__INTEL_COMPILER) && defined(ENABLE_PURECFMA_SCALAR)
  t = vsel_vd_vo_vd_vd(w.eq($f64x::splat(0.)), w, t);
#endif

  return t;
}

pub fn xlog(d: $f64x) -> $f64x {
  $f64x x, x2;
  $f64x t, m;
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mx o = d.lt($f64x::splat(f64::MIN));
  d = vsel_vd_vo_vd_vd(o, d*$f64x::splat(D1_32 * D1_32), d);
  $ix e = vilogb2k_vi_vd(d*$f64x::splat(1./0.75));
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - $ix::splat(64), e);
#else
  $f64x e = vgetexp_vd_vd(d*$f64x::splat(1./0.75));
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), $f64x::splat(1024.), e);
  m = vgetmant_vd_vd(d);
#endif
  
  x = ($f64x::splat(-1.) + m) / ($f64x::splat(1.) + m);
  x2 = x*x;

  t = $f64x::splat(0.153487338491425068243146)
      .mla(x2, $f64x::splat(0.152519917006351951593857))
      .mla(x2, $f64x::splat(0.181863266251982985677316))
      .mla(x2, $f64x::splat(0.222221366518767365905163))
      .mla(x2, $f64x::splat(0.285714294746548025383248))
      .mla(x2, $f64x::splat(0.399999999950799600689777))
      .mla(x2, $f64x::splat(0.6666666666667778740063))
      .mla(x2, $f64x::splat(2.));

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  x = x.mla(t, $f64x::splat(0.693147180559945286226764)*$f64x::from(e));

  x = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), $f64x::splat(SLEEF_INFINITY), x);
  x = vsel_vd_vo_vd_vd(d.lt($f64x::splat(0.)) | visnan_vo_vd(d), $f64x::splat(SLEEF_NAN), x);
  x = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), $f64x::splat(-SLEEF_INFINITY), x);
#else
  x = x.mla(t, $f64x::splat(0.693147180559945286226764)*e);
  x = vfixup_vd_vd_vd_vi2_i(x, d, $ix2::splat((5 << (5*4))), 0);
#endif

  return x;
}

pub fn xexp(d: $f64x) -> $f64x {
  $f64x u = vrint_vd_vd(d*$f64x::splat(R_LN2));
  s;
  $ix q = vrint_vi_vd(u);

  s = u.mla($f64x::splat(-L2U), d);
  s = u.mla($f64x::splat(-L2L), s);

#ifdef ENABLE_FMA_DP
#ifdef SPLIT_KERNEL
  $f64x s2 = s*s;
  v;

  u = $f64x::splat(+0.2081276378237164457e-8);
  u = vfma_vd_vd_vd_vd(u, s2, $f64x::splat(+0.2755762628169491192e-6));
  u = vfma_vd_vd_vd_vd(u, s2, $f64x::splat(+0.2480158687479686264e-4));
  u = vfma_vd_vd_vd_vd(u, s2, $f64x::splat(+0.1388888888914497797e-2));
  u = vfma_vd_vd_vd_vd(u, s2, $f64x::splat(+0.4166666666666602598e-1));
  u = vfma_vd_vd_vd_vd(u, s2, $f64x::splat(+0.5000000000000000000e+0));

  v = $f64x::splat(+0.2511210703042288022e-7);
  v = vfma_vd_vd_vd_vd(v, s2, $f64x::splat(+0.2755723402025388239e-5));
  v = vfma_vd_vd_vd_vd(v, s2, $f64x::splat(+0.1984126989855865850e-3));
  v = vfma_vd_vd_vd_vd(v, s2, $f64x::splat(+0.8333333333314938210e-2));
  v = vfma_vd_vd_vd_vd(v, s2, $f64x::splat(+0.1666666666666669072e+0));

  u = v.mla(s, u);
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.1000000000000000000e+1));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.1000000000000000000e+1));
#else // #ifdef SPLIT_KERNEL
  u = $f64x::splat(+0.2081276378237164457e-8);
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.2511210703042288022e-7));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.2755762628169491192e-6));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.2755723402025388239e-5));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.2480158687479686264e-4));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.1984126989855865850e-3));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.1388888888914497797e-2));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.8333333333314938210e-2));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.4166666666666602598e-1));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.1666666666666669072e+0));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.5000000000000000000e+0));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.1000000000000000000e+1));
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(+0.1000000000000000000e+1));
#endif // #ifdef SPLIT_KERNEL
#else // #ifdef ENABLE_FMA_DP
  u = $f64x::splat(2.08860621107283687536341e-09)
      .mla(s, $f64x::splat(2.51112930892876518610661e-08))
      .mla(s, $f64x::splat(2.75573911234900471893338e-07))
      .mla(s, $f64x::splat(2.75572362911928827629423e-06))
      .mla(s, $f64x::splat(2.4801587159235472998791e-05))
      .mla(s, $f64x::splat(0.000198412698960509205564975))
      .mla(s, $f64x::splat(0.00138888888889774492207962))
      .mla(s, $f64x::splat(0.00833333333331652721664984))
      .mla(s, $f64x::splat(0.0416666666666665047591422))
      .mla(s, $f64x::splat(0.166666666666666851703837))
      .mla(s, $f64x::splat(0.5));

  u = $f64x::splat(1.) + (s*s).mla(u, s);
#endif // #ifdef ENABLE_FMA_DP
  
  u = vldexp2_vd_vd_vi(u, q);

  u = vsel_vd_vo_vd_vd(d.gt($f64x::splat(709.78271114955742909217217426)), $f64x::splat(SLEEF_INFINITY), u);
  u = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.lt($f64x::splat(-1000.)), vreinterpret_vm_vd(u)));

  return u;
}
#[inline]
fn expm1k(d: $f64x) -> $f64x {
  $f64x u = vrint_vd_vd(d*$f64x::splat(R_LN2)), s;
  $ix q = vrint_vi_vd(u);

  s = u.mla($f64x::splat(-L2U), d);
  s = u.mla($f64x::splat(-L2L), s);

  u = $f64x::splat(2.08860621107283687536341e-09)
      .mla(s, $f64x::splat(2.51112930892876518610661e-08))
      .mla(s, $f64x::splat(2.75573911234900471893338e-07))
      .mla(s, $f64x::splat(2.75572362911928827629423e-06))
      .mla(s, $f64x::splat(2.4801587159235472998791e-05))
      .mla(s, $f64x::splat(0.000198412698960509205564975))
      .mla(s, $f64x::splat(0.00138888888889774492207962))
      .mla(s, $f64x::splat(0.00833333333331652721664984))
      .mla(s, $f64x::splat(0.0416666666666665047591422))
      .mla(s, $f64x::splat(0.166666666666666851703837))
      .mla(s, $f64x::splat(0.5));
  u = (s*s).mla(u, s);
  
  u = vsel_vd_vo_vd_vd(vcast_vo64_vo32(veq_vo_vi_vi(q, $ix::splat(0))), u,
		       vldexp2_vd_vd_vi(u + $f64x::splat(1.), q) - $f64x::splat(1.));

  return u;
}
#[inline]
fn logk(d: $f64x) -> D2 {
  D2 x, x2, s;
  $f64x t, m;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mx o = d.lt($f64x::splat(f64::MIN));
  d = vsel_vd_vo_vd_vd(o, d*$f64x::splat(D1_32 * D1_32), d);
  $ix e = vilogb2k_vi_vd(d*$f64x::splat(1./0.75));
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - $ix::splat(64), e);
#else
  $f64x e = vgetexp_vd_vd(d*$f64x::splat(1./0.75));
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), $f64x::splat(1024.), e);
  m = vgetmant_vd_vd(d);
#endif

  x = $f64x::splat(-1.).add_as_d2(m) / $f64x::splat(1.).add_as_d2(m);
  x2 = x.square();

  t = $f64x::splat(0.116255524079935043668677)
      .mla(x2.0, $f64x::splat(0.103239680901072952701192))
      .mla(x2.0, $f64x::splat(0.117754809412463995466069))
      .mla(x2.0, $f64x::splat(0.13332981086846273921509))
      .mla(x2.0, $f64x::splat(0.153846227114512262845736))
      .mla(x2.0, $f64x::splat(0.181818180850050775676507))
      .mla(x2.0, $f64x::splat(0.222222222230083560345903))
      .mla(x2.0, $f64x::splat(0.285714285714249172087875))
      .mla(x2.0, $f64x::splat(0.400000000000000077715612));
  D2 c = D2::from((0.666666666666666629659233, 3.80554962542412056336616e-17));

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  s = D2::from((0.693147180559945286226764, 2.319046813846299558417771e-17)) * $f64x::from(e);
#else
  s = D2::new($f64x::splat(0.693147180559945286226764), $f64x::splat(2.319046813846299558417771e-17)) * e;
#endif

  s = s.add_checked(x.scale($f64x::splat(2.)));
  s = s.add_checked(x2 * x * (x2 * t + c));
  return s;
}

pub fn xlog_u1(d: $f64x) -> $f64x {
  D2 x;
  $f64x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mx o = d.lt($f64x::splat(f64::MIN));
  d = vsel_vd_vo_vd_vd(o, d*$f64x::splat(D1_32 * D1_32), d);
  $ix e = vilogb2k_vi_vd(d*$f64x::splat(1./0.75));
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - $ix::splat(64), e);
#else
  $f64x e = vgetexp_vd_vd(d*$f64x::splat(1./0.75));
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), $f64x::splat(1024.), e);
  m = vgetmant_vd_vd(d);
#endif

  x = $f64x::splat(-1.).add_as_d2(m) / $f64x::splat(1.).add_as_d2(m);
  x2 = x.0*x.0;

  t = $f64x::splat(0.1532076988502701353e+0)
      .mla(x2, $f64x::splat(0.1525629051003428716e+0))
      .mla(x2, $f64x::splat(0.1818605932937785996e+0))
      .mla(x2, $f64x::splat(0.2222214519839380009e+0))
      .mla(x2, $f64x::splat(0.2857142932794299317e+0))
      .mla(x2, $f64x::splat(0.3999999999635251990e+0))
      .mla(x2, $f64x::splat(0.6666666666667333541e+0));
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  D2 s = D2::from((0.693147180559945286226764, 2.319046813846299558417771e-17)) * $f64x::from(e);
#else
  D2 s = D2::from((0.693147180559945286226764, 2.319046813846299558417771e-17)) * e;
#endif

  s = s.add_checked(x.scale($f64x::splat(2.)));
  s = s.add_checked(x2*x.0*t);

  $f64x r = s.0 + s.1;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), $f64x::splat(SLEEF_INFINITY), r);
  r = vsel_vd_vo_vd_vd(d.lt($f64x::splat(0.)) | visnan_vo_vd(d), $f64x::splat(SLEEF_NAN), r);
  r = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), $f64x::splat(-SLEEF_INFINITY), r);
#else
  r = vfixup_vd_vd_vd_vi2_i(r, d, $ix2::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}
#[inline]
fn expk(D2 d) -> $f64x {
  $f64x u = (d.0 + d.1)*$f64x::splat(R_LN2);
  $f64x dq = vrint_vd_vd(u);
  $ix q = vrint_vi_vd(dq);
  D2 s, t;

  s = d + dq*$f64x::splat(-L2U);
  s += dq*$f64x::splat(-L2L);

  s = s.normalize();

  u = $f64x::splat(2.51069683420950419527139e-08)
      .mla(s.0, $f64x::splat(2.76286166770270649116855e-07))
      .mla(s.0, $f64x::splat(2.75572496725023574143864e-06))
      .mla(s.0, $f64x::splat(2.48014973989819794114153e-05))
      .mla(s.0, $f64x::splat(0.000198412698809069797676111))
      .mla(s.0, $f64x::splat(0.0013888888939977128960529))
      .mla(s.0, $f64x::splat(0.00833333333332371417601081))
      .mla(s.0, $f64x::splat(0.0416666666665409524128449))
      .mla(s.0, $f64x::splat(0.166666666666666740681535))
      .mla(s.0, $f64x::splat(0.500000000000000999200722));
  
  t = s.add_checked(s.square() * u);

  t = $f64x::splat(1.).add_checked(t);
  u = t.0 + t.1;
  u = vldexp2_vd_vd_vi(u, q);

  u = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.0.lt($f64x::splat(-1000.)), vreinterpret_vm_vd(u)));
  
  return u;
}

pub fn xpow(x: $f64x, y: $f64x) -> $f64x {
#if 1
  $mx yisint = visint_vo_vd(y);
  $mx yisodd = visodd_vo_vd(y) & yisint;

  D2 d = logk(vabs_vd_vd(x)) * y;
  $f64x result = expk(d);
  result = vsel_vd_vo_vd_vd(d.0.gt($f64x::splat(709.78271114955742909217217426)), $f64x::splat(SLEEF_INFINITY), result);

  result *= vsel_vd_vo_vd_vd(x.gt($f64x::splat(0.)),
					  $f64x::splat(1.),
					  vsel_vd_vo_vd_vd(yisint, vsel_vd_vo_vd_vd(yisodd, $f64x::splat(-1.), $f64x::splat(1.)), $f64x::splat(SLEEF_NAN)));

  $f64x efx = vmulsign_vd_vd_vd(vabs_vd_vd(x) - $f64x::splat(1.), y);

  result = vsel_vd_vo_vd_vd(visinf_vo_vd(y),
			    vreinterpret_vd_vm(vandnot_vm_vo64_vm(efx.lt($f64x::splat(0.)),
								  vreinterpret_vm_vd(vsel_vd_vo_vd_vd(efx.eq($f64x::splat(0.)),
												      $f64x::splat(1.),
												      $f64x::splat(SLEEF_INFINITY))))),
			    result);

  result = vsel_vd_vo_vd_vd(visinf_vo_vd(x) | x.eq($f64x::splat(0.)),
			    vsel_vd_vo_vd_vd(yisodd, vsign_vd_vd(x), $f64x::splat(1.)) *
					  vreinterpret_vd_vm(vandnot_vm_vo64_vm(vsel_vd_vo_vd_vd(x.eq($f64x::splat(0.)), -y, y).lt($f64x::splat(0.)),
										vreinterpret_vm_vd($f64x::splat(SLEEF_INFINITY)))),
			    result);

  result = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x) | visnan_vo_vd(y), vreinterpret_vm_vd(result)));

  result = vsel_vd_vo_vd_vd(y.eq($f64x::splat(0.)) | x.eq($f64x::splat(1.)), $f64x::splat(1.), result);

  return result;
#else
  return expk(logk(x) * y);
#endif
}
#[inline]
fn expk2(D2 d) -> D2 {
  $f64x u = (d.0 + d.1)*$f64x::splat(R_LN2);
  $f64x dq = vrint_vd_vd(u);
  $ix q = vrint_vi_vd(dq);
  D2 s, t;

  s = d + dq*$f64x::splat(-L2U);
  s += dq*$f64x::splat(-L2L);

  u = $f64x::splat(+0.1602472219709932072e-9)
      .mla(s.0, $f64x::splat(+0.2092255183563157007e-8))
      .mla(s.0, $f64x::splat(+0.2505230023782644465e-7))
      .mla(s.0, $f64x::splat(+0.2755724800902135303e-6))
      .mla(s.0, $f64x::splat(+0.2755731892386044373e-5))
      .mla(s.0, $f64x::splat(+0.2480158735605815065e-4))
      .mla(s.0, $f64x::splat(+0.1984126984148071858e-3))
      .mla(s.0, $f64x::splat(+0.1388888888886763255e-2))
      .mla(s.0, $f64x::splat(+0.8333333333333347095e-2))
      .mla(s.0, $f64x::splat(+0.4166666666666669905e-1));

  t = s * u + $f64x::splat(+0.1666666666666666574e+0);
  t = s * t + $f64x::splat(0.5);
  t = s + s.square() * t;

  t = $f64x::splat(1.).add_checked(t);

  t.0 = vldexp2_vd_vd_vi(t.0, q);
  t.1 = vldexp2_vd_vd_vi(t.1, q);

  t.0 = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.0.lt($f64x::splat(-1000.)), vreinterpret_vm_vd(t.0)));
  t.1 = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.0.lt($f64x::splat(-1000.)), vreinterpret_vm_vd(t.1)));

  return t;
}

pub fn xsinh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  D2 d = expk2(D2::new(y, $f64x::splat(0.)));
  d = d.sub_checked(d.rec());
  y = (d.0 + d.1)*$f64x::splat(0.5);

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(710.)) | visnan_vo_vd(y), $f64x::splat(SLEEF_INFINITY), y);
  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xcosh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  D2 d = expk2(D2::new(y, $f64x::splat(0.)));
  d = d.add_checked(d.rec());
  y = (d.0 + d.1)*$f64x::splat(0.5);

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(710.)) | visnan_vo_vd(y), $f64x::splat(SLEEF_INFINITY), y);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xtanh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  D2 d = expk2(D2::new(y, $f64x::splat(0.)));
  D2 e = d.rec();
  d = (d + (-e)) / (d + e);
  y = d.0 + d.1;

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(18.714973875)) | visnan_vo_vd(y), $f64x::splat(1.), y);
  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xsinh_u35(x: $f64x) -> $f64x {
  $f64x e = expm1k(vabs_vd_vd(x));

  $f64x y = (e + $f64x::splat(2.)) / (e + $f64x::splat(1.));
  y = y*($f64x::splat(0.5)*e);

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(709.)) | visnan_vo_vd(y), $f64x::splat(SLEEF_INFINITY), y);
  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xcosh_u35(x: $f64x) -> $f64x {
  $f64x e = xexp(vabs_vd_vd(x));
  $f64x y = $f64x::splat(0.5).mla(e, $f64x::splat(0.5) / e);

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(709.)) | visnan_vo_vd(y), $f64x::splat(SLEEF_INFINITY), y);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xtanh_u35(x: $f64x) -> $f64x {
  $f64x d = expm1k($f64x::splat(2.)*vabs_vd_vd(x));
  $f64x y = d / ($f64x::splat(2.) + d);

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(18.714973875)) | visnan_vo_vd(y), $f64x::splat(1.), y);
  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}
#[inline]
fn logk2(D2 d) -> D2 {
  D2 x, x2, m, s;
  $f64x t;
  $ix e;
  
  e = vilogbk_vi_vd(d.0*$f64x::splat(1./0.75));

  m.0 = vldexp2_vd_vd_vi(d.0, vneg_vi_vi(e));
  m.1 = vldexp2_vd_vd_vi(d.1, vneg_vi_vi(e));

  x = (m + $f64x::splat(-1.)) / (m + $f64x::splat(1.));
  x2 = x.square();

  t = $f64x::splat(0.13860436390467167910856)
      .mla(x2.0, $f64x::splat(0.131699838841615374240845))
      .mla(x2.0, $f64x::splat(0.153914168346271945653214))
      .mla(x2.0, $f64x::splat(0.181816523941564611721589))
      .mla(x2.0, $f64x::splat(0.22222224632662035403996))
      .mla(x2.0, $f64x::splat(0.285714285511134091777308))
      .mla(x2.0, $f64x::splat(0.400000000000914013309483))
      .mla(x2.0, $f64x::splat(0.666666666666664853302393));

  s = D2::from((0.693147180559945286226764, 2.319046813846299558417771e-17)) * $f64x::from(e);
  s = s.add_checked(x.scale($f64x::splat(2.)));
  s = s.add_checked(x2 * x * t);

  return  s;
}

pub fn xasinh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  $mx o = y.gt($f64x::splat(1.));
  D2 d;
  
  d = vsel_vd2_vo_vd2_vd2(o, x.rec_as_d2(), D2::new(y, $f64x::splat(0.)));
  d = (d.square() + $f64x::splat(1.)).sqrt();
  d = vsel_vd2_vo_vd2_vd2(o, d * y, d);

  d = logk2((d + x).normalize());
  y = d.0 + d.1;
  
  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(SQRT_DBL_MAX)) | visnan_vo_vd(y),
		       vmulsign_vd_vd_vd($f64x::splat(SLEEF_INFINITY), x), y);

  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));
  y = vsel_vd_vo_vd_vd(visnegzero_vo_vd(x), $f64x::splat(-0.), y);
  
  return y;
}

pub fn xacosh(x: $f64x) -> $f64x {
  D2 d = logk2(x.add_as_d2($f64x::splat(1.)).sqrt() * x.add_as_d2($f64x::splat(-1.)).sqrt() + x);
  $f64x y = d.0 + d.1;

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt($f64x::splat(SQRT_DBL_MAX)) | visnan_vo_vd(y),
		       $f64x::splat(SLEEF_INFINITY), y);
  y = vreinterpret_vd_vm(vandnot_vm_vo64_vm(x.eq($f64x::splat(1.)), vreinterpret_vm_vd(y)));

  y = vreinterpret_vd_vm(vor_vm_vo64_vm(x.lt($f64x::splat(1.)), vreinterpret_vm_vd(y)));
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));
  
  return y;
}

pub fn xatanh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  D2 d = logk2($f64x::splat(1.).add_as_d2(y) / $f64x::splat(1.).add_as_d2(-y));
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(y.gt($f64x::splat(1.)), vreinterpret_vm_vd(vsel_vd_vo_vd_vd(y.eq($f64x::splat(1.)), $f64x::splat(SLEEF_INFINITY), (d.0 + d.1)*$f64x::splat(0.5)))));

  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(x) | visnan_vo_vd(y), vreinterpret_vm_vd(y)));
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xcbrt(d: $f64x) -> $f64x {
  $f64x x, y, q = $f64x::splat(1.);
  $ix e, qu, re;
  $f64x t;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  $f64x s = d;
#endif
  e = vilogbk_vi_vd(vabs_vd_vd(d)) + $ix::splat(1);
  d = vldexp2_vd_vd_vi(d, vneg_vi_vi(e));

  t = $f64x::from(e) + $f64x::splat(6144.);
  qu = vtruncate_vi_vd(t*$f64x::splat(1./3.));
  re = vtruncate_vi_vd(t*($f64x::from(qu)*$f64x::splat(3.)));

  q = vsel_vd_vo_vd_vd(vcast_vo64_vo32(veq_vo_vi_vi(re, $ix::splat(1))), $f64x::splat(1.2599210498948731647672106), q);
  q = vsel_vd_vo_vd_vd(vcast_vo64_vo32(veq_vo_vi_vi(re, $ix::splat(2))), $f64x::splat(1.5874010519681994747517056), q);
  q = vldexp2_vd_vd_vi(q, qu - $ix::splat(2048));

  q = vmulsign_vd_vd_vd(q, d);

  d = vabs_vd_vd(d);

  x = $f64x::splat(-0.640245898480692909870982)
      .mla(d, $f64x::splat(2.96155103020039511818595))
      .mla(d, $f64x::splat(-5.73353060922947843636166))
      .mla(d, $f64x::splat(6.03990368989458747961407))
      .mla(d, $f64x::splat(-3.85841935510444988821632))
      .mla(d, $f64x::splat(2.2307275302496609725722));

  y = x*x;
  y = y*y;
  x -= vmlapn_vd_vd_vd_vd(d, y, x)*$f64x::splat(1. / 3.);
  y = d*x*x;
  y = (y - $f64x::splat(2. / 3.)*y*y.mla(x, $f64x::splat(-1.)))*q;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  y = vsel_vd_vo_vd_vd(visinf_vo_vd(s), vmulsign_vd_vd_vd($f64x::splat(SLEEF_INFINITY), s), y);
  y = vsel_vd_vo_vd_vd(s.eq($f64x::splat(0.)), vmulsign_vd_vd_vd($f64x::splat(0.), s), y);
#endif
  
  return y;
}

pub fn xcbrt_u1(d: $f64x) -> $f64x {
  $f64x x, y, z, t;
  D2 q2 = D2::from((1., 0.)), u, v;
  $ix e, qu, re;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  $f64x s = d;
#endif
  e = vilogbk_vi_vd(vabs_vd_vd(d)) + $ix::splat(1);
  d = vldexp2_vd_vd_vi(d, vneg_vi_vi(e));

  t = $f64x::from(e) + $f64x::splat(6144.);
  qu = vtruncate_vi_vd(t*$f64x::splat(1./3.));
  re = vtruncate_vi_vd(t - $f64x::from(qu)*$f64x::splat(3.));

  q2 = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(veq_vo_vi_vi(re, $ix::splat(1))), D2::from((1.2599210498948731907, -2.5899333753005069177e-17)), q2);
  q2 = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(veq_vo_vi_vi(re, $ix::splat(2))), D2::from((1.5874010519681995834, -1.0869008194197822986e-16)), q2);

  q2.0 = vmulsign_vd_vd_vd(q2.0, d); q2.1 = vmulsign_vd_vd_vd(q2.1, d);
  d = vabs_vd_vd(d);

  x = $f64x::splat(-0.640245898480692909870982)
      .mla(d, $f64x::splat(2.96155103020039511818595))
      .mla(d, $f64x::splat(-5.73353060922947843636166))
      .mla(d, $f64x::splat(6.03990368989458747961407))
      .mla(d, $f64x::splat(-3.85841935510444988821632))
      .mla(d, $f64x::splat(2.2307275302496609725722));

  y = x*x;
  y = y*y;
  x -= vmlapn_vd_vd_vd_vd(d, y, x)*$f64x::splat(1. / 3.);

  z = x;

  u = x.mul_as_d2(x);
  u = u * u;
  u *= d;
  u += -x);
  y = u.0 + u.1;

  y = $f64x::splat(-2. / 3.)*y*z;
  v = z.mul_as_d2(z) + y;
  v *= d;
  v *= q2;
  z = vldexp2_vd_vd_vi(v.0 + v.1, qu - $ix::splat(2048));

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  z = vsel_vd_vo_vd_vd(visinf_vo_vd(d), vmulsign_vd_vd_vd($f64x::splat(SLEEF_INFINITY), q2.0), z);
  z = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), vreinterpret_vd_vm(vsignbit_vm_vd(q2.0)), z);
#else
  z = vsel_vd_vo_vd_vd(visinf_vo_vd(s), vmulsign_vd_vd_vd($f64x::splat(SLEEF_INFINITY), s), z);
  z = vsel_vd_vo_vd_vd(s.eq($f64x::splat(0.)), vmulsign_vd_vd_vd($f64x::splat(0.), s), z);
#endif
  
  return z;
}

pub fn xexp2(d: $f64x) -> $f64x {
  $f64x u = vrint_vd_vd(d), s;
  $ix q = vrint_vi_vd(u);

  s = d - u;

#ifdef SPLIT_KERNEL
  $f64x s2 = s*s;
   v;

  u = $f64x::splat(+0.4434359082926529454e-9)
      .mla(s2, $f64x::splat(+0.1017819260921760451e-6))
      .mla(s2, $f64x::splat(+0.1525273353517584730e-4))
      .mla(s2, $f64x::splat(+0.1333355814670499073e-2))
      .mla(s2, $f64x::splat(+0.5550410866482046596e-1));

  v = $f64x::splat(+0.7073164598085707425e-8)
      .mla(s2, $f64x::splat(+0.1321543872511327615e-5))
      .mla(s2, $f64x::splat(+0.1540353045101147808e-3))
      .mla(s2, $f64x::splat(+0.9618129107597600536e-2))
      .mla(s2, $f64x::splat(+0.2402265069591012214e+0));

  u = u.mla(s, v)
      .mla(s, $f64x::splat(+0.6931471805599452862e+0));
#else
  u = $f64x::splat(+0.4434359082926529454e-9)
      .mla(s, $f64x::splat(+0.7073164598085707425e-8))
      .mla(s, $f64x::splat(+0.1017819260921760451e-6))
      .mla(s, $f64x::splat(+0.1321543872511327615e-5))
      .mla(s, $f64x::splat(+0.1525273353517584730e-4))
      .mla(s, $f64x::splat(+0.1540353045101147808e-3))
      .mla(s, $f64x::splat(+0.1333355814670499073e-2))
      .mla(s, $f64x::splat(+0.9618129107597600536e-2))
      .mla(s, $f64x::splat(+0.5550410866482046596e-1))
      .mla(s, $f64x::splat(+0.2402265069591012214e+0))
      .mla(s, $f64x::splat(+0.6931471805599452862e+0));
#endif
  
#ifdef ENABLE_FMA_DP
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(1.));
#else
  u = $f64x::splat(1.).add_checked(u.mul_as_d2(s)).normalize().0;
#endif
  
  u = vldexp2_vd_vd_vi(u, q);

  u = vsel_vd_vo_vd_vd(d.ge($f64x::splat(1024.)), $f64x::splat(SLEEF_INFINITY), u);
  u = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.lt($f64x::splat(-2000.)), vreinterpret_vm_vd(u)));

  return u;
}

pub fn xexp10(d: $f64x) -> $f64x {
  $f64x u = vrint_vd_vd(d*$f64x::splat(LOG10_2));
  s;
  $ix q = vrint_vi_vd(u);

  s = u.mla($f64x::splat(-L10U), d);
  s = u.mla($f64x::splat(-L10L), s);

#ifdef SPLIT_KERNEL
  $f64x s2 = s*s;
  v;

  u = $f64x::splat(+0.2411463498334267652e-3)
      .mla(s2, $f64x::splat(+0.5013975546789733659e-2))
      .mla(s2, $f64x::splat(+0.6808936399446784138e-1))
      .mla(s2, $f64x::splat(+0.5393829292058536229e+0))
      .mla(s2, $f64x::splat(+0.2034678592293432953e+1));

  v = $f64x::splat(+0.1157488415217187375e-2)
      .mla(s2, $f64x::splat(+0.1959762320720533080e-1))
      .mla(s2, $f64x::splat(+0.2069958494722676234e+0))
      .mla(s2, $f64x::splat(+0.1171255148908541655e+1))
      .mla(s2, $f64x::splat(+0.2650949055239205876e+1));

  u = u.mla(s, v)
      .mla(s, $f64x::splat(+0.2302585092994045901e+1));
#else
  u = $f64x::splat(+0.2411463498334267652e-3)
      .mla(s, $f64x::splat(+0.1157488415217187375e-2))
      .mla(s, $f64x::splat(+0.5013975546789733659e-2))
      .mla(s, $f64x::splat(+0.1959762320720533080e-1))
      .mla(s, $f64x::splat(+0.6808936399446784138e-1))
      .mla(s, $f64x::splat(+0.2069958494722676234e+0))
      .mla(s, $f64x::splat(+0.5393829292058536229e+0))
      .mla(s, $f64x::splat(+0.1171255148908541655e+1))
      .mla(s, $f64x::splat(+0.2034678592293432953e+1))
      .mla(s, $f64x::splat(+0.2650949055239205876e+1))
      .mla(s, $f64x::splat(+0.2302585092994045901e+1));
#endif
  
#ifdef ENABLE_FMA_DP
  u = vfma_vd_vd_vd_vd(u, s, $f64x::splat(1.));
#else
  u = $f64x::splat(1.).add_checked(u.mul_as_d2(s)).normalize().0;
#endif
  
  u = vldexp2_vd_vd_vi(u, q);

  u = vsel_vd_vo_vd_vd(d.gt($f64x::splat(308.25471555991671)), $f64x::splat(SLEEF_INFINITY), u);
  u = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.lt($f64x::splat(-350.)), vreinterpret_vm_vd(u)));

  return u;
}

pub fn xexpm1(a: $f64x) -> $f64x {
  D2 d = expk2(D2::new(a, $f64x::splat(0.))) + $f64x::splat(-1.);
  $f64x x = d.0 + d.1;
  x = vsel_vd_vo_vd_vd(a.gt($f64x::splat(709.782712893383996732223)), $f64x::splat(SLEEF_INFINITY), x);
  x = vsel_vd_vo_vd_vd(a.lt($f64x::splat(-36.736800569677101399113302437)), $f64x::splat(-1.), x);
  x = vsel_vd_vo_vd_vd(visnegzero_vo_vd(a), $f64x::splat(-0.), x);
  return x;
}

pub fn xlog10(d: $f64x) -> $f64x {
  D2 x;
  $f64x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mx o = d.lt($f64x::splat(f64::MIN));
  d = vsel_vd_vo_vd_vd(o, d*$f64x::splat(D1_32 * D1_32), d);
  $ix e = vilogb2k_vi_vd(d*$f64x::splat(1./0.75));
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - $ix::splat(64), e);
#else
  $f64x e = vgetexp_vd_vd(d*$f64x::splat(1./0.75));
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), $f64x::splat(1024.), e);
  m = vgetmant_vd_vd(d);
#endif

  x = $f64x::splat(-1.).add_as_d2(m) / $f64x::splat(1.).add_as_d2(m);
  x2 = x.0*x.0;

  t = $f64x::splat(+0.6653725819576758460e-1)
      .mla(x2, $f64x::splat(+0.6625722782820833712e-1))
      .mla(x2, $f64x::splat(+0.7898105214313944078e-1))
      .mla(x2, $f64x::splat(+0.9650955035715275132e-1))
      .mla(x2, $f64x::splat(+0.1240841409721444993e+0))
      .mla(x2, $f64x::splat(+0.1737177927454605086e+0))
      .mla(x2, $f64x::splat(+0.2895296546021972617e+0));
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  D2 s = D2::from((0.30102999566398119802, -2.803728127785170339e-18)) * $f64x::from(e);
#else
  D2 s = D2::from((0.30102999566398119802, -2.803728127785170339e-18)) * e;
#endif

  s = s.add_checked(x * D2::from((0.86858896380650363334, 1.1430059694096389311e-17)));
  s = s.add_checked(x2*x.0*t);

  $f64x r = s.0 + s.1;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), $f64x::splat(SLEEF_INFINITY), r);
  r = vsel_vd_vo_vd_vd(d.lt($f64x::splat(0.)) | visnan_vo_vd(d), $f64x::splat(SLEEF_NAN), r);
  r = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), $f64x::splat(-SLEEF_INFINITY), r);
#else
  r = vfixup_vd_vd_vd_vi2_i(r, d, $ix2::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}

pub fn xlog2(d: $f64x) -> $f64x {
  D2 x;
  $f64x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mx o = d.lt($f64x::splat(f64::MIN));
  d = vsel_vd_vo_vd_vd(o, d*$f64x::splat(D1_32 * D1_32), d);
  $ix e = vilogb2k_vi_vd(d*$f64x::splat(1./0.75));
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - $ix::splat(64), e);
#else
  $f64x e = vgetexp_vd_vd(d*$f64x::splat(1.0/0.75));
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), $f64x::splat(1024.), e);
  m = vgetmant_vd_vd(d);
#endif

  x = $f64x::splat(-1.).add_as_d2(m) / $f64x::splat(1.).add_as_d2(m);
  x2 = x.0*x.0;

  t = $f64x::splat(+0.2211941750456081490e+0)
      .mla(x2, $f64x::splat(+0.2200768693152277689e+0))
      .mla(x2, $f64x::splat(+0.2623708057488514656e+0))
      .mla(x2, $f64x::splat(+0.3205977477944495502e+0))
      .mla(x2, $f64x::splat(+0.4121985945485324709e+0))
      .mla(x2, $f64x::splat(+0.5770780162997058982e+0))
      .mla(x2, $f64x::splat(+0.96179669392608091449));
  
  if !cfg!("enable_avx512f") && !cfg!("enable_avx512fnofma")
    D2 s = $f64x::from(e) + x * D2::from((2.885390081777926774, 6.0561604995516736434e-18));
  } else {
    D2 s = e + x * D2::from((2.885390081777926774, 6.0561604995516736434e-18));
  }

  s += x2*x.0*t;

  $f64x r = s.0 + s.1;

  if !cfg!("enable_avx512f") && !cfg!("enable_avx512fnofma")
    r = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), $f64x::splat(SLEEF_INFINITY), r);
    r = vsel_vd_vo_vd_vd(d.lt($f64x::splat(0.)) | visnan_vo_vd(d), $f64x::splat(SLEEF_NAN), r);
    r = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), $f64x::splat(-SLEEF_INFINITY), r);
  } else {
    r = vfixup_vd_vd_vd_vi2_i(r, d, $ix2::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
  }
  
  return r;
}

pub fn xlog1p(d: $f64x) -> $f64x {
  D2 x;
  $f64x t, m, x2;

  $f64x dp1 = d + $f64x::splat(1.);

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mx o = dp1.lt($f64x::splat(f64::MIN));
  dp1 = vsel_vd_vo_vd_vd(o, dp1*$f64x::splat(D1_32 * D1_32), dp1);
  $ix e = vilogb2k_vi_vd(dp1*$f64x::splat(1./0.75));
  t = vldexp3_vd_vd_vi($f64x::splat(1.), vneg_vi_vi(e));
  m = d.mla(t, t - $f64x::splat(1.));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - $ix::splat(64), e);
  D2 s = D2::from((0.693147180559945286226764, 2.319046813846299558417771e-17)) * $f64x::from(e);
#else
  $f64x e = vgetexp_vd_vd(dp1, $f64x::splat(1./0.75));
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), $f64x::splat(1024.), e);
  t = vldexp3_vd_vd_vi($f64x::splat(1.), vneg_vi_vi(vrint_vi_vd(e)));
  m = d.mla(t, t - $f64x::splat(1.));
  D2 s = D2::from((0.693147180559945286226764, 2.319046813846299558417771e-17)) * e;
#endif

  x = D2::new(m, $f64x::splat(0.)) / $f64x::splat(2.).add_checked_as_d2(m);
  x2 = x.0*x.0;

  t = $f64x::splat(0.1532076988502701353e+0)
      .mla(x2, $f64x::splat(0.1525629051003428716e+0))
      .mla(x2, $f64x::splat(0.1818605932937785996e+0))
      .mla(x2, $f64x::splat(0.2222214519839380009e+0))
      .mla(x2, $f64x::splat(0.2857142932794299317e+0))
      .mla(x2, $f64x::splat(0.3999999999635251990e+0))
      .mla(x2, $f64x::splat(0.6666666666667333541e+0));
  
  s = s.add_checked(x.scale($f64x::splat(2.)));
  s = s.add_checked(x2*x.0*t);

  $f64x r = s.0 + s.1;
  
  r = vsel_vd_vo_vd_vd(d.gt($f64x::splat(1e+307)), $f64x::splat(SLEEF_INFINITY), r);
  r = vsel_vd_vo_vd_vd(d.lt($f64x::splat(-1.)) | visnan_vo_vd(d), $f64x::splat(SLEEF_NAN), r);
  r = vsel_vd_vo_vd_vd(d.eq($f64x::splat(-1.)), $f64x::splat(-SLEEF_INFINITY), r);
  r = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), $f64x::splat(-0.), r);
  
  return r;
}

//
#[inline]
fn vcast_vi2_i_i(int i0, int i1) -> $ix2 { return vcast_vi2_vm(vcast_vm_i_i(i0, i1)); }

pub fn xfabs(x: $f64x) -> $f64x { return vabs_vd_vd(x); }

pub fn xcopysign(x: $f64x, y: $f64x) -> $f64x { return vcopysign_vd_vd_vd(x, y); }

pub fn xfmax(x: $f64x, y: $f64x) -> $f64x {
#if (defined(__x86_64__) || defined(__i386__)) && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
  return vsel_vd_vo_vd_vd(visnan_vo_vd(y), x, x.max(y));
#else
  return vsel_vd_vo_vd_vd(visnan_vo_vd(y), x, vsel_vd_vo_vd_vd(x.gt(y), x, y));
#endif
}

pub fn xfmin(x: $f64x, y: $f64x) -> $f64x {
#if (defined(__x86_64__) || defined(__i386__)) && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
  return vsel_vd_vo_vd_vd(visnan_vo_vd(y), x, x.min(y));
#else
  return vsel_vd_vo_vd_vd(visnan_vo_vd(y), x, vsel_vd_vo_vd_vd(y.gt(x), x, y));
#endif
}

pub fn xfdim(x: $f64x, y: $f64x) -> $f64x {
  $f64x ret = x - y;
  ret = vsel_vd_vo_vd_vd(ret.lt($f64x::splat(0.)) | x.eq(y), $f64x::splat(0.), ret);
  return ret;
}

pub fn xtrunc(x: $f64x) -> $f64x {
  $f64x fr = x - $f64x::splat(D1_31)*$f64x::from(vtruncate_vi_vd(x*$f64x::splat(1. / D1_31)));
  fr -= $f64x::from(vtruncate_vi_vd(fr));
  return vsel_vd_vo_vd_vd(visinf_vo_vd(x) | vabs_vd_vd(x).ge($f64x::splat(D1_52)), x, vcopysign_vd_vd_vd(x - fr, x));
}

pub fn xfloor(x: $f64x) -> $f64x {
  $f64x fr = x - $f64x::splat(D1_31)*$f64x::from(vtruncate_vi_vd(x*$f64x::splat(1. / D1_31)));
  fr -= $f64x::from(vtruncate_vi_vd(fr));
  fr = vsel_vd_vo_vd_vd(fr.lt($f64x::splat(0.)), fr + $f64x::splat(1.), fr);
  return vsel_vd_vo_vd_vd(visinf_vo_vd(x) | vabs_vd_vd(x).ge($f64x::splat(D1_52)), x, vcopysign_vd_vd_vd(x - fr, x));
}

pub fn xceil(x: $f64x) -> $f64x {
  $f64x fr = x - $f64x::splat(D1_31)*$f64x::from(vtruncate_vi_vd(x*$f64x::splat(1. / D1_31)));
  fr -= $f64x::from(vtruncate_vi_vd(fr));
  fr = vsel_vd_vo_vd_vd(fr.le($f64x::splat(0.)), fr, fr - $f64x::splat(1.));
  return vsel_vd_vo_vd_vd(visinf_vo_vd(x) | vabs_vd_vd(x).ge($f64x::splat(D1_52)), x, vcopysign_vd_vd_vd(x - fr, x));
}

pub fn xround(d: $f64x) -> $f64x {
  $f64x x = d + $f64x::splat(0.5);
  $f64x fr = x - $f64x::splat(D1_31)*$f64x::from(vtruncate_vi_vd(x*$f64x::splat(1. / D1_31)));
  fr -= $f64x::from(vtruncate_vi_vd(fr));
  x = vsel_vd_vo_vd_vd(x.le($f64x::splat(0.)) & fr.eq($f64x::splat(0.)), x - $f64x::splat(1.), x);
  fr = vsel_vd_vo_vd_vd(fr.lt($f64x::splat(0.)), fr + $f64x::splat(1.), fr);
  x = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.49999999999999994449)), $f64x::splat(0.), x);
  return vsel_vd_vo_vd_vd(visinf_vo_vd(d) | vabs_vd_vd(d).ge($f64x::splat(D1_52)), d, vcopysign_vd_vd_vd(x - fr, d));
}

pub fn xrint(d: $f64x) -> $f64x {
  $f64x x = d + $f64x::splat(0.5);
  $f64x fr = x - $f64x::splat(D1_31)*$f64x::from(vtruncate_vi_vd(x*$f64x::splat(1. / D1_31)));
  $mx isodd = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi($ix::splat(1), vtruncate_vi_vd(fr)), $ix::splat(1)));
  fr -= $f64x::from(vtruncate_vi_vd(fr));
  fr = vsel_vd_vo_vd_vd(fr.lt($f64x::splat(0)) | (fr.eq($f64x::splat(0)) & isodd), fr + $f64x::splat(1.), fr);
  x = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.50000000000000011102)), $f64x::splat(0.), x);
  $f64x ret = vsel_vd_vo_vd_vd(visinf_vo_vd(d) | vabs_vd_vd(d).ge($f64x::splat(D1_52)), d, vcopysign_vd_vd_vd(x - fr, d));
  return ret;
}

pub fn xnextafter(x: $f64x, y: $f64x) -> $f64x {
  x = vsel_vd_vo_vd_vd(x.eq($f64x::splat(0.)), vmulsign_vd_vd_vd($f64x::splat(0.), y), x);
  $ix2 t, xi2 = vreinterpret_vi2_vd(x);
  $mx c = vsignbit_vo_vd(x) ^ y.ge(x);

  t = vadd_vi2_vi2_vi2(vxor_vi2_vi2_vi2(xi2, vcast_vi2_i_i(0x7fffffff, 0xffffffff)), vcast_vi2_i_i(0, 1));
  t = vadd_vi2_vi2_vi2(t, vrev21_vi2_vi2(vand_vi2_vi2_vi2(vcast_vi2_i_i(0, 1), veq_vi2_vi2_vi2(t, vcast_vi2_i_i(-1, 0)))));
  xi2 = vreinterpret_vi2_vd(vsel_vd_vo_vd_vd(c, vreinterpret_vd_vi2(t), vreinterpret_vd_vi2(xi2)));

  xi2 = vsub_vi2_vi2_vi2(xi2, vcast_vi2_vm(vand_vm_vo64_vm(x.ne(y), vcast_vm_i_i(0, 1))));

  xi2 = vreinterpret_vi2_vd(vsel_vd_vo_vd_vd(x.ne(y),
					     vreinterpret_vd_vi2(vadd_vi2_vi2_vi2(xi2, vrev21_vi2_vi2(vand_vi2_vi2_vi2(vcast_vi2_i_i(0, -1), veq_vi2_vi2_vi2(xi2, vcast_vi2_i_i(0, -1)))))),
					     vreinterpret_vd_vi2(xi2)));

  t = vadd_vi2_vi2_vi2(vxor_vi2_vi2_vi2(xi2, vcast_vi2_i_i(0x7fffffff, 0xffffffff)), vcast_vi2_i_i(0, 1));
  t = vadd_vi2_vi2_vi2(t, vrev21_vi2_vi2(vand_vi2_vi2_vi2(vcast_vi2_i_i(0, 1), veq_vi2_vi2_vi2(t, vcast_vi2_i_i(-1, 0)))));
  xi2 = vreinterpret_vi2_vd(vsel_vd_vo_vd_vd(c, vreinterpret_vd_vi2(t), vreinterpret_vd_vi2(xi2)));

  $f64x ret = vreinterpret_vd_vi2(xi2);

  ret = vsel_vd_vo_vd_vd(ret.eq($f64x::splat(0.)) & x.ne($f64x::splat(0.)), 
			 vmulsign_vd_vd_vd($f64x::splat(0.), x), ret);

  ret = vsel_vd_vo_vd_vd(x.eq($f64x::splat(0.)) & y.eq($f64x::splat(0.)), y, ret);

  ret = vsel_vd_vo_vd_vd(visnan_vo_vd(x) | visnan_vo_vd(y), $f64x::splat(SLEEF_NAN), ret);
  
  return ret;
}

pub fn xfrfrexp(x: $f64x) -> $f64x {
  x = vsel_vd_vo_vd_vd(vabs_vd_vd(x).lt($f64x::splat(f64::MIN)), x*$f64x::splat(D1_63), x);

  vmask xm = vreinterpret_vm_vd(x);
  xm = vand_vm_vm_vm(xm, vcast_vm_i_i(~0x7ff00000, ~0));
  xm = vor_vm_vm_vm (xm, vcast_vm_i_i( 0x3fe00000,  0));

  $f64x ret = vreinterpret_vd_vm(xm);

  ret = vsel_vd_vo_vd_vd(visinf_vo_vd(x), vmulsign_vd_vd_vd($f64x::splat(SLEEF_INFINITY), x), ret);
  ret = vsel_vd_vo_vd_vd(x.eq($f64x::splat(0.)), x, ret);
  
  return ret;
}

pub fn xexpfrexp(x: $f64x) -> $ix {
  x = vsel_vd_vo_vd_vd(vabs_vd_vd(x).lt($f64x::splat(f64::MIN)), x*$f64x::splat(D1_63), x);

  let ret = $ix::from(vreinterpret_vi2_vd(x));
  ret = vand_vi_vi_vi(vsrl_vi_vi_i(ret, 20), $ix::splat(0x7ff)) - $ix::splat(0x3fe);

  ret = vsel_vi_vo_vi_vi(x.eq($f64x::splat(0.)) | visnan_vo_vd(x) | visinf_vo_vd(x), $ix::splat(0), ret);
  
  return ret;
}

pub fn xfma($f64x x, $f64x y, $f64x z) -> $f64x {
  $f64x h2 = x*y + z;
  q = $f64x::splat(1.);
  $mx o = (vabs_vd_vd(h2).lt($f64x::splat(1e-300));
  const C0 : f64 = D1_54;
  const C1 : f64 = C0 * C0;
  const C2 : f64 = C1 * C1;
  {
    x = vsel_vd_vo_vd_vd(o, x*$f64x::splat(C1), x);
    y = vsel_vd_vo_vd_vd(o, y*$f64x::splat(C1), y);
    z = vsel_vd_vo_vd_vd(o, z*$f64x::splat(C2), z);
    q = vsel_vd_vo_vd_vd(o, $f64x::splat(1. / C2), q);
  }
  o = vabs_vd_vd(h2).gt($f64x::splat(1e+300));
  {
    x = vsel_vd_vo_vd_vd(o, x*$f64x::splat(1. / C1), x);
    y = vsel_vd_vo_vd_vd(o, y*$f64x::splat(1. / C1), y);
    z = vsel_vd_vo_vd_vd(o, z*$f64x::splat(1. / C2), z);
    q = vsel_vd_vo_vd_vd(o, $f64x::splat(C2), q);
  }
  D2 d = x.mul_as_d2(y);
  d += z;
  $f64x ret = vsel_vd_vo_vd_vd(x.eq($f64x::splat(0.)) | y.eq($f64x::splat(0.)), z, d.0 + d.1);
  o = visinf_vo_vd(z);
  o = vandnot_vo_vo_vo(visinf_vo_vd(x), o);
  o = vandnot_vo_vo_vo(visnan_vo_vd(x), o);
  o = vandnot_vo_vo_vo(visinf_vo_vd(y), o);
  o = vandnot_vo_vo_vo(visnan_vo_vd(y), o);
  h2 = vsel_vd_vo_vd_vd(o, z, h2);

  o = visinf_vo_vd(h2) | visnan_vo_vd(h2);
  
  return vsel_vd_vo_vd_vd(o, h2, ret*q);
}

SQRTU05_FUNCATR $f64x xsqrt_u05(d: $f64x) {
  $f64x q;
  $mx o;
  
  d = vsel_vd_vo_vd_vd(d.lt($f64x::splat(0.)), $f64x::splat(SLEEF_NAN), d);

  o = d.lt($f64x::splat(8.636168555094445e-78));
  d = vsel_vd_vo_vd_vd(o, d*$f64x::splat(1.157920892373162e77), d);
  q = vsel_vd_vo_vd_vd(o, $f64x::splat(2.9387358770557188e-39*0.5), $f64x::splat(0.5));

  o = d.gt($f64x::splat(1.3407807929942597e+154));
  d = vsel_vd_vo_vd_vd(o, d*$f64x::splat(7.4583407312002070e-155), d);
  q = vsel_vd_vo_vd_vd(o, $f64x::splat(1.1579208923731620e+77*0.5), q);

  $f64x x = vreinterpret_vd_vi2(vsub_vi2_vi2_vi2(vcast_vi2_i_i(0x5fe6ec86, 0), vsrl_vi2_vi2_i(vreinterpret_vi2_vd(d + $f64x::splat(1e-320)), 1)));

  x *= $f64x::splat(1.5) - $f64x::splat(0.5)*d*x*x;
  x *= $f64x::splat(1.5) - $f64x::splat(0.5)*d*x*x;
  x *= $f64x::splat(1.5) - $f64x::splat(0.5)*d*x*x;
  x *= d;

  D2 d2 = (d + x.mul_as_d2(x)) * x.rec_as_d2();

  x = (d2.0 + d2.1)*q;

  x = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), $f64x::splat(SLEEF_INFINITY), x);
  x = vsel_vd_vo_vd_vd(d.eq($f64x::splat(0.)), d, x);
  
  return x;
}

pub fn xsqrt(d: $f64x) -> $f64x {
#ifdef ACCURATE_SQRT
  return vsqrt_vd_vd(d);
#endif
  // fall back to approximation if ACCURATE_SQRT is undefined
  return xsqrt_u05(d);
}

pub fn xsqrt_u35(d: $f64x) -> $f64x { return xsqrt_u05(d); }

pub fn xhypot_u05(x: $f64x, y: $f64x) -> $f64x {
  x = vabs_vd_vd(x);
  y = vabs_vd_vd(y);
  $f64x min = x.min(y);
  n = min;
  $f64x max = x.max(y);
  d = max;

  $mx o = max.lt($f64x::splat(f64::MIN));
  n = vsel_vd_vo_vd_vd(o, n*$f64x::splat(D1_54), n);
  d = vsel_vd_vo_vd_vd(o, d*$f64x::splat(D1_54), d);

  D2 t = D2::new(n, $f64x::splat(0.) / D2::new(d, $f64x::splat(0.));
  t = (t.square() + $f64x::splat(1.)).sqrt() * max;
  $f64x ret = t.0 + t.1;
  ret = vsel_vd_vo_vd_vd(visnan_vo_vd(ret), $f64x::splat(SLEEF_INFINITY), ret);
  ret = vsel_vd_vo_vd_vd(min.eq($f64x::splat(0.)), max, ret);
  ret = vsel_vd_vo_vd_vd(visnan_vo_vd(x) | visnan_vo_vd(y), $f64x::splat(SLEEF_NAN), ret);
  ret = vsel_vd_vo_vd_vd(x.eq($f64x::splat(SLEEF_INFINITY)) | y.eq($f64x::splat(SLEEF_INFINITY)), $f64x::splat(SLEEF_INFINITY), ret);

  return ret;
}

pub fn xhypot_u35(x: $f64x, y: $f64x) -> $f64x {
  x = vabs_vd_vd(x);
  y = vabs_vd_vd(y);
  $f64x min = x.min(y);
  $f64x max = x.max(y);

  $f64x t = min / max;
  $f64x ret = max*vsqrt_vd_vd(t.mla(t, $f64x::splat(1.)));
  ret = vsel_vd_vo_vd_vd(min.eq($f64x::splat(0.)), max, ret);
  ret = vsel_vd_vo_vd_vd(visnan_vo_vd(x) | visnan_vo_vd(y), $f64x::splat(SLEEF_NAN), ret);
  ret = vsel_vd_vo_vd_vd(x.eq($f64x::splat(SLEEF_INFINITY)) | y.eq($f64x::splat(SLEEF_INFINITY)), $f64x::splat(SLEEF_INFINITY), ret);

  return ret;
}
#[inline]
fn vtoward0(x: $f64x) -> $f64x { // returns nextafter(x, 0)
  $f64x t = vreinterpret_vd_vm(vadd64_vm_vm_vm(vreinterpret_vm_vd(x), vcast_vm_i_i(-1, -1)));
  return vsel_vd_vo_vd_vd(x.eq($f64x::splat(0.)), $f64x::splat(0.), t);
}
#[inline]
fn vptrunc(x: $f64x) -> $f64x { // round to integer toward 0, positive argument only
#ifdef FULL_FP_ROUNDING
  return vtruncate_vd_vd(x);
#else
  $f64x fr = $f64x::splat(-D1_31).mla($f64x::from(vtruncate_vi_vd(x*$f64x::splat(1. / D1_31))), x);
  fr -= $f64x::from(vtruncate_vi_vd(fr));
  return vsel_vd_vo_vd_vd(vabs_vd_vd(x).ge($f64x::splat(D1_52)), x, x - fr);
#endif
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xfmod(x: $f64x, y: $f64x) -> $f64x {
  $f64x nu = vabs_vd_vd(x), de = vabs_vd_vd(y), s = $f64x::splat(1.), q;
  $mx o = de.lt($f64x::splat(f64::MIN));
  nu = vsel_vd_vo_vd_vd(o, nu*$f64x::splat(D1_54), nu);
  de = vsel_vd_vo_vd_vd(o, de*$f64x::splat(D1_54), de);
  s  = vsel_vd_vo_vd_vd(o, s*$f64x::splat(1. / D1_54), s);
  $f64x rde = vtoward0(vrec_vd_vd(de));
  D2 r = D2::new(nu, $f64x::splat(0.));

  for(int i=0;i<21;i++) { // ceil(log2(DBL_MAX) / 51) + 1
    q = vsel_vd_vo_vd_vd((de + de).gt(r.0) & r.0.ge(de),
			 $f64x::splat(1.), vtoward0(r.0)*rde);
    q = vreinterpret_vd_vm(vand_vm_vm_vm(vreinterpret_vm_vd(vptrunc(q)), vcast_vm_i_i(0xffffffff, 0xfffffffe)));
    r = (r + q.mul_as_d2(-de)).normalize();
    if (vtestallones_i_vo64(r.0.lt(de))) break;
  }
  
  $f64x ret = r.0*s;
  ret = vsel_vd_vo_vd_vd((r.0 + r.1).eq(de), $f64x::splat(0.), ret);

  ret = vmulsign_vd_vd_vd(ret, x);

  ret = vsel_vd_vo_vd_vd(nu.lt(de), x, ret);
  ret = vsel_vd_vo_vd_vd(de.eq($f64x::splat(0.)), $f64x::splat(SLEEF_NAN), ret);

  return ret;
}

#if defined(ENABLE_SVE) || defined(ENABLE_SVENOFMA)
  typedef __sizeless_struct {
    D2 a, b;
  } dd2;
#else
  typedef struct {
    D2 a, b;
  } dd2;
#endif

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
static CONST dd2 gammak(a: $f64x) {
  D2 clc = D2::from((0., 0.));
  clln = D2::from((1., 0.));
  clld = D2::from((1., 0.));
  D2 v = D2::from((1., 0.));
  x, y, z;
  $f64x t, u;

  $mx otiny = vabs_vd_vd(a).lt($f64x::splat(1e-306));
  oref = a.lt($f64x::splat(0.5));

  x = vsel_vd2_vo_vd2_vd2(otiny, D2::from((0., 0.)),
			  vsel_vd2_vo_vd2_vd2(oref, $f64x::splat(1.).add_as_d2(-a),
					      D2::new(a, $f64x::splat(0.))));

  $mx o0 = $f64x::splat(0.5).le(x.0) & x.0.le($f64x::splat(1.1));
  $mx o2 = $f64x::splat(2.3).le(x.0);
  
  y = ((x + $f64x::splat(1.)) * x).normalize();
  y = ((x + $f64x::splat(2.)) * y).normalize();
  y = ((x + $f64x::splat(3.)) * y).normalize();
  y = ((x + $f64x::splat(4.)) * y).normalize();

  $mx o = o2 & x.0.le($f64x::splat(7.));
  clln = vsel_vd2_vo_vd2_vd2(o, y, clln);

  x = vsel_vd2_vo_vd2_vd2(o, x + $f64x::splat(5.), x);
  
  t = vsel_vd_vo_vd_vd(o2, vrec_vd_vd(x.0), (x + vsel_vd_vo_d_d(o0, -1, -2)).normalize().0);

  u = vsel_vd_vo_vo_d_d_d(o2, o0, -156.801412704022726379848862, 0.2947916772827614196e+2, 0.7074816000864609279e-7)
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 1.120804464289911606838558160000, 0.1281459691827820109e+3, 0.4009244333008730443e-6))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 13.39798545514258921833306020000, 0.2617544025784515043e+3, 0.1040114641628246946e-5))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -0.116546276599463200848033357000, 0.3287022855685790432e+3, 0.1508349150733329167e-5))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -1.391801093265337481495562410000, 0.2818145867730348186e+3, 0.1288143074933901020e-5))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 0.015056113040026424412918973400, 0.1728670414673559605e+3, 0.4744167749884993937e-6))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 0.179540117061234856098844714000, 0.7748735764030416817e+2, -0.6554816306542489902e-7))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -0.002481743600264997730942489280, 0.2512856643080930752e+2, -0.3189252471452599844e-6))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -0.029527880945699120504851034100, 0.5766792106140076868e+1, 0.1358883821470355377e-6))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 0.000540164767892604515196325186, 0.7270275473996180571e+0, -0.4343931277157336040e-6))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 0.006403362833808069794787256200, 0.8396709124579147809e-1, 0.9724785897406779555e-6))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -0.000162516262783915816896611252, -0.8211558669746804595e-1, -0.2036886057225966011e-5))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -0.001914438498565477526465972390, 0.6828831828341884458e-1, 0.4373363141819725815e-5))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 7.20489541602001055898311517e-05, -0.7712481339961671511e-1, -0.9439951268304008677e-5))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 0.000839498720672087279971000786, 0.8337492023017314957e-1, 0.2050727030376389804e-4))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -5.17179090826059219329394422e-05, -0.9094964931456242518e-1, -0.4492620183431184018e-4))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -0.000592166437353693882857342347, 0.1000996313575929358e+0, 0.9945751236071875931e-4))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 6.97281375836585777403743539e-05, -0.1113342861544207724e+0, -0.2231547599034983196e-3))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 0.000784039221720066627493314301, 0.1255096673213020875e+0, 0.5096695247101967622e-3))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -0.000229472093621399176949318732, -0.1440498967843054368e+0, -0.1192753911667886971e-2))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, -0.002681327160493827160473958490, 0.1695571770041949811e+0, 0.2890510330742210310e-2))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 0.003472222222222222222175164840, -0.2073855510284092762e+0, -0.7385551028674461858e-2))
      .mla(t, vsel_vd_vo_vo_d_d_d(o2, o0, 0.083333333333333333335592087900, 0.2705808084277815939e+0, 0.2058080842778455335e-1));

  y = (x + $f64x::splat(-0.5)) * logk2(x);
  y += -x;
  y += D2::from((0.91893853320467278056, -3.8782941580672414498e-17)); // 0.5*log(2*M_PI)

  z = u.mul_as_d2(t) + vsel_vd_vo_d_d(o0, -0.4006856343865314862e+0, -0.6735230105319810201e-1);
  z = z * t + vsel_vd_vo_d_d(o0, 0.8224670334241132030e+0, 0.3224670334241132030e+0);
  z = z * t + vsel_vd_vo_d_d(o0, -0.5772156649015328655e+0, 0.4227843350984671345e+0);
  z = z * t;

  clc = vsel_vd2_vo_vd2_vd2(o2, y, z);
  
  clld = vsel_vd2_vo_vd2_vd2(o2, u.mul_as_d2(t) + $f64x::splat(1.), clld);
  
  y = clln;

  clc = vsel_vd2_vo_vd2_vd2(otiny, D2::from((83.1776616671934334590333, 3.67103459631568507221878e-15)), // log(2^120)
			    vsel_vd2_vo_vd2_vd2(oref, D2::from((1.1447298858494001639, 1.026595116270782638e-17)) + (-clc), clc)); // log(M_PI)
  clln = vsel_vd2_vo_vd2_vd2(otiny, D2::from((1., 0.)), vsel_vd2_vo_vd2_vd2(oref, clln, clld));

  if (!vtestallones_i_vo64(vnot_vo64_vo64(oref))) {
    t = a - D1_28*$f64x::from(vtruncate_vi_vd(a*$f64x::splat(1. / D1_28)));
    x = clld * sinpik(t);
  }
  
  clld = vsel_vd2_vo_vd2_vd2(otiny, D2::new(a*$f64x::splat(D1_60*D1_60), $f64x::splat(0.)),
			     vsel_vd2_vo_vd2_vd2(oref, x, y));

  dd2 ret = { clc, clln / clld };

  return ret;
}

pub fn xtgamma_u1(a: $f64x) -> $f64x {
  dd2 d = gammak(a);
  D2 y = expk2(d.a) * d.b;
  $f64x r = y.0 + y.1;
  $mx o;

  o = a.eq($f64x::splat(-SLEEF_INFINITY)) |
				(a.lt($f64x::splat(0.)) & visint_vo_vd(a)) |
		   (visnumber_vo_vd(a) & a.lt($f64x::splat(0.)) & visnan_vo_vd(r));
  r = vsel_vd_vo_vd_vd(o, $f64x::splat(SLEEF_NAN), r);

  o = ((a.eq($f64x::splat(SLEEF_INFINITY)) | visnumber_vo_vd(a)) &
				  a.ge($f64x::splat(-f64::MIN))) &
		    (a.eq($f64x::splat(0.)) | a.gt($f64x::splat(200)) | visnan_vo_vd(r));
  r = vsel_vd_vo_vd_vd(o, vmulsign_vd_vd_vd($f64x::splat(SLEEF_INFINITY), a), r);
  
  return r;
}

pub fn xlgamma_u1(a: $f64x) -> $f64x {
  dd2 d = gammak(a);
  D2 y = d.a + logk2(d.b.abs());
  $f64x r = y.0 + y.1;
  $mx o;

  o = visinf_vo_vd(a) |
		   (a.le($f64x::splat(0.)) & visint_vo_vd(a)) |
				(visnumber_vo_vd(a) & visnan_vo_vd(r));
  r = vsel_vd_vo_vd_vd(o, $f64x::splat(SLEEF_INFINITY), r);

  return r;
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xerf_u1(a: $f64x) -> $f64x {
  $f64x s = a, t, u;
  D2 d;

  a = vabs_vd_vd(a);
  $mx o0 = a.lt($f64x::splat(1.));
  $mx o1 = a.lt($f64x::splat(3.7));
  $mx o2 = a.lt($f64x::splat(6.));
  u = vsel_vd_vo_vd_vd(o0, a*a, a);
  
  t = vsel_vd_vo_vo_d_d_d(o0, o1, 0.6801072401395392157e-20, 0.2830954522087717660e-13, -0.5846750404269610493e-17)
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.2161766247570056391e-18, -0.1509491946179481940e-11, 0.6076691048812607898e-15))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.4695919173301598752e-17, 0.3827857177807173152e-10, -0.3007518609604893831e-13))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.9049140419888010819e-16, -0.6139733921558987241e-09, 0.9427906260824646063e-12))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.1634018903557411517e-14, 0.6985387934608038824e-08, -0.2100110908269393629e-10))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.2783485786333455216e-13, -0.5988224513034371474e-07, 0.3534639523461223473e-09))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.4463221276786412722e-12, 0.4005716952355346640e-06, -0.4664967728285395926e-08))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.6711366622850138987e-11, -0.2132190104575784400e-05, 0.4943823283769000532e-07))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.9422759050232658346e-10, 0.9092461304042630325e-05, -0.4271203394761148254e-06))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.1229055530100228477e-08, -0.3079188080966205457e-04, 0.3034067677404915895e-05))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.1480719281585085023e-07, 0.7971413443082370762e-04, -0.1776295289066871135e-04))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.1636584469123402714e-06, -0.1387853215225442864e-03, 0.8524547630559505050e-04))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.1646211436588923363e-05, 0.6469678026257590965e-04, -0.3290582944961784398e-03))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.1492565035840624866e-04, 0.4996645280372945860e-03, 0.9696966068789101157e-03))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.1205533298178966496e-03, -0.1622802482842520535e-02, -0.1812527628046986137e-02))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.8548327023450851166e-03, 0.1615320557049377171e-03, -0.4725409828123619017e-03))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.5223977625442188799e-02, 0.1915262325574875607e-01, 0.2090315427924229266e-01))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.2686617064513125569e-01, -0.1027818298486033455e+00, -0.1052041921842776645e+00))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, 0.1128379167095512753e+00, -0.6366172819842503827e+00, -0.6345351808766568347e+00))
      .mla(u, vsel_vd_vo_vo_d_d_d(o0, o1, -0.3761263890318375380e+00, -0.1128379590648910469e+01, -0.1129442929103524396e+01));
  d = t.mul_as_d2(u);

  d += D2::new(vsel_vd_vo_vo_d_d_d(o0, o1, 1.1283791670955125586, 3.4110644736196137587e-08, 0.00024963035690526438285),
					    vsel_vd_vo_vo_d_d_d(o0, o1, 1.5335459613165822674e-17, -2.4875650708323294246e-24, -5.4362665034856259795e-21));
  d = vsel_vd2_vo_vd2_vd2(o0, d * a, $f64x::splat(1.).add_checked(-expk2(d)));

  u = vmulsign_vd_vd_vd(vsel_vd_vo_vd_vd(o2, d.0 + d.1, $f64x::splat(1.)), s);
  u = vsel_vd_vo_vd_vd(visnan_vo_vd(a), $f64x::splat(SLEEF_NAN), u);

  return u;
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xerfc_u15(a: $f64x) -> $f64x {
  $f64x s = a, r = $f64x::splat(0.), t;
  D2 u, d, x;
  a = vabs_vd_vd(a);
  $mx o0 = a.lt($f64x::splat(1.));
  $mx o1 = a.lt($f64x::splat(2.2));
  $mx o2 = a.lt($f64x::splat(4.2));
  $mx o3 = a.lt($f64x::splat(27.3));

  u = vsel_vd2_vo_vd2_vd2(o0, a.mul_as_d2(a), vsel_vd2_vo_vd2_vd2(o1, D2::new(a, $f64x::splat(0.)), D2::from((1., 0.)) / D2::new(a, $f64x::splat(0.))));

  t = vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.6801072401395386139e-20, 0.3438010341362585303e-12, -0.5757819536420710449e+2, 0.2334249729638701319e+5)
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.2161766247570055669e-18, -0.1237021188160598264e-10, 0.4669289654498104483e+3, -0.4695661044933107769e+5))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.4695919173301595670e-17, 0.2117985839877627852e-09, -0.1796329879461355858e+4, 0.3173403108748643353e+5))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.9049140419888007122e-16, -0.2290560929177369506e-08, 0.4355892193699575728e+4, 0.3242982786959573787e+4))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.1634018903557410728e-14, 0.1748931621698149538e-07, -0.7456258884965764992e+4, -0.2014717999760347811e+5))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.2783485786333451745e-13, -0.9956602606623249195e-07, 0.9553977358167021521e+4, 0.1554006970967118286e+5))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.4463221276786415752e-12, 0.4330010240640327080e-06, -0.9470019905444229153e+4, -0.6150874190563554293e+4))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.6711366622850136563e-11, -0.1435050600991763331e-05, 0.7387344321849855078e+4, 0.1240047765634815732e+4))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.9422759050232662223e-10, 0.3460139479650695662e-05, -0.4557713054166382790e+4, -0.8210325475752699731e+2))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.1229055530100229098e-08, -0.4988908180632898173e-05, 0.2207866967354055305e+4, 0.3242443880839930870e+2))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.1480719281585086512e-07, -0.1308775976326352012e-05, -0.8217975658621754746e+3, -0.2923418863833160586e+2))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.1636584469123399803e-06, 0.2825086540850310103e-04, 0.2268659483507917400e+3, 0.3457461732814383071e+0))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.1646211436588923575e-05, -0.6393913713069986071e-04, -0.4633361260318560682e+2, 0.5489730155952392998e+1))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.1492565035840623511e-04, -0.2566436514695078926e-04, 0.9557380123733945965e+1, 0.1559934132251294134e-2))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.1205533298178967851e-03, 0.5895792375659440364e-03, -0.2958429331939661289e+1, -0.1541741566831520638e+1))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.8548327023450850081e-03, -0.1695715579163588598e-02, 0.1670329508092765480e+0, 0.2823152230558364186e-5))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.5223977625442187932e-02, 0.2089116434918055149e-03, 0.6096615680115419211e+0, 0.6249999184195342838e+0))
      .mla(u.0, vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.2686617064513125222e-01, 0.1912855949584917753e-01, 0.1059212443193543585e-2, 0.1741749416408701288e-8));

  d = u * t;
  d += D2::new(vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.11283791670955126141, -0.10277263343147646779, -0.50005180473999022439, -0.5000000000258444377),
					    vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -4.0175691625932118483e-18, -6.2338714083404900225e-18, 2.6362140569041995803e-17, -4.0074044712386992281e-17));
  d *= u;
  d += D2::new(vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.37612638903183753802, -0.63661976742916359662, 1.601106273924963368e-06, 2.3761973137523364792e-13),
					    vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 1.3391897206042552387e-17, 7.6321019159085724662e-18, 1.1974001857764476775e-23, -1.1670076950531026582e-29));
  d *= u;
  d += D2::new(vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 1.1283791670955125586, -1.1283791674717296161, -0.57236496645145429341, -0.57236494292470108114),
					    vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 1.5335459613165822674e-17, 8.0896847755965377194e-17, 3.0704553245872027258e-17, -2.3984352208056898003e-17));
  
  x = vsel_vd2_vo_vd2_vd2(o1, d, D2::new(-a, $f64x::splat(0.))) * a;
  x = vsel_vd2_vo_vd2_vd2(o1, x, x + d);
  x = vsel_vd2_vo_vd2_vd2(o0, D2::from((1., 0.)).sub_checked(x), expk2(x));
  x = vsel_vd2_vo_vd2_vd2(o1, x, x * u);

  r = vsel_vd_vo_vd_vd(o3, x.0 + x.1, $f64x::splat(0.));
  r = vsel_vd_vo_vd_vd(vsignbit_vo_vd(s), $f64x::splat(2.) - r, r);
  r = vsel_vd_vo_vd_vd(visnan_vo_vd(s), $f64x::splat(SLEEF_NAN), r);
  return r;
}

#ifndef ENABLE_GNUABI
pub fn xgetInt(int name) -> int {
  if (1 <= name && name <= 10) return vavailability_i(name);
  return 0;
}

pub fn *xgetPtr(int name) -> void {
  if (name == 0) return ISANAME;
  return (void *)0;
}
#endif

/*#ifdef ALIAS_NO_EXT_SUFFIX
#include ALIAS_NO_EXT_SUFFIX
#endif

#ifdef ENABLE_GNUABI
/* "finite" aliases for compatibility with GLIBC */
pub fn $f64x __acos_finite     ($f64x)          __attribute__((weak, alias(str_xacos     )));
pub fn $f64x __acosh_finite    ($f64x)          __attribute__((weak, alias(str_xacosh    )));
pub fn $f64x __asin_finite     (double)           __attribute__((weak, alias(str_xasin_u1  )));
pub fn $f64x __atan2_finite    ($f64x, $f64x) __attribute__((weak, alias(str_xatan2_u1 )));
pub fn $f64x __atanh_finite    ($f64x)          __attribute__((weak, alias(str_xatanh    )));
pub fn $f64x __cosh_finite     ($f64x)          __attribute__((weak, alias(str_xcosh     )));
pub fn $f64x __exp10_finite    ($f64x)          __attribute__((weak, alias(str_xexp10    )));
pub fn $f64x __exp2_finite     ($f64x)          __attribute__((weak, alias(str_xexp2     )));
pub fn $f64x __exp_finite      ($f64x)          __attribute__((weak, alias(str_xexp      )));
pub fn $f64x __fmod_finite     ($f64x, $f64x) __attribute__((weak, alias(str_xfmod     )));
pub fn $f64x __modf_finite     ($f64x, $f64x *) __attribute__((weak, alias(str_xmodf   )));
pub fn $f64x __hypot_u05_finite($f64x, $f64x) __attribute__((weak, alias(str_xhypot_u05)));
pub fn $f64x __lgamma_u1_finite($f64x)          __attribute__((weak, alias(str_xlgamma_u1)));
pub fn $f64x __log10_finite    ($f64x)          __attribute__((weak, alias(str_xlog10    )));
pub fn $f64x __log_finite      ($f64x)          __attribute__((weak, alias(str_xlog_u1   )));
pub fn $f64x __pow_finite      ($f64x, $f64x) __attribute__((weak, alias(str_xpow      )));
pub fn $f64x __sinh_finite     ($f64x)          __attribute__((weak, alias(str_xsinh     )));
pub fn $f64x __sqrt_finite     ($f64x)          __attribute__((weak, alias(str_xsqrt     )));
pub fn $f64x __tgamma_u1_finite($f64x)          __attribute__((weak, alias(str_xtgamma_u1)));

#ifdef HEADER_MASKED
#include HEADER_MASKED
#endif
#endif /* #ifdef ENABLE_GNUABI */
*/
