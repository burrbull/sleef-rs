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

//

#include "df.h"
#[inline]
fn visnegzero_vo_vf(d: $f32x) -> $mox {
  return veq_vo_vi2_vi2(vreinterpret_vi2_vf(d), vreinterpret_vi2_vf($f32x::splat(-0.)));
}

#[inline]
fn $mox vnot_vo32_vo32($mox x) {
  return x ^ veq_vo_vi2_vi2($ix2::splat(0), $ix2::splat(0));
}
#[inline]
fn vsignbit_vm_vf(f: $f32x) -> vmask {
  return vand_vm_vm_vm(vreinterpret_vm_vf(f), vreinterpret_vm_vf($f32x::splat(-0.)));
}
#[inline]
fn vmulsign_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x {
  return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x), vsignbit_vm_vf(y)));
}
#[inline]
fn vcopysign_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x {
  return vreinterpret_vf_vm(vxor_vm_vm_vm(vandnot_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.)), vreinterpret_vm_vf(x)), 
					  vand_vm_vm_vm   (vreinterpret_vm_vf($f32x::splat(-0.)), vreinterpret_vm_vf(y))));
}
#[inline]
fn vsign_vf_vf(f: $f32x) -> $f32x {
  return vreinterpret_vf_vm(vor_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(1.)), vand_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.)), vreinterpret_vm_vf(f))));
}
#[inline]
fn vsignbit_vo_vf(d: $f32x) -> $mox {
  return veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vreinterpret_vi2_vf(d), $ix2::splat(0x80000000)), $ix2::splat(0x80000000));
}
#[inline]
fn vsel_vi2_vf_vf_vi2_vi2($f32x f0, $f32x f1, $ix2 x, $ix2 y) -> $ix2 {
  return vsel_vi2_vo_vi2_vi2(f0.lt(f1), x, y);
}
#[inline]
fn vsel_vi2_vf_vi2($f32x d, $ix2 x) -> $ix2 {
  return vand_vi2_vo_vi2(vsignbit_vo_vf(d), x);
}
#[inline]
fn visint_vo_vf(y: $f32x) -> $mox { return vtruncate_vf_vf(y).eq(y); }
#[inline]
fn visnumber_vo_vf(x: $f32x) -> $mox { return vnot_vo32_vo32(visinf_vo_vf(x) | visnan_vo_vf(x)); }

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)#[inline]
fn vilogbk_vi2_vf(d: $f32x) -> $ix2 {
  $mox o = d.lt($f32x::splat(5.421010862427522e-20));
  d = vsel_vf_vo_vf_vf(o, $f32x::splat(1.8446744073709552e19) * d, d);
  $ix2 q = vand_vi2_vi2_vi2(vsrl_vi2_vi2_i(vcast_vi2_vm(vreinterpret_vm_vf(d)), 23), $ix2::splat(0xff));
  q = vsub_vi2_vi2_vi2(q, vsel_vi2_vo_vi2_vi2(o, $ix2::splat(64 + 0x7f), $ix2::splat(0x7f)));
  return q;
}
#[inline]
fn vilogb2k_vi2_vf(d: $f32x) -> $ix2 {
  $ix2 q = vreinterpret_vi2_vf(d);
  q = vsrl_vi2_vi2_i(q, 23);
  q = vand_vi2_vi2_vi2(q, $ix2::splat(0xff));
  q = vsub_vi2_vi2_vi2(q, $ix2::splat(0x7f));
  return q;
}
#endif

//

pub fn xilogbf(d: $f32x) -> $ix2 {
  $ix2 e = vilogbk_vi2_vf(vabs_vf_vf(d));
  e = vsel_vi2_vo_vi2_vi2(d.eq($f32x::splat(0.)), $ix2::splat(SLEEF_FP_ILOGB0), e);
  e = vsel_vi2_vo_vi2_vi2(visnan_vo_vf(d), $ix2::splat(SLEEF_FP_ILOGBNAN), e);
  e = vsel_vi2_vo_vi2_vi2(visinf_vo_vf(d), $ix2::splat(INT_MAX), e);
  return e;
}
#[inline]
fn vpow2i_vf_vi2($ix2 q) -> $f32x {
  return vreinterpret_vf_vm(vcast_vm_vi2(vsll_vi2_vi2_i(vadd_vi2_vi2_vi2(q, $ix2::splat(0x7f)), 23)));
}
#[inline]
fn vldexp_vf_vf_vi2($f32x x, $ix2 q) -> $f32x {
  $f32x u;
  $ix2 m = vsra_vi2_vi2_i(q, 31);
  m = vsll_vi2_vi2_i(vsub_vi2_vi2_vi2(vsra_vi2_vi2_i(vadd_vi2_vi2_vi2(m, q), 6), m), 4);
  q = vsub_vi2_vi2_vi2(q, vsll_vi2_vi2_i(m, 2));
  m = vadd_vi2_vi2_vi2(m, $ix2::splat(0x7f));
  m = vand_vi2_vi2_vi2(vgt_vi2_vi2_vi2(m, $ix2::splat(0)), m);
  $ix2 n = vgt_vi2_vi2_vi2(m, $ix2::splat(0xff));
  m = vor_vi2_vi2_vi2(vandnot_vi2_vi2_vi2(n, m), vand_vi2_vi2_vi2(n, $ix2::splat(0xff)));
  u = vreinterpret_vf_vm(vcast_vm_vi2(vsll_vi2_vi2_i(m, 23)));
  x *= u*u*u*u;
  u = vreinterpret_vf_vm(vcast_vm_vi2(vsll_vi2_vi2_i(vadd_vi2_vi2_vi2(q, $ix2::splat(0x7f)), 23)));
  x * u
}
#[inline]
fn vldexp2_vf_vf_vi2($f32x d, $ix2 e) -> $f32x {
  d * vpow2i_vf_vi2(vsra_vi2_vi2_i(e, 1)) * vpow2i_vf_vi2(vsub_vi2_vi2_vi2(e, vsra_vi2_vi2_i(e, 1)))
}
#[inline]
fn vldexp3_vf_vf_vi2($f32x d, $ix2 q) -> $f32x {
  return vreinterpret_vf_vi2(vadd_vi2_vi2_vi2(vreinterpret_vi2_vf(d), vsll_vi2_vi2_i(q, 23)));
}

pub fn xldexpf($f32x x, $ix2 q) -> $f32x { return vldexp_vf_vf_vi2(x, q); }

#[inline]
fn rempisubf(x: $f32x) -> ($f32x, $ix2) {
 if cfg!(feature="full_fp_rounding") {
  $f32x y = vrint_vf_vf(x * $f32x::splat(4.));
  $ix2 vi = vtruncate_vi2_vf(y - vrint_vf_vf(x) * $f32x::splat(4.));
  ( x - y * $f32x::splat(0.25), vi )
 } else {
  $f32x fr = x - $f32x::splat(F1_10) * vtruncate_vf_vf(x * $f32x::splat(1. / F1_10));
  $ix2 vi = vadd_vi2_vi2_vi2(vsel_vi2_vo_vi2_vi2(x.gt($f32x::splat(0.)), $ix2::splat(4), $ix2::splat(3)), vtruncate_vi2_vf(fr * $f32x::splat(8.)));
  vi = vsra_vi2_vi2_i(vsub_vi2_vi2_vi2(vand_vi2_vi2_vi2($ix2::splat(7), vi), $ix2::splat(3)), 1);
  fr -= $f32x::splat(0.25) * vtruncate_vf_vf(fr.mla($f32x::splat(4.), vmulsign_vf_vf_vf($f32x::splat(0.5), x)));
  fr = vsel_vf_vo_vf_vf(vabs_vf_vf(fr).gt($f32x::splat(0.25)), fr - vmulsign_vf_vf_vf($f32x::splat(0.5), x), fr);
  fr = vsel_vf_vo_vf_vf(vabs_vf_vf(fr).gt($f32x::splat(1e+10)), $f32x::splat(0), fr);
  $mox o = vabs_vf_vf(x).eq($f32x::splat(0.12499999254941940308));
  fr = vsel_vf_vo_vf_vf(o, x, fr);
  vi = vsel_vi2_vo_vi2_vi2(o, $ix2::splat(0), vi);
  ( fr, vi )
  }
}
#[inline]
fn rempif(a: $f32x) -> (F2, $ix2) {
  F2 x, y, z;
  $ix2 ex = vilogb2k_vi2_vf(a);
  if cfg!("enable_avx512f") || cfg!("enable_avx512fnofma") {
    ex = vandnot_vi2_vi2_vi2(vsra_vi2_vi2_i(ex, 31), ex);
    ex = vand_vi2_vi2_vi2(ex, $ix2::splat(127));
  }
  ex = vsub_vi2_vi2_vi2(ex, $ix2::splat(25));
  $ix2 q = vand_vi2_vo_vi2(vgt_vo_vi2_vi2(ex, $ix2::splat(90-25)), $ix2::splat(-64));
  a = vldexp3_vf_vf_vi2(a, q);
  ex = vandnot_vi2_vi2_vi2(vsra_vi2_vi2_i(ex, 31), ex);
  ex = vsll_vi2_vi2_i(ex, 2);
  x = dfmul_vf2_vf_vf(a, vgather_vf_p_vi2(rempitabsp, ex));
  let (did, dii) = rempisubf(x.0);
  q = di.i;
  x.0 = di.d;
  x = dfnormalize_vf2_vf2(x);
  y = dfmul_vf2_vf_vf(a, vgather_vf_p_vi2(rempitabsp+1, ex));
  x = dfadd2_vf2_vf2_vf2(x, y);
  di = rempisubf(x.0);
  q = vadd_vi2_vi2_vi2(q, di.i);
  x.0 = di.d;
  x = dfnormalize_vf2_vf2(x);
  y = F2::new(vgather_vf_p_vi2(rempitabsp+2, ex), vgather_vf_p_vi2(rempitabsp+3, ex));
  y = dfmul_vf2_vf2_vf(y, a);
  x = dfadd2_vf2_vf2_vf2(x, y);
  x = dfnormalize_vf2_vf2(x);
  x = dfmul_vf2_vf2_vf2(x, F2::from((3.1415927410125732422*2., -8.7422776573475857731e-08*2.)));
  x = vsel_vf2_vo_vf2_vf2(vabs_vf_vf(a).lt($f32x::splat(0.7)), F2::new(a, $f32x::splat(0.)), x);
  ( x, q )
}

pub fn xsinf(d: $f32x) -> $f32x {
  $ix2 q;
  $f32x u, s, r = d;

  if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX2_F))))) {
    q = vrint_vi2_vf(d*$f32x::splat(M_1_PI_F));
    u = $f32x::from(q);
    d = u.mla($f32x::splat(-PI_A2_F), d);
    d = u.mla($f32x::splat(-PI_B2_F), d);
    d = u.mla($f32x::splat(-PI_C2_F), d);
  } else if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX_F))))) {
    q = vrint_vi2_vf(d*$f32x::splat(M_1_PI_F));
    u = $f32x::from(q);
    d = u.mla($f32x::splat(-PI_A_F), d);
    d = u.mla($f32x::splat(-PI_B_F), d);
    d = u.mla($f32x::splat(-PI_C_F), d);
    d = u.mla($f32x::splat(-PI_D_F), d);
  } else {
    let (dfidf, dfii) = rempif(d);
    q = vand_vi2_vi2_vi2(dfii, $ix2::splat(3));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vsel_vi2_vo_vi2_vi2(dfidf.0.gt($f32x::splat(0.)), $ix2::splat(2), $ix2::splat(1)));
    q = vsra_vi2_vi2_i(q, 2);
    $mox o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(dfii, $ix2::splat(1)), $ix2::splat(1));
    F2 x = F2::new(vmulsign_vf_vf_vf($f32x::splat(3.1415927410125732422*-0.5), dfidf.0), 
				vmulsign_vf_vf_vf($f32x::splat(-8.7422776573475857731e-08*-0.5), dfidf.0));
    x = dfadd2_vf2_vf2_vf2(dfidf, x);
    dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
    d = dfidf.0 + dfidf.1;

    d = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(r) | visnan_vo_vf(r), vreinterpret_vm_vf(d)));
  }

  s = d*d;

  d = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(1)), vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(d)));

  u = $f32x::splat(2.6083159809786593541503e-06)
      .mla(s, $f32x::splat(-0.0001981069071916863322258))
      .mla(s, $f32x::splat(0.00833307858556509017944336))
      .mla(s, $f32x::splat(-0.166666597127914428710938));

  u = s*(u*d) + d;

  u = vsel_vf_vo_vf_vf(visnegzero_vo_vf(r), r, u);

  return u;
}

pub fn xcosf(d: $f32x) -> $f32x {
  $ix2 q;
  $f32x u, s, r = d;

  if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX2_F))))) {
    q = vrint_vi2_vf(d*$f32x::splat(M_1_PI_F) - $f32x::splat(0.5));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), $ix2::splat(1));

    u = $f32x::from(q);
    d = u.mla($f32x::splat(-PI_A2_F*0.5), d);
    d = u.mla($f32x::splat(-PI_B2_F*0.5), d);
    d = u.mla($f32x::splat(-PI_C2_F*0.5), d);
  } else if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX_F))))) {
    q = vrint_vi2_vf(d*$f32x::splat(M_1_PI_F) - $f32x::splat(0.5));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), $ix2::splat(1));

    u = $f32x::from(q);
    d = u.mla($f32x::splat(-PI_A_F*0.5), d);
    d = u.mla($f32x::splat(-PI_B_F*0.5), d);
    d = u.mla($f32x::splat(-PI_C_F*0.5), d);
    d = u.mla($f32x::splat(-PI_D_F*0.5), d);
  } else {
    let (dfidf, dfii) = rempif(d);
    q = vand_vi2_vi2_vi2(dfii, $ix2::splat(3));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vsel_vi2_vo_vi2_vi2(dfidf.0.gt($f32x::splat(0.)), $ix2::splat(8), $ix2::splat(7)));
    q = vsra_vi2_vi2_i(q, 1);
    $mox o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(dfii, $ix2::splat(1)), $ix2::splat(0));
    $f32x y = vsel_vf_vo_vf_vf(dfidf.0.gt($f32x::splat(0.)), $f32x::splat(0.), $f32x::splat(-1.));
    F2 x = F2::new(vmulsign_vf_vf_vf($f32x::splat(3.1415927410125732422*-0.5), y),
				vmulsign_vf_vf_vf($f32x::splat(-8.7422776573475857731e-08*-0.5), y));
    x = dfadd2_vf2_vf2_vf2(dfidf, x);
    dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
    d = dfidf.0 + dfidf.1;

    d = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(r) | visnan_vo_vf(r), vreinterpret_vm_vf(d)));
  }

  s = d * d;

  d = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(0)), vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(d)));

  u = $f32x::splat(2.6083159809786593541503e-06)
      .mla(s, $f32x::splat(-0.0001981069071916863322258))
      .mla(s, $f32x::splat(0.00833307858556509017944336))
      .mla(s, $f32x::splat(-0.166666597127914428710938));

  u = s * (u * d) + d;

  return u;
}

pub fn xtanf(d: $f32x) -> $f32x {
  $ix2 q;
  $mox o;
  $f32x u, s, x;

  x = d;

  if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX2_F*0.5))))) {
    q = vrint_vi2_vf(d * $f32x::splat(2. * M_1_PI_F));
    u = $f32x::from(q);
    x = u.mla($f32x::splat(-PI_A2_F*0.5), x);
    x = u.mla($f32x::splat(-PI_B2_F*0.5), x);
    x = u.mla($f32x::splat(-PI_C2_F*0.5), x);
  } else if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX_F))))) {
    q = vrint_vi2_vf(d * (2. * $f32x::splat(M_1_PI_F)));
    u = $f32x::from(q);
    x = u.mla($f32x::splat(-PI_A_F*0.5), x);
    x = u.mla($f32x::splat(-PI_B_F*0.5), x);
    x = u.mla($f32x::splat(-PI_C_F*0.5), x);
    x = u.mla($f32x::splat(-PI_D_F*0.5), x);
  } else {
    let (dfidf, dfii) = rempif(d);
    q = dfii;
    x = dfidf.0 + dfidf.1;
    x = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(d) | visnan_vo_vf(d), vreinterpret_vm_vf(x)));
    x = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), d, x);
  }

  s = x * x;

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(1));
  x = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(x)));

  u = $f32x::splat(0.00927245803177356719970703)
      .mla(s, $f32x::splat(0.00331984995864331722259521))
      .mla(s, $f32x::splat(0.0242998078465461730957031))
      .mla(s, $f32x::splat(0.0534495301544666290283203))
      .mla(s, $f32x::splat(0.133383005857467651367188))
      .mla(s, $f32x::splat(0.333331853151321411132812));

  u = s.mla(u * x, x);

  u = vsel_vf_vo_vf_vf(o, vrec_vf_vf(u), u);

  return u;
}

pub fn xsinf_u1(d: $f32x) -> $f32x {
  $ix2 q;
  $f32x u, v;
  F2 s, t, x;

  if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX2_F))))) {
    u = vrint_vf_vf(d, $f32x::splat(M_1_PI_F));
    q = vrint_vi2_vf(u);
    v = u.mla($f32x::splat(-PI_A2_F), d);
    s = dfadd2_vf2_vf_vf(v, u * $f32x::splat(-PI_B2_F));
    s = dfadd_vf2_vf2_vf(s, u * $f32x::splat(-PI_C2_F));
  } else {
    let (dfidf, dfii) = rempif(d);
    q = vand_vi2_vi2_vi2(dfii, $ix2::splat(3));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vsel_vi2_vo_vi2_vi2(dfidf.0.gt($f32x::splat(0.)), $ix2::splat(2), $ix2::splat(1)));
    q = vsra_vi2_vi2_i(q, 2);
    $mox o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(dfii, $ix2::splat(1)), $ix2::splat(1));
    F2 x = F2::new(vmulsign_vf_vf_vf($f32x::splat(3.1415927410125732422*-0.5), dfidf.0), 
				vmulsign_vf_vf_vf($f32x::splat(-8.7422776573475857731e-08*-0.5), dfidf.0));
    x = dfadd2_vf2_vf2_vf2(dfidf, x);
    dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
    s = dfnormalize_vf2_vf2(dfidf);

    s.0 = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(d) | visnan_vo_vf(d), vreinterpret_vm_vf(s.0)));
  }

  t = s;
  s = dfsqu_vf2_vf2(s);

  u = $f32x::splat(2.6083159809786593541503e-06)
      .mla(s.0, $f32x::splat(-0.0001981069071916863322258))
      .mla(s.0, $f32x::splat(0.00833307858556509017944336));

  x = dfadd_vf2_vf_vf2($f32x::splat(1.), dfmul_vf2_vf2_vf2(dfadd_vf2_vf_vf($f32x::splat(-0.166666597127914428710938), u * s.0), s));

  u = dfmul_vf_vf2_vf2(t, x);

  u = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(1)), vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(u)));

  u = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), d, u);

  return u;
}

pub fn xcosf_u1(d: $f32x) -> $f32x {
  $ix2 q;
  $f32x u;
  F2 s, t, x;

  if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX2_F))))) {
    $f32x dq = vrint_vf_vf(d.mla($f32x::splat(M_1_PI_F), $f32x::splat(-0.5))).mla(
				 $f32x::splat(2.), $f32x::splat(1.));
    q = vrint_vi2_vf(dq);
    s = dfadd2_vf2_vf_vf (d, dq * $f32x::splat(-PI_A2_F*0.5));
    s = dfadd2_vf2_vf2_vf(s, dq * $f32x::splat(-PI_B2_F*0.5));
    s = dfadd2_vf2_vf2_vf(s, dq * $f32x::splat(-PI_C2_F*0.5));
  } else {
    let (dfidf, dfii) = rempif(d);
    q = vand_vi2_vi2_vi2(dfii, $ix2::splat(3));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vsel_vi2_vo_vi2_vi2(dfidf.0.gt($f32x::splat(0.)), $ix2::splat(8), $ix2::splat(7)));
    q = vsra_vi2_vi2_i(q, 1);
    $mox o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(dfii, $ix2::splat(1)), $ix2::splat(0));
    $f32x y = vsel_vf_vo_vf_vf(dfidf.0.gt($f32x::splat(0.)), $f32x::splat(0.), $f32x::splat(-1));
    F2 x = F2::new(vmulsign_vf_vf_vf($f32x::splat(3.1415927410125732422*-0.5), y),
				vmulsign_vf_vf_vf($f32x::splat(-8.7422776573475857731e-08*-0.5), y));
    x = dfadd2_vf2_vf2_vf2(dfidf, x);
    dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
    s = dfnormalize_vf2_vf2(dfidf);

    s.0 = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(d) | visnan_vo_vf(d), vreinterpret_vm_vf(s.0)));
  }

  t = s;
  s = dfsqu_vf2_vf2(s);

  u = $f32x::splat(2.6083159809786593541503e-06)
      .mla(s.0, $f32x::splat(-0.0001981069071916863322258))
      .mla(s.0, $f32x::splat(0.00833307858556509017944336));

  x = dfadd_vf2_vf_vf2($f32x::splat(1.), dfmul_vf2_vf2_vf2(dfadd_vf2_vf_vf($f32x::splat(-0.166666597127914428710938), u * s.0), s));

  u = dfmul_vf_vf2_vf2(t, x);

  u = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(0)), vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(u)));
  
  return u;
}


pub fn xsincosf(d: $f32x) -> ($f32x, $f32x) {
  $ix2 q;
  $mox o;
  $f32x u, s, t, rx, ry;
  F2 r;

  s = d;

  if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX2_F))))) {
    q = vrint_vi2_vf(d * $f32x::splat(M_2_PI_F));
    u = $f32x::from(q);
    s = u.mla($f32x::splat(-PI_A2_F*0.5), s);
    s = u.mla($f32x::splat(-PI_B2_F*0.5), s);
    s = u.mla($f32x::splat(-PI_C2_F*0.5), s);
  } else if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX_F))))) {
    q = vrint_vi2_vf(d * $f32x::splat(M_2_PI_F));
    u = $f32x::from(q);
    s = u.mla($f32x::splat(-PI_A_F*0.5), s);
    s = u.mla($f32x::splat(-PI_B_F*0.5), s);
    s = u.mla($f32x::splat(-PI_C_F*0.5), s);
    s = u.mla($f32x::splat(-PI_D_F*0.5), s);
  } else {
    let (dfidf, dfii) = rempif(d);
    q = dfii;
    s = dfidf.0 + dfidf.1;
    s = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(d) | visnan_vo_vf(d), vreinterpret_vm_vf(s)));
  }

  t = s;

  s = s * s;

  u = $f32x::splat(-0.000195169282960705459117889)
      .mla(s, $f32x::splat(0.00833215750753879547119141))
      .mla(s, $f32x::splat(-0.166666537523269653320312));

  rx = (u * s).mla(t, t);
  rx = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), $f32x::splat(-0.), rx);

  u = $f32x::splat(-2.71811842367242206819355e-07)
      .mla(s, $f32x::splat(2.47990446951007470488548e-05))
      .mla(s, $f32x::splat(-0.00138888787478208541870117))
      .mla(s, $f32x::splat(0.0416666641831398010253906))
      .mla(s, $f32x::splat(-0.5));

  ry = s.mla(u, $f32x::splat(1.));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(0));
  rsin = vsel_vf_vo_vf_vf(o, rx, ry);
  rcos = vsel_vf_vo_vf_vf(o, ry, rx);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(2));
  rsin = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(rsin)));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(2)), $ix2::splat(2));
  rcos = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(rcos)));

  (rsin, rcos)
}

pub fn xsincosf_u1(d: $f32x) -> ($f32x, $f32x) {
  $ix2 q;
  $mox o;
  $f32x u, v, rx, ry;
  F2 r, s, t, x;

  if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX2_F))))) {
    u = vrint_vf_vf(d * $f32x::splat(2. * M_1_PI_F));
    q = vrint_vi2_vf(u);
    v = u.mla($f32x::splat(-PI_A2_F*0.5), d);
    s = dfadd2_vf2_vf_vf(v, u * $f32x::splat(-PI_B2_F*0.5));
    s = dfadd_vf2_vf2_vf(s, u * $f32x::splat(-PI_C2_F*0.5));
  } else {
    let (dfidf, dfii) = rempif(d);
    q = dfii;
    s = dfidf;
    o = visinf_vo_vf(d) | visnan_vo_vf(d);
    s.0 = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(s.0)));
  }

  t = s;

  s.0 = dfsqu_vf_vf2(s);

  u = $f32x::splat(-0.000195169282960705459117889)
      .mla(s.0, $f32x::splat(0.00833215750753879547119141))
      .mla(s.0, $f32x::splat(-0.166666537523269653320312));

  u *= s.0 * t.0;

  x = dfadd_vf2_vf2_vf(t, u);
  rx = x.0 + x.1;

  rx = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), $f32x::splat(-0.), rx);

  u = $f32x::splat(-2.71811842367242206819355e-07)
      .mla(s.0, $f32x::splat(2.47990446951007470488548e-05))
      .mla(s.0, $f32x::splat(-0.00138888787478208541870117))
      .mla(s.0, $f32x::splat(0.0416666641831398010253906))
      .mla(s.0, $f32x::splat(-0.5));

  x = dfadd_vf2_vf_vf2($f32x::splat(1.), dfmul_vf2_vf_vf(s.0, u));
  ry = x.0 + x.1;

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(0));
  rsin = vsel_vf_vo_vf_vf(o, rx, ry);
  rcos = vsel_vf_vo_vf_vf(o, ry, rx);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(2));
  rsin = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(rsin)));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(2)), $ix2::splat(2));
  rcos = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(rcos)));

  (rsin, rcos)
}

pub fn xsincospif_u05(d: $f32x) -> ($f32x, $f32x) {
  $mox o;
  $f32x u, s, t, rx, ry;
  F2 r, x, s2;

  u = d * $f32x::splat(4.);
  $ix2 q = vtruncate_vi2_vf(u);
  q = vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vxor_vi2_vi2_vi2(vsrl_vi2_vi2_i(q, 31), $ix2::splat(1))), $ix2::splat(~1));
  s = u - $f32x::from(q);

  t = s;
  s = s * s;
  s2 = dfmul_vf2_vf_vf(t, t);
  
  //

  u = $f32x::splat(+0.3093842054e-6)
      .mla(s, $f32x::splat(-0.3657307388e-4))
      .mla(s, $f32x::splat(+0.2490393585e-2));
  x = dfadd2_vf2_vf_vf2(u * s, F2::from((-0.080745510756969451904, -1.3373665339076936258e-09)));
  x = dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(s2, x), F2::from((0.78539818525314331055, -2.1857338617566484855e-08)));

  x = dfmul_vf2_vf2_vf(x, t);
  rx = x.0 + x.1;

  rx = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), $f32x::splat(-0.), rx);
  
  //
  
  u = $f32x::splat(-0.2430611801e-7)
      .mla(s, $f32x::splat(+0.3590577080e-5))
      .mla(s, $f32x::splat(-0.3259917721e-3));
  x = dfadd2_vf2_vf_vf2(u * s, F2::from((0.015854343771934509277, 4.4940051354032242811e-10)));
  x = dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(s2, x), F2::from((-0.30842512845993041992, -9.0728339030733922277e-09)));

  x = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf2(x, s2), $f32x::splat(1.));
  ry = x.0 + x.1;

  //

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(0));
  rsin = vsel_vf_vo_vf_vf(o, rx, ry);
  rcos = vsel_vf_vo_vf_vf(o, ry, rx);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(4)), $ix2::splat(4));
  rsin = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(rsin)));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(4)), $ix2::splat(4));
  rcos = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(rcos)));

  o = vabs_vf_vf(d).gt($f32x::splat(1e+7));
  rsin = vreinterpret_vf_vm(vandnot_vm_vo32_vm(o, vreinterpret_vm_vf(rsin)));
  rcos = vreinterpret_vf_vm(vandnot_vm_vo32_vm(o, vreinterpret_vm_vf(rcos)));
  
  o = visinf_vo_vf(d);
  rsin = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(rsin)));
  rcos = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(rcos)));

  (rsin, rcos)
}

pub fn xsincospif_u35(d: $f32x) -> ($f32x, $f32x) {
  $mox o;
  $f32x u, s, t, rx, ry;
  F2 r;

  u = d * $f32x::splat(4.);
  $ix2 q = vtruncate_vi2_vf(u);
  q = vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vxor_vi2_vi2_vi2(vsrl_vi2_vi2_i(q, 31), $ix2::splat(1))), $ix2::splat(~1));
  s = u - $f32x::from(q);

  t = s;
  s = s * s;
  
  //

  u = $f32x::splat(-0.3600925265e-4)
      .mla(s, $f32x::splat(+0.2490088111e-2))
      .mla(s, $f32x::splat(-0.8074551076e-1))
      .mla(s, $f32x::splat(+0.7853981853e+0));

  rx = u * t;

  //
  
  u = $f32x::splat(+0.3539815225e-5)
      .mla(s, $f32x::splat(-0.3259574005e-3))
      .mla(s, $f32x::splat(+0.1585431583e-1))
      .mla(s, $f32x::splat(-0.3084251285e+0))
      .mla(s, $f32x::splat(1.));

  ry = u;

  //

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(0));
  r.0 = vsel_vf_vo_vf_vf(o, rx, ry);
  r.1 = vsel_vf_vo_vf_vf(o, ry, rx);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(4)), $ix2::splat(4));
  r.0 = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(r.0)));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(4)), $ix2::splat(4));
  r.1 = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(r.1)));

  o = vabs_vf_vf(d).gt($f32x::splat(1e+7));
  r.0 = vreinterpret_vf_vm(vandnot_vm_vo32_vm(o, vreinterpret_vm_vf(r.0)));
  r.1 = vreinterpret_vf_vm(vandnot_vm_vo32_vm(o, vreinterpret_vm_vf(r.1)));
  
  o = visinf_vo_vf(d);
  r.0 = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(r.0)));
  r.1 = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(r.1)));

  (rsin, rcos)
}

pub fn xmodff(x: $f32x) -> ($f32x, $f32x) {
  $f32x fr = x - $f32x::from(vtruncate_vi2_vf(x));
  fr = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(F1_23)), $f32x::splat(0.), fr);

  F2 ret;

  retx = vcopysign_vf_vf_vf(fr, x);
  rety = vcopysign_vf_vf_vf(x - fr, x);

  (retx, rety)
}


pub fn xtanf_u1(d: $f32x) -> $f32x {
  $ix2 q;
  $f32x u, v;
  F2 s, t, x;
  $mox o;

  if (LIKELY(vtestallones_i_vo32(vabs_vf_vf(d).lt($f32x::splat(TRIGRANGEMAX2_F))))) {
    u = vrint_vf_vf(d * $f32x::splat(2. * M_1_PI_F));
    q = vrint_vi2_vf(u);
    v = u.mla($f32x::splat(-PI_A2_F*0.5), d);
    s = dfadd2_vf2_vf_vf(v, u * $f32x::splat(-PI_B2_F*0.5));
    s = dfadd_vf2_vf2_vf(s, u * $f32x::splat(-PI_C2_F*0.5));
  } else {
    let (dfidf, dfii) = rempif(d);
    q = dfii;
    s = dfidf;
    o = visinf_vo_vf(d) | visnan_vo_vf(d);
    s.0 = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(s.0)));
    s.1 = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(s.1)));
  }

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(1));
  vmask n = vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.)));
  s.0 = vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(s.0), n));
  s.1 = vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(s.1), n));

  t = s;
  s = dfsqu_vf2_vf2(s);
  s = dfnormalize_vf2_vf2(s);

  u = $f32x::splat(0.00446636462584137916564941)
      .mla(s.0, $f32x::splat(-8.3920182078145444393158e-05))
      .mla(s.0, $f32x::splat(0.0109639242291450500488281))
      .mla(s.0, $f32x::splat(0.0212360303848981857299805))
      .mla(s.0, $f32x::splat(0.0540687143802642822265625));

  x = dfadd_vf2_vf_vf($f32x::splat(0.133325666189193725585938), u * s.0);
  x = dfadd_vf2_vf_vf2($f32x::splat(1.), dfmul_vf2_vf2_vf2(dfadd_vf2_vf_vf2($f32x::splat(0.33333361148834228515625), dfmul_vf2_vf2_vf2(s, x)), s));
  x = dfmul_vf2_vf2_vf2(t, x);

  x = vsel_vf2_vo_vf2_vf2(o, dfrec_vf2_vf2(x), x);

  u = x.0 + x.1;

  u = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), d, u);
  
  return u;
}

pub fn xatanf(d: $f32x) -> $f32x {
  $f32x s, t, u;
  $ix2 q;

  q = vsel_vi2_vf_vi2(d, $ix2::splat(2));
  s = vabs_vf_vf(d);

  q = vsel_vi2_vf_vf_vi2_vi2($f32x::splat(1.), s, vadd_vi2_vi2_vi2(q, $ix2::splat(1)), q);
  s = vsel_vf_vo_vf_vf($f32x::splat(1.).lt(s), vrec_vf_vf(s), s);

  t = s * s;

  u = $f32x::splat(0.00282363896258175373077393)
      .mla(t, $f32x::splat(-0.0159569028764963150024414))
      .mla(t, $f32x::splat(0.0425049886107444763183594))
      .mla(t, $f32x::splat(-0.0748900920152664184570312))
      .mla(t, $f32x::splat(0.106347933411598205566406))
      .mla(t, $f32x::splat(-0.142027363181114196777344))
      .mla(t, $f32x::splat(0.199926957488059997558594))
      .mla(t, $f32x::splat(-0.333331018686294555664062));

  t = s.mla(t * u, s);

  t = vsel_vf_vo_vf_vf(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(1)), $ix2::splat(1)), $f32x::splat(M_PI_F/2.) - t, t);

  t = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(2)), vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(t)));

#if defined(ENABLE_NEON32) || defined(ENABLE_NEON32VFPV4)
  t = vsel_vf_vo_vf_vf(visinf_vo_vf(d), vmulsign_vf_vf_vf($f32x::splat(1.5874010519681994747517056), d), t);
#endif

  return t;
}
#[inline]
fn atan2kf(y: $f32x, x: $f32x) -> $f32x {
  $f32x s, t, u;
  $ix2 q;
  $mox p;

  q = vsel_vi2_vf_vi2(x, $ix2::splat(-2));
  x = vabs_vf_vf(x);

  q = vsel_vi2_vf_vf_vi2_vi2(x, y, vadd_vi2_vi2_vi2(q, $ix2::splat(1)), q);
  p = x.lt(y);
  s = vsel_vf_vo_vf_vf(p, -x, y);
  t = x.max(y);

  s = s / t;
  t = s * s;

  u = $f32x::splat(0.00282363896258175373077393)
      .mla(t, $f32x::splat(-0.0159569028764963150024414))
      .mla(t, $f32x::splat(0.0425049886107444763183594))
      .mla(t, $f32x::splat(-0.0748900920152664184570312))
      .mla(t, $f32x::splat(0.106347933411598205566406))
      .mla(t, $f32x::splat(-0.142027363181114196777344))
      .mla(t, $f32x::splat(0.199926957488059997558594))
      .mla(t, $f32x::splat(-0.333331018686294555664062));

  t = s.mla(t * u, s);
  t = $f32x::from(q).mla($f32x::splat(M_PI_F/2.), t);

  return t;
}
#[inline]
fn visinf2_vf_vf_vf(d: $f32x, m: $f32x) -> $f32x {
  return vreinterpret_vf_vm(vand_vm_vo32_vm(visinf_vo_vf(d), vor_vm_vm_vm(vsignbit_vm_vf(d), vreinterpret_vm_vf(m))));
}

pub fn xatan2f(y: $f32x, x: $f32x) -> $f32x {
  $f32x r = atan2kf(vabs_vf_vf(y), x);

  r = vmulsign_vf_vf_vf(r, x);
  r = vsel_vf_vo_vf_vf(visinf_vo_vf(x) | x.eq($f32x::splat(0.)), $f32x::splat(M_PI_F/2.) - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::splat(M_PI_F/2.), x)), r);
  r = vsel_vf_vo_vf_vf(visinf_vo_vf(y), $f32x::splat(M_PI_F/2.) - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::splat(M_PI_F/4.), x)), r);

  r = vsel_vf_vo_vf_vf(y.eq($f32x::splat(0.)), vreinterpret_vf_vm(vand_vm_vo32_vm(vsignbit_vo_vf(x), vreinterpret_vm_vf($f32x::splat(M_PI_F)))), r);

  r = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x) | visnan_vo_vf(y), vreinterpret_vm_vf(vmulsign_vf_vf_vf(r, y))));
  return r;
}

pub fn xasinf(d: $f32x) -> $f32x {
  $mox o = vabs_vf_vf(d).lt($f32x::splat(0.5));
  $f32x x2 = vsel_vf_vo_vf_vf(o, d * d, ($f32x::splat(1.) - vabs_vf_vf(d)) * $f32x::splat(0.5));
  $f32x x = vsel_vf_vo_vf_vf(o, vabs_vf_vf(d), vsqrt_vf_vf(x2)), u;

  u = $f32x::splat(+0.4197454825e-1)
      .mla(x2, $f32x::splat(+0.2424046025e-1))
      .mla(x2, $f32x::splat(+0.4547423869e-1))
      .mla(x2, $f32x::splat(+0.7495029271e-1))
      .mla(x2, $f32x::splat(+0.1666677296e+0))
      .mla(x * x2, x);

  $f32x r = vsel_vf_vo_vf_vf(o, u, u.mla($f32x::splat(-2.), $f32x::splat(M_PI_F/2.)));
  return vmulsign_vf_vf_vf(r, d);
}

pub fn xacosf(d: $f32x) -> $f32x {
  $mox o = vabs_vf_vf(d).lt($f32x::splat(0.5));
  $f32x x2 = vsel_vf_vo_vf_vf(o, d * d,
				($f32x::splat(1.) - vabs_vf_vf(d)) * $f32x::splat(0.5)), u;
  $f32x x = vsel_vf_vo_vf_vf(o, vabs_vf_vf(d), vsqrt_vf_vf(x2));
  x = vsel_vf_vo_vf_vf(vabs_vf_vf(d).eq($f32x::splat(1.)), $f32x::splat(0.), x);

  u = $f32x::splat(+0.4197454825e-1)
      .mla(x2, $f32x::splat(+0.2424046025e-1))
      .mla(x2, $f32x::splat(+0.4547423869e-1))
      .mla(x2, $f32x::splat(+0.7495029271e-1))
      .mla(x2, $f32x::splat(+0.1666677296e+0));
  u *= x2 * x;

  $f32x y = $f32x::splat(3.1415926535897932/2.) - (vmulsign_vf_vf_vf(x, d) + vmulsign_vf_vf_vf(u, d));
  x = x + u;
  $f32x r = vsel_vf_vo_vf_vf(o, y, x * $f32x::splat(2.));
  return vsel_vf_vo_vf_vf(vandnot_vo_vo_vo(o, d.lt($f32x::splat(0.))),
			  dfadd_vf2_vf2_vf(F2::from((3.1415927410125732422,-8.7422776573475857731e-08)),
					   -r).0, r);
}

//
#[inline]
fn atan2kf_u1(F2 y, F2 x) -> F2 {
  $f32x u;
  F2 s, t;
  $ix2 q;
  $mox p;
  vmask r;
  
  q = vsel_vi2_vf_vf_vi2_vi2(x.0, $f32x::splat(0.), $ix2::splat(-2), $ix2::splat(0));
  p = x.0.lt($f32x::splat(0.));
  r = vand_vm_vo32_vm(p, vreinterpret_vm_vf($f32x::splat(-0.)));
  x.0 = vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x.0), r));
  x.1 = vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x.1), r));

  q = vsel_vi2_vf_vf_vi2_vi2(x.0, y.0, vadd_vi2_vi2_vi2(q, $ix2::splat(1)), q);
  p = x.0.lt(y.0);
  s = vsel_vf2_vo_vf2_vf2(p, dfneg_vf2_vf2(x), y);
  t = vsel_vf2_vo_vf2_vf2(p, y, x);

  s = dfdiv_vf2_vf2_vf2(s, t);
  t = dfsqu_vf2_vf2(s);
  t = dfnormalize_vf2_vf2(t);

  u = $f32x::splat(-0.00176397908944636583328247)
      .mla(t.0, $f32x::splat(0.0107900900766253471374512))
      .mla(t.0, $f32x::splat(-0.0309564601629972457885742))
      .mla(t.0, $f32x::splat(0.0577365085482597351074219))
      .mla(t.0, $f32x::splat(-0.0838950723409652709960938))
      .mla(t.0, $f32x::splat(0.109463557600975036621094))
      .mla(t.0, $f32x::splat(-0.142626821994781494140625))
      .mla(t.0, $f32x::splat(0.199983194470405578613281));

  t = dfmul_vf2_vf2_vf2(t, dfadd_vf2_vf_vf($f32x::splat(-0.333332866430282592773438), u * t.0));
  t = dfmul_vf2_vf2_vf2(s, dfadd_vf2_vf_vf2($f32x::splat(1.), t));
  t = dfadd_vf2_vf2_vf2(dfmul_vf2_vf2_vf(F2::from((1.5707963705062866211, -4.3711388286737928865e-08)), $f32x::from(q)), t);

  return t;
}

pub fn xatan2f_u1(y: $f32x, x: $f32x) -> $f32x {
  $mox o = vabs_vf_vf(x).lt($f32x::splat(2.9387372783541830947e-39)); // nexttowardf((1.0 / FLT_MAX), 1)
  x = vsel_vf_vo_vf_vf(o, x * $f32x::splat(F1_24), x);
  y = vsel_vf_vo_vf_vf(o, y * $f32x::splat(F1_24), y);
  
  F2 d = atan2kf_u1(F2::new(vabs_vf_vf(y), $f32x::splat(0.)), F2::new(x, $f32x::splat(0.)));
  $f32x r = d.0 + d.1;

  r = vmulsign_vf_vf_vf(r, x);
  r = vsel_vf_vo_vf_vf(visinf_vo_vf(x) | x.eq($f32x::splat(0.)), $f32x::splat(M_PI_F/2.) - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::splat(M_PI_F/2.), x)), r);
  r = vsel_vf_vo_vf_vf(visinf_vo_vf(y), $f32x::splat(M_PI_F/2.) - visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf($f32x::splat(M_PI_F/4.), x)), r);
  r = vsel_vf_vo_vf_vf(y.eq($f32x::splat(0.)), vreinterpret_vf_vm(vand_vm_vo32_vm(vsignbit_vo_vf(x), vreinterpret_vm_vf($f32x::splat(M_PI_F)))), r);

  r = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x) | visnan_vo_vf(y), vreinterpret_vm_vf(vmulsign_vf_vf_vf(r, y))));
  return r;
}

pub fn xasinf_u1(d: $f32x) -> $f32x {
  $mox o = vabs_vf_vf(d).lt($f32x::splat(0.5));
  $f32x x2 = vsel_vf_vo_vf_vf(o, d * d, ($f32x::splat(1.) - vabs_vf_vf(d)) * $f32x::splat(0.5)), u;
  F2 x = vsel_vf2_vo_vf2_vf2(o, F2::new(vabs_vf_vf(d), $f32x::splat(0.)), dfsqrt_vf2_vf(x2));
  x = vsel_vf2_vo_vf2_vf2(vabs_vf_vf(d).eq($f32x::splat(1.)), F2::from((0., 0.)), x);

  u = $f32x::splat(+0.4197454825e-1)
      .mla(x2, $f32x::splat(+0.2424046025e-1))
      .mla(x2, $f32x::splat(+0.4547423869e-1))
      .mla(x2, $f32x::splat(+0.7495029271e-1))
      .mla(x2, $f32x::splat(+0.1666677296e+0));
  u *= x2 * x.0;

  F2 y = dfsub_vf2_vf2_vf(dfsub_vf2_vf2_vf2(F2::from((3.1415927410125732422/4.,-8.7422776573475857731e-08/4.)), x), u);
  
  $f32x r = vsel_vf_vo_vf_vf(o, u + x.0,
			       (y.0 + y.1) * $f32x::splat(2.));
  return vmulsign_vf_vf_vf(r, d);
}

pub fn xacosf_u1(d: $f32x) -> $f32x {
  $mox o = vabs_vf_vf(d).lt($f32x::splat(0.5));
  $f32x x2 = vsel_vf_vo_vf_vf(o, d * d, ($f32x::splat(1.) - vabs_vf_vf(d)) * $f32x::splat(0.5));
   u;
  F2 x = vsel_vf2_vo_vf2_vf2(o, F2::new(vabs_vf_vf(d), $f32x::splat(0.)), dfsqrt_vf2_vf(x2));
  x = vsel_vf2_vo_vf2_vf2(vabs_vf_vf(d).eq($f32x::splat(1.)), F2::from((0., 0.)), x);

  u = $f32x::splat(+0.4197454825e-1)
      .mla(x2, $f32x::splat(+0.2424046025e-1))
      .mla(x2, $f32x::splat(+0.4547423869e-1))
      .mla(x2, $f32x::splat(+0.7495029271e-1))
      .mla(x2, $f32x::splat(+0.1666677296e+0));
  u *= x2 * x.0;

  F2 y = dfsub_vf2_vf2_vf2(F2::from((3.1415927410125732422/2., -8.7422776573475857731e-08/2.)),
				 dfadd_vf2_vf_vf(vmulsign_vf_vf_vf(x.0, d), vmulsign_vf_vf_vf(u, d)));
  x = dfadd_vf2_vf2_vf(x, u);

  y = vsel_vf2_vo_vf2_vf2(o, y, dfscale_vf2_vf2_vf(x, $f32x::splat(2.)));
  
  y = vsel_vf2_vo_vf2_vf2(vandnot_vo_vo_vo(o, d.lt($f32x::splat(0.))),
			  dfsub_vf2_vf2_vf2(F2::from((3.1415927410125732422, -8.7422776573475857731e-08)), y), y);

  y.0 + y.1
}

pub fn xatanf_u1(d: $f32x) -> $f32x {
  F2 d2 = atan2kf_u1(F2::new(vabs_vf_vf(d), $f32x::splat(0.)), F2::from((1., 0.)));
  $f32x r = d2.0 + d2.1;
  r = vsel_vf_vo_vf_vf(visinf_vo_vf(d), $f32x::splat(1.570796326794896557998982), r);
  return vmulsign_vf_vf_vf(r, d);
}

//

pub fn xlogf(d: $f32x) -> $f32x {
  $f32x x, x2, t, m;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mox o = d.lt($f32x::splat(f32::MIN));
  d = vsel_vf_vo_vf_vf(o, d * $f32x::splat(F1_32 * F1_32), d);
  $ix2 e = vilogb2k_vi2_vf(d * $f32x::splat(1./0.75));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, $ix2::splat(64)), e);
#else
  $f32x e = vgetexp_vf_vf(d * $f32x::splat(1./0.75));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), $f32x::splat(128.), e);
  m = vgetmant_vf_vf(d);
#endif
  
  x = ($f32x::splat(-1.) + m) / ($f32x::splat(1.) + m);
  x2 = x * x;

  t = $f32x::splat(0.2392828464508056640625)
      .mla(x2, $f32x::splat(0.28518211841583251953125))
      .mla(x2, $f32x::splat(0.400005877017974853515625))
      .mla(x2, $f32x::splat(0.666666686534881591796875))
      .mla(x2, $f32x::splat(2.));

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  x = x.mla(t, $f32x::splat(0.693147180559945286226764) * $f32x::from(e));
  x = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), $f32x::splat(SLEEF_INFINITY_F), x);
  x = vsel_vf_vo_vf_vf(d.lt($f32x::splat(0.)) | visnan_vo_vf(d), $f32x::splat(SLEEF_NAN_F), x);
  x = vsel_vf_vo_vf_vf(d.eq($f32x::splat(0.)), $f32x::splat(-SLEEF_INFINITY_F), x);
#else
  x = x.mla(t, $f32x::splat(0.693147180559945286226764) * e);
  x = vfixup_vf_vf_vf_vi2_i(x, d, $ix2::splat((5 << (5*4))), 0);
#endif
  
  return x;
}

pub fn xexpf(d: $f32x) -> $f32x {
  $ix2 q = vrint_vi2_vf(d * $f32x::splat(R_LN2_F));
  $f32x s, u;

  s = $f32x::from(q).mla($f32x::splat(-L2U_F), d);
  s = $f32x::from(q).mla($f32x::splat(-L2L_F), s);

  u = $f32x::splat(0.000198527617612853646278381)
      .mla(s, $f32x::splat(0.00139304355252534151077271))
      .mla(s, $f32x::splat(0.00833336077630519866943359))
      .mla(s, $f32x::splat(0.0416664853692054748535156))
      .mla(s, $f32x::splat(0.166666671633720397949219))
      .mla(s, $f32x::splat(0.5));

  u = $f32x::splat(1.) + (s * s).mla(u, s);

  u = vldexp2_vf_vf_vi2(u, q);

  u = vreinterpret_vf_vm(vandnot_vm_vo32_vm(d.lt($f32x::splat(-104.)), vreinterpret_vm_vf(u)));
  u = vsel_vf_vo_vf_vf($f32x::splat(100.).lt(d), $f32x::splat(SLEEF_INFINITY_F), u);

  return u;
}
#[inline]
fn expm1fk(d: $f32x) -> $f32x {
  $ix2 q = vrint_vi2_vf(d * $f32x::splat(R_LN2_F));
  $f32x s, u;

  s = $f32x::from(q).mla($f32x::splat(-L2U_F), d);
  s = $f32x::from(q).mla($f32x::splat(-L2L_F), s);

  u = $f32x::splat(0.000198527617612853646278381)
      .mla(s, $f32x::splat(0.00139304355252534151077271))
      .mla(s, $f32x::splat(0.00833336077630519866943359))
      .mla(s, $f32x::splat(0.0416664853692054748535156))
      .mla(s, $f32x::splat(0.166666671633720397949219))
      .mla(s, $f32x::splat(0.5));

  u = (s * s).mla(u, s);

  u = vsel_vf_vo_vf_vf(veq_vo_vi2_vi2(q, $ix2::splat(0)), u,
		       vldexp2_vf_vf_vi2(u + $f32x::splat(1.), q) - $f32x::splat(1.));

  return u;
}

#if defined(ENABLE_NEON32) || defined(ENABLE_NEON32VFPV4)
pub fn xsqrtf_u35(d: $f32x) -> $f32x {
  $f32x e = vreinterpret_vf_vi2(vadd_vi2_vi2_vi2($ix2::splat(0x20000000), vand_vi2_vi2_vi2($ix2::splat(0x7f000000), vsrl_vi2_vi2_i(vreinterpret_vi2_vf(d), 1))));
  $f32x m = vreinterpret_vf_vi2(vadd_vi2_vi2_vi2($ix2::splat(0x3f000000), vand_vi2_vi2_vi2($ix2::splat(0x01ffffff), vreinterpret_vi2_vf(d))));
  float32x4_t x = vrsqrteq_f32(m);
  x = vmulq_f32(x, vrsqrtsq_f32(m, vmulq_f32(x, x)));
  float32x4_t u = vmulq_f32(x, m);
  u = vmlaq_f32(u, vmlsq_f32(m, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
  e = vreinterpret_vf_vm(vandnot_vm_vo32_vm(d.eq($f32x::splat(0.)), vreinterpret_vm_vf(e)));
  u = e * u;

  u = vsel_vf_vo_vf_vf(visinf_vo_vf(d), $f32x::splat(SLEEF_INFINITY_F), u);
  u = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(d) | d.lt($f32x::splat(0.)), vreinterpret_vm_vf(u)));
  u = vmulsign_vf_vf_vf(u, d);

  return u;
}
#elif defined(ENABLE_VECEXT)
pub fn xsqrtf_u35(d: $f32x) -> $f32x {
  $f32x q = vsqrt_vf_vf(d);
  q = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), $f32x::splat(-0.), q);
  return vsel_vf_vo_vf_vf(vispinf_vo_vf(d), $f32x::splat(SLEEF_INFINITY_F), q);
}
#else
pub fn xsqrtf_u35(d: $f32x) -> $f32x { return vsqrt_vf_vf(d); }
#endif

pub fn xcbrtf(d: $f32x) -> $f32x {
  $f32x x, y, q = $f32x::splat(1.), t;
  $ix2 e, qu, re;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  $f32x s = d;
#endif
  e = vadd_vi2_vi2_vi2(vilogbk_vi2_vf(vabs_vf_vf(d)), $ix2::splat(1));
  d = vldexp2_vf_vf_vi2(d, vneg_vi2_vi2(e));

  t = $f32x::from(e) + $f32x::splat(6144.);
  qu = vtruncate_vi2_vf(t * $f32x::splat(1./3.));
  re = vtruncate_vi2_vf(t - $f32x::from(qu) * $f32x::splat(3.));

  q = vsel_vf_vo_vf_vf(veq_vo_vi2_vi2(re, $ix2::splat(1)), $f32x::splat(1.2599210498948731647672106), q);
  q = vsel_vf_vo_vf_vf(veq_vo_vi2_vi2(re, $ix2::splat(2)), $f32x::splat(1.5874010519681994747517056), q);
  q = vldexp2_vf_vf_vi2(q, vsub_vi2_vi2_vi2(qu, $ix2::splat(2048)));

  q = vmulsign_vf_vf_vf(q, d);
  d = vabs_vf_vf(d);

  x = $f32x::splat(-0.601564466953277587890625)
      .mla(d, $f32x::splat(2.8208892345428466796875))
      .mla(d, $f32x::splat(-5.532182216644287109375))
      .mla(d, $f32x::splat(5.898262500762939453125))
      .mla(d, $f32x::splat(-3.8095417022705078125))
      .mla(d, $f32x::splat(2.2241256237030029296875));

  y = d * x * x;
  y = (y - $f32x::splat(2. / 3.) * y * y.mla(x, $f32x::splat(-1.))) * q;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  y = vsel_vf_vo_vf_vf(visinf_vo_vf(s), vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), s), y);
  y = vsel_vf_vo_vf_vf(s.eq($f32x::splat(0.)), vmulsign_vf_vf_vf($f32x::splat(0.), s), y);
#endif
  
  return y;
}

pub fn xcbrtf_u1(d: $f32x) -> $f32x {
  $f32x x, y, z, t;
  F2 q2 = F2::from((1., 0.));
  , u, v;
  $ix2 e, qu, re;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  $f32x s = d;
#endif
  e = vadd_vi2_vi2_vi2(vilogbk_vi2_vf(vabs_vf_vf(d)), $ix2::splat(1));
  d = vldexp2_vf_vf_vi2(d, vneg_vi2_vi2(e));

  t = $f32x::from(e) + $f32x::splat(6144.);
  qu = vtruncate_vi2_vf(t * $f32x::splat(1./3.));
  re = vtruncate_vi2_vf(t - $f32x::from(qu) * $f32x::splat(3.));

  q2 = vsel_vf2_vo_vf2_vf2(veq_vo_vi2_vi2(re, $ix2::splat(1)), F2::from((1.2599210739135742188, -2.4018701694217270415e-08)), q2);
  q2 = vsel_vf2_vo_vf2_vf2(veq_vo_vi2_vi2(re, $ix2::splat(2)), F2::from((1.5874010324478149414,  1.9520385308169352356e-08)), q2);

  q2.0 = vmulsign_vf_vf_vf(q2.0, d); q2.1 = vmulsign_vf_vf_vf(q2.1, d);
  d = vabs_vf_vf(d);

  x = $f32x::splat(-0.601564466953277587890625)
      .mla(d, $f32x::splat(2.8208892345428466796875))
      .mla(d, $f32x::splat(-5.532182216644287109375))
      .mla(d, $f32x::splat(5.898262500762939453125))
      .mla(d, $f32x::splat(-3.8095417022705078125))
      .mla(d, $f32x::splat(2.2241256237030029296875));

  y = x * x;
  y = y * y;
  x -= vmlanp_vf_vf_vf_vf(d, y, x) * $f32x::splat(-1. / 3.);

  z = x;

  u = dfmul_vf2_vf_vf(x, x);
  u = dfmul_vf2_vf2_vf2(u, u);
  u = dfmul_vf2_vf2_vf(u, d);
  u = dfadd2_vf2_vf2_vf(u, -x);
  y = u.0 + u.1;

  y = $f32x::splat(-2. / 3.) * y * z;
  v = dfadd2_vf2_vf2_vf(dfmul_vf2_vf_vf(z, z), y);
  v = dfmul_vf2_vf2_vf(v, d);
  v = dfmul_vf2_vf2_vf2(v, q2);
  z = vldexp2_vf_vf_vi2(v.0 + v.1, vsub_vi2_vi2_vi2(qu, $ix2::splat(2048)));

  z = vsel_vf_vo_vf_vf(visinf_vo_vf(d), vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), q2.0), z);
  z = vsel_vf_vo_vf_vf(d.eq($f32x::splat(0.)), vreinterpret_vf_vm(vsignbit_vm_vf(q2.0)), z);

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  z = vsel_vf_vo_vf_vf(visinf_vo_vf(s), vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), s), z);
  z = vsel_vf_vo_vf_vf(s.eq($f32x::splat(0.)), vmulsign_vf_vf_vf($f32x::splat(0.), s), z);
#endif

  return z;
}
#[inline]
fn logkf(d: $f32x) -> F2 {
  F2 x, x2;
  $f32x t, m;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mox o = d.lt($f32x::splat(f32::MIN));
  d = vsel_vf_vo_vf_vf(o, d * $f32x::splat(F1_32 * F1_32), d);
  $ix2 e = vilogb2k_vi2_vf(d * $f32x::splat(1./0.75));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, $ix2::splat(64)), e);
#else
  $f32x e = vgetexp_vf_vf(d * $f32x::splat(1./0.75));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), $f32x::splat(128.), e);
  m = vgetmant_vf_vf(d);
#endif

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf($f32x::splat(-1.), m), dfadd2_vf2_vf_vf($f32x::splat(1.), m));
  x2 = dfsqu_vf2_vf2(x);

  t = $f32x::splat(0.240320354700088500976562)
      .mla(x2.0, $f32x::splat(0.285112679004669189453125))
      .mla(x2.0, $f32x::splat(0.400007992982864379882812));
  F2 c = F2::from((0.66666662693023681640625, 3.69183861259614332084311e-09));

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  F2 s = dfmul_vf2_vf2_vf(F2::from((0.69314718246459960938, -1.904654323148236017e-09)), $f32x::from(e));
#else
  F2 s = dfmul_vf2_vf2_vf(F2::from((0.69314718246459960938, -1.904654323148236017e-09)), e);
#endif

  s = dfadd_vf2_vf2_vf2(s, dfscale_vf2_vf2_vf(x, $f32x::splat(2.)));
  s = dfadd_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(x2, x),
					     dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf(x2, t), c)));
  return s;
}

pub fn xlogf_u1(d: $f32x) -> $f32x {
  F2 x;
  $f32x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mox o = d.lt($f32x::splat(f32::MIN));
  d = vsel_vf_vo_vf_vf(o, d * $f32x::splat(F1_32 * F1_32), d);
  $ix2 e = vilogb2k_vi2_vf(d * $f32x::splat(1./0.75));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, $ix2::splat(64)), e);
  F2 s = dfmul_vf2_vf2_vf(F2::from((0.69314718246459960938, -1.904654323148236017e-09)), $f32x::from(e));
#else
  $f32x e = vgetexp_vf_vf(d * $f32x::splat(1./0.75));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), $f32x::splat(128.), e);
  m = vgetmant_vf_vf(d);
  F2 s = dfmul_vf2_vf2_vf(F2::from((0.69314718246459960938, -1.904654323148236017e-09)), e);
#endif

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf($f32x::splat(-1.), m), dfadd2_vf2_vf_vf($f32x::splat(1.), m));
  x2 = x.0 * x.0;

  t = $f32x::splat(+0.3027294874e+0)
      .mla(x2, $f32x::splat(+0.3996108174e+0))
      .mla(x2, $f32x::splat(+0.6666694880e+0));
  
  s = dfadd_vf2_vf2_vf2(s, dfscale_vf2_vf2_vf(x, $f32x::splat(2.)));
  s = dfadd_vf2_vf2_vf(s, x2 * x.0 * t);

  $f32x r = s.0 + s.1;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), $f32x::splat(SLEEF_INFINITY_F), r);
  r = vsel_vf_vo_vf_vf(d.lt($f32x::splat(0.)) | visnan_vo_vf(d), $f32x::splat(SLEEF_NAN_F), r);
  r = vsel_vf_vo_vf_vf(d.eq($f32x::splat(0.)), $f32x::splat(-SLEEF_INFINITY_F), r);
#else
  r = vfixup_vf_vf_vf_vi2_i(r, d, $ix2::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}
#[inline]
fn expkf(F2 d) -> $f32x {
  $f32x u = (d.0 + d.1) * $f32x::splat(R_LN2_F);
  $ix2 q = vrint_vi2_vf(u);
  F2 s, t;

  s = dfadd2_vf2_vf2_vf(d, $f32x::from(q) * $f32x::splat(-L2U_F));
  s = dfadd2_vf2_vf2_vf(s, $f32x::from(q) * $f32x::splat(-L2L_F));

  s = dfnormalize_vf2_vf2(s);

  u = $f32x::splat(0.00136324646882712841033936)
      .mla(s.0, $f32x::splat(0.00836596917361021041870117))
      .mla(s.0, $f32x::splat(0.0416710823774337768554688))
      .mla(s.0, $f32x::splat(0.166665524244308471679688))
      .mla(s.0, $f32x::splat(0.499999850988388061523438));

  t = dfadd_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf(dfsqu_vf2_vf2(s), u));

  t = dfadd_vf2_vf_vf2($f32x::splat(1.), t);
  u = t.0 + t.1;
  u = vldexp_vf_vf_vi2(u, q);

  u = vreinterpret_vf_vm(vandnot_vm_vo32_vm(d.0.lt($f32x::splat(-104.)), vreinterpret_vm_vf(u)));
  
  return u;
}

pub fn xpowf(x: $f32x, y: $f32x) -> $f32x {
#if 1
  $mox yisint = vtruncate_vf_vf(y).eq(y) | vabs_vf_vf(y).gt($f32x::splat(F1_24));
  $mox yisodd = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vtruncate_vi2_vf(y), $ix2::splat(1)), $ix2::splat(1)) & yisint &
				 vabs_vf_vf(y).lt($f32x::splat(F1_24));

#if defined(ENABLE_NEON32) || defined(ENABLE_NEON32VFPV4)
  yisodd = vandnot_vm_vo32_vm(visinf_vo_vf(y), yisodd);
#endif

  $f32x result = expkf(dfmul_vf2_vf2_vf(logkf(vabs_vf_vf(x)), y));

  result = vsel_vf_vo_vf_vf(visnan_vo_vf(result), $f32x::splat(SLEEF_INFINITY_F), result);
  
  result *= vsel_vf_vo_vf_vf(x.gt($f32x::splat(0.)),
					  $f32x::splat(1.),
					  vsel_vf_vo_vf_vf(yisint, vsel_vf_vo_vf_vf(yisodd, $f32x::splat(-1.), $f32x::splat(1.)), $f32x::splat(SLEEF_NAN_F)));

  $f32x efx = vmulsign_vf_vf_vf(vabs_vf_vf(x) - $f32x::splat(1.), y);

  result = vsel_vf_vo_vf_vf(visinf_vo_vf(y),
			    vreinterpret_vf_vm(vandnot_vm_vo32_vm(efx.lt($f32x::splat(0.)),
								  vreinterpret_vm_vf(vsel_vf_vo_vf_vf(efx.eq($f32x::splat(0.)),
												      $f32x::splat(1.),
												      $f32x::splat(SLEEF_INFINITY_F))))),
			    result);

  result = vsel_vf_vo_vf_vf(visinf_vo_vf(x) | x.eq($f32x::splat(0.)),
			    vsel_vf_vo_vf_vf(yisodd, vsign_vf_vf(x), $f32x::splat(1.)) *
					  vreinterpret_vf_vm(vandnot_vm_vo32_vm(vsel_vf_vo_vf_vf(x.eq($f32x::splat(0.)), -y, y).lt($f32x::splat(0.)),
										vreinterpret_vm_vf($f32x::splat(SLEEF_INFINITY_F)))),
			    result);

  result = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x) | visnan_vo_vf(y), vreinterpret_vm_vf(result)));

  result = vsel_vf_vo_vf_vf(y.eq($f32x::splat(0.)) | x.eq($f32x::splat(1.)), $f32x::splat(1.), result);

  return result;
#else
  return expkf(dfmul_vf2_vf2_vf(logkf(x), y));
#endif
}
#[inline]
fn expk2f(F2 d) -> F2 {
  $f32x u = (d.0 + d.1) * $f32x::splat(R_LN2_F);
  $ix2 q = vrint_vi2_vf(u);
  F2 s, t;

  s = dfadd2_vf2_vf2_vf(d, $f32x::from(q) * $f32x::splat(-L2U_F));
  s = dfadd2_vf2_vf2_vf(s, $f32x::from(q) * $f32x::splat(-L2L_F));

  u = $f32x::splat(+0.1980960224e-3)
      .mla(s.0, $f32x::splat(+0.1394256484e-2))
      .mla(s.0, $f32x::splat(+0.8333456703e-2))
      .mla(s.0, $f32x::splat(+0.4166637361e-1));

  t = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(s, u), $f32x::splat(+0.166666659414234244790680580464e+0));
  t = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf2(s, t), $f32x::splat(0.5));
  t = dfadd2_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf2(dfsqu_vf2_vf2(s), t));

  t = dfadd_vf2_vf_vf2($f32x::splat(1.), t);

  t.0 = vldexp2_vf_vf_vi2(t.0, q);
  t.1 = vldexp2_vf_vf_vi2(t.1, q);

  t.0 = vreinterpret_vf_vm(vandnot_vm_vo32_vm(d.0.lt($f32x::splat(-104.)), vreinterpret_vm_vf(t.0)));
  t.1 = vreinterpret_vf_vm(vandnot_vm_vo32_vm(d.0.lt($f32x::splat(-104.)), vreinterpret_vm_vf(t.1)));

  return t;
}

pub fn xsinhf(x: $f32x) -> $f32x {
  $f32x y = vabs_vf_vf(x);
  F2 d = expk2f(F2::new(y, $f32x::splat(0.)));
  d = dfsub_vf2_vf2_vf2(d, dfrec_vf2_vf2(d));
  y = (d.0 + d.1) * $f32x::splat(0.5);

  y = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(89.)) |
				    visnan_vo_vf(y), $f32x::splat(SLEEF_INFINITY_F), y);
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xcoshf(x: $f32x) -> $f32x {
  $f32x y = vabs_vf_vf(x);
  F2 d = expk2f(F2::new(y, $f32x::splat(0.)));
  d = dfadd_vf2_vf2_vf2(d, dfrec_vf2_vf2(d));
  y = (d.0 + d.1) * $f32x::splat(0.5);

  y = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(89.)) |
				    visnan_vo_vf(y), $f32x::splat(SLEEF_INFINITY_F), y);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xtanhf(x: $f32x) -> $f32x {
  $f32x y = vabs_vf_vf(x);
  F2 d = expk2f(F2::new(y, $f32x::splat(0.)));
  F2 e = dfrec_vf2_vf2(d);
  d = dfdiv_vf2_vf2_vf2(dfadd_vf2_vf2_vf2(d, dfneg_vf2_vf2(e)), dfadd_vf2_vf2_vf2(d, e));
  y = d.0 + d.1;

  y = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(8.664339742)) |
				    visnan_vo_vf(y), $f32x::splat(1.), y);
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xsinhf_u35(x: $f32x) -> $f32x {
  $f32x e = expm1fk(vabs_vf_vf(x));
  $f32x y = (e + $f32x::splat(2.)) / (e + $f32x::splat(1.));
  y *= $f32x::splat(0.5) * e;

  y = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(88.)) |
				    visnan_vo_vf(y), $f32x::splat(SLEEF_INFINITY_F), y);
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xcoshf_u35(x: $f32x) -> $f32x {
  $f32x e = xexpf(vabs_vf_vf(x));
  $f32x y = $f32x::splat(0.5).mla(e, $f32x::splat(0.5) / e);

  y = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(88.)) |
				    visnan_vo_vf(y), $f32x::splat(SLEEF_INFINITY_F), y);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xtanhf_u35(x: $f32x) -> $f32x {
  $f32x d = expm1fk($f32x::splat(2.) * vabs_vf_vf(x));
  $f32x y = d / ($f32x::splat(2.) + d);

  y = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(8.664339742)) |
				    visnan_vo_vf(y), $f32x::splat(1.), y);
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}
#[inline]
fn logk2f(F2 d) -> F2 {
  F2 x, x2, m, s;
  $f32x t;
  $ix2 e;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  e = vilogbk_vi2_vf(d.0 * $f32x::splat(1./0.75));
#else
  e = vrint_vi2_vf(vgetexp_vf_vf(d.0 * $f32x::splat(1./0.75)));
#endif
  m = dfscale_vf2_vf2_vf(d, vpow2i_vf_vi2(vneg_vi2_vi2(e)));

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf2_vf(m, $f32x::splat(-1.)), dfadd2_vf2_vf2_vf(m, $f32x::splat(1.)));
  x2 = dfsqu_vf2_vf2(x);

  t = $f32x::splat(0.2392828464508056640625)
      .mla(x2.0, $f32x::splat(0.28518211841583251953125))
      .mla(x2.0, $f32x::splat(0.400005877017974853515625))
      .mla(x2.0, $f32x::splat(0.666666686534881591796875));

  s = dfmul_vf2_vf2_vf(F2::new($f32x::splat(0.69314718246459960938), $f32x::splat(-1.904654323148236017e-09)), $f32x::from(e));
  s = dfadd_vf2_vf2_vf2(s, dfscale_vf2_vf2_vf(x, $f32x::splat(2.)));
  s = dfadd_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf(dfmul_vf2_vf2_vf2(x2, x), t));

  return s;
}

pub fn xasinhf(x: $f32x) -> $f32x {
  $f32x y = vabs_vf_vf(x);
  $mox o = y.gt($f32x::splat(1.));
  F2 d;
  
  d = vsel_vf2_vo_vf2_vf2(o, dfrec_vf2_vf(x), F2::new(y, $f32x::splat(0.)));
  d = dfsqrt_vf2_vf2(dfadd2_vf2_vf2_vf(dfsqu_vf2_vf2(d), $f32x::splat(1.)));
  d = vsel_vf2_vo_vf2_vf2(o, dfmul_vf2_vf2_vf(d, y), d);

  d = logk2f(dfnormalize_vf2_vf2(dfadd2_vf2_vf2_vf(d, x)));
  y = d.0 + d.1;

  y = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(SQRT_FLT_MAX)) |
				    visnan_vo_vf(y),
		       vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), x), y);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));
  y = vsel_vf_vo_vf_vf(visnegzero_vo_vf(x), $f32x::splat(-0.), y);

  return y;
}

pub fn xacoshf(x: $f32x) -> $f32x {
  F2 d = logk2f(dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfsqrt_vf2_vf2(dfadd2_vf2_vf_vf(x, $f32x::splat(1.))), dfsqrt_vf2_vf2(dfadd2_vf2_vf_vf(x, $f32x::splat(-1.)))), x));
  $f32x y = d.0 + d.1;

  y = vsel_vf_vo_vf_vf(vabs_vf_vf(x).gt($f32x::splat(SQRT_FLT_MAX)) |
				    visnan_vo_vf(y),
		       $f32x::splat(SLEEF_INFINITY_F), y);

  y = vreinterpret_vf_vm(vandnot_vm_vo32_vm(x.eq($f32x::splat(1.)), vreinterpret_vm_vf(y)));

  y = vreinterpret_vf_vm(vor_vm_vo32_vm(x.lt($f32x::splat(1.)), vreinterpret_vm_vf(y)));
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xatanhf(x: $f32x) -> $f32x {
  $f32x y = vabs_vf_vf(x);
  F2 d = logk2f(dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf($f32x::splat(1.), y), dfadd2_vf2_vf_vf($f32x::splat(1.), -y)));
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(y.gt($f32x::splat(1.)), vreinterpret_vm_vf(vsel_vf_vo_vf_vf(y.eq($f32x::splat(1.)), $f32x::splat(SLEEF_INFINITY_F), (d.0 + d.1) * $f32x::splat(0.5)))));

  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(x) | visnan_vo_vf(y), vreinterpret_vm_vf(y)));
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xexp2f(d: $f32x) -> $f32x {
  $f32x u = vrint_vf_vf(d), s;
  $ix2 q = vrint_vi2_vf(u);

  s = d - u;

  u = $f32x::splat(+0.1535920892e-3)
      .mla(s, $f32x::splat(+0.1339262701e-2))
      .mla(s, $f32x::splat(+0.9618384764e-2))
      .mla(s, $f32x::splat(+0.5550347269e-1))
      .mla(s, $f32x::splat(+0.2402264476e+0))
      .mla(s, $f32x::splat(+0.6931471825e+0));

#ifdef ENABLE_FMA_SP
  u = vfma_vf_vf_vf_vf(u, s, $f32x::splat(1.));
#else
  u = dfnormalize_vf2_vf2(dfadd_vf2_vf_vf2($f32x::splat(1.), dfmul_vf2_vf_vf(u, s))).0;
#endif
  
  u = vldexp2_vf_vf_vi2(u, q);

  u = vsel_vf_vo_vf_vf(d.ge($f32x::splat(128.)), $f32x::splat(SLEEF_INFINITY), u);
  u = vreinterpret_vf_vm(vandnot_vm_vo32_vm(d.lt($f32x::splat(-150.)), vreinterpret_vm_vf(u)));

  return u;
}

pub fn xexp10f(d: $f32x) -> $f32x {
  $f32x u = vrint_vf_vf(d * $f32x::splat(LOG10_2)), s;
  $ix2 q = vrint_vi2_vf(u);

  s = u.mla($f32x::splat(-L10U_F), d);
  s = u.mla($f32x::splat(-L10L_F), s);

  u = $f32x::splat(+0.2064004987e+0)
      .mla(s, $f32x::splat(+0.5417877436e+0))
      .mla(s, $f32x::splat(+0.1171286821e+1))
      .mla(s, $f32x::splat(+0.2034656048e+1))
      .mla(s, $f32x::splat(+0.2650948763e+1))
      .mla(s, $f32x::splat(+0.2302585125e+1));

#ifdef ENABLE_FMA_SP
  u = vfma_vf_vf_vf_vf(u, s, $f32x::splat(1.));
#else
  u = dfnormalize_vf2_vf2(dfadd_vf2_vf_vf2($f32x::splat(1.), dfmul_vf2_vf_vf(u, s))).0;
#endif
  
  u = vldexp2_vf_vf_vi2(u, q);

  u = vsel_vf_vo_vf_vf(d.gt($f32x::splat(38.5318394191036238941387)), $f32x::splat(SLEEF_INFINITY_F), u);
  u = vreinterpret_vf_vm(vandnot_vm_vo32_vm(d.lt($f32x::splat(-50.)), vreinterpret_vm_vf(u)));

  return u;
}

pub fn xexpm1f(a: $f32x) -> $f32x {
  F2 d = dfadd2_vf2_vf2_vf(expk2f(F2::new(a, $f32x::splat(0.))), $f32x::splat(-1.));
  $f32x x = d.0 + d.1;
  x = vsel_vf_vo_vf_vf(a.gt($f32x::splat(88.72283172607421875)), $f32x::splat(SLEEF_INFINITY_F), x);
  x = vsel_vf_vo_vf_vf(a.lt($f32x::splat(-16.635532333438687426013570)), $f32x::splat(-1.), x);
  x = vsel_vf_vo_vf_vf(visnegzero_vo_vf(a), $f32x::splat(-0.), x);
  return x;
}

pub fn xlog10f(d: $f32x) -> $f32x {
  F2 x;
  $f32x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mox o = d.lt($f32x::splat(f32::MIN));
  d = vsel_vf_vo_vf_vf(o, d * $f32x::splat(F1_32 * F1_32), d);
  $ix2 e = vilogb2k_vi2_vf(d * $f32x::splat(1./0.75));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, $ix2::splat(64)), e);
#else
  $f32x e = vgetexp_vf_vf(d * $f32x::splat(1./0.75));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), $f32x::splat(128.), e);
  m = vgetmant_vf_vf(d);
#endif

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf($f32x::splat(-1.), m), dfadd2_vf2_vf_vf($f32x::splat(1.), m));
  x2 = x.0 * x.0;

  t = $f32x::splat(0.1314289868e+0)
      .mla(x2, $f32x::splat( 0.1735493541e+0))
      .mla(x2, $f32x::splat( 0.2895309627e+0));
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  F2 s = dfmul_vf2_vf2_vf(F2::from((0.30103001, -1.432098889e-08)), $f32x::from(e));
#else
  F2 s = dfmul_vf2_vf2_vf(F2::from((0.30103001, -1.432098889e-08)), e);
#endif

  s = dfadd_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf2(x, F2::from((0.868588984, -2.170757285e-08))));
  s = dfadd_vf2_vf2_vf(s, x2 * x.0 * t);

  $f32x r = s.0 + s.1;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), $f32x::splat(SLEEF_INFINITY), r);
  r = vsel_vf_vo_vf_vf(d.lt($f32x::splat(0.)) | visnan_vo_vf(d), $f32x::splat(SLEEF_NAN), r);
  r = vsel_vf_vo_vf_vf(d.eq($f32x::splat(0.)), $f32x::splat(-SLEEF_INFINITY), r);
#else
  r = vfixup_vf_vf_vf_vi2_i(r, d, $ix2::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}

pub fn xlog2f(d: $f32x) -> $f32x {
  F2 x;
  $f32x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mox o = d.lt($f32x::splat(f32::MIN));
  d = vsel_vf_vo_vf_vf(o, d * $f32x::splat(F1_32 * F1_32), d);
  $ix2 e = vilogb2k_vi2_vf(d * $f32x::splat(1./0.75));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, $ix2::splat(64)), e);
#else
  $f32x e = vgetexp_vf_vf(d * $f32x::splat(1./0.75));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), $f32x::splat(128.), e);
  m = vgetmant_vf_vf(d);
#endif

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf($f32x::splat(-1.), m), dfadd2_vf2_vf_vf($f32x::splat(1.), m));
  x2 = x.0 * x.0;

  t = $f32x::splat(+0.4374550283e+0)
      .mla(x2, $f32x::splat(+0.5764790177e+0))
      .mla(x2, $f32x::splat(+0.9618012905120));
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  F2 s = dfadd2_vf2_vf_vf2($f32x::from(e),
				dfmul_vf2_vf2_vf2(x, F2::from((2.8853900432586669922, 3.2734474483568488616e-08))));
#else
  F2 s = dfadd2_vf2_vf_vf2(e,
				dfmul_vf2_vf2_vf2(x, F2::from((2.8853900432586669922, 3.2734474483568488616e-08))));
#endif

  s = dfadd2_vf2_vf2_vf(s, x2 * x.0 * t);

  $f32x r = s.0 + s.1;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), $f32x::splat(SLEEF_INFINITY), r);
  r = vsel_vf_vo_vf_vf(d.lt($f32x::splat(0.)) | visnan_vo_vf(d), $f32x::splat(SLEEF_NAN), r);
  r = vsel_vf_vo_vf_vf(d.eq($f32x::splat(0.)), $f32x::splat(-SLEEF_INFINITY), r);
#else
  r = vfixup_vf_vf_vf_vi2_i(r, d, $ix2::splat((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}

pub fn xlog1pf(d: $f32x) -> $f32x {
  F2 x;
  $f32x t, m, x2;

  $f32x dp1 = d + $f32x::splat(1.);

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $mox o = dp1.lt($f32x::splat(f32::MIN));
  dp1 = vsel_vf_vo_vf_vf(o, dp1 * $f32x::splat(F1_32 * F1_32), dp1);
  $ix2 e = vilogb2k_vi2_vf(dp1 * $f32x::splat(1./0.75));
  t = vldexp3_vf_vf_vi2($f32x::splat(1.), vneg_vi2_vi2(e));
  m = d.mla(t, t - $f32x::splat(1.));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, $ix2::splat(64)), e);
  F2 s = dfmul_vf2_vf2_vf(F2::from((0.69314718246459960938, -1.904654323148236017e-09)), $f32x::from(e));
#else
  $f32x e = vgetexp_vf_vf(dp1, $f32x::splat(1./0.75));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), $f32x::splat(128.), e);
  t = vldexp3_vf_vf_vi2($f32x::splat(1.), vneg_vi2_vi2(vrint_vi2_vf(e)));
  m = d.mla(t, t - $f32x::splat(1.));
  F2 s = dfmul_vf2_vf2_vf(F2::from((0.69314718246459960938, -1.904654323148236017e-09)), e);
#endif

  x = dfdiv_vf2_vf2_vf2(F2::new(m, $f32x::splat(0.)), dfadd_vf2_vf_vf($f32x::splat(2.), m));
  x2 = x.0 * x.0;

  t = $f32x::splat(+0.3027294874e+0)
      .mla(x2, $f32x::splat(+0.3996108174e+0))
      .mla(x2, $f32x::splat(+0.6666694880e+0));
  
  s = dfadd_vf2_vf2_vf2(s, dfscale_vf2_vf2_vf(x, $f32x::splat(2.)));
  s = dfadd_vf2_vf2_vf(s, x2 * x.0 * t);

  $f32x r = s.0 + s.1;
  
  r = vsel_vf_vo_vf_vf(d.gt($f32x::splat(1e+38)), $f32x::splat(SLEEF_INFINITY_F), r);
  r = vreinterpret_vf_vm(vor_vm_vo32_vm($f32x::splat(-1.).gt(d), vreinterpret_vm_vf(r)));
  r = vsel_vf_vo_vf_vf(d.eq($f32x::splat(-1.)), $f32x::splat(-SLEEF_INFINITY_F), r);
  r = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), $f32x::splat(-0.), r);

  return r;
}

//

pub fn xfabsf(x: $f32x) -> $f32x { return vabs_vf_vf(x); }

pub fn xcopysignf(x: $f32x, y: $f32x) -> $f32x { return vcopysign_vf_vf_vf(x, y); }

pub fn xfmaxf(x: $f32x, y: $f32x) -> $f32x {
#if (defined(__x86_64__) || defined(__i386__)) && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
  return vsel_vf_vo_vf_vf(visnan_vo_vf(y), x, x.max(y));
#else
  return vsel_vf_vo_vf_vf(visnan_vo_vf(y), x, vsel_vf_vo_vf_vf(x.gt(y), x, y));
#endif
}

pub fn xfminf(x: $f32x, y: $f32x) -> $f32x {
#if (defined(__x86_64__) || defined(__i386__)) && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
  return vsel_vf_vo_vf_vf(visnan_vo_vf(y), x, x.min(y));
#else
  return vsel_vf_vo_vf_vf(visnan_vo_vf(y), x, vsel_vf_vo_vf_vf(y.gt(x), x, y));
#endif
}

pub fn xfdimf(x: $f32x, y: $f32x) -> $f32x {
  $f32x ret = x - y;
  ret = vsel_vf_vo_vf_vf(ret.lt($f32x::splat(0.)) | x.eq(y), $f32x::splat(0.), ret);
  return ret;
}

pub fn xtruncf(x: $f32x) -> $f32x {
  $f32x fr = x - $f32x::from(vtruncate_vi2_vf(x));
  return vsel_vf_vo_vf_vf(visinf_vo_vf(x) | vabs_vf_vf(x).ge($f32x::splat(F1_23)), x, vcopysign_vf_vf_vf(x - fr, x));
}

pub fn xfloorf(x: $f32x) -> $f32x {
  $f32x fr = x - $f32x::from(vtruncate_vi2_vf(x));
  fr = vsel_vf_vo_vf_vf(fr.lt($f32x::splat(0.)), fr + $f32x::splat(1.), fr);
  return vsel_vf_vo_vf_vf(visinf_vo_vf(x) | vabs_vf_vf(x).ge($f32x::splat(F1_23)), x, vcopysign_vf_vf_vf(x - fr, x));
}

pub fn xceilf(x: $f32x) -> $f32x {
  $f32x fr = x - $f32x::from(vtruncate_vi2_vf(x));
  fr = vsel_vf_vo_vf_vf(fr.le($f32x::splat(0.)), fr, fr - $f32x::splat(1.));
  return vsel_vf_vo_vf_vf(visinf_vo_vf(x) | vabs_vf_vf(x).ge($f32x::splat(F1_23)), x, vcopysign_vf_vf_vf(x - fr, x));
}

pub fn xroundf(d: $f32x) -> $f32x {
  $f32x x = d + $f32x::splat(0.5);
  $f32x fr = x - $f32x::from(vtruncate_vi2_vf(x));
  x = vsel_vf_vo_vf_vf(x.le($f32x::splat(0.)) & fr.eq($f32x::splat(0.)), x - $f32x::splat(1.), x);
  fr = vsel_vf_vo_vf_vf(fr.lt($f32x::splat(0.)), fr + $f32x::splat(1.), fr);
  x = vsel_vf_vo_vf_vf(d.eq($f32x::splat(0.4999999701976776123)), $f32x::splat(0.), x);
  return vsel_vf_vo_vf_vf(visinf_vo_vf(d) | vabs_vf_vf(d).ge($f32x::splat(F1_23)), d, vcopysign_vf_vf_vf(x - fr, d));
}

pub fn xrintf(d: $f32x) -> $f32x {
  $f32x x = d + $f32x::splat(0.5);
  $mox isodd = veq_vo_vi2_vi2(vand_vi2_vi2_vi2($ix2::splat(1), vtruncate_vi2_vf(x)), $ix2::splat(1));
  $f32x fr = x - $f32x::from(vtruncate_vi2_vf(x));
  fr = vsel_vf_vo_vf_vf(fr.lt($f32x::splat(0.)) | (fr.eq($f32x::splat(0.)) & isodd), fr + $f32x::splat(1.), fr);
  x = vsel_vf_vo_vf_vf(d.eq($f32x::splat(0.50000005960464477539)), $f32x::splat(0.), x);
  $f32x ret = vsel_vf_vo_vf_vf(visinf_vo_vf(d) | vabs_vf_vf(d).ge($f32x::splat(F1_23)), d, vcopysign_vf_vf_vf(x - fr, d));
  return ret;
}

pub fn xfmaf($f32x x, $f32x y, $f32x z) -> $f32x {
  $f32x h2 = x * y + z;
   q = $f32x::splat(1.);
  $mox o = vabs_vf_vf(h2).lt($f32x::splat(1e-38));
  {
    const float c0 = 1ULL << 25, c1 = c0 * c0, c2 = c1 * c1;
    x = vsel_vf_vo_vf_vf(o, x * $f32x::splat(c1), x);
    y = vsel_vf_vo_vf_vf(o, y * $f32x::splat(c1), y);
    z = vsel_vf_vo_vf_vf(o, z * $f32x::splat(c2), z);
    q = vsel_vf_vo_vf_vf(o, $f32x::splat(1. / c2), q);
  }
  o = vabs_vf_vf(h2).gt($f32x::splat(1e+38));
  {
    const float c0 = 1ULL << 25, c1 = c0 * c0, c2 = c1 * c1;
    x = vsel_vf_vo_vf_vf(o, x * $f32x::splat(1. / c1), x);
    y = vsel_vf_vo_vf_vf(o, y * $f32x::splat(1. / c1), y);
    z = vsel_vf_vo_vf_vf(o, z * $f32x::splat(1. / c2), z);
    q = vsel_vf_vo_vf_vf(o, $f32x::splat(c2), q);
  }
  F2 d = dfmul_vf2_vf_vf(x, y);
  d = dfadd2_vf2_vf2_vf(d, z);
  $f32x ret = vsel_vf_vo_vf_vf(x.eq($f32x::splat(0.)) | y.eq($f32x::splat(0.)), z, d.0 + d.1);
  o = visinf_vo_vf(z);
  o = vandnot_vo_vo_vo(visinf_vo_vf(x), o);
  o = vandnot_vo_vo_vo(visnan_vo_vf(x), o);
  o = vandnot_vo_vo_vo(visinf_vo_vf(y), o);
  o = vandnot_vo_vo_vo(visnan_vo_vf(y), o);
  h2 = vsel_vf_vo_vf_vf(o, z, h2);

  o = visinf_vo_vf(h2) | visnan_vo_vf(h2);
  
  return vsel_vf_vo_vf_vf(o, h2, ret * q);
}
#[inline]
fn vcast_vi2_i_i(int i0, int i1) -> $ix2 { return vcast_vi2_vm(vcast_vm_i_i(i0, i1)); }

SQRTFU05_FUNCATR $f32x xsqrtf_u05(d: $f32x) {
  $f32x q;
  $mox o;
  
  d = vsel_vf_vo_vf_vf(d.lt($f32x::splat(0.)), $f32x::splat(SLEEF_NAN_F), d);

  o = d.lt($f32x::splat(5.2939559203393770e-23));
  d = vsel_vf_vo_vf_vf(o, d * $f32x::splat(1.8889465931478580e+22), d);
  q = vsel_vf_vo_vf_vf(o, $f32x::splat(7.2759576141834260e-12*0.5), $f32x::splat(0.5));

  o = d.gt($f32x::splat(1.8446744073709552e+19));
  d = vsel_vf_vo_vf_vf(o, d * $f32x::splat(5.4210108624275220e-20), d);
  q = vsel_vf_vo_vf_vf(o, $f32x::splat(4294967296.0 * 0.5), q);

  $f32x x = vreinterpret_vf_vi2(vsub_vi2_vi2_vi2($ix2::splat(0x5f375a86), vsrl_vi2_vi2_i(vreinterpret_vi2_vf(d + $f32x::splat(1e-45)), 1)));

  x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
  x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
  x *= $f32x::splat(1.5) - $f32x::splat(0.5) * d * x * x;
  x *= d;

  F2 d2 = dfmul_vf2_vf2_vf2(dfadd2_vf2_vf_vf2(d, dfmul_vf2_vf_vf(x, x)), dfrec_vf2_vf(x));

  x = (d2.0 + d2.1) * q;

  x = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), $f32x::splat(SLEEF_INFINITY_F), x);
  x = vsel_vf_vo_vf_vf(d.eq($f32x::splat(0.)), d, x);
  
  return x;
}

pub fn xsqrtf(d: $f32x) -> $f32x {
#ifdef ACCURATE_SQRT
  return vsqrt_vf_vf(d);
#endif
  // fall back to approximation if ACCURATE_SQRT is undefined
  return xsqrtf_u05(d);
}

pub fn xhypotf_u05(x: $f32x, y: $f32x) -> $f32x {
  x = vabs_vf_vf(x);
  y = vabs_vf_vf(y);
  $f32x min = x.min(y);
  n = min;
  $f32x max = x.max(y);
  d = max;

  $mox o = max.lt($f32x::splat(f32::MIN));
  n = vsel_vf_vo_vf_vf(o, n * $f32x::splat(F1_24), n);
  d = vsel_vf_vo_vf_vf(o, d * $f32x::splat(F1_24), d);

  F2 t = dfdiv_vf2_vf2_vf2(F2::new(n, $f32x::splat(0.)), F2::new(d, $f32x::splat(0.)));
  t = dfmul_vf2_vf2_vf(dfsqrt_vf2_vf2(dfadd2_vf2_vf2_vf(dfsqu_vf2_vf2(t), $f32x::splat(1.))), max);
  $f32x ret = t.0 + t.1;
  ret = vsel_vf_vo_vf_vf(visnan_vo_vf(ret), $f32x::splat(SLEEF_INFINITY_F), ret);
  ret = vsel_vf_vo_vf_vf(min.eq($f32x::splat(0.)), max, ret);
  ret = vsel_vf_vo_vf_vf(visnan_vo_vf(x) | visnan_vo_vf(y), $f32x::splat(SLEEF_NAN_F), ret);
  ret = vsel_vf_vo_vf_vf(x.eq($f32x::splat(SLEEF_INFINITY_F)) | y.eq($f32x::splat(SLEEF_INFINITY_F)), $f32x::splat(SLEEF_INFINITY_F), ret);

  return ret;
}

pub fn xhypotf_u35(x: $f32x, y: $f32x) -> $f32x {
  x = vabs_vf_vf(x);
  y = vabs_vf_vf(y);
  $f32x min = x.min(y);
  n = min;
  $f32x max = x.max(y);
  d = max;

  $f32x t = min / max;
  $f32x ret = max * vsqrt_vf_vf(t.mla(t, $f32x::splat(1.)));
  ret = vsel_vf_vo_vf_vf(min.eq($f32x::splat(0.)), max, ret);
  ret = vsel_vf_vo_vf_vf(visnan_vo_vf(x) | visnan_vo_vf(y), $f32x::splat(SLEEF_NAN_F), ret);
  ret = vsel_vf_vo_vf_vf(x.eq($f32x::splat(SLEEF_INFINITY_F)) | y.eq($f32x::splat(SLEEF_INFINITY_F)), $f32x::splat(SLEEF_INFINITY_F), ret);

  return ret;
}

pub fn xnextafterf(x: $f32x, y: $f32x) -> $f32x {
  x = vsel_vf_vo_vf_vf(x.eq($f32x::splat(0.)), vmulsign_vf_vf_vf($f32x::splat(0.), y), x);
  $ix2 t, xi2 = vreinterpret_vi2_vf(x);
  $mox c = vsignbit_vo_vf(x) ^ y.ge(x);

  xi2 = vsel_vi2_vo_vi2_vi2(c, vsub_vi2_vi2_vi2($ix2::splat(0), vxor_vi2_vi2_vi2(xi2, $ix2::splat(1 << 31))), xi2);

  xi2 = vsel_vi2_vo_vi2_vi2(x.ne(y), vsub_vi2_vi2_vi2(xi2, $ix2::splat(1)), xi2);

  xi2 = vsel_vi2_vo_vi2_vi2(c, vsub_vi2_vi2_vi2($ix2::splat(0), vxor_vi2_vi2_vi2(xi2, $ix2::splat(1 << 31))), xi2);

  $f32x ret = vreinterpret_vf_vi2(xi2);

  ret = vsel_vf_vo_vf_vf(ret.eq($f32x::splat(0.)) & x.ne($f32x::splat(0.)), 
			 vmulsign_vf_vf_vf($f32x::splat(0.), x), ret);

  ret = vsel_vf_vo_vf_vf(x.eq($f32x::splat(0.)) & y.eq($f32x::splat(0.)), y, ret);

  ret = vsel_vf_vo_vf_vf(visnan_vo_vf(x) | visnan_vo_vf(y), $f32x::splat(SLEEF_NAN_F), ret);
  
  return ret;
}

pub fn xfrfrexpf(x: $f32x) -> $f32x {
  x = vsel_vf_vo_vf_vf(vabs_vf_vf(x).lt($f32x::splat(f32::MIN)), x * $f32x::splat(F1_32), x);

  vmask xm = vreinterpret_vm_vf(x);
  xm = vand_vm_vm_vm(xm, vcast_vm_i_i(~0x7f800000U, ~0x7f800000U));
  xm = vor_vm_vm_vm (xm, vcast_vm_i_i( 0x3f000000U,  0x3f000000U));

  $f32x ret = vreinterpret_vf_vm(xm);

  ret = vsel_vf_vo_vf_vf(visinf_vo_vf(x), vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), x), ret);
  ret = vsel_vf_vo_vf_vf(x.eq($f32x::splat(0.)), x, ret);
  
  return ret;
}

pub fn xexpfrexpf(x: $f32x) -> $ix2 {
  /*
  x = vsel_vf_vo_vf_vf(vabs_vf_vf(x).lt($f32x::splat(f32::MIN)), x * $f32x::splat(F1_63), x);

  let ret = vreinterpret_vi2_vf$ix::splat(x);
  ret = vand_vi_vi_vi(vsrl_vi_vi_i(ret, 20), $ix::splat(0x7ff)) - $ix::splat(0x3fe);

  ret = vsel_vi_vo_vi_vi(x.eq($f32x::splat(0.)) | visnan_vo_vf(x) | visinf_vo_vf(x), $ix::splat(0), ret);
  
  return ret;
  */
  return $ix2::splat(0);
}
#[inline]
fn vtoward0f(x: $f32x) -> $f32x {
  $f32x t = vreinterpret_vf_vi2(vsub_vi2_vi2_vi2(vreinterpret_vi2_vf(x), $ix2::splat(1)));
  return vsel_vf_vo_vf_vf(x.eq($f32x::splat(0.)), $f32x::splat(0.), t);
}
#[inline]
fn vptruncf(x: $f32x) -> $f32x {
#ifdef FULL_FP_ROUNDING
  return vtruncate_vf_vf(x);
#else
  $f32x fr = x - $f32x::from(vtruncate_vi2_vf(x));
  return vsel_vf_vo_vf_vf(vabs_vf_vf(x).ge($f32x::splat(F1_23)), x, x - fr);
#endif
}

pub fn xfmodf(x: $f32x, y: $f32x) -> $f32x {
  $f32x nu = vabs_vf_vf(x), de = vabs_vf_vf(y), s = $f32x::splat(1.), q;
  $mox o = de.lt($f32x::splat(f32::MIN));
  nu = vsel_vf_vo_vf_vf(o, nu * $f32x::splat(F1_25), nu);
  de = vsel_vf_vo_vf_vf(o, de * $f32x::splat(F1_25), de);
  s  = vsel_vf_vo_vf_vf(o, s * $f32x::splat(1. / F1_25), s);
  $f32x rde = vtoward0f(vrec_vf_vf(de));
#if defined(ENABLE_NEON32) || defined(ENABLE_NEON32VFPV4)
  rde = vtoward0f(rde);
#endif
  F2 r = F2::new(nu, $f32x::splat(0.));

  for(int i=0;i<8;i++) { // ceil(log2(FLT_MAX) / 22)+1
    q = vsel_vf_vo_vf_vf((de + de).gt(r.0) &
				       r.0.ge(de),
			 $f32x::splat(1.), vtoward0f(r.0) * rde);
    r = dfnormalize_vf2_vf2(dfadd2_vf2_vf2_vf2(r, dfmul_vf2_vf_vf(vptruncf(q), -de)));
    if (vtestallones_i_vo32(r.0.lt(de))) break;
  }
  
  $f32x ret = (r.0 + r.1) * s;
  ret = vsel_vf_vo_vf_vf((r.0 + r.1).eq(de), $f32x::splat(0.), ret);

  ret = vmulsign_vf_vf_vf(ret, x);

  ret = vsel_vf_vo_vf_vf(nu.lt(de), x, ret);
  ret = vsel_vf_vo_vf_vf(de.eq($f32x::splat(0.)), $f32x::splat(SLEEF_NAN_F), ret);

  return ret;
}

//
#[inline]
fn sinpifk(d: $f32x) -> F2 {
  $mox o;
  $f32x u, s, t;
  F2 x, s2;

  u = d * $f32x::splat(4.);
  $ix2 q = vtruncate_vi2_vf(u);
  q = vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vxor_vi2_vi2_vi2(vsrl_vi2_vi2_i(q, 31), $ix2::splat(1))), $ix2::splat(~1));
  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(2));

  s = u - $f32x::from(q);
  t = s;
  s = s * s;
  s2 = dfmul_vf2_vf_vf(t, t);

  //

  u = vsel_vf_vo_f_f(o, -0.2430611801e-7, 0.3093842054e-6)
      .mla(s, vsel_vf_vo_f_f(o, 0.3590577080e-5, -0.3657307388e-4))
      .mla(s, vsel_vf_vo_f_f(o, -0.3259917721e-3, 0.2490393585e-2));
  x = dfadd2_vf2_vf_vf2(u * s,
			vsel_vf2_vo_f_f_f_f(o, 0.015854343771934509277, 4.4940051354032242811e-10,
					    -0.080745510756969451904, -1.3373665339076936258e-09));
  x = dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(s2, x),
			 vsel_vf2_vo_f_f_f_f(o, -0.30842512845993041992, -9.0728339030733922277e-09,
					     0.78539818525314331055, -2.1857338617566484855e-08));

  x = dfmul_vf2_vf2_vf2(x, vsel_vf2_vo_vf2_vf2(o, s2, F2::new(t, $f32x::splat(0.))));
  x = vsel_vf2_vo_vf2_vf2(o, dfadd2_vf2_vf2_vf(x, $f32x::splat(1.)), x);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(4)), $ix2::splat(4));
  x.0 = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(x.0)));
  x.1 = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(x.1)));

  return x;
}

pub fn xsinpif_u05(d: $f32x) -> $f32x {
  F2 x = sinpifk(d);
  $f32x r = x.0 + x.1;

  r = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), $f32x::splat(-0.), r);
  r = vreinterpret_vf_vm(vandnot_vm_vo32_vm(vabs_vf_vf(d).gt($f32x::splat(TRIGRANGEMAX4_F)), vreinterpret_vm_vf(r)));
  r = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(d), vreinterpret_vm_vf(r)));
  
  return r;
}
#[inline]
fn cospifk(d: $f32x) -> F2 {
  $mox o;
  $f32x u, s, t;
  F2 x, s2;

  u = d * $f32x::splat(4.);
  $ix2 q = vtruncate_vi2_vf(u);
  q = vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vxor_vi2_vi2_vi2(vsrl_vi2_vi2_i(q, 31), $ix2::splat(1))), $ix2::splat(!1));
  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(0));

  s = u - $f32x::from(q);
  t = s;
  s = s * s;
  s2 = dfmul_vf2_vf_vf(t, t);
  
  //

  u = vsel_vf_vo_f_f(o, -0.2430611801e-7, 0.3093842054e-6)
      .mla(s, vsel_vf_vo_f_f(o, 0.3590577080e-5, -0.3657307388e-4))
      .mla(s, vsel_vf_vo_f_f(o, -0.3259917721e-3, 0.2490393585e-2));
  x = dfadd2_vf2_vf_vf2(u * s,
			vsel_vf2_vo_f_f_f_f(o, 0.015854343771934509277, 4.4940051354032242811e-10,
					    -0.080745510756969451904, -1.3373665339076936258e-09));
  x = dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(s2, x),
			 vsel_vf2_vo_f_f_f_f(o, -0.30842512845993041992, -9.0728339030733922277e-09,
					     0.78539818525314331055, -2.1857338617566484855e-08));

  x = dfmul_vf2_vf2_vf2(x, vsel_vf2_vo_vf2_vf2(o, s2, F2::new(t, $f32x::splat(0.))));
  x = vsel_vf2_vo_vf2_vf2(o, dfadd2_vf2_vf2_vf(x, $f32x::splat(1.)), x);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, $ix2::splat(2)), $ix2::splat(4)), $ix2::splat(4));
  x.0 = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(x.0)));
  x.1 = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf($f32x::splat(-0.))), vreinterpret_vm_vf(x.1)));

  return x;
}

pub fn xcospif_u05(d: $f32x) -> $f32x {
  F2 x = cospifk(d);
  $f32x r = x.0 + x.1;

  r = vsel_vf_vo_vf_vf(vabs_vf_vf(d).gt($f32x::splat(TRIGRANGEMAX4_F)), $f32x::splat(1.), r);
  r = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(d), vreinterpret_vm_vf(r)));
  
  return r;
}

#if defined(ENABLE_SVE) || defined(ENABLE_SVENOFMA)
  typedef __sizeless_struct {
    F2 a, b;
  } df2;
#else
  typedef struct {
    F2 a, b;
  } df2;
#endif

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
static CONST df2 gammafk(a: $f32x) {
  F2 clc = F2::from((0., 0.));
  clln = F2::from((1., 0.));
  clld = F2::from((1., 0.));
  F2 v = F2::from((1., 0.)),
  x, y, z;
  $f32x t, u;

  $mox otiny = vabs_vf_vf(a).lt($f32x::splat(1e-30)), oref = a.lt($f32x::splat(0.5));

  x = vsel_vf2_vo_vf2_vf2(otiny, F2::from((0., 0.)),
			  vsel_vf2_vo_vf2_vf2(oref, dfadd2_vf2_vf_vf($f32x::splat(1.), -a),
					      F2::new(a, $f32x::splat(0.))));

  $mox o0 = $f32x::splat(0.5).le(x.0) & x.0.le($f32x::splat(1.2));
  $mox o2 = $f32x::splat(2.3).le(x.0);
  
  y = dfnormalize_vf2_vf2(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf(x, $f32x::splat(1.)), x));
  y = dfnormalize_vf2_vf2(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf(x, $f32x::splat(2.)), y));

  $mox o = o2 & x.0.le($f32x::splat(7.));
  clln = vsel_vf2_vo_vf2_vf2(o, y, clln);

  x = vsel_vf2_vo_vf2_vf2(o, dfadd2_vf2_vf2_vf(x, $f32x::splat(3.)), x);
  t = vsel_vf_vo_vf_vf(o2, vrec_vf_vf(x.0), dfnormalize_vf2_vf2(dfadd2_vf2_vf2_vf(x, vsel_vf_vo_f_f(o0, -1, -2))).0);

  u = vsel_vf_vo_vo_f_f_f(o2, o0, 0.000839498720672087279971000786, 0.9435157776e+0, 0.1102489550e-3)
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, -5.17179090826059219329394422e-05, 0.8670063615e+0, 0.8160019934e-4))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, -0.000592166437353693882857342347, 0.4826702476e+0, 0.1528468856e-3))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, 6.97281375836585777403743539e-05, -0.8855129778e-1, -0.2355068718e-3))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, 0.000784039221720066627493314301, 0.1013825238e+0, 0.4962242092e-3))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, -0.000229472093621399176949318732, -0.1493408978e+0, -0.1193488017e-2))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, -0.002681327160493827160473958490, 0.1697509140e+0, 0.2891599433e-2))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, 0.003472222222222222222175164840, -0.2072454542e+0, -0.7385451812e-2))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, 0.083333333333333333335592087900, 0.2705872357e+0, 0.2058077045e-1));

  y = dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf(x, $f32x::splat(-0.5)), logk2f(x));
  y = dfadd2_vf2_vf2_vf2(y, dfneg_vf2_vf2(x));
  y = dfadd2_vf2_vf2_vf2(y, F2::from(0.91893853320467278056_f64)); // 0.5*log(2*M_PI)

  z = dfadd2_vf2_vf2_vf(dfmul_vf2_vf_vf (u, t), vsel_vf_vo_f_f(o0, -0.400686534596170958447352690395e+0, -0.673523028297382446749257758235e-1));
  z = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(z, t), vsel_vf_vo_f_f(o0, 0.822466960142643054450325495997e+0, 0.322467033928981157743538726901e+0));
  z = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(z, t), vsel_vf_vo_f_f(o0, -0.577215665946766039837398973297e+0, 0.422784335087484338986941629852e+0));
  z = dfmul_vf2_vf2_vf(z, t);

  clc = vsel_vf2_vo_vf2_vf2(o2, y, z);
  
  clld = vsel_vf2_vo_vf2_vf2(o2, dfadd2_vf2_vf2_vf(dfmul_vf2_vf_vf(u, t), $f32x::splat(1.)), clld);
  
  y = clln;

  clc = vsel_vf2_vo_vf2_vf2(otiny, F2::from(41.58883083359671856503_f64), // log(2^60)
			    vsel_vf2_vo_vf2_vf2(oref, dfadd2_vf2_vf2_vf2(F2::from(1.1447298858494001639_f64), dfneg_vf2_vf2(clc)), clc)); // log(M_PI)
  clln = vsel_vf2_vo_vf2_vf2(otiny, F2::from((1., 0.)), vsel_vf2_vo_vf2_vf2(oref, clln, clld));

  if (!vtestallones_i_vo32(vnot_vo32_vo32(oref))) {
    t = a - $f32x::splat(F1_12) * $f32x::from(vtruncate_vi2_vf(a * $f32x::splat(1. / F1_12)));
    x = dfmul_vf2_vf2_vf2(clld, sinpifk(t));
  }
  
  clld = vsel_vf2_vo_vf2_vf2(otiny, F2::new(a * $f32x::splat(F1_30*F1_30), $f32x::splat(0.)),
			     vsel_vf2_vo_vf2_vf2(oref, x, y));

  df2 ret = { clc, dfdiv_vf2_vf2_vf2(clln, clld) };

  return ret;
}

pub fn xtgammaf_u1(a: $f32x) -> $f32x {
  df2 d = gammafk(a);
  F2 y = dfmul_vf2_vf2_vf2(expk2f(d.a), d.b);
  $f32x r = y.0 + y.1;
  $mox o;

  o = a.eq($f32x::splat(-SLEEF_INFINITY_F)) |
				(a.lt($f32x::splat(0.)) & visint_vo_vf(a)) |
		   (visnumber_vo_vf(a) & a.lt($f32x::splat(0.)) & visnan_vo_vf(r));
  r = vsel_vf_vo_vf_vf(o, $f32x::splat(SLEEF_NAN_F), r);

  o = (a.eq($f32x::splat(SLEEF_INFINITY_F)) | visnumber_vo_vf(a)) &
				  a.ge($f32x::splat(-f32::MIN)) &
		    (a.eq($f32x::splat(0.)) | a.gt($f32x::splat(36.)) | visnan_vo_vf(r));
  r = vsel_vf_vo_vf_vf(o, vmulsign_vf_vf_vf($f32x::splat(SLEEF_INFINITY_F), a), r);
  
  return r;
}

pub fn xlgammaf_u1(a: $f32x) -> $f32x {
  df2 d = gammafk(a);
  F2 y = dfadd2_vf2_vf2_vf2(d.a, logk2f(dfabs_vf2_vf2(d.b)));
  $f32x r = y.0 + y.1;
  $mox o;

  o = visinf_vo_vf(a) |
		   ((a.le($f32x::splat(0.)) & visint_vo_vf(a)) |
				(visnumber_vo_vf(a) & visnan_vo_vf(r)));
  r = vsel_vf_vo_vf_vf(o, $f32x::splat(SLEEF_INFINITY_F), r);

  return r;
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xerff_u1(a: $f32x) -> $f32x {
  $f32x s = a, t, u;
  F2 d;

  a = vabs_vf_vf(a);
  $mox o0 = a.lt($f32x::splat(1.1));
  $mox o1 = a.lt($f32x::splat(2.4));
  $mox o2 = a.lt($f32x::splat(4.));
  u = vsel_vf_vo_vf_vf(o0, a * a, a);
  
  t = vsel_vf_vo_vo_f_f_f(o0, o1, 0.7089292194e-4, -0.1792667899e-4, -0.9495757695e-5)
      .mla(u, vsel_vf_vo_vo_f_f_f(o0, o1, -0.7768311189e-3, 0.3937633010e-3, 0.2481465926e-3))
      .mla(u, vsel_vf_vo_vo_f_f_f(o0, o1, 0.5159463733e-2, -0.3949181177e-2, -0.2918176819e-2))
      .mla(u, vsel_vf_vo_vo_f_f_f(o0, o1, -0.2683781274e-1, 0.2445474640e-1, 0.2059706673e-1))
      .mla(u, vsel_vf_vo_vo_f_f_f(o0, o1, 0.1128318012e+0, -0.1070996150e+0, -0.9901899844e-1));
  d = dfmul_vf2_vf_vf(t, u);
  d = dfadd2_vf2_vf2_vf2(d, vsel_vf2_vo_vo_d_d_d(o0, o1, -0.376125876000657465175213237214e+0, -0.634588905908410389971210809210e+0, -0.643598050547891613081201721633e+0));
  d = dfmul_vf2_vf2_vf(d, u);
  d = dfadd2_vf2_vf2_vf2(d, vsel_vf2_vo_vo_d_d_d(o0, o1, 0.112837916021059138255978217023e+1, -0.112879855826694507209862753992e+1, -0.112461487742845562801052956293e+1));
  d = dfmul_vf2_vf2_vf(d, a);
  d = vsel_vf2_vo_vf2_vf2(o0, d, dfadd_vf2_vf_vf2($f32x::splat(1.), dfneg_vf2_vf2(expk2f(d))));
  u = vmulsign_vf_vf_vf(vsel_vf_vo_vf_vf(o2, d.0 + d.1, $f32x::splat(1.)), s);
  u = vsel_vf_vo_vf_vf(visnan_vo_vf(a), $f32x::splat(SLEEF_NAN_F), u);

  return u;
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xerfcf_u15(a: $f32x) -> $f32x {
  $f32x s = a, r = $f32x::splat(0.), t;
  F2 u, d, x;
  a = vabs_vf_vf(a);
  $mox o0 = a.lt($f32x::splat(1.));
  $mox o1 = a.lt($f32x::splat(2.2));
  $mox o2 = a.lt($f32x::splat(4.3));
  $mox o3 = a.lt($f32x::splat(10.1));

  u = vsel_vf2_vo_vf2_vf2(o1, F2::new(a, $f32x::splat(0.)), dfdiv_vf2_vf2_vf2(F2::from((1., 0.)), F2::new(a, $f32x::splat(0.))));

  t = vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, -0.8638041618e-4, -0.6236977242e-5, -0.3869504035e+0, 0.1115344167e+1)
      .mla(u.0, vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, 0.6000166177e-3, 0.5749821503e-4, 0.1288077235e+1, -0.9454904199e+0))
      .mla(u.0, vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, -0.1665703603e-2, 0.6002851478e-5, -0.1816803217e+1, -0.3667259514e+0))
      .mla(u.0, vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, 0.1795156277e-3, -0.2851036377e-2, 0.1249150872e+1, 0.7155663371e+0))
      .mla(u.0, vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, 0.1914106123e-1, 0.2260518074e-1, -0.1328857988e+0, -0.1262947265e-1));

  d = dfmul_vf2_vf2_vf(u, t);
  d = dfadd2_vf2_vf2_vf2(d, vsel_vf2_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.102775359343930288081655368891e+0, -0.105247583459338632253369014063e+0, -0.482365310333045318680618892669e+0, -0.498961546254537647970305302739e+0));
  d = dfmul_vf2_vf2_vf2(d, u);
  d = dfadd2_vf2_vf2_vf2(d, vsel_vf2_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.636619483208481931303752546439e+0, -0.635609463574589034216723775292e+0, -0.134450203224533979217859332703e-2, -0.471199543422848492080722832666e-4));
  d = dfmul_vf2_vf2_vf2(d, u);
  d = dfadd2_vf2_vf2_vf2(d, vsel_vf2_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.112837917790537404939545770596e+1, -0.112855987376668622084547028949e+1, -0.572319781150472949561786101080e+0, -0.572364030327966044425932623525e+0));
  
  x = dfmul_vf2_vf2_vf(vsel_vf2_vo_vf2_vf2(o1, d, F2::new(-a, $f32x::splat(0.))), a);
  x = vsel_vf2_vo_vf2_vf2(o1, x, dfadd2_vf2_vf2_vf2(x, d));

  x = expk2f(x);
  x = vsel_vf2_vo_vf2_vf2(o1, x, dfmul_vf2_vf2_vf2(x, u));

  r = vsel_vf_vo_vf_vf(o3, x.0 + x.1, $f32x::splat(0.));
  r = vsel_vf_vo_vf_vf(vsignbit_vo_vf(s), $f32x::splat(2.) - r, r);
  r = vsel_vf_vo_vf_vf(visnan_vo_vf(s), $f32x::splat(SLEEF_NAN_F), r);
  return r;
}

#ifndef ENABLE_GNUABI
pub fn xgetIntf(int name) -> int {
  if (1 <= name && name <= 10) return vavailability_i(name);
  return 0;
}

pub fn *xgetPtrf(int name) -> void {
  if (name == 0) return ISANAME;
  return (void *)0;
}
#endif

/*#ifdef ALIAS_NO_EXT_SUFFIX
#include ALIAS_NO_EXT_SUFFIX
#endif

#ifdef ENABLE_GNUABI
pub fn $f32x __acosf_finite     ($f32x)         __attribute__((weak, alias(str_xacosf_u1  )));
pub fn $f32x __acoshf_finite    ($f32x)         __attribute__((weak, alias(str_xacoshf    )));
pub fn $f32x __asinf_finite     (double)         __attribute__((weak, alias(str_xasinf_u1  )));
pub fn $f32x __atan2f_finite    ($f32x, $f32x) __attribute__((weak, alias(str_xatan2f_u1 )));
pub fn $f32x __atanhf_finite    ($f32x)         __attribute__((weak, alias(str_xatanhf    )));
pub fn $f32x __coshf_finite     ($f32x)         __attribute__((weak, alias(str_xcoshf     )));
pub fn $f32x __exp10f_finite    ($f32x)         __attribute__((weak, alias(str_xexp10f    )));
pub fn $f32x __exp2f_finite     ($f32x)         __attribute__((weak, alias(str_xexp2f     )));
pub fn $f32x __expf_finite      ($f32x)         __attribute__((weak, alias(str_xexpf      )));
pub fn $f32x __fmodf_finite     ($f32x, $f32x) __attribute__((weak, alias(str_xfmodf     )));
pub fn $f32x __modff_finite      ($f32x, $f32x *) __attribute__((weak, alias(str_xmodff  )));
pub fn $f32x __hypotf_u05_finite($f32x, $f32x) __attribute__((weak, alias(str_xhypotf_u05)));
pub fn $f32x __lgammaf_u1_finite($f32x)         __attribute__((weak, alias(str_xlgammaf_u1)));
pub fn $f32x __log10f_finite    ($f32x)         __attribute__((weak, alias(str_xlog10f    )));
pub fn $f32x __logf_finite      ($f32x)         __attribute__((weak, alias(str_xlogf_u1   )));
pub fn $f32x __powf_finite      ($f32x, $f32x) __attribute__((weak, alias(str_xpowf      )));
pub fn $f32x __sinhf_finite     ($f32x)         __attribute__((weak, alias(str_xsinhf     )));
pub fn $f32x __sqrtf_finite     ($f32x)         __attribute__((weak, alias(str_xsqrtf     )));
pub fn $f32x __tgammaf_u1_finite($f32x)         __attribute__((weak, alias(str_xtgammaf_u1)));

#ifdef HEADER_MASKED
#include HEADER_MASKED
#endif
#endif /* #ifdef ENABLE_GNUABI */

*/
