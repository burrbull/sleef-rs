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
fn $m64x vnot_vo64_vo64($m64x x) {
  return x ^ veq64_vo_vm_vm(vcast_vm_i_i(0, 0), vcast_vm_i_i(0, 0));
}
#[inline]
fn vsignbit_vo_vd(d: $f64x) -> $m64x {
  return veq64_vo_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd((-0.).as_vd()));
}

// return d0 < d1 ? x : y
#[inline]
fn vsel_vi_vd_vd_vi_vi($f64x d0, $f64x d1, $i64x x, $i64x y) -> $i64x { return vsel_vi_vo_vi_vi(vcast_vo32_vo64(d0.lt(d1)), x, y); } 

// return d0 < 0 ? x : 0
#[inline]
fn vsel_vi_vd_vi($f64x d, $i64x x) -> $i64x { return vand_vi_vo_vi(vcast_vo32_vo64(vsignbit_vo_vd(d)), x); }
#[inline]
fn visnegzero_vo_vd(d: $f64x) -> $m64x {
  return veq64_vo_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd((-0.).as_vd()));
}
#[inline]
fn visnumber_vo_vd(x: $f64x) -> $m64x {
  return vandnot_vo_vo_vo(visinf_vo_vd(x), x.eq(x));
}
#[inline]
fn vsignbit_vm_vd(d: $f64x) -> vmask {
  return vand_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd((-0.).as_vd()));
}
#[inline]
fn vmulsign_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x {
  return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(x), vsignbit_vm_vd(y)));
}
#[inline]
fn vcopysign_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x {
  return vreinterpret_vd_vm(vxor_vm_vm_vm(vandnot_vm_vm_vm(vreinterpret_vm_vd((-0.).as_vd()), vreinterpret_vm_vd(x)), 
					  vand_vm_vm_vm   (vreinterpret_vm_vd((-0.).as_vd()), vreinterpret_vm_vd(y))));
}
#[inline]
fn vsign_vd_vd(d: $f64x) -> $f64x {
  return vmulsign_vd_vd_vd((1.).as_vd(), d);
}
#[inline]
fn vpow2i_vd_vi(q: $i64x) -> $f64x {
  q = (0x3ff).as_vi() + q;
  let r = q.as_vi2();
  return vreinterpret_vd_vi2(vsll_vi2_vi2_i(r, 20));
}
#[inline]
fn vldexp_vd_vd_vi(x: $f64x, q: $i64x) -> $f64x {
  $i64x m = vsra_vi_vi_i(q, 31);
  m = vsll_vi_vi_i(vsra_vi_vi_i(m + q, 9) - m, 7);
  q = q - vsll_vi_vi_i(m, 2);
  m = (0x3ff).as_vi() + m;
  m = vandnot_vi_vo_vi(vgt_vo_vi_vi((0).as_vi(), m), m);
  m = vsel_vi_vo_vi_vi(vgt_vo_vi_vi(m, (0x7ff).as_vi()), (0x7ff).as_vi(), m);
  let r = m.as_vi2();
  $f64x y = vreinterpret_vd_vi2(vsll_vi2_vi2_i(r, 20));
  return x * y * y * y * y * vpow2i_vd_vi(q);
}
#[inline]
fn vldexp2_vd_vd_vi(d: $f64x, e: $i64x) -> $f64x {
  return d * vpow2i_vd_vi(vsra_vi_vi_i(e, 1)) * vpow2i_vd_vi(e - vsra_vi_vi_i(e, 1));
}
#[inline]
fn vldexp3_vd_vd_vi(d: $f64x, q: $i64x) -> $f64x {
  return vreinterpret_vd_vi2(vadd_vi2_vi2_vi2(vreinterpret_vi2_vd(d), vsll_vi2_vi2_i(q.as_vi2(), 20)));
}

#[cfg(all(not(feature="enable_avx512f"), not(feature="enable_avx512fnofma")))]#[inline]
fn vilogbk_vi_vd(d: $f64x) -> $i64x {
  $m64x o = d.lt((4.9090934652977266e-91).as_vd());
  d = vsel_vd_vo_vd_vd(o, (2.037035976334486e90).as_vd() * d, d);
  let q = vreinterpret_vi2_vd(d).as_vi();
  q = vand_vi_vi_vi(q, (((1 << 12)-1) << 20).as_vi());
  q = vsrl_vi_vi_i(q, 20);
  q - vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), (300 + 0x3ff).as_vi(), (0x3ff).as_vi())
}
#[inline]
fn vilogb2k_vi_vd(d: $f64x) -> $i64x {
  let q = vreinterpret_vi2_vd(d).as_vi();
  q = vsrl_vi_vi_i(q, 20);
  q = vand_vi_vi_vi(q, (0x7ff).as_vi());
  q - (0x3ff).as_vi()
}
#endif
#[inline]
fn visint_vo_vd(d: $f64x) -> $m64x {
  $f64x x = vtruncate_vd_vd(d * (1. / D1_31).as_vd());
  x = (-D1_31).as_vd().mla(x, d);
  vtruncate_vd_vd(x).eq(x) | vabs_vd_vd(d).gt(D1_53.as_vd())
}
#[inline]
fn visodd_vo_vd(d: $f64x) -> $m64x {
  $f64x x = vtruncate_vd_vd(d * (1. / D1_31).as_vd());
  x = (-D1_31).as_vd().mla(x, d);

  vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(vtruncate_vi_vd(x), (1).as_vi()), (1).as_vi())) &
		       vabs_vd_vd(d).lt((D1_53).as_vd())
}

//

pub fn xldexp(x: $f64x, q: $i64x) -> $f64x { return vldexp_vd_vd_vi(x, q); }

pub fn xilogb(d: $f64x) -> $i64x {
  $f64x e = vcast_vd_vi(vilogbk_vi_vd(vabs_vd_vd(d)));
  e = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), SLEEF_FP_ILOGB0.as_vd(), e);
  e = vsel_vd_vo_vd_vd(visnan_vo_vd(d), SLEEF_FP_ILOGBNAN.as_vd(), e);
  e = vsel_vd_vo_vd_vd(visinf_vo_vd(d), INT_MAX.as_vd(), e);
  return vrint_vi_vd(e);
}

#[inline]
fn rempisub(x: $f64x) -> ($f64x, $i64x) {
   if cfg!(feature="full_fp_rounding") {
    $f64x y = vrint_vd_vd(x * (4.).as_vd());
    $i64x vi = vtruncate_vi_vd(y - vrint_vd_vd(x) * (4.).as_vd());
    ( x - y * (0.25).as_vd(), vi )
   } else {
    $f64x fr = x - D1_28.as_vd() * vtruncate_vd_vd(x * (1. / D1_28).as_vd());
    $i64x vi = vsel_vi_vo_vi_vi(vcast_vo32_vo64(x.gt((0.).as_vd())), (4).as_vi(), (3).as_vi()) + vtruncate_vi_vd(fr * (8.).as_vd());
    vi = vsra_vi_vi_i(vand_vi_vi_vi((7).as_vi(), vi) - (3).as_vi(), 1);
    fr = fr - (0.25).as_vd() * vtruncate_vd_vd(fr.mla((4.).as_vd(), vmulsign_vd_vd_vd((0.5).as_vd(), x)));
    fr = vsel_vd_vo_vd_vd(vabs_vd_vd(fr).gt((0.25).as_vd()), fr - vmulsign_vd_vd_vd((0.5).as_vd(), x), fr);
    fr = vsel_vd_vo_vd_vd(vabs_vd_vd(fr).gt((1e+10).as_vd()), (0).as_vd(), fr);
    $m64x o = vabs_vd_vd(x).eq((0.12499999999999998612).as_vd());
    fr = vsel_vd_vo_vd_vd(o, x, fr);
    vi = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), (0).as_vi(), vi);
    ( fr, vi )
  }
}
#[inline]
fn rempi(a: $f64x) -> (VDouble2, $i64x) {
  VDouble2 x, y, z;
  $i64x ex = vilogb2k_vi_vd(a);
#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  ex = vandnot_vi_vi_vi(vsra_vi_vi_i(ex, 31), ex);
  ex = vand_vi_vi_vi(ex, (1023).as_vi());
#endif
  ex = ex - (55).as_vi();
  $i64x q = vand_vi_vo_vi(vgt_vo_vi_vi(ex, vcast_vi_i(700-55)), vcast_vi_i(-64));
  a = vldexp3_vd_vd_vi(a, q);
  ex = vandnot_vi_vi_vi(vsra_vi_vi_i(ex, 31), ex);
  ex = vsll_vi_vi_i(ex, 2);
  x = ddmul_vd2_vd_vd(a, vgather_vd_p_vi(rempitabdp, ex));
  let (dii, did) = rempisub(x.0);
  q = dii;
  x.0 = did;
  x = ddnormalize_vd2_vd2(x);
  y = ddmul_vd2_vd_vd(a, vgather_vd_p_vi(rempitabdp+1, ex));
  x = ddadd2_vd2_vd2_vd2(x, y);
  let (dii, did) = rempisub(x.0);
  q = q + dii;
  x.0 = did;
  x = ddnormalize_vd2_vd2(x);
  y = vcast_vd2_vd_vd(vgather_vd_p_vi(rempitabdp+2, ex), vgather_vd_p_vi(rempitabdp+3, ex));
  y = ddmul_vd2_vd2_vd(y, a);
  x = ddadd2_vd2_vd2_vd2(x, y);
  x = ddnormalize_vd2_vd2(x);
  x = ddmul_vd2_vd2_vd2(x, vcast_vd2_d_d(3.141592653589793116*2, 1.2246467991473532072e-16*2));
  $m64x o = vabs_vd_vd(a).lt((0.7).as_vd());
  x.0 = vsel_vd_vo_vd_vd(o, a, x.0);
  x.1 = vreinterpret_vd_vm(vandnot_vm_vo64_vm(o, vreinterpret_vm_vd(x.1)));
  ( x, q )
}

pub fn xsin(d: $f64x) -> $f64x {
  $f64x u, s, r = d;
  $i64x ql;

  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX2.as_vd())))) {
    $f64x dql = vrint_vd_vd(d * M_1_PI.as_vd());
    ql = vrint_vi_vd(dql);
    d = dql.mla((-PI_A2).as_vd(), d);
    d = dql.mla((-PI_B2).as_vd(), d);
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX.as_vd())))) {
    $f64x dqh = vtruncate_vd_vd(d * (M_1_PI / D1_24).as_vd());
    dqh = dqh * D1_24.as_vd();
    $f64x dql = vrint_vd_vd(vmlapn_vd_vd_vd_vd(d, M_1_PI.as_vd(), dqh));
    ql = vrint_vi_vd(dql);

    d = dqh.mla((-PI_A).as_vd(), d);
    d = dql.mla((-PI_A).as_vd(), d);
    d = dqh.mla((-PI_B).as_vd(), d);
    d = dql.mla((-PI_B).as_vd(), d);
    d = dqh.mla((-PI_C).as_vd(), d);
    d = dql.mla((-PI_C).as_vd(), d);
    d = (dqh + dql).mla((-PI_D).as_vd(), d);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = vand_vi_vi_vi(ddii, (3).as_vi());
    ql = ql + ql + vsel_vi_vo_vi_vi(vcast_vo32_vo64(ddidd.0.gt((0.).as_vd())), (2).as_vi(), (1).as_vi());
    ql = vsra_vi_vi_i(ql, 2);
    $m64x o = veq_vo_vi_vi(vand_vi_vi_vi(ddii, (1).as_vi()), (1).as_vi());
    VDouble2 x = vcast_vd2_vd_vd(vmulsign_vd_vd_vd((-3.141592653589793116 * 0.5).as_vd(), ddidd.0), 
				 vmulsign_vd_vd_vd((-1.2246467991473532072e-16 * 0.5).as_vd(), ddidd.0));
    x = ddadd2_vd2_vd2_vd2(ddidd, x);
    ddidd = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(o), x, ddidd);
    d = ddidd.0 + ddidd.1;
    d = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(r) | visnan_vo_vd(r), vreinterpret_vm_vd(d)));
  }

  s = d * d;

  d = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (1).as_vi()), (1).as_vi())), vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(d)));

  u = (-7.97255955009037868891952e-18).as_vd()
      .mla(s, (2.81009972710863200091251e-15).as_vd())
      .mla(s, (-7.64712219118158833288484e-13).as_vd())
      .mla(s, (1.60590430605664501629054e-10).as_vd())
      .mla(s, (-2.50521083763502045810755e-08).as_vd())
      .mla(s, (2.75573192239198747630416e-06).as_vd())
      .mla(s, (-0.000198412698412696162806809).as_vd())
      .mla(s, (0.00833333333333332974823815).as_vd())
      .mla(s, (-0.166666666666666657414808).as_vd());

  u = s*(u*d) + d;

  u = vsel_vd_vo_vd_vd(visnegzero_vo_vd(r), r, u);
  
  return u;
}

pub fn xsin_u1(d: $f64x) -> $f64x {
  $f64x u;
  VDouble2 s, t, x;
  $i64x ql;
  
  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX2.as_vd())))) {
    const $f64x dql = vrint_vd_vd(d*(M_1_PI).as_vd());
    ql = vrint_vi_vd(dql);
    u = dql.mla((-PI_A2).as_vd(), d);
    s = ddadd_vd2_vd_vd (u, dql*(-PI_B2).as_vd());
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX.as_vd())))) {
    $f64x dqh = vtruncate_vd_vd(d*(M_1_PI / D1_24).as_vd());
    dqh = dqh*(D1_24).as_vd();
    const $f64x dql = vrint_vd_vd(vmlapn_vd_vd_vd_vd(d, (M_1_PI).as_vd(), dqh));
    ql = vrint_vi_vd(dql);

    u = dqh.mla((-PI_A).as_vd(), d);
    s = ddadd_vd2_vd_vd  (u, dql*(-PI_A).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dqh*(-PI_B).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dql*(-PI_B).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dqh*(-PI_C).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dql*(-PI_C).as_vd());
    s = ddadd_vd2_vd2_vd(s, (dqh + dql)*(-PI_D).as_vd());
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = vand_vi_vi_vi(ddii, (3).as_vi());
    ql = ql + ql + vsel_vi_vo_vi_vi(vcast_vo32_vo64(ddidd.0.gt((0.).as_vd())), (2).as_vi(), (1).as_vi());
    ql = vsra_vi_vi_i(ql, 2);
    $m64x o = veq_vo_vi_vi(vand_vi_vi_vi(ddii, (1).as_vi()), (1).as_vi());
    VDouble2 x = vcast_vd2_vd_vd(vmulsign_vd_vd_vd((-3.141592653589793116 * 0.5).as_vd(), ddidd.0), 
				 vmulsign_vd_vd_vd((-1.2246467991473532072e-16 * 0.5).as_vd(), ddidd.0));
    x = ddadd2_vd2_vd2_vd2(ddidd, x);
    ddidd = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(o), x, ddidd);
    s = ddnormalize_vd2_vd2(ddidd);
    s.0 = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d) | visnan_vo_vd(d), vreinterpret_vm_vd(s.0)));
  }
  
  t = s;
  s = ddsqu_vd2_vd2(s);

  u = (2.72052416138529567917983e-15).as_vd()
      .mla(s.0, (-7.6429259411395447190023e-13).as_vd())
      .mla(s.0, (1.60589370117277896211623e-10).as_vd())
      .mla(s.0, (-2.5052106814843123359368e-08).as_vd())
      .mla(s.0, (2.75573192104428224777379e-06).as_vd())
      .mla(s.0, (-0.000198412698412046454654947).as_vd())
      .mla(s.0, (0.00833333333333318056201922).as_vd());

  x = ddadd_vd2_vd_vd2((1.).as_vd(), ddmul_vd2_vd2_vd2(ddadd_vd2_vd_vd((-0.166666666666666657414808).as_vd(), u*s.0), s));

  u = ddmul_vd_vd2_vd2(t, x);
  
  u = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (1).as_vi()), (1).as_vi())),
						       vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(u)));
  u = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), d, u);
  
  return u;
}

pub fn xcos(d: $f64x) -> $f64x {
  $f64x u, s, r = d;
  $i64x ql;

  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt((TRIGRANGEMAX2).as_vd())))) {
    let dql = (2.).as_vd().mla(
            vrint_vd_vd(d.mla((M_1_PI).as_vd(), (-0.5).as_vd())),
				   (1.).as_vd()
    );
    ql = vrint_vi_vd(dql);
    d = dql.mla((-PI_A2 * 0.5).as_vd(), d);
    d = dql.mla((-PI_B2 * 0.5).as_vd(), d);
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt((TRIGRANGEMAX).as_vd())))) {
    $f64x dqh = vtruncate_vd_vd(d.mla((M_1_PI / D1_23).as_vd(), (-M_1_PI / D1_24).as_vd()));
    ql = vrint_vi_vd(d*M_1_PI.as_vd() +
				   dqh.mla((-D1_23).as_vd(), (-0.5).as_vd()));
    dqh = dqh*D1_24.as_vd();
    ql = ql + ql + (1).as_vi();
    $f64x dql = vcast_vd_vi(ql);

    d = dqh.mla((-PI_A * 0.5).as_vd(), d);
    d = dql.mla((-PI_A * 0.5).as_vd(), d);
    d = dqh.mla((-PI_B * 0.5).as_vd(), d);
    d = dql.mla((-PI_B * 0.5).as_vd(), d);
    d = dqh.mla((-PI_C * 0.5).as_vd(), d);
    d = dql.mla((-PI_C * 0.5).as_vd(), d);
    d = (dqh + dql).mla((-PI_D * 0.5).as_vd(), d);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = vand_vi_vi_vi(ddii, (3).as_vi());
    ql = ql + ql + vsel_vi_vo_vi_vi(vcast_vo32_vo64(ddidd.0.gt((0.).as_vd())), (8).as_vi(), (7).as_vi());
    ql = vsra_vi_vi_i(ql, 1);
    $m64x o = veq_vo_vi_vi(vand_vi_vi_vi(ddii, (1).as_vi()), (0).as_vi());
    $f64x y = vsel_vd_vo_vd_vd(ddidd.0.gt((0.).as_vd()), (0.).as_vd(), (-1.).as_vd());
    VDouble2 x = vcast_vd2_vd_vd(vmulsign_vd_vd_vd((-3.141592653589793116 * 0.5).as_vd(), y), 
				 vmulsign_vd_vd_vd((-1.2246467991473532072e-16 * 0.5).as_vd(), y));
    x = ddadd2_vd2_vd2_vd2(ddidd, x);
    ddidd = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(o), x, ddidd);
    d = ddidd.0 + ddidd.1;
    d = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(r) | visnan_vo_vd(r), vreinterpret_vm_vd(d)));
  }

  s = d*d;

  d = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (2).as_vi()), (0).as_vi())), vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(d)));

  u = (-7.97255955009037868891952e-18).as_vd()
      .mla(s, (2.81009972710863200091251e-15).as_vd())
      .mla(s, (-7.64712219118158833288484e-13).as_vd())
      .mla(s, (1.60590430605664501629054e-10).as_vd())
      .mla(s, (-2.50521083763502045810755e-08).as_vd())
      .mla(s, (2.75573192239198747630416e-06).as_vd())
      .mla(s, (-0.000198412698412696162806809).as_vd())
      .mla(s, (0.00833333333333332974823815).as_vd())
      .mla(s, (-0.166666666666666657414808).as_vd());

  u = s*(u*d) + d;
  
  return u;
}

pub fn xcos_u1(d: $f64x) -> $f64x {
  $f64x u;
  VDouble2 s, t, x;
  $i64x ql;
  
  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt((TRIGRANGEMAX2).as_vd())))) {
    $f64x dql = vrint_vd_vd(d.mla(M_1_PI.as_vd(), (-0.5).as_vd()));
    dql = (2.).as_vd().mla(dql, (1.).as_vd());
    ql = vrint_vi_vd(dql);
    s = ddadd2_vd2_vd_vd(d, dql*(-PI_A2*0.5).as_vd());
    s = ddadd_vd2_vd2_vd(s, dql*(-PI_B2*0.5).as_vd());
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX.as_vd())))) {
    $f64x dqh = vtruncate_vd_vd(d.mla((M_1_PI / D1_23).as_vd(), (-M_1_PI / D1_24).as_vd()));
    ql = vrint_vi_vd(d*M_1_PI.as_vd() +
					dqh.mla((-D1_23).as_vd(), (-0.5).as_vd()));
    dqh = dqh*D1_24.as_vd();
    ql = ql + ql + (1).as_vi();
    const $f64x dql = vcast_vd_vi(ql);

    u = dqh.mla((-PI_A * 0.5).as_vd(), d);
    s = ddadd2_vd2_vd_vd(u, dql*(-PI_A*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dqh*(-PI_B*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dql*(-PI_B*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dqh*(-PI_C*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dql*(-PI_C*0.5).as_vd());
    s = ddadd_vd2_vd2_vd(s, (dqh + dql)*(-PI_D*0.5).as_vd());
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = vand_vi_vi_vi(ddii, (3).as_vi());
    ql = ql + ql + vsel_vi_vo_vi_vi(vcast_vo32_vo64(ddidd.0.gt((0.).as_vd())), (8).as_vi(), (7).as_vi());
    ql = vsra_vi_vi_i(ql, 1);
    $m64x o = veq_vo_vi_vi(vand_vi_vi_vi(ddii, (1).as_vi()), (0).as_vi());
    $f64x y = vsel_vd_vo_vd_vd(ddidd.0.gt((0.).as_vd()), (0.).as_vd(), (-1.).as_vd());
    VDouble2 x = vcast_vd2_vd_vd(vmulsign_vd_vd_vd((-3.141592653589793116 * 0.5).as_vd(), y), 
				 vmulsign_vd_vd_vd((-1.2246467991473532072e-16 * 0.5).as_vd(), y));
    x = ddadd2_vd2_vd2_vd2(ddidd, x);
    ddidd = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(o), x, ddidd);
    s = ddnormalize_vd2_vd2(ddidd);
    s.0 = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d) | visnan_vo_vd(d), vreinterpret_vm_vd(s.0)));
  }
  
  t = s;
  s = ddsqu_vd2_vd2(s);

  u = (2.72052416138529567917983e-15).as_vd()
      .mla(s.0, (-7.6429259411395447190023e-13).as_vd())
      .mla(s.0, (1.60589370117277896211623e-10).as_vd())
      .mla(s.0, (-2.5052106814843123359368e-08).as_vd())
      .mla(s.0, (2.75573192104428224777379e-06).as_vd())
      .mla(s.0, (-0.000198412698412046454654947).as_vd())
      .mla(s.0, (0.00833333333333318056201922).as_vd());

  x = ddadd_vd2_vd_vd2((1.).as_vd(), ddmul_vd2_vd2_vd2(ddadd_vd2_vd_vd((-0.166666666666666657414808).as_vd(), u*s.0), s));

  u = ddmul_vd_vd2_vd2(t, x);
  
  u = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (2).as_vi()), (0).as_vi())), vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(u)));
  
  return u;
}


pub fn xsincos(d: $f64x) -> ($f64x, $f64x) {
  $m64x o;
  $f64x u, t, rx, ry, s;
  VDouble2 r;
  $i64x ql;

  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX2.as_vd())))) {
    $f64x dql = vrint_vd_vd(d*(2 * M_1_PI).as_vd());
    ql = vrint_vi_vd(dql);
    s = dql.mla((-PI_A2 * 0.5).as_vd(), d);
    s = dql.mla((-PI_B2 * 0.5).as_vd(), s);
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX.as_vd())))) {
    $f64x dqh = vtruncate_vd_vd(d*(2*M_1_PI / D1_24).as_vd());
    dqh = dqh*D1_24.as_vd();
    $f64x dql = vrint_vd_vd(d*(2*M_1_PI).as_vd() - dqh);
    ql = vrint_vi_vd(dql);

    s = dqh.mla((-PI_A * 0.5).as_vd(), d);
    s = dql.mla((-PI_A * 0.5).as_vd(), s);
    s = dqh.mla((-PI_B * 0.5).as_vd(), s);
    s = dql.mla((-PI_B * 0.5).as_vd(), s);
    s = dqh.mla((-PI_C * 0.5).as_vd(), s);
    s = dql.mla((-PI_C * 0.5).as_vd(), s);
    s = (dqh + dql).mla((-PI_D * 0.5).as_vd(), s);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = ddii;
    s = ddidd.0 + ddidd.1;
    s = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d) | visnan_vo_vd(d), vreinterpret_vm_vd(s)));
  }
  
  t = s;

  s = s*s;

  u = (1.58938307283228937328511e-10).as_vd()
      .mla(s, (-2.50506943502539773349318e-08).as_vd())
      .mla(s, (2.75573131776846360512547e-06).as_vd())
      .mla(s, (-0.000198412698278911770864914).as_vd())
      .mla(s, (0.0083333333333191845961746).as_vd())
      .mla(s, (-0.166666666666666130709393).as_vd());

  rx = (u*s).mla(t, t);
  rx = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), (-0.).as_vd(), rx);

  u = (-1.13615350239097429531523e-11).as_vd()
      .mla(s, (2.08757471207040055479366e-09).as_vd())
      .mla(s, (-2.75573144028847567498567e-07).as_vd())
      .mla(s, (2.48015872890001867311915e-05).as_vd())
      .mla(s, (-0.00138888888888714019282329).as_vd())
      .mla(s, (0.0416666666666665519592062).as_vd())
      .mla(s, (-0.5).as_vd());

  ry = s.mla(u, (1.).as_vd());

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (1).as_vi()), (0).as_vi()));
  rsin = vsel_vd_vo_vd_vd(o, rx, ry);
  rcos = vsel_vd_vo_vd_vd(o, ry, rx);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (2).as_vi()), (2).as_vi()));
  rsin = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(rsin)));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql + (1).as_vi(), (2).as_vi()), (2).as_vi()));
  rcos = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(rcos)));
  
  (rsin, rcos)
}

pub fn xsincos_u1(d: $f64x) -> ($f64x, $f64x) {
  $m64x o;
  $f64x u, rx, ry;
  VDouble2 r, s, t, x;
  $i64x ql;
  
  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX2.as_vd())))) {
    const $f64x dql = vrint_vd_vd(d*(2 * M_1_PI).as_vd());
    ql = vrint_vi_vd(dql);
    u = dql.mla((-PI_A2*0.5).as_vd(), d);
    s = ddadd_vd2_vd_vd(u, dql*(-PI_B2*0.5).as_vd());
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX.as_vd())))) {
    $f64x dqh = vtruncate_vd_vd(d*(2*M_1_PI / D1_24).as_vd());
    dqh = dqh*D1_24.as_vd();
    const $f64x dql = vrint_vd_vd(d*(2*M_1_PI).as_vd() - dqh);
    ql = vrint_vi_vd(dql);
    
    u = dqh.mla((-PI_A * 0.5).as_vd(), d);
    s = ddadd_vd2_vd_vd(u, dql*(-PI_A*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dqh*(-PI_B*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dql*(-PI_B*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dqh*(-PI_C*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dql*(-PI_C*0.5).as_vd());
    s = ddadd_vd2_vd2_vd(s, (dqh + dql)*(-PI_D*0.5).as_vd());
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = ddii;
    s = ddidd;
    o = visinf_vo_vd(d) | visnan_vo_vd(d);
    s.0 = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(s.0)));
    s.1 = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(s.1)));
  }
  
  t = s;

  s.0 = ddsqu_vd_vd2(s);
  
  u = (1.58938307283228937328511e-10).as_vd()
      .mla(s.0, (-2.50506943502539773349318e-08).as_vd())
      .mla(s.0, (2.75573131776846360512547e-06).as_vd())
      .mla(s.0, (-0.000198412698278911770864914).as_vd())
      .mla(s.0, (0.0083333333333191845961746).as_vd())
      .mla(s.0, (-0.166666666666666130709393).as_vd());

  u = u*(s.0*t.0);

  x = ddadd_vd2_vd2_vd(t, u);
  rx = x.0 + x.1;

  rx = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), (-0.).as_vd(), rx);
  
  u = (-1.13615350239097429531523e-11).as_vd()
      .mla(s.0, (2.08757471207040055479366e-09).as_vd())
      .mla(s.0, (-2.75573144028847567498567e-07).as_vd())
      .mla(s.0, (2.48015872890001867311915e-05).as_vd())
      .mla(s.0, (-0.00138888888888714019282329).as_vd())
      .mla(s.0, (0.0416666666666665519592062).as_vd())
      .mla(s.0, (-0.5).as_vd());

  x = ddadd_vd2_vd_vd2((1.).as_vd(), ddmul_vd2_vd_vd(s.0, u));
  ry = x.0 + x.1;

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (1).as_vi()), (0).as_vi()));
  rsin = vsel_vd_vo_vd_vd(o, rx, ry);
  rcos = vsel_vd_vo_vd_vd(o, ry, rx);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (2).as_vi()), (2).as_vi()));
  rsin = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(rsin)));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql + (1).as_vi(), (2).as_vi()), (2).as_vi()));
  rcos = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(rcos)));

  (rsin, rcos)
}

pub fn xsincospi_u05(d: $f64x) -> ($f64x, $f64x) {
  $m64x o;
  $f64x u, s, t, rx, ry;
  VDouble2 r, x, s2;

  u = d*(4.).as_vd();
  $i64x q = vtruncate_vi_vd(u);
  q = vand_vi_vi_vi(q + vxor_vi_vi_vi(vsrl_vi_vi_i(q, 31), (1).as_vi()), vcast_vi_i(~1));
  s = u - vcast_vd_vi(q);
  
  t = s;
  s = s*s;
  s2 = t*t;
  
  //

  u = (-2.02461120785182399295868e-14).as_vd()
      .mla(s, (6.94821830580179461327784e-12).as_vd())
      .mla(s, (-1.75724749952853179952664e-09).as_vd())
      .mla(s, (3.13361688966868392878422e-07).as_vd())
      .mla(s, (-3.6576204182161551920361e-05).as_vd())
      .mla(s, (0.00249039457019271850274356).as_vd());
  x = ddadd2_vd2_vd_vd2(u*s, vcast_vd2_d_d(-0.0807455121882807852484731, 3.61852475067037104849987e-18));
  x = ddadd2_vd2_vd2_vd2(ddmul_vd2_vd2_vd2(s2, x), vcast_vd2_d_d(0.785398163397448278999491, 3.06287113727155002607105e-17));

  x = ddmul_vd2_vd2_vd(x, t);
  rx = x.0 + x.1;

  rx = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), (-0.).as_vd(), rx);
  
  //
  
  u = (9.94480387626843774090208e-16).as_vd()
      .mla(s, (-3.89796226062932799164047e-13).as_vd())
      .mla(s, (1.15011582539996035266901e-10).as_vd())
      .mla(s, (-2.4611369501044697495359e-08).as_vd())
      .mla(s, (3.59086044859052754005062e-06).as_vd())
      .mla(s, (-0.000325991886927389905997954).as_vd());
  x = ddadd2_vd2_vd_vd2(u*s, vcast_vd2_d_d(0.0158543442438155018914259, -1.04693272280631521908845e-18));
  x = ddadd2_vd2_vd2_vd2(ddmul_vd2_vd2_vd2(s2, x), vcast_vd2_d_d(-0.308425137534042437259529, -1.95698492133633550338345e-17));

  x = ddadd2_vd2_vd2_vd(ddmul_vd2_vd2_vd2(x, s2), (1.).as_vd());
  ry = x.0 + x.1;

  //

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (2).as_vi()), (0).as_vi()));
  rsin = vsel_vd_vo_vd_vd(o, rx, ry);
  rcos = vsel_vd_vo_vd_vd(o, ry, rx);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (4).as_vi()), (4).as_vi()));
  rsin = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(rsin)));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q + (2).as_vi(), (4).as_vi()), (4).as_vi()));
  rcos = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(rcos)));

  o = vabs_vd_vd(d).gt((TRIGRANGEMAX3/4.).as_vd());
  rsin = vreinterpret_vd_vm(vandnot_vm_vo64_vm(o, vreinterpret_vm_vd(rsin)));
  rcos = vsel_vd_vo_vd_vd(o, (1.).as_vd(), rcos);

  o = visinf_vo_vd(d);
  rsin = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(rsin)));
  rcos = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(rcos)));

  (rsin, rcos)
}

pub fn xsincospi_u35(d: $f64x) -> ($f64x, $f64x) {
  $m64x o;
  $f64x u, s, t, rx, ry;
  VDouble2 r;

  u = d*(4.).as_vd();
  $i64x q = vtruncate_vi_vd(u);
  q = vand_vi_vi_vi(q + vxor_vi_vi_vi(vsrl_vi_vi_i(q, 31), (1).as_vi()), (!1).as_vi());
  s = u - vcast_vd_vi(q);

  t = s;
  s = s*s;
  
  //

  u = (+0.6880638894766060136e-11).as_vd()
      .mla(s, (-0.1757159564542310199e-8).as_vd())
      .mla(s, (+0.3133616327257867311e-6).as_vd())
      .mla(s, (-0.3657620416388486452e-4).as_vd())
      .mla(s, (+0.2490394570189932103e-2).as_vd())
      .mla(s, (-0.8074551218828056320e-1).as_vd())
      .mla(s, (+0.7853981633974482790e+0).as_vd());

  rx = u*t;

  //
  
  u = (-0.3860141213683794352e-12).as_vd()
      .mla(s, (+0.1150057888029681415e-9).as_vd())
      .mla(s, (-0.2461136493006663553e-7).as_vd())
      .mla(s, (+0.3590860446623516713e-5).as_vd())
      .mla(s, (-0.3259918869269435942e-3).as_vd())
      .mla(s, (+0.1585434424381541169e-1).as_vd())
      .mla(s, (-0.3084251375340424373e+0).as_vd())
      .mla(s, (1.).as_vd());

  ry = u;

  //

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (2).as_vi()), (0).as_vi()));
  rsin = vsel_vd_vo_vd_vd(o, rx, ry);
  rcos = vsel_vd_vo_vd_vd(o, ry, rx);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (4).as_vi()), (4).as_vi()));
  rsin = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(rsin)));

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q + (2).as_vi(), (4).as_vi()), (4).as_vi()));
  rcos = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(rcos)));

  o = vabs_vd_vd(d).gt((TRIGRANGEMAX3/4.).as_vd());
  rsin = vreinterpret_vd_vm(vandnot_vm_vo64_vm(o, vreinterpret_vm_vd(rsin)));
  rcos = vreinterpret_vd_vm(vandnot_vm_vo64_vm(o, vreinterpret_vm_vd(rcos)));

  o = visinf_vo_vd(d);
  rsin = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(rsin)));
  rcos = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(rcos)));

  (rsin, rcos)
}

pub fn xmodf(x: $f64x) -> ($f64x, $f64x) {
  $f64x fr = x - D1_31.as_vd()*vcast_vd_vi(vtruncate_vi_vd(x*(1. / D1_31).as_vd()));
  fr -= vcast_vd_vi(vtruncate_vi_vd(fr));
  fr = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt(D1_52.as_vd()), (0.).as_vd(), fr);

  VDouble2 ret;

  retx = vcopysign_vd_vd_vd(fr, x);
  rety = vcopysign_vd_vd_vd(x - fr, x);

  (retx, rety)
}

#[inline]
fn sinpik(d: $f64x) -> VDouble2 {
  $m64x o;
  $f64x u, s, t;
  VDouble2 x, s2;

  u = d*(4.).as_vd();
  $i64x q = vtruncate_vi_vd(u);
  q = vand_vi_vi_vi(q + vxor_vi_vi_vi(vsrl_vi_vi_i(q, 31), (1).as_vi()), vcast_vi_i(~1));
  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (2).as_vi()), (2).as_vi()));

  s = u - vcast_vd_vi(q);
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
  x = ddadd2_vd2_vd_vd2(u*s,
			vsel_vd2_vo_d_d_d_d(o, 0.0158543442438155018914259, -1.04693272280631521908845e-18,
					    -0.0807455121882807852484731, 3.61852475067037104849987e-18));
  x = ddadd2_vd2_vd2_vd2(ddmul_vd2_vd2_vd2(s2, x),
			 vsel_vd2_vo_d_d_d_d(o, -0.308425137534042437259529, -1.95698492133633550338345e-17,
					     0.785398163397448278999491, 3.06287113727155002607105e-17));

  x = ddmul_vd2_vd2_vd2(x, vsel_vd2_vo_vd2_vd2(o, s2, vcast_vd2_vd_vd(t, (0.).as_vd())));
  x = vsel_vd2_vo_vd2_vd2(o, ddadd2_vd2_vd2_vd(x, (1.).as_vd()), x);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (4).as_vi()), (4).as_vi()));
  x.0 = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(x.0)));
  x.1 = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(x.1)));

  return x;
}

pub fn xsinpi_u05(d: $f64x) -> $f64x {
  VDouble2 x = sinpik(d);
  $f64x r = x.0 + x.1;

  r = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), (-0.).as_vd(), r);
  r = vreinterpret_vd_vm(vandnot_vm_vo64_vm(vabs_vd_vd(d).gt((TRIGRANGEMAX3/4.).as_vd()), vreinterpret_vm_vd(r)));
  r = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d), vreinterpret_vm_vd(r)));
  
  return r;
}
#[inline]
fn cospik(d: $f64x) -> VDouble2 {
  $m64x o;
  $f64x u, s, t;
  VDouble2 x, s2;

  u = d*(4.).as_vd();
  $i64x q = vtruncate_vi_vd(u);
  q = vand_vi_vi_vi(q + vxor_vi_vi_vi(vsrl_vi_vi_i(q, 31), (1).as_vi()), vcast_vi_i(~1));
  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (2).as_vi()), (0).as_vi()));

  s = u - vcast_vd_vi(q);
  t = s;
  s = s*s;
  s2 = ddmul_vd2_vd_vd(t, t);
  
  //

  u = vsel_vd_vo_d_d(o, 9.94480387626843774090208e-16, -2.02461120785182399295868e-14)
      .mla(s, vsel_vd_vo_d_d(o, -3.89796226062932799164047e-13, 6.948218305801794613277840e-12))
      .mla(s, vsel_vd_vo_d_d(o, 1.150115825399960352669010e-10, -1.75724749952853179952664e-09))
      .mla(s, vsel_vd_vo_d_d(o, -2.46113695010446974953590e-08, 3.133616889668683928784220e-07))
      .mla(s, vsel_vd_vo_d_d(o, 3.590860448590527540050620e-06, -3.65762041821615519203610e-05))
      .mla(s, vsel_vd_vo_d_d(o, -0.000325991886927389905997954, 0.0024903945701927185027435600));
  x = ddadd2_vd2_vd_vd2(u*s,
			vsel_vd2_vo_d_d_d_d(o, 0.0158543442438155018914259, -1.04693272280631521908845e-18,
					    -0.0807455121882807852484731, 3.61852475067037104849987e-18));
  x = ddadd2_vd2_vd2_vd2(ddmul_vd2_vd2_vd2(s2, x),
			 vsel_vd2_vo_d_d_d_d(o, -0.308425137534042437259529, -1.95698492133633550338345e-17,
					     0.785398163397448278999491, 3.06287113727155002607105e-17));

  x = ddmul_vd2_vd2_vd2(x, vsel_vd2_vo_vd2_vd2(o, s2, vcast_vd2_vd_vd(t, (0.).as_vd())));
  x = vsel_vd2_vo_vd2_vd2(o, ddadd2_vd2_vd2_vd(x, (1.).as_vd()), x);

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q + (2).as_vi(), (4).as_vi()), (4).as_vi()));
  x.0 = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(x.0)));
  x.1 = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(x.1)));

  return x;
}

pub fn xcospi_u05(d: $f64x) -> $f64x {
  VDouble2 x = cospik(d);
  $f64x r = x.0 + x.1;

  r = vsel_vd_vo_vd_vd(vabs_vd_vd(d).gt((TRIGRANGEMAX3/4.).as_vd()), (1.).as_vd(), r);
  r = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d), vreinterpret_vm_vd(r)));
  
  return r;
}

pub fn xtan(d: $f64x) -> $f64x {
  $f64x u, s, x;
  $m64x o;
  $i64x ql;

  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX2.as_vd())))) {
    $f64x dql = vrint_vd_vd(d*(2 * M_1_PI).as_vd());
    ql = vrint_vi_vd(dql);
    x = dql.mla((-PI_A2 * 0.5).as_vd(), d);
    x = dql.mla((-PI_B2 * 0.5).as_vd(), x);
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt((1e+7).as_vd())))) {
    $f64x dqh = vtruncate_vd_vd(d*(2*M_1_PI / D1_24).as_vd());
    dqh = dqh*D1_24.as_vd();
    $f64x dql = vrint_vd_vd(d*(2*M_1_PI).as_vd() - dqh);
    ql = vrint_vi_vd(dql);

    x = dqh.mla((-PI_A * 0.5).as_vd(), d);
    x = dql.mla((-PI_A * 0.5).as_vd(), x);
    x = dqh.mla((-PI_B * 0.5).as_vd(), x);
    x = dql.mla((-PI_B * 0.5).as_vd(), x);
    x = dqh.mla((-PI_C * 0.5).as_vd(), x);
    x = dql.mla((-PI_C * 0.5).as_vd(), x);
    x = (dqh + dql).mla((-PI_D * 0.5).as_vd(), x);
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = ddii;
    x = ddidd.0 + ddidd.1;
    x = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d), vreinterpret_vm_vd(x)));
    x = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(d) | visnan_vo_vd(d), vreinterpret_vm_vd(x)));
  }

  s = x*x;

  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (1).as_vi()), (1).as_vi()));
  x = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(x)));

#ifdef SPLIT_KERNEL
  $f64x s2 = s*s;
  v;

  u = (-4.31184585467324750724175e-05).as_vd()
      .mla(s2, (-0.000137892809714281708733524).as_vd())
      .mla(s2, (-6.07500301486087879295969e-05).as_vd())
      .mla(s2, (0.000219040550724571513561967).as_vd())
      .mla(s2, (0.00145461240472358871965441).as_vd())
      .mla(s2, (0.00886321546662684547901456).as_vd())
      .mla(s2, (0.0539682539049961967903002).as_vd())
      .mla(s2, (0.333333333333320047664472).as_vd());

  v = (9.99583485362149960784268e-06).as_vd()
      .mla(s2, (0.000103573238391744000389851).as_vd())
      .mla(s2, (0.000157624358465342784274554).as_vd())
      .mla(s2, (0.000148898734751616411290179).as_vd())
      .mla(s2, (0.000595799595197098359744547).as_vd())
      .mla(s2, (0.0035923150771440177410343).as_vd())
      .mla(s2, (0.0218694899718446938985394).as_vd())
      .mla(s2, (0.133333333334818976423364).as_vd());
  
  u = v.mla(s, u);
#else
  u = (9.99583485362149960784268e-06).as_vd()
      .mla(s, (-4.31184585467324750724175e-05).as_vd())
      .mla(s, (0.000103573238391744000389851).as_vd())
      .mla(s, (-0.000137892809714281708733524).as_vd())
      .mla(s, (0.000157624358465342784274554).as_vd())
      .mla(s, (-6.07500301486087879295969e-05).as_vd())
      .mla(s, (0.000148898734751616411290179).as_vd())
      .mla(s, (0.000219040550724571513561967).as_vd())
      .mla(s, (0.000595799595197098359744547).as_vd())
      .mla(s, (0.00145461240472358871965441).as_vd())
      .mla(s, (0.0035923150771440177410343).as_vd())
      .mla(s, (0.00886321546662684547901456).as_vd())
      .mla(s, (0.0218694899718446938985394).as_vd())
      .mla(s, (0.0539682539049961967903002).as_vd())
      .mla(s, (0.133333333334818976423364).as_vd())
      .mla(s, (0.333333333333320047664472).as_vd());
#endif
  
  u = s.mla(u*x, x);

  u = vsel_vd_vo_vd_vd(o, vrec_vd_vd(u), u);
  u = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), d, u);
  
  return u;
}

pub fn xtan_u1(d: $f64x) -> $f64x {
  $f64x u;
  VDouble2 s, t, x;
  $m64x o;
  $i64x ql;
  
  if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX2.as_vd())))) {
    $f64x dql = vrint_vd_vd(d*(2 * M_1_PI).as_vd());
    ql = vrint_vi_vd(dql);
    u = dql.mla((-PI_A2*0.5).as_vd(), d);
    s = ddadd_vd2_vd_vd (u, dql*(-PI_B2*0.5).as_vd());
  } else if (LIKELY(vtestallones_i_vo64(vabs_vd_vd(d).lt(TRIGRANGEMAX.as_vd())))) {
    $f64x dqh = vtruncate_vd_vd(d*(2*M_1_PI / D1_24).as_vd());
    dqh = dqh*D1_24.as_vd();
    s = ddadd2_vd2_vd2_vd(ddmul_vd2_vd2_vd(vcast_vd2_d_d(M_2_PI_H, M_2_PI_L), d),
			  vsel_vd_vo_vd_vd(d.lt((0.).as_vd()),
							 (-0.5).as_vd(), (0.5).as_vd()) - dqh);
    const $f64x dql = vtruncate_vd_vd(s.0 + s.1);
    ql = vrint_vi_vd(dql);

    u = dqh.mla((-PI_A * 0.5).as_vd(), d);
    s = ddadd_vd2_vd_vd(u, dql*(-PI_A*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dqh*(-PI_B*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dql*(-PI_B*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dqh*(-PI_C*0.5).as_vd());
    s = ddadd2_vd2_vd2_vd(s, dql*(-PI_C*0.5).as_vd());
    s = ddadd_vd2_vd2_vd(s, (dqh + dql)*(-PI_D*0.5).as_vd());
  } else {
    let (ddidd, ddii) = rempi(d);
    ql = ddii;
    s = ddidd;
    o = visinf_vo_vd(d) | visnan_vo_vd(d);
    s.0 = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(s.0)));
    s.1 = vreinterpret_vd_vm(vor_vm_vo64_vm(o, vreinterpret_vm_vd(s.1)));
  }
  
  o = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(ql, (1).as_vi()), (1).as_vi()));
  vmask n = vand_vm_vo64_vm(o, vreinterpret_vm_vd((-0.).as_vd()));
  s.0 = vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(s.0), n));
  s.1 = vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(s.1), n));

  t = s;
  s = ddsqu_vd2_vd2(s);

#ifdef SPLIT_KERNEL
  $f64x sx2 = s.0*s.0;
  v;

  u = (-2.59519791585924697698614e-05).as_vd()
      .mla(sx2, (-3.05033014433946488225616e-05).as_vd())
      .mla(sx2, (8.09674518280159187045078e-05).as_vd())
      .mla(sx2, (0.000588505168743587154904506).as_vd())
      .mla(sx2, (0.00359208743836906619142924).as_vd())
      .mla(sx2, (0.0218694882853846389592078).as_vd())
      .mla(sx2, (0.133333333333125941821962).as_vd());

  v = (1.01419718511083373224408e-05).as_vd()
      .mla(sx2, (5.23388081915899855325186e-05).as_vd())
      .mla(sx2, (7.14707504084242744267497e-05).as_vd())
      .mla(sx2, (0.000244884931879331847054404).as_vd())
      .mla(sx2, (0.00145612788922812427978848).as_vd())
      .mla(sx2, (0.00886323944362401618113356).as_vd())
      .mla(sx2, (0.0539682539781298417636002).as_vd());

  u = v.mla(s.0, u);
#else
  u = (1.01419718511083373224408e-05).as_vd()
      .mla(s.0, (-2.59519791585924697698614e-05).as_vd())
      .mla(s.0, (5.23388081915899855325186e-05).as_vd())
      .mla(s.0, (-3.05033014433946488225616e-05).as_vd())
      .mla(s.0, (7.14707504084242744267497e-05).as_vd())
      .mla(s.0, (8.09674518280159187045078e-05).as_vd())
      .mla(s.0, (0.000244884931879331847054404).as_vd())
      .mla(s.0, (0.000588505168743587154904506).as_vd())
      .mla(s.0, (0.00145612788922812427978848).as_vd())
      .mla(s.0, (0.00359208743836906619142924).as_vd())
      .mla(s.0, (0.00886323944362401618113356).as_vd())
      .mla(s.0, (0.0218694882853846389592078).as_vd())
      .mla(s.0, (0.0539682539781298417636002).as_vd())
      .mla(s.0, (0.133333333333125941821962).as_vd());
#endif
  
  x = ddadd_vd2_vd_vd2((1.).as_vd(), ddmul_vd2_vd2_vd2(ddadd_vd2_vd_vd((0.333333333333334980164153).as_vd(), u*s.0), s));
  x = ddmul_vd2_vd2_vd2(t, x);

  x = vsel_vd2_vo_vd2_vd2(o, ddrec_vd2_vd2(x), x);

  u = x.0 + x.1;

  u = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), d, u);

  return u;
}
#[inline]
fn atan2k(y: $f64x, x: $f64x) -> $f64x {
  $f64x s, t, u;
  $i64x q;
  $m64x p;

  q = vsel_vi_vd_vi(x, vcast_vi_i(-2));
  x = vabs_vd_vd(x);

  q = vsel_vi_vd_vd_vi_vi(x, y, q + (1).as_vi(), q);
  p = x.lt(y);
  s = vsel_vd_vo_vd_vd(p, -x, y);
  t = x.max(y);

  s = s / t;
  t = s*s;

#ifdef SPLIT_KERNEL
  $f64x t2 = t*t;
  v;

  u = (-1.88796008463073496563746e-05).as_vd()
      .mla(t2, (-0.00110611831486672482563471).as_vd())
      .mla(t2, (-0.00889896195887655491740809).as_vd())
      .mla(t2, (-0.0254517624932312641616861).as_vd())
      .mla(t2, (-0.0407629191276836500001934).as_vd())
      .mla(t2, (-0.0523674852303482457616113).as_vd())
      .mla(t2, (-0.0666573579361080525984562).as_vd())
      .mla(t2, (-0.090908995008245008229153).as_vd())
      .mla(t2, (-0.14285714266771329383765).as_vd())
      .mla(t2, (-0.333333333333311110369124).as_vd());

  v = (0.000209850076645816976906797).as_vd()
      .mla(t2, (0.00370026744188713119232403).as_vd())
      .mla(t2, (0.016599329773529201970117).as_vd())
      .mla(t2, (0.0337852580001353069993897).as_vd())
      .mla(t2, (0.0466667150077840625632675).as_vd())
      .mla(t2, (0.0587666392926673580854313).as_vd())
      .mla(t2, (0.0769219538311769618355029).as_vd())
      .mla(t2, (0.111111105648261418443745).as_vd())
      .mla(t2, (0.199999999996591265594148).as_vd());

  u = v.mla(t, u);
#else
  u = (-1.88796008463073496563746e-05).as_vd()
      .mla(t, (0.000209850076645816976906797).as_vd())
      .mla(t, (-0.00110611831486672482563471).as_vd())
      .mla(t, (0.00370026744188713119232403).as_vd())
      .mla(t, (-0.00889896195887655491740809).as_vd())
      .mla(t, (0.016599329773529201970117).as_vd())
      .mla(t, (-0.0254517624932312641616861).as_vd())
      .mla(t, (0.0337852580001353069993897).as_vd())
      .mla(t, (-0.0407629191276836500001934).as_vd())
      .mla(t, (0.0466667150077840625632675).as_vd())
      .mla(t, (-0.0523674852303482457616113).as_vd())
      .mla(t, (0.0587666392926673580854313).as_vd())
      .mla(t, (-0.0666573579361080525984562).as_vd())
      .mla(t, (0.0769219538311769618355029).as_vd())
      .mla(t, (-0.090908995008245008229153).as_vd())
      .mla(t, (0.111111105648261418443745).as_vd())
      .mla(t, (-0.14285714266771329383765).as_vd())
      .mla(t, (0.199999999996591265594148).as_vd())
      .mla(t, (-0.333333333333311110369124).as_vd());
#endif
  
  t = s.mla(t*u, s);
  t = vcast_vd_vi(q).mla((M_PI/2.).as_vd(), t);

  return t;
}
#[inline]
fn atan2k_u1(VDouble2 y, VDouble2 x) -> VDouble2 {
  $f64x u;
  VDouble2 s, t;
  $i64x q;
  $m64x p;

  q = vsel_vi_vd_vi(x.0, vcast_vi_i(-2));
  p = x.0.lt((0.).as_vd());
  vmask b = vand_vm_vo64_vm(p, vreinterpret_vm_vd((-0.).as_vd()));
  x.0 = vreinterpret_vd_vm(vxor_vm_vm_vm(b, vreinterpret_vm_vd(x.0)));
  x.1 = vreinterpret_vd_vm(vxor_vm_vm_vm(b, vreinterpret_vm_vd(x.1)));

  q = vsel_vi_vd_vd_vi_vi(x.0, y.0, q + (1).as_vi(), q);
  p = x.0.lt(y.0);
  s = vsel_vd2_vo_vd2_vd2(p, ddneg_vd2_vd2(x), y);
  t = vsel_vd2_vo_vd2_vd2(p, y, x);

  s = dddiv_vd2_vd2_vd2(s, t);
  t = ddsqu_vd2_vd2(s);
  t = ddnormalize_vd2_vd2(t);

#ifdef SPLIT_KERNEL
  $f64x tx3 = t.0*t.0*t.0;
  v;

  u = (0.00070557664296393412389774).as_vd()
      .mla(t.0, (-0.00251865614498713360352999).as_vd())
      .mla(tx3, (0.0208024799924145797902497).as_vd())
      .mla(t.0, (-0.0289002344784740315686289).as_vd())
      .mla(tx3, (0.0470843011653283988193763).as_vd())
      .mla(t.0, (-0.0524914210588448421068719).as_vd())
      .mla(tx3, (0.0769225330296203768654095).as_vd())
      .mla(t.0, (-0.0909090442773387574781907).as_vd())
      .mla(tx3, (0.199999999997977351284817).as_vd())
      .mla(t.0, (-0.333333333333317605173818).as_vd());
  
  v = (1.06298484191448746607415e-05).as_vd()
      .mla(t.0, (-0.000125620649967286867384336).as_vd())
      .mla(tx3, (0.00646262899036991172313504).as_vd())
      .mla(t.0, (-0.0128281333663399031014274).as_vd())
      .mla(tx3, (0.0359785005035104590853656).as_vd())
      .mla(t.0, (-0.041848579703592507506027).as_vd())
      .mla(tx3, (0.0587946590969581003860434).as_vd())
      .mla(t.0, (-0.0666620884778795497194182).as_vd())
      .mla(tx3, (0.111111108376896236538123).as_vd())
      .mla(t.0, (-0.142857142756268568062339).as_vd());
  
  u = v.mla(t.0*t.0, u);
#else
  u = (1.06298484191448746607415e-05).as_vd()
      .mla(t.0, (-0.000125620649967286867384336).as_vd())
      .mla(t.0, (0.00070557664296393412389774).as_vd())
      .mla(t.0, (-0.00251865614498713360352999).as_vd())
      .mla(t.0, (0.00646262899036991172313504).as_vd())
      .mla(t.0, (-0.0128281333663399031014274).as_vd())
      .mla(t.0, (0.0208024799924145797902497).as_vd())
      .mla(t.0, (-0.0289002344784740315686289).as_vd())
      .mla(t.0, (0.0359785005035104590853656).as_vd())
      .mla(t.0, (-0.041848579703592507506027).as_vd())
      .mla(t.0, (0.0470843011653283988193763).as_vd())
      .mla(t.0, (-0.0524914210588448421068719).as_vd())
      .mla(t.0, (0.0587946590969581003860434).as_vd())
      .mla(t.0, (-0.0666620884778795497194182).as_vd())
      .mla(t.0, (0.0769225330296203768654095).as_vd())
      .mla(t.0, (-0.0909090442773387574781907).as_vd())
      .mla(t.0, (0.111111108376896236538123).as_vd())
      .mla(t.0, (-0.142857142756268568062339).as_vd())
      .mla(t.0, (0.199999999997977351284817).as_vd())
      .mla(t.0, (-0.333333333333317605173818).as_vd());
#endif
  
  t = ddmul_vd2_vd2_vd(t, u);
  t = ddmul_vd2_vd2_vd2(s, ddadd_vd2_vd_vd2((1.).as_vd(), t));
  t = ddadd_vd2_vd2_vd2(ddmul_vd2_vd2_vd(vcast_vd2_d_d(1.570796326794896557998982, 6.12323399573676603586882e-17), vcast_vd_vi(q)), t);

  return t;
}
#[inline]
fn visinf2_vd_vd_vd(d: $f64x, m: $f64x) -> $f64x {
  return vreinterpret_vd_vm(vand_vm_vo64_vm(visinf_vo_vd(d), vor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(m))));
}

pub fn xatan2(y: $f64x, x: $f64x) -> $f64x {
  $f64x r = atan2k(vabs_vd_vd(y), x);

  r = vmulsign_vd_vd_vd(r, x);
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(x) | x.eq((0.).as_vd()), (M_PI/2.).as_vd() - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd((M_PI/2.).as_vd(), x)), r);
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(y), (M_PI/2.).as_vd() - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd((M_PI/4.).as_vd(), x)), r);
  r = vsel_vd_vo_vd_vd(y.eq((0.).as_vd()), vreinterpret_vd_vm(vand_vm_vo64_vm(vsignbit_vo_vd(x), vreinterpret_vm_vd(M_PI.as_vd()))), r);

  r = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x) | visnan_vo_vd(y), vreinterpret_vm_vd(vmulsign_vd_vd_vd(r, y))));
  return r;
}

pub fn xatan2_u1(y: $f64x, x: $f64x) -> $f64x {
  $m64x o = vabs_vd_vd(x).lt((5.5626846462680083984e-309).as_vd()); // nexttoward((1.0 / DBL_MAX), 1)
  x = vsel_vd_vo_vd_vd(o, x*D1_53.as_vd(), x);
  y = vsel_vd_vo_vd_vd(o, y*D1_23.as_vd(), y);

  VDouble2 d = atan2k_u1(vcast_vd2_vd_vd(vabs_vd_vd(y), (0.).as_vd()), vcast_vd2_vd_vd(x, (0.).as_vd()));
  $f64x r = d.0 + d.1;

  r = vmulsign_vd_vd_vd(r, x);
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(x) | x.eq((0.).as_vd()), (M_PI/2.).as_vd() - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd((M_PI/2.).as_vd(), x)), r);
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(y), (M_PI/2.).as_vd() - visinf2_vd_vd_vd(x, vmulsign_vd_vd_vd((M_PI/4.).as_vd(), x)), r);
  r = vsel_vd_vo_vd_vd(y.eq((0.).as_vd()), vreinterpret_vd_vm(vand_vm_vo64_vm(vsignbit_vo_vd(x), vreinterpret_vm_vd(M_PI.as_vd()))), r);

  r = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x) | visnan_vo_vd(y), vreinterpret_vm_vd(vmulsign_vd_vd_vd(r, y))));
  return r;
}

pub fn xasin(d: $f64x) -> $f64x {
  $m64x o = vabs_vd_vd(d).lt((0.5).as_vd());
  $f64x x2 = vsel_vd_vo_vd_vd(o, d*d, ((1.).as_vd() - vabs_vd_vd(d))*(0.5).as_vd());
  $f64x x = vsel_vd_vo_vd_vd(o, vabs_vd_vd(d), vsqrt_vd_vd(x2)), u;

#ifdef SPLIT_KERNEL
  $f64x x4 = x2*x2
  v;

  u = (-0.1581918243329996643e-1).as_vd()
      .mla(x4, (+0.6606077476277170610e-2).as_vd())
      .mla(x4, (+0.1388715184501609218e-1).as_vd())
      .mla(x4, (+0.2237176181932048341e-1).as_vd())
      .mla(x4, (+0.4464285681377102438e-1).as_vd())
      .mla(x4, (+0.1666666666666497543e+0).as_vd());
  
  v = (+0.3161587650653934628e-1).as_vd()
      .mla(x4, (+0.1929045477267910674e-1).as_vd())
      .mla(x4, (+0.1215360525577377331e-1).as_vd())
      .mla(x4, (+0.1735956991223614604e-1).as_vd())
      .mla(x4, (+0.3038195928038132237e-1).as_vd())
      .mla(x4, (+0.7500000000378581611e-1).as_vd());

  u = v.mla(x2, u);
#else
  u = (+0.3161587650653934628e-1).as_vd()
      .mla(x2, (-0.1581918243329996643e-1).as_vd())
      .mla(x2, (+0.1929045477267910674e-1).as_vd())
      .mla(x2, (+0.6606077476277170610e-2).as_vd())
      .mla(x2, (+0.1215360525577377331e-1).as_vd())
      .mla(x2, (+0.1388715184501609218e-1).as_vd())
      .mla(x2, (+0.1735956991223614604e-1).as_vd())
      .mla(x2, (+0.2237176181932048341e-1).as_vd())
      .mla(x2, (+0.3038195928038132237e-1).as_vd())
      .mla(x2, (+0.4464285681377102438e-1).as_vd())
      .mla(x2, (+0.7500000000378581611e-1).as_vd())
      .mla(x2, (+0.1666666666666497543e+0).as_vd());
#endif

  u = u.mla(x*x2, x);
  
  $f64x r = vsel_vd_vo_vd_vd(o, u, u.mla((-2.).as_vd(), (M_PI/2.).as_vd()));
  return vmulsign_vd_vd_vd(r, d);
}

pub fn xasin_u1(d: $f64x) -> $f64x {
  $m64x o = vabs_vd_vd(d).lt((0.5).as_vd());
  $f64x x2 = vsel_vd_vo_vd_vd(o, d*d, ((1.).as_vd()-vabs_vd_vd(d))*(0.5).as_vd());
  u;
  VDouble2 x = vsel_vd2_vo_vd2_vd2(o, vcast_vd2_vd_vd(vabs_vd_vd(d), (0.).as_vd()), ddsqrt_vd2_vd(x2));
  x = vsel_vd2_vo_vd2_vd2(vabs_vd_vd(d).eq((1.).as_vd()), vcast_vd2_d_d(0, 0), x);

#ifdef SPLIT_KERNEL
  $f64x x4 = x2*x2;
  v;

  u = (-0.1581918243329996643e-1).as_vd()
      .mla(x4, (+0.6606077476277170610e-2).as_vd())
      .mla(x4, (+0.1388715184501609218e-1).as_vd())
      .mla(x4, (+0.2237176181932048341e-1).as_vd())
      .mla(x4, (+0.4464285681377102438e-1).as_vd())
      .mla(x4, (+0.1666666666666497543e+0).as_vd());
  
  v = (+0.3161587650653934628e-1).as_vd()
      .mla(x4, (+0.1929045477267910674e-1).as_vd())
      .mla(x4, (+0.1215360525577377331e-1).as_vd())
      .mla(x4, (+0.1735956991223614604e-1).as_vd())
      .mla(x4, (+0.3038195928038132237e-1).as_vd())
      .mla(x4, (+0.7500000000378581611e-1).as_vd());

  u = v.mla(x2, u);
#else
  u = (+0.3161587650653934628e-1).as_vd()
      .mla(x2, (-0.1581918243329996643e-1).as_vd())
      .mla(x2, (+0.1929045477267910674e-1).as_vd())
      .mla(x2, (+0.6606077476277170610e-2).as_vd())
      .mla(x2, (+0.1215360525577377331e-1).as_vd())
      .mla(x2, (+0.1388715184501609218e-1).as_vd())
      .mla(x2, (+0.1735956991223614604e-1).as_vd())
      .mla(x2, (+0.2237176181932048341e-1).as_vd())
      .mla(x2, (+0.3038195928038132237e-1).as_vd())
      .mla(x2, (+0.4464285681377102438e-1).as_vd())
      .mla(x2, (+0.7500000000378581611e-1).as_vd())
      .mla(x2, (+0.1666666666666497543e+0).as_vd());
#endif

  u *= (x2*x.0);

  VDouble2 y = ddsub_vd2_vd2_vd(ddsub_vd2_vd2_vd2(vcast_vd2_d_d(3.141592653589793116/4, 1.2246467991473532072e-16/4), x), u);
  
  $f64x r = vsel_vd_vo_vd_vd(o, u + x.0,
			       (y.0 + y.1)*(2.).as_vd());
  return vmulsign_vd_vd_vd(r, d);
}

pub fn xacos(d: $f64x) -> $f64x {
  $m64x o = vabs_vd_vd(d).lt((0.5).as_vd());
  $f64x x2 = vsel_vd_vo_vd_vd(o, d*d,
				((1.).as_vd() - vabs_vd_vd(d))*(0.5).as_vd()), u;
  $f64x x = vsel_vd_vo_vd_vd(o, vabs_vd_vd(d), vsqrt_vd_vd(x2));
  x = vsel_vd_vo_vd_vd(vabs_vd_vd(d).eq((1.).as_vd()), (0.).as_vd(), x);

#ifdef SPLIT_KERNEL
  $f64x x4 = x2*x2;
  v;

  u = (-0.1581918243329996643e-1).as_vd()
      .mla(x4, (+0.6606077476277170610e-2).as_vd())
      .mla(x4, (+0.1388715184501609218e-1).as_vd())
      .mla(x4, (+0.2237176181932048341e-1).as_vd())
      .mla(x4, (+0.4464285681377102438e-1).as_vd())
      .mla(x4, (+0.1666666666666497543e+0).as_vd());
  
  v = (+0.3161587650653934628e-1).as_vd()
      .mla(x4, (+0.1929045477267910674e-1).as_vd())
      .mla(x4, (+0.1215360525577377331e-1).as_vd())
      .mla(x4, (+0.1735956991223614604e-1).as_vd())
      .mla(x4, (+0.3038195928038132237e-1).as_vd())
      .mla(x4, (+0.7500000000378581611e-1).as_vd());

  u = v.mla(x2, u);
#else
  u = (+0.3161587650653934628e-1).as_vd()
      .mla(x2, (-0.1581918243329996643e-1).as_vd())
      .mla(x2, (+0.1929045477267910674e-1).as_vd())
      .mla(x2, (+0.6606077476277170610e-2).as_vd())
      .mla(x2, (+0.1215360525577377331e-1).as_vd())
      .mla(x2, (+0.1388715184501609218e-1).as_vd())
      .mla(x2, (+0.1735956991223614604e-1).as_vd())
      .mla(x2, (+0.2237176181932048341e-1).as_vd())
      .mla(x2, (+0.3038195928038132237e-1).as_vd())
      .mla(x2, (+0.4464285681377102438e-1).as_vd())
      .mla(x2, (+0.7500000000378581611e-1).as_vd())
      .mla(x2, (+0.1666666666666497543e+0).as_vd());
#endif

  u *= x2*x;

  $f64x y = (M_PI/2.).as_vd() - (vmulsign_vd_vd_vd(x, d) + vmulsign_vd_vd_vd(u, d));
  x = x + u;
  $f64x r = vsel_vd_vo_vd_vd(o, y, x*(2.).as_vd());
  return vsel_vd_vo_vd_vd(vandnot_vo_vo_vo(o, d.lt((0.).as_vd())),
			  ddadd_vd2_vd2_vd(vcast_vd2_d_d(3.141592653589793116, 1.2246467991473532072e-16),
					   -r).0, r);
}

pub fn xacos_u1(d: $f64x) -> $f64x {
  $m64x o = vabs_vd_vd(d).lt((0.5).as_vd());
  $f64x x2 = vsel_vd_vo_vd_vd(o, d*d, ((1.).as_vd() - vabs_vd_vd(d))*(0.5).as_vd()), u;
  VDouble2 x = vsel_vd2_vo_vd2_vd2(o, vcast_vd2_vd_vd(vabs_vd_vd(d), (0.).as_vd()), ddsqrt_vd2_vd(x2));
  x = vsel_vd2_vo_vd2_vd2(vabs_vd_vd(d).eq((1.).as_vd()), vcast_vd2_d_d(0, 0), x);

#ifdef SPLIT_KERNEL
  $f64x x4 = x2*x2;
  v;

  u = (-0.1581918243329996643e-1).as_vd()
      .mla(x4, (+0.6606077476277170610e-2).as_vd())
      .mla(x4, (+0.1388715184501609218e-1).as_vd())
      .mla(x4, (+0.2237176181932048341e-1).as_vd())
      .mla(x4, (+0.4464285681377102438e-1).as_vd())
      .mla(x4, (+0.1666666666666497543e+0).as_vd());
  
  v = (+0.3161587650653934628e-1).as_vd()
      .mla(x4, (+0.1929045477267910674e-1).as_vd())
      .mla(x4, (+0.1215360525577377331e-1).as_vd())
      .mla(x4, (+0.1735956991223614604e-1).as_vd())
      .mla(x4, (+0.3038195928038132237e-1).as_vd())
      .mla(x4, (+0.7500000000378581611e-1).as_vd());

  u = v.mla(x2, u);
#else
  u = (+0.3161587650653934628e-1).as_vd()
      .mla(x2, (-0.1581918243329996643e-1).as_vd())
      .mla(x2, (+0.1929045477267910674e-1).as_vd())
      .mla(x2, (+0.6606077476277170610e-2).as_vd())
      .mla(x2, (+0.1215360525577377331e-1).as_vd())
      .mla(x2, (+0.1388715184501609218e-1).as_vd())
      .mla(x2, (+0.1735956991223614604e-1).as_vd())
      .mla(x2, (+0.2237176181932048341e-1).as_vd())
      .mla(x2, (+0.3038195928038132237e-1).as_vd())
      .mla(x2, (+0.4464285681377102438e-1).as_vd())
      .mla(x2, (+0.7500000000378581611e-1).as_vd())
      .mla(x2, (+0.1666666666666497543e+0).as_vd());
#endif

  u *= (x2*x.0);

  VDouble2 y = ddsub_vd2_vd2_vd2(vcast_vd2_d_d(3.141592653589793116/2, 1.2246467991473532072e-16/2),
				 ddadd_vd2_vd_vd(vmulsign_vd_vd_vd(x.0, d), vmulsign_vd_vd_vd(u, d)));
  x = ddadd_vd2_vd2_vd(x, u);
  
  y = vsel_vd2_vo_vd2_vd2(o, y, ddscale_vd2_vd2_vd(x, (2.).as_vd()));
  
  y = vsel_vd2_vo_vd2_vd2(vandnot_vo_vo_vo(o, d.lt((0.).as_vd())),
			  ddsub_vd2_vd2_vd2(vcast_vd2_d_d(3.141592653589793116, 1.2246467991473532072e-16), y), y);

  return y.0 + y.1;
}

pub fn xatan_u1(d: $f64x) -> $f64x {
  VDouble2 d2 = atan2k_u1(vcast_vd2_vd_vd(vabs_vd_vd(d), (0.).as_vd()), vcast_vd2_d_d(1, 0));
  $f64x r = d2.0 + d2.1;
  r = vsel_vd_vo_vd_vd(visinf_vo_vd(d), (1.570796326794896557998982).as_vd(), r);
  return vmulsign_vd_vd_vd(r, d);
}

pub fn xatan(s: $f64x) -> $f64x {
  $f64x t, u;
  $i64x q;
#if defined(__INTEL_COMPILER) && defined(ENABLE_PURECFMA_SCALAR)
  $f64x w = s;
#endif

  q = vsel_vi_vd_vi(s, (2).as_vi());
  s = vabs_vd_vd(s);

  q = vsel_vi_vd_vd_vi_vi((1.).as_vd(), s, q + (1).as_vi(), q);
  s = vsel_vd_vo_vd_vd((1.).as_vd().lt(s), vrec_vd_vd(s), s);

  t = s*s;

#ifdef SPLIT_KERNEL
  $f64x t2 = t*t;
  v;

  u = (-1.88796008463073496563746e-05).as_vd()
      .mla(t2, (-0.00110611831486672482563471).as_vd())
      .mla(t2, (-0.00889896195887655491740809).as_vd())
      .mla(t2, (-0.0254517624932312641616861).as_vd())
      .mla(t2, (-0.0407629191276836500001934).as_vd())
      .mla(t2, (-0.0523674852303482457616113).as_vd())
      .mla(t2, (-0.0666573579361080525984562).as_vd())
      .mla(t2, (-0.090908995008245008229153).as_vd())
      .mla(t2, (-0.14285714266771329383765).as_vd())
      .mla(t2, (-0.333333333333311110369124).as_vd());

  v = (0.000209850076645816976906797).as_vd()
      .mla(t2, (0.00370026744188713119232403).as_vd())
      .mla(t2, (0.016599329773529201970117).as_vd())
      .mla(t2, (0.0337852580001353069993897).as_vd())
      .mla(t2, (0.0466667150077840625632675).as_vd())
      .mla(t2, (0.0587666392926673580854313).as_vd())
      .mla(t2, (0.0769219538311769618355029).as_vd())
      .mla(t2, (0.111111105648261418443745).as_vd())
      .mla(t2, (0.199999999996591265594148).as_vd());

  u = v.mla(t, u);
#else
  u = (-1.88796008463073496563746e-05).as_vd()
      .mla(t, (0.000209850076645816976906797).as_vd())
      .mla(t, (-0.00110611831486672482563471).as_vd())
      .mla(t, (0.00370026744188713119232403).as_vd())
      .mla(t, (-0.00889896195887655491740809).as_vd())
      .mla(t, (0.016599329773529201970117).as_vd())
      .mla(t, (-0.0254517624932312641616861).as_vd())
      .mla(t, (0.0337852580001353069993897).as_vd())
      .mla(t, (-0.0407629191276836500001934).as_vd())
      .mla(t, (0.0466667150077840625632675).as_vd())
      .mla(t, (-0.0523674852303482457616113).as_vd())
      .mla(t, (0.0587666392926673580854313).as_vd())
      .mla(t, (-0.0666573579361080525984562).as_vd())
      .mla(t, (0.0769219538311769618355029).as_vd())
      .mla(t, (-0.090908995008245008229153).as_vd())
      .mla(t, (0.111111105648261418443745).as_vd())
      .mla(t, (-0.14285714266771329383765).as_vd())
      .mla(t, (0.199999999996591265594148).as_vd())
      .mla(t, (-0.333333333333311110369124).as_vd());
#endif
  
  t = s.mla(t*u, s);

  t = vsel_vd_vo_vd_vd(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (1).as_vi()), (1).as_vi())), (M_PI/2.).as_vd() - t, t);
  t = vreinterpret_vd_vm(vxor_vm_vm_vm(vand_vm_vo64_vm(vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi(q, (2).as_vi()), (2).as_vi())), vreinterpret_vm_vd((-0.).as_vd())), vreinterpret_vm_vd(t)));

#if defined(__INTEL_COMPILER) && defined(ENABLE_PURECFMA_SCALAR)
  t = vsel_vd_vo_vd_vd(w.eq((0.).as_vd()), w, t);
#endif

  return t;
}

pub fn xlog(d: $f64x) -> $f64x {
  $f64x x, x2;
  $f64x t, m;
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $m64x o = d.lt(f64::MIN.as_vd());
  d = vsel_vd_vo_vd_vd(o, d*(D1_32 * D1_32).as_vd(), d);
  $i64x e = vilogb2k_vi_vd(d*(1./0.75).as_vd());
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - (64).as_vi(), e);
#else
  $f64x e = vgetexp_vd_vd(d*(1./0.75).as_vd());
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), (1024.).as_vd(), e);
  m = vgetmant_vd_vd(d);
#endif
  
  x = ((-1.).as_vd() + m) / ((1.).as_vd() + m);
  x2 = x*x;

  t = (0.153487338491425068243146).as_vd()
      .mla(x2, (0.152519917006351951593857).as_vd())
      .mla(x2, (0.181863266251982985677316).as_vd())
      .mla(x2, (0.222221366518767365905163).as_vd())
      .mla(x2, (0.285714294746548025383248).as_vd())
      .mla(x2, (0.399999999950799600689777).as_vd())
      .mla(x2, (0.6666666666667778740063).as_vd())
      .mla(x2, (2.).as_vd());

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  x = x.mla(t, (0.693147180559945286226764).as_vd()*vcast_vd_vi(e));

  x = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), SLEEF_INFINITY.as_vd(), x);
  x = vsel_vd_vo_vd_vd(d.lt((0.).as_vd()) | visnan_vo_vd(d), (SLEEF_NAN).as_vd(), x);
  x = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), (-SLEEF_INFINITY).as_vd(), x);
#else
  x = x.mla(t, (0.693147180559945286226764).as_vd()*e);
  x = vfixup_vd_vd_vd_vi2_i(x, d, vcast_vi2_i((5 << (5*4))), 0);
#endif

  return x;
}

pub fn xexp(d: $f64x) -> $f64x {
  $f64x u = vrint_vd_vd(d*R_LN2.as_vd());
  s;
  $i64x q = vrint_vi_vd(u);

  s = u.mla((-L2U).as_vd(), d);
  s = u.mla((-L2L).as_vd(), s);

#ifdef ENABLE_FMA_DP
#ifdef SPLIT_KERNEL
  $f64x s2 = s*s;
  v;

  u = (+0.2081276378237164457e-8).as_vd();
  u = vfma_vd_vd_vd_vd(u, s2, (+0.2755762628169491192e-6).as_vd());
  u = vfma_vd_vd_vd_vd(u, s2, (+0.2480158687479686264e-4).as_vd());
  u = vfma_vd_vd_vd_vd(u, s2, (+0.1388888888914497797e-2).as_vd());
  u = vfma_vd_vd_vd_vd(u, s2, (+0.4166666666666602598e-1).as_vd());
  u = vfma_vd_vd_vd_vd(u, s2, (+0.5000000000000000000e+0).as_vd());

  v = (+0.2511210703042288022e-7).as_vd();
  v = vfma_vd_vd_vd_vd(v, s2, (+0.2755723402025388239e-5).as_vd());
  v = vfma_vd_vd_vd_vd(v, s2, (+0.1984126989855865850e-3).as_vd());
  v = vfma_vd_vd_vd_vd(v, s2, (+0.8333333333314938210e-2).as_vd());
  v = vfma_vd_vd_vd_vd(v, s2, (+0.1666666666666669072e+0).as_vd());

  u = v.mla(s, u);
  u = vfma_vd_vd_vd_vd(u, s, (+0.1000000000000000000e+1).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.1000000000000000000e+1).as_vd());
#else // #ifdef SPLIT_KERNEL
  u = (+0.2081276378237164457e-8).as_vd();
  u = vfma_vd_vd_vd_vd(u, s, (+0.2511210703042288022e-7).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.2755762628169491192e-6).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.2755723402025388239e-5).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.2480158687479686264e-4).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.1984126989855865850e-3).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.1388888888914497797e-2).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.8333333333314938210e-2).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.4166666666666602598e-1).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.1666666666666669072e+0).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.5000000000000000000e+0).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.1000000000000000000e+1).as_vd());
  u = vfma_vd_vd_vd_vd(u, s, (+0.1000000000000000000e+1).as_vd());
#endif // #ifdef SPLIT_KERNEL
#else // #ifdef ENABLE_FMA_DP
  u = (2.08860621107283687536341e-09).as_vd()
      .mla(s, (2.51112930892876518610661e-08).as_vd())
      .mla(s, (2.75573911234900471893338e-07).as_vd())
      .mla(s, (2.75572362911928827629423e-06).as_vd())
      .mla(s, (2.4801587159235472998791e-05).as_vd())
      .mla(s, (0.000198412698960509205564975).as_vd())
      .mla(s, (0.00138888888889774492207962).as_vd())
      .mla(s, (0.00833333333331652721664984).as_vd())
      .mla(s, (0.0416666666666665047591422).as_vd())
      .mla(s, (0.166666666666666851703837).as_vd())
      .mla(s, (0.5).as_vd());

  u = (1.).as_vd() + (s*s).mla(u, s);
#endif // #ifdef ENABLE_FMA_DP
  
  u = vldexp2_vd_vd_vi(u, q);

  u = vsel_vd_vo_vd_vd(d.gt((709.78271114955742909217217426).as_vd()), SLEEF_INFINITY.as_vd(), u);
  u = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.lt((-1000.).as_vd()), vreinterpret_vm_vd(u)));

  return u;
}
#[inline]
fn expm1k(d: $f64x) -> $f64x {
  $f64x u = vrint_vd_vd(d*R_LN2.as_vd()), s;
  $i64x q = vrint_vi_vd(u);

  s = u.mla((-L2U).as_vd(), d);
  s = u.mla((-L2L).as_vd(), s);

  u = (2.08860621107283687536341e-09).as_vd()
      .mla(s, (2.51112930892876518610661e-08).as_vd())
      .mla(s, (2.75573911234900471893338e-07).as_vd())
      .mla(s, (2.75572362911928827629423e-06).as_vd())
      .mla(s, (2.4801587159235472998791e-05).as_vd())
      .mla(s, (0.000198412698960509205564975).as_vd())
      .mla(s, (0.00138888888889774492207962).as_vd())
      .mla(s, (0.00833333333331652721664984).as_vd())
      .mla(s, (0.0416666666666665047591422).as_vd())
      .mla(s, (0.166666666666666851703837).as_vd())
      .mla(s, (0.5).as_vd());
  u = (s*s).mla(u, s);
  
  u = vsel_vd_vo_vd_vd(vcast_vo64_vo32(veq_vo_vi_vi(q, (0).as_vi())), u,
		       vldexp2_vd_vd_vi(u + (1.).as_vd(), q) - (1.).as_vd());

  return u;
}
#[inline]
fn logk(d: $f64x) -> VDouble2 {
  VDouble2 x, x2, s;
  $f64x t, m;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $m64x o = d.lt(f64::MIN.as_vd());
  d = vsel_vd_vo_vd_vd(o, d*(D1_32 * D1_32).as_vd(), d);
  $i64x e = vilogb2k_vi_vd(d*(1./0.75).as_vd());
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - (64).as_vi(), e);
#else
  $f64x e = vgetexp_vd_vd(d*(1./0.75).as_vd());
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), (1024.).as_vd(), e);
  m = vgetmant_vd_vd(d);
#endif

  x = dddiv_vd2_vd2_vd2(ddadd2_vd2_vd_vd((-1.).as_vd(), m), ddadd2_vd2_vd_vd((1.).as_vd(), m));
  x2 = ddsqu_vd2_vd2(x);

  t = (0.116255524079935043668677).as_vd()
      .mla(x2.0, (0.103239680901072952701192).as_vd())
      .mla(x2.0, (0.117754809412463995466069).as_vd())
      .mla(x2.0, (0.13332981086846273921509).as_vd())
      .mla(x2.0, (0.153846227114512262845736).as_vd())
      .mla(x2.0, (0.181818180850050775676507).as_vd())
      .mla(x2.0, (0.222222222230083560345903).as_vd())
      .mla(x2.0, (0.285714285714249172087875).as_vd())
      .mla(x2.0, (0.400000000000000077715612).as_vd());
  VDouble2 c = vcast_vd2_d_d(0.666666666666666629659233, 3.80554962542412056336616e-17);

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  s = ddmul_vd2_vd2_vd(vcast_vd2_d_d(0.693147180559945286226764, 2.319046813846299558417771e-17), vcast_vd_vi(e));
#else
  s = ddmul_vd2_vd2_vd(vcast_vd2_vd_vd((0.693147180559945286226764).as_vd(), (2.319046813846299558417771e-17).as_vd()), e);
#endif

  s = ddadd_vd2_vd2_vd2(s, ddscale_vd2_vd2_vd(x, (2.).as_vd()));
  s = ddadd_vd2_vd2_vd2(s, ddmul_vd2_vd2_vd2(ddmul_vd2_vd2_vd2(x2, x),
					     ddadd2_vd2_vd2_vd2(ddmul_vd2_vd2_vd(x2, t), c)));
  return s;
}

pub fn xlog_u1(d: $f64x) -> $f64x {
  VDouble2 x;
  $f64x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $m64x o = d.lt(f64::MIN.as_vd());
  d = vsel_vd_vo_vd_vd(o, d*(D1_32 * D1_32).as_vd(), d);
  $i64x e = vilogb2k_vi_vd(d*(1./0.75).as_vd());
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - (64).as_vi(), e);
#else
  $f64x e = vgetexp_vd_vd(d*(1./0.75).as_vd());
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), (1024.).as_vd(), e);
  m = vgetmant_vd_vd(d);
#endif

  x = dddiv_vd2_vd2_vd2(ddadd2_vd2_vd_vd((-1.).as_vd(), m), ddadd2_vd2_vd_vd((1.).as_vd(), m));
  x2 = x.0*x.0;

  t = (0.1532076988502701353e+0).as_vd()
      .mla(x2, (0.1525629051003428716e+0).as_vd())
      .mla(x2, (0.1818605932937785996e+0).as_vd())
      .mla(x2, (0.2222214519839380009e+0).as_vd())
      .mla(x2, (0.2857142932794299317e+0).as_vd())
      .mla(x2, (0.3999999999635251990e+0).as_vd())
      .mla(x2, (0.6666666666667333541e+0).as_vd());
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VDouble2 s = ddmul_vd2_vd2_vd(vcast_vd2_d_d(0.693147180559945286226764, 2.319046813846299558417771e-17), vcast_vd_vi(e));
#else
  VDouble2 s = ddmul_vd2_vd2_vd(vcast_vd2_d_d(0.693147180559945286226764, 2.319046813846299558417771e-17), e);
#endif

  s = ddadd_vd2_vd2_vd2(s, ddscale_vd2_vd2_vd(x, (2.).as_vd()));
  s = ddadd_vd2_vd2_vd(s, x2*x.0*t);

  $f64x r = s.0 + s.1;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), SLEEF_INFINITY.as_vd(), r);
  r = vsel_vd_vo_vd_vd(d.lt((0.).as_vd()) | visnan_vo_vd(d), (SLEEF_NAN).as_vd(), r);
  r = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), (-SLEEF_INFINITY).as_vd(), r);
#else
  r = vfixup_vd_vd_vd_vi2_i(r, d, vcast_vi2_i((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}
#[inline]
fn expk(VDouble2 d) -> $f64x {
  $f64x u = (d.0 + d.1)*R_LN2.as_vd();
  $f64x dq = vrint_vd_vd(u);
  $i64x q = vrint_vi_vd(dq);
  VDouble2 s, t;

  s = ddadd2_vd2_vd2_vd(d, dq*(-L2U).as_vd());
  s = ddadd2_vd2_vd2_vd(s, dq*(-L2L).as_vd());

  s = ddnormalize_vd2_vd2(s);

  u = (2.51069683420950419527139e-08).as_vd()
      .mla(s.0, (2.76286166770270649116855e-07).as_vd())
      .mla(s.0, (2.75572496725023574143864e-06).as_vd())
      .mla(s.0, (2.48014973989819794114153e-05).as_vd())
      .mla(s.0, (0.000198412698809069797676111).as_vd())
      .mla(s.0, (0.0013888888939977128960529).as_vd())
      .mla(s.0, (0.00833333333332371417601081).as_vd())
      .mla(s.0, (0.0416666666665409524128449).as_vd())
      .mla(s.0, (0.166666666666666740681535).as_vd())
      .mla(s.0, (0.500000000000000999200722).as_vd());
  
  t = ddadd_vd2_vd2_vd2(s, ddmul_vd2_vd2_vd(ddsqu_vd2_vd2(s), u));

  t = ddadd_vd2_vd_vd2((1.).as_vd(), t);
  u = t.0 + t.1;
  u = vldexp2_vd_vd_vi(u, q);

  u = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.0.lt((-1000.).as_vd()), vreinterpret_vm_vd(u)));
  
  return u;
}

pub fn xpow(x: $f64x, y: $f64x) -> $f64x {
#if 1
  $m64x yisint = visint_vo_vd(y);
  $m64x yisodd = visodd_vo_vd(y) & yisint;

  VDouble2 d = ddmul_vd2_vd2_vd(logk(vabs_vd_vd(x)), y);
  $f64x result = expk(d);
  result = vsel_vd_vo_vd_vd(d.0.gt((709.78271114955742909217217426).as_vd()), SLEEF_INFINITY.as_vd(), result);

  result *= vsel_vd_vo_vd_vd(x.gt((0.).as_vd()),
					  (1.).as_vd(),
					  vsel_vd_vo_vd_vd(yisint, vsel_vd_vo_vd_vd(yisodd, (-1.).as_vd(), (1.).as_vd()), (SLEEF_NAN).as_vd()));

  $f64x efx = vmulsign_vd_vd_vd(vabs_vd_vd(x) - (1.).as_vd(), y);

  result = vsel_vd_vo_vd_vd(visinf_vo_vd(y),
			    vreinterpret_vd_vm(vandnot_vm_vo64_vm(efx.lt((0.).as_vd()),
								  vreinterpret_vm_vd(vsel_vd_vo_vd_vd(efx.eq((0.).as_vd()),
												      (1.).as_vd(),
												      SLEEF_INFINITY.as_vd())))),
			    result);

  result = vsel_vd_vo_vd_vd(visinf_vo_vd(x) | x.eq((0.).as_vd()),
			    vsel_vd_vo_vd_vd(yisodd, vsign_vd_vd(x), (1.).as_vd()) *
					  vreinterpret_vd_vm(vandnot_vm_vo64_vm(vsel_vd_vo_vd_vd(x.eq((0.).as_vd()), -y, y).lt((0.).as_vd()),
										vreinterpret_vm_vd(SLEEF_INFINITY.as_vd()))),
			    result);

  result = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x) | visnan_vo_vd(y), vreinterpret_vm_vd(result)));

  result = vsel_vd_vo_vd_vd(y.eq((0.).as_vd()) | x.eq((1.).as_vd()), (1.).as_vd(), result);

  return result;
#else
  return expk(ddmul_vd2_vd2_vd(logk(x), y));
#endif
}
#[inline]
fn expk2(VDouble2 d) -> VDouble2 {
  $f64x u = (d.0 + d.1)*R_LN2.as_vd();
  $f64x dq = vrint_vd_vd(u);
  $i64x q = vrint_vi_vd(dq);
  VDouble2 s, t;

  s = ddadd2_vd2_vd2_vd(d, dq*(-L2U).as_vd());
  s = ddadd2_vd2_vd2_vd(s, dq*(-L2L).as_vd());

  u = (+0.1602472219709932072e-9).as_vd()
      .mla(s.0, (+0.2092255183563157007e-8).as_vd())
      .mla(s.0, (+0.2505230023782644465e-7).as_vd())
      .mla(s.0, (+0.2755724800902135303e-6).as_vd())
      .mla(s.0, (+0.2755731892386044373e-5).as_vd())
      .mla(s.0, (+0.2480158735605815065e-4).as_vd())
      .mla(s.0, (+0.1984126984148071858e-3).as_vd())
      .mla(s.0, (+0.1388888888886763255e-2).as_vd())
      .mla(s.0, (+0.8333333333333347095e-2).as_vd())
      .mla(s.0, (+0.4166666666666669905e-1).as_vd());

  t = ddadd2_vd2_vd2_vd(ddmul_vd2_vd2_vd(s, u), (+0.1666666666666666574e+0).as_vd());
  t = ddadd2_vd2_vd2_vd(ddmul_vd2_vd2_vd2(s, t), (0.5).as_vd());
  t = ddadd2_vd2_vd2_vd2(s, ddmul_vd2_vd2_vd2(ddsqu_vd2_vd2(s), t));

  t = ddadd_vd2_vd_vd2((1.).as_vd(), t);

  t.0 = vldexp2_vd_vd_vi(t.0, q);
  t.1 = vldexp2_vd_vd_vi(t.1, q);

  t.0 = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.0.lt((-1000.).as_vd()), vreinterpret_vm_vd(t.0)));
  t.1 = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.0.lt((-1000.).as_vd()), vreinterpret_vm_vd(t.1)));

  return t;
}

pub fn xsinh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  VDouble2 d = expk2(vcast_vd2_vd_vd(y, (0.).as_vd()));
  d = ddsub_vd2_vd2_vd2(d, ddrec_vd2_vd2(d));
  y = (d.0 + d.1)*(0.5).as_vd();

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt((710.).as_vd()) | visnan_vo_vd(y), SLEEF_INFINITY.as_vd(), y);
  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xcosh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  VDouble2 d = expk2(vcast_vd2_vd_vd(y, (0.).as_vd()));
  d = ddadd_vd2_vd2_vd2(d, ddrec_vd2_vd2(d));
  y = (d.0 + d.1)*(0.5).as_vd();

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt((710.).as_vd()) | visnan_vo_vd(y), SLEEF_INFINITY.as_vd(), y);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xtanh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  VDouble2 d = expk2(vcast_vd2_vd_vd(y, (0.).as_vd()));
  VDouble2 e = ddrec_vd2_vd2(d);
  d = dddiv_vd2_vd2_vd2(ddadd2_vd2_vd2_vd2(d, ddneg_vd2_vd2(e)), ddadd2_vd2_vd2_vd2(d, e));
  y = d.0 + d.1;

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt((18.714973875).as_vd()) | visnan_vo_vd(y), (1.).as_vd(), y);
  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xsinh_u35(x: $f64x) -> $f64x {
  $f64x e = expm1k(vabs_vd_vd(x));

  $f64x y = (e + (2.).as_vd()) / (e + (1.).as_vd());
  y = y*((0.5).as_vd()*e);

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt((709.).as_vd()) | visnan_vo_vd(y), SLEEF_INFINITY.as_vd(), y);
  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xcosh_u35(x: $f64x) -> $f64x {
  $f64x e = xexp(vabs_vd_vd(x));
  $f64x y = (0.5).as_vd().mla(e, (0.5).as_vd() / e);

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt((709.).as_vd()) | visnan_vo_vd(y), SLEEF_INFINITY.as_vd(), y);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xtanh_u35(x: $f64x) -> $f64x {
  $f64x d = expm1k((2.).as_vd()*vabs_vd_vd(x));
  $f64x y = d / ((2.).as_vd() + d);

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt((18.714973875).as_vd()) | visnan_vo_vd(y), (1.).as_vd(), y);
  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}
#[inline]
fn logk2(VDouble2 d) -> VDouble2 {
  VDouble2 x, x2, m, s;
  $f64x t;
  $i64x e;
  
  e = vilogbk_vi_vd(d.0*(1./0.75).as_vd());

  m.0 = vldexp2_vd_vd_vi(d.0, vneg_vi_vi(e));
  m.1 = vldexp2_vd_vd_vi(d.1, vneg_vi_vi(e));

  x = dddiv_vd2_vd2_vd2(ddadd2_vd2_vd2_vd(m, (-1.).as_vd()), ddadd2_vd2_vd2_vd(m, (1.).as_vd()));
  x2 = ddsqu_vd2_vd2(x);

  t = (0.13860436390467167910856).as_vd()
      .mla(x2.0, (0.131699838841615374240845).as_vd())
      .mla(x2.0, (0.153914168346271945653214).as_vd())
      .mla(x2.0, (0.181816523941564611721589).as_vd())
      .mla(x2.0, (0.22222224632662035403996).as_vd())
      .mla(x2.0, (0.285714285511134091777308).as_vd())
      .mla(x2.0, (0.400000000000914013309483).as_vd())
      .mla(x2.0, (0.666666666666664853302393).as_vd());

  s = ddmul_vd2_vd2_vd(vcast_vd2_d_d(0.693147180559945286226764, 2.319046813846299558417771e-17), vcast_vd_vi(e));
  s = ddadd_vd2_vd2_vd2(s, ddscale_vd2_vd2_vd(x, (2.).as_vd()));
  s = ddadd_vd2_vd2_vd2(s, ddmul_vd2_vd2_vd(ddmul_vd2_vd2_vd2(x2, x), t));

  return  s;
}

pub fn xasinh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  $m64x o = y.gt((1.).as_vd());
  VDouble2 d;
  
  d = vsel_vd2_vo_vd2_vd2(o, ddrec_vd2_vd(x), vcast_vd2_vd_vd(y, (0.).as_vd()));
  d = ddsqrt_vd2_vd2(ddadd2_vd2_vd2_vd(ddsqu_vd2_vd2(d), (1.).as_vd()));
  d = vsel_vd2_vo_vd2_vd2(o, ddmul_vd2_vd2_vd(d, y), d);

  d = logk2(ddnormalize_vd2_vd2(ddadd2_vd2_vd2_vd(d, x)));
  y = d.0 + d.1;
  
  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt(SQRT_DBL_MAX.as_vd()) | visnan_vo_vd(y),
		       vmulsign_vd_vd_vd(SLEEF_INFINITY.as_vd(), x), y);

  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));
  y = vsel_vd_vo_vd_vd(visnegzero_vo_vd(x), (-0.).as_vd(), y);
  
  return y;
}

pub fn xacosh(x: $f64x) -> $f64x {
  VDouble2 d = logk2(ddadd2_vd2_vd2_vd(ddmul_vd2_vd2_vd2(ddsqrt_vd2_vd2(ddadd2_vd2_vd_vd(x, (1.).as_vd())), ddsqrt_vd2_vd2(ddadd2_vd2_vd_vd(x, (-1.).as_vd()))), x));
  $f64x y = d.0 + d.1;

  y = vsel_vd_vo_vd_vd(vabs_vd_vd(x).gt(SQRT_DBL_MAX.as_vd()) | visnan_vo_vd(y),
		       SLEEF_INFINITY.as_vd(), y);
  y = vreinterpret_vd_vm(vandnot_vm_vo64_vm(x.eq((1.).as_vd()), vreinterpret_vm_vd(y)));

  y = vreinterpret_vd_vm(vor_vm_vo64_vm(x.lt((1.).as_vd()), vreinterpret_vm_vd(y)));
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));
  
  return y;
}

pub fn xatanh(x: $f64x) -> $f64x {
  $f64x y = vabs_vd_vd(x);
  VDouble2 d = logk2(dddiv_vd2_vd2_vd2(ddadd2_vd2_vd_vd((1.).as_vd(), y), ddadd2_vd2_vd_vd((1.).as_vd(), -y)));
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(y.gt((1.).as_vd()), vreinterpret_vm_vd(vsel_vd_vo_vd_vd(y.eq((1.).as_vd()), SLEEF_INFINITY.as_vd(), (d.0 + d.1)*(0.5).as_vd()))));

  y = vmulsign_vd_vd_vd(y, x);
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visinf_vo_vd(x) | visnan_vo_vd(y), vreinterpret_vm_vd(y)));
  y = vreinterpret_vd_vm(vor_vm_vo64_vm(visnan_vo_vd(x), vreinterpret_vm_vd(y)));

  return y;
}

pub fn xcbrt(d: $f64x) -> $f64x {
  $f64x x, y, q = (1.).as_vd();
  $i64x e, qu, re;
  $f64x t;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  $f64x s = d;
#endif
  e = vilogbk_vi_vd(vabs_vd_vd(d)) + (1).as_vi();
  d = vldexp2_vd_vd_vi(d, vneg_vi_vi(e));

  t = vcast_vd_vi(e) + (6144.).as_vd();
  qu = vtruncate_vi_vd(t*(1./3.).as_vd());
  re = vtruncate_vi_vd(t*(vcast_vd_vi(qu)*(3.).as_vd()));

  q = vsel_vd_vo_vd_vd(vcast_vo64_vo32(veq_vo_vi_vi(re, (1).as_vi())), (1.2599210498948731647672106).as_vd(), q);
  q = vsel_vd_vo_vd_vd(vcast_vo64_vo32(veq_vo_vi_vi(re, (2).as_vi())), (1.5874010519681994747517056).as_vd(), q);
  q = vldexp2_vd_vd_vi(q, qu - (2048).as_vi());

  q = vmulsign_vd_vd_vd(q, d);

  d = vabs_vd_vd(d);

  x = (-0.640245898480692909870982).as_vd()
      .mla(d, (2.96155103020039511818595).as_vd())
      .mla(d, (-5.73353060922947843636166).as_vd())
      .mla(d, (6.03990368989458747961407).as_vd())
      .mla(d, (-3.85841935510444988821632).as_vd())
      .mla(d, (2.2307275302496609725722).as_vd());

  y = x*x;
  y = y*y;
  x -= vmlapn_vd_vd_vd_vd(d, y, x)*(1. / 3.).as_vd();
  y = d*x*x;
  y = (y - (2. / 3.).as_vd()*y*y.mla(x, (-1.).as_vd()))*q;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  y = vsel_vd_vo_vd_vd(visinf_vo_vd(s), vmulsign_vd_vd_vd(SLEEF_INFINITY.as_vd(), s), y);
  y = vsel_vd_vo_vd_vd(s.eq((0.).as_vd()), vmulsign_vd_vd_vd((0.).as_vd(), s), y);
#endif
  
  return y;
}

pub fn xcbrt_u1(d: $f64x) -> $f64x {
  $f64x x, y, z, t;
  VDouble2 q2 = vcast_vd2_d_d(1, 0), u, v;
  $i64x e, qu, re;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  $f64x s = d;
#endif
  e = vilogbk_vi_vd(vabs_vd_vd(d)) + (1).as_vi();
  d = vldexp2_vd_vd_vi(d, vneg_vi_vi(e));

  t = vcast_vd_vi(e) + (6144.).as_vd();
  qu = vtruncate_vi_vd(t*(1./3.).as_vd());
  re = vtruncate_vi_vd(t - vcast_vd_vi(qu)*(3.).as_vd());

  q2 = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(veq_vo_vi_vi(re, (1).as_vi())), vcast_vd2_d_d(1.2599210498948731907, -2.5899333753005069177e-17), q2);
  q2 = vsel_vd2_vo_vd2_vd2(vcast_vo64_vo32(veq_vo_vi_vi(re, (2).as_vi())), vcast_vd2_d_d(1.5874010519681995834, -1.0869008194197822986e-16), q2);

  q2.0 = vmulsign_vd_vd_vd(q2.0, d); q2.1 = vmulsign_vd_vd_vd(q2.1, d);
  d = vabs_vd_vd(d);

  x = (-0.640245898480692909870982).as_vd()
      .mla(d, (2.96155103020039511818595).as_vd())
      .mla(d, (-5.73353060922947843636166).as_vd())
      .mla(d, (6.03990368989458747961407).as_vd())
      .mla(d, (-3.85841935510444988821632).as_vd())
      .mla(d, (2.2307275302496609725722).as_vd());

  y = x*x;
  y = y*y;
  x -= vmlapn_vd_vd_vd_vd(d, y, x)*(1. / 3.).as_vd();

  z = x;

  u = ddmul_vd2_vd_vd(x, x);
  u = ddmul_vd2_vd2_vd2(u, u);
  u = ddmul_vd2_vd2_vd(u, d);
  u = ddadd2_vd2_vd2_vd(u, -x);
  y = u.0 + u.1;

  y = (-2. / 3.).as_vd()*y*z;
  v = ddadd2_vd2_vd2_vd(ddmul_vd2_vd_vd(z, z), y);
  v = ddmul_vd2_vd2_vd(v, d);
  v = ddmul_vd2_vd2_vd2(v, q2);
  z = vldexp2_vd_vd_vi(v.0 + v.1, qu - (2048).as_vi());

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  z = vsel_vd_vo_vd_vd(visinf_vo_vd(d), vmulsign_vd_vd_vd(SLEEF_INFINITY.as_vd(), q2.0), z);
  z = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), vreinterpret_vd_vm(vsignbit_vm_vd(q2.0)), z);
#else
  z = vsel_vd_vo_vd_vd(visinf_vo_vd(s), vmulsign_vd_vd_vd(SLEEF_INFINITY.as_vd(), s), z);
  z = vsel_vd_vo_vd_vd(s.eq((0.).as_vd()), vmulsign_vd_vd_vd((0.).as_vd(), s), z);
#endif
  
  return z;
}

pub fn xexp2(d: $f64x) -> $f64x {
  $f64x u = vrint_vd_vd(d), s;
  $i64x q = vrint_vi_vd(u);

  s = d - u;

#ifdef SPLIT_KERNEL
  $f64x s2 = s*s;
   v;

  u = (+0.4434359082926529454e-9).as_vd()
      .mla(s2, (+0.1017819260921760451e-6).as_vd())
      .mla(s2, (+0.1525273353517584730e-4).as_vd())
      .mla(s2, (+0.1333355814670499073e-2).as_vd())
      .mla(s2, (+0.5550410866482046596e-1).as_vd());

  v = (+0.7073164598085707425e-8).as_vd()
      .mla(s2, (+0.1321543872511327615e-5).as_vd())
      .mla(s2, (+0.1540353045101147808e-3).as_vd())
      .mla(s2, (+0.9618129107597600536e-2).as_vd())
      .mla(s2, (+0.2402265069591012214e+0).as_vd());

  u = u.mla(s, v)
      .mla(s, (+0.6931471805599452862e+0).as_vd());
#else
  u = (+0.4434359082926529454e-9).as_vd()
      .mla(s, (+0.7073164598085707425e-8).as_vd())
      .mla(s, (+0.1017819260921760451e-6).as_vd())
      .mla(s, (+0.1321543872511327615e-5).as_vd())
      .mla(s, (+0.1525273353517584730e-4).as_vd())
      .mla(s, (+0.1540353045101147808e-3).as_vd())
      .mla(s, (+0.1333355814670499073e-2).as_vd())
      .mla(s, (+0.9618129107597600536e-2).as_vd())
      .mla(s, (+0.5550410866482046596e-1).as_vd())
      .mla(s, (+0.2402265069591012214e+0).as_vd())
      .mla(s, (+0.6931471805599452862e+0).as_vd());
#endif
  
#ifdef ENABLE_FMA_DP
  u = vfma_vd_vd_vd_vd(u, s, (1.).as_vd());
#else
  u = ddnormalize_vd2_vd2(ddadd_vd2_vd_vd2((1.).as_vd(), ddmul_vd2_vd_vd(u, s))).0;
#endif
  
  u = vldexp2_vd_vd_vi(u, q);

  u = vsel_vd_vo_vd_vd(d.ge((1024.).as_vd()), SLEEF_INFINITY.as_vd(), u);
  u = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.lt((-2000.).as_vd()), vreinterpret_vm_vd(u)));

  return u;
}

pub fn xexp10(d: $f64x) -> $f64x {
  $f64x u = vrint_vd_vd(d*LOG10_2.as_vd());
  s;
  $i64x q = vrint_vi_vd(u);

  s = u.mla((-L10U).as_vd(), d);
  s = u.mla((-L10L).as_vd(), s);

#ifdef SPLIT_KERNEL
  $f64x s2 = s*s;
  v;

  u = (+0.2411463498334267652e-3).as_vd()
      .mla(s2, (+0.5013975546789733659e-2).as_vd())
      .mla(s2, (+0.6808936399446784138e-1).as_vd())
      .mla(s2, (+0.5393829292058536229e+0).as_vd())
      .mla(s2, (+0.2034678592293432953e+1).as_vd());

  v = (+0.1157488415217187375e-2).as_vd()
      .mla(s2, (+0.1959762320720533080e-1).as_vd())
      .mla(s2, (+0.2069958494722676234e+0).as_vd())
      .mla(s2, (+0.1171255148908541655e+1).as_vd())
      .mla(s2, (+0.2650949055239205876e+1).as_vd());

  u = u.mla(s, v)
      .mla(s, (+0.2302585092994045901e+1).as_vd());
#else
  u = (+0.2411463498334267652e-3).as_vd()
      .mla(s, (+0.1157488415217187375e-2).as_vd())
      .mla(s, (+0.5013975546789733659e-2).as_vd())
      .mla(s, (+0.1959762320720533080e-1).as_vd())
      .mla(s, (+0.6808936399446784138e-1).as_vd())
      .mla(s, (+0.2069958494722676234e+0).as_vd())
      .mla(s, (+0.5393829292058536229e+0).as_vd())
      .mla(s, (+0.1171255148908541655e+1).as_vd())
      .mla(s, (+0.2034678592293432953e+1).as_vd())
      .mla(s, (+0.2650949055239205876e+1).as_vd())
      .mla(s, (+0.2302585092994045901e+1).as_vd());
#endif
  
#ifdef ENABLE_FMA_DP
  u = vfma_vd_vd_vd_vd(u, s, (1.).as_vd());
#else
  u = ddnormalize_vd2_vd2(ddadd_vd2_vd_vd2((1.).as_vd(), ddmul_vd2_vd_vd(u, s))).0;
#endif
  
  u = vldexp2_vd_vd_vi(u, q);

  u = vsel_vd_vo_vd_vd(d.gt((308.25471555991671).as_vd()), SLEEF_INFINITY.as_vd(), u);
  u = vreinterpret_vd_vm(vandnot_vm_vo64_vm(d.lt((-350.).as_vd()), vreinterpret_vm_vd(u)));

  return u;
}

pub fn xexpm1(a: $f64x) -> $f64x {
  VDouble2 d = ddadd2_vd2_vd2_vd(expk2(vcast_vd2_vd_vd(a, (0.).as_vd())), (-1.).as_vd());
  $f64x x = d.0 + d.1;
  x = vsel_vd_vo_vd_vd(a.gt((709.782712893383996732223).as_vd()), SLEEF_INFINITY.as_vd(), x);
  x = vsel_vd_vo_vd_vd(a.lt((-36.736800569677101399113302437).as_vd()), (-1.).as_vd(), x);
  x = vsel_vd_vo_vd_vd(visnegzero_vo_vd(a), (-0.).as_vd(), x);
  return x;
}

pub fn xlog10(d: $f64x) -> $f64x {
  VDouble2 x;
  $f64x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $m64x o = d.lt(f64::MIN.as_vd());
  d = vsel_vd_vo_vd_vd(o, d*(D1_32 * D1_32).as_vd(), d);
  $i64x e = vilogb2k_vi_vd(d*(1./0.75).as_vd());
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - (64).as_vi(), e);
#else
  $f64x e = vgetexp_vd_vd(d*(1./0.75).as_vd());
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), (1024.).as_vd(), e);
  m = vgetmant_vd_vd(d);
#endif

  x = dddiv_vd2_vd2_vd2(ddadd2_vd2_vd_vd((-1.).as_vd(), m), ddadd2_vd2_vd_vd((1.).as_vd(), m));
  x2 = x.0*x.0;

  t = (+0.6653725819576758460e-1).as_vd()
      .mla(x2, (+0.6625722782820833712e-1).as_vd())
      .mla(x2, (+0.7898105214313944078e-1).as_vd())
      .mla(x2, (+0.9650955035715275132e-1).as_vd())
      .mla(x2, (+0.1240841409721444993e+0).as_vd())
      .mla(x2, (+0.1737177927454605086e+0).as_vd())
      .mla(x2, (+0.2895296546021972617e+0).as_vd());
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VDouble2 s = ddmul_vd2_vd2_vd(vcast_vd2_d_d(0.30102999566398119802, -2.803728127785170339e-18), vcast_vd_vi(e));
#else
  VDouble2 s = ddmul_vd2_vd2_vd(vcast_vd2_d_d(0.30102999566398119802, -2.803728127785170339e-18), e);
#endif

  s = ddadd_vd2_vd2_vd2(s, ddmul_vd2_vd2_vd2(x, vcast_vd2_d_d(0.86858896380650363334, 1.1430059694096389311e-17)));
  s = ddadd_vd2_vd2_vd(s, x2*x.0*t);

  $f64x r = s.0 + s.1;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), SLEEF_INFINITY.as_vd(), r);
  r = vsel_vd_vo_vd_vd(d.lt((0.).as_vd()) | visnan_vo_vd(d), (SLEEF_NAN).as_vd(), r);
  r = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), (-SLEEF_INFINITY).as_vd(), r);
#else
  r = vfixup_vd_vd_vd_vi2_i(r, d, vcast_vi2_i((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}

pub fn xlog2(d: $f64x) -> $f64x {
  VDouble2 x;
  $f64x t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $m64x o = d.lt(f64::MIN.as_vd());
  d = vsel_vd_vo_vd_vd(o, d*(D1_32 * D1_32).as_vd(), d);
  $i64x e = vilogb2k_vi_vd(d*(1./0.75).as_vd());
  m = vldexp3_vd_vd_vi(d, vneg_vi_vi(e));
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - (64).as_vi(), e);
#else
  $f64x e = vgetexp_vd_vd(d*(1.0/0.75).as_vd());
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), (1024.).as_vd(), e);
  m = vgetmant_vd_vd(d);
#endif

  x = dddiv_vd2_vd2_vd2(ddadd2_vd2_vd_vd((-1.).as_vd(), m), ddadd2_vd2_vd_vd((1.).as_vd(), m));
  x2 = x.0*x.0;

  t = (+0.2211941750456081490e+0).as_vd()
      .mla(x2, (+0.2200768693152277689e+0).as_vd())
      .mla(x2, (+0.2623708057488514656e+0).as_vd())
      .mla(x2, (+0.3205977477944495502e+0).as_vd())
      .mla(x2, (+0.4121985945485324709e+0).as_vd())
      .mla(x2, (+0.5770780162997058982e+0).as_vd())
      .mla(x2, (+0.96179669392608091449).as_vd());
  
  if !cfg!("enable_avx512f") && !cfg!("enable_avx512fnofma")
    VDouble2 s = ddadd2_vd2_vd_vd2(vcast_vd_vi(e),
           ddmul_vd2_vd2_vd2(x, vcast_vd2_d_d(2.885390081777926774, 6.0561604995516736434e-18)));
  } else {
    VDouble2 s = ddadd2_vd2_vd_vd2(e,
           ddmul_vd2_vd2_vd2(x, vcast_vd2_d_d(2.885390081777926774, 6.0561604995516736434e-18)));
  }

  s = ddadd2_vd2_vd2_vd(s, x2*x.0*t);

  $f64x r = s.0 + s.1;

  if !cfg!("enable_avx512f") && !cfg!("enable_avx512fnofma")
    r = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), SLEEF_INFINITY.as_vd(), r);
    r = vsel_vd_vo_vd_vd(d.lt((0.).as_vd()) | visnan_vo_vd(d), (SLEEF_NAN).as_vd(), r);
    r = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), (-SLEEF_INFINITY).as_vd(), r);
  } else {
    r = vfixup_vd_vd_vd_vi2_i(r, d, vcast_vi2_i((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
  }
  
  return r;
}

pub fn xlog1p(d: $f64x) -> $f64x {
  VDouble2 x;
  $f64x t, m, x2;

  $f64x dp1 = d + (1.).as_vd();

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  $m64x o = dp1.lt(f64::MIN.as_vd());
  dp1 = vsel_vd_vo_vd_vd(o, dp1*(D1_32 * D1_32).as_vd(), dp1);
  $i64x e = vilogb2k_vi_vd(dp1*(1./0.75).as_vd());
  t = vldexp3_vd_vd_vi((1.).as_vd(), vneg_vi_vi(e));
  m = d.mla(t, t - (1.).as_vd());
  e = vsel_vi_vo_vi_vi(vcast_vo32_vo64(o), e - (64).as_vi(), e);
  VDouble2 s = ddmul_vd2_vd2_vd(vcast_vd2_d_d(0.693147180559945286226764, 2.319046813846299558417771e-17), vcast_vd_vi(e));
#else
  $f64x e = vgetexp_vd_vd(dp1, (1./0.75).as_vd());
  e = vsel_vd_vo_vd_vd(vispinf_vo_vd(e), (1024.).as_vd(), e);
  t = vldexp3_vd_vd_vi((1.).as_vd(), vneg_vi_vi(vrint_vi_vd(e)));
  m = d.mla(t, t - (1.).as_vd());
  VDouble2 s = ddmul_vd2_vd2_vd(vcast_vd2_d_d(0.693147180559945286226764, 2.319046813846299558417771e-17), e);
#endif

  x = dddiv_vd2_vd2_vd2(vcast_vd2_vd_vd(m, (0.).as_vd()), ddadd_vd2_vd_vd((2.).as_vd(), m));
  x2 = x.0*x.0;

  t = (0.1532076988502701353e+0).as_vd()
      .mla(x2, (0.1525629051003428716e+0).as_vd())
      .mla(x2, (0.1818605932937785996e+0).as_vd())
      .mla(x2, (0.2222214519839380009e+0).as_vd())
      .mla(x2, (0.2857142932794299317e+0).as_vd())
      .mla(x2, (0.3999999999635251990e+0).as_vd())
      .mla(x2, (0.6666666666667333541e+0).as_vd());
  
  s = ddadd_vd2_vd2_vd2(s, ddscale_vd2_vd2_vd(x, (2.).as_vd()));
  s = ddadd_vd2_vd2_vd(s, x2*x.0*t);

  $f64x r = s.0 + s.1;
  
  r = vsel_vd_vo_vd_vd(d.gt((1e+307).as_vd()), SLEEF_INFINITY.as_vd(), r);
  r = vsel_vd_vo_vd_vd(d.lt((-1.).as_vd()) | visnan_vo_vd(d), SLEEF_NAN.as_vd(), r);
  r = vsel_vd_vo_vd_vd(d.eq((-1.).as_vd()), (-SLEEF_INFINITY).as_vd(), r);
  r = vsel_vd_vo_vd_vd(visnegzero_vo_vd(d), (-0.).as_vd(), r);
  
  return r;
}

//
#[inline]
fn vcast_vi2_i_i(int i0, int i1) -> $i64x2 { return vcast_vi2_vm(vcast_vm_i_i(i0, i1)); }

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
  ret = vsel_vd_vo_vd_vd(ret.lt((0.).as_vd()) | x.eq(y), (0.).as_vd(), ret);
  return ret;
}

pub fn xtrunc(x: $f64x) -> $f64x {
  $f64x fr = x - D1_31.as_vd()*vcast_vd_vi(vtruncate_vi_vd(x*(1. / D1_31).as_vd()));
  fr -= vcast_vd_vi(vtruncate_vi_vd(fr));
  return vsel_vd_vo_vd_vd(visinf_vo_vd(x) | vabs_vd_vd(x).ge(D1_52.as_vd()), x, vcopysign_vd_vd_vd(x - fr, x));
}

pub fn xfloor(x: $f64x) -> $f64x {
  $f64x fr = x - D1_31.as_vd()*vcast_vd_vi(vtruncate_vi_vd(x*(1. / D1_31).as_vd()));
  fr -= vcast_vd_vi(vtruncate_vi_vd(fr));
  fr = vsel_vd_vo_vd_vd(fr.lt((0.).as_vd()), fr + (1.).as_vd(), fr);
  return vsel_vd_vo_vd_vd(visinf_vo_vd(x) | vabs_vd_vd(x).ge(D1_52.as_vd()), x, vcopysign_vd_vd_vd(x - fr, x));
}

pub fn xceil(x: $f64x) -> $f64x {
  $f64x fr = x - D1_31.as_vd()*vcast_vd_vi(vtruncate_vi_vd(x*(1. / D1_31).as_vd()));
  fr -= vcast_vd_vi(vtruncate_vi_vd(fr));
  fr = vsel_vd_vo_vd_vd(fr.le((0.).as_vd()), fr, fr - (1.).as_vd());
  return vsel_vd_vo_vd_vd(visinf_vo_vd(x) | vabs_vd_vd(x).ge(D1_52.as_vd()), x, vcopysign_vd_vd_vd(x - fr, x));
}

pub fn xround(d: $f64x) -> $f64x {
  $f64x x = d + (0.5).as_vd();
  $f64x fr = x - D1_31.as_vd()*vcast_vd_vi(vtruncate_vi_vd(x*(1. / D1_31).as_vd()));
  fr -= vcast_vd_vi(vtruncate_vi_vd(fr));
  x = vsel_vd_vo_vd_vd(x.le((0.).as_vd()) & fr.eq((0.).as_vd()), x - (1.).as_vd(), x);
  fr = vsel_vd_vo_vd_vd(fr.lt((0.).as_vd()), fr + (1.).as_vd(), fr);
  x = vsel_vd_vo_vd_vd(d.eq((0.49999999999999994449).as_vd()), (0.).as_vd(), x);
  return vsel_vd_vo_vd_vd(visinf_vo_vd(d) | vabs_vd_vd(d).ge(D1_52.as_vd()), d, vcopysign_vd_vd_vd(x - fr, d));
}

pub fn xrint(d: $f64x) -> $f64x {
  $f64x x = d + (0.5).as_vd();
  $f64x fr = x - D1_31.as_vd()*vcast_vd_vi(vtruncate_vi_vd(x*(1. / D1_31).as_vd()));
  $m64x isodd = vcast_vo64_vo32(veq_vo_vi_vi(vand_vi_vi_vi((1).as_vi(), vtruncate_vi_vd(fr)), (1).as_vi()));
  fr -= vcast_vd_vi(vtruncate_vi_vd(fr));
  fr = vsel_vd_vo_vd_vd(fr.lt((0).as_vd()) | (fr.eq((0).as_vd()) & isodd), fr + (1.).as_vd(), fr);
  x = vsel_vd_vo_vd_vd(d.eq((0.50000000000000011102).as_vd()), (0.).as_vd(), x);
  $f64x ret = vsel_vd_vo_vd_vd(visinf_vo_vd(d) | vabs_vd_vd(d).ge(D1_52.as_vd()), d, vcopysign_vd_vd_vd(x - fr, d));
  return ret;
}

pub fn xnextafter(x: $f64x, y: $f64x) -> $f64x {
  x = vsel_vd_vo_vd_vd(x.eq((0.).as_vd()), vmulsign_vd_vd_vd((0.).as_vd(), y), x);
  $i64x2 t, xi2 = vreinterpret_vi2_vd(x);
  $m64x c = vsignbit_vo_vd(x) ^ y.ge(x);

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

  ret = vsel_vd_vo_vd_vd(ret.eq((0.).as_vd()) & x.ne((0.).as_vd()), 
			 vmulsign_vd_vd_vd((0.).as_vd(), x), ret);

  ret = vsel_vd_vo_vd_vd(x.eq((0.).as_vd()) & y.eq((0.).as_vd()), y, ret);

  ret = vsel_vd_vo_vd_vd(visnan_vo_vd(x) | visnan_vo_vd(y), (SLEEF_NAN).as_vd(), ret);
  
  return ret;
}

pub fn xfrfrexp(x: $f64x) -> $f64x {
  x = vsel_vd_vo_vd_vd(vabs_vd_vd(x).lt(f64::MIN.as_vd()), x*D1_63.as_vd(), x);

  vmask xm = vreinterpret_vm_vd(x);
  xm = vand_vm_vm_vm(xm, vcast_vm_i_i(~0x7ff00000, ~0));
  xm = vor_vm_vm_vm (xm, vcast_vm_i_i( 0x3fe00000,  0));

  $f64x ret = vreinterpret_vd_vm(xm);

  ret = vsel_vd_vo_vd_vd(visinf_vo_vd(x), vmulsign_vd_vd_vd(SLEEF_INFINITY.as_vd(), x), ret);
  ret = vsel_vd_vo_vd_vd(x.eq((0.).as_vd()), x, ret);
  
  return ret;
}

pub fn xexpfrexp(x: $f64x) -> $i64x {
  x = vsel_vd_vo_vd_vd(vabs_vd_vd(x).lt(f64::MIN.as_vd()), x*D1_63.as_vd(), x);

  let ret = vreinterpret_vi2_vd(x).as_vi();
  ret = vand_vi_vi_vi(vsrl_vi_vi_i(ret, 20), vcast_vi_i(0x7ff)) - vcast_vi_i(0x3fe);

  ret = vsel_vi_vo_vi_vi(x.eq((0.).as_vd()) | visnan_vo_vd(x) | visinf_vo_vd(x), (0).as_vi(), ret);
  
  return ret;
}

pub fn xfma($f64x x, $f64x y, $f64x z) -> $f64x {
  $f64x h2 = x*y + z;
  q = (1.).as_vd();
  $m64x o = (vabs_vd_vd(h2).lt((1e-300).as_vd());
  const C0 : f64 = D1_54;
  const C1 : f64 = C0 * C0;
  const C2 : f64 = C1 * C1;
  {
    x = vsel_vd_vo_vd_vd(o, x*C1.as_vd(), x);
    y = vsel_vd_vo_vd_vd(o, y*C1.as_vd(), y);
    z = vsel_vd_vo_vd_vd(o, z*C2.as_vd(), z);
    q = vsel_vd_vo_vd_vd(o, (1. / C2).as_vd(), q);
  }
  o = vabs_vd_vd(h2).gt((1e+300).as_vd());
  {
    x = vsel_vd_vo_vd_vd(o, x*(1. / C1).as_vd(), x);
    y = vsel_vd_vo_vd_vd(o, y*(1. / C1).as_vd(), y);
    z = vsel_vd_vo_vd_vd(o, z*(1. / C2).as_vd(), z);
    q = vsel_vd_vo_vd_vd(o, C2.as_vd(), q);
  }
  VDouble2 d = ddmul_vd2_vd_vd(x, y);
  d = ddadd2_vd2_vd2_vd(d, z);
  $f64x ret = vsel_vd_vo_vd_vd(x.eq((0.).as_vd()) | y.eq((0.).as_vd()), z, d.0 + d.1);
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
  $m64x o;
  
  d = vsel_vd_vo_vd_vd(d.lt((0.).as_vd()), (SLEEF_NAN).as_vd(), d);

  o = d.lt((8.636168555094445e-78).as_vd());
  d = vsel_vd_vo_vd_vd(o, d*(1.157920892373162e77).as_vd(), d);
  q = vsel_vd_vo_vd_vd(o, (2.9387358770557188e-39*0.5).as_vd(), (0.5).as_vd());

  o = d.gt((1.3407807929942597e+154).as_vd());
  d = vsel_vd_vo_vd_vd(o, d*(7.4583407312002070e-155).as_vd(), d);
  q = vsel_vd_vo_vd_vd(o, (1.1579208923731620e+77*0.5).as_vd(), q);

  $f64x x = vreinterpret_vd_vi2(vsub_vi2_vi2_vi2(vcast_vi2_i_i(0x5fe6ec86, 0), vsrl_vi2_vi2_i(vreinterpret_vi2_vd(d + (1e-320).as_vd()), 1)));

  x *= (1.5).as_vd() - (0.5).as_vd()*d*x*x;
  x *= (1.5).as_vd() - (0.5).as_vd()*d*x*x;
  x *= (1.5).as_vd() - (0.5).as_vd()*d*x*x;
  x *= d;

  VDouble2 d2 = ddmul_vd2_vd2_vd2(ddadd2_vd2_vd_vd2(d, ddmul_vd2_vd_vd(x, x)), ddrec_vd2_vd(x));

  x = (d2.0 + d2.1)*q;

  x = vsel_vd_vo_vd_vd(vispinf_vo_vd(d), SLEEF_INFINITY.as_vd(), x);
  x = vsel_vd_vo_vd_vd(d.eq((0.).as_vd()), d, x);
  
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

  $m64x o = max.lt(f64::MIN.as_vd());
  n = vsel_vd_vo_vd_vd(o, n*D1_54.as_vd(), n);
  d = vsel_vd_vo_vd_vd(o, d*D1_54.as_vd(), d);

  VDouble2 t = dddiv_vd2_vd2_vd2(vcast_vd2_vd_vd(n, (0.).as_vd()), vcast_vd2_vd_vd(d, (0.).as_vd()));
  t = ddmul_vd2_vd2_vd(ddsqrt_vd2_vd2(ddadd2_vd2_vd2_vd(ddsqu_vd2_vd2(t), (1.).as_vd())), max);
  $f64x ret = t.0 + t.1;
  ret = vsel_vd_vo_vd_vd(visnan_vo_vd(ret), SLEEF_INFINITY.as_vd(), ret);
  ret = vsel_vd_vo_vd_vd(min.eq((0.).as_vd()), max, ret);
  ret = vsel_vd_vo_vd_vd(visnan_vo_vd(x) | visnan_vo_vd(y), (SLEEF_NAN).as_vd(), ret);
  ret = vsel_vd_vo_vd_vd(x.eq(SLEEF_INFINITY.as_vd()) | y.eq(SLEEF_INFINITY.as_vd()), SLEEF_INFINITY.as_vd(), ret);

  return ret;
}

pub fn xhypot_u35(x: $f64x, y: $f64x) -> $f64x {
  x = vabs_vd_vd(x);
  y = vabs_vd_vd(y);
  $f64x min = x.min(y);
  $f64x max = x.max(y);

  $f64x t = min / max;
  $f64x ret = max*vsqrt_vd_vd(t.mla(t, (1.).as_vd()));
  ret = vsel_vd_vo_vd_vd(min.eq((0.).as_vd()), max, ret);
  ret = vsel_vd_vo_vd_vd(visnan_vo_vd(x) | visnan_vo_vd(y), (SLEEF_NAN).as_vd(), ret);
  ret = vsel_vd_vo_vd_vd(x.eq(SLEEF_INFINITY.as_vd()) | y.eq(SLEEF_INFINITY.as_vd()), SLEEF_INFINITY.as_vd(), ret);

  return ret;
}
#[inline]
fn vtoward0(x: $f64x) -> $f64x { // returns nextafter(x, 0)
  $f64x t = vreinterpret_vd_vm(vadd64_vm_vm_vm(vreinterpret_vm_vd(x), vcast_vm_i_i(-1, -1)));
  return vsel_vd_vo_vd_vd(x.eq((0.).as_vd()), (0.).as_vd(), t);
}
#[inline]
fn vptrunc(x: $f64x) -> $f64x { // round to integer toward 0, positive argument only
#ifdef FULL_FP_ROUNDING
  return vtruncate_vd_vd(x);
#else
  $f64x fr = (-D1_31).as_vd().mla(vcast_vd_vi(vtruncate_vi_vd(x*(1. / D1_31).as_vd())), x);
  fr -= vcast_vd_vi(vtruncate_vi_vd(fr));
  return vsel_vd_vo_vd_vd(vabs_vd_vd(x).ge(D1_52.as_vd()), x, x - fr);
#endif
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xfmod(x: $f64x, y: $f64x) -> $f64x {
  $f64x nu = vabs_vd_vd(x), de = vabs_vd_vd(y), s = (1.).as_vd(), q;
  $m64x o = de.lt(f64::MIN.as_vd());
  nu = vsel_vd_vo_vd_vd(o, nu*D1_54.as_vd(), nu);
  de = vsel_vd_vo_vd_vd(o, de*D1_54.as_vd(), de);
  s  = vsel_vd_vo_vd_vd(o, s*(1. / D1_54).as_vd(), s);
  $f64x rde = vtoward0(vrec_vd_vd(de));
  VDouble2 r = vcast_vd2_vd_vd(nu, (0.).as_vd());

  for(int i=0;i<21;i++) { // ceil(log2(DBL_MAX) / 51) + 1
    q = vsel_vd_vo_vd_vd((de + de).gt(r.0) & r.0.ge(de),
			 (1.).as_vd(), vtoward0(r.0)*rde);
    q = vreinterpret_vd_vm(vand_vm_vm_vm(vreinterpret_vm_vd(vptrunc(q)), vcast_vm_i_i(0xffffffff, 0xfffffffe)));
    r = ddnormalize_vd2_vd2(ddadd2_vd2_vd2_vd2(r, ddmul_vd2_vd_vd(q, -de)));
    if (vtestallones_i_vo64(r.0.lt(de))) break;
  }
  
  $f64x ret = r.0*s;
  ret = vsel_vd_vo_vd_vd((r.0 + r.1).eq(de), (0.).as_vd(), ret);

  ret = vmulsign_vd_vd_vd(ret, x);

  ret = vsel_vd_vo_vd_vd(nu.lt(de), x, ret);
  ret = vsel_vd_vo_vd_vd(de.eq((0.).as_vd()), (SLEEF_NAN).as_vd(), ret);

  return ret;
}

#if defined(ENABLE_SVE) || defined(ENABLE_SVENOFMA)
  typedef __sizeless_struct {
    VDouble2 a, b;
  } dd2;
#else
  typedef struct {
    VDouble2 a, b;
  } dd2;
#endif

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
static CONST dd2 gammak(a: $f64x) {
  VDouble2 clc = vcast_vd2_d_d(0, 0), clln = vcast_vd2_d_d(1, 0), clld = vcast_vd2_d_d(1, 0);
  VDouble2 v = vcast_vd2_d_d(1, 0), x, y, z;
  $f64x t, u;

  $m64x otiny = vabs_vd_vd(a).lt((1e-306).as_vd());
  oref = a.lt((0.5).as_vd());

  x = vsel_vd2_vo_vd2_vd2(otiny, vcast_vd2_d_d(0, 0),
			  vsel_vd2_vo_vd2_vd2(oref, ddadd2_vd2_vd_vd((1.).as_vd(), -a),
					      vcast_vd2_vd_vd(a, (0.).as_vd())));

  $m64x o0 = (0.5).as_vd().le(x.0) & x.0.le((1.1).as_vd());
  $m64x o2 = (2.3).as_vd().le(x.0);
  
  y = ddnormalize_vd2_vd2(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd2_vd(x, (1.).as_vd()), x));
  y = ddnormalize_vd2_vd2(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd2_vd(x, (2.).as_vd()), y));
  y = ddnormalize_vd2_vd2(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd2_vd(x, (3.).as_vd()), y));
  y = ddnormalize_vd2_vd2(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd2_vd(x, (4.).as_vd()), y));

  $m64x o = o2 & x.0.le((7.).as_vd());
  clln = vsel_vd2_vo_vd2_vd2(o, y, clln);

  x = vsel_vd2_vo_vd2_vd2(o, ddadd2_vd2_vd2_vd(x, (5.).as_vd()), x);
  
  t = vsel_vd_vo_vd_vd(o2, vrec_vd_vd(x.0), ddnormalize_vd2_vd2(ddadd2_vd2_vd2_vd(x, vsel_vd_vo_d_d(o0, -1, -2))).0);

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

  y = ddmul_vd2_vd2_vd2(ddadd2_vd2_vd2_vd(x, (-0.5).as_vd()), logk2(x));
  y = ddadd2_vd2_vd2_vd2(y, ddneg_vd2_vd2(x));
  y = ddadd2_vd2_vd2_vd2(y, vcast_vd2_d_d(0.91893853320467278056, -3.8782941580672414498e-17)); // 0.5*log(2*M_PI)

  z = ddadd2_vd2_vd2_vd(ddmul_vd2_vd_vd (u, t), vsel_vd_vo_d_d(o0, -0.4006856343865314862e+0, -0.6735230105319810201e-1));
  z = ddadd2_vd2_vd2_vd(ddmul_vd2_vd2_vd(z, t), vsel_vd_vo_d_d(o0, 0.8224670334241132030e+0, 0.3224670334241132030e+0));
  z = ddadd2_vd2_vd2_vd(ddmul_vd2_vd2_vd(z, t), vsel_vd_vo_d_d(o0, -0.5772156649015328655e+0, 0.4227843350984671345e+0));
  z = ddmul_vd2_vd2_vd(z, t);

  clc = vsel_vd2_vo_vd2_vd2(o2, y, z);
  
  clld = vsel_vd2_vo_vd2_vd2(o2, ddadd2_vd2_vd2_vd(ddmul_vd2_vd_vd(u, t), (1.).as_vd()), clld);
  
  y = clln;

  clc = vsel_vd2_vo_vd2_vd2(otiny, vcast_vd2_d_d(83.1776616671934334590333, 3.67103459631568507221878e-15), // log(2^120)
			    vsel_vd2_vo_vd2_vd2(oref, ddadd2_vd2_vd2_vd2(vcast_vd2_d_d(1.1447298858494001639, 1.026595116270782638e-17), ddneg_vd2_vd2(clc)), clc)); // log(M_PI)
  clln = vsel_vd2_vo_vd2_vd2(otiny, vcast_vd2_d_d(1, 0), vsel_vd2_vo_vd2_vd2(oref, clln, clld));

  if (!vtestallones_i_vo64(vnot_vo64_vo64(oref))) {
    t = a - D1_28*vcast_vd_vi(vtruncate_vi_vd(a*(1. / D1_28).as_vd()));
    x = ddmul_vd2_vd2_vd2(clld, sinpik(t));
  }
  
  clld = vsel_vd2_vo_vd2_vd2(otiny, vcast_vd2_vd_vd(a*(D1_60*D1_60).as_vd(), (0.).as_vd()),
			     vsel_vd2_vo_vd2_vd2(oref, x, y));

  dd2 ret = { clc, dddiv_vd2_vd2_vd2(clln, clld) };

  return ret;
}

pub fn xtgamma_u1(a: $f64x) -> $f64x {
  dd2 d = gammak(a);
  VDouble2 y = ddmul_vd2_vd2_vd2(expk2(d.a), d.b);
  $f64x r = y.0 + y.1;
  $m64x o;

  o = a.eq((-SLEEF_INFINITY).as_vd()) |
				(a.lt((0.).as_vd()) & visint_vo_vd(a)) |
		   (visnumber_vo_vd(a) & a.lt((0.).as_vd()) & visnan_vo_vd(r));
  r = vsel_vd_vo_vd_vd(o, (SLEEF_NAN).as_vd(), r);

  o = ((a.eq(SLEEF_INFINITY.as_vd()) | visnumber_vo_vd(a)) &
				  a.ge((-f64::MIN).as_vd())) &
		    (a.eq((0.).as_vd()) | a.gt((200).as_vd()) | visnan_vo_vd(r));
  r = vsel_vd_vo_vd_vd(o, vmulsign_vd_vd_vd(SLEEF_INFINITY.as_vd(), a), r);
  
  return r;
}

pub fn xlgamma_u1(a: $f64x) -> $f64x {
  dd2 d = gammak(a);
  VDouble2 y = ddadd2_vd2_vd2_vd2(d.a, logk2(ddabs_vd2_vd2(d.b)));
  $f64x r = y.0 + y.1;
  $m64x o;

  o = visinf_vo_vd(a) |
		   (a.le((0.).as_vd()) & visint_vo_vd(a)) |
				(visnumber_vo_vd(a) & visnan_vo_vd(r));
  r = vsel_vd_vo_vd_vd(o, SLEEF_INFINITY.as_vd(), r);

  return r;
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xerf_u1(a: $f64x) -> $f64x {
  $f64x s = a, t, u;
  VDouble2 d;

  a = vabs_vd_vd(a);
  $m64x o0 = a.lt((1.).as_vd());
  $m64x o1 = a.lt((3.7).as_vd());
  $m64x o2 = a.lt((6.).as_vd());
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
  d = ddmul_vd2_vd_vd(t, u);

  d = ddadd2_vd2_vd2_vd2(d, vcast_vd2_vd_vd(vsel_vd_vo_vo_d_d_d(o0, o1, 1.1283791670955125586, 3.4110644736196137587e-08, 0.00024963035690526438285),
					    vsel_vd_vo_vo_d_d_d(o0, o1, 1.5335459613165822674e-17, -2.4875650708323294246e-24, -5.4362665034856259795e-21)));
  d = vsel_vd2_vo_vd2_vd2(o0, ddmul_vd2_vd2_vd(d, a), ddadd_vd2_vd_vd2((1.).as_vd(), ddneg_vd2_vd2(expk2(d))));

  u = vmulsign_vd_vd_vd(vsel_vd_vo_vd_vd(o2, d.0 + d.1, (1.).as_vd()), s);
  u = vsel_vd_vo_vd_vd(visnan_vo_vd(a), (SLEEF_NAN).as_vd(), u);

  return u;
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xerfc_u15(a: $f64x) -> $f64x {
  $f64x s = a, r = (0.).as_vd(), t;
  VDouble2 u, d, x;
  a = vabs_vd_vd(a);
  $m64x o0 = a.lt((1.).as_vd());
  $m64x o1 = a.lt((2.2).as_vd());
  $m64x o2 = a.lt((4.2).as_vd());
  $m64x o3 = a.lt((27.3).as_vd());

  u = vsel_vd2_vo_vd2_vd2(o0, ddmul_vd2_vd_vd(a, a), vsel_vd2_vo_vd2_vd2(o1, vcast_vd2_vd_vd(a, (0.).as_vd()), dddiv_vd2_vd2_vd2(vcast_vd2_d_d(1, 0), vcast_vd2_vd_vd(a, (0.).as_vd()))));

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

  d = ddmul_vd2_vd2_vd(u, t);
  d = ddadd2_vd2_vd2_vd2(d, vcast_vd2_vd_vd(vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 0.11283791670955126141, -0.10277263343147646779, -0.50005180473999022439, -0.5000000000258444377),
					    vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -4.0175691625932118483e-18, -6.2338714083404900225e-18, 2.6362140569041995803e-17, -4.0074044712386992281e-17)));
  d = ddmul_vd2_vd2_vd2(d, u);
  d = ddadd2_vd2_vd2_vd2(d, vcast_vd2_vd_vd(vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.37612638903183753802, -0.63661976742916359662, 1.601106273924963368e-06, 2.3761973137523364792e-13),
					    vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 1.3391897206042552387e-17, 7.6321019159085724662e-18, 1.1974001857764476775e-23, -1.1670076950531026582e-29)));
  d = ddmul_vd2_vd2_vd2(d, u);
  d = ddadd2_vd2_vd2_vd2(d, vcast_vd2_vd_vd(vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 1.1283791670955125586, -1.1283791674717296161, -0.57236496645145429341, -0.57236494292470108114),
					    vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o2, 1.5335459613165822674e-17, 8.0896847755965377194e-17, 3.0704553245872027258e-17, -2.3984352208056898003e-17)));
  
  x = ddmul_vd2_vd2_vd(vsel_vd2_vo_vd2_vd2(o1, d, vcast_vd2_vd_vd(-a, (0.).as_vd())), a);
  x = vsel_vd2_vo_vd2_vd2(o1, x, ddadd2_vd2_vd2_vd2(x, d));
  x = vsel_vd2_vo_vd2_vd2(o0, ddsub_vd2_vd2_vd2(vcast_vd2_d_d(1, 0), x), expk2(x));
  x = vsel_vd2_vo_vd2_vd2(o1, x, ddmul_vd2_vd2_vd2(x, u));

  r = vsel_vd_vo_vd_vd(o3, x.0 + x.1, (0.).as_vd());
  r = vsel_vd_vo_vd_vd(vsignbit_vo_vd(s), (2.).as_vd() - r, r);
  r = vsel_vd_vo_vd_vd(visnan_vo_vd(s), (SLEEF_NAN).as_vd(), r);
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
