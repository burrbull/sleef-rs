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
fn visnegzero_vo_vf(d: VFloat) -> VOpMask {
  return veq_vo_vi2_vi2(vreinterpret_vi2_vf(d), vreinterpret_vi2_vf((-0.).as_vf()));
}

#[inline]
fn VOpMask vnot_vo32_vo32(VOpMask x) {
  return vxor_vo_vo_vo(x, veq_vo_vi2_vi2(vcast_vi2_i(0), vcast_vi2_i(0)));
}
#[inline]
fn vsignbit_vm_vf(f: VFloat) -> vmask {
  return vand_vm_vm_vm(vreinterpret_vm_vf(f), vreinterpret_vm_vf((-0.).as_vf()));
}
#[inline]
fn vmulsign_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat {
  return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x), vsignbit_vm_vf(y)));
}
#[inline]
fn vcopysign_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat {
  return vreinterpret_vf_vm(vxor_vm_vm_vm(vandnot_vm_vm_vm(vreinterpret_vm_vf((-0.).as_vf()), vreinterpret_vm_vf(x)), 
					  vand_vm_vm_vm   (vreinterpret_vm_vf((-0.).as_vf()), vreinterpret_vm_vf(y))));
}
#[inline]
fn vsign_vf_vf(f: VFloat) -> VFloat {
  return vreinterpret_vf_vm(vor_vm_vm_vm(vreinterpret_vm_vf((1.).as_vf()), vand_vm_vm_vm(vreinterpret_vm_vf((-0.).as_vf()), vreinterpret_vm_vf(f))));
}
#[inline]
fn vsignbit_vo_vf(d: VFloat) -> VOpMask {
  return veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vreinterpret_vi2_vf(d), vcast_vi2_i(0x80000000)), vcast_vi2_i(0x80000000));
}
#[inline]
fn vsel_vi2_vf_vf_vi2_vi2(VFloat f0, VFloat f1, VInt2 x, VInt2 y) -> VInt2 {
  return vsel_vi2_vo_vi2_vi2(vlt_vo_vf_vf(f0, f1), x, y);
}
#[inline]
fn vsel_vi2_vf_vi2(VFloat d, VInt2 x) -> VInt2 {
  return vand_vi2_vo_vi2(vsignbit_vo_vf(d), x);
}
#[inline]
fn visint_vo_vf(y: VFloat) -> VOpMask { return veq_vo_vf_vf(vtruncate_vf_vf(y), y); }
#[inline]
fn visnumber_vo_vf(x: VFloat) -> VOpMask { return vnot_vo32_vo32(vor_vo_vo_vo(visinf_vo_vf(x), visnan_vo_vf(x))); }

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)#[inline]
fn vilogbk_vi2_vf(d: VFloat) -> VInt2 {
  VOpMask o = vlt_vo_vf_vf(d, (5.421010862427522e-20).as_vf());
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf((1.8446744073709552e19).as_vf(), d), d);
  VInt2 q = vand_vi2_vi2_vi2(vsrl_vi2_vi2_i(vcast_vi2_vm(vreinterpret_vm_vf(d)), 23), vcast_vi2_i(0xff));
  q = vsub_vi2_vi2_vi2(q, vsel_vi2_vo_vi2_vi2(o, vcast_vi2_i(64 + 0x7f), vcast_vi2_i(0x7f)));
  return q;
}
#[inline]
fn vilogb2k_vi2_vf(d: VFloat) -> VInt2 {
  VInt2 q = vreinterpret_vi2_vf(d);
  q = vsrl_vi2_vi2_i(q, 23);
  q = vand_vi2_vi2_vi2(q, vcast_vi2_i(0xff));
  q = vsub_vi2_vi2_vi2(q, vcast_vi2_i(0x7f));
  return q;
}
#endif

//

pub fn xilogbf(d: VFloat) -> VInt2 {
  VInt2 e = vilogbk_vi2_vf(vabs_vf_vf(d));
  e = vsel_vi2_vo_vi2_vi2(veq_vo_vf_vf(d, (0.).as_vf()), vcast_vi2_i(SLEEF_FP_ILOGB0), e);
  e = vsel_vi2_vo_vi2_vi2(visnan_vo_vf(d), vcast_vi2_i(SLEEF_FP_ILOGBNAN), e);
  e = vsel_vi2_vo_vi2_vi2(visinf_vo_vf(d), vcast_vi2_i(INT_MAX), e);
  return e;
}
#[inline]
fn vpow2i_vf_vi2(VInt2 q) -> VFloat {
  return vreinterpret_vf_vm(vcast_vm_vi2(vsll_vi2_vi2_i(vadd_vi2_vi2_vi2(q, vcast_vi2_i(0x7f)), 23)));
}
#[inline]
fn vldexp_vf_vf_vi2(VFloat x, VInt2 q) -> VFloat {
  VFloat u;
  VInt2 m = vsra_vi2_vi2_i(q, 31);
  m = vsll_vi2_vi2_i(vsub_vi2_vi2_vi2(vsra_vi2_vi2_i(vadd_vi2_vi2_vi2(m, q), 6), m), 4);
  q = vsub_vi2_vi2_vi2(q, vsll_vi2_vi2_i(m, 2));
  m = vadd_vi2_vi2_vi2(m, vcast_vi2_i(0x7f));
  m = vand_vi2_vi2_vi2(vgt_vi2_vi2_vi2(m, vcast_vi2_i(0)), m);
  VInt2 n = vgt_vi2_vi2_vi2(m, vcast_vi2_i(0xff));
  m = vor_vi2_vi2_vi2(vandnot_vi2_vi2_vi2(n, m), vand_vi2_vi2_vi2(n, vcast_vi2_i(0xff)));
  u = vreinterpret_vf_vm(vcast_vm_vi2(vsll_vi2_vi2_i(m, 23)));
  x = vmul_vf_vf_vf(vmul_vf_vf_vf(vmul_vf_vf_vf(vmul_vf_vf_vf(x, u), u), u), u);
  u = vreinterpret_vf_vm(vcast_vm_vi2(vsll_vi2_vi2_i(vadd_vi2_vi2_vi2(q, vcast_vi2_i(0x7f)), 23)));
  return vmul_vf_vf_vf(x, u);
}
#[inline]
fn vldexp2_vf_vf_vi2(VFloat d, VInt2 e) -> VFloat {
  return vmul_vf_vf_vf(vmul_vf_vf_vf(d, vpow2i_vf_vi2(vsra_vi2_vi2_i(e, 1))), vpow2i_vf_vi2(vsub_vi2_vi2_vi2(e, vsra_vi2_vi2_i(e, 1))));
}
#[inline]
fn vldexp3_vf_vf_vi2(VFloat d, VInt2 q) -> VFloat {
  return vreinterpret_vf_vi2(vadd_vi2_vi2_vi2(vreinterpret_vi2_vf(d), vsll_vi2_vi2_i(q, 23)));
}

pub fn xldexpf(VFloat x, VInt2 q) -> VFloat { return vldexp_vf_vf_vi2(x, q); }

#[inline]
fn rempisubf(x: VFloat) -> (VFloat, VInt2) {
 if cfg!(feature="full_fp_rounding") {
  VFloat y = vrint_vf_vf(vmul_vf_vf_vf(x, (4.).as_vf()));
  VInt2 vi = vtruncate_vi2_vf(vsub_vf_vf_vf(y, vmul_vf_vf_vf(vrint_vf_vf(x), (4.).as_vf())));
  ( vsub_vf_vf_vf(x, vmul_vf_vf_vf(y, (0.25).as_vf())), vi )
 } else {
  VFloat fr = vsub_vf_vf_vf(x, vmul_vf_vf_vf(F1_10.as_vf(), vtruncate_vf_vf(vmul_vf_vf_vf(x, (1. / F1_10).as_vf()))));
  VInt2 vi = vadd_vi2_vi2_vi2(vsel_vi2_vo_vi2_vi2(vgt_vo_vf_vf(x, (0.).as_vf()), vcast_vi2_i(4), vcast_vi2_i(3)), vtruncate_vi2_vf(vmul_vf_vf_vf(fr, (8.).as_vf())));
  vi = vsra_vi2_vi2_i(vsub_vi2_vi2_vi2(vand_vi2_vi2_vi2(vcast_vi2_i(7), vi), vcast_vi2_i(3)), 1);
  fr = vsub_vf_vf_vf(fr, vmul_vf_vf_vf((0.25).as_vf(), vtruncate_vf_vf(fr.mla((4.).as_vf(), vmulsign_vf_vf_vf((0.5).as_vf(), x)))));
  fr = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(vabs_vf_vf(fr), (0.25).as_vf()), vsub_vf_vf_vf(fr, vmulsign_vf_vf_vf((0.5).as_vf(), x)), fr);
  fr = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(vabs_vf_vf(fr), (1e+10).as_vf()), (0).as_vf(), fr);
  VOpMask o = veq_vo_vf_vf(vabs_vf_vf(x), (0.12499999254941940308).as_vf());
  fr = vsel_vf_vo_vf_vf(o, x, fr);
  vi = vsel_vi2_vo_vi2_vi2(o, vcast_vi2_i(0), vi);
  ( fr, vi )
  }
}
#[inline]
fn rempif(a: VFloat) -> (VFloat2, VInt2) {
  VFloat2 x, y, z;
  VInt2 ex = vilogb2k_vi2_vf(a);
  if cfg!("enable_avx512f") || cfg!("enable_avx512fnofma") {
    ex = vandnot_vi2_vi2_vi2(vsra_vi2_vi2_i(ex, 31), ex);
    ex = vand_vi2_vi2_vi2(ex, vcast_vi2_i(127));
  }
  ex = vsub_vi2_vi2_vi2(ex, vcast_vi2_i(25));
  VInt2 q = vand_vi2_vo_vi2(vgt_vo_vi2_vi2(ex, vcast_vi2_i(90-25)), vcast_vi2_i(-64));
  a = vldexp3_vf_vf_vi2(a, q);
  ex = vandnot_vi2_vi2_vi2(vsra_vi2_vi2_i(ex, 31), ex);
  ex = vsll_vi2_vi2_i(ex, 2);
  x = dfmul_vf2_vf_vf(a, vgather_vf_p_vi2(rempitabsp, ex));
  let (did, dii) = rempisubf(x.x);
  q = di.i;
  x.x = di.d;
  x = dfnormalize_vf2_vf2(x);
  y = dfmul_vf2_vf_vf(a, vgather_vf_p_vi2(rempitabsp+1, ex));
  x = dfadd2_vf2_vf2_vf2(x, y);
  di = rempisubf(x.x);
  q = vadd_vi2_vi2_vi2(q, di.i);
  x.x = di.d;
  x = dfnormalize_vf2_vf2(x);
  y = vcast_vf2_vf_vf(vgather_vf_p_vi2(rempitabsp+2, ex), vgather_vf_p_vi2(rempitabsp+3, ex));
  y = dfmul_vf2_vf2_vf(y, a);
  x = dfadd2_vf2_vf2_vf2(x, y);
  x = dfnormalize_vf2_vf2(x);
  x = dfmul_vf2_vf2_vf2(x, vcast_vf2_f_f(3.1415927410125732422*2., -8.7422776573475857731e-08*2.));
  x = vsel_vf2_vo_vf2_vf2(vlt_vo_vf_vf(vabs_vf_vf(a), (0.7).as_vf()), vcast_vf2_vf_vf(a, (0.).as_vf()), x);
  ( x, q )
}

pub fn xsinf(d: VFloat) -> VFloat {
  VInt2 q;
  VFloat u, s, r = d;

  if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX2_F.as_vf())))) {
    q = vrint_vi2_vf(vmul_vf_vf_vf(d, M_1_PI_F.as_vf()));
    u = vcast_vf_vi2(q);
    d = u.mla((-PI_A2_F).as_vf(), d);
    d = u.mla((-PI_B2_F).as_vf(), d);
    d = u.mla((-PI_C2_F).as_vf(), d);
  } else if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX_F.as_vf())))) {
    q = vrint_vi2_vf(vmul_vf_vf_vf(d, M_1_PI_F.as_vf()));
    u = vcast_vf_vi2(q);
    d = u.mla((-PI_A_F).as_vf(), d);
    d = u.mla((-PI_B_F).as_vf(), d);
    d = u.mla((-PI_C_F).as_vf(), d);
    d = u.mla((-PI_D_F).as_vf(), d);
  } else {
    let (dfidf, dfii) = rempif(d);
    q = vand_vi2_vi2_vi2(dfii, vcast_vi2_i(3));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vsel_vi2_vo_vi2_vi2(vgt_vo_vf_vf(dfidf.x, (0.).as_vf()), vcast_vi2_i(2), vcast_vi2_i(1)));
    q = vsra_vi2_vi2_i(q, 2);
    VOpMask o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(dfii, vcast_vi2_i(1)), vcast_vi2_i(1));
    VFloat2 x = vcast_vf2_vf_vf(vmulsign_vf_vf_vf((3.1415927410125732422*-0.5).as_vf(), dfidf.x), 
				vmulsign_vf_vf_vf((-8.7422776573475857731e-08*-0.5).as_vf(), dfidf.x));
    x = dfadd2_vf2_vf2_vf2(dfidf, x);
    dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
    d = vadd_vf_vf_vf(dfidf.x, dfidf.y);

    d = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visinf_vo_vf(r), visnan_vo_vf(r)), vreinterpret_vm_vf(d)));
  }

  s = vmul_vf_vf_vf(d, d);

  d = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(1)), vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(d)));

  u = (2.6083159809786593541503e-06).as_vf()
      .mla(s, (-0.0001981069071916863322258).as_vf())
      .mla(s, (0.00833307858556509017944336).as_vf())
      .mla(s, (-0.166666597127914428710938).as_vf());

  u = vadd_vf_vf_vf(vmul_vf_vf_vf(s, vmul_vf_vf_vf(u, d)), d);

  u = vsel_vf_vo_vf_vf(visnegzero_vo_vf(r), r, u);

  return u;
}

pub fn xcosf(d: VFloat) -> VFloat {
  VInt2 q;
  VFloat u, s, r = d;

  if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX2_F.as_vf())))) {
    q = vrint_vi2_vf(vsub_vf_vf_vf(vmul_vf_vf_vf(d, M_1_PI_F.as_vf()), (0.5).as_vf()));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vcast_vi2_i(1));

    u = vcast_vf_vi2(q);
    d = u.mla((-PI_A2_F*0.5).as_vf(), d);
    d = u.mla((-PI_B2_F*0.5).as_vf(), d);
    d = u.mla((-PI_C2_F*0.5).as_vf(), d);
  } else if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX_F.as_vf())))) {
    q = vrint_vi2_vf(vsub_vf_vf_vf(vmul_vf_vf_vf(d, M_1_PI_F.as_vf()), (0.5).as_vf()));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vcast_vi2_i(1));

    u = vcast_vf_vi2(q);
    d = u.mla((-PI_A_F*0.5).as_vf(), d);
    d = u.mla((-PI_B_F*0.5).as_vf(), d);
    d = u.mla((-PI_C_F*0.5).as_vf(), d);
    d = u.mla((-PI_D_F*0.5).as_vf(), d);
  } else {
    let (dfidf, dfii) = rempif(d);
    q = vand_vi2_vi2_vi2(dfii, vcast_vi2_i(3));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vsel_vi2_vo_vi2_vi2(vgt_vo_vf_vf(dfidf.x, (0.).as_vf()), vcast_vi2_i(8), vcast_vi2_i(7)));
    q = vsra_vi2_vi2_i(q, 1);
    VOpMask o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(dfii, vcast_vi2_i(1)), vcast_vi2_i(0));
    VFloat y = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(dfidf.x, (0.).as_vf()), (0.).as_vf(), (-1.).as_vf());
    VFloat2 x = vcast_vf2_vf_vf(vmulsign_vf_vf_vf((3.1415927410125732422*-0.5).as_vf(), y),
				vmulsign_vf_vf_vf((-8.7422776573475857731e-08*-0.5).as_vf(), y));
    x = dfadd2_vf2_vf2_vf2(dfidf, x);
    dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
    d = vadd_vf_vf_vf(dfidf.x, dfidf.y);

    d = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visinf_vo_vf(r), visnan_vo_vf(r)), vreinterpret_vm_vf(d)));
  }

  s = vmul_vf_vf_vf(d, d);

  d = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(0)), vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(d)));

  u = (2.6083159809786593541503e-06).as_vf()
      .mla(s, (-0.0001981069071916863322258).as_vf())
      .mla(s, (0.00833307858556509017944336).as_vf())
      .mla(s, (-0.166666597127914428710938).as_vf());

  u = vadd_vf_vf_vf(vmul_vf_vf_vf(s, vmul_vf_vf_vf(u, d)), d);

  return u;
}

pub fn xtanf(d: VFloat) -> VFloat {
  VInt2 q;
  VOpMask o;
  VFloat u, s, x;

  x = d;

  if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), (TRIGRANGEMAX2_F*0.5).as_vf())))) {
    q = vrint_vi2_vf(vmul_vf_vf_vf(d, (2. * M_1_PI_F).as_vf()));
    u = vcast_vf_vi2(q);
    x = u.mla((-PI_A2_F*0.5).as_vf(), x);
    x = u.mla((-PI_B2_F*0.5).as_vf(), x);
    x = u.mla((-PI_C2_F*0.5).as_vf(), x);
  } else if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX_F.as_vf())))) {
    q = vrint_vi2_vf(vmul_vf_vf_vf(d, (2. * M_1_PI_F.as_vf())));
    u = vcast_vf_vi2(q);
    x = u.mla((-PI_A_F*0.5).as_vf(), x);
    x = u.mla((-PI_B_F*0.5).as_vf(), x);
    x = u.mla((-PI_C_F*0.5).as_vf(), x);
    x = u.mla((-PI_D_F*0.5).as_vf(), x);
  } else {
    let (dfidf, dfii) = rempif(d);
    q = dfii;
    x = vadd_vf_vf_vf(dfidf.x, dfidf.y);
    x = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visinf_vo_vf(d), visnan_vo_vf(d)), vreinterpret_vm_vf(x)));
    x = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), d, x);
  }

  s = vmul_vf_vf_vf(x, x);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(1));
  x = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(x)));

  u = (0.00927245803177356719970703).as_vf()
      .mla(s, (0.00331984995864331722259521).as_vf())
      .mla(s, (0.0242998078465461730957031).as_vf())
      .mla(s, (0.0534495301544666290283203).as_vf())
      .mla(s, (0.133383005857467651367188).as_vf())
      .mla(s, (0.333331853151321411132812).as_vf());

  u = s.mla(vmul_vf_vf_vf(u, x), x);

  u = vsel_vf_vo_vf_vf(o, vrec_vf_vf(u), u);

  return u;
}

pub fn xsinf_u1(d: VFloat) -> VFloat {
  VInt2 q;
  VFloat u, v;
  VFloat2 s, t, x;

  if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX2_F.as_vf())))) {
    u = vrint_vf_vf(vmul_vf_vf_vf(d, M_1_PI_F.as_vf()));
    q = vrint_vi2_vf(u);
    v = u.mla((-PI_A2_F).as_vf(), d);
    s = dfadd2_vf2_vf_vf(v, vmul_vf_vf_vf(u, (-PI_B2_F).as_vf()));
    s = dfadd_vf2_vf2_vf(s, vmul_vf_vf_vf(u, (-PI_C2_F).as_vf()));
  } else {
    let (dfidf, dfii) = rempif(d);
    q = vand_vi2_vi2_vi2(dfii, vcast_vi2_i(3));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vsel_vi2_vo_vi2_vi2(vgt_vo_vf_vf(dfidf.x, (0.).as_vf()), vcast_vi2_i(2), vcast_vi2_i(1)));
    q = vsra_vi2_vi2_i(q, 2);
    VOpMask o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(dfii, vcast_vi2_i(1)), vcast_vi2_i(1));
    VFloat2 x = vcast_vf2_vf_vf(vmulsign_vf_vf_vf((3.1415927410125732422*-0.5).as_vf(), dfidf.x), 
				vmulsign_vf_vf_vf((-8.7422776573475857731e-08*-0.5).as_vf(), dfidf.x));
    x = dfadd2_vf2_vf2_vf2(dfidf, x);
    dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
    s = dfnormalize_vf2_vf2(dfidf);

    s.x = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visinf_vo_vf(d), visnan_vo_vf(d)), vreinterpret_vm_vf(s.x)));
  }

  t = s;
  s = dfsqu_vf2_vf2(s);

  u = (2.6083159809786593541503e-06).as_vf()
      .mla(s.x, (-0.0001981069071916863322258).as_vf())
      .mla(s.x, (0.00833307858556509017944336).as_vf());

  x = dfadd_vf2_vf_vf2((1.).as_vf(), dfmul_vf2_vf2_vf2(dfadd_vf2_vf_vf((-0.166666597127914428710938).as_vf(), vmul_vf_vf_vf(u, s.x)), s));

  u = dfmul_vf_vf2_vf2(t, x);

  u = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(1)), vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(u)));

  u = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), d, u);

  return u;
}

pub fn xcosf_u1(d: VFloat) -> VFloat {
  VInt2 q;
  VFloat u;
  VFloat2 s, t, x;

  if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX2_F.as_vf())))) {
    VFloat dq = vrint_vf_vf(d.mla(M_1_PI_F.as_vf(), (-0.5).as_vf())).mla(
				 (2.).as_vf(), (1.).as_vf());
    q = vrint_vi2_vf(dq);
    s = dfadd2_vf2_vf_vf (d, vmul_vf_vf_vf(dq, (-PI_A2_F*0.5).as_vf()));
    s = dfadd2_vf2_vf2_vf(s, vmul_vf_vf_vf(dq, (-PI_B2_F*0.5).as_vf()));
    s = dfadd2_vf2_vf2_vf(s, vmul_vf_vf_vf(dq, (-PI_C2_F*0.5).as_vf()));
  } else {
    let (dfidf, dfii) = rempif(d);
    q = vand_vi2_vi2_vi2(dfii, vcast_vi2_i(3));
    q = vadd_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, q), vsel_vi2_vo_vi2_vi2(vgt_vo_vf_vf(dfidf.x, (0.).as_vf()), vcast_vi2_i(8), vcast_vi2_i(7)));
    q = vsra_vi2_vi2_i(q, 1);
    VOpMask o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(dfii, vcast_vi2_i(1)), vcast_vi2_i(0));
    VFloat y = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(dfidf.x, (0.).as_vf()), (0.).as_vf(), (-1).as_vf());
    VFloat2 x = vcast_vf2_vf_vf(vmulsign_vf_vf_vf((3.1415927410125732422*-0.5).as_vf(), y),
				vmulsign_vf_vf_vf((-8.7422776573475857731e-08*-0.5).as_vf(), y));
    x = dfadd2_vf2_vf2_vf2(dfidf, x);
    dfidf = vsel_vf2_vo_vf2_vf2(o, x, dfidf);
    s = dfnormalize_vf2_vf2(dfidf);

    s.x = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visinf_vo_vf(d), visnan_vo_vf(d)), vreinterpret_vm_vf(s.x)));
  }

  t = s;
  s = dfsqu_vf2_vf2(s);

  u = (2.6083159809786593541503e-06).as_vf()
      .mla(s.x, (-0.0001981069071916863322258).as_vf())
      .mla(s.x, (0.00833307858556509017944336).as_vf());

  x = dfadd_vf2_vf_vf2((1.).as_vf(), dfmul_vf2_vf2_vf2(dfadd_vf2_vf_vf((-0.166666597127914428710938).as_vf(), vmul_vf_vf_vf(u, s.x)), s));

  u = dfmul_vf_vf2_vf2(t, x);

  u = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(0)), vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(u)));
  
  return u;
}


pub fn xsincosf(d: VFloat) -> (VFloat, VFloat) {
  VInt2 q;
  VOpMask o;
  VFloat u, s, t, rx, ry;
  VFloat2 r;

  s = d;

  if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX2_F.as_vf())))) {
    q = vrint_vi2_vf(vmul_vf_vf_vf(d, M_2_PI_F.as_vf()));
    u = vcast_vf_vi2(q);
    s = u.mla((-PI_A2_F*0.5).as_vf(), s);
    s = u.mla((-PI_B2_F*0.5).as_vf(), s);
    s = u.mla((-PI_C2_F*0.5).as_vf(), s);
  } else if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX_F.as_vf())))) {
    q = vrint_vi2_vf(vmul_vf_vf_vf(d, M_2_PI_F.as_vf()));
    u = vcast_vf_vi2(q);
    s = u.mla((-PI_A_F*0.5).as_vf(), s);
    s = u.mla((-PI_B_F*0.5).as_vf(), s);
    s = u.mla((-PI_C_F*0.5).as_vf(), s);
    s = u.mla((-PI_D_F*0.5).as_vf(), s);
  } else {
    let (dfidf, dfii) = rempif(d);
    q = dfii;
    s = vadd_vf_vf_vf(dfidf.x, dfidf.y);
    s = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visinf_vo_vf(d), visnan_vo_vf(d)), vreinterpret_vm_vf(s)));
  }

  t = s;

  s = vmul_vf_vf_vf(s, s);

  u = (-0.000195169282960705459117889).as_vf()
      .mla(s, (0.00833215750753879547119141).as_vf())
      .mla(s, (-0.166666537523269653320312).as_vf());

  rx = vmul_vf_vf_vf(u, s).mla(t, t);
  rx = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), (-0.).as_vf(), rx);

  u = (-2.71811842367242206819355e-07).as_vf()
      .mla(s, (2.47990446951007470488548e-05).as_vf())
      .mla(s, (-0.00138888787478208541870117).as_vf())
      .mla(s, (0.0416666641831398010253906).as_vf())
      .mla(s, (-0.5).as_vf());

  ry = s.mla(u, (1.).as_vf());

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(0));
  rsin = vsel_vf_vo_vf_vf(o, rx, ry);
  rcos = vsel_vf_vo_vf_vf(o, ry, rx);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(2));
  rsin = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(rsin)));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(2)), vcast_vi2_i(2));
  rcos = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(rcos)));

  (rsin, rcos)
}

pub fn xsincosf_u1(d: VFloat) -> (VFloat, VFloat) {
  VInt2 q;
  VOpMask o;
  VFloat u, v, rx, ry;
  VFloat2 r, s, t, x;

  if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX2_F.as_vf())))) {
    u = vrint_vf_vf(vmul_vf_vf_vf(d, (2. * M_1_PI_F).as_vf()));
    q = vrint_vi2_vf(u);
    v = u.mla((-PI_A2_F*0.5).as_vf(), d);
    s = dfadd2_vf2_vf_vf(v, vmul_vf_vf_vf(u, (-PI_B2_F*0.5).as_vf()));
    s = dfadd_vf2_vf2_vf(s, vmul_vf_vf_vf(u, (-PI_C2_F*0.5).as_vf()));
  } else {
    let (dfidf, dfii) = rempif(d);
    q = dfii;
    s = dfidf;
    o = vor_vo_vo_vo(visinf_vo_vf(d), visnan_vo_vf(d));
    s.x = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(s.x)));
  }

  t = s;

  s.x = dfsqu_vf_vf2(s);

  u = (-0.000195169282960705459117889).as_vf()
      .mla(s.x, (0.00833215750753879547119141).as_vf())
      .mla(s.x, (-0.166666537523269653320312).as_vf());

  u = vmul_vf_vf_vf(u, vmul_vf_vf_vf(s.x, t.x));

  x = dfadd_vf2_vf2_vf(t, u);
  rx = vadd_vf_vf_vf(x.x, x.y);

  rx = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), (-0.).as_vf(), rx);

  u = (-2.71811842367242206819355e-07).as_vf()
      .mla(s.x, (2.47990446951007470488548e-05).as_vf())
      .mla(s.x, (-0.00138888787478208541870117).as_vf())
      .mla(s.x, (0.0416666641831398010253906).as_vf())
      .mla(s.x, (-0.5).as_vf());

  x = dfadd_vf2_vf_vf2((1.).as_vf(), dfmul_vf2_vf_vf(s.x, u));
  ry = vadd_vf_vf_vf(x.x, x.y);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(0));
  rsin = vsel_vf_vo_vf_vf(o, rx, ry);
  rcos = vsel_vf_vo_vf_vf(o, ry, rx);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(2));
  rsin = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(rsin)));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(2)), vcast_vi2_i(2));
  rcos = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(rcos)));

  (rsin, rcos)
}

pub fn xsincospif_u05(d: VFloat) -> (VFloat, VFloat) {
  VOpMask o;
  VFloat u, s, t, rx, ry;
  VFloat2 r, x, s2;

  u = vmul_vf_vf_vf(d, (4.).as_vf());
  VInt2 q = vtruncate_vi2_vf(u);
  q = vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vxor_vi2_vi2_vi2(vsrl_vi2_vi2_i(q, 31), vcast_vi2_i(1))), vcast_vi2_i(~1));
  s = vsub_vf_vf_vf(u, vcast_vf_vi2(q));

  t = s;
  s = vmul_vf_vf_vf(s, s);
  s2 = dfmul_vf2_vf_vf(t, t);
  
  //

  u = (+0.3093842054e-6).as_vf()
      .mla(s, (-0.3657307388e-4).as_vf())
      .mla(s, (+0.2490393585e-2).as_vf());
  x = dfadd2_vf2_vf_vf2(vmul_vf_vf_vf(u, s), vcast_vf2_f_f(-0.080745510756969451904, -1.3373665339076936258e-09));
  x = dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(s2, x), vcast_vf2_f_f(0.78539818525314331055, -2.1857338617566484855e-08));

  x = dfmul_vf2_vf2_vf(x, t);
  rx = vadd_vf_vf_vf(x.x, x.y);

  rx = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), (-0.).as_vf(), rx);
  
  //
  
  u = (-0.2430611801e-7).as_vf()
      .mla(s, (+0.3590577080e-5).as_vf())
      .mla(s, (-0.3259917721e-3).as_vf());
  x = dfadd2_vf2_vf_vf2(vmul_vf_vf_vf(u, s), vcast_vf2_f_f(0.015854343771934509277, 4.4940051354032242811e-10));
  x = dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(s2, x), vcast_vf2_f_f(-0.30842512845993041992, -9.0728339030733922277e-09));

  x = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf2(x, s2), (1.).as_vf());
  ry = vadd_vf_vf_vf(x.x, x.y);

  //

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(0));
  rsin = vsel_vf_vo_vf_vf(o, rx, ry);
  rcos = vsel_vf_vo_vf_vf(o, ry, rx);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(4)), vcast_vi2_i(4));
  rsin = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(rsin)));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(4)), vcast_vi2_i(4));
  rcos = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(rcos)));

  o = vgt_vo_vf_vf(vabs_vf_vf(d), (1e+7).as_vf());
  rsin = vreinterpret_vf_vm(vandnot_vm_vo32_vm(o, vreinterpret_vm_vf(rsin)));
  rcos = vreinterpret_vf_vm(vandnot_vm_vo32_vm(o, vreinterpret_vm_vf(rcos)));
  
  o = visinf_vo_vf(d);
  rsin = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(rsin)));
  rcos = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(rcos)));

  (rsin, rcos)
}

pub fn xsincospif_u35(d: VFloat) -> (VFloat, VFloat) {
  VOpMask o;
  VFloat u, s, t, rx, ry;
  VFloat2 r;

  u = vmul_vf_vf_vf(d, (4.).as_vf());
  VInt2 q = vtruncate_vi2_vf(u);
  q = vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vxor_vi2_vi2_vi2(vsrl_vi2_vi2_i(q, 31), vcast_vi2_i(1))), vcast_vi2_i(~1));
  s = vsub_vf_vf_vf(u, vcast_vf_vi2(q));

  t = s;
  s = vmul_vf_vf_vf(s, s);
  
  //

  u = (-0.3600925265e-4).as_vf()
      .mla(s, (+0.2490088111e-2).as_vf())
      .mla(s, (-0.8074551076e-1).as_vf())
      .mla(s, (+0.7853981853e+0).as_vf());

  rx = vmul_vf_vf_vf(u, t);

  //
  
  u = (+0.3539815225e-5).as_vf()
      .mla(s, (-0.3259574005e-3).as_vf())
      .mla(s, (+0.1585431583e-1).as_vf())
      .mla(s, (-0.3084251285e+0).as_vf())
      .mla(s, (1.).as_vf());

  ry = u;

  //

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(0));
  r.x = vsel_vf_vo_vf_vf(o, rx, ry);
  r.y = vsel_vf_vo_vf_vf(o, ry, rx);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(4)), vcast_vi2_i(4));
  r.x = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(r.x)));

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(4)), vcast_vi2_i(4));
  r.y = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(r.y)));

  o = vgt_vo_vf_vf(vabs_vf_vf(d), (1e+7).as_vf());
  r.x = vreinterpret_vf_vm(vandnot_vm_vo32_vm(o, vreinterpret_vm_vf(r.x)));
  r.y = vreinterpret_vf_vm(vandnot_vm_vo32_vm(o, vreinterpret_vm_vf(r.y)));
  
  o = visinf_vo_vf(d);
  r.x = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(r.x)));
  r.y = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(r.y)));

  (rsin, rcos)
}

pub fn xmodff(x: VFloat) -> (VFloat, VFloat) {
  VFloat fr = vsub_vf_vf_vf(x, vcast_vf_vi2(vtruncate_vi2_vf(x)));
  fr = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(vabs_vf_vf(x), F1_23.as_vf()), (0.).as_vf(), fr);

  VFloat2 ret;

  retx = vcopysign_vf_vf_vf(fr, x);
  rety = vcopysign_vf_vf_vf(vsub_vf_vf_vf(x, fr), x);

  (retx, rety)
}


pub fn xtanf_u1(d: VFloat) -> VFloat {
  VInt2 q;
  VFloat u, v;
  VFloat2 s, t, x;
  VOpMask o;

  if (LIKELY(vtestallones_i_vo32(vlt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX2_F.as_vf())))) {
    u = vrint_vf_vf(vmul_vf_vf_vf(d, (2. * M_1_PI_F).as_vf()));
    q = vrint_vi2_vf(u);
    v = u.mla((-PI_A2_F*0.5).as_vf(), d);
    s = dfadd2_vf2_vf_vf(v, vmul_vf_vf_vf(u, (-PI_B2_F*0.5).as_vf()));
    s = dfadd_vf2_vf2_vf(s, vmul_vf_vf_vf(u, (-PI_C2_F*0.5).as_vf()));
  } else {
    let (dfidf, dfii) = rempif(d);
    q = dfii;
    s = dfidf;
    o = vor_vo_vo_vo(visinf_vo_vf(d), visnan_vo_vf(d));
    s.x = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(s.x)));
    s.y = vreinterpret_vf_vm(vor_vm_vo32_vm(o, vreinterpret_vm_vf(s.y)));
  }

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(1));
  vmask n = vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf()));
  s.x = vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(s.x), n));
  s.y = vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(s.y), n));

  t = s;
  s = dfsqu_vf2_vf2(s);
  s = dfnormalize_vf2_vf2(s);

  u = (0.00446636462584137916564941).as_vf()
      .mla(s.x, (-8.3920182078145444393158e-05).as_vf())
      .mla(s.x, (0.0109639242291450500488281).as_vf())
      .mla(s.x, (0.0212360303848981857299805).as_vf())
      .mla(s.x, (0.0540687143802642822265625).as_vf());

  x = dfadd_vf2_vf_vf((0.133325666189193725585938).as_vf(), vmul_vf_vf_vf(u, s.x));
  x = dfadd_vf2_vf_vf2((1.).as_vf(), dfmul_vf2_vf2_vf2(dfadd_vf2_vf_vf2((0.33333361148834228515625).as_vf(), dfmul_vf2_vf2_vf2(s, x)), s));
  x = dfmul_vf2_vf2_vf2(t, x);

  x = vsel_vf2_vo_vf2_vf2(o, dfrec_vf2_vf2(x), x);

  u = vadd_vf_vf_vf(x.x, x.y);

  u = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), d, u);
  
  return u;
}

pub fn xatanf(d: VFloat) -> VFloat {
  VFloat s, t, u;
  VInt2 q;

  q = vsel_vi2_vf_vi2(d, vcast_vi2_i(2));
  s = vabs_vf_vf(d);

  q = vsel_vi2_vf_vf_vi2_vi2((1.).as_vf(), s, vadd_vi2_vi2_vi2(q, vcast_vi2_i(1)), q);
  s = vsel_vf_vo_vf_vf(vlt_vo_vf_vf((1.).as_vf(), s), vrec_vf_vf(s), s);

  t = vmul_vf_vf_vf(s, s);

  u = (0.00282363896258175373077393).as_vf()
      .mla(t, (-0.0159569028764963150024414).as_vf())
      .mla(t, (0.0425049886107444763183594).as_vf())
      .mla(t, (-0.0748900920152664184570312).as_vf())
      .mla(t, (0.106347933411598205566406).as_vf())
      .mla(t, (-0.142027363181114196777344).as_vf())
      .mla(t, (0.199926957488059997558594).as_vf())
      .mla(t, (-0.333331018686294555664062).as_vf());

  t = s.mla(vmul_vf_vf_vf(t, u), s);

  t = vsel_vf_vo_vf_vf(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(1)), vcast_vi2_i(1)), vsub_vf_vf_vf((M_PI_F/2.).as_vf(), t), t);

  t = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(2)), vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(t)));

#if defined(ENABLE_NEON32) || defined(ENABLE_NEON32VFPV4)
  t = vsel_vf_vo_vf_vf(visinf_vo_vf(d), vmulsign_vf_vf_vf((1.5874010519681994747517056).as_vf(), d), t);
#endif

  return t;
}
#[inline]
fn atan2kf(y: VFloat, x: VFloat) -> VFloat {
  VFloat s, t, u;
  VInt2 q;
  VOpMask p;

  q = vsel_vi2_vf_vi2(x, vcast_vi2_i(-2));
  x = vabs_vf_vf(x);

  q = vsel_vi2_vf_vf_vi2_vi2(x, y, vadd_vi2_vi2_vi2(q, vcast_vi2_i(1)), q);
  p = vlt_vo_vf_vf(x, y);
  s = vsel_vf_vo_vf_vf(p, vneg_vf_vf(x), y);
  t = vmax_vf_vf_vf(x, y);

  s = vdiv_vf_vf_vf(s, t);
  t = vmul_vf_vf_vf(s, s);

  u = (0.00282363896258175373077393).as_vf()
      .mla(t, (-0.0159569028764963150024414).as_vf())
      .mla(t, (0.0425049886107444763183594).as_vf())
      .mla(t, (-0.0748900920152664184570312).as_vf())
      .mla(t, (0.106347933411598205566406).as_vf())
      .mla(t, (-0.142027363181114196777344).as_vf())
      .mla(t, (0.199926957488059997558594).as_vf())
      .mla(t, (-0.333331018686294555664062).as_vf());

  t = s.mla(vmul_vf_vf_vf(t, u), s);
  t = vcast_vf_vi2(q).mla((M_PI_F/2.).as_vf(), t);

  return t;
}
#[inline]
fn visinf2_vf_vf_vf(d: VFloat, m: VFloat) -> VFloat {
  return vreinterpret_vf_vm(vand_vm_vo32_vm(visinf_vo_vf(d), vor_vm_vm_vm(vsignbit_vm_vf(d), vreinterpret_vm_vf(m))));
}

pub fn xatan2f(y: VFloat, x: VFloat) -> VFloat {
  VFloat r = atan2kf(vabs_vf_vf(y), x);

  r = vmulsign_vf_vf_vf(r, x);
  r = vsel_vf_vo_vf_vf(vor_vo_vo_vo(visinf_vo_vf(x), veq_vo_vf_vf(x, (0.).as_vf())), vsub_vf_vf_vf((M_PI_F/2.).as_vf(), visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf((M_PI_F/2.).as_vf(), x))), r);
  r = vsel_vf_vo_vf_vf(visinf_vo_vf(y), vsub_vf_vf_vf((M_PI_F/2.).as_vf(), visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf((M_PI_F/4.).as_vf(), x))), r);

  r = vsel_vf_vo_vf_vf(veq_vo_vf_vf(y, (0.).as_vf()), vreinterpret_vf_vm(vand_vm_vo32_vm(vsignbit_vo_vf(x), vreinterpret_vm_vf(M_PI_F.as_vf()))), r);

  r = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visnan_vo_vf(x), visnan_vo_vf(y)), vreinterpret_vm_vf(vmulsign_vf_vf_vf(r, y))));
  return r;
}

pub fn xasinf(d: VFloat) -> VFloat {
  VOpMask o = vlt_vo_vf_vf(vabs_vf_vf(d), (0.5).as_vf());
  VFloat x2 = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, d), vmul_vf_vf_vf(vsub_vf_vf_vf((1.).as_vf(), vabs_vf_vf(d)), (0.5).as_vf()));
  VFloat x = vsel_vf_vo_vf_vf(o, vabs_vf_vf(d), vsqrt_vf_vf(x2)), u;

  u = (+0.4197454825e-1).as_vf()
      .mla(x2, (+0.2424046025e-1).as_vf())
      .mla(x2, (+0.4547423869e-1).as_vf())
      .mla(x2, (+0.7495029271e-1).as_vf())
      .mla(x2, (+0.1666677296e+0).as_vf())
      .mla(vmul_vf_vf_vf(x, x2), x);

  VFloat r = vsel_vf_vo_vf_vf(o, u, u.mla((-2.).as_vf(), (M_PI_F/2.).as_vf()));
  return vmulsign_vf_vf_vf(r, d);
}

pub fn xacosf(d: VFloat) -> VFloat {
  VOpMask o = vlt_vo_vf_vf(vabs_vf_vf(d), (0.5).as_vf());
  VFloat x2 = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, d),
				vmul_vf_vf_vf(vsub_vf_vf_vf((1.).as_vf(), vabs_vf_vf(d)), (0.5).as_vf())), u;
  VFloat x = vsel_vf_vo_vf_vf(o, vabs_vf_vf(d), vsqrt_vf_vf(x2));
  x = vsel_vf_vo_vf_vf(veq_vo_vf_vf(vabs_vf_vf(d), (1.).as_vf()), (0.).as_vf(), x);

  u = (+0.4197454825e-1).as_vf()
      .mla(x2, (+0.2424046025e-1).as_vf())
      .mla(x2, (+0.4547423869e-1).as_vf())
      .mla(x2, (+0.7495029271e-1).as_vf())
      .mla(x2, (+0.1666677296e+0).as_vf());
  u = vmul_vf_vf_vf(u, vmul_vf_vf_vf(x2, x));

  VFloat y = vsub_vf_vf_vf((3.1415926535897932/2.).as_vf(), vadd_vf_vf_vf(vmulsign_vf_vf_vf(x, d), vmulsign_vf_vf_vf(u, d)));
  x = vadd_vf_vf_vf(x, u);
  VFloat r = vsel_vf_vo_vf_vf(o, y, vmul_vf_vf_vf(x, (2.).as_vf()));
  return vsel_vf_vo_vf_vf(vandnot_vo_vo_vo(o, vlt_vo_vf_vf(d, (0.).as_vf())),
			  dfadd_vf2_vf2_vf(vcast_vf2_f_f(3.1415927410125732422,-8.7422776573475857731e-08),
					   vneg_vf_vf(r)).x, r);
}

//
#[inline]
fn atan2kf_u1(VFloat2 y, VFloat2 x) -> VFloat2 {
  VFloat u;
  VFloat2 s, t;
  VInt2 q;
  VOpMask p;
  vmask r;
  
  q = vsel_vi2_vf_vf_vi2_vi2(x.x, (0.).as_vf(), vcast_vi2_i(-2), vcast_vi2_i(0));
  p = vlt_vo_vf_vf(x.x, (0.).as_vf());
  r = vand_vm_vo32_vm(p, vreinterpret_vm_vf((-0.).as_vf()));
  x.x = vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x.x), r));
  x.y = vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x.y), r));

  q = vsel_vi2_vf_vf_vi2_vi2(x.x, y.x, vadd_vi2_vi2_vi2(q, vcast_vi2_i(1)), q);
  p = vlt_vo_vf_vf(x.x, y.x);
  s = vsel_vf2_vo_vf2_vf2(p, dfneg_vf2_vf2(x), y);
  t = vsel_vf2_vo_vf2_vf2(p, y, x);

  s = dfdiv_vf2_vf2_vf2(s, t);
  t = dfsqu_vf2_vf2(s);
  t = dfnormalize_vf2_vf2(t);

  u = (-0.00176397908944636583328247).as_vf()
      .mla(t.x, (0.0107900900766253471374512).as_vf())
      .mla(t.x, (-0.0309564601629972457885742).as_vf())
      .mla(t.x, (0.0577365085482597351074219).as_vf())
      .mla(t.x, (-0.0838950723409652709960938).as_vf())
      .mla(t.x, (0.109463557600975036621094).as_vf())
      .mla(t.x, (-0.142626821994781494140625).as_vf())
      .mla(t.x, (0.199983194470405578613281).as_vf());

  t = dfmul_vf2_vf2_vf2(t, dfadd_vf2_vf_vf((-0.333332866430282592773438).as_vf(), vmul_vf_vf_vf(u, t.x)));
  t = dfmul_vf2_vf2_vf2(s, dfadd_vf2_vf_vf2((1.).as_vf(), t));
  t = dfadd_vf2_vf2_vf2(dfmul_vf2_vf2_vf(vcast_vf2_f_f(1.5707963705062866211, -4.3711388286737928865e-08), vcast_vf_vi2(q)), t);

  return t;
}

pub fn xatan2f_u1(y: VFloat, x: VFloat) -> VFloat {
  VOpMask o = vlt_vo_vf_vf(vabs_vf_vf(x), (2.9387372783541830947e-39).as_vf()); // nexttowardf((1.0 / FLT_MAX), 1)
  x = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(x, F1_24.as_vf(), x);
  y = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(y, F1_24.as_vf(), y);
  
  VFloat2 d = atan2kf_u1(vcast_vf2_vf_vf(vabs_vf_vf(y), (0.).as_vf()), vcast_vf2_vf_vf(x, (0.).as_vf()));
  VFloat r = vadd_vf_vf_vf(d.x, d.y);

  r = vmulsign_vf_vf_vf(r, x);
  r = vsel_vf_vo_vf_vf(vor_vo_vo_vo(visinf_vo_vf(x), veq_vo_vf_vf(x, (0.).as_vf())), vsub_vf_vf_vf((M_PI_F/2.).as_vf(), visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf((M_PI_F/2.).as_vf(), x))), r);
  r = vsel_vf_vo_vf_vf(visinf_vo_vf(y), vsub_vf_vf_vf((M_PI_F/2.).as_vf(), visinf2_vf_vf_vf(x, vmulsign_vf_vf_vf((M_PI_F/4.).as_vf(), x))), r);
  r = vsel_vf_vo_vf_vf(veq_vo_vf_vf(y, (0.).as_vf()), vreinterpret_vf_vm(vand_vm_vo32_vm(vsignbit_vo_vf(x), vreinterpret_vm_vf(M_PI_F.as_vf()))), r);

  r = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visnan_vo_vf(x), visnan_vo_vf(y)), vreinterpret_vm_vf(vmulsign_vf_vf_vf(r, y))));
  return r;
}

pub fn xasinf_u1(d: VFloat) -> VFloat {
  VOpMask o = vlt_vo_vf_vf(vabs_vf_vf(d), (0.5).as_vf());
  VFloat x2 = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, d), vmul_vf_vf_vf(vsub_vf_vf_vf((1.).as_vf(), vabs_vf_vf(d)), (0.5).as_vf())), u;
  VFloat2 x = vsel_vf2_vo_vf2_vf2(o, vcast_vf2_vf_vf(vabs_vf_vf(d), (0.).as_vf()), dfsqrt_vf2_vf(x2));
  x = vsel_vf2_vo_vf2_vf2(veq_vo_vf_vf(vabs_vf_vf(d), (1.).as_vf()), vcast_vf2_f_f(0, 0), x);

  u = (+0.4197454825e-1).as_vf()
      .mla(x2, (+0.2424046025e-1).as_vf())
      .mla(x2, (+0.4547423869e-1).as_vf())
      .mla(x2, (+0.7495029271e-1).as_vf())
      .mla(x2, (+0.1666677296e+0).as_vf());
  u = vmul_vf_vf_vf(u, vmul_vf_vf_vf(x2, x.x));

  VFloat2 y = dfsub_vf2_vf2_vf(dfsub_vf2_vf2_vf2(vcast_vf2_f_f(3.1415927410125732422/4.,-8.7422776573475857731e-08/4.), x), u);
  
  VFloat r = vsel_vf_vo_vf_vf(o, vadd_vf_vf_vf(u, x.x),
			       vmul_vf_vf_vf(vadd_vf_vf_vf(y.x, y.y), (2.).as_vf()));
  return vmulsign_vf_vf_vf(r, d);
}

pub fn xacosf_u1(d: VFloat) -> VFloat {
  VOpMask o = vlt_vo_vf_vf(vabs_vf_vf(d), (0.5).as_vf());
  VFloat x2 = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, d), vmul_vf_vf_vf(vsub_vf_vf_vf((1.).as_vf(), vabs_vf_vf(d)), (0.5).as_vf())), u;
  VFloat2 x = vsel_vf2_vo_vf2_vf2(o, vcast_vf2_vf_vf(vabs_vf_vf(d), (0.).as_vf()), dfsqrt_vf2_vf(x2));
  x = vsel_vf2_vo_vf2_vf2(veq_vo_vf_vf(vabs_vf_vf(d), (1.).as_vf()), vcast_vf2_f_f(0, 0), x);

  u = (+0.4197454825e-1).as_vf()
      .mla(x2, (+0.2424046025e-1).as_vf())
      .mla(x2, (+0.4547423869e-1).as_vf())
      .mla(x2, (+0.7495029271e-1).as_vf())
      .mla(x2, (+0.1666677296e+0).as_vf());
  u = vmul_vf_vf_vf(u, vmul_vf_vf_vf(x2, x.x));

  VFloat2 y = dfsub_vf2_vf2_vf2(vcast_vf2_f_f(3.1415927410125732422/2., -8.7422776573475857731e-08/2.),
				 dfadd_vf2_vf_vf(vmulsign_vf_vf_vf(x.x, d), vmulsign_vf_vf_vf(u, d)));
  x = dfadd_vf2_vf2_vf(x, u);

  y = vsel_vf2_vo_vf2_vf2(o, y, dfscale_vf2_vf2_vf(x, (2.).as_vf()));
  
  y = vsel_vf2_vo_vf2_vf2(vandnot_vo_vo_vo(o, vlt_vo_vf_vf(d, (0.).as_vf())),
			  dfsub_vf2_vf2_vf2(vcast_vf2_f_f(3.1415927410125732422, -8.7422776573475857731e-08), y), y);

  return vadd_vf_vf_vf(y.x, y.y);
}

pub fn xatanf_u1(d: VFloat) -> VFloat {
  VFloat2 d2 = atan2kf_u1(vcast_vf2_vf_vf(vabs_vf_vf(d), (0.).as_vf()), vcast_vf2_f_f(1, 0));
  VFloat r = vadd_vf_vf_vf(d2.x, d2.y);
  r = vsel_vf_vo_vf_vf(visinf_vo_vf(d), (1.570796326794896557998982).as_vf(), r);
  return vmulsign_vf_vf_vf(r, d);
}

//

pub fn xlogf(d: VFloat) -> VFloat {
  VFloat x, x2, t, m;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VOpMask o = vlt_vo_vf_vf(d, f32::MIN.as_vf());
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, (F1_32 * F1_32).as_vf()), d);
  VInt2 e = vilogb2k_vi2_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, vcast_vi2_i(64)), e);
#else
  VFloat e = vgetexp_vf_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), (128.).as_vf(), e);
  m = vgetmant_vf_vf(d);
#endif
  
  x = vdiv_vf_vf_vf(vadd_vf_vf_vf((-1.).as_vf(), m), vadd_vf_vf_vf((1.).as_vf(), m));
  x2 = vmul_vf_vf_vf(x, x);

  t = (0.2392828464508056640625).as_vf()
      .mla(x2, (0.28518211841583251953125).as_vf())
      .mla(x2, (0.400005877017974853515625).as_vf())
      .mla(x2, (0.666666686534881591796875).as_vf())
      .mla(x2, (2.).as_vf());

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  x = x.mla(t, vmul_vf_vf_vf((0.693147180559945286226764).as_vf(), vcast_vf_vi2(e)));
  x = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), SLEEF_INFINITY_F.as_vf(), x);
  x = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vlt_vo_vf_vf(d, (0.).as_vf()), visnan_vo_vf(d)), SLEEF_NAN_F.as_vf(), x);
  x = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (0.).as_vf()), (-SLEEF_INFINITY_F).as_vf(), x);
#else
  x = x.mla(t, vmul_vf_vf_vf((0.693147180559945286226764).as_vf(), e));
  x = vfixup_vf_vf_vf_vi2_i(x, d, vcast_vi2_i((5 << (5*4))), 0);
#endif
  
  return x;
}

pub fn xexpf(d: VFloat) -> VFloat {
  VInt2 q = vrint_vi2_vf(vmul_vf_vf_vf(d, R_LN2_F.as_vf()));
  VFloat s, u;

  s = vcast_vf_vi2(q).mla((-L2U_F).as_vf(), d);
  s = vcast_vf_vi2(q).mla((-L2L_F).as_vf(), s);

  u = (0.000198527617612853646278381).as_vf()
      .mla(s, (0.00139304355252534151077271).as_vf())
      .mla(s, (0.00833336077630519866943359).as_vf())
      .mla(s, (0.0416664853692054748535156).as_vf())
      .mla(s, (0.166666671633720397949219).as_vf())
      .mla(s, (0.5).as_vf());

  u = vadd_vf_vf_vf((1.).as_vf(), vmul_vf_vf_vf(s, s).mla(u, s));

  u = vldexp2_vf_vf_vi2(u, q);

  u = vreinterpret_vf_vm(vandnot_vm_vo32_vm(vlt_vo_vf_vf(d, (-104.).as_vf()), vreinterpret_vm_vf(u)));
  u = vsel_vf_vo_vf_vf(vlt_vo_vf_vf((100.).as_vf(), d), SLEEF_INFINITY_F.as_vf(), u);

  return u;
}
#[inline]
fn expm1fk(d: VFloat) -> VFloat {
  VInt2 q = vrint_vi2_vf(vmul_vf_vf_vf(d, R_LN2_F.as_vf()));
  VFloat s, u;

  s = vcast_vf_vi2(q).mla((-L2U_F).as_vf(), d);
  s = vcast_vf_vi2(q).mla((-L2L_F).as_vf(), s);

  u = (0.000198527617612853646278381).as_vf()
      .mla(s, (0.00139304355252534151077271).as_vf())
      .mla(s, (0.00833336077630519866943359).as_vf())
      .mla(s, (0.0416664853692054748535156).as_vf())
      .mla(s, (0.166666671633720397949219).as_vf())
      .mla(s, (0.5).as_vf());

  u = vmul_vf_vf_vf(s, s).mla(u, s);

  u = vsel_vf_vo_vf_vf(veq_vo_vi2_vi2(q, vcast_vi2_i(0)), u,
		       vsub_vf_vf_vf(vldexp2_vf_vf_vi2(vadd_vf_vf_vf(u, (1.).as_vf()), q), (1.).as_vf()));

  return u;
}

#if defined(ENABLE_NEON32) || defined(ENABLE_NEON32VFPV4)
pub fn xsqrtf_u35(d: VFloat) -> VFloat {
  VFloat e = vreinterpret_vf_vi2(vadd_vi2_vi2_vi2(vcast_vi2_i(0x20000000), vand_vi2_vi2_vi2(vcast_vi2_i(0x7f000000), vsrl_vi2_vi2_i(vreinterpret_vi2_vf(d), 1))));
  VFloat m = vreinterpret_vf_vi2(vadd_vi2_vi2_vi2(vcast_vi2_i(0x3f000000), vand_vi2_vi2_vi2(vcast_vi2_i(0x01ffffff), vreinterpret_vi2_vf(d))));
  float32x4_t x = vrsqrteq_f32(m);
  x = vmulq_f32(x, vrsqrtsq_f32(m, vmulq_f32(x, x)));
  float32x4_t u = vmulq_f32(x, m);
  u = vmlaq_f32(u, vmlsq_f32(m, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
  e = vreinterpret_vf_vm(vandnot_vm_vo32_vm(veq_vo_vf_vf(d, (0.).as_vf()), vreinterpret_vm_vf(e)));
  u = vmul_vf_vf_vf(e, u);

  u = vsel_vf_vo_vf_vf(visinf_vo_vf(d), SLEEF_INFINITY_F.as_vf(), u);
  u = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visnan_vo_vf(d), vlt_vo_vf_vf(d, (0.).as_vf())), vreinterpret_vm_vf(u)));
  u = vmulsign_vf_vf_vf(u, d);

  return u;
}
#elif defined(ENABLE_VECEXT)
pub fn xsqrtf_u35(d: VFloat) -> VFloat {
  VFloat q = vsqrt_vf_vf(d);
  q = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), (-0.).as_vf(), q);
  return vsel_vf_vo_vf_vf(vispinf_vo_vf(d), SLEEF_INFINITY_F.as_vf(), q);
}
#else
pub fn xsqrtf_u35(d: VFloat) -> VFloat { return vsqrt_vf_vf(d); }
#endif

pub fn xcbrtf(d: VFloat) -> VFloat {
  VFloat x, y, q = (1.).as_vf(), t;
  VInt2 e, qu, re;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  VFloat s = d;
#endif
  e = vadd_vi2_vi2_vi2(vilogbk_vi2_vf(vabs_vf_vf(d)), vcast_vi2_i(1));
  d = vldexp2_vf_vf_vi2(d, vneg_vi2_vi2(e));

  t = vadd_vf_vf_vf(vcast_vf_vi2(e), (6144.).as_vf());
  qu = vtruncate_vi2_vf(vmul_vf_vf_vf(t, (1./3.).as_vf()));
  re = vtruncate_vi2_vf(vsub_vf_vf_vf(t, vmul_vf_vf_vf(vcast_vf_vi2(qu), (3.).as_vf())));

  q = vsel_vf_vo_vf_vf(veq_vo_vi2_vi2(re, vcast_vi2_i(1)), (1.2599210498948731647672106).as_vf(), q);
  q = vsel_vf_vo_vf_vf(veq_vo_vi2_vi2(re, vcast_vi2_i(2)), (1.5874010519681994747517056).as_vf(), q);
  q = vldexp2_vf_vf_vi2(q, vsub_vi2_vi2_vi2(qu, vcast_vi2_i(2048)));

  q = vmulsign_vf_vf_vf(q, d);
  d = vabs_vf_vf(d);

  x = (-0.601564466953277587890625).as_vf()
      .mla(d, (2.8208892345428466796875).as_vf())
      .mla(d, (-5.532182216644287109375).as_vf())
      .mla(d, (5.898262500762939453125).as_vf())
      .mla(d, (-3.8095417022705078125).as_vf())
      .mla(d, (2.2241256237030029296875).as_vf());

  y = vmul_vf_vf_vf(vmul_vf_vf_vf(d, x), x);
  y = vmul_vf_vf_vf(vsub_vf_vf_vf(y, vmul_vf_vf_vf(vmul_vf_vf_vf((2. / 3.).as_vf(), y), y.mla(x, (-1.).as_vf()))), q);

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  y = vsel_vf_vo_vf_vf(visinf_vo_vf(s), vmulsign_vf_vf_vf(SLEEF_INFINITY_F.as_vf(), s), y);
  y = vsel_vf_vo_vf_vf(veq_vo_vf_vf(s, (0.).as_vf()), vmulsign_vf_vf_vf((0.).as_vf(), s), y);
#endif
  
  return y;
}

pub fn xcbrtf_u1(d: VFloat) -> VFloat {
  VFloat x, y, z, t;
  VFloat2 q2 = vcast_vf2_f_f(1, 0), u, v;
  VInt2 e, qu, re;

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  VFloat s = d;
#endif
  e = vadd_vi2_vi2_vi2(vilogbk_vi2_vf(vabs_vf_vf(d)), vcast_vi2_i(1));
  d = vldexp2_vf_vf_vi2(d, vneg_vi2_vi2(e));

  t = vadd_vf_vf_vf(vcast_vf_vi2(e), (6144.).as_vf());
  qu = vtruncate_vi2_vf(vmul_vf_vf_vf(t, (1./3.).as_vf()));
  re = vtruncate_vi2_vf(vsub_vf_vf_vf(t, vmul_vf_vf_vf(vcast_vf_vi2(qu), (3.).as_vf())));

  q2 = vsel_vf2_vo_vf2_vf2(veq_vo_vi2_vi2(re, vcast_vi2_i(1)), vcast_vf2_f_f(1.2599210739135742188, -2.4018701694217270415e-08), q2);
  q2 = vsel_vf2_vo_vf2_vf2(veq_vo_vi2_vi2(re, vcast_vi2_i(2)), vcast_vf2_f_f(1.5874010324478149414,  1.9520385308169352356e-08), q2);

  q2.x = vmulsign_vf_vf_vf(q2.x, d); q2.y = vmulsign_vf_vf_vf(q2.y, d);
  d = vabs_vf_vf(d);

  x = (-0.601564466953277587890625).as_vf()
      .mla(d, (2.8208892345428466796875).as_vf())
      .mla(d, (-5.532182216644287109375).as_vf())
      .mla(d, (5.898262500762939453125).as_vf())
      .mla(d, (-3.8095417022705078125).as_vf())
      .mla(d, (2.2241256237030029296875).as_vf());

  y = vmul_vf_vf_vf(x, x); y = vmul_vf_vf_vf(y, y); x = vsub_vf_vf_vf(x, vmul_vf_vf_vf(vmlanp_vf_vf_vf_vf(d, y, x), (-1. / 3.).as_vf()));

  z = x;

  u = dfmul_vf2_vf_vf(x, x);
  u = dfmul_vf2_vf2_vf2(u, u);
  u = dfmul_vf2_vf2_vf(u, d);
  u = dfadd2_vf2_vf2_vf(u, vneg_vf_vf(x));
  y = vadd_vf_vf_vf(u.x, u.y);

  y = vmul_vf_vf_vf(vmul_vf_vf_vf((-2. / 3.).as_vf(), y), z);
  v = dfadd2_vf2_vf2_vf(dfmul_vf2_vf_vf(z, z), y);
  v = dfmul_vf2_vf2_vf(v, d);
  v = dfmul_vf2_vf2_vf2(v, q2);
  z = vldexp2_vf_vf_vi2(vadd_vf_vf_vf(v.x, v.y), vsub_vi2_vi2_vi2(qu, vcast_vi2_i(2048)));

  z = vsel_vf_vo_vf_vf(visinf_vo_vf(d), vmulsign_vf_vf_vf(SLEEF_INFINITY_F.as_vf(), q2.x), z);
  z = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (0.).as_vf()), vreinterpret_vf_vm(vsignbit_vm_vf(q2.x)), z);

#if defined(ENABLE_AVX512F) || defined(ENABLE_AVX512FNOFMA)
  z = vsel_vf_vo_vf_vf(visinf_vo_vf(s), vmulsign_vf_vf_vf(SLEEF_INFINITY_F.as_vf(), s), z);
  z = vsel_vf_vo_vf_vf(veq_vo_vf_vf(s, (0.).as_vf()), vmulsign_vf_vf_vf((0.).as_vf(), s), z);
#endif

  return z;
}
#[inline]
fn logkf(d: VFloat) -> VFloat2 {
  VFloat2 x, x2;
  VFloat t, m;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VOpMask o = vlt_vo_vf_vf(d, f32::MIN.as_vf());
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, (F1_32 * F1_32).as_vf()), d);
  VInt2 e = vilogb2k_vi2_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, vcast_vi2_i(64)), e);
#else
  VFloat e = vgetexp_vf_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), (128.).as_vf(), e);
  m = vgetmant_vf_vf(d);
#endif

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf((-1.).as_vf(), m), dfadd2_vf2_vf_vf((1.).as_vf(), m));
  x2 = dfsqu_vf2_vf2(x);

  t = (0.240320354700088500976562).as_vf()
      .mla(x2.x, (0.285112679004669189453125).as_vf())
      .mla(x2.x, (0.400007992982864379882812).as_vf());
  VFloat2 c = vcast_vf2_f_f(0.66666662693023681640625, 3.69183861259614332084311e-09);

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VFloat2 s = dfmul_vf2_vf2_vf(vcast_vf2_f_f(0.69314718246459960938, -1.904654323148236017e-09), vcast_vf_vi2(e));
#else
  VFloat2 s = dfmul_vf2_vf2_vf(vcast_vf2_f_f(0.69314718246459960938, -1.904654323148236017e-09), e);
#endif

  s = dfadd_vf2_vf2_vf2(s, dfscale_vf2_vf2_vf(x, (2.).as_vf()));
  s = dfadd_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(x2, x),
					     dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf(x2, t), c)));
  return s;
}

pub fn xlogf_u1(d: VFloat) -> VFloat {
  VFloat2 x;
  VFloat t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VOpMask o = vlt_vo_vf_vf(d, f32::MIN.as_vf());
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, (F1_32 * F1_32).as_vf()), d);
  VInt2 e = vilogb2k_vi2_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, vcast_vi2_i(64)), e);
  VFloat2 s = dfmul_vf2_vf2_vf(vcast_vf2_f_f(0.69314718246459960938, -1.904654323148236017e-09), vcast_vf_vi2(e));
#else
  VFloat e = vgetexp_vf_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), (128.).as_vf(), e);
  m = vgetmant_vf_vf(d);
  VFloat2 s = dfmul_vf2_vf2_vf(vcast_vf2_f_f(0.69314718246459960938, -1.904654323148236017e-09), e);
#endif

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf((-1.).as_vf(), m), dfadd2_vf2_vf_vf((1.).as_vf(), m));
  x2 = vmul_vf_vf_vf(x.x, x.x);

  t = (+0.3027294874e+0).as_vf()
      .mla(x2, (+0.3996108174e+0).as_vf())
      .mla(x2, (+0.6666694880e+0).as_vf());
  
  s = dfadd_vf2_vf2_vf2(s, dfscale_vf2_vf2_vf(x, (2.).as_vf()));
  s = dfadd_vf2_vf2_vf(s, vmul_vf_vf_vf(vmul_vf_vf_vf(x2, x.x), t));

  VFloat r = vadd_vf_vf_vf(s.x, s.y);

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), SLEEF_INFINITY_F.as_vf(), r);
  r = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vlt_vo_vf_vf(d, (0.).as_vf()), visnan_vo_vf(d)), SLEEF_NAN_F.as_vf(), r);
  r = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (0.).as_vf()), (-SLEEF_INFINITY_F).as_vf(), r);
#else
  r = vfixup_vf_vf_vf_vi2_i(r, d, vcast_vi2_i((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}
#[inline]
fn expkf(VFloat2 d) -> VFloat {
  VFloat u = vmul_vf_vf_vf(vadd_vf_vf_vf(d.x, d.y), R_LN2_F.as_vf());
  VInt2 q = vrint_vi2_vf(u);
  VFloat2 s, t;

  s = dfadd2_vf2_vf2_vf(d, vmul_vf_vf_vf(vcast_vf_vi2(q), (-L2U_F).as_vf()));
  s = dfadd2_vf2_vf2_vf(s, vmul_vf_vf_vf(vcast_vf_vi2(q), (-L2L_F).as_vf()));

  s = dfnormalize_vf2_vf2(s);

  u = (0.00136324646882712841033936).as_vf()
      .mla(s.x, (0.00836596917361021041870117).as_vf())
      .mla(s.x, (0.0416710823774337768554688).as_vf())
      .mla(s.x, (0.166665524244308471679688).as_vf())
      .mla(s.x, (0.499999850988388061523438).as_vf());

  t = dfadd_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf(dfsqu_vf2_vf2(s), u));

  t = dfadd_vf2_vf_vf2((1.).as_vf(), t);
  u = vadd_vf_vf_vf(t.x, t.y);
  u = vldexp_vf_vf_vi2(u, q);

  u = vreinterpret_vf_vm(vandnot_vm_vo32_vm(vlt_vo_vf_vf(d.x, (-104.).as_vf()), vreinterpret_vm_vf(u)));
  
  return u;
}

pub fn xpowf(x: VFloat, y: VFloat) -> VFloat {
#if 1
  VOpMask yisint = vor_vo_vo_vo(veq_vo_vf_vf(vtruncate_vf_vf(y), y), vgt_vo_vf_vf(vabs_vf_vf(y), F1_24.as_vf()));
  VOpMask yisodd = vand_vo_vo_vo(vand_vo_vo_vo(veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vtruncate_vi2_vf(y), vcast_vi2_i(1)), vcast_vi2_i(1)), yisint),
				 vlt_vo_vf_vf(vabs_vf_vf(y), F1_24.as_vf()));

#if defined(ENABLE_NEON32) || defined(ENABLE_NEON32VFPV4)
  yisodd = vandnot_vm_vo32_vm(visinf_vo_vf(y), yisodd);
#endif

  VFloat result = expkf(dfmul_vf2_vf2_vf(logkf(vabs_vf_vf(x)), y));

  result = vsel_vf_vo_vf_vf(visnan_vo_vf(result), SLEEF_INFINITY_F.as_vf(), result);
  
  result = vmul_vf_vf_vf(result,
			 vsel_vf_vo_vf_vf(vgt_vo_vf_vf(x, (0.).as_vf()),
					  (1.).as_vf(),
					  vsel_vf_vo_vf_vf(yisint, vsel_vf_vo_vf_vf(yisodd, (-1.).as_vf(), (1.).as_vf()), SLEEF_NAN_F.as_vf())));

  VFloat efx = vmulsign_vf_vf_vf(vsub_vf_vf_vf(vabs_vf_vf(x), (1.).as_vf()), y);

  result = vsel_vf_vo_vf_vf(visinf_vo_vf(y),
			    vreinterpret_vf_vm(vandnot_vm_vo32_vm(vlt_vo_vf_vf(efx, (0.).as_vf()),
								  vreinterpret_vm_vf(vsel_vf_vo_vf_vf(veq_vo_vf_vf(efx, (0.).as_vf()),
												      (1.).as_vf(),
												      SLEEF_INFINITY_F.as_vf())))),
			    result);

  result = vsel_vf_vo_vf_vf(vor_vo_vo_vo(visinf_vo_vf(x), veq_vo_vf_vf(x, (0.).as_vf())),
			    vmul_vf_vf_vf(vsel_vf_vo_vf_vf(yisodd, vsign_vf_vf(x), (1.).as_vf()),
					  vreinterpret_vf_vm(vandnot_vm_vo32_vm(vlt_vo_vf_vf(vsel_vf_vo_vf_vf(veq_vo_vf_vf(x, (0.).as_vf()), vneg_vf_vf(y), y), (0.).as_vf()),
										vreinterpret_vm_vf(SLEEF_INFINITY_F.as_vf())))),
			    result);

  result = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visnan_vo_vf(x), visnan_vo_vf(y)), vreinterpret_vm_vf(result)));

  result = vsel_vf_vo_vf_vf(vor_vo_vo_vo(veq_vo_vf_vf(y, (0.).as_vf()), veq_vo_vf_vf(x, (1.).as_vf())), (1.).as_vf(), result);

  return result;
#else
  return expkf(dfmul_vf2_vf2_vf(logkf(x), y));
#endif
}
#[inline]
fn expk2f(VFloat2 d) -> VFloat2 {
  VFloat u = vmul_vf_vf_vf(vadd_vf_vf_vf(d.x, d.y), R_LN2_F.as_vf());
  VInt2 q = vrint_vi2_vf(u);
  VFloat2 s, t;

  s = dfadd2_vf2_vf2_vf(d, vmul_vf_vf_vf(vcast_vf_vi2(q), (-L2U_F).as_vf()));
  s = dfadd2_vf2_vf2_vf(s, vmul_vf_vf_vf(vcast_vf_vi2(q), (-L2L_F).as_vf()));

  u = (+0.1980960224e-3).as_vf()
      .mla(s.x, (+0.1394256484e-2).as_vf())
      .mla(s.x, (+0.8333456703e-2).as_vf())
      .mla(s.x, (+0.4166637361e-1).as_vf());

  t = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(s, u), (+0.166666659414234244790680580464e+0).as_vf());
  t = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf2(s, t), (0.5).as_vf());
  t = dfadd2_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf2(dfsqu_vf2_vf2(s), t));

  t = dfadd_vf2_vf_vf2((1.).as_vf(), t);

  t.x = vldexp2_vf_vf_vi2(t.x, q);
  t.y = vldexp2_vf_vf_vi2(t.y, q);

  t.x = vreinterpret_vf_vm(vandnot_vm_vo32_vm(vlt_vo_vf_vf(d.x, (-104.).as_vf()), vreinterpret_vm_vf(t.x)));
  t.y = vreinterpret_vf_vm(vandnot_vm_vo32_vm(vlt_vo_vf_vf(d.x, (-104.).as_vf()), vreinterpret_vm_vf(t.y)));

  return t;
}

pub fn xsinhf(x: VFloat) -> VFloat {
  VFloat y = vabs_vf_vf(x);
  VFloat2 d = expk2f(vcast_vf2_vf_vf(y, (0.).as_vf()));
  d = dfsub_vf2_vf2_vf2(d, dfrec_vf2_vf2(d));
  y = vmul_vf_vf_vf(vadd_vf_vf_vf(d.x, d.y), (0.5).as_vf());

  y = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vgt_vo_vf_vf(vabs_vf_vf(x), (89.).as_vf()),
				    visnan_vo_vf(y)), SLEEF_INFINITY_F.as_vf(), y);
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xcoshf(x: VFloat) -> VFloat {
  VFloat y = vabs_vf_vf(x);
  VFloat2 d = expk2f(vcast_vf2_vf_vf(y, (0.).as_vf()));
  d = dfadd_vf2_vf2_vf2(d, dfrec_vf2_vf2(d));
  y = vmul_vf_vf_vf(vadd_vf_vf_vf(d.x, d.y), (0.5).as_vf());

  y = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vgt_vo_vf_vf(vabs_vf_vf(x), (89.).as_vf()),
				    visnan_vo_vf(y)), SLEEF_INFINITY_F.as_vf(), y);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xtanhf(x: VFloat) -> VFloat {
  VFloat y = vabs_vf_vf(x);
  VFloat2 d = expk2f(vcast_vf2_vf_vf(y, (0.).as_vf()));
  VFloat2 e = dfrec_vf2_vf2(d);
  d = dfdiv_vf2_vf2_vf2(dfadd_vf2_vf2_vf2(d, dfneg_vf2_vf2(e)), dfadd_vf2_vf2_vf2(d, e));
  y = vadd_vf_vf_vf(d.x, d.y);

  y = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vgt_vo_vf_vf(vabs_vf_vf(x), (8.664339742).as_vf()),
				    visnan_vo_vf(y)), (1.).as_vf(), y);
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xsinhf_u35(x: VFloat) -> VFloat {
  VFloat e = expm1fk(vabs_vf_vf(x));
  VFloat y = vdiv_vf_vf_vf(vadd_vf_vf_vf(e, (2.).as_vf()), vadd_vf_vf_vf(e, (1.).as_vf()));
  y = vmul_vf_vf_vf(y, vmul_vf_vf_vf((0.5).as_vf(), e));

  y = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vgt_vo_vf_vf(vabs_vf_vf(x), (88.).as_vf()),
				    visnan_vo_vf(y)), SLEEF_INFINITY_F.as_vf(), y);
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xcoshf_u35(x: VFloat) -> VFloat {
  VFloat e = xexpf(vabs_vf_vf(x));
  VFloat y = (0.5).as_vf().mla(e, vdiv_vf_vf_vf((0.5).as_vf(), e));

  y = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vgt_vo_vf_vf(vabs_vf_vf(x), (88.).as_vf()),
				    visnan_vo_vf(y)), SLEEF_INFINITY_F.as_vf(), y);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xtanhf_u35(x: VFloat) -> VFloat {
  VFloat d = expm1fk(vmul_vf_vf_vf((2.).as_vf(), vabs_vf_vf(x)));
  VFloat y = vdiv_vf_vf_vf(d, vadd_vf_vf_vf((2.).as_vf(), d));

  y = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vgt_vo_vf_vf(vabs_vf_vf(x), (8.664339742).as_vf()),
				    visnan_vo_vf(y)), (1.).as_vf(), y);
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}
#[inline]
fn logk2f(VFloat2 d) -> VFloat2 {
  VFloat2 x, x2, m, s;
  VFloat t;
  VInt2 e;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  e = vilogbk_vi2_vf(vmul_vf_vf_vf(d.x, (1./0.75).as_vf()));
#else
  e = vrint_vi2_vf(vgetexp_vf_vf(vmul_vf_vf_vf(d.x, (1./0.75).as_vf())));
#endif
  m = dfscale_vf2_vf2_vf(d, vpow2i_vf_vi2(vneg_vi2_vi2(e)));

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf2_vf(m, (-1.).as_vf()), dfadd2_vf2_vf2_vf(m, (1.).as_vf()));
  x2 = dfsqu_vf2_vf2(x);

  t = (0.2392828464508056640625).as_vf()
      .mla(x2.x, (0.28518211841583251953125).as_vf())
      .mla(x2.x, (0.400005877017974853515625).as_vf())
      .mla(x2.x, (0.666666686534881591796875).as_vf());

  s = dfmul_vf2_vf2_vf(vcast_vf2_vf_vf((0.69314718246459960938).as_vf(), (-1.904654323148236017e-09).as_vf()), vcast_vf_vi2(e));
  s = dfadd_vf2_vf2_vf2(s, dfscale_vf2_vf2_vf(x, (2.).as_vf()));
  s = dfadd_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf(dfmul_vf2_vf2_vf2(x2, x), t));

  return s;
}

pub fn xasinhf(x: VFloat) -> VFloat {
  VFloat y = vabs_vf_vf(x);
  VOpMask o = vgt_vo_vf_vf(y, (1.).as_vf());
  VFloat2 d;
  
  d = vsel_vf2_vo_vf2_vf2(o, dfrec_vf2_vf(x), vcast_vf2_vf_vf(y, (0.).as_vf()));
  d = dfsqrt_vf2_vf2(dfadd2_vf2_vf2_vf(dfsqu_vf2_vf2(d), (1.).as_vf()));
  d = vsel_vf2_vo_vf2_vf2(o, dfmul_vf2_vf2_vf(d, y), d);

  d = logk2f(dfnormalize_vf2_vf2(dfadd2_vf2_vf2_vf(d, x)));
  y = vadd_vf_vf_vf(d.x, d.y);

  y = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vgt_vo_vf_vf(vabs_vf_vf(x), SQRT_FLT_MAX.as_vf()),
				    visnan_vo_vf(y)),
		       vmulsign_vf_vf_vf(SLEEF_INFINITY_F.as_vf(), x), y);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));
  y = vsel_vf_vo_vf_vf(visnegzero_vo_vf(x), (-0.).as_vf(), y);

  return y;
}

pub fn xacoshf(x: VFloat) -> VFloat {
  VFloat2 d = logk2f(dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfsqrt_vf2_vf2(dfadd2_vf2_vf_vf(x, (1.).as_vf())), dfsqrt_vf2_vf2(dfadd2_vf2_vf_vf(x, (-1.).as_vf()))), x));
  VFloat y = vadd_vf_vf_vf(d.x, d.y);

  y = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vgt_vo_vf_vf(vabs_vf_vf(x), SQRT_FLT_MAX.as_vf()),
				    visnan_vo_vf(y)),
		       SLEEF_INFINITY_F.as_vf(), y);

  y = vreinterpret_vf_vm(vandnot_vm_vo32_vm(veq_vo_vf_vf(x, (1.).as_vf()), vreinterpret_vm_vf(y)));

  y = vreinterpret_vf_vm(vor_vm_vo32_vm(vlt_vo_vf_vf(x, (1.).as_vf()), vreinterpret_vm_vf(y)));
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xatanhf(x: VFloat) -> VFloat {
  VFloat y = vabs_vf_vf(x);
  VFloat2 d = logk2f(dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf((1.).as_vf(), y), dfadd2_vf2_vf_vf((1.).as_vf(), vneg_vf_vf(y))));
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(vgt_vo_vf_vf(y, (1.).as_vf()), vreinterpret_vm_vf(vsel_vf_vo_vf_vf(veq_vo_vf_vf(y, (1.).as_vf()), SLEEF_INFINITY_F.as_vf(), vmul_vf_vf_vf(vadd_vf_vf_vf(d.x, d.y), (0.5).as_vf())))));

  y = vreinterpret_vf_vm(vor_vm_vo32_vm(vor_vo_vo_vo(visinf_vo_vf(x), visnan_vo_vf(y)), vreinterpret_vm_vf(y)));
  y = vmulsign_vf_vf_vf(y, x);
  y = vreinterpret_vf_vm(vor_vm_vo32_vm(visnan_vo_vf(x), vreinterpret_vm_vf(y)));

  return y;
}

pub fn xexp2f(d: VFloat) -> VFloat {
  VFloat u = vrint_vf_vf(d), s;
  VInt2 q = vrint_vi2_vf(u);

  s = vsub_vf_vf_vf(d, u);

  u = (+0.1535920892e-3).as_vf()
      .mla(s, (+0.1339262701e-2).as_vf())
      .mla(s, (+0.9618384764e-2).as_vf())
      .mla(s, (+0.5550347269e-1).as_vf())
      .mla(s, (+0.2402264476e+0).as_vf())
      .mla(s, (+0.6931471825e+0).as_vf());

#ifdef ENABLE_FMA_SP
  u = vfma_vf_vf_vf_vf(u, s, (1.).as_vf());
#else
  u = dfnormalize_vf2_vf2(dfadd_vf2_vf_vf2((1.).as_vf(), dfmul_vf2_vf_vf(u, s))).x;
#endif
  
  u = vldexp2_vf_vf_vi2(u, q);

  u = vsel_vf_vo_vf_vf(vge_vo_vf_vf(d, (128.).as_vf()), SLEEF_INFINITY.as_vf(), u);
  u = vreinterpret_vf_vm(vandnot_vm_vo32_vm(vlt_vo_vf_vf(d, (-150.).as_vf()), vreinterpret_vm_vf(u)));

  return u;
}

pub fn xexp10f(d: VFloat) -> VFloat {
  VFloat u = vrint_vf_vf(vmul_vf_vf_vf(d, LOG10_2.as_vf())), s;
  VInt2 q = vrint_vi2_vf(u);

  s = u.mla((-L10U_F).as_vf(), d);
  s = u.mla((-L10L_F).as_vf(), s);

  u = (+0.2064004987e+0).as_vf()
      .mla(s, (+0.5417877436e+0).as_vf())
      .mla(s, (+0.1171286821e+1).as_vf())
      .mla(s, (+0.2034656048e+1).as_vf())
      .mla(s, (+0.2650948763e+1).as_vf())
      .mla(s, (+0.2302585125e+1).as_vf());

#ifdef ENABLE_FMA_SP
  u = vfma_vf_vf_vf_vf(u, s, (1.).as_vf());
#else
  u = dfnormalize_vf2_vf2(dfadd_vf2_vf_vf2((1.).as_vf(), dfmul_vf2_vf_vf(u, s))).x;
#endif
  
  u = vldexp2_vf_vf_vi2(u, q);

  u = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(d, (38.5318394191036238941387).as_vf()), SLEEF_INFINITY_F.as_vf(), u);
  u = vreinterpret_vf_vm(vandnot_vm_vo32_vm(vlt_vo_vf_vf(d, (-50.).as_vf()), vreinterpret_vm_vf(u)));

  return u;
}

pub fn xexpm1f(a: VFloat) -> VFloat {
  VFloat2 d = dfadd2_vf2_vf2_vf(expk2f(vcast_vf2_vf_vf(a, (0.).as_vf())), (-1.).as_vf());
  VFloat x = vadd_vf_vf_vf(d.x, d.y);
  x = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(a, (88.72283172607421875).as_vf()), SLEEF_INFINITY_F.as_vf(), x);
  x = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(a, (-16.635532333438687426013570).as_vf()), (-1.).as_vf(), x);
  x = vsel_vf_vo_vf_vf(visnegzero_vo_vf(a), (-0.).as_vf(), x);
  return x;
}

pub fn xlog10f(d: VFloat) -> VFloat {
  VFloat2 x;
  VFloat t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VOpMask o = vlt_vo_vf_vf(d, f32::MIN.as_vf());
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, (F1_32 * F1_32).as_vf()), d);
  VInt2 e = vilogb2k_vi2_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, vcast_vi2_i(64)), e);
#else
  VFloat e = vgetexp_vf_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), (128.).as_vf(), e);
  m = vgetmant_vf_vf(d);
#endif

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf((-1.).as_vf(), m), dfadd2_vf2_vf_vf((1.).as_vf(), m));
  x2 = vmul_vf_vf_vf(x.x, x.x);

  t = (0.1314289868e+0).as_vf()
      .mla(x2, ( 0.1735493541e+0).as_vf())
      .mla(x2, ( 0.2895309627e+0).as_vf());
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VFloat2 s = dfmul_vf2_vf2_vf(vcast_vf2_f_f(0.30103001, -1.432098889e-08), vcast_vf_vi2(e));
#else
  VFloat2 s = dfmul_vf2_vf2_vf(vcast_vf2_f_f(0.30103001, -1.432098889e-08), e);
#endif

  s = dfadd_vf2_vf2_vf2(s, dfmul_vf2_vf2_vf2(x, vcast_vf2_f_f(0.868588984, -2.170757285e-08)));
  s = dfadd_vf2_vf2_vf(s, vmul_vf_vf_vf(vmul_vf_vf_vf(x2, x.x), t));

  VFloat r = vadd_vf_vf_vf(s.x, s.y);

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), SLEEF_INFINITY.as_vf(), r);
  r = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vlt_vo_vf_vf(d, (0.).as_vf()), visnan_vo_vf(d)), SLEEF_NAN.as_vf(), r);
  r = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (0.).as_vf()), (-SLEEF_INFINITY).as_vf(), r);
#else
  r = vfixup_vf_vf_vf_vi2_i(r, d, vcast_vi2_i((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}

pub fn xlog2f(d: VFloat) -> VFloat {
  VFloat2 x;
  VFloat t, m, x2;

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VOpMask o = vlt_vo_vf_vf(d, f32::MIN.as_vf());
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, (F1_32 * F1_32).as_vf()), d);
  VInt2 e = vilogb2k_vi2_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  m = vldexp3_vf_vf_vi2(d, vneg_vi2_vi2(e));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, vcast_vi2_i(64)), e);
#else
  VFloat e = vgetexp_vf_vf(vmul_vf_vf_vf(d, (1./0.75).as_vf()));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), (128.).as_vf(), e);
  m = vgetmant_vf_vf(d);
#endif

  x = dfdiv_vf2_vf2_vf2(dfadd2_vf2_vf_vf((-1.).as_vf(), m), dfadd2_vf2_vf_vf((1.).as_vf(), m));
  x2 = vmul_vf_vf_vf(x.x, x.x);

  t = (+0.4374550283e+0).as_vf()
      .mla(x2, (+0.5764790177e+0).as_vf())
      .mla(x2, (+0.9618012905120).as_vf());
  
#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VFloat2 s = dfadd2_vf2_vf_vf2(vcast_vf_vi2(e),
				dfmul_vf2_vf2_vf2(x, vcast_vf2_f_f(2.8853900432586669922, 3.2734474483568488616e-08)));
#else
  VFloat2 s = dfadd2_vf2_vf_vf2(e,
				dfmul_vf2_vf2_vf2(x, vcast_vf2_f_f(2.8853900432586669922, 3.2734474483568488616e-08)));
#endif

  s = dfadd2_vf2_vf2_vf(s, vmul_vf_vf_vf(vmul_vf_vf_vf(x2, x.x), t));

  VFloat r = vadd_vf_vf_vf(s.x, s.y);

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  r = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), SLEEF_INFINITY.as_vf(), r);
  r = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vlt_vo_vf_vf(d, (0.).as_vf()), visnan_vo_vf(d)), SLEEF_NAN.as_vf(), r);
  r = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (0.).as_vf()), (-SLEEF_INFINITY).as_vf(), r);
#else
  r = vfixup_vf_vf_vf_vi2_i(r, d, vcast_vi2_i((4 << (2*4)) | (3 << (4*4)) | (5 << (5*4)) | (2 << (6*4))), 0);
#endif
  
  return r;
}

pub fn xlog1pf(d: VFloat) -> VFloat {
  VFloat2 x;
  VFloat t, m, x2;

  VFloat dp1 = vadd_vf_vf_vf(d, (1.).as_vf());

#if !defined(ENABLE_AVX512F) && !defined(ENABLE_AVX512FNOFMA)
  VOpMask o = vlt_vo_vf_vf(dp1, f32::MIN.as_vf());
  dp1 = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(dp1, (F1_32 * F1_32).as_vf()), dp1);
  VInt2 e = vilogb2k_vi2_vf(vmul_vf_vf_vf(dp1, (1./0.75).as_vf()));
  t = vldexp3_vf_vf_vi2((1.).as_vf(), vneg_vi2_vi2(e));
  m = d.mla(t, vsub_vf_vf_vf(t, (1.).as_vf()));
  e = vsel_vi2_vo_vi2_vi2(o, vsub_vi2_vi2_vi2(e, vcast_vi2_i(64)), e);
  VFloat2 s = dfmul_vf2_vf2_vf(vcast_vf2_f_f(0.69314718246459960938, -1.904654323148236017e-09), vcast_vf_vi2(e));
#else
  VFloat e = vgetexp_vf_vf(vmul_vf_vf_vf(dp1, (1./0.75).as_vf()));
  e = vsel_vf_vo_vf_vf(vispinf_vo_vf(e), (128.).as_vf(), e);
  t = vldexp3_vf_vf_vi2((1.).as_vf(), vneg_vi2_vi2(vrint_vi2_vf(e)));
  m = d.mla(t, vsub_vf_vf_vf(t, (1.).as_vf()));
  VFloat2 s = dfmul_vf2_vf2_vf(vcast_vf2_f_f(0.69314718246459960938, -1.904654323148236017e-09), e);
#endif

  x = dfdiv_vf2_vf2_vf2(vcast_vf2_vf_vf(m, (0.).as_vf()), dfadd_vf2_vf_vf((2.).as_vf(), m));
  x2 = vmul_vf_vf_vf(x.x, x.x);

  t = (+0.3027294874e+0).as_vf()
      .mla(x2, (+0.3996108174e+0).as_vf())
      .mla(x2, (+0.6666694880e+0).as_vf());
  
  s = dfadd_vf2_vf2_vf2(s, dfscale_vf2_vf2_vf(x, (2.).as_vf()));
  s = dfadd_vf2_vf2_vf(s, vmul_vf_vf_vf(vmul_vf_vf_vf(x2, x.x), t));

  VFloat r = vadd_vf_vf_vf(s.x, s.y);
  
  r = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(d, (1e+38).as_vf()), SLEEF_INFINITY_F.as_vf(), r);
  r = vreinterpret_vf_vm(vor_vm_vo32_vm(vgt_vo_vf_vf((-1.).as_vf(), d), vreinterpret_vm_vf(r)));
  r = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (-1.).as_vf()), (-SLEEF_INFINITY_F).as_vf(), r);
  r = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), (-0.).as_vf(), r);

  return r;
}

//

pub fn xfabsf(x: VFloat) -> VFloat { return vabs_vf_vf(x); }

pub fn xcopysignf(x: VFloat, y: VFloat) -> VFloat { return vcopysign_vf_vf_vf(x, y); }

pub fn xfmaxf(x: VFloat, y: VFloat) -> VFloat {
#if (defined(__x86_64__) || defined(__i386__)) && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
  return vsel_vf_vo_vf_vf(visnan_vo_vf(y), x, vmax_vf_vf_vf(x, y));
#else
  return vsel_vf_vo_vf_vf(visnan_vo_vf(y), x, vsel_vf_vo_vf_vf(vgt_vo_vf_vf(x, y), x, y));
#endif
}

pub fn xfminf(x: VFloat, y: VFloat) -> VFloat {
#if (defined(__x86_64__) || defined(__i386__)) && !defined(ENABLE_VECEXT) && !defined(ENABLE_PUREC)
  return vsel_vf_vo_vf_vf(visnan_vo_vf(y), x, vmin_vf_vf_vf(x, y));
#else
  return vsel_vf_vo_vf_vf(visnan_vo_vf(y), x, vsel_vf_vo_vf_vf(vgt_vo_vf_vf(y, x), x, y));
#endif
}

pub fn xfdimf(x: VFloat, y: VFloat) -> VFloat {
  VFloat ret = vsub_vf_vf_vf(x, y);
  ret = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vlt_vo_vf_vf(ret, (0.).as_vf()), veq_vo_vf_vf(x, y)), (0.).as_vf(), ret);
  return ret;
}

pub fn xtruncf(x: VFloat) -> VFloat {
  VFloat fr = vsub_vf_vf_vf(x, vcast_vf_vi2(vtruncate_vi2_vf(x)));
  return vsel_vf_vo_vf_vf(vor_vo_vo_vo(visinf_vo_vf(x), vge_vo_vf_vf(vabs_vf_vf(x), F1_23.as_vf())), x, vcopysign_vf_vf_vf(vsub_vf_vf_vf(x, fr), x));
}

pub fn xfloorf(x: VFloat) -> VFloat {
  VFloat fr = vsub_vf_vf_vf(x, vcast_vf_vi2(vtruncate_vi2_vf(x)));
  fr = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(fr, (0.).as_vf()), vadd_vf_vf_vf(fr, (1.).as_vf()), fr);
  return vsel_vf_vo_vf_vf(vor_vo_vo_vo(visinf_vo_vf(x), vge_vo_vf_vf(vabs_vf_vf(x), F1_23.as_vf())), x, vcopysign_vf_vf_vf(vsub_vf_vf_vf(x, fr), x));
}

pub fn xceilf(x: VFloat) -> VFloat {
  VFloat fr = vsub_vf_vf_vf(x, vcast_vf_vi2(vtruncate_vi2_vf(x)));
  fr = vsel_vf_vo_vf_vf(vle_vo_vf_vf(fr, (0.).as_vf()), fr, vsub_vf_vf_vf(fr, (1.).as_vf()));
  return vsel_vf_vo_vf_vf(vor_vo_vo_vo(visinf_vo_vf(x), vge_vo_vf_vf(vabs_vf_vf(x), F1_23.as_vf())), x, vcopysign_vf_vf_vf(vsub_vf_vf_vf(x, fr), x));
}

pub fn xroundf(d: VFloat) -> VFloat {
  VFloat x = vadd_vf_vf_vf(d, (0.5).as_vf());
  VFloat fr = vsub_vf_vf_vf(x, vcast_vf_vi2(vtruncate_vi2_vf(x)));
  x = vsel_vf_vo_vf_vf(vand_vo_vo_vo(vle_vo_vf_vf(x, (0.).as_vf()), veq_vo_vf_vf(fr, (0.).as_vf())), vsub_vf_vf_vf(x, (1.).as_vf()), x);
  fr = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(fr, (0.).as_vf()), vadd_vf_vf_vf(fr, (1.).as_vf()), fr);
  x = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (0.4999999701976776123).as_vf()), (0.).as_vf(), x);
  return vsel_vf_vo_vf_vf(vor_vo_vo_vo(visinf_vo_vf(d), vge_vo_vf_vf(vabs_vf_vf(d), F1_23.as_vf())), d, vcopysign_vf_vf_vf(vsub_vf_vf_vf(x, fr), d));
}

pub fn xrintf(d: VFloat) -> VFloat {
  VFloat x = vadd_vf_vf_vf(d, (0.5).as_vf());
  VOpMask isodd = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vcast_vi2_i(1), vtruncate_vi2_vf(x)), vcast_vi2_i(1));
  VFloat fr = vsub_vf_vf_vf(x, vcast_vf_vi2(vtruncate_vi2_vf(x)));
  fr = vsel_vf_vo_vf_vf(vor_vo_vo_vo(vlt_vo_vf_vf(fr, (0.).as_vf()), vand_vo_vo_vo(veq_vo_vf_vf(fr, (0.).as_vf()), isodd)), vadd_vf_vf_vf(fr, (1.).as_vf()), fr);
  x = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (0.50000005960464477539).as_vf()), (0.).as_vf(), x);
  VFloat ret = vsel_vf_vo_vf_vf(vor_vo_vo_vo(visinf_vo_vf(d), vge_vo_vf_vf(vabs_vf_vf(d), F1_23.as_vf())), d, vcopysign_vf_vf_vf(vsub_vf_vf_vf(x, fr), d));
  return ret;
}

pub fn xfmaf(VFloat x, VFloat y, VFloat z) -> VFloat {
  VFloat h2 = vadd_vf_vf_vf(vmul_vf_vf_vf(x, y), z), q = (1.).as_vf();
  VOpMask o = vlt_vo_vf_vf(vabs_vf_vf(h2), (1e-38).as_vf());
  {
    const float c0 = 1ULL << 25, c1 = c0 * c0, c2 = c1 * c1;
    x = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(x, c1.as_vf()), x);
    y = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(y, c1.as_vf()), y);
    z = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(z, c2.as_vf()), z);
    q = vsel_vf_vo_vf_vf(o, (1. / c2).as_vf(), q);
  }
  o = vgt_vo_vf_vf(vabs_vf_vf(h2), (1e+38).as_vf());
  {
    const float c0 = 1ULL << 25, c1 = c0 * c0, c2 = c1 * c1;
    x = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(x, (1. / c1).as_vf()), x);
    y = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(y, (1. / c1).as_vf()), y);
    z = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(z, (1. / c2).as_vf()), z);
    q = vsel_vf_vo_vf_vf(o, c2.as_vf(), q);
  }
  VFloat2 d = dfmul_vf2_vf_vf(x, y);
  d = dfadd2_vf2_vf2_vf(d, z);
  VFloat ret = vsel_vf_vo_vf_vf(vor_vo_vo_vo(veq_vo_vf_vf(x, (0.).as_vf()), veq_vo_vf_vf(y, (0.).as_vf())), z, vadd_vf_vf_vf(d.x, d.y));
  o = visinf_vo_vf(z);
  o = vandnot_vo_vo_vo(visinf_vo_vf(x), o);
  o = vandnot_vo_vo_vo(visnan_vo_vf(x), o);
  o = vandnot_vo_vo_vo(visinf_vo_vf(y), o);
  o = vandnot_vo_vo_vo(visnan_vo_vf(y), o);
  h2 = vsel_vf_vo_vf_vf(o, z, h2);

  o = vor_vo_vo_vo(visinf_vo_vf(h2), visnan_vo_vf(h2));
  
  return vsel_vf_vo_vf_vf(o, h2, vmul_vf_vf_vf(ret, q));
}
#[inline]
fn vcast_vi2_i_i(int i0, int i1) -> VInt2 { return vcast_vi2_vm(vcast_vm_i_i(i0, i1)); }

SQRTFU05_FUNCATR VFloat xsqrtf_u05(d: VFloat) {
  VFloat q;
  VOpMask o;
  
  d = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(d, (0.).as_vf()), SLEEF_NAN_F.as_vf(), d);

  o = vlt_vo_vf_vf(d, (5.2939559203393770e-23).as_vf());
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, (1.8889465931478580e+22).as_vf()), d);
  q = vsel_vf_vo_vf_vf(o, (7.2759576141834260e-12*0.5).as_vf(), (0.5).as_vf());

  o = vgt_vo_vf_vf(d, (1.8446744073709552e+19).as_vf());
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, (5.4210108624275220e-20).as_vf()), d);
  q = vsel_vf_vo_vf_vf(o, (4294967296.0 * 0.5).as_vf(), q);

  VFloat x = vreinterpret_vf_vi2(vsub_vi2_vi2_vi2(vcast_vi2_i(0x5f375a86), vsrl_vi2_vi2_i(vreinterpret_vi2_vf(vadd_vf_vf_vf(d, (1e-45).as_vf())), 1)));

  x = vmul_vf_vf_vf(x, vsub_vf_vf_vf((1.5).as_vf(), vmul_vf_vf_vf(vmul_vf_vf_vf(vmul_vf_vf_vf((0.5).as_vf(), d), x), x)));
  x = vmul_vf_vf_vf(x, vsub_vf_vf_vf((1.5).as_vf(), vmul_vf_vf_vf(vmul_vf_vf_vf(vmul_vf_vf_vf((0.5).as_vf(), d), x), x)));
  x = vmul_vf_vf_vf(x, vsub_vf_vf_vf((1.5).as_vf(), vmul_vf_vf_vf(vmul_vf_vf_vf(vmul_vf_vf_vf((0.5).as_vf(), d), x), x)));
  x = vmul_vf_vf_vf(x, d);

  VFloat2 d2 = dfmul_vf2_vf2_vf2(dfadd2_vf2_vf_vf2(d, dfmul_vf2_vf_vf(x, x)), dfrec_vf2_vf(x));

  x = vmul_vf_vf_vf(vadd_vf_vf_vf(d2.x, d2.y), q);

  x = vsel_vf_vo_vf_vf(vispinf_vo_vf(d), SLEEF_INFINITY_F.as_vf(), x);
  x = vsel_vf_vo_vf_vf(veq_vo_vf_vf(d, (0.).as_vf()), d, x);
  
  return x;
}

pub fn xsqrtf(d: VFloat) -> VFloat {
#ifdef ACCURATE_SQRT
  return vsqrt_vf_vf(d);
#endif
  // fall back to approximation if ACCURATE_SQRT is undefined
  return xsqrtf_u05(d);
}

pub fn xhypotf_u05(x: VFloat, y: VFloat) -> VFloat {
  x = vabs_vf_vf(x);
  y = vabs_vf_vf(y);
  VFloat min = vmin_vf_vf_vf(x, y), n = min;
  VFloat max = vmax_vf_vf_vf(x, y), d = max;

  VOpMask o = vlt_vo_vf_vf(max, f32::MIN.as_vf());
  n = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(n, F1_24.as_vf()), n);
  d = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(d, F1_24.as_vf()), d);

  VFloat2 t = dfdiv_vf2_vf2_vf2(vcast_vf2_vf_vf(n, (0.).as_vf()), vcast_vf2_vf_vf(d, (0.).as_vf()));
  t = dfmul_vf2_vf2_vf(dfsqrt_vf2_vf2(dfadd2_vf2_vf2_vf(dfsqu_vf2_vf2(t), (1.).as_vf())), max);
  VFloat ret = vadd_vf_vf_vf(t.x, t.y);
  ret = vsel_vf_vo_vf_vf(visnan_vo_vf(ret), SLEEF_INFINITY_F.as_vf(), ret);
  ret = vsel_vf_vo_vf_vf(veq_vo_vf_vf(min, (0.).as_vf()), max, ret);
  ret = vsel_vf_vo_vf_vf(vor_vo_vo_vo(visnan_vo_vf(x), visnan_vo_vf(y)), SLEEF_NAN_F.as_vf(), ret);
  ret = vsel_vf_vo_vf_vf(vor_vo_vo_vo(veq_vo_vf_vf(x, SLEEF_INFINITY_F.as_vf()), veq_vo_vf_vf(y, SLEEF_INFINITY_F.as_vf())), SLEEF_INFINITY_F.as_vf(), ret);

  return ret;
}

pub fn xhypotf_u35(x: VFloat, y: VFloat) -> VFloat {
  x = vabs_vf_vf(x);
  y = vabs_vf_vf(y);
  VFloat min = vmin_vf_vf_vf(x, y), n = min;
  VFloat max = vmax_vf_vf_vf(x, y), d = max;

  VFloat t = vdiv_vf_vf_vf(min, max);
  VFloat ret = vmul_vf_vf_vf(max, vsqrt_vf_vf(t.mla(t, (1.).as_vf())));
  ret = vsel_vf_vo_vf_vf(veq_vo_vf_vf(min, (0.).as_vf()), max, ret);
  ret = vsel_vf_vo_vf_vf(vor_vo_vo_vo(visnan_vo_vf(x), visnan_vo_vf(y)), SLEEF_NAN_F.as_vf(), ret);
  ret = vsel_vf_vo_vf_vf(vor_vo_vo_vo(veq_vo_vf_vf(x, SLEEF_INFINITY_F.as_vf()), veq_vo_vf_vf(y, SLEEF_INFINITY_F.as_vf())), SLEEF_INFINITY_F.as_vf(), ret);

  return ret;
}

pub fn xnextafterf(x: VFloat, y: VFloat) -> VFloat {
  x = vsel_vf_vo_vf_vf(veq_vo_vf_vf(x, (0.).as_vf()), vmulsign_vf_vf_vf((0.).as_vf(), y), x);
  VInt2 t, xi2 = vreinterpret_vi2_vf(x);
  VOpMask c = vxor_vo_vo_vo(vsignbit_vo_vf(x), vge_vo_vf_vf(y, x));

  xi2 = vsel_vi2_vo_vi2_vi2(c, vsub_vi2_vi2_vi2(vcast_vi2_i(0), vxor_vi2_vi2_vi2(xi2, vcast_vi2_i(1 << 31))), xi2);

  xi2 = vsel_vi2_vo_vi2_vi2(vneq_vo_vf_vf(x, y), vsub_vi2_vi2_vi2(xi2, vcast_vi2_i(1)), xi2);

  xi2 = vsel_vi2_vo_vi2_vi2(c, vsub_vi2_vi2_vi2(vcast_vi2_i(0), vxor_vi2_vi2_vi2(xi2, vcast_vi2_i(1 << 31))), xi2);

  VFloat ret = vreinterpret_vf_vi2(xi2);

  ret = vsel_vf_vo_vf_vf(vand_vo_vo_vo(veq_vo_vf_vf(ret, (0.).as_vf()), vneq_vo_vf_vf(x, (0.).as_vf())), 
			 vmulsign_vf_vf_vf((0.).as_vf(), x), ret);

  ret = vsel_vf_vo_vf_vf(vand_vo_vo_vo(veq_vo_vf_vf(x, (0.).as_vf()), veq_vo_vf_vf(y, (0.).as_vf())), y, ret);

  ret = vsel_vf_vo_vf_vf(vor_vo_vo_vo(visnan_vo_vf(x), visnan_vo_vf(y)), SLEEF_NAN_F.as_vf(), ret);
  
  return ret;
}

pub fn xfrfrexpf(x: VFloat) -> VFloat {
  x = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(vabs_vf_vf(x), f32::MIN.as_vf()), vmul_vf_vf_vf(x, F1_32.as_vf()), x);

  vmask xm = vreinterpret_vm_vf(x);
  xm = vand_vm_vm_vm(xm, vcast_vm_i_i(~0x7f800000U, ~0x7f800000U));
  xm = vor_vm_vm_vm (xm, vcast_vm_i_i( 0x3f000000U,  0x3f000000U));

  VFloat ret = vreinterpret_vf_vm(xm);

  ret = vsel_vf_vo_vf_vf(visinf_vo_vf(x), vmulsign_vf_vf_vf(SLEEF_INFINITY_F.as_vf(), x), ret);
  ret = vsel_vf_vo_vf_vf(veq_vo_vf_vf(x, (0.).as_vf()), x, ret);
  
  return ret;
}

pub fn xexpfrexpf(x: VFloat) -> VInt2 {
  /*
  x = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(vabs_vf_vf(x), f32::MIN.as_vf()), vmul_vf_vf_vf(x, F1_63.as_vf()), x);

  let ret = vreinterpret_vi2_vf(x).as_vi();
  ret = vand_vi_vi_vi(vsrl_vi_vi_i(ret, 20), (0x7ff).as_vi()) - (0x3fe).as_vi();

  ret = vsel_vi_vo_vi_vi(vor_vo_vo_vo(vor_vo_vo_vo(veq_vo_vf_vf(x, (0.).as_vf()), visnan_vo_vf(x)), visinf_vo_vf(x)), (0).as_vi(), ret);
  
  return ret;
  */
  return vcast_vi2_i(0);
}
#[inline]
fn vtoward0f(x: VFloat) -> VFloat {
  VFloat t = vreinterpret_vf_vi2(vsub_vi2_vi2_vi2(vreinterpret_vi2_vf(x), vcast_vi2_i(1)));
  return vsel_vf_vo_vf_vf(veq_vo_vf_vf(x, (0.).as_vf()), (0.).as_vf(), t);
}
#[inline]
fn vptruncf(x: VFloat) -> VFloat {
#ifdef FULL_FP_ROUNDING
  return vtruncate_vf_vf(x);
#else
  VFloat fr = vsub_vf_vf_vf(x, vcast_vf_vi2(vtruncate_vi2_vf(x)));
  return vsel_vf_vo_vf_vf(vge_vo_vf_vf(vabs_vf_vf(x), F1_23.as_vf()), x, vsub_vf_vf_vf(x, fr));
#endif
}

pub fn xfmodf(x: VFloat, y: VFloat) -> VFloat {
  VFloat nu = vabs_vf_vf(x), de = vabs_vf_vf(y), s = (1.).as_vf(), q;
  VOpMask o = vlt_vo_vf_vf(de, f32::MIN.as_vf());
  nu = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(nu, F1_25.as_vf()), nu);
  de = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(de, F1_25.as_vf()), de);
  s  = vsel_vf_vo_vf_vf(o, vmul_vf_vf_vf(s , (1. / F1_25).as_vf()), s);
  VFloat rde = vtoward0f(vrec_vf_vf(de));
#if defined(ENABLE_NEON32) || defined(ENABLE_NEON32VFPV4)
  rde = vtoward0f(rde);
#endif
  VFloat2 r = vcast_vf2_vf_vf(nu, (0.).as_vf());

  for(int i=0;i<8;i++) { // ceil(log2(FLT_MAX) / 22)+1
    q = vsel_vf_vo_vf_vf(vand_vo_vo_vo(vgt_vo_vf_vf(vadd_vf_vf_vf(de, de), r.x),
				       vge_vo_vf_vf(r.x, de)),
			 (1.).as_vf(), vmul_vf_vf_vf(vtoward0f(r.x), rde));
    r = dfnormalize_vf2_vf2(dfadd2_vf2_vf2_vf2(r, dfmul_vf2_vf_vf(vptruncf(q), vneg_vf_vf(de))));
    if (vtestallones_i_vo32(vlt_vo_vf_vf(r.x, de))) break;
  }
  
  VFloat ret = vmul_vf_vf_vf(vadd_vf_vf_vf(r.x, r.y), s);
  ret = vsel_vf_vo_vf_vf(veq_vo_vf_vf(vadd_vf_vf_vf(r.x, r.y), de), (0.).as_vf(), ret);

  ret = vmulsign_vf_vf_vf(ret, x);

  ret = vsel_vf_vo_vf_vf(vlt_vo_vf_vf(nu, de), x, ret);
  ret = vsel_vf_vo_vf_vf(veq_vo_vf_vf(de, (0.).as_vf()), SLEEF_NAN_F.as_vf(), ret);

  return ret;
}

//
#[inline]
fn sinpifk(d: VFloat) -> VFloat2 {
  VOpMask o;
  VFloat u, s, t;
  VFloat2 x, s2;

  u = vmul_vf_vf_vf(d, (4.).as_vf());
  VInt2 q = vtruncate_vi2_vf(u);
  q = vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vxor_vi2_vi2_vi2(vsrl_vi2_vi2_i(q, 31), vcast_vi2_i(1))), vcast_vi2_i(~1));
  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(2));

  s = vsub_vf_vf_vf(u, vcast_vf_vi2(q));
  t = s;
  s = vmul_vf_vf_vf(s, s);
  s2 = dfmul_vf2_vf_vf(t, t);

  //

  u = vsel_vf_vo_f_f(o, -0.2430611801e-7, 0.3093842054e-6)
      .mla(s, vsel_vf_vo_f_f(o, 0.3590577080e-5, -0.3657307388e-4))
      .mla(s, vsel_vf_vo_f_f(o, -0.3259917721e-3, 0.2490393585e-2));
  x = dfadd2_vf2_vf_vf2(vmul_vf_vf_vf(u, s),
			vsel_vf2_vo_f_f_f_f(o, 0.015854343771934509277, 4.4940051354032242811e-10,
					    -0.080745510756969451904, -1.3373665339076936258e-09));
  x = dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(s2, x),
			 vsel_vf2_vo_f_f_f_f(o, -0.30842512845993041992, -9.0728339030733922277e-09,
					     0.78539818525314331055, -2.1857338617566484855e-08));

  x = dfmul_vf2_vf2_vf2(x, vsel_vf2_vo_vf2_vf2(o, s2, vcast_vf2_vf_vf(t, (0.).as_vf())));
  x = vsel_vf2_vo_vf2_vf2(o, dfadd2_vf2_vf2_vf(x, (1.).as_vf()), x);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(4)), vcast_vi2_i(4));
  x.x = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(x.x)));
  x.y = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(x.y)));

  return x;
}

pub fn xsinpif_u05(d: VFloat) -> VFloat {
  VFloat2 x = sinpifk(d);
  VFloat r = vadd_vf_vf_vf(x.x, x.y);

  r = vsel_vf_vo_vf_vf(visnegzero_vo_vf(d), (-0.).as_vf(), r);
  r = vreinterpret_vf_vm(vandnot_vm_vo32_vm(vgt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX4_F.as_vf()), vreinterpret_vm_vf(r)));
  r = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(d), vreinterpret_vm_vf(r)));
  
  return r;
}
#[inline]
fn cospifk(d: VFloat) -> VFloat2 {
  VOpMask o;
  VFloat u, s, t;
  VFloat2 x, s2;

  u = vmul_vf_vf_vf(d, (4.).as_vf());
  VInt2 q = vtruncate_vi2_vf(u);
  q = vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vxor_vi2_vi2_vi2(vsrl_vi2_vi2_i(q, 31), vcast_vi2_i(1))), vcast_vi2_i(~1));
  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(0));

  s = vsub_vf_vf_vf(u, vcast_vf_vi2(q));
  t = s;
  s = vmul_vf_vf_vf(s, s);
  s2 = dfmul_vf2_vf_vf(t, t);
  
  //

  u = vsel_vf_vo_f_f(o, -0.2430611801e-7, 0.3093842054e-6)
      .mla(s, vsel_vf_vo_f_f(o, 0.3590577080e-5, -0.3657307388e-4))
      .mla(s, vsel_vf_vo_f_f(o, -0.3259917721e-3, 0.2490393585e-2));
  x = dfadd2_vf2_vf_vf2(vmul_vf_vf_vf(u, s),
			vsel_vf2_vo_f_f_f_f(o, 0.015854343771934509277, 4.4940051354032242811e-10,
					    -0.080745510756969451904, -1.3373665339076936258e-09));
  x = dfadd2_vf2_vf2_vf2(dfmul_vf2_vf2_vf2(s2, x),
			 vsel_vf2_vo_f_f_f_f(o, -0.30842512845993041992, -9.0728339030733922277e-09,
					     0.78539818525314331055, -2.1857338617566484855e-08));

  x = dfmul_vf2_vf2_vf2(x, vsel_vf2_vo_vf2_vf2(o, s2, vcast_vf2_vf_vf(t, (0.).as_vf())));
  x = vsel_vf2_vo_vf2_vf2(o, dfadd2_vf2_vf2_vf(x, (1.).as_vf()), x);

  o = veq_vo_vi2_vi2(vand_vi2_vi2_vi2(vadd_vi2_vi2_vi2(q, vcast_vi2_i(2)), vcast_vi2_i(4)), vcast_vi2_i(4));
  x.x = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(x.x)));
  x.y = vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vo32_vm(o, vreinterpret_vm_vf((-0.).as_vf())), vreinterpret_vm_vf(x.y)));

  return x;
}

pub fn xcospif_u05(d: VFloat) -> VFloat {
  VFloat2 x = cospifk(d);
  VFloat r = vadd_vf_vf_vf(x.x, x.y);

  r = vsel_vf_vo_vf_vf(vgt_vo_vf_vf(vabs_vf_vf(d), TRIGRANGEMAX4_F.as_vf()), (1.).as_vf(), r);
  r = vreinterpret_vf_vm(vor_vm_vo32_vm(visinf_vo_vf(d), vreinterpret_vm_vf(r)));
  
  return r;
}

#if defined(ENABLE_SVE) || defined(ENABLE_SVENOFMA)
  typedef __sizeless_struct {
    VFloat2 a, b;
  } df2;
#else
  typedef struct {
    VFloat2 a, b;
  } df2;
#endif

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
static CONST df2 gammafk(a: VFloat) {
  VFloat2 clc = vcast_vf2_f_f(0, 0), clln = vcast_vf2_f_f(1, 0), clld = vcast_vf2_f_f(1, 0);
  VFloat2 v = vcast_vf2_f_f(1, 0), x, y, z;
  VFloat t, u;

  VOpMask otiny = vlt_vo_vf_vf(vabs_vf_vf(a), (1e-30).as_vf()), oref = vlt_vo_vf_vf(a, (0.5).as_vf());

  x = vsel_vf2_vo_vf2_vf2(otiny, vcast_vf2_f_f(0, 0),
			  vsel_vf2_vo_vf2_vf2(oref, dfadd2_vf2_vf_vf((1.).as_vf(), vneg_vf_vf(a)),
					      vcast_vf2_vf_vf(a, (0.).as_vf())));

  VOpMask o0 = vand_vo_vo_vo(vle_vo_vf_vf((0.5).as_vf(), x.x), vle_vo_vf_vf(x.x, (1.2).as_vf()));
  VOpMask o2 = vle_vo_vf_vf((2.3).as_vf(), x.x);
  
  y = dfnormalize_vf2_vf2(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf(x, (1.).as_vf()), x));
  y = dfnormalize_vf2_vf2(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf(x, (2.).as_vf()), y));

  VOpMask o = vand_vo_vo_vo(o2, vle_vo_vf_vf(x.x, (7.).as_vf()));
  clln = vsel_vf2_vo_vf2_vf2(o, y, clln);

  x = vsel_vf2_vo_vf2_vf2(o, dfadd2_vf2_vf2_vf(x, (3.).as_vf()), x);
  t = vsel_vf_vo_vf_vf(o2, vrec_vf_vf(x.x), dfnormalize_vf2_vf2(dfadd2_vf2_vf2_vf(x, vsel_vf_vo_f_f(o0, -1, -2))).x);

  u = vsel_vf_vo_vo_f_f_f(o2, o0, 0.000839498720672087279971000786, 0.9435157776e+0, 0.1102489550e-3)
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, -5.17179090826059219329394422e-05, 0.8670063615e+0, 0.8160019934e-4))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, -0.000592166437353693882857342347, 0.4826702476e+0, 0.1528468856e-3))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, 6.97281375836585777403743539e-05, -0.8855129778e-1, -0.2355068718e-3))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, 0.000784039221720066627493314301, 0.1013825238e+0, 0.4962242092e-3))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, -0.000229472093621399176949318732, -0.1493408978e+0, -0.1193488017e-2))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, -0.002681327160493827160473958490, 0.1697509140e+0, 0.2891599433e-2))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, 0.003472222222222222222175164840, -0.2072454542e+0, -0.7385451812e-2))
      .mla(t, vsel_vf_vo_vo_f_f_f(o2, o0, 0.083333333333333333335592087900, 0.2705872357e+0, 0.2058077045e-1));

  y = dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf(x, (-0.5).as_vf()), logk2f(x));
  y = dfadd2_vf2_vf2_vf2(y, dfneg_vf2_vf2(x));
  y = dfadd2_vf2_vf2_vf2(y, vcast_vf2_d(0.91893853320467278056)); // 0.5*log(2*M_PI)

  z = dfadd2_vf2_vf2_vf(dfmul_vf2_vf_vf (u, t), vsel_vf_vo_f_f(o0, -0.400686534596170958447352690395e+0, -0.673523028297382446749257758235e-1));
  z = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(z, t), vsel_vf_vo_f_f(o0, 0.822466960142643054450325495997e+0, 0.322467033928981157743538726901e+0));
  z = dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(z, t), vsel_vf_vo_f_f(o0, -0.577215665946766039837398973297e+0, 0.422784335087484338986941629852e+0));
  z = dfmul_vf2_vf2_vf(z, t);

  clc = vsel_vf2_vo_vf2_vf2(o2, y, z);
  
  clld = vsel_vf2_vo_vf2_vf2(o2, dfadd2_vf2_vf2_vf(dfmul_vf2_vf_vf(u, t), (1.).as_vf()), clld);
  
  y = clln;

  clc = vsel_vf2_vo_vf2_vf2(otiny, vcast_vf2_d(41.58883083359671856503), // log(2^60)
			    vsel_vf2_vo_vf2_vf2(oref, dfadd2_vf2_vf2_vf2(vcast_vf2_d(1.1447298858494001639), dfneg_vf2_vf2(clc)), clc)); // log(M_PI)
  clln = vsel_vf2_vo_vf2_vf2(otiny, vcast_vf2_f_f(1, 0), vsel_vf2_vo_vf2_vf2(oref, clln, clld));

  if (!vtestallones_i_vo32(vnot_vo32_vo32(oref))) {
    t = vsub_vf_vf_vf(a, vmul_vf_vf_vf(F1_12.as_vf(), vcast_vf_vi2(vtruncate_vi2_vf(vmul_vf_vf_vf(a, (1. / F1_12).as_vf())))));
    x = dfmul_vf2_vf2_vf2(clld, sinpifk(t));
  }
  
  clld = vsel_vf2_vo_vf2_vf2(otiny, vcast_vf2_vf_vf(vmul_vf_vf_vf(a, (F1_30*F1_30).as_vf()), (0.).as_vf()),
			     vsel_vf2_vo_vf2_vf2(oref, x, y));

  df2 ret = { clc, dfdiv_vf2_vf2_vf2(clln, clld) };

  return ret;
}

pub fn xtgammaf_u1(a: VFloat) -> VFloat {
  df2 d = gammafk(a);
  VFloat2 y = dfmul_vf2_vf2_vf2(expk2f(d.a), d.b);
  VFloat r = vadd_vf_vf_vf(y.x, y.y);
  VOpMask o;

  o = vor_vo_vo_vo(vor_vo_vo_vo(veq_vo_vf_vf(a, (-SLEEF_INFINITY_F).as_vf()),
				vand_vo_vo_vo(vlt_vo_vf_vf(a, (0.).as_vf()), visint_vo_vf(a))),
		   vand_vo_vo_vo(vand_vo_vo_vo(visnumber_vo_vf(a), vlt_vo_vf_vf(a, (0.).as_vf())), visnan_vo_vf(r)));
  r = vsel_vf_vo_vf_vf(o, SLEEF_NAN_F.as_vf(), r);

  o = vand_vo_vo_vo(vand_vo_vo_vo(vor_vo_vo_vo(veq_vo_vf_vf(a, SLEEF_INFINITY_F.as_vf()), visnumber_vo_vf(a)),
				  vge_vo_vf_vf(a, (-f32::MIN).as_vf())),
		    vor_vo_vo_vo(vor_vo_vo_vo(veq_vo_vf_vf(a, (0.).as_vf()), vgt_vo_vf_vf(a, (36.).as_vf())), visnan_vo_vf(r)));
  r = vsel_vf_vo_vf_vf(o, vmulsign_vf_vf_vf(SLEEF_INFINITY_F.as_vf(), a), r);
  
  return r;
}

pub fn xlgammaf_u1(a: VFloat) -> VFloat {
  df2 d = gammafk(a);
  VFloat2 y = dfadd2_vf2_vf2_vf2(d.a, logk2f(dfabs_vf2_vf2(d.b)));
  VFloat r = vadd_vf_vf_vf(y.x, y.y);
  VOpMask o;

  o = vor_vo_vo_vo(visinf_vo_vf(a),
		   vor_vo_vo_vo(vand_vo_vo_vo(vle_vo_vf_vf(a, (0.).as_vf()), visint_vo_vf(a)),
				vand_vo_vo_vo(visnumber_vo_vf(a), visnan_vo_vf(r))));
  r = vsel_vf_vo_vf_vf(o, SLEEF_INFINITY_F.as_vf(), r);

  return r;
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xerff_u1(a: VFloat) -> VFloat {
  VFloat s = a, t, u;
  VFloat2 d;

  a = vabs_vf_vf(a);
  VOpMask o0 = vlt_vo_vf_vf(a, (1.1).as_vf());
  VOpMask o1 = vlt_vo_vf_vf(a, (2.4).as_vf());
  VOpMask o2 = vlt_vo_vf_vf(a, (4.).as_vf());
  u = vsel_vf_vo_vf_vf(o0, vmul_vf_vf_vf(a, a), a);
  
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
  d = vsel_vf2_vo_vf2_vf2(o0, d, dfadd_vf2_vf_vf2((1.).as_vf(), dfneg_vf2_vf2(expk2f(d))));
  u = vmulsign_vf_vf_vf(vsel_vf_vo_vf_vf(o2, vadd_vf_vf_vf(d.x, d.y), (1.).as_vf()), s);
  u = vsel_vf_vo_vf_vf(visnan_vo_vf(a), SLEEF_NAN_F.as_vf(), u);

  return u;
}

/* TODO AArch64: potential optimization by using `vfmad_lane_f64` */
pub fn xerfcf_u15(a: VFloat) -> VFloat {
  VFloat s = a, r = (0.).as_vf(), t;
  VFloat2 u, d, x;
  a = vabs_vf_vf(a);
  VOpMask o0 = vlt_vo_vf_vf(a, (1.).as_vf());
  VOpMask o1 = vlt_vo_vf_vf(a, (2.2).as_vf());
  VOpMask o2 = vlt_vo_vf_vf(a, (4.3).as_vf());
  VOpMask o3 = vlt_vo_vf_vf(a, (10.1).as_vf());

  u = vsel_vf2_vo_vf2_vf2(o1, vcast_vf2_vf_vf(a, (0.).as_vf()), dfdiv_vf2_vf2_vf2(vcast_vf2_f_f(1, 0), vcast_vf2_vf_vf(a, (0.).as_vf())));

  t = vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, -0.8638041618e-4, -0.6236977242e-5, -0.3869504035e+0, 0.1115344167e+1)
      .mla(u.x, vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, 0.6000166177e-3, 0.5749821503e-4, 0.1288077235e+1, -0.9454904199e+0))
      .mla(u.x, vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, -0.1665703603e-2, 0.6002851478e-5, -0.1816803217e+1, -0.3667259514e+0))
      .mla(u.x, vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, 0.1795156277e-3, -0.2851036377e-2, 0.1249150872e+1, 0.7155663371e+0))
      .mla(u.x, vsel_vf_vo_vo_vo_f_f_f_f(o0, o1, o2, 0.1914106123e-1, 0.2260518074e-1, -0.1328857988e+0, -0.1262947265e-1));

  d = dfmul_vf2_vf2_vf(u, t);
  d = dfadd2_vf2_vf2_vf2(d, vsel_vf2_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.102775359343930288081655368891e+0, -0.105247583459338632253369014063e+0, -0.482365310333045318680618892669e+0, -0.498961546254537647970305302739e+0));
  d = dfmul_vf2_vf2_vf2(d, u);
  d = dfadd2_vf2_vf2_vf2(d, vsel_vf2_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.636619483208481931303752546439e+0, -0.635609463574589034216723775292e+0, -0.134450203224533979217859332703e-2, -0.471199543422848492080722832666e-4));
  d = dfmul_vf2_vf2_vf2(d, u);
  d = dfadd2_vf2_vf2_vf2(d, vsel_vf2_vo_vo_vo_d_d_d_d(o0, o1, o2, -0.112837917790537404939545770596e+1, -0.112855987376668622084547028949e+1, -0.572319781150472949561786101080e+0, -0.572364030327966044425932623525e+0));
  
  x = dfmul_vf2_vf2_vf(vsel_vf2_vo_vf2_vf2(o1, d, vcast_vf2_vf_vf(vneg_vf_vf(a), (0.).as_vf())), a);
  x = vsel_vf2_vo_vf2_vf2(o1, x, dfadd2_vf2_vf2_vf2(x, d));

  x = expk2f(x);
  x = vsel_vf2_vo_vf2_vf2(o1, x, dfmul_vf2_vf2_vf2(x, u));

  r = vsel_vf_vo_vf_vf(o3, vadd_vf_vf_vf(x.x, x.y), (0.).as_vf());
  r = vsel_vf_vo_vf_vf(vsignbit_vo_vf(s), vsub_vf_vf_vf((2.).as_vf(), r), r);
  r = vsel_vf_vo_vf_vf(visnan_vo_vf(s), SLEEF_NAN_F.as_vf(), r);
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
pub fn VFloat __acosf_finite     (VFloat)         __attribute__((weak, alias(str_xacosf_u1  )));
pub fn VFloat __acoshf_finite    (VFloat)         __attribute__((weak, alias(str_xacoshf    )));
pub fn VFloat __asinf_finite     (double)         __attribute__((weak, alias(str_xasinf_u1  )));
pub fn VFloat __atan2f_finite    (VFloat, VFloat) __attribute__((weak, alias(str_xatan2f_u1 )));
pub fn VFloat __atanhf_finite    (VFloat)         __attribute__((weak, alias(str_xatanhf    )));
pub fn VFloat __coshf_finite     (VFloat)         __attribute__((weak, alias(str_xcoshf     )));
pub fn VFloat __exp10f_finite    (VFloat)         __attribute__((weak, alias(str_xexp10f    )));
pub fn VFloat __exp2f_finite     (VFloat)         __attribute__((weak, alias(str_xexp2f     )));
pub fn VFloat __expf_finite      (VFloat)         __attribute__((weak, alias(str_xexpf      )));
pub fn VFloat __fmodf_finite     (VFloat, VFloat) __attribute__((weak, alias(str_xfmodf     )));
pub fn VFloat __modff_finite      (VFloat, VFloat *) __attribute__((weak, alias(str_xmodff  )));
pub fn VFloat __hypotf_u05_finite(VFloat, VFloat) __attribute__((weak, alias(str_xhypotf_u05)));
pub fn VFloat __lgammaf_u1_finite(VFloat)         __attribute__((weak, alias(str_xlgammaf_u1)));
pub fn VFloat __log10f_finite    (VFloat)         __attribute__((weak, alias(str_xlog10f    )));
pub fn VFloat __logf_finite      (VFloat)         __attribute__((weak, alias(str_xlogf_u1   )));
pub fn VFloat __powf_finite      (VFloat, VFloat) __attribute__((weak, alias(str_xpowf      )));
pub fn VFloat __sinhf_finite     (VFloat)         __attribute__((weak, alias(str_xsinhf     )));
pub fn VFloat __sqrtf_finite     (VFloat)         __attribute__((weak, alias(str_xsqrtf     )));
pub fn VFloat __tgammaf_u1_finite(VFloat)         __attribute__((weak, alias(str_xtgammaf_u1)));

#ifdef HEADER_MASKED
#include HEADER_MASKED
#endif
#endif /* #ifdef ENABLE_GNUABI */

*/
