//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

/*#if defined(ENABLE_SVE) || defined(ENABLE_SVENOFMA)
typedef __sizeless_struct VFloat2 {
  svfoat32_t x;
  svfloat32_t y;
} VFloat2;
#else*/
pub struct VFloat2(VFloat, VFloat);
//#endif
#[inline]
fn vupper_vf_vf(d: VFloat) -> VFloat {
  return vreinterpret_vf_vi2(vand_vi2_vi2_vi2(vreinterpret_vi2_vf(d), vcast_vi2_i(0xfffff000)));
}

#[inline]
fn vcast_vf2_vf_vf(VFloat h, VFloat l) -> VFloat2 {
  VFloat2 ret = {h, l};
  return ret;
}

#[inline]
fn vcast_vf2_f_f(float h, float l) -> VFloat2 {
  VFloat2 ret = {vcast_vf_f(h), vcast_vf_f(l)};
  return ret;
}

#[inline]
fn vcast_vf2_d(double d) -> VFloat2 {
  VFloat2 ret = {vcast_vf_f(d), vcast_vf_f(d - (float)d)};
  return ret;
}

#[inline]
fn vsel_vf2_vo_vf2_vf2(VOpMask m, VFloat2 x, VFloat2 y) -> VFloat2 {
  VFloat2 r;
  r.0 = vsel_vf_vo_vf_vf(m, x.0, y.0);
  r.1 = vsel_vf_vo_vf_vf(m, x.1, y.1);
  return r;
}

#[inline]
fn vsel_vf2_vo_f_f_f_f(VOpMask o, float x1, float y1, float x0, float y0) -> VFloat2 {
  VFloat2 r;
  r.0 = vsel_vf_vo_f_f(o, x1, x0);
  r.1 = vsel_vf_vo_f_f(o, y1, y0);
  return r;
}

#[inline]
fn vsel_vf2_vo_vo_d_d_d(VOpMask o0, VOpMask o1, double d0, double d1, double d2) -> VFloat2 {
  return vsel_vf2_vo_vf2_vf2(o0, vcast_vf2_d(d0), vsel_vf2_vo_vf2_vf2(o1, vcast_vf2_d(d1), vcast_vf2_d(d2)));
}

#[inline]
fn vsel_vf2_vo_vo_vo_d_d_d_d(VOpMask o0, VOpMask o1, VOpMask o2, double d0, double d1, double d2, double d3) -> VFloat2 {
  return vsel_vf2_vo_vf2_vf2(o0, vcast_vf2_d(d0), vsel_vf2_vo_vf2_vf2(o1, vcast_vf2_d(d1), vsel_vf2_vo_vf2_vf2(o2, vcast_vf2_d(d2), vcast_vf2_d(d3))));
}

#[inline]
fn vabs_vf2_vf2(x: VFloat2) -> VFloat2 {
  return vcast_vf2_vf_vf(vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vf(vcast_vf_f(-0.0)), vreinterpret_vm_vf(x.0)), vreinterpret_vm_vf(x.0))),
			 vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vf(vcast_vf_f(-0.0)), vreinterpret_vm_vf(x.0)), vreinterpret_vm_vf(x.1))));
}

#[inline]
fn vadd_vf_3vf(VFloat v0, VFloat v1, VFloat v2) -> VFloat {
  return v0 + v1 + v2;
}

#[inline]
fn vadd_vf_4vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3) -> VFloat {
  return vadd_vf_3vf(v0 + v1, v2, v3);
}

#[inline]
fn vadd_vf_5vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3, VFloat v4) -> VFloat {
  return vadd_vf_4vf(v0 + v1, v2, v3, v4);
}

#[inline]
fn vadd_vf_6vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3, VFloat v4, VFloat v5) -> VFloat {
  return vadd_vf_5vf(v0 + v1, v2, v3, v4, v5);
}

#[inline]
fn vadd_vf_7vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3, VFloat v4, VFloat v5, VFloat v6) -> VFloat {
  return vadd_vf_6vf(v0 + v1, v2, v3, v4, v5, v6);
}

#[inline]
fn vsub_vf_3vf(VFloat v0, VFloat v1, VFloat v2) -> VFloat {
  return v0 - v1 - v2;
}

#[inline]
fn vsub_vf_4vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3) -> VFloat {
  return vsub_vf_3vf(v0 - v1, v2, v3);
}

#[inline]
fn vsub_vf_5vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3, VFloat v4) -> VFloat {
  return vsub_vf_4vf(v0 - v1, v2, v3, v4);
}

//

#[inline]
fn dfneg_vf2_vf2(x: VFloat2) -> VFloat2 {
  return vcast_vf2_vf_vf(-x.0, -x.1);
}

#[inline]
fn dfabs_vf2_vf2(x: VFloat2) -> VFloat2 {
  return vcast_vf2_vf_vf(vabs_vf_vf(x.0),
			 vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x.1), vand_vm_vm_vm(vreinterpret_vm_vf(x.0), vreinterpret_vm_vf(vcast_vf_f(-0.0f))))));
}

#[inline]
fn dfnormalize_vf2_vf2(t: VFloat2) -> VFloat2 {
  VFloat2 s;

  s.0 = t.0 + t.1;
  s.1 = t.0 - s.0 + t.1;

  return s;
}

#[inline]
fn dfscale_vf2_vf2_vf(VFloat2 d, VFloat s) -> VFloat2 {
  VFloat2 r = {d.0 * s, d.1 * s};
  return r;
}

#[inline]
fn dfadd_vf2_vf_vf(VFloat x, VFloat y) -> VFloat2 {
  VFloat2 r;

  r.0 = x + y;
  r.1 = x - r.0 + y;

  return r;
}

#[inline]
fn dfadd2_vf2_vf_vf(VFloat x, VFloat y) -> VFloat2 {
  VFloat2 r;

  r.0 = x + y;
  VFloat v = r.0 - x;
  r.1 = x - (r.0 - v) + (y - v);

  return r;
}

#[inline]
fn dfadd2_vf2_vf_vf2(VFloat x, VFloat2 y) -> VFloat2 {
  VFloat2 r;

  r.0  = x + y.0;
  VFloat v = r.0 - x;
  r.1 = x - (r.0 - v) + (y.0 - v) + y.1;

  return r;
}

#[inline]
fn dfadd_vf2_vf2_vf(VFloat2 x, VFloat y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 + y;
  r.1 = vadd_vf_3vf(x.0 - r.0, y, x.1);

  return r;
}

#[inline]
fn dfsub_vf2_vf2_vf(VFloat2 x, VFloat y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 - y;
  r.1 = x.0 - r.0 - y + x.1;

  return r;
}

#[inline]
fn dfadd2_vf2_vf2_vf(VFloat2 x, VFloat y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 + y;
  VFloat v = r.0 - x.0;
  r.1 = x.0 - (r.0 - v) + (y - v);
  r.1 = r.1 + x.1;

  return r;
}

#[inline]
fn dfadd_vf2_vf_vf2(VFloat x, VFloat2 y) -> VFloat2 {
  VFloat2 r;

  r.0 = x + y.0;
  r.1 = vadd_vf_3vf(x - r.0, y.0, y.1);

  return r;
}

#[inline]
fn dfadd_vf2_vf2_vf2(VFloat2 x, VFloat2 y) -> VFloat2 {
  // |x| >= |y|

  VFloat2 r;

  r.0 = x.0 + y.0;
  r.1 = vadd_vf_4vf(x.0 - r.0, y.0, x.1, y.1);

  return r;
}

#[inline]
fn dfadd2_vf2_vf2_vf2(VFloat2 x, VFloat2 y) -> VFloat2 {
  VFloat2 r;

  r.0  = x.0 + y.0;
  VFloat v = r.0 - x.0;
  r.1 = x.0 - (r.0 - v) + (y.0 - v);
  r.1 = r.1 + (x.1 + y.1);

  return r;
}

#[inline]
fn dfsub_vf2_vf_vf(VFloat x, VFloat y) -> VFloat2 {
  // |x| >= |y|

  VFloat2 r;

  r.0 = x - y;
  r.1 = x - r.0 - y;

  return r;
}

#[inline]
fn dfsub_vf2_vf2_vf2(VFloat2 x, VFloat2 y) -> VFloat2 {
  // |x| >= |y|

  VFloat2 r;

  r.0 = x.0 - y.0;
  r.1 = x.0 - r.0;
  r.1 = r.1 - y.0;
  r.1 = r.1 + x.1;
  r.1 = r.1 - y.1;

  return r;
}

#ifdef ENABLE_FMA_SP
#[inline]
fn dfdiv_vf2_vf2_vf2(VFloat2 n, VFloat2 d) -> VFloat2 {
  VFloat2 q;
  VFloat t = vrec_vf_vf(d.0), u;

  q.0 = n.0 * t;
  u = vfmapn_vf_vf_vf_vf(t, n.0, q.0);
  q.1 = vfmanp_vf_vf_vf_vf(d.1, t, vfmanp_vf_vf_vf_vf(d.0, t, vcast_vf_f(1)));
  q.1 = vfma_vf_vf_vf_vf(q.0, q.1, vfma_vf_vf_vf_vf(n.1, t, u));

  return q;
}

#[inline]
fn dfmul_vf2_vf_vf(VFloat x, VFloat y) -> VFloat2 {
  VFloat2 r;

  r.0 = x * y;
  r.1 = vfmapn_vf_vf_vf_vf(x, y, r.0);

  return r;
}

#[inline]
fn dfsqu_vf2_vf2(x: VFloat2) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 * x.0;
  r.1 = vfma_vf_vf_vf_vf(x.0 + x.0, x.1, vfmapn_vf_vf_vf_vf(x.0, x.0, r.0));

  return r;
}

#[inline]
fn dfsqu_vf_vf2(x: VFloat2) -> VFloat {
  return vfma_vf_vf_vf_vf(x.0, x.0, x.0 * x.1 + x.0 * x.1);
}

#[inline]
fn dfmul_vf2_vf2_vf2(VFloat2 x, VFloat2 y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 * y.0;
  r.1 = vfma_vf_vf_vf_vf(x.0, y.1, vfma_vf_vf_vf_vf(x.1, y.0, vfmapn_vf_vf_vf_vf(x.0, y.0, r.0)));

  return r;
}

#[inline]
fn dfmul_vf_vf2_vf2(VFloat2 x, VFloat2 y) -> VFloat {
  return vfma_vf_vf_vf_vf(x.0, y.0, vfma_vf_vf_vf_vf(x.1, y.0, x.0 * y.1));
}

#[inline]
fn dfmul_vf2_vf2_vf(VFloat2 x, VFloat y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 * y;
  r.1 = vfma_vf_vf_vf_vf(x.1, y, vfmapn_vf_vf_vf_vf(x.0, y, r.0));

  return r;
}

#[inline]
fn dfrec_vf2_vf(d: VFloat) -> VFloat2 {
  VFloat2 q;

  q.0 = vrec_vf_vf(d);
  q.1 = q.0 * vfmanp_vf_vf_vf_vf(d, q.0, vcast_vf_f(1));

  return q;
}

#[inline]
fn dfrec_vf2_vf2(d: VFloat2) -> VFloat2 {
  VFloat2 q;

  q.0 = vrec_vf_vf(d.0);
  q.1 = q.0 * vfmanp_vf_vf_vf_vf(d.1, q.0, vfmanp_vf_vf_vf_vf(d.0, q.0, vcast_vf_f(1)));

  return q;
}
#else
#[inline]
fn dfdiv_vf2_vf2_vf2(VFloat2 n, VFloat2 d) -> VFloat2 {
  VFloat t = vrec_vf_vf(d.0);
  VFloat dh  = vupper_vf_vf(d.0), dl  = d.0 -  dh;
  VFloat th  = vupper_vf_vf(t  ), tl  = t   -  th;
  VFloat nhh = vupper_vf_vf(n.0), nhl = n.0 - nhh;

  VFloat2 q;

  q.0 = n.0 * t;

  VFloat u, w;
  w = vcast_vf_f(-1);
  w = dh.mla(th, w);
  w = dh.mla(tl, w);
  w = dl.mla(th, w);
  w = dl.mla(tl, w);
  w = -w;

  u = nhh.mla(th, -q.0);
  u = nhh.mla(tl, u);
  u = nhl.mla(th, u);
  u = nhl.mla(tl, u);
  u = q.0.mla(w , u);

  q.1 = t.mla(n.1 - q.0 * d.1, u);

  return q;
}

#[inline]
fn dfmul_vf2_vf_vf(VFloat x, VFloat y) -> VFloat2 {
  VFloat xh = vupper_vf_vf(x), xl = x - xh;
  VFloat yh = vupper_vf_vf(y), yl = y - yh;
  VFloat2 r;

  r.0 = x * y;

  VFloat t;
  t = xh.mla(yh, -r.0);
  t = xl.mla(yh, t);
  t = xh.mla(yl, t);
  t = xl.mla(yl, t);
  r.1 = t;

  return r;
}

#[inline]
fn dfmul_vf2_vf2_vf(VFloat2 x, VFloat y) -> VFloat2 {
  VFloat xh = vupper_vf_vf(x.0), xl = x.0 - xh;
  VFloat yh = vupper_vf_vf(y  ), yl = y   - yh;
  VFloat2 r;

  r.0 = x.0 * y;

  VFloat t;
  t = xh.mla(yh, -r.0);
  t = xl.mla(yh, t);
  t = xh.mla(yl, t);
  t = xl.mla(yl, t);
  t = x.1.mla(y, t);
  r.1 = t;

  return r;
}

#[inline]
fn dfmul_vf2_vf2_vf2(VFloat2 x, VFloat2 y) -> VFloat2 {
  VFloat xh = vupper_vf_vf(x.0), xl = x.0 - xh;
  VFloat yh = vupper_vf_vf(y.0), yl = y.0 - yh;
  VFloat2 r;

  r.0 = x.0 * y.0;

  VFloat t;
  t = xh.mla(yh, -r.0);
  t = xl.mla(yh, t);
  t = xh.mla(yl, t);
  t = xl.mla(yl, t);
  t = x.0.mla(y.1, t);
  t = x.1.mla(y.0, t);
  r.1 = t;

  return r;
}

#[inline]
fn dfmul_vf_vf2_vf2(VFloat2 x, VFloat2 y) -> VFloat {
  VFloat xh = vupper_vf_vf(x.0), xl = x.0 - xh;
  VFloat yh = vupper_vf_vf(y.0), yl = y.0 - yh;

  return vadd_vf_6vf(x.1 * yh, xh * y.1, xl * yl, xh * yl, xl * yh, xh * yh);
}

#[inline]
fn dfsqu_vf2_vf2(x: VFloat2) -> VFloat2 {
  VFloat xh = vupper_vf_vf(x.0), xl = x.0 - xh;
  VFloat2 r;

  r.0 = x.0 * x.0;

  VFloat t;
  t = xh.mla(xh, -r.0);
  t = (xh + xh).mla(xl, t);
  t = xl.mla(xl, t);
  t = x.0.mla(x.1 + x.1, t);
  r.1 = t;

  return r;
}

#[inline]
fn dfsqu_vf_vf2(x: VFloat2) -> VFloat {
  VFloat xh = vupper_vf_vf(x.0), xl = x.0 - xh;

  return vadd_vf_5vf(xh * x.1, xh * x.1, xl * xl, xh * xl + xh * xl, xh * xh);
}

#[inline]
fn dfrec_vf2_vf(d: VFloat) -> VFloat2 {
  VFloat t = vrec_vf_vf(d);
  VFloat dh = vupper_vf_vf(d), dl = d - dh;
  VFloat th = vupper_vf_vf(t), tl = t - th;
  VFloat2 q;

  q.0 = t;

  VFloat u = vcast_vf_f(-1);
  u = dh.mla(th, u);
  u = dh.mla(tl, u);
  u = dl.mla(th, u);
  u = dl.mla(tl, u);
  q.1 = (-t) * u;

  return q;
}

#[inline]
fn dfrec_vf2_vf2(d: VFloat2) -> VFloat2 {
  VFloat t = vrec_vf_vf(d.0);
  VFloat dh = vupper_vf_vf(d.0), dl = d.0 - dh;
  VFloat th = vupper_vf_vf(t  ), tl = t   - th;
  VFloat2 q;

  q.0 = t;

  VFloat u = vcast_vf_f(-1);
  u = dh.mla(th, u);
  u = dh.mla(tl, u);
  u = dl.mla(th, u);
  u = dl.mla(tl, u);
  u = d.1.mla(t, u);
  q.1 = (-t) * u;

  return q;
}
#endif

#[inline]
fn dfsqrt_vf2_vf2(d: VFloat2) -> VFloat2 {
#ifdef ENABLE_RECSQRT_SP
  VFloat x = vrecsqrt_vf_vf(d.0 + d.1);
  VFloat2 r = dfmul_vf2_vf2_vf(d, x);
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(r, dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(r, x), vcast_vf_f(-3.0))), vcast_vf_f(-0.5));
#else
  VFloat t = vsqrt_vf_vf(d.0 + d.1);
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf2(d, dfmul_vf2_vf_vf(t, t)), dfrec_vf2_vf(t)), vcast_vf_f(0.5));
#endif
}

#[inline]
fn dfsqrt_vf2_vf(d: VFloat) -> VFloat2 {
  VFloat t = vsqrt_vf_vf(d);
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf_vf2(d, dfmul_vf2_vf_vf(t, t)), dfrec_vf2_vf(t)), vcast_vf_f(0.5));
}
