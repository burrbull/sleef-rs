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
pub struct VFloat2($f32x, $f32x);
//#endif
#[inline]
fn vupper_vf_vf(d: $f32x) -> $f32x {
  return vreinterpret_vf_vi2(vand_vi2_vi2_vi2(vreinterpret_vi2_vf(d), $ix2::splat(0xfffff000)));
}

#[inline]
fn vcast_vf2_vf_vf($f32x h, $f32x l) -> VFloat2 {
  VFloat2 ret = {h, l};
  return ret;
}

#[inline]
fn vcast_vf2_f_f(h: f32, l: f32) -> VFloat2 {
  VFloat2 ret = {$f32x::splat(h), $f32x::splat(l)};
  return ret;
}

#[inline]
fn vcast_vf2_d(double d) -> VFloat2 {
  VFloat2 ret = {$f32x::splat(d), $f32x::splat(d - (float)d)};
  return ret;
}

#[inline]
fn vsel_vf2_vo_vf2_vf2($mox m, VFloat2 x, VFloat2 y) -> VFloat2 {
  VFloat2 r;
  r.0 = vsel_vf_vo_vf_vf(m, x.0, y.0);
  r.1 = vsel_vf_vo_vf_vf(m, x.1, y.1);
  return r;
}

#[inline]
fn vsel_vf2_vo_f_f_f_f(o: $mox, x1: f32, y1: f32, x0: f32, y0: f32) -> VFloat2 {
  VFloat2 r;
  r.0 = vsel_vf_vo_f_f(o, x1, x0);
  r.1 = vsel_vf_vo_f_f(o, y1, y0);
  return r;
}

#[inline]
fn vsel_vf2_vo_vo_d_d_d(o0: $mox, o1: $mox, d0: f64, d1: f64, d2: f64) -> VFloat2 {
  return vsel_vf2_vo_vf2_vf2(o0, vcast_vf2_d(d0), vsel_vf2_vo_vf2_vf2(o1, vcast_vf2_d(d1), vcast_vf2_d(d2)));
}

#[inline]
fn vsel_vf2_vo_vo_vo_d_d_d_d(o0: $mox, o1: $mox, o2: $mox, d0: f64, d1: f64, d2: f64, d3: f64) -> VFloat2 {
  return vsel_vf2_vo_vf2_vf2(o0, vcast_vf2_d(d0), vsel_vf2_vo_vf2_vf2(o1, vcast_vf2_d(d1), vsel_vf2_vo_vf2_vf2(o2, vcast_vf2_d(d2), vcast_vf2_d(d3))));
}

#[inline]
fn vabs_vf2_vf2(x: VFloat2) -> VFloat2 {
  return vcast_vf2_vf_vf(vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.0)), vreinterpret_vm_vf(x.0)), vreinterpret_vm_vf(x.0))),
			 vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.0)), vreinterpret_vm_vf(x.0)), vreinterpret_vm_vf(x.1))));
}

#[inline]
fn vadd_vf_3vf($f32x v0, $f32x v1, $f32x v2) -> $f32x {
  return v0 + v1 + v2;
}

#[inline]
fn vadd_vf_4vf($f32x v0, $f32x v1, $f32x v2, $f32x v3) -> $f32x {
  return vadd_vf_3vf(v0 + v1, v2, v3);
}

#[inline]
fn vadd_vf_5vf($f32x v0, $f32x v1, $f32x v2, $f32x v3, $f32x v4) -> $f32x {
  return vadd_vf_4vf(v0 + v1, v2, v3, v4);
}

#[inline]
fn vadd_vf_6vf($f32x v0, $f32x v1, $f32x v2, $f32x v3, $f32x v4, $f32x v5) -> $f32x {
  return vadd_vf_5vf(v0 + v1, v2, v3, v4, v5);
}

#[inline]
fn vadd_vf_7vf($f32x v0, $f32x v1, $f32x v2, $f32x v3, $f32x v4, $f32x v5, $f32x v6) -> $f32x {
  return vadd_vf_6vf(v0 + v1, v2, v3, v4, v5, v6);
}

#[inline]
fn vsub_vf_3vf($f32x v0, $f32x v1, $f32x v2) -> $f32x {
  return v0 - v1 - v2;
}

#[inline]
fn vsub_vf_4vf($f32x v0, $f32x v1, $f32x v2, $f32x v3) -> $f32x {
  return vsub_vf_3vf(v0 - v1, v2, v3);
}

#[inline]
fn vsub_vf_5vf($f32x v0, $f32x v1, $f32x v2, $f32x v3, $f32x v4) -> $f32x {
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
			 vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x.1), vand_vm_vm_vm(vreinterpret_vm_vf(x.0), vreinterpret_vm_vf($f32x::splat(-0.0f))))));
}

#[inline]
fn dfnormalize_vf2_vf2(t: VFloat2) -> VFloat2 {
  VFloat2 s;

  s.0 = t.0 + t.1;
  s.1 = t.0 - s.0 + t.1;

  return s;
}

#[inline]
fn dfscale_vf2_vf2_vf(VFloat2 d, $f32x s) -> VFloat2 {
  VFloat2 r = {d.0 * s, d.1 * s};
  return r;
}

#[inline]
fn dfadd_vf2_vf_vf($f32x x, $f32x y) -> VFloat2 {
  VFloat2 r;

  r.0 = x + y;
  r.1 = x - r.0 + y;

  return r;
}

#[inline]
fn dfadd2_vf2_vf_vf($f32x x, $f32x y) -> VFloat2 {
  VFloat2 r;

  r.0 = x + y;
  $f32x v = r.0 - x;
  r.1 = x - (r.0 - v) + (y - v);

  return r;
}

#[inline]
fn dfadd2_vf2_vf_vf2($f32x x, VFloat2 y) -> VFloat2 {
  VFloat2 r;

  r.0  = x + y.0;
  $f32x v = r.0 - x;
  r.1 = x - (r.0 - v) + (y.0 - v) + y.1;

  return r;
}

#[inline]
fn dfadd_vf2_vf2_vf(VFloat2 x, $f32x y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 + y;
  r.1 = vadd_vf_3vf(x.0 - r.0, y, x.1);

  return r;
}

#[inline]
fn dfsub_vf2_vf2_vf(VFloat2 x, $f32x y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 - y;
  r.1 = x.0 - r.0 - y + x.1;

  return r;
}

#[inline]
fn dfadd2_vf2_vf2_vf(VFloat2 x, $f32x y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 + y;
  $f32x v = r.0 - x.0;
  r.1 = x.0 - (r.0 - v) + (y - v);
  r.1 = r.1 + x.1;

  return r;
}

#[inline]
fn dfadd_vf2_vf_vf2($f32x x, VFloat2 y) -> VFloat2 {
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
  $f32x v = r.0 - x.0;
  r.1 = x.0 - (r.0 - v) + (y.0 - v);
  r.1 = r.1 + (x.1 + y.1);

  return r;
}

#[inline]
fn dfsub_vf2_vf_vf($f32x x, $f32x y) -> VFloat2 {
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
  $f32x t = vrec_vf_vf(d.0), u;

  q.0 = n.0 * t;
  u = vfmapn_vf_vf_vf_vf(t, n.0, q.0);
  q.1 = vfmanp_vf_vf_vf_vf(d.1, t, vfmanp_vf_vf_vf_vf(d.0, t, $f32x::splat(1)));
  q.1 = vfma_vf_vf_vf_vf(q.0, q.1, vfma_vf_vf_vf_vf(n.1, t, u));

  return q;
}

#[inline]
fn dfmul_vf2_vf_vf($f32x x, $f32x y) -> VFloat2 {
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
fn dfsqu_vf_vf2(x: VFloat2) -> $f32x {
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
fn dfmul_vf_vf2_vf2(VFloat2 x, VFloat2 y) -> $f32x {
  return vfma_vf_vf_vf_vf(x.0, y.0, vfma_vf_vf_vf_vf(x.1, y.0, x.0 * y.1));
}

#[inline]
fn dfmul_vf2_vf2_vf(VFloat2 x, $f32x y) -> VFloat2 {
  VFloat2 r;

  r.0 = x.0 * y;
  r.1 = vfma_vf_vf_vf_vf(x.1, y, vfmapn_vf_vf_vf_vf(x.0, y, r.0));

  return r;
}

#[inline]
fn dfrec_vf2_vf(d: $f32x) -> VFloat2 {
  VFloat2 q;

  q.0 = vrec_vf_vf(d);
  q.1 = q.0 * vfmanp_vf_vf_vf_vf(d, q.0, $f32x::splat(1));

  return q;
}

#[inline]
fn dfrec_vf2_vf2(d: VFloat2) -> VFloat2 {
  VFloat2 q;

  q.0 = vrec_vf_vf(d.0);
  q.1 = q.0 * vfmanp_vf_vf_vf_vf(d.1, q.0, vfmanp_vf_vf_vf_vf(d.0, q.0, $f32x::splat(1)));

  return q;
}
#else
#[inline]
fn dfdiv_vf2_vf2_vf2(VFloat2 n, VFloat2 d) -> VFloat2 {
  $f32x t = vrec_vf_vf(d.0);
  $f32x dh  = vupper_vf_vf(d.0), dl  = d.0 -  dh;
  $f32x th  = vupper_vf_vf(t  ), tl  = t   -  th;
  $f32x nhh = vupper_vf_vf(n.0), nhl = n.0 - nhh;

  VFloat2 q;

  q.0 = n.0 * t;

  $f32x u, w;
  w = $f32x::splat(-1);
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
fn dfmul_vf2_vf_vf($f32x x, $f32x y) -> VFloat2 {
  $f32x xh = vupper_vf_vf(x), xl = x - xh;
  $f32x yh = vupper_vf_vf(y), yl = y - yh;
  VFloat2 r;

  r.0 = x * y;

  $f32x t;
  t = xh.mla(yh, -r.0);
  t = xl.mla(yh, t);
  t = xh.mla(yl, t);
  t = xl.mla(yl, t);
  r.1 = t;

  return r;
}

#[inline]
fn dfmul_vf2_vf2_vf(VFloat2 x, $f32x y) -> VFloat2 {
  $f32x xh = vupper_vf_vf(x.0), xl = x.0 - xh;
  $f32x yh = vupper_vf_vf(y  ), yl = y   - yh;
  VFloat2 r;

  r.0 = x.0 * y;

  $f32x t;
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
  $f32x xh = vupper_vf_vf(x.0), xl = x.0 - xh;
  $f32x yh = vupper_vf_vf(y.0), yl = y.0 - yh;
  VFloat2 r;

  r.0 = x.0 * y.0;

  $f32x t;
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
fn dfmul_vf_vf2_vf2(VFloat2 x, VFloat2 y) -> $f32x {
  $f32x xh = vupper_vf_vf(x.0), xl = x.0 - xh;
  $f32x yh = vupper_vf_vf(y.0), yl = y.0 - yh;

  return vadd_vf_6vf(x.1 * yh, xh * y.1, xl * yl, xh * yl, xl * yh, xh * yh);
}

#[inline]
fn dfsqu_vf2_vf2(x: VFloat2) -> VFloat2 {
  $f32x xh = vupper_vf_vf(x.0), xl = x.0 - xh;
  VFloat2 r;

  r.0 = x.0 * x.0;

  $f32x t;
  t = xh.mla(xh, -r.0);
  t = (xh + xh).mla(xl, t);
  t = xl.mla(xl, t);
  t = x.0.mla(x.1 + x.1, t);
  r.1 = t;

  return r;
}

#[inline]
fn dfsqu_vf_vf2(x: VFloat2) -> $f32x {
  $f32x xh = vupper_vf_vf(x.0), xl = x.0 - xh;

  return vadd_vf_5vf(xh * x.1, xh * x.1, xl * xl, xh * xl + xh * xl, xh * xh);
}

#[inline]
fn dfrec_vf2_vf(d: $f32x) -> VFloat2 {
  $f32x t = vrec_vf_vf(d);
  $f32x dh = vupper_vf_vf(d), dl = d - dh;
  $f32x th = vupper_vf_vf(t), tl = t - th;
  VFloat2 q;

  q.0 = t;

  $f32x u = $f32x::splat(-1);
  u = dh.mla(th, u);
  u = dh.mla(tl, u);
  u = dl.mla(th, u);
  u = dl.mla(tl, u);
  q.1 = (-t) * u;

  return q;
}

#[inline]
fn dfrec_vf2_vf2(d: VFloat2) -> VFloat2 {
  $f32x t = vrec_vf_vf(d.0);
  $f32x dh = vupper_vf_vf(d.0), dl = d.0 - dh;
  $f32x th = vupper_vf_vf(t  ), tl = t   - th;
  VFloat2 q;

  q.0 = t;

  $f32x u = $f32x::splat(-1);
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
  $f32x x = vrecsqrt_vf_vf(d.0 + d.1);
  VFloat2 r = dfmul_vf2_vf2_vf(d, x);
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(r, dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(r, x), $f32x::splat(-3.0))), $f32x::splat(-0.5));
#else
  $f32x t = vsqrt_vf_vf(d.0 + d.1);
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf2(d, dfmul_vf2_vf_vf(t, t)), dfrec_vf2_vf(t)), $f32x::splat(0.5));
#endif
}

#[inline]
fn dfsqrt_vf2_vf(d: $f32x) -> VFloat2 {
  $f32x t = vsqrt_vf_vf(d);
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf_vf2(d, dfmul_vf2_vf_vf(t, t)), dfrec_vf2_vf(t)), $f32x::splat(0.5));
}
