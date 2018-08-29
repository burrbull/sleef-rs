//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#if defined(ENABLE_SVE) || defined(ENABLE_SVENOFMA)
typedef __sizeless_struct VFloat2 {
  svfoat32_t x;
  svfloat32_t y;
} VFloat2;
#else
typedef struct {
  VFloat x, y;
} VFloat2;
#endif
static INLINE CONST VFloat vupper_vf_vf(VFloat d) {
  return vreinterpret_vf_vi2(vand_vi2_vi2_vi2(vreinterpret_vi2_vf(d), vcast_vi2_i(0xfffff000)));
}

static INLINE CONST VFloat2 vcast_vf2_vf_vf(VFloat h, VFloat l) {
  VFloat2 ret = {h, l};
  return ret;
}

static INLINE CONST VFloat2 vcast_vf2_f_f(float h, float l) {
  VFloat2 ret = {vcast_vf_f(h), vcast_vf_f(l)};
  return ret;
}

static INLINE CONST VFloat2 vcast_vf2_d(double d) {
  VFloat2 ret = {vcast_vf_f(d), vcast_vf_f(d - (float)d)};
  return ret;
}

static INLINE CONST VFloat2 vsel_vf2_vo_vf2_vf2(vopmask m, VFloat2 x, VFloat2 y) {
  VFloat2 r;
  r.0 = vsel_vf_vo_vf_vf(m, x.0, y.0);
  r.1 = vsel_vf_vo_vf_vf(m, x.1, y.1);
  return r;
}

static INLINE CONST VFloat2 vsel_vf2_vo_f_f_f_f(vopmask o, float x1, float y1, float x0, float y0) {
  VFloat2 r;
  r.0 = vsel_vf_vo_f_f(o, x1, x0);
  r.1 = vsel_vf_vo_f_f(o, y1, y0);
  return r;
}

static INLINE CONST VFloat2 vsel_vf2_vo_vo_d_d_d(vopmask o0, vopmask o1, double d0, double d1, double d2) {
  return vsel_vf2_vo_vf2_vf2(o0, vcast_vf2_d(d0), vsel_vf2_vo_vf2_vf2(o1, vcast_vf2_d(d1), vcast_vf2_d(d2)));
}

static INLINE CONST VFloat2 vsel_vf2_vo_vo_vo_d_d_d_d(vopmask o0, vopmask o1, vopmask o2, double d0, double d1, double d2, double d3) {
  return vsel_vf2_vo_vf2_vf2(o0, vcast_vf2_d(d0), vsel_vf2_vo_vf2_vf2(o1, vcast_vf2_d(d1), vsel_vf2_vo_vf2_vf2(o2, vcast_vf2_d(d2), vcast_vf2_d(d3))));
}

static INLINE CONST VFloat2 vabs_vf2_vf2(VFloat2 x) {
  return vcast_vf2_vf_vf(vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vf(vcast_vf_f(-0.0)), vreinterpret_vm_vf(x.0)), vreinterpret_vm_vf(x.0))),
			 vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vf(vcast_vf_f(-0.0)), vreinterpret_vm_vf(x.0)), vreinterpret_vm_vf(x.1))));
}

static INLINE CONST VFloat vadd_vf_3vf(VFloat v0, VFloat v1, VFloat v2) {
  return vadd_vf_vf_vf(vadd_vf_vf_vf(v0, v1), v2);
}

static INLINE CONST VFloat vadd_vf_4vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3) {
  return vadd_vf_3vf(vadd_vf_vf_vf(v0, v1), v2, v3);
}

static INLINE CONST VFloat vadd_vf_5vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3, VFloat v4) {
  return vadd_vf_4vf(vadd_vf_vf_vf(v0, v1), v2, v3, v4);
}

static INLINE CONST VFloat vadd_vf_6vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3, VFloat v4, VFloat v5) {
  return vadd_vf_5vf(vadd_vf_vf_vf(v0, v1), v2, v3, v4, v5);
}

static INLINE CONST VFloat vadd_vf_7vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3, VFloat v4, VFloat v5, VFloat v6) {
  return vadd_vf_6vf(vadd_vf_vf_vf(v0, v1), v2, v3, v4, v5, v6);
}

static INLINE CONST VFloat vsub_vf_3vf(VFloat v0, VFloat v1, VFloat v2) {
  return vsub_vf_vf_vf(vsub_vf_vf_vf(v0, v1), v2);
}

static INLINE CONST VFloat vsub_vf_4vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3) {
  return vsub_vf_3vf(vsub_vf_vf_vf(v0, v1), v2, v3);
}

static INLINE CONST VFloat vsub_vf_5vf(VFloat v0, VFloat v1, VFloat v2, VFloat v3, VFloat v4) {
  return vsub_vf_4vf(vsub_vf_vf_vf(v0, v1), v2, v3, v4);
}

//

static INLINE CONST VFloat2 dfneg_vf2_vf2(VFloat2 x) {
  return vcast_vf2_vf_vf(vneg_vf_vf(x.0), vneg_vf_vf(x.1));
}

static INLINE CONST VFloat2 dfabs_vf2_vf2(VFloat2 x) {
  return vcast_vf2_vf_vf(vabs_vf_vf(x.0),
			 vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x.1), vand_vm_vm_vm(vreinterpret_vm_vf(x.0), vreinterpret_vm_vf(vcast_vf_f(-0.0f))))));
}

static INLINE CONST VFloat2 dfnormalize_vf2_vf2(VFloat2 t) {
  VFloat2 s;

  s.0 = vadd_vf_vf_vf(t.0, t.1);
  s.1 = vadd_vf_vf_vf(vsub_vf_vf_vf(t.0, s.0), t.1);

  return s;
}

static INLINE CONST VFloat2 dfscale_vf2_vf2_vf(VFloat2 d, VFloat s) {
  VFloat2 r = {vmul_vf_vf_vf(d.0, s), vmul_vf_vf_vf(d.1, s)};
  return r;
}

static INLINE CONST VFloat2 dfadd_vf2_vf_vf(VFloat x, VFloat y) {
  VFloat2 r;

  r.0 = vadd_vf_vf_vf(x, y);
  r.1 = vadd_vf_vf_vf(vsub_vf_vf_vf(x, r.0), y);

  return r;
}

static INLINE CONST VFloat2 dfadd2_vf2_vf_vf(VFloat x, VFloat y) {
  VFloat2 r;

  r.0 = vadd_vf_vf_vf(x, y);
  VFloat v = vsub_vf_vf_vf(r.0, x);
  r.1 = vadd_vf_vf_vf(vsub_vf_vf_vf(x, vsub_vf_vf_vf(r.0, v)), vsub_vf_vf_vf(y, v));

  return r;
}

static INLINE CONST VFloat2 dfadd2_vf2_vf_vf2(VFloat x, VFloat2 y) {
  VFloat2 r;

  r.0  = vadd_vf_vf_vf(x, y.0);
  VFloat v = vsub_vf_vf_vf(r.0, x);
  r.1 = vadd_vf_vf_vf(vadd_vf_vf_vf(vsub_vf_vf_vf(x, vsub_vf_vf_vf(r.0, v)), vsub_vf_vf_vf(y.0, v)), y.1);

  return r;
}

static INLINE CONST VFloat2 dfadd_vf2_vf2_vf(VFloat2 x, VFloat y) {
  VFloat2 r;

  r.0 = vadd_vf_vf_vf(x.0, y);
  r.1 = vadd_vf_3vf(vsub_vf_vf_vf(x.0, r.0), y, x.1);

  return r;
}

static INLINE CONST VFloat2 dfsub_vf2_vf2_vf(VFloat2 x, VFloat y) {
  VFloat2 r;

  r.0 = vsub_vf_vf_vf(x.0, y);
  r.1 = vadd_vf_vf_vf(vsub_vf_vf_vf(vsub_vf_vf_vf(x.0, r.0), y), x.1);

  return r;
}

static INLINE CONST VFloat2 dfadd2_vf2_vf2_vf(VFloat2 x, VFloat y) {
  VFloat2 r;

  r.0 = vadd_vf_vf_vf(x.0, y);
  VFloat v = vsub_vf_vf_vf(r.0, x.0);
  r.1 = vadd_vf_vf_vf(vsub_vf_vf_vf(x.0, vsub_vf_vf_vf(r.0, v)), vsub_vf_vf_vf(y, v));
  r.1 = vadd_vf_vf_vf(r.1, x.1);

  return r;
}

static INLINE CONST VFloat2 dfadd_vf2_vf_vf2(VFloat x, VFloat2 y) {
  VFloat2 r;

  r.0 = vadd_vf_vf_vf(x, y.0);
  r.1 = vadd_vf_3vf(vsub_vf_vf_vf(x, r.0), y.0, y.1);

  return r;
}

static INLINE CONST VFloat2 dfadd_vf2_vf2_vf2(VFloat2 x, VFloat2 y) {
  // |x| >= |y|

  VFloat2 r;

  r.0 = vadd_vf_vf_vf(x.0, y.0);
  r.1 = vadd_vf_4vf(vsub_vf_vf_vf(x.0, r.0), y.0, x.1, y.1);

  return r;
}

static INLINE CONST VFloat2 dfadd2_vf2_vf2_vf2(VFloat2 x, VFloat2 y) {
  VFloat2 r;

  r.0  = vadd_vf_vf_vf(x.0, y.0);
  VFloat v = vsub_vf_vf_vf(r.0, x.0);
  r.1 = vadd_vf_vf_vf(vsub_vf_vf_vf(x.0, vsub_vf_vf_vf(r.0, v)), vsub_vf_vf_vf(y.0, v));
  r.1 = vadd_vf_vf_vf(r.1, vadd_vf_vf_vf(x.1, y.1));

  return r;
}

static INLINE CONST VFloat2 dfsub_vf2_vf_vf(VFloat x, VFloat y) {
  // |x| >= |y|

  VFloat2 r;

  r.0 = vsub_vf_vf_vf(x, y);
  r.1 = vsub_vf_vf_vf(vsub_vf_vf_vf(x, r.0), y);

  return r;
}

static INLINE CONST VFloat2 dfsub_vf2_vf2_vf2(VFloat2 x, VFloat2 y) {
  // |x| >= |y|

  VFloat2 r;

  r.0 = vsub_vf_vf_vf(x.0, y.0);
  r.1 = vsub_vf_vf_vf(x.0, r.0);
  r.1 = vsub_vf_vf_vf(r.1, y.0);
  r.1 = vadd_vf_vf_vf(r.1, x.1);
  r.1 = vsub_vf_vf_vf(r.1, y.1);

  return r;
}

#ifdef ENABLE_FMA_SP
static INLINE CONST VFloat2 dfdiv_vf2_vf2_vf2(VFloat2 n, VFloat2 d) {
  VFloat2 q;
  VFloat t = vrec_vf_vf(d.0), u;

  q.0 = vmul_vf_vf_vf(n.0, t);
  u = vfmapn_vf_vf_vf_vf(t, n.0, q.0);
  q.1 = vfmanp_vf_vf_vf_vf(d.1, t, vfmanp_vf_vf_vf_vf(d.0, t, vcast_vf_f(1)));
  q.1 = vfma_vf_vf_vf_vf(q.0, q.1, vfma_vf_vf_vf_vf(n.1, t, u));

  return q;
}

static INLINE CONST VFloat2 dfmul_vf2_vf_vf(VFloat x, VFloat y) {
  VFloat2 r;

  r.0 = vmul_vf_vf_vf(x, y);
  r.1 = vfmapn_vf_vf_vf_vf(x, y, r.0);

  return r;
}

static INLINE CONST VFloat2 dfsqu_vf2_vf2(VFloat2 x) {
  VFloat2 r;

  r.0 = vmul_vf_vf_vf(x.0, x.0);
  r.1 = vfma_vf_vf_vf_vf(vadd_vf_vf_vf(x.0, x.0), x.1, vfmapn_vf_vf_vf_vf(x.0, x.0, r.0));

  return r;
}

static INLINE CONST VFloat dfsqu_vf_vf2(VFloat2 x) {
  return vfma_vf_vf_vf_vf(x.0, x.0, vadd_vf_vf_vf(vmul_vf_vf_vf(x.0, x.1), vmul_vf_vf_vf(x.0, x.1)));
}

static INLINE CONST VFloat2 dfmul_vf2_vf2_vf2(VFloat2 x, VFloat2 y) {
  VFloat2 r;

  r.0 = vmul_vf_vf_vf(x.0, y.0);
  r.1 = vfma_vf_vf_vf_vf(x.0, y.1, vfma_vf_vf_vf_vf(x.1, y.0, vfmapn_vf_vf_vf_vf(x.0, y.0, r.0)));

  return r;
}

static INLINE CONST VFloat dfmul_vf_vf2_vf2(VFloat2 x, VFloat2 y) {
  return vfma_vf_vf_vf_vf(x.0, y.0, vfma_vf_vf_vf_vf(x.1, y.0, vmul_vf_vf_vf(x.0, y.1)));
}

static INLINE CONST VFloat2 dfmul_vf2_vf2_vf(VFloat2 x, VFloat y) {
  VFloat2 r;

  r.0 = vmul_vf_vf_vf(x.0, y);
  r.1 = vfma_vf_vf_vf_vf(x.1, y, vfmapn_vf_vf_vf_vf(x.0, y, r.0));

  return r;
}

static INLINE CONST VFloat2 dfrec_vf2_vf(VFloat d) {
  VFloat2 q;

  q.0 = vrec_vf_vf(d);
  q.1 = vmul_vf_vf_vf(q.0, vfmanp_vf_vf_vf_vf(d, q.0, vcast_vf_f(1)));

  return q;
}

static INLINE CONST VFloat2 dfrec_vf2_vf2(VFloat2 d) {
  VFloat2 q;

  q.0 = vrec_vf_vf(d.0);
  q.1 = vmul_vf_vf_vf(q.0, vfmanp_vf_vf_vf_vf(d.1, q.0, vfmanp_vf_vf_vf_vf(d.0, q.0, vcast_vf_f(1))));

  return q;
}
#else
static INLINE CONST VFloat2 dfdiv_vf2_vf2_vf2(VFloat2 n, VFloat2 d) {
  VFloat t = vrec_vf_vf(d.0);
  VFloat dh  = vupper_vf_vf(d.0), dl  = vsub_vf_vf_vf(d.0,  dh);
  VFloat th  = vupper_vf_vf(t  ), tl  = vsub_vf_vf_vf(t  ,  th);
  VFloat nhh = vupper_vf_vf(n.0), nhl = vsub_vf_vf_vf(n.0, nhh);

  VFloat2 q;

  q.0 = vmul_vf_vf_vf(n.0, t);

  VFloat u, w;
  w = vcast_vf_f(-1);
  w = vmla_vf_vf_vf_vf(dh, th, w);
  w = vmla_vf_vf_vf_vf(dh, tl, w);
  w = vmla_vf_vf_vf_vf(dl, th, w);
  w = vmla_vf_vf_vf_vf(dl, tl, w);
  w = vneg_vf_vf(w);

  u = vmla_vf_vf_vf_vf(nhh, th, vneg_vf_vf(q.0));
  u = vmla_vf_vf_vf_vf(nhh, tl, u);
  u = vmla_vf_vf_vf_vf(nhl, th, u);
  u = vmla_vf_vf_vf_vf(nhl, tl, u);
  u = vmla_vf_vf_vf_vf(q.0, w , u);

  q.1 = vmla_vf_vf_vf_vf(t, vsub_vf_vf_vf(n.1, vmul_vf_vf_vf(q.0, d.1)), u);

  return q;
}

static INLINE CONST VFloat2 dfmul_vf2_vf_vf(VFloat x, VFloat y) {
  VFloat xh = vupper_vf_vf(x), xl = vsub_vf_vf_vf(x, xh);
  VFloat yh = vupper_vf_vf(y), yl = vsub_vf_vf_vf(y, yh);
  VFloat2 r;

  r.0 = vmul_vf_vf_vf(x, y);

  VFloat t;
  t = vmla_vf_vf_vf_vf(xh, yh, vneg_vf_vf(r.0));
  t = vmla_vf_vf_vf_vf(xl, yh, t);
  t = vmla_vf_vf_vf_vf(xh, yl, t);
  t = vmla_vf_vf_vf_vf(xl, yl, t);
  r.1 = t;

  return r;
}

static INLINE CONST VFloat2 dfmul_vf2_vf2_vf(VFloat2 x, VFloat y) {
  VFloat xh = vupper_vf_vf(x.0), xl = vsub_vf_vf_vf(x.0, xh);
  VFloat yh = vupper_vf_vf(y  ), yl = vsub_vf_vf_vf(y  , yh);
  VFloat2 r;

  r.0 = vmul_vf_vf_vf(x.0, y);

  VFloat t;
  t = vmla_vf_vf_vf_vf(xh, yh, vneg_vf_vf(r.0));
  t = vmla_vf_vf_vf_vf(xl, yh, t);
  t = vmla_vf_vf_vf_vf(xh, yl, t);
  t = vmla_vf_vf_vf_vf(xl, yl, t);
  t = vmla_vf_vf_vf_vf(x.1, y, t);
  r.1 = t;

  return r;
}

static INLINE CONST VFloat2 dfmul_vf2_vf2_vf2(VFloat2 x, VFloat2 y) {
  VFloat xh = vupper_vf_vf(x.0), xl = vsub_vf_vf_vf(x.0, xh);
  VFloat yh = vupper_vf_vf(y.0), yl = vsub_vf_vf_vf(y.0, yh);
  VFloat2 r;

  r.0 = vmul_vf_vf_vf(x.0, y.0);

  VFloat t;
  t = vmla_vf_vf_vf_vf(xh, yh, vneg_vf_vf(r.0));
  t = vmla_vf_vf_vf_vf(xl, yh, t);
  t = vmla_vf_vf_vf_vf(xh, yl, t);
  t = vmla_vf_vf_vf_vf(xl, yl, t);
  t = vmla_vf_vf_vf_vf(x.0, y.1, t);
  t = vmla_vf_vf_vf_vf(x.1, y.0, t);
  r.1 = t;

  return r;
}

static INLINE CONST VFloat dfmul_vf_vf2_vf2(VFloat2 x, VFloat2 y) {
  VFloat xh = vupper_vf_vf(x.0), xl = vsub_vf_vf_vf(x.0, xh);
  VFloat yh = vupper_vf_vf(y.0), yl = vsub_vf_vf_vf(y.0, yh);

  return vadd_vf_6vf(vmul_vf_vf_vf(x.1, yh), vmul_vf_vf_vf(xh, y.1), vmul_vf_vf_vf(xl, yl), vmul_vf_vf_vf(xh, yl), vmul_vf_vf_vf(xl, yh), vmul_vf_vf_vf(xh, yh));
}

static INLINE CONST VFloat2 dfsqu_vf2_vf2(VFloat2 x) {
  VFloat xh = vupper_vf_vf(x.0), xl = vsub_vf_vf_vf(x.0, xh);
  VFloat2 r;

  r.0 = vmul_vf_vf_vf(x.0, x.0);

  VFloat t;
  t = vmla_vf_vf_vf_vf(xh, xh, vneg_vf_vf(r.0));
  t = vmla_vf_vf_vf_vf(vadd_vf_vf_vf(xh, xh), xl, t);
  t = vmla_vf_vf_vf_vf(xl, xl, t);
  t = vmla_vf_vf_vf_vf(x.0, vadd_vf_vf_vf(x.1, x.1), t);
  r.1 = t;

  return r;
}

static INLINE CONST VFloat dfsqu_vf_vf2(VFloat2 x) {
  VFloat xh = vupper_vf_vf(x.0), xl = vsub_vf_vf_vf(x.0, xh);

  return vadd_vf_5vf(vmul_vf_vf_vf(xh, x.1), vmul_vf_vf_vf(xh, x.1), vmul_vf_vf_vf(xl, xl), vadd_vf_vf_vf(vmul_vf_vf_vf(xh, xl), vmul_vf_vf_vf(xh, xl)), vmul_vf_vf_vf(xh, xh));
}

static INLINE CONST VFloat2 dfrec_vf2_vf(VFloat d) {
  VFloat t = vrec_vf_vf(d);
  VFloat dh = vupper_vf_vf(d), dl = vsub_vf_vf_vf(d, dh);
  VFloat th = vupper_vf_vf(t), tl = vsub_vf_vf_vf(t, th);
  VFloat2 q;

  q.0 = t;

  VFloat u = vcast_vf_f(-1);
  u = vmla_vf_vf_vf_vf(dh, th, u);
  u = vmla_vf_vf_vf_vf(dh, tl, u);
  u = vmla_vf_vf_vf_vf(dl, th, u);
  u = vmla_vf_vf_vf_vf(dl, tl, u);
  q.1 = vmul_vf_vf_vf(vneg_vf_vf(t), u);

  return q;
}

static INLINE CONST VFloat2 dfrec_vf2_vf2(VFloat2 d) {
  VFloat t = vrec_vf_vf(d.0);
  VFloat dh = vupper_vf_vf(d.0), dl = vsub_vf_vf_vf(d.0, dh);
  VFloat th = vupper_vf_vf(t  ), tl = vsub_vf_vf_vf(t  , th);
  VFloat2 q;

  q.0 = t;

  VFloat u = vcast_vf_f(-1);
  u = vmla_vf_vf_vf_vf(dh, th, u);
  u = vmla_vf_vf_vf_vf(dh, tl, u);
  u = vmla_vf_vf_vf_vf(dl, th, u);
  u = vmla_vf_vf_vf_vf(dl, tl, u);
  u = vmla_vf_vf_vf_vf(d.1, t, u);
  q.1 = vmul_vf_vf_vf(vneg_vf_vf(t), u);

  return q;
}
#endif

static INLINE CONST VFloat2 dfsqrt_vf2_vf2(VFloat2 d) {
#ifdef ENABLE_RECSQRT_SP
  VFloat x = vrecsqrt_vf_vf(vadd_vf_vf_vf(d.0, d.1));
  VFloat2 r = dfmul_vf2_vf2_vf(d, x);
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(r, dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(r, x), vcast_vf_f(-3.0))), vcast_vf_f(-0.5));
#else
  VFloat t = vsqrt_vf_vf(vadd_vf_vf_vf(d.0, d.1));
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf2(d, dfmul_vf2_vf_vf(t, t)), dfrec_vf2_vf(t)), vcast_vf_f(0.5));
#endif
}

static INLINE CONST VFloat2 dfsqrt_vf2_vf(VFloat d) {
  VFloat t = vsqrt_vf_vf(d);
  return dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf_vf2(d, dfmul_vf2_vf_vf(t, t)), dfrec_vf2_vf(t)), vcast_vf_f(0.5f));
}
