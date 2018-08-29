//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#if defined(ENABLE_SVE) || defined(ENABLE_SVENOFMA)
typedef __sizeless_struct VDouble2 {
  sVFloat64_t x;
  sVFloat64_t y;
} VDouble2;
#else
typedef struct {
  VDouble x, y;
} VDouble2;
#endif

static INLINE CONST VDouble vupper_vd_vd(VDouble d) {
  return vreinterpret_vd_vm(vand_vm_vm_vm(vreinterpret_vm_vd(d), vcast_vm_i_i(0xffffffff, 0xf8000000)));
}

static INLINE CONST VDouble2 vcast_vd2_vd_vd(VDouble h, VDouble l) {
  VDouble2 ret = {h, l};
  return ret;
}

static INLINE CONST VDouble2 vcast_vd2_d_d(double h, double l) {
  VDouble2 ret = {vcast_vd_d(h), vcast_vd_d(l)};
  return ret;
}

static INLINE CONST VDouble2 vsel_vd2_vo_vd2_vd2(vopmask m, VDouble2 x, VDouble2 y) {
  VDouble2 r;
  r.0 = vsel_vd_vo_vd_vd(m, x.0, y.0);
  r.1 = vsel_vd_vo_vd_vd(m, x.1, y.1);
  return r;
}

static INLINE CONST VDouble2 vsel_vd2_vo_d_d_d_d(vopmask o, double x1, double y1, double x0, double y0) {
  VDouble2 r;
  r.0 = vsel_vd_vo_d_d(o, x1, x0);
  r.1 = vsel_vd_vo_d_d(o, y1, y0);
  return r;
}

static INLINE CONST VDouble vadd_vd_3vd(VDouble v0, VDouble v1, VDouble v2) {
  return vadd_vd_vd_vd(vadd_vd_vd_vd(v0, v1), v2);
}

static INLINE CONST VDouble vadd_vd_4vd(VDouble v0, VDouble v1, VDouble v2, VDouble v3) {
  return vadd_vd_3vd(vadd_vd_vd_vd(v0, v1), v2, v3);
}

static INLINE CONST VDouble vadd_vd_5vd(VDouble v0, VDouble v1, VDouble v2, VDouble v3, VDouble v4) {
  return vadd_vd_4vd(vadd_vd_vd_vd(v0, v1), v2, v3, v4);
}

static INLINE CONST VDouble vadd_vd_6vd(VDouble v0, VDouble v1, VDouble v2, VDouble v3, VDouble v4, VDouble v5) {
  return vadd_vd_5vd(vadd_vd_vd_vd(v0, v1), v2, v3, v4, v5);
}

static INLINE CONST VDouble vadd_vd_7vd(VDouble v0, VDouble v1, VDouble v2, VDouble v3, VDouble v4, VDouble v5, VDouble v6) {
  return vadd_vd_6vd(vadd_vd_vd_vd(v0, v1), v2, v3, v4, v5, v6);
}

static INLINE CONST VDouble vsub_vd_3vd(VDouble v0, VDouble v1, VDouble v2) {
  return vsub_vd_vd_vd(vsub_vd_vd_vd(v0, v1), v2);
}

static INLINE CONST VDouble vsub_vd_4vd(VDouble v0, VDouble v1, VDouble v2, VDouble v3) {
  return vsub_vd_3vd(vsub_vd_vd_vd(v0, v1), v2, v3);
}

static INLINE CONST VDouble vsub_vd_5vd(VDouble v0, VDouble v1, VDouble v2, VDouble v3, VDouble v4) {
  return vsub_vd_4vd(vsub_vd_vd_vd(v0, v1), v2, v3, v4);
}

static INLINE CONST VDouble vsub_vd_6vd(VDouble v0, VDouble v1, VDouble v2, VDouble v3, VDouble v4, VDouble v5) {
  return vsub_vd_5vd(vsub_vd_vd_vd(v0, v1), v2, v3, v4, v5);
}

//

static INLINE CONST VDouble2 ddneg_vd2_vd2(VDouble2 x) {
  return vcast_vd2_vd_vd(vneg_vd_vd(x.0), vneg_vd_vd(x.1));
}

static INLINE CONST VDouble2 ddabs_vd2_vd2(VDouble2 x) {
  return vcast_vd2_vd_vd(vabs_vd_vd(x.0),
			 vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(x.1), vand_vm_vm_vm(vreinterpret_vm_vd(x.0), vreinterpret_vm_vd(vcast_vd_d(-0.0))))));
}

static INLINE CONST VDouble2 ddnormalize_vd2_vd2(VDouble2 t) {
  VDouble2 s;

  s.0 = vadd_vd_vd_vd(t.0, t.1);
  s.1 = vadd_vd_vd_vd(vsub_vd_vd_vd(t.0, s.0), t.1);

  return s;
}

static INLINE CONST VDouble2 ddscale_vd2_vd2_vd(VDouble2 d, VDouble s) {
  VDouble2 r = {vmul_vd_vd_vd(d.0, s), vmul_vd_vd_vd(d.1, s)};
  return r;
}

static INLINE CONST VDouble2 ddadd_vd2_vd_vd(VDouble x, VDouble y) {
  VDouble2 r;

  r.0 = vadd_vd_vd_vd(x, y);
  r.1 = vadd_vd_vd_vd(vsub_vd_vd_vd(x, r.0), y);

  return r;
}

static INLINE CONST VDouble2 ddadd2_vd2_vd_vd(VDouble x, VDouble y) {
  VDouble2 r;

  r.0 = vadd_vd_vd_vd(x, y);
  VDouble v = vsub_vd_vd_vd(r.0, x);
  r.1 = vadd_vd_vd_vd(vsub_vd_vd_vd(x, vsub_vd_vd_vd(r.0, v)), vsub_vd_vd_vd(y, v));

  return r;
}

static INLINE CONST VDouble2 ddadd_vd2_vd2_vd(VDouble2 x, VDouble y) {
  VDouble2 r;

  r.0 = vadd_vd_vd_vd(x.0, y);
  r.1 = vadd_vd_3vd(vsub_vd_vd_vd(x.0, r.0), y, x.1);

  return r;
}

static INLINE CONST VDouble2 ddsub_vd2_vd2_vd(VDouble2 x, VDouble y) {
  VDouble2 r;

  r.0 = vsub_vd_vd_vd(x.0, y);
  r.1 = vadd_vd_vd_vd(vsub_vd_vd_vd(vsub_vd_vd_vd(x.0, r.0), y), x.1);

  return r;
}

static INLINE CONST VDouble2 ddadd2_vd2_vd2_vd(VDouble2 x, VDouble y) {
  VDouble2 r;

  r.0 = vadd_vd_vd_vd(x.0, y);
  VDouble v = vsub_vd_vd_vd(r.0, x.0);
  r.1 = vadd_vd_vd_vd(vsub_vd_vd_vd(x.0, vsub_vd_vd_vd(r.0, v)), vsub_vd_vd_vd(y, v));
  r.1 = vadd_vd_vd_vd(r.1, x.1);

  return r;
}

static INLINE CONST VDouble2 ddadd_vd2_vd_vd2(VDouble x, VDouble2 y) {
  VDouble2 r;

  r.0 = vadd_vd_vd_vd(x, y.0);
  r.1 = vadd_vd_3vd(vsub_vd_vd_vd(x, r.0), y.0, y.1);

  return r;
}

static INLINE CONST VDouble2 ddadd2_vd2_vd_vd2(VDouble x, VDouble2 y) {
  VDouble2 r;

  r.0  = vadd_vd_vd_vd(x, y.0);
  VDouble v = vsub_vd_vd_vd(r.0, x);
  r.1 = vadd_vd_vd_vd(vadd_vd_vd_vd(vsub_vd_vd_vd(x, vsub_vd_vd_vd(r.0, v)), vsub_vd_vd_vd(y.0, v)), y.1);

  return r;
}

static INLINE CONST VDouble2 ddadd_vd2_vd2_vd2(VDouble2 x, VDouble2 y) {
  // |x| >= |y|

  VDouble2 r;

  r.0 = vadd_vd_vd_vd(x.0, y.0);
  r.1 = vadd_vd_4vd(vsub_vd_vd_vd(x.0, r.0), y.0, x.1, y.1);

  return r;
}

static INLINE CONST VDouble2 ddadd2_vd2_vd2_vd2(VDouble2 x, VDouble2 y) {
  VDouble2 r;

  r.0  = vadd_vd_vd_vd(x.0, y.0);
  VDouble v = vsub_vd_vd_vd(r.0, x.0);
  r.1 = vadd_vd_vd_vd(vsub_vd_vd_vd(x.0, vsub_vd_vd_vd(r.0, v)), vsub_vd_vd_vd(y.0, v));
  r.1 = vadd_vd_vd_vd(r.1, vadd_vd_vd_vd(x.1, y.1));

  return r;
}

static INLINE CONST VDouble2 ddsub_vd2_vd_vd(VDouble x, VDouble y) {
  // |x| >= |y|

  VDouble2 r;

  r.0 = vsub_vd_vd_vd(x, y);
  r.1 = vsub_vd_vd_vd(vsub_vd_vd_vd(x, r.0), y);

  return r;
}

static INLINE CONST VDouble2 ddsub_vd2_vd2_vd2(VDouble2 x, VDouble2 y) {
  // |x| >= |y|

  VDouble2 r;

  r.0 = vsub_vd_vd_vd(x.0, y.0);
  r.1 = vsub_vd_vd_vd(x.0, r.0);
  r.1 = vsub_vd_vd_vd(r.1, y.0);
  r.1 = vadd_vd_vd_vd(r.1, x.1);
  r.1 = vsub_vd_vd_vd(r.1, y.1);

  return r;
}

#ifdef ENABLE_FMA_DP
static INLINE CONST VDouble2 dddiv_vd2_vd2_vd2(VDouble2 n, VDouble2 d) {
  VDouble2 q;
  VDouble t = vrec_vd_vd(d.0), u;

  q.0 = vmul_vd_vd_vd(n.0, t);
  u = vfmapn_vd_vd_vd_vd(t, n.0, q.0);
  q.1 = vfmanp_vd_vd_vd_vd(d.1, t, vfmanp_vd_vd_vd_vd(d.0, t, vcast_vd_d(1)));
  q.1 = vfma_vd_vd_vd_vd(q.0, q.1, vfma_vd_vd_vd_vd(n.1, t, u));

  return q;
}

static INLINE CONST VDouble2 ddmul_vd2_vd_vd(VDouble x, VDouble y) {
  VDouble2 r;

  r.0 = vmul_vd_vd_vd(x, y);
  r.1 = vfmapn_vd_vd_vd_vd(x, y, r.0);

  return r;
}

static INLINE CONST VDouble2 ddsqu_vd2_vd2(VDouble2 x) {
  VDouble2 r;

  r.0 = vmul_vd_vd_vd(x.0, x.0);
  r.1 = vfma_vd_vd_vd_vd(vadd_vd_vd_vd(x.0, x.0), x.1, vfmapn_vd_vd_vd_vd(x.0, x.0, r.0));

  return r;
}

static INLINE CONST VDouble2 ddmul_vd2_vd2_vd2(VDouble2 x, VDouble2 y) {
  VDouble2 r;

  r.0 = vmul_vd_vd_vd(x.0, y.0);
  r.1 = vfma_vd_vd_vd_vd(x.0, y.1, vfma_vd_vd_vd_vd(x.1, y.0, vfmapn_vd_vd_vd_vd(x.0, y.0, r.0)));

  return r;
}

static INLINE CONST VDouble ddmul_vd_vd2_vd2(VDouble2 x, VDouble2 y) {
  return vfma_vd_vd_vd_vd(x.0, y.0, vfma_vd_vd_vd_vd(x.1, y.0, vmul_vd_vd_vd(x.0, y.1)));
}

static INLINE CONST VDouble ddsqu_vd_vd2(VDouble2 x) {
  return vfma_vd_vd_vd_vd(x.0, x.0, vadd_vd_vd_vd(vmul_vd_vd_vd(x.0, x.1), vmul_vd_vd_vd(x.0, x.1)));
}

static INLINE CONST VDouble2 ddmul_vd2_vd2_vd(VDouble2 x, VDouble y) {
  VDouble2 r;

  r.0 = vmul_vd_vd_vd(x.0, y);
  r.1 = vfma_vd_vd_vd_vd(x.1, y, vfmapn_vd_vd_vd_vd(x.0, y, r.0));

  return r;
}

static INLINE CONST VDouble2 ddrec_vd2_vd(VDouble d) {
  VDouble2 q;

  q.0 = vrec_vd_vd(d);
  q.1 = vmul_vd_vd_vd(q.0, vfmanp_vd_vd_vd_vd(d, q.0, vcast_vd_d(1)));

  return q;
}

static INLINE CONST VDouble2 ddrec_vd2_vd2(VDouble2 d) {
  VDouble2 q;

  q.0 = vrec_vd_vd(d.0);
  q.1 = vmul_vd_vd_vd(q.0, vfmanp_vd_vd_vd_vd(d.1, q.0, vfmanp_vd_vd_vd_vd(d.0, q.0, vcast_vd_d(1))));

  return q;
}
#else
static INLINE CONST VDouble2 dddiv_vd2_vd2_vd2(VDouble2 n, VDouble2 d) {
  VDouble t = vrec_vd_vd(d.0);
  VDouble dh  = vupper_vd_vd(d.0), dl  = vsub_vd_vd_vd(d.0,  dh);
  VDouble th  = vupper_vd_vd(t  ), tl  = vsub_vd_vd_vd(t  ,  th);
  VDouble nhh = vupper_vd_vd(n.0), nhl = vsub_vd_vd_vd(n.0, nhh);

  VDouble2 q;

  q.0 = vmul_vd_vd_vd(n.0, t);

  VDouble u = vadd_vd_5vd(vsub_vd_vd_vd(vmul_vd_vd_vd(nhh, th), q.0), vmul_vd_vd_vd(nhh, tl), vmul_vd_vd_vd(nhl, th), vmul_vd_vd_vd(nhl, tl),
		    vmul_vd_vd_vd(q.0, vsub_vd_5vd(vcast_vd_d(1), vmul_vd_vd_vd(dh, th), vmul_vd_vd_vd(dh, tl), vmul_vd_vd_vd(dl, th), vmul_vd_vd_vd(dl, tl))));

  q.1 = vmla_vd_vd_vd_vd(t, vsub_vd_vd_vd(n.1, vmul_vd_vd_vd(q.0, d.1)), u);

  return q;
}

static INLINE CONST VDouble2 ddmul_vd2_vd_vd(VDouble x, VDouble y) {
  VDouble xh = vupper_vd_vd(x), xl = vsub_vd_vd_vd(x, xh);
  VDouble yh = vupper_vd_vd(y), yl = vsub_vd_vd_vd(y, yh);
  VDouble2 r;

  r.0 = vmul_vd_vd_vd(x, y);
  r.1 = vadd_vd_5vd(vmul_vd_vd_vd(xh, yh), vneg_vd_vd(r.0), vmul_vd_vd_vd(xl, yh), vmul_vd_vd_vd(xh, yl), vmul_vd_vd_vd(xl, yl));

  return r;
}

static INLINE CONST VDouble2 ddmul_vd2_vd2_vd(VDouble2 x, VDouble y) {
  VDouble xh = vupper_vd_vd(x.0), xl = vsub_vd_vd_vd(x.0, xh);
  VDouble yh = vupper_vd_vd(y  ), yl = vsub_vd_vd_vd(y  , yh);
  VDouble2 r;

  r.0 = vmul_vd_vd_vd(x.0, y);
  r.1 = vadd_vd_6vd(vmul_vd_vd_vd(xh, yh), vneg_vd_vd(r.0), vmul_vd_vd_vd(xl, yh), vmul_vd_vd_vd(xh, yl), vmul_vd_vd_vd(xl, yl), vmul_vd_vd_vd(x.1, y));

  return r;
}

static INLINE CONST VDouble2 ddmul_vd2_vd2_vd2(VDouble2 x, VDouble2 y) {
  VDouble xh = vupper_vd_vd(x.0), xl = vsub_vd_vd_vd(x.0, xh);
  VDouble yh = vupper_vd_vd(y.0), yl = vsub_vd_vd_vd(y.0, yh);
  VDouble2 r;

  r.0 = vmul_vd_vd_vd(x.0, y.0);
  r.1 = vadd_vd_7vd(vmul_vd_vd_vd(xh, yh), vneg_vd_vd(r.0), vmul_vd_vd_vd(xl, yh), vmul_vd_vd_vd(xh, yl), vmul_vd_vd_vd(xl, yl), vmul_vd_vd_vd(x.0, y.1), vmul_vd_vd_vd(x.1, y.0));

  return r;
}

static INLINE CONST VDouble ddmul_vd_vd2_vd2(VDouble2 x, VDouble2 y) {
  VDouble xh = vupper_vd_vd(x.0), xl = vsub_vd_vd_vd(x.0, xh);
  VDouble yh = vupper_vd_vd(y.0), yl = vsub_vd_vd_vd(y.0, yh);

  return vadd_vd_6vd(vmul_vd_vd_vd(x.1, yh), vmul_vd_vd_vd(xh, y.1), vmul_vd_vd_vd(xl, yl), vmul_vd_vd_vd(xh, yl), vmul_vd_vd_vd(xl, yh), vmul_vd_vd_vd(xh, yh));
}

static INLINE CONST VDouble2 ddsqu_vd2_vd2(VDouble2 x) {
  VDouble xh = vupper_vd_vd(x.0), xl = vsub_vd_vd_vd(x.0, xh);
  VDouble2 r;

  r.0 = vmul_vd_vd_vd(x.0, x.0);
  r.1 = vadd_vd_5vd(vmul_vd_vd_vd(xh, xh), vneg_vd_vd(r.0), vmul_vd_vd_vd(vadd_vd_vd_vd(xh, xh), xl), vmul_vd_vd_vd(xl, xl), vmul_vd_vd_vd(x.0, vadd_vd_vd_vd(x.1, x.1)));

  return r;
}

static INLINE CONST VDouble ddsqu_vd_vd2(VDouble2 x) {
  VDouble xh = vupper_vd_vd(x.0), xl = vsub_vd_vd_vd(x.0, xh);

  return vadd_vd_5vd(vmul_vd_vd_vd(xh, x.1), vmul_vd_vd_vd(xh, x.1), vmul_vd_vd_vd(xl, xl), vadd_vd_vd_vd(vmul_vd_vd_vd(xh, xl), vmul_vd_vd_vd(xh, xl)), vmul_vd_vd_vd(xh, xh));
}

static INLINE CONST VDouble2 ddrec_vd2_vd(VDouble d) {
  VDouble t = vrec_vd_vd(d);
  VDouble dh = vupper_vd_vd(d), dl = vsub_vd_vd_vd(d, dh);
  VDouble th = vupper_vd_vd(t), tl = vsub_vd_vd_vd(t, th);
  VDouble2 q;

  q.0 = t;
  q.1 = vmul_vd_vd_vd(t, vsub_vd_5vd(vcast_vd_d(1), vmul_vd_vd_vd(dh, th), vmul_vd_vd_vd(dh, tl), vmul_vd_vd_vd(dl, th), vmul_vd_vd_vd(dl, tl)));

  return q;
}

static INLINE CONST VDouble2 ddrec_vd2_vd2(VDouble2 d) {
  VDouble t = vrec_vd_vd(d.0);
  VDouble dh = vupper_vd_vd(d.0), dl = vsub_vd_vd_vd(d.0, dh);
  VDouble th = vupper_vd_vd(t  ), tl = vsub_vd_vd_vd(t  , th);
  VDouble2 q;

  q.0 = t;
  q.1 = vmul_vd_vd_vd(t, vsub_vd_6vd(vcast_vd_d(1), vmul_vd_vd_vd(dh, th), vmul_vd_vd_vd(dh, tl), vmul_vd_vd_vd(dl, th), vmul_vd_vd_vd(dl, tl), vmul_vd_vd_vd(d.1, t)));

  return q;
}
#endif

static INLINE CONST VDouble2 ddsqrt_vd2_vd2(VDouble2 d) {
  VDouble t = vsqrt_vd_vd(vadd_vd_vd_vd(d.0, d.1));
  return ddscale_vd2_vd2_vd(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd2_vd2(d, ddmul_vd2_vd_vd(t, t)), ddrec_vd2_vd(t)), vcast_vd_d(0.5));
}

static INLINE CONST VDouble2 ddsqrt_vd2_vd(VDouble d) {
  VDouble t = vsqrt_vd_vd(d);
  return ddscale_vd2_vd2_vd(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd_vd2(d, ddmul_vd2_vd_vd(t, t)), ddrec_vd2_vd(t)), vcast_vd_d(0.5));
}
