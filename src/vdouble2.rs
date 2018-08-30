//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

/*#if defined(ENABLE_SVE) || defined(ENABLE_SVENOFMA)
typedef __sizeless_struct VDouble2 {
  sVFloat64_t x;
  sVFloat64_t y;
} VDouble2;
#else*/

pub struct VDouble2(VDouble, VDouble);
//#endif

#[inline]
fn vupper_vd_vd(d: VDouble) -> VDouble {
  return vreinterpret_vd_vm(vand_vm_vm_vm(vreinterpret_vm_vd(d), vcast_vm_i_i(0xffffffff, 0xf8000000)));
}

#[inline]
fn vcast_vd2_vd_vd(h: VDouble, l: VDouble) -> VDouble2 {
  VDouble2::new(h, l)
}

#[inline]
fn vcast_vd2_d_d(h: f64, l: f64) -> VDouble2 {
  VDouble2::new(h.as_vd(), l.as_vd())
}

#[inline]
fn vsel_vd2_vo_vd2_vd2(m: VOpMask, x: VDouble2, y: VDouble2) -> VDouble2 {
  VDouble2::new(vsel_vd_vo_vd_vd(m, x.0, y.0), vsel_vd_vo_vd_vd(m, x.1, y.1))
}

#[inline]
fn vsel_vd2_vo_d_d_d_d(o: VOpMask, x1: f64, y1: f64, x0: f64, y0: f64) -> VDouble2 {
  VDouble2::new(vsel_vd_vo_d_d(o, x1, x0), vsel_vd_vo_d_d(o, y1, y0))
}

#[inline]
fn vsub_vd_3vd(v0: VDouble, v1: VDouble, v2: VDouble) -> VDouble {
  return v0 - v1 - v2;
}

#[inline]
fn vsub_vd_4vd(v0: VDouble, v1: VDouble, v2: VDouble, v3: VDouble) -> VDouble {
  return vsub_vd_3vd(v0 - v1, v2, v3);
}

#[inline]
fn vsub_vd_5vd(v0: VDouble, v1: VDouble, v2: VDouble, v3: VDouble, v4: VDouble) -> VDouble {
  return vsub_vd_4vd(v0 - v1, v2, v3, v4);
}

#[inline]
fn vsub_vd_6vd(v0: VDouble, v1: VDouble, v2: VDouble, v3: VDouble, v4: VDouble, v5: VDouble) -> VDouble {
  return vsub_vd_5vd(v0 - v1, v2, v3, v4, v5);
}

//

#[inline]
fn ddneg_vd2_vd2(x: VDouble2) -> VDouble2 {
  return vcast_vd2_vd_vd(-x.0, -x.1);
}

#[inline]
fn ddabs_vd2_vd2(x: VDouble2) -> VDouble2 {
  return vcast_vd2_vd_vd(vabs_vd_vd(x.0),
			 vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(x.1), vand_vm_vm_vm(vreinterpret_vm_vd(x.0), vreinterpret_vm_vd(vcast_vd_d(-0.0))))));
}

#[inline]
fn ddnormalize_vd2_vd2(t: VDouble2) -> VDouble2 {
  VDouble2 s;

  s.0 = t.0 + t.1;
  s.1 = t.0 - s.0 + t.1;

  return s;
}

#[inline]
fn ddscale_vd2_vd2_vd(VDouble2 d, VDouble s) -> VDouble2 {
  VDouble2 r = {d.0 * s, d.1 * s};
  return r;
}

#[inline]
fn ddadd_vd2_vd_vd(VDouble x, VDouble y) -> VDouble2 {
  VDouble2 r;

  r.0 = x + y;
  r.1 = x - r.0 + y;

  return r;
}

#[inline]
fn ddadd2_vd2_vd_vd(VDouble x, VDouble y) -> VDouble2 {
  VDouble2 r;

  r.0 = x + y;
  VDouble v = r.0 - x;
  r.1 = x - (r.0 - v) + (y - v);

  return r;
}

#[inline]
fn ddadd_vd2_vd2_vd(VDouble2 x, VDouble y) -> VDouble2 {
  VDouble2 r;

  r.0 = x.0 + y;
  r.1 = x.0 - r.0 + y + x.1;

  return r;
}

#[inline]
fn ddsub_vd2_vd2_vd(VDouble2 x, VDouble y) -> VDouble2 {
  VDouble2 r;

  r.0 = x.0 - y;
  r.1 = x.0 - r.0 - y + x.1;

  return r;
}

#[inline]
fn ddadd2_vd2_vd2_vd(VDouble2 x, VDouble y) -> VDouble2 {
  VDouble2 r;

  r.0 = x.0 + y;
  VDouble v = r.0 - x.0;
  r.1 = x.0 - (r.0 - v) + (y - v);
  r.1 = r.1 + x.1;

  return r;
}

#[inline]
fn ddadd_vd2_vd_vd2(VDouble x, VDouble2 y) -> VDouble2 {
  VDouble2 r;

  r.0 = x + y.0;
  r.1 = x - r.0 + y.0 + y.1;

  return r;
}

#[inline]
fn ddadd2_vd2_vd_vd2(VDouble x, VDouble2 y) -> VDouble2 {
  VDouble2 r;

  r.0  = x + y.0;
  VDouble v = r.0 - x;
  r.1 = x - (r.0 - v) + (y.0 - v) + y.1;

  return r;
}

#[inline]
fn ddadd_vd2_vd2_vd2(VDouble2 x, VDouble2 y) -> VDouble2 {
  // |x| >= |y|

  VDouble2 r;

  r.0 = x.0 + y.0;
  r.1 = x.0 - r.0 + y.0 + x.1 + y.1;

  return r;
}

#[inline]
fn ddadd2_vd2_vd2_vd2(VDouble2 x, VDouble2 y) -> VDouble2 {
  VDouble2 r;

  r.0  = x.0 + y.0;
  VDouble v = r.0 - x.0;
  r.1 = x.0 - (r.0 - v) + (y.0 - v);
  r.1 = r.1 + (x.1 + y.1);

  return r;
}

#[inline]
fn ddsub_vd2_vd_vd(VDouble x, VDouble y) -> VDouble2 {
  // |x| >= |y|

  VDouble2 r;

  r.0 = x - y;
  r.1 = x - r.0 - y;

  return r;
}

#[inline]
fn ddsub_vd2_vd2_vd2(VDouble2 x, VDouble2 y) -> VDouble2 {
  // |x| >= |y|

  VDouble2 r;

  r.0 = x.0 - y.0;
  r.1 = x.0 - r.0;
  r.1 = r.1 - y.0;
  r.1 = r.1 + x.1;
  r.1 = r.1 - y.1;

  return r;
}

#ifdef ENABLE_FMA_DP
#[inline]
fn dddiv_vd2_vd2_vd2(VDouble2 n, VDouble2 d) -> VDouble2 {
  VDouble2 q;
  VDouble t = vrec_vd_vd(d.0), u;

  q.0 = n.0 * t;
  u = vfmapn_vd_vd_vd_vd(t, n.0, q.0);
  q.1 = vfmanp_vd_vd_vd_vd(d.1, t, vfmanp_vd_vd_vd_vd(d.0, t, vcast_vd_d(1)));
  q.1 = vfma_vd_vd_vd_vd(q.0, q.1, vfma_vd_vd_vd_vd(n.1, t, u));

  return q;
}

#[inline]
fn ddmul_vd2_vd_vd(VDouble x, VDouble y) -> VDouble2 {
  VDouble2 r;

  r.0 = x * y;
  r.1 = vfmapn_vd_vd_vd_vd(x, y, r.0);

  return r;
}

#[inline]
fn ddsqu_vd2_vd2(x: VDouble2) -> VDouble2 {
  VDouble2 r;

  r.0 = x.0 * x.0;
  r.1 = vfma_vd_vd_vd_vd(x.0 + x.0, x.1, vfmapn_vd_vd_vd_vd(x.0, x.0, r.0));

  return r;
}

#[inline]
fn ddmul_vd2_vd2_vd2(VDouble2 x, VDouble2 y) -> VDouble2 {
  VDouble2 r;

  r.0 = x.0 * y.0;
  r.1 = vfma_vd_vd_vd_vd(x.0, y.1, vfma_vd_vd_vd_vd(x.1, y.0, vfmapn_vd_vd_vd_vd(x.0, y.0, r.0)));

  return r;
}

#[inline]
fn ddmul_vd_vd2_vd2(VDouble2 x, VDouble2 y) -> VDouble {
  return vfma_vd_vd_vd_vd(x.0, y.0, vfma_vd_vd_vd_vd(x.1, y.0, x.0 * y.1));
}

#[inline]
fn ddsqu_vd_vd2(x: VDouble2) -> VDouble {
  return vfma_vd_vd_vd_vd(x.0, x.0, x.0 * x.1 + x.0 * x.1);
}

#[inline]
fn ddmul_vd2_vd2_vd(VDouble2 x, VDouble y) -> VDouble2 {
  VDouble2 r;

  r.0 = x.0 * y;
  r.1 = vfma_vd_vd_vd_vd(x.1, y, vfmapn_vd_vd_vd_vd(x.0, y, r.0));

  return r;
}

#[inline]
fn ddrec_vd2_vd(d: VDouble) -> VDouble2 {
  VDouble2 q;

  q.0 = vrec_vd_vd(d);
  q.1 = q.0 * vfmanp_vd_vd_vd_vd(d, q.0, vcast_vd_d(1));

  return q;
}

#[inline]
fn ddrec_vd2_vd2(d: VDouble2) -> VDouble2 {
  VDouble2 q;

  q.0 = vrec_vd_vd(d.0);
  q.1 = q.0 * vfmanp_vd_vd_vd_vd(d.1, q.0, vfmanp_vd_vd_vd_vd(d.0, q.0, vcast_vd_d(1)));

  return q;
}
#else
#[inline]
fn dddiv_vd2_vd2_vd2(VDouble2 n, VDouble2 d) -> VDouble2 {
  VDouble t = vrec_vd_vd(d.0);
  VDouble dh  = vupper_vd_vd(d.0), dl  = d.0 -  dh;
  VDouble th  = vupper_vd_vd(t  ), tl  = t   -  th;
  VDouble nhh = vupper_vd_vd(n.0), nhl = n.0 - nhh;

  VDouble2 q;

  q.0 = n.0 * t;

  VDouble u = (nhh * th - q.0 + nhh * tl + nhl * th + nhl * tl +
		    q.0 * vsub_vd_5vd(vcast_vd_d(1), dh * th, dh * tl, dl * th, dl * tl));

  q.1 = t.mla(n.1 - q.0 * d.1, u);

  return q;
}

#[inline]
fn ddmul_vd2_vd_vd(VDouble x, VDouble y) -> VDouble2 {
  VDouble xh = vupper_vd_vd(x), xl = x - xh;
  VDouble yh = vupper_vd_vd(y), yl = y - yh;
  VDouble2 r;

  r.0 = x * y;
  r.1 = xh * yh + (-r.0) + xl * yh + xh * yl + xl * yl;

  return r;
}

#[inline]
fn ddmul_vd2_vd2_vd(VDouble2 x, VDouble y) -> VDouble2 {
  VDouble xh = vupper_vd_vd(x.0), xl = x.0 - xh;
  VDouble yh = vupper_vd_vd(y  ), yl = y   - yh;
  VDouble2 r;

  r.0 = x.0 * y;
  r.1 = xh * yh + (-r.0) + xl * yh + xh * yl + xl * yl + x.1 * y;

  return r;
}

#[inline]
fn ddmul_vd2_vd2_vd2(VDouble2 x, VDouble2 y) -> VDouble2 {
  VDouble xh = vupper_vd_vd(x.0), xl = x.0 - xh;
  VDouble yh = vupper_vd_vd(y.0), yl = y.0 - yh;
  VDouble2 r;

  r.0 = x.0 * y.0;
  r.1 = xh * yh + (-r.0) + xl * yh + xh * yl + xl * yl + x.0 * y.1 + x.1 * y.0;

  return r;
}

#[inline]
fn ddmul_vd_vd2_vd2(VDouble2 x, VDouble2 y) -> VDouble {
  VDouble xh = vupper_vd_vd(x.0);
   xl = x.0 - xh;
  VDouble yh = vupper_vd_vd(y.0);
   yl = y.0 - yh;

  x.1 * yh + xh * y.1 + xl * yl + xh * yl + xl * yh + xh * yh
}

#[inline]
fn ddsqu_vd2_vd2(x: VDouble2) -> VDouble2 {
  VDouble xh = vupper_vd_vd(x.0), xl = x.0 - xh;
  VDouble2 r;

  r.0 = x.0 * x.0;
  r.1 = xh * xh + (-r.0) + (xh + xh) * xl + xl * xl + x.0 * (x.1 + x.1);

  return r;
}

#[inline]
fn ddsqu_vd_vd2(x: VDouble2) -> VDouble {
  VDouble xh = vupper_vd_vd(x.0), xl = x.0 - xh;

  xh * x.1 + xh * x.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
}

#[inline]
fn ddrec_vd2_vd(d: VDouble) -> VDouble2 {
  VDouble t = vrec_vd_vd(d);
  VDouble dh = vupper_vd_vd(d);
   dl = d - dh;
  VDouble th = vupper_vd_vd(t);
   tl = t - th;
  VDouble2 q;

  q.0 = t;
  q.1 = t * vsub_vd_5vd(vcast_vd_d(1), dh * th, dh * tl, dl * th, dl * tl);

  return q;
}

#[inline]
fn ddrec_vd2_vd2(d: VDouble2) -> VDouble2 {
  VDouble t = vrec_vd_vd(d.0);
  VDouble dh = vupper_vd_vd(d.0);
   dl = d.0 - dh;
  VDouble th = vupper_vd_vd(t  );
   tl = t   - th;
  VDouble2 q;

  q.0 = t;
  q.1 = t * vsub_vd_6vd(vcast_vd_d(1), dh * th, dh * tl, dl * th, dl * tl, d.1 * t);

  return q;
}
#endif

#[inline]
fn ddsqrt_vd2_vd2(d: VDouble2) -> VDouble2 {
  VDouble t = vsqrt_vd_vd(d.0 + d.1);
  return ddscale_vd2_vd2_vd(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd2_vd2(d, ddmul_vd2_vd_vd(t, t)), ddrec_vd2_vd(t)), vcast_vd_d(0.5));
}

#[inline]
fn ddsqrt_vd2_vd(d: VDouble) -> VDouble2 {
  VDouble t = vsqrt_vd_vd(d);
  return ddscale_vd2_vd2_vd(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd_vd2(d, ddmul_vd2_vd_vd(t, t)), ddrec_vd2_vd(t)), vcast_vd_d(0.5));
}
