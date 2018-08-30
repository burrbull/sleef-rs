#[inline]
fn vupper_vf_vf(d: $f32x) -> $f32x {
  return vreinterpret_vf_vi2(vand_vi2_vi2_vi2(vreinterpret_vi2_vf(d), $ix2::splat(0xfffff000)));
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct F2<T>(pub T, pub T);

impl F2<$f32x> {
  pub fn new(x0: $f32x, x1: $f32x) {
    F2(x0, x1)
  }
}

impl std::convert::From<f64> for F2<$f32x> {
  fn from(d: f32) -> Self {
    Self::new($f32x::splat(d as f32), $f32x::splat((d as f32) - (d as f32)))
  }
}

impl std::convert::From<(f32, f32)> for F2<$f32x> {
  fn from(f: (f32, f32)) -> Self {
    Self::new($f32x::splat(f.0), $f32x::splat(f.1))
  }
}

#[inline]
fn vsel_vf2_vo_vf2_vf2($mox m, F2<$f32x> x, F2<$f32x> y) -> F2<$f32x> {
  Self::new( vsel_vf_vo_vf_vf(m, x.0, y.0), vsel_vf_vo_vf_vf(m, x.1, y.1) )
}

#[inline]
fn vsel_vf2_vo_f_f_f_f(o: $mox, x1: f32, y1: f32, x0: f32, y0: f32) -> F2<$f32x> {
  Self::new( vsel_vf_vo_f_f(o, x1, x0),  vsel_vf_vo_f_f(o, y1, y0) )
}

#[inline]
fn vsel_vf2_vo_vo_d_d_d(o0: $mox, o1: $mox, d0: f64, d1: f64, d2: f64) -> F2<$f32x> {
  vsel_vf2_vo_vf2_vf2(o0, F2::from(d0), vsel_vf2_vo_vf2_vf2(o1, F2::from(d1), F2::from(d2)))
}

#[inline]
fn vsel_vf2_vo_vo_vo_d_d_d_d(o0: $mox, o1: $mox, o2: $mox, d0: f64, d1: f64, d2: f64, d3: f64) -> F2 {
  vsel_vf2_vo_vf2_vf2(o0, F2::from(d0), vsel_vf2_vo_vf2_vf2(o1, F2::from(d1), vsel_vf2_vo_vf2_vf2(o2, F2::from(d2), F2::from(d3))))
}

#[inline]
fn vabs_vf2_vf2(x: F2<$f32x>) -> F2<$f32x> {
  Self::new(vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.0)), vreinterpret_vm_vf(x.0)), vreinterpret_vm_vf(x.0))),
			 vreinterpret_vf_vm(vxor_vm_vm_vm(vand_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.0)), vreinterpret_vm_vf(x.0)), vreinterpret_vm_vf(x.1))))
}


#[inline]
fn dfneg_vf2_vf2(x: F2<$f32x>) -> F2<$f32x> {
  Self::new(-x.0, -x.1);
}

#[inline]
fn dfabs_vf2_vf2(x: F2<$f32x>) -> F2<$f32x> {
  Self::new(vabs_vf_vf(x.0),
			 vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(x.1), vand_vm_vm_vm(vreinterpret_vm_vf(x.0), vreinterpret_vm_vf($f32x::splat(-0.))))))
}

#[inline]
fn dfnormalize_vf2_vf2(t: F2<$f32x>) -> F2<$f32x> {
  let s0 = t.0 + t.1;
  Self::new(s0, t.0 - s0 + t.1)
}

#[inline]
fn dfscale_vf2_vf2_vf(d: F2<$f32x>, s: $f32x) -> F2<$f32x> {
  Self::new( d.0 * s, d.1 * s )
}

#[inline]
fn dfadd_vf2_vf_vf($f32x x, $f32x y) -> F2<$f32x> {
  let r0 = x + y,
  Self::new(r0, x - r0 + y )
}

#[inline]
fn dfadd2_vf2_vf_vf($f32x x, $f32x y) -> F2<$f32x> {
  let r0 = x + y;
  let v = r0 - x;
  Self::new(r0, x - (r0 - v) + (y - v) )
}

#[inline]
fn dfadd2_vf2_vf_vf2($f32x x, F2<$f32x> y) -> F2<$f32x> {
  let r0  = x + y.0;
  let v = r0 - x;
  Self::new(r0, x - (r0 - v) + (y.0 - v) + y.1 )
}

#[inline]
fn dfadd_vf2_vf2_vf(F2<$f32x> x, $f32x y) -> F2<$f32x> {
  let r0 = x.0 + y;
  Self::new(r0, x.0 - r0 + y + x.1 )
}

#[inline]
fn dfsub_vf2_vf2_vf(F2<$f32x> x, $f32x y) -> F2<$f32x> {
  let r0 = x.0 - y;
  Self::new(r0, x.0 - r0 - y + x.1 )
}

#[inline]
fn dfadd2_vf2_vf2_vf(F2<$f32x> x, $f32x y) -> F2<$f32x> {
  let r0 = x.0 + y;
  let v = r0 - x.0;
  Self::new(r0, x.0 - (r0 - v) + (y - v) + x.1 )
}

#[inline]
fn dfadd_vf2_vf_vf2($f32x x, y: F2<$f32x>) -> F2<$f32x> {
  let r0 = x + y.0;
  Self::new(r0, x - r0 + y.0 + y.1)
}

#[inline]
fn dfadd_vf2_vf2_vf2(x: F2<$f32x>, y: F2<$f32x>) -> F2<$f32x> {
  // |x| >= |y|
  let r0 = x.0 + y.0;
  Self::new(r0, x.0 - r0 + y.0 + x.1 + y.1)
}

#[inline]
fn dfadd2_vf2_vf2_vf2(x: F2<$f32x>, y: F2<$f32x>) -> F2<$f32x> {
  let r0  = x.0 + y.0;
  let v = r0 - x.0;
  Self::new(r0, x.0 - (r0 - v) + (y.0 - v) + (x.1 + y.1) )
}

#[inline]
fn dfsub_vf2_vf_vf(x: $f32x, y: $f32x) -> F2<$f32x> {
  // |x| >= |y|
  let r0 = x - y;
  Self::new(r0, x - r0 - y )
}

#[inline]
fn dfsub_vf2_vf2_vf2(x: F2<$f32x>, y: F2<$f32x>) -> F2<$f32x> {
  // |x| >= |y|
  let r0 = x.0 - y.0;
  let mut r1 = x.0 - r0;
  r1 = r1 - y.0;
  r1 = r1 + x.1;
  r1 = r1 - y.1;

  Self::new(r0, r1)
}

#ifdef ENABLE_FMA_SP
#[inline]
fn dfdiv_vf2_vf2_vf2(n: F2<$f32x>, d: F2<$f32x>) -> F2<$f32x> {
  let t = vrec_vf_vf(d.0);

  let q0 = n.0 * t;
  let u = vfmapn_vf_vf_vf_vf(t, n.0, q0);
  let mut q1 = vfmanp_vf_vf_vf_vf(d.1, t, vfmanp_vf_vf_vf_vf(d.0, t, $f32x::splat(1)));
  q1 = vfma_vf_vf_vf_vf(q0, q1, vfma_vf_vf_vf_vf(n.1, t, u));

  Self::new(q0, q1)
}

#[inline]
fn dfmul_vf2_vf_vf(x: $f32x, y: $f32x) -> F2<$f32x> {
  let r0 = x * y;
  Self::new(r0, vfmapn_vf_vf_vf_vf(x, y, r0) )
}

#[inline]
fn dfsqu_vf2_vf2(x: F2<$f32x>) -> F2<$f32x> {
  let r0 = x.0 * x.0;
  Self::new(r0, vfma_vf_vf_vf_vf(x.0 + x.0, x.1, vfmapn_vf_vf_vf_vf(x.0, x.0, r0)) )
}

#[inline]
fn dfsqu_vf_vf2(x: F2<$f32x>) -> $f32x {
  vfma_vf_vf_vf_vf(x.0, x.0, x.0 * x.1 + x.0 * x.1)
}

#[inline]
fn dfmul_vf2_vf2_vf2(x: F2<$f32x>, y: F2<$f32x>) -> F2<$f32x> {
  let r0 = x.0 * y.0;
  Self::new(r0, vfma_vf_vf_vf_vf(x.0, y.1, vfma_vf_vf_vf_vf(x.1, y.0, vfmapn_vf_vf_vf_vf(x.0, y.0, r0))) )
}

#[inline]
fn dfmul_vf_vf2_vf2(x: F2<$f32x>, y: F2<$f32x>) -> $f32x {
  vfma_vf_vf_vf_vf(x.0, y.0, vfma_vf_vf_vf_vf(x.1, y.0, x.0 * y.1))
}

#[inline]
fn dfmul_vf2_vf2_vf(x: F2<$f32x>, y: $f32x) -> F2<$f32x> {
  let r0 = x.0 * y;
  Self::new(r0, vfma_vf_vf_vf_vf(x.1, y, vfmapn_vf_vf_vf_vf(x.0, y, r0)) )
}

#[inline]
fn dfrec_vf2_vf(d: $f32x) -> F2<$f32x> {
  let q0 = vrec_vf_vf(d);
  Self::new(q0, q0 * vfmanp_vf_vf_vf_vf(d, q0, $f32x::splat(1)) )
}

#[inline]
fn dfrec_vf2_vf2(d: F2<$f32x>) -> F2<$f32x> {
  let q0 = vrec_vf_vf(d.0);
  Self::new(r0, q0 * vfmanp_vf_vf_vf_vf(d.1, q0, vfmanp_vf_vf_vf_vf(d.0, q0, $f32x::splat(1))) )
}
#else
#[inline]
fn dfdiv_vf2_vf2_vf2(n: F2<$f32x>, d: F2<$f32x>) -> F2<$f32x> {
  let t = vrec_vf_vf(d.0);
  let dh  = vupper_vf_vf(d.0);
  let dl  = d.0 -  dh;
  let th  = vupper_vf_vf(t  );
  let tl  = t   -  th;
  let nhh = vupper_vf_vf(n.0);
  let nhl = n.0 - nhh;

  let q0 = n.0 * t;

  let mut w = $f32x::splat(-1);
  w = dh.mla(th, w);
  w = dh.mla(tl, w);
  w = dl.mla(th, w);
  w = dl.mla(tl, w);
  w = -w;

  let mut u = nhh.mla(th, -q0);
  u = nhh.mla(tl, u);
  u = nhl.mla(th, u);
  u = nhl.mla(tl, u);
  u = q0.mla(w , u);

  Self::new(q0, t.mla(n.1 - q0 * d.1, u) )
}

#[inline]
fn dfmul_vf2_vf_vf(x: $f32x, y: $f32x) -> F2<$f32x> {
  let xh = vupper_vf_vf(x);
  let xl = x - xh;
  let yh = vupper_vf_vf(y);
  let yl = y - yh;
  let r0 = x * y;

  $f32x t;
  t = xh.mla(yh, -r0);
  t = xl.mla(yh, t);
  t = xh.mla(yl, t);
  t = xl.mla(yl, t);
  Self::new(r0, t )
}

#[inline]
fn dfmul_vf2_vf2_vf(x: F2<$f32x>, y: $f32x) -> F2<$f32x> {
  let xh = vupper_vf_vf(x.0);
  let xl = x.0 - xh;
  let yh = vupper_vf_vf(y  );
  let yl = y   - yh;
  let r0 = x.0 * y;

  let mut t = xh.mla(yh, -r0);
  t = xl.mla(yh, t);
  t = xh.mla(yl, t);
  t = xl.mla(yl, t);
  t = x.1.mla(y, t);
  Self::new(r0, t )
}

#[inline]
fn dfmul_vf2_vf2_vf2(x: F2<$f32x>, y: F2<$f32x>) -> F2<$f32x> {
  let xh = vupper_vf_vf(x.0);
  let xl = x.0 - xh;
  let yh = vupper_vf_vf(y.0);
  let yl = y.0 - yh;
  let r0 = x.0 * y.0;

  let mut t = xh.mla(yh, -r0);
  t = xl.mla(yh, t);
  t = xh.mla(yl, t);
  t = xl.mla(yl, t);
  t = x.0.mla(y.1, t);
  t = x.1.mla(y.0, t);
  Self::new(r0, t)
}

#[inline]
fn dfmul_vf_vf2_vf2(x: F2<$f32x>, y: F2<$f32x>) -> $f32x {
  let xh = vupper_vf_vf(x.0);
  let xl = x.0 - xh;
  let yh = vupper_vf_vf(y.0);
  let yl = y.0 - yh;
  x.1 * yh + xh * y.1 + xl * yl + xh * yl + xl * yh + xh * yh
}

#[inline]
fn dfsqu_vf2_vf2(x: F2<$f32x>) -> F2<$f32x> {
  let xh = vupper_vf_vf(x.0);
  let xl = x.0 - xh;
  let r0 = x.0 * x.0;

  let mut  t = xh.mla(xh, -r0);
  t = (xh + xh).mla(xl, t);
  t = xl.mla(xl, t);
  t = x.0.mla(x.1 + x.1, t);
  Self::new(r0, t)
}

#[inline]
fn dfsqu_vf_vf2(x: F2) -> $f32x {
  let xh = vupper_vf_vf(x.0);
  let xl = x.0 - xh;
  xh * x.1 + xh * x.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
}

#[inline]
fn dfrec_vf2_vf(d: $f32x) -> F2<$f32x> {
  let t = vrec_vf_vf(d);
  let dh = vupper_vf_vf(d);
  let dl = d - dh;
  let th = vupper_vf_vf(t);
  let tl = t - th;
  let q0 = t;

  let mut u = $f32x::splat(-1);
  u = dh.mla(th, u);
  u = dh.mla(tl, u);
  u = dl.mla(th, u);
  u = dl.mla(tl, u);
  Self::new(q0, (-t) * u)
}

#[inline]
fn dfrec_vf2_vf2(d: F2<$f32x>) -> F2<$f32x> {
  let t = vrec_vf_vf(d.0);
  let dh = vupper_vf_vf(d.0);
  let dl = d.0 - dh;
  let th = vupper_vf_vf(t  );
  let tl = t   - th;
  let q0 = t;

  let mut u = $f32x::splat(-1);
  u = dh.mla(th, u);
  u = dh.mla(tl, u);
  u = dl.mla(th, u);
  u = dl.mla(tl, u);
  u = d.1.mla(t, u);
  Self::new(q0, (-t) * u )
}
#endif

#[inline]
fn dfsqrt_vf2_vf2(d: F2<$f32x>) -> F2<$f32x> {
#ifdef ENABLE_RECSQRT_SP
  let x = vrecsqrt_vf_vf(d.0 + d.1);
  let r = dfmul_vf2_vf2_vf(d, x);
  dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(r, dfadd2_vf2_vf2_vf(dfmul_vf2_vf2_vf(r, x), $f32x::splat(-3.0))), $f32x::splat(-0.5))
#else
  let t = vsqrt_vf_vf(d.0 + d.1);
  dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf2_vf2(d, dfmul_vf2_vf_vf(t, t)), dfrec_vf2_vf(t)), $f32x::splat(0.5))
#endif
}

#[inline]
fn dfsqrt_vf2_vf(d: $f32x) -> F2<$f32x> {
  let t = vsqrt_vf_vf(d);
  dfscale_vf2_vf2_vf(dfmul_vf2_vf2_vf2(dfadd2_vf2_vf_vf2(d, dfmul_vf2_vf_vf(t, t)), dfrec_vf2_vf(t)), $f32x::splat(0.5))
}
