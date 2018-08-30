#[inline]
fn vupper_vd_vd(d: $f64x) -> $f64x {
  return vreinterpret_vd_vm(vand_vm_vm_vm(vreinterpret_vm_vd(d), vcast_vm_i_i(0xffffffff, 0xf8000000)));
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct D2<T>(pub T, pub T);

impl D2<$f64x> {
  pub fn new(x0: $f64x, x1: $f64x) {
    D2(x0, x1)
  }
}

impl std::convert::From<(f64, f64)> for D2<$f64x> {
  fn from(f: (f64, f64)) -> Self {
    Self::new($f64x::splat(f.0), $f64x::splat(f.1))
  }
}

#[inline]
fn vsel_vd2_vo_vd2_vd2(m: $mox, x: D2<$f64x>, y: D2<$f64x>) -> D2<$f64x> {
  D2::new(vsel_vd_vo_vd_vd(m, x.0, y.0), vsel_vd_vo_vd_vd(m, x.1, y.1))
}

#[inline]
fn vsel_vd2_vo_d_d_d_d(o: $mox, x1: f64, y1: f64, x0: f64, y0: f64) -> D2<$f64x> {
  D2::new(vsel_vd_vo_d_d(o, x1, x0), vsel_vd_vo_d_d(o, y1, y0))
}

//

#[inline]
fn ddneg_vd2_vd2(x: D2<$f64x>) -> D2<$f64x> {
  Self::new(-x.0, -x.1)
}

#[inline]
fn ddabs_vd2_vd2(x: D2<$f64x>) -> D2<$f64x> {
  Self::new(vabs_vd_vd(x.0),
			 vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(x.1), vand_vm_vm_vm(vreinterpret_vm_vd(x.0), vreinterpret_vm_vd($f64x::splat((-0.))))))
}

#[inline]
fn ddnormalize_vd2_vd2(t: D2<$f64x>) -> D2<$f64x> {
  let s0 = t.0 + t.1;
  Self::new(s0,  t.0 - s0 + t.1 )
}

#[inline]
fn ddscale_vd2_vd2_vd(d: D2<$f64x>, s: $f64x) -> D2<$f64x> {
  D2::new(d.0 * s, d.1 * s)
}

#[inline]
fn ddadd_vd2_vd_vd(x: $f64x, y: $f64x) -> D2<$f64x> {
  let r0 = x + y;
  Self::new(r0,  x - r0 + y )
}

#[inline]
fn ddadd2_vd2_vd_vd(x: $f64x, y: $f64x) -> D2<$f64x> {
  let r0 = x + y;
  let v = r0 - x;
  Self::new(r0,  x - (r0 - v) + (y - v) )
}

#[inline]
fn ddadd_vd2_vd2_vd(x: D2<$f64x>, y: $f64x) -> D2<$f64x> {
  let r0 = x.0 + y;
  Self::new(r0, x.0 - r0 + y + x.1 )
}

#[inline]
fn ddsub_vd2_vd2_vd(x: D2<$f64x>, y: $f64x) -> D2<$f64x> {
  let r0 = x.0 - y;
  Self::new(r0, x.0 - r0 - y + x.1 )
}

#[inline]
fn ddadd2_vd2_vd2_vd(x: D2<$f64x>, y: $f64x) -> D2<$f64x> {
  let r0 = x.0 + y;
  let v = r0 - x.0;
  Self::new(r0, x.0 - (r0 - v) + (y - v) + x.1 )
}

#[inline]
fn ddadd_vd2_vd_vd2(x: $f64x, y: D2<$f64x>) -> D2<$f64x> {
  let r0 = x + y.0;
  Self::new(r0, x - r0 + y.0 + y.1 )
}

#[inline]
fn ddadd2_vd2_vd_vd2(x: $f64x, y: D2<$f64x>) -> D2<$f64x> {
  let r0  = x + y.0;
  let v = r0 - x;
  Self::new(r0,  x - (r0 - v) + (y.0 - v) + y.1)
}

#[inline]
fn ddadd_vd2_vd2_vd2(x: D2<$f64x>, y: D2<$f64x>) -> D2<$f64x> {
  // |x| >= |y|
  let r0 = x.0 + y.0;
  Self::new(r0,  x.0 - r0 + y.0 + x.1 + y.1 )
}

#[inline]
fn ddadd2_vd2_vd2_vd2(x: D2<$f64x>, y: D2<$f64x>) -> D2<$f64x> {
  let r0  = x.0 + y.0;
  let v = r0 - x.0;
  Self::new(r0,  x.0 - (r0 - v) + (y.0 - v) + (x.1 + y.1) )
}

#[inline]
fn ddsub_vd2_vd_vd(x: $f64x, y: $f64x) -> D2<$f64x> {
  // |x| >= |y|
  let r0 = x - y;
  Self::new(r0, x - r0 - y )
}

#[inline]
fn ddsub_vd2_vd2_vd2(x: D2<$f64x>, y: D2<$f64x>) -> D2<$f64x> {
  // |x| >= |y|
  let r0 = x.0 - y.0;
  let mut r1 = x.0 - r0;
  r1 = r1 - y.0;
  r1 = r1 + x.1;
  r1 = r1 - y.1;

  Self::new(r0, r1)
}

#ifdef ENABLE_FMA_DP
#[inline]
fn dddiv_vd2_vd2_vd2(n: D2<$f64x>, d: D2<$f64x>) -> D2<$f64x> {
  let t = vrec_vd_vd(d.0);

  let q0 = n.0 * t;
  let u = vfmapn_vd_vd_vd_vd(t, n.0, q0);
  let mut q1 = vfmanp_vd_vd_vd_vd(d.1, t, vfmanp_vd_vd_vd_vd(d.0, t, $f64x::splat(1.)));
  q1 = vfma_vd_vd_vd_vd(q0, q1, vfma_vd_vd_vd_vd(n.1, t, u));

  Self::new(q0, q1)
}

#[inline]
fn ddmul_vd2_vd_vd(x: $f64x, y: $f64x) -> D2<$f64x> {
  let r0 = x * y;
  Self::new(r0, vfmapn_vd_vd_vd_vd(x, y, r0) )
}

#[inline]
fn ddsqu_vd2_vd2(x: D2<$f64x>) -> D2<$f64x> {
  let r0 = x.0 * x.0;
  Self::new(r0, vfma_vd_vd_vd_vd(x.0 + x.0, x.1, vfmapn_vd_vd_vd_vd(x.0, x.0, r0)) )
}

#[inline]
fn ddmul_vd2_vd2_vd2(x: D2<$f64x>, y: D2<$f64x>) -> D2<$f64x> {
  let r0 = x.0 * y.0;
  Self::new(r0, vfma_vd_vd_vd_vd(x.0, y.1, vfma_vd_vd_vd_vd(x.1, y.0, vfmapn_vd_vd_vd_vd(x.0, y.0, r0))) )
}

#[inline]
fn ddmul_vd_vd2_vd2(x: D2<$f64x>, y: D2<$f64x>) -> $f64x {
  vfma_vd_vd_vd_vd(x.0, y.0, vfma_vd_vd_vd_vd(x.1, y.0, x.0 * y.1))
}

#[inline]
fn ddsqu_vd_vd2(x: D2<$f64x>) -> $f64x {
  vfma_vd_vd_vd_vd(x.0, x.0, x.0 * x.1 + x.0 * x.1)
}

#[inline]
fn ddmul_vd2_vd2_vd(x: D2<$f64x>, y: $f64x) -> D2<$f64x> {
  let r0 = x.0 * y;
  Self::new(r0, vfma_vd_vd_vd_vd(x.1, y, vfmapn_vd_vd_vd_vd(x.0, y, r0)) )
}

#[inline]
fn ddrec_vd2_vd(d: $f64x) -> D2<$f64x> {
  let q0 = vrec_vd_vd(d);
  Self::new(q0, q0 * vfmanp_vd_vd_vd_vd(d, q0, $f64x::splat(1.)) )
}

#[inline]
fn ddrec_vd2_vd2(d: D2<$f64x>) -> D2<$f64x> {
  let q0 = vrec_vd_vd(d.0);
  Self::new(q0, q0 * vfmanp_vd_vd_vd_vd(d.1, q0, vfmanp_vd_vd_vd_vd(d.0, q0, $f64x::splat(1))) )
}
#else
#[inline]
fn dddiv_vd2_vd2_vd2(n: D2<$f64x>, d: D2<$f64x>) -> D2<$f64x> {
  let t = vrec_vd_vd(d.0);
  let dh  = vupper_vd_vd(d.0);
  let dl  = d.0 -  dh;
  let th  = vupper_vd_vd(t  );
  let tl  = t   -  th;
  let nhh = vupper_vd_vd(n.0);
  let nhl = n.0 - nhh;

  let q0 = n.0 * t;

  $f64x u = (nhh * th - q0 + nhh * tl + nhl * th + nhl * tl +
		    q0 * ($f64x::splat(1) - dh * th - dh * tl - dl * th - dl * tl));

  Self::new(q0, t.mla(n.1 - q0 * d.1, u) )
}

#[inline]
fn ddmul_vd2_vd_vd(x: $f64x, y: $f64x) -> D2<$f64x> {
  let xh = vupper_vd_vd(x);
  let xl = x - xh;
  let yh = vupper_vd_vd(y);
  let yl = y - yh;
  let r0 = x * y;
  Self::new(r0, xh * yh + (-r0) + xl * yh + xh * yl + xl * yl )
}

#[inline]
fn ddmul_vd2_vd2_vd(x: D2<$f64x>, y: $f64x) -> D2<$f64x> {
  let xh = vupper_vd_vd(x.0);
  let xl = x.0 - xh;
  let yh = vupper_vd_vd(y  );
  let yl = y   - yh;
  let r0 = x.0 * y;
  Self::new(r0, xh * yh + (-r0) + xl * yh + xh * yl + xl * yl + x.1 * y )
}

#[inline]
fn ddmul_vd2_vd2_vd2(x: D2<$f64x>, y: D2<$f64x>) -> D2<$f64x> {
  let xh = vupper_vd_vd(x.0);
  let xl = x.0 - xh;
  let yh = vupper_vd_vd(y.0);
  let yl = y.0 - yh;
  let r0 = x.0 * y.0;
  Self::new(r0, xh * yh + (-r0) + xl * yh + xh * yl + xl * yl + x.0 * y.1 + x.1 * y.0 )
}

#[inline]
fn ddmul_vd_vd2_vd2(x: D2<$f64x>, y: D2<$f64x>) -> $f64x {
  let xh = vupper_vd_vd(x.0);
  let xl = x.0 - xh;
  let yh = vupper_vd_vd(y.0);
  let yl = y.0 - yh;

  x.1 * yh + xh * y.1 + xl * yl + xh * yl + xl * yh + xh * yh
}

#[inline]
fn ddsqu_vd2_vd2(x: D2<$f64x>) -> D2<$f64x> {
  let xh = vupper_vd_vd(x.0);
  let xl = x.0 - xh;
  let r0 = x.0 * x.0;
  Self::new(r0, xh * xh + (-r0) + (xh + xh) * xl + xl * xl + x.0 * (x.1 + x.1) )
}

#[inline]
fn ddsqu_vd_vd2(x: D2<$f64x>) -> $f64x {
  let xh = vupper_vd_vd(x.0);
  let xl = x.0 - xh;

  xh * x.1 + xh * x.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
}

#[inline]
fn ddrec_vd2_vd(d: $f64x) -> D2<$f64x> {
  let t = vrec_vd_vd(d);
  let dh = vupper_vd_vd(d);
  let dl = d - dh;
  let th = vupper_vd_vd(t);
  let tl = t - th;
  let q0 = t;
  Self::new(r0, t * ($f64x::splat(1) - dh * th - dh * tl - dl * th - dl * tl) )
}

#[inline]
fn ddrec_vd2_vd2(d: D2<$f64x>) -> D2<$f64x> {
  let t = vrec_vd_vd(d.0);
  let dh = vupper_vd_vd(d.0);
  let dl = d.0 - dh;
  let th = vupper_vd_vd(t  );
  let tl = t   - th;
  let q0 = t;
  Self::new(r0, t * ($f64x::splat(1) - dh * th - dh * tl - dl * th - dl * tl - d.1 * t) )
}
#endif

#[inline]
fn ddsqrt_vd2_vd2(d: D2<$f64x>) -> D2<$f64x> {
  let t = vsqrt_vd_vd(d.0 + d.1);
  ddscale_vd2_vd2_vd(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd2_vd2(d, ddmul_vd2_vd_vd(t, t)), ddrec_vd2_vd(t)), $f64x::splat(0.5))
}

#[inline]
fn ddsqrt_vd2_vd(d: $f64x) -> D2<$f64x> {
  let t = vsqrt_vd_vd(d);
  ddscale_vd2_vd2_vd(ddmul_vd2_vd2_vd2(ddadd2_vd2_vd_vd2(d, ddmul_vd2_vd_vd(t, t)), ddrec_vd2_vd(t)), $f64x::splat(0.5))
}
