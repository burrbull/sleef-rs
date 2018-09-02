#[inline]
fn vupper_vf_vf(d: $f32x) -> $f32x {
  $f32x::from($ix2::from(d) & $ix2::splat(0xfffff000))
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct F2<T>(pub T, pub T);

impl F2<$f32x> {
    pub fn new(x0: $f32x, x1: $f32x) {
      F2(x0, x1)
    }
  
    #[inline]
    pub fn normalize(self) -> Self {
        let s0 = self.0 + self.1;
        Self::new(s0, self.0 - s0 + self.1)
    }

    #[inline]
    pub fn abs(self) -> Self {
        Self::new(self.0.abs(),
            $f32x::from($mx::from(self.1) ^ ($mx::from(self.0) & $mx::from($f32x::splat(-0.))))
    }

    #[inline]
    pub fn scale(self, other: $f32x) -> Self {
        Self::new( self.0 * other, self.1 * other )
    }

    #[cfg(feature="enable_fma_dp")]
    #[inline]
    pub fn square(self) -> Self {
        let r0 = self.0 * self.0;
        Self::new(r0, vfma_vf_vf_vf_vf(self.0 + self.0, self.1, vfmapn_vf_vf_vf_vf(self.0, self.0, r0)) )
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    pub fn square(self) -> Self {
        let xh = vupper_vf_vf(self.0);
        let xl = self.0 - xh;
        let r0 = self.0 * self.0;

        let mut  t = xh.mla(xh, -r0);
        t = (xh + xh).mla(xl, t);
        t = xl.mla(xl, t);
        t = self.0.mla(self.1 + self.1, t);
        Self::new(r0, t)
    }

    #[cfg(feature="enable_fma_dp")]
    #[inline]
    pub fn square_as_f(self) -> $f32x {
        vfma_vf_vf_vf_vf(self.0, self.0, self.0 * self.1 + self.0 * self.1)
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    pub fn square_as_f(self) -> $f32x {
        let xh = vupper_vf_vf(self.0);
        let xl = self.0 - xh;
        xh * self.1 + xh * self.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
    }

    #[cfg(feature="enable_recsqrt_sp")]
    #[inline]
    pub fn sqrt(self) -> Self {
        let x = vrecsqrt_vf_vf(self.0 + self.1);
        let r = self * x;
        (r * (r * x + $f32x::splat(-3.0))).scale($f32x::splat(-0.5))
    }
    #[cfg(not(feature="enable_recsqrt_sp"))]
    #[inline]
    pub fn sqrt(self) -> Self {
        let t = vsqrt_vf_vf(self.0 + self.1);
        ((self + t.mul_as_f2(t)) * t.rec()).scale($f32x::splat(0.5))
    }

    #[cfg(feature="enable_fma_dp")]
    #[inline]
    pub fn mul_as_f(self, other: Self) -> $f32x {
        vfma_vf_vf_vf_vf(self.0, other.0, vfma_vf_vf_vf_vf(self.1, other.0, self.0 * other.1))
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    pub fn mul_as_f(self, other: Self) -> $f32x {
        let xh = vupper_vf_vf(self.0);
        let xl = self.0 - xh;
        let yh = vupper_vf_vf(other.0);
        let yl = other.0 - yh;
        self.1 * yh + xh * other.1 + xl * yl + xh * yl + xl * yh + xh * yh
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

impl std::ops::Neg for F2<$f32x> {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        Self::new(-self.0, -self.1)
    }
}

impl std::ops::Add for F2<$f32x> {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        let r0  = self.0 + other.0;
        let v = r0 - self.0;
        Self::new(r0, self.0 - (r0 - v) + (other.0 - v) + (self.1 + other.1) )
    }
}
impl std::ops::AddAssign for F2<$f32x> {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl std::ops::Add<$f32x> for F2<$f32x> {
    type Output = Self;
    #[inline]
    fn add(self, other: $f32x) -> Self {
        let r0 = self.0 + other;
        let v = r0 - self.0;
        Self::new(r0, self.0 - (r0 - v) + (other - v) + self.1 )
    }
}
impl std::ops::AddAssign<$f32x> for F2<$f32x> {
    #[inline]
    fn add_assign(&mut self, other: $f32x) {
        *self = *self + other;
    }
}


impl std::ops::Add<F2<$f32x>> for $f32x {
    type Output = F2<Self>;
    #[inline]
    fn add(self, other: F2<Self>) -> Self::Output {
        let r0  = self + other.0;
        let v = r0 - self;
        F2<Self>::new(r0, self - (r0 - v) + (other.0 - v) + other.1 )
    }
}

impl std::ops::Mul for F2<$f32x> {
    type Output = Self;
    #[cfg(feature="enable_fma_dp")]
    #[inline]
    fn mul(self, other: Self) -> Self {
        let r0 = self.0 * other.0;
        Self::new(r0, vfma_vf_vf_vf_vf(self.0, other.1, vfma_vf_vf_vf_vf(self.1, other.0, vfmapn_vf_vf_vf_vf(self.0, other.0, r0))) )
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    fn mul(self, other: Self) -> Self {
        let xh = vupper_vf_vf(self.0);
        let xl = self.0 - xh;
        let yh = vupper_vf_vf(other.0);
        let yl = other.0 - yh;
        let r0 = self.0 * other.0;

        let mut t = xh.mla(yh, -r0);
        t = xl.mla(yh, t);
        t = xh.mla(yl, t);
        t = xl.mla(yl, t);
        t = self.0.mla(other.1, t);
        t = self.1.mla(other.0, t);
        Self::new(r0, t)
    }
}
impl std::ops::MulAssign for F2<$f32x> {
    #[inline]
    fn mul_assign(&mut self, other: Self) {
        *self = *self * other;
    }
}

impl std::ops::Mul<$f32x> for F2<$f32x> {
    type Output = Self;
    #[cfg(feature="enable_fma_dp")]
    #[inline]
    fn mul(self, other: $f32x) -> Self {
        let r0 = self.0 * other;
        Self::new(r0, vfma_vf_vf_vf_vf(self.1, other, vfmapn_vf_vf_vf_vf(self.0, other, r0)) )
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    fn mul(self, other: $f32x) -> Self {
        let xh = vupper_vf_vf(self.0);
        let xl = self.0 - xh;
        let yh = vupper_vf_vf(other  );
        let yl = other   - yh;
        let r0 = self.0 * other;

        let mut t = xh.mla(yh, -r0);
        t = xl.mla(yh, t);
        t = xh.mla(yl, t);
        t = xl.mla(yl, t);
        t = self.1.mla(other, t);
        Self::new(r0, t )
    }
}
impl std::ops::MulAssign<$f32x> for F2<$f32x> {
    #[inline]
    fn mul_assign(&mut self, other: $f32x) {
        *self = *self * other;
    }
}

impl std::ops::Div for F2<$f32x> {
    type Output = Self;
    #[cfg(feature="enable_fma_dp")]
    #[inline]
    fn div(self, other: Self) -> Self {
        let t = other.0.rec();

        let q0 = self.0 * t;
        let u = vfmapn_vf_vf_vf_vf(t, self.0, q0);
        let mut q1 = vfmanp_vf_vf_vf_vf(other.1, t, vfmanp_vf_vf_vf_vf(other.0, t, $f32x::splat(1)));
        q1 = vfma_vf_vf_vf_vf(q0, q1, vfma_vf_vf_vf_vf(self.1, t, u));

        Self::new(q0, q1)
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    fn div(self, other: Self) -> Self {
        let t = (other.0.rec();
        let dh  = vupper_vf_vf(other.0);
        let dl  = other.0 -  dh;
        let th  = vupper_vf_vf(t  );
        let tl  = t   -  th;
        let nhh = vupper_vf_vf(self.0);
        let nhl = self.0 - nhh;

        let q0 = self.0 * t;

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

        Self::new(q0, t.mla(self.1 - q0 * other.1, u) )
    }
}


impl AddChecked for F2<$f32x> {
    type Output = Self;
    #[inline]
    fn add_checked(self, other: Self) -> Self::Output {
        // |self| >= |other|
        let r0 = self.0 + other.0;
        Self::new(r0, self.0 - r0 + other.0 + self.1 + other.1)
    }
}

impl AddChecked<$f32x> for F2<$f32x> {
    type Output = Self;
    #[inline]
    fn add_checked(self, other: $f32x) -> Self::Output {
        // |self| >= |other|
        let r0 = self.0 + other;
        Self::new(r0, self.0 - r0 + other + self.1 )
    }
}
impl AddCheckedAssign<$f32x> for F2<$f32x> {
    #[inline]
    fn add_checked_assign(&mut self, other: $f32x) {
        *self = (*self).add_checked(other);
    }
}

impl AddChecked<F2<$f32x>> for $f32x {
    type Output = F2<Self>;
    #[inline]
    fn add_checked(self, other: F2<Self>) -> Self::Output {
        let r0 = self + other.0;
        F2<Self>::new(r0, self - r0 + other.0 + other.1)
    }
}

impl AddAsF2 for $f32x {
    #[inline]
    fn add_as_f2(self, other: Self) -> F2<Self> {
        let r0 = self + other;
        let v = r0 - self;
        F2<Self>::new(r0, self - (r0 - v) + (other - v) )
    }
}
impl AddCheckedAsF2 for $f32x {
    #[inline]
    fn add_checked_as_f2(self, other: Self) -> F2<Self> {
        let r0 = self + other,
        F2<Self>::new(r0, self - r0 + other )
    }
}



impl SubChecked for F2<$f32x> {
    type Output = Self;
    #[inline]
    fn sub_checked(self, other: Self) -> Self::Output {
        // |self| >= |other|
        let r0 = self.0 - other.0;
        let mut r1 = self.0 - r0;
        r1 = r1 - other.0;
        r1 = r1 + self.1;
        r1 = r1 - other.1;

        Self::new(r0, r1)
    }
}

impl SubChecked<$f32x> for F2<$f32x> {
    type Output = Self;
    #[inline]
    fn sub_checked(self, other: $f32x) -> Self::Output {
        // |self| >= |other|
        let r0 = self.0 - other;
        Self::new(r0, self.0 - r0 - other + self.1 )
    }
}


impl MulAsF2 for $f32x {
    #[cfg(feature="enable_fma_dp")]
    #[inline]
    fn mul_as_f2(self, other: Self) -> F2<Self> {
        let r0 = self * other;
        Self::new(r0, vfmapn_vf_vf_vf_vf(self, other, r0) )
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    fn mul_as_f2(self, other: Self) -> F2<Self> {
        let xh = vupper_vf_vf(self);
        let xl = self - xh;
        let yh = vupper_vf_vf(other);
        let yl = other - yh;
        let r0 = self * other;

        let mut t = xh.mla(yh, -r0);
        t = xl.mla(yh, t);
        t = xh.mla(yl, t);
        t = xl.mla(yl, t);
        Self::new(r0, t )
    }
}


impl SqrtAsF2 for $f32x {
    #[inline]
    fn sqrt_as_d2(self) -> F2<Self> {
        let t = vsqrt_vf_vf(self);
        ((self + selffmul_vf2_vf_vf(t, t)) * t.rec()).scale(Self::splat(0.5))
    }
}


impl Rec for F2<$f32x> {
    #[cfg(feature="enable_fma_dp")]
    #[inline]
    fn rec(self) -> Self {
        let q0 = self.0.rec();
        Self::new(r0, q0 * vfmanp_vf_vf_vf_vf(self.1, q0, vfmanp_vf_vf_vf_vf(self.0, q0, $f32x::splat(1))) )
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    fn rec(self) -> Self {
        let t = self.0.rec();
        let dh = vupper_vf_vf(self.0);
        let dl = self.0 - dh;
        let th = vupper_vf_vf(t  );
        let tl = t   - th;
        let q0 = t;

        let mut u = $f32x::splat(-1);
        u = dh.mla(th, u);
        u = dh.mla(tl, u);
        u = dl.mla(th, u);
        u = dl.mla(tl, u);
        u = self.1.mla(t, u);
        Self::new(q0, (-t) * u )
    }
}

impl RecAsF2<F2<$f32x>> for $f32x {
    #[cfg(feature="enable_fma_dp")]
    #[inline]
    fn rec_as_f2(self) -> F2<Self> {
        let q0 = self.rec();
        F2<Self>::new(q0, q0 * vfmanp_vf_vf_vf_vf(self, q0, Self::splat(1)) )
    }
    #[cfg(not(feature="enable_fma_dp"))]
    #[inline]
    fn rec_as_f2(self) -> F2<Self> {
        let t = self.rec();
        let dh = vupper_vf_vf(self);
        let dl = self - dh;
        let th = vupper_vf_vf(t);
        let tl = t - th;
        let q0 = t;

        let mut u = Self::splat(-1);
        u = dh.mla(th, u);
        u = dh.mla(tl, u);
        u = dl.mla(th, u);
        u = dl.mla(tl, u);
        F2<Self>::new(q0, (-t) * u)
    }
}



#[inline]
fn vsel_vf2_vo_vf2_vf2(m: $mox, x: F2<$f32x>, y: F2<$f32x>) -> F2<$f32x> {
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
