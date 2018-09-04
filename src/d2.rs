#[inline]
fn vupper_vd_vd(d: $f64x) -> $f64x {
  $f64x::from_bits($u64x::from_bits(d) & $u64x::from_u32((0xffffffff, 0xf8000000)))
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct D2<T>(pub T, pub T);

impl D2<$f64x> {
    pub fn new(x0: $f64x, x1: $f64x) {
      D2(x0, x1)
    }

    #[inline]
    pub fn normalize(self) -> Self {
        let s0 = self.0 + self.1;
        Self::new(s0,  self.0 - s0 + self.1 )
    }

    #[inline]
    pub fn abs(self) -> Self {
        Self::new(self.0.abs(),
            $f64x::from_bits($u64x::from_bits(self.1) ^ ($u64x::from_bits(self.0) & $u64x::from_bits($f64x::splat((-0.)))))
    }

    #[inline]
    pub fn scale(self, other: $f64x) -> Self {
        Self::new(self.0 * other, self.1 * other)
    }

    #[target_feature(enable = "fma")]
    #[inline]
    pub fn square(self) -> Self {
        let r0 = self.0 * self.0;
        Self::new(r0, (self.0 + self.0).mul_adde(self.1, self.0.mul_sube(self.0, r0)) )
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    pub fn square(self) -> Self {
        let xh = vupper_vd_vd(self.0);
        let xl = self.0 - xh;
        let r0 = self.0 * self.0;
        Self::new(r0, xh * xh + (-r0) + (xh + xh) * xl + xl * xl + self.0 * (self.1 + self.1) )
    }

    #[target_feature(enable = "fma")]
    #[inline]
    pub fn square_as_d(self) -> $f64x {
        self.0.mul_adde(self.0, self.0 * self.1 + self.0 * self.1)
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    pub fn square_as_d(self) -> $f64x {
        let xh = vupper_vd_vd(self.0);
        let xl = self.0 - xh;

        xh * self.1 + xh * self.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
    }

    #[inline]
    pub fn sqrt(self) -> Self {
        let t = (self.0 + self.1).sqrt();
        ((d + t.mul_as_d2(t)) * t.recpre_as_d2()).scale($f64x::splat(0.5))
    }

    #[target_feature(enable = "fma")]
    #[inline]
    pub fn mul_as_d(self, other: Self) -> $f64x {
        self.0.mul_adde(other.0, self.1.mul_adde(other.0, self.0 * other.1))
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    pub fn mul_as_d(self, other: Self) -> $f64x {
        let xh = vupper_vd_vd(self.0);
        let xl = self.0 - xh;
        let yh = vupper_vd_vd(other.0);
        let yl = other.0 - yh;

        self.1 * yh + xh * other.1 + xl * yl + xh * yl + xl * yh + xh * yh
    }
  
}

impl std::convert::From<(f64, f64)> for D2<$f64x> {
  fn from(f: (f64, f64)) -> Self {
    Self::new($f64x::splat(f.0), $f64x::splat(f.1))
  }
}


impl std::ops::Neg for D2<$f64x> {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        Self::new(-self.0, -self.1)
    }
}

impl std::ops::Add for D2<$f64x> {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        let r0  = self.0 + other.0;
        let v = r0 - self.0;
        Self::new(r0,  self.0 - (r0 - v) + (other.0 - v) + (self.1 + other.1) )
    }
}
impl std::ops::AddAssign for D2<$f64x> {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl std::ops::Add<$f32x> for D2<$f64x> {
    type Output = Self;
    #[inline]
    fn add(self, other: $f64x) -> Self {
        let r0 = self.0 + other;
        let v = r0 - self.0;
        Self::new(r0, self.0 - (r0 - v) + (other - v) + self.1 )
    }
}
impl std::ops::AddAssign<$f64x> for D2<$f64x> {
    #[inline]
    fn add_assign(&mut self, other: $f64x) {
        *self = *self + other;
    }
}

impl std::ops::Add<D2<$f64x>> for $f64x {
    type Output = D2<Self>;
    #[inline]
    fn add(self, other: D2<Self>) -> Self::Output {
        let r0  = self + other.0;
        let v = r0 - self;
        D2<Self>::new(r0,  self - (r0 - v) + (other.0 - v) + other.1)
    }
}

impl std::ops::Mul for D2<$f64x> {
    type Output = Self;
    #[target_feature(enable = "fma")]
    #[inline]
    fn mul(self, other: Self) -> Self {
        let r0 = self.0 * other.0;
        Self::new(r0, self.0.mul_adde(other.1, self.1.mul_adde(other.0, self.0.mul_sube(other.0, r0))) )
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    fn mul(self, other: Self) -> Self {
        let xh = vupper_vd_vd(self.0);
        let xl = self.0 - xh;
        let yh = vupper_vd_vd(other.0);
        let yl = other.0 - yh;
        let r0 = self.0 * other.0;
        Self::new(r0, xh * yh + (-r0) + xl * yh + xh * yl + xl * yl + self.0 * other.1 + self.1 * other.0 )
    }
}
impl std::ops::MulAssign for D2<$f64x> {
    #[inline]
    fn mul_assign(&mut self, other: Self) {
        *self = *self * other;
    }
}


impl std::ops::Mul<$f64x> for D2<$f64x> {
    type Output = Self;
    #[target_feature(enable = "fma")]
    #[inline]
    fn mul(self, other: $f64x) -> Self {
        let r0 = self.0 * other;
        Self::new(r0, self.1.mul_adde(other, self.0.mul_sube(other, r0)) )
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    fn mul(self, other: $f64x) -> Self {
        let xh = vupper_vd_vd(self.0);
        let xl = self.0 - xh;
        let yh = vupper_vd_vd(other);
        let yl = other   - yh;
        let r0 = self.0 * other;
        Self::new(r0, xh * yh + (-r0) + xl * yh + xh * yl + xl * yl + self.1 * other )
    }
}
impl std::ops::MulAssign<$f64x> for D2<$f64x> {
    #[inline]
    fn mul_assign(&mut self, other: $f64x) {
        *self = *self * other;
    }
}

impl std::ops::Div for D2<$f64x> {
    type Output = Self;
    #[target_feature(enable = "fma")]
    #[inline]
    fn div(self, other: Self) -> Self {
        let t = other.0.recpre();

        let q0 = self.0 * t;
        let u = t.mul_sube(self.0, q0);
        let mut q1 = other.1.fmanp(t, other.0.fmanp(t, $f64x::splat(1.)));
        q1 = q0.mul_adde(q1, self.1.mul_adde(t, u));

        Self::new(q0, q1)
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    fn div(self, other: Self) -> Self {
        let t = other.0.recpre();
        let dh  = vupper_vd_vd(other.0);
        let dl  = other.0 -  dh;
        let th  = vupper_vd_vd(t  );
        let tl  = t   -  th;
        let nhh = vupper_vd_vd(self.0);
        let nhl = self.0 - nhh;

        let q0 = self.0 * t;

        $f64x u = (nhh * th - q0 + nhh * tl + nhl * th + nhl * tl +
              q0 * ($f64x::splat(1) - dh * th - dh * tl - dl * th - dl * tl));

        Self::new(q0, t.mul_add(self.1 - q0 * other.1, u) )
    }
}

impl AddChecked for D2<$f64x> {
    type Output = Self;
    #[inline]
    fn add_checked(self, other: Self) -> Self::Output {
        // |self| >= |other|
        let r0 = self.0 + other.0;
        Self::new(r0,  self.0 - r0 + other.0 + self.1 + other.1 )
    }
}
impl AddCheckedAssign for D2<$f64x> {
    #[inline]
    fn add_checked_assign(&mut self, other: Self) {
        *self = (*self).add_checked(other);
    }
}
impl AddChecked<$f64x> for D2<$f64x> {
    type Output = Self;
    #[inline]
    fn add_checked(self, other: $f32x) -> Self::Output {
        // |self| >= |other|
        let r0 = self.0 + other;
        Self::new(r0, self.0 - r0 + other + self.1 )
    }
}
impl AddCheckedAssign<$f64x> for D2<$f64x> {
    #[inline]
    fn add_checked_assign(&mut self, other: $f32x) {
        *self = (*self).add_checked(other);
    }
}

impl AddChecked<D2<$f64x>> for $f64x {
    type Output = D2<Self>;
    #[inline]
    fn add_checked(self, other: D2<Self>) -> Self::Output {
        let r0 = self + other.0;
        D2<Self>::new(r0, self - r0 + other.0 + other.1 )
    }
}

impl AddAsD2 for $f64x {
    #[inline]
    fn add_as_d2(self, other: Self) -> D2<Self> {
        let r0 = self + other;
        let v = r0 - self;
        D2<Self>::new(r0,  self - (r0 - v) + (other - v) )
    }
}
impl AddCheckedAsD2 for $f64x {
    #[inline]
    fn add_checked_as_d2(self, other: Self) -> D2<Self> {
        let r0 = self + other;
        D2<Self>::new(r0,  self - r0 + other )
    }
}


impl SubChecked for D2<$f64x> {
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

impl SubChecked<$f64x> for D2<$f64x> {
    type Output = Self;
    #[inline]
    fn sub_checked(self, other: $f64x) -> Self::Output {
        // |self| >= |other|
        let r0 = self.0 - other;
        Self::new(r0, self.0 - r0 - other + self.1 )
    }
}

impl MulAsD2 for $f64x {
    #[target_feature(enable = "fma")]
    #[inline]
    fn mul_as_d2(self, other: Self) -> D2<Self> {
        let r0 = self * other;
        D2<Self>::new(r0, self.mul_sube(other, r0) )
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    fn mul_as_d2(self, other: Self) -> D2<Self> {
        let xh = vupper_vd_vd(self);
        let xl = self - xh;
        let yh = vupper_vd_vd(other);
        let yl = other - yh;
        let r0 = self * other;
        D2<Self>::new(r0, xh * yh + (-r0) + xl * yh + xh * yl + xl * yl )
    }
}

impl SqrtAsD2 for $f64x {
    #[inline]
    fn sqrt_as_d2(self) -> D2<Self> {
        let t = d.sqrt();
        ((d + t.mul_as_d2(t)) * t.recpre_as_d2()).scale(Self::splat(0.5))
    }
}


impl Rec for D2<$f64x> {
    #[target_feature(enable = "fma")]
    #[inline]
    fn recpre(self) -> Self {
        let q0 = self.0.recpre();
        Self::new(q0, q0 * self.1.fmanp(q0, self.0.fmanp(q0, $f64x::splat(1))) )
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    fn recpre(self) -> D2<$f64x> {
        let t = self.0.recpre();
        let dh = vupper_vd_vd(self.0);
        let dl = self.0 - dh;
        let th = vupper_vd_vd(t  );
        let tl = t   - th;
        let q0 = t;
        Self::new(r0, t * ($f64x::splat(1) - dh * th - dh * tl - dl * th - dl * tl - self.1 * t) )
    }
}

impl RecAsD2<D2<$f64x> for $f64x {
    #[target_feature(enable = "fma")]
    #[inline]
    fn recpre_as_d2(self) -> D2<Self> {
        let q0 = vrec_vself_vself(self);
        D2<Self>::new(q0, q0 * vfmanp_vself_vself_vself_vself(self, q0, Self::splat(1.)) )
    }
    #[target_feature(not(enable = "fma"))]
    #[inline]
    fn recpre_as_d2(self) -> D2<Self> {
        let t = vrec_vself_vself(self);
        let selfh = vupper_vself_vself(self);
        let selfl = self - selfh;
        let th = vupper_vself_vself(t);
        let tl = t - th;
        let q0 = t;
        D2<Self>::new(r0, t * (Self::splat(1) - selfh * th - selfh * tl - selfl * th - selfl * tl) )
    }
}


#[inline]
fn vsel_vd2_vo_vd2_vd2(m: $ox, x: D2<$f64x>, y: D2<$f64x>) -> D2<$f64x> {
  D2::new(m.select(x.0, y.0), m.select(x.1, y.1))
}

#[inline]
fn vsel_vd2_vo_d_d_d_d(o: $ox, x1: f64, y1: f64, x0: f64, y0: f64) -> D2<$f64x> {
  D2::new(vsel_vd_vo_d_d(o, x1, x0), vsel_vd_vo_d_d(o, y1, y0))
}
