use std;
pub use consts::*;
pub use std::f32;
pub use std::i32;
pub use std::f64;
pub use std::i64;


// ------------------

pub fn SQRTF(x: f32) -> f32 {
    x.sqrt()
}
//---------------------------------------------

// ------------------

pub fn SQRT(x: f64) -> f64 {
    x.sqrt()
}
//---------------------------------------------

pub(crate) const M_1_PI_F: f32 = M_1_PI as f32;
pub(crate) const M_2_PI_F: f32 = M_2_PI as f32;
pub(crate) const M_PI_2_F: f32 = (M_PI / 2.) as f32;
pub(crate) const M_PI_4_F: f32 = (M_PI / 4.) as f32;

pub(crate) const F1_32: f32 = (1u64 << 32) as f32;
pub(crate) const F1_30: f32 = (1u32 << 30) as f32;
pub(crate) const F1_25: f32 = (1u32 << 25) as f32;
pub(crate) const F1_24: f32 = (1u32 << 24) as f32;
pub(crate) const F1_23: f32 = (1u32 << 23) as f32;
pub(crate) const F1_12: f32 = (1u32 << 12) as f32;
pub(crate) const F1_10: f32 = (1u32 << 10) as f32;


pub(crate) const D1_63: f64 = (1u64 << 63) as f64;
pub(crate) const D1_60: f64 = (1u64 << 60) as f64;
pub(crate) const D1_54: f64 = (1u64 << 54) as f64;
pub(crate) const D1_53: f64 = (1u64 << 53) as f64;
pub(crate) const D1_52: f64 = (1u64 << 52) as f64;
pub(crate) const D1_32: f64 = (1u64 << 32) as f64;
pub(crate) const D1_31: f64 = (1u64 << 31) as f64;
pub(crate) const D1_28: f64 = (1u64 << 28) as f64;
pub(crate) const D1_24: f64 = (1u64 << 24) as f64;
pub(crate) const D1_23: f64 = (1u64 << 23) as f64;

#[inline]
pub(crate) fn dd(h: f64, l: f64) -> f64n2 {
    f64n2::new(h, l)
}


#[inline]
pub(crate) fn df(h: f32, l: f32) -> f32n2 {
    f32n2::new(h, l)
}

#[inline]
fn upperf(d: f32) -> f32 {
    return int_bits_to_float(float_to_raw_int_bits(d) & 0xfffff000);
}


#[inline]
pub(crate) fn float_to_raw_int_bits(d: f32) -> i32 {
    d.to_bits() as i32
}

#[inline]
pub(crate) fn int_bits_to_float(i: i32) -> f32 {
    f32::from_bits(i as u32)
}

#[inline]
pub(crate) fn fabsfk(x: f32) -> f32 {
    int_bits_to_float(0x7fffffff & float_to_raw_int_bits(x))
}
#[inline]
pub(crate) fn xisintf(x: f32) -> bool {
    x == (x as i32 as f32)
}

#[inline]
pub(crate) fn xisnanf(x: f32) -> bool {
    x != x
}
#[inline]
pub(crate) fn xisinff(x: f32) -> bool {
    (x == SLEEF_INFINITY_F) || (x == -SLEEF_INFINITY_F)
}
/*#[inline]
pub(crate) fn xisminff(x: f32) -> bool {
    x == -SLEEF_INFINITY_F
}*/
/*#[inline]
pub(crate) fn xispinff(x: f32) -> bool {
    x == SLEEF_INFINITY_F
}*/
#[inline]
pub(crate) fn xisnegzerof(x: f32) -> bool {
    float_to_raw_int_bits(x) == float_to_raw_int_bits(-0.0)
}
#[inline]
pub(crate) fn xisnumberf(x: f32) -> bool {
    !xisinff(x) && !xisnanf(x)
}

#[inline]
pub(crate) fn mulsignf(x: f32, y: f32) -> f32 {
    int_bits_to_float(float_to_raw_int_bits(x) ^ (float_to_raw_int_bits(y) & (1 << 31)))
}

#[inline]
pub(crate) fn copysignfk(x: f32, y: f32) -> f32 {
    int_bits_to_float(
        (float_to_raw_int_bits(x) & !(1 << 31)) ^ (float_to_raw_int_bits(y) & (1 << 31)),
    )
}

#[inline]
pub(crate) fn signf(d: f32) -> f32 {
    mulsignf(1., d)
}
#[inline]
pub(crate) fn mlaf(x: f32, y: f32, z: f32) -> f32 {
    x * y + z
}


#[inline]
fn upper(d: f64) -> f64 {
    return long_bits_to_double(double_to_raw_long_bits(d) & 0xfffffffff8000000);
}


#[inline]
pub(crate) fn double_to_raw_long_bits(d: f64) -> i64 {
    d.to_bits() as i64
}

#[inline]
pub(crate) fn long_bits_to_double(i: i64) -> f64 {
    f64::from_bits(i as u64)
}

#[inline]
pub(crate) fn fabsk(x: f64) -> f64 {
    long_bits_to_double(0x7fffffffffffffff & double_to_raw_long_bits(x))
}


#[inline]
pub(crate) fn xisnan(x: f64) -> bool {
    x != x
}
#[inline]
pub(crate) fn xisinf(x: f64) -> bool {
    (x == SLEEF_INFINITY) || (x == -SLEEF_INFINITY)
}
/*#[inline]
pub(crate) fn xisminf(x: f64) -> bool {
    (x == -SLEEF_INFINITY)
}*/
/*#[inline]
pub(crate) fn xispinf(x: f64) -> bool {
    (x == SLEEF_INFINITY)
}*/
#[inline]
pub(crate) fn xisnegzero(x: f64) -> bool {
    double_to_raw_long_bits(x) == double_to_raw_long_bits(-0.)
}
#[inline]
pub(crate) fn xisnumber(x: f64) -> bool {
    !xisinf(x) && !xisnan(x)
}

#[inline]
pub(crate) fn xisint(d: f64) -> bool {
    let x = d - D1_31 * ((d * (1. / D1_31)) as isize as f64);
    (x == x as isize as f64) || (fabsk(d) >= D1_53)
}

#[inline]
pub(crate) fn xisodd(d: f64) -> bool {
    let x = d - D1_31 * ((d * (1. / D1_31)) as isize as f64);
    ((1 & (x as isize)) != 0) && (fabsk(d) < D1_53)
}

#[inline]
pub(crate) fn mulsign(x: f64, y: f64) -> f64 {
    long_bits_to_double(double_to_raw_long_bits(x) ^ (double_to_raw_long_bits(y) & (1 << 63)))
}

#[inline]
pub(crate) fn copysignk(x: f64, y: f64) -> f64 {
    long_bits_to_double((double_to_raw_long_bits(x) & !(1 << 63)) ^ (double_to_raw_long_bits(y) & (1 << 63)))
}

#[inline]
pub(crate) fn sign(d: f64) -> f64 {
    mulsign(1., d)
}
#[inline]
pub(crate) fn mla(x: f64, y: f64, z: f64) -> f64 {
    x * y + z
}

// ---- Advanced Traits -----------------

pub trait Mla {
    fn mla(self, y: Self, z: Self) -> Self;
}

trait Check {
    fn check(self) -> bool;
}

pub trait AddChecked<T = Self> {
    type Output;
    fn add_checked(self, other: T) -> Self::Output;
}

pub trait AddCheckedAssign<T> {
    fn add_checked_assign(&mut self, other: T);
}

pub trait AddAsF2 {
    fn add_as_f2(self, other: f32) -> f32n2;
}

pub trait AddCheckedAsF2 {
    fn add_checked_as_f2(self, other: f32) -> f32n2;
}

pub trait SubChecked<T = Self> {
    type Output;
    fn sub_checked(self, other: T) -> Self::Output;
}

pub trait MulAsF2 {
    fn mul_as_f2(self, other: Self) -> f32n2;
}

pub trait SqrtAsF2 {
    fn sqrt_as_f2(self) -> f32n2;
}

pub trait Rec<T=Self> {
    fn rec(self) -> T;
}


// ---- Advanced Traits -----------------

pub trait AddAsD2 {
    fn add_as_d2(self, other: f64) -> f64n2;
}

pub trait AddCheckedAsD2 {
    fn add_checked_as_d2(self, other: f64) -> f64n2;
}

pub trait MulAsD2 {
    fn mul_as_d2(self, other: Self) -> f64n2;
}

pub trait SqrtAsD2 {
    fn sqrt_as_d2(self) -> f64n2;
}

//  --------------------

impl Mla for f32 {
    fn mla(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}

impl Mla for f64 {
    fn mla(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}
impl Check for f32 {
    fn check(self) -> bool {
        xisinff(self) || xisnanf(self)
    }
}
impl Check for f64 {
    fn check(self) -> bool {
        xisinf(self) || xisnan(self)
    }
}


#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq)]
pub struct f32n2(pub f32, pub f32);
impl f32n2 {
    #[inline]
    pub fn new(x0: f32, x1: f32) -> Self {
        f32n2(x0, x1)
    }

    #[inline]
    pub fn normalize(self) -> Self {
        let x = self.0 + self.1;
        Self::new(x, self.0 - x + self.1) // [t.0+t.1, 0.]
    }

    #[inline]
    pub fn abs(self) -> Self {
        Self::new(
            if self.0 < 0. { -self.0 } else { self.0 },
            if self.0 < 0. { -self.1 } else { self.1 },
        )
    }

    #[inline]
    pub fn scale(self, other: f32) -> Self {
        Self::new(self.0 * other, self.1 * other)
    }

    #[inline]
    pub fn square(self) -> Self {
        let xh = upperf(self.0);
        let xl = self.0 - xh;
        let r0 = self.0 * self.0;
        Self::new(
            r0,
            xh * xh - r0 + (xh + xh) * xl + xl * xl + self.0 * (self.1 + self.1),
        )
    }

    #[inline]
    pub fn square_as_f(self) -> f32 {
        let xh = upperf(self.0);
        let xl = self.0 - xh;
        xh * self.1 + xh * self.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
    }

    #[inline]
    pub fn sqrt(self) -> Self {
        let t = SQRTF(self.0 + self.1);
        ((self + t.mul_as_f2(t)) * t.rec()).scale(0.5)
    }

    #[inline]
    pub fn mul_as_f(self, other: Self) -> f32 {
        let xh = upperf(self.0);
        let xl = self.0 - xh;
        let yh = upperf(other.0);
        let yl = other.0 - yh;
        self.1 * yh + xh * other.1 + xl * yl + xh * yl + xl * yh + xh * yh
    }
}

impl std::ops::Neg for f32n2 {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        Self::new(-self.0, -self.1)
    }
}

impl std::ops::Add for f32n2 {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        let r0 = self.0 + other.0;
        let v = r0 - self.0;
        Self::new(r0, (self.0 - (r0 - v)) + (other.0 - v) + self.1 + other.1) // [self.0+other.0, self.1+other.1]
    }
}

impl std::ops::AddAssign for f32n2 {
    #[inline]
    fn add_assign(&mut self, other: f32n2) {
        *self = *self + other;
    }
}

impl std::ops::Add<f32> for f32n2 {
    type Output = Self;
    #[inline]
    fn add(self, other: f32) -> Self {
        // |self| >= |other|
        let r0 = self.0 + other;
        let v = r0 - self.0; // == other
        Self::new(r0, (self.0 - (r0 - v)) + (other - v) + self.1) // [self.0+other, self.1]
    }
}

impl std::ops::AddAssign<f32> for f32n2 {
    #[inline]
    fn add_assign(&mut self, other: f32) {
        *self = *self + other;
    }
}

impl std::ops::Add<f32n2> for f32 {
    type Output = f32n2;
    #[inline]
    fn add(self, other: f32n2) -> Self::Output {
        let r0 = self + other.0;
        let v = r0 - self; // == other.0
        f32n2::new(r0, (self - (r0 - v)) + (other.0 - v) + other.1) // [other.0+self, other.1]
    }
}

impl std::ops::Mul for f32n2 {
    type Output = Self;
    #[inline]
    fn mul(self, other: Self) -> Self {
        let xh = upperf(self.0);
        let xl = self.0 - xh;
        let yh = upperf(other.0);
        let yl = other.0 - yh;
        let r0 = self.0 * other.0;
        Self::new(
            r0,
            xh * yh - r0 + xl * yh + xh * yl + xl * yl + self.0 * other.1 + self.1 * other.0,
        )
    }
}

impl std::ops::MulAssign for f32n2 {
    #[inline]
    fn mul_assign(&mut self, other: f32n2) {
        *self = *self * other;
    }
}

impl std::ops::Mul<f32> for f32n2 {
    type Output = Self;
    #[inline]
    fn mul(self, other: f32) -> Self {
        let xh = upperf(self.0);
        let xl = self.0 - xh;
        let yh = upperf(other);
        let yl = other - yh;
        let r0 = self.0 * other;
        Self::new(
            r0,
            xh * yh - r0 + xl * yh + xh * yl + xl * yl + self.1 * other,
        )
    }
}

impl std::ops::MulAssign<f32> for f32n2 {
    #[inline]
    fn mul_assign(&mut self, other: f32) {
        *self = *self * other;
    }
}

impl std::ops::Div for f32n2 {
    type Output = Self;
    #[inline]
    fn div(self, other: Self) -> Self {
        let t = 1. / other.0;
        let dh = upperf(other.0);
        let dl = other.0 - dh;
        let th = upperf(t);
        let tl = t - th;
        let nhh = upperf(self.0);
        let nhl = self.0 - nhh;

        let q0 = self.0 * t;

        let u = -q0
            + nhh * th
            + nhh * tl
            + nhl * th
            + nhl * tl
            + q0 * (1. - dh * th - dh * tl - dl * th - dl * tl);

        Self::new(q0, t * (self.1 - q0 * other.1) + u)
    }
}

impl AddChecked for f32n2 {
    type Output = Self;
    #[inline]
    fn add_checked(self, other: Self) -> Self::Output {
        // |self| >= |other|
        debug_assert!(
            self.0.check() || other.0.check() || fabsfk(self.0) >= fabsfk(other.0),
            "[dfadd_f2_f2_f2 : {:e} {:e}]",
            self.0,
            other.0
        );
        let r0 = self.0 + other.0;
        Self::new(r0, self.0 - r0 + other.0 + self.1 + other.1) // [self.0+other.0, self.1+other.1]
    }
}

impl AddChecked<f32> for f32n2 {
    type Output = Self;
    #[inline]
    fn add_checked(self, other: f32) -> Self::Output {
        // |self| >= |other|
        debug_assert!(
            self.0.check() || other.check() || fabsfk(self.0) >= fabsfk(other),
            "[dfadd_f2_f2_f : {:e}, {:e}]",
            self.0,
            other
        );
        let r0 = self.0 + other;
        Self::new(r0, self.0 - r0 + other + self.1) // [self.0+y, self.1]
    }
}

impl AddCheckedAssign<f32> for f32n2 {
    #[inline]
    fn add_checked_assign(&mut self, other: f32) {
        *self = (*self).add_checked(other);
    }
}

impl AddChecked<f32n2> for f32 {
    type Output = f32n2;
    #[inline]
    fn add_checked(self, other: f32n2) -> Self::Output {
        // |self| >= |other|
        debug_assert!(
            self.check() || other.0.check() || fabsfk(self) >= fabsfk(other.0),
            "[dfadd_f2_f_f2 : {:e}, {:e}]",
            self,
            other.0
        );
        let r0 = self + other.0;
        Self::Output::new(r0, self - r0 + other.0 + other.1) // [other.0+self, other.1]
    }
}

impl AddAsF2 for f32 {
    #[inline]
    fn add_as_f2(self, other: f32) -> f32n2 {
        let r0 = self + other;
        let v = r0 - self; // = other
        f32n2::new(r0, (self - (r0 - v)) + (other - v)) // [self+other, 0.]
    }
}

impl AddCheckedAsF2 for f32 {
    #[inline]
    fn add_checked_as_f2(self, other: f32) -> f32n2 {
        // |self| >= |other|
        debug_assert!(
            self.check() || other.check() || fabsfk(self) >= fabsfk(other),
            "[dfadd_f2_f_f : {:e}, {:e}]",
            self,
            other
        );
        let r0 = self + other;
        f32n2::new(r0, self - r0 + other) // [self+other, 0.]
    }
}

impl SubChecked for f32n2 {
    type Output = Self;
    #[inline]
    fn sub_checked(self, other: Self) -> Self::Output {
        // |self| >= |other|
        debug_assert!(
            self.0.check() || other.0.check() || fabsfk(self.0) >= fabsfk(other.0),
            "[dfsub_f2_f2_f2 : {:e} {:e}]",
            self.0,
            other.0
        );
        let r0 = self.0 - other.0;
        f32n2::new(r0, self.0 - r0 - other.0 + self.1 - other.1) // [self.0-other.0, self.1-other.1]
    }
}

impl MulAsF2 for f32 {
    #[inline]
    fn mul_as_f2(self, other: Self) -> f32n2 {
        let xh = upperf(self);
        let xl = self - xh;
        let yh = upperf(other);
        let yl = other - yh;
        let r0 = self * other;
        f32n2::new(r0, xh * yh - r0 + xl * yh + xh * yl + xl * yl)
    }
}

impl SqrtAsF2 for f32 {
    #[inline]
    fn sqrt_as_f2(self) -> f32n2 {
        let t = SQRTF(self);
        ((self + t.mul_as_f2(t)) * t.rec()).scale(0.5)
    }
}

impl Rec for f32n2 {
    fn rec(self) -> f32n2 {
        let t = 1. / self.0;
        let dh = upperf(self.0);
        let dl = self.0 - dh;
        let th = upperf(t);
        let tl = t - th;
        f32n2::new(
            t,
            t * (1. - dh * th - dh * tl - dl * th - dl * tl - self.1 * t),
        )
    }
}

impl Rec<f32n2> for f32 {
    fn rec(self) -> f32n2 {
        let t = 1. / self;
        let dh = upperf(self);
        let dl = self - dh;
        let th = upperf(t);
        let tl = t - th;
        f32n2::new(t, t * (1. - dh * th - dh * tl - dl * th - dl * tl))
    }
}




#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq)]
pub struct f64n2(pub f64, pub f64);
impl f64n2 {
    #[inline]
    pub fn new(x0: f64, x1: f64) -> Self {
        f64n2(x0, x1)
    }

    #[inline]
    pub fn normalize(self) -> Self {
        let s0 = self.0 + self.1;
        Self::new(s0, self.0 - s0 + self.1)
    }

    #[inline]
    pub fn abs(self) -> Self {
        Self::new(
            if self.0 < 0. { -self.0 } else { self.0 },
            if self.0 < 0. { -self.1 } else { self.1 },
        )
    }

    #[inline]
    pub fn scale(self, other: f64) -> Self {
        Self::new(self.0 * other, self.1 * other)
    }

    #[inline]
    pub fn square(self) -> Self {
        let xh = upper(self.0);
        let xl = self.0 - xh;
        let r0 = self.0 * self.0;
        Self::new(
            r0,
            xh * xh - r0 + (xh + xh) * xl + xl * xl + self.0 * (self.1 + self.1),
        )
    }

    #[inline]
    pub fn square_as_d(self) -> f64 {
        let xh = upper(self.0);
        let xl = self.0 - xh;
        xh * self.1 + xh * self.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
    }

    #[inline]
    pub fn sqrt(self) -> Self {
        let t = SQRT(self.0 + self.1);
        ((self + t.mul_as_d2(t)) * t.rec()).scale(0.5)
    }

    #[inline]
    pub fn mul_as_d(self, other: Self) -> f64 {
        let xh = upper(self.0);
        let xl = self.0 - xh;
        let yh = upper(other.0);
        let yl = other.0 - yh;
        self.1 * yh + xh * other.1 + xl * yl + xh * yl + xl * yh + xh * yh
    }
}

impl std::ops::Neg for f64n2 {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        Self::new(-self.0, -self.1)
    }
}

impl std::ops::Add for f64n2 {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        let r0 = self.0 + other.0;
        let v = r0 - self.0;
        Self::new(r0, (self.0 - (r0 - v)) + (other.0 - v) + self.1 + other.1) // [self.0+other.0, self.1+other.1]
    }
}

impl std::ops::AddAssign for f64n2 {
    #[inline]
    fn add_assign(&mut self, other: f64n2) {
        *self = *self + other;
    }
}

impl std::ops::Add<f64> for f64n2 {
    type Output = Self;
    #[inline]
    fn add(self, other: f64) -> Self {
        let r0 = self.0 + other;
        let v = r0 - self.0; // == other
        Self::new(r0, (self.0 - (r0 - v)) + (other - v) + self.1) // [self.0+other, self.1]
    }
}

impl std::ops::AddAssign<f64> for f64n2 {
    #[inline]
    fn add_assign(&mut self, other: f64) {
        *self = *self + other;
    }
}

impl std::ops::Add<f64n2> for f64 {
    type Output = f64n2;
    #[inline]
    fn add(self, other: f64n2) -> Self::Output {
        let r0 = self + other.0;
        let v = r0 - self; // == other.0
        f64n2::new(r0, (self - (r0 - v)) + (other.0 - v) + other.1) // [other.0+self, other.1]
    }
}

impl std::ops::Mul for f64n2 {
    type Output = Self;
    #[inline]
    fn mul(self, other: Self) -> Self {
        let xh = upper(self.0);
        let xl = self.0 - xh;
        let yh = upper(other.0);
        let yl = other.0 - yh;
        let r0 = self.0 * other.0;
        Self::new(
            r0,
            xh * yh - r0 + xl * yh + xh * yl + xl * yl + self.0 * other.1 + self.1 * other.0,
        )
    }
}

impl std::ops::MulAssign for f64n2 {
    #[inline]
    fn mul_assign(&mut self, other: f64n2) {
        *self = *self * other;
    }
}

impl std::ops::Mul<f64> for f64n2 {
    type Output = Self;
    #[inline]
    fn mul(self, other: f64) -> Self {
        let xh = upper(self.0);
        let xl = self.0 - xh;
        let yh = upper(other);
        let yl = other - yh;
        let r0 = self.0 * other;
        Self::new(
            r0,
            xh * yh - r0 + xl * yh + xh * yl + xl * yl + self.1 * other,
        )
    }
}

impl std::ops::MulAssign<f64> for f64n2 {
    #[inline]
    fn mul_assign(&mut self, other: f64) {
        *self = *self * other;
    }
}

impl std::ops::Div for f64n2 {
    type Output = Self;
    #[inline]
    fn div(self, other: Self) -> Self {
        let t = 1. / other.0;
        let dh = upper(other.0);
        let dl = other.0 - dh;
        let th = upper(t);
        let tl = t - th;
        let nhh = upper(self.0);
        let nhl = self.0 - nhh;

        let q0 = self.0 * t;

        let u = -q0
            + nhh * th
            + nhh * tl
            + nhl * th
            + nhl * tl
            + q0 * (1. - dh * th - dh * tl - dl * th - dl * tl);

        Self::new(q0, t * (self.1 - q0 * other.1) + u)
    }
}


impl AddChecked for f64n2 {
    type Output = Self;
    #[inline]
    fn add_checked(self, other: Self) -> Self::Output {
        // |self| >= |other|
        debug_assert!(
            self.0.check()
                || other.0.check()
                || fabsk(self.0) >= fabsk(other.0)
                || ((fabsk(self.0 + other.0) <= fabsk(self.0))
                    && (fabsk(self.0 + other.0) <= fabsk(other.0))),
            "[ddadd_d2_d2_d2 : {:e} {:e}]\n",
            self.0,
            other.0
        );
        let r0 = self.0 + other.0;
        Self::new(r0, self.0 - r0 + other.0 + self.1 + other.1) // [self.0+other.0, self.1+other.1]
    }
}

impl AddChecked<f64> for f64n2 {
    type Output = Self;
    #[inline]
    fn add_checked(self, other: f64) -> Self::Output {
        // |self| >= |other|
        debug_assert!(
            self.0.check()
                || other.check()
                || fabsk(self.0) >= fabsk(other)
                || ((fabsk(self.0 + other) <= fabsk(self.0))
                    && (fabsk(self.0 + other) <= fabsk(other))),
            "[ddadd_d2_d2_d : {:e} {:e}]\n",
            self.0,
            other
        );
        let r0 = self.0 + other;
        Self::new(r0, self.0 - r0 + other + self.1)
    }
}

impl AddCheckedAssign<f64> for f64n2 {
    #[inline]
    fn add_checked_assign(&mut self, other: f64) {
        *self = (*self).add_checked(other);
    }
}

impl AddChecked<f64n2> for f64 {
    type Output = f64n2;
    #[inline]
    fn add_checked(self, other: f64n2) -> Self::Output {
        // |self| >= |other|
        debug_assert!(
            self.check()
                || other.0.check()
                || fabsk(self) >= fabsk(other.0)
                || ((fabsk(self + other.0) <= fabsk(self))
                    && (fabsk(self + other.0) <= fabsk(other.0))),
            "[ddadd_d2_d_d2 : {:e} {:e}]\n",
            self,
            other.0
        );
        let r0 = self + other.0;
        f64n2::new(r0, self - r0 + other.0 + other.1) // [other.0+self, other.1]
    }
}

impl AddAsD2 for f64 {
    #[inline]
    fn add_as_d2(self, other: f64) -> f64n2 {
        let r0 = self + other;
        let v = r0 - self;
        f64n2::new(r0, (self - (r0 - v)) + (other - v))
    }
}

impl AddCheckedAsD2 for f64 {
    #[inline]
    fn add_checked_as_d2(self, other: f64) -> f64n2 {
        // |self| >= |other|
        debug_assert!(
            self.check()
                || other.check()
                || fabsk(self) >= fabsk(other)
                || ((fabsk(self + other) <= fabsk(self)) && (fabsk(self + other) <= fabsk(other))),
            "[ddadd_d2_d_d : {:e}, {:e}]\n",
            self,
            other
        );
        let r0 = self + other;
        f64n2::new(r0, self - r0 + other)
    }
}

impl SubChecked for f64n2 {
    type Output = Self;
    #[inline]
    fn sub_checked(self, other: Self) -> Self::Output {
        // |self| >= |other|
        debug_assert!(
            self.0.check()
                || other.0.check()
                || fabsk(self.0) >= fabsk(other.0)
                || ((fabsk(self.0 - other.0) <= fabsk(self.0))
                    && (fabsk(self.0 - other.0) <= fabsk(other.0))),
            "[ddsub_d2_d2_d2 : {:e} {:e}]\n",
            self.0,
            other.0
        );
        let r0 = self.0 - other.0;
        Self::new(r0, self.0 - r0 - other.0 + self.1 - other.1) // [self.0-other.0, self.1-other.1]
    }
}

impl MulAsD2 for f64 {
    #[inline]
    fn mul_as_d2(self, other: Self) -> f64n2 {
        let xh = upper(self);
        let xl = self - xh;
        let yh = upper(other);
        let yl = other - yh;
        let r0 = self * other;
        f64n2::new(r0, xh * yh - r0 + xl * yh + xh * yl + xl * yl)
    }
}

impl SqrtAsD2 for f64 {
    #[inline]
    fn sqrt_as_d2(self) -> f64n2 {
        let t = SQRT(self);
        ((self + t.mul_as_d2(t)) * t.rec()).scale(0.5)
    }
}

impl Rec for f64n2 {
    fn rec(self) -> f64n2 {
        let t = 1. / self.0;
        let dh = upper(self.0);
        let dl = self.0 - dh;
        let th = upper(t);
        let tl = t - th;
        let q0 = t;
        Self::new(
            q0,
            t * (1. - dh * th - dh * tl - dl * th - dl * tl - self.1 * t),
        )
    }
}

impl Rec<f64n2> for f64 {
    fn rec(self) -> f64n2 {
        let t = 1. / self;
        let dh = upper(self);
        let dl = self - dh;
        let th = upper(t);
        let tl = t - th;
        let q0 = t;
        f64n2::new(q0, t * (1. - dh * th - dh * tl - dl * th - dl * tl))
    }
}
