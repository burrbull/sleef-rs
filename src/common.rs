pub use consts::*;
use std;
pub use std::f32;
pub use std::f64;
pub use std::i32;
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

pub(crate) const LOG10_2_F: f32 = LOG10_2 as f32;

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
pub(crate) fn upperf(d: f32) -> f32 {
    int_bits_to_float(float_to_raw_int_bits(d) & 0xfffff000)
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


impl IsInf for f32 {
  type Mask = bool;
  #[inline]
  fn isinf(self) -> Self::Mask {
     (self == SLEEF_INFINITY_F) || (self == -SLEEF_INFINITY_F)
  }
  #[inline]
  fn ispinf(self) -> Self::Mask {
    self == SLEEF_INFINITY_F
  }
}
impl IsNan for f32 {
  type Mask = bool;
  #[inline]
  fn isnan(self) -> Self::Mask {
    self != self
  }
}

#[inline]
pub(crate) fn xisintf(x: f32) -> bool {
    x == (x as i32 as f32)
}

#[inline]
pub(crate) fn xisnegzerof(x: f32) -> bool {
    float_to_raw_int_bits(x) == float_to_raw_int_bits(-0.0)
}
#[inline]
pub(crate) fn xisnumberf(x: f32) -> bool {
    !x.isinf() && !x.isnan()
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
pub(crate) fn upper(d: f64) -> f64 {
    long_bits_to_double(double_to_raw_long_bits(d) & 0xfffffffff8000000)
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

impl IsInf for f64 {
  type Mask = bool;
  #[inline]
  fn isinf(self) -> Self::Mask {
     (self == SLEEF_INFINITY) || (self == -SLEEF_INFINITY)
  }
  #[inline]
  fn ispinf(self) -> Self::Mask {
    self == SLEEF_INFINITY
  }
}
impl IsNan for f64 {
  type Mask = bool;
  #[inline]
  fn isnan(self) -> Self::Mask {
    self != self
  }
}

#[inline]
pub(crate) fn xisnegzero(x: f64) -> bool {
    double_to_raw_long_bits(x) == double_to_raw_long_bits(-0.)
}
#[inline]
pub(crate) fn xisnumber(x: f64) -> bool {
    !x.isinf() && !x.isnan()
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
    long_bits_to_double(
        (double_to_raw_long_bits(x) & !(1 << 63)) ^ (double_to_raw_long_bits(y) & (1 << 63)),
    )
}

#[inline]
pub(crate) fn sign(d: f64) -> f64 {
    mulsign(1., d)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct F2<T>(pub T, pub T);

impl<T> F2<T> where T: std::marker::Sized {
    #[inline]
    pub fn new(x0: T, x1: T) -> Self {
        F2(x0, x1)
    }
}

// ---- Advanced Traits -----------------

pub trait IsInf {
  type Mask;
  fn isinf(self) -> Self::Mask;
  fn ispinf(self) -> Self::Mask;
}
pub trait IsNan {
  type Mask;
  fn isnan(self) -> Self::Mask;
}

pub trait Round {
    type Int;
    
    fn truncate(self) -> Self;
    
    fn truncatei(self) -> Self::Int;
    
    fn rint(self) -> Self;
    
    fn rinti(self) -> Self::Int;
}

pub trait AsF2
    where Self: std::marker::Sized {
    fn add_as_f2(self, other: Self) -> F2<Self>;
    
    fn add_checked_as_f2(self, other: Self) -> F2<Self>;
    
    fn mul_as_f2(self, other: Self) -> F2<Self>;
    
    fn sqrt_as_f2(self) -> F2<Self>;
}
pub trait RecPreAsF2
    where Self: std::marker::Sized {
    fn recpre_as_f2(self) -> F2<Self>;
}

pub trait MulAdd {
    fn mul_add(self, y: Self, z: Self) -> Self;
}

pub trait Check {
    fn check(self) -> bool;
}

pub trait AddChecked<T = Self> {
    type Output;
    fn add_checked(self, other: T) -> Self::Output;
}

pub trait AddCheckedAssign<T> {
    fn add_checked_assign(&mut self, other: T);
}

pub trait SubChecked<T = Self> {
    type Output;
    fn sub_checked(self, other: T) -> Self::Output;
}

pub trait RecPre<T = Self> {
    fn recpre(self) -> T;
}

impl MulAdd for f32 {
    fn mul_add(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}

impl MulAdd for f64 {
    fn mul_add(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}
impl Check for f32 {
    fn check(self) -> bool {
        self.isinf() || self.isnan()
    }
}
impl Check for f64 {
    fn check(self) -> bool {
        self.isinf() || self.isnan()
    }
}
