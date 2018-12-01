pub use crate::consts::*;
pub use core::f32;
pub use core::f64;
pub use core::i32;
pub use core::i64;

// ---- Advanced Traits -----------------

use doubled::*;

pub trait SqrtAsDoubled where Self: Sized {
    fn sqrt_as_doubled(self) -> Doubled<Self>;
}

impl SqrtAsDoubled for f32 {
    #[inline]
    fn sqrt_as_doubled(self) -> Doubled<Self> {
        let t = self.sqrt();
        ((self + t.mul_as_doubled(t)) * t.recpre()).scale(0.5)
    }
}

impl SqrtAsDoubled for f64 {
    #[inline]
    fn sqrt_as_doubled(self) -> Doubled<Self> {
        let t = self.sqrt();
        ((self + t.mul_as_doubled(t)) * t.recpre()).scale(0.5)
    }
}

pub trait Sqrt {
    fn sqrt(self) -> Self;
}

impl Sqrt for f32 {
    #[inline]
    fn sqrt(self) -> Self {
        crate::f32::u05::sqrtf(self)
    }
}

impl Sqrt for f64 {
    #[inline]
    fn sqrt(self) -> Self {
        crate::f64::u05::sqrt(self)
    }
}


impl Sqrt for Doubled<f32> {
    #[inline]
    fn sqrt(self) -> Self {
        let t = (self.0 + self.1).sqrt();
        ((self + t.mul_as_doubled(t)) * t.recpre()).scale(0.5)
    }
}

impl Sqrt for Doubled<f64> {
    #[inline]
    fn sqrt(self) -> Self {
        let t = (self.0 + self.1).sqrt();
        ((self + t.mul_as_doubled(t)) * t.recpre()).scale(0.5)
    }
}

pub trait Round {
    type Int;

    fn truncate(self) -> Self;

    fn truncatei(self) -> Self::Int;

    fn rint(self) -> Self;

    fn rinti(self) -> Self::Int;
}

pub trait MulAdd {
    fn mul_add(self, y: Self, z: Self) -> Self;
}


pub trait MulSub {
    fn mul_sub(self, y: Self, z: Self) -> Self;
}

impl MulAdd for f32 {
    #[inline]
    fn mul_add(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}

impl MulAdd for f64 {
    #[inline]
    fn mul_add(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}
