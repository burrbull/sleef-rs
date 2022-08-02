pub use core::f32;
pub use core::f64;
pub use core::i32;
pub use core::i64;

// ---- Advanced Traits -----------------

use doubled::*;

pub trait SqrtAsDoubled
where
    Self: Sized,
{
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

pub(crate) trait Sqrt {
    fn sqrt(self) -> Self;
}

impl Sqrt for f32 {
    #[inline]
    fn sqrt(self) -> Self {
        crate::f32::sqrt_u05(self)
    }
}

impl Sqrt for f64 {
    #[inline]
    fn sqrt(self) -> Self {
        crate::f64::sqrt_u05(self)
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
    fn trunc(self) -> Self;

    fn trunci(self) -> Self::Int;

    fn round(self) -> Self;

    fn roundi(self) -> Self::Int;
}

pub trait MulAdd {
    fn mul_add(self, y: Self, z: Self) -> Self;
}

pub trait MulSub {
    fn mul_sub(self, y: Self, z: Self) -> Self;
}

pub trait NegMulAdd {
    fn neg_mul_add(self, y: Self, z: Self) -> Self;
}

pub trait VectorizedSelect<T> {
    type Output;
    fn select_splat(self, l: T, r: T) -> Self::Output;
}

pub trait DoubledSelect<T>
where
    T: core::marker::Sized,
{
    fn select_doubled(self, l: Doubled<T>, r: Doubled<T>) -> Doubled<T>;
}

pub trait Sign: MaskType + BitsType {
    fn is_sign_negative(self) -> Self::Mask;
    fn is_sign_positive(self) -> Self::Mask;
    fn sign_bit(self) -> Self::Bits;
    fn sign(self) -> Self;
    fn mul_sign(self, other: Self) -> Self;
    fn copy_sign(self, other: Self) -> Self;
}

pub trait IsInt: MaskType {
    fn is_integer(self) -> Self::Mask;
}

pub trait IsOdd: MaskType {
    fn is_odd(self) -> Self::Mask;
}

pub trait IsNegZero: MaskType {
    fn is_neg_zero(self) -> Self::Mask;
}

pub trait MaskType {
    type Mask;
}

pub trait BitsType {
    type Bits;
}

pub trait SelectSeveral<T>: MaskType {
    fn select3(o0: Self::Mask, o1: Self::Mask, d0: T, d1: T, d2: T) -> Self;
    fn select4(o0: Self::Mask, o1: Self::Mask, o2: Self::Mask, d0: T, d1: T, d2: T, d3: T) -> Self;
}

#[allow(clippy::too_many_arguments)]
pub trait Poly<B>: MulAdd
where
    Self: Sized + Copy,
{
    fn c2v(c: B) -> Self;
    fn poly2(x: Self, c1: B, c0: B) -> Self {
        x.mul_add(Poly::c2v(c1), Poly::c2v(c0))
    }
    fn poly3(x: Self, x2: Self, c2: B, c1: B, c0: B) -> Self {
        x2.mul_add(Poly::c2v(c2), x.mul_add(Poly::c2v(c1), Poly::c2v(c0)))
    }
    fn poly4(x: Self, x2: Self, c3: B, c2: B, c1: B, c0: B) -> Self {
        x2.mul_add(
            x.mul_add(Poly::c2v(c3), Poly::c2v(c2)),
            x.mul_add(Poly::c2v(c1), Poly::c2v(c0)),
        )
    }
    fn poly5(x: Self, x2: Self, x4: Self, c4: B, c3: B, c2: B, c1: B, c0: B) -> Self {
        x4.mul_add(Poly::c2v(c4), Poly::poly4(x, x2, c3, c2, c1, c0))
    }
    fn poly6(x: Self, x2: Self, x4: Self, c5: B, c4: B, c3: B, c2: B, c1: B, c0: B) -> Self {
        x4.mul_add(Poly::poly2(x, c5, c4), Poly::poly4(x, x2, c3, c2, c1, c0))
    }
    fn poly7(x: Self, x2: Self, x4: Self, c6: B, c5: B, c4: B, c3: B, c2: B, c1: B, c0: B) -> Self {
        x4.mul_add(
            Poly::poly3(x, x2, c6, c5, c4),
            Poly::poly4(x, x2, c3, c2, c1, c0),
        )
    }
    fn poly8(
        x: Self,
        x2: Self,
        x4: Self,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x4.mul_add(
            Poly::poly4(x, x2, c7, c6, c5, c4),
            Poly::poly4(x, x2, c3, c2, c1, c0),
        )
    }
    fn poly9(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x8.mul_add(
            Poly::c2v(c8),
            Poly::poly8(x, x2, x4, c7, c6, c5, c4, c3, c2, c1, c0),
        )
    }
    fn poly10(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x8.mul_add(
            Poly::poly2(x, c9, c8),
            Poly::poly8(x, x2, x4, c7, c6, c5, c4, c3, c2, c1, c0),
        )
    }
    fn poly11(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x8.mul_add(
            Poly::poly3(x, x2, ca, c9, c8),
            Poly::poly8(x, x2, x4, c7, c6, c5, c4, c3, c2, c1, c0),
        )
    }
    fn poly12(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x8.mul_add(
            Poly::poly4(x, x2, cb, ca, c9, c8),
            Poly::poly8(x, x2, x4, c7, c6, c5, c4, c3, c2, c1, c0),
        )
    }
    fn poly13(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x8.mul_add(
            Poly::poly5(x, x2, x4, cc, cb, ca, c9, c8),
            Poly::poly8(x, x2, x4, c7, c6, c5, c4, c3, c2, c1, c0),
        )
    }
    fn poly14(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        cd: B,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x8.mul_add(
            Poly::poly6(x, x2, x4, cd, cc, cb, ca, c9, c8),
            Poly::poly8(x, x2, x4, c7, c6, c5, c4, c3, c2, c1, c0),
        )
    }
    fn poly15(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        ce: B,
        cd: B,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x8.mul_add(
            Poly::poly7(x, x2, x4, ce, cd, cc, cb, ca, c9, c8),
            Poly::poly8(x, x2, x4, c7, c6, c5, c4, c3, c2, c1, c0),
        )
    }
    fn poly16(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        cf: B,
        ce: B,
        cd: B,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x8.mul_add(
            Poly::poly8(x, x2, x4, cf, ce, cd, cc, cb, ca, c9, c8),
            Poly::poly8(x, x2, x4, c7, c6, c5, c4, c3, c2, c1, c0),
        )
    }
    fn poly17(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        x16: Self,
        d0: B,
        cf: B,
        ce: B,
        cd: B,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x16.mul_add(
            Poly::c2v(d0),
            Poly::poly16(
                x, x2, x4, x8, cf, ce, cd, cc, cb, ca, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0,
            ),
        )
    }
    fn poly18(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        x16: Self,
        d1: B,
        d0: B,
        cf: B,
        ce: B,
        cd: B,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x16.mul_add(
            Poly::poly2(x, d1, d0),
            Poly::poly16(
                x, x2, x4, x8, cf, ce, cd, cc, cb, ca, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0,
            ),
        )
    }
    fn poly19(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        x16: Self,
        d2: B,
        d1: B,
        d0: B,
        cf: B,
        ce: B,
        cd: B,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x16.mul_add(
            Poly::poly3(x, x2, d2, d1, d0),
            Poly::poly16(
                x, x2, x4, x8, cf, ce, cd, cc, cb, ca, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0,
            ),
        )
    }
    fn poly20(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        x16: Self,
        d3: B,
        d2: B,
        d1: B,
        d0: B,
        cf: B,
        ce: B,
        cd: B,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x16.mul_add(
            Poly::poly4(x, x2, d3, d2, d1, d0),
            Poly::poly16(
                x, x2, x4, x8, cf, ce, cd, cc, cb, ca, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0,
            ),
        )
    }
    fn poly21(
        x: Self,
        x2: Self,
        x4: Self,
        x8: Self,
        x16: Self,
        d4: B,
        d3: B,
        d2: B,
        d1: B,
        d0: B,
        cf: B,
        ce: B,
        cd: B,
        cc: B,
        cb: B,
        ca: B,
        c9: B,
        c8: B,
        c7: B,
        c6: B,
        c5: B,
        c4: B,
        c3: B,
        c2: B,
        c1: B,
        c0: B,
    ) -> Self {
        x16.mul_add(
            Poly::poly5(x, x2, x4, d4, d3, d2, d1, d0),
            Poly::poly16(
                x, x2, x4, x8, cf, ce, cd, cc, cb, ca, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0,
            ),
        )
    }
}
