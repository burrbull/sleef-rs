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

pub trait Round: crate::AssociatedInt {
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

pub trait BaseType {
    type Base;
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
pub trait Poly: BaseType + MulAdd
where
    Self: Sized + Copy,
{
    fn c2v(c: Self::Base) -> Self;
    fn poly2(x: Self, c1: Self::Base, c0: Self::Base) -> Self {
        x.mul_add(Poly::c2v(c1), Poly::c2v(c0))
    }
    fn poly3(x: Self, x2: Self, c2: Self::Base, c1: Self::Base, c0: Self::Base) -> Self {
        x2.mul_add(Poly::c2v(c2), x.mul_add(Poly::c2v(c1), Poly::c2v(c0)))
    }
    fn poly4(
        x: Self,
        x2: Self,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
    ) -> Self {
        x2.mul_add(
            x.mul_add(Poly::c2v(c3), Poly::c2v(c2)),
            x.mul_add(Poly::c2v(c1), Poly::c2v(c0)),
        )
    }
    fn poly5(
        x: Self,
        x2: Self,
        x4: Self,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
    ) -> Self {
        x4.mul_add(Poly::c2v(c4), Poly::poly4(x, x2, c3, c2, c1, c0))
    }
    fn poly6(
        x: Self,
        x2: Self,
        x4: Self,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
    ) -> Self {
        x4.mul_add(Poly::poly2(x, c5, c4), Poly::poly4(x, x2, c3, c2, c1, c0))
    }
    fn poly7(
        x: Self,
        x2: Self,
        x4: Self,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
    ) -> Self {
        x4.mul_add(
            Poly::poly3(x, x2, c6, c5, c4),
            Poly::poly4(x, x2, c3, c2, c1, c0),
        )
    }
    fn poly8(
        x: Self,
        x2: Self,
        x4: Self,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        cb: Self::Base,
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        cc: Self::Base,
        cb: Self::Base,
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        cd: Self::Base,
        cc: Self::Base,
        cb: Self::Base,
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        ce: Self::Base,
        cd: Self::Base,
        cc: Self::Base,
        cb: Self::Base,
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        cf: Self::Base,
        ce: Self::Base,
        cd: Self::Base,
        cc: Self::Base,
        cb: Self::Base,
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        d0: Self::Base,
        cf: Self::Base,
        ce: Self::Base,
        cd: Self::Base,
        cc: Self::Base,
        cb: Self::Base,
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        d1: Self::Base,
        d0: Self::Base,
        cf: Self::Base,
        ce: Self::Base,
        cd: Self::Base,
        cc: Self::Base,
        cb: Self::Base,
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
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
        d2: Self::Base,
        d1: Self::Base,
        d0: Self::Base,
        cf: Self::Base,
        ce: Self::Base,
        cd: Self::Base,
        cc: Self::Base,
        cb: Self::Base,
        ca: Self::Base,
        c9: Self::Base,
        c8: Self::Base,
        c7: Self::Base,
        c6: Self::Base,
        c5: Self::Base,
        c4: Self::Base,
        c3: Self::Base,
        c2: Self::Base,
        c1: Self::Base,
        c0: Self::Base,
    ) -> Self {
        x16.mul_add(
            Poly::poly3(x, x2, d2, d1, d0),
            Poly::poly16(
                x, x2, x4, x8, cf, ce, cd, cc, cb, ca, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0,
            ),
        )
    }
}
