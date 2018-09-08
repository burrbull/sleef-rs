pub use common::*;

macro_rules! impl_f2_f32 {
    ($f32x:ident, $u32x:ident, $m32x:ident) => {
        use common::*;

        //----------------------
        #[inline]
        fn vsel_vf_vo_f_f(o: $m32x, v1: f32, v0: f32) -> $f32x {
          o.select($f32x::splat(v1), $f32x::splat(v0))
        }

        #[inline]
        fn vsel_vf_vo_vo_f_f_f(o0: $m32x, o1: $m32x, d0: f32, d1: f32, d2: f32) -> $f32x {
          o0.select($f32x::splat(d0), vsel_vf_vo_f_f(o1, d1, d2))
        }

        #[inline]
        fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $m32x, o1: $m32x, o2: $m32x, d0: f32, d1: f32, d2: f32, d3: f32) -> $f32x {
          o0.select($f32x::splat(d0), o1.select($f32x::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)))
        }
        //---------------------

        #[inline]
        fn vupper_vf_vf(d: $f32x) -> $f32x {
            ($u32x::from_bits(d) & $u32x::splat(0xfffff000)).into_bits()
        }

        impl F2<$f32x> {

            #[inline]
            pub fn normalize(self) -> Self {
                let s0 = self.0 + self.1;
                Self::new(s0, self.0 - s0 + self.1)
            }

            #[inline]
            pub fn abs(self) -> Self {
                Self::new(
                    self.0.abs(),
                    ($u32x::from_bits(self.1)
                            ^ ($u32x::from_bits(self.0) & $u32x::from_bits($f32x::splat(-0.)))
                    ).into_bits()
                )
            }

            #[inline]
            pub fn scale(self, other: $f32x) -> Self {
                Self::new(self.0 * other, self.1 * other)
            }

            #[cfg(target_feature = "fma")]
            #[inline]
            pub fn square(self) -> Self {
                let r0 = self.0 * self.0;
                Self::new(r0, (self.0 + self.0).mul_adde(self.1, self.0.mul_sube(self.0, r0)))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            pub fn square(self) -> Self {
                let xh = vupper_vf_vf(self.0);
                let xl = self.0 - xh;
                let r0 = self.0 * self.0;

                let mut t = xh.mul_add(xh, -r0);
                t = (xh + xh).mul_add(xl, t);
                t = xl.mul_add(xl, t);
                t = self.0.mul_add(self.1 + self.1, t);
                Self::new(r0, t)
            }

            #[cfg(target_feature = "fma")]
            #[inline]
            pub fn square_as_f(self) -> $f32x {
                self.0.mul_adde(self.0, self.0 * self.1 + self.0 * self.1)
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            pub fn square_as_f(self) -> $f32x {
                let xh = vupper_vf_vf(self.0);
                let xl = self.0 - xh;
                xh * self.1 + xh * self.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
            }

            #[cfg(feature = "enable_recsqrt_sp")]
            #[inline]
            pub fn sqrt(self) -> Self {
                let x = (self.0 + self.1).rsqrte();
                let r = self * x;
                (r * (r * x + $f32x::splat(-3.0))).scale($f32x::splat(-0.5))
            }
            #[cfg(not(feature = "enable_recsqrt_sp"))]
            #[inline]
            pub fn sqrt(self) -> Self {
                let t = (self.0 + self.1).sqrt();
                ((self + t.mul_as_f2(t)) * t.recpre()).scale($f32x::splat(0.5))
            }

            #[cfg(target_feature = "fma")]
            #[inline]
            pub fn mul_as_f(self, other: Self) -> $f32x {
                self.0.mul_adde(other.0, self.1.mul_adde(other.0, self.0 * other.1))
            }
            #[cfg(not(target_feature = "fma"))]
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
            fn from(d: f64) -> Self {
                Self::new(
                    $f32x::splat(d as f32),
                    $f32x::splat((d as f32) - (d as f32)),
                )
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
                let r0 = self.0 + other.0;
                let v = r0 - self.0;
                Self::new(r0, self.0 - (r0 - v) + (other.0 - v) + (self.1 + other.1))
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
                Self::new(r0, self.0 - (r0 - v) + (other - v) + self.1)
            }
        }
        impl std::ops::AddAssign<$f32x> for F2<$f32x> {
            #[inline]
            fn add_assign(&mut self, other: $f32x) {
                *self = *self + other;
            }
        }

        impl std::ops::Add<F2<$f32x>> for $f32x {
            type Output = F2<$f32x> ;
            #[inline]
            fn add(self, other: F2<$f32x> ) -> Self::Output {
                let r0 = self + other.0;
                let v = r0 - self;
                F2::new(r0, self - (r0 - v) + (other.0 - v) + other.1)
            }
        }

        impl std::ops::Mul for F2<$f32x> {
            type Output = Self;
            #[cfg(target_feature = "fma")]
            #[inline]
            fn mul(self, other: Self) -> Self {
                let r0 = self.0 * other.0;
                Self::new(
                    r0,
                    self.0
                        .mul_adde(other.1, self.1.mul_adde(other.0, self.0.mul_sube(other.0, r0))),
                )
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn mul(self, other: Self) -> Self {
                let xh = vupper_vf_vf(self.0);
                let xl = self.0 - xh;
                let yh = vupper_vf_vf(other.0);
                let yl = other.0 - yh;
                let r0 = self.0 * other.0;

                let mut t = xh.mul_add(yh, -r0);
                t = xl.mul_add(yh, t);
                t = xh.mul_add(yl, t);
                t = xl.mul_add(yl, t);
                t = self.0.mul_add(other.1, t);
                t = self.1.mul_add(other.0, t);
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
            #[cfg(target_feature = "fma")]
            #[inline]
            fn mul(self, other: $f32x) -> Self {
                let r0 = self.0 * other;
                Self::new(r0, self.1.mul_adde(other, self.0.fmanp(other, r0)))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn mul(self, other: $f32x) -> Self {
                let xh = vupper_vf_vf(self.0);
                let xl = self.0 - xh;
                let yh = vupper_vf_vf(other);
                let yl = other - yh;
                let r0 = self.0 * other;

                let mut t = xh.mul_add(yh, -r0);
                t = xl.mul_add(yh, t);
                t = xh.mul_add(yl, t);
                t = xl.mul_add(yl, t);
                t = self.1.mul_add(other, t);
                Self::new(r0, t)
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
            #[cfg(target_feature = "fma")]
            #[inline]
            fn div(self, other: Self) -> Self {
                let t = other.0.recpre();

                let q0 = self.0 * t;
                let u = t.mul_sube(self.0, q0);
                let mut q1 = other.1.fmanp(t, other.0.fmanp(t, $f32x::splat(1)));
                q1 = q0.mul_adde(q1, self.1.mul_adde(t, u));

                Self::new(q0, q1)
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn div(self, other: Self) -> Self {
                let t = other.0.recpre();
                let dh = vupper_vf_vf(other.0);
                let dl = other.0 - dh;
                let th = vupper_vf_vf(t);
                let tl = t - th;
                let nhh = vupper_vf_vf(self.0);
                let nhl = self.0 - nhh;

                let q0 = self.0 * t;

                let mut w = $f32x::splat(-1.);
                w = dh.mul_add(th, w);
                w = dh.mul_add(tl, w);
                w = dl.mul_add(th, w);
                w = dl.mul_add(tl, w);
                w = -w;

                let mut u = nhh.mul_add(th, -q0);
                u = nhh.mul_add(tl, u);
                u = nhl.mul_add(th, u);
                u = nhl.mul_add(tl, u);
                u = q0.mul_add(w, u);

                Self::new(q0, t.mul_add(self.1 - q0 * other.1, u))
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
                Self::new(r0, self.0 - r0 + other + self.1)
            }
        }
        impl AddCheckedAssign<$f32x> for F2<$f32x> {
            #[inline]
            fn add_checked_assign(&mut self, other: $f32x) {
                *self = (*self).add_checked(other);
            }
        }

        impl AddChecked<F2<$f32x>> for $f32x {
            type Output = F2<$f32x> ;
            #[inline]
            fn add_checked(self, other: F2<$f32x>) -> Self::Output {
                let r0 = self + other.0;
                F2::new(r0, self - r0 + other.0 + other.1)
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
                Self::new(r0, self.0 - r0 - other + self.1)
            }
        }

        impl AsF2 for $f32x {
            #[inline]
            fn add_as_f2(self, other: Self) -> F2<Self> {
                let r0 = self + other;
                let v = r0 - self;
                F2::new(r0, self - (r0 - v) + (other - v))
            }
            #[inline]
            fn add_checked_as_f2(self, other: Self) -> F2<Self> {
                let r0 = self + other;
                F2::new(r0, self - r0 + other)
            }
            #[cfg(target_feature = "fma")]
            #[inline]
            fn mul_as_f2(self, other: Self) -> F2<Self> {
                let r0 = self * other;
                Self::new(r0, self.mul_sube(other, r0))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn mul_as_f2(self, other: Self) -> F2<Self> {
                let xh = vupper_vf_vf(self);
                let xl = self - xh;
                let yh = vupper_vf_vf(other);
                let yl = other - yh;
                let r0 = self * other;

                let mut t = xh.mul_add(yh, -r0);
                t = xl.mul_add(yh, t);
                t = xh.mul_add(yl, t);
                t = xl.mul_add(yl, t);
                F2::new(r0, t)
            }
            #[inline]
            fn sqrt_as_f2(self) -> F2<Self> {
                let t = self.sqrt();
                ((self + t.mul_as_f2(t)) * t.recpre()).scale(Self::splat(0.5))
            }
        }

        impl RecPre for F2<$f32x> {
            #[cfg(target_feature = "fma")]
            #[inline]
            fn recpre(self) -> Self {
                let q0 = self.0.recpre();
                Self::new(q0, q0 * self.1.fmanp(q0, self.0.fmanp(q0, $f32x::splat(1))))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn recpre(self) -> Self {
                let t = self.0.recpre();
                let dh = vupper_vf_vf(self.0);
                let dl = self.0 - dh;
                let th = vupper_vf_vf(t);
                let tl = t - th;
                let q0 = t;

                let mut u = $f32x::splat(-1.);
                u = dh.mul_add(th, u);
                u = dh.mul_add(tl, u);
                u = dl.mul_add(th, u);
                u = dl.mul_add(tl, u);
                u = self.1.mul_add(t, u);
                Self::new(q0, (-t) * u)
            }
        }

        impl RecPreAsF2 for $f32x {
            #[cfg(target_feature = "fma")]
            #[inline]
            fn recpre_as_f2(self) -> F2<Self>  {
                let q0 = self.recpre();
                F2::new(q0, q0 * self.fmanp(q0, Self::splat(1)))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn recpre_as_f2(self) -> F2<Self> {
                let t = self.recpre();
                let dh = vupper_vf_vf(self);
                let dl = self - dh;
                let th = vupper_vf_vf(t);
                let tl = t - th;
                let q0 = t;

                let mut u = Self::splat(-1.);
                u = dh.mul_add(th, u);
                u = dh.mul_add(tl, u);
                u = dl.mul_add(th, u);
                u = dl.mul_add(tl, u);
                F2::new(q0, (-t) * u)
            }
        }

        #[inline]
        fn vsel_vf2_vo_vf2_vf2(m: $m32x, x: F2<$f32x>, y: F2<$f32x>) -> F2<$f32x> {
            F2::new(m.select(x.0, y.0), m.select(x.1, y.1))
        }

        #[inline]
        fn vsel_vf2_vo_f_f_f_f(o: $m32x, x1: f32, y1: f32, x0: f32, y0: f32) -> F2<$f32x> {
            F2::new(vsel_vf_vo_f_f(o, x1, x0), vsel_vf_vo_f_f(o, y1, y0))
        }

        #[inline]
        fn vsel_vf2_vo_vo_d_d_d(o0: $m32x, o1: $m32x, d0: f64, d1: f64, d2: f64) -> F2<$f32x> {
            vsel_vf2_vo_vf2_vf2(
                o0,
                F2::from(d0),
                vsel_vf2_vo_vf2_vf2(o1, F2::from(d1), F2::from(d2)),
            )
        }

        #[inline]
        fn vsel_vf2_vo_vo_vo_d_d_d_d(
            o0: $m32x,
            o1: $m32x,
            o2: $m32x,
            d0: f64,
            d1: f64,
            d2: f64,
            d3: f64,
        ) -> F2<$f32x>  {
            vsel_vf2_vo_vf2_vf2(
                o0,
                F2::from(d0),
                vsel_vf2_vo_vf2_vf2(
                    o1,
                    F2::from(d1),
                    vsel_vf2_vo_vf2_vf2(o2, F2::from(d2), F2::from(d3)),
                ),
            )
        }
    };
}

macro_rules! impl_f2_f64 {
    ($f64x:ident, $u64x:ident, $m64x:ident) => {
        use common::*;

        impl FromU32 for $u64x {
            fn from_u32(i: (u32, u32)) -> Self {
                let mut a = [0_u32; $u64x::lanes() * 2];
                for j in 0..$u64x::lanes() {
                    a[2 * j] = i.0;
                    a[2 * j + 1] = i.1;
                }
                $u64x::from_bits(Simd::<[u32; $u64x::lanes() * 2]>::from_slice_aligned(&a))
            }
        }

        // --------------------
        #[inline]
        fn vsel_vd_vo_d_d(o: $m64x, v1: f64, v0: f64) -> $f64x {
            o.select($f64x::splat(v1), $f64x::splat(v0))
        }

        #[inline]
        fn vsel_vd_vo_vo_d_d_d(o0: $m64x, o1: $m64x, d0: f64, d1: f64, d2: f64) -> $f64x {
            o0.select($f64x::splat(d0), vsel_vd_vo_d_d(o1, d1, d2))
        }

        #[inline]
        fn vsel_vd_vo_vo_vo_d_d_d_d(
            o0: $m64x,
            o1: $m64x,
            o2: $m64x,
            d0: f64,
            d1: f64,
            d2: f64,
            d3: f64,
        ) -> $f64x {
            o0.select(
                $f64x::splat(d0),
                o1.select($f64x::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)),
            )
        }
        // -------------------

        #[inline]
        fn vupper_vd_vd(d: $f64x) -> $f64x {
            $f64x::from_bits($u64x::from_bits(d) & $u64x::from_u32((0xffffffff, 0xf8000000)))
        }

        impl F2<$f64x> {
            #[inline]
            pub fn normalize(self) -> Self {
                let s0 = self.0 + self.1;
                Self::new(s0, self.0 - s0 + self.1)
            }

            #[inline]
            pub fn abs(self) -> Self {
                Self::new(
                    self.0.abs(),
                    $f64x::from_bits(
                        $u64x::from_bits(self.1)
                            ^ ($u64x::from_bits(self.0) & $u64x::from_bits($f64x::splat(-0.))),
                    ),
                )
            }

            #[inline]
            pub fn scale(self, other: $f64x) -> Self {
                Self::new(self.0 * other, self.1 * other)
            }

            #[cfg(target_feature = "fma")]
            #[inline]
            pub fn square(self) -> Self {
                let r0 = self.0 * self.0;
                Self::new(
                    r0,
                    (self.0 + self.0).mul_adde(self.1, self.0.mul_sube(self.0, r0)),
                )
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            pub fn square(self) -> Self {
                let xh = vupper_vd_vd(self.0);
                let xl = self.0 - xh;
                let r0 = self.0 * self.0;
                Self::new(
                    r0,
                    xh * xh + (-r0) + (xh + xh) * xl + xl * xl + self.0 * (self.1 + self.1),
                )
            }

            #[cfg(target_feature = "fma")]
            #[inline]
            pub fn square_as_f(self) -> $f64x {
                self.0.mul_adde(self.0, self.0 * self.1 + self.0 * self.1)
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            pub fn square_as_f(self) -> $f64x {
                let xh = vupper_vd_vd(self.0);
                let xl = self.0 - xh;

                xh * self.1 + xh * self.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
            }

            #[inline]
            pub fn sqrt(self) -> Self {
                let t = (self.0 + self.1).sqrt();
                ((self + t.mul_as_f2(t)) * t.recpre_as_f2()).scale($f64x::splat(0.5))
            }

            #[cfg(target_feature = "fma")]
            #[inline]
            pub fn mul_as_f(self, other: Self) -> $f64x {
                self.0
                    .mul_adde(other.0, self.1.mul_adde(other.0, self.0 * other.1))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            pub fn mul_as_f(self, other: Self) -> $f64x {
                let xh = vupper_vd_vd(self.0);
                let xl = self.0 - xh;
                let yh = vupper_vd_vd(other.0);
                let yl = other.0 - yh;

                self.1 * yh + xh * other.1 + xl * yl + xh * yl + xl * yh + xh * yh
            }
        }

        impl std::convert::From<(f64, f64)> for F2<$f64x> {
            fn from(f: (f64, f64)) -> Self {
                Self::new($f64x::splat(f.0), $f64x::splat(f.1))
            }
        }

        impl std::ops::Neg for F2<$f64x> {
            type Output = Self;
            #[inline]
            fn neg(self) -> Self {
                Self::new(-self.0, -self.1)
            }
        }

        impl std::ops::Add for F2<$f64x> {
            type Output = Self;
            #[inline]
            fn add(self, other: Self) -> Self {
                let r0 = self.0 + other.0;
                let v = r0 - self.0;
                Self::new(r0, self.0 - (r0 - v) + (other.0 - v) + (self.1 + other.1))
            }
        }
        impl std::ops::AddAssign for F2<$f64x> {
            #[inline]
            fn add_assign(&mut self, other: Self) {
                *self = *self + other;
            }
        }

        impl std::ops::Add<$f64x> for F2<$f64x> {
            type Output = Self;
            #[inline]
            fn add(self, other: $f64x) -> Self {
                let r0 = self.0 + other;
                let v = r0 - self.0;
                Self::new(r0, self.0 - (r0 - v) + (other - v) + self.1)
            }
        }
        impl std::ops::AddAssign<$f64x> for F2<$f64x> {
            #[inline]
            fn add_assign(&mut self, other: $f64x) {
                *self = *self + other;
            }
        }

        impl std::ops::Add<F2<$f64x>> for $f64x {
            type Output = F2<Self>;
            #[inline]
            fn add(self, other: F2<Self>) -> Self::Output {
                let r0 = self + other.0;
                let v = r0 - self;
                F2::new(r0, self - (r0 - v) + (other.0 - v) + other.1)
            }
        }

        impl std::ops::Mul for F2<$f64x> {
            type Output = Self;
            #[cfg(target_feature = "fma")]
            #[inline]
            fn mul(self, other: Self) -> Self {
                let r0 = self.0 * other.0;
                Self::new(
                    r0,
                    self.0.mul_adde(
                        other.1,
                        self.1.mul_adde(other.0, self.0.mul_sube(other.0, r0)),
                    ),
                )
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn mul(self, other: Self) -> Self {
                let xh = vupper_vd_vd(self.0);
                let xl = self.0 - xh;
                let yh = vupper_vd_vd(other.0);
                let yl = other.0 - yh;
                let r0 = self.0 * other.0;
                Self::new(
                    r0,
                    xh * yh
                        + (-r0)
                        + xl * yh
                        + xh * yl
                        + xl * yl
                        + self.0 * other.1
                        + self.1 * other.0,
                )
            }
        }
        impl std::ops::MulAssign for F2<$f64x> {
            #[inline]
            fn mul_assign(&mut self, other: Self) {
                *self = *self * other;
            }
        }

        impl std::ops::Mul<$f64x> for F2<$f64x> {
            type Output = Self;
            #[cfg(target_feature = "fma")]
            #[inline]
            fn mul(self, other: $f64x) -> Self {
                let r0 = self.0 * other;
                Self::new(r0, self.1.mul_adde(other, self.0.mul_sube(other, r0)))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn mul(self, other: $f64x) -> Self {
                let xh = vupper_vd_vd(self.0);
                let xl = self.0 - xh;
                let yh = vupper_vd_vd(other);
                let yl = other - yh;
                let r0 = self.0 * other;
                Self::new(
                    r0,
                    xh * yh + (-r0) + xl * yh + xh * yl + xl * yl + self.1 * other,
                )
            }
        }
        impl std::ops::MulAssign<$f64x> for F2<$f64x> {
            #[inline]
            fn mul_assign(&mut self, other: $f64x) {
                *self = *self * other;
            }
        }

        impl std::ops::Div for F2<$f64x> {
            type Output = Self;
            #[cfg(target_feature = "fma")]
            #[inline]
            fn div(self, other: Self) -> Self {
                let t = other.0.recpre();

                let q0 = self.0 * t;
                let u = t.mul_sube(self.0, q0);
                let mut q1 = other.1.fmanp(t, other.0.fmanp(t, $f64x::splat(1.)));
                q1 = q0.mul_adde(q1, self.1.mul_adde(t, u));

                Self::new(q0, q1)
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn div(self, other: Self) -> Self {
                let t = other.0.recpre();
                let dh = vupper_vd_vd(other.0);
                let dl = other.0 - dh;
                let th = vupper_vd_vd(t);
                let tl = t - th;
                let nhh = vupper_vd_vd(self.0);
                let nhl = self.0 - nhh;

                let q0 = self.0 * t;

                let u = nhh * th - q0
                    + nhh * tl
                    + nhl * th
                    + nhl * tl
                    + q0 * ($f64x::splat(1.) - dh * th - dh * tl - dl * th - dl * tl);

                Self::new(q0, t.mul_add(self.1 - q0 * other.1, u))
            }
        }

        impl AddChecked for F2<$f64x> {
            type Output = Self;
            #[inline]
            fn add_checked(self, other: Self) -> Self::Output {
                // |self| >= |other|
                let r0 = self.0 + other.0;
                Self::new(r0, self.0 - r0 + other.0 + self.1 + other.1)
            }
        }
        impl AddCheckedAssign<F2<$f64x>> for F2<$f64x> {
            #[inline]
            fn add_checked_assign(&mut self, other: Self) {
                *self = (*self).add_checked(other);
            }
        }
        impl AddChecked<$f64x> for F2<$f64x> {
            type Output = Self;
            #[inline]
            fn add_checked(self, other: $f64x) -> Self::Output {
                // |self| >= |other|
                let r0 = self.0 + other;
                Self::new(r0, self.0 - r0 + other + self.1)
            }
        }
        impl AddCheckedAssign<$f64x> for F2<$f64x> {
            #[inline]
            fn add_checked_assign(&mut self, other: $f64x) {
                *self = (*self).add_checked(other);
            }
        }

        impl AddChecked<F2<$f64x>> for $f64x {
            type Output = F2<Self>;
            #[inline]
            fn add_checked(self, other: F2<Self>) -> Self::Output {
                let r0 = self + other.0;
                F2::new(r0, self - r0 + other.0 + other.1)
            }
        }

        impl SubChecked for F2<$f64x> {
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

        impl SubChecked<$f64x> for F2<$f64x> {
            type Output = Self;
            #[inline]
            fn sub_checked(self, other: $f64x) -> Self::Output {
                // |self| >= |other|
                let r0 = self.0 - other;
                Self::new(r0, self.0 - r0 - other + self.1)
            }
        }

        impl AsF2 for $f64x {
            #[inline]
            fn add_as_f2(self, other: Self) -> F2<Self> {
                let r0 = self + other;
                let v = r0 - self;
                F2::new(r0, self - (r0 - v) + (other - v))
            }
            #[inline]
            fn add_checked_as_f2(self, other: Self) -> F2<Self> {
                let r0 = self + other;
                F2::new(r0, self - r0 + other)
            }
            #[cfg(target_feature = "fma")]
            #[inline]
            fn mul_as_f2(self, other: Self) -> F2<Self> {
                let r0 = self * other;
                F2::new(r0, self.mul_sube(other, r0))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn mul_as_f2(self, other: Self) -> F2<Self> {
                let xh = vupper_vd_vd(self);
                let xl = self - xh;
                let yh = vupper_vd_vd(other);
                let yl = other - yh;
                let r0 = self * other;
                F2::new(r0, xh * yh + (-r0) + xl * yh + xh * yl + xl * yl)
            }
            #[inline]
            fn sqrt_as_f2(self) -> F2<Self> {
                let t = self.sqrt();
                ((self + t.mul_as_f2(t)) * t.recpre_as_f2()).scale(Self::splat(0.5))
            }
        }

        impl RecPre for F2<$f64x> {
            #[cfg(target_feature = "fma")]
            #[inline]
            fn recpre(self) -> Self {
                let q0 = self.0.recpre();
                Self::new(q0, q0 * self.1.fmanp(q0, self.0.fmanp(q0, $f64x::splat(1))))
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn recpre(self) -> F2<$f64x> {
                let t = self.0.recpre();
                let dh = vupper_vd_vd(self.0);
                let dl = self.0 - dh;
                let th = vupper_vd_vd(t);
                let tl = t - th;
                let q0 = t;
                Self::new(
                    q0,
                    t * ($f64x::splat(1.) - dh * th - dh * tl - dl * th - dl * tl - self.1 * t),
                )
            }
        }

        impl RecPreAsF2 for $f64x {
            #[cfg(target_feature = "fma")]
            #[inline]
            fn recpre_as_f2(self) -> F2<Self> {
                let q0 = self.recpre();
                F2::new(q0, q0 * (self, q0, Self::splat(1.)).fmanp())
            }
            #[cfg(not(target_feature = "fma"))]
            #[inline]
            fn recpre_as_f2(self) -> F2<Self> {
                let t = self.recpre();
                let dh = vupper_vd_vd(self);
                let dl = self - dh;
                let th = vupper_vd_vd(t);
                let tl = t - th;
                let q0 = t;
                F2::new(
                    q0,
                    t * (Self::splat(1.) - dh * th - dh * tl - dl * th - dl * tl),
                )
            }
        }

        #[inline]
        fn vsel_vd2_vo_vd2_vd2(m: $m64x, x: F2<$f64x>, y: F2<$f64x>) -> F2<$f64x> {
            F2::new(m.select(x.0, y.0), m.select(x.1, y.1))
        }

        #[inline]
        fn vsel_vd2_vo_d_d_d_d(o: $m64x, x1: f64, y1: f64, x0: f64, y0: f64) -> F2<$f64x> {
            F2::new(vsel_vd_vo_d_d(o, x1, x0), vsel_vd_vo_d_d(o, y1, y0))
        }
    };
}

impl F2<f32> {
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
        ((self + t.mul_as_f2(t)) * t.recpre()).scale(0.5)
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

impl std::ops::Neg for F2<f32> {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        Self::new(-self.0, -self.1)
    }
}

impl std::ops::Add for F2<f32> {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        let r0 = self.0 + other.0;
        let v = r0 - self.0;
        Self::new(r0, (self.0 - (r0 - v)) + (other.0 - v) + self.1 + other.1) // [self.0+other.0, self.1+other.1]
    }
}
impl std::ops::AddAssign for F2<f32> {
    #[inline]
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl std::ops::Add<f32> for F2<f32> {
    type Output = Self;
    #[inline]
    fn add(self, other: f32) -> Self {
        // |self| >= |other|
        let r0 = self.0 + other;
        let v = r0 - self.0; // == other
        Self::new(r0, (self.0 - (r0 - v)) + (other - v) + self.1) // [self.0+other, self.1]
    }
}
impl std::ops::AddAssign<f32> for F2<f32> {
    #[inline]
    fn add_assign(&mut self, other: f32) {
        *self = *self + other;
    }
}

impl std::ops::Add<F2<f32>> for f32 {
    type Output = F2<f32>;
    #[inline]
    fn add(self, other: F2<f32>) -> Self::Output {
        let r0 = self + other.0;
        let v = r0 - self; // == other.0
        F2::new(r0, (self - (r0 - v)) + (other.0 - v) + other.1) // [other.0+self, other.1]
    }
}

impl std::ops::Mul for F2<f32> {
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
impl std::ops::MulAssign for F2<f32> {
    #[inline]
    fn mul_assign(&mut self, other: Self) {
        *self = *self * other;
    }
}

impl std::ops::Mul<f32> for F2<f32> {
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
impl std::ops::MulAssign<f32> for F2<f32> {
    #[inline]
    fn mul_assign(&mut self, other: f32) {
        *self = *self * other;
    }
}

impl std::ops::Div for F2<f32> {
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

impl AddChecked for F2<f32> {
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

impl AddChecked<f32> for F2<f32> {
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
impl AddCheckedAssign<f32> for F2<f32> {
    #[inline]
    fn add_checked_assign(&mut self, other: f32) {
        *self = (*self).add_checked(other);
    }
}

impl AddChecked<F2<f32>> for f32 {
    type Output = F2<f32>;
    #[inline]
    fn add_checked(self, other: F2<f32>) -> Self::Output {
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

impl SubChecked for F2<f32> {
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
        F2::new(r0, self.0 - r0 - other.0 + self.1 - other.1) // [self.0-other.0, self.1-other.1]
    }
}

impl AsF2 for f32 {
    #[inline]
    fn add_as_f2(self, other: Self) -> F2<Self> {
        let r0 = self + other;
        let v = r0 - self; // = other
        F2::new(r0, (self - (r0 - v)) + (other - v)) // [self+other, 0.]
    }
    #[inline]
    fn add_checked_as_f2(self, other: Self) -> F2<Self> {
        // |self| >= |other|
        debug_assert!(
            self.check() || other.check() || fabsfk(self) >= fabsfk(other),
            "[dfadd_f2_f_f : {:e}, {:e}]",
            self,
            other
        );
        let r0 = self + other;
        F2::new(r0, self - r0 + other) // [self+other, 0.]
    }
    #[inline]
    fn mul_as_f2(self, other: Self) -> F2<Self> {
        let xh = upperf(self);
        let xl = self - xh;
        let yh = upperf(other);
        let yl = other - yh;
        let r0 = self * other;
        F2::new(r0, xh * yh - r0 + xl * yh + xh * yl + xl * yl)
    }
    #[inline]
    fn sqrt_as_f2(self) -> F2<Self> {
        let t = SQRTF(self);
        ((self + t.mul_as_f2(t)) * t.recpre()).scale(0.5)
    }
}

impl RecPre for F2<f32> {
    fn recpre(self) -> Self {
        let t = 1. / self.0;
        let dh = upperf(self.0);
        let dl = self.0 - dh;
        let th = upperf(t);
        let tl = t - th;
        F2::new(
            t,
            t * (1. - dh * th - dh * tl - dl * th - dl * tl - self.1 * t),
        )
    }
}

impl RecPre<F2<f32>> for f32 {
    fn recpre(self) -> F2<f32> {
        let t = 1. / self;
        let dh = upperf(self);
        let dl = self - dh;
        let th = upperf(t);
        let tl = t - th;
        F2::new(t, t * (1. - dh * th - dh * tl - dl * th - dl * tl))
    }
}

impl F2<f64> {
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
    pub fn square_as_f(self) -> f64 {
        let xh = upper(self.0);
        let xl = self.0 - xh;
        xh * self.1 + xh * self.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
    }

    #[inline]
    pub fn sqrt(self) -> Self {
        let t = SQRT(self.0 + self.1);
        ((self + t.mul_as_f2(t)) * t.recpre()).scale(0.5)
    }

    #[inline]
    pub fn mul_as_f(self, other: Self) -> f64 {
        let xh = upper(self.0);
        let xl = self.0 - xh;
        let yh = upper(other.0);
        let yl = other.0 - yh;
        self.1 * yh + xh * other.1 + xl * yl + xh * yl + xl * yh + xh * yh
    }
}

impl std::ops::Neg for F2<f64> {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        Self::new(-self.0, -self.1)
    }
}

impl std::ops::Add for F2<f64> {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        let r0 = self.0 + other.0;
        let v = r0 - self.0;
        Self::new(r0, (self.0 - (r0 - v)) + (other.0 - v) + self.1 + other.1) // [self.0+other.0, self.1+other.1]
    }
}

impl std::ops::AddAssign for F2<f64> {
    #[inline]
    fn add_assign(&mut self, other: F2<f64>) {
        *self = *self + other;
    }
}

impl std::ops::Add<f64> for F2<f64> {
    type Output = Self;
    #[inline]
    fn add(self, other: f64) -> Self {
        let r0 = self.0 + other;
        let v = r0 - self.0; // == other
        Self::new(r0, (self.0 - (r0 - v)) + (other - v) + self.1) // [self.0+other, self.1]
    }
}

impl std::ops::AddAssign<f64> for F2<f64> {
    #[inline]
    fn add_assign(&mut self, other: f64) {
        *self = *self + other;
    }
}

impl std::ops::Add<F2<f64>> for f64 {
    type Output = F2<f64>;
    #[inline]
    fn add(self, other: F2<f64>) -> Self::Output {
        let r0 = self + other.0;
        let v = r0 - self; // == other.0
        F2::new(r0, (self - (r0 - v)) + (other.0 - v) + other.1) // [other.0+self, other.1]
    }
}

impl std::ops::Mul for F2<f64> {
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

impl std::ops::MulAssign for F2<f64> {
    #[inline]
    fn mul_assign(&mut self, other: F2<f64>) {
        *self = *self * other;
    }
}

impl std::ops::Mul<f64> for F2<f64> {
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

impl std::ops::MulAssign<f64> for F2<f64> {
    #[inline]
    fn mul_assign(&mut self, other: f64) {
        *self = *self * other;
    }
}

impl std::ops::Div for F2<f64> {
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

impl AddChecked for F2<f64> {
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

impl AddChecked<f64> for F2<f64> {
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

impl AddCheckedAssign<f64> for F2<f64> {
    #[inline]
    fn add_checked_assign(&mut self, other: f64) {
        *self = (*self).add_checked(other);
    }
}

impl AddChecked<F2<f64>> for f64 {
    type Output = F2<f64>;
    #[inline]
    fn add_checked(self, other: F2<f64>) -> Self::Output {
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
        F2::new(r0, self - r0 + other.0 + other.1) // [other.0+self, other.1]
    }
}

impl SubChecked for F2<f64> {
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

impl AsF2 for f64 {
    #[inline]
    fn add_as_f2(self, other: Self) -> F2<Self> {
        let r0 = self + other;
        let v = r0 - self;
        F2::new(r0, (self - (r0 - v)) + (other - v))
    }
    #[inline]
    fn add_checked_as_f2(self, other: Self) -> F2<Self> {
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
        F2::new(r0, self - r0 + other)
    }
    #[inline]
    fn mul_as_f2(self, other: Self) -> F2<Self> {
        let xh = upper(self);
        let xl = self - xh;
        let yh = upper(other);
        let yl = other - yh;
        let r0 = self * other;
        F2::new(r0, xh * yh - r0 + xl * yh + xh * yl + xl * yl)
    }
    #[inline]
    fn sqrt_as_f2(self) -> F2<Self> {
        let t = SQRT(self);
        ((self + t.mul_as_f2(t)) * t.recpre()).scale(0.5)
    }
}

impl RecPre for F2<f64> {
    fn recpre(self) -> F2<f64> {
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

impl RecPre<F2<f64>> for f64 {
    fn recpre(self) -> F2<f64> {
        let t = 1. / self;
        let dh = upper(self);
        let dl = self - dh;
        let th = upper(t);
        let tl = t - th;
        let q0 = t;
        F2::new(q0, t * (1. - dh * th - dh * tl - dl * th - dl * tl))
    }
}

#[inline]
pub fn df(h: f32, l: f32) -> F2<f32> {
    F2::new(h, l)
}

#[inline]
pub fn dd(h: f64, l: f64) -> F2<f64> {
    F2::new(h, l)
}
