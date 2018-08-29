

#[inline]
fn dfnormalize_f2_f2(t: f32n2) -> f32n2 { // Self::normalize
    let x = t.0 + t.1;
    f32n2::new(x, t.0 - x + t.1) // [t.0+t.1, 0.]
}

#[inline]
fn dfabs_f2_f2(x: f32n2) -> f32n2 { // Self::abs
    df(
        if x.0 < 0. { -x.0 } else { x.0 },
        if x.0 < 0. { -x.1 } else { x.1 },
    )
}

#[inline]
fn dfscale_f2_f2_f(d : f32n2, s : f32) -> f32n2 { // Self::scale
  f32n2::new(d.0*s, d.1*s)
}

#[inline]
fn dfsqu_f2_f2(x: f32n2) -> f32n2 { // Self::square
    let xh = upperf(x.0);
    let xl = x.0 - xh;
    let r0 = x.0 * x.0;
    f32n2::new(
        r0,
        xh * xh - r0 + (xh + xh) * xl + xl * xl + x.0 * (x.1 + x.1),
    )
}

#[inline]
fn dfsqu_f_f2(x: f32n2) -> f32 { // Self::square_as_f
    let xh = upperf(x.0);
    let xl = x.0 - xh;
    xh * x.1 + xh * x.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
}

#[inline]
fn dfsqrt_f2_f2(d: f32n2) -> f32n2 { // Self::sqrt
    let t = SQRTF(d.0 + d.1);
    ((d + t.mul_as_f2(t)) * t.rec()).scale(0.5)
}

// -----------------------------------------------------------
// -----------------------------------------------------------

#[inline]
fn dfneg_f2_f2(d:f32n2) -> f32n2 { // std::ops::Neg for f32n2
  f32n2::new(-d.0, -d.1)
}

#[inline]
fn dfadd2_f2_f2_f2(x : f32n2, y : f32n2) -> f32n2 { // std::ops::Add for f32n2, std::ops::AddAssign for f32n2 `+`, `+=`
  let r0  = x.0 + y.0;
  let v = r0 - x.0;
  f32n2::new(r0, (x.0 - (r0 - v)) + (y.0 - v) + x.1 + y.1); // [x.0+y.0, x.1+y.1]
}

#[inline]
fn dfadd2_f2_f2_f(x: f32n2, y: f32) -> f32n2 { // std::ops::Add<f32> for f32n2, std::ops::AddAssign<f32> for f32n2 `+`, `+=`
    // |x| >= |y|
    let r0 = x.0 + y;
    let v = r0 - x.0; // == y
    f32n2::new(r0, (x.0 - (r0 - v)) + (y - v) + x.1) // [x.0+y, x.1]
}

#[inline]
fn dfadd2_f2_f_f2(x: f32, y: f32n2) -> f32n2 { // std::ops::Add<f32n2> for f32 `+`
    let r0 = x + y.0;
    let v = r0 - x; // == y.0
    f32n2::new(r0, (x - (r0 - v)) + (y.0 - v) + y.1) // [y.0+x, y.1]
}

#[inline]
fn dfmul_f2_f2_f2(x: f32n2, y: f32n2) -> f32n2 { // std::ops::Mul for f32n2, std::ops::MulAssign for f32n2 `*`, `*=`
    let xh = upperf(x.0);
    let xl = x.0 - xh;
    let yh = upperf(y.0);
    let yl = y.0 - yh;
    let r0 = x.0 * y.0;
    f32n2::new(
        r0,
        xh * yh - r0 + xl * yh + xh * yl + xl * yl + x.0 * y.1 + x.1 * y.0,
    )
}

#[inline]
fn dfmul_f2_f2_f(x: f32n2, y: f32) -> f32n2 { // std::ops::Mul<f32> for f32n2, std::ops::MulAssign<f32> for f32n2 `*`, `*=`
    let xh = upperf(x.0);
    let xl = x.0 - xh;
    let yh = upperf(y);
    let yl = y - yh;
    let r0 = x.0 * y;
    f32n2::new(r0, xh * yh - r0 + xl * yh + xh * yl + xl * yl + x.1 * y)
}

#[inline]
fn dfdiv_f2_f2_f2(n: f32n2, d: f32n2) -> f32n2 { // std::ops::Div for f32n2 `/`
    let t = 1. / d.0;
    let dh = upperf(d.0);
    let dl = d.0 - dh;
    let th = upperf(t);
    let tl = t - th;
    let nhh = upperf(n.0);
    let nhl = n.0 - nhh;

    let q0 = n.0 * t;

    let u = -q0
        + nhh * th
        + nhh * tl
        + nhl * th
        + nhl * tl
        + q0 * (1. - dh * th - dh * tl - dl * th - dl * tl);

    f32n2::new(q0, t * (n.1 - q0 * d.1) + u)
}

// -----------------------------------------------------------
// -----------------------------------------------------------

#[inline]
fn dfadd_f2_f2_f2(x : f32n2, y : f32n2) -> f32n2 { // AddChecked for f32n2 .add_checked(()
  // |x| >= |y|
  debug_assert!(checkfp(x.0) || checkfp(y.0) || fabsfk(x.0) >= fabsfk(y.0), "[dfadd_f2_f2_f2 : %g %g]", x.0, y.0);
  let r0 = x.0 + y.0;
  f32n2::new(r0, x.0 - r0 + y.0 + x.1 + y.1); // [x.0+y.0, x.1+y.1]
}

#[inline]
fn dfadd_f2_f2_f(x: f32n2, y: f32) -> f32n2 { // AddChecked<f32> for f32n2, AddCheckedAssign<f32> for f32n2 .add_checked()
    // |x| >= |y|
    debug_assert!(
        checkfp(x.0) || checkfp(y) || fabsfk(x.0) >= fabsfk(y),
        "[dfadd_f2_f2_f : {:e}, {:e}]",
        x.0,
        y
    );
    let r0 = x.0 + y;
    f32n2::new(r0, x.0 - r0 + y + x.1) // [x.0+y, x.1]
}


#[inline]
fn dfadd_f2_f_f2(x: f32, y: f32n2) -> f32n2 { // AddChecked<f32n2> for f32 .add_checked()
    // |x| >= |y|
    debug_assert!(
        checkfp(x) || checkfp(y.0) || fabsfk(x) >= fabsfk(y.0),
        "[dfadd_f2_f_f2 : {:e}, {:e}]",
        x,
        y.0
    );
    let r0 = x + y.0;
    f32n2::new(r0, x - r0 + y.0 + y.1) // [y.0+x, y.1]
}

#[inline]
fn dfadd2_f2_f_f(x: f32, y: f32) -> f32n2 { // AddAsF2 for f32 .add_as_f2()
    let r0 = x + y;
    let v = r0 - x; // = y
    f32n2::new(r0, (x - (r0 - v)) + (y - v)) // [x+y, 0.]
}

#[inline]
fn dfadd_f2_f_f(x: f32, y: f32) -> f32n2 { // AddCheckedAsF2 for f32 .add_as_f2_checked()
    // |x| >= |y|
    debug_assert!(
        checkfp(x) || checkfp(y) || fabsfk(x) >= fabsfk(y),
        "[dfadd_f2_f_f : {:e}, {:e}]",
        x,
        y
    );
    let r0 = x + y;
    f32n2::new(r0, x - r0 + y) // [x+y, 0.]
}

#[inline]
fn dfsub_f2_f2_f2(x : f32n2, y : f32n2) -> f32n2 { // SubChecked for f32n2 .sub_checked()
  // |x| >= |y|
  debug_assert!(checkfp(x.0) || checkfp(y.0) || fabsfk(x.0) >= fabsfk(y.0), "[dfsub_f2_f2_f2 : %g %g]", x.0, y.0);
  let r0 = x.0 - y.0;
  f32n2::new(r0, x.0 - r.0 - y.0 + x.1 - y.1); // [x.0-y.0, x.1-y.1]
}

#[inline]
fn dfmul_f2_f_f(x: f32, y: f32) -> f32n2 { // MulAsF2 for f32 .mul_as_f2()
    let xh = upperf(x);
    let xl = x - xh;
    let yh = upperf(y);
    let yl = y - yh;
    let r0 = x * y;
    f32n2::new(r0, xh * yh - r0 + xl * yh + xh * yl + xl * yl)
}

#[inline]
fn dfmul_f_f2_f2(x: f32n2, y: f32n2) -> f32 { // MulAsF for f32n2 .mul_as_f()
    let xh = upperf(x.0);
    let xl = x.0 - xh;
    let yh = upperf(y.0);
    let yl = y.0 - yh;
    x.1 * yh + xh * y.1 + xl * yl + xh * yl + xl * yh + xh * yh
}


// SqrtAsF2 for f32 .sqrt_as_f2()


#[inline]
fn dfrec_f2_f2(d: f32n2) -> f32n2 { // Rec for f32n2 .rec()
    let t = 1. / d.0;
    let dh = upperf(d.0);
    let dl = d.0 - dh;
    let th = upperf(t);
    let tl = t - th;
    f32n2::new(
        t,
        t * (1. - dh * th - dh * tl - dl * th - dl * tl - d.1 * t),
    )
}

#[inline]
fn dfrec_f2_f(d: f32) -> f32n2 { // Rec for f32 .rec()
    let t = 1. / d;
    let dh = upperf(d);
    let dl = d - dh;
    let th = upperf(t);
    let tl = t - th;
    f32n2::new(t, t * (1. - dh * th - dh * tl - dl * th - dl * tl))
}




/*
#[inline]
fn ddnormalize_d2_d2(t: f64n2) -> f64n2 {
  let s0 = t.0 + t.1;
  f64n2::new(s0, t.0 - s.0 + t.1)
}
*/
/*
#[inline]
fn ddscale_d2_d2_d(d: f64n2, s : f64) -> f64n2 { // Self::scale
  f64n2::new(d.0 * s, d.1 * s)
}
*/
/*
#[inline]
fn ddneg_d2_d2(f64n2 d) -> f64n2 { // std::ops::Neg for f64n2
  f64n2::new(-d.0, -d.1)
}
*/
/*
#[inline]
fn ddabs_d2_d2(f64n2 x) -> f64n2 {
  f64n2::new(if x.0 < 0 { -x.0 } else { x.0 },
  if x.0 < 0 { -x.1 } else { x.1 })
}
*/
/*
 * ddadd and ddadd2 are functions for double-double addition.  ddadd
 * is simpler and faster than ddadd2, but it requires the absolute
 * value of first argument to be larger than the second argument. The
 * exact condition that should be met is checked if NDEBUG macro is
 * not defined.
 *
 * Please note that if the results won't be used, it is no problem to
 * feed arguments that do not meet this condition. You will see
 * warning messages if you turn off NDEBUG macro and run tester2, but
 * this is normal.
  let t = 1. / d;
  let dh = upper(d);
  let dl = d - dh;
  let th = upper(t);
  let tl = t - th;
  let q0 = t;
  f64n2::new(q0, t * (1 - dh * th - dh * tl - dl * th - dl * tl))
 * 
 * Please see :
 * Jonathan Richard Shewchuk, Adaptive Precision Floating-Point
 * Arithmetic and Fast Robust Geometric Predicates, Discrete &
 * Computational Geometry 18:305-363, 1997.
 */
/*
#[inline]
fn ddadd_d2_d_d(x : f64, y : f64) -> f64n2 {
  // |x| >= |y|
debug_assert!(checkfp(x) || checkfp(y) || fabsk(x) >= fabsk(y) || ((fabsk(x+y) <= fabsk(x)) && (fabsk(x+y) <= fabsk(y))), "[ddadd_d2_d_d : %g, %g]\n", x, y);
  f64n2::new(x + y, x - r.0 + y)
}*/
/*
#[inline]
fn ddadd2_d2_d_d(x : f64, y : f64) -> f64n2 {
  let r0 = x + y;
  let v = r0 - x;
  f64n2(r0, (x - (r.0 - v)) + (y - v))
}*/
/*
#[inline]
fn ddadd_d2_d2_d(x : f64n2, y : f64) -> f64n2 { // AddChecked<f64> for f64n2 .add_checked
  // |x| >= |y|
debug_assert!(checkfp(x.0) || checkfp(y) || fabsk(x.0) >= fabsk(y) || ((fabsk(x.0+y) <= fabsk(x.0)) && (fabsk(x.0+y) <= fabsk(y))), "[ddadd_d2_d2_d : %g %g]\n", x.0, y);
  let r0 = x.0 + y;
  f64n2(r0, x.0 - r0 + y + x.1)
}
*/
/*
#[inline]
fn ddadd2_d2_d2_d(x : f64n2, y : f64) -> f64n2 {
    let r0 = x.0 + y;
    let v = r0 - x.0; // == y
    f64n2::new(r0, (x.0 - (r0 - v)) + (y - v) + x.1) // [x.0+y, x.1]
}
*/
/*
#[inline]
fn ddadd_d2_d_d2(double x, f64n2 y) -> f64n2 {
  // |x| >= |y|
debug_assert!(checkfp(x) || checkfp(y.0) || fabsk(x) >= fabsk(y.0) || ((fabsk(x+y.0) <= fabsk(x)) && (fabsk(x+y.0) <= fabsk(y.0))), "[ddadd_d2_d_d2 : %g %g]\n", x, y.0);
    let r0 = x + y.0;
    f64n2::new(r0, x - r0 + y.0 + y.1) // [y.0+x, y.1]
}
*/
/*
#[inline]
fn ddadd2_d2_d_d2(double x, f64n2 y) -> f64n2 {
    let r0 = x + y.0;
    let v = r0 - x; // == y.0
    f64n2::new(r0, (x - (r0 - v)) + (y.0 - v) + y.1) // [y.0+x, y.1]
}
*/
/*#[inline]
fn ddadd2_d_d_d2(x : f64, y: f64n2) -> f64 { y.1 + y.0 + x } // not used
*/
/*
#[inline]
fn ddadd_d2_d2_d2(x: f64n2, y : f64n2) -> f64n2 { // AddChecked for f64n2 .add_checked()
  // |x| >= |y|
debug_assert!(checkfp(x.0) || checkfp(y.0) || fabsk(x.0) >= fabsk(y.0) || ((fabsk(x.0+y.0) <= fabsk(x.0)) && (fabsk(x.0+y.0) <= fabsk(y.0))), "[ddadd_d2_d2_d2 : %g %g]\n", x.0, y.0);
  let r0 = x.0 + y.0;
  f64n2::new(r0, x.0 - r0 + y.0 + x.1 + y.1); // [x.0+y.0, x.1+y.1]
}
*/
/*
#[inline]
fn ddadd2_d2_d2_d2(x: f64n2, y : f64n2) -> f64n2 { // std::ops::Add for f64n2
  let r0  = x.0 + y.0;
  let v = r0 - x.0;
  f64n2::new(r0, (x.0 - (r0 - v)) + (y.0 - v) + x.1 + y.1); // [x.0+y.0, x.1+y.1]
}
*/
/*#[inline]
fn ddsub_d2_d2_d2(x: f64n2, y : f64n2) -> f64n2 { SubChecked for f64n2 .sub_checked()
  // |x| >= |y|
debug_assert!(checkfp(x.0) || checkfp(y.0) || fabsk(x.0) >= fabsk(y.0) || ((fabsk(x.0-y.0) <= fabsk(x.0)) && (fabsk(x.0-y.0) <= fabsk(y.0))), "[ddsub_d2_d2_d2 : %g %g]\n", x.0, y.0);
  let r0 = x.0 - y.0;
  f64n2::new(r0, x.0 - r.0 - y.0 + x.1 - y.1); // [x.0-y.0, x.1-y.1]
}*/
/*
#[inline]
fn dddiv_d2_d2_d2(f64n2 n, f64n2 d) -> f64n2 { std::ops::Div for f64n2
  let t = 1. / d.0;
  let dh  = upper(d.0);
  let dl  = d.0 - dh;
  let th  = upper(t  );
  let tl  = t   - th;
  let nhh = upper(n.0);
  let nhl = n.0 - nhh;

  let q0 = n.0 * t;

  let u = -q0 + nhh * th + nhh * tl + nhl * th + nhl * tl +
    q0 * (1 - dh * th - dh * tl - dl * th - dl * tl);

  f64n2::new(q0, t * (n.1 - q0 * d.1) + u)
}*/
/*
#[inline]
fn ddmul_d2_d_d(x : f64, y : f64) -> f64n2 {
  let xh = upper(x);
  let xl = x - xh;
  let yh = upper(y);
  let yl = y - yh;
  let r0 = x * y;
  f64n2::new(r0, xh * yh - r.0 + xl * yh + xh * yl + xl * yl)
}
*/
/*
#[inline]
fn ddmul_d2_d2_d(x : f64n2, y : f64) -> f64n2 {
  let xh = upper(x.0);
  let xl = x.0 - xh;
  let yh = upper(y  );
  let yl = y   - yh;
  let r0 = x.0 * y;
  f64n2::new(r0, xh * yh - r.0 + xl * yh + xh * yl + xl * yl + x.1 * y);
}
*/
/*
#[inline]
fn ddmul_d2_d2_d2(x: f64n2, y : f64n2) -> f64n2 {
  let xh = upper(x.0);
  let xl = x.0 - xh;
  let yh = upper(y.0);
  let yl = y.0 - yh;
  let r0 = x.0 * y.0;
  f64n2::new(r0, xh * yh - r0 + xl * yh + xh * yl + xl * yl + x.0 * y.1 + x.1 * y.0)
}*/
/*
#[inline]
fn ddmul_d_d2_d2(x: f64n2, y : f64n2) -> f64 {
  let xh = upper(x.0);
  let xl = x.0 - xh;
  let yh = upper(y.0);
  let yl = y.0 - yh;
  x.1 * yh + xh * y.1 + xl * yl + xh * yl + xl * yh + xh * yh
}
*/
/*
#[inline]
fn ddsqu_d2_d2(x: f64n2) -> f64n2 { // Self::square
  let xh = upper(x.0);
  let xl = x.0 - xh;
  let r0 = x.0 * x.0;
  f64n2::new(r0, xh * xh - r0 + (xh + xh) * xl + xl * xl + x.0 * (x.1 + x.1))
}
*/
/*
#[inline]
fn ddsqu_d_d2(x: f64n2) -> f64 { // Self::square_as_d
  let xh = upper(x.0);
  let xl = x.0 - xh;
  xh * x.1 + xh * x.1 + xl * xl + (xh * xl + xh * xl) + xh * xh
}
*/
/*
#[inline]
fn ddrec_d2_d(d : f64) -> f64n2 {
  let t = 1. / d;
  let dh = upper(d);
  let dl = d - dh;
  let th = upper(t);
  let tl = t - th;
  let q0 = t;
  f64n2::new(q0, t * (1 - dh * th - dh * tl - dl * th - dl * tl))
}
*/
/*
#[inline]
fn ddrec_d2_d2(f64n2 d) -> f64n2 {
  let t = 1. / d.0;
  let dh = upper(d.0);
  let dl = d.0 - dh;
  let th = upper(t  );
  let tl = t   - th;
  let q0 = t;
  f64n2::new(q0, t * (1 - dh * th - dh * tl - dl * th - dl * tl - d.1 * t)
}
*/
/*
#[inline]
fn ddsqrt_d2_d2(f64n2 d) -> f64n2 { // Self::sqrt
  let t = SQRT(d.0 + d.1);
  ddmul_d2_d2_d2(d + ddmul_d2_d_d(t, t), ddrec_d2_d(t)).scale(0.5)
}
*/
/*
#[inline]
fn ddsqrt_d2_d(d : f64) -> f64n2 { // SqrtAsD2 for f64
  let t = SQRT(d);
  ((d + t.mul_as_d2(t)) * ddrec_d2_d(t)).scale(0.5)
}
*/
//


vcast_vf_f(f: float) // VCast for f32
