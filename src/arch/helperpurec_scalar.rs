//          Copyright Naoki Shibata 2010 - 2018.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <stdint.h>

#ifndef ENABLE_BUILTIN_MATH
#include <math.h>

#define SQRT sqrt
#define SQRTF sqrtf
#define FMA fma
#define FMAF fmaf
#define RINT rint
#define RINTF rintf
#define TRUNC trunc
#define TRUNCF truncf

#else

#define SQRT __builtin_sqrt
#define SQRTF __builtin_sqrtf
#define FMA __builtin_fma
#define FMAF __builtin_fmaf
#define RINT __builtin_rint
#define RINTF __builtin_rintf
#define TRUNC __builtin_trunc
#define TRUNCF __builtin_truncf

#endif

#include "misc.h"

#ifndef CONFIG
#error CONFIG macro not defined
#endif

#define ENABLE_DP
#define ENABLE_SP

#if CONFIG == 2
#define ENABLE_FMA_DP
#define ENABLE_FMA_SP

#if defined(__AVX2__) || defined(__aarch64__) || defined(__arm__) || defined(__powerpc64__)
#ifndef FP_FAST_FMA
#define FP_FAST_FMA
#endif
#ifndef FP_FAST_FMAF
#define FP_FAST_FMAF
#endif
#endif

#if !defined(FP_FAST_FMA) || !defined(FP_FAST_FMAF)
#error FP_FAST_FMA or FP_FAST_FMAF not defined
#endif
#define ISANAME "Pure C scalar with FMA"

#else // #if CONFIG == 2
#define ISANAME "Pure C scalar"
#endif // #if CONFIG == 2

#define LOG2VECTLENDP 0
#define VECTLENDP (1 << LOG2VECTLENDP)
#define LOG2VECTLENSP 0
#define VECTLENSP (1 << LOG2VECTLENSP)

#define ACCURATE_SQRT

#if defined(__SSE4_1__) || defined(__aarch64__)
#define FULL_FP_ROUNDING
#endif

#define DFTPRIORITY LOG2VECTLENDP

typedef union {
  uint32_t u[2];
  int32_t i[2];
  uint64_t x;
  double d;
  float f;
  int64_t i2;
} versatileVector;

typedef uint64_t $bx;
typedef uint32_t $mox;
typedef double $f64x;
typedef int32_t $ix;
typedef float $f32x;
typedef int64_t $ix2;

//

#[inline]
fn vtestallones_i_vo64(g: $mox) -> int { return g; }
#[inline]
fn vtestallones_i_vo32(g: $mox) -> int { return g; }

//

static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { *p = v; }
static void vstoreu_v_p_vi(int32_t *p, $ix v) { *p = v; }

//

#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox { return m; }
#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox { return m; }
#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $bx { return (((uint64_t)h) << 32) | (uint32_t)l; }


#[inline]
fn vrev21_vi2_vi2(vi2: $ix2) -> $ix2 { return (((uint64_t)vi2) << 32) | (((uint64_t)vi2) >> 32); }


//

#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { return y & ~x; }

#[inline]
fn vandnot_vm_vm_vm  (x: $bx, y: $bx)     -> $bx { return y & ~x; }
#[inline]
fn vor_vm_vm_vm      (x: $bx, y: $bx)     -> $bx { return x | y; }

#[inline]
fn vcast_vm_vo(o: $mox) -> $bx { return ($bx)o | ((($bx)o) << 32); }

#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $bx)      -> $bx { return vcast_vm_vo(x) & y; }
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $bx)   -> $bx { return y & ~vcast_vm_vo(x); }
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $bx)       -> $bx { return vcast_vm_vo(x) | y; }
#[inline]
fn vxor_vm_vo64_vm(x: $mox, y: $bx)      -> $bx { return vcast_vm_vo(x) ^ y; }

#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $bx)      -> $bx { return vcast_vm_vo(x) & y; }
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $bx)   -> $bx { return y & ~vcast_vm_vo(x); }
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $bx)       -> $bx { return vcast_vm_vo(x) | y; }
#[inline]
fn vxor_vm_vo32_vm(x: $mox, y: $bx)      -> $bx { return vcast_vm_vo(x) ^ y; }

//

#[inline]
fn vsel_vd_vo_vd_vd   (o: $mox, x: $f64x, y: $f64x) -> $f64x { return o ? x : y; }
#[inline]
fn   vsel_vi2_vo_vi2_vi2(o: $mox, x: $ix2, y: $ix2)     -> $ix2 { return o ? x : y; }

#[inline]
fn vsel_vd_vo_d_d(o: $mox, v1: f64, v0: f64) -> CONST -> $f64x { return o ? v1 : v0; }

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $mox, o1: $mox, d0: f64, d1: f64, d2: f64) -> $f64x {
  return vsel_vd_vo_vd_vd(o0, $f64x::splat(d0), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $mox, o1: $mox, o2: $mox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  return vsel_vd_vo_vd_vd(o0, $f64x::splat(d0), vsel_vd_vo_vd_vd(o1, $f64x::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)));
}


#ifdef FULL_FP_ROUNDING
#[inline]
fn vrint_vi_vd(d: $f64x) -> $ix { return (int32_t)RINT(d); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return RINT(vd); }
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return TRUNC(vd); }
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix { return (int32_t)TRUNC(vd); }
#else
#[inline]
fn vrint_vi_vd(a: $f64x) -> $ix {
  a += a > 0 ? 0.5 : -0.5;
  versatileVector v = { .d = a }; v.x -= 1 & (int)a;
  return (int32_t)v.d;
}
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return $f64x::from(vrint_vi_vd(vd)); }
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix { return vd; }
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return $f64x::from(vtruncate_vi_vd(vd)); }
#endif

#[inline]
fn veq64_vo_vm_vm(x: $bx, y: $bx) -> $mox { return x == y ? ~(uint32_t)0 : 0; }
#[inline]
fn vadd64_vm_vm_vm(x: $bx, y: $bx) -> $bx { return x + y; }

//

impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        1. / self
    }
}
impl Abs for $f64x {
    fn abs(self) -> Self {
        versatileVector v = -> $f64x { .d = d }; v.x &= 0x7fffffffffffffffULL; return v.d;
    }
}


#ifndef ENABLE_FMA_DP
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        x * y - z
    }
}
#else
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        FMA(x, y, z)
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        FMA(x, y, -z)
    }
}
#[inline]
fn vfma_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return FMA(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return FMA(x, y, -z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return FMA(-x, y, z); }
#endif


impl std::ops::Neg for $ix {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        -self
    }
}

#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return y & ~x; }

#[inline]
fn vand_vi_vo_vi(x: $mox, y: $ix)    -> $ix { return x & y; }
#[inline]
fn vandnot_vi_vo_vi(x: $mox, y: $ix) -> $ix { return y & ~x; }

#[inline]
fn vsrl_vi_vi_i(x: $ix, c: int) -> $ix { return (uint32_t)x >> c; }


#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix { return m ? x : y; }

#[inline]
fn visinf_vo_vd(d: $f64x)  -> $mox { return (d == SLEEF_INFINITY || d == -SLEEF_INFINITY) ? ~(uint32_t)0 : 0; }
#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox { return d == SLEEF_INFINITY ? ~(uint32_t)0 : 0; }
#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox { return d == -SLEEF_INFINITY ? ~(uint32_t)0 : 0; }
#[inline]
fn visnan_vo_vd(d: $f64x)  -> $mox { return d != d ? ~(uint32_t)0 : 0; }

#[inline]
fn vsqrt_vd_vd(d: $f64x) -> $f64x { return SQRT(d); }
#[inline]
fn vsqrt_vf_vf(x: $f32x) -> $f32x { return SQRTF(x); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x { return ptr[vi]; }


//

#ifdef FULL_FP_ROUNDING
#[inline]
fn vrint_vi2_vf(d: $f32x) -> $ix2 { return (int)RINTF(d); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return RINTF(vd); }
#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x { return TRUNCF(vd); }
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return (int32_t)TRUNCF(vf); }
#else
#[inline]
fn vrint_vi2_vf(a: $f32x) -> $ix2 {
  a += a > 0 ? 0.5 : -0.5;
  versatileVector v = { .f = a }; v.u[0] -= 1 & (int)a;
  return (int32_t)v.f;
}
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vrint_vi2_vf(vd)); }
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return vf; }
#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#endif
impl Rec for $f32x {
    #[inline]
    fn rec(self) -> Self {
        1 / self
    }
}

impl Abs for $f32x {
    fn abs(self) -> Self {
        versatileVector v = -> $f32x { .f = x }; v.x &= 0x7fffffff; return v.f;
    }
}


#ifndef ENABLE_FMA_SP
impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return - x * y + z; }
#else
impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        FMAF(x, y, z)
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return FMAF(-x, y, z); }
#[inline]
fn vfma_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return FMAF(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return FMAF(x, y, -z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return FMAF(-x, y, z); }
#endif


#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return y & ~x; }

#[inline]
fn vsel_vf_vo_vf_vf(o: $mox, x: $f32x, y: $f32x) -> $f32x { return o ? x : y; }
#[inline]
fn vsel_vf_vo_f_f(o: $mox, v1: f32, v0: f32) -> $f32x { return o ? v1 : v0; }

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: $mox, o1: $mox, d0: f32, d1: f32, d2: f32) -> $f32x {
  return vsel_vf_vo_vf_vf(o0, $f32x::splat(d0), vsel_vf_vo_f_f(o1, d1, d2));
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $mox, o1: $mox, o2: $mox, d0: f32, d1: f32, d2: f32, d3: f32) -> $f32x {
  return vsel_vf_vo_vf_vf(o0, $f32x::splat(d0), vsel_vf_vo_vf_vf(o1, $f32x::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)));
}

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vcast_vm_vo(x) & y; }
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return y & ~vcast_vm_vo(x); }

#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) { versatileVector v = -> $ix2 { .i2 = x }; v.u[0] >>= c; v.u[1] >>= c; return v.i2; }

#[inline]
fn visinf_vo_vf (d: $f32x) -> $mox { return (d == SLEEF_INFINITYf || d == -SLEEF_INFINITYf) ? ~(uint32_t)0 : 0; }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { return d == SLEEF_INFINITYf ? ~(uint32_t)0 : 0; }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { return d == -SLEEF_INFINITYf ? ~(uint32_t)0 : 0; }
#[inline]
fn visnan_vo_vf (d: $f32x) -> $mox { return d != d ? ~(uint32_t)0 : 0; }

#[inline]
fn   veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return (int32_t)x == (int32_t)y ? ~(uint32_t)0 : 0; }
#[inline]
fn   vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return (int32_t)x >  (int32_t)y ? ~(uint32_t)0 : 0; }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi) -> $f32x { return ptr[vi]; }

