//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
/*
#if CONFIG == 2

#if !defined(__SSE2__)
#error Please specify -msse2.
#endif

#elif CONFIG == 3

#if !defined(__SSE2__) || !defined(__SSE3__)
#error Please specify -msse2 and -msse3
#endif

#elif CONFIG == 4

#if !defined(__SSE2__) || !defined(__SSE3__) || !defined(__SSE4_1__)
#error Please specify -msse2, -msse3 and -msse4.1
#endif

#else
#error CONFIG macro invalid or not defined
#endif

#define ENABLE_DP
#define LOG2VECTLENDP 1
#define VECTLENDP (1 << LOG2VECTLENDP)

#define ENABLE_SP
#define LOG2VECTLENSP (LOG2VECTLENDP+1)
#define VECTLENSP (1 << LOG2VECTLENSP)

#define ACCURATE_SQRT

#if defined(_MSC_VER)
#include <intrin.h>
#else
#include <x86intrin.h>
#endif

#include <stdint.h>
#include "misc.h"
*/

typedef __m128i $ux;
typedef __m128i $ox; // m64x2  m32x4

typedef __m128d f64x2;
typedef __m128i i64x2;

typedef __m128  f32x4;
typedef __m128i i32x4;


static void vstoreu_v_p_vi2(int32_t *p, i32x4 v) { _mm_storeu_si128((__m128i *)p, v); }

static void vstoreu_v_p_vi(int32_t *p, i64x2 v) { _mm_storeu_si128((__m128i *)p, v); }

//

#[inline]
fn vandnot_vm_vm_vm(x: u64x2, y: u64x2) -> u64x2 { _mm_andnot_si128(x, y) }

#[inline]
fn vandnot_vm_vm_vm(x: u32x4, y: u32x4) -> u32x4 { _mm_andnot_si128(x, y) }

#[inline]
fn vandnot_vo_vo_vo(x: m64x2, y: m64x2) -> m64x2 { _mm_andnot_si128(x, y) }

#[inline]
fn vandnot_vo_vo_vo(x: m32x4, y: m32x4) -> m32x4 { _mm_andnot_si128(x, y) }

#[inline]
fn vand_vm_vo64_vm(x: m64x2, y: u64x2) -> u64x2 { _mm_and_si128(x, y) }
#[inline]
fn vor_vm_vo64_vm(x: m64x2, y: u64x2) -> u64x2 { _mm_or_si128(x, y) }
#[inline]
fn vandnot_vm_vo64_vm(x: m64x2, y: u64x2) -> u64x2 { _mm_andnot_si128(x, y) }

#[inline]
fn vand_vm_vo32_vm(x: m32x4, y: u32x4) -> u32x4 { _mm_and_si128(x, y) }
#[inline]
fn vor_vm_vo32_vm(x: m32x4, y: u32x4) -> u32x4 { _mm_or_si128(x, y) }
#[inline]
fn vandnot_vm_vo32_vm(x: u32x4, y: u32x4) -> u32x4 { _mm_andnot_si128(x, y) }

#[inline]
fn vandnot_vi_vi_vi(x: i64x2, y: i64x2) -> i64x2 { _mm_andnot_si128(x, y) }

#[inline]
fn vand_vi_vo_vi(x: m64x2, y: i64x2) -> i64x2 { _mm_and_si128(x, y) }
#[inline]
fn vandnot_vi_vo_vi(x: m64x2, y: i64x2) -> i64x2 { _mm_andnot_si128(x, y) }


#[inline]
fn vandnot_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { vandnot_vi_vi_vi(x, y) }

#[inline]
fn vand_vi2_vo_vi2(x: m32x4, y: i32x4) -> i32x4 { vand_vi_vo_vi(x, y) }

#[inline]
fn veq_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { _mm_cmpeq_epi32(x, y) }
#[inline]
fn vgt_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { _mm_cmpgt_epi32(x, y) }



impl Round for f64x2 {
    type Int = i64x2;
    #[target_feature(enable = "sse4.1")]
    #[inline]
    fn truncate(self) -> Self {
        _mm_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
    }
    #[inline]
    fn truncatei(self) -> Self::Int {
        _mm_cvttpd_epi32(vd)
    }
    #[target_feature(enable = "sse4.1")]
    #[inline]
    fn truncate(self) -> Self {
        f64x2::from(vtruncate_vi_vd(vd))
    }
    #[target_feature(enable = "sse4.1")]
    #[inline]
    fn rint(self) -> Self {
        _mm_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
    }
    #[target_feature(not(enable = "sse4.1"))]
    #[inline]
    fn rint(self) -> Self {
        f64x2::from(vrint_vi_vd(vd))
    }
    #[inline]
    fn rinti(self) -> Self::Int {
        _mm_cvtpd_epi32(vd)
    }
}


impl Round for f32x4 {
    type Int = i32x4;
    #[target_feature(enable = "sse4.1")]
    #[inline]
    fn truncate(self) -> Self {
        _mm_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
    }
    #[target_feature(not(enable = "sse4.1"))]
    #[inline]
    fn truncate(self) -> Self {
        Self::from(vd.truncatei())
    }
    #[inline]
    fn truncatei(self) -> Self::Int {
        _mm_cvttps_epi32(vf)
    }
    #[target_feature(enable = "sse4.1")]
    #[inline]
    fn rint(self) -> Self {
      _mm_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
    }
    #[target_feature(not(enable = "sse4.1"))]
    #[inline]
    fn rint(self) -> Self {
        Self::From(vf.rinti())
    }
    #[inline]
    fn rinti(self) -> Self::Int {
      _mm_cvtps_epi32(vf)
    }
}


impl FromU32 for u64x2 {
  fn from_u32(i: (u32, u32)) -> Self {
      u64x2::from(u32x4::new(i0, i1, i0, i1))
  }
}
/*#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $ux { _mm_set_epi32(i0, i1, i0, i1) }
*/

impl Mla for f64x2 {
    #[inline]
    fn mul_sub(self, y: Self, z: Self) -> Self {
        x*y - z
    }
}
/*
#[inline]
fn vsel_vd_vo_d_d(o: m64x2, v1: f64, v0: f64) -> CONST -> f64x2 {
  o.select(f64x2::splat(v1), f64x2::splat(v0))
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: m64x2, o1: m64x2, d0: f64, d1: f64, d2: f64) -> f64x2 {
  o0.select(f64x2::splat(d0), vsel_vd_vo_d_d(o1, d1, d2))
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: m64x2, o1: m64x2, o2: m64x2, d0: f64, d1: f64, d2: f64, d3: f64) -> f64x2 {
  o0.select(f64x2::splat(d0), o1.select(f64x2::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)))
}
*/
//

#[inline]
fn vgather_vd_p_vi(const double *ptr, i64x2 vi) -> f64x2 {
  int a[sizeof(i64x2)/sizeof(int)];
  vstoreu_v_p_vi(a, vi);
  _mm_set_pd(ptr[a[1]], ptr[a[0]])
}

#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x4, y: f32x4, z: f32x4) -> f32x4 { z - x * y }


/*
#[inline]
fn vsel_vf_vo_f_f(o: m32x4, v1: f32, v0: f32) -> f32x4 {
  o.select(f32x4::splat(v1), f32x4::splat(v0))
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: m32x4, o1: m32x4, d0: f32, d1: f32, d2: f32) -> f32x4 {
  o0.select(f32x4::splat(d0), vsel_vf_vo_f_f(o1, d1, d2))
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: m32x4, o1: m32x4, o2: m32x4, d0: f32, d1: f32, d2: f32, d3: f32) -> f32x4 {
  o0.select(f32x4::splat(d0), o1.select(f32x4::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)))
}
*/
#[inline]
fn vgather_vf_p_vi2(const float *ptr, i32x4 vi) -> f32x4 {
  int a[VECTLENSP];
  vstoreu_v_p_vi2(a, vi);
  _mm_set_ps(ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]])
}
//

//

#[inline]
fn vrev21_vf_vf(d0: f32x4) -> f32x4 { _mm_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)) }
#[inline]
fn vrev21_vi2_vi2(i: i32x4) -> i32x4 { i32x4::from(vrev21_vf_vf(f32x4::from(i))) }
