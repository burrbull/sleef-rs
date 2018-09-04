//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#if CONFIG == 1

#ifndef __AVX2__
#error Please specify -mavx2.
#endif

#else
#error CONFIG macro invalid or not defined
#endif

#define ENABLE_DP
#define LOG2VECTLENDP 1
#define VECTLENDP (1 << LOG2VECTLENDP)
#define ENABLE_FMA_DP

#define ENABLE_SP
#define LOG2VECTLENSP (LOG2VECTLENDP+1)
#define VECTLENSP (1 << LOG2VECTLENSP)
#define ENABLE_FMA_SP

#define FULL_FP_ROUNDING
#define SPLIT_KERNEL
#define ACCURATE_SQRT

#if defined(_MSC_VER)
#include <intrin.h>
#else
#include <x86intrin.h>
#endif

#include <stdint.h>
#include "misc.h"

type $ux = __m128i; // i64x2, i32x4
type $ox = __m128i;

type f64x2 = __m128d;
type i64x2 = __m128i;

type f32x4 = __m128;
type i32x4 = __m128i;

//

#[inline]
fn vandnot_vm_vm_vm(x: u64x2, y: u64x2) -> u64x2 { u64x2::from(_mm_andnot_pd(f64x2::from(x), f64x2::from(y))) }

#[inline]
fn vandnot_vo_vo_vo(x: $ox, y: $ox) -> $ox { u64x2::from(_mm_andnot_pd(f64x2::from(x), f64x2::from(y))) }

#[inline]
fn vand_vm_vo64_vm(x: m64x2, y: u64x2) -> u64x2 { return u64x2::from(_mm_and_pd(f64x2::from(x), f64x2::from(y))); }
#[inline]
fn vandnot_vm_vo64_vm(x: m64x2, y: u64x2) -> u64x2 { return u64x2::from(_mm_andnot_pd(f64x2::from(x), f64x2::from(y))); }
#[inline]
fn vor_vm_vo64_vm(x: m64x2, y: u64x2) -> u64x2 { return u64x2::from(_mm_or_pd(f64x2::from(x), f64x2::from(y))); }

#[inline]
fn vand_vm_vo32_vm(x: m64x2, y: u64x2) -> u64x2 { return u64x2::from(_mm_and_pd(f64x2::from(x), f64x2::from(y))); }
#[inline]
fn vandnot_vm_vo32_vm(x: m64x2, y: u64x2) -> u64x2 { return u64x2::from(_mm_andnot_pd(f64x2::from(x), f64x2::from(y))); }
#[inline]
fn vor_vm_vo32_vm(x: m64x2, y: u64x2) -> u64x2 { return u64x2::from(_mm_or_pd(f64x2::from(x), f64x2::from(y))); }


impl Round for f64x2 {
    type Int = i64x2;
    #[inline]
    fn truncate(self) -> Self {
        _mm_round_pd(self, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
    }
    fn truncatei(self) -> Self::Int {
        _mm_cvttpd_epi32(self)
    }
    #[inline]
    fn rint(self) -> Self {
        _mm_round_pd(self, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
    }
    fn rinti(self) -> Self::Int {
        _mm_cvtpd_epi32(self)
    }
}

impl FromU32 for u64x2 {
  fn from_u32(i: (u32, u32)) -> Self {
      u64x2::from(u32x4::new(i0, i1, i0, i1))
  }
}
/*
#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $ux { return _mm_set_epi32(i0, i1, i0, i1); }
*/

impl Mla for f64x2 {
    #[inline]
    fn mul_sub(self, y: Self, z: Self) -> Self {
        _mm_fmsub_pd(x, y, z)
    }
}

#[target_feature(enable = "fma")]
impl Fma for $f64x {
    #[inline]
    fn mul_sube(self, y: Self, z: Self) -> Self {
      _mm_fmsub_pd(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
      _mm_fnmadd_pd(x, y, z)
    }
}

//


#[inline]
fn vandnot_vi_vi_vi(x: i64x2, y: i64x2) -> i64x2 { return _mm_andnot_si128(x, y); }

#[inline]
fn vand_vi_vo_vi(x: $ox, y: i64x2) -> i64x2 { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vi_vo_vi(x: $ox, y: i64x2) -> i64x2 { return _mm_andnot_si128(x, y); }

#[inline]
fn vsel_vd_vo_d_d(o: $ox, v1: f64, v0: f64) -> f64x2 {
  o.select(f64x2::splat(v1), f64x2::splat(v0))
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $ox, o1: $ox, d0: f64, d1: f64, d2: f64) -> f64x2 {
  o0.select(f64x2::splat(d0), vsel_vd_vo_d_d(o1, d1, d2))
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $ox, o1: $ox, o2: $ox, d0: f64, d1: f64, d2: f64, d3: f64) -> f64x2 {
  o0.select(f64x2::splat(d0), o1.select(f64x2::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)))
}

#[inline]
fn visinf_vo_vd(d: f64x2) -> m64x2 {
  m64x2::from(_mm_cmp_pd(d.abs(), _mm_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ))
}

#[inline]
fn vispinf_vo_vd(d: f64x2) -> m64x2 {
  m64x2::from(_mm_cmp_pd(d, _mm_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ))
}

#[inline]
fn visnan_vo_vd(d: f64x2) -> m64x2 {
  m64x2::from(_mm_cmp_pd(d, d, _CMP_NEQ_UQ))
}

#[inline]
fn vgather_vd_p_vi(const double *ptr, i64x2 vi) -> f64x2 { return _mm_i32gather_pd(ptr, vi, 8); }


impl Round for f32x4 {
    type Int = i32x4;
    #[inline]
    fn truncate(self) -> Self {
        _mm_round_ps(self, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
    }
    #[inline]
    fn truncatei(self) -> Self::Int {
      Self::Int::from(_mm_cvttps_epi32(self))
    }
    #[inline]
    fn rint(self) -> Self {
      _mm_round_ps(self, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
    }
    #[inline]
    fn rinti(self) -> Self::Int {
      Self::Int::from(_mm_cvtps_epi32(self))
    }
}

#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x4, y: f32x4, z: f32x4) -> f32x4 { return _mm_fnmadd_ps(x, y, z); }

#[target_feature(enable = "fma")]
impl Fma for f32x4 {
    #[inline]
    fn mul_sube(self, y: Self, z: Self) -> Self {
        _mm_fmsub_ps(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        _mm_fnmadd_ps(x, y, z)
    }
}

#[inline]
fn vandnot_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { return _mm_andnot_si128(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: $ox, y: i32x4) -> i32x4 { i32x4::from(x) & y }
#[inline]
fn vandnot_vi2_vo_vi2(x: $ox, y: i32x4) -> i32x4 { return vandnot_vi2_vi2_vi2(i32x4::from(x), y); }

#[inline]
fn veq_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { return _mm_cmpgt_epi32(x, y); }

#[inline]
fn vsel_vf_vo_f_f(o: $ox, v1: f32, v0: f32) -> f32x4 {
  o.select(f32x4::splat(v1), f32x4::splat(v0))
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: $ox, o1: $ox, d0: f32, d1: f32, d2: f32) -> f32x4 {
  o0.select(f32x4::splat(d0), vsel_vf_vo_f_f(o1, d1, d2))
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $ox, o1: $ox, o2: $ox, d0: f32, d1: f32, d2: f32, d3: f32) -> f32x4 {
  o0.select(f32x4::splat(d0), o1.select(f32x4::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)))
}

#[inline]
fn visinf_vo_vf(d: f32x4) -> $ox { return d.abs().ne(f32x4::splat(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: f32x4) -> $ox { return d.ne(f32x4::splat(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: f32x4) -> $ox { return d.ne(f32x4::splat(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: f32x4) -> $ox { return d.ne(d); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, i32x4 vi2) -> f32x4 { return _mm_i32gather_ps(ptr, vi2, 4); }


//

#[inline]
fn vrev21_vf_vf(d0: f32x4) -> f32x4 { return _mm_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: i32x4) -> i32x4 { return i32x4::from(vrev21_vf_vf(f32x4::from(i))); }
