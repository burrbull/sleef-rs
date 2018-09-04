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
#define LOG2VECTLENDP 2
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

type $ux = __m256i;
type $ox = __m256i;

type f64x4 = __m256d;
type i32x4 = __m128i;

type f32x8 = __m256;
type i32x8 = __m256i;


#[inline]
fn vandnot_vm_vm_vm(x: $ux, y: $ux) -> $ux { return $ux::from_bits(_mm256_andnot_pd(f64x4::from(x), f64x4::from(y))); }

#[inline]
fn vandnot_vo_vo_vo(x: $ox, y: $ox) -> $ox { return $ux::from_bits(_mm256_andnot_pd(f64x4::from(x), f64x4::from(y))); }

#[inline]
fn vand_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return $ux::from_bits(_mm256_and_pd(f64x4::from(x), f64x4::from(y))); }
#[inline]
fn vandnot_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return $ux::from_bits(_mm256_andnot_pd(f64x4::from(x), f64x4::from(y))); }
#[inline]
fn vor_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return $ux::from_bits(_mm256_or_pd(f64x4::from(x), f64x4::from(y))); }

#[inline]
fn vand_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return $ux::from_bits(_mm256_and_pd(f64x4::from(x), f64x4::from(y))); }
#[inline]
fn vandnot_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return $ux::from_bits(_mm256_andnot_pd(f64x4::from(x), f64x4::from(y))); }
#[inline]
fn vor_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return $ux::from_bits(_mm256_or_pd(f64x4::from(x), f64x4::from(y))); }


impl Round for f64x4 {
    type Int = i32x4;
    #[inline]
    fn truncate(self) -> Self {
        _mm256_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
    }
    #[inline]
    fn truncatei(self) -> Self::Int {
        _mm256_cvttpd_epi32(vd)
    }
    #[inline]
    fn rint(self) -> Self {
        _mm256_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
    }
    #[inline]
    fn rinti(self) -> Self::Int {
        _mm256_cvtpd_epi32(vd)
    }
}

impl FromU32 for u64x4 {
  fn from_u32(i: (u32, u32)) -> Self {
      u64x4::from(u32x8::new(i0, i1, i0, i1, i0, i1, i0, i1))
  }
}
/*#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $ux {
  return _mm256_set_epi32(i0, i1, i0, i1, i0, i1, i0, i1);
}*/

//

impl Mla for f64x4 {
    #[inline]
    fn mul_sub(self, y: Self, z: Self) -> Self {
        _mm256_fmsub_pd(x, y, z)
    }
  }

#[target_feature(enable = "fma")]
impl Fma for f64x4 {
    #[inline]
    fn mul_sube(self, y: Self, z: Self) -> Self {
      _mm256_fmsub_pd(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
      _mm256_fnmadd_pd(x, y, z)
    }
}

//


#[inline]
fn vandnot_vi_vi_vi(x: i32x4, y: i32x4) -> i32x4 { return _mm_andnot_si128(x, y); }

#[inline]
fn vandnot_vi_vo_vi(m: $ox, y: i32x4) -> i32x4 { return _mm_andnot_si128(_mm256_castsi256_si128(m), y); }
#[inline]
fn vand_vi_vo_vi(m: $ox, y: i32x4) -> i32x4 { return _mm_and_si128(_mm256_castsi256_si128(m), y); }

#[inline]
fn vsel_vd_vo_d_d(o: $ox, v1: f64, v0: f64) -> f64x4 { return _mm256_permutevar_pd(_mm256_set_pd(v1, v0, v1, v0), o); }

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d($ox o0, $ox o1, $ox o2, d0: f64, d1: f64, d2: f64, d3: f64) -> f64x4 {
  __m256i v = _mm256_castpd_si256(o0.select(_mm256_castsi256_pd(_mm256_set_epi32(1, 0, 1, 0, 1, 0, 1, 0)),
						   o1.select(_mm256_castsi256_pd(_mm256_set_epi32(3, 2, 3, 2, 3, 2, 3, 2)),
								    o2.select(_mm256_castsi256_pd(_mm256_set_epi32(5, 4, 5, 4, 5, 4, 5, 4)),
										     _mm256_castsi256_pd(_mm256_set_epi32(7, 6, 7, 6, 7, 6, 7, 6))))));
  _mm256_castsi256_pd(_mm256_permutevar8x32_epi32(_mm256_castpd_si256(_mm256_set_pd(d3, d2, d1, d0)), v))
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $ox, o1: $ox, d0: f64, d1: f64, d2: f64) -> f64x4 {
  return vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o1, d0, d1, d2, d2);
}

#[inline]
fn visinf_vo_vd(d: f64x4) -> $ox {
  return $ux::from_bits(_mm256_cmp_pd(d.abs(), _mm256_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn vispinf_vo_vd(d: f64x4) -> $ox {
  return $ux::from_bits(_mm256_cmp_pd(d, _mm256_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visnan_vo_vd(d: f64x4) -> $ox {
  return $ux::from_bits(_mm256_cmp_pd(d, d, _CMP_NEQ_UQ));
}

#[inline]
fn vgather_vd_p_vi(const double *ptr, i32x4 vi) -> f64x4 { return _mm256_i32gather_pd(ptr, vi, 8); }

//

impl Round for f32x8 {
    type Int = i32x8;
    #[inline]
    fn truncate(self) -> Self {
        _mm256_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
    }
    #[inline]
    fn truncatei(self) -> Self::Int {
        Self::Int::from(_mm256_cvttps_epi32(vf))
    }
    #[inline]
    fn rint(self) -> Self {
      _mm256_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
    }
    #[inline]
    fn rint(self) -> Self::Int {
      Self::Int::from(_mm256_cvtps_epi32(vf))
    }
}

#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x8, y: f32x8, z: f32x8) -> f32x8 { return _mm256_fnmadd_ps(x, y, z); }

#[target_feature(enable = "fma")]
impl Fma for f32x8 {
    #[inline]
    fn mul_sube(self, y: Self, z: Self) -> Self {
        _mm256_fmsub_ps(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        _mm256_fnmadd_ps(x, y, z)
    }
}
#[inline]
fn vandnot_vi2_vi2_vi2(i32x8 x, i32x8 y) -> i32x8 { return _mm256_andnot_si256(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: $ox, y: i32x8) -> i32x8 { i32x8::from(x) & y }
#[inline]
fn vandnot_vi2_vo_vi2(x: $ox, y: i32x8) -> i32x8 { return vandnot_vi2_vi2_vi2(i32x8::from(x), y); }

#[inline]
fn veq_vi2_vi2_vi2(i32x8 x, i32x8 y) -> i32x8 { return _mm256_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2(i32x8 x, i32x8 y) -> i32x8 { return _mm256_cmpgt_epi32(x, y); }


// At this point, the following three functions are implemented in a generic way,
// but I will try target-specific optimization later on.
#[inline]
fn vsel_vf_vo_f_f(o: $ox, v1: f32, v0: f32) -> CONST -> f32x8 {
  o.select(f32x8::splat(v1), f32x8::splat(v0))
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: $ox, o1: $ox, d0: f32, d1: f32, d2: f32) -> f32x8 {
  o0, f32x8::splat(d0), vsel_vf_vo_f_f(o1, d1, d2))
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $ox, o1: $ox, o2: $ox, d0: f32, d1: f32, d2: f32, d3: f32) -> f32x8 {
  o0.select(f32x8::splat(d0), o1.select(f32x8::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)))
}

#[inline]
fn visinf_vo_vf(d: f32x8) -> $ox { return d.abs().ne(f32x8::splat(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: f32x8) -> $ox { return d.ne(f32x8::splat(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: f32x8) -> $ox { return d.ne(f32x8::splat(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: f32x8) -> $ox { return d.ne(d); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, i32x8 vi2) -> f32x8 { return _mm256_i32gather_ps(ptr, vi2, 4); }

//


//

#[inline]
fn vrev21_vf_vf(d0: f32x8) -> f32x8 { return _mm256_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: i32x8) -> i32x8 { return i32x8::from(vrev21_vf_vf(f32x8::from(i))); }
