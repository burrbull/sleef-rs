//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#if CONFIG == 1

#if !defined(__AVX__)
#error Please specify -mavx.
#endif

#elif CONFIG == 4

#if !defined(__AVX__) || !defined(__FMA4__)
#error Please specify -mavx and -mfma4.
#endif

#else
#error CONFIG macro invalid or not defined
#endif

#define ENABLE_DP
#define LOG2VECTLENDP 2
#define VECTLENDP (1 << LOG2VECTLENDP)

#define ENABLE_SP
#define LOG2VECTLENSP (LOG2VECTLENDP+1)
#define VECTLENSP (1 << LOG2VECTLENSP)

#define FULL_FP_ROUNDING
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
typedef struct { __m128i x, y; } i32x8s; // i64x4s



static void vstoreu_v_p_vi2(int32_t *p, i32x8s v) {
  _mm_storeu_si128((__m128i *) p     , v.x);
  _mm_storeu_si128((__m128i *)(p + 4), v.y);  
}

static void vstoreu_v_p_vi(int32_t *p, i32x4 v) { _mm_storeu_si128((__m128i *)p, v); }

//

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


#if CONFIG == 1
impl Mla for f64x4 {
    #[inline]
    fn mul_sub(self, y: Self, z: Self) -> Self {
        x*y - z
    }
}
#else
impl Mla for f64x4 {
    #[inline]
    fn mul_sub(self, y: Self, z: Self) -> Self {
        _mm256_msub_pd(x, y, z)
    }
}

#[target_feature(enable = "fma")]
impl Fma for f64x4 {
    #[inline]
    fn mul_sube(self, y: Self, z: Self) -> Self {
      _mm256_msub_pd(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
      _mm256_nmacc_pd(x, y, z)
    }
}


//

/*
#[inline]
fn vandnot_vi_vi_vi(x: i32x4, y: i32x4) -> i32x4 { return _mm_andnot_si128(x, y); }

#[inline]
fn vandnot_vi_vo_vi(m: $ox, y: i32x4) -> i32x4 { return _mm_andnot_si128(_mm256_castsi256_si128(m), y); }
#[inline]
fn vand_vi_vo_vi(m: $ox, y: i32x4) -> i32x4 { return _mm_and_si128(_mm256_castsi256_si128(m), y); }

#[inline]
fn vsel_vd_vo_d_d(o: $ox, v1: f64, v0: f64) -> f64x4 {
  o.select(f64x4::splat(v1), f64x4::splat(v0))
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $ox, o1: $ox, d0: f64, d1: f64, d2: f64) -> f64x4 {
  o0.select(f64x4::splat(d0), vsel_vd_vo_d_d(o1, d1, d2))
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $ox, o1: $ox, o2: $ox, d0: f64, d1: f64, d2: f64, d3: f64) -> f64x4 {
  o0.select(f64x4::splat(d0), o1.select(f64x4::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)))
}
*/

#[inline]
fn vgather_vd_p_vi(const double *ptr, i32x4 vi) -> f64x4 {
  int a[VECTLENDP];
  vstoreu_v_p_vi(a, vi);
  return _mm256_set_pd(ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]]);
}


impl Round for f32x8 {
    type Int = i32x8s;
    #[inline]
    fn truncate(self) -> Self {
        _mm256_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
    }
    fn truncatei(self) -> Self::Int {
        Self::Int::from(_mm256_cvttps_epi32(vf))
    }
    #[inline]
    fn rint(self) -> Self {
      _mm256_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
    }
    #[inline]
    fn rinti(self) -> Self::Int {
      Self::Int::from(_mm256_cvtps_epi32(vf))
    }
}

#if CONFIG == 1
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x8, y: f32x8, z: f32x8) -> f32x8 { return z - x * y); }
#else
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x8, y: f32x8, z: f32x8) -> f32x8 { return _mm256_nmacc_ps(x, y, z); }


#[target_feature(enable = "fma")]
impl Fma for f32x8 {
    #[inline]
    fn mul_sube(self, y: Self, z: Self) -> Self {
        _mm256_msub_ps(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        _mm256_nmacc_ps(x, y, z)
    }
}

#[inline]
fn vandnot_vi2_vi2_vi2(i32x8s x, i32x8s y) -> i32x8s {
  i32x8s vi = { _mm_andnot_si128(x.x, y.x), _mm_andnot_si128(x.y, y.y) };
  return vi;
}


#[inline]
fn vand_vi2_vo_vi2(x: $ox, y: i32x8s) -> i32x8s { return i32x8s::from(x) & y }


#[inline]
fn veq_vi2_vi2_vi2(i32x8s x, i32x8s y) -> i32x8s {
  i32x8s r;
  r.x = _mm_cmpeq_epi32(x.0, y.0);
  r.y = _mm_cmpeq_epi32(x.1, y.1);
  return r;
}

#[inline]
fn vgt_vi2_vi2_vi2(i32x8s x, i32x8s y) -> i32x8s {
  i32x8s r;
  r.x = _mm_cmpgt_epi32(x.0, y.0);
  r.y = _mm_cmpgt_epi32(x.1, y.1);
  return r;
}

/*
#[inline]
fn vsel_vf_vo_f_f(o: $ox, v1: f32, v0: f32) -> f32x8 {
  o.select(f32x8::splat(v1), f32x8::splat(v0))
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: $ox, o1: $ox, d0: f32, d1: f32, d2: f32) -> f32x8 {
  o0.select(f32x8::splat(d0), vsel_vf_vo_f_f(o1, d1, d2))
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $ox, o1: $ox, o2: $ox, d0: f32, d1: f32, d2: f32, d3: f32) -> f32x8 {
  o0.select(f32x8::splat(d0), o1.select(f32x8::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)))
}
*/
#[inline]
fn vgather_vf_p_vi2(const float *ptr, i32x8s vi2) -> f32x8 {
  int a[VECTLENSP];
  vstoreu_v_p_vi2(a, vi2);
  return _mm256_set_ps(ptr[a[7]], ptr[a[6]], ptr[a[5]], ptr[a[4]],
		       ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]]);
}

//

#[inline]
fn vrev21_vf_vf(d0: f32x8) -> f32x8 { return _mm256_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: i32x8s) -> i32x8s { return i32x8s::from(vrev21_vf_vf(f32x8::from(i))); }
