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
type $ix = __m128i;

type f32x4 = __m128;
type $ix2 = __m128i;


#[inline]
fn vtestallones_i_vo32(g: $ox) -> int { return _mm_movemask_epi8(g) == 0xFFFF; }
#[inline]
fn vtestallones_i_vo64(g: $ox) -> int { return _mm_movemask_epi8(g) == 0xFFFF; }

//

static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { _mm_storeu_si128((__m128i *)p, v); }
static void vstoreu_v_p_vi(int32_t *p, $ix v) { _mm_storeu_si128((__m128i *)p, v); }

//

#[inline]
fn vandnot_vm_vm_vm(x: u64x2, y: u64x2) -> u64x2 { return u64x2::from(_mm_andnot_pd(f64x2::from(x), f64x2::from(y))); }

#[inline]
fn vandnot_vo_vo_vo(x: $ox, y: $ox) -> $ox { return u64x2::from(_mm_andnot_pd(f64x2::from(x), f64x2::from(y))); }

#[inline]
fn vand_vm_vo64_vm(x: $ox, y: u64x2) -> u64x2 { return u64x2::from(_mm_and_pd(f64x2::from(x), f64x2::from(y))); }
#[inline]
fn vandnot_vm_vo64_vm(x: $ox, y: u64x2) -> u64x2 { return u64x2::from(_mm_andnot_pd(f64x2::from(x), f64x2::from(y))); }
#[inline]
fn vor_vm_vo64_vm(x: $ox, y: u64x2) -> u64x2 { return u64x2::from(_mm_or_pd(f64x2::from(x), f64x2::from(y))); }

#[inline]
fn vand_vm_vo32_vm(x: $ox, y: u64x2) -> u64x2 { return u64x2::from(_mm_and_pd(f64x2::from(x), f64x2::from(y))); }
#[inline]
fn vandnot_vm_vo32_vm(x: $ox, y: u64x2) -> u64x2 { return u64x2::from(_mm_andnot_pd(f64x2::from(x), f64x2::from(y))); }
#[inline]
fn vor_vm_vo32_vm(x: $ox, y: u64x2) -> u64x2 { return u64x2::from(_mm_or_pd(f64x2::from(x), f64x2::from(y))); }

#[inline]
fn vcast_vo32_vo64(m: m64x2) -> m32x2 { return _mm_shuffle_epi32(m, 0x08); }
#[inline]
fn vcast_vo64_vo32(m: m32x2) -> m64x2 { return _mm_shuffle_epi32(m, 0x50); }

//

#[inline]
fn vrint_vi_vd(vd: f64x2) -> $ix { return _mm_cvtpd_epi32(vd); }
#[inline]
fn vtruncate_vi_vd(vd: f64x2) -> $ix { return _mm_cvttpd_epi32(vd); }
impl RInt for f64x2 {
    #[inline]
    fn rint(self) -> Self {
        _mm_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
    }
}
#[inline]
fn vrint_vf_vf(vd: f32x4) -> f32x4 { return _mm_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }

impl Truncate for f64x2 {
    #[inline]
    fn truncate(self) -> Self {
        _mm_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
    }
}
#[inline]
fn vtruncate_vf_vf(vf: f32x4) -> f32x4 { return _mm_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }


impl FromU32 for u64x2 {
  fn from_u32(i: (u32, u32)) -> Self {
      u64x2::from(m32x4::new(i0, i1, i0, i1))
  }
}
/*
#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $ux { return _mm_set_epi32(i0, i1, i0, i1); }
*/
#[inline]
fn veq64_vo_vm_vm(x: $ux, y: $ux) -> $ox { return _mm_cmpeq_epi64(x, y); }
#[inline]
fn vadd64_vm_vm_vm(x: $ux, y: $ux) -> $ux { return _mm_add_epi64(x, y); }

impl Rec for f64x2 {
    #[inline]
    fn rec(self) -> Self {
        _mm_div_pd(_mm_set1_pd(1.), self)
    }
}
impl Sqrt for f64x2 {
    #[inline]
    fn sqrt(self) -> Self {
        _mm_sqrt_pd(x)
    }
}
impl Abs for f64x2 {
    fn abs(self) -> Self {
        _mm_andnot_pd(_mm_set1_pd(-0.0), d)
    }
}
impl Mla for f64x2 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        _mm_fmadd_pd(x, y, z)
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        _mm_fmsub_pd(x, y, z)
    }
}

impl Fma for $f64x {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
      _mm_fmadd_pd(x, y, z)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self {
      _mm_fmsub_pd(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
      _mm_fnmadd_pd(x, y, z)
    }
}

//


#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_andnot_si128(x, y); }

#[inline]
fn vand_vi_vo_vi(x: $ox, y: $ix) -> $ix { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vi_vo_vi(x: $ox, y: $ix) -> $ix { return _mm_andnot_si128(x, y); }

#[inline]
fn vsrl_vi_vi_i(x: $ix, c: int) -> $ix { return _mm_srli_epi32(x, c); }

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
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> f64x2 { return _mm_i32gather_pd(ptr, vi, 8); }


//

#[inline]
fn vrint_vi2_vf(vf: f32x4) -> $ix2 { return $ix2::from(_mm_cvtps_epi32(vf)); }
#[inline]
fn vtruncate_vi2_vf(vf: f32x4) -> $ix2 { return $ix2::from(_mm_cvttps_epi32(vf)); }
impl Rec for f32x4 {
    #[inline]
    fn rec(self) -> Self {
        Self::splat(1.) / self
    }
}
impl Sqrt for $f32x {
    #[inline]
    fn sqrt(self) -> Self {
        _mm_sqrt_ps(x)
    }
}

impl Abs for f32x4 {
    fn abs(self) -> Self {
        f32x4::from(vandnot_vm_vm_vm(m32x4::from(f32x4::splat(-0.)), m32x4::from(f)))
    }
}
impl Mla for f32x4 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        _mm_fmadd_ps(x, y, z)
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x4, y: f32x4, z: f32x4) -> f32x4 { return _mm_fnmadd_ps(x, y, z); }

impl Fma for f32x4 {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
        _mm_fmadd_ps(x, y, z)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self {
        _mm_fmsub_ps(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        _mm_fnmadd_ps(x, y, z)
    }
}

#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_andnot_si128(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: $ox, y: $ix2) -> $ix2 { $ix2::from(x) & y }
#[inline]
fn vandnot_vi2_vo_vi2(x: $ox, y: $ix2) -> $ix2 { return vandnot_vi2_vi2_vi2($ix2::from(x), y); }

#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return _mm_srli_epi32(x, c); }

#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_cmpgt_epi32(x, y); }

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
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> f32x4 { return _mm_i32gather_ps(ptr, vi2, 4); }


//

#[inline]
fn vrev21_vf_vf(d0: f32x4) -> f32x4 { return _mm_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf(f32x4::from(i))); }
