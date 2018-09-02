//          Copyright Naoki Shibata 2010 - 2018.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#if CONFIG == 1 || CONFIG == 2

#ifndef __AVX512F__
#error Please specify -mavx512f.
#endif

#else
#error CONFIG macro invalid or not defined
#endif

#define ENABLE_DP
#define LOG2VECTLENDP 3
#define VECTLENDP (1 << LOG2VECTLENDP)

#define ENABLE_SP
#define LOG2VECTLENSP (LOG2VECTLENDP+1)
#define VECTLENSP (1 << LOG2VECTLENSP)

#if CONFIG == 1
#define ENABLE_FMA_DP
#define ENABLE_FMA_SP
#define SPLIT_KERNEL
#endif

#define FULL_FP_ROUNDING
#define ACCURATE_SQRT

#if defined(_MSC_VER)
#include <intrin.h>
#else
#include <x86intrin.h>
#endif

#include <stdint.h>
#include "misc.h"

type $ux = __m512i;
type $ox = __mmask16;

type f64x8 = __m512d;
type i32x8 = __m256i;

type f32x16 = __m512;
type i32x16 = __m512i;


#ifdef __INTEL_COMPILER
#[inline]
fn vtestallones_i_vo64(g: m1x8) -> int { _mm512_mask2int(g) == 0xff }
#[inline]
fn vtestallones_i_vo32(g: m1x16) -> int { _mm512_mask2int(g) == 0xffff }
#else
#[inline]
fn vtestallones_i_vo64(g: m1x8) -> int { g == 0xff }
#[inline]
fn vtestallones_i_vo32(g: m1x16) -> int { g == 0xffff }
#endif

//

static void vstoreu_v_p_vi2(int32_t *p, i32x16 v) { _mm512_storeu_si512((__m512i *)p, v); }
static void vstoreu_v_p_vi(int32_t *p, i32x8 v) { _mm256_storeu_si256((__m256i *)p, v); }

//

#[inline]
fn vandnot_vm_vm_vm(x: $ux, y: $ux) -> $ux { _mm512_andnot_si512(x, y) }

#[inline]
fn vandnot_vo_vo_vo(x: $ox, y: $ox) -> $ox { _mm512_kandn(x, y) }

#[inline]
fn vand_vm_vo64_vm(o: m1x8, m: u64x8) -> u64x8 { _mm512_mask_and_epi64(_mm512_set1_epi32(0), o, m, m) }
#[inline]
fn vandnot_vm_vo64_vm(o: m1x8, m: u64x8) -> u64x8 { _mm512_mask_and_epi64(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)) }
#[inline]
fn vor_vm_vo64_vm(o: m1x8, m: u64x8) -> u64x8 { _mm512_mask_or_epi64(m, o, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1)) }

#[inline]
fn vand_vm_vo32_vm(o: m1x16, m: u32x16) -> u32x16 { _mm512_mask_and_epi32(_mm512_set1_epi32(0), o, m, m) }
#[inline]
fn vandnot_vm_vo32_vm(o: m1x16, m: u32x16) -> u32x16 { _mm512_mask_and_epi32(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)) }
#[inline]
fn vor_vm_vo32_vm(o: m1x16, m: u32x16) -> u32x16 { _mm512_mask_or_epi32(m, o, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1)) }

#[inline]
fn vcast_vo32_vo64(o: m1x8) -> $ox { o }
#[inline]
fn vcast_vo64_vo32(o: m1x16) -> $ox { o }

//

#[inline]
fn vrint_vi_vd(vd: f64x8) -> i32x8 {
  _mm512_cvt_roundpd_epi32(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)
}

#[inline]
fn vtruncate_vi_vd(vd: f64x8) -> i32x8 {
  _mm512_cvt_roundpd_epi32(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)
}

impl Truncate for f64x8 {
    #[inline]
    fn truncate(self) -> Self {
        let hi = _mm512_extractf64x4_pd(vd, 1);
        let lo = _mm512_extractf64x4_pd(vd, 0);
        let hi = _mm256_round_pd(hi, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
        let lo = _mm256_round_pd(lo, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
        _mm512_insertf64x4(_mm512_castpd256_pd512(lo), hi, 1)
    }
}
}
impl RInt for f64x8 {
    #[inline]
    fn rint(self) -> Self {
        let hi = _mm512_extractf64x4_pd(vd, 1);
        let lo = _mm512_extractf64x4_pd(vd, 0);
        let hi = _mm256_round_pd(hi, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
        let lo = _mm256_round_pd(lo, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
        _mm512_insertf64x4(_mm512_castpd256_pd512(lo), hi, 1)
    }
}

impl FromU32 for u64x8 {
  fn from_u32(i: (u32, u32)) -> Self {
      u64x8::from(m1x16::new(i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1))
  }
}
/*#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $ux { _mm512_set_epi32(i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1) }
*/
#[inline]
fn veq64_vo_vm_vm(x: $ux, y: $ux) -> m1x8 { _mm512_cmp_epi64_mask(x, y, _MM_CMPINT_EQ) }
#[inline]
fn vadd64_vm_vm_vm(x: $ux, y: $ux) -> $ux { _mm512_add_epi64(x, y) }

//

impl Rec for f64x8 {
    #[inline]
    fn rec(self) -> Self {
        _mm512_div_pd(_mm512_set1_pd(1.), self)
    }
}

impl Sqrt for f64x8 {
    #[inline]
    fn sqrt(self) -> Self {
        _mm512_sqrt_pd(x)
    }
}
impl Abs for f64x8 {
    fn abs(self) -> Self {
        f64x8::from(_mm512_andnot_si512($ux::from_bits(_mm512_set1_pd(-0.0)), $ux::from_bits(d)))
    }
}

#if CONFIG == 1
impl Mla for f64x8 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        _mm512_fmadd_pd(x, y, z)
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        _mm512_fmsub_pd(x, y, z)
    }
}
#else
impl Mla for f64x8 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        x*y + z
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        x*y - z
    }
}
#endif

impl Fma for f64x8 {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
        _mm512_fmadd_pd(x, y, z)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self {
        _mm512_fmsub_pd(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        _mm512_fnmadd_pd(x, y, z)
    }
}


//


#[inline]
fn vandnot_vi_vi_vi(x: i32x8, y: i32x8) -> i32x8 { _mm256_andnot_si256(x, y) }

#[inline]
fn vandnot_vi_vo_vi(o: m1x8, y: i32x8) -> i32x8 {
  _mm512_castsi512_si256(_mm512_mask_and_epi32(_mm512_castsi256_si512(y), o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)))
}
#[inline]
fn vand_vi_vo_vi(o: m1x8, y: i32x8) -> i32x8 {
  _mm512_castsi512_si256(_mm512_mask_and_epi32(_mm512_set1_epi32(0), o, _mm512_castsi256_si512(y), _mm512_castsi256_si512(y)))
}

#define vsrl_vi_vi_i(x, c) _mm256_srli_epi32(x, c)


#[inline]
fn vsel_vd_vo_d_d(o: m1x8, v1: f64, v0: f64) -> CONST -> f64x8 {
  o.select(f64x8::splat(v1), f64x8::splat(v0))
}

#if 1
// Probably this is faster
#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: m1x8, o1: m1x8, o2: m1x8, d0: f64, d1: f64, d2: f64, d3: f64) -> f64x8 {
  __m512i v = _mm512_castpd_si512(o0.select(_mm512_castsi512_pd(_mm512_set_epi64(0, 0, 0, 0, 0, 0, 0, 0)),
						   o1.select(_mm512_castsi512_pd(_mm512_set_epi64(1, 1, 1, 1, 1, 1, 1, 1)),
								    o2.select(_mm512_castsi512_pd(_mm512_set_epi64(2, 2, 2, 2, 2, 2, 2, 2)),
										     _mm512_castsi512_pd(_mm512_set_epi64(3, 3, 3, 3, 3, 3, 3, 3))))));
  _mm512_permutexvar_pd(v, _mm512_castpd256_pd512(_mm256_set_pd(d3, d2, d1, d0)))
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: m1x8, o1: m1x8, d0: f64, d1: f64, d2: f64) -> f64x8 {
  vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o1, d0, d1, d2, d2)
}
#else
#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: m1x8, o1: m1x8, d0: f64, d1: f64, d2: f64) -> f64x8 {
  o0.select(f64x8::splat(d0), vsel_vd_vo_d_d(o1, d1, d2))
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: m1x8, o1: m1x8, o2: m1x8, d0: f64, d1: f64, d2: f64, d3: f64) -> f64x8 {
   o0.select(f64x8::splat(d0), o1.select(f64x8::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)))
}
#endif

#[inline]
fn visinf_vo_vd(d: f64x8) -> m1x8 {
  _mm512_cmp_pd_mask(d.abs(), _mm512_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ)
}

#[inline]
fn vispinf_vo_vd(d: f64x8) -> m1x8 {
  _mm512_cmp_pd_mask(d, _mm512_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ)
}

#[inline]
fn visnan_vo_vd(d: f64x8) -> m1x8 {
  _mm512_cmp_pd_mask(d, d, _CMP_NEQ_UQ)
}

#[inline]
fn vilogbk_vi_vd(d: f64x8) -> i32x8 { vrint_vi_vd(_mm512_getexp_pd(d)) }

// vilogb2k_vi_vd is similar to vilogbk_vi_vd, but the argument has to
// be a normalized FP value.
#[inline]
fn vilogb2k_vi_vd(d: f64x8) -> i32x8 { vrint_vi_vd(_mm512_getexp_pd(d));}

#[inline]
fn vgetexp_vd_vd(d: f64x8) -> f64x8 { _mm512_getexp_pd(d) }
#[inline]
fn vgetexp_vf_vf(d: f32x16) -> f32x16 { _mm512_getexp_ps(d) }

#[inline]
fn vgetmant_vd_vd(d: f64x8) -> f64x8 { _mm512_getmant_pd(d, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_nan) }
#[inline]
fn vgetmant_vf_vf(d: f32x16) -> f32x16 { _mm512_getmant_ps(d, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_nan) }

#define vfixup_vd_vd_vd_vi2_i(a, b, c, imm) _mm512_fixupimm_pd((a), (b), (c), (imm))
#define vfixup_vf_vf_vf_vi2_i(a, b, c, imm) _mm512_fixupimm_ps((a), (b), (c), (imm))

#[inline]
fn vgather_vd_p_vi(const double *ptr, i32x8 vi) -> f64x8 { _mm512_i32gather_pd(vi, ptr, 8) }


#[inline]
fn vrint_vi2_vf(vf: f32x16) -> i32x16 { i32x16::from(_mm512_cvtps_epi32(vf)) }
#[inline]
fn vtruncate_vi2_vf(vf: f32x16) -> i32x16 { i32x16::from(_mm512_cvttps_epi32(vf)) }

#[inline]
fn vtruncate_vf_vf(vd: f32x16) -> f32x16 {
  __m256 hi = _mm256_castpd_ps(_mm512_extractf64x4_pd(f64x8::from(vd), 1));
  __m256 lo = _mm256_castpd_ps(_mm512_extractf64x4_pd(f64x8::from(vd), 0));
  hi = _mm256_round_ps(hi, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  lo = _mm256_round_ps(lo, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  f32x16::from(_mm512_insertf64x4(_mm512_castpd256_pd512(_mm256_castps_pd(lo)), _mm256_castps_pd(hi), 1))
}
 
#[inline]
fn vrint_vf_vf(vd: f32x16) -> f32x16 {
  __m256 hi = _mm256_castpd_ps(_mm512_extractf64x4_pd(f64x8::from(vd), 1));
  __m256 lo = _mm256_castpd_ps(_mm512_extractf64x4_pd(f64x8::from(vd), 0));
  hi = _mm256_round_ps(hi, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  lo = _mm256_round_ps(lo, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  f32x16::from(_mm512_insertf64x4(_mm512_castpd256_pd512(_mm256_castps_pd(lo)), _mm256_castps_pd(hi), 1))
}
impl Rec for f32x16 {
    #[inline]
    fn rec(self) -> Self {
        Self::splat(1.) / self
    }
}
impl Sqrt for f32x16 {
    #[inline]
    fn sqrt(self) -> Self {
        _mm512_sqrt_ps(x)
    }
}
impl Abs for f32x16 {
    fn abs(self) -> Self {
        f32x16::from(vandnot_vm_vm_vm($ux::from_bits(f32x16::splat(-0.)), $ux::from_bits(f)))
    }
}

#if CONFIG == 1
impl Mla for f32x16 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        _mm512_fmadd_ps(x, y, z)
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x16, y: f32x16, z: f32x16) -> f32x16 { _mm512_fnmadd_ps(x, y, z) }
#else
impl Mla for f32x16 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        x*y+z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x16, y: f32x16, z: f32x16) -> f32x16 { z - x * y }
#endif

impl Fma for f32x16 {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
        _mm512_fmadd_ps(x, y, z)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self {
        _mm512_fmsub_ps(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        _mm512_fnmadd_ps(x, y, z)
    }
}

#[inline]
fn vandnot_vi2_vi2_vi2(i32x16 x, i32x16 y) -> i32x16 { _mm512_andnot_si512(x, y) }

#[inline]
fn vand_vi2_vo_vi2(o: m1x16, m: i32x16) -> i32x16 {
  _mm512_mask_and_epi32(_mm512_set1_epi32(0), o, m, m)
}

#[inline]
fn vandnot_vi2_vo_vi2(o: m1x16, m: i32x16) -> i32x16 {
  _mm512_mask_and_epi32(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0))
}

#define vsrl_vi2_vi2_i(x, c) _mm512_srli_epi32(x, c)

#[inline]
fn vgt_vi2_vi2_vi2(x: i32x16, y: i32x16) -> i32x16 {
  __mmask16 m = _mm512_cmp_epi32_mask(y, x, _MM_CMPINT_LT);
  _mm512_mask_and_epi32(_mm512_set1_epi32(0), m, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1))
}

// At this point, the following three functions are implemented in a generic way,
// but I will try target-specific optimization later on.
#[inline]
fn vsel_vf_vo_f_f(o: m1x16, v1: f32, v0: f32) -> CONST -> f32x16 {
  o.select(f32x16::splat(v1), f32x16::splat(v0))
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: m1x16, o1: m1x16, d0: f32, d1: f32, d2: f32) -> f32x16 {
  o0.select(f32x16::splat(d0), vsel_vf_vo_f_f(o1, d1, d2))
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: m1x16, o1: m1x16, o2: m1x16, d0: f32, d1: f32, d2: f32, d3: f32) -> f32x16 {
  o0.select(f32x16::splat(d0), o1.select(f32x16::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)))
}

#[inline]
fn visinf_vo_vf(d: f32x16) -> m1x16 { d.abs().ne(f32x16::splat(SLEEF_INFINITYf)) }
#[inline]
fn vispinf_vo_vf(d: f32x16) -> m1x16 { d.ne(f32x16::splat(SLEEF_INFINITYf)) }
#[inline]
fn visminf_vo_vf(d: f32x16) -> m1x16 { d.ne(f32x16::splat(-SLEEF_INFINITYf)) }
#[inline]
fn visnan_vo_vf(d: f32x16) -> m1x16 { d.ne(d) }

#[inline]
fn vilogbk_vi2_vf(d: f32x16) -> i32x16 { vrint_vi2_vf(_mm512_getexp_ps(d)) }
#[inline]
fn vilogb2k_vi2_vf(d: f32x16) -> i32x16 { vrint_vi2_vf(_mm512_getexp_ps(d)) }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, i32x16 vi2) -> f32x16 { _mm512_i32gather_ps(vi2, ptr, 4) }


#[inline]
fn vrev21_vf_vf(vf: f32x16) -> f32x16 { _mm512_permute_ps(vf, 0xb1) }
#[inline]
fn vrev21_vi2_vi2(i: i32x16) -> i32x16 { i32x16::from(vrev21_vf_vf(f32x16::from(i))) }
