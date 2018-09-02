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

type $mx = __m256i;
type $mox = __m256i;

type $f64x = __m256d;
type $ix = __m128i;

type $f32x = __m256;
type $ix2 = __m256i;

//

#ifndef __SLEEF_H__
void Sleef_x86CpuID(int32_t out[4], uint32_t eax, uint32_t ecx);
#endif

static int cpuSupportsAVX2() {
    int32_t reg[4];
    Sleef_x86CpuID(reg, 7, 0);
    return (reg[1] & (1 << 5)) != 0;
}

static int cpuSupportsFMA() {
    int32_t reg[4];
    Sleef_x86CpuID(reg, 1, 0);
    return (reg[2] & (1 << 12)) != 0;
}


#[inline]
fn vprefetch_v_p(const void *ptr) -> void { _mm_prefetch(ptr, _MM_HINT_T0); }

#[inline]
fn vtestallones_i_vo32(g: $mox) -> int {
  return _mm_test_all_ones(_mm_and_si128(_mm256_extractf128_si256(g, 0), _mm256_extractf128_si256(g, 1)));
}

#[inline]
fn vtestallones_i_vo64(g: $mox) -> int {
  return _mm_test_all_ones(_mm_and_si128(_mm256_extractf128_si256(g, 0), _mm256_extractf128_si256(g, 1)));
}

//

static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { _mm256_storeu_si256((__m256i *)p, v); }
static void vstoreu_v_p_vi(int32_t *p, $ix v) { _mm_storeu_si128((__m128i *)p, v); }

//

#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx { return $mx::from(_mm256_andnot_pd($f64x::from(x), $f64x::from(y))); }

#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { return $mx::from(_mm256_andnot_pd($f64x::from(x), $f64x::from(y))); }
//#[inline]
//fn vxor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return $mx::from(_mm256_xor_pd($f64x::from(x), $f64x::from(y))); }

#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return $mx::from(_mm256_and_pd($f64x::from(x), $f64x::from(y))); }
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return $mx::from(_mm256_andnot_pd($f64x::from(x), $f64x::from(y))); }
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return $mx::from(_mm256_or_pd($f64x::from(x), $f64x::from(y))); }
#[inline]
fn vxor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return $mx::from(_mm256_xor_pd($f64x::from(x), $f64x::from(y))); }

#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return $mx::from(_mm256_and_pd($f64x::from(x), $f64x::from(y))); }
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return $mx::from(_mm256_andnot_pd($f64x::from(x), $f64x::from(y))); }
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return $mx::from(_mm256_or_pd($f64x::from(x), $f64x::from(y))); }
#[inline]
fn vxor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return $mx::from(_mm256_xor_pd($f64x::from(x), $f64x::from(y))); }

#[inline]
fn vcast_vo32_vo64(o: $mox) -> $mox {
  return _mm256_permutevar8x32_epi32(o, _mm256_set_epi32(0, 0, 0, 0, 6, 4, 2, 0));
}

#[inline]
fn vcast_vo64_vo32(o: $mox) -> $mox {
  return _mm256_permutevar8x32_epi32(o, _mm256_set_epi32(3, 3, 2, 2, 1, 1, 0, 0));
}

//

#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix { return _mm256_cvtpd_epi32(vd); }
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix { return _mm256_cvttpd_epi32(vd); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return _mm256_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return _mm256_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return _mm256_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vf_vf(vf: $f32x) -> $f32x { return _mm256_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $mx {
  return _mm256_set_epi32(i0, i1, i0, i1, i0, i1, i0, i1);
}

#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox { return _mm256_cmpeq_epi64(x, y); }
#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm256_add_epi64(x, y); }

//

impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        _mm256_div_pd(_mm256_set1_pd(1.), self)
    }
}
#[inline]
fn vsqrt_vd_vd(x: $f64x) -> $f64x { return _mm256_sqrt_pd(x); }
impl Abs for $f64x {
    fn abs(self) -> Self {
        _mm256_andnot_pd(_mm256_set1_pd(-0.0), d)
    }
}
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        _mm256_fmadd_pd(x, y, z)
    }
  }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_fmsub_pd(x, y, z); }
#[inline]
fn vmlanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_fnmadd_pd(x, y, z); }

#[inline]
fn vfma_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_fmadd_pd(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_fmadd_pd(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_fmsub_pd(x, y, z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_fnmadd_pd(x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_fnmsub_pd(x, y, z); }


//


#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_andnot_si128(x, y); }

#[inline]
fn vandnot_vi_vo_vi(m: $mox, y: $ix) -> $ix { return _mm_andnot_si128(_mm256_castsi256_si128(m), y); }
#[inline]
fn vand_vi_vo_vi(m: $mox, y: $ix) -> $ix { return _mm_and_si128(_mm256_castsi256_si128(m), y); }

#[inline]
fn vsll_vi_vi_i(x: $ix, c: int) -> $ix { return _mm_slli_epi32(x, c); }
#[inline]
fn vsrl_vi_vi_i(x: $ix, c: int) -> $ix { return _mm_srli_epi32(x, c); }
#[inline]
fn vsra_vi_vi_i(x: $ix, c: int) -> $ix { return _mm_srai_epi32(x, c); }


#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix { return _mm_blendv_epi8(y, x, _mm256_castsi256_si128(m)); }

#[inline]
fn vsel_vd_vo_vd_vd(o: $mox, x: $f64x, y: $f64x) -> $f64x { return _mm256_blendv_pd(y, x, _mm256_castsi256_pd(o)); }
#[inline]
fn vsel_vd_vo_d_d(o: $mox, v1: f64, v0: f64) -> $f64x { return _mm256_permutevar_pd(_mm256_set_pd(v1, v0, v1, v0), o); }

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d($mox o0, $mox o1, $mox o2, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  __m256i v = _mm256_castpd_si256(vsel_vd_vo_vd_vd(o0, _mm256_castsi256_pd(_mm256_set_epi32(1, 0, 1, 0, 1, 0, 1, 0)),
						   vsel_vd_vo_vd_vd(o1, _mm256_castsi256_pd(_mm256_set_epi32(3, 2, 3, 2, 3, 2, 3, 2)),
								    vsel_vd_vo_vd_vd(o2, _mm256_castsi256_pd(_mm256_set_epi32(5, 4, 5, 4, 5, 4, 5, 4)),
										     _mm256_castsi256_pd(_mm256_set_epi32(7, 6, 7, 6, 7, 6, 7, 6))))));
  return _mm256_castsi256_pd(_mm256_permutevar8x32_epi32(_mm256_castpd_si256(_mm256_set_pd(d3, d2, d1, d0)), v));
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $mox, o1: $mox, d0: f64, d1: f64, d2: f64) -> $f64x {
  return vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o1, d0, d1, d2, d2);
}

#[inline]
fn visinf_vo_vd(d: $f64x) -> $mox {
  return $mx::from(_mm256_cmp_pd(d.abs(), _mm256_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox {
  return $mx::from(_mm256_cmp_pd(d, _mm256_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox {
  return $mx::from(_mm256_cmp_pd(d, _mm256_set1_pd(-SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visnan_vo_vd(d: $f64x) -> $mox {
  return $mx::from(_mm256_cmp_pd(d, d, _CMP_NEQ_UQ));
}

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x { return _mm256_i32gather_pd(ptr, vi, 8); }

//

#[inline]
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { return $ix2::from(_mm256_cvtps_epi32(vf)); }
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return $ix2::from(_mm256_cvttps_epi32(vf)); }
impl Rec for $f32x {
    #[inline]
    fn rec(self) -> Self {
        Self::splat(1.) / self
    }
}
#[inline]
fn vsqrt_vf_vf(x: $f32x) -> $f32x { return _mm256_sqrt_ps(x); }

impl Abs for $f32x {
    fn abs(self) -> Self {
        $f32x::from(vandnot_vm_vm_vm($mx::from($f32x::splat(-0.)), $mx::from(f)))
    }
}
impl Mla for $f32x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        _mm256_fmadd_ps(x, y, z)
    }
}
#[inline]
fn vmlapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_fmsub_ps(x, y, z); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_fnmadd_ps(x, y, z); }

#[inline]
fn vfma_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_fmadd_ps(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_fmadd_ps(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_fmsub_ps(x, y, z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_fnmadd_ps(x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_fnmsub_ps(x, y, z); }

#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm256_andnot_si256(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { $ix2::from(x) & y }
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vandnot_vi2_vi2_vi2($ix2::from(x), y); }

#[inline]
fn vsll_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return _mm256_slli_epi32(x, c); }
#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return _mm256_srli_epi32(x, c); }
#[inline]
fn vsra_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return _mm256_srai_epi32(x, c); }

#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm256_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm256_cmpgt_epi32(x, y); }

#[inline]
fn vsel_vi2_vo_vi2_vi2(m: $mox, x: $ix2, y: $ix2) -> $ix2 {
  return _mm256_blendv_epi8(y, x, m);
}

#[inline]
fn vsel_vf_vo_vf_vf(o: $mox, x: $f32x, y: $f32x) -> $f32x { return _mm256_blendv_ps(y, x, _mm256_castsi256_ps(o)); }

// At this point, the following three functions are implemented in a generic way,
// but I will try target-specific optimization later on.
#[inline]
fn vsel_vf_vo_f_f(o: $mox, v1: f32, v0: f32) -> CONST -> $f32x {
  return vsel_vf_vo_vf_vf(o, $f32x::splat(v1), $f32x::splat(v0));
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: $mox, o1: $mox, d0: f32, d1: f32, d2: f32) -> $f32x {
  return vsel_vf_vo_vf_vf(o0, $f32x::splat(d0), vsel_vf_vo_f_f(o1, d1, d2));
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $mox, o1: $mox, o2: $mox, d0: f32, d1: f32, d2: f32, d3: f32) -> $f32x {
  return vsel_vf_vo_vf_vf(o0, $f32x::splat(d0), vsel_vf_vo_vf_vf(o1, $f32x::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)));
}

#[inline]
fn visinf_vo_vf(d: $f32x) -> $mox { return d.abs().ne($f32x::splat(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return d.ne(d); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x { return _mm256_i32gather_ps(ptr, vi2, 4); }

//

#define PNMASK (($f64x) { +0.0, -0.0, +0.0, -0.0 })
#define NPMASK (($f64x) { -0.0, +0.0, -0.0, +0.0 })
#define PNMASKf (($f32x) { +0., -0., +0., -0., +0., -0., +0., -0. })
#define NPMASKf (($f32x) { -0., +0., -0., +0., -0., +0., -0., +0. })

#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return $f64x::from($mx::from(d) ^ $mx::from(PNMASK)); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return $f64x::from($mx::from(d) ^ $mx::from(NPMASK)); }
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return $f32x::from($mx::from(d) ^ $mx::from(PNMASKf)); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return $f32x::from($mx::from(d) ^ $mx::from(NPMASKf)); }

#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return _mm256_addsub_pd(x, y); }
#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { return _mm256_addsub_ps(x, y); }

#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x.mla(y, vnegpos_vd_vd(z)); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return x.mla(y, vnegpos_vf_vf(z)); }



//

#[inline]
fn vrev21_vf_vf(d0: $f32x) -> $f32x { return _mm256_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf($f32x::from(i))); }
