//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

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

typedef __m128i $mx;
typedef __m128i $mox;

typedef __m128d $f64x;
typedef __m128i $ix;

typedef __m128  $f32x;
typedef __m128i $ix2;

//

#ifndef __SLEEF_H__
void Sleef_x86CpuID(int32_t out[4], uint32_t eax, uint32_t ecx);
#endif

static int cpuSupportsSSE2() {
    int32_t reg[4];
    Sleef_x86CpuID(reg, 1, 0);
    return (reg[3] & (1 << 26)) != 0;
}

static int cpuSupportsSSE3() {
    int32_t reg[4];
    Sleef_x86CpuID(reg, 1, 0);
    return (reg[2] & (1 << 0)) != 0;
}

static int cpuSupportsSSE4_1() {
    int32_t reg[4];
    Sleef_x86CpuID(reg, 1, 0);
    return (reg[2] & (1 << 19)) != 0;
}


#[inline]
fn vprefetch_v_p(const void *ptr) -> void { _mm_prefetch(ptr, _MM_HINT_T0); }

#[inline]
fn vtestallones_i_vo32(g: $mox) -> int { return _mm_movemask_epi8(g) == 0xFFFF; }
#[inline]
fn vtestallones_i_vo64(g: $mox) -> int { return _mm_movemask_epi8(g) == 0xFFFF; }

//

static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { _mm_storeu_si128((__m128i *)p, v); }

static void vstoreu_v_p_vi(int32_t *p, $ix v) { _mm_storeu_si128((__m128i *)p, v); }

//

#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm_andnot_si128(x, y); }

#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { return _mm_andnot_si128(x, y); }

#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return _mm_and_si128(x, y); }
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return _mm_or_si128(x, y); }
#[inline]
fn vandnot_vm_vo64_vm(x: $mx, y: $mx) -> $mx { return _mm_andnot_si128(x, y); }
#[inline]
fn vxor_vm_vo64_vm(x: $mx, y: $mx) -> $mx { return _mm_xor_si128(x, y); }

#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return _mm_and_si128(x, y); }
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return _mm_or_si128(x, y); }
#[inline]
fn vandnot_vm_vo32_vm(x: $mx, y: $mx) -> $mx { return _mm_andnot_si128(x, y); }
#[inline]
fn vxor_vm_vo32_vm(x: $mx, y: $mx) -> $mx { return _mm_xor_si128(x, y); }

#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox { return _mm_shuffle_epi32(m, 0x08); }
#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox { return _mm_shuffle_epi32(m, 0x50); }

//

#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix { return _mm_cvtpd_epi32(vd); }
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix { return _mm_cvttpd_epi32(vd); }

#ifdef __SSE4_1__
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return _mm_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return _mm_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vf_vf(vf: $f32x) -> $f32x { return _mm_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return _mm_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox { return _mm_cmpeq_epi64(x, y); }
#define FULL_FP_ROUNDING
#else
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return $f64x::from(vtruncate_vi_vd(vd)); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return $f64x::from(vrint_vi_vd(vd)); }
#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox {
  $mx t = _mm_cmpeq_epi32(x, y);
  t & _mm_shuffle_epi32(t, 0xb1)
}
#endif

#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm_add_epi64(x, y); }

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $mx { return _mm_set_epi32(i0, i1, i0, i1); }

impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        _mm_div_pd(_mm_set1_pd(1.), self)
    }
}
#[inline]
fn vsqrt_vd_vd(x: $f64x) -> $f64x { return _mm_sqrt_pd(x); }
impl Abs for $f64x {
    fn abs(self) -> Self {
        _mm_andnot_pd(_mm_set1_pd(-0.0), d)
    }
}
impl Mla for $f64x {
    fn mla(self, y: Self, z: Self) -> Self {
        x*y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x*y - z; }


#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_andnot_si128(x, y); }

#[inline]
fn vand_vi_vo_vi(x: $mox, y: $ix) -> $ix { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vi_vo_vi(x: $mox, y: $ix) -> $ix { return _mm_andnot_si128(x, y); }

#[inline]
fn vsll_vi_vi_i(x: $ix, c: int) -> $ix { return _mm_slli_epi32(x, c); }
#[inline]
fn vsrl_vi_vi_i(x: $ix, c: int) -> $ix { return _mm_srli_epi32(x, c); }
#[inline]
fn vsra_vi_vi_i(x: $ix, c: int) -> $ix { return _mm_srai_epi32(x, c); }


#ifdef __SSE4_1__
#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix { return _mm_blendv_epi8(y, x, m); }

#[inline]
fn vsel_vd_vo_vd_vd(m: $mox, x: $f64x, y: $f64x) -> $f64x { return _mm_blendv_pd(y, x, _mm_castsi128_pd(m)); }
#else
#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix { (m & x) | vandnot_vm_vm_vm(m, y) }

#[inline]
fn vsel_vd_vo_vd_vd(opmask: $mox, x: $f64x, y: $f64x) -> $f64x {
  $f64x::from((opmask & $mx::from(x)) | vandnot_vm_vm_vm(opmask, $mx::from(y)))
}
#endif

#[inline]
fn vsel_vd_vo_d_d(o: $mox, v1: f64, v0: f64) -> CONST -> $f64x {
  return vsel_vd_vo_vd_vd(o, $f64x::splat(v1), $f64x::splat(v0));
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $mox, o1: $mox, d0: f64, d1: f64, d2: f64) -> $f64x {
  return vsel_vd_vo_vd_vd(o0, $f64x::splat(d0), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $mox, o1: $mox, o2: $mox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  return vsel_vd_vo_vd_vd(o0, $f64x::splat(d0), vsel_vd_vo_vd_vd(o1, $f64x::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)));
}

#[inline]
fn visinf_vo_vd(d: $f64x) -> $mox {
  return $mx::from(_mm_cmpeq_pd(d.abs(), _mm_set1_pd(SLEEF_INFINITY)));
}

#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox {
  return $mx::from(_mm_cmpeq_pd(d, _mm_set1_pd(SLEEF_INFINITY)));
}

#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox {
  return $mx::from(_mm_cmpeq_pd(d, _mm_set1_pd(-SLEEF_INFINITY)));
}

#[inline]
fn visnan_vo_vd(d: $f64x) -> $mox {
  return $mx::from(_mm_cmpneq_pd(d, d));
}

//

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x {
  int a[sizeof($ix)/sizeof(int)];
  vstoreu_v_p_vi(a, vi);
  return _mm_set_pd(ptr[a[1]], ptr[a[0]]);
}


//
#[inline]
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { return _mm_cvtps_epi32(vf); }
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return _mm_cvttps_epi32(vf); }

#ifndef __SSE4_1__
#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vf: $f32x) -> $f32x { return vcast_vf_vi2(vrint_vi2_vf(vf)); }
#endif
impl Rec for $f32x {
    #[inline]
    fn rec(self) -> Self {
        Self::splat(1.) / self
    }
}
#[inline]
fn vsqrt_vf_vf(x: $f32x) -> $f32x { return _mm_sqrt_ps(x); }

impl Abs for $f32x {
    fn abs(self) -> Self {
        $f32x::from(vandnot_vm_vm_vm($mx::from($f32x::splat(-0.)), $mx::from(f)))
    }
}

impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return z - x * y); }


#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vandnot_vi_vi_vi(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vand_vi_vo_vi(x, y); }
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vandnot_vi_vo_vi(x, y); }

#[inline]
fn vsll_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return vsll_vi_vi_i(x, c); }
#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return vsrl_vi_vi_i(x, c); }
#[inline]
fn vsra_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return vsra_vi_vi_i(x, c); }

#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_cmpgt_epi32(x, y); }

#ifdef __SSE4_1__
#[inline]
fn vsel_vi2_vo_vi2_vi2(m: $mox, x: $ix2, y: $ix2) -> $ix2 { return _mm_blendv_epi8(y, x, m); }

#[inline]
fn vsel_vf_vo_vf_vf(m: $mox, x: $f32x, y: $f32x) -> $f32x { return _mm_blendv_ps(y, x, _mm_castsi128_ps(m)); }
#else
#[inline]
fn vsel_vi2_vo_vi2_vi2(m: $mox, x: $ix2, y: $ix2) -> $ix2 {
  (m & x) | vandnot_vi2_vi2_vi2(m, y)
}

#[inline]
fn vsel_vf_vo_vf_vf(mask: $mox, x: $f32x, y: $f32x) -> $f32x {
  $f32x::from((mask & $mx::from(x)) | vandnot_vm_vm_vm(mask, $mx::from(y)))
}
#endif

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
fn visinf_vo_vf(d: $f32x) -> $mox { return d.abs().ne($f32x::splat(SLEEF_INFINITY_F)); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(SLEEF_INFINITY_F)); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(-SLEEF_INFINITY_F)); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return d.ne(d); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi) -> $f32x {
  int a[VECTLENSP];
  vstoreu_v_p_vi2(a, vi);
  return _mm_set_ps(ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]]);
}
//

#define PNMASK (($f64x) { +0.0, -0.0 })
#define NPMASK (($f64x) { -0.0, +0.0 })
#define PNMASKf (($f32x) { +0., -0., +0., -0. })
#define NPMASKf (($f32x) { -0., +0., -0., +0. })

#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return $f64x::from($mx::from(d) ^ $mx::from(PNMASK)); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return $f64x::from($mx::from(d) ^ $mx::from(NPMASK)); }
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return $f32x::from($mx::from(d) ^ $mx::from(PNMASKf)); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return $f32x::from($mx::from(d) ^ $mx::from(NPMASKf)); }

#ifdef __SSE3__
#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return _mm_addsub_pd(x, y); }
#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { return _mm_addsub_ps(x, y); }
#else
#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return x + vnegpos_vd_vd(y); }
#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { return x + vnegpos_vf_vf(y); }
#endif
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vsubadd_vd_vd_vd(x*y, z); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vsubadd_vf_vf_vf(x * y, z); }


//

#[inline]
fn vrev21_vf_vf(d0: $f32x) -> $f32x { return _mm_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf($f32x::from(i))); }
