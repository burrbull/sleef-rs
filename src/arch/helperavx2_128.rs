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

type $mx = __m128i;
type $mox = __m128i;

type $f64x = __m128d;
type $ix = __m128i;

type $f32x = __m128;
type $ix2 = __m128i;

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

#if CONFIG == 1 && defined(__AVX2__)
#[inline]
fn vavailability_i(name: int) -> int {
  int d = cpuSupportsAVX2() && cpuSupportsFMA();
  return d ? 3 : 0;
}
#define ISANAME "AVX2"
#define DFTPRIORITY 25
#endif

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { _mm_prefetch(ptr, _MM_HINT_T0); }

#[inline]
fn vtestallones_i_vo32(g: $mox) -> int { return _mm_movemask_epi8(g) == 0xFFFF; }
#[inline]
fn vtestallones_i_vo64(g: $mox) -> int { return _mm_movemask_epi8(g) == 0xFFFF; }

//

#[inline]
fn vreinterpret_vm_vd(vd: $f64x) -> $mx { return _mm_castpd_si128(vd); }
#[inline]
fn vreinterpret_vd_vm(vm: $mx) -> $f64x { return _mm_castsi128_pd(vm);  }
#[inline]
fn vreinterpret_vi2_vd(vd: $f64x) -> $ix2 { return _mm_castpd_si128(vd); }
#[inline]
fn vreinterpret_vd_vi2(vi: $ix2) -> $f64x { return _mm_castsi128_pd(vi); }

//

static $ix2 vloadu_vi2_p(int32_t *p) { return _mm_loadu_si128((__m128i const *)p); }
static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { _mm_storeu_si128((__m128i *)p, v); }
static $ix vloadu_vi_p(int32_t *p) { return _mm_loadu_si128((__m128i *)p); }
static void vstoreu_v_p_vi(int32_t *p, $ix v) { _mm_storeu_si128((__m128i *)p, v); }

//

#[inline]
fn vand_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

//#[inline]
//fn vand_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vreinterpret_vm_vd(_mm_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vreinterpret_vm_vd(_mm_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
//#[inline]
//fn vor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vreinterpret_vm_vd(_mm_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
//#[inline]
//fn vxor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vreinterpret_vm_vd(_mm_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox { return _mm_shuffle_epi32(m, 0x08); }
#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox { return _mm_shuffle_epi32(m, 0x50); }

//

#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix { return _mm_cvtpd_epi32(vd); }
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix { return _mm_cvttpd_epi32(vd); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return _mm_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return _mm_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return _mm_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vf_vf(vf: $f32x) -> $f32x { return _mm_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $mx { return _mm_set_epi32(i0, i1, i0, i1); }

#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox { return _mm_cmpeq_epi64(x, y); }
#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm_add_epi64(x, y); }

//
impl std::ops::Add for $f64x {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        _mm_add_pd(self, other)
    }
}
#[inline]
fn vrec_vd_vd(x: $f64x) -> $f64x { return _mm_div_pd(_mm_set1_pd(1), x); }
#[inline]
fn vsqrt_vd_vd(x: $f64x) -> $f64x { return _mm_sqrt_pd(x); }
#[inline]
fn vabs_vd_vd(d: $f64x) -> $f64x { return _mm_andnot_pd(_mm_set1_pd(-0.0), d); }
#[inline]
fn vmla_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm_fmadd_pd(x, y, z); }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm_fmsub_pd(x, y, z); }
#[inline]
fn vmlanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm_fnmadd_pd(x, y, z); }

#[inline]
fn vfma_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm_fmadd_pd(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm_fmadd_pd(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm_fmsub_pd(x, y, z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm_fnmadd_pd(x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm_fnmsub_pd(x, y, z); }


//

#[inline]
fn vadd_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_add_epi32(x, y); }
#[inline]
fn vsub_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_sub_epi32(x, y); }
#[inline]
fn vneg_vi_vi(e: $ix) -> $ix { return vsub_vi_vi_vi(vcast_vi_i(0), e); }

#[inline]
fn vand_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_andnot_si128(x, y); }
#[inline]
fn vor_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_or_si128(x, y); }
#[inline]
fn vxor_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_xor_si128(x, y); }

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

#[inline]
fn veq_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_cmpgt_epi32(x, y); }

#[inline]
fn veq_vo_vi_vi(x: $ix, y: $ix) -> $mox { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vo_vi_vi(x: $ix, y: $ix) -> $mox { return _mm_cmpgt_epi32(x, y); }

#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix { return _mm_blendv_epi8(y, x, m); }

#[inline]
fn vsel_vd_vo_vd_vd(o: $mox, x: $f64x, y: $f64x) -> $f64x { return _mm_blendv_pd(y, x, _mm_castsi128_pd(o)); }

#[inline]
fn vsel_vd_vo_d_d(o: $mox, v1: f64, v0: f64) -> $f64x {
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
  return vreinterpret_vm_vd(_mm_cmp_pd(vabs_vd_vd(d), _mm_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox {
  return vreinterpret_vm_vd(_mm_cmp_pd(d, _mm_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox {
  return vreinterpret_vm_vd(_mm_cmp_pd(d, _mm_set1_pd(-SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visnan_vo_vd(d: $f64x) -> $mox {
  return vreinterpret_vm_vd(_mm_cmp_pd(d, d, _CMP_NEQ_UQ));
}

#[inline]
fn vload_vd_p(const double *ptr) -> $f64x { return _mm_load_pd(ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> $f64x { return _mm_loadu_pd(ptr); }

#[inline]
fn vstore_v_p_vd(double *ptr, $f64x v) -> void { _mm_store_pd(ptr, v); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, $f64x v) -> void { _mm_storeu_pd(ptr, v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x { return _mm_i32gather_pd(ptr, vi, 8); }

#if defined(_MSC_VER)
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_d_vd(v: $f64x) -> double {
  double a[VECTLENDP];
  vstoreu_v_p_vd(a, v);
  return a[0];
}
#endif

//

#[inline]
fn vcast_vi2_vm(vm: $mx) -> $ix2 { return vm; }
#[inline]
fn vcast_vm_vi2(vi: $ix2) -> $mx { return vi; }

#[inline]
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { return vcast_vi2_vm(_mm_cvtps_epi32(vf)); }
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return vcast_vi2_vm(_mm_cvttps_epi32(vf)); }

#[inline]
fn vreinterpret_vm_vf(vf: $f32x) -> $mx { return _mm_castps_si128(vf); }
#[inline]
fn vreinterpret_vf_vm(vm: $mx) -> $f32x { return _mm_castsi128_ps(vm); }

#[inline]
fn vreinterpret_vf_vi2(vi: $ix2) -> $f32x { return vreinterpret_vf_vm(vcast_vm_vi2(vi)); }
#[inline]
fn vreinterpret_vi2_vf(vf: $f32x) -> $ix2 { return vcast_vi2_vm(vreinterpret_vm_vf(vf)); }

#[inline]
fn vrec_vf_vf(x: $f32x) -> $f32x { return $f32x::splat(1.0) / x); }
#[inline]
fn vsqrt_vf_vf(x: $f32x) -> $f32x { return _mm_sqrt_ps(x); }
#[inline]
fn vabs_vf_vf(f: $f32x) -> $f32x { return vreinterpret_vf_vm(vandnot_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.0f)), vreinterpret_vm_vf(f))); }
#[inline]
fn vmla_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm_fmadd_ps(x, y, z); }
#[inline]
fn vmlapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm_fmsub_ps(x, y, z); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm_fnmadd_ps(x, y, z); }

#[inline]
fn vfma_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm_fmadd_ps(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm_fmadd_ps(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm_fmsub_ps(x, y, z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm_fnmadd_ps(x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm_fnmsub_ps(x, y, z); }

#[inline]
fn vadd_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_add_epi32(x, y); }
#[inline]
fn vsub_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_sub_epi32(x, y); }
#[inline]
fn vneg_vi2_vi2(e: $ix2) -> $ix2 { return vsub_vi2_vi2_vi2($ix2::from(0), e); }

#[inline]
fn vand_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_andnot_si128(x, y); }
#[inline]
fn vor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_or_si128(x, y); }
#[inline]
fn vxor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_xor_si128(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vand_vi2_vi2_vi2(vcast_vi2_vm(x), y); }
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vandnot_vi2_vi2_vi2(vcast_vi2_vm(x), y); }

#[inline]
fn vsll_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return _mm_slli_epi32(x, c); }
#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return _mm_srli_epi32(x, c); }
#[inline]
fn vsra_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return _mm_srai_epi32(x, c); }

#[inline]
fn veq_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return _mm_cmpgt_epi32(x, y); }
#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm_cmpgt_epi32(x, y); }

#[inline]
fn vsel_vi2_vo_vi2_vi2(m: $mox, x: $ix2, y: $ix2) -> $ix2 {
  return _mm_blendv_epi8(y, x, m);
}

#[inline]
fn vsel_vf_vo_vf_vf(o: $mox, x: $f32x, y: $f32x) -> $f32x { return _mm_blendv_ps(y, x, _mm_castsi128_ps(o)); }

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
fn visinf_vo_vf(d: $f32x) -> $mox { return vabs_vf_vf(d).ne($f32x::splat(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return d.ne(d); }

#[inline]
fn vload_vf_p(const float *ptr) -> $f32x { return _mm_load_ps(ptr); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> $f32x { return _mm_loadu_ps(ptr); }

#[inline]
fn vstore_v_p_vf(float *ptr, $f32x v) -> void { _mm_store_ps(ptr, v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, $f32x v) -> void { _mm_storeu_ps(ptr, v); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x { return _mm_i32gather_ps(ptr, vi2, 4); }

#ifdef _MSC_VER
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_f_vf(v: $f32x) -> float {
  float a[VECTLENSP];
  vstoreu_v_p_vf(a, v);
  return a[0];
}
#endif

//

#define PNMASK (($f64x) { +0.0, -0.0 })
#define NPMASK (($f64x) { -0.0, +0.0 })
#define PNMASKf (($f32x) { +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf (($f32x) { -0.0f, +0.0f, -0.0f, +0.0f })

#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(PNMASK))); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(NPMASK))); }
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(PNMASKf))); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(NPMASKf))); }

#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return _mm_addsub_pd(x, y); }
#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { return _mm_addsub_ps(x, y); }

#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vmla_vd_vd_vd_vd(x, y, vnegpos_vd_vd(z)); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vmla_vf_vf_vf_vf(x, y, vnegpos_vf_vf(z)); }

#[inline]
fn vrev21_vd_vd(d0: $f64x) -> $f64x { return _mm_shuffle_pd(d0, d0, 1); }
#[inline]
fn vreva2_vd_vd(vd: $f64x) -> $f64x { return vd; }

#[inline]
fn vstream_v_p_vd(double *ptr, $f64x v) -> void { _mm_stream_pd(ptr, v); }
#[inline]
fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void { vstore_v_p_vd((double *)(&ptr[2*offset]), v); }
#[inline]
fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void { _mm_stream_pd((double *)(&ptr[2*offset]), v); }

//

#[inline]
fn vrev21_vf_vf(d0: $f32x) -> $f32x { return _mm_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vreva2_vf_vf(d0: $f32x) -> $f32x { return _mm_shuffle_ps(d0, d0, (1 << 6) | (0 << 4) | (3 << 2) | (2 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vstream_v_p_vf(float *ptr, $f32x v) -> void { _mm_stream_ps(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
  _mm_storel_pd((double *)(ptr+(offset + step * 0)*2), vreinterpret_vd_vm(vreinterpret_vm_vf(v)));
  _mm_storeh_pd((double *)(ptr+(offset + step * 1)*2), vreinterpret_vd_vm(vreinterpret_vm_vf(v)));
}

#[inline]
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
  _mm_storel_pd((double *)(ptr+(offset + step * 0)*2), vreinterpret_vd_vm(vreinterpret_vm_vf(v)));
  _mm_storeh_pd((double *)(ptr+(offset + step * 1)*2), vreinterpret_vd_vm(vreinterpret_vm_vf(v)));
}
