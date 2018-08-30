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

type $mx = __m512i;
type $mox = __mmask16;

type $f64x = __m512d;
type $ix = __m256i;

type $f32x = __m512;
type $ix2 = __m512i;

//

#ifndef __SLEEF_H__
void Sleef_x86CpuID(int32_t out[4], uint32_t eax, uint32_t ecx);
#endif

static int cpuSupportsAVX512F() {
    int32_t reg[4];
    Sleef_x86CpuID(reg, 7, 0);
    return (reg[1] & (1 << 16)) != 0;
}

#if CONFIG == 1 && defined(__AVX512F__)
#[inline]
fn vavailability_i(name: int) -> int {
  int d = cpuSupportsAVX512F();
  return d ? 3 : 0;
}
#define ISANAME "AVX512F"
#define DFTPRIORITY 30
#endif

#if CONFIG == 2 && defined(__AVX512F__)
#[inline]
fn vavailability_i(name: int) -> int {
  int d = cpuSupportsAVX512F();
  return d ? 3 : 0;
}
#define ISANAME "AVX512FNOFMA"
#define DFTPRIORITY 0
#endif

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { _mm_prefetch(ptr, _MM_HINT_T0); }

#ifdef __INTEL_COMPILER
#[inline]
fn vtestallones_i_vo64(g: $mox) -> int { return _mm512_mask2int(g) == 0xff; }
#[inline]
fn vtestallones_i_vo32(g: $mox) -> int { return _mm512_mask2int(g) == 0xffff; }
#else
#[inline]
fn vtestallones_i_vo64(g: $mox) -> int { return g == 0xff; }
#[inline]
fn vtestallones_i_vo32(g: $mox) -> int { return g == 0xffff; }
#endif

//

static $ix2 vloadu_vi2_p(int32_t *p) { return _mm512_loadu_si512((__m512i const *)p); }
static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { _mm512_storeu_si512((__m512i *)p, v); }
static $ix vloadu_vi_p(int32_t *p) { return _mm256_loadu_si256((__m256i const *)p); }
static void vstoreu_v_p_vi(int32_t *p, $ix v) { _mm256_storeu_si256((__m256i *)p, v); }

//

#[inline]
fn vand_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm512_and_si512(x, y); }
#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm512_andnot_si512(x, y); }
#[inline]
fn vor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm512_or_si512(x, y); }
#[inline]
fn vxor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm512_xor_si512(x, y); }

//#[inline]
//fn vand_vo_vo_vo(x: $mox, y: $mox) -> $mox { return _mm512_kand(x, y); }
#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { return _mm512_kandn(x, y); }
//#[inline]
//fn vor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return _mm512_kor(x, y); }
//#[inline]
//fn vxor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return _mm512_kxor(x, y); }

#[inline]
fn vand_vm_vo64_vm(o: $mox, m: $mx) -> $mx { return _mm512_mask_and_epi64(_mm512_set1_epi32(0), o, m, m); }
#[inline]
fn vandnot_vm_vo64_vm(o: $mox, m: $mx) -> $mx { return _mm512_mask_and_epi64(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)); }
#[inline]
fn vor_vm_vo64_vm(o: $mox, m: $mx) -> $mx { return _mm512_mask_or_epi64(m, o, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1)); }

#[inline]
fn vand_vm_vo32_vm(o: $mox, m: $mx) -> $mx { return _mm512_mask_and_epi32(_mm512_set1_epi32(0), o, m, m); }
#[inline]
fn vandnot_vm_vo32_vm(o: $mox, m: $mx) -> $mx { return _mm512_mask_and_epi32(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)); }
#[inline]
fn vor_vm_vo32_vm(o: $mox, m: $mx) -> $mx { return _mm512_mask_or_epi32(m, o, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1)); }

#[inline]
fn vcast_vo32_vo64(o: $mox) -> $mox { return o; }
#[inline]
fn vcast_vo64_vo32(o: $mox) -> $mox { return o; }

//

#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix {
  return _mm512_cvt_roundpd_epi32(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
}

#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix {
  return _mm512_cvt_roundpd_epi32(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
}

#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x {
  __m256d hi = _mm512_extractf64x4_pd(vd, 1), lo = _mm512_extractf64x4_pd(vd, 0);
  hi = _mm256_round_pd(hi, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  lo = _mm256_round_pd(lo, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  return _mm512_insertf64x4(_mm512_castpd256_pd512(lo), hi, 1);
}

#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x {
  __m256d hi = _mm512_extractf64x4_pd(vd, 1), lo = _mm512_extractf64x4_pd(vd, 0);
  hi = _mm256_round_pd(hi, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  lo = _mm256_round_pd(lo, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  return _mm512_insertf64x4(_mm512_castpd256_pd512(lo), hi, 1);
}


#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $mx { return _mm512_set_epi32(i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1); }

#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox { return _mm512_cmp_epi64_mask(x, y, _MM_CMPINT_EQ); }
#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx { return _mm512_add_epi64(x, y); }

//


#[inline]
fn vreinterpret_vm_vd(vd: $f64x) -> $mx { return _mm512_castpd_si512(vd); }
#[inline]
fn vreinterpret_vd_vm(vm: $mx) -> $f64x { return _mm512_castsi512_pd(vm); }
#[inline]
fn vreinterpret_vi2_vd(vd: $f64x) -> $ix2 { return _mm512_castpd_si512(vd); }
#[inline]
fn vreinterpret_vd_vi2(vi: $ix2) -> $f64x { return _mm512_castsi512_pd(vi); }

#[inline]
fn vrec_vd_vd(x: $f64x) -> $f64x { return _mm512_div_pd(_mm512_set1_pd(1), x); }
#[inline]
fn vsqrt_vd_vd(x: $f64x) -> $f64x { return _mm512_sqrt_pd(x); }
#[inline]
fn vabs_vd_vd(d: $f64x) -> $f64x { return vreinterpret_vd_vm(_mm512_andnot_si512(vreinterpret_vm_vd(_mm512_set1_pd(-0.0)), vreinterpret_vm_vd(d))); }

#if CONFIG == 1
#[inline]
fn vmla_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fmadd_pd(x, y, z); }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fmsub_pd(x, y, z); }
#[inline]
fn vmlanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fnmadd_pd(x, y, z); }
#else
#[inline]
fn vmla_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x*y + z; }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x*y - z; }
#endif

#[inline]
fn vfma_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fmadd_pd(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fmadd_pd(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fmsub_pd(x, y, z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fnmadd_pd(x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fnmsub_pd(x, y, z); }


//

#[inline]
fn vadd_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm256_add_epi32(x, y); }
#[inline]
fn vsub_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm256_sub_epi32(x, y); }
#[inline]
fn vneg_vi_vi(e: $ix) -> $ix { return vsub_vi_vi_vi(vcast_vi_i(0), e); }

#[inline]
fn vand_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm256_and_si256(x, y); }
#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm256_andnot_si256(x, y); }

#[inline]
fn vandnot_vi_vo_vi(o: $mox, y: $ix) -> $ix {
  return _mm512_castsi512_si256(_mm512_mask_and_epi32(_mm512_castsi256_si512(y), o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)));
}
#[inline]
fn vand_vi_vo_vi(o: $mox, y: $ix) -> $ix {
  return _mm512_castsi512_si256(_mm512_mask_and_epi32(_mm512_set1_epi32(0), o, _mm512_castsi256_si512(y), _mm512_castsi256_si512(y)));
}

#[inline]
fn vor_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm256_or_si256(x, y); }
#[inline]
fn vxor_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm256_xor_si256(x, y); }
#define vsll_vi_vi_i(x, c) _mm256_slli_epi32(x, c)
#define vsrl_vi_vi_i(x, c) _mm256_srli_epi32(x, c)
#define vsra_vi_vi_i(x, c) _mm256_srai_epi32(x, c)

#[inline]
fn veq_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm256_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm256_cmpgt_epi32(x, y); }

#[inline]
fn veq_vo_vi_vi(x: $ix, y: $ix) -> $mox {
  return _mm512_cmp_epi32_mask(_mm512_castsi256_si512(x), _mm512_castsi256_si512(y), _MM_CMPINT_EQ);
}
#[inline]
fn vgt_vo_vi_vi(x: $ix, y: $ix) -> $mox {
  return _mm512_cmp_epi32_mask(_mm512_castsi256_si512(y), _mm512_castsi256_si512(x), _MM_CMPINT_LT);
}

#[inline]
fn vsel_vd_vo_vd_vd(mask: $mox, x: $f64x, y: $f64x) -> $f64x {
  return _mm512_mask_blend_pd(mask, y, x);
}

#[inline]
fn vsel_vd_vo_d_d(o: $mox, v1: f64, v0: f64) -> CONST -> $f64x {
  return vsel_vd_vo_vd_vd(o, $f64x::splat(v1), $f64x::splat(v0));
}

#if 1
// Probably this is faster
#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $mox, o1: $mox, o2: $mox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  __m512i v = _mm512_castpd_si512(vsel_vd_vo_vd_vd(o0, _mm512_castsi512_pd(_mm512_set_epi64(0, 0, 0, 0, 0, 0, 0, 0)),
						   vsel_vd_vo_vd_vd(o1, _mm512_castsi512_pd(_mm512_set_epi64(1, 1, 1, 1, 1, 1, 1, 1)),
								    vsel_vd_vo_vd_vd(o2, _mm512_castsi512_pd(_mm512_set_epi64(2, 2, 2, 2, 2, 2, 2, 2)),
										     _mm512_castsi512_pd(_mm512_set_epi64(3, 3, 3, 3, 3, 3, 3, 3))))));
  return _mm512_permutexvar_pd(v, _mm512_castpd256_pd512(_mm256_set_pd(d3, d2, d1, d0)));
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $mox, o1: $mox, d0: f64, d1: f64, d2: f64) -> $f64x {
  return vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o1, d0, d1, d2, d2);
}
#else
#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $mox, o1: $mox, d0: f64, d1: f64, d2: f64) -> $f64x {
  return vsel_vd_vo_vd_vd(o0, $f64x::splat(d0), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $mox, o1: $mox, o2: $mox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  return vsel_vd_vo_vd_vd(o0, $f64x::splat(d0), vsel_vd_vo_vd_vd(o1, $f64x::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)));
}
#endif

#[inline]
fn visinf_vo_vd(d: $f64x) -> $mox {
  return _mm512_cmp_pd_mask(vabs_vd_vd(d), _mm512_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ);
}

#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox {
  return _mm512_cmp_pd_mask(d, _mm512_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ);
}

#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox {
  return _mm512_cmp_pd_mask(d, _mm512_set1_pd(-SLEEF_INFINITY), _CMP_EQ_OQ);
}

#[inline]
fn visnan_vo_vd(d: $f64x) -> $mox {
  return _mm512_cmp_pd_mask(d, d, _CMP_NEQ_UQ);
}

#[inline]
fn vilogbk_vi_vd(d: $f64x) -> $ix { return vrint_vi_vd(_mm512_getexp_pd(d)); }

// vilogb2k_vi_vd is similar to vilogbk_vi_vd, but the argument has to
// be a normalized FP value.
#[inline]
fn vilogb2k_vi_vd(d: $f64x) -> $ix { return vrint_vi_vd(_mm512_getexp_pd(d)); }

#[inline]
fn vgetexp_vd_vd(d: $f64x) -> $f64x { return _mm512_getexp_pd(d); }
#[inline]
fn vgetexp_vf_vf(d: $f32x) -> $f32x { return _mm512_getexp_ps(d); }

#[inline]
fn vgetmant_vd_vd(d: $f64x) -> $f64x { return _mm512_getmant_pd(d, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_nan); }
#[inline]
fn vgetmant_vf_vf(d: $f32x) -> $f32x { return _mm512_getmant_ps(d, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_nan); }

#define vfixup_vd_vd_vd_vi2_i(a, b, c, imm) _mm512_fixupimm_pd((a), (b), (c), (imm))
#define vfixup_vf_vf_vf_vi2_i(a, b, c, imm) _mm512_fixupimm_ps((a), (b), (c), (imm))

#if defined(_MSC_VER)
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_d_vd(v: $f64x) -> double {
  double s[VECTLENDP];
  _mm512_storeu_pd(s, v);
  return s[0];
}
#endif

#[inline]
fn vload_vd_p(const double *ptr) -> $f64x { return _mm512_load_pd(ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> $f64x { return _mm512_loadu_pd(ptr); }

#[inline]
fn vstore_v_p_vd(double *ptr, $f64x v) -> void { _mm512_store_pd(ptr, v); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, $f64x v) -> void { _mm512_storeu_pd(ptr, v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x { return _mm512_i32gather_pd(vi, ptr, 8); }

//

#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix {
  return _mm512_castsi512_si256(_mm512_mask_blend_epi32(m, _mm512_castsi256_si512(y), _mm512_castsi256_si512(x)));
}

//

#[inline]
fn vreinterpret_vm_vf(vf: $f32x) -> $mx { return _mm512_castps_si512(vf); }
#[inline]
fn vreinterpret_vf_vm(vm: $mx) -> $f32x { return _mm512_castsi512_ps(vm); }
#[inline]
fn vreinterpret_vf_vi2(vi: $ix2) -> $f32x { return _mm512_castsi512_ps(vi); }
#[inline]
fn vreinterpret_vi2_vf(vf: $f32x) -> $ix2 { return _mm512_castps_si512(vf); }

#[inline]
fn vreinterpret_vd_vf(vf: $f32x) -> $f64x { return _mm512_castps_pd(vf); }
#[inline]
fn vreinterpret_vf_vd(vd: $f64x) -> $f32x { return _mm512_castpd_ps(vd); }

#[inline]
fn vcast_vi2_vm(vm: $mx) -> $ix2 { return vm; }
#[inline]
fn vcast_vm_vi2(vi: $ix2) -> $mx { return vi; }

#[inline]
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { return vcast_vi2_vm(_mm512_cvtps_epi32(vf)); }
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return vcast_vi2_vm(_mm512_cvttps_epi32(vf)); }

#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x {
  __m256 hi = _mm256_castpd_ps(_mm512_extractf64x4_pd(vreinterpret_vd_vf(vd), 1));
  __m256 lo = _mm256_castpd_ps(_mm512_extractf64x4_pd(vreinterpret_vd_vf(vd), 0));
  hi = _mm256_round_ps(hi, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  lo = _mm256_round_ps(lo, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  return vreinterpret_vf_vd(_mm512_insertf64x4(_mm512_castpd256_pd512(_mm256_castps_pd(lo)), _mm256_castps_pd(hi), 1));
}
 
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x {
  __m256 hi = _mm256_castpd_ps(_mm512_extractf64x4_pd(vreinterpret_vd_vf(vd), 1));
  __m256 lo = _mm256_castpd_ps(_mm512_extractf64x4_pd(vreinterpret_vd_vf(vd), 0));
  hi = _mm256_round_ps(hi, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  lo = _mm256_round_ps(lo, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  return vreinterpret_vf_vd(_mm512_insertf64x4(_mm512_castpd256_pd512(_mm256_castps_pd(lo)), _mm256_castps_pd(hi), 1));
}

#[inline]
fn vrec_vf_vf(x: $f32x) -> $f32x { return $f32x::splat(1.) / x); }
#[inline]
fn vsqrt_vf_vf(x: $f32x) -> $f32x { return _mm512_sqrt_ps(x); }
#[inline]
fn vabs_vf_vf(f: $f32x) -> $f32x { return vreinterpret_vf_vm(vandnot_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.0f)), vreinterpret_vm_vf(f))); }

#if CONFIG == 1
#[inline]
fn vmla_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fmadd_ps(x, y, z); }
#[inline]
fn vmlapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fmsub_ps(x, y, z); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fnmadd_ps(x, y, z); }
#else
#[inline]
fn vmla_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return x*y+z; }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return z - x * y; }
#endif

#[inline]
fn vfma_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fmadd_ps(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fmadd_ps(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fmsub_ps(x, y, z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fnmadd_ps(x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fnmsub_ps(x, y, z); }

#[inline]
fn vadd_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm512_add_epi32(x, y); }
#[inline]
fn vsub_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm512_sub_epi32(x, y); }
#[inline]
fn vneg_vi2_vi2(e: $ix2) -> $ix2 { return vsub_vi2_vi2_vi2($ix2::from(0), e); }
#[inline]
fn vand_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm512_and_si512(x, y); }
#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm512_andnot_si512(x, y); }
#[inline]
fn vor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm512_or_si512(x, y); }
#[inline]
fn vxor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return _mm512_xor_si512(x, y); }

#[inline]
fn vand_vi2_vo_vi2(o: $mox, m: $ix2) -> $ix2 {
  return _mm512_mask_and_epi32(_mm512_set1_epi32(0), o, m, m);
}

#[inline]
fn vandnot_vi2_vo_vi2(o: $mox, m: $ix2) -> $ix2 {
  return _mm512_mask_and_epi32(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0));
}

#define vsll_vi2_vi2_i(x, c) _mm512_slli_epi32(x, c)
#define vsrl_vi2_vi2_i(x, c) _mm512_srli_epi32(x, c)
#define vsra_vi2_vi2_i(x, c) _mm512_srai_epi32(x, c)
#[inline]
fn veq_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return _mm512_cmpeq_epi32_mask(x, y); }
#[inline]
fn vgt_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return _mm512_cmpgt_epi32_mask(x, y); }

#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  __mmask16 m = _mm512_cmp_epi32_mask(x, y, _MM_CMPINT_EQ);
  return _mm512_mask_and_epi32(_mm512_set1_epi32(0), m, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1));
}
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  __mmask16 m = _mm512_cmp_epi32_mask(y, x, _MM_CMPINT_LT);
  return _mm512_mask_and_epi32(_mm512_set1_epi32(0), m, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1));
}

#[inline]
fn vsel_vi2_vo_vi2_vi2(m: $mox, x: $ix2, y: $ix2) -> $ix2 {
  return _mm512_mask_blend_epi32(m, y, x);
}

#[inline]
fn vsel_vf_vo_vf_vf(m: $mox, x: $f32x, y: $f32x) -> $f32x {
  return _mm512_mask_blend_ps(m, y, x);
}

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
fn visinf_vo_vf(d: $f32x) -> $mox { return vabs_vf_vf(d).ne($f32x::splat(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return d.ne(d); }

#[inline]
fn vilogbk_vi2_vf(d: $f32x) -> $ix2 { return vrint_vi2_vf(_mm512_getexp_ps(d)); }
#[inline]
fn vilogb2k_vi2_vf(d: $f32x) -> $ix2 { return vrint_vi2_vf(_mm512_getexp_ps(d)); }

#ifdef _MSC_VER
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_f_vf(v: $f32x) -> float {
  float s[VECTLENSP];
  _mm512_storeu_ps(s, v);
  return s[0];
}
#endif

#[inline]
fn vload_vf_p(const float *ptr) -> $f32x { return _mm512_load_ps(ptr); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> $f32x { return _mm512_loadu_ps(ptr); }

#[inline]
fn vstore_v_p_vf(float *ptr, $f32x v) -> void { _mm512_store_ps(ptr, v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, $f32x v) -> void { _mm512_storeu_ps(ptr, v); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x { return _mm512_i32gather_ps(vi2, ptr, 4); }

//

#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x {
  return vreinterpret_vd_vm(_mm512_mask_xor_epi32(vreinterpret_vm_vd(d), 0xcccc, vreinterpret_vm_vd(d), vreinterpret_vm_vd(_mm512_set1_pd(-0.0))));
}
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x {
  return vreinterpret_vd_vm(_mm512_mask_xor_epi32(vreinterpret_vm_vd(d), 0x3333, vreinterpret_vm_vd(d), vreinterpret_vm_vd(_mm512_set1_pd(-0.0))));
}
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x {
  return vreinterpret_vf_vm(_mm512_mask_xor_epi32(vreinterpret_vm_vf(d), 0xaaaa, vreinterpret_vm_vf(d), vreinterpret_vm_vf(_mm512_set1_ps(-0.0f))));
}
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x {
  return vreinterpret_vf_vm(_mm512_mask_xor_epi32(vreinterpret_vm_vf(d), 0x5555, vreinterpret_vm_vf(d), vreinterpret_vm_vf(_mm512_set1_ps(-0.0f))));
}

#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return x - vnegpos_vd_vd(y); }
#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { return x + vnegpos_vf_vf(y); }

#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm512_fmaddsub_pd(x, y, z); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm512_fmaddsub_ps(x, y, z); }

#[inline]
fn vrev21_vd_vd(vd: $f64x) -> $f64x { return _mm512_permute_pd(vd, 0x55); }

#[inline]
fn vreva2_vd_vd(vd: $f64x) -> $f64x {
  return vreinterpret_vd_vm(_mm512_permutexvar_epi32(_mm512_set_epi32(3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12), vreinterpret_vm_vd(vd)));
}

#[inline]
fn vstream_v_p_vd(double *ptr, $f64x v) -> void { _mm512_stream_pd(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void {
  _mm_store_pd(&ptr[(offset + step * 0)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 0)));
  _mm_store_pd(&ptr[(offset + step * 1)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 1)));
  _mm_store_pd(&ptr[(offset + step * 2)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 2)));
  _mm_store_pd(&ptr[(offset + step * 3)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 3)));
}

#[inline]
fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void {
  _mm_stream_pd(&ptr[(offset + step * 0)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 0)));
  _mm_stream_pd(&ptr[(offset + step * 1)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 1)));
  _mm_stream_pd(&ptr[(offset + step * 2)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 2)));
  _mm_stream_pd(&ptr[(offset + step * 3)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 3)));
}

//

#[inline]
fn vrev21_vf_vf(vf: $f32x) -> $f32x { return _mm512_permute_ps(vf, 0xb1); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vreva2_vf_vf(vf: $f32x) -> $f32x {
  return vreinterpret_vf_vm(_mm512_permutexvar_epi32(_mm512_set_epi32(1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14), vreinterpret_vm_vf(vf)));
}

#[inline]
fn vstream_v_p_vf(float *ptr, $f32x v) -> void { _mm512_stream_ps(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
  _mm_storel_pd((double *)(ptr+(offset + step * 0)*2), _mm_castps_pd(_mm512_extractf32x4_ps(v, 0)));
  _mm_storeh_pd((double *)(ptr+(offset + step * 1)*2), _mm_castps_pd(_mm512_extractf32x4_ps(v, 0)));
  _mm_storel_pd((double *)(ptr+(offset + step * 2)*2), _mm_castps_pd(_mm512_extractf32x4_ps(v, 1)));
  _mm_storeh_pd((double *)(ptr+(offset + step * 3)*2), _mm_castps_pd(_mm512_extractf32x4_ps(v, 1)));
  _mm_storel_pd((double *)(ptr+(offset + step * 4)*2), _mm_castps_pd(_mm512_extractf32x4_ps(v, 2)));
  _mm_storeh_pd((double *)(ptr+(offset + step * 5)*2), _mm_castps_pd(_mm512_extractf32x4_ps(v, 2)));
  _mm_storel_pd((double *)(ptr+(offset + step * 6)*2), _mm_castps_pd(_mm512_extractf32x4_ps(v, 3)));
  _mm_storeh_pd((double *)(ptr+(offset + step * 7)*2), _mm_castps_pd(_mm512_extractf32x4_ps(v, 3)));
}

#[inline]
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }
