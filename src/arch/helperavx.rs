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

type $mx = __m256i;
type $mox = __m256i;

type $f64x = __m256d;
type $ix = __m128i;

type $f32x = __m256;
typedef struct { __m128i x, y; } $ix2;

//

#ifndef __SLEEF_H__
void Sleef_x86CpuID(int32_t out[4], uint32_t eax, uint32_t ecx);
#endif

static int cpuSupportsAVX() {
    int32_t reg[4];
    Sleef_x86CpuID(reg, 1, 0);
    return (reg[2] & (1 << 28)) != 0;
}

static int cpuSupportsFMA4() {
    int32_t reg[4];
    Sleef_x86CpuID(reg, 0x80000001, 0);
    return (reg[3] & (1 << 16)) != 0;
}

#if CONFIG == 4 && defined(__AVX__) && defined(__FMA4__)
#[inline]
fn vavailability_i(name: int) -> int {
  //int d = __builtin_cpu_supports("avx") && __builtin_cpu_supports("fma4");
  int d = cpuSupportsAVX() && cpuSupportsFMA4();
  return d ? 3 : 0;
}

//typedef $ix2 $ix2_fma4;

#define ENABLE_FMA_DP
#define ENABLE_FMA_SP

#define ISANAME "AVX + AMD FMA4"
#define DFTPRIORITY 21
#else
#[inline]
fn vavailability_i(name: int) -> int {
  int d = cpuSupportsAVX();
  return d ? 3 : 0;
}
//typedef $ix2 $ix2_avx;

#define ISANAME "AVX"
#define DFTPRIORITY 20
#endif

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


#[inline]
fn vreinterpret_vm_vd(vd: $f64x) -> $mx { return _mm256_castpd_si256(vd); }
#[inline]
fn vreinterpret_vd_vm(vm: $mx) -> $f64x { return _mm256_castsi256_pd(vm);  }
#[inline]
fn vreinterpret_vi2_vd(vd: $f64x) -> $ix2 {
  $ix2 r;
  r.x = _mm256_castsi256_si128(vreinterpret_vm_vd(vd));
  r.y = _mm256_extractf128_si256(vreinterpret_vm_vd(vd), 1);
  return r;
}
#[inline]
fn vreinterpret_vd_vi2(vi: $ix2) -> $f64x {
  $mx m = _mm256_castsi128_si256(vi.x);
  m = _mm256_insertf128_si256(m, vi.y, 1);
  return vreinterpret_vd_vm(m);
}

//

static $ix2 vloadu_vi2_p(int32_t *p) {
  $ix2 r;
  r.x = _mm_loadu_si128((__m128i *) p     );
  r.y = _mm_loadu_si128((__m128i *)(p + 4));
  return r;
}

static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) {
  _mm_storeu_si128((__m128i *) p     , v.x);
  _mm_storeu_si128((__m128i *)(p + 4), v.y);  
}

static $ix vloadu_vi_p(int32_t *p) { return _mm_loadu_si128((__m128i *)p); }
static void vstoreu_v_p_vi(int32_t *p, $ix v) { _mm_storeu_si128((__m128i *)p, v); }

//

#[inline]
fn vand_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

//#[inline]
//fn vand_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vreinterpret_vm_vd(_mm256_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vreinterpret_vm_vd(_mm256_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
//#[inline]
//fn vor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vreinterpret_vm_vd(_mm256_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
//#[inline]
//fn vxor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vreinterpret_vm_vd(_mm256_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vreinterpret_vm_vd(_mm256_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vcast_vo32_vo64(o: $mox) -> $mox {
  return _mm256_castsi128_si256(_mm256_cvtpd_epi32(_mm256_and_pd(vreinterpret_vd_vm(o), _mm256_set1_pd(-1.0))));
}

#[inline]
fn vcast_vo64_vo32(o: $mox) -> $mox {
  return vreinterpret_vm_vd(_mm256_cmp_pd(_mm256_cvtepi32_pd(_mm256_castsi256_si128(o)), _mm256_set1_pd(-1.0), _CMP_EQ_OQ));
}

//

#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix { return _mm256_cvtpd_epi32(vd); }
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix { return _mm256_cvttpd_epi32(vd); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return _mm256_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return _mm256_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return _mm256_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vf_vf(vf: $f32x) -> $f32x { return _mm256_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $mx {
  return _mm256_set_epi32(i0, i1, i0, i1, i0, i1, i0, i1);
}

#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox {
  return vreinterpret_vm_vd(_mm256_cmp_pd(vreinterpret_vd_vm(vxor_vm_vm_vm(vxor_vm_vm_vm(x, y), vreinterpret_vm_vd(_mm256_set1_pd(1.0)))), _mm256_set1_pd(1.0), _CMP_EQ_OQ));
}


#[inline]
fn vrec_vd_vd(x: $f64x) -> $f64x { return _mm256_div_pd(_mm256_set1_pd(1), x); }
#[inline]
fn vsqrt_vd_vd(x: $f64x) -> $f64x { return _mm256_sqrt_pd(x); }
#[inline]
fn vabs_vd_vd(d: $f64x) -> $f64x { return _mm256_andnot_pd(_mm256_set1_pd(-0.0), d); }

#if CONFIG == 1
#[inline]
fn vmla_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x*y + z; }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x*y- z; }
#else
#[inline]
fn vmla_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_macc_pd(x, y, z); }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_msub_pd(x, y, z); }
#[inline]
fn vfma_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_macc_pd(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_macc_pd(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_msub_pd(x, y, z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_nmacc_pd(x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return _mm256_nmsub_pd(x, y, z); }
#endif


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
fn veq_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi_vi_vi(x: $ix, y: $ix) -> $ix { return _mm_cmpgt_epi32(x, y); }

#[inline]
fn veq_vo_vi_vi(x: $ix, y: $ix) -> $mox { return _mm256_castsi128_si256(_mm_cmpeq_epi32(x, y)); }
#[inline]
fn vgt_vo_vi_vi(x: $ix, y: $ix) -> $mox { return _mm256_castsi128_si256(_mm_cmpgt_epi32(x, y)); }

#[inline]
fn vsel_vi_vo_vi_vi(o: $mox, x: $ix, y: $ix) -> $ix { return _mm_blendv_epi8(y, x, _mm256_castsi256_si128(o)); }

#[inline]
fn vsel_vd_vo_vd_vd(o: $mox, x: $f64x, y: $f64x) -> $f64x { return _mm256_blendv_pd(y, x, _mm256_castsi256_pd(o)); }

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
  return vreinterpret_vm_vd(_mm256_cmp_pd(vabs_vd_vd(d), _mm256_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox {
  return vreinterpret_vm_vd(_mm256_cmp_pd(d, _mm256_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox {
  return vreinterpret_vm_vd(_mm256_cmp_pd(d, _mm256_set1_pd(-SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visnan_vo_vd(d: $f64x) -> $mox {
  return vreinterpret_vm_vd(_mm256_cmp_pd(d, d, _CMP_NEQ_UQ));
}

#[inline]
fn vload_vd_p(const double *ptr) -> $f64x { return _mm256_load_pd(ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> $f64x { return _mm256_loadu_pd(ptr); }

#[inline]
fn vstore_v_p_vd(double *ptr, $f64x v) -> void { _mm256_store_pd(ptr, v); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, $f64x v) -> void { _mm256_storeu_pd(ptr, v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x {
  int a[VECTLENDP];
  vstoreu_v_p_vi(a, vi);
  return _mm256_set_pd(ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]]);
}

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
fn vcast_vi2_vm(vm: $mx) -> $ix2 {
  $ix2 r;
  r.x = _mm256_castsi256_si128(vm);
  r.y = _mm256_extractf128_si256(vm, 1);
  return r;
}

#[inline]
fn vcast_vm_vi2(vi: $ix2) -> $mx {
  $mx m = _mm256_castsi128_si256(vi.x);
  m = _mm256_insertf128_si256(m, vi.y, 1);
  return m;
}

#[inline]
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { return vcast_vi2_vm(_mm256_cvtps_epi32(vf)); }
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return vcast_vi2_vm(_mm256_cvttps_epi32(vf)); }

#[inline]
fn vreinterpret_vm_vf(vf: $f32x) -> $mx { return _mm256_castps_si256(vf); }
#[inline]
fn vreinterpret_vf_vm(vm: $mx) -> $f32x { return _mm256_castsi256_ps(vm); }

#[inline]
fn vreinterpret_vf_vi2(vi: $ix2) -> $f32x { return vreinterpret_vf_vm(vcast_vm_vi2(vi)); }
#[inline]
fn vreinterpret_vi2_vf(vf: $f32x) -> $ix2 { return vcast_vi2_vm(vreinterpret_vm_vf(vf)); }

#[inline]
fn vrec_vf_vf(x: $f32x) -> $f32x { return $f32x::splat(1.) / x); }
#[inline]
fn vsqrt_vf_vf(x: $f32x) -> $f32x { return _mm256_sqrt_ps(x); }
#[inline]
fn vabs_vf_vf(f: $f32x) -> $f32x { return vreinterpret_vf_vm(vandnot_vm_vm_vm(vreinterpret_vm_vf($f32x::splat(-0.0f)), vreinterpret_vm_vf(f))); }

#if CONFIG == 1
#[inline]
fn vmla_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return x*y+z; }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return z - x * y); }
#else
#[inline]
fn vmla_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_macc_ps(x, y, z); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_nmacc_ps(x, y, z); }
#[inline]
fn vfma_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_macc_ps(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_macc_ps(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_msub_ps(x, y, z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_nmacc_ps(x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return _mm256_nmsub_ps(x, y, z); }
#endif

#[inline]
fn vadd_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  $ix2 vi = { _mm_add_epi32(x.x, y.x), _mm_add_epi32(x.y, y.y) };
  return vi;
}

#[inline]
fn vsub_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  $ix2 vi = { _mm_sub_epi32(x.x, y.x), _mm_sub_epi32(x.y, y.y) };
  return vi;
}

#[inline]
fn vneg_vi2_vi2(e: $ix2) -> $ix2 {
  $ix2 vi = { _mm_sub_epi32(_mm_set1_epi32(0), e.x), _mm_sub_epi32(_mm_set1_epi32(0), e.y) };
  return vi;
}

#[inline]
fn vand_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  $ix2 vi = { _mm_and_si128(x.x, y.x), _mm_and_si128(x.y, y.y) };
  return vi;
}

#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  $ix2 vi = { _mm_andnot_si128(x.x, y.x), _mm_andnot_si128(x.y, y.y) };
  return vi;
}

#[inline]
fn vor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  $ix2 vi = { _mm_or_si128(x.x, y.x), _mm_or_si128(x.y, y.y) };
  return vi;
}

#[inline]
fn vxor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  $ix2 vi = { _mm_xor_si128(x.x, y.x), _mm_xor_si128(x.y, y.y) };
  return vi;
}

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vand_vi2_vi2_vi2(vcast_vi2_vm(x), y); }
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vandnot_vi2_vi2_vi2(vcast_vi2_vm(x), y); }

#[inline]
fn vsll_vi2_vi2_i(x: $ix2, c: int) -> $ix2 {
  $ix2 vi = { _mm_slli_epi32(x.x, c), _mm_slli_epi32(x.y, c) };
  return vi;
}

#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 {
  $ix2 vi = { _mm_srli_epi32(x.x, c), _mm_srli_epi32(x.y, c) };
  return vi;
}

#[inline]
fn vsra_vi2_vi2_i(x: $ix2, c: int) -> $ix2 {
  $ix2 vi = { _mm_srai_epi32(x.x, c), _mm_srai_epi32(x.y, c) };
  return vi;
}

#[inline]
fn veq_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox {
  $ix2 r;
  r.x = _mm_cmpeq_epi32(x.x, y.x);
  r.y = _mm_cmpeq_epi32(x.y, y.y);
  return vcast_vm_vi2(r);
}

#[inline]
fn vgt_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox {
  $ix2 r;
  r.x = _mm_cmpgt_epi32(x.x, y.x);
  r.y = _mm_cmpgt_epi32(x.y, y.y);
  return vcast_vm_vi2(r);
}

#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  $ix2 r;
  r.x = _mm_cmpeq_epi32(x.x, y.x);
  r.y = _mm_cmpeq_epi32(x.y, y.y);
  return r;
}

#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  $ix2 r;
  r.x = _mm_cmpgt_epi32(x.x, y.x);
  r.y = _mm_cmpgt_epi32(x.y, y.y);
  return r;
}

#[inline]
fn vsel_vi2_vo_vi2_vi2(m: $mox, x: $ix2, y: $ix2) -> $ix2 {
  $ix2 n = vcast_vi2_vm(m);
  $ix2 r = { _mm_blendv_epi8(y.x, x.x, n.x), _mm_blendv_epi8(y.y, x.y, n.y) };
  return r;
}

#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  $ix2 ix = vcast_vi2_vm(x), iy = vcast_vi2_vm(y), iz;
  iz.x = _mm_add_epi64(ix.x, iy.x);
  iz.y = _mm_add_epi64(ix.y, iy.y);
  return vcast_vm_vi2(iz);
}

#[inline]
fn vsel_vf_vo_vf_vf(o: $mox, x: $f32x, y: $f32x) -> $f32x { return _mm256_blendv_ps(y, x, _mm256_castsi256_ps(o)); }

#[inline]
fn vsel_vf_vo_f_f(o: $mox, v1: f32, v0: f32) -> $f32x {
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

//

#[inline]
fn vload_vf_p(const float *ptr) -> $f32x { return _mm256_load_ps(ptr); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> $f32x { return _mm256_loadu_ps(ptr); }

#[inline]
fn vstore_v_p_vf(float *ptr, $f32x v) -> void { _mm256_store_ps(ptr, v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, $f32x v) -> void { _mm256_storeu_ps(ptr, v); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x {
  int a[VECTLENSP];
  vstoreu_v_p_vi2(a, vi2);
  return _mm256_set_ps(ptr[a[7]], ptr[a[6]], ptr[a[5]], ptr[a[4]],
		       ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]]);
}

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

#define PNMASK (($f64x) { +0.0, -0.0, +0.0, -0.0 })
#define NPMASK (($f64x) { -0.0, +0.0, -0.0, +0.0 })
#define PNMASKf (($f32x) { +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf (($f32x) { -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f })

#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(PNMASK))); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(NPMASK))); }
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(PNMASKf))); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(NPMASKf))); }

#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return _mm256_addsub_pd(x, y); }
#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { return _mm256_addsub_ps(x, y); }

#if CONFIG == 1
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vsubadd_vd_vd_vd(x*y, z); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vsubadd_vf_vf_vf(x * y, z); }
#else
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vmla_vd_vd_vd_vd(x, y, vnegpos_vd_vd(z)); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vmla_vf_vf_vf_vf(x, y, vnegpos_vf_vf(z)); }
#endif


#[inline]
fn vrev21_vd_vd(d0: $f64x) -> $f64x { return  _mm256_shuffle_pd(d0, d0, (0 << 3) | (1 << 2) | (0 << 1) | (1 << 0)); }
#[inline]
fn vreva2_vd_vd(d0: $f64x) -> $f64x { d0 = _mm256_permute2f128_pd(d0, d0, 1); return _mm256_shuffle_pd(d0, d0, (1 << 3) | (0 << 2) | (1 << 1) | (0 << 0)); }

#[inline]
fn vstream_v_p_vd(double *ptr, $f64x v) -> void { _mm256_stream_pd(ptr, v); }
#[inline]
fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void {
  _mm_store_pd(&ptr[(offset + step * 0)*2], _mm256_extractf128_pd(v, 0));
  _mm_store_pd(&ptr[(offset + step * 1)*2], _mm256_extractf128_pd(v, 1));
}

#[inline]
fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void {
  _mm_stream_pd(&ptr[(offset + step * 0)*2], _mm256_extractf128_pd(v, 0));
  _mm_stream_pd(&ptr[(offset + step * 1)*2], _mm256_extractf128_pd(v, 1));
}

//

#[inline]
fn vrev21_vf_vf(d0: $f32x) -> $f32x { return _mm256_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vreva2_vf_vf(d0: $f32x) -> $f32x { d0 = _mm256_permute2f128_ps(d0, d0, 1); return _mm256_shuffle_ps(d0, d0, (1 << 6) | (0 << 4) | (3 << 2) | (2 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vstream_v_p_vf(float *ptr, $f32x v) -> void { _mm256_stream_ps(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
  _mm_storel_pd((double *)(ptr+(offset + step * 0)*2), _mm_castsi128_pd(_mm_castps_si128(_mm256_extractf128_ps(v, 0))));
  _mm_storeh_pd((double *)(ptr+(offset + step * 1)*2), _mm_castsi128_pd(_mm_castps_si128(_mm256_extractf128_ps(v, 0))));
  _mm_storel_pd((double *)(ptr+(offset + step * 2)*2), _mm_castsi128_pd(_mm_castps_si128(_mm256_extractf128_ps(v, 1))));
  _mm_storeh_pd((double *)(ptr+(offset + step * 3)*2), _mm_castsi128_pd(_mm_castps_si128(_mm256_extractf128_ps(v, 1))));
}

#[inline]
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }
