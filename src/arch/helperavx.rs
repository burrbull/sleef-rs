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

type VMask = __m256i;
type VOpMask = __m256i;

type VDouble = __m256d;
type VInt = __m128i;

type VFloat = __m256;
typedef struct { __m128i x, y; } VInt2;

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

//typedef VInt2 VInt2_fma4;

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
//typedef VInt2 VInt2_avx;

#define ISANAME "AVX"
#define DFTPRIORITY 20
#endif

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { _mm_prefetch(ptr, _MM_HINT_T0); }

#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int {
  return _mm_test_all_ones(_mm_and_si128(_mm256_extractf128_si256(g, 0), _mm256_extractf128_si256(g, 1)));
}

#[inline]
fn vtestallones_i_vo64(g: VOpMask) -> int {
  return _mm_test_all_ones(_mm_and_si128(_mm256_extractf128_si256(g, 0), _mm256_extractf128_si256(g, 1)));
}

//

impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        _mm256_set1_pd(d)
    }
}
#[inline]
fn vreinterpret_vm_vd(vd: VDouble) -> VMask { return _mm256_castpd_si256(vd); }
#[inline]
fn vreinterpret_vd_vm(vm: VMask) -> VDouble { return _mm256_castsi256_pd(vm);  }
#[inline]
fn vreinterpret_vi2_vd(vd: VDouble) -> VInt2 {
  VInt2 r;
  r.x = _mm256_castsi256_si128(vreinterpret_vm_vd(vd));
  r.y = _mm256_extractf128_si256(vreinterpret_vm_vd(vd), 1);
  return r;
}
#[inline]
fn vreinterpret_vd_vi2(vi: VInt2) -> VDouble {
  VMask m = _mm256_castsi128_si256(vi.x);
  m = _mm256_insertf128_si256(m, vi.y, 1);
  return vreinterpret_vd_vm(m);
}

//

static VInt2 vloadu_vi2_p(int32_t *p) {
  VInt2 r;
  r.x = _mm_loadu_si128((__m128i *) p     );
  r.y = _mm_loadu_si128((__m128i *)(p + 4));
  return r;
}

static void vstoreu_v_p_vi2(int32_t *p, VInt2 v) {
  _mm_storeu_si128((__m128i *) p     , v.x);
  _mm_storeu_si128((__m128i *)(p + 4), v.y);  
}

static VInt vloadu_vi_p(int32_t *p) { return _mm_loadu_si128((__m128i *)p); }
static void vstoreu_v_p_vi(int32_t *p, VInt v) { _mm_storeu_si128((__m128i *)p, v); }

//

#[inline]
fn vand_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

//#[inline]
//fn vand_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return vreinterpret_vm_vd(_mm256_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return vreinterpret_vm_vd(_mm256_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
//#[inline]
//fn vor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return vreinterpret_vm_vd(_mm256_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
//#[inline]
//fn vxor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return vreinterpret_vm_vd(_mm256_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vand_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vand_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_and_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vandnot_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_andnot_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_or_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }
#[inline]
fn vxor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return vreinterpret_vm_vd(_mm256_xor_pd(vreinterpret_vd_vm(x), vreinterpret_vd_vm(y))); }

#[inline]
fn vcast_vo32_vo64(o: VOpMask) -> VOpMask {
  return _mm256_castsi128_si256(_mm256_cvtpd_epi32(_mm256_and_pd(vreinterpret_vd_vm(o), _mm256_set1_pd(-1.0))));
}

#[inline]
fn vcast_vo64_vo32(o: VOpMask) -> VOpMask {
  return vreinterpret_vm_vd(_mm256_cmp_pd(_mm256_cvtepi32_pd(_mm256_castsi256_si128(o)), _mm256_set1_pd(-1.0), _CMP_EQ_OQ));
}

//

#[inline]
fn vrint_vi_vd(vd: VDouble) -> VInt { return _mm256_cvtpd_epi32(vd); }
#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt { return _mm256_cvttpd_epi32(vd); }
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble { return _mm256_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return _mm256_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vrint_vf_vf(vd: VFloat) -> VFloat { return _mm256_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vf_vf(vf: VFloat) -> VFloat { return _mm256_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vcast_vd_vi(vi: VInt) -> VDouble { return _mm256_cvtepi32_pd(vi); }
#[inline]
fn vcast_vi_i(i: int) -> VInt { return _mm_set1_epi32(i); }
#[inline]
fn vcastu_vi2_vi(vi: VInt) -> VInt2 {
  VInt2 r;
  r.x = _mm_and_si128(_mm_shuffle_epi32(vi, 0x40), _mm_set_epi32(-1, 0, -1, 0));
  r.y = _mm_and_si128(_mm_shuffle_epi32(vi, 0xc8), _mm_set_epi32(-1, 0, -1, 0));
  return r;
}

#[inline]
fn vcastu_vi_vi2(vi: VInt2) -> VInt {
  return _mm_or_si128(_mm_and_si128(_mm_shuffle_epi32(vi.x, 0x0d), _mm_set_epi32( 0,  0, -1, -1)),
		      _mm_and_si128(_mm_shuffle_epi32(vi.y, 0xd0), _mm_set_epi32(-1, -1,  0,  0)));
}

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> VMask {
  return _mm256_set_epi32(i0, i1, i0, i1, i0, i1, i0, i1);
}

#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask {
  return vreinterpret_vm_vd(_mm256_cmp_pd(vreinterpret_vd_vm(vxor_vm_vm_vm(vxor_vm_vm_vm(x, y), vreinterpret_vm_vd(_mm256_set1_pd(1.0)))), _mm256_set1_pd(1.0), _CMP_EQ_OQ));
}


#[inline]
fn vrec_vd_vd(x: VDouble) -> VDouble { return _mm256_div_pd(_mm256_set1_pd(1), x); }
#[inline]
fn vsqrt_vd_vd(x: VDouble) -> VDouble { return _mm256_sqrt_pd(x); }
#[inline]
fn vabs_vd_vd(d: VDouble) -> VDouble { return _mm256_andnot_pd(_mm256_set1_pd(-0.0), d); }

#if CONFIG == 1
#[inline]
fn vmla_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return x*y + z; }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return x*y- z; }
#else
#[inline]
fn vmla_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm256_macc_pd(x, y, z); }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm256_msub_pd(x, y, z); }
#[inline]
fn vfma_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm256_macc_pd(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm256_macc_pd(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm256_msub_pd(x, y, z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm256_nmacc_pd(x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm256_nmsub_pd(x, y, z); }
#endif


//

#[inline]
fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_add_epi32(x, y); }
#[inline]
fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_sub_epi32(x, y); }
#[inline]
fn vneg_vi_vi(e: VInt) -> VInt { return vsub_vi_vi_vi(vcast_vi_i(0), e); }

#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_andnot_si128(x, y); }
#[inline]
fn vor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_or_si128(x, y); }
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_xor_si128(x, y); }

#[inline]
fn vandnot_vi_vo_vi(m: VOpMask, y: VInt) -> VInt { return _mm_andnot_si128(_mm256_castsi256_si128(m), y); }
#[inline]
fn vand_vi_vo_vi(m: VOpMask, y: VInt) -> VInt { return _mm_and_si128(_mm256_castsi256_si128(m), y); }

#[inline]
fn vsll_vi_vi_i(x: VInt, c: int) -> VInt { return _mm_slli_epi32(x, c); }
#[inline]
fn vsrl_vi_vi_i(x: VInt, c: int) -> VInt { return _mm_srli_epi32(x, c); }
#[inline]
fn vsra_vi_vi_i(x: VInt, c: int) -> VInt { return _mm_srai_epi32(x, c); }

#[inline]
fn veq_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_cmpgt_epi32(x, y); }

#[inline]
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return _mm256_castsi128_si256(_mm_cmpeq_epi32(x, y)); }
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return _mm256_castsi128_si256(_mm_cmpgt_epi32(x, y)); }

#[inline]
fn vsel_vi_vo_vi_vi(o: VOpMask, x: VInt, y: VInt) -> VInt { return _mm_blendv_epi8(y, x, _mm256_castsi256_si128(o)); }

#[inline]
fn vsel_vd_vo_vd_vd(o: VOpMask, x: VDouble, y: VDouble) -> VDouble { return _mm256_blendv_pd(y, x, _mm256_castsi256_pd(o)); }

#[inline]
fn vsel_vd_vo_d_d(o: VOpMask, v1: double, v0: double) -> CONST -> VDouble {
  return vsel_vd_vo_vd_vd(o, vcast_vd_d(v1), vcast_vd_d(v0));
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(VOpMask o0, VOpMask o1, double d0, double d1, double d2) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, vcast_vd_d(d0), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(VOpMask o0, VOpMask o1, VOpMask o2, double d0, double d1, double d2, double d3) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, vcast_vd_d(d0), vsel_vd_vo_vd_vd(o1, vcast_vd_d(d1), vsel_vd_vo_d_d(o2, d2, d3)));
}

#[inline]
fn visinf_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpret_vm_vd(_mm256_cmp_pd(vabs_vd_vd(d), _mm256_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn vispinf_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpret_vm_vd(_mm256_cmp_pd(d, _mm256_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visminf_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpret_vm_vd(_mm256_cmp_pd(d, _mm256_set1_pd(-SLEEF_INFINITY), _CMP_EQ_OQ));
}

#[inline]
fn visnan_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpret_vm_vd(_mm256_cmp_pd(d, d, _CMP_NEQ_UQ));
}

#[inline]
fn vload_vd_p(const double *ptr) -> VDouble { return _mm256_load_pd(ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble { return _mm256_loadu_pd(ptr); }

#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { _mm256_store_pd(ptr, v); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void { _mm256_storeu_pd(ptr, v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble {
  int a[VECTLENDP];
  vstoreu_v_p_vi(a, vi);
  return _mm256_set_pd(ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]]);
}

#if defined(_MSC_VER)
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_d_vd(v: VDouble) -> double {
  double a[VECTLENDP];
  vstoreu_v_p_vd(a, v);
  return a[0];
}
#endif

//

#[inline]
fn vcast_vi2_vm(vm: VMask) -> VInt2 {
  VInt2 r;
  r.x = _mm256_castsi256_si128(vm);
  r.y = _mm256_extractf128_si256(vm, 1);
  return r;
}

#[inline]
fn vcast_vm_vi2(vi: VInt2) -> VMask {
  VMask m = _mm256_castsi128_si256(vi.x);
  m = _mm256_insertf128_si256(m, vi.y, 1);
  return m;
}

#[inline]
fn vrint_vi2_vf(vf: VFloat) -> VInt2 { return vcast_vi2_vm(_mm256_cvtps_epi32(vf)); }
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { return vcast_vi2_vm(_mm256_cvttps_epi32(vf)); }
#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat { return _mm256_cvtepi32_ps(vcast_vm_vi2(vi)); }

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        _mm256_set1_ps(self)
    }
}
#[inline]
fn vcast_vi2_i(i: int) -> VInt2 { VInt2 r; r.x = r.y = _mm_set1_epi32(i); return r; }
#[inline]
fn vreinterpret_vm_vf(vf: VFloat) -> VMask { return _mm256_castps_si256(vf); }
#[inline]
fn vreinterpret_vf_vm(vm: VMask) -> VFloat { return _mm256_castsi256_ps(vm); }

#[inline]
fn vreinterpret_vf_vi2(vi: VInt2) -> VFloat { return vreinterpret_vf_vm(vcast_vm_vi2(vi)); }
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) -> VInt2 { return vcast_vi2_vm(vreinterpret_vm_vf(vf)); }

#[inline]
fn vrec_vf_vf(x: VFloat) -> VFloat { return vcast_vf_f(1.) / x); }
#[inline]
fn vsqrt_vf_vf(x: VFloat) -> VFloat { return _mm256_sqrt_ps(x); }
#[inline]
fn vabs_vf_vf(f: VFloat) -> VFloat { return vreinterpret_vf_vm(vandnot_vm_vm_vm(vreinterpret_vm_vf(vcast_vf_f(-0.0f)), vreinterpret_vm_vf(f))); }

#if CONFIG == 1
#[inline]
fn vmla_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return x*y+z; }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return z - x * y); }
#else
#[inline]
fn vmla_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm256_macc_ps(x, y, z); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm256_nmacc_ps(x, y, z); }
#[inline]
fn vfma_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm256_macc_ps(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm256_macc_ps(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm256_msub_ps(x, y, z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm256_nmacc_ps(x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm256_nmsub_ps(x, y, z); }
#endif

#[inline]
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  VInt2 vi = { _mm_add_epi32(x.x, y.x), _mm_add_epi32(x.y, y.y) };
  return vi;
}

#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  VInt2 vi = { _mm_sub_epi32(x.x, y.x), _mm_sub_epi32(x.y, y.y) };
  return vi;
}

#[inline]
fn vneg_vi2_vi2(e: VInt2) -> VInt2 {
  VInt2 vi = { _mm_sub_epi32(_mm_set1_epi32(0), e.x), _mm_sub_epi32(_mm_set1_epi32(0), e.y) };
  return vi;
}

#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  VInt2 vi = { _mm_and_si128(x.x, y.x), _mm_and_si128(x.y, y.y) };
  return vi;
}

#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  VInt2 vi = { _mm_andnot_si128(x.x, y.x), _mm_andnot_si128(x.y, y.y) };
  return vi;
}

#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  VInt2 vi = { _mm_or_si128(x.x, y.x), _mm_or_si128(x.y, y.y) };
  return vi;
}

#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  VInt2 vi = { _mm_xor_si128(x.x, y.x), _mm_xor_si128(x.y, y.y) };
  return vi;
}

#[inline]
fn vand_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return vand_vi2_vi2_vi2(vcast_vi2_vm(x), y); }
#[inline]
fn vandnot_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return vandnot_vi2_vi2_vi2(vcast_vi2_vm(x), y); }

#[inline]
fn vsll_vi2_vi2_i(x: VInt2, c: int) -> VInt2 {
  VInt2 vi = { _mm_slli_epi32(x.x, c), _mm_slli_epi32(x.y, c) };
  return vi;
}

#[inline]
fn vsrl_vi2_vi2_i(x: VInt2, c: int) -> VInt2 {
  VInt2 vi = { _mm_srli_epi32(x.x, c), _mm_srli_epi32(x.y, c) };
  return vi;
}

#[inline]
fn vsra_vi2_vi2_i(x: VInt2, c: int) -> VInt2 {
  VInt2 vi = { _mm_srai_epi32(x.x, c), _mm_srai_epi32(x.y, c) };
  return vi;
}

#[inline]
fn veq_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask {
  VInt2 r;
  r.x = _mm_cmpeq_epi32(x.x, y.x);
  r.y = _mm_cmpeq_epi32(x.y, y.y);
  return vcast_vm_vi2(r);
}

#[inline]
fn vgt_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask {
  VInt2 r;
  r.x = _mm_cmpgt_epi32(x.x, y.x);
  r.y = _mm_cmpgt_epi32(x.y, y.y);
  return vcast_vm_vi2(r);
}

#[inline]
fn veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  VInt2 r;
  r.x = _mm_cmpeq_epi32(x.x, y.x);
  r.y = _mm_cmpeq_epi32(x.y, y.y);
  return r;
}

#[inline]
fn vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  VInt2 r;
  r.x = _mm_cmpgt_epi32(x.x, y.x);
  r.y = _mm_cmpgt_epi32(x.y, y.y);
  return r;
}

#[inline]
fn vsel_vi2_vo_vi2_vi2(m: VOpMask, x: VInt2, y: VInt2) -> VInt2 {
  VInt2 n = vcast_vi2_vm(m);
  VInt2 r = { _mm_blendv_epi8(y.x, x.x, n.x), _mm_blendv_epi8(y.y, x.y, n.y) };
  return r;
}

#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  VInt2 ix = vcast_vi2_vm(x), iy = vcast_vi2_vm(y), iz;
  iz.x = _mm_add_epi64(ix.x, iy.x);
  iz.y = _mm_add_epi64(ix.y, iy.y);
  return vcast_vm_vi2(iz);
}

#[inline]
fn vsel_vf_vo_vf_vf(o: VOpMask, x: VFloat, y: VFloat) -> VFloat { return _mm256_blendv_ps(y, x, _mm256_castsi256_ps(o)); }

#[inline]
fn vsel_vf_vo_f_f(o: VOpMask, v1: float, v0: float) -> CONST -> VFloat {
  return vsel_vf_vo_vf_vf(o, vcast_vf_f(v1), vcast_vf_f(v0));
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(VOpMask o0, VOpMask o1, float d0, float d1, float d2) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, vcast_vf_f(d0), vsel_vf_vo_f_f(o1, d1, d2));
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(VOpMask o0, VOpMask o1, VOpMask o2, float d0, float d1, float d2, float d3) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, vcast_vf_f(d0), vsel_vf_vo_vf_vf(o1, vcast_vf_f(d1), vsel_vf_vo_f_f(o2, d2, d3)));
}

#[inline]
fn visinf_vo_vf(d: VFloat) -> VOpMask { return vabs_vf_vf(d).ne(vcast_vf_f(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask { return d.ne(vcast_vf_f(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask { return d.ne(vcast_vf_f(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: VFloat) -> VOpMask { return d.ne(d); }

//

#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return _mm256_load_ps(ptr); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat { return _mm256_loadu_ps(ptr); }

#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { _mm256_store_ps(ptr, v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void { _mm256_storeu_ps(ptr, v); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi2) -> VFloat {
  int a[VECTLENSP];
  vstoreu_v_p_vi2(a, vi2);
  return _mm256_set_ps(ptr[a[7]], ptr[a[6]], ptr[a[5]], ptr[a[4]],
		       ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]]);
}

#ifdef _MSC_VER
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_f_vf(v: VFloat) -> float {
  float a[VECTLENSP];
  vstoreu_v_p_vf(a, v);
  return a[0];
}
#endif
//

#define PNMASK ((VDouble) { +0.0, -0.0, +0.0, -0.0 })
#define NPMASK ((VDouble) { -0.0, +0.0, -0.0, +0.0 })
#define PNMASKf ((VFloat) { +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf ((VFloat) { -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f })

#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(PNMASK))); }
#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(NPMASK))); }
#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(PNMASKf))); }
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(NPMASKf))); }

#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm256_addsub_pd(x, y); }
#[inline]
fn vsubadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm256_addsub_ps(x, y); }

#if CONFIG == 1
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vsubadd_vd_vd_vd(x*y, z); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vsubadd_vf_vf_vf(x * y, z); }
#else
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vmla_vd_vd_vd_vd(x, y, vnegpos_vd_vd(z)); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vmla_vf_vf_vf_vf(x, y, vnegpos_vf_vf(z)); }
#endif


#[inline]
fn vrev21_vd_vd(d0: VDouble) -> VDouble { return  _mm256_shuffle_pd(d0, d0, (0 << 3) | (1 << 2) | (0 << 1) | (1 << 0)); }
#[inline]
fn vreva2_vd_vd(d0: VDouble) -> VDouble { d0 = _mm256_permute2f128_pd(d0, d0, 1); return _mm256_shuffle_pd(d0, d0, (1 << 3) | (0 << 2) | (1 << 1) | (0 << 0)); }

#[inline]
fn vstream_v_p_vd(double *ptr, VDouble v) -> void { _mm256_stream_pd(ptr, v); }
#[inline]
fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void {
  _mm_store_pd(&ptr[(offset + step * 0)*2], _mm256_extractf128_pd(v, 0));
  _mm_store_pd(&ptr[(offset + step * 1)*2], _mm256_extractf128_pd(v, 1));
}

#[inline]
fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void {
  _mm_stream_pd(&ptr[(offset + step * 0)*2], _mm256_extractf128_pd(v, 0));
  _mm_stream_pd(&ptr[(offset + step * 1)*2], _mm256_extractf128_pd(v, 1));
}

//

#[inline]
fn vrev21_vf_vf(d0: VFloat) -> VFloat { return _mm256_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vreva2_vf_vf(d0: VFloat) -> VFloat { d0 = _mm256_permute2f128_ps(d0, d0, 1); return _mm256_shuffle_ps(d0, d0, (1 << 6) | (0 << 4) | (3 << 2) | (2 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vstream_v_p_vf(float *ptr, VFloat v) -> void { _mm256_stream_ps(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
  _mm_storel_pd((double *)(ptr+(offset + step * 0)*2), _mm_castsi128_pd(_mm_castps_si128(_mm256_extractf128_ps(v, 0))));
  _mm_storeh_pd((double *)(ptr+(offset + step * 1)*2), _mm_castsi128_pd(_mm_castps_si128(_mm256_extractf128_ps(v, 0))));
  _mm_storel_pd((double *)(ptr+(offset + step * 2)*2), _mm_castsi128_pd(_mm_castps_si128(_mm256_extractf128_ps(v, 1))));
  _mm_storeh_pd((double *)(ptr+(offset + step * 3)*2), _mm_castsi128_pd(_mm_castps_si128(_mm256_extractf128_ps(v, 1))));
}

#[inline]
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }
