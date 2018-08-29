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

type VMask = __m512i;
type VOpMask = __mmask16;

type VDouble = __m512d;
type VInt = __m256i;

type VFloat = __m512;
type VInt2 = __m512i;

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
fn vtestallones_i_vo64(g: VOpMask) -> int { return _mm512_mask2int(g) == 0xff; }
#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int { return _mm512_mask2int(g) == 0xffff; }
#else
#[inline]
fn vtestallones_i_vo64(g: VOpMask) -> int { return g == 0xff; }
#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int { return g == 0xffff; }
#endif

//

static VInt2 vloadu_vi2_p(int32_t *p) { return _mm512_loadu_si512((__m512i const *)p); }
static void vstoreu_v_p_vi2(int32_t *p, VInt2 v) { _mm512_storeu_si512((__m512i *)p, v); }
static VInt vloadu_vi_p(int32_t *p) { return _mm256_loadu_si256((__m256i const *)p); }
static void vstoreu_v_p_vi(int32_t *p, VInt v) { _mm256_storeu_si256((__m256i *)p, v); }

//

#[inline]
fn vand_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm512_and_si512(x, y); }
#[inline]
fn vandnot_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm512_andnot_si512(x, y); }
#[inline]
fn vor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm512_or_si512(x, y); }
#[inline]
fn vxor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm512_xor_si512(x, y); }

#[inline]
fn vand_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return _mm512_kand(x, y); }
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return _mm512_kandn(x, y); }
#[inline]
fn vor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return _mm512_kor(x, y); }
#[inline]
fn vxor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return _mm512_kxor(x, y); }

#[inline]
fn vand_vm_vo64_vm(o: VOpMask, m: VMask) -> VMask { return _mm512_mask_and_epi64(_mm512_set1_epi32(0), o, m, m); }
#[inline]
fn vandnot_vm_vo64_vm(o: VOpMask, m: VMask) -> VMask { return _mm512_mask_and_epi64(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)); }
#[inline]
fn vor_vm_vo64_vm(o: VOpMask, m: VMask) -> VMask { return _mm512_mask_or_epi64(m, o, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1)); }

#[inline]
fn vand_vm_vo32_vm(o: VOpMask, m: VMask) -> VMask { return _mm512_mask_and_epi32(_mm512_set1_epi32(0), o, m, m); }
#[inline]
fn vandnot_vm_vo32_vm(o: VOpMask, m: VMask) -> VMask { return _mm512_mask_and_epi32(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)); }
#[inline]
fn vor_vm_vo32_vm(o: VOpMask, m: VMask) -> VMask { return _mm512_mask_or_epi32(m, o, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1)); }

#[inline]
fn vcast_vo32_vo64(o: VOpMask) -> VOpMask { return o; }
#[inline]
fn vcast_vo64_vo32(o: VOpMask) -> VOpMask { return o; }

//

#[inline]
fn vrint_vi_vd(vd: VDouble) -> VInt {
  return _mm512_cvt_roundpd_epi32(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
}

#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt {
  return _mm512_cvt_roundpd_epi32(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
}

#[inline]
fn vcast_vd_vi(vi: VInt) -> VDouble { return _mm512_cvtepi32_pd(vi); }
#[inline]
fn vcast_vi_i(i: int) -> VInt { return _mm256_set1_epi32(i); }

#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble {
  __m256d hi = _mm512_extractf64x4_pd(vd, 1), lo = _mm512_extractf64x4_pd(vd, 0);
  hi = _mm256_round_pd(hi, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  lo = _mm256_round_pd(lo, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  return _mm512_insertf64x4(_mm512_castpd256_pd512(lo), hi, 1);
}

#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble {
  __m256d hi = _mm512_extractf64x4_pd(vd, 1), lo = _mm512_extractf64x4_pd(vd, 0);
  hi = _mm256_round_pd(hi, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  lo = _mm256_round_pd(lo, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  return _mm512_insertf64x4(_mm512_castpd256_pd512(lo), hi, 1);
}

#[inline]
fn vcastu_vi2_vi(vi: VInt) -> VInt2 {
  return _mm512_maskz_permutexvar_epi32(0xaaaa, _mm512_set_epi32(7, 7, 6, 6, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1, 0, 0), _mm512_castsi256_si512(vi));
}

#[inline]
fn vcastu_vi_vi2(vi: VInt2) -> VInt {
  return _mm512_castsi512_si256(_mm512_maskz_permutexvar_epi32(0x00ff, _mm512_set_epi32(0, 0, 0, 0, 0, 0, 0, 0, 15, 13, 11, 9, 7, 5, 3, 1), vi));
}

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> VMask { return _mm512_set_epi32(i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1, i0, i1); }

#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask { return _mm512_cmp_epi64_mask(x, y, _MM_CMPINT_EQ); }
#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm512_add_epi64(x, y); }

//

impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        _mm512_set1_pd(d)
    }
}
#[inline]
fn vreinterpret_vm_vd(vd: VDouble) -> VMask { return _mm512_castpd_si512(vd); }
#[inline]
fn vreinterpret_vd_vm(vm: VMask) -> VDouble { return _mm512_castsi512_pd(vm); }
#[inline]
fn vreinterpret_vi2_vd(vd: VDouble) -> VInt2 { return _mm512_castpd_si512(vd); }
#[inline]
fn vreinterpret_vd_vi2(vi: VInt2) -> VDouble { return _mm512_castsi512_pd(vi); }

#[inline]
fn vadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm512_add_pd(x, y); }
#[inline]
fn vsub_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm512_sub_pd(x, y); }
#[inline]
fn vmul_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm512_mul_pd(x, y); }
#[inline]
fn vdiv_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm512_div_pd(x, y); }
#[inline]
fn vrec_vd_vd(x: VDouble) -> VDouble { return _mm512_div_pd(_mm512_set1_pd(1), x); }
#[inline]
fn vsqrt_vd_vd(x: VDouble) -> VDouble { return _mm512_sqrt_pd(x); }
#[inline]
fn vabs_vd_vd(d: VDouble) -> VDouble { return vreinterpret_vd_vm(_mm512_andnot_si512(vreinterpret_vm_vd(_mm512_set1_pd(-0.0)), vreinterpret_vm_vd(d))); }
#[inline]
fn vneg_vd_vd(d: VDouble) -> VDouble { return vreinterpret_vd_vm(_mm512_xor_si512(vreinterpret_vm_vd(_mm512_set1_pd(-0.0)), vreinterpret_vm_vd(d))); }
#[inline]
fn vmax_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm512_max_pd(x, y); }
#[inline]
fn vmin_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm512_min_pd(x, y); }

#if CONFIG == 1
#[inline]
fn vmla_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fmadd_pd(x, y, z); }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fmsub_pd(x, y, z); }
#[inline]
fn vmlanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fnmadd_pd(x, y, z); }
#else
#[inline]
fn vmla_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vadd_vd_vd_vd(vmul_vd_vd_vd(x, y), z); }
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vsub_vd_vd_vd(vmul_vd_vd_vd(x, y), z); }
#endif

#[inline]
fn vfma_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fmadd_pd(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fmadd_pd(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fmsub_pd(x, y, z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fnmadd_pd(x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fnmsub_pd(x, y, z); }

#[inline]
fn veq_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm512_cmp_pd_mask(x, y, _CMP_EQ_OQ); }
#[inline]
fn vneq_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm512_cmp_pd_mask(x, y, _CMP_NEQ_UQ); }
#[inline]
fn vlt_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm512_cmp_pd_mask(x, y, _CMP_LT_OQ); }
#[inline]
fn vle_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm512_cmp_pd_mask(x, y, _CMP_LE_OQ); }
#[inline]
fn vgt_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm512_cmp_pd_mask(x, y, _CMP_GT_OQ); }
#[inline]
fn vge_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm512_cmp_pd_mask(x, y, _CMP_GE_OQ); }

//

#[inline]
fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm256_add_epi32(x, y); }
#[inline]
fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm256_sub_epi32(x, y); }
#[inline]
fn vneg_vi_vi(e: VInt) -> VInt { return vsub_vi_vi_vi(vcast_vi_i(0), e); }

#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm256_and_si256(x, y); }
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm256_andnot_si256(x, y); }

#[inline]
fn vandnot_vi_vo_vi(o: VOpMask, y: VInt) -> VInt {
  return _mm512_castsi512_si256(_mm512_mask_and_epi32(_mm512_castsi256_si512(y), o, _mm512_set1_epi32(0), _mm512_set1_epi32(0)));
}
#[inline]
fn vand_vi_vo_vi(o: VOpMask, y: VInt) -> VInt {
  return _mm512_castsi512_si256(_mm512_mask_and_epi32(_mm512_set1_epi32(0), o, _mm512_castsi256_si512(y), _mm512_castsi256_si512(y)));
}

#[inline]
fn vor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm256_or_si256(x, y); }
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm256_xor_si256(x, y); }
#define vsll_vi_vi_i(x, c) _mm256_slli_epi32(x, c)
#define vsrl_vi_vi_i(x, c) _mm256_srli_epi32(x, c)
#define vsra_vi_vi_i(x, c) _mm256_srai_epi32(x, c)

#[inline]
fn veq_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm256_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm256_cmpgt_epi32(x, y); }

#[inline]
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask {
  return _mm512_cmp_epi32_mask(_mm512_castsi256_si512(x), _mm512_castsi256_si512(y), _MM_CMPINT_EQ);
}
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask {
  return _mm512_cmp_epi32_mask(_mm512_castsi256_si512(y), _mm512_castsi256_si512(x), _MM_CMPINT_LT);
}

#[inline]
fn vsel_vd_vo_vd_vd(mask: VOpMask, x: VDouble, y: VDouble) -> VDouble {
  return _mm512_mask_blend_pd(mask, y, x);
}

#[inline]
fn VDouble vsel_vd_vo_d_d(o: VOpMask, v1: double, v0: double) -> CONST {
  return vsel_vd_vo_vd_vd(o, vcast_vd_d(v1), vcast_vd_d(v0));
}

#if 1
// Probably this is faster
#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(VOpMask o0, VOpMask o1, VOpMask o2, double d0, double d1, double d2, double d3) -> VDouble {
  __m512i v = _mm512_castpd_si512(vsel_vd_vo_vd_vd(o0, _mm512_castsi512_pd(_mm512_set_epi64(0, 0, 0, 0, 0, 0, 0, 0)),
						   vsel_vd_vo_vd_vd(o1, _mm512_castsi512_pd(_mm512_set_epi64(1, 1, 1, 1, 1, 1, 1, 1)),
								    vsel_vd_vo_vd_vd(o2, _mm512_castsi512_pd(_mm512_set_epi64(2, 2, 2, 2, 2, 2, 2, 2)),
										     _mm512_castsi512_pd(_mm512_set_epi64(3, 3, 3, 3, 3, 3, 3, 3))))));
  return _mm512_permutexvar_pd(v, _mm512_castpd256_pd512(_mm256_set_pd(d3, d2, d1, d0)));
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(VOpMask o0, VOpMask o1, double d0, double d1, double d2) -> VDouble {
  return vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o1, d0, d1, d2, d2);
}
#else
#[inline]
fn vsel_vd_vo_vo_d_d_d(VOpMask o0, VOpMask o1, double d0, double d1, double d2) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, vcast_vd_d(d0), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(VOpMask o0, VOpMask o1, VOpMask o2, double d0, double d1, double d2, double d3) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, vcast_vd_d(d0), vsel_vd_vo_vd_vd(o1, vcast_vd_d(d1), vsel_vd_vo_d_d(o2, d2, d3)));
}
#endif

#[inline]
fn visinf_vo_vd(d: VDouble) -> VOpMask {
  return _mm512_cmp_pd_mask(vabs_vd_vd(d), _mm512_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ);
}

#[inline]
fn vispinf_vo_vd(d: VDouble) -> VOpMask {
  return _mm512_cmp_pd_mask(d, _mm512_set1_pd(SLEEF_INFINITY), _CMP_EQ_OQ);
}

#[inline]
fn visminf_vo_vd(d: VDouble) -> VOpMask {
  return _mm512_cmp_pd_mask(d, _mm512_set1_pd(-SLEEF_INFINITY), _CMP_EQ_OQ);
}

#[inline]
fn visnan_vo_vd(d: VDouble) -> VOpMask {
  return _mm512_cmp_pd_mask(d, d, _CMP_NEQ_UQ);
}

#[inline]
fn vilogbk_vi_vd(d: VDouble) -> VInt { return vrint_vi_vd(_mm512_getexp_pd(d)); }

// vilogb2k_vi_vd is similar to vilogbk_vi_vd, but the argument has to
// be a normalized FP value.
#[inline]
fn vilogb2k_vi_vd(d: VDouble) -> VInt { return vrint_vi_vd(_mm512_getexp_pd(d)); }

#[inline]
fn vgetexp_vd_vd(d: VDouble) -> VDouble { return _mm512_getexp_pd(d); }
#[inline]
fn vgetexp_vf_vf(d: VFloat) -> VFloat { return _mm512_getexp_ps(d); }

#[inline]
fn vgetmant_vd_vd(d: VDouble) -> VDouble { return _mm512_getmant_pd(d, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_nan); }
#[inline]
fn vgetmant_vf_vf(d: VFloat) -> VFloat { return _mm512_getmant_ps(d, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_nan); }

#define vfixup_vd_vd_vd_vi2_i(a, b, c, imm) _mm512_fixupimm_pd((a), (b), (c), (imm))
#define vfixup_vf_vf_vf_vi2_i(a, b, c, imm) _mm512_fixupimm_ps((a), (b), (c), (imm))

#if defined(_MSC_VER)
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_d_vd(v: VDouble) -> double {
  double s[VECTLENDP];
  _mm512_storeu_pd(s, v);
  return s[0];
}
#endif

#[inline]
fn vload_vd_p(const double *ptr) -> VDouble { return _mm512_load_pd(ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble { return _mm512_loadu_pd(ptr); }

#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { _mm512_store_pd(ptr, v); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void { _mm512_storeu_pd(ptr, v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble { return _mm512_i32gather_pd(vi, ptr, 8); }

//

#[inline]
fn vsel_vi_vo_vi_vi(m: VOpMask, x: VInt, y: VInt) -> VInt {
  return _mm512_castsi512_si256(_mm512_mask_blend_epi32(m, _mm512_castsi256_si512(y), _mm512_castsi256_si512(x)));
}

//

#[inline]
fn vreinterpret_vm_vf(vf: VFloat) -> VMask { return _mm512_castps_si512(vf); }
#[inline]
fn vreinterpret_vf_vm(vm: VMask) -> VFloat { return _mm512_castsi512_ps(vm); }
#[inline]
fn vreinterpret_vf_vi2(vi: VInt2) -> VFloat { return _mm512_castsi512_ps(vi); }
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) -> VInt2 { return _mm512_castps_si512(vf); }

#[inline]
fn vreinterpret_vd_vf(vf: VFloat) -> VDouble { return _mm512_castps_pd(vf); }
#[inline]
fn vreinterpret_vf_vd(vd: VDouble) -> VFloat { return _mm512_castpd_ps(vd); }

#[inline]
fn vcast_vi2_vm(vm: VMask) -> VInt2 { return vm; }
#[inline]
fn vcast_vm_vi2(vi: VInt2) -> VMask { return vi; }
 
#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat { return _mm512_cvtepi32_ps(vcast_vm_vi2(vi)); }

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        _mm512_set1_ps(self)
    }
}
#[inline]
fn vcast_vi2_i(i: int) -> VInt2 { return _mm512_set1_epi32(i); }
#[inline]
fn vrint_vi2_vf(vf: VFloat) -> VInt2 { return vcast_vi2_vm(_mm512_cvtps_epi32(vf)); }
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { return vcast_vi2_vm(_mm512_cvttps_epi32(vf)); }

#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat {
  __m256 hi = _mm256_castpd_ps(_mm512_extractf64x4_pd(vreinterpret_vd_vf(vd), 1));
  __m256 lo = _mm256_castpd_ps(_mm512_extractf64x4_pd(vreinterpret_vd_vf(vd), 0));
  hi = _mm256_round_ps(hi, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  lo = _mm256_round_ps(lo, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC);
  return vreinterpret_vf_vd(_mm512_insertf64x4(_mm512_castpd256_pd512(_mm256_castps_pd(lo)), _mm256_castps_pd(hi), 1));
}
 
#[inline]
fn vrint_vf_vf(vd: VFloat) -> VFloat {
  __m256 hi = _mm256_castpd_ps(_mm512_extractf64x4_pd(vreinterpret_vd_vf(vd), 1));
  __m256 lo = _mm256_castpd_ps(_mm512_extractf64x4_pd(vreinterpret_vd_vf(vd), 0));
  hi = _mm256_round_ps(hi, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  lo = _mm256_round_ps(lo, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC);
  return vreinterpret_vf_vd(_mm512_insertf64x4(_mm512_castpd256_pd512(_mm256_castps_pd(lo)), _mm256_castps_pd(hi), 1));
}

#[inline]
fn vadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm512_add_ps(x, y); }
#[inline]
fn vsub_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm512_sub_ps(x, y); }
#[inline]
fn vmul_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm512_mul_ps(x, y); }
#[inline]
fn vdiv_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm512_div_ps(x, y); }
#[inline]
fn vrec_vf_vf(x: VFloat) -> VFloat { return vdiv_vf_vf_vf(vcast_vf_f(1.0f), x); }
#[inline]
fn vsqrt_vf_vf(x: VFloat) -> VFloat { return _mm512_sqrt_ps(x); }
#[inline]
fn vabs_vf_vf(f: VFloat) -> VFloat { return vreinterpret_vf_vm(vandnot_vm_vm_vm(vreinterpret_vm_vf(vcast_vf_f(-0.0f)), vreinterpret_vm_vf(f))); }
#[inline]
fn vneg_vf_vf(d: VFloat) -> VFloat { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(vcast_vf_f(-0.0f)), vreinterpret_vm_vf(d))); }
#[inline]
fn vmax_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm512_max_ps(x, y); }
#[inline]
fn vmin_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm512_min_ps(x, y); }

#if CONFIG == 1
#[inline]
fn vmla_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fmadd_ps(x, y, z); }
#[inline]
fn vmlapn_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fmsub_ps(x, y, z); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fnmadd_ps(x, y, z); }
#else
#[inline]
fn vmla_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vadd_vf_vf_vf(vmul_vf_vf_vf(x, y), z); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vsub_vf_vf_vf(z, vmul_vf_vf_vf(x, y)); }
#endif

#[inline]
fn vfma_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fmadd_ps(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fmadd_ps(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fmsub_ps(x, y, z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fnmadd_ps(x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fnmsub_ps(x, y, z); }

#[inline]
fn veq_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return _mm512_cmp_ps_mask(x, y, _CMP_EQ_OQ); }
#[inline]
fn vneq_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return _mm512_cmp_ps_mask(x, y, _CMP_NEQ_UQ); }
#[inline]
fn vlt_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return _mm512_cmp_ps_mask(x, y, _CMP_LT_OQ); }
#[inline]
fn vle_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return _mm512_cmp_ps_mask(x, y, _CMP_LE_OQ); }
#[inline]
fn vgt_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return _mm512_cmp_ps_mask(x, y, _CMP_GT_OQ); }
#[inline]
fn vge_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return _mm512_cmp_ps_mask(x, y, _CMP_GE_OQ); }

#[inline]
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return _mm512_add_epi32(x, y); }
#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return _mm512_sub_epi32(x, y); }
#[inline]
fn vneg_vi2_vi2(e: VInt2) -> VInt2 { return vsub_vi2_vi2_vi2(vcast_vi2_i(0), e); }
#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return _mm512_and_si512(x, y); }
#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return _mm512_andnot_si512(x, y); }
#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return _mm512_or_si512(x, y); }
#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return _mm512_xor_si512(x, y); }

#[inline]
fn vand_vi2_vo_vi2(o: VOpMask, m: VInt2) -> VInt2 {
  return _mm512_mask_and_epi32(_mm512_set1_epi32(0), o, m, m);
}

#[inline]
fn vandnot_vi2_vo_vi2(o: VOpMask, m: VInt2) -> VInt2 {
  return _mm512_mask_and_epi32(m, o, _mm512_set1_epi32(0), _mm512_set1_epi32(0));
}

#define vsll_vi2_vi2_i(x, c) _mm512_slli_epi32(x, c)
#define vsrl_vi2_vi2_i(x, c) _mm512_srli_epi32(x, c)
#define vsra_vi2_vi2_i(x, c) _mm512_srai_epi32(x, c)
#[inline]
fn veq_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return _mm512_cmpeq_epi32_mask(x, y); }
#[inline]
fn vgt_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return _mm512_cmpgt_epi32_mask(x, y); }

#[inline]
fn veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  __mmask16 m = _mm512_cmp_epi32_mask(x, y, _MM_CMPINT_EQ);
  return _mm512_mask_and_epi32(_mm512_set1_epi32(0), m, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1));
}
#[inline]
fn vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  __mmask16 m = _mm512_cmp_epi32_mask(y, x, _MM_CMPINT_LT);
  return _mm512_mask_and_epi32(_mm512_set1_epi32(0), m, _mm512_set1_epi32(-1), _mm512_set1_epi32(-1));
}

#[inline]
fn vsel_vi2_vo_vi2_vi2(m: VOpMask, x: VInt2, y: VInt2) -> VInt2 {
  return _mm512_mask_blend_epi32(m, y, x);
}

#[inline]
fn vsel_vf_vo_vf_vf(m: VOpMask, x: VFloat, y: VFloat) -> VFloat {
  return _mm512_mask_blend_ps(m, y, x);
}

// At this point, the following three functions are implemented in a generic way,
// but I will try target-specific optimization later on.
#[inline]
fn VFloat vsel_vf_vo_f_f(o: VOpMask, v1: float, v0: float) -> CONST {
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
fn visinf_vo_vf(d: VFloat) -> VOpMask { return veq_vo_vf_vf(vabs_vf_vf(d), vcast_vf_f(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask { return veq_vo_vf_vf(d, vcast_vf_f(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask { return veq_vo_vf_vf(d, vcast_vf_f(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: VFloat) -> VOpMask { return vneq_vo_vf_vf(d, d); }

#[inline]
fn vilogbk_vi2_vf(d: VFloat) -> VInt2 { return vrint_vi2_vf(_mm512_getexp_ps(d)); }
#[inline]
fn vilogb2k_vi2_vf(d: VFloat) -> VInt2 { return vrint_vi2_vf(_mm512_getexp_ps(d)); }

#ifdef _MSC_VER
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_f_vf(v: VFloat) -> float {
  float s[VECTLENSP];
  _mm512_storeu_ps(s, v);
  return s[0];
}
#endif

#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return _mm512_load_ps(ptr); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat { return _mm512_loadu_ps(ptr); }

#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { _mm512_store_ps(ptr, v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void { _mm512_storeu_ps(ptr, v); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi2) -> VFloat { return _mm512_i32gather_ps(vi2, ptr, 4); }

//

#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble {
  return vreinterpret_vd_vm(_mm512_mask_xor_epi32(vreinterpret_vm_vd(d), 0xcccc, vreinterpret_vm_vd(d), vreinterpret_vm_vd(_mm512_set1_pd(-0.0))));
}
#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble {
  return vreinterpret_vd_vm(_mm512_mask_xor_epi32(vreinterpret_vm_vd(d), 0x3333, vreinterpret_vm_vd(d), vreinterpret_vm_vd(_mm512_set1_pd(-0.0))));
}
#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat {
  return vreinterpret_vf_vm(_mm512_mask_xor_epi32(vreinterpret_vm_vf(d), 0xaaaa, vreinterpret_vm_vf(d), vreinterpret_vm_vf(_mm512_set1_ps(-0.0f))));
}
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat {
  return vreinterpret_vf_vm(_mm512_mask_xor_epi32(vreinterpret_vm_vf(d), 0x5555, vreinterpret_vm_vf(d), vreinterpret_vm_vf(_mm512_set1_ps(-0.0f))));
}

#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return vadd_vd_vd_vd(x, vnegpos_vd_vd(y)); }
#[inline]
fn vsubadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return vadd_vf_vf_vf(x, vnegpos_vf_vf(y)); }

#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return _mm512_fmaddsub_pd(x, y, z); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return _mm512_fmaddsub_ps(x, y, z); }

#[inline]
fn vrev21_vd_vd(vd: VDouble) -> VDouble { return _mm512_permute_pd(vd, 0x55); }

#[inline]
fn vreva2_vd_vd(vd: VDouble) -> VDouble {
  return vreinterpret_vd_vm(_mm512_permutexvar_epi32(_mm512_set_epi32(3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12), vreinterpret_vm_vd(vd)));
}

#[inline]
fn vstream_v_p_vd(double *ptr, VDouble v) -> void { _mm512_stream_pd(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void {
  _mm_store_pd(&ptr[(offset + step * 0)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 0)));
  _mm_store_pd(&ptr[(offset + step * 1)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 1)));
  _mm_store_pd(&ptr[(offset + step * 2)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 2)));
  _mm_store_pd(&ptr[(offset + step * 3)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 3)));
}

#[inline]
fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void {
  _mm_stream_pd(&ptr[(offset + step * 0)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 0)));
  _mm_stream_pd(&ptr[(offset + step * 1)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 1)));
  _mm_stream_pd(&ptr[(offset + step * 2)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 2)));
  _mm_stream_pd(&ptr[(offset + step * 3)*2], _mm_castps_pd(_mm512_extractf32x4_ps(vreinterpret_vf_vd(v), 3)));
}

//

#[inline]
fn vrev21_vf_vf(vf: VFloat) -> VFloat { return _mm512_permute_ps(vf, 0xb1); }
#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vreva2_vf_vf(vf: VFloat) -> VFloat {
  return vreinterpret_vf_vm(_mm512_permutexvar_epi32(_mm512_set_epi32(1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14), vreinterpret_vm_vf(vf)));
}

#[inline]
fn vstream_v_p_vf(float *ptr, VFloat v) -> void { _mm512_stream_ps(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
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
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }
