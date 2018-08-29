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

typedef __m128i VMask;
typedef __m128i VOpMask;

typedef __m128d VDouble;
typedef __m128i VInt;

typedef __m128  VFloat;
typedef __m128i VInt2;

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

#if defined(__SSE2__) && defined(__SSE3__) && defined(__SSE4_1__)
#[inline]
fn vavailability_i(name: int) -> int {
  //int d = __builtin_cpu_supports("sse2") && __builtin_cpu_supports("sse3") && __builtin_cpu_supports("sse4.1");
  int d = cpuSupportsSSE2() && cpuSupportsSSE3() && cpuSupportsSSE4_1();
  return d ? 3 : 0;
}
#define ISANAME "SSE4.1"
#define DFTPRIORITY 12
#elif defined(__SSE2__) && defined(__SSE3__)
#[inline]
fn vavailability_i(name: int) -> int {
  //int d = __builtin_cpu_supports("sse2") && __builtin_cpu_supports("sse3");
  int d = cpuSupportsSSE2() && cpuSupportsSSE3();
  return d ? 3 : 0;
}
#define ISANAME "SSE3"
#define DFTPRIORITY 11
#else
#[inline]
fn vavailability_i(name: int) -> int {
  int d = cpuSupportsSSE2();
  return d ? 3 : 0;
}
#define ISANAME "SSE2"
#define DFTPRIORITY 10
#endif

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { _mm_prefetch(ptr, _MM_HINT_T0); }

#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int { return _mm_movemask_epi8(g) == 0xFFFF; }
#[inline]
fn vtestallones_i_vo64(g: VOpMask) -> int { return _mm_movemask_epi8(g) == 0xFFFF; }

//

static VInt2 vloadu_vi2_p(int32_t *p) { return _mm_loadu_si128((__m128i *)p); }
static void vstoreu_v_p_vi2(int32_t *p, VInt2 v) { _mm_storeu_si128((__m128i *)p, v); }

static VInt vloadu_vi_p(int32_t *p) { return _mm_loadu_si128((__m128i *)p); }
static void vstoreu_v_p_vi(int32_t *p, VInt v) { _mm_storeu_si128((__m128i *)p, v); }

//

#[inline]
fn vand_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm_andnot_si128(x, y); }
#[inline]
fn vor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm_or_si128(x, y); }
#[inline]
fn vxor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm_xor_si128(x, y); }

#[inline]
fn vand_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return _mm_andnot_si128(x, y); }
#[inline]
fn vor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return _mm_or_si128(x, y); }
#[inline]
fn vxor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return _mm_xor_si128(x, y); }

#[inline]
fn vand_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return _mm_and_si128(x, y); }
#[inline]
fn vor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return _mm_or_si128(x, y); }
#[inline]
fn vandnot_vm_vo64_vm(x: VMask, y: VMask) -> VMask { return _mm_andnot_si128(x, y); }
#[inline]
fn vxor_vm_vo64_vm(x: VMask, y: VMask) -> VMask { return _mm_xor_si128(x, y); }

#[inline]
fn vand_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return _mm_and_si128(x, y); }
#[inline]
fn vor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return _mm_or_si128(x, y); }
#[inline]
fn vandnot_vm_vo32_vm(x: VMask, y: VMask) -> VMask { return _mm_andnot_si128(x, y); }
#[inline]
fn vxor_vm_vo32_vm(x: VMask, y: VMask) -> VMask { return _mm_xor_si128(x, y); }

#[inline]
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask { return _mm_shuffle_epi32(m, 0x08); }
#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask { return _mm_shuffle_epi32(m, 0x50); }

//

#[inline]
fn vrint_vi_vd(vd: VDouble) -> VInt { return _mm_cvtpd_epi32(vd); }
#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt { return _mm_cvttpd_epi32(vd); }
#[inline]
fn vcast_vd_vi(vi: VInt) -> VDouble { return _mm_cvtepi32_pd(vi); }
impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        _mm_set_epi32(0, 0, self, self)
    }
}
//#[inline]
//fn vcast_vi_i(i: int) -> VInt { return _mm_set_epi32(0, 0, i, i); }

impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        _mm_and_si128(_mm_shuffle_epi32(vi, 0x73), _mm_set_epi32(-1, 0, -1, 0))
    }
}
//#[inline]
//fn vcastu_vi2_vi(vi: VInt) -> VInt2 { return _mm_and_si128(_mm_shuffle_epi32(vi, 0x73), _mm_set_epi32(-1, 0, -1, 0)); }
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
      _mm_shuffle_epi32(vi, 0x0d)
    }
}
//#[inline]
//fn vcastu_vi_vi2(vi: VInt2) -> VInt { return _mm_shuffle_epi32(vi, 0x0d); }

#ifdef __SSE4_1__
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return _mm_round_pd(vd, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble { return _mm_round_pd(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn vtruncate_vf_vf(vf: VFloat) -> VFloat { return _mm_round_ps(vf, _MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC); }
#[inline]
fn vrint_vf_vf(vd: VFloat) -> VFloat { return _mm_round_ps(vd, _MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC); }
#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask { return _mm_cmpeq_epi64(x, y); }
#define FULL_FP_ROUNDING
#else
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return vcast_vd_vi(vtruncate_vi_vd(vd)); }
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble { return vcast_vd_vi(vrint_vi_vd(vd)); }
#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask {
  VMask t = _mm_cmpeq_epi32(x, y);
  return vand_vm_vm_vm(t, _mm_shuffle_epi32(t, 0xb1));
}
#endif

#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask { return _mm_add_epi64(x, y); }

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> VMask { return _mm_set_epi32(i0, i1, i0, i1); }

//

impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        _mm_set1_pd(d)
    }
}
#[inline]
fn vreinterpret_vm_vd(vd: VDouble) -> VMask { return _mm_castpd_si128(vd); }
#[inline]
fn vreinterpret_vi2_vd(vd: VDouble) -> VInt2 { return _mm_castpd_si128(vd); }
#[inline]
fn vreinterpret_vd_vi2(vi: VInt2) -> VDouble { return _mm_castsi128_pd(vi); }
#[inline]
fn vreinterpret_vd_vm(vm: VMask) -> VDouble { return _mm_castsi128_pd(vm); }

#[inline]
fn vadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm_add_pd(x, y); }
#[inline]
fn vsub_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm_sub_pd(x, y); }
#[inline]
fn vmul_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm_mul_pd(x, y); }
#[inline]
fn vdiv_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm_div_pd(x, y); }
#[inline]
fn vrec_vd_vd(x: VDouble) -> VDouble { return _mm_div_pd(_mm_set1_pd(1), x); }
#[inline]
fn vsqrt_vd_vd(x: VDouble) -> VDouble { return _mm_sqrt_pd(x); }
#[inline]
fn vabs_vd_vd(d: VDouble) -> VDouble { return _mm_andnot_pd(_mm_set1_pd(-0.0), d); }
#[inline]
fn vneg_vd_vd(d: VDouble) -> VDouble { return _mm_xor_pd(_mm_set1_pd(-0.0), d); }
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        vadd_vd_vd_vd(vmul_vd_vd_vd(x, y), z)
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vsub_vd_vd_vd(vmul_vd_vd_vd(x, y), z); }
#[inline]
fn vmax_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm_max_pd(x, y); }
#[inline]
fn vmin_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm_min_pd(x, y); }

#[inline]
fn veq_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm_castpd_si128(_mm_cmpeq_pd(x, y)); }
#[inline]
fn vneq_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm_castpd_si128(_mm_cmpneq_pd(x, y)); }
#[inline]
fn vlt_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm_castpd_si128(_mm_cmplt_pd(x, y)); }
#[inline]
fn vle_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm_castpd_si128(_mm_cmple_pd(x, y)); }
#[inline]
fn vgt_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm_castpd_si128(_mm_cmpgt_pd(x, y)); }
#[inline]
fn vge_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return _mm_castpd_si128(_mm_cmpge_pd(x, y)); }

impl std::ops::Add for VInt {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        _mm_add_epi32(self, other)
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_add_epi32(x, y); }
impl std::ops::Sub for VInt {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        _mm_sub_epi32(x, y)
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_sub_epi32(x, y); }
impl std::ops::Neg for VInt {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        vcast_vi_i(0) - self
    }
}
//#[inline]
//fn vneg_vi_vi(e: VInt) -> VInt { return vsub_vi_vi_vi(vcast_vi_i(0), e); }

#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_andnot_si128(x, y); }
#[inline]
fn vor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_or_si128(x, y); }
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return _mm_xor_si128(x, y); }

#[inline]
fn vand_vi_vo_vi(x: VOpMask, y: VInt) -> VInt { return _mm_and_si128(x, y); }
#[inline]
fn vandnot_vi_vo_vi(x: VOpMask, y: VInt) -> VInt { return _mm_andnot_si128(x, y); }

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
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return _mm_cmpgt_epi32(x, y); }

#ifdef __SSE4_1__
#[inline]
fn vsel_vi_vo_vi_vi(m: VOpMask, x: VInt, y: VInt) -> VInt { return _mm_blendv_epi8(y, x, m); }

#[inline]
fn vsel_vd_vo_vd_vd(m: VOpMask, x: VDouble, y: VDouble) -> VDouble { return _mm_blendv_pd(y, x, _mm_castsi128_pd(m)); }
#else
#[inline]
fn vsel_vi_vo_vi_vi(m: VOpMask, x: VInt, y: VInt) -> VInt { return vor_vm_vm_vm(vand_vm_vm_vm(m, x), vandnot_vm_vm_vm(m, y)); }

#[inline]
fn vsel_vd_vo_vd_vd(opmask: VOpMask, x: VDouble, y: VDouble) -> VDouble {
  return vreinterpret_vd_vm(vor_vm_vm_vm(vand_vm_vm_vm(opmask, vreinterpret_vm_vd(x)), vandnot_vm_vm_vm(opmask, vreinterpret_vm_vd(y))));
}
#endif

#[inline]
fn VDouble vsel_vd_vo_d_d(o: VOpMask, v1: double, v0: double) -> CONST {
  return vsel_vd_vo_vd_vd(o, v1.as_vd(), v0.as_vd());
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(VOpMask o0, VOpMask o1, double d0, double d1, double d2) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, d0.as_vd(), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(VOpMask o0, VOpMask o1, VOpMask o2, double d0, double d1, double d2, double d3) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, d0.as_vd(), vsel_vd_vo_vd_vd(o1, d1.as_vd(), vsel_vd_vo_d_d(o2, d2, d3)));
}

#[inline]
fn visinf_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpret_vm_vd(_mm_cmpeq_pd(vabs_vd_vd(d), _mm_set1_pd(SLEEF_INFINITY)));
}

#[inline]
fn vispinf_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpret_vm_vd(_mm_cmpeq_pd(d, _mm_set1_pd(SLEEF_INFINITY)));
}

#[inline]
fn visminf_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpret_vm_vd(_mm_cmpeq_pd(d, _mm_set1_pd(-SLEEF_INFINITY)));
}

#[inline]
fn visnan_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpret_vm_vd(_mm_cmpneq_pd(d, d));
}

//

#[inline]
fn vload_vd_p(const double *ptr) -> VDouble { return _mm_load_pd(ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble { return _mm_loadu_pd(ptr); }

#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { _mm_store_pd(ptr, v); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void { _mm_storeu_pd(ptr, v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble {
  int a[sizeof(VInt)/sizeof(int)];
  vstoreu_v_p_vi(a, vi);
  return _mm_set_pd(ptr[a[1]], ptr[a[0]]);
}

// This function is for debugging
#[inline]
fn vcast_d_vd(v: VDouble) -> double {
  double a[VECTLENDP];
  vstoreu_v_p_vd(a, v);
  return a[0];
}

//

#[inline]
fn vcast_vi2_vm(vm: VMask) -> VInt2 { return vm; }
#[inline]
fn vcast_vm_vi2(vi: VInt2) -> VMask { return vi; }
#[inline]
fn vrint_vi2_vf(vf: VFloat) -> VInt2 { return _mm_cvtps_epi32(vf); }
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { return _mm_cvttps_epi32(vf); }
#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat { return _mm_cvtepi32_ps(vcast_vm_vi2(vi)); }

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        _mm_set1_ps(self)
    }
}
#[inline]
fn vcast_vi2_i(i: int) -> VInt2 { return _mm_set1_epi32(i); }
#[inline]
fn vreinterpret_vm_vf(vf: VFloat) -> VMask { return _mm_castps_si128(vf); }
#[inline]
fn vreinterpret_vf_vm(vm: VMask) -> VFloat { return _mm_castsi128_ps(vm); }
#[inline]
fn vreinterpret_vf_vi2(vm: VInt2) -> VFloat { return _mm_castsi128_ps(vm); }
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) -> VInt2 { return _mm_castps_si128(vf); }

#ifndef __SSE4_1__
#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vf: VFloat) -> VFloat { return vcast_vf_vi2(vrint_vi2_vf(vf)); }
#endif

#[inline]
fn vadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm_add_ps(x, y); }
#[inline]
fn vsub_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm_sub_ps(x, y); }
#[inline]
fn vmul_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm_mul_ps(x, y); }
#[inline]
fn vdiv_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm_div_ps(x, y); }
#[inline]
fn vrec_vf_vf(x: VFloat) -> VFloat { return vdiv_vf_vf_vf((1.).as_vf(), x); }
#[inline]
fn vsqrt_vf_vf(x: VFloat) -> VFloat { return _mm_sqrt_ps(x); }
#[inline]
fn vabs_vf_vf(f: VFloat) -> VFloat { return vreinterpret_vf_vm(vandnot_vm_vm_vm(vreinterpret_vm_vf((-0.).as_vf()), vreinterpret_vm_vf(f))); }
#[inline]
fn vneg_vf_vf(d: VFloat) -> VFloat { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf((-0.).as_vf()), vreinterpret_vm_vf(d))); }
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        vadd_vf_vf_vf(vmul_vf_vf_vf(x, y), z)
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vsub_vf_vf_vf(z, vmul_vf_vf_vf(x, y)); }
#[inline]
fn vmax_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm_max_ps(x, y); }
#[inline]
fn vmin_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm_min_ps(x, y); }

#[inline]
fn veq_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return vreinterpret_vm_vf(_mm_cmpeq_ps(x, y)); }
#[inline]
fn vneq_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return vreinterpret_vm_vf(_mm_cmpneq_ps(x, y)); }
#[inline]
fn vlt_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return vreinterpret_vm_vf(_mm_cmplt_ps(x, y)); }
#[inline]
fn vle_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return vreinterpret_vm_vf(_mm_cmple_ps(x, y)); }
#[inline]
fn vgt_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return vreinterpret_vm_vf(_mm_cmpgt_ps(x, y)); }
#[inline]
fn vge_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return vreinterpret_vm_vf(_mm_cmpge_ps(x, y)); }

#[inline]
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { VInt::from(x)+VInt::from(y) }
#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { VInt::from(x)-VInt::from(y) }
#[inline]
fn vneg_vi2_vi2(e: VInt2) -> VInt2 { return vsub_vi2_vi2_vi2(vcast_vi2_i(0), e); }

#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vand_vi_vi_vi(x, y); }
#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vandnot_vi_vi_vi(x, y); }
#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vor_vi_vi_vi(x, y); }
#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vxor_vi_vi_vi(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return vand_vi_vo_vi(x, y); }
#[inline]
fn vandnot_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return vandnot_vi_vo_vi(x, y); }

#[inline]
fn vsll_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { return vsll_vi_vi_i(x, c); }
#[inline]
fn vsrl_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { return vsrl_vi_vi_i(x, c); }
#[inline]
fn vsra_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { return vsra_vi_vi_i(x, c); }

#[inline]
fn veq_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return _mm_cmpgt_epi32(x, y); }
#[inline]
fn veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return _mm_cmpeq_epi32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return _mm_cmpgt_epi32(x, y); }

#ifdef __SSE4_1__
#[inline]
fn vsel_vi2_vo_vi2_vi2(m: VOpMask, x: VInt2, y: VInt2) -> VInt2 { return _mm_blendv_epi8(y, x, m); }

#[inline]
fn vsel_vf_vo_vf_vf(m: VOpMask, x: VFloat, y: VFloat) -> VFloat { return _mm_blendv_ps(y, x, _mm_castsi128_ps(m)); }
#else
#[inline]
fn vsel_vi2_vo_vi2_vi2(m: VOpMask, x: VInt2, y: VInt2) -> VInt2 {
  return vor_vi2_vi2_vi2(vand_vi2_vi2_vi2(m, x), vandnot_vi2_vi2_vi2(m, y));
}

#[inline]
fn vsel_vf_vo_vf_vf(mask: VOpMask, x: VFloat, y: VFloat) -> VFloat {
  return vreinterpret_vf_vm(vor_vm_vm_vm(vand_vm_vm_vm(mask, vreinterpret_vm_vf(x)), vandnot_vm_vm_vm(mask, vreinterpret_vm_vf(y))));
}
#endif

#[inline]
fn VFloat vsel_vf_vo_f_f(o: VOpMask, v1: float, v0: float) -> CONST {
  return vsel_vf_vo_vf_vf(o, v1.as_vf(), v0.as_vf());
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(VOpMask o0, VOpMask o1, float d0, float d1, float d2) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, d0.as_vf(), vsel_vf_vo_f_f(o1, d1, d2));
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(VOpMask o0, VOpMask o1, VOpMask o2, float d0, float d1, float d2, float d3) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, d0.as_vf(), vsel_vf_vo_vf_vf(o1, d1.as_vf(), vsel_vf_vo_f_f(o2, d2, d3)));
}

#[inline]
fn visinf_vo_vf(d: VFloat) -> VOpMask { return veq_vo_vf_vf(vabs_vf_vf(d), SLEEF_INFINITY_F.as_vf()); }
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask { return veq_vo_vf_vf(d, SLEEF_INFINITY_F.as_vf()); }
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask { return veq_vo_vf_vf(d, (-SLEEF_INFINITY_F).as_vf()); }
#[inline]
fn visnan_vo_vf(d: VFloat) -> VOpMask { return vneq_vo_vf_vf(d, d); }

#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return _mm_load_ps(ptr); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat { return _mm_loadu_ps(ptr); }

#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { _mm_store_ps(ptr, v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void { _mm_storeu_ps(ptr, v); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi) -> VFloat {
  int a[VECTLENSP];
  vstoreu_v_p_vi2(a, vi);
  return _mm_set_ps(ptr[a[3]], ptr[a[2]], ptr[a[1]], ptr[a[0]]);
}

// This function is for debugging
#[inline]
fn vcast_f_vf(v: VFloat) -> float {
  float a[VECTLENSP];
  vstoreu_v_p_vf(a, v);
  return a[0];
}

//

#define PNMASK ((VDouble) { +0.0, -0.0 })
#define NPMASK ((VDouble) { -0.0, +0.0 })
#define PNMASKf ((VFloat) { +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf ((VFloat) { -0.0f, +0.0f, -0.0f, +0.0f })

#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(PNMASK))); }
#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(NPMASK))); }
#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(PNMASKf))); }
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(NPMASKf))); }

#ifdef __SSE3__
#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return _mm_addsub_pd(x, y); }
#[inline]
fn vsubadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return _mm_addsub_ps(x, y); }
#else
#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return vadd_vd_vd_vd(x, vnegpos_vd_vd(y)); }
#[inline]
fn vsubadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return vadd_vf_vf_vf(x, vnegpos_vf_vf(y)); }
#endif
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vsubadd_vd_vd_vd(vmul_vd_vd_vd(x, y), z); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vsubadd_vf_vf_vf(vmul_vf_vf_vf(x, y), z); }

#[inline]
fn vrev21_vd_vd(d0: VDouble) -> VDouble { return _mm_shuffle_pd(d0, d0, 1); }
#[inline]
fn vreva2_vd_vd(vd: VDouble) -> VDouble { return vd; }

//#[inline]
//fn vstream_v_p_vd(double *ptr, VDouble v) -> void { _mm_stream_pd(ptr, v); }
//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { vstore_v_p_vd((double *)(&ptr[2*offset]), v); }
//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { _mm_stream_pd((double *)(&ptr[2*offset]), v); }

//

#[inline]
fn vrev21_vf_vf(d0: VFloat) -> VFloat { return _mm_shuffle_ps(d0, d0, (2 << 6) | (3 << 4) | (0 << 2) | (1 << 0)); }
#[inline]
fn vreva2_vf_vf(d0: VFloat) -> VFloat { return _mm_shuffle_ps(d0, d0, (1 << 6) | (0 << 4) | (3 << 2) | (2 << 0)); }
#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

//#[inline]
//fn vstream_v_p_vf(float *ptr, VFloat v) -> void { _mm_stream_ps(ptr, v); }

//#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
//  _mm_storel_pd((double *)(ptr+(offset + step * 0)*2), vreinterpret_vd_vm(vreinterpret_vm_vf(v)));
//  _mm_storeh_pd((double *)(ptr+(offset + step * 1)*2), vreinterpret_vd_vm(vreinterpret_vm_vf(v)));
//}

#[inline]
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
  _mm_storel_pd((double *)(ptr+(offset + step * 0)*2), vreinterpret_vd_vm(vreinterpret_vm_vf(v)));
  _mm_storeh_pd((double *)(ptr+(offset + step * 1)*2), vreinterpret_vd_vm(vreinterpret_vm_vf(v)));
}
