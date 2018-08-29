//          Copyright Naoki Shibata 2010 - 2018.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <stdint.h>

#ifndef ENABLE_BUILTIN_MATH
#include <math.h>

#define SQRT sqrt
#define SQRTF sqrtf
#define FMA fma
#define FMAF fmaf
#define RINT rint
#define RINTF rintf
#define TRUNC trunc
#define TRUNCF truncf

#else

#define SQRT __builtin_sqrt
#define SQRTF __builtin_sqrtf
#define FMA __builtin_fma
#define FMAF __builtin_fmaf
#define RINT __builtin_rint
#define RINTF __builtin_rintf
#define TRUNC __builtin_trunc
#define TRUNCF __builtin_truncf

#endif

#include "misc.h"

#ifndef CONFIG
#error CONFIG macro not defined
#endif

#define ENABLE_DP
#define ENABLE_SP

#if CONFIG == 2
#define ENABLE_FMA_DP
#define ENABLE_FMA_SP

#if defined(__AVX2__) || defined(__aarch64__) || defined(__arm__) || defined(__powerpc64__)
#ifndef FP_FAST_FMA
#define FP_FAST_FMA
#endif
#ifndef FP_FAST_FMAF
#define FP_FAST_FMAF
#endif
#endif

#if !defined(FP_FAST_FMA) || !defined(FP_FAST_FMAF)
#error FP_FAST_FMA or FP_FAST_FMAF not defined
#endif
#define ISANAME "Pure C scalar with FMA"

#else // #if CONFIG == 2
#define ISANAME "Pure C scalar"
#endif // #if CONFIG == 2

#define LOG2VECTLENDP 0
#define VECTLENDP (1 << LOG2VECTLENDP)
#define LOG2VECTLENSP 0
#define VECTLENSP (1 << LOG2VECTLENSP)

#define ACCURATE_SQRT

#if defined(__SSE4_1__) || defined(__aarch64__)
#define FULL_FP_ROUNDING
#endif

#define DFTPRIORITY LOG2VECTLENDP

typedef union {
  uint32_t u[2];
  int32_t i[2];
  uint64_t x;
  double d;
  float f;
  int64_t i2;
} versatileVector;

typedef uint64_t VMask;
typedef uint32_t VOpMask;
typedef double VDouble;
typedef int32_t VInt;
typedef float VFloat;
typedef int64_t VInt2;

//

#[inline]
fn vavailability_i(name: int) -> int { return -1; }
#[inline]
fn vprefetch_v_p(const void *ptr) -> void {}

#[inline]
fn vtestallones_i_vo64(g: VOpMask) -> int { return g; }
#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int { return g; }

//

static VInt2 vloadu_vi2_p(int32_t *p) { return *p; }
static void vstoreu_v_p_vi2(int32_t *p, VInt2 v) { *p = v; }
static VInt vloadu_vi_p(int32_t *p) { return *p; }
static void vstoreu_v_p_vi(int32_t *p, VInt v) { *p = v; }

//

#[inline]
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask { return m; }
#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask { return m; }
#[inline]
fn vcast_vm_i_i(h: int, l: int) -> VMask { return (((uint64_t)h) << 32) | (uint32_t)l; }

impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        (self as u64) << 32
    }
}
//#[inline]
//fn vcastu_vi2_vi(vi: VInt) -> VInt2 { return ((int64_t)vi) << 32; }
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
      self >> 32
    }
}
//#[inline]
//fn vcastu_vi_vi2(vi2: VInt2) -> VInt { return vi2 >> 32; }

#[inline]
fn vrev21_vi2_vi2(vi2: VInt2) -> VInt2 { return (((uint64_t)vi2) << 32) | (((uint64_t)vi2) >> 32); }

impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        d
    }
}

//

#[inline]
fn vand_vo_vo_vo   (x: VOpMask, y: VOpMask) -> VOpMask { return x & y; }
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return y & ~x; }
#[inline]
fn vor_vo_vo_vo    (x: VOpMask, y: VOpMask) -> VOpMask { return x | y; }
#[inline]
fn vxor_vo_vo_vo   (x: VOpMask, y: VOpMask) -> VOpMask { return x ^ y; }

#[inline]
fn vand_vm_vm_vm     (x: VMask, y: VMask)     -> VMask { return x & y; }
#[inline]
fn vandnot_vm_vm_vm  (x: VMask, y: VMask)     -> VMask { return y & ~x; }
#[inline]
fn vor_vm_vm_vm      (x: VMask, y: VMask)     -> VMask { return x | y; }
#[inline]
fn vxor_vm_vm_vm     (x: VMask, y: VMask)     -> VMask { return x ^ y; }

#[inline]
fn vcast_vm_vo(o: VOpMask) -> VMask { return (VMask)o | (((VMask)o) << 32); }

#[inline]
fn vand_vm_vo64_vm(x: VOpMask, y: VMask)      -> VMask { return vcast_vm_vo(x) & y; }
#[inline]
fn vandnot_vm_vo64_vm(x: VOpMask, y: VMask)   -> VMask { return y & ~vcast_vm_vo(x); }
#[inline]
fn vor_vm_vo64_vm(x: VOpMask, y: VMask)       -> VMask { return vcast_vm_vo(x) | y; }
#[inline]
fn vxor_vm_vo64_vm(x: VOpMask, y: VMask)      -> VMask { return vcast_vm_vo(x) ^ y; }

#[inline]
fn vand_vm_vo32_vm(x: VOpMask, y: VMask)      -> VMask { return vcast_vm_vo(x) & y; }
#[inline]
fn vandnot_vm_vo32_vm(x: VOpMask, y: VMask)   -> VMask { return y & ~vcast_vm_vo(x); }
#[inline]
fn vor_vm_vo32_vm(x: VOpMask, y: VMask)       -> VMask { return vcast_vm_vo(x) | y; }
#[inline]
fn vxor_vm_vo32_vm(x: VOpMask, y: VMask)      -> VMask { return vcast_vm_vo(x) ^ y; }

//

#[inline]
fn vsel_vd_vo_vd_vd   (o: VOpMask, x: VDouble, y: VDouble) -> VDouble { return o ? x : y; }
#[inline]
fn   vsel_vi2_vo_vi2_vi2(o: VOpMask, x: VInt2, y: VInt2)     -> VInt2 { return o ? x : y; }

#[inline]
fn VDouble vsel_vd_vo_d_d(o: VOpMask, v1: double, v0: double) -> CONST { return o ? v1 : v0; }

#[inline]
fn vsel_vd_vo_vo_d_d_d(VOpMask o0, VOpMask o1, double d0, double d1, double d2) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, d0.as_vd(), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(VOpMask o0, VOpMask o1, VOpMask o2, double d0, double d1, double d2, double d3) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, d0.as_vd(), vsel_vd_vo_vd_vd(o1, d1.as_vd(), vsel_vd_vo_d_d(o2, d2, d3)));
}

#[inline]
fn vcast_vd_vi(vi: VInt) -> VDouble { return vi; }
impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        self
    }
}
//#[inline]
//fn vcast_vi_i(j: int) -> VInt { return j; }

#ifdef FULL_FP_ROUNDING
#[inline]
fn vrint_vi_vd(d: VDouble) -> VInt { return (int32_t)RINT(d); }
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble { return RINT(vd); }
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return TRUNC(vd); }
#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt { return (int32_t)TRUNC(vd); }
#else
#[inline]
fn vrint_vi_vd(a: VDouble) -> VInt {
  a += a > 0 ? 0.5 : -0.5;
  versatileVector v = { .d = a }; v.x -= 1 & (int)a;
  return (int32_t)v.d;
}
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble { return vcast_vd_vi(vrint_vi_vd(vd)); }
#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt { return vd; }
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return vcast_vd_vi(vtruncate_vi_vd(vd)); }
#endif

#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask { return x == y ? ~(uint32_t)0 : 0; }
#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask { return x + y; }

//

#[inline]
fn vreinterpret_vm_vd(vd: VDouble) { union -> VMask { VDouble vd; VMask vm; } cnv; cnv.vd = vd; return cnv.vm; }
#[inline]
fn vreinterpret_vi2_vd(vd: VDouble) { union -> VInt2 { VDouble vd; VInt2 vi2; } cnv; cnv.vd = vd; return cnv.vi2; }
#[inline]
fn vreinterpret_vd_vi2(vi: VInt2) { union -> VDouble { VInt2 vi2; VDouble vd; } cnv; cnv.vi2 = vi; return cnv.vd; }
#[inline]
fn vreinterpret_vd_vm(vm: VMask) { union -> VDouble { VMask vm; VDouble vd; } cnv; cnv.vm = vm; return cnv.vd; }

#[inline]
fn vadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x + y; }
#[inline]
fn vsub_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x - y; }
#[inline]
fn vmul_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x * y; }
#[inline]
fn vdiv_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x / y; }
#[inline]
fn vrec_vd_vd(x: VDouble)               -> VDouble { return 1 / x; }

#[inline]
fn vabs_vd_vd(d: VDouble) { versatileVector v = -> VDouble { .d = d }; v.x &= 0x7fffffffffffffffULL; return v.d; }
#[inline]
fn vneg_vd_vd(d: VDouble) -> VDouble { return -d; }

#[inline]
fn vmax_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x > y ? x : y; }
#[inline]
fn vmin_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x < y ? x : y; }

#ifndef ENABLE_FMA_DP
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return x * y - z; }
#else
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        FMA(x, y, z)
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return FMA(x, y, -z); }
#[inline]
fn vmlanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return FMA(-x, y, z); }
#[inline]
fn vfma_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return FMA(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return FMA(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return FMA(x, y, -z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return FMA(-x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return FMA(-x, y, -z); }
#endif

#[inline]
fn veq_vo_vd_vd(x: VDouble, y: VDouble)  -> VOpMask { return x == y ? ~(uint32_t)0 : 0; }
#[inline]
fn vneq_vo_vd_vd(x: VDouble, y: VDouble) -> VOpMask { return x != y ? ~(uint32_t)0 : 0; }
#[inline]
fn vlt_vo_vd_vd(x: VDouble, y: VDouble)  -> VOpMask { return x <  y ? ~(uint32_t)0 : 0; }
#[inline]
fn vle_vo_vd_vd(x: VDouble, y: VDouble)  -> VOpMask { return x <= y ? ~(uint32_t)0 : 0; }
#[inline]
fn vgt_vo_vd_vd(x: VDouble, y: VDouble)  -> VOpMask { return x >  y ? ~(uint32_t)0 : 0; }
#[inline]
fn vge_vo_vd_vd(x: VDouble, y: VDouble)  -> VOpMask { return x >= y ? ~(uint32_t)0 : 0; }

impl std::ops::Add for VInt {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        self + other
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x + y; }
impl std::ops::Sub for VInt {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        self - other
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x - y; }
impl std::ops::Neg for VInt {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        -self
    }
}

#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt)    -> VInt { return x & y; }
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt { return y & ~x; }
#[inline]
fn vor_vi_vi_vi(x: VInt, y: VInt)     -> VInt { return x | y; }
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt)    -> VInt { return x ^ y; }

#[inline]
fn vand_vi_vo_vi(x: VOpMask, y: VInt)    -> VInt { return x & y; }
#[inline]
fn vandnot_vi_vo_vi(x: VOpMask, y: VInt) -> VInt { return y & ~x; }

#[inline]
fn vsll_vi_vi_i(x: VInt, c: int) -> VInt { return (uint32_t)x << c; }
#[inline]
fn vsrl_vi_vi_i(x: VInt, c: int) -> VInt { return (uint32_t)x >> c; }
#[inline]
fn vsra_vi_vi_i(x: VInt, c: int) -> VInt { return x >> c; }

#[inline]
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return x == y ? ~(uint32_t)0 : 0; }
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return x >  y ? ~(uint32_t)0 : 0; }

#[inline]
fn vsel_vi_vo_vi_vi(m: VOpMask, x: VInt, y: VInt) -> VInt { return m ? x : y; }

#[inline]
fn visinf_vo_vd(d: VDouble)  -> VOpMask { return (d == SLEEF_INFINITY || d == -SLEEF_INFINITY) ? ~(uint32_t)0 : 0; }
#[inline]
fn vispinf_vo_vd(d: VDouble) -> VOpMask { return d == SLEEF_INFINITY ? ~(uint32_t)0 : 0; }
#[inline]
fn visminf_vo_vd(d: VDouble) -> VOpMask { return d == -SLEEF_INFINITY ? ~(uint32_t)0 : 0; }
#[inline]
fn visnan_vo_vd(d: VDouble)  -> VOpMask { return d != d ? ~(uint32_t)0 : 0; }

#[inline]
fn vsqrt_vd_vd(d: VDouble) -> VDouble { return SQRT(d); }
#[inline]
fn vsqrt_vf_vf(x: VFloat) -> VFloat { return SQRTF(x); }

#[inline]
fn vcast_d_vd(v: VDouble) -> double { return v; }

#[inline]
fn vload_vd_p(const double *ptr) -> VDouble { return *ptr; }
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble { return *ptr; }
#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble { return ptr[vi]; }

#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { *ptr = v; }
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void { *ptr = v; }
//#[inline]
//fn vstream_v_p_vd(double *ptr, VDouble v) -> void { *ptr = v; }

//

#[inline]
fn vcast_vi2_vm(vm: VMask) { union -> VInt2 { VInt2 vi2; VMask vm; } cnv; cnv.vm = vm; return cnv.vi2; }
#[inline]
fn vcast_vm_vi2(vi: VInt2) { union -> VMask { VInt2 vi2; VMask vm; } cnv; cnv.vi2 = vi; return cnv.vm; }

#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat { return (int32_t)vi; }
#[inline]
fn vcast_vi2_i(j: int) -> VInt2 { return j; }

#ifdef FULL_FP_ROUNDING
#[inline]
fn vrint_vi2_vf(d: VFloat) -> VInt2 { return (int)RINTF(d); }
#[inline]
fn vrint_vf_vf(vd: VFloat) -> VFloat { return RINTF(vd); }
#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat { return TRUNCF(vd); }
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { return (int32_t)TRUNCF(vf); }
#else
#[inline]
fn vrint_vi2_vf(a: VFloat) -> VInt2 {
  a += a > 0 ? 0.5f : -0.5f;
  versatileVector v = { .f = a }; v.u[0] -= 1 & (int)a;
  return (int32_t)v.f;
}
#[inline]
fn vrint_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vrint_vi2_vf(vd)); }
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { return vf; }
#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#endif

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        self
    }
}
#[inline]
fn vreinterpret_vm_vf(vf: VFloat) { union -> VMask { VFloat vf; VMask vm; } cnv; cnv.vf = vf; return cnv.vm; }
#[inline]
fn vreinterpret_vf_vm(vm: VMask) { union -> VFloat { VFloat vf; VMask vm; } cnv; cnv.vm = vm; return cnv.vf; }
#[inline]
fn vreinterpret_vf_vi2(vi: VInt2) { union -> VFloat { VFloat vf; VInt2 vi2; } cnv; cnv.vi2 = vi; return cnv.vf; }
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) { union -> VInt2 { VFloat vf; VInt2 vi2; } cnv; cnv.vi2 = 0; cnv.vf = vf; return cnv.vi2; }

#[inline]
fn vadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return x + y; }
#[inline]
fn vsub_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return x - y; }
#[inline]
fn vmul_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return x * y; }
#[inline]
fn vdiv_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return x / y; }
#[inline]
fn vrec_vf_vf   (x: VFloat)           -> VFloat { return 1 / x; }

#[inline]
fn vabs_vf_vf(x: VFloat) { versatileVector v = -> VFloat { .f = x }; v.x &= 0x7fffffff; return v.f; }
#[inline]
fn vneg_vf_vf(x: VFloat) -> VFloat { return -x; }

#[inline]
fn vmax_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return x > y ? x : y; }
#[inline]
fn vmin_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return x < y ? x : y; }

#ifndef ENABLE_FMA_SP
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return - x * y + z; }
#else
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        FMAF(x, y, z)
    }
}
#[inline]
fn vmlapn_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return FMAF(x, y, -z); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return FMAF(-x, y, z); }
#[inline]
fn vfma_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return FMAF(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return FMAF(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return FMAF(x, y, -z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return FMAF(-x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return FMAF(-x, y, -z); }
#endif

#[inline]
fn veq_vo_vf_vf(x: VFloat, y: VFloat)  -> VOpMask { return x == y ? ~(uint32_t)0 : 0; }
#[inline]
fn vneq_vo_vf_vf(x: VFloat, y: VFloat) -> VOpMask { return x != y ? ~(uint32_t)0 : 0; }
#[inline]
fn vlt_vo_vf_vf(x: VFloat, y: VFloat)  -> VOpMask { return x <  y ? ~(uint32_t)0 : 0; }
#[inline]
fn vle_vo_vf_vf(x: VFloat, y: VFloat)  -> VOpMask { return x <= y ? ~(uint32_t)0 : 0; }
#[inline]
fn vgt_vo_vf_vf(x: VFloat, y: VFloat)  -> VOpMask { return x >  y ? ~(uint32_t)0 : 0; }
#[inline]
fn vge_vo_vf_vf(x: VFloat, y: VFloat)  -> VOpMask { return x >= y ? ~(uint32_t)0 : 0; }

#[inline]
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) { versatileVector v = { .i2 = x }, w = -> VInt2 { .i2 = y }; v.i[0] += w.i[0]; v.i[1] += w.i[1]; return v.i2; }
#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) { versatileVector v = { .i2 = x }, w = -> VInt2 { .i2 = y }; v.i[0] -= w.i[0]; v.i[1] -= w.i[1]; return v.i2; }
#[inline]
fn vneg_vi2_vi2(x: VInt2)              { versatileVector v = -> VInt2 { .i2 = x }; v.i[0] = -v.i[0]; return v.i2; }

#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y)    -> VInt2 { return x & y; }
#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return y & ~x; }
#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y)     -> VInt2 { return x | y; }
#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y)    -> VInt2 { return x ^ y; }

#[inline]
fn vsel_vf_vo_vf_vf(o: VOpMask, x: VFloat, y: VFloat) -> VFloat { return o ? x : y; }
#[inline]
fn vsel_vf_vo_f_f(o: VOpMask, v1: float, v0: float) -> VFloat { return o ? v1 : v0; }

#[inline]
fn vsel_vf_vo_vo_f_f_f(VOpMask o0, VOpMask o1, float d0, float d1, float d2) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, d0.as_vf(), vsel_vf_vo_f_f(o1, d1, d2));
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(VOpMask o0, VOpMask o1, VOpMask o2, float d0, float d1, float d2, float d3) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, d0.as_vf(), vsel_vf_vo_vf_vf(o1, d1.as_vf(), vsel_vf_vo_f_f(o2, d2, d3)));
}

#[inline]
fn vand_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return vcast_vm_vo(x) & y; }
#[inline]
fn vandnot_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return y & ~vcast_vm_vo(x); }

#[inline]
fn vsll_vi2_vi2_i(x: VInt2, c: int) { versatileVector v = -> VInt2 { .i2 = x }; v.u[0] <<= c; v.u[1] <<= c; return v.i2; }
#[inline]
fn vsrl_vi2_vi2_i(x: VInt2, c: int) { versatileVector v = -> VInt2 { .i2 = x }; v.u[0] >>= c; v.u[1] >>= c; return v.i2; }
#[inline]
fn vsra_vi2_vi2_i(x: VInt2, c: int) { versatileVector v = -> VInt2 { .i2 = x }; v.i[0] >>= c; v.i[1] >>= c; return v.i2; }

#[inline]
fn visinf_vo_vf (d: VFloat) -> VOpMask { return (d == SLEEF_INFINITYf || d == -SLEEF_INFINITYf) ? ~(uint32_t)0 : 0; }
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask { return d == SLEEF_INFINITYf ? ~(uint32_t)0 : 0; }
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask { return d == -SLEEF_INFINITYf ? ~(uint32_t)0 : 0; }
#[inline]
fn visnan_vo_vf (d: VFloat) -> VOpMask { return d != d ? ~(uint32_t)0 : 0; }

#[inline]
fn veq_vo_vi2_vi2 (VInt2 x, VInt2 y) -> VOpMask { return (int32_t)x == (int32_t)y ? ~(uint32_t)0 : 0; }
#[inline]
fn vgt_vo_vi2_vi2 (VInt2 x, VInt2 y) -> VOpMask { return (int32_t)x >  (int32_t)y ? ~(uint32_t)0 : 0; }
#[inline]
fn   veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return (int32_t)x == (int32_t)y ? ~(uint32_t)0 : 0; }
#[inline]
fn   vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return (int32_t)x >  (int32_t)y ? ~(uint32_t)0 : 0; }

#[inline]
fn vcast_f_vf(v: VFloat) -> float { return v; }

#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return *ptr; }
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat { return *ptr; }
#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi) -> VFloat { return ptr[vi]; }

#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { *ptr = v; }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void { *ptr = v; }
//#[inline]
//fn vstream_v_p_vf(float *ptr, VFloat v) -> void { *ptr = v; }
