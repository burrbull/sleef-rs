/*********************************************************************/
/*          Copyright ARM Ltd. 2010 - 2017.                          */
/* Distributed under the Boost Software License, Version 1.0.        */
/*    (See accompanying file LICENSE.txt or copy at                  */
/*          http://www.boost.org/LICENSE_1_0.txt)                    */
/*********************************************************************/

#ifndef __ARM_FEATURE_SVE
#error Please specify SVE flags.
#endif

#include <arm_sve.h>
#include <stdint.h>

#include "misc.h"

#if defined(VECTLENDP) || defined(VECTLENSP)
#error VECTLENDP or VECTLENSP already defined
#endif

#if CONFIG == 1 || CONFIG == 2
// Vector length agnostic
#define VECTLENSP (svcntw())
#define VECTLENDP (svcntd())
#define ISANAME "AArch64 SVE"
#define ptrue svptrue_b8()
#elif CONFIG == 8
// 256-bit vector length
#define ISANAME "AArch64 SVE 256-bit"
#define LOG2VECTLENDP 2
#define ptrue svptrue_pat_b8(SV_VL32)
#define DFTPRIORITY 20
#elif CONFIG == 9
// 512-bit vector length
#define ISANAME "AArch64 SVE 512-bit"
#define LOG2VECTLENDP 3
#define ptrue svptrue_pat_b8(SV_VL64)
#define DFTPRIORITY 21
#elif CONFIG == 10
// 1024-bit vector length
#define ISANAME "AArch64 SVE 1024-bit"
#define LOG2VECTLENDP 4
#define ptrue svptrue_pat_b8(SV_VL128)
#define DFTPRIORITY 22
#elif CONFIG == 11
// 2048-bit vector length
#define ISANAME "AArch64 SVE 2048-bit"
#define LOG2VECTLENDP 5
#define ptrue svptrue_pat_b8(SV_VL256)
#define DFTPRIORITY 23
#else
#error CONFIG macro invalid or not defined
#endif

#ifdef LOG2VECTLENDP
// For DFT, VECTLENDP and VECTLENSP are not the size of the available
// vector length, but the size of the partial vectors utilized in the
// computation. The appropriate VECTLENDP and VECTLENSP are chosen by
// the dispatcher according to the value of svcntd().

#define LOG2VECTLENSP (LOG2VECTLENDP+1)
#define VECTLENDP (1 << LOG2VECTLENDP)
#define VECTLENSP (1 << LOG2VECTLENSP)
#[inline]
fn vavailability_i(name: int) -> int { return svcntd() >= VECTLENDP ? 3 : 0; }
#else
#[inline]
fn vavailability_i(name: int) -> int { return 3; }
#endif

#define ENABLE_SP
#define ENABLE_DP

#if CONFIG != 2
#define ENABLE_FMA_SP
#define ENABLE_FMA_DP
//#define SPLIT_KERNEL // Benchmark comparison is needed to determine whether this option should be enabled.
#endif

#define FULL_FP_ROUNDING
#define ACCURATE_SQRT

// Mask definition
typedef sVInt32_t VMask;
typedef svbool_t VOpMask;

// Single precision definitions
typedef sVFloat32_t VFloat;
typedef sVInt32_t VInt2;

// Double precision definitions
typedef sVFloat64_t VDouble;
typedef sVInt32_t VInt;

// masking predicates
#define ALL_TRUE_MASK svdup_n_s32(0xffffffff)
#define ALL_FALSE_MASK svdup_n_s32(0x0)

#[inline]
fn vprefetch_v_p(const void *ptr) -> void {}

//
//
//
// Test if all lanes are active
//
//
//
#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int {
  svbool_t pg = svptrue_b32();
  return (svcntp_b32(pg, g) == svcntw());
}

#[inline]
fn vtestallones_i_vo64(g: VOpMask) -> int {
  svbool_t pg = svptrue_b64();
  return (svcntp_b64(pg, g) == svcntd());
}
//
//
//
//
//
//

// Vector load / store
#[inline]
fn vstoreu_v_p_vi2(int32_t *p, VInt2 v) -> void { svst1_s32(ptrue, p, v); }

#[inline]
fn vload_vf_p(const float *ptr) -> VFloat {
  return svld1_f32(ptrue, ptr);
}
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat {
  return svld1_f32(ptrue, ptr);
}
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void {
  svst1_f32(ptrue, ptr, v);
}

// Basic logical operations for mask
#[inline]
fn vand_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  return svand_s32_x(ptrue, x, y);
}
#[inline]
fn vandnot_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  return svbic_s32_x(ptrue, y, x);
}
#[inline]
fn vor_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  return svorr_s32_x(ptrue, x, y);
}
#[inline]
fn vxor_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  return sveor_s32_x(ptrue, x, y);
}

#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  return svreinterpret_s32_s64(
           svadd_s64_x(ptrue, svreinterpret_s64_s32(x),
                              svreinterpret_s64_s32(y)));
}

// Mask <--> single precision reinterpret
#[inline]
fn vreinterpret_vm_vf(vf: VFloat) -> VMask {
  return svreinterpret_s32_f32(vf);
}
#[inline]
fn vreinterpret_vf_vm(vm: VMask) -> VFloat {
  return svreinterpret_f32_s32(vm);
}
#[inline]
fn vreinterpret_vf_vi2(vm: VInt2) -> VFloat {
  return svreinterpret_f32_s32(vm);
}
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) -> VInt2 {
  return svreinterpret_s32_f32(vf);
}
#[inline]
fn vcast_vi2_vm(vm: VMask) -> VInt2 { return vm; }
#[inline]
fn vcast_vm_vi2(vi: VInt2) -> VMask { return vi; }

// Conditional select
#[inline]
fn vsel_vi2_vm_vi2_vi2(VMask m, VInt2 x, VInt2 y) -> VInt2 {
  return svsel_s32(svcmpeq_s32(ptrue, m, ALL_TRUE_MASK), x, y);
}

/****************************************/
/* Single precision FP operations */
/****************************************/
// Broadcast
impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        svdup_n_f32(self)
    }
}

// Add, Sub, Mul, Reciprocal 1/x, Division, Square root
#[inline]
fn vrec_vf_vf(d: VFloat) -> VFloat {
  return svdivr_n_f32_x(ptrue, d, 1.);
}
#[inline]
fn vsqrt_vf_vf(d: VFloat) -> VFloat { return svsqrt_f32_x(ptrue, d); }

// |x|, -x
#[inline]
fn vabs_vf_vf(f: VFloat) -> VFloat { return svabs_f32_x(ptrue, f); }


// int <--> float conversions
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 {
  return svcvt_s32_f32_x(ptrue, vf);
}
#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat {
  return svcvt_f32_s32_x(ptrue, vi);
}
#[inline]
fn vcast_vi2_i(i: int) -> VInt2 { return svdup_n_s32(i); }
#[inline]
fn vrint_vi2_vf(d: VFloat) -> VInt2 {
  return svcvt_s32_f32_x(ptrue, svrintn_f32_x(ptrue, d));
}

#if CONFIG == 1
// Multiply accumulate: z = z + x * y
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        svmad_f32_x(ptrue, x, y, z)
    }
}

// Multiply subtract: z = z - x * y
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat {
  return svmsb_f32_x(ptrue, x, y, z);
}
#else
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return z - x * y; }
#endif

// fused multiply add / sub
#[inline]
fn VFloat vfma_vf_vf_vf_vf(VFloat x, VFloat y,
                                      VFloat z) { // z + x * y
  return svmad_f32_x(ptrue, x, y, z);
}
#[inline]
fn VFloat vfmanp_vf_vf_vf_vf(VFloat x, VFloat y,
                                        VFloat z) { // z - x * y
  return svmsb_f32_x(ptrue, x, y, z);
}
#[inline]
fn VFloat vfmapn_vf_vf_vf_vf(VFloat x, VFloat y,
                                        VFloat z) { // x * y - z
  return svnmsb_f32_x(ptrue, x, y, z);
}

// conditional select
#[inline]
fn vsel_vf_vo_vf_vf(mask: VOpMask, x: VFloat, y: VFloat) -> VFloat {
  return svsel_f32(mask, x, y);
}

//
//
//
//
//
//
#[inline]
fn vsel_vf_vo_f_f(o: VOpMask, v1: float, v0: float) -> CONST -> VFloat {
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
//
//
//
//
//
//

// truncate
#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat {
  return svrintz_f32_x(ptrue, vd);
}

//
//
//
// Round float
//
//
//
#[inline]
fn vrint_vf_vf(vf: VFloat) -> VFloat {
  return svrintn_f32_x(svptrue_b32(), vf);
}
//
//
//
//
//
//

/***************************************/
/* Single precision integer operations */
/***************************************/

// Add, Sub, Neg (-x)
#[inline]
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return svadd_s32_x(ptrue, x, y);
}
#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return svsub_s32_x(ptrue, x, y);
}
#[inline]
fn vneg_vi2_vi2(e: VInt2) -> VInt2 { return svneg_s32_x(ptrue, e); }

// Logical operations
#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return svand_s32_x(ptrue, x, y);
}
#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return svbic_s32_x(ptrue, y, x);
}
#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return svorr_s32_x(ptrue, x, y);
}
#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return sveor_s32_x(ptrue, x, y);
}

// Shifts
#define vsll_vi2_vi2_i(x, c) svlsl_n_s32_x(ptrue, x, c)
#define vsrl_vi2_vi2_i(x, c)                                                   \
  svreinterpret_s32_u32(svlsr_n_u32_x(ptrue, svreinterpret_u32_s32(x), c))

#define vsra_vi2_vi2_i(x, c) svasr_n_s32_x(ptrue, x, c)

// Comparison returning integers
#[inline]
fn vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return svsel_s32(svcmpge_s32(ptrue, x, y), ALL_TRUE_MASK, ALL_FALSE_MASK);
}

// conditional select
#[inline]
fn vsel_vi2_vo_vi2_vi2(m: VOpMask, x: VInt2, y: VInt2) -> VInt2 {
  return svsel_s32(m, x, y);
}


#[inline]
fn visinf_vo_vf(d: VFloat) -> VOpMask {
  return svcmpeq_n_f32(ptrue, vabs_vf_vf(d), SLEEF_INFINITYf);
}
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask {
  return svcmpeq_n_f32(ptrue, d, SLEEF_INFINITYf);
}
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask {
  return svcmpeq_n_f32(ptrue, d, -SLEEF_INFINITYf);
}
#[inline]
fn visnan_vo_vf(d: VFloat) -> VOpMask { return d.ne(d); }

// integers
#[inline]
fn veq_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask {
  return svcmpeq_s32(ptrue, x, y);
}
#[inline]
fn vgt_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask {
  return svcmpgt_s32(ptrue, x, y);
}

// logical opmask
//#[inline]
//fn vand_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask {
//  return svand_b_z(ptrue, x, y);
//}
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask {
  return svbic_b_z(ptrue, y, x);
}
//#[inline]
//fn vor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask {
//  return svorr_b_z(ptrue, x, y);
//}
//#[inline]
//fn vxor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask {
//  return sveor_b_z(ptrue, x, y);
//}

#[inline]
fn vand_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 {
  // This needs to be zeroing to prevent asinf and atanf denormal test
  // failing.
  return svand_s32_z(x, y, y);
}

// bitmask logical operations
#[inline]
fn vand_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask {
  return svsel_s32(x, y, ALL_FALSE_MASK);
}
#[inline]
fn vandnot_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask {
  return svsel_s32(x, ALL_FALSE_MASK, y);
}
#[inline]
fn vor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask {
  return svsel_s32(x, ALL_TRUE_MASK, y);
}

// broadcast bitmask
#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> VMask {
  return svreinterpret_s32_u64(
      svdup_n_u64((0xffffffff & (uint64_t)i1) | (((uint64_t)i0) << 32)));
}

/*********************************/
/* SVE for double precision math */
/*********************************/

// Vector load/store
#[inline]
fn vload_vd_p(const double *ptr) -> VDouble {
  return svld1_f64(ptrue, ptr);
}
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble {
  return svld1_f64(ptrue, ptr);
}
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void {
  svst1_f64(ptrue, ptr, v);
}

#[inline]
fn vstoreu_v_p_vi(int *ptr, VInt v) -> void {
  svst1w_s64(ptrue, ptr, svreinterpret_s64_s32(v));
}
static VInt vloadu_vi_p(int32_t *p) {
  return svreinterpret_s32_s64(svld1uw_s64(ptrue, (uint32_t *)p));
}

// Reinterpret
#[inline]
fn vreinterpret_vd_vm(vm: VMask) -> VDouble {
  return svreinterpret_f64_s32(vm);
}
#[inline]
fn vreinterpret_vm_vd(vd: VDouble) -> VMask {
  return svreinterpret_s32_f64(vd);
}
#[inline]
fn vreinterpret_vd_vi2(x: VInt2) -> VDouble {
  return svreinterpret_f64_s32(x);
}
#[inline]
fn vreinterpret_vi2_vd(x: VDouble) -> VInt2 {
  return svreinterpret_s32_f64(x);
}
impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        svreinterpret_s32_s64(
      svlsl_n_s64_x(ptrue, svreinterpret_s64_s32(x), 32))
    }
}
//#[inline]
//fn vcastu_vi2_vi(x: VInt) -> VInt2 {
//  return svreinterpret_s32_s64(
//      svlsl_n_s64_x(ptrue, svreinterpret_s64_s32(x), 32));
//}
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
      svreinterpret_s32_s64(
      svlsr_n_s64_x(ptrue, svreinterpret_s64_s32(x), 32))
    }
}
//#[inline]
//fn vcastu_vi_vi2(x: VInt2) -> VInt {
//  return svreinterpret_s32_s64(
//      svlsr_n_s64_x(ptrue, svreinterpret_s64_s32(x), 32));
//}
#[inline]
fn vcast_vd_vi(vi: VInt) -> VDouble {
  return svcvt_f64_s32_x(ptrue, vi);
}

// Splat
impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        svdup_n_f64(d)
    }
}

// Conditional select
#[inline]
fn vsel_vd_vo_vd_vd(o: VOpMask, x: VDouble, y: VDouble) -> VDouble {
  return svsel_f64(o, x, y);
}

#[inline]
fn vsel_vd_vo_d_d(o: VOpMask, v1: double, v0: double) -> CONST -> VDouble {
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
fn vsel_vi_vo_vi_vi(o: VOpMask, x: VInt, y: VInt) -> VInt {
  return svsel_s32(o, x, y);
}
// truncate
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble {
  return svrintz_f64_x(ptrue, vd);
}
#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt {
  return svcvt_s32_f64_x(ptrue, vd);
}
#[inline]
fn vrint_vi_vd(vd: VDouble) -> VInt {
  return svcvt_s32_f64_x(ptrue, svrintn_f64_x(ptrue, vd));
}
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble {
  return svrintn_f64_x(ptrue, vd);
}

// FP math operations


#[inline]
fn vrec_vd_vd(x: VDouble) -> VDouble {
  return svdivr_n_f64_x(ptrue, x, 1.0);
}
#[inline]
fn vsqrt_vd_vd(x: VDouble) -> VDouble { return svsqrt_f64_x(ptrue, x); }
#[inline]
fn vabs_vd_vd(x: VDouble) -> VDouble { return svabs_f64_x(ptrue, x); }


#if CONFIG == 1
// Multiply accumulate / subtract
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        svmad_f64_x(ptrue, x, y, z)
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { // z = x * y - z
  return svnmsb_f64_x(ptrue, x, y, z);
}
#else
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        x*y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return x*y - z; }
#endif

#[inline]
fn vfma_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { // z + x * y
  return svmad_f64_x(ptrue, x, y, z);
}
#[inline]
fn vfmanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble{ // z - x * y
  return svmsb_f64_x(ptrue, x, y, z);
}
#[inline]
fn vfmapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble{ // x * y - z
  return svnmsb_f64_x(ptrue, x, y, z);
}


// predicates
#[inline]
fn visnan_vo_vd(vd: VDouble) -> VOpMask {
  return svcmpne_f64(ptrue, vd, vd);
}
#[inline]
fn visinf_vo_vd(vd: VDouble) -> VOpMask {
  return svcmpeq_n_f64(ptrue, svabs_f64_x(ptrue, vd), SLEEF_INFINITY);
}
#[inline]
fn vispinf_vo_vd(vd: VDouble) -> VOpMask {
  return svcmpeq_n_f64(ptrue, vd, SLEEF_INFINITY);
}
#[inline]
fn visminf_vo_vd(vd: VDouble) -> VOpMask {
  return svcmpeq_n_f64(ptrue, vd, -SLEEF_INFINITY);
}

// Comparing bit masks
#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask {
  return svcmpeq_s64(ptrue, svreinterpret_s64_s32(x), svreinterpret_s64_s32(y));
}

// pure predicate operations
#[inline]
fn vcast_vo32_vo64(o: VOpMask) -> VOpMask { return o; }
#[inline]
fn vcast_vo64_vo32(o: VOpMask) -> VOpMask { return o; }

// logical integer operations
#[inline]
fn vand_vi_vo_vi(x: VOpMask, y: VInt) -> VInt {
  // This needs to be a zeroing instruction because we need to make
  // sure that the inactive elements for the unpacked integers vector
  // are zero.
  return svand_s32_z(x, y, y);
}

#[inline]
fn vandnot_vi_vo_vi(x: VOpMask, y: VInt) -> VInt {
  return svsel_s32(x, ALL_FALSE_MASK, y);
}
#define vsra_vi_vi_i(x, c) svasr_n_s32_x(ptrue, x, c)
#define vsll_vi_vi_i(x, c) svlsl_n_s32_x(ptrue, x, c)
#define vsrl_vi_vi_i(x, c) svlsr_n_s32_x(ptrue, x, c)

#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt) -> VInt {
  return svand_s32_x(ptrue, x, y);
}
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt {
  return svbic_s32_x(ptrue, y, x);
}
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt) -> VInt {
  return sveor_s32_x(ptrue, x, y);
}

// integer math
impl std::ops::Add for VInt {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        svadd_s32_x(ptrue, self, other)
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt {
//  return svadd_s32_x(ptrue, x, y);
//}
impl std::ops::Sub for VInt {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        svsub_s32_x(ptrue, self, other)
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt {
//  return svsub_s32_x(ptrue, x, y);
//}
impl std::ops::Neg for VInt {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        -svneg_s32_x(ptrue, self)
    }
}
//#[inline]
//fn vneg_vi_vi(x: VInt) -> VInt { return svneg_s32_x(ptrue, x); }

// integer comparison
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask {
  return svcmpgt_s32(ptrue, x, y);
}
#[inline]
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask {
  return svcmpeq_s32(ptrue, x, y);
}

// Splat
impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        svdup_n_s32(self)
    }
}
//#[inline]
//fn vcast_vi_i(i: int) -> VInt { return svdup_n_s32(i); }

// bitmask logical operations
#[inline]
fn vand_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask {
  // This needs to be a zeroing instruction because we need to make
  // sure that the inactive elements for the unpacked integers vector
  // are zero.
  return svreinterpret_s32_s64(
      svand_s64_z(x, svreinterpret_s64_s32(y), svreinterpret_s64_s32(y)));
}
#[inline]
fn vandnot_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask {
  return svreinterpret_s32_s64(svsel_s64(
      x, svreinterpret_s64_s32(ALL_FALSE_MASK), svreinterpret_s64_s32(y)));
}
#[inline]
fn vor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask {
  return svreinterpret_s32_s64(svsel_s64(
      x, svreinterpret_s64_s32(ALL_TRUE_MASK), svreinterpret_s64_s32(y)));
}

#[inline]
fn vrev21_vf_vf(vf: VFloat) -> VFloat {
  return svreinterpret_f32_u64(svrevw_u64_x(ptrue, svreinterpret_u64_f32(vf)));
}

#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

// Comparison returning integer
#[inline]
fn veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return svsel_s32(svcmpeq_s32(ptrue, x, y), ALL_TRUE_MASK, ALL_FALSE_MASK);
}

// Gather

#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble {
  return svld1_gather_s64index_f64(ptrue, ptr, svreinterpret_s64_s32(vi));
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi2) -> VFloat {
  return svld1_gather_s32index_f32(ptrue, ptr, vi2);
}

// Operations for DFT

#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble {
  return svneg_f64_m(d, svdupq_n_b64(false, true), d);
}

#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble {
  return svneg_f64_m(d, svdupq_n_b64(true, false), d);
}

#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat {
  return svneg_f32_m(d, svdupq_n_b32(false, true, false, true), d);
}

#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat {
  return svneg_f32_m(d, svdupq_n_b32(true, false, true, false), d);
}

#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x + vnegpos_vd_vd(y); }
#[inline]
fn vsubadd_vf_vf_vf(d0: VFloat, d1: VFloat) -> VFloat { return d0 + vnegpos_vf_vf(d1); }
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vfma_vd_vd_vd_vd(x, y, vnegpos_vd_vd(z)); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vfma_vf_vf_vf_vf(x, y, vnegpos_vf_vf(z)); }

//

#[inline]
fn vrev21_vd_vd(x: VDouble) -> VDouble { return svzip1_f64(svuzp2_f64(x, x), svuzp1_f64(x, x)); }

#[inline]
fn vreva2_vd_vd(vd: VDouble) -> VDouble {
  sVInt64_t x = svindex_s64((VECTLENDP-1), -1);
  x = svzip1_s64(svuzp2_s64(x, x), svuzp1_s64(x, x));
  return svtbl_f64(vd, svreinterpret_u64_s64(x));
}

#[inline]
fn vreva2_vf_vf(vf: VFloat) -> VFloat {
  sVInt32_t x = svindex_s32((VECTLENSP-1), -1);
  x = svzip1_s32(svuzp2_s32(x, x), svuzp1_s32(x, x));
  return svtbl_f32(vf, svreinterpret_u32_s32(x));
}

//

//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void {
//  svst1_scatter_u64index_f64(ptrue, ptr + offset*2, svzip1_u64(svindex_u64(0, step*2), svindex_u64(1, step*2)), v);
//}

//#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
//  svst1_scatter_u32index_f32(ptrue, ptr + offset*2, svzip1_u32(svindex_u32(0, step*2), svindex_u32(1, step*2)), v);
//}

#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { vstoreu_v_p_vd(ptr, v); }
//#[inline]
//fn vstream_v_p_vd(double *ptr, VDouble v) -> void { vstore_v_p_vd(ptr, v); }
#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { vstoreu_v_p_vf(ptr, v); }
//#[inline]
//fn vstream_v_p_vf(float *ptr, VFloat v) -> void { vstore_v_p_vf(ptr, v); }
//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { vscatter2_v_p_i_i_vd(ptr, offset, step, v); }
//#[inline]
//fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }

// These functions are for debugging
static double vcast_d_vd(v: VDouble) {
  double a[svcntd()];
  vstoreu_v_p_vd(a, v);
  return a[0];
}

static float vcast_f_vf(v: VFloat) {
  float a[svcntw()];
  vstoreu_v_p_vf(a, v);
  return a[0];
}

static int vcast_i_vi(v: VInt) {
  int a[svcntw()];
  vstoreu_v_p_vi(a, v);
  return a[0];
}

static int vcast_i_vi2(v: VInt2) {
  int a[svcntw()];
  vstoreu_v_p_vi2(a, v);
  return a[0];
}
