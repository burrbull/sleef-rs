/*********************************************************************/
/*          Copyright ARM Ltd. 2010 - 2017.                          */
/* Distributed under the Boost Software License, Version 1.0.        */
/*    (See accompanying file LICENSE.txt or copy at                  */
/*          http://www.boost.org/LICENSE_1_0.txt)                    */
/*********************************************************************/

#ifndef __ARM_NEON
#error Please specify advsimd flags.
#endif

#include <arm_neon.h>
#include <stdint.h>

#include "misc.h"

#define ENABLE_DP
#define LOG2VECTLENDP 1
#define VECTLENDP (1 << LOG2VECTLENDP)

#define ENABLE_SP
#define LOG2VECTLENSP 2
#define VECTLENSP (1 << LOG2VECTLENSP)

#if CONFIG == 1
#define ENABLE_FMA_DP
#define ENABLE_FMA_SP
//#define SPLIT_KERNEL // Benchmark comparison is needed to determine whether this option should be enabled.
#endif

#define FULL_FP_ROUNDING
#define ACCURATE_SQRT

#define ISANAME "AArch64 AdvSIMD"

// Mask definition
type VMask = uint32x4_t;
type VopMask = uint32x4_t;

// Single precision definitions
type VFloat = float32x4_t;
type VInt32 = int32x4_t;

// Double precision definitions
type VDouble = float64x2_t;
type VInt = int32x2_t;

#define DFTPRIORITY 10

#[inline]
fn vavailability_i(name: int) -> int { return 3; }
#[inline]
fn vprefetch_v_p(const void *ptr) -> void { }

#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int {
  uint32x2_t x0 = vand_u32(vget_low_u32(g), vget_high_u32(g));
  uint32x2_t x1 = vpmin_u32(x0, x0);
  return vget_lane_u32(x1, 0);
}

#[inline]
fn vtestallones_i_vo64(g: VOpMask) -> int {
  uint32x2_t x0 = vand_u32(vget_low_u32(g), vget_high_u32(g));
  uint32x2_t x1 = vpmin_u32(x0, x0);
  return vget_lane_u32(x1, 0);
}

// Vector load / store
#[inline]
fn vload_vd_p(const double *ptr) -> VDouble { return vld1q_f64(ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble { return vld1q_f64(ptr); }
#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { vst1q_f64(ptr, v); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void { vst1q_f64(ptr, v); }
#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return vld1q_f32(ptr); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat { return vld1q_f32(ptr); }
#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { vst1q_f32(ptr, v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void { vst1q_f32(ptr, v); }
#[inline]
fn vloadu_vi2_p(int32_t *p) -> VInt2 { return vld1q_s32(p); }
#[inline]
fn vstoreu_v_p_vi2(int32_t *p, VInt2 v) -> void { vst1q_s32(p, v); }
#[inline]
fn vloadu_vi_p(int32_t *p) -> VInt { return vld1_s32(p); }
#[inline]
fn vstoreu_v_p_vi(int32_t *p, VInt v) -> void { vst1_s32(p, v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble {
  return ((VDouble) { ptr[vget_lane_s32(vi, 0)], ptr[vget_lane_s32(vi, 1)]} );
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi2) -> VFloat {
  return ((VFloat) {
      ptr[vgetq_lane_s32(vi2, 0)],
      ptr[vgetq_lane_s32(vi2, 1)],
      ptr[vgetq_lane_s32(vi2, 2)],
      ptr[vgetq_lane_s32(vi2, 3)]
    });
}

// Basic logical operations for mask
#[inline]
fn vand_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  return vbicq_u32(y, x);
}
#[inline]
fn vor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vorrq_u32(x, y); }
#[inline]
fn vxor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return veorq_u32(x, y); }

// Mask <--> single precision reinterpret
#[inline]
fn vreinterpret_vm_vf(vf: VFloat) -> VMask {
  return vreinterpretq_u32_f32(vf);
}
#[inline]
fn vreinterpret_vf_vm(vm: VMask) -> VFloat {
  return vreinterpretq_f32_u32(vm);
}

impl VCastI2 for VMask {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        vreinterpretq_s32_u32(vm)
    }
}
//#[inline]
//fn vcast_vi2_vm(vm: VMask) -> VInt2 { return vreinterpretq_s32_u32(vm); }
#[inline]
fn vcast_vm_vi2(vi: VInt2) -> VMask { return vreinterpretq_u32_s32(vi); }

// Mask <--> double precision reinterpret
#[inline]
fn vreinterpret_vm_vd(vd: VDouble) -> VMask {
  return vreinterpretq_u32_f64(vd);
}
#[inline]
fn vreinterpret_vd_vm(vm: VMask) -> VDouble {
  return vreinterpretq_f64_u32(vm);
}
#[inline]
fn vreinterpret_vf_vi2(vm: VInt2) -> VFloat {
  return vreinterpretq_f32_s32(vm);
}
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) -> VInt2 {
  return vreinterpretq_s32_f32(vf);
}
#[inline]
fn vreinterpret_vi2_vd(vd: VDouble) -> VInt2 {
  return vreinterpretq_s32_f64(vd);
}

/****************************************/
/* Single precision FP operations */
/****************************************/
// Broadcast

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        vdupq_n_f32(self)
    }
}

// Add, Sub, Mul, Reciprocal 1/x, Division, Square root
#[inline]
fn vrec_vf_vf(d: VFloat) -> VFloat {
  return vdivq_f32((1.).as_vf(), d);
}
#[inline]
fn vsqrt_vf_vf(d: VFloat) -> VFloat { return vsqrtq_f32(d); }

#if CONFIG == 1
// Multiply accumulate: z = z + x * y
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        vfmaq_f32(z, x, y)
    }
}
// Multiply subtract: z = z = x * y
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat {
  return vfmsq_f32(z, x, y);
}
#else
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return z - x * y); }
#endif

// |x|, -x
#[inline]
fn vabs_vf_vf(f: VFloat) -> VFloat { return vabsq_f32(f); }


// Comparisons
#[inline]
fn veq_vm_vf_vf(x: VFloat, y: VFloat) -> VMask { return vceqq_f32(x, y); }
#[inline]
fn vneq_vm_vf_vf(x: VFloat, y: VFloat) -> VMask {
  return vmvnq_u32(vceqq_f32(x, y));
}
#[inline]
fn vlt_vm_vf_vf(x: VFloat, y: VFloat) -> VMask { return vcltq_f32(x, y); }
#[inline]
fn vle_vm_vf_vf(x: VFloat, y: VFloat) -> VMask { return vcleq_f32(x, y); }
#[inline]
fn vgt_vm_vf_vf(x: VFloat, y: VFloat) -> VMask { return vcgtq_f32(x, y); }
#[inline]
fn vge_vm_vf_vf(x: VFloat, y: VFloat) -> VMask { return vcgeq_f32(x, y); }

// Conditional select
#[inline]
fn vsel_vf_vm_vf_vf(VMask mask, VFloat x, VFloat y) -> VFloat {
  return vbslq_f32(mask, x, y);
}

// int <--> float conversions
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { return vcvtq_s32_f32(vf); }
#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat { return vcvtq_f32_s32(vi); }

impl VCastI2 for isize {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        vdupq_n_s32(i)
    }
}
//#[inline]
//fn vcast_vi2_i(i: int) -> VInt2 { return vdupq_n_s32(i); }
#[inline]
fn vrint_vi2_vf(d: VFloat) -> VInt2 {
  return vcvtq_s32_f32(vrndnq_f32(d));
}

/***************************************/
/* Single precision integer operations */
/***************************************/

// Add, Sub, Neg (-x)
#[inline]
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return vaddq_s32(x, y);
}
#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return vsubq_s32(x, y);
}
#[inline]
fn vneg_vi2_vi2(e: VInt2) -> VInt2 { return vnegq_s32(e); }

// Logical operations
#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return vandq_s32(x, y);
}
#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return vbicq_s32(y, x);
}
#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return vorrq_s32(x, y);
}
#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return veorq_s32(x, y);
}

// Shifts
#define vsll_vi2_vi2_i(x, c) vshlq_n_s32(x, c)
#define vsrl_vi2_vi2_i(x, c)                                                   \
  vreinterpretq_s32_u32(vshrq_n_u32(vreinterpretq_u32_s32(x), c))

#define vsra_vi2_vi2_i(x, c) vshrq_n_s32(x, c)
#define vsra_vi_vi_i(x, c) vshr_n_s32(x, c)
#define vsll_vi_vi_i(x, c) vshl_n_s32(x, c)
#define vsrl_vi_vi_i(x, c)                                                     \
  vreinterpret_s32_u32(vshr_n_u32(vreinterpret_u32_s32(x), c))

// Comparison returning masks
#[inline]
fn veq_vm_vi2_vi2(VInt2 x, VInt2 y) -> VMask { return vceqq_s32(x, y); }
#[inline]
fn vgt_vm_vi2_vi2(VInt2 x, VInt2 y) -> VMask { return vcgeq_s32(x, y); }
// Comparison returning integers
#[inline]
fn vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return vreinterpretq_s32_u32(vcgeq_s32(x, y));
}
#[inline]
fn veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 {
  return vreinterpretq_s32_u32(vceqq_s32(x, y));
}

// Conditional select
#[inline]
fn vsel_vi2_vm_vi2_vi2(VMask m, VInt2 x, VInt2 y) -> VInt2 {
  return vbslq_s32(m, x, y);
}

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

/****************************************/
/* Double precision FP operations */
/****************************************/
// Broadcast
impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        vdupq_n_f64(f)
    }
}

// Add, Sub, Mul, Reciprocal 1/x, Division, Square root

#[inline]
fn vrec_vd_vd(d: VDouble) -> VDouble {
  return vdivq_f64((1.).as_vd(), d);
}

#[inline]
fn vsqrt_vd_vd(d: VDouble) -> VDouble { return vsqrtq_f64(d); }

// |x|, -x
#[inline]
fn vabs_vd_vd(f: VDouble) -> VDouble { return vabsq_f64(f); }



#if CONFIG == 1
// Multiply accumulate: z = z + x * y
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        vfmaq_f64(z, x, y)
    }
}

#[inline]
fn vmlanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble {
  return vfmsq_f64(z, x, y);
}

//[z = x * y - z]
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble {
  return -vfmsq_f64(z, x, y);
}
#else
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        self*y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return x*y- z; }
#endif

#[inline]
fn vfma_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { // z + x * y
  return vfmaq_f64(z, x, y);
}

#[inline]
fn vfmanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { // z - x * y
  return vfmsq_f64(z, x, y);
}

#[inline]
fn vfmapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { // x * y - z
  return -(vfmanp_vd_vd_vd_vd(x, y, z);
}

#[inline]
fn vfma_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { // z + x * y
  return vfmaq_f32(z, x, y);
}

#[inline]
fn vfmanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { // z - x * y
  return vfmsq_f32(z, x, y);
}

#[inline]
fn vfmapn_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { // x * y - z
  return -vfmanp_vf_vf_vf_vf(x, y, z);
}


// Conditional select
#[inline]
fn vsel_vd_vo_vd_vd(mask: VOpMask, x: VDouble, y: VDouble) -> VDouble {
  return vbslq_f64(vreinterpretq_u64_u32(mask), x, y);
}

#if 1
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
#else
// This implementation is slower on the current CPU models (as of May 2017.)
// I(Naoki Shibata) expect that on future CPU models with hardware similar to Super Shuffle Engine, this implementation will be faster.
#[inline]
fn vsel_vd_vo_d_d(o: VOpMask, d0: double, d1: double) -> CONST -> VDouble {
  uint8x16_t idx = vbslq_u8(vreinterpretq_u8_u32(o), (uint8x16_t) { 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7 },
			    (uint8x16_t) { 8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15 });
  
  uint8x16_t tab = (uint8x16_t) (float64x2_t) { d0, d1 };
  return (VDouble) vqtbl1q_u8(tab, idx);
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(VOpMask o0, VOpMask o1, VOpMask o2, double d0, double d1, double d2, double d3) -> VDouble {
  uint8x16_t idx = vbslq_u8(vreinterpretq_u8_u32(o0), (uint8x16_t) { 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7 },
			    vbslq_u8(vreinterpretq_u8_u32(o1), (uint8x16_t) { 8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15 },
				     vbslq_u8(vreinterpretq_u8_u32(o2), (uint8x16_t) { 16, 17, 18, 19, 20, 21, 22, 23, 16, 17, 18, 19, 20, 21, 22, 23 },
					      (uint8x16_t) { 24, 25, 26, 27, 28, 29, 30, 31, 24, 25, 26, 27, 28, 29, 30, 31 })));
  
  uint8x16x2_t tab = { { (uint8x16_t) (float64x2_t) { d0, d1 }, (uint8x16_t) (float64x2_t) { d2, d3 } } }; 
  return (VDouble) vqtbl2q_u8(tab, idx);
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(VOpMask o0, VOpMask o1, double d0, double d1, double d2) -> VDouble {
  return vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o1, d0, d1, d2, d2);
}
#endif

#[inline]
fn vrint_vd_vd(d: VDouble) -> VDouble { return vrndnq_f64(d); }
#[inline]
fn vrint_vf_vf(d: VFloat) -> VFloat { return vrndnq_f32(d); }

/****************************************/
/* int <--> float conversions           */
/****************************************/
#[inline]
fn vtruncate_vi_vd(vf: VDouble) -> VInt {
  return vmovn_s64(vcvtq_s64_f64(vf));
}
#[inline]
fn vcast_vd_vi(vi: VInt) -> VDouble {
  return vcvtq_f64_s64(vmovl_s32(vi));
}
impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        vdup_n_s32(self)
    }
}
//#[inline]
//fn vcast_vi_i(i: int) -> VInt { return vdup_n_s32(i); }
#[inline]
fn vrint_vi_vd(d: VDouble) -> VInt {
  return vqmovn_s64(vcvtq_s64_f64(vrndnq_f64(d)));
}

/***************************************/
/* Integer operations */
/***************************************/

// Add, Sub, Neg (-x)
impl std::ops::Add for VInt {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        vadd_s32(x, y)
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vadd_s32(x, y); }
impl std::ops::Sub for VInt {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        vsub_s32(x, y)
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vsub_s32(x, y); }
impl std::ops::Neg for VInt {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        vneg_s32(e)
    }
}
//#[inline]
//fn vneg_vi_vi(e: VInt) -> VInt { return vneg_s32(e); }

// Logical operations
#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vand_s32(x, y); }
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vbic_s32(y, x); }
#[inline]
fn vor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vorr_s32(x, y); }
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return veor_s32(x, y); }

// Comparison returning masks
#[inline]
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask {
  return vcombine_u32(vceq_s32(x, y), vdup_n_u32(0));
}

// Conditional select
#[inline]
fn vsel_vi_vm_vi_vi(VMask m, VInt x, VInt y) -> VInt {
  return vbsl_s32(vget_low_u32(m), x, y);
}

/***************************************/
/* Predicates                          */
/***************************************/
#[inline]
fn visinf_vo_vd(d: VDouble) -> VOpMask {
  const float64x2_t inf = vdupq_n_f64(SLEEF_INFINITY);
  const float64x2_t neg_inf = vdupq_n_f64(-SLEEF_INFINITY);
  uint64x2_t cmp = vorrq_u64(vceqq_f64(d, inf), vceqq_f64(d, neg_inf));
  return vreinterpretq_u32_u64(cmp);
}

#[inline]
fn visnan_vo_vd(d: VDouble) -> VOpMask {
  return vmvnq_u32(vreinterpretq_u32_u64(vceqq_f64(d, d)));
}

#[inline]
fn vispinf_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpretq_u32_u64(vceqq_f64(d, vdupq_n_f64(SLEEF_INFINITY)));
}

#[inline]
fn visminf_vo_vd(d: VDouble) -> VOpMask {
  return vreinterpretq_u32_u64(vceqq_f64(d, vdupq_n_f64(-SLEEF_INFINITY)));
}

#[inline]
fn vsel_vf_vo_vf_vf(mask: VOpMask, x: VFloat, y: VFloat) -> VFloat {
  return vbslq_f32(mask, x, y);
}

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


#[inline]
fn veq_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask {
  return vceqq_s32(x, y);
}
#[inline]
fn vgt_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask {
  return vcgtq_s32(x, y);
}
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask {
  return vcombine_u32(vcgt_s32(x, y), vdup_n_u32(0));
}
#[inline]
fn visinf_vo_vf(d: VFloat) -> VOpMask {
  return vabs_vf_vf(d).ne(SLEEF_INFINITY_F.as_vf());
}
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask {
  return d.ne(SLEEF_INFINITY_F.as_vf());
}
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask {
  return d.ne((-SLEEF_INFINITY_F).as_vf());
}
#[inline]
fn visnan_vo_vf(d: VFloat) -> VOpMask { return d.ne(d); }

#[inline]
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask {
  return vuzpq_u32(m, m).val[0];
}
#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask {
  return vzipq_u32(m, m).val[0];
}

//#[inline]
//fn vand_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask {
//  return vandq_u32(x, y);
//}
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask {
  return vbicq_u32(y, x);
}
//#[inline]
//fn vor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask {
//  return vorrq_u32(x, y);
//}
//#[inline]
//fn vxor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask {
//  return veorq_u32(x, y);
//}

#[inline]
fn vsel_vi2_vo_vi2_vi2(m: VOpMask, x: VInt2, y: VInt2) -> VInt2 {
  return vbslq_s32(m, x, y);
}
#[inline]
fn vand_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 {
  return vandq_s32(vreinterpretq_s32_u32(x), y);
}
#[inline]
fn vandnot_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 {
  return vbicq_s32(y, vreinterpretq_s32_u32(x));
}
#[inline]
fn vandnot_vi_vo_vi(x: VOpMask, y: VInt) -> VInt {
  return vbic_s32(y, vget_low_s32(vreinterpretq_s32_u32(x)));
}
#[inline]
fn vand_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask {
  return vandq_u32(x, y);
}
#[inline]
fn vand_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask {
  return vandq_u32(x, y);
}
#[inline]
fn vandnot_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask {
  return vbicq_u32(y, x);
}
#[inline]
fn vandnot_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask {
  return vbicq_u32(y, x);
}
#[inline]
fn vor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask {
  return vorrq_u32(x, y);
}
#[inline]
fn vor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask {
  return vorrq_u32(x, y);
}
#[inline]
fn vxor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask {
  return veorq_u32(x, y);
}

#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat { return vrndq_f32(vd); }

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> VMask {
  return vreinterpretq_u32_u64(vdupq_n_u64((0xffffffff & (i1 as u64)) | (((i0 as u64)) << 32)));
}

#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask {
  return vreinterpretq_u32_u64(vceqq_s64(vreinterpretq_s64_u32(x), vreinterpretq_s64_u32(y)));
}

#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  return vreinterpretq_u32_s64(vaddq_s64(vreinterpretq_s64_u32(x), vreinterpretq_s64_u32(y)));
}

#[inline]
fn vsel_vi_vo_vi_vi(m: VOpMask, x: VInt, y: VInt) -> VInt {
  return vbsl_s32(vget_low_u32(m), x, y);
}

// Logical operations
#[inline]
fn vand_vi_vo_vi(x: VOpMask, y: VInt) -> VInt {
  return vand_s32(vreinterpret_s32_u32(vget_low_u32(x)), y);
}

impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        vreinterpretq_s32_u32(vrev64q_u32(vreinterpretq_u32_u64(vmovl_u32(vreinterpret_u32_s32(vi)))))
    }
}
//#[inline]
//fn vcastu_vi2_vi(vi: VInt) -> VInt2 {
//  return vreinterpretq_s32_u32(vrev64q_u32(vreinterpretq_u32_u64(vmovl_u32(vreinterpret_u32_s32(vi)))));
//}
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
        vreinterpret_s32_u32(vmovn_u64(vreinterpretq_u64_u32(vrev64q_u32(vreinterpretq_u32_s32(vi2)))))
    }
}
//#[inline]
//fn vcastu_vi_vi2(vi2: VInt2) -> VInt {
//  return vreinterpret_s32_u32(vmovn_u64(vreinterpretq_u64_u32(vrev64q_u32(vreinterpretq_u32_s32(vi2)))));
//}
#[inline]
fn vreinterpret_vd_vi2(vi: VInt2) -> VDouble {
  return vreinterpretq_f64_s32(vi);
}
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return vrndq_f64(vd); }

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
fn vposneg_vf_vf(d: VFloat) -> VFloat { return (VFloat)vxor_vm_vm_vm((VMask)d, (VMask)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat { return (VFloat)vxor_vm_vm_vm((VMask)d, (VMask)NPMASKf); }

#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x + vnegpos_vd_vd(y); }
#[inline]
fn vsubadd_vf_vf_vf(d0: VFloat, d1: VFloat) -> VFloat { return d0 + vnegpos_vf_vf(d1); }
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vsubadd_vd_vd_vd(x*y, z); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vsubadd_vf_vf_vf(x * y, z); }

#[inline]
fn vrev21_vd_vd(d0: VDouble) -> VDouble { return (float64x2_t)vcombine_u64(vget_high_u64((uint64x2_t)d0), vget_low_u64((uint64x2_t)d0)); }
#[inline]
fn vreva2_vd_vd(vd: VDouble) -> VDouble { return vd; }

//#[inline]
//fn vstream_v_p_vd(double *ptr, VDouble v) -> void { vstore_v_p_vd(ptr, v); }
//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { vstore_v_p_vd((double *)(&ptr[2*offset]), v); }
//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { vstore_v_p_vd((double *)(&ptr[2*offset]), v); }

#[inline]
fn vrev21_vf_vf(d0: VFloat) -> VFloat { return vrev64q_f32(d0); }
#[inline]
fn vreva2_vf_vf(d0: VFloat) -> VFloat { return vcombine_f32(vget_high_f32(d0), vget_low_f32(d0)); }
#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

//#[inline]
//fn vstream_v_p_vf(float *ptr, VFloat v) -> void { vstore_v_p_vf(ptr, v); }

//#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
//  vst1_f32((float *)(ptr+(offset + step * 0)*2), vget_low_f32(v));
//  vst1_f32((float *)(ptr+(offset + step * 1)*2), vget_high_f32(v));
//}

#[inline]
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
  vst1_f32((float *)(ptr+(offset + step * 0)*2), vget_low_f32(v));
  vst1_f32((float *)(ptr+(offset + step * 1)*2), vget_high_f32(v));
}
