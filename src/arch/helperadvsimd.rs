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
type $mx = uint32x4_t;
type VopMask = uint32x4_t;

// Single precision definitions
type $f32x = float32x4_t;
type $ix32 = int32x4_t;

// Double precision definitions
type $f64x = float64x2_t;
type $ix = int32x2_t;

#define DFTPRIORITY 10

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { }

#[inline]
fn vtestallones_i_vo32(g: $mox) -> int {
  uint32x2_t x0 = vand_u32(vget_low_u32(g), vget_high_u32(g));
  uint32x2_t x1 = vpmin_u32(x0, x0);
  return vget_lane_u32(x1, 0);
}

#[inline]
fn vtestallones_i_vo64(g: $mox) -> int {
  uint32x2_t x0 = vand_u32(vget_low_u32(g), vget_high_u32(g));
  uint32x2_t x1 = vpmin_u32(x0, x0);
  return vget_lane_u32(x1, 0);
}

// Vector load / store
#[inline]
fn vstoreu_v_p_vi2(int32_t *p, $ix2 v) -> void { vst1q_s32(p, v); }
#[inline]
fn vstoreu_v_p_vi(int32_t *p, $ix v) -> void { vst1_s32(p, v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x {
  return (($f64x) { ptr[vget_lane_s32(vi, 0)], ptr[vget_lane_s32(vi, 1)]} );
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x {
  return (($f32x) {
      ptr[vgetq_lane_s32(vi2, 0)],
      ptr[vgetq_lane_s32(vi2, 1)],
      ptr[vgetq_lane_s32(vi2, 2)],
      ptr[vgetq_lane_s32(vi2, 3)]
    });
}

// Basic logical operations for mask
#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  return vbicq_u32(y, x);
}

/****************************************/
/* Single precision FP operations */
/****************************************/
// Broadcast

// Add, Sub, Mul, Reciprocal 1/x, Division, Square root
impl Rec for $f32x {
    #[inline]
    fn rec(self) -> Self {
        vdivq_f32(Self::splat(1.), self)
    }
}
#[inline]
fn vsqrt_vf_vf(d: $f32x) -> $f32x { return vsqrtq_f32(d); }

#if CONFIG == 1
// Multiply accumulate: z = z + x * y
impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        vfmaq_f32(z, x, y)
    }
}
// Multiply subtract: z = z = x * y
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x {
  return vfmsq_f32(z, x, y);
}
#else
impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return z - x * y); }
#endif

// |x|, -x
impl Abs for $f32x {
    fn abs(self) -> Self {
        vabsq_f32(f)
    }
}


// Comparisons
#[inline]
fn veq_vm_vf_vf(x: $f32x, y: $f32x) -> $mx { return vceqq_f32(x, y); }
#[inline]
fn vneq_vm_vf_vf(x: $f32x, y: $f32x) -> $mx {
  return vmvnq_u32(vceqq_f32(x, y));
}
#[inline]
fn vlt_vm_vf_vf(x: $f32x, y: $f32x) -> $mx { return vcltq_f32(x, y); }
#[inline]
fn vle_vm_vf_vf(x: $f32x, y: $f32x) -> $mx { return vcleq_f32(x, y); }
#[inline]
fn vgt_vm_vf_vf(x: $f32x, y: $f32x) -> $mx { return vcgtq_f32(x, y); }
#[inline]
fn vge_vm_vf_vf(x: $f32x, y: $f32x) -> $mx { return vcgeq_f32(x, y); }

// Conditional select
#[inline]
fn vsel_vf_vm_vf_vf($mx mask, $f32x x, $f32x y) -> $f32x {
  return vbslq_f32(mask, x, y);
}

// int <--> float conversions
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return vcvtq_s32_f32(vf); }

#[inline]
fn vrint_vi2_vf(d: $f32x) -> $ix2 {
  return vcvtq_s32_f32(vrndnq_f32(d));
}

/***************************************/
/* Single precision integer operations */
/***************************************/


// Logical operations

#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return vbicq_s32(y, x);
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
fn veq_vm_vi2_vi2($ix2 x, $ix2 y) -> $mx { return vceqq_s32(x, y); }
#[inline]
fn vgt_vm_vi2_vi2($ix2 x, $ix2 y) -> $mx { return vcgeq_s32(x, y); }
// Comparison returning integers
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return vreinterpretq_s32_u32(vcgeq_s32(x, y));
}
#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return vreinterpretq_s32_u32(vceqq_s32(x, y));
}

// Conditional select
#[inline]
fn vsel_vi2_vm_vi2_vi2($mx m, $ix2 x, $ix2 y) -> $ix2 {
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


// Add, Sub, Mul, Reciprocal 1/x, Division, Square root

impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        vdivq_f64($f64x::splat(1.), self)
    }
}

#[inline]
fn vsqrt_vd_vd(d: $f64x) -> $f64x { return vsqrtq_f64(d); }

// |x|, -x
impl Abs for $f64x {
    fn abs(self) -> Self {
        vabsq_f64(f)
    }
}



#if CONFIG == 1
// Multiply accumulate: z = z + x * y
impl Mla for $f64x {
    fn mla(self, y: Self, z: Self) -> Self {
        vfmaq_f64(z, x, y)
    }
}

#[inline]
fn vmlanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x {
  return vfmsq_f64(z, x, y);
}

//[z = x * y - z]
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x {
  return -vfmsq_f64(z, x, y);
}
#else
impl Mla for $f64x {
    fn mla(self, y: Self, z: Self) -> Self {
        self*y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x*y- z; }
#endif

#[inline]
fn vfma_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { // z + x * y
  return vfmaq_f64(z, x, y);
}

#[inline]
fn vfmanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { // z - x * y
  return vfmsq_f64(z, x, y);
}

#[inline]
fn vfmapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { // x * y - z
  return -(vfmanp_vd_vd_vd_vd(x, y, z);
}

#[inline]
fn vfma_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { // z + x * y
  return vfmaq_f32(z, x, y);
}

#[inline]
fn vfmanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { // z - x * y
  return vfmsq_f32(z, x, y);
}

#[inline]
fn vfmapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { // x * y - z
  return -vfmanp_vf_vf_vf_vf(x, y, z);
}


// Conditional select
#[inline]
fn vsel_vd_vo_vd_vd(mask: $mox, x: $f64x, y: $f64x) -> $f64x {
  return vbslq_f64(vreinterpretq_u64_u32(mask), x, y);
}

#if 1
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
#else
// This implementation is slower on the current CPU models (as of May 2017.)
// I(Naoki Shibata) expect that on future CPU models with hardware similar to Super Shuffle Engine, this implementation will be faster.
#[inline]
fn vsel_vd_vo_d_d(o: $mox, d0: f64, d1: f64) -> CONST -> $f64x {
  uint8x16_t idx = vbslq_u8(vreinterpretq_u8_u32(o), (uint8x16_t) { 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7 },
			    (uint8x16_t) { 8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15 });
  
  uint8x16_t tab = (uint8x16_t) (float64x2_t) { d0, d1 };
  return ($f64x) vqtbl1q_u8(tab, idx);
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $mox, o1: $mox, o2: $mox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  uint8x16_t idx = vbslq_u8(vreinterpretq_u8_u32(o0), (uint8x16_t) { 0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5, 6, 7 },
			    vbslq_u8(vreinterpretq_u8_u32(o1), (uint8x16_t) { 8, 9, 10, 11, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15 },
				     vbslq_u8(vreinterpretq_u8_u32(o2), (uint8x16_t) { 16, 17, 18, 19, 20, 21, 22, 23, 16, 17, 18, 19, 20, 21, 22, 23 },
					      (uint8x16_t) { 24, 25, 26, 27, 28, 29, 30, 31, 24, 25, 26, 27, 28, 29, 30, 31 })));
  
  uint8x16x2_t tab = { { (uint8x16_t) (float64x2_t) { d0, d1 }, (uint8x16_t) (float64x2_t) { d2, d3 } } }; 
  return ($f64x) vqtbl2q_u8(tab, idx);
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $mox, o1: $mox, d0: f64, d1: f64, d2: f64) -> $f64x {
  return vsel_vd_vo_vo_vo_d_d_d_d(o0, o1, o1, d0, d1, d2, d2);
}
#endif

#[inline]
fn vrint_vd_vd(d: $f64x) -> $f64x { return vrndnq_f64(d); }
#[inline]
fn vrint_vf_vf(d: $f32x) -> $f32x { return vrndnq_f32(d); }

/****************************************/
/* int <--> float conversions           */
/****************************************/
#[inline]
fn vtruncate_vi_vd(vf: $f64x) -> $ix {
  return vmovn_s64(vcvtq_s64_f64(vf));
}

#[inline]
fn vrint_vi_vd(d: $f64x) -> $ix {
  return vqmovn_s64(vcvtq_s64_f64(vrndnq_f64(d)));
}

/***************************************/
/* Integer operations */
/***************************************/

// Add, Sub, Neg (-x)

// Logical operations
#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vbic_s32(y, x); }

// Comparison returning masks


// Conditional select
#[inline]
fn vsel_vi_vm_vi_vi($mx m, $ix x, $ix y) -> $ix {
  return vbsl_s32(vget_low_u32(m), x, y);
}

/***************************************/
/* Predicates                          */
/***************************************/
#[inline]
fn visinf_vo_vd(d: $f64x) -> $mox {
  const float64x2_t inf = vdupq_n_f64(SLEEF_INFINITY);
  const float64x2_t neg_inf = vdupq_n_f64(-SLEEF_INFINITY);
  uint64x2_t cmp = vorrq_u64(vceqq_f64(d, inf), vceqq_f64(d, neg_inf));
  return vreinterpretq_u32_u64(cmp);
}

#[inline]
fn visnan_vo_vd(d: $f64x) -> $mox {
  return vmvnq_u32(vreinterpretq_u32_u64(vceqq_f64(d, d)));
}

#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox {
  return vreinterpretq_u32_u64(vceqq_f64(d, vdupq_n_f64(SLEEF_INFINITY)));
}

#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox {
  return vreinterpretq_u32_u64(vceqq_f64(d, vdupq_n_f64(-SLEEF_INFINITY)));
}

#[inline]
fn vsel_vf_vo_vf_vf(mask: $mox, x: $f32x, y: $f32x) -> $f32x {
  return vbslq_f32(mask, x, y);
}

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
fn visinf_vo_vf(d: $f32x) -> $mox {
  return d.abs().ne($f32x::splat(SLEEF_INFINITY_F));
}
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox {
  return d.ne($f32x::splat(SLEEF_INFINITY_F));
}
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox {
  return d.ne($f32x::splat(-SLEEF_INFINITY_F));
}
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return d.ne(d); }

#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox {
  return vuzpq_u32(m, m).val[0];
}
#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox {
  return vzipq_u32(m, m).val[0];
}

#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox {
  return vbicq_u32(y, x);
}

#[inline]
fn vsel_vi2_vo_vi2_vi2(m: $mox, x: $ix2, y: $ix2) -> $ix2 {
  return vbslq_s32(m, x, y);
}
#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 {
  return vandq_s32(vreinterpretq_s32_u32(x), y);
}
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 {
  return vbicq_s32(y, vreinterpretq_s32_u32(x));
}
#[inline]
fn vandnot_vi_vo_vi(x: $mox, y: $ix) -> $ix {
  return vbic_s32(y, vget_low_s32(vreinterpretq_s32_u32(x)));
}
#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $mx) -> $mx {
  return vandq_u32(x, y);
}
#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $mx) -> $mx {
  return vandq_u32(x, y);
}
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $mx) -> $mx {
  return vbicq_u32(y, x);
}
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $mx) -> $mx {
  return vbicq_u32(y, x);
}
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $mx) -> $mx {
  return vorrq_u32(x, y);
}
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $mx) -> $mx {
  return vorrq_u32(x, y);
}
#[inline]
fn vxor_vm_vo32_vm(x: $mox, y: $mx) -> $mx {
  return veorq_u32(x, y);
}

#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x { return vrndq_f32(vd); }

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $mx {
  return vreinterpretq_u32_u64(vdupq_n_u64((0xffffffff & (i1 as u64)) | (((i0 as u64)) << 32)));
}

#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox {
  return vreinterpretq_u32_u64(vceqq_s64(vreinterpretq_s64_u32(x), vreinterpretq_s64_u32(y)));
}

#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  return vreinterpretq_u32_s64(vaddq_s64(vreinterpretq_s64_u32(x), vreinterpretq_s64_u32(y)));
}

#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix {
  return vbsl_s32(vget_low_u32(m), x, y);
}

// Logical operations
#[inline]
fn vand_vi_vo_vi(x: $mox, y: $ix) -> $ix {
  return vand_s32(vreinterpret_s32_u32(vget_low_u32(x)), y);
}



#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return vrndq_f64(vd); }

//

#define PNMASK (($f64x) { +0.0, -0.0 })
#define NPMASK (($f64x) { -0.0, +0.0 })
#define PNMASKf (($f32x) { +0., -0., +0., -0. })
#define NPMASKf (($f32x) { -0., +0., -0., +0. })

#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return $f64x::from(($mx::from(d) ^ $mx::from(PNMASK))); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return $f64x::from(($mx::from(d) ^ $mx::from(NPMASK))); }
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return ($f32x)(($mx)d ^ ($mx)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return ($f32x)(($mx)d ^ ($mx)NPMASKf); }

#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return x + vnegpos_vd_vd(y); }
#[inline]
fn vsubadd_vf_vf_vf(d0: $f32x, d1: $f32x) -> $f32x { return d0 + vnegpos_vf_vf(d1); }
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vsubadd_vd_vd_vd(x*y, z); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vsubadd_vf_vf_vf(x * y, z); }


#[inline]
fn vrev21_vf_vf(d0: $f32x) -> $f32x { return vrev64q_f32(d0); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf($f32x::from(i))); }
