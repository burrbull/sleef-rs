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
typedef svint32_t $bx;
typedef svbool_t $mox;

// Single precision definitions
typedef svfloat32_t $f32x;
typedef svint32_t $ix2;

// Double precision definitions
typedef svfloat64_t $f64x;
typedef svint32_t $ix;

// masking predicates
#define ALL_TRUE_MASK svdup_n_s32(0xffffffff)
#define ALL_FALSE_MASK svdup_n_s32(0x0)

//
//
//
// Test if all lanes are active
//
//
//
#[inline]
fn vtestallones_i_vo32(g: $mox) -> int {
  svbool_t pg = svptrue_b32();
  return (svcntp_b32(pg, g) == svcntw());
}

#[inline]
fn vtestallones_i_vo64(g: $mox) -> int {
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
fn vstoreu_v_p_vi2(int32_t *p, $ix2 v) -> void { svst1_s32(ptrue, p, v); }

// Basic logical operations for mask
#[inline]
fn vandnot_vm_vm_vm(x: $bx, y: $bx) -> $bx {
  return svbic_s32_x(ptrue, y, x);
}

#[inline]
fn vadd64_vm_vm_vm(x: $bx, y: $bx) -> $bx {
  return svreinterpret_s32_s64(
           svadd_s64_x(ptrue, svreinterpret_s64_s32(x),
                              svreinterpret_s64_s32(y)));
}


// Conditional select
#[inline]
fn vsel_vi2_vm_vi2_vi2($bx m, $ix2 x, $ix2 y) -> $ix2 {
  return svsel_s32(svcmpeq_s32(ptrue, m, ALL_TRUE_MASK), x, y);
}

/****************************************/
/* Single precision FP operations */
/****************************************/
// Broadcast

// Add, Sub, Mul, Reciprocal 1/x, Division, Square root
impl Rec for $f32x {
    #[inline]
    fn rec(self) -> Self {
        svdivr_n_f32_x(ptrue, self, 1.)
    }
}
#[inline]
fn vsqrt_vf_vf(d: $f32x) -> $f32x { return svsqrt_f32_x(ptrue, d); }

// |x|, -x
impl Abs for $f32x {
    fn abs(self) -> Self {
        svabs_f32_x(ptrue, f)
    }
}


// int <--> float conversions
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 {
  return svcvt_s32_f32_x(ptrue, vf);
}

#[inline]
fn vrint_vi2_vf(d: $f32x) -> $ix2 {
  return svcvt_s32_f32_x(ptrue, svrintn_f32_x(ptrue, d));
}

#if CONFIG == 1
// Multiply accumulate: z = z + x * y
impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        svmad_f32_x(ptrue, x, y, z)
    }
}

// Multiply subtract: z = z - x * y
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x {
  return svmsb_f32_x(ptrue, x, y, z);
}
#else
impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        x * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return z - x * y; }
#endif

// fused multiply add / sub
#[inline]
fn $f32x vfma_vf_vf_vf_vf($f32x x, $f32x y,
                                      $f32x z) { // z + x * y
  return svmad_f32_x(ptrue, x, y, z);
}
#[inline]
fn $f32x vfmanp_vf_vf_vf_vf($f32x x, $f32x y,
                                        $f32x z) { // z - x * y
  return svmsb_f32_x(ptrue, x, y, z);
}
#[inline]
fn $f32x vfmapn_vf_vf_vf_vf($f32x x, $f32x y,
                                        $f32x z) { // x * y - z
  return svnmsb_f32_x(ptrue, x, y, z);
}

// conditional select
#[inline]
fn vsel_vf_vo_vf_vf(mask: $mox, x: $f32x, y: $f32x) -> $f32x {
  return svsel_f32(mask, x, y);
}

//
//
//
//
//
//
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
//
//
//
//
//
//

// truncate
#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x {
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
fn vrint_vf_vf(vf: $f32x) -> $f32x {
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


// Logical operations

#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return svbic_s32_x(ptrue, y, x);
}

// Shifts
#define vsrl_vi2_vi2_i(x, c)                                                   \
  svreinterpret_s32_u32(svlsr_n_u32_x(ptrue, svreinterpret_u32_s32(x), c))


// Comparison returning integers
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return svsel_s32(svcmpge_s32(ptrue, x, y), ALL_TRUE_MASK, ALL_FALSE_MASK);
}

// conditional select
#[inline]
fn vsel_vi2_vo_vi2_vi2(m: $mox, x: $ix2, y: $ix2) -> $ix2 {
  return svsel_s32(m, x, y);
}


#[inline]
fn visinf_vo_vf(d: $f32x) -> $mox {
  return svcmpeq_n_f32(ptrue, d.abs(), SLEEF_INFINITYf);
}
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox {
  return svcmpeq_n_f32(ptrue, d, SLEEF_INFINITYf);
}
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox {
  return svcmpeq_n_f32(ptrue, d, -SLEEF_INFINITYf);
}
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return d.ne(d); }

// integers

// logical opmask
#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox {
  return svbic_b_z(ptrue, y, x);
}

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 {
  // This needs to be zeroing to prevent asinf and atanf denormal test
  // failing.
  return svand_s32_z(x, y, y);
}

// bitmask logical operations
#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $bx) -> $bx {
  return svsel_s32(x, y, ALL_FALSE_MASK);
}
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $bx) -> $bx {
  return svsel_s32(x, ALL_FALSE_MASK, y);
}
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $bx) -> $bx {
  return svsel_s32(x, ALL_TRUE_MASK, y);
}

// broadcast bitmask
#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $bx {
  return svreinterpret_s32_u64(
      svdup_n_u64((0xffffffff & (uint64_t)i1) | (((uint64_t)i0) << 32)));
}

/*********************************/
/* SVE for double precision math */
/*********************************/

// Vector load/store

#[inline]
fn vstoreu_v_p_vi(int *ptr, $ix v) -> void {
  svst1w_s64(ptrue, ptr, svreinterpret_s64_s32(v));
}


// Conditional select
#[inline]
fn vsel_vd_vo_vd_vd(o: $mox, x: $f64x, y: $f64x) -> $f64x {
  return svsel_f64(o, x, y);
}

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
fn vsel_vi_vo_vi_vi(o: $mox, x: $ix, y: $ix) -> $ix {
  return svsel_s32(o, x, y);
}
// truncate
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x {
  return svrintz_f64_x(ptrue, vd);
}
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix {
  return svcvt_s32_f64_x(ptrue, vd);
}
#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix {
  return svcvt_s32_f64_x(ptrue, svrintn_f64_x(ptrue, vd));
}
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x {
  return svrintn_f64_x(ptrue, vd);
}

// FP math operations


impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        svdivr_n_f64_x(ptrue, self, 1.)
    }
}
#[inline]
fn vsqrt_vd_vd(x: $f64x) -> $f64x { return svsqrt_f64_x(ptrue, x); }
impl Abs for $f64x {
    fn abs(self) -> Self {
        svabs_f64_x(ptrue, x)
    }
}


#if CONFIG == 1
// Multiply accumulate / subtract
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        svmad_f64_x(ptrue, x, y, z)
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        svnmsb_f64_x(ptrue, x, y, z)
    }
}
#else
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        x*y + z
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        x*y - z
    }
}
#endif

#[inline]
fn vfma_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { // z + x * y
  return svmad_f64_x(ptrue, x, y, z);
}
#[inline]
fn vfmanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x{ // z - x * y
  return svmsb_f64_x(ptrue, x, y, z);
}
#[inline]
fn vfmapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x{ // x * y - z
  return svnmsb_f64_x(ptrue, x, y, z);
}


// predicates
#[inline]
fn visnan_vo_vd(vd: $f64x) -> $mox {
  return svcmpne_f64(ptrue, vd, vd);
}
#[inline]
fn visinf_vo_vd(vd: $f64x) -> $mox {
  return svcmpeq_n_f64(ptrue, svabs_f64_x(ptrue, vd), SLEEF_INFINITY);
}
#[inline]
fn vispinf_vo_vd(vd: $f64x) -> $mox {
  return svcmpeq_n_f64(ptrue, vd, SLEEF_INFINITY);
}
#[inline]
fn visminf_vo_vd(vd: $f64x) -> $mox {
  return svcmpeq_n_f64(ptrue, vd, -SLEEF_INFINITY);
}

// Comparing bit masks
#[inline]
fn veq64_vo_vm_vm(x: $bx, y: $bx) -> $mox {
  return svcmpeq_s64(ptrue, svreinterpret_s64_s32(x), svreinterpret_s64_s32(y));
}

// pure predicate operations
#[inline]
fn vcast_vo32_vo64(o: $mox) -> $mox { return o; }
#[inline]
fn vcast_vo64_vo32(o: $mox) -> $mox { return o; }

// logical integer operations
#[inline]
fn vand_vi_vo_vi(x: $mox, y: $ix) -> $ix {
  // This needs to be a zeroing instruction because we need to make
  // sure that the inactive elements for the unpacked integers vector
  // are zero.
  return svand_s32_z(x, y, y);
}

#[inline]
fn vandnot_vi_vo_vi(x: $mox, y: $ix) -> $ix {
  return svsel_s32(x, ALL_FALSE_MASK, y);
}
#define vsrl_vi_vi_i(x, c) svlsr_n_s32_x(ptrue, x, c)

#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix {
  return svbic_s32_x(ptrue, y, x);
}


// bitmask logical operations
#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $bx) -> $bx {
  // This needs to be a zeroing instruction because we need to make
  // sure that the inactive elements for the unpacked integers vector
  // are zero.
  return svreinterpret_s32_s64(
      svand_s64_z(x, svreinterpret_s64_s32(y), svreinterpret_s64_s32(y)));
}
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $bx) -> $bx {
  return svreinterpret_s32_s64(svsel_s64(
      x, svreinterpret_s64_s32(ALL_FALSE_MASK), svreinterpret_s64_s32(y)));
}
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $bx) -> $bx {
  return svreinterpret_s32_s64(svsel_s64(
      x, svreinterpret_s64_s32(ALL_TRUE_MASK), svreinterpret_s64_s32(y)));
}

#[inline]
fn vrev21_vf_vf(vf: $f32x) -> $f32x {
  return svreinterpret_f32_u64(svrevw_u64_x(ptrue, svreinterpret_u64_f32(vf)));
}

#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf($f32x::from(i))); }

// Comparison returning integer
#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return svsel_s32(svcmpeq_s32(ptrue, x, y), ALL_TRUE_MASK, ALL_FALSE_MASK);
}

// Gather

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x {
  return svld1_gather_s64index_f64(ptrue, ptr, svreinterpret_s64_s32(vi));
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x {
  return svld1_gather_s32index_f32(ptrue, ptr, vi2);
}

// Operations for DFT

#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x {
  return svneg_f64_m(d, svdupq_n_b64(false, true), d);
}

#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x {
  return svneg_f64_m(d, svdupq_n_b64(true, false), d);
}

#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x {
  return svneg_f32_m(d, svdupq_n_b32(false, true, false, true), d);
}

#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x {
  return svneg_f32_m(d, svdupq_n_b32(true, false, true, false), d);
}

#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return x + vnegpos_vd_vd(y); }
#[inline]
fn vsubadd_vf_vf_vf(d0: $f32x, d1: $f32x) -> $f32x { return d0 + vnegpos_vf_vf(d1); }
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vfma_vd_vd_vd_vd(x, y, vnegpos_vd_vd(z)); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vfma_vf_vf_vf_vf(x, y, vnegpos_vf_vf(z)); }

//

//


