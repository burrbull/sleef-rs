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
typedef svint32_t $ux;
typedef svbool_t $ox;

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
fn vtestallones_i_vo32(g: $ox) -> int {
  svbool_t pg = svptrue_b32();
  return (svcntp_b32(pg, g) == svcntw());
}

#[inline]
fn vtestallones_i_vo64(g: $ox) -> int {
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
fn vandnot_vm_vm_vm(x: $ux, y: $ux) -> $ux {
  return svbic_s32_x(ptrue, y, x);
}

#[inline]
fn vadd64_vm_vm_vm(x: $ux, y: $ux) -> $ux {
  return svreinterpret_s32_s64(
           svadd_s64_x(ptrue, svreinterpret_s64_s32(x),
                              svreinterpret_s64_s32(y)));
}


// Conditional select
#[inline]
fn vsel_vi2_vm_vi2_vi2($ux m, $ix2 x, $ix2 y) -> $ix2 {
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
impl Sqrt for $f32x {
    #[inline]
    fn sqrt(self) -> Self {
        svsqrt_f32_x(ptrue, d)
    }
}

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
impl Fma for $f32x {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
        svmad_f32_x(ptrue, x, y, z)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self {
        svnmsb_f32_x(ptrue, x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        svmsb_f32_x(ptrue, x, y, z)
    }
}

// conditional select

//
#[inline]
fn vsel_vf_vo_f_f(o: $ox, v1: f32, v0: f32) -> $f32x {
  o.select($f32x::splat(v1), $f32x::splat(v0))
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: $ox, o1: $ox, d0: f32, d1: f32, d2: f32) -> $f32x {
  o0.select($f32x::splat(d0), vsel_vf_vo_f_f(o1, d1, d2))
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $ox, o1: $ox, o2: $ox, d0: f32, d1: f32, d2: f32, d3: f32) -> $f32x {
  o0.select($f32x::splat(d0), o1.select($f32x::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)))
}

//

// truncate
impl Truncate for $f32x {
    #[inline]
    fn truncate(self) -> Self {
        svrintz_f32_x(ptrue, vd)
    }
}

//
//
//
// Round float
//
//
//

impl RInt for $f32x {
    #[inline]
    fn rint(self) -> Self {
      svrintn_f32_x(svptrue_b32(), vf)
    }
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
fn visinf_vo_vf(d: $f32x) -> $ox {
  return svcmpeq_n_f32(ptrue, d.abs(), SLEEF_INFINITYf);
}
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $ox {
  return svcmpeq_n_f32(ptrue, d, SLEEF_INFINITYf);
}
#[inline]
fn visminf_vo_vf(d: $f32x) -> $ox {
  return svcmpeq_n_f32(ptrue, d, -SLEEF_INFINITYf);
}
#[inline]
fn visnan_vo_vf(d: $f32x) -> $ox { return d.ne(d); }

// integers

// logical opmask
#[inline]
fn vandnot_vo_vo_vo(x: $ox, y: $ox) -> $ox {
  return svbic_b_z(ptrue, y, x);
}

#[inline]
fn vand_vi2_vo_vi2(x: $ox, y: $ix2) -> $ix2 {
  // This needs to be zeroing to prevent asinf and atanf denormal test
  // failing.
  return svand_s32_z(x, y, y);
}

// bitmask logical operations
#[inline]
fn vand_vm_vo32_vm(x: $ox, y: $ux) -> $ux {
  return svsel_s32(x, y, ALL_FALSE_MASK);
}
#[inline]
fn vandnot_vm_vo32_vm(x: $ox, y: $ux) -> $ux {
  return svsel_s32(x, ALL_FALSE_MASK, y);
}
#[inline]
fn vor_vm_vo32_vm(x: $ox, y: $ux) -> $ux {
  return svsel_s32(x, ALL_TRUE_MASK, y);
}

// broadcast bitmask
#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $ux {
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
fn vsel_vd_vo_d_d(o: $ox, v1: f64, v0: f64) -> CONST -> $f64x {
  o.select($f64x::splat(v1), $f64x::splat(v0));
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $ox, o1: $ox, d0: f64, d1: f64, d2: f64) -> $f64x {
  o0.select($f64x::splat(d0), vsel_vd_vo_d_d(o1, d1, d2))
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $ox, o1: $ox, o2: $ox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  o0.select($f64x::splat(d0), o1.select($f64x::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)))
}

// truncate
impl Truncate for $f64x {
    #[inline]
    fn truncate(self) -> Self {
        svrintz_f64_x(ptrue, vd)
    }
}
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix {
  return svcvt_s32_f64_x(ptrue, vd);
}
#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix {
  return svcvt_s32_f64_x(ptrue, svrintn_f64_x(ptrue, vd));
}

impl RInt for $f64x {
    #[inline]
    fn rint(self) -> Self {
        svrintn_f64_x(ptrue, vd)
    }
}

// FP math operations


impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        svdivr_n_f64_x(ptrue, self, 1.)
    }
}
impl Sqrt for $f64x {
    #[inline]
    fn sqrt(self) -> Self {
        svsqrt_f64_x(ptrue, x)
    }
}
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


impl Fma for $f64x {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self { // z + x * y
      svmad_f64_x(ptrue, x, y, z)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self { // z - x * y
      svmsb_f64_x(ptrue, x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self { // x * y - z
      svnmsb_f64_x(ptrue, x, y, z)
    }
}


// predicates
#[inline]
fn visnan_vo_vd(vd: $f64x) -> $ox {
  return svcmpne_f64(ptrue, vd, vd);
}
#[inline]
fn visinf_vo_vd(vd: $f64x) -> $ox {
  return svcmpeq_n_f64(ptrue, svabs_f64_x(ptrue, vd), SLEEF_INFINITY);
}
#[inline]
fn vispinf_vo_vd(vd: $f64x) -> $ox {
  return svcmpeq_n_f64(ptrue, vd, SLEEF_INFINITY);
}

// Comparing bit masks
#[inline]
fn veq64_vo_vm_vm(x: $ux, y: $ux) -> $ox {
  return svcmpeq_s64(ptrue, svreinterpret_s64_s32(x), svreinterpret_s64_s32(y));
}

// pure predicate operations
#[inline]
fn $m32x::from(o: $ox) -> $ox { return o; }
#[inline]
fn $mx::from(o: $ox) -> $ox { return o; }

// logical integer operations
#[inline]
fn vand_vi_vo_vi(x: $ox, y: $ix) -> $ix {
  // This needs to be a zeroing instruction because we need to make
  // sure that the inactive elements for the unpacked integers vector
  // are zero.
  return svand_s32_z(x, y, y);
}

#[inline]
fn vandnot_vi_vo_vi(x: $ox, y: $ix) -> $ix {
  return svsel_s32(x, ALL_FALSE_MASK, y);
}
#define vsrl_vi_vi_i(x, c) svlsr_n_s32_x(ptrue, x, c)

#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix {
  return svbic_s32_x(ptrue, y, x);
}


// bitmask logical operations
#[inline]
fn vand_vm_vo64_vm(x: $ox, y: $ux) -> $ux {
  // This needs to be a zeroing instruction because we need to make
  // sure that the inactive elements for the unpacked integers vector
  // are zero.
  return svreinterpret_s32_s64(
      svand_s64_z(x, svreinterpret_s64_s32(y), svreinterpret_s64_s32(y)));
}
#[inline]
fn vandnot_vm_vo64_vm(x: $ox, y: $ux) -> $ux {
  return svreinterpret_s32_s64(svsel_s64(
      x, svreinterpret_s64_s32(ALL_FALSE_MASK), svreinterpret_s64_s32(y)));
}
#[inline]
fn vor_vm_vo64_vm(x: $ox, y: $ux) -> $ux {
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




//

//


