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
typedef s$ix32_t $mx;
typedef svbool_t $mox;

// Single precision definitions
typedef s$f32x32_t $f32x;
typedef s$ix32_t $ix2;

// Double precision definitions
typedef s$f32x64_t $f64x;
typedef s$ix32_t $ix;

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

#[inline]
fn vload_vf_p(const float *ptr) -> $f32x {
  return svld1_f32(ptrue, ptr);
}
#[inline]
fn vloadu_vf_p(const float *ptr) -> $f32x {
  return svld1_f32(ptrue, ptr);
}
#[inline]
fn vstoreu_v_p_vf(float *ptr, $f32x v) -> void {
  svst1_f32(ptrue, ptr, v);
}

// Basic logical operations for mask
#[inline]
fn vand_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  return svand_s32_x(ptrue, x, y);
}
#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  return svbic_s32_x(ptrue, y, x);
}
#[inline]
fn vor_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  return svorr_s32_x(ptrue, x, y);
}
#[inline]
fn vxor_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  return sveor_s32_x(ptrue, x, y);
}

#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  return svreinterpret_s32_s64(
           svadd_s64_x(ptrue, svreinterpret_s64_s32(x),
                              svreinterpret_s64_s32(y)));
}

// Mask <--> single precision reinterpret
#[inline]
fn vreinterpret_vm_vf(vf: $f32x) -> $mx {
  return svreinterpret_s32_f32(vf);
}
#[inline]
fn vreinterpret_vf_vm(vm: $mx) -> $f32x {
  return svreinterpret_f32_s32(vm);
}
#[inline]
fn vreinterpret_vf_vi2(vm: $ix2) -> $f32x {
  return svreinterpret_f32_s32(vm);
}
#[inline]
fn vreinterpret_vi2_vf(vf: $f32x) -> $ix2 {
  return svreinterpret_s32_f32(vf);
}
#[inline]
fn vcast_vi2_vm(vm: $mx) -> $ix2 { return vm; }
#[inline]
fn vcast_vm_vi2(vi: $ix2) -> $mx { return vi; }

// Conditional select
#[inline]
fn vsel_vi2_vm_vi2_vi2($mx m, $ix2 x, $ix2 y) -> $ix2 {
  return svsel_s32(svcmpeq_s32(ptrue, m, ALL_TRUE_MASK), x, y);
}

/****************************************/
/* Single precision FP operations */
/****************************************/
// Broadcast

// Add, Sub, Mul, Reciprocal 1/x, Division, Square root
#[inline]
fn vrec_vf_vf(d: $f32x) -> $f32x {
  return svdivr_n_f32_x(ptrue, d, 1.);
}
#[inline]
fn vsqrt_vf_vf(d: $f32x) -> $f32x { return svsqrt_f32_x(ptrue, d); }

// |x|, -x
#[inline]
fn vabs_vf_vf(f: $f32x) -> $f32x { return svabs_f32_x(ptrue, f); }


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

// Add, Sub, Neg (-x)
#[inline]
fn vadd_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return svadd_s32_x(ptrue, x, y);
}
#[inline]
fn vsub_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return svsub_s32_x(ptrue, x, y);
}
#[inline]
fn vneg_vi2_vi2(e: $ix2) -> $ix2 { return svneg_s32_x(ptrue, e); }

// Logical operations
#[inline]
fn vand_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return svand_s32_x(ptrue, x, y);
}
#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return svbic_s32_x(ptrue, y, x);
}
#[inline]
fn vor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return svorr_s32_x(ptrue, x, y);
}
#[inline]
fn vxor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 {
  return sveor_s32_x(ptrue, x, y);
}

// Shifts
#define vsll_vi2_vi2_i(x, c) svlsl_n_s32_x(ptrue, x, c)
#define vsrl_vi2_vi2_i(x, c)                                                   \
  svreinterpret_s32_u32(svlsr_n_u32_x(ptrue, svreinterpret_u32_s32(x), c))

#define vsra_vi2_vi2_i(x, c) svasr_n_s32_x(ptrue, x, c)

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
  return svcmpeq_n_f32(ptrue, vabs_vf_vf(d), SLEEF_INFINITYf);
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
#[inline]
fn veq_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox {
  return svcmpeq_s32(ptrue, x, y);
}
#[inline]
fn vgt_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox {
  return svcmpgt_s32(ptrue, x, y);
}

// logical opmask
//#[inline]
//fn vand_vo_vo_vo(x: $mox, y: $mox) -> $mox {
//  return svand_b_z(ptrue, x, y);
//}
#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox {
  return svbic_b_z(ptrue, y, x);
}
//#[inline]
//fn vor_vo_vo_vo(x: $mox, y: $mox) -> $mox {
//  return svorr_b_z(ptrue, x, y);
//}
//#[inline]
//fn vxor_vo_vo_vo(x: $mox, y: $mox) -> $mox {
//  return sveor_b_z(ptrue, x, y);
//}

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 {
  // This needs to be zeroing to prevent asinf and atanf denormal test
  // failing.
  return svand_s32_z(x, y, y);
}

// bitmask logical operations
#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $mx) -> $mx {
  return svsel_s32(x, y, ALL_FALSE_MASK);
}
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $mx) -> $mx {
  return svsel_s32(x, ALL_FALSE_MASK, y);
}
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $mx) -> $mx {
  return svsel_s32(x, ALL_TRUE_MASK, y);
}

// broadcast bitmask
#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $mx {
  return svreinterpret_s32_u64(
      svdup_n_u64((0xffffffff & (uint64_t)i1) | (((uint64_t)i0) << 32)));
}

/*********************************/
/* SVE for double precision math */
/*********************************/

// Vector load/store
#[inline]
fn vload_vd_p(const double *ptr) -> $f64x {
  return svld1_f64(ptrue, ptr);
}
#[inline]
fn vloadu_vd_p(const double *ptr) -> $f64x {
  return svld1_f64(ptrue, ptr);
}
#[inline]
fn vstoreu_v_p_vd(double *ptr, $f64x v) -> void {
  svst1_f64(ptrue, ptr, v);
}

#[inline]
fn vstoreu_v_p_vi(int *ptr, $ix v) -> void {
  svst1w_s64(ptrue, ptr, svreinterpret_s64_s32(v));
}
static $ix vloadu_vi_p(int32_t *p) {
  return svreinterpret_s32_s64(svld1uw_s64(ptrue, (uint32_t *)p));
}

// Reinterpret
#[inline]
fn vreinterpret_vd_vm(vm: $mx) -> $f64x {
  return svreinterpret_f64_s32(vm);
}
#[inline]
fn vreinterpret_vm_vd(vd: $f64x) -> $mx {
  return svreinterpret_s32_f64(vd);
}
#[inline]
fn vreinterpret_vd_vi2(x: $ix2) -> $f64x {
  return svreinterpret_f64_s32(x);
}
#[inline]
fn vreinterpret_vi2_vd(x: $f64x) -> $ix2 {
  return svreinterpret_s32_f64(x);
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


#[inline]
fn vrec_vd_vd(x: $f64x) -> $f64x {
  return svdivr_n_f64_x(ptrue, x, 1.0);
}
#[inline]
fn vsqrt_vd_vd(x: $f64x) -> $f64x { return svsqrt_f64_x(ptrue, x); }
#[inline]
fn vabs_vd_vd(x: $f64x) -> $f64x { return svabs_f64_x(ptrue, x); }


#if CONFIG == 1
// Multiply accumulate / subtract
impl Mla for $f64x {
    fn mla(self, y: Self, z: Self) -> Self {
        svmad_f64_x(ptrue, x, y, z)
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { // z = x * y - z
  return svnmsb_f64_x(ptrue, x, y, z);
}
#else
impl Mla for $f64x {
    fn mla(self, y: Self, z: Self) -> Self {
        x*y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x*y - z; }
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
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox {
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
#define vsra_vi_vi_i(x, c) svasr_n_s32_x(ptrue, x, c)
#define vsll_vi_vi_i(x, c) svlsl_n_s32_x(ptrue, x, c)
#define vsrl_vi_vi_i(x, c) svlsr_n_s32_x(ptrue, x, c)

#[inline]
fn vand_vi_vi_vi(x: $ix, y: $ix) -> $ix {
  return svand_s32_x(ptrue, x, y);
}
#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix {
  return svbic_s32_x(ptrue, y, x);
}
#[inline]
fn vxor_vi_vi_vi(x: $ix, y: $ix) -> $ix {
  return sveor_s32_x(ptrue, x, y);
}

// integer math
impl std::ops::Add for $ix {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        svadd_s32_x(ptrue, self, other)
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: $ix, y: $ix) -> $ix {
//  return svadd_s32_x(ptrue, x, y);
//}
impl std::ops::Sub for $ix {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        svsub_s32_x(ptrue, self, other)
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: $ix, y: $ix) -> $ix {
//  return svsub_s32_x(ptrue, x, y);
//}
impl std::ops::Neg for $ix {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        -svneg_s32_x(ptrue, self)
    }
}
//#[inline]
//fn vneg_vi_vi(x: $ix) -> $ix { return svneg_s32_x(ptrue, x); }

// integer comparison
#[inline]
fn vgt_vo_vi_vi(x: $ix, y: $ix) -> $mox {
  return svcmpgt_s32(ptrue, x, y);
}
#[inline]
fn veq_vo_vi_vi(x: $ix, y: $ix) -> $mox {
  return svcmpeq_s32(ptrue, x, y);
}



// bitmask logical operations
#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $mx) -> $mx {
  // This needs to be a zeroing instruction because we need to make
  // sure that the inactive elements for the unpacked integers vector
  // are zero.
  return svreinterpret_s32_s64(
      svand_s64_z(x, svreinterpret_s64_s32(y), svreinterpret_s64_s32(y)));
}
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $mx) -> $mx {
  return svreinterpret_s32_s64(svsel_s64(
      x, svreinterpret_s64_s32(ALL_FALSE_MASK), svreinterpret_s64_s32(y)));
}
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $mx) -> $mx {
  return svreinterpret_s32_s64(svsel_s64(
      x, svreinterpret_s64_s32(ALL_TRUE_MASK), svreinterpret_s64_s32(y)));
}

#[inline]
fn vrev21_vf_vf(vf: $f32x) -> $f32x {
  return svreinterpret_f32_u64(svrevw_u64_x(ptrue, svreinterpret_u64_f32(vf)));
}

#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

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

#[inline]
fn vrev21_vd_vd(x: $f64x) -> $f64x { return svzip1_f64(svuzp2_f64(x, x), svuzp1_f64(x, x)); }

#[inline]
fn vreva2_vd_vd(vd: $f64x) -> $f64x {
  s$ix64_t x = svindex_s64((VECTLENDP-1), -1);
  x = svzip1_s64(svuzp2_s64(x, x), svuzp1_s64(x, x));
  return svtbl_f64(vd, svreinterpret_u64_s64(x));
}

#[inline]
fn vreva2_vf_vf(vf: $f32x) -> $f32x {
  s$ix32_t x = svindex_s32((VECTLENSP-1), -1);
  x = svzip1_s32(svuzp2_s32(x, x), svuzp1_s32(x, x));
  return svtbl_f32(vf, svreinterpret_u32_s32(x));
}

//

//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void {
//  svst1_scatter_u64index_f64(ptrue, ptr + offset*2, svzip1_u64(svindex_u64(0, step*2), svindex_u64(1, step*2)), v);
//}

//#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
//  svst1_scatter_u32index_f32(ptrue, ptr + offset*2, svzip1_u32(svindex_u32(0, step*2), svindex_u32(1, step*2)), v);
//}

#[inline]
fn vstore_v_p_vd(double *ptr, $f64x v) -> void { vstoreu_v_p_vd(ptr, v); }
//#[inline]
//fn vstream_v_p_vd(double *ptr, $f64x v) -> void { vstore_v_p_vd(ptr, v); }
#[inline]
fn vstore_v_p_vf(float *ptr, $f32x v) -> void { vstoreu_v_p_vf(ptr, v); }
//#[inline]
//fn vstream_v_p_vf(float *ptr, $f32x v) -> void { vstore_v_p_vf(ptr, v); }
//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void { vscatter2_v_p_i_i_vd(ptr, offset, step, v); }
//#[inline]
//fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }

// These functions are for debugging
static double vcast_d_vd(v: $f64x) {
  double a[svcntd()];
  vstoreu_v_p_vd(a, v);
  return a[0];
}

static float vcast_f_vf(v: $f32x) {
  float a[svcntw()];
  vstoreu_v_p_vf(a, v);
  return a[0];
}

static int vcast_i_vi(v: $ix) {
  int a[svcntw()];
  vstoreu_v_p_vi(a, v);
  return a[0];
}

static int vcast_i_vi2(v: $ix2) {
  int a[svcntw()];
  vstoreu_v_p_vi2(a, v);
  return a[0];
}
