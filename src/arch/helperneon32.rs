//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef __ARM_NEON
#error Please specify -mfpu=neon.
#endif

#ifdef __aarch64__
#warning This implementation is for AARCH32.
#endif

#define ENABLE_SP
#define LOG2VECTLENSP 2
#define VECTLENSP (1 << LOG2VECTLENSP)

#if CONFIG == 4
#define ISANAME "AARCH32 NEON-VFPV4"
#define ENABLE_FMA_SP
#else
#define ISANAME "AARCH32 NEON"
#endif
#define DFTPRIORITY 10

#define ENABLE_RECSQRT_SP

#include <arm_neon.h>
#include <stdint.h>

#include "misc.h"

type $ux = uint32x4_t;
type $ox = uint32x4_t;

//type $ix = int32x4_t;

type f32x4 = float32x4_t;
type i32x4 = int32x4_t;

//

#[inline]
fn vtestallones_i_vo32($ox g) -> bool {
  uint32x2_t x0 = vand_u32(vget_low_u32(g), vget_high_u32(g));
  uint32x2_t x1 = vpmin_u32(x0, x0);
  return vget_lane_u32(x1, 0);
}

//

#[inline]
fn vandnot_vm_vm_vm(x: $ux, y: $ux) -> $ux { return vbicq_u32(y, x); }

#[inline]
fn vandnot_vo_vo_vo($ox x, $ox y) -> $ox { return vbicq_u32(y, x); }

#[inline]
fn vand_vm_vo64_vm($ox x, $ux y) -> $ux { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vo64_vm($ox x, $ux y) -> $ux { return vbicq_u32(y, x); }
#[inline]
fn vor_vm_vo64_vm($ox x, $ux y) -> $ux { return vorrq_u32(x, y); }

#[inline]
fn vand_vm_vo32_vm($ox x, $ux y) -> $ux { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vo32_vm($ox x, $ux y) -> $ux { return vbicq_u32(y, x); }
#[inline]
fn vor_vm_vo32_vm($ox x, $ux y) -> $ux { return vorrq_u32(x, y); }


#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $ux { return ($ux)vdupq_n_u64((uint64_t)i0 | (((uint64_t)i1) << 32)); }

//
#[inline]
fn vrint_vi2_vf(d: f32x4) -> i32x4 {
  return vcvtq_s32_f32(vaddq_f32(d, (float32x4_t)vorrq_u32(vandq_u32((uint32x4_t)d, (uint32x4_t)vdupq_n_f32(-0.)), (uint32x4_t)vdupq_n_f32(0.5f))));
}
#[inline]
fn vtruncate_vi2_vf(vf: f32x4) -> i32x4 { return vcvtq_s32_f32(vf); }

impl Truncate for f32x4 {
    #[inline]
    fn truncate(self) -> Self {
        vcast_vf_vi2(vtruncate_vi2_vf(vd))
    }
}
impl RInt for f32x4 {
    #[inline]
    fn rint(self) -> Self {
      vcast_vf_vi2(vrint_vi2_vf(vd))
    }
}

#if CONFIG == 4
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x4, y: f32x4, z: f32x4) -> f32x4 { return vfmsq_f32(z, x, y); }


#[target_feature(enable = "fma")]
impl Fma for f32x4 {
    #[inline]
    fn mul_sube(self, y: Self, z: Self) -> Self {
        -(x.fmanp(y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        vfmsq_f32(z, x, y)
    }
}

#else // #if CONFIG == 4
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x4, y: f32x4, z: f32x4) -> f32x4 { return vmlsq_f32(z, x, y); }


#endif // #if CONFIG == 4

#[inline]
fn vandnot_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { return vbicq_s32(y, x); }

#[inline]
fn vand_vi2_vo_vi2($ox x, i32x4 y) -> i32x4 { return (i32x4)vandq_u32(x, ($ox)y); }


#[inline]
fn veq_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { return (i32x4)vceqq_s32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2(i32x4 x, i32x4 y) -> i32x4 { return (i32x4)vcgtq_s32(x, y); }


/*
#[inline]
fn vsel_vf_vo_f_f(o: $ox, v1: f32, v0: f32) -> f32x4 {
  o.select(f32x4::splat(v1), f32x4::splat(v0))
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: $ox, o1: $ox, d0: f32, d1: f32, d2: f32) -> f32x4 {
  o0.select(f32x4::splat(d0), vsel_vf_vo_f_f(o1, d1, d2))
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $ox, o1: $ox, o2: $ox, d0: f32, d1: f32, d2: f32, d3: f32) -> f32x4 {
  o0.select(f32x4::splat(d0), o1.select(f32x4::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)))
}
*/
#[inline]
fn vgather_vf_p_vi2(const float *ptr, i32x4 vi2) -> f32x4 {
  return ((f32x4) {
      ptr[vgetq_lane_s32(vi2, 0)],
      ptr[vgetq_lane_s32(vi2, 1)],
      ptr[vgetq_lane_s32(vi2, 2)],
      ptr[vgetq_lane_s32(vi2, 3)]
    });
}

#define PNMASK_F ((f32x4) { 0., -0., 0., -0. })
#define NPMASK_F ((f32x4) { -0., 0., -0., 0. })



#[inline]
fn vrev21_vf_vf(d0: f32x4) -> f32x4 { return vrev64q_f32(d0); }
#[inline]
fn vrev21_vi2_vi2(i: i32x4) -> i32x4 { return i32x4::from(vrev21_vf_vf(f32x4::from(i))); }
