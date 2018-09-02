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
type $ix2 = int32x4_t;

//

#[inline]
fn vtestallones_i_vo32($ox g) -> int {
  uint32x2_t x0 = vand_u32(vget_low_u32(g), vget_high_u32(g));
  uint32x2_t x1 = vpmin_u32(x0, x0);
  return vget_lane_u32(x1, 0);
}

static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { vst1q_s32(p, v); }

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
fn vcast_vo32_vo64($ox m) -> $ox { return vuzpq_u32(m, m).val[0]; }
#[inline]
fn vcast_vo64_vo32($ox m) -> $ox { return vzipq_u32(m, m).val[0]; }

//

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $ux { return ($ux)vdupq_n_u64((uint64_t)i0 | (((uint64_t)i1) << 32)); }
#[inline]
fn veq64_vo_vm_vm(x: $ux, y: $ux) -> $ox {
  uint32x4_t t = vceqq_u32(x, y);
  return vandq_u32(t, vrev64q_u32(t));
}

//
#[inline]
fn vrint_vi2_vf(d: f32x4) -> $ix2 {
  return vcvtq_s32_f32(vaddq_f32(d, (float32x4_t)vorrq_u32(vandq_u32((uint32x4_t)d, (uint32x4_t)vdupq_n_f32(-0.)), (uint32x4_t)vdupq_n_f32(0.5f))));
}
#[inline]
fn vtruncate_vi2_vf(vf: f32x4) -> $ix2 { return vcvtq_s32_f32(vf); }

#[inline]
fn vtruncate_vf_vf(vd: f32x4) -> f32x4 { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vd: f32x4) -> f32x4 { return vcast_vf_vi2(vrint_vi2_vf(vd)); }

impl Abs for f32x4 {
    fn abs(self) -> Self {
        vabsq_f32(f)
    }
}
#if CONFIG == 4
impl Mla for f32x4 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        vfmaq_f32(z, x, y)
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x4, y: f32x4, z: f32x4) -> f32x4 { return vfmsq_f32(z, x, y); }


impl Fma for f32x4 {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
        vfmaq_f32(z, x, y)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self {
        -(x.fmanp(y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        vfmsq_f32(z, x, y)
    }
}

impl Sqrt for f32x4 {
    #[inline]
    fn sqrt(self) -> Self {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  float32x4_t u = vmulq_f32(x, d);
  u = vfmaq_f32(u, vfmsq_f32(d, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
  return vreinterpretq_f32_u32(vbicq_u32(vreinterpretq_u32_f32(u), vceqq_f32(d, vdupq_n_f32(0.))));
        
    }
}
}
impl Rec for f32x4 {
    #[inline]
    fn rec(self) -> Self {
        float32x4_t t = vrecpeq_f32(y), u;
        t = vmulq_f32(t, vrecpsq_f32(y, t));
        t = vfmaq_f32(t, vfmsq_f32(vdupq_n_f32(1.), y, t), t);
        return vfmaq_f32(t, vfmsq_f32(vdupq_n_f32(1.), y, t), t);
    }
}

#[inline]
fn vrecsqrt_vf_vf(d: f32x4) -> f32x4 {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  return vfmaq_f32(x, vfmsq_f32(vdupq_n_f32(1), x, vmulq_f32(x, d)), vmulq_f32(x, vdupq_n_f32(0.5)));
}
#else // #if CONFIG == 4
impl Mla for f32x4 {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        vmlaq_f32(z, x, y)
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: f32x4, y: f32x4, z: f32x4) -> f32x4 { return vmlsq_f32(z, x, y); }


impl Sqrt for f32x4 {
    #[inline]
    fn sqrt(self) -> Self {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  float32x4_t u = vmulq_f32(x, d);
  u = vmlaq_f32(u, vmlsq_f32(d, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
  return vreinterpretq_f32_u32(vbicq_u32(vreinterpretq_u32_f32(u), vceqq_f32(d, vdupq_n_f32(0.))));
        
    }
}
impl Rec for f32x4 {
    #[inline]
    fn rec(self) -> Self {
        float32x4_t x = vrecpeq_f32(d);
        x = vmulq_f32(x, vrecpsq_f32(d, x));
        return vmlsq_f32(vaddq_f32(x, x), vmulq_f32(x, x), d);
    }
}


#[inline]
fn vrecsqrt_vf_vf(d: f32x4) -> f32x4 {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  return vmlaq_f32(x, vmlsq_f32(vdupq_n_f32(1), x, vmulq_f32(x, d)), vmulq_f32(x, vdupq_n_f32(0.5)));
}
#endif // #if CONFIG == 4

#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vbicq_s32(y, x); }

#[inline]
fn vand_vi2_vo_vi2($ox x, $ix2 y) -> $ix2 { return ($ix2)vandq_u32(x, ($ox)y); }
#[inline]
fn vandnot_vi2_vo_vi2($ox x, $ix2 y) -> $ix2 { return ($ix2)vbicq_u32(($ox)y, x); }

#define vsrl_vi2_vi2_i(x, c) vreinterpretq_s32_u32(vshrq_n_u32(vreinterpretq_u32_s32(x), c))

#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return ($ix2)vceqq_s32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return ($ix2)vcgtq_s32(x, y); }



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

#[inline]
fn visinf_vo_vf(d: f32x4) -> $ox { return d.abs().ne(f32x4::splat(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: f32x4) -> $ox { return d.ne(f32x4::splat(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: f32x4) -> $ox { return d.ne(f32x4::splat(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: f32x4) -> $ox { return d.ne(d); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> f32x4 {
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
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf(f32x4::from(i))); }
