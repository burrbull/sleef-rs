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

type $mx = uint32x4_t;
type $mox = uint32x4_t;

//type $ix = int32x4_t;

type $f32x = float32x4_t;
type $ix2 = int32x4_t;

//

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { }

#[inline]
fn vtestallones_i_vo32($mox g) -> int {
  uint32x2_t x0 = vand_u32(vget_low_u32(g), vget_high_u32(g));
  uint32x2_t x1 = vpmin_u32(x0, x0);
  return vget_lane_u32(x1, 0);
}

static $f32x vloaduf(float *p) { return vld1q_f32(p); }
static void vstoreuf(float *p, $f32x v) { vst1q_f32(p, v); }

static $ix2 vloadu_vi2_p(int32_t *p) { return vld1q_s32(p); }
static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { vst1q_s32(p, v); }

//

#[inline]
fn vand_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vbicq_u32(y, x); }
#[inline]
fn vor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vorrq_u32(x, y); }
#[inline]
fn vxor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return veorq_u32(x, y); }

//#[inline]
//fn vand_vo_vo_vo($mox x, $mox y) -> $mox { return vandq_u32(x, y); }
#[inline]
fn vandnot_vo_vo_vo($mox x, $mox y) -> $mox { return vbicq_u32(y, x); }
//#[inline]
//fn vor_vo_vo_vo($mox x, $mox y) -> $mox { return vorrq_u32(x, y); }
//#[inline]
//fn vxor_vo_vo_vo($mox x, $mox y) -> $mox { return veorq_u32(x, y); }

#[inline]
fn vand_vm_vo64_vm($mox x, $mx y) -> $mx { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vo64_vm($mox x, $mx y) -> $mx { return vbicq_u32(y, x); }
#[inline]
fn vor_vm_vo64_vm($mox x, $mx y) -> $mx { return vorrq_u32(x, y); }
#[inline]
fn vxor_vm_vo64_vm($mox x, $mx y) -> $mx { return veorq_u32(x, y); }

#[inline]
fn vand_vm_vo32_vm($mox x, $mx y) -> $mx { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vo32_vm($mox x, $mx y) -> $mx { return vbicq_u32(y, x); }
#[inline]
fn vor_vm_vo32_vm($mox x, $mx y) -> $mx { return vorrq_u32(x, y); }
#[inline]
fn vxor_vm_vo32_vm($mox x, $mx y) -> $mx { return veorq_u32(x, y); }

#[inline]
fn vcast_vo32_vo64($mox m) -> $mox { return vuzpq_u32(m, m).val[0]; }
#[inline]
fn vcast_vo64_vo32($mox m) -> $mox { return vzipq_u32(m, m).val[0]; }

//

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> $mx { return ($mx)vdupq_n_u64((uint64_t)i0 | (((uint64_t)i1) << 32)); }
#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox {
  uint32x4_t t = vceqq_u32(x, y);
  return vandq_u32(t, vrev64q_u32(t));
}

//

#[inline]
fn vcast_vi2_vm(vm: $mx) -> $ix2 { return ($ix2)vm; }
#[inline]
fn vcast_vm_vi2(vi: $ix2) -> $mx { return ($mx)vi; }
#[inline]
fn vrint_vi2_vf(d: $f32x) -> $ix2 {
  return vcvtq_s32_f32(vaddq_f32(d, (float32x4_t)vorrq_u32(vandq_u32((uint32x4_t)d, (uint32x4_t)vdupq_n_f32(-0.0f)), (uint32x4_t)vdupq_n_f32(0.5f))));
}
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return vcvtq_s32_f32(vf); }

#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vrint_vi2_vf(vd)); }


#[inline]
fn vreinterpret_vm_vf(vf: $f32x) -> $mx { return ($mx)vf; }
#[inline]
fn vreinterpret_vf_vm(vm: $mx) -> $f32x { return ($f32x)vm; }
#[inline]
fn vreinterpret_vf_vi2(vm: $ix2) -> $f32x { return ($f32x)vm; }
#[inline]
fn vreinterpret_vi2_vf(vf: $f32x) -> $ix2 { return ($ix2)vf; }


#[inline]
fn vabs_vf_vf(f: $f32x) -> $f32x { return vabsq_f32(f); }
#if CONFIG == 4
#[inline]
fn vmla_vf_vf_vf_vf  (x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vfmaq_f32(z, x, y); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vfmsq_f32(z, x, y); }
#[inline]
fn vfma_vf_vf_vf_vf  (x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vfmaq_f32(z, x, y); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vfmsq_f32(z, x, y); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return -vfmanp_vf_vf_vf_vf(x, y, z); }


#[inline]
fn vsqrt_vf_vf(d: $f32x) -> $f32x {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  float32x4_t u = vmulq_f32(x, d);
  u = vfmaq_f32(u, vfmsq_f32(d, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
  return vreinterpretq_f32_u32(vbicq_u32(vreinterpretq_u32_f32(u), vceqq_f32(d, vdupq_n_f32(0.0f))));
}

#[inline]
fn vrec_vf_vf(y: $f32x) -> $f32x {
  float32x4_t t = vrecpeq_f32(y), u;
  t = vmulq_f32(t, vrecpsq_f32(y, t));
  t = vfmaq_f32(t, vfmsq_f32(vdupq_n_f32(1.0f), y, t), t);
  return vfmaq_f32(t, vfmsq_f32(vdupq_n_f32(1.0f), y, t), t);
}

#[inline]
fn vrecsqrt_vf_vf(d: $f32x) -> $f32x {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  return vfmaq_f32(x, vfmsq_f32(vdupq_n_f32(1), x, vmulq_f32(x, d)), vmulq_f32(x, vdupq_n_f32(0.5)));
}
#else // #if CONFIG == 4
#[inline]
fn vmla_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vmlaq_f32(z, x, y); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vmlsq_f32(z, x, y); }


#[inline]
fn vsqrt_vf_vf(d: $f32x) -> $f32x {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  float32x4_t u = vmulq_f32(x, d);
  u = vmlaq_f32(u, vmlsq_f32(d, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
  return vreinterpretq_f32_u32(vbicq_u32(vreinterpretq_u32_f32(u), vceqq_f32(d, vdupq_n_f32(0.0f))));
}

#[inline]
fn vrec_vf_vf(d: $f32x) -> $f32x {
  float32x4_t x = vrecpeq_f32(d);
  x = vmulq_f32(x, vrecpsq_f32(d, x));
  return vmlsq_f32(vaddq_f32(x, x), vmulq_f32(x, x), d);
}

#[inline]
fn vrecsqrt_vf_vf(d: $f32x) -> $f32x {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  return vmlaq_f32(x, vmlsq_f32(vdupq_n_f32(1), x, vmulq_f32(x, d)), vmulq_f32(x, vdupq_n_f32(0.5)));
}
#endif // #if CONFIG == 4

#[inline]
fn vadd_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vaddq_s32(x, y); }
#[inline]
fn vsub_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vsubq_s32(x, y); }
#[inline]
fn vneg_vi2_vi2(e: $ix2) -> $ix2 { return vnegq_s32(e); }

#[inline]
fn vand_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vandq_s32(x, y); }
#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vbicq_s32(y, x); }
#[inline]
fn vor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vorrq_s32(x, y); }
#[inline]
fn vxor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return veorq_s32(x, y); }

#[inline]
fn vand_vi2_vo_vi2($mox x, $ix2 y) -> $ix2 { return ($ix2)vandq_u32(x, ($mox)y); }
#[inline]
fn vandnot_vi2_vo_vi2($mox x, $ix2 y) -> $ix2 { return ($ix2)vbicq_u32(($mox)y, x); }

#define vsll_vi2_vi2_i(x, c) vshlq_n_s32(x, c)
#define vsrl_vi2_vi2_i(x, c) vreinterpretq_s32_u32(vshrq_n_u32(vreinterpretq_u32_s32(x), c))
#define vsra_vi2_vi2_i(x, c) vshrq_n_s32(x, c)

#[inline]
fn veq_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return vceqq_s32(x, y); }
#[inline]
fn vgt_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return vcgtq_s32(x, y); }
#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return ($ix2)vceqq_s32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return ($ix2)vcgtq_s32(x, y); }

#[inline]
fn vsel_vi2_vo_vi2_vi2($mox m, $ix2 x, $ix2 y) -> $ix2 { return ($ix2)vbslq_u32(m, ($mx)x, ($mx)y); }

#[inline]
fn vsel_vf_vo_vf_vf($mox mask, $f32x x, $f32x y) -> $f32x {
  return ($f32x)vbslq_u32(mask, ($mx)x, ($mx)y);
}

#[inline]
fn vsel_vf_vo_f_f(o: $mox, v1: f32, v0: f32) -> $f32x {
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
fn visinf_vo_vf(d: $f32x) -> $mox { return vabs_vf_vf(d).ne($f32x::splat(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { return d.ne($f32x::splat(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return d.ne(d); }

// This function is needed when debugging on MSVC.
#[inline]
fn vcast_f_vf(v: $f32x) -> float {
  float p[4];
  vst1q_f32 (p, v);
  return p[0];
}

#[inline]
fn vavailability_i(name: int) -> int {
  if (name != 2) return 0;
  return vcast_f_vf($f32x::splat(name) + $f32x::splat(name)) != 0.0;
}


#[inline]
fn vload_vf_p(const float *ptr) -> $f32x { return vld1q_f32(__builtin_assume_aligned(ptr, 16)); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> $f32x { return vld1q_f32(ptr); }

#[inline]
fn vstore_v_p_vf(float *ptr, $f32x v) -> void { vst1q_f32(__builtin_assume_aligned(ptr, 16), v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, $f32x v) -> void { vst1q_f32(ptr, v); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x {
  return (($f32x) {
      ptr[vgetq_lane_s32(vi2, 0)],
      ptr[vgetq_lane_s32(vi2, 1)],
      ptr[vgetq_lane_s32(vi2, 2)],
      ptr[vgetq_lane_s32(vi2, 3)]
    });
}

#define PNMASKf (($f32x) { +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf (($f32x) { -0.0f, +0.0f, -0.0f, +0.0f })

#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return ($f32x)vxor_vm_vm_vm(($mx)d, ($mx)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return ($f32x)vxor_vm_vm_vm(($mx)d, ($mx)NPMASKf); }

#[inline]
fn vsubadd_vf_vf_vf(d0: $f32x, d1: $f32x) -> $f32x { return d0 + vnegpos_vf_vf(d1); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vsubadd_vf_vf_vf(x * y, z); }

#[inline]
fn vrev21_vf_vf(d0: $f32x) -> $f32x { return vrev64q_f32(d0); }
#[inline]
fn vreva2_vf_vf(d0: $f32x) -> $f32x { return vcombine_f32(vget_high_f32(d0), vget_low_f32(d0)); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vstream_v_p_vf(float *ptr, $f32x v) -> void { vstore_v_p_vf(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
  vst1_f32((float *)(ptr+(offset + step * 0)*2), vget_low_f32(v));
  vst1_f32((float *)(ptr+(offset + step * 1)*2), vget_high_f32(v));
}

#[inline]
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
  vst1_f32((float *)(ptr+(offset + step * 0)*2), vget_low_f32(v));
  vst1_f32((float *)(ptr+(offset + step * 1)*2), vget_high_f32(v));
}
