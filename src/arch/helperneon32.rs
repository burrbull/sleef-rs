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

type VMask = uint32x4_t;
type VOpMask = uint32x4_t;

//type VInt = int32x4_t;

type VFloat = float32x4_t;
type VInt2 = int32x4_t;

//

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { }

#[inline]
fn vtestallones_i_vo32(VOpMask g) -> int {
  uint32x2_t x0 = vand_u32(vget_low_u32(g), vget_high_u32(g));
  uint32x2_t x1 = vpmin_u32(x0, x0);
  return vget_lane_u32(x1, 0);
}

static VFloat vloaduf(float *p) { return vld1q_f32(p); }
static void vstoreuf(float *p, VFloat v) { vst1q_f32(p, v); }

static VInt2 vloadu_vi2_p(int32_t *p) { return vld1q_s32(p); }
static void vstoreu_v_p_vi2(int32_t *p, VInt2 v) { vst1q_s32(p, v); }

//

#[inline]
fn vand_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vbicq_u32(y, x); }
#[inline]
fn vor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vorrq_u32(x, y); }
#[inline]
fn vxor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return veorq_u32(x, y); }

//#[inline]
//fn vand_vo_vo_vo(VOpMask x, VOpMask y) -> VOpMask { return vandq_u32(x, y); }
#[inline]
fn vandnot_vo_vo_vo(VOpMask x, VOpMask y) -> VOpMask { return vbicq_u32(y, x); }
//#[inline]
//fn vor_vo_vo_vo(VOpMask x, VOpMask y) -> VOpMask { return vorrq_u32(x, y); }
//#[inline]
//fn vxor_vo_vo_vo(VOpMask x, VOpMask y) -> VOpMask { return veorq_u32(x, y); }

#[inline]
fn vand_vm_vo64_vm(VOpMask x, VMask y) -> VMask { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vo64_vm(VOpMask x, VMask y) -> VMask { return vbicq_u32(y, x); }
#[inline]
fn vor_vm_vo64_vm(VOpMask x, VMask y) -> VMask { return vorrq_u32(x, y); }
#[inline]
fn vxor_vm_vo64_vm(VOpMask x, VMask y) -> VMask { return veorq_u32(x, y); }

#[inline]
fn vand_vm_vo32_vm(VOpMask x, VMask y) -> VMask { return vandq_u32(x, y); }
#[inline]
fn vandnot_vm_vo32_vm(VOpMask x, VMask y) -> VMask { return vbicq_u32(y, x); }
#[inline]
fn vor_vm_vo32_vm(VOpMask x, VMask y) -> VMask { return vorrq_u32(x, y); }
#[inline]
fn vxor_vm_vo32_vm(VOpMask x, VMask y) -> VMask { return veorq_u32(x, y); }

#[inline]
fn vcast_vo32_vo64(VOpMask m) -> VOpMask { return vuzpq_u32(m, m).val[0]; }
#[inline]
fn vcast_vo64_vo32(VOpMask m) -> VOpMask { return vzipq_u32(m, m).val[0]; }

//

#[inline]
fn vcast_vm_i_i(i0: int, i1: int) -> VMask { return (VMask)vdupq_n_u64((uint64_t)i0 | (((uint64_t)i1) << 32)); }
#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask {
  uint32x4_t t = vceqq_u32(x, y);
  return vandq_u32(t, vrev64q_u32(t));
}

//

#[inline]
fn vcast_vi2_vm(vm: VMask) -> VInt2 { return (VInt2)vm; }
#[inline]
fn vcast_vm_vi2(vi: VInt2) -> VMask { return (VMask)vi; }
#[inline]
fn vrint_vi2_vf(d: VFloat) -> VInt2 {
  return vcvtq_s32_f32(vaddq_f32(d, (float32x4_t)vorrq_u32(vandq_u32((uint32x4_t)d, (uint32x4_t)vdupq_n_f32(-0.0f)), (uint32x4_t)vdupq_n_f32(0.5f))));
}
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { return vcvtq_s32_f32(vf); }
#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat { return vcvtq_f32_s32(vi); }

#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vrint_vi2_vf(vd)); }

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        vdupq_n_f32(self)
    }
}
#[inline]
fn vcast_vi2_i(i: int) -> VInt2 { return vdupq_n_s32(i); }
#[inline]
fn vreinterpret_vm_vf(vf: VFloat) -> VMask { return (VMask)vf; }
#[inline]
fn vreinterpret_vf_vm(vm: VMask) -> VFloat { return (VFloat)vm; }
#[inline]
fn vreinterpret_vf_vi2(vm: VInt2) -> VFloat { return (VFloat)vm; }
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) -> VInt2 { return (VInt2)vf; }


#[inline]
fn vabs_vf_vf(f: VFloat) -> VFloat { return vabsq_f32(f); }
#if CONFIG == 4
#[inline]
fn vmla_vf_vf_vf_vf  (x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vfmaq_f32(z, x, y); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vfmsq_f32(z, x, y); }
#[inline]
fn vfma_vf_vf_vf_vf  (x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vfmaq_f32(z, x, y); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vfmsq_f32(z, x, y); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return -vfmanp_vf_vf_vf_vf(x, y, z); }


#[inline]
fn vsqrt_vf_vf(d: VFloat) -> VFloat {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  float32x4_t u = vmulq_f32(x, d);
  u = vfmaq_f32(u, vfmsq_f32(d, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
  return vreinterpretq_f32_u32(vbicq_u32(vreinterpretq_u32_f32(u), vceqq_f32(d, vdupq_n_f32(0.0f))));
}

#[inline]
fn vrec_vf_vf(y: VFloat) -> VFloat {
  float32x4_t t = vrecpeq_f32(y), u;
  t = vmulq_f32(t, vrecpsq_f32(y, t));
  t = vfmaq_f32(t, vfmsq_f32(vdupq_n_f32(1.0f), y, t), t);
  return vfmaq_f32(t, vfmsq_f32(vdupq_n_f32(1.0f), y, t), t);
}

#[inline]
fn vrecsqrt_vf_vf(d: VFloat) -> VFloat {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  return vfmaq_f32(x, vfmsq_f32(vdupq_n_f32(1), x, vmulq_f32(x, d)), vmulq_f32(x, vdupq_n_f32(0.5)));
}
#else // #if CONFIG == 4
#[inline]
fn vmla_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vmlaq_f32(z, x, y); }
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vmlsq_f32(z, x, y); }


#[inline]
fn vsqrt_vf_vf(d: VFloat) -> VFloat {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  float32x4_t u = vmulq_f32(x, d);
  u = vmlaq_f32(u, vmlsq_f32(d, u, u), vmulq_f32(x, vdupq_n_f32(0.5)));
  return vreinterpretq_f32_u32(vbicq_u32(vreinterpretq_u32_f32(u), vceqq_f32(d, vdupq_n_f32(0.0f))));
}

#[inline]
fn vrec_vf_vf(d: VFloat) -> VFloat {
  float32x4_t x = vrecpeq_f32(d);
  x = vmulq_f32(x, vrecpsq_f32(d, x));
  return vmlsq_f32(vaddq_f32(x, x), vmulq_f32(x, x), d);
}

#[inline]
fn vrecsqrt_vf_vf(d: VFloat) -> VFloat {
  float32x4_t x = vrsqrteq_f32(d);
  x = vmulq_f32(x, vrsqrtsq_f32(d, vmulq_f32(x, x)));
  return vmlaq_f32(x, vmlsq_f32(vdupq_n_f32(1), x, vmulq_f32(x, d)), vmulq_f32(x, vdupq_n_f32(0.5)));
}
#endif // #if CONFIG == 4

#[inline]
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vaddq_s32(x, y); }
#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vsubq_s32(x, y); }
#[inline]
fn vneg_vi2_vi2(e: VInt2) -> VInt2 { return vnegq_s32(e); }

#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vandq_s32(x, y); }
#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vbicq_s32(y, x); }
#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vorrq_s32(x, y); }
#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return veorq_s32(x, y); }

#[inline]
fn vand_vi2_vo_vi2(VOpMask x, VInt2 y) -> VInt2 { return (VInt2)vandq_u32(x, (VOpMask)y); }
#[inline]
fn vandnot_vi2_vo_vi2(VOpMask x, VInt2 y) -> VInt2 { return (VInt2)vbicq_u32((VOpMask)y, x); }

#define vsll_vi2_vi2_i(x, c) vshlq_n_s32(x, c)
#define vsrl_vi2_vi2_i(x, c) vreinterpretq_s32_u32(vshrq_n_u32(vreinterpretq_u32_s32(x), c))
#define vsra_vi2_vi2_i(x, c) vshrq_n_s32(x, c)

#[inline]
fn veq_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return vceqq_s32(x, y); }
#[inline]
fn vgt_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return vcgtq_s32(x, y); }
#[inline]
fn veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return (VInt2)vceqq_s32(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return (VInt2)vcgtq_s32(x, y); }

#[inline]
fn vsel_vi2_vo_vi2_vi2(VOpMask m, VInt2 x, VInt2 y) -> VInt2 { return (VInt2)vbslq_u32(m, (VMask)x, (VMask)y); }

#[inline]
fn vsel_vf_vo_vf_vf(VOpMask mask, VFloat x, VFloat y) -> VFloat {
  return (VFloat)vbslq_u32(mask, (VMask)x, (VMask)y);
}

#[inline]
fn vsel_vf_vo_f_f(VOpMask o, float v1, float v0) -> CONST -> VFloat {
  return vsel_vf_vo_vf_vf(o, vcast_vf_f(v1), vcast_vf_f(v0));
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(VOpMask o0, VOpMask o1, float d0, float d1, float d2) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, vcast_vf_f(d0), vsel_vf_vo_f_f(o1, d1, d2));
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(VOpMask o0, VOpMask o1, VOpMask o2, float d0, float d1, float d2, float d3) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, vcast_vf_f(d0), vsel_vf_vo_vf_vf(o1, vcast_vf_f(d1), vsel_vf_vo_f_f(o2, d2, d3)));
}

#[inline]
fn visinf_vo_vf(d: VFloat) -> VOpMask { return vabs_vf_vf(d).ne(vcast_vf_f(SLEEF_INFINITYf)); }
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask { return d.ne(vcast_vf_f(SLEEF_INFINITYf)); }
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask { return d.ne(vcast_vf_f(-SLEEF_INFINITYf)); }
#[inline]
fn visnan_vo_vf(d: VFloat) -> VOpMask { return d.ne(d); }

// This function is needed when debugging on MSVC.
#[inline]
fn vcast_f_vf(v: VFloat) -> float {
  float p[4];
  vst1q_f32 (p, v);
  return p[0];
}

#[inline]
fn vavailability_i(name: int) -> int {
  if (name != 2) return 0;
  return vcast_f_vf(vcast_vf_f(name) + vcast_vf_f(name)) != 0.0;
}


#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return vld1q_f32(__builtin_assume_aligned(ptr, 16)); }
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat { return vld1q_f32(ptr); }

#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { vst1q_f32(__builtin_assume_aligned(ptr, 16), v); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void { vst1q_f32(ptr, v); }

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi2) -> VFloat {
  return ((VFloat) {
      ptr[vgetq_lane_s32(vi2, 0)],
      ptr[vgetq_lane_s32(vi2, 1)],
      ptr[vgetq_lane_s32(vi2, 2)],
      ptr[vgetq_lane_s32(vi2, 3)]
    });
}

#define PNMASKf ((VFloat) { +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf ((VFloat) { -0.0f, +0.0f, -0.0f, +0.0f })

#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat { return (VFloat)vxor_vm_vm_vm((VMask)d, (VMask)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat { return (VFloat)vxor_vm_vm_vm((VMask)d, (VMask)NPMASKf); }

#[inline]
fn vsubadd_vf_vf_vf(d0: VFloat, d1: VFloat) -> VFloat { return d0 + vnegpos_vf_vf(d1); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vsubadd_vf_vf_vf(x * y, z); }

#[inline]
fn vrev21_vf_vf(d0: VFloat) -> VFloat { return vrev64q_f32(d0); }
#[inline]
fn vreva2_vf_vf(d0: VFloat) -> VFloat { return vcombine_f32(vget_high_f32(d0), vget_low_f32(d0)); }
#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vstream_v_p_vf(float *ptr, VFloat v) -> void { vstore_v_p_vf(ptr, v); }

#[inline]
fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
  vst1_f32((float *)(ptr+(offset + step * 0)*2), vget_low_f32(v));
  vst1_f32((float *)(ptr+(offset + step * 1)*2), vget_high_f32(v));
}

#[inline]
fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
  vst1_f32((float *)(ptr+(offset + step * 0)*2), vget_low_f32(v));
  vst1_f32((float *)(ptr+(offset + step * 1)*2), vget_high_f32(v));
}
