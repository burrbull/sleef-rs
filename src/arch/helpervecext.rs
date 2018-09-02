//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <stdint.h>
#include "misc.h"

#ifndef CONFIG
#error CONFIG macro not defined
#endif

#define ENABLE_DP
#define ENABLE_SP

#define LOG2VECTLENDP CONFIG
#define VECTLENDP (1 << LOG2VECTLENDP)
#define LOG2VECTLENSP (LOG2VECTLENDP+1)
#define VECTLENSP (1 << LOG2VECTLENSP)

#define DFTPRIORITY LOG2VECTLENDP

#if defined(__clang__)
#define ISANAME "Clang Vector Extension"

typedef uint32_t $ux __attribute__((ext_vector_type(VECTLENDP*2)));
typedef uint32_t $ox __attribute__((ext_vector_type(VECTLENDP*2)));

typedef double $f64x __attribute__((ext_vector_type(VECTLENDP)));
typedef int32_t $ix __attribute__((ext_vector_type(VECTLENDP)));

typedef float $f32x __attribute__((ext_vector_type(VECTLENDP*2)));
typedef int32_t $ix2 __attribute__((ext_vector_type(VECTLENDP*2)));

#ifdef Sleef_quad2_DEFINED
typedef uint8_t $uxq __attribute__((ext_vector_type(sizeof(Sleef_quad)*VECTLENDP)));

#endif
#elif defined(__GNUC__)
#define ISANAME "GCC Vector Extension"

typedef uint32_t $ux __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));
typedef uint32_t $ox __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));

typedef double $f64x __attribute__((vector_size(sizeof(double)*VECTLENDP)));
typedef int32_t $ix __attribute__((vector_size(sizeof(int32_t)*VECTLENDP)));

typedef float $f32x __attribute__((vector_size(sizeof(float)*VECTLENDP*2)));
typedef int32_t $ix2 __attribute__((vector_size(sizeof(int32_t)*VECTLENDP*2)));


#ifdef Sleef_quad2_DEFINED
typedef uint8_t $uxq __attribute__((vector_size(sizeof(Sleef_quad)*VECTLENDP)));
typedef Sleef_quad VQuad __attribute__((vector_size(sizeof(Sleef_quad)*VECTLENDP)));
#endif
#endif

//

#if VECTLENDP == 2
#[inline]
fn vcast_vo32_vo64(m: $ox) -> $ox { return ($ox){ m[1], m[3], 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: $ox) -> $ox { return ($ox){ m[0], m[0], m[1], m[1] }; }


#ifdef Sleef_quad2_DEFINED
#[inline]
fn vcast_vq_q(Sleef_quad d) { return (VQuad) -> VQuad { d, d }; }
#endif

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $ux { return ($ux){ l, h, l, h }; }


#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix { return ($ix){ vi2[0], vi2[1] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 { return ($ix2){ vi[0], vi[1], 0, 0 }; }

#[inline]
fn vrev21_vf_vf(vd: $f32x) { return ($f32x) -> $f32x { vd[1], vd[0], vd[3], vd[2] }; }


#ifdef Sleef_quad2_DEFINED
#[inline]
fn vrev21_vq_vq(vd: VQuad) { return (VQuad) -> VQuad { vd[1], vd[0] }; }
#[inline]
fn vreva2_vq_vq(vd: VQuad) -> VQuad { return vd; }
#[inline]
fn vposneg_vq_vq(vd: VQuad) { return (VQuad) -> VQuad { +vd[0], -vd[1] }; }
#[inline]
fn vnegpos_vq_vq(vd: VQuad) { return (VQuad) -> VQuad { -vd[0], +vd[1] }; }
#endif


#define PNMASK_F (($f32x) { 0., -0., 0., -0. })
#define NPMASK_F (($f32x) { -0., 0., -0., 0. })
#elif VECTLENDP == 4
#[inline]
fn vcast_vo32_vo64(m: $ox) -> $ox { return ($ox){ m[1], m[3], m[5], m[7], 0, 0, 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: $ox) -> $ox { return ($ox){ m[0], m[0], m[1], m[1], m[2], m[2], m[3], m[3] }; }

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $ux { return ($ux){ l, h, l, h, l, h, l, h }; }

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix { return ($ix){ vi2[0], vi2[1], vi2[2], vi2[3] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 { return ($ix2){ vi[0], vi[1], vi[2], vi[3], 0, 0, 0, 0 }; }


#[inline]
fn vrev21_vf_vf(vd: $f32x) { return ($f32x) -> $f32x { vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6] }; }

#elif VECTLENDP == 8
#[inline]
fn vcast_vo32_vo64(m: $ox) -> $ox { return ($ox){ m[1], m[3], m[5], m[7], m[9], m[11], m[13], m[15], 0, 0, 0, 0, 0, 0, 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: $ox) -> $ox { return ($ox){ m[0], m[0], m[1], m[1], m[2], m[2], m[3], m[3], m[4], m[4], m[5], m[5], m[6], m[6], m[7], m[7] }; }


#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $ux { return ($ux){ l, h, l, h, l, h, l, h, l, h, l, h, l, h, l, h }; }

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix { return ($ix){ vi2[0], vi2[1], vi2[2], vi2[3], vi2[4], vi2[5], vi2[6], vi2[7] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 { return ($ix2){ vi[0], vi[1], vi[2], vi[3], vi[4], vi[5], vi[6], vi[7], 0, 0, 0, 0, 0, 0, 0, 0 }; }


#define PNMASK_F (($f32x) { 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0. })
#define NPMASK_F (($f32x) { -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0. })

#[inline]
fn vrev21_vf_vf(vd: $f32x) -> $f32x {
  return ($f32x) {
    vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6],
      vd[9], vd[8], vd[11], vd[10], vd[13], vd[12], vd[15], vd[14] };
}


#[inline]
fn vcast_vo32_vo64(m: $ox) -> $ox {
  $ox ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = m[i*2+1];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret[i] = 0;
  return ret;
}

#[inline]
fn vcast_vo64_vo32(m: $ox) -> $ox {
  $ox ret;
  for(int i=0;i<VECTLENDP;i++) ret[i*2] = ret[i*2+1] = m[i];
  return ret;
}

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $ux {
  $ux ret;
  for(int i=0;i<VECTLENDP;i++) {
    ret[i*2+0] = l;
    ret[i*2+1] = h;
  }
  return ret;
}


#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix {
  $ix ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = vi2[i];
  return ret;
}

#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 {
  $ix2 ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = vi[i];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret[i] = 0;
  return ret;
}


#[inline]
fn vrev21_vf_vf(d0: $f32x) -> $f32x {
  $f32x r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r[i*2+0] = d0[i*2+1];
    r[i*2+1] = d0[i*2+0];
  }
  return r;
}




#[inline]
fn vtestallones_i_vo64(g: $ox) -> int {
  int ret = 1; for(int i=0;i<VECTLENDP*2;i++) ret = ret && g[i]; return ret;
}

#[inline]
fn vtestallones_i_vo32(g: $ox) -> int {
  int ret = 1; for(int i=0;i<VECTLENDP*2;i++) ret = ret && g[i]; return ret;
}

//

static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) {
  for(int i=0;i<VECTLENSP;i++) p[i] = v[i];
}


static void vstoreu_v_p_vi(int32_t *p, $ix v) {
  for(int i=0;i<VECTLENDP;i++) p[i] = v[i];
}

//

#[inline]
fn vandnot_vm_vm_vm(x: $ux, y: $ux) -> $ux { return y & ~x; }

#[inline]
fn vandnot_vo_vo_vo(x: $ox, y: $ox) -> $ox { return y & ~x; }

#[inline]
fn vand_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return x & y; }
#[inline]
fn vandnot_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return y & ~x; }
#[inline]
fn vor_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return x | y; }

#[inline]
fn vand_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return x & y; }
#[inline]
fn vandnot_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return y & ~x; }
#[inline]
fn vor_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return x | y; }

//

#[inline]
fn vsel_vd_vo_d_d(o: $ox, v1: f64, v0: f64) -> CONST -> $f64x {
  o.select($f64x::splat(v1), $f64x::splat(v0))
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $ox o0, o1: $ox, d0: f64, d1: f64, d2: f64) -> $f64x {
  o0.select($f64x::splat(d0), vsel_vd_vo_d_d(o1, d1, d2))
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $ox, o1: $ox, o2: $ox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  o0.select($f64x::splat(d0), o1.select($f64x::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)))
}

#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix {
#if defined(__clang__)
  return __builtin_convertvector(vd, $ix);
#else
  $ix vi;
  for(int i=0;i<VECTLENDP;i++) vi[i] = vd[i];
  return vi;
#endif
}
#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix { return vtruncate_vi_vd(($ox)(vd < 0.0).select(vd - 0.5, vd + 0.5)); }

impl Truncate for $f64x {
    #[inline]
    fn truncate(self) -> Self {
        $f64x::from(vtruncate_vi_vd(vd))
    }
}


impl RInt for $f64x {
    #[inline]
    fn rint(self) -> Self {
        $f64x::from(vrint_vi_vd(vd))
    }
}

#[inline]
fn veq64_vo_vm_vm(x: $ux, y: $ux) -> $ox {
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  return ($ox)((vi64)x == (vi64)y);
}

#[inline]
fn vadd64_vm_vm_vm(x: $ux, y: $ux) -> $ux {
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  return ($ux)((vi64)x + (vi64)y);
}

//

impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        1. / self
    }
}

impl Abs for $f64x {
    #[inline]
    fn abs(self) -> Self {
        ($f64x)(($ux)d & ~($ux)$f64x::splat(-0.));
    }
}
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        self * y + z
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        self * y - z
    }
}


#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return y & ~x; }

#[inline]
fn vand_vi_vo_vi(x: $ox, y: $ix) -> $ix { return vreinterpretFirstHalf_vi_vi2(($ix2)x) & y; }
#[inline]
fn vandnot_vi_vo_vi(x: $ox, y: $ix) -> $ix { return y & ~vreinterpretFirstHalf_vi_vi2(($ix2)x); }


#[inline]
fn vsrl_vi_vi_i(x: $ix, c: int) -> $ix {
#if defined(__clang__)
  typedef uint32_t vu __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef uint32_t vu __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP)));
#endif
  return ($ix)(((vu)x) >> c);
}


#[inline]
fn visinf_vo_vd(d: $f64x) -> $ox { return ($ox)(d.abs() == SLEEF_INFINITY); }
#[inline]
fn vispinf_vo_vd(d: $f64x) -> $ox { return ($ox)(d == SLEEF_INFINITY); }
#[inline]
fn visnan_vo_vd(d: $f64x) -> $ox { return ($ox)(d != d); }

impl Sqrt for $f64x {
    #[inline]
    fn sqrt(self) -> Self {
        
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  
  $f64x q = $f64x::splat(1);

  $ox o = ($ox)(d < 8.636168555094445E-78);
  d = ($f64x)((o & ($ux)(d * 1.157920892373162E77)) | (~o & ($ux)d));

  q = ($f64x)((o & ($ux)$f64x::splat(2.9387358770557188e-39)) | (~o & ($ux)$f64x::splat(1)));

  q = ($f64x)(d.lt($f64x::splat(0.)) | ($ux)q);
  
  $f64x x = ($f64x)(0x5fe6ec85e7de30daLL - ((vi64)(d + 1e-320) >> 1));
  x = x * (  3 - d * x * x);
  x = x * ( 12 - d * x * x);
  x = x * (768 - d * x * x);
  x *= 1.0 / (1 << 13);
  x = (d - (d * x) * (d * x)) * (x * 0.5) + d * x;

  x * q
    }
}

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x {
  $f64x vd;
  for(int i=0;i<VECTLENDP;i++) vd[i] = ptr[vi[i]];
  return vd;
}

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

#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 {
#if defined(__clang__)
  return __builtin_convertvector(vf, $ix2);
#else
  $ix2 vi;
  for(int i=0;i<VECTLENDP*2;i++) vi[i] = vf[i];
  return vi;
#endif
}

#[inline]
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { return vtruncate_vi2_vf(($ox)(vf < 0).select(vf - 0.5, vf + 0.5)); }
#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vrint_vi2_vf(vd)); }

#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf($f32x::from(i))); }
impl Rec for $f32x {
    #[inline]
    fn rec(self) -> Self {
        1 / self
    }
}

impl Abs for $f32x {
    fn abs(self) -> Self {
        ($f32x)vandnot_vm_vm_vm(($ux)$f32x::splat(-0.), ($ux)f)
    }
}

impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return z-x*y; }


#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return  y & ~x; }

#[inline]
fn vand_vi2_vo_vi2(x: $ox, y: $ix2) -> $ix2 { return ($ix2)x & y; }
#[inline]
fn vandnot_vi2_vo_vi2(x: $ox, y: $ix2) -> $ix2 { return y & ~($ix2)x; }

#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 {
#if defined(__clang__)
  typedef uint32_t vu __attribute__((ext_vector_type(VECTLENDP*2)));
#else
  typedef uint32_t vu __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));
#endif
  return ($ix2)(((vu)x) >> c);
}

#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x == y; }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x > y; }

#[inline]
fn visinf_vo_vf(d: $f32x) -> $ox { return ($ox)(d.abs() == SLEEF_INFINITYf); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $ox { return ($ox)(d == SLEEF_INFINITYf); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $ox { return ($ox)(d == -SLEEF_INFINITYf); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $ox { return ($ox)(d != d); }

impl Sqrt for $f32x {
    #[inline]
    fn sqrt(self) -> Self {
  $f32x q = $f32x::splat(1);

  $ox o = ($ox)(d < 5.4210108624275221700372640043497e-20); // 2^-64
  d = ($f32x)((o & ($ux)(d * $f32x::splat( 18446744073709551616.))) | (~o & ($ux)d)); // 2^64
  q = ($f32x)((o & ($ux)$f32x::splat(0.00000000023283064365386962890625)) | (~o & ($ux)$f32x::splat(1))); // 2^-32
  q = ($f32x)(d.lt($f32x::splat(0.)) | ($ux)q);
  
  $f32x x = ($f32x)(0x5f330de2 - ((($ix2)d) >> 1));
  x = x * ( 3. - d * x * x);
  x = x * (12. - d * x * x);
  x *= 0.0625;
  x = (d - (d * x) * (d * x)) * (x * 0.5) + d * x;

  return x * q;
}
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x {
  $f32x vf;
  for(int i=0;i<VECTLENSP;i++) vf[i] = ptr[vi2[i]];
  return vf;
}


//

#ifdef Sleef_quad2_DEFINED
#[inline]
fn vadd_vq_vq_vq(x: VQuad, y: VQuad) -> VQuad { return x + y; }
#[inline]
fn vsub_vq_vq_vq(x: VQuad, y: VQuad) -> VQuad { return x - y; }
#[inline]
fn vmul_vq_vq_vq(x: VQuad, y: VQuad) -> VQuad { return x * y; }

#[inline]
fn vneg_vq_vq(d: VQuad) -> VQuad { return -d; }
#[inline]
fn vsubadd_vq_vq_vq(x: VQuad, y: VQuad) -> VQuad { return vadd_vq_vq_vq(x, vnegpos_vq_vq(y)); }
#[inline]
fn vmlsubadd_vq_vq_vq_vq(VQuad x, VQuad y, VQuad z) -> VQuad { return vsubadd_vq_vq_vq(vmul_vq_vq_vq(x, y), z); }

