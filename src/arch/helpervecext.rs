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

typedef uint32_t $mx __attribute__((ext_vector_type(VECTLENDP*2)));
typedef uint32_t $mox __attribute__((ext_vector_type(VECTLENDP*2)));

typedef double $f64x __attribute__((ext_vector_type(VECTLENDP)));
typedef int32_t $ix __attribute__((ext_vector_type(VECTLENDP)));

typedef float $f32x __attribute__((ext_vector_type(VECTLENDP*2)));
typedef int32_t $ix2 __attribute__((ext_vector_type(VECTLENDP*2)));

/*#ifdef ENABLE_LONGDOUBLE
typedef uint8_t $mxL __attribute__((ext_vector_type(sizeof(long double)*VECTLENDP)));
typedef long double VLongDouble __attribute__((ext_vector_type(VECTLENDP)));
#endif*/

#ifdef Sleef_quad2_DEFINED
typedef uint8_t $mxq __attribute__((ext_vector_type(sizeof(Sleef_quad)*VECTLENDP)));
/*#ifdef ENABLE_LONGDOUBLE
typedef Sleef_quad VQuad __attribute__((ext_vector_type(VECTLENDP)));
#endif*/
#endif
#elif defined(__GNUC__)
#define ISANAME "GCC Vector Extension"

typedef uint32_t $mx __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));
typedef uint32_t $mox __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));

typedef double $f64x __attribute__((vector_size(sizeof(double)*VECTLENDP)));
typedef int32_t $ix __attribute__((vector_size(sizeof(int32_t)*VECTLENDP)));

typedef float $f32x __attribute__((vector_size(sizeof(float)*VECTLENDP*2)));
typedef int32_t $ix2 __attribute__((vector_size(sizeof(int32_t)*VECTLENDP*2)));

/*#ifdef ENABLE_LONGDOUBLE
typedef uint8_t $mxL __attribute__((vector_size(sizeof(long double)*VECTLENDP)));
typedef long double VLongDouble __attribute__((vector_size(sizeof(long double)*VECTLENDP)));
#endif*/

#ifdef Sleef_quad2_DEFINED
typedef uint8_t $mxq __attribute__((vector_size(sizeof(Sleef_quad)*VECTLENDP)));
typedef Sleef_quad VQuad __attribute__((vector_size(sizeof(Sleef_quad)*VECTLENDP)));
#endif
#endif

//

#if VECTLENDP == 2
#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox { return ($mox){ m[1], m[3], 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox { return ($mox){ m[0], m[0], m[1], m[1] }; }

/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vcast_vl_l(long double d) { return (VLongDouble) -> VLongDouble { d, d }; }
#endif*/
#ifdef Sleef_quad2_DEFINED
#[inline]
fn vcast_vq_q(Sleef_quad d) { return (VQuad) -> VQuad { d, d }; }
#endif

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $mx { return ($mx){ l, h, l, h }; }


#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix { return ($ix){ vi2[0], vi2[1] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 { return ($ix2){ vi[0], vi[1], 0, 0 }; }

#[inline]
fn vrev21_vd_vd(vd: $f64x) { return ($f64x) -> $f64x { vd[1], vd[0] }; }
#[inline]
fn vreva2_vd_vd(vd: $f64x) -> $f64x { return vd; }
#[inline]
fn vrev21_vf_vf(vd: $f32x) { return ($f32x) -> $f32x { vd[1], vd[0], vd[3], vd[2] }; }
#[inline]
fn vreva2_vf_vf(vd: $f32x) { return ($f32x) -> $f32x { vd[2], vd[3], vd[0], vd[1] }; }
/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vrev21_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { vd[1], vd[0] }; }
#[inline]
fn vreva2_vl_vl(vd: VLongDouble) -> VLongDouble { return vd; }
#[inline]
fn vposneg_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { +vd[0], -vd[1] }; }
#[inline]
fn vnegpos_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { -vd[0], +vd[1] }; }
#endif*/

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

#define PNMASK (($f64x) { +0.0, -0.0 })
#define NPMASK (($f64x) { -0.0, +0.0 })
#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return ($f64x)(($mx)d ^ ($mx)PNMASK); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return ($f64x)(($mx)d ^ ($mx)NPMASK); }

#define PNMASKf (($f32x) { +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf (($f32x) { -0.0f, +0.0f, -0.0f, +0.0f })
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return ($f32x)(($mx)d ^ ($mx)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return ($f32x)(($mx)d ^ ($mx)NPMASKf); }
#elif VECTLENDP == 4
#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox { return ($mox){ m[1], m[3], m[5], m[7], 0, 0, 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox { return ($mox){ m[0], m[0], m[1], m[1], m[2], m[2], m[3], m[3] }; }

/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vcast_vl_l(long double d) { return (VLongDouble) -> VLongDouble { d, d, d, d }; }
#endif*/

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $mx { return ($mx){ l, h, l, h, l, h, l, h }; }

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix { return ($ix){ vi2[0], vi2[1], vi2[2], vi2[3] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 { return ($ix2){ vi[0], vi[1], vi[2], vi[3], 0, 0, 0, 0 }; }

#define PNMASK (($f64x) { +0.0, -0.0, +0.0, -0.0 })
#define NPMASK (($f64x) { -0.0, +0.0, -0.0, +0.0 })
#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return ($f64x)(($mx)d ^ ($mx)PNMASK); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return ($f64x)(($mx)d ^ ($mx)NPMASK); }

#define PNMASKf (($f32x) { +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf (($f32x) { -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f })
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return ($f32x)(($mx)d ^ ($mx)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return ($f32x)(($mx)d ^ ($mx)NPMASKf); }

#[inline]
fn vrev21_vd_vd(vd: $f64x) { return ($f64x) -> $f64x { vd[1], vd[0], vd[3], vd[2] }; }
#[inline]
fn vreva2_vd_vd(vd: $f64x) { return ($f64x) -> $f64x { vd[2], vd[3], vd[0], vd[1] }; }
#[inline]
fn vrev21_vf_vf(vd: $f32x) { return ($f32x) -> $f32x { vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6] }; }
#[inline]
fn vreva2_vf_vf(vd: $f32x) { return ($f32x) -> $f32x { vd[6], vd[7], vd[4], vd[5], vd[2], vd[3], vd[0], vd[1] }; }
/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vrev21_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { vd[1], vd[0], vd[3], vd[2] }; }
#[inline]
fn vreva2_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { vd[2], vd[3], vd[0], vd[1] }; }
#[inline]
fn vposneg_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { +vd[0], -vd[1], +vd[2], -vd[3] }; }
#[inline]
fn vnegpos_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { -vd[0], +vd[1], -vd[2], +vd[3] }; }
#endif*/
#elif VECTLENDP == 8
#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox { return ($mox){ m[1], m[3], m[5], m[7], m[9], m[11], m[13], m[15], 0, 0, 0, 0, 0, 0, 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox { return ($mox){ m[0], m[0], m[1], m[1], m[2], m[2], m[3], m[3], m[4], m[4], m[5], m[5], m[6], m[6], m[7], m[7] }; }

/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vcast_vl_l(long double d) { return (VLongDouble) -> VLongDouble { d, d, d, d, d, d, d, d }; }
#endif*/

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $mx { return ($mx){ l, h, l, h, l, h, l, h, l, h, l, h, l, h, l, h }; }

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix { return ($ix){ vi2[0], vi2[1], vi2[2], vi2[3], vi2[4], vi2[5], vi2[6], vi2[7] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 { return ($ix2){ vi[0], vi[1], vi[2], vi[3], vi[4], vi[5], vi[6], vi[7], 0, 0, 0, 0, 0, 0, 0, 0 }; }

#define PNMASK (($f64x) { 0., -0., 0., -0., 0., -0., 0., -0. })
#define NPMASK (($f64x) { -0., 0., -0., 0., -0., 0., -0., 0. })
#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return ($f64x)(($mx)d ^ ($mx)PNMASK); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return ($f64x)(($mx)d ^ ($mx)NPMASK); }

#define PNMASKf (($f32x) { 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0. })
#define NPMASKf (($f32x) { -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0. })
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return ($f32x)(($mx)d ^ ($mx)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return ($f32x)(($mx)d ^ ($mx)NPMASKf); }

#[inline]
fn vrev21_vd_vd(vd: $f64x) { return ($f64x) -> $f64x { vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6] }; }
#[inline]
fn vreva2_vd_vd(vd: $f64x) { return ($f64x) -> $f64x { vd[6], vd[7], vd[4], vd[5], vd[2], vd[3], vd[0], vd[1] }; }
#[inline]
fn vrev21_vf_vf(vd: $f32x) -> $f32x {
  return ($f32x) {
    vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6],
      vd[9], vd[8], vd[11], vd[10], vd[13], vd[12], vd[15], vd[14] };
}
#[inline]
fn vreva2_vf_vf(vd: $f32x) -> $f32x {
  return ($f32x) {
    vd[14], vd[15], vd[12], vd[13], vd[10], vd[11], vd[8], vd[9],
      vd[6], vd[7], vd[4], vd[5], vd[2], vd[3], vd[0], vd[1]};
}
/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vrev21_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6] }; }
#[inline]
fn vreva2_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { vd[6], vd[7], vd[4], vd[5], vd[2], vd[3], vd[0], vd[1] }; }
#[inline]
fn vposneg_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { +vd[0], -vd[1], +vd[2], -vd[3], +vd[4], -vd[5], +vd[6], -vd[7] }; }
#[inline]
fn vnegpos_vl_vl(vd: VLongDouble) { return (VLongDouble) -> VLongDouble { -vd[0], +vd[1], -vd[2], +vd[3], -vd[4], +vd[5], -vd[6], +vd[7] }; }
#endif
#else*/


/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vcast_vl_l(long double d) -> VLongDouble {
  VLongDouble ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = d;
  return ret;
}
#endif*/

#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox {
  $mox ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = m[i*2+1];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret[i] = 0;
  return ret;
}

#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox {
  $mox ret;
  for(int i=0;i<VECTLENDP;i++) ret[i*2] = ret[i*2+1] = m[i];
  return ret;
}

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $mx {
  $mx ret;
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
fn vrev21_vd_vd(d0: $f64x) -> $f64x {
  $f64x r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = d0[i*2+1];
    r[i*2+1] = d0[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vd_vd(d0: $f64x) -> $f64x {
  $f64x r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = d0[(VECTLENDP/2-1-i)*2+0];
    r[i*2+1] = d0[(VECTLENDP/2-1-i)*2+1];
  }
  return r;
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
fn vreva2_vf_vf(d0: $f32x) -> $f32x {
  $f32x r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r[i*2+0] = d0[(VECTLENSP/2-1-i)*2+0];
    r[i*2+1] = d0[(VECTLENSP/2-1-i)*2+1];
  }
  return r;
}

/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vrev21_vl_vl(d0: VLongDouble) -> VLongDouble {
  VLongDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = d0[i*2+1];
    r[i*2+1] = d0[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vl_vl(d0: VLongDouble) -> VLongDouble {
  VLongDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = d0[(VECTLENDP/2-1-i)*2+0];
    r[i*2+1] = d0[(VECTLENDP/2-1-i)*2+1];
  }
  return r;
}
#endif*/

#[inline]
fn vposneg_vd_vd(d0: $f64x) -> $f64x {
  $f64x r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = +d0[i*2+0];
    r[i*2+1] = -d0[i*2+1];
  }
  return r;
}

#[inline]
fn vnegpos_vd_vd(d0: $f64x) -> $f64x {
  $f64x r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = -d0[i*2+0];
    r[i*2+1] = +d0[i*2+1];
  }
  return r;
}

#[inline]
fn vposneg_vf_vf(d0: $f32x) -> $f32x {
  $f32x r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r[i*2+0] = +d0[i*2+0];
    r[i*2+1] = -d0[i*2+1];
  }
  return r;
}

#[inline]
fn vnegpos_vf_vf(d0: $f32x) -> $f32x {
  $f32x r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r[i*2+0] = -d0[i*2+0];
    r[i*2+1] = +d0[i*2+1];
  }
  return r;
}

/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vposneg_vl_vl(d0: VLongDouble) -> VLongDouble {
  VLongDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = +d0[i*2+0];
    r[i*2+1] = -d0[i*2+1];
  }
  return r;
}

#[inline]
fn vnegpos_vl_vl(d0: VLongDouble) -> VLongDouble {
  VLongDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = -d0[i*2+0];
    r[i*2+1] = +d0[i*2+1];
  }
  return r;
}
#endif*/
//#endif

//

#[inline]
fn vavailability_i(name: int) -> int { return -1; }
#[inline]
fn vprefetch_v_p(const void *ptr) -> void { }

#[inline]
fn vtestallones_i_vo64(g: $mox) -> int {
  int ret = 1; for(int i=0;i<VECTLENDP*2;i++) ret = ret && g[i]; return ret;
}

#[inline]
fn vtestallones_i_vo32(g: $mox) -> int {
  int ret = 1; for(int i=0;i<VECTLENDP*2;i++) ret = ret && g[i]; return ret;
}

//

static $ix2 vloadu_vi2_p(int32_t *p) {
  $ix2 vi;
  for(int i=0;i<VECTLENSP;i++) vi[i] = p[i];
  return vi;
}

static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) {
  for(int i=0;i<VECTLENSP;i++) p[i] = v[i];
}

static $ix vloadu_vi_p(int32_t *p) {
  $ix vi;
  for(int i=0;i<VECTLENDP;i++) vi[i] = p[i];
  return vi;
}

static void vstoreu_v_p_vi(int32_t *p, $ix v) {
  for(int i=0;i<VECTLENDP;i++) p[i] = v[i];
}

//

#[inline]
fn vand_vm_vm_vm(x: $mx, y: $mx) -> $mx { return x & y; }
#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx { return y & ~x; }
#[inline]
fn vor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return x | y; }
#[inline]
fn vxor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return x ^ y; }

//#[inline]
//fn vand_vo_vo_vo(x: $mox, y: $mox) -> $mox { return x & y; }
#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { return y & ~x; }
//#[inline]
//fn vor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return x | y; }
//#[inline]
//fn vxor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return x ^ y; }

#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return x & y; }
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return y & ~x; }
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return x | y; }
#[inline]
fn vxor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return x ^ y; }

#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return x & y; }
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return y & ~x; }
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return x | y; }
#[inline]
fn vxor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return x ^ y; }

//

#[inline]
fn vsel_vd_vo_vd_vd(o: $mox, x: $f64x, y: $f64x) -> $f64x { return ($f64x)((($mx)o & ($mx)x) | (($mx)y & ~($mx)o)); }
#[inline]
fn vsel_vi2_vo_vi2_vi2(o: $mox, x: $ix2, y: $ix2) -> $ix2 { return ($ix2)((($mx)o & ($mx)x) | (($mx)y & ~($mx)o)); }

#[inline]
fn vsel_vd_vo_d_d(o: $mox, v1: f64, v0: f64) -> CONST -> $f64x {
  return vsel_vd_vo_vd_vd(o, $f64x::splat(v1), $f64x::splat(v0));
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $mox o0, o1: $mox, d0: f64, d1: f64, d2: f64) -> $f64x {
  return vsel_vd_vo_vd_vd(o0, $f64x::splat(d0), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $mox, o1: $mox, o2: $mox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  return vsel_vd_vo_vd_vd(o0, $f64x::splat(d0), vsel_vd_vo_vd_vd(o1, $f64x::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)));
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
fn vrint_vi_vd(vd: $f64x) -> $ix { return vtruncate_vi_vd(vsel_vd_vo_vd_vd(($mox)(vd < 0.0), vd - 0.5, vd + 0.5)); }
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return $f64x::from(vtruncate_vi_vd(vd)); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return $f64x::from(vrint_vi_vd(vd)); }

#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox {
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  return ($mox)((vi64)x == (vi64)y);
}

#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx {
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  return ($mx)((vi64)x + (vi64)y);
}

//

#[inline]
fn vreinterpret_vm_vd(vd: $f64x) -> $mx { return ($mx)vd; }
#[inline]
fn vreinterpret_vi2_vd(vd: $f64x) -> $ix2 { return ($ix2)vd; }
#[inline]
fn vreinterpret_vd_vi2(vi: $ix2) -> $f64x { return ($f64x)vi; }
#[inline]
fn vreinterpret_vd_vm(vm: $mx) -> $f64x { return ($f64x)vm; }


#[inline]
fn vrec_vd_vd(x: $f64x) -> $f64x { return 1.0 / x; }

#[inline]
fn vabs_vd_vd(d: $f64x) -> $f64x { return ($f64x)(($mx)d & ~($mx)$f64x::splat(-0.)); }
impl Mla for $f64x {
    fn mla(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x * y - z; }

#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return x + vnegpos_vd_vd(y); }
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vsubadd_vd_vd_vd(x*y, z); }


impl std::ops::Add for $ix {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        self + other
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: $ix, y: $ix) -> $ix { return x + y; }
impl std::ops::Sub for $ix {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        self - other
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: $ix, y: $ix) -> $ix { return x - y; }
impl std::ops::Neg for $ix {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        -self
    }
}
//#[inline]
//fn vneg_vi_vi(e: $ix) -> $ix { return -e; }

#[inline]
fn vand_vi_vi_vi(x: $ix, y: $ix) -> $ix { return x & y; }
#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return y & ~x; }
#[inline]
fn vor_vi_vi_vi(x: $ix, y: $ix) -> $ix { return x | y; }
#[inline]
fn vxor_vi_vi_vi(x: $ix, y: $ix) -> $ix { return x ^ y; }

#[inline]
fn vand_vi_vo_vi(x: $mox, y: $ix) -> $ix { return vreinterpretFirstHalf_vi_vi2(($ix2)x) & y; }
#[inline]
fn vandnot_vi_vo_vi(x: $mox, y: $ix) -> $ix { return y & ~vreinterpretFirstHalf_vi_vi2(($ix2)x); }

#[inline]
fn vsll_vi_vi_i(x: $ix, c: int) -> $ix {
#if defined(__clang__)
  typedef uint32_t vu __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef uint32_t vu __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP)));
#endif
  return ($ix)(((vu)x) << c);
}

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
fn vsra_vi_vi_i(x: $ix, c: int) -> $ix { return x >> c; }

#[inline]
fn veq_vi_vi_vi(x: $ix, y: $ix) -> $ix { return x == y; }
#[inline]
fn vgt_vi_vi_vi(x: $ix, y: $ix) -> $ix { return x > y; }

#[inline]
fn veq_vo_vi_vi(x: $ix, y: $ix) -> $mox { return ($mox)vreinterpretFirstHalf_vi2_vi(x == y); }
#[inline]
fn vgt_vo_vi_vi(x: $ix, y: $ix) -> $mox { return ($mox)vreinterpretFirstHalf_vi2_vi(x > y);}

#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix {
  return vor_vi_vi_vi(vand_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(($ix2)m), x),
		      vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(($ix2)m), y));
}

#[inline]
fn visinf_vo_vd(d: $f64x) -> $mox { return ($mox)(vabs_vd_vd(d) == SLEEF_INFINITY); }
#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox { return ($mox)(d == SLEEF_INFINITY); }
#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox { return ($mox)(d == -SLEEF_INFINITY); }
#[inline]
fn visnan_vo_vd(d: $f64x) -> $mox { return ($mox)(d != d); }

#[inline]
fn vsqrt_vd_vd(d: $f64x) -> $f64x {
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  
  $f64x q = $f64x::splat(1);

  $mox o = ($mox)(d < 8.636168555094445E-78);
  d = ($f64x)((o & ($mx)(d * 1.157920892373162E77)) | (~o & ($mx)d));

  q = ($f64x)((o & ($mx)$f64x::splat(2.9387358770557188e-39)) | (~o & ($mx)$f64x::splat(1)));

  q = ($f64x)vor_vm_vm_vm(d.lt($f64x::splat(0.)), ($mx)q);
  
  $f64x x = ($f64x)(0x5fe6ec85e7de30daLL - ((vi64)(d + 1e-320) >> 1));
  x = x * (  3 - d * x * x);
  x = x * ( 12 - d * x * x);
  x = x * (768 - d * x * x);
  x *= 1.0 / (1 << 13);
  x = (d - (d * x) * (d * x)) * (x * 0.5) + d * x;

  return x * q;
}

#[inline]
fn vcast_d_vd(v: $f64x) -> double { return v[0]; }
#[inline]
fn vcast_f_vf(v: $f32x) -> float { return v[0]; }

#[inline]
fn vload_vd_p(const double *ptr) -> $f64x { return *($f64x *)ptr; }
#[inline]
fn vloadu_vd_p(const double *ptr) -> $f64x {
  $f64x vd;
  for(int i=0;i<VECTLENDP;i++) vd[i] = ptr[i];
  return vd;
}

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x {
  $f64x vd;
  for(int i=0;i<VECTLENDP;i++) vd[i] = ptr[vi[i]];
  return vd;
}

#[inline]
fn vstore_v_p_vd(double *ptr, $f64x v) -> void { *($f64x *)ptr = v; }
#[inline]
fn vstoreu_v_p_vd(double *ptr, $f64x v) -> void {
  for(int i=0;i<VECTLENDP;i++) ptr[i] = v[i];
}
//#[inline]
//fn vstream_v_p_vd(double *ptr, $f64x v) -> void { *($f64x *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void {
//  for(int i=0;i<VECTLENDP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void { vscatter2_v_p_i_i_vd(ptr, offset, step, v); }

//

#[inline]
fn vsel_vf_vo_vf_vf(o: $mox, x: $f32x, y: $f32x) -> $f32x { return ($f32x)((($mx)o & ($mx)x) | (~($mx)o & ($mx)y)); }

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
fn vcast_vi2_vm(vm: $mx) -> $ix2 { return ($ix2)vm; }
#[inline]
fn vcast_vm_vi2(vi: $ix2) -> $mx { return ($mx)vi; }


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
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { return vtruncate_vi2_vf(vsel_vf_vo_vf_vf(($mox)(vf < 0), vf - 0.5f, vf + 0.5)); }
#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vrint_vi2_vf(vd)); }

#[inline]
fn vreinterpret_vm_vf(vf: $f32x) -> $mx { return ($mx)vf; }
#[inline]
fn vreinterpret_vf_vm(vm: $mx) -> $f32x { return ($f32x)vm; }
#[inline]
fn vreinterpret_vf_vi2(vi: $ix2) -> $f32x { return ($f32x)vi; }
#[inline]
fn vreinterpret_vi2_vf(vf: $f32x) -> $ix2 { return ($ix2)vf; }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vrec_vf_vf(x: $f32x) -> $f32x { return 1. / x; }

#[inline]
fn vabs_vf_vf(f: $f32x) -> $f32x { return ($f32x)vandnot_vm_vm_vm(($mx)$f32x::splat(-0.), ($mx)f); }

impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return z-x*y; }

#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { return x + vnegpos_vf_vf(y); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vsubadd_vf_vf_vf(x * y, z); }

#[inline]
fn vadd_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x + y; }
#[inline]
fn vsub_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x - y; }
#[inline]
fn vneg_vi2_vi2(e: $ix2) -> $ix2 { return -e; }

#[inline]
fn vand_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x & y; }
#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return  y & ~x; }
#[inline]
fn vor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x | y; }
#[inline]
fn vxor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x ^ y; }

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return ($ix2)x & y; }
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return y & ~($ix2)x; }

#[inline]
fn vsll_vi2_vi2_i(x: $ix2, c: int) -> $ix2 {
#if defined(__clang__)
  typedef uint32_t vu __attribute__((ext_vector_type(VECTLENDP*2)));
#else
  typedef uint32_t vu __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));
#endif
  return ($ix2)(((vu)x) << c);
}
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
fn vsra_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return x >> c; }

#[inline]
fn veq_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return ($mox)(x == y); }
#[inline]
fn vgt_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return ($mox)(x > y); }
#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x == y; }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return x > y; }

#[inline]
fn visinf_vo_vf(d: $f32x) -> $mox { return ($mox)(vabs_vf_vf(d) == SLEEF_INFINITYf); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { return ($mox)(d == SLEEF_INFINITYf); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { return ($mox)(d == -SLEEF_INFINITYf); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return ($mox)(d != d); }

#[inline]
fn vsqrt_vf_vf(d: $f32x) -> $f32x {
  $f32x q = $f32x::splat(1);

  $mox o = ($mox)(d < 5.4210108624275221700372640043497e-20); // 2^-64
  d = ($f32x)((o & ($mx)(d * $f32x::splat( 18446744073709551616.))) | (~o & ($mx)d)); // 2^64
  q = ($f32x)((o & ($mx)$f32x::splat(0.00000000023283064365386962890625)) | (~o & ($mx)$f32x::splat(1))); // 2^-32
  q = ($f32x)vor_vm_vm_vm(d.lt($f32x::splat(0.)), ($mx)q);
  
  $f32x x = ($f32x)(0x5f330de2 - ((($ix2)d) >> 1));
  x = x * ( 3. - d * x * x);
  x = x * (12. - d * x * x);
  x *= 0.0625;
  x = (d - (d * x) * (d * x)) * (x * 0.5) + d * x;

  return x * q;
}

#[inline]
fn vload_vf_p(const float *ptr) -> $f32x { return *($f32x *)ptr; }
#[inline]
fn vloadu_vf_p(const float *ptr) -> $f32x {
  $f32x vf;
  for(int i=0;i<VECTLENSP;i++) vf[i] = ptr[i];
  return vf;
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x {
  $f32x vf;
  for(int i=0;i<VECTLENSP;i++) vf[i] = ptr[vi2[i]];
  return vf;
}

#[inline]
fn vstore_v_p_vf(float *ptr, $f32x v) -> void { *($f32x *)ptr = v; }
#[inline]
fn vstoreu_v_p_vf(float *ptr, $f32x v) -> void {
  for(int i=0;i<VECTLENSP;i++) ptr[i] = v[i];
}
//#[inline]
//fn vstream_v_p_vf(float *ptr, $f32x v) -> void { *($f32x *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
//  for(int i=0;i<VECTLENSP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }

//

/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vadd_vl_vl_vl(VLongDouble x, VLongDouble y) -> VLongDouble { return x + y; }
#[inline]
fn vsub_vl_vl_vl(VLongDouble x, VLongDouble y) -> VLongDouble { return x - y; }
#[inline]
fn vmul_vl_vl_vl(VLongDouble x, VLongDouble y) -> VLongDouble { return x * y; }

#[inline]
fn vneg_vl_vl(d: VLongDouble) -> VLongDouble { return -d; }
#[inline]
fn vsubadd_vl_vl_vl(VLongDouble x, VLongDouble y) -> VLongDouble { return vadd_vl_vl_vl(x, vnegpos_vl_vl(y)); }
#[inline]
fn vmlsubadd_vl_vl_vl_vl(VLongDouble x, VLongDouble y, VLongDouble z) -> VLongDouble { return vsubadd_vl_vl_vl(vmul_vl_vl_vl(x, y), z); }

#[inline]
fn vload_vl_p(const long double *ptr) -> VLongDouble { return *(VLongDouble *)ptr; }
#[inline]
fn vloadu_vl_p(const long double *ptr) -> VLongDouble {
  VLongDouble vd;
  for(int i=0;i<VECTLENDP;i++) vd[i] = ptr[i];
  return vd;
}

#[inline]
fn vstore_v_p_vl(long double *ptr, VLongDouble v) -> void { *(VLongDouble *)ptr = v; }
#[inline]
fn vstoreu_v_p_vl(long double *ptr, VLongDouble v) -> void {
  for(int i=0;i<VECTLENDP;i++) ptr[i] = v[i];
}
#[inline]
fn vstream_v_p_vl(long double *ptr, VLongDouble v) -> void { *(VLongDouble *)ptr = v; }

#[inline]
fn vscatter2_v_p_i_i_vl(long double *ptr, int offset, int step, VLongDouble v) -> void {
  for(int i=0;i<VECTLENDP/2;i++) {
    *(ptr+(offset + step * i)*2 + 0) = v[i*2+0];
    *(ptr+(offset + step * i)*2 + 1) = v[i*2+1];
  }
}

#[inline]
fn vsscatter2_v_p_i_i_vl(long double *ptr, int offset, int step, VLongDouble v) -> void { vscatter2_v_p_i_i_vl(ptr, offset, step, v); }
#endif*/

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

#[inline]
fn vload_vq_p(const Sleef_quad *ptr) -> VQuad { return *(VQuad *)ptr; }
#[inline]
fn vloadu_vq_p(const Sleef_quad *ptr) -> VQuad {
  VQuad vd;
  for(int i=0;i<VECTLENDP;i++) vd[i] = ptr[i];
  return vd;
}

#[inline]
fn vstore_v_p_vq(Sleef_quad *ptr, VQuad v) -> void { *(VQuad *)ptr = v; }
#[inline]
fn vstoreu_v_p_vq(Sleef_quad *ptr, VQuad v) -> void {
  for(int i=0;i<VECTLENDP;i++) ptr[i] = v[i];
}
//#[inline]
//fn vstream_v_p_vq(Sleef_quad *ptr, VQuad v) -> void { *(VQuad *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vq(Sleef_quad *ptr, int offset, int step, VQuad v) -> void {
//  for(int i=0;i<VECTLENDP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vq(Sleef_quad *ptr, int offset, int step, VQuad v) -> void { vscatter2_v_p_i_i_vq(ptr, offset, step, v); }
#endif
