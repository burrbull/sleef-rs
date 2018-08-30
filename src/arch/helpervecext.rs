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

typedef uint32_t VMask __attribute__((ext_vector_type(VECTLENDP*2)));
typedef uint32_t VOpMask __attribute__((ext_vector_type(VECTLENDP*2)));

typedef double VDouble __attribute__((ext_vector_type(VECTLENDP)));
typedef int32_t VInt __attribute__((ext_vector_type(VECTLENDP)));

typedef float VFloat __attribute__((ext_vector_type(VECTLENDP*2)));
typedef int32_t VInt2 __attribute__((ext_vector_type(VECTLENDP*2)));

/*#ifdef ENABLE_LONGDOUBLE
typedef uint8_t VMaskL __attribute__((ext_vector_type(sizeof(long double)*VECTLENDP)));
typedef long double VLongDouble __attribute__((ext_vector_type(VECTLENDP)));
#endif*/

#ifdef Sleef_quad2_DEFINED
typedef uint8_t VMaskq __attribute__((ext_vector_type(sizeof(Sleef_quad)*VECTLENDP)));
/*#ifdef ENABLE_LONGDOUBLE
typedef Sleef_quad VQuad __attribute__((ext_vector_type(VECTLENDP)));
#endif*/
#endif
#elif defined(__GNUC__)
#define ISANAME "GCC Vector Extension"

typedef uint32_t VMask __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));
typedef uint32_t VOpMask __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));

typedef double VDouble __attribute__((vector_size(sizeof(double)*VECTLENDP)));
typedef int32_t VInt __attribute__((vector_size(sizeof(int32_t)*VECTLENDP)));

typedef float VFloat __attribute__((vector_size(sizeof(float)*VECTLENDP*2)));
typedef int32_t VInt2 __attribute__((vector_size(sizeof(int32_t)*VECTLENDP*2)));

/*#ifdef ENABLE_LONGDOUBLE
typedef uint8_t VMaskL __attribute__((vector_size(sizeof(long double)*VECTLENDP)));
typedef long double VLongDouble __attribute__((vector_size(sizeof(long double)*VECTLENDP)));
#endif*/

#ifdef Sleef_quad2_DEFINED
typedef uint8_t VMaskq __attribute__((vector_size(sizeof(Sleef_quad)*VECTLENDP)));
typedef Sleef_quad VQuad __attribute__((vector_size(sizeof(Sleef_quad)*VECTLENDP)));
#endif
#endif

//

#if VECTLENDP == 2
#[inline]
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask { return (VOpMask){ m[1], m[3], 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask { return (VOpMask){ m[0], m[0], m[1], m[1] }; }

impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        VInt::new(self, self)
    }
}
//#[inline]
//fn vcast_vi_i(i: int) { return (VInt) -> VInt { i, i }; }
#[inline]
fn vcast_vi2_i(i: int) { return (VInt2) -> VInt2 { i, i, i, i }; }

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        VFloat { self, self, self, self }
    }
}

impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        VDouble { d, d }
    }
}
/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vcast_vl_l(long double d) { return (VLongDouble) -> VLongDouble { d, d }; }
#endif*/
#ifdef Sleef_quad2_DEFINED
#[inline]
fn vcast_vq_q(Sleef_quad d) { return (VQuad) -> VQuad { d, d }; }
#endif

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> VMask { return (VMask){ l, h, l, h }; }
impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        VInt2::new( 0, vi[0], 0, vi[1] )
    }
}
//#[inline]
//fn vcastu_vi2_vi(vi: VInt) -> VInt2 { return (VInt2){ 0, vi[0], 0, vi[1] }; }
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
      VInt::new( vi2[1], vi2[3] )
    }
}
//#[inline]
//fn vcastu_vi_vi2(vi2: VInt2) -> VInt { return (VInt){ vi2[1], vi2[3] }; }

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: VInt2) -> VInt { return (VInt){ vi2[0], vi2[1] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: VInt) -> VInt2 { return (VInt2){ vi[0], vi[1], 0, 0 }; }

#[inline]
fn vrev21_vd_vd(vd: VDouble) { return (VDouble) -> VDouble { vd[1], vd[0] }; }
#[inline]
fn vreva2_vd_vd(vd: VDouble) -> VDouble { return vd; }
#[inline]
fn vrev21_vf_vf(vd: VFloat) { return (VFloat) -> VFloat { vd[1], vd[0], vd[3], vd[2] }; }
#[inline]
fn vreva2_vf_vf(vd: VFloat) { return (VFloat) -> VFloat { vd[2], vd[3], vd[0], vd[1] }; }
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

#define PNMASK ((VDouble) { +0.0, -0.0 })
#define NPMASK ((VDouble) { -0.0, +0.0 })
#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble { return (VDouble)((VMask)d ^ (VMask)PNMASK); }
#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble { return (VDouble)((VMask)d ^ (VMask)NPMASK); }

#define PNMASKf ((VFloat) { +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf ((VFloat) { -0.0f, +0.0f, -0.0f, +0.0f })
#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat { return (VFloat)((VMask)d ^ (VMask)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat { return (VFloat)((VMask)d ^ (VMask)NPMASKf); }
#elif VECTLENDP == 4
#[inline]
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask { return (VOpMask){ m[1], m[3], m[5], m[7], 0, 0, 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask { return (VOpMask){ m[0], m[0], m[1], m[1], m[2], m[2], m[3], m[3] }; }
impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        VInt::new(self, self, self, self)
    }
}
//#[inline]
//fn vcast_vi_i(i: int) { return (VInt) -> VInt { i, i, i, i }; }
#[inline]
fn vcast_vi2_i(i: int) { return (VInt2) -> VInt2 { i, i, i, i, i, i, i, i }; }

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        VFloat { self, self, self, self, self, self, self, self }
    }
}
impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        VDouble { self, self, self, self }
    }
}
/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vcast_vl_l(long double d) { return (VLongDouble) -> VLongDouble { d, d, d, d }; }
#endif*/

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> VMask { return (VMask){ l, h, l, h, l, h, l, h }; }
impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        VInt2::new( 0, vi[0], 0, vi[1], 0, vi[2], 0, vi[3] )
    }
}
//#[inline]
//fn vcastu_vi2_vi(vi: VInt) -> VInt2 { return (VInt2){ 0, vi[0], 0, vi[1], 0, vi[2], 0, vi[3] }; }
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
      VInt::new( vi2[1], vi2[3], vi2[5], vi2[7] )
    }
}
//#[inline]
//fn vcastu_vi_vi2(vi2: VInt2) -> VInt { return (VInt){ vi2[1], vi2[3], vi2[5], vi2[7] }; }

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: VInt2) -> VInt { return (VInt){ vi2[0], vi2[1], vi2[2], vi2[3] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: VInt) -> VInt2 { return (VInt2){ vi[0], vi[1], vi[2], vi[3], 0, 0, 0, 0 }; }

#define PNMASK ((VDouble) { +0.0, -0.0, +0.0, -0.0 })
#define NPMASK ((VDouble) { -0.0, +0.0, -0.0, +0.0 })
#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble { return (VDouble)((VMask)d ^ (VMask)PNMASK); }
#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble { return (VDouble)((VMask)d ^ (VMask)NPMASK); }

#define PNMASKf ((VFloat) { +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf ((VFloat) { -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f, -0.0f, +0.0f })
#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat { return (VFloat)((VMask)d ^ (VMask)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat { return (VFloat)((VMask)d ^ (VMask)NPMASKf); }

#[inline]
fn vrev21_vd_vd(vd: VDouble) { return (VDouble) -> VDouble { vd[1], vd[0], vd[3], vd[2] }; }
#[inline]
fn vreva2_vd_vd(vd: VDouble) { return (VDouble) -> VDouble { vd[2], vd[3], vd[0], vd[1] }; }
#[inline]
fn vrev21_vf_vf(vd: VFloat) { return (VFloat) -> VFloat { vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6] }; }
#[inline]
fn vreva2_vf_vf(vd: VFloat) { return (VFloat) -> VFloat { vd[6], vd[7], vd[4], vd[5], vd[2], vd[3], vd[0], vd[1] }; }
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
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask { return (VOpMask){ m[1], m[3], m[5], m[7], m[9], m[11], m[13], m[15], 0, 0, 0, 0, 0, 0, 0, 0 }; }
#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask { return (VOpMask){ m[0], m[0], m[1], m[1], m[2], m[2], m[3], m[3], m[4], m[4], m[5], m[5], m[6], m[6], m[7], m[7] }; }
impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        VInt::new(self, self, self, self, self, self, self, self)
    }
}
//#[inline]
//fn vcast_vi_i(i: int) { return (VInt) -> VInt { i, i, i, i, i, i, i, i }; }
#[inline]
fn vcast_vi2_i(i: int) { return (VInt2) -> VInt2 { i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i }; }
impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        VFloat { self, self, self, self, self, self, self, self, self, self, self, self, self, self, self, self }
    }
}
impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
        VDouble { self, self, self, self, self, self, self, self }
    }
}
/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vcast_vl_l(long double d) { return (VLongDouble) -> VLongDouble { d, d, d, d, d, d, d, d }; }
#endif*/

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> VMask { return (VMask){ l, h, l, h, l, h, l, h, l, h, l, h, l, h, l, h }; }
impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        VInt2::new( 0, vi[0], 0, vi[1], 0, vi[2], 0, vi[3], 0, vi[4], 0, vi[5], 0, vi[6], 0, vi[7] } )
    }
}
//#[inline]
//fn vcastu_vi2_vi(vi: VInt) -> VInt2 { return (VInt2){ 0, vi[0], 0, vi[1], 0, vi[2], 0, vi[3], 0, vi[4], 0, vi[5], 0, vi[6], 0, vi[7] }; }
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
      VInt::new( vi2[1], vi2[3], vi2[5], vi2[7], vi2[9], vi2[11], vi2[13], vi2[15] )
    }
}
//#[inline]
//fn vcastu_vi_vi2(vi2: VInt2) -> VInt { return (VInt){ vi2[1], vi2[3], vi2[5], vi2[7], vi2[9], vi2[11], vi2[13], vi2[15] }; }

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: VInt2) -> VInt { return (VInt){ vi2[0], vi2[1], vi2[2], vi2[3], vi2[4], vi2[5], vi2[6], vi2[7] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: VInt) -> VInt2 { return (VInt2){ vi[0], vi[1], vi[2], vi[3], vi[4], vi[5], vi[6], vi[7], 0, 0, 0, 0, 0, 0, 0, 0 }; }

#define PNMASK ((VDouble) { 0., -0., 0., -0., 0., -0., 0., -0. })
#define NPMASK ((VDouble) { -0., 0., -0., 0., -0., 0., -0., 0. })
#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble { return (VDouble)((VMask)d ^ (VMask)PNMASK); }
#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble { return (VDouble)((VMask)d ^ (VMask)NPMASK); }

#define PNMASKf ((VFloat) { 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0. })
#define NPMASKf ((VFloat) { -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0., -0., 0. })
#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat { return (VFloat)((VMask)d ^ (VMask)PNMASKf); }
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat { return (VFloat)((VMask)d ^ (VMask)NPMASKf); }

#[inline]
fn vrev21_vd_vd(vd: VDouble) { return (VDouble) -> VDouble { vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6] }; }
#[inline]
fn vreva2_vd_vd(vd: VDouble) { return (VDouble) -> VDouble { vd[6], vd[7], vd[4], vd[5], vd[2], vd[3], vd[0], vd[1] }; }
#[inline]
fn vrev21_vf_vf(vd: VFloat) -> VFloat {
  return (VFloat) {
    vd[1], vd[0], vd[3], vd[2], vd[5], vd[4], vd[7], vd[6],
      vd[9], vd[8], vd[11], vd[10], vd[13], vd[12], vd[15], vd[14] };
}
#[inline]
fn vreva2_vf_vf(vd: VFloat) -> VFloat {
  return (VFloat) {
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
#[inline]
fn vcast_vi_i(k: int) -> VInt {
  VInt ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = k;
  return ret;
}

#[inline]
fn vcast_vi2_i(k: int) -> VInt2 {
  VInt2 ret;
  for(int i=0;i<VECTLENSP;i++) ret[i] = k;
  return ret;
}

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
  VFloat ret;
  for(int i=0;i<VECTLENSP;i++) { ret[i] = self; }
  ret
    }
}
impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
  VDouble ret;
  for(int i=0;i<VECTLENDP;i++) { ret[i] = self; }
  ret
    }
}
/*#ifdef ENABLE_LONGDOUBLE
#[inline]
fn vcast_vl_l(long double d) -> VLongDouble {
  VLongDouble ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = d;
  return ret;
}
#endif*/

#[inline]
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask {
  VOpMask ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = m[i*2+1];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret[i] = 0;
  return ret;
}

#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask {
  VOpMask ret;
  for(int i=0;i<VECTLENDP;i++) ret[i*2] = ret[i*2+1] = m[i];
  return ret;
}

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> VMask {
  VMask ret;
  for(int i=0;i<VECTLENDP;i++) {
    ret[i*2+0] = l;
    ret[i*2+1] = h;
  }
  return ret;
}
impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        VInt2 ret;
        for(int i=0;i<VECTLENDP;i++) {
          ret[i*2+0] = 0;
          ret[i*2+1] = vi[i];
        }
        return ret;
    }
}
//#[inline]
//fn vcastu_vi2_vi(vi: VInt) -> VInt2 {
//}
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
      VInt ret;
      for(int i=0;i<VECTLENDP;i++) ret[i] = vi2[i*2+1];
      return ret;
    }
}
//#[inline]
//fn vcastu_vi_vi2(vi2: VInt2) -> VInt {
//}

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: VInt2) -> VInt {
  VInt ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = vi2[i];
  return ret;
}

#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: VInt) -> VInt2 {
  VInt2 ret;
  for(int i=0;i<VECTLENDP;i++) ret[i] = vi[i];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret[i] = 0;
  return ret;
}

#[inline]
fn vrev21_vd_vd(d0: VDouble) -> VDouble {
  VDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = d0[i*2+1];
    r[i*2+1] = d0[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vd_vd(d0: VDouble) -> VDouble {
  VDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = d0[(VECTLENDP/2-1-i)*2+0];
    r[i*2+1] = d0[(VECTLENDP/2-1-i)*2+1];
  }
  return r;
}

#[inline]
fn vrev21_vf_vf(d0: VFloat) -> VFloat {
  VFloat r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r[i*2+0] = d0[i*2+1];
    r[i*2+1] = d0[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vf_vf(d0: VFloat) -> VFloat {
  VFloat r;
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
fn vposneg_vd_vd(d0: VDouble) -> VDouble {
  VDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = +d0[i*2+0];
    r[i*2+1] = -d0[i*2+1];
  }
  return r;
}

#[inline]
fn vnegpos_vd_vd(d0: VDouble) -> VDouble {
  VDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r[i*2+0] = -d0[i*2+0];
    r[i*2+1] = +d0[i*2+1];
  }
  return r;
}

#[inline]
fn vposneg_vf_vf(d0: VFloat) -> VFloat {
  VFloat r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r[i*2+0] = +d0[i*2+0];
    r[i*2+1] = -d0[i*2+1];
  }
  return r;
}

#[inline]
fn vnegpos_vf_vf(d0: VFloat) -> VFloat {
  VFloat r;
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
fn vtestallones_i_vo64(g: VOpMask) -> int {
  int ret = 1; for(int i=0;i<VECTLENDP*2;i++) ret = ret && g[i]; return ret;
}

#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int {
  int ret = 1; for(int i=0;i<VECTLENDP*2;i++) ret = ret && g[i]; return ret;
}

//

static VInt2 vloadu_vi2_p(int32_t *p) {
  VInt2 vi;
  for(int i=0;i<VECTLENSP;i++) vi[i] = p[i];
  return vi;
}

static void vstoreu_v_p_vi2(int32_t *p, VInt2 v) {
  for(int i=0;i<VECTLENSP;i++) p[i] = v[i];
}

static VInt vloadu_vi_p(int32_t *p) {
  VInt vi;
  for(int i=0;i<VECTLENDP;i++) vi[i] = p[i];
  return vi;
}

static void vstoreu_v_p_vi(int32_t *p, VInt v) {
  for(int i=0;i<VECTLENDP;i++) p[i] = v[i];
}

//

#[inline]
fn vand_vm_vm_vm(x: VMask, y: VMask) -> VMask { return x & y; }
#[inline]
fn vandnot_vm_vm_vm(x: VMask, y: VMask) -> VMask { return y & ~x; }
#[inline]
fn vor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return x | y; }
#[inline]
fn vxor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return x ^ y; }

//#[inline]
//fn vand_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return x & y; }
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return y & ~x; }
//#[inline]
//fn vor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return x | y; }
//#[inline]
//fn vxor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return x ^ y; }

#[inline]
fn vand_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return x & y; }
#[inline]
fn vandnot_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return y & ~x; }
#[inline]
fn vor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return x | y; }
#[inline]
fn vxor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return x ^ y; }

#[inline]
fn vand_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return x & y; }
#[inline]
fn vandnot_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return y & ~x; }
#[inline]
fn vor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return x | y; }
#[inline]
fn vxor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return x ^ y; }

//

#[inline]
fn vsel_vd_vo_vd_vd(o: VOpMask, x: VDouble, y: VDouble) -> VDouble { return (VDouble)(((VMask)o & (VMask)x) | ((VMask)y & ~(VMask)o)); }
#[inline]
fn vsel_vi2_vo_vi2_vi2(o: VOpMask, x: VInt2, y: VInt2) -> VInt2 { return (VInt2)(((VMask)o & (VMask)x) | ((VMask)y & ~(VMask)o)); }

#[inline]
fn vsel_vd_vo_d_d(o: VOpMask, v1: double, v0: double) -> CONST -> VDouble {
  return vsel_vd_vo_vd_vd(o, v1.as_vd(), v0.as_vd());
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(VOpMask o0, VOpMask o1, double d0, double d1, double d2) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, d0.as_vd(), vsel_vd_vo_d_d(o1, d1, d2));
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(VOpMask o0, VOpMask o1, VOpMask o2, double d0, double d1, double d2, double d3) -> VDouble {
  return vsel_vd_vo_vd_vd(o0, d0.as_vd(), vsel_vd_vo_vd_vd(o1, d1.as_vd(), vsel_vd_vo_d_d(o2, d2, d3)));
}

#[inline]
fn vcast_vd_vi(vi: VInt) -> VDouble {
#if defined(__clang__)
  return __builtin_convertvector(vi, VDouble);
#else
  VDouble vd;
  for(int i=0;i<VECTLENDP;i++) vd[i] = vi[i];
  return vd;
#endif
}
#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt {
#if defined(__clang__)
  return __builtin_convertvector(vd, VInt);
#else
  VInt vi;
  for(int i=0;i<VECTLENDP;i++) vi[i] = vd[i];
  return vi;
#endif
}
#[inline]
fn vrint_vi_vd(vd: VDouble) -> VInt { return vtruncate_vi_vd(vsel_vd_vo_vd_vd((VOpMask)(vd < 0.0), vd - 0.5, vd + 0.5)); }
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return vcast_vd_vi(vtruncate_vi_vd(vd)); }
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble { return vcast_vd_vi(vrint_vi_vd(vd)); }

#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask {
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  return (VOpMask)((vi64)x == (vi64)y);
}

#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask {
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  return (VMask)((vi64)x + (vi64)y);
}

//

#[inline]
fn vreinterpret_vm_vd(vd: VDouble) -> VMask { return (VMask)vd; }
#[inline]
fn vreinterpret_vi2_vd(vd: VDouble) -> VInt2 { return (VInt2)vd; }
#[inline]
fn vreinterpret_vd_vi2(vi: VInt2) -> VDouble { return (VDouble)vi; }
#[inline]
fn vreinterpret_vd_vm(vm: VMask) -> VDouble { return (VDouble)vm; }


#[inline]
fn vrec_vd_vd(x: VDouble) -> VDouble { return 1.0 / x; }

#[inline]
fn vabs_vd_vd(d: VDouble) -> VDouble { return (VDouble)((VMask)d & ~(VMask)vcast_vd_d(-0.)); }
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return x * y - z; }

#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x + vnegpos_vd_vd(y); }
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vsubadd_vd_vd_vd(x*y, z); }


impl std::ops::Add for VInt {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        self + other
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x + y; }
impl std::ops::Sub for VInt {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        self - other
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x - y; }
impl std::ops::Neg for VInt {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        -self
    }
}
//#[inline]
//fn vneg_vi_vi(e: VInt) -> VInt { return -e; }

#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x & y; }
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt { return y & ~x; }
#[inline]
fn vor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x | y; }
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x ^ y; }

#[inline]
fn vand_vi_vo_vi(x: VOpMask, y: VInt) -> VInt { return vreinterpretFirstHalf_vi_vi2((VInt2)x) & y; }
#[inline]
fn vandnot_vi_vo_vi(x: VOpMask, y: VInt) -> VInt { return y & ~vreinterpretFirstHalf_vi_vi2((VInt2)x); }

#[inline]
fn vsll_vi_vi_i(x: VInt, c: int) -> VInt {
#if defined(__clang__)
  typedef uint32_t vu __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef uint32_t vu __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP)));
#endif
  return (VInt)(((vu)x) << c);
}

#[inline]
fn vsrl_vi_vi_i(x: VInt, c: int) -> VInt {
#if defined(__clang__)
  typedef uint32_t vu __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef uint32_t vu __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP)));
#endif
  return (VInt)(((vu)x) >> c);
}

#[inline]
fn vsra_vi_vi_i(x: VInt, c: int) -> VInt { return x >> c; }

#[inline]
fn veq_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x == y; }
#[inline]
fn vgt_vi_vi_vi(x: VInt, y: VInt) -> VInt { return x > y; }

#[inline]
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return (VOpMask)vreinterpretFirstHalf_vi2_vi(x == y); }
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return (VOpMask)vreinterpretFirstHalf_vi2_vi(x > y);}

#[inline]
fn vsel_vi_vo_vi_vi(m: VOpMask, x: VInt, y: VInt) -> VInt {
  return vor_vi_vi_vi(vand_vi_vi_vi(vreinterpretFirstHalf_vi_vi2((VInt2)m), x),
		      vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2((VInt2)m), y));
}

#[inline]
fn visinf_vo_vd(d: VDouble) -> VOpMask { return (VOpMask)(vabs_vd_vd(d) == SLEEF_INFINITY); }
#[inline]
fn vispinf_vo_vd(d: VDouble) -> VOpMask { return (VOpMask)(d == SLEEF_INFINITY); }
#[inline]
fn visminf_vo_vd(d: VDouble) -> VOpMask { return (VOpMask)(d == -SLEEF_INFINITY); }
#[inline]
fn visnan_vo_vd(d: VDouble) -> VOpMask { return (VOpMask)(d != d); }

#[inline]
fn vsqrt_vd_vd(d: VDouble) -> VDouble {
#if defined(__clang__)
  typedef int64_t vi64 __attribute__((ext_vector_type(VECTLENDP)));
#else
  typedef int64_t vi64 __attribute__((vector_size(sizeof(int64_t)*VECTLENDP)));
#endif
  
  VDouble q = (1).as_vd();

  VOpMask o = (VOpMask)(d < 8.636168555094445E-78);
  d = (VDouble)((o & (VMask)(d * 1.157920892373162E77)) | (~o & (VMask)d));

  q = (VDouble)((o & (VMask)vcast_vd_d(2.9387358770557188e-39)) | (~o & (VMask)vcast_vd_d(1)));

  q = (VDouble)vor_vm_vm_vm(d.lt((0.).as_vd()), (VMask)q);
  
  VDouble x = (VDouble)(0x5fe6ec85e7de30daLL - ((vi64)(d + 1e-320) >> 1));
  x = x * (  3 - d * x * x);
  x = x * ( 12 - d * x * x);
  x = x * (768 - d * x * x);
  x *= 1.0 / (1 << 13);
  x = (d - (d * x) * (d * x)) * (x * 0.5) + d * x;

  return x * q;
}

#[inline]
fn vcast_d_vd(v: VDouble) -> double { return v[0]; }
#[inline]
fn vcast_f_vf(v: VFloat) -> float { return v[0]; }

#[inline]
fn vload_vd_p(const double *ptr) -> VDouble { return *(VDouble *)ptr; }
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble {
  VDouble vd;
  for(int i=0;i<VECTLENDP;i++) vd[i] = ptr[i];
  return vd;
}

#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble {
  VDouble vd;
  for(int i=0;i<VECTLENDP;i++) vd[i] = ptr[vi[i]];
  return vd;
}

#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { *(VDouble *)ptr = v; }
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void {
  for(int i=0;i<VECTLENDP;i++) ptr[i] = v[i];
}
//#[inline]
//fn vstream_v_p_vd(double *ptr, VDouble v) -> void { *(VDouble *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void {
//  for(int i=0;i<VECTLENDP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { vscatter2_v_p_i_i_vd(ptr, offset, step, v); }

//

#[inline]
fn vsel_vf_vo_vf_vf(o: VOpMask, x: VFloat, y: VFloat) -> VFloat { return (VFloat)(((VMask)o & (VMask)x) | (~(VMask)o & (VMask)y)); }

#[inline]
fn vsel_vf_vo_f_f(o: VOpMask, v1: float, v0: float) -> CONST -> VFloat {
  return vsel_vf_vo_vf_vf(o, v1.as_vf(), v0.as_vf());
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(VOpMask o0, VOpMask o1, float d0, float d1, float d2) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, d0.as_vf(), vsel_vf_vo_f_f(o1, d1, d2));
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(VOpMask o0, VOpMask o1, VOpMask o2, float d0, float d1, float d2, float d3) -> VFloat {
  return vsel_vf_vo_vf_vf(o0, d0.as_vf(), vsel_vf_vo_vf_vf(o1, d1.as_vf(), vsel_vf_vo_f_f(o2, d2, d3)));
}

#[inline]
fn vcast_vi2_vm(vm: VMask) -> VInt2 { return (VInt2)vm; }
#[inline]
fn vcast_vm_vi2(vi: VInt2) -> VMask { return (VMask)vi; }

#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat {
#if defined(__clang__)
  return __builtin_convertvector(vi, VFloat);
#else
  VFloat vf;
  for(int i=0;i<VECTLENDP*2;i++) vf[i] = vi[i];
  return vf;
#endif
}

#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 {
#if defined(__clang__)
  return __builtin_convertvector(vf, VInt2);
#else
  VInt2 vi;
  for(int i=0;i<VECTLENDP*2;i++) vi[i] = vf[i];
  return vi;
#endif
}

#[inline]
fn vrint_vi2_vf(vf: VFloat) -> VInt2 { return vtruncate_vi2_vf(vsel_vf_vo_vf_vf((VOpMask)(vf < 0), vf - 0.5f, vf + 0.5)); }
#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vrint_vi2_vf(vd)); }

#[inline]
fn vreinterpret_vm_vf(vf: VFloat) -> VMask { return (VMask)vf; }
#[inline]
fn vreinterpret_vf_vm(vm: VMask) -> VFloat { return (VFloat)vm; }
#[inline]
fn vreinterpret_vf_vi2(vi: VInt2) -> VFloat { return (VFloat)vi; }
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) -> VInt2 { return (VInt2)vf; }
#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vrec_vf_vf(x: VFloat) -> VFloat { return 1. / x; }

#[inline]
fn vabs_vf_vf(f: VFloat) -> VFloat { return (VFloat)vandnot_vm_vm_vm((VMask)vcast_vf_f(-0.), (VMask)f); }

impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        self * y + z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return z-x*y; }

#[inline]
fn vsubadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return x + vnegpos_vf_vf(y); }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vsubadd_vf_vf_vf(x * y, z); }

#[inline]
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return x + y; }
#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return x - y; }
#[inline]
fn vneg_vi2_vi2(e: VInt2) -> VInt2 { return -e; }

#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return x & y; }
#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return  y & ~x; }
#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return x | y; }
#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return x ^ y; }

#[inline]
fn vand_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return (VInt2)x & y; }
#[inline]
fn vandnot_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return y & ~(VInt2)x; }

#[inline]
fn vsll_vi2_vi2_i(x: VInt2, c: int) -> VInt2 {
#if defined(__clang__)
  typedef uint32_t vu __attribute__((ext_vector_type(VECTLENDP*2)));
#else
  typedef uint32_t vu __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));
#endif
  return (VInt2)(((vu)x) << c);
}
#[inline]
fn vsrl_vi2_vi2_i(x: VInt2, c: int) -> VInt2 {
#if defined(__clang__)
  typedef uint32_t vu __attribute__((ext_vector_type(VECTLENDP*2)));
#else
  typedef uint32_t vu __attribute__((vector_size(sizeof(uint32_t)*VECTLENDP*2)));
#endif
  return (VInt2)(((vu)x) >> c);
}
#[inline]
fn vsra_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { return x >> c; }

#[inline]
fn veq_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return (VOpMask)(x == y); }
#[inline]
fn vgt_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return (VOpMask)(x > y); }
#[inline]
fn veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return x == y; }
#[inline]
fn vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return x > y; }

#[inline]
fn visinf_vo_vf(d: VFloat) -> VOpMask { return (VOpMask)(vabs_vf_vf(d) == SLEEF_INFINITYf); }
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask { return (VOpMask)(d == SLEEF_INFINITYf); }
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask { return (VOpMask)(d == -SLEEF_INFINITYf); }
#[inline]
fn visnan_vo_vf(d: VFloat) -> VOpMask { return (VOpMask)(d != d); }

#[inline]
fn vsqrt_vf_vf(d: VFloat) -> VFloat {
  VFloat q = (1).as_vf();

  VOpMask o = (VOpMask)(d < 5.4210108624275221700372640043497e-20); // 2^-64
  d = (VFloat)((o & (VMask)(d * (18446744073709551616.).as_vf())) | (~o & (VMask)d)); // 2^64
  q = (VFloat)((o & (VMask)vcast_vf_f(0.00000000023283064365386962890625)) | (~o & (VMask)vcast_vf_f(1))); // 2^-32
  q = (VFloat)vor_vm_vm_vm(d.lt((0.).as_vf()), (VMask)q);
  
  VFloat x = (VFloat)(0x5f330de2 - (((VInt2)d) >> 1));
  x = x * ( 3.0f - d * x * x);
  x = x * (12.0f - d * x * x);
  x *= 0.0625f;
  x = (d - (d * x) * (d * x)) * (x * 0.5) + d * x;

  return x * q;
}

#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return *(VFloat *)ptr; }
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat {
  VFloat vf;
  for(int i=0;i<VECTLENSP;i++) vf[i] = ptr[i];
  return vf;
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi2) -> VFloat {
  VFloat vf;
  for(int i=0;i<VECTLENSP;i++) vf[i] = ptr[vi2[i]];
  return vf;
}

#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { *(VFloat *)ptr = v; }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void {
  for(int i=0;i<VECTLENSP;i++) ptr[i] = v[i];
}
//#[inline]
//fn vstream_v_p_vf(float *ptr, VFloat v) -> void { *(VFloat *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
//  for(int i=0;i<VECTLENSP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }

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
