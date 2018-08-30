//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <stdint.h>
#include <math.h>
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

#define ACCURATE_SQRT

#define DFTPRIORITY LOG2VECTLENDP
#define ISANAME "Pure C Array"

typedef union {
  uint32_t u[VECTLENDP*2];
  uint64_t x[VECTLENDP];
  double d[VECTLENDP];
  float f[VECTLENDP*2];
  int32_t i[VECTLENDP*2];
} versatileVector;

typedef versatileVector VMask;
typedef versatileVector VOpMask;
typedef versatileVector VDouble;
typedef versatileVector VInt;
typedef versatileVector VFloat;
typedef versatileVector VInt2;

typedef union {
  uint8_t u[sizeof(long double)*VECTLENDP];
  long double ld[VECTLENDP];
} longdoubleVector;

typedef longdoubleVector VMaskL;
typedef longdoubleVector VLongDouble;

#ifdef Sleef_quad2_DEFINED
typedef union {
  uint8_t u[sizeof(Sleef_quad)*VECTLENDP];
  Sleef_quad q[VECTLENDP];
} quadVector;

typedef quadVector VMaskq;
typedef quadVector VQuad;
#endif

//

#[inline]
fn vavailability_i(name: int) -> int { return -1; }
#[inline]
fn vprefetch_v_p(const void *ptr) -> void { }

#[inline]
fn vtestallones_i_vo64(g: VOpMask) -> int {
  int ret = 1; for(int i=0;i<VECTLENDP;i++) ret = ret && g.x[i]; return ret;
}

#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int {
  int ret = 1; for(int i=0;i<VECTLENSP;i++) ret = ret && g.u[i]; return ret;
}

//

static VInt2 vloadu_vi2_p(int32_t *p) {
  VInt2 vi;
  for(int i=0;i<VECTLENSP;i++) vi.i[i] = p[i];
  return vi;
}

static void vstoreu_v_p_vi2(int32_t *p, VInt2 v) {
  for(int i=0;i<VECTLENSP;i++) p[i] = v.i[i];
}

static VInt vloadu_vi_p(int32_t *p) {
  VInt vi;
  for(int i=0;i<VECTLENDP;i++) vi.i[i] = p[i];
  return vi;
}

static void vstoreu_v_p_vi(int32_t *p, VInt v) {
  for(int i=0;i<VECTLENDP;i++) p[i] = v.i[i];
}

//

#[inline]
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask {
  VOpMask ret;
  for(int i=0;i<VECTLENDP;i++) ret.u[i] = m.u[i*2+1];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret.u[i] = 0;
  return ret;
}

#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask {
  VOpMask ret;
  for(int i=0;i<VECTLENDP;i++) ret.u[i*2] = ret.u[i*2+1] = m.u[i];
  return ret;
}

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> VMask {
  VMask ret;
  for(int i=0;i<VECTLENDP;i++) {
    ret.u[i*2+0] = l;
    ret.u[i*2+1] = h;
  }
  return ret;
}

impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        VInt2 ret;
        for(int i=0;i<VECTLENDP;i++) {
          ret.i[i*2+0] = 0;
          ret.i[i*2+1] = vi.i[i];
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
      for(int i=0;i<VECTLENDP;i++) ret.i[i] = vi2.i[i*2+1];
      return ret;
    }
}

//#[inline]
//fn vcastu_vi_vi2(vi2: VInt2) -> VInt {
//}

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: VInt2) -> VInt {
  VInt ret;
  for(int i=0;i<VECTLENDP;i++) ret.i[i] = vi2.i[i];
  return ret;
}

#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: VInt) -> VInt2 {
  VInt2 ret;
  for(int i=0;i<VECTLENDP;i++) ret.i[i] = vi.i[i];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret.i[i] = 0;
  return ret;
}

#[inline]
fn vrev21_vd_vd(d0: VDouble) -> VDouble {
  VDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r.d[i*2+0] = d0.d[i*2+1];
    r.d[i*2+1] = d0.d[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vd_vd(d0: VDouble) -> VDouble {
  VDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r.d[i*2+0] = d0.d[(VECTLENDP/2-1-i)*2+0];
    r.d[i*2+1] = d0.d[(VECTLENDP/2-1-i)*2+1];
  }
  return r;
}

#[inline]
fn vrev21_vf_vf(d0: VFloat) -> VFloat {
  VFloat r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r.f[i*2+0] = d0.f[i*2+1];
    r.f[i*2+1] = d0.f[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vf_vf(d0: VFloat) -> VFloat {
  VFloat r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r.f[i*2+0] = d0.f[(VECTLENSP/2-1-i)*2+0];
    r.f[i*2+1] = d0.f[(VECTLENSP/2-1-i)*2+1];
  }
  return r;
}

impl VCastD for f64 {
    #[inline]
    fn as_vd(self) -> VDouble {
      VDouble ret;
      for(int i=0;i<VECTLENDP;i++) {
        ret.d[i] = d;
      }
      ret
    }
}

//

//#[inline]
//fn vand_vo_vo_vo   (x: VOpMask, y: VOpMask) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] &  y.u[i]; return ret; }
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = y.u[i] & ~x.u[i]; return ret; }
//#[inline]
//fn vor_vo_vo_vo    (x: VOpMask, y: VOpMask) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] |  y.u[i]; return ret; }
//#[inline]
//fn vxor_vo_vo_vo   (x: VOpMask, y: VOpMask) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] ^  y.u[i]; return ret; }

#[inline]
fn vand_vm_vm_vm     (x: VMask, y: VMask)     -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] &  y.u[i]; return ret; }
#[inline]
fn vandnot_vm_vm_vm  (x: VMask, y: VMask)     -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = y.u[i] & ~x.u[i]; return ret; }
#[inline]
fn vor_vm_vm_vm      (x: VMask, y: VMask)     -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] |  y.u[i]; return ret; }
#[inline]
fn vxor_vm_vm_vm     (x: VMask, y: VMask)     -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] ^  y.u[i]; return ret; }

#[inline]
fn vand_vm_vo64_vm(x: VOpMask, y: VMask)      -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] &  y.u[i]; return ret; }
#[inline]
fn vandnot_vm_vo64_vm(x: VOpMask, y: VMask)   -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = y.u[i] & ~x.u[i]; return ret; }
#[inline]
fn vor_vm_vo64_vm(x: VOpMask, y: VMask)       -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] |  y.u[i]; return ret; }
#[inline]
fn vxor_vm_vo64_vm(x: VOpMask, y: VMask)      -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] ^  y.u[i]; return ret; }

#[inline]
fn vand_vm_vo32_vm(x: VOpMask, y: VMask)      -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] &  y.u[i]; return ret; }
#[inline]
fn vandnot_vm_vo32_vm(x: VOpMask, y: VMask)   -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = y.u[i] & ~x.u[i]; return ret; }
#[inline]
fn vor_vm_vo32_vm(x: VOpMask, y: VMask)       -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] |  y.u[i]; return ret; }
#[inline]
fn vxor_vm_vo32_vm(x: VOpMask, y: VMask)      -> VMask { VMask   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] ^  y.u[i]; return ret; }

//

#[inline]
fn vsel_vd_vo_vd_vd   (o: VOpMask, x: VDouble, y: VDouble) -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = (o.u[i] & x.u[i]) | (y.u[i] & ~o.u[i]); return ret; }
#[inline]
fn   vsel_vi2_vo_vi2_vi2(o: VOpMask, x: VInt2, y: VInt2)     -> VInt2 { VInt2 ret;   for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = (o.u[i] & x.u[i]) | (y.u[i] & ~o.u[i]); return ret; }

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
fn vcast_vd_vi(vi: VInt) -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = vi.i[i]; return ret; }
#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = (int)vd.d[i]; return ret; }
#[inline]
fn vrint_vi_vd(vd: VDouble) -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = vd.d[i] > 0 ? (int)(vd.d[i] + 0.5) : (int)(vd.d[i] - 0.5); return ret; }
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return vcast_vd_vi(vtruncate_vi_vd(vd)); }
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble { return vcast_vd_vi(vrint_vi_vd(vd)); }
impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        VInt ret; for(int i=0;i<VECTLENDP;i++) {ret.i[i] = j;}
        ret
    }
}
//#[inline]
//fn vcast_vi_i(j: int) -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = j; return ret; }

#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = x.x[i] == y.x[i] ? -1 : 0; return ret; }
#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask { VMask ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = x.x[i] + y.x[i]; return ret; }

//

#[inline]
fn vreinterpret_vm_vd(vd: VDouble) { union -> VMask { VDouble vd; VMask vm; } cnv; cnv.vd = vd; return cnv.vm; }
#[inline]
fn vreinterpret_vi2_vd(vd: VDouble) { union -> VInt2 { VDouble vd; VInt2 vi2; } cnv; cnv.vd = vd; return cnv.vi2; }
#[inline]
fn vreinterpret_vd_vi2(vi: VInt2) { union -> VDouble { VInt2 vi2; VDouble vd; } cnv; cnv.vi2 = vi; return cnv.vd; }
#[inline]
fn vreinterpret_vd_vm(vm: VMask) { union -> VDouble { VMask vm; VDouble vd; } cnv; cnv.vm = vm; return cnv.vd; }


#[inline]
fn vrec_vd_vd(x: VDouble)               -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = 1.0 / x.d[i];    return ret; }

#[inline]
fn vabs_vd_vd(d: VDouble) -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = d.x[i] & 0x7fffffffffffffffULL; return ret; }
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = x.d[i] * y.d[i] + z.d[i]; return ret;
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = x.d[i] * y.d[i] - z.d[i]; return ret; }

#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = (i & 1) == 0 ?  d.d[i] : -d.d[i]; return ret; }
#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = (i & 1) == 0 ? -d.d[i] :  d.d[i]; return ret; }
#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = (i & 1) == 0 ? x.d[i] - y.d[i] : x.d[i] + y.d[i]; return ret; }
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vsubadd_vd_vd_vd(x*y, z); }


impl std::ops::Add for VInt {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        VInt ret;
        for(int i=0;i<VECTLENDP;i++) { ret.i[i] = x.i[i] + y.i[i]; }
        ret
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt {  }
impl std::ops::Sub for VInt {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        VInt ret;
        for(int i=0;i<VECTLENDP;i++) { ret.i[i] = x.i[i] - y.i[i]; }
        ret
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt {  }
impl std::ops::Neg for VInt {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        VInt ret; for(int i=0;i<VECTLENDP;i++) { ret.i[i] = -x.i[i]; }
        ret
    }
}
//#[inline]
//fn vneg_vi_vi   (x: VInt)         -> VInt {  }

#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt)    -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = x.i[i] &  y.i[i]; return ret; }
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = y.i[i] & ~x.i[i]; return ret; }
#[inline]
fn vor_vi_vi_vi(x: VInt, y: VInt)     -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = x.i[i] |  y.i[i]; return ret; }
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt)    -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = x.i[i] ^  y.i[i]; return ret; }

#[inline]
fn vand_vi_vo_vi(x: VOpMask, y: VInt)    -> VInt { return vand_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(x), y); }
#[inline]
fn vandnot_vi_vo_vi(x: VOpMask, y: VInt) -> VInt { return vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(x), y); }

#[inline]
fn vsll_vi_vi_i(x: VInt, c: int) -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = x.i[i] << c; return ret; }
#[inline]
fn vsrl_vi_vi_i(x: VInt, c: int) -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = ((uint32_t)x.i[i]) >> c; return ret; }
#[inline]
fn vsra_vi_vi_i(x: VInt, c: int) -> VInt { VInt ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = x.i[i] >> c; return ret; }

#[inline]
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP;i++) ret.u[i] = x.i[i] == y.i[i] ? -1 : 0; return ret; }
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP;i++) ret.u[i] = x.i[i] >  y.i[i] ? -1 : 0; return ret; }

#[inline]
fn vsel_vi_vo_vi_vi(m: VOpMask, x: VInt, y: VInt) -> VInt {
  union { VOpMask vo; VInt2 vi2; } cnv;
  cnv.vo = m;
  return vor_vi_vi_vi(vand_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(cnv.vi2), x),
		      vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(cnv.vi2), y));
}

#[inline]
fn visinf_vo_vd(d: VDouble)  -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = (d.d[i] == SLEEF_INFINITY || d.d[i] == -SLEEF_INFINITY) ? -1 : 0; return ret; }
#[inline]
fn vispinf_vo_vd(d: VDouble) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = d.d[i] == SLEEF_INFINITY ? -1 : 0; return ret; }
#[inline]
fn visminf_vo_vd(d: VDouble) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = d.d[i] == -SLEEF_INFINITY ? -1 : 0; return ret; }
#[inline]
fn visnan_vo_vd(d: VDouble)  -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = d.d[i] != d.d[i] ? -1 : 0; return ret; }

#[inline]
fn vsqrt_vd_vd(d: VDouble) -> VDouble { VDouble ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = sqrt(d.d[i]); return ret; }

#if defined(_MSC_VER)
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_d_vd(v: VDouble) -> double { return v.d[0]; }
#endif

#[inline]
fn vload_vd_p(const double *ptr) -> VDouble { return *(VDouble *)ptr; }
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble { VDouble vd; for(int i=0;i<VECTLENDP;i++) vd.d[i] = ptr[i]; return vd; }

#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble {
  VDouble vd;
  for(int i=0;i<VECTLENDP;i++) vd.d[i] = ptr[vi.i[i]];
  return vd;
}

#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { *(VDouble *)ptr = v; }
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void { for(int i=0;i<VECTLENDP;i++) ptr[i] = v.d[i]; }
//#[inline]
//fn vstream_v_p_vd(double *ptr, VDouble v) -> void { *(VDouble *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void {
//  for(int i=0;i<VECTLENDP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v.d[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v.d[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { vscatter2_v_p_i_i_vd(ptr, offset, step, v); }

//

#[inline]
fn vcast_vi2_vm(vm: VMask) { union -> VInt2 { VInt2 vi2; VMask vm; } cnv; cnv.vm = vm; return cnv.vi2; }
#[inline]
fn vcast_vm_vi2(vi: VInt2) { union -> VMask { VInt2 vi2; VMask vm; } cnv; cnv.vi2 = vi; return cnv.vm; }

#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = vi.i[i]; return ret; }
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { VInt2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = (int)vf.f[i]; return ret; }
#[inline]
fn vrint_vi2_vf(vf: VFloat) -> VInt2 { VInt ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = vf.f[i] > 0 ? (int)(vf.f[i] + 0.5) : (int)(vf.f[i] - 0.5); return ret; }
#[inline]
fn vcast_vi2_i(j: int) -> VInt2 { VInt2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = j; return ret; }
#[inline]
fn vtruncate_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vd: VFloat) -> VFloat { return vcast_vf_vi2(vrint_vi2_vf(vd)); }

impl VCastF for f32 {
    #[inline]
    fn as_vf(self) -> VFloat {
        VFloat ret;
        for(int i=0;i<VECTLENSP;i++) {
            ret.f[i] = self;
        }
        ret
    }
}
#[inline]
fn vreinterpret_vm_vf(vf: VFloat) { union -> VMask { VFloat vf; VMask vm; } cnv; cnv.vf = vf; return cnv.vm; }
#[inline]
fn vreinterpret_vf_vm(vm: VMask) { union -> VFloat { VFloat vf; VMask vm; } cnv; cnv.vm = vm; return cnv.vf; }
#[inline]
fn vreinterpret_vf_vi2(vi: VInt2) { union -> VFloat { VFloat vf; VInt2 vi2; } cnv; cnv.vi2 = vi; return cnv.vf; }
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) { union -> VInt2 { VFloat vf; VInt2 vi2; } cnv; cnv.vf = vf; return cnv.vi2; }

#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn vrec_vf_vf   (x: VFloat)           -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = 1.0    / x.f[i]; return ret; }

#[inline]
fn vabs_vf_vf(x: VFloat) -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] & 0x7fffffff; return ret; }
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = x.f[i] * y.f[i] + z.f[i]; return ret;
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = x.f[i] * y.f[i] - z.f[i]; return ret; }

#[inline]
fn vposneg_vf_vf(x: VFloat) -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = (i & 1) == 0 ?  x.f[i] : -x.f[i]; return ret; }
#[inline]
fn vnegpos_vf_vf(x: VFloat) -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = (i & 1) == 0 ? -x.f[i] :  x.f[i]; return ret; }
#[inline]
fn vsubadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = (i & 1) == 0 ? x.f[i] - y.f[i] : x.f[i] + y.f[i]; return ret; }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vsubadd_vf_vf_vf(x * y, z); }


#[inline]
fn vadd_vi2_vi2_vi2(x: VInt, y: VInt) -> VInt { VInt ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] + y.i[i]; return ret; }
#[inline]
fn vsub_vi2_vi2_vi2(x: VInt, y: VInt) -> VInt { VInt ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] - y.i[i]; return ret; }
#[inline]
fn vneg_vi2_vi2(x: VInt)             -> VInt { VInt ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = -x.i[i]; return ret; }

#[inline]
fn vand_vi2_vi2_vi2(x: VInt, y: VInt)    -> VInt { VInt ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] &  y.i[i]; return ret; }
#[inline]
fn vandnot_vi2_vi2_vi2(x: VInt, y: VInt) -> VInt { VInt ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = y.i[i] & ~x.i[i]; return ret; }
#[inline]
fn vor_vi2_vi2_vi2(x: VInt, y: VInt)     -> VInt { VInt ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] |  y.i[i]; return ret; }
#[inline]
fn vxor_vi2_vi2_vi2(x: VInt, y: VInt)    -> VInt { VInt ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] ^  y.i[i]; return ret; }

#[inline]
fn vsel_vf_vo_vf_vf(o: VOpMask, x: VFloat, y: VFloat) -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = (o.u[i] & x.u[i]) | (y.u[i] & ~o.u[i]); return ret; }

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
fn vand_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 {
  union { VOpMask vo; VInt2 vi2; } cnv;
  cnv.vo = x;
  return vand_vi2_vi2_vi2(cnv.vi2, y);
}
#[inline]
fn vandnot_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return vandnot_vi2_vi2_vi2(x, y); }

#[inline]
fn vsll_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { VInt2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] << c; return ret; }
#[inline]
fn vsrl_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { VInt2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = ((uint32_t)x.i[i]) >> c; return ret; }
#[inline]
fn vsra_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { VInt2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] >> c; return ret; }

#[inline]
fn visinf_vo_vf (d: VFloat) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = (d.f[i] == SLEEF_INFINITYf || d.f[i] == -SLEEF_INFINITYf) ? -1 : 0; return ret; }
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = d.f[i] == SLEEF_INFINITYf ? -1 : 0; return ret; }
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = d.f[i] == -SLEEF_INFINITYf ? -1 : 0; return ret; }
#[inline]
fn visnan_vo_vf (d: VFloat) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = d.f[i] != d.f[i] ? -1 : 0; return ret; }

#[inline]
fn veq_vo_vi2_vi2 (VInt2 x, VInt2 y) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = x.i[i] == y.i[i] ? -1 : 0; return ret; }
#[inline]
fn vgt_vo_vi2_vi2 (VInt2 x, VInt2 y) -> VOpMask { VOpMask ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = x.i[i] >  y.i[i] ? -1 : 0; return ret; }
#[inline]
fn   veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { VOpMask ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] == y.i[i] ? -1 : 0; return ret; }
#[inline]
fn   vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { VOpMask ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] >  y.i[i] ? -1 : 0; return ret; }

#[inline]
fn vsqrt_vf_vf(x: VFloat) -> VFloat { VFloat ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = sqrtf(x.f[i]); return ret; }

#ifdef _MSC_VER
// This function is needed when debugging on MSVC.
#[inline]
fn vcast_f_vf(v: VFloat) -> float { return v.f[0]; }
#endif

#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return *(VFloat *)ptr; }
#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat {
  VFloat vf;
  for(int i=0;i<VECTLENSP;i++) vf.f[i] = ptr[i];
  return vf;
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi2) -> VFloat {
  VFloat vf;
  for(int i=0;i<VECTLENSP;i++) vf.f[i] = ptr[vi2.i[i]];
  return vf;
}

#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { *(VFloat *)ptr = v; }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void {
  for(int i=0;i<VECTLENSP;i++) ptr[i] = v.f[i];
}
//#[inline]
//fn vstream_v_p_vf(float *ptr, VFloat v) -> void { *(VFloat *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
//  for(int i=0;i<VECTLENSP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v.f[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v.f[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }

//

#[inline]
fn vcast_vl_l(long double d) -> VLongDouble { VLongDouble ret; for(int i=0;i<VECTLENDP;i++) ret.ld[i] = d; return ret; }

#[inline]
fn vrev21_vl_vl(d0: VLongDouble) -> VLongDouble {
  VLongDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r.ld[i*2+0] = d0.ld[i*2+1];
    r.ld[i*2+1] = d0.ld[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vl_vl(d0: VLongDouble) -> VLongDouble {
  VLongDouble r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r.ld[i*2+0] = d0.ld[(VECTLENDP/2-1-i)*2+0];
    r.ld[i*2+1] = d0.ld[(VECTLENDP/2-1-i)*2+1];
  }
  return r;
}

#[inline]
fn vadd_vl_vl_vl(VLongDouble x, VLongDouble y) -> VLongDouble { VLongDouble ret; for(int i=0;i<VECTLENDP;i++) ret.ld[i] = x.ld[i] + y.ld[i]; return ret; }
#[inline]
fn vsub_vl_vl_vl(VLongDouble x, VLongDouble y) -> VLongDouble { VLongDouble ret; for(int i=0;i<VECTLENDP;i++) ret.ld[i] = x.ld[i] - y.ld[i]; return ret; }
#[inline]
fn vmul_vl_vl_vl(VLongDouble x, VLongDouble y) -> VLongDouble { VLongDouble ret; for(int i=0;i<VECTLENDP;i++) ret.ld[i] = x.ld[i] * y.ld[i]; return ret; }

#[inline]
fn vneg_vl_vl(x: VLongDouble) -> VLongDouble { VLongDouble ret; for(int i=0;i<VECTLENDP;i++) ret.ld[i] = -x.ld[i]; return ret; }
#[inline]
fn vsubadd_vl_vl_vl(VLongDouble x, VLongDouble y) -> VLongDouble { VLongDouble ret; for(int i=0;i<VECTLENDP;i++) ret.ld[i] = (i & 1) == 0 ? x.ld[i] - y.ld[i] : x.ld[i] + y.ld[i]; return ret; }
#[inline]
fn vmlsubadd_vl_vl_vl_vl(VLongDouble x, VLongDouble y, VLongDouble z) -> VLongDouble { return vsubadd_vl_vl_vl(vmul_vl_vl_vl(x, y), z); }
#[inline]
fn vposneg_vl_vl(x: VLongDouble) -> VLongDouble { VLongDouble ret; for(int i=0;i<VECTLENDP;i++) ret.ld[i] = (i & 1) == 0 ?  x.ld[i] : -x.ld[i]; return ret; }
#[inline]
fn vnegpos_vl_vl(x: VLongDouble) -> VLongDouble { VLongDouble ret; for(int i=0;i<VECTLENDP;i++) ret.ld[i] = (i & 1) == 0 ? -x.ld[i] :  x.ld[i]; return ret; }

#[inline]
fn vload_vl_p(const long double *ptr) -> VLongDouble { return *(VLongDouble *)ptr; }
#[inline]
fn vloadu_vl_p(const long double *ptr) -> VLongDouble {
  VLongDouble vd;
  for(int i=0;i<VECTLENDP;i++) vd.ld[i] = ptr[i];
  return vd;
}

#[inline]
fn vstore_v_p_vl(long double *ptr, VLongDouble v) -> void { *(VLongDouble *)ptr = v; }
#[inline]
fn vstoreu_v_p_vl(long double *ptr, VLongDouble v) -> void {
  for(int i=0;i<VECTLENDP;i++) ptr[i] = v.ld[i];
}
//#[inline]
//fn vstream_v_p_vl(long double *ptr, VLongDouble v) -> void { *(VLongDouble *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vl(long double *ptr, int offset, int step, VLongDouble v) -> void {
//  for(int i=0;i<VECTLENDP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v.ld[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v.ld[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vl(long double *ptr, int offset, int step, VLongDouble v) -> void { vscatter2_v_p_i_i_vl(ptr, offset, step, v); }

#ifdef Sleef_quad2_DEFINED
#[inline]
fn vcast_vq_q(Sleef_quad d) -> VQuad { VQuad ret; for(int i=0;i<VECTLENDP;i++) ret.q[i] = d; return ret; }

#[inline]
fn vrev21_vq_vq(d0: VQuad) -> VQuad {
  VQuad r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r.q[i*2+0] = d0.q[i*2+1];
    r.q[i*2+1] = d0.q[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vq_vq(d0: VQuad) -> VQuad {
  VQuad r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r.q[i*2+0] = d0.q[(VECTLENDP/2-1-i)*2+0];
    r.q[i*2+1] = d0.q[(VECTLENDP/2-1-i)*2+1];
  }
  return r;
}

#[inline]
fn vadd_vq_vq_vq(x: VQuad, y: VQuad) -> VQuad { VQuad ret; for(int i=0;i<VECTLENDP;i++) ret.q[i] = x.q[i] + y.q[i]; return ret; }
#[inline]
fn vsub_vq_vq_vq(x: VQuad, y: VQuad) -> VQuad { VQuad ret; for(int i=0;i<VECTLENDP;i++) ret.q[i] = x.q[i] - y.q[i]; return ret; }
#[inline]
fn vmul_vq_vq_vq(x: VQuad, y: VQuad) -> VQuad { VQuad ret; for(int i=0;i<VECTLENDP;i++) ret.q[i] = x.q[i] * y.q[i]; return ret; }

#[inline]
fn vneg_vq_vq(x: VQuad) -> VQuad { VQuad ret; for(int i=0;i<VECTLENDP;i++) ret.q[i] = -x.q[i]; return ret; }
#[inline]
fn vsubadd_vq_vq_vq(x: VQuad, y: VQuad) -> VQuad { VQuad ret; for(int i=0;i<VECTLENDP;i++) ret.q[i] = (i & 1) == 0 ? x.q[i] - y.q[i] : x.q[i] + y.q[i]; return ret; }
#[inline]
fn vmlsubadd_vq_vq_vq_vq(VQuad x, VQuad y, VQuad z) -> VQuad { return vsubadd_vq_vq_vq(vmul_vq_vq_vq(x, y), z); }
#[inline]
fn vposneg_vq_vq(x: VQuad) -> VQuad { VQuad ret; for(int i=0;i<VECTLENDP;i++) ret.q[i] = (i & 1) == 0 ?  x.q[i] : -x.q[i]; return ret; }
#[inline]
fn vnegpos_vq_vq(x: VQuad) -> VQuad { VQuad ret; for(int i=0;i<VECTLENDP;i++) ret.q[i] = (i & 1) == 0 ? -x.q[i] :  x.q[i]; return ret; }

#[inline]
fn vload_vq_p(const Sleef_quad *ptr) -> VQuad { return *(VQuad *)ptr; }
#[inline]
fn vloadu_vq_p(const Sleef_quad *ptr) -> VQuad {
  VQuad vd;
  for(int i=0;i<VECTLENDP;i++) vd.q[i] = ptr[i];
  return vd;
}

#[inline]
fn vstore_v_p_vq(Sleef_quad *ptr, VQuad v) -> void { *(VQuad *)ptr = v; }
#[inline]
fn vstoreu_v_p_vq(Sleef_quad *ptr, VQuad v) -> void {
  for(int i=0;i<VECTLENDP;i++) ptr[i] = v.q[i];
}
//#[inline]
//fn vstream_v_p_vq(Sleef_quad *ptr, VQuad v) -> void { *(VQuad *)ptr = v; }

//#[inline]
//fn vscatter2_v_p_i_i_vq(Sleef_quad *ptr, int offset, int step, VQuad v) -> void {
//  for(int i=0;i<VECTLENDP/2;i++) {
//    *(ptr+(offset + step * i)*2 + 0) = v.q[i*2+0];
//    *(ptr+(offset + step * i)*2 + 1) = v.q[i*2+1];
//  }
//}

//#[inline]
//fn vsscatter2_v_p_i_i_vq(Sleef_quad *ptr, int offset, int step, VQuad v) -> void { vscatter2_v_p_i_i_vq(ptr, offset, step, v); }
#endif
