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

typedef versatileVector $bx;
typedef versatileVector $mox;
typedef versatileVector $f64x;
typedef versatileVector $ix;
typedef versatileVector $f32x;
typedef versatileVector $ix2;

typedef union {
  uint8_t u[sizeof(long double)*VECTLENDP];
  long double ld[VECTLENDP];
} longdoubleVector;

typedef longdoubleVector $bxL;
typedef longdoubleVector VLongDouble;

#ifdef Sleef_quad2_DEFINED
typedef union {
  uint8_t u[sizeof(Sleef_quad)*VECTLENDP];
  Sleef_quad q[VECTLENDP];
} quadVector;

typedef quadVector $bxq;
typedef quadVector VQuad;
#endif

//

#[inline]
fn vtestallones_i_vo64(g: $mox) -> int {
  int ret = 1; for(int i=0;i<VECTLENDP;i++) ret = ret && g.x[i]; return ret;
}

#[inline]
fn vtestallones_i_vo32(g: $mox) -> int {
  int ret = 1; for(int i=0;i<VECTLENSP;i++) ret = ret && g.u[i]; return ret;
}

//


static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) {
  for(int i=0;i<VECTLENSP;i++) p[i] = v.i[i];
}


static void vstoreu_v_p_vi(int32_t *p, $ix v) {
  for(int i=0;i<VECTLENDP;i++) p[i] = v.i[i];
}

//

#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox {
  $mox ret;
  for(int i=0;i<VECTLENDP;i++) ret.u[i] = m.u[i*2+1];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret.u[i] = 0;
  return ret;
}

#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox {
  $mox ret;
  for(int i=0;i<VECTLENDP;i++) ret.u[i*2] = ret.u[i*2+1] = m.u[i];
  return ret;
}

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $bx {
  $bx ret;
  for(int i=0;i<VECTLENDP;i++) {
    ret.u[i*2+0] = l;
    ret.u[i*2+1] = h;
  }
  return ret;
}



#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix {
  $ix ret;
  for(int i=0;i<VECTLENDP;i++) ret.i[i] = vi2.i[i];
  return ret;
}

#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 {
  $ix2 ret;
  for(int i=0;i<VECTLENDP;i++) ret.i[i] = vi.i[i];
  for(int i=VECTLENDP;i<VECTLENDP*2;i++) ret.i[i] = 0;
  return ret;
}

#[inline]
fn vrev21_vd_vd(d0: $f64x) -> $f64x {
  $f64x r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r.d[i*2+0] = d0.d[i*2+1];
    r.d[i*2+1] = d0.d[i*2+0];
  }
  return r;
}

#[inline]
fn vreva2_vd_vd(d0: $f64x) -> $f64x {
  $f64x r;
  for(int i=0;i<VECTLENDP/2;i++) {
    r.d[i*2+0] = d0.d[(VECTLENDP/2-1-i)*2+0];
    r.d[i*2+1] = d0.d[(VECTLENDP/2-1-i)*2+1];
  }
  return r;
}

#[inline]
fn vrev21_vf_vf(d0: $f32x) -> $f32x {
  $f32x r;
  for(int i=0;i<VECTLENSP/2;i++) {
    r.f[i*2+0] = d0.f[i*2+1];
    r.f[i*2+1] = d0.f[i*2+0];
  }
  return r;
}



//

#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { $mox ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = y.u[i] & ~x.u[i]; return ret; }

#[inline]
fn vandnot_vm_vm_vm  (x: $bx, y: $bx)     -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = y.u[i] & ~x.u[i]; return ret; }
#[inline]
fn vor_vm_vm_vm      (x: $bx, y: $bx)     -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] |  y.u[i]; return ret; }

#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $bx)      -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] &  y.u[i]; return ret; }
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $bx)   -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = y.u[i] & ~x.u[i]; return ret; }
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $bx)       -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] |  y.u[i]; return ret; }
#[inline]
fn vxor_vm_vo64_vm(x: $mox, y: $bx)      -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] ^  y.u[i]; return ret; }

#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $bx)      -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] &  y.u[i]; return ret; }
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $bx)   -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = y.u[i] & ~x.u[i]; return ret; }
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $bx)       -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] |  y.u[i]; return ret; }
#[inline]
fn vxor_vm_vo32_vm(x: $mox, y: $bx)      -> $bx { $bx   ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = x.u[i] ^  y.u[i]; return ret; }

//

#[inline]
fn vsel_vd_vo_vd_vd   (o: $mox, x: $f64x, y: $f64x) -> $f64x { $f64x ret; for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = (o.u[i] & x.u[i]) | (y.u[i] & ~o.u[i]); return ret; }
#[inline]
fn   vsel_vi2_vo_vi2_vi2(o: $mox, x: $ix2, y: $ix2)     -> $ix2 { $ix2 ret;   for(int i=0;i<VECTLENDP*2;i++) ret.u[i] = (o.u[i] & x.u[i]) | (y.u[i] & ~o.u[i]); return ret; }

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
fn vtruncate_vi_vd(vd: $f64x) -> $ix { $ix ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = (int)vd.d[i]; return ret; }
#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix { $ix ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = vd.d[i] > 0 ? (int)(vd.d[i] + 0.5) : (int)(vd.d[i] - 0.5); return ret; }
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return $f64x::from(vtruncate_vi_vd(vd)); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return $f64x::from(vrint_vi_vd(vd)); }

#[inline]
fn veq64_vo_vm_vm(x: $bx, y: $bx) -> $mox { $mox ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = x.x[i] == y.x[i] ? -1 : 0; return ret; }
#[inline]
fn vadd64_vm_vm_vm(x: $bx, y: $bx) -> $bx { $bx ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = x.x[i] + y.x[i]; return ret; }

//

impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        $f64x ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = 1.0 / x.d[i];    return ret;
    }
}
impl Abs for $f64x {
    fn abs(self) -> Self {
        $f64x ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = d.x[i] & 0x7fffffffffffffffULL; return ret;
    }
}
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        $f64x ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = x.d[i] * y.d[i] + z.d[i]; return ret;
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        $f64x ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = x.d[i] * y.d[i] - z.d[i]; return ret;
    }
}
#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { $f64x ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = (i & 1) == 0 ?  d.d[i] : -d.d[i]; return ret; }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { $f64x ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = (i & 1) == 0 ? -d.d[i] :  d.d[i]; return ret; }
#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { $f64x ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = (i & 1) == 0 ? x.d[i] - y.d[i] : x.d[i] + y.d[i]; return ret; }
#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vsubadd_vd_vd_vd(x*y, z); }



#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { $ix ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = y.i[i] & ~x.i[i]; return ret; }

#[inline]
fn vand_vi_vo_vi(x: $mox, y: $ix)    -> $ix { vreinterpretFirstHalf_vi_vi2(x) & y }
#[inline]
fn vandnot_vi_vo_vi(x: $mox, y: $ix) -> $ix { return vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(x), y); }

#[inline]
fn vsrl_vi_vi_i(x: $ix, c: int) -> $ix { $ix ret; for(int i=0;i<VECTLENDP;i++) ret.i[i] = ((uint32_t)x.i[i]) >> c; return ret; }


#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix {
  union { $mox vo; $ix2 vi2; } cnv;
  cnv.vo = m;
  vreinterpretFirstHalf_vi_vi2(cnv.vi2) & x) |
		      vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(cnv.vi2), y)
}

#[inline]
fn visinf_vo_vd(d: $f64x)  -> $mox { $mox ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = (d.d[i] == SLEEF_INFINITY || d.d[i] == -SLEEF_INFINITY) ? -1 : 0; return ret; }
#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox { $mox ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = d.d[i] == SLEEF_INFINITY ? -1 : 0; return ret; }
#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox { $mox ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = d.d[i] == -SLEEF_INFINITY ? -1 : 0; return ret; }
#[inline]
fn visnan_vo_vd(d: $f64x)  -> $mox { $mox ret; for(int i=0;i<VECTLENDP;i++) ret.x[i] = d.d[i] != d.d[i] ? -1 : 0; return ret; }

#[inline]
fn vsqrt_vd_vd(d: $f64x) -> $f64x { $f64x ret; for(int i=0;i<VECTLENDP;i++) ret.d[i] = sqrt(d.d[i]); return ret; }



#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x {
  $f64x vd;
  for(int i=0;i<VECTLENDP;i++) vd.d[i] = ptr[vi.i[i]];
  return vd;
}



//

#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { $ix2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = (int)vf.f[i]; return ret; }
#[inline]
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { $ix ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = vf.f[i] > 0 ? (int)(vf.f[i] + 0.5) : (int)(vf.f[i] - 0.5); return ret; }
#[inline]
fn vtruncate_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
#[inline]
fn vrint_vf_vf(vd: $f32x) -> $f32x { return vcast_vf_vi2(vrint_vi2_vf(vd)); }

#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf($f32x::from(i))); }
impl Rec for $f32x {
    #[inline]
    fn rec(self) -> Self {
        $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = 1.0    / x.f[i]; return ret;
    }
}

impl Abs for $f32x {
    fn abs(self) -> Self {
        $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] & 0x7fffffff; return ret;
    }
}

impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = x.f[i] * y.f[i] + z.f[i]; return ret;
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = x.f[i] * y.f[i] - z.f[i]; return ret; }

#[inline]
fn vposneg_vf_vf(x: $f32x) -> $f32x { $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = (i & 1) == 0 ?  x.f[i] : -x.f[i]; return ret; }
#[inline]
fn vnegpos_vf_vf(x: $f32x) -> $f32x { $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = (i & 1) == 0 ? -x.f[i] :  x.f[i]; return ret; }
#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = (i & 1) == 0 ? x.f[i] - y.f[i] : x.f[i] + y.f[i]; return ret; }
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vsubadd_vf_vf_vf(x * y, z); }


#[inline]
fn vandnot_vi2_vi2_vi2(x: $ix, y: $ix) -> $ix { $ix ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = y.i[i] & ~x.i[i]; return ret; }

#[inline]
fn vsel_vf_vo_vf_vf(o: $mox, x: $f32x, y: $f32x) -> $f32x { $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = (o.u[i] & x.u[i]) | (y.u[i] & ~o.u[i]); return ret; }

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

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 {
  union { $mox vo; $ix2 vi2; } cnv;
  cnv.vo = x;
  cnv.vi2 & y
}
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vandnot_vi2_vi2_vi2(x, y); }

#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { $ix2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = ((uint32_t)x.i[i]) >> c; return ret; }

#[inline]
fn visinf_vo_vf (d: $f32x) -> $mox { $mox ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = (d.f[i] == SLEEF_INFINITYf || d.f[i] == -SLEEF_INFINITYf) ? -1 : 0; return ret; }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { $mox ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = d.f[i] == SLEEF_INFINITYf ? -1 : 0; return ret; }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { $mox ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = d.f[i] == -SLEEF_INFINITYf ? -1 : 0; return ret; }
#[inline]
fn visnan_vo_vf (d: $f32x) -> $mox { $mox ret; for(int i=0;i<VECTLENSP;i++) ret.u[i] = d.f[i] != d.f[i] ? -1 : 0; return ret; }

#[inline]
fn   veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { $mox ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] == y.i[i] ? -1 : 0; return ret; }
#[inline]
fn   vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { $mox ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] >  y.i[i] ? -1 : 0; return ret; }

#[inline]
fn vsqrt_vf_vf(x: $f32x) -> $f32x { $f32x ret; for(int i=0;i<VECTLENSP;i++) ret.f[i] = sqrtf(x.f[i]); return ret; }


#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x {
  $f32x vf;
  for(int i=0;i<VECTLENSP;i++) vf.f[i] = ptr[vi2.i[i]];
  return vf;
}


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

