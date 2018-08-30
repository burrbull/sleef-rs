//          Copyright Naoki Shibata 2010 - 2018.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#if CONFIG == 1 || CONFIG == 2

#ifndef __VSX__
#error Please specify -mvsx.
#endif

#else
#error CONFIG macro invalid or not defined
#endif

#define ENABLE_DP
#define LOG2VECTLENDP 1
#define VECTLENDP (1 << LOG2VECTLENDP)

#define ENABLE_SP
#define LOG2VECTLENSP (LOG2VECTLENDP+1)
#define VECTLENSP (1 << LOG2VECTLENSP)

#if CONFIG == 1
#define ENABLE_FMA_DP
#define ENABLE_FMA_SP
//#define SPLIT_KERNEL // Benchmark comparison is needed to determine whether this option should be enabled.
#endif

#define ACCURATE_SQRT
#define FULL_FP_ROUNDING

#include <altivec.h>

#include <stdint.h>
#include "misc.h"

typedef vector unsigned int VMask;
typedef vector unsigned int VOpMask;

typedef vector double VDouble;
typedef vector int VInt;

typedef vector float  VFloat;
typedef vector int VInt2;

//

#[inline]
fn vavailability_i(name: int) -> int { return 3; }

#define ISANAME "VSX"
#define DFTPRIORITY 25

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { }

static VInt2 vloadu_vi2_p(int32_t *p) { return vec_ld(0, p); }
static void vstoreu_v_p_vi2(int32_t *p, VInt2 v) { vec_st(v, 0, p); }
static VInt vloadu_vi_p(int32_t *p) { return vec_ld(0, p); }
static void vstoreu_v_p_vi(int32_t *p, VInt v) { vec_st(v, 0, p); }

#[inline]
fn vload_vd_p(const double *ptr) -> VDouble { return (vector double)vec_ld(0, (const int *)ptr); }
#[inline]
fn vstore_v_p_vd(double *ptr, VDouble v) -> void { vec_st((vector int)v, 0, (int *)ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> VDouble { return (vector double) ( ptr[0], ptr[1] ); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, VDouble v) -> void { ptr[0] = v[0]; ptr[1] = v[1]; }

#[inline]
fn vload_vf_p(const float *ptr) -> VFloat { return (vector float)vec_ld(0, (const int *)ptr); }
#[inline]
fn vstore_v_p_vf(float *ptr, VFloat v) -> void { vec_st((vector int)v, 0, (int *)ptr); }
#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void {
//  *(ptr+(offset + step * 0)*2 + 0) = v[0];
//  *(ptr+(offset + step * 0)*2 + 1) = v[1];
//  *(ptr+(offset + step * 1)*2 + 0) = v[2];
//  *(ptr+(offset + step * 1)*2 + 1) = v[3];
//}

#[inline]
fn vloadu_vf_p(const float *ptr) -> VFloat { return (VFloat) ( ptr[0], ptr[1], ptr[2], ptr[3] ); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, VFloat v) -> void { ptr[0] = v[0]; ptr[1] = v[1]; ptr[2] = v[2]; ptr[3] = v[3]; }

//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { vstore_v_p_vd((double *)(&ptr[2*offset]), v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, VInt vi) -> VDouble {
  int a[VECTLENDP];
  vstoreu_v_p_vi(a, vi);
  return ((VDouble) { ptr[a[0]], ptr[a[1]] });
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, VInt2 vi2) -> VFloat {
  int a[VECTLENSP];
  vstoreu_v_p_vi2(a, vi2);
  return ((VFloat) { ptr[a[0]], ptr[a[1]], ptr[a[2]], ptr[a[3]] });
}
impl VCastI for isize {
    #[inline]
    fn as_vi(self) -> VInt {
        Vint::new(self, self)
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

#[inline]
fn vcast_vd_vi(vi: VInt) -> VDouble { return vec_doubleh(vi); }
#[inline]
fn vcast_vf_vi2(vi: VInt2) -> VFloat { return vec_float(vi); }
#[inline]
fn vrint_vi_vd(vd: VDouble) -> VInt {
  vd = vec_signed(vec_round(vd));
  return vec_perm(vd, vd, (vector unsigned char)(0, 1, 2, 3, 8, 9, 10, 11, 8, 9, 10, 11, 12, 13, 14, 15));
}
#[inline]
fn vrint_vi2_vf(vf: VFloat) -> VInt2 { return vec_signed(vec_round(vf)); }
#[inline]
fn vtruncate_vi_vd(vd: VDouble) -> VInt {
  return vec_perm(vec_signed(vd), vec_signed(vd), (vector unsigned char)(0, 1, 2, 3, 8, 9, 10, 11, 8, 9, 10, 11, 12, 13, 14, 15));
}
#[inline]
fn vtruncate_vi2_vf(vf: VFloat) -> VInt2 { return vec_signed(vf); }
#[inline]
fn vtruncate_vd_vd(vd: VDouble) -> VDouble { return vec_trunc(vd); }
#[inline]
fn vtruncate_vf_vf(vf: VFloat) -> VFloat { return vec_trunc(vf); }
#[inline]
fn vrint_vd_vd(vd: VDouble) -> VDouble { return vec_round(vd); }
#[inline]
fn vrint_vf_vf(vf: VFloat) -> VFloat { return vec_round(vf); }

#[inline]
fn vreinterpret_vm_vd(vd: VDouble) -> VMask { return (VMask)vd; }
#[inline]
fn vreinterpret_vd_vm(vm: VMask) -> VDouble { return (VDouble)vm; }
#[inline]
fn vreinterpret_vi2_vd(vd: VDouble) -> VInt2 { return (VInt2)vd; }
#[inline]
fn vreinterpret_vd_vi2(vi: VInt2) -> VDouble { return (VDouble)vi; }

#[inline]
fn vreinterpret_vm_vf(vf: VFloat) -> VMask { return (VMask)vf; }
#[inline]
fn vreinterpret_vf_vm(vm: VMask) -> VFloat { return (VFloat)vm; }
#[inline]
fn vreinterpret_vf_vi2(vi: VInt2) -> VFloat { return (VFloat)vi; }
#[inline]
fn vreinterpret_vi2_vf(vf: VFloat) -> VInt2 { return (VInt2)vf; }

#[inline]
fn vrec_vd_vd(x: VDouble) -> VDouble { return vec_div((1.).as_vd(), x); }

#[inline]
fn vrec_vf_vf(x: VFloat) -> VFloat { return vec_div((1.).as_vf(), x); }

#[inline]
fn vand_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vec_and(x, y); }
#[inline]
fn vandnot_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vec_andc(y, x); }
#[inline]
fn vor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vec_or(x, y); }
#[inline]
fn vxor_vm_vm_vm(x: VMask, y: VMask) -> VMask { return vec_xor(x, y); }

//#[inline]
//fn vand_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return vec_and(x, y); }
#[inline]
fn vandnot_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return vec_andc(y, x); }
//#[inline]
//fn vor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return vec_or(x, y); }
//#[inline]
//fn vxor_vo_vo_vo(x: VOpMask, y: VOpMask) -> VOpMask { return vec_xor(x, y); }

#[inline]
fn vand_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return vec_and((VMask)x, y); }
#[inline]
fn vandnot_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return vec_andc(y, x); }
#[inline]
fn vor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return vec_or((VMask)x, y); }
#[inline]
fn vxor_vm_vo64_vm(x: VOpMask, y: VMask) -> VMask { return vec_xor((VMask)x, y); }

#[inline]
fn vand_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return vec_and((VMask)x, y); }
#[inline]
fn vandnot_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return vec_andc(y, x); }
#[inline]
fn vor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return vec_or((VMask)x, y); }
#[inline]
fn vxor_vm_vo32_vm(x: VOpMask, y: VMask) -> VMask { return vec_xor((VMask)x, y); }

#[inline]
fn vsel_vd_vo_vd_vd(o: VOpMask, x: VDouble, y: VDouble) -> VDouble { return vec_sel(y, x, (vector unsigned long long)o); }
#[inline]
fn vsel_vf_vo_vf_vf(o: VOpMask, x: VFloat, y: VFloat) -> VFloat { return vec_sel(y, x, o); }
#[inline]
fn vsel_vi2_vo_vi2_vi2(o: VOpMask, x: VInt2, y: VInt2) -> VInt2 { return vec_sel(y, x, o); }

#[inline]
fn vtestallones_i_vo64(g: VOpMask) -> int {
  return vec_all_ne(vec_and(g, (vector unsigned int)(0, 0, 0xffffffff, 0xffffffff)), (vector unsigned int)(0, 0, 0, 0));
}
#[inline]
fn vtestallones_i_vo32(g: VOpMask) -> int { return vec_all_ne(g, (vector unsigned int)(0, 0, 0, 0)); }

#[inline]
fn vcast_vo32_vo64(m: VOpMask) -> VOpMask { return vec_perm(m, m, (vector unsigned char)(4, 5, 6, 7, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15 )); }
#[inline]
fn vcast_vo64_vo32(m: VOpMask) -> VOpMask { return vec_perm(m, m, (vector unsigned char)(0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 6, 7, 4, 5, 6, 7)); }

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> VMask { return (VMask){ l, h, l, h }; }

impl VCastI2 for VInt {
    #[inline]
    fn as_vi2(self) -> VInt2 {
        VInt2::new( 0, self[0], 0, self[1] )
    }
}
//#[inline]
//fn vcastu_vi2_vi(vi: VInt) -> VInt2 { return ; }
impl VCastI for VInt2 {
    #[inline]
    fn as_vi(self) -> VInt {
        VInt2::new( self[1], self[3] )
    }
}
//#[inline]
//fn vcastu_vi_vi2(vi2: VInt2) -> VInt { return (VInt){ vi2[1], vi2[3] }; }

#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: VInt2) -> VInt { return (VInt){ vi2[0], vi2[1] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: VInt) -> VInt2 { return (VInt2){ vi[0], vi[1], 0, 0 }; }

#[inline]
fn vrev21_vd_vd(vd: VDouble) -> VDouble { return vec_perm(vd, vd, (vector unsigned char)(8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7)); }
#[inline]
fn vreva2_vd_vd(vd: VDouble) -> VDouble { return vd; }
#[inline]
fn vrev21_vf_vf(vf: VFloat) -> VFloat { return vec_perm(vf, vf, (vector unsigned char)(4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11)); }
#[inline]
fn vreva2_vf_vf(vf: VFloat) -> VFloat { return vec_perm(vf, vf, (vector unsigned char)(8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7)); }
#[inline]
fn vrev21_vi2_vi2(i: VInt2) -> VInt2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn veq64_vo_vm_vm(x: VMask, y: VMask) -> VOpMask {
  VOpMask o = vec_cmpeq(x, y);
  return o & vec_perm(o, o, (vector unsigned char)(4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11));
}

#[inline]
fn vadd64_vm_vm_vm(x: VMask, y: VMask) -> VMask {
  return (VMask)vec_add((vector long long)x, (vector long long)y);
}

//

#define PNMASK ((VDouble) { +0.0, -0.0 })
#define NPMASK ((VDouble) { -0.0, +0.0 })
#define PNMASKf ((VFloat) { +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf ((VFloat) { -0.0f, +0.0f, -0.0f, +0.0f })

#[inline]
fn vposneg_vd_vd(d: VDouble) -> VDouble { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(PNMASK))); }
#[inline]
fn vnegpos_vd_vd(d: VDouble) -> VDouble { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(NPMASK))); }
#[inline]
fn vposneg_vf_vf(d: VFloat) -> VFloat { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(PNMASKf))); }
#[inline]
fn vnegpos_vf_vf(d: VFloat) -> VFloat { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(NPMASKf))); }

//

#[inline]
fn vabs_vd_vd(d: VDouble) -> VDouble { return vec_abs(d); }
#[inline]
fn vsubadd_vd_vd_vd(x: VDouble, y: VDouble) -> VDouble { return x + vnegpos_vd_vd(y); }
#[inline]
fn vsqrt_vd_vd(d: VDouble) -> VDouble { return vec_sqrt(d); }

#if CONFIG == 1
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        vec_madd(x, y, z)
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vec_msub(x, y, z); }
#else
impl Mla for VDouble {
    fn mla(self, y: Self, z: Self) -> Self {
        x*y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return x*y - z; }
#endif

#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return x.mla(y, vnegpos_vd_vd(z)); }
#[inline]
fn vfma_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vec_madd(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vec_madd(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vec_msub(x, y, z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vec_nmsub(x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: VDouble, y: VDouble, z: VDouble) -> VDouble { return vec_nmadd(x, y, z); }

#[inline]
fn vabs_vf_vf(f: VFloat) -> VFloat { return vec_abs(f); }

#[inline]
fn vsubadd_vf_vf_vf(x: VFloat, y: VFloat) -> VFloat { return x + vnegpos_vf_vf(y); }
#[inline]
fn vsqrt_vf_vf(d: VFloat) -> VFloat { return vec_sqrt(d); }

#if CONFIG == 1
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        vec_madd(x, y, z)
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vec_nmsub(x, y, z); }
#else
impl Mla for VFloat {
    fn mla(self, y: Self, z: Self) -> Self {
        x*y +z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return z - x * y); }
#endif
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return x.mla(y, vnegpos_vf_vf(z)); }
#[inline]
fn vfma_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vec_madd(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vec_madd(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vec_msub(x, y, z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vec_nmsub(x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: VFloat, y: VFloat, z: VFloat) -> VFloat { return vec_nmadd(x, y, z); }

//

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

//

#[inline]
fn vnot_vo_vo(o: VOpMask) -> VOpMask { return vec_nand(o, o); }


impl std::ops::Add for VInt {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        vec_add(x, y)
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vec_add(x, y); }
impl std::ops::Sub for VInt {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        vec_sub(x, y)
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vec_sub(x, y); }
impl std::ops::Neg for VInt {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        vec_neg(e)
    }
}
//#[inline]
//fn vneg_vi_vi(e: VInt) -> VInt { return vec_neg(e); }

#[inline]
fn vand_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vec_and(x, y); }
#[inline]
fn vandnot_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vec_andc(y, x); }
#[inline]
fn vor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vec_or(x, y); }
#[inline]
fn vxor_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vec_xor(x, y); }

#[inline]
fn vand_vi_vo_vi(x: VOpMask, y: VInt) -> VInt { return vreinterpretFirstHalf_vi_vi2((VInt2)x) & y; }
#[inline]
fn vandnot_vi_vo_vi(x: VOpMask, y: VInt) -> VInt { return vec_andc(y, vreinterpretFirstHalf_vi_vi2((VInt2)x)); }

#[inline]
fn vsll_vi_vi_i(x: VInt, c: int) -> VInt { return vec_sl (x, (vector unsigned int)(c, c, c, c)); }
#[inline]
fn vsrl_vi_vi_i(x: VInt, c: int) -> VInt { return vec_sr (x, (vector unsigned int)(c, c, c, c)); }
#[inline]
fn vsra_vi_vi_i(x: VInt, c: int) -> VInt { return vec_sra(x, (vector unsigned int)(c, c, c, c)); }

#[inline]
fn veq_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vec_cmpeq(x, y); }
#[inline]
fn vgt_vi_vi_vi(x: VInt, y: VInt) -> VInt { return vec_cmpgt(x, y); }

#[inline]
fn veq_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return (VOpMask)vreinterpretFirstHalf_vi2_vi(vec_cmpeq(x, y)); }
#[inline]
fn vgt_vo_vi_vi(x: VInt, y: VInt) -> VOpMask { return (VOpMask)vreinterpretFirstHalf_vi2_vi(vec_cmpgt(x, y));}

#[inline]
fn vsel_vi_vo_vi_vi(m: VOpMask, x: VInt, y: VInt) -> VInt {
  return vor_vi_vi_vi(vand_vi_vi_vi(vreinterpretFirstHalf_vi_vi2((VInt2)m), x),
		      vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2((VInt2)m), y));
}

#[inline]
fn visinf_vo_vd(d: VDouble) -> VOpMask { return (VOpMask)(vec_cmpeq(vabs_vd_vd(d), SLEEF_INFINITY.as_vd())); }
#[inline]
fn vispinf_vo_vd(d: VDouble) -> VOpMask { return (VOpMask)(vec_cmpeq(d, SLEEF_INFINITY.as_vd())); }
#[inline]
fn visminf_vo_vd(d: VDouble) -> VOpMask { return (VOpMask)(vec_cmpeq(d, (-SLEEF_INFINITY).as_vd())); }
#[inline]
fn visnan_vo_vd(d: VDouble) -> VOpMask { return (VOpMask)(vnot_vo_vo(vec_cmpeq(d, d))); }

#[inline]
fn vcast_d_vd(v: VDouble) -> double { return v[0]; }
#[inline]
fn vcast_f_vf(v: VFloat) -> float { return v[0]; }

//#[inline]
//fn vstream_v_p_vd(double *ptr, VDouble v) -> void { vstore_v_p_vd(ptr, v); }
//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, VDouble v) -> void { vscatter2_v_p_i_i_vd(ptr, offset, step, v); }

//

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
fn vadd_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vec_add(x, y); }
#[inline]
fn vsub_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vec_sub(x, y); }
#[inline]
fn vneg_vi2_vi2(e: VInt2) -> VInt2 { return vec_neg(e); }

#[inline]
fn vand_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vec_and(x, y); }
#[inline]
fn vandnot_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vec_andc(y, x); }
#[inline]
fn vor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vec_or(x, y); }
#[inline]
fn vxor_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vec_xor(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return (VInt2)vec_and((VInt2)x, y); }
#[inline]
fn vandnot_vi2_vo_vi2(x: VOpMask, y: VInt2) -> VInt2 { return vec_andc(y, (VInt2)x); }

#[inline]
fn vsll_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { return vec_sl (x, (vector unsigned int)(c, c, c, c)); }
#[inline]
fn vsrl_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { return vec_sr (x, (vector unsigned int)(c, c, c, c)); }
#[inline]
fn vsra_vi2_vi2_i(x: VInt2, c: int) -> VInt2 { return vec_sra(x, (vector unsigned int)(c, c, c, c)); }

#[inline]
fn veq_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return (VOpMask)vec_cmpeq(x, y); }
#[inline]
fn vgt_vo_vi2_vi2(VInt2 x, VInt2 y) -> VOpMask { return (VOpMask)vec_cmpgt(x, y); }
#[inline]
fn veq_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vec_cmpeq(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2(VInt2 x, VInt2 y) -> VInt2 { return vec_cmpgt(x, y); }

#[inline]
fn visinf_vo_vf(d: VFloat) -> VOpMask { return (VOpMask)vec_cmpeq(vabs_vf_vf(d), SLEEF_INFINITY_F.as_vf()); }
#[inline]
fn vispinf_vo_vf(d: VFloat) -> VOpMask { return (VOpMask)vec_cmpeq(d, SLEEF_INFINITY_F.as_vf()); }
#[inline]
fn visminf_vo_vf(d: VFloat) -> VOpMask { return (VOpMask)vec_cmpeq(d, (-SLEEF_INFINITY_F).as_vf()); }
#[inline]
fn visnan_vo_vf(d: VFloat) -> VOpMask { return (VOpMask)vnot_vo_vo(vec_cmpeq(d, d)); }

//#[inline]
//fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, VFloat v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }
//#[inline]
//fn vstream_v_p_vf(float *ptr, VFloat v) -> void { vstore_v_p_vf(ptr, v); }
