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

typedef vector unsigned int $mx;
typedef vector unsigned int $mox;

typedef vector double $f64x;
typedef vector int $ix;

typedef vector float  $f32x;
typedef vector int $ix2;

//

#[inline]
fn vavailability_i(name: int) -> int { return 3; }

#define ISANAME "VSX"
#define DFTPRIORITY 25

#[inline]
fn vprefetch_v_p(const void *ptr) -> void { }

static $ix2 vloadu_vi2_p(int32_t *p) { return vec_ld(0, p); }
static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { vec_st(v, 0, p); }
static $ix vloadu_vi_p(int32_t *p) { return vec_ld(0, p); }
static void vstoreu_v_p_vi(int32_t *p, $ix v) { vec_st(v, 0, p); }

#[inline]
fn vload_vd_p(const double *ptr) -> $f64x { return (vector double)vec_ld(0, (const int *)ptr); }
#[inline]
fn vstore_v_p_vd(double *ptr, $f64x v) -> void { vec_st((vector int)v, 0, (int *)ptr); }
#[inline]
fn vloadu_vd_p(const double *ptr) -> $f64x { return (vector double) ( ptr[0], ptr[1] ); }
#[inline]
fn vstoreu_v_p_vd(double *ptr, $f64x v) -> void { ptr[0] = v[0]; ptr[1] = v[1]; }

#[inline]
fn vload_vf_p(const float *ptr) -> $f32x { return (vector float)vec_ld(0, (const int *)ptr); }
#[inline]
fn vstore_v_p_vf(float *ptr, $f32x v) -> void { vec_st((vector int)v, 0, (int *)ptr); }
#[inline]
//fn vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void {
//  *(ptr+(offset + step * 0)*2 + 0) = v[0];
//  *(ptr+(offset + step * 0)*2 + 1) = v[1];
//  *(ptr+(offset + step * 1)*2 + 0) = v[2];
//  *(ptr+(offset + step * 1)*2 + 1) = v[3];
//}

#[inline]
fn vloadu_vf_p(const float *ptr) -> $f32x { return ($f32x) ( ptr[0], ptr[1], ptr[2], ptr[3] ); }
#[inline]
fn vstoreu_v_p_vf(float *ptr, $f32x v) -> void { ptr[0] = v[0]; ptr[1] = v[1]; ptr[2] = v[2]; ptr[3] = v[3]; }

//#[inline]
//fn vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void { vstore_v_p_vd((double *)(&ptr[2*offset]), v); }

#[inline]
fn vgather_vd_p_vi(const double *ptr, $ix vi) -> $f64x {
  int a[VECTLENDP];
  vstoreu_v_p_vi(a, vi);
  return (($f64x) { ptr[a[0]], ptr[a[1]] });
}

#[inline]
fn vgather_vf_p_vi2(const float *ptr, $ix2 vi2) -> $f32x {
  int a[VECTLENSP];
  vstoreu_v_p_vi2(a, vi2);
  return (($f32x) { ptr[a[0]], ptr[a[1]], ptr[a[2]], ptr[a[3]] });
}

#[inline]
fn vrint_vi_vd(vd: $f64x) -> $ix {
  vd = vec_signed(vec_round(vd));
  return vec_perm(vd, vd, (vector unsigned char)(0, 1, 2, 3, 8, 9, 10, 11, 8, 9, 10, 11, 12, 13, 14, 15));
}
#[inline]
fn vrint_vi2_vf(vf: $f32x) -> $ix2 { return vec_signed(vec_round(vf)); }
#[inline]
fn vtruncate_vi_vd(vd: $f64x) -> $ix {
  return vec_perm(vec_signed(vd), vec_signed(vd), (vector unsigned char)(0, 1, 2, 3, 8, 9, 10, 11, 8, 9, 10, 11, 12, 13, 14, 15));
}
#[inline]
fn vtruncate_vi2_vf(vf: $f32x) -> $ix2 { return vec_signed(vf); }
#[inline]
fn vtruncate_vd_vd(vd: $f64x) -> $f64x { return vec_trunc(vd); }
#[inline]
fn vtruncate_vf_vf(vf: $f32x) -> $f32x { return vec_trunc(vf); }
#[inline]
fn vrint_vd_vd(vd: $f64x) -> $f64x { return vec_round(vd); }
#[inline]
fn vrint_vf_vf(vf: $f32x) -> $f32x { return vec_round(vf); }

#[inline]
fn vreinterpret_vm_vd(vd: $f64x) -> $mx { return ($mx)vd; }
#[inline]
fn vreinterpret_vd_vm(vm: $mx) -> $f64x { return ($f64x)vm; }
#[inline]
fn vreinterpret_vi2_vd(vd: $f64x) -> $ix2 { return ($ix2)vd; }
#[inline]
fn vreinterpret_vd_vi2(vi: $ix2) -> $f64x { return ($f64x)vi; }

#[inline]
fn vreinterpret_vm_vf(vf: $f32x) -> $mx { return ($mx)vf; }
#[inline]
fn vreinterpret_vf_vm(vm: $mx) -> $f32x { return ($f32x)vm; }
#[inline]
fn vreinterpret_vf_vi2(vi: $ix2) -> $f32x { return ($f32x)vi; }
#[inline]
fn vreinterpret_vi2_vf(vf: $f32x) -> $ix2 { return ($ix2)vf; }

#[inline]
fn vrec_vd_vd(x: $f64x) -> $f64x { return vec_div($f64x::splat(1.), x); }

#[inline]
fn vrec_vf_vf(x: $f32x) -> $f32x { return vec_div($f32x::splat(1.), x); }

#[inline]
fn vand_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vec_and(x, y); }
#[inline]
fn vandnot_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vec_andc(y, x); }
#[inline]
fn vor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vec_or(x, y); }
#[inline]
fn vxor_vm_vm_vm(x: $mx, y: $mx) -> $mx { return vec_xor(x, y); }

//#[inline]
//fn vand_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vec_and(x, y); }
#[inline]
fn vandnot_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vec_andc(y, x); }
//#[inline]
//fn vor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vec_or(x, y); }
//#[inline]
//fn vxor_vo_vo_vo(x: $mox, y: $mox) -> $mox { return vec_xor(x, y); }

#[inline]
fn vand_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vec_and(($mx)x, y); }
#[inline]
fn vandnot_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vec_andc(y, x); }
#[inline]
fn vor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vec_or(($mx)x, y); }
#[inline]
fn vxor_vm_vo64_vm(x: $mox, y: $mx) -> $mx { return vec_xor(($mx)x, y); }

#[inline]
fn vand_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vec_and(($mx)x, y); }
#[inline]
fn vandnot_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vec_andc(y, x); }
#[inline]
fn vor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vec_or(($mx)x, y); }
#[inline]
fn vxor_vm_vo32_vm(x: $mox, y: $mx) -> $mx { return vec_xor(($mx)x, y); }

#[inline]
fn vsel_vd_vo_vd_vd(o: $mox, x: $f64x, y: $f64x) -> $f64x { return vec_sel(y, x, (vector unsigned long long)o); }
#[inline]
fn vsel_vf_vo_vf_vf(o: $mox, x: $f32x, y: $f32x) -> $f32x { return vec_sel(y, x, o); }
#[inline]
fn vsel_vi2_vo_vi2_vi2(o: $mox, x: $ix2, y: $ix2) -> $ix2 { return vec_sel(y, x, o); }

#[inline]
fn vtestallones_i_vo64(g: $mox) -> int {
  return vec_all_ne(vec_and(g, (vector unsigned int)(0, 0, 0xffffffff, 0xffffffff)), (vector unsigned int)(0, 0, 0, 0));
}
#[inline]
fn vtestallones_i_vo32(g: $mox) -> int { return vec_all_ne(g, (vector unsigned int)(0, 0, 0, 0)); }

#[inline]
fn vcast_vo32_vo64(m: $mox) -> $mox { return vec_perm(m, m, (vector unsigned char)(4, 5, 6, 7, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15 )); }
#[inline]
fn vcast_vo64_vo32(m: $mox) -> $mox { return vec_perm(m, m, (vector unsigned char)(0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 6, 7, 4, 5, 6, 7)); }

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $mx { return ($mx){ l, h, l, h }; }


#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix { return ($ix){ vi2[0], vi2[1] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 { return ($ix2){ vi[0], vi[1], 0, 0 }; }

#[inline]
fn vrev21_vd_vd(vd: $f64x) -> $f64x { return vec_perm(vd, vd, (vector unsigned char)(8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7)); }
#[inline]
fn vreva2_vd_vd(vd: $f64x) -> $f64x { return vd; }
#[inline]
fn vrev21_vf_vf(vf: $f32x) -> $f32x { return vec_perm(vf, vf, (vector unsigned char)(4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11)); }
#[inline]
fn vreva2_vf_vf(vf: $f32x) -> $f32x { return vec_perm(vf, vf, (vector unsigned char)(8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6, 7)); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return vreinterpret_vi2_vf(vrev21_vf_vf(vreinterpret_vf_vi2(i))); }

#[inline]
fn veq64_vo_vm_vm(x: $mx, y: $mx) -> $mox {
  $mox o = vec_cmpeq(x, y);
  return o & vec_perm(o, o, (vector unsigned char)(4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11));
}

#[inline]
fn vadd64_vm_vm_vm(x: $mx, y: $mx) -> $mx {
  return ($mx)vec_add((vector long long)x, (vector long long)y);
}

//

#define PNMASK (($f64x) { +0.0, -0.0 })
#define NPMASK (($f64x) { -0.0, +0.0 })
#define PNMASKf (($f32x) { +0.0f, -0.0f, +0.0f, -0.0f })
#define NPMASKf (($f32x) { -0.0f, +0.0f, -0.0f, +0.0f })

#[inline]
fn vposneg_vd_vd(d: $f64x) -> $f64x { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(PNMASK))); }
#[inline]
fn vnegpos_vd_vd(d: $f64x) -> $f64x { return vreinterpret_vd_vm(vxor_vm_vm_vm(vreinterpret_vm_vd(d), vreinterpret_vm_vd(NPMASK))); }
#[inline]
fn vposneg_vf_vf(d: $f32x) -> $f32x { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(PNMASKf))); }
#[inline]
fn vnegpos_vf_vf(d: $f32x) -> $f32x { return vreinterpret_vf_vm(vxor_vm_vm_vm(vreinterpret_vm_vf(d), vreinterpret_vm_vf(NPMASKf))); }

//

#[inline]
fn vabs_vd_vd(d: $f64x) -> $f64x { return vec_abs(d); }
#[inline]
fn vsubadd_vd_vd_vd(x: $f64x, y: $f64x) -> $f64x { return x + vnegpos_vd_vd(y); }
#[inline]
fn vsqrt_vd_vd(d: $f64x) -> $f64x { return vec_sqrt(d); }

#if CONFIG == 1
impl Mla for $f64x {
    fn mla(self, y: Self, z: Self) -> Self {
        vec_madd(x, y, z)
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vec_msub(x, y, z); }
#else
impl Mla for $f64x {
    fn mla(self, y: Self, z: Self) -> Self {
        x*y + z
    }
}
#[inline]
fn vmlapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x*y - z; }
#endif

#[inline]
fn vmlsubadd_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return x.mla(y, vnegpos_vd_vd(z)); }
#[inline]
fn vfma_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vec_madd(x, y, z); }
#[inline]
fn vfmapp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vec_madd(x, y, z); }
#[inline]
fn vfmapn_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vec_msub(x, y, z); }
#[inline]
fn vfmanp_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vec_nmsub(x, y, z); }
#[inline]
fn vfmann_vd_vd_vd_vd(x: $f64x, y: $f64x, z: $f64x) -> $f64x { return vec_nmadd(x, y, z); }

#[inline]
fn vabs_vf_vf(f: $f32x) -> $f32x { return vec_abs(f); }

#[inline]
fn vsubadd_vf_vf_vf(x: $f32x, y: $f32x) -> $f32x { return x + vnegpos_vf_vf(y); }
#[inline]
fn vsqrt_vf_vf(d: $f32x) -> $f32x { return vec_sqrt(d); }

#if CONFIG == 1
impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        vec_madd(x, y, z)
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vec_nmsub(x, y, z); }
#else
impl Mla for $f32x {
    fn mla(self, y: Self, z: Self) -> Self {
        x*y +z
    }
}
#[inline]
fn vmlanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return z - x * y); }
#endif
#[inline]
fn vmlsubadd_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return x.mla(y, vnegpos_vf_vf(z)); }
#[inline]
fn vfma_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vec_madd(x, y, z); }
#[inline]
fn vfmapp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vec_madd(x, y, z); }
#[inline]
fn vfmapn_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vec_msub(x, y, z); }
#[inline]
fn vfmanp_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vec_nmsub(x, y, z); }
#[inline]
fn vfmann_vf_vf_vf_vf(x: $f32x, y: $f32x, z: $f32x) -> $f32x { return vec_nmadd(x, y, z); }

//

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

//

#[inline]
fn vnot_vo_vo(o: $mox) -> $mox { return vec_nand(o, o); }


impl std::ops::Add for $ix {
    type Output = Self;
    #[inline]
    fn add(self, other: Self) -> Self {
        vec_add(x, y)
    }
}
//#[inline]
//fn vadd_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_add(x, y); }
impl std::ops::Sub for $ix {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self {
        vec_sub(x, y)
    }
}
//#[inline]
//fn vsub_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_sub(x, y); }
impl std::ops::Neg for $ix {
    type Output = Self;
    #[inline]
    fn neg(self) -> Self {
        vec_neg(e)
    }
}
//#[inline]
//fn vneg_vi_vi(e: $ix) -> $ix { return vec_neg(e); }

#[inline]
fn vand_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_and(x, y); }
#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_andc(y, x); }
#[inline]
fn vor_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_or(x, y); }
#[inline]
fn vxor_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_xor(x, y); }

#[inline]
fn vand_vi_vo_vi(x: $mox, y: $ix) -> $ix { return vreinterpretFirstHalf_vi_vi2(($ix2)x) & y; }
#[inline]
fn vandnot_vi_vo_vi(x: $mox, y: $ix) -> $ix { return vec_andc(y, vreinterpretFirstHalf_vi_vi2(($ix2)x)); }

#[inline]
fn vsll_vi_vi_i(x: $ix, c: int) -> $ix { return vec_sl (x, (vector unsigned int)(c, c, c, c)); }
#[inline]
fn vsrl_vi_vi_i(x: $ix, c: int) -> $ix { return vec_sr (x, (vector unsigned int)(c, c, c, c)); }
#[inline]
fn vsra_vi_vi_i(x: $ix, c: int) -> $ix { return vec_sra(x, (vector unsigned int)(c, c, c, c)); }

#[inline]
fn veq_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_cmpeq(x, y); }
#[inline]
fn vgt_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_cmpgt(x, y); }

#[inline]
fn veq_vo_vi_vi(x: $ix, y: $ix) -> $mox { return ($mox)vreinterpretFirstHalf_vi2_vi(vec_cmpeq(x, y)); }
#[inline]
fn vgt_vo_vi_vi(x: $ix, y: $ix) -> $mox { return ($mox)vreinterpretFirstHalf_vi2_vi(vec_cmpgt(x, y));}

#[inline]
fn vsel_vi_vo_vi_vi(m: $mox, x: $ix, y: $ix) -> $ix {
  return vor_vi_vi_vi(vand_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(($ix2)m), x),
		      vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(($ix2)m), y));
}

#[inline]
fn visinf_vo_vd(d: $f64x) -> $mox { return ($mox)(vec_cmpeq(vabs_vd_vd(d), $f64x::splat(SLEEF_INFINITY))); }
#[inline]
fn vispinf_vo_vd(d: $f64x) -> $mox { return ($mox)(vec_cmpeq(d, $f64x::splat(SLEEF_INFINITY))); }
#[inline]
fn visminf_vo_vd(d: $f64x) -> $mox { return ($mox)(vec_cmpeq(d, $f64x::splat(-SLEEF_INFINITY))); }
#[inline]
fn visnan_vo_vd(d: $f64x) -> $mox { return ($mox)(vnot_vo_vo(vec_cmpeq(d, d))); }

#[inline]
fn vcast_d_vd(v: $f64x) -> double { return v[0]; }
#[inline]
fn vcast_f_vf(v: $f32x) -> float { return v[0]; }

//#[inline]
//fn vstream_v_p_vd(double *ptr, $f64x v) -> void { vstore_v_p_vd(ptr, v); }
//#[inline]
//fn vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, $f64x v) -> void { vscatter2_v_p_i_i_vd(ptr, offset, step, v); }

//

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
fn vcast_vi2_vm(vm: $mx) -> $ix2 { return ($ix2)vm; }
#[inline]
fn vcast_vm_vi2(vi: $ix2) -> $mx { return ($mx)vi; }

#[inline]
fn vadd_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_add(x, y); }
#[inline]
fn vsub_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_sub(x, y); }
#[inline]
fn vneg_vi2_vi2(e: $ix2) -> $ix2 { return vec_neg(e); }

#[inline]
fn vand_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_and(x, y); }
#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_andc(y, x); }
#[inline]
fn vor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_or(x, y); }
#[inline]
fn vxor_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_xor(x, y); }

#[inline]
fn vand_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return ($ix2)vec_and(($ix2)x, y); }
#[inline]
fn vandnot_vi2_vo_vi2(x: $mox, y: $ix2) -> $ix2 { return vec_andc(y, ($ix2)x); }

#[inline]
fn vsll_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return vec_sl (x, (vector unsigned int)(c, c, c, c)); }
#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return vec_sr (x, (vector unsigned int)(c, c, c, c)); }
#[inline]
fn vsra_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return vec_sra(x, (vector unsigned int)(c, c, c, c)); }

#[inline]
fn veq_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return ($mox)vec_cmpeq(x, y); }
#[inline]
fn vgt_vo_vi2_vi2($ix2 x, $ix2 y) -> $mox { return ($mox)vec_cmpgt(x, y); }
#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_cmpeq(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_cmpgt(x, y); }

#[inline]
fn visinf_vo_vf(d: $f32x) -> $mox { return ($mox)vec_cmpeq(vabs_vf_vf(d), $f32x::splat(SLEEF_INFINITY_F)); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $mox { return ($mox)vec_cmpeq(d, $f32x::splat(SLEEF_INFINITY_F)); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $mox { return ($mox)vec_cmpeq(d, $f32x::splat(-SLEEF_INFINITY_F); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $mox { return ($mox)vnot_vo_vo(vec_cmpeq(d, d)); }

//#[inline]
//fn vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, $f32x v) -> void { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }
//#[inline]
//fn vstream_v_p_vf(float *ptr, $f32x v) -> void { vstore_v_p_vf(ptr, v); }
