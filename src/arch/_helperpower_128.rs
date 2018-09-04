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

typedef vector unsigned int $ux;
typedef vector unsigned int $ox;

typedef vector double $f64x;
typedef vector int $ix;

typedef vector float  $f32x;
typedef vector int $ix2;

//

#define ISANAME "VSX"
#define DFTPRIORITY 25


static void vstoreu_v_p_vi2(int32_t *p, $ix2 v) { vec_st(v, 0, p); }
static void vstoreu_v_p_vi(int32_t *p, $ix v) { vec_st(v, 0, p); }



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

impl Truncate for $f64x {
    #[inline]
    fn truncate(self) -> Self {
        vec_trunc(vd)
    }
}
impl Truncate for $f32x {
    #[inline]
    fn truncate(self) -> Self {
        vec_trunc(vf)
    }
}

impl RInt for $f64x {
    #[inline]
    fn rint(self) -> Self {
        vec_round(vd)
    }
}
impl RInt for $f32x {
    #[inline]
    fn rint(self) -> Self {
      vec_round(vf)
    }
}

impl Rec for $f64x {
    #[inline]
    fn rec(self) -> Self {
        vec_div(Self::splat(1.), self)
    }
}
impl Rec for $f32x {
    #[inline]
    fn rec(self) -> Self {
        vec_div(Self::splat(1.), self)
    }
}

#[inline]
fn vandnot_vm_vm_vm(x: $ux, y: $ux) -> $ux { return vec_andc(y, x); }

#[inline]
fn vandnot_vo_vo_vo(x: $ox, y: $ox) -> $ox { return vec_andc(y, x); }

#[inline]
fn vand_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return vec_and(($ux)x, y); }
#[inline]
fn vandnot_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return vec_andc(y, x); }
#[inline]
fn vor_vm_vo64_vm(x: $ox, y: $ux) -> $ux { return vec_or(($ux)x, y); }

#[inline]
fn vand_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return vec_and(($ux)x, y); }
#[inline]
fn vandnot_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return vec_andc(y, x); }
#[inline]
fn vor_vm_vo32_vm(x: $ox, y: $ux) -> $ux { return vec_or(($ux)x, y); }

#[inline]
fn vtestallones_i_vo64(g: $ox) -> int {
  return vec_all_ne(vec_and(g, (vector unsigned int)(0, 0, 0xffffffff, 0xffffffff)), (vector unsigned int)(0, 0, 0, 0));
}
#[inline]
fn vtestallones_i_vo32(g: $ox) -> int { return vec_all_ne(g, (vector unsigned int)(0, 0, 0, 0)); }

#[inline]
fn $m32x::from(m: $ox) -> $ox { return vec_perm(m, m, (vector unsigned char)(4, 5, 6, 7, 12, 13, 14, 15, 8, 9, 10, 11, 12, 13, 14, 15 )); }
#[inline]
fn $mx::from(m: $ox) -> $ox { return vec_perm(m, m, (vector unsigned char)(0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 6, 7, 4, 5, 6, 7)); }

#[inline]
fn vcast_vm_i_i(h: int, l: int) -> $ux { return ($ux){ l, h, l, h }; }


#[inline]
fn vreinterpretFirstHalf_vi_vi2(vi2: $ix2) -> $ix { return ($ix){ vi2[0], vi2[1] }; }
#[inline]
fn vreinterpretFirstHalf_vi2_vi(vi: $ix) -> $ix2 { return ($ix2){ vi[0], vi[1], 0, 0 }; }

#[inline]
fn vrev21_vf_vf(vf: $f32x) -> $f32x { return vec_perm(vf, vf, (vector unsigned char)(4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11)); }
#[inline]
fn vrev21_vi2_vi2(i: $ix2) -> $ix2 { return $ix2::from(vrev21_vf_vf($f32x::from(i))); }

#[inline]
fn veq64_vo_vm_vm(x: $ux, y: $ux) -> $ox {
  $ox o = vec_cmpeq(x, y);
  return o & vec_perm(o, o, (vector unsigned char)(4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11));
}

#[inline]
fn vadd64_vm_vm_vm(x: $ux, y: $ux) -> $ux {
  return ($ux)vec_add((vector long long)x, (vector long long)y);
}


impl Abs for $f64x {
    fn abs(self) -> Self {
        vec_abs(d);
    }
}
impl Sqrt for $f64x {
    #[inline]
    fn sqrt(self) -> Self {
        vec_sqrt(d)
    }
}

#if CONFIG == 1
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        vec_madd(x, y, z)
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        vec_msub(x, y, z)
    }
}
#else
impl Mla for $f64x {
    #[inline]
    fn mla(self, y: Self, z: Self) -> Self {
        x*y + z
    }
    #[inline]
    fn mlapn(self, y: Self, z: Self) -> Self {
        x*y - z
    }
}
#endif


impl Fma for $f64x {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
      vec_madd(x, y, z)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self {
      vec_msub(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
      vec_nmsub(x, y, z)
    }
}

impl Abs for $f32x {
    fn abs(self) -> Self {
        vec_abs(f)
    }
}

impl Sqrt for $f32x {
    #[inline]
    fn sqrt(self) -> Self {
        vec_sqrt(d)
    }
}

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



impl Fma for $f32x {
    #[inline]
    fn fma(self, y: Self, z: Self) -> Self {
        vec_madd(x, y, z)
    }
    #[inline]
    fn fmapn(self, y: Self, z: Self) -> Self {
        vec_msub(x, y, z)
    }
    #[inline]
    fn fmanp(self, y: Self, z: Self) -> Self {
        vec_nmsub(x, y, z)
    }
}
//

#[inline]
fn vsel_vd_vo_d_d(o: $ox, v1: f64, v0: f64) -> CONST -> $f64x {
  o.select($f64x::splat(v1), $f64x::splat(v0))
}

#[inline]
fn vsel_vd_vo_vo_d_d_d(o0: $ox, o1: $ox, d0: f64, d1: f64, d2: f64) -> $f64x {
  o0.select($f64x::splat(d0), vsel_vd_vo_d_d(o1, d1, d2))
}

#[inline]
fn vsel_vd_vo_vo_vo_d_d_d_d(o0: $ox, o1: $ox, o2: $ox, d0: f64, d1: f64, d2: f64, d3: f64) -> $f64x {
  o0.select($f64x::splat(d0), o1.select($f64x::splat(d1), vsel_vd_vo_d_d(o2, d2, d3)))
}

//

#[inline]
fn vnot_vo_vo(o: $ox) -> $ox { return vec_nand(o, o); }



#[inline]
fn vandnot_vi_vi_vi(x: $ix, y: $ix) -> $ix { return vec_andc(y, x); }

#[inline]
fn vand_vi_vo_vi(x: $ox, y: $ix) -> $ix { return vreinterpretFirstHalf_vi_vi2(($ix2)x) & y; }
#[inline]
fn vandnot_vi_vo_vi(x: $ox, y: $ix) -> $ix { return vec_andc(y, vreinterpretFirstHalf_vi_vi2(($ix2)x)); }

#[inline]
fn vsrl_vi_vi_i(x: $ix, c: int) -> $ix { return vec_sr (x, (vector unsigned int)(c, c, c, c)); }

#[inline]
fn visinf_vo_vd(d: $f64x) -> $ox { return ($ox)(vec_cmpeq(d.abs(), $f64x::splat(SLEEF_INFINITY))); }
#[inline]
fn vispinf_vo_vd(d: $f64x) -> $ox { return ($ox)(vec_cmpeq(d, $f64x::splat(SLEEF_INFINITY))); }
#[inline]
fn visnan_vo_vd(d: $f64x) -> $ox { return ($ox)(vnot_vo_vo(vec_cmpeq(d, d))); }

#[inline]
fn vsel_vf_vo_f_f(o: $ox, v1: f32, v0: f32) -> CONST -> $f32x {
  o.select($f32x::splat(v1), $f32x::splat(v0));
}

#[inline]
fn vsel_vf_vo_vo_f_f_f(o0: $ox, o1: $ox, d0: f32, d1: f32, d2: f32) -> $f32x {
  o0.select($f32x::splat(d0), vsel_vf_vo_f_f(o1, d1, d2));
}

#[inline]
fn vsel_vf_vo_vo_vo_f_f_f_f(o0: $ox, o1: $ox, o2: $ox, d0: f32, d1: f32, d2: f32, d3: f32) -> $f32x {
  o0.select($f32x::splat(d0), o1.select($f32x::splat(d1), vsel_vf_vo_f_f(o2, d2, d3)));
}


#[inline]
fn vandnot_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_andc(y, x); }

#[inline]
fn vand_vi2_vo_vi2(x: $ox, y: $ix2) -> $ix2 { return ($ix2)vec_and(($ix2)x, y); }
#[inline]
fn vandnot_vi2_vo_vi2(x: $ox, y: $ix2) -> $ix2 { return vec_andc(y, ($ix2)x); }

#[inline]
fn vsrl_vi2_vi2_i(x: $ix2, c: int) -> $ix2 { return vec_sr (x, (vector unsigned int)(c, c, c, c)); }

#[inline]
fn veq_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_cmpeq(x, y); }
#[inline]
fn vgt_vi2_vi2_vi2($ix2 x, $ix2 y) -> $ix2 { return vec_cmpgt(x, y); }

#[inline]
fn visinf_vo_vf(d: $f32x) -> $ox { return ($ox)vec_cmpeq(d.abs(), $f32x::splat(SLEEF_INFINITY_F)); }
#[inline]
fn vispinf_vo_vf(d: $f32x) -> $ox { return ($ox)vec_cmpeq(d, $f32x::splat(SLEEF_INFINITY_F)); }
#[inline]
fn visminf_vo_vf(d: $f32x) -> $ox { return ($ox)vec_cmpeq(d, $f32x::splat(-SLEEF_INFINITY_F); }
#[inline]
fn visnan_vo_vf(d: $f32x) -> $ox { return ($ox)vnot_vo_vo(vec_cmpeq(d, d)); }
