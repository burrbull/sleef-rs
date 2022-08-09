use super::{F32x, I32x, LaneCount, SupportedLaneCount};

macro_rules! splat {
    ($($fun:ident: $ty:ident = $val:expr;)*) => {
        $(
            #[inline]
            pub fn $fun<const N: usize>() -> $ty<N>
            where
                LaneCount<N>: SupportedLaneCount,
            {
                $ty::splat($val)
            }
        )*
    };
}

splat! {
    pi: F32x = core::f32::consts::PI;
    frac_1_pi: F32x =  core::f32::consts::FRAC_1_PI;
    frac_2_pi: F32x =  core::f32::consts::FRAC_2_PI;
    frac_pi_2: F32x =  core::f32::consts::FRAC_PI_2;
    frac_pi_4: F32x =  core::f32::consts::FRAC_PI_4;
    nan: F32x =        f32::NAN;
    infinity: F32x =   f32::INFINITY;
    neg_infinity: F32x = f32::NEG_INFINITY;
    zero: F32x =       0.;
    neg_zero: F32x =  -0.;
    one: F32x =        1.;
    half: F32x =       0.5;
    f1_32x: F32x =     crate::f32::F1_32;
    f1_30x: F32x =     crate::f32::F1_30;
    f1_25x: F32x =     crate::f32::F1_25;
    f1_24x: F32x =     crate::f32::F1_24;
    f1_23x: F32x =     crate::f32::F1_23;
    f1_12x: F32x =     crate::f32::F1_12;
    pi_a_f: F32x =     crate::f32::PI_A_F;
    pi_b_f: F32x =     crate::f32::PI_B_F;
    pi_c_f: F32x =     crate::f32::PI_C_F;
    pi_d_f: F32x =     crate::f32::PI_D_F;
    pi_a2_f: F32x =    crate::f32::PI_A2_F;
    pi_b2_f: F32x =    crate::f32::PI_B2_F;
    pi_c2_f: F32x =    crate::f32::PI_C2_F;
    trigrangemax_f: F32x = crate::f32::TRIGRANGEMAX_F;
    trigrangemax2_f: F32x = crate::f32::TRIGRANGEMAX2_F;
    trigrangemax4_f: F32x = crate::f32::TRIGRANGEMAX4_F;
    sleef_fp_ilogb0: I32x = crate::f32::SLEEF_FP_ILOGB0;
    sleef_fp_ilogbnan: I32x = crate::f32::SLEEF_FP_ILOGBNAN;
    sqrt_flt_max: F32x = crate::f32::SQRT_FLT_MAX;
    l10u_f: F32x =     crate::f32::L10U_F;
    l10l_f: F32x =     crate::f32::L10L_F;
    l2u_f: F32x =      crate::f32::L2U_F;
    l2l_f: F32x =      crate::f32::L2L_F;
    r_ln2_f: F32x =    crate::f32::R_LN2_F;
    log10_2_f: F32x =  crate::f32::LOG10_2_F;
}
