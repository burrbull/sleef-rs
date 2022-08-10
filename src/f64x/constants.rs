use super::{F64x, LaneCount, SupportedLaneCount};

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
    pi: F64x = core::f64::consts::PI;
    frac_1_pi: F64x =  core::f64::consts::FRAC_1_PI;
    frac_2_pi: F64x =  core::f64::consts::FRAC_2_PI;
    frac_pi_2: F64x =  core::f64::consts::FRAC_PI_2;
    frac_pi_4: F64x =  core::f64::consts::FRAC_PI_4;
    nan: F64x =        f64::NAN;
    infinity: F64x =   f64::INFINITY;
    neg_infinity: F64x = f64::NEG_INFINITY;
    zero: F64x =       0.;
    neg_zero: F64x =  -0.;
    one: F64x =        1.;
    half: F64x =       0.5;
    d1_63x: F64x = crate::f64::D1_63;
    d1_60x: F64x = crate::f64::D1_60;
    d1_54x: F64x = crate::f64::D1_54;
    d1_53x: F64x = crate::f64::D1_53;
    d1_52x: F64x = crate::f64::D1_52;
    d1_32x: F64x = crate::f64::D1_32;
    d1_31x: F64x = crate::f64::D1_31;
    d1_28x: F64x = crate::f64::D1_28;
    d1_24x: F64x = crate::f64::D1_24;
    d1_23x: F64x = crate::f64::D1_23;
    pi_a: F64x =     crate::f64::PI_A;
    pi_b: F64x =     crate::f64::PI_B;
    pi_c: F64x =     crate::f64::PI_C;
    pi_d: F64x =     crate::f64::PI_D;
    pi_a2: F64x =    crate::f64::PI_A2;
    pi_b2: F64x =    crate::f64::PI_B2;
    trigrangemax: F64x = crate::f64::TRIGRANGEMAX;
    trigrangemax2: F64x = crate::f64::TRIGRANGEMAX2;
    trigrangemax3: F64x = crate::f64::TRIGRANGEMAX3;
    sleef_fp_ilogb0: F64x = crate::f64::SLEEF_FP_ILOGB0 as f64;
    sleef_fp_ilogbnan: F64x = crate::f64::SLEEF_FP_ILOGBNAN as f64;
    sqrt_dbl_max: F64x = crate::f64::SQRT_DBL_MAX;
    l10_u: F64x =     crate::f64::L10_U;
    l10_l: F64x =     crate::f64::L10_L;
    l2_u: F64x =      crate::f64::L2_U;
    l2_l: F64x =      crate::f64::L2_L;
    r_ln2: F64x =    crate::f64::R_LN2;
    log10_2: F64x =  crate::f64::LOG10_2;
}
