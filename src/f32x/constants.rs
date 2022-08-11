use super::{F32x, LaneCount, SupportedLaneCount};

impl<const N: usize> crate::common::Constants for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    crate::common::cons! {
        PI: F32x = core::f32::consts::PI;
        FRAC_1_PI: F32x =  core::f32::consts::FRAC_1_PI;
        FRAC_2_PI: F32x =  core::f32::consts::FRAC_2_PI;
        FRAC_PI_2: F32x =  core::f32::consts::FRAC_PI_2;
        FRAC_PI_4: F32x =  core::f32::consts::FRAC_PI_4;
        NAN: F32x =        f32::NAN;
        INFINITY: F32x =   f32::INFINITY;
        NEG_INFINITY: F32x = f32::NEG_INFINITY;
        ZERO: F32x =       0.;
        NEG_ZERO: F32x =  -0.;
        ONE: F32x =        1.;
        HALF: F32x =       0.5;
        L10_U: F32x =     crate::f32::L10U_F;
        L10_L: F32x =     crate::f32::L10L_F;
        L2_U: F32x =      crate::f32::L2U_F;
        L2_L: F32x =      crate::f32::L2L_F;
        R_LN2: F32x =    crate::f32::R_LN2_F;
        LOG10_2: F32x =  crate::f32::LOG10_2_F;
    }
}

pub trait F32Constants {
    const F1_32: Self;
    const F1_30: Self;
    const F1_25: Self;
    const F1_24: Self;
    const F1_23: Self;
    const F1_12: Self;
    const PI_A: Self;
    const PI_B: Self;
    const PI_C: Self;
    const PI_D: Self;
    const PI_A2: Self;
    const PI_B2: Self;
    const PI_C2: Self;
    const TRIGRANGEMAX: Self;
    const TRIGRANGEMAX2: Self;
    const TRIGRANGEMAX4: Self;
    const SQRT_FLT_MAX: Self;
}

impl<const N: usize> F32Constants for F32x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    crate::common::cons! {
        F1_32: F32x = crate::f32::F1_32;
        F1_30: F32x = crate::f32::F1_30;
        F1_25: F32x = crate::f32::F1_25;
        F1_24: F32x = crate::f32::F1_24;
        F1_23: F32x = crate::f32::F1_23;
        F1_12: F32x = crate::f32::F1_12;
        PI_A: F32x = crate::f32::PI_A_F;
        PI_B: F32x = crate::f32::PI_B_F;
        PI_C: F32x = crate::f32::PI_C_F;
        PI_D: F32x = crate::f32::PI_D_F;
        PI_A2: F32x = crate::f32::PI_A2_F;
        PI_B2: F32x = crate::f32::PI_B2_F;
        PI_C2: F32x = crate::f32::PI_C2_F;
        TRIGRANGEMAX: F32x = crate::f32::TRIGRANGEMAX_F;
        TRIGRANGEMAX2: F32x = crate::f32::TRIGRANGEMAX2_F;
        TRIGRANGEMAX4: F32x = crate::f32::TRIGRANGEMAX4_F;
        SQRT_FLT_MAX: F32x = crate::f32::SQRT_FLT_MAX;
    }
}
