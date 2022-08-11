use super::{F64x, LaneCount, SupportedLaneCount};

impl<const N: usize> crate::common::Constants for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    crate::common::cons! {
        PI: F64x = core::f64::consts::PI;
        FRAC_1_PI: F64x =  core::f64::consts::FRAC_1_PI;
        FRAC_2_PI: F64x =  core::f64::consts::FRAC_2_PI;
        FRAC_PI_2: F64x =  core::f64::consts::FRAC_PI_2;
        FRAC_PI_4: F64x =  core::f64::consts::FRAC_PI_4;
        NAN: F64x =        f64::NAN;
        INFINITY: F64x =   f64::INFINITY;
        NEG_INFINITY: F64x = f64::NEG_INFINITY;
        ZERO: F64x =       0.;
        NEG_ZERO: F64x =  -0.;
        ONE: F64x =        1.;
        HALF: F64x =       0.5;
        L10_U: F64x =     crate::f64::L10_U;
        L10_L: F64x =     crate::f64::L10_L;
        L2_U: F64x =      crate::f64::L2_U;
        L2_L: F64x =      crate::f64::L2_L;
        R_LN2: F64x =    crate::f64::R_LN2;
        LOG10_2: F64x =  crate::f64::LOG10_2;
    }
}

pub trait F64Constants {
    const D1_63: Self;
    const D1_60: Self;
    const D1_54: Self;
    const D1_53: Self;
    const D1_52: Self;
    const D1_32: Self;
    const D1_31: Self;
    const D1_28: Self;
    const D1_24: Self;
    const D1_23: Self;
    const PI_A: Self;
    const PI_B: Self;
    const PI_C: Self;
    const PI_D: Self;
    const PI_A2: Self;
    const PI_B2: Self;
    const TRIGRANGEMAX: Self;
    const TRIGRANGEMAX2: Self;
    const TRIGRANGEMAX3: Self;
    const SQRT_DBL_MAX: Self;
}

impl<const N: usize> F64Constants for F64x<N>
where
    LaneCount<N>: SupportedLaneCount,
{
    crate::common::cons! {
        D1_63: F64x = crate::f64::D1_63;
        D1_60: F64x = crate::f64::D1_60;
        D1_54: F64x = crate::f64::D1_54;
        D1_53: F64x = crate::f64::D1_53;
        D1_52: F64x = crate::f64::D1_52;
        D1_32: F64x = crate::f64::D1_32;
        D1_31: F64x = crate::f64::D1_31;
        D1_28: F64x = crate::f64::D1_28;
        D1_24: F64x = crate::f64::D1_24;
        D1_23: F64x = crate::f64::D1_23;
        PI_A: F64x = crate::f64::PI_A;
        PI_B: F64x = crate::f64::PI_B;
        PI_C: F64x = crate::f64::PI_C;
        PI_D: F64x = crate::f64::PI_D;
        PI_A2: F64x = crate::f64::PI_A2;
        PI_B2: F64x = crate::f64::PI_B2;
        TRIGRANGEMAX: F64x = crate::f64::TRIGRANGEMAX;
        TRIGRANGEMAX2: F64x = crate::f64::TRIGRANGEMAX2;
        TRIGRANGEMAX3: F64x = crate::f64::TRIGRANGEMAX3;
        SQRT_DBL_MAX: F64x = crate::f64::SQRT_DBL_MAX;
    }
}
