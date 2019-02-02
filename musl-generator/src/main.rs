extern crate sleef;
extern crate shared;
extern crate packed_simd;

use std::error::Error;
use std::fs::File;
use std::io::Write;

#[macro_use]
mod macros;

use packed_simd::Simd;

fn main() -> Result<(), Box<Error>> {
    f32! { u10, [
        acosf,
        asinf,
        atanf,
        cbrtf,
    //    ceilf,
        cosf,
        coshf,
        exp2f,
        expf,
        expm1f,
    //    fabsf,
    //    floorf,
        log10f,
        log1pf,
        log2f,
        logf,
    //    roundf,
        sinf,
        sinhf,
        tanf,
        tanhf,
    //    truncf,
    ]}
    f32! { u05, [
        sqrtf,
    ]}
    
    f32x! {
        F32X2, f32x2, u10, 2, [
            acosf,
            asinf,
            atanf,
            cbrtf,
        //    ceilf,
            cosf,
            coshf,
        //    exp2f,
            expf,
            expm1f,
        //    fabsf,
        //    floorf,
            log10f,
        //    log1pf,
            log2f,
            logf,
        //    roundf,
            sinf,
            sinhf,
            tanf,
            tanhf,
        //    truncf,
        ]
    }
    f32x! {
        F32X2, f32x2, u05, 2, [
            sqrtf,
        ]
    }

    f32f32! { u10, [
        atan2f,
    //    fdimf,
    //    fmodf,
        powf,
    ]}
    f32f32! { u05, [
        hypotf,
    ]}

    f32f32x! {
        F32F32X2, f32x2, u10, 2, [
            atan2f,
        //    fdimf,
        //    fmodf,
            powf,
        ]
    }
    f32f32x! {
        F32F32X2, f32x2, u05, 2, [
            hypotf,
        ]
    }

    f64! { u10, [
        acos,
        asin,
        atan,
        cbrt,
    //    ceil,
        cos,
        cosh,
        exp,
        exp2,
        expm1,
    //    fabs,
    //    floor,
        log,
    //    log10,
        log1p,
        log2,
    //    round,
        sin,
        sinh,
        tan,
    //    tanh,
    //    trunc,
    ]}
    f64! { u05, [
        sqrt,
    ]}

    f64x! {
        F64X2, f64x2, u10, 2, [
            acos,
            asin,
            atan,
            cbrt,
        //    ceil,
            cos,
            cosh,
            exp,
            exp2,
            expm1,
        //    fabs,
        //   floor,
            log,
            log10,
            log1p,
            log2,
        //    round,
            sin,
            sinh,
            tan,
            tanh,
        //    trunc,
        ]
    }
    f64x! {
        F64X2, f64x2, u05, 2, [
            sqrt,
        ]
    }

    f64f64! { u10, [
        atan2,
        //fdim,
        //fmod,
        pow,
    ]}
    f64f64! { u05, [
        hypot,
    ]}

    f64f64x! {
        F64F64X2, f64x2, u10, 2, [
            atan2,
        //    fdim,
         //   fmod,
            pow,
        ]
    }
    f64f64x! {
        F64F64X2, f64x2, u05, 2, [
            hypot,
        ]
    }

    Ok(())
}
