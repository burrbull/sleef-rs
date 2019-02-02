extern crate libm;
extern crate shared;

use std::error::Error;
use std::fs::File;
use std::io::Write;

#[macro_use]
mod macros;

fn main() -> Result<(), Box<Error>> {
    f32! {
        acosf,
        asinf,
        atanf,
        cbrtf,
        ceilf,
        cosf,
        coshf,
        exp2f,
        expf,
        expm1f,
        fabsf,
        floorf,
        log10f,
        log1pf,
        log2f,
        logf,
        roundf,
        sinf,
        sinhf,
        sqrtf,
        tanf,
        tanhf,
        truncf,
    }
    f32x! {
        F32X2, f32x2, 2, acosf,
        F32X2, f32x2, 2, asinf,
        F32X2, f32x2, 2, atanf,
        F32X2, f32x2, 2, cbrtf,
        F32X2, f32x2, 2, ceilf,
        F32X2, f32x2, 2, cosf,
        F32X2, f32x2, 2, coshf,
        F32X2, f32x2, 2, exp2f,
        F32X2, f32x2, 2, expf,
        F32X2, f32x2, 2, expm1f,
        F32X2, f32x2, 2, fabsf,
        F32X2, f32x2, 2, floorf,
        F32X2, f32x2, 2, log10f,
        F32X2, f32x2, 2, log1pf,
        F32X2, f32x2, 2, log2f,
        F32X2, f32x2, 2, logf,
        F32X2, f32x2, 2, roundf,
        F32X2, f32x2, 2, sinf,
        F32X2, f32x2, 2, sinhf,
        F32X2, f32x2, 2, sqrtf,
        F32X2, f32x2, 2, tanf,
        F32X2, f32x2, 2, tanhf,
        F32X2, f32x2, 2, truncf,
    }

    f32f32! {
        atan2f,
        fdimf,
        fmodf,
        hypotf,
        powf,
    }

    f32f32x! {
        F32F32X2, f32x2, 2, atan2f,
        F32F32X2, f32x2, 2, fdimf,
        F32F32X2, f32x2, 2, fmodf,
        F32F32X2, f32x2, 2, hypotf,
        F32F32X2, f32x2, 2, powf,
    }

    f32i32! {
        scalbnf,
    }

    f32i32x! {
        F32I32X2, f32x2, 2, scalbnf,
    }

    f32f32f32! {
        fmaf,
    }

    f32f32f32x! {
        F32F32X2, f32x2, 2, fmaf,
    }

    f64! {
        acos,
        asin,
        atan,
        cbrt,
        ceil,
        cos,
        cosh,
        exp,
        exp2,
        expm1,
        fabs,
        floor,
        log,
        log10,
        log1p,
        log2,
        round,
        sin,
        sinh,
        sqrt,
        tan,
        tanh,
        trunc,
    }

    f64x! {
        F64X2, f64x2, 2, acos,
        F64X2, f64x2, 2, asin,
        F64X2, f64x2, 2, atan,
        F64X2, f64x2, 2, cbrt,
        F64X2, f64x2, 2, ceil,
        F64X2, f64x2, 2, cos,
        F64X2, f64x2, 2, cosh,
        F64X2, f64x2, 2, exp,
        F64X2, f64x2, 2, exp2,
        F64X2, f64x2, 2, expm1,
        F64X2, f64x2, 2, fabs,
        F64X2, f64x2, 2, floor,
        F64X2, f64x2, 2, log,
        F64X2, f64x2, 2, log10,
        F64X2, f64x2, 2, log1p,
        F64X2, f64x2, 2, log2,
        F64X2, f64x2, 2, round,
        F64X2, f64x2, 2, sin,
        F64X2, f64x2, 2, sinh,
        F64X2, f64x2, 2, sqrt,
        F64X2, f64x2, 2, tan,
        F64X2, f64x2, 2, tanh,
        F64X2, f64x2, 2, trunc,
    }

    f64f64! {
        atan2,
        fdim,
        fmod,
        hypot,
        pow,
    }

    f64f64x! {
        F64F64X2, f64x2, 2, atan2,
        F64F64X2, f64x2, 2, fdim,
        F64F64X2, f64x2, 2, fmod,
        F64F64X2, f64x2, 2, hypot,
        F64F64X2, f64x2, 2, pow,
    }

    f64i32! {
        scalbn,
    }

    f64i32x! {
        F64I32X2, f64x2, 2, scalbn,
    }

    f64f64f64! {
        fma,
    }

    f64f64f64x! {
        F64F64F64X2, f64x2, 2, fma,
    }

    Ok(())
}
