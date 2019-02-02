#[macro_use]
extern crate lazy_static;
extern crate packed_simd;

use packed_simd::Simd;

macro_rules! f32x_vec {
    (
        $size:expr,
        $namef32x:ident,
        $namef32f32x:ident,
    ) => {
        lazy_static! {
            pub static ref $namef32x: Vec<Simd<[f32; $size]>> = {
                let bytes = include_bytes!(concat!("../../bin/input/f32x", stringify!($size)));
                bytes
                    .chunks_exact(4*$size)
                    .map(|chunk| {
                        let vec: Vec<f32> = chunk.chunks_exact(4)
                            .map(|ch| {
                                let mut buf = [0; 4];
                                buf.copy_from_slice(ch);
                                f32::from_bits(u32::from_le_bytes(buf))
                            })
                            .collect();
                        Simd::<[f32; $size]>::from_slice_unaligned(&vec[..])
                    })
                    .collect()
            };
            pub static ref $namef32f32x: Vec<(Simd<[f32; $size]>,Simd<[f32; $size]>)> = {
                let bytes = include_bytes!(concat!("../../bin/input/f32f32x", stringify!($size)));
                bytes
                    .chunks_exact(4*$size*2)
                    .map(|chunk| {
                        let vec0: Vec<f32> = chunk[4*$size..].chunks_exact(4)
                            .map(|ch| {
                                let mut x0 = [0; 4];
                                x0.copy_from_slice(ch);
                                f32::from_bits(u32::from_le_bytes(x0))
                            })
                            .collect();
                        let vec1: Vec<f32> = chunk[..4*$size].chunks_exact(4)
                            .map(|ch| {
                                let mut x1 = [0; 4];
                                x1.copy_from_slice(ch);
                                f32::from_bits(u32::from_le_bytes(x1))
                            })
                            .collect();
                        (
                            Simd::<[f32; $size]>::from_slice_unaligned(&vec0[..]),
                            Simd::<[f32; $size]>::from_slice_unaligned(&vec1[..]),
                        )
                    })
                    .collect()
            };
        }
    };
}
f32x_vec!(
    2,
    F32X2,
    F32F32X2,
);
f32x_vec!(
    4,
    F32X4,
    F32F32X4,
);
f32x_vec!(
    8,
    F32X8,
    F32F32X8,
);
f32x_vec!(
    16,
    F32X16,
    F32F32X16,
);

macro_rules! f64x_vec {
    (
        $size:expr,
        $namef64x:ident,
        $namef64f64x:ident,
    ) => {
        lazy_static! {
            pub static ref $namef64x: Vec<Simd<[f64; $size]>> = {
                let bytes = include_bytes!(concat!("../../bin/input/f64x", stringify!($size)));

                bytes
                    .chunks_exact(8*$size)
                    .map(|chunk| {
                        let vec: Vec<f64> = chunk.chunks_exact(8)
                            .map(|ch| {
                                let mut buf = [0; 8];
                                buf.copy_from_slice(ch);
                                f64::from_bits(u64::from_le(u64::from_ne_bytes(buf)))
                            })
                            .collect();
                        Simd::<[f64; $size]>::from_slice_unaligned(&vec[..])
                    })
                    .collect()
            };
            pub static ref $namef64f64x: Vec<(Simd<[f64; $size]>,Simd<[f64; $size]>)> = {
                let bytes = include_bytes!(concat!("../../bin/input/f64f64x", stringify!($size)));

                bytes
                    .chunks_exact(8*$size*2)
                    .map(|chunk| {
                        let vec0: Vec<f64> = chunk[8*$size..].chunks_exact(8)
                            .map(|ch| {
                                let mut x0 = [0; 8];
                                x0.copy_from_slice(ch);
                                f64::from_bits(u64::from_le(u64::from_ne_bytes(x0)))
                            })
                            .collect();
                        let vec1: Vec<f64> = chunk[..8*$size].chunks_exact(8)
                            .map(|ch| {
                                let mut x1 = [0; 8];
                                x1.copy_from_slice(ch);
                                f64::from_bits(u64::from_le(u64::from_ne_bytes(x1)))
                            })
                            .collect();
                        (
                            Simd::<[f64; $size]>::from_slice_unaligned(&vec0[..]),
                            Simd::<[f64; $size]>::from_slice_unaligned(&vec1[..]),
                        )
                    })
                    .collect()
            };
        }
    };
}

f64x_vec!(
    2,
    F64X2,
    F64F64X2,
);

f64x_vec!(
    4,
    F64X4,
    F64F64X4,
);

f64x_vec!(
    8,
    F64X8,
    F64F64X8,
);

lazy_static! {
    pub static ref F32: Vec<f32> = {
        let bytes = include_bytes!("../../bin/input/f32");

        bytes
            .chunks_exact(4)
            .map(|chunk| {
                let mut buf = [0; 4];
                buf.copy_from_slice(chunk);
                f32::from_bits(u32::from_le_bytes(buf))
            })
            .collect()
    };
    pub static ref F32F32: Vec<(f32, f32)> = {
        let bytes = include_bytes!("../../bin/input/f32f32");

        bytes
            .chunks_exact(8)
            .map(|chunk| {
                let mut x0 = [0; 4];
                let mut x1 = [0; 4];
                x0.copy_from_slice(&chunk[..4]);
                x1.copy_from_slice(&chunk[4..]);

                (
                    f32::from_bits(u32::from_le_bytes(x0)),
                    f32::from_bits(u32::from_le_bytes(x1)),
                )
            })
            .collect()
    };
    pub static ref F64: Vec<f64> = {
        let bytes = include_bytes!("../../bin/input/f64");

        bytes
            .chunks_exact(8)
            .map(|chunk| {
                let mut buf = [0; 8];
                buf.copy_from_slice(chunk);
                f64::from_bits(u64::from_le(u64::from_ne_bytes(buf)))
            })
            .collect()
    };
    pub static ref F64F64: Vec<(f64, f64)> = {
        let bytes = include_bytes!("../../bin/input/f64f64");

        bytes
            .chunks_exact(16)
            .map(|chunk| {
                let mut x0 = [0; 8];
                let mut x1 = [0; 8];
                x0.copy_from_slice(&chunk[..8]);
                x1.copy_from_slice(&chunk[8..]);

                (
                    f64::from_bits(u64::from_le(u64::from_ne_bytes(x0))),
                    f64::from_bits(u64::from_le(u64::from_ne_bytes(x1))),
                )
            })
            .collect()
    };
}

#[macro_export]
macro_rules! f32x {
    ($lib:expr, $mod:ident, $ulpmod:ident, $size:ident, $ulp:literal, $($fun:ident),+) => {
        $(
            #[test]
            fn $fun() {
                let expected = include_bytes!(concat!("../bin/output/", $lib, ".", stringify!($mod), ".", stringify!($fun)))
                    .chunks_exact(4*$size)
                    .map(|chunk| {
                        let vec: Vec<f32> = chunk.chunks_exact(4)
                            .map(|ch| {
                                let mut buf = [0; 4];
                                buf.copy_from_slice(ch);
                                f32::from_bits(u32::from_le_bytes(buf))
                            })
                            .collect();
                        Simd::<[f32; $size]>::from_slice_unaligned(&vec[..])
                    })
                    .collect::<Vec<_>>();

                for (input, expected) in $crate::F32.iter().zip(&expected) {
                    if let Ok(output) = panic::catch_unwind(|| sleef::$mod::$ulpmod::$fun(*input)) {
                        if let Err(error) = sleef::$mod::_eq(output, *expected, $ulp) {
                            panic!(
                                "INPUT: {:#x}, OUTPUT: {:#x}, EXPECTED: {:#x}, ERROR: {}",
                                input.to_bits(),
                                output.to_bits(),
                                expected.to_bits(),
                                error
                            );
                        }
                    } else {
                        panic!(
                            "INPUT: {:#x}, OUTPUT: PANIC!, EXPECTED: {:#x}",
                            input.to_bits(),
                            expected.to_bits()
                        );
                    }
                }
            }
        )+
    }
}

#[macro_export]
macro_rules! f32 {
    ($lib:expr, $($fun:ident),+) => {
        $(
            #[test]
            fn $fun() {
                let expected = include_bytes!(concat!("../bin/output/", $lib, ".", stringify!($fun)))
                    .chunks_exact(4)
                    .map(|chunk| {
                        let mut buf = [0; 4];
                        buf.copy_from_slice(chunk);
                        f32::from_bits(u32::from_le_bytes(buf))
                    })
                    .collect::<Vec<_>>();

                for (input, expected) in $crate::F32.iter().zip(&expected) {
                    if let Ok(output) = panic::catch_unwind(|| libm::$fun(*input)) {
                        if let Err(error) = libm::_eqf(output, *expected) {
                            panic!(
                                "INPUT: {:#x}, OUTPUT: {:#x}, EXPECTED: {:#x}, ERROR: {}",
                                input.to_bits(),
                                output.to_bits(),
                                expected.to_bits(),
                                error
                            );
                        }
                    } else {
                        panic!(
                            "INPUT: {:#x}, OUTPUT: PANIC!, EXPECTED: {:#x}",
                            input.to_bits(),
                            expected.to_bits()
                        );
                    }
                }
            }
        )+
    }
}

#[macro_export]
macro_rules! f32f32 {
    ($lib:expr, $($fun:ident),+) => {
        $(
            #[test]
            fn $fun() {
                let expected = include_bytes!(concat!("../bin/output/", $lib, ".", stringify!($fun)))
                    .chunks_exact(4)
                    .map(|chunk| {
                        let mut buf = [0; 4];
                        buf.copy_from_slice(chunk);
                        f32::from_bits(u32::from_le_bytes(buf))
                    })
                    .collect::<Vec<_>>();

                for ((i0, i1), expected) in $crate::F32F32.iter().zip(&expected) {
                    if let Ok(output) = panic::catch_unwind(|| libm::$fun(*i0, *i1)) {
                        if let Err(error) = libm::_eqf(output, *expected) {
                            panic!(
                                "INPUT: ({:#x}, {:#x}), OUTPUT: {:#x}, EXPECTED: {:#x}, ERROR: {}",
                                i0.to_bits(),
                                i1.to_bits(),
                                output.to_bits(),
                                expected.to_bits(),
                                error
                            );
                        }
                    } else {
                        panic!(
                            "INPUT: ({:#x}, {:#x}), OUTPUT: PANIC!, EXPECTED: {:#x}",
                            i0.to_bits(),
                            i1.to_bits(),
                            expected.to_bits()
                        );
                    }
                }
            }
        )+
    }
}

#[macro_export]
macro_rules! f64 {
    ($lib:expr, $($fun:ident),+) => {
        $(
            #[test]
            fn $fun() {
                let expected = include_bytes!(concat!("../bin/output/", $lib, ".", stringify!($fun)))
                    .chunks_exact(8)
                    .map(|chunk| {
                        let mut buf = [0; 8];
                        buf.copy_from_slice(chunk);
                        f64::from_bits(u64::from_le(u64::from_ne_bytes(buf)))
                    })
                    .collect::<Vec<_>>();

                for (input, expected) in shared::F64.iter().zip(&expected) {
                    if let Ok(output) = panic::catch_unwind(|| libm::$fun(*input)) {
                        if let Err(error) = libm::_eq(output, *expected) {
                            panic!(
                                "INPUT: {:#x}, OUTPUT: {:#x}, EXPECTED: {:#x}, ERROR: {}",
                                input.to_bits(),
                                output.to_bits(),
                                expected.to_bits(),
                                error
                            );
                        }
                    } else {
                        panic!(
                            "INPUT: {:#x}, OUTPUT: PANIC!, EXPECTED: {:#x}",
                            input.to_bits(),
                            expected.to_bits()
                        );
                    }
                }
            }
        )+
    }
}

#[macro_export]
macro_rules! f64f64 {
    ($lib:expr, $($fun:ident),+) => {
        $(
            #[test]
            fn $fun() {
                let expected = include_bytes!(concat!("../bin/output/", $lib, ".", stringify!($fun)))
                    .chunks_exact(8)
                    .map(|chunk| {
                        let mut buf = [0; 8];
                        buf.copy_from_slice(chunk);
                        f64::from_bits(u64::from_le(u64::from_ne_bytes(buf)))
                    })
                    .collect::<Vec<_>>();

                for ((i0, i1), expected) in shared::F64F64.iter().zip(&expected) {
                    if let Ok(output) = panic::catch_unwind(|| libm::$fun(*i0, *i1)) {
                        if let Err(error) = libm::_eq(output, *expected) {
                            panic!(
                                "INPUT: ({:#x}, {:#x}), OUTPUT: {:#x}, EXPECTED: {:#x}, ERROR: {}",
                                i0.to_bits(),
                                i1.to_bits(),
                                output.to_bits(),
                                expected.to_bits(),
                                error
                            );
                        }
                    } else {
                        panic!(
                            "INPUT: ({:#x}, {:#x}), OUTPUT: PANIC!, EXPECTED: {:#x}",
                            i0.to_bits(),
                            i1.to_bits(),
                            expected.to_bits()
                        );
                    }
                }
            }
        )+
    }
}
