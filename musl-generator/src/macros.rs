macro_rules! f32x {
    ($name:ident, $mod:ident, $ulpmod:ident, $size:literal, [$($fun:ident,)+]) => {{
        $(
            // check type signature
            let _: fn(Simd<[f32; $size]>) -> Simd<[f32; $size]> = sleef::$mod::$ulpmod::$fun;
            let mut $fun = File::create(concat!("bin/output/musl.", stringify!($mod), ".", stringify!($fun)))?;
        )+

        for x in shared::$name.iter() {
            $(
                for i in 0..$size {
                    let y = unsafe {
                        extern "C" {
                            fn $fun(_: f32) -> f32;
                        }

                        $fun(x.extract(i))
                    };

                    $fun.write_all(&y.to_bits().to_ne_bytes())?;
                }
            )+
        }
    }};
}

macro_rules! f32f32x {
    ($name:ident, $mod:ident, $ulpmod:ident, $size:literal, [$($fun:ident,)+]) => {{
        $(
            // check type signature
            let _: fn(Simd<[f32; $size]>, Simd<[f32; $size]>) -> Simd<[f32; $size]> = sleef::$mod::$ulpmod::$fun;
            let mut $fun = File::create(concat!("bin/output/musl.", stringify!($mod), ".", stringify!($fun)))?;
        )+

        for (x0, x1) in shared::$name.iter() {
            $(
                for i in 0..$size {
                    let y = unsafe {
                        extern "C" {
                            fn $fun(_: f32, _: f32) -> f32;
                        }

                        $fun(x0.extract(i), x1.extract(i))
                    };

                    $fun.write_all(&y.to_bits().to_ne_bytes())?;
                }
            )+
        }
    }};
}

macro_rules! f64x {
    ($name:ident, $mod:ident, $ulpmod:ident, $size:literal, [$($fun:ident,)+]) => {{
        $(
            // check type signature
            let _: fn(Simd<[f64; $size]>) -> Simd<[f64; $size]> = sleef::$mod::$ulpmod::$fun;
            let mut $fun = File::create(concat!("bin/output/musl.", stringify!($mod), ".", stringify!($fun)))?;
        )+

        for x in shared::$name.iter() {
            $(
                for i in 0..$size {
                    let y = unsafe {
                        extern "C" {
                            fn $fun(_: f64) -> f64;
                        }

                        $fun(x.extract(i))
                    };

                    $fun.write_all(&y.to_bits().to_ne_bytes())?;
                }
            )+
        }
    }};
}

macro_rules! f64f64x {
    ($name:ident, $mod:ident, $ulpmod:ident, $size:literal, [$($fun:ident,)+]) => {{
        $(
            // check type signature
            let _: fn(Simd<[f64; $size]>, Simd<[f64; $size]>) -> Simd<[f64; $size]> = sleef::$mod::$ulpmod::$fun;
            let mut $fun = File::create(concat!("bin/output/musl.", stringify!($mod), ".", stringify!($fun)))?;
        )+

        for (x0, x1) in shared::$name.iter() {
            $(
                for i in 0..$size {
                    let y = unsafe {
                        extern "C" {
                            fn $fun(_: f64, _: f64) -> f64;
                        }

                        $fun(x0.extract(i), x1.extract(i))
                    };

                    $fun.write_all(&y.to_bits().to_ne_bytes())?;
                }
            )+
        }
    }};
}

macro_rules! f32 {
    ($ulpmod:ident, [$($fun:ident,)+]) => {{
        $(
            // check type signature
            let _: fn(f32) -> f32 = sleef::f32::$ulpmod::$fun;
            let mut $fun = File::create(concat!("bin/output/musl.", stringify!($fun)))?;
        )+

        for x in shared::F32.iter() {
            $(
                let y = unsafe {
                    extern "C" {
                        fn $fun(_: f32) -> f32;
                    }

                    $fun(*x)
                };

                $fun.write_all(&y.to_bits().to_ne_bytes())?;
            )+
        }
    }};
}

macro_rules! f32f32 {
    ($ulpmod:ident, [$($fun:ident,)+]) => {{
        $(
            // check type signature
            let _: fn(f32, f32) -> f32 = sleef::f32::$ulpmod::$fun;
            let mut $fun = File::create(concat!("bin/output/musl.", stringify!($fun)))?;
        )+

        for (x0, x1) in shared::F32F32.iter() {
            $(
                let y = unsafe {
                    extern "C" {
                        fn $fun(_: f32, _: f32) -> f32;
                    }

                    $fun(*x0, *x1)
                };

                $fun.write_all(&y.to_bits().to_ne_bytes())?;
            )+
        }
    }};
}

macro_rules! f64 {
    ($ulpmod:ident, [$($fun:ident,)+]) => {{
        $(
            // check type signature
            let _: fn(f64) -> f64 = sleef::f64::$ulpmod::$fun;
            let mut $fun = File::create(concat!("bin/output/musl.", stringify!($fun)))?;
        )+

        for x in shared::F64.iter() {
            $(
                let y = unsafe {
                    extern "C" {
                        fn $fun(_: f64) -> f64;
                    }

                    $fun(*x)
                };

                $fun.write_all(&y.to_bits().to_ne_bytes())?;
            )+
        }
    }};
}

macro_rules! f64f64 {
    ($ulpmod:ident, [$($fun:ident,)+]) => {{
        $(
            // check type signature
            let _: fn(f64, f64) -> f64 = sleef::f64::$ulpmod::$fun;
            let mut $fun = File::create(concat!("bin/output/musl.", stringify!($fun)))?;
        )+

        for (x0, x1) in shared::F64F64.iter() {
            $(
                let y = unsafe {
                    extern "C" {
                        fn $fun(_: f64, _: f64) -> f64;
                    }

                    $fun(*x0, *x1)
                };

                $fun.write_all(&y.to_bits().to_ne_bytes())?;
            )+
        }
    }};
}
