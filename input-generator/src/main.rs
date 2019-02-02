extern crate rand;

use std::collections::BTreeSet;
use std::error::Error;
use std::fs::{self, File};
use std::io::Write;

use rand::{RngCore, SeedableRng, XorShiftRng};

const NTESTS: usize = 10_000;

fn main() -> Result<(), Box<Error>> {
    let mut rng = XorShiftRng::from_rng(&mut rand::thread_rng())?;

    fs::remove_dir_all("bin").ok();
    fs::create_dir_all("bin/input")?;
    fs::create_dir_all("bin/output")?;

    f32(&mut rng)?;
    f32x(&mut rng, 2)?;
    f32x(&mut rng, 4)?;
    f32x(&mut rng, 8)?;
    f32x(&mut rng, 16)?;
    f32f32(&mut rng)?;
    f32f32x(&mut rng, 2)?;
    f32f32x(&mut rng, 4)?;
    f32f32x(&mut rng, 8)?;
    f32f32x(&mut rng, 16)?;
    f64(&mut rng)?;
    f64x(&mut rng, 2)?;
    f64x(&mut rng, 4)?;
    f64x(&mut rng, 8)?;
    f64f64(&mut rng)?;
    f64f64x(&mut rng, 2)?;
    f64f64x(&mut rng, 4)?;
    f64f64x(&mut rng, 8)?;

    Ok(())
}

fn f32(rng: &mut XorShiftRng) -> Result<(), Box<Error>> {
    let mut set = BTreeSet::new();

    while set.len() < NTESTS {
        let f = f32::from_bits(rng.next_u32());

        if f.is_nan() {
            continue;
        }

        set.insert(f.to_bits());
    }

    let mut f = File::create("bin/input/f32")?;
    for i in set {
        f.write_all(&i.to_ne_bytes())?;
    }

    Ok(())
}

fn f32x(rng: &mut XorShiftRng, size: usize) -> Result<(), Box<Error>> {
    let mut set = BTreeSet::new();

    while set.len() < NTESTS*size {
        let f = f32::from_bits(rng.next_u32());

        if f.is_nan() {
            continue;
        }

        set.insert(f.to_bits());
    }

    let mut f = File::create("bin/input/f32x".to_owned()+&size.to_string())?;
    for i in set {
        f.write_all(&i.to_ne_bytes())?;
    }

    Ok(())
}

fn f32f32(rng: &mut XorShiftRng) -> Result<(), Box<Error>> {
    let mut f = File::create("bin/input/f32f32")?;
    let mut i = 0;
    while i < NTESTS {
        let x0 = f32::from_bits(rng.next_u32());
        let x1 = f32::from_bits(rng.next_u32());

        if x0.is_nan() || x1.is_nan() {
            continue;
        }

        i += 1;
        f.write_all(&x0.to_bits().to_ne_bytes())?;
        f.write_all(&x1.to_bits().to_ne_bytes())?;
    }

    Ok(())
}

fn f32f32x(rng: &mut XorShiftRng, size: usize) -> Result<(), Box<Error>> {
    let mut f = File::create("bin/input/f32f32x".to_owned()+&size.to_string())?;
    for _ in 0..NTESTS {
        let mut i = 0;
        while i < size {
            let x0 = f32::from_bits(rng.next_u32());

            if x0.is_nan() {
                continue;
            }
            i += 1;
            f.write_all(&x0.to_bits().to_ne_bytes())?;
        }
        
        let mut i = 0;
        while i < size {
            let x1 = f32::from_bits(rng.next_u32());
            if x1.is_nan() {
                continue;
            }
            i += 1;
            f.write_all(&x1.to_bits().to_ne_bytes())?;
        }
    }

    Ok(())
}

fn f64(rng: &mut XorShiftRng) -> Result<(), Box<Error>> {
    let mut set = BTreeSet::new();

    while set.len() < NTESTS {
        let f = f64::from_bits(rng.next_u64());

        if f.is_nan() {
            continue;
        }

        set.insert(f.to_bits());
    }

    let mut f = File::create("bin/input/f64")?;
    for i in set {
        f.write_all(&i.to_ne_bytes())?;
    }

    Ok(())
}

fn f64x(rng: &mut XorShiftRng, size: usize) -> Result<(), Box<Error>> {
    let mut set = BTreeSet::new();

    while set.len() < NTESTS*size {
        let f = f64::from_bits(rng.next_u64());

        if f.is_nan() {
            continue;
        }

        set.insert(f.to_bits());
    }

    let mut f = File::create("bin/input/f64x".to_owned()+&size.to_string())?;
    for i in set {
        f.write_all(&i.to_ne_bytes())?;
    }

    Ok(())
}

fn f64f64(rng: &mut XorShiftRng) -> Result<(), Box<Error>> {
    let mut f = File::create("bin/input/f64f64")?;
    let mut i = 0;
    while i < NTESTS {
        let x0 = f64::from_bits(rng.next_u64());
        let x1 = f64::from_bits(rng.next_u64());

        if x0.is_nan() || x1.is_nan() {
            continue;
        }

        i += 1;
        f.write_all(&x0.to_bits().to_ne_bytes())?;
        f.write_all(&x1.to_bits().to_ne_bytes())?;
    }

    Ok(())
}

fn f64f64x(rng: &mut XorShiftRng, size: usize) -> Result<(), Box<Error>> {
    let mut f = File::create("bin/input/f64f64x".to_owned()+&size.to_string())?;
    for _ in 0..NTESTS {
        let mut i = 0;
        while i < size {
            let x0 = f64::from_bits(rng.next_u64());

            if x0.is_nan() {
                continue;
            }
            i += 1;
            f.write_all(&x0.to_bits().to_ne_bytes())?;
        }
        
        let mut i = 0;
        while i < size {
            let x1 = f64::from_bits(rng.next_u64());
            if x1.is_nan() {
                continue;
            }
            i += 1;
            f.write_all(&x1.to_bits().to_ne_bytes())?;
        }
    }

    Ok(())
}
