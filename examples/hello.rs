#![feature(portable_simd)]

use sleef::f64x2::f64x2;

fn main() {
    let input = f64x2::from_array([1.43, 0.57]);
    let output = sleef::f64x2::sin_u10(input);
    println!("sin(α) = {:?}", output);
}
