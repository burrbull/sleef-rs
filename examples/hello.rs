use packed_simd::f64x2;

fn main() {
    let input = f64x2::new(1.43, 0.57);
    let output = sleef::f64x2::sin_u10(input);
    println!("sin(α) = {:?}", output);
}
