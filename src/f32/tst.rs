use super::*;

use rug::{Assign, Float};
pub const PRECF32: u32 = 80;

pub fn count_ulp(d: f32, c: &Float) -> f32 {
    let c2 = c.to_f32();

    if (c2 == 0. || c2.is_subnormal()) && (d == 0. || d.is_subnormal()) {
        return 0.;
    }

    if (c2 == 0.) && (d != 0.) {
        return 10000.;
    }

    if c2.is_infinite() && d.is_infinite() {
        return 0.;
    }

    let prec = c.prec();

    let mut fry = Float::with_val(prec, d);

    let mut frw = Float::new(prec);

    let (_, e) = c.to_f32_exp();

    frw.assign(Float::u_exp(1, e - 24_i32));

    fry -= c;
    fry /= &frw;
    fabsf(fry.to_f32())
}

pub fn gen_input(rng: &mut rand::rngs::ThreadRng, range: core::ops::RangeInclusive<f32>) -> f32 {
    use rand::Rng;
    let mut start = *range.start();
    if start == f32::MIN {
        start = -1e37;
    }
    let mut end = *range.end();
    if end == f32::MAX {
        end = 1e37;
    }
    rng.gen_range(start..=end)
}

pub fn test_f_f(
    f_tested: fn(f32) -> f32,
    f_sample: fn(Float) -> Float,
    range: core::ops::RangeInclusive<f32>,
    ulp_ex: f32,
) {
    test_c_f_f(f_tested, f_sample, range, |ulp, _, _| {
        (ulp <= ulp_ex, format!("ULP: {ulp} > {ulp_ex}"))
    })
}

pub fn test_c_f_f(
    f_tested: fn(f32) -> f32,
    f_sample: fn(Float) -> Float,
    range: core::ops::RangeInclusive<f32>,
    cf: impl Fn(f32, f32, &Float) -> (bool, String),
) {
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input = gen_input(&mut rng, range.clone());
        let output = f_tested(input);
        let expected = f_sample(Float::with_val(PRECF32, input));
        if expected.is_nan() && output.is_nan() {
            continue;
        }
        let ulp = count_ulp(output, &expected);
        let (b, fault_string) = cf(ulp, output, &expected);
        assert!(
            b,
            "Iteration: {n}, Input: {input:e}, Output: {output}, Expected: {expected}, {}",
            fault_string
        );
    }
}

pub fn test_f_ff(
    fun_fx: fn(f32) -> (f32, f32),
    fun_f: fn(Float) -> (Float, Float),
    range: core::ops::RangeInclusive<f32>,
    ulp_ex: f32,
) {
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input = gen_input(&mut rng, range.clone());
        let (output1, output2) = fun_fx(input);
        let (expected1, expected2) = fun_f(Float::with_val(PRECF32, input));
        if (expected1.is_nan() && output1.is_nan()) || (expected2.is_nan() && output2.is_nan()) {
            continue;
        }
        let ulp1 = count_ulp(output1, &expected1);
        let ulp2 = count_ulp(output2, &expected2);
        assert!(
            ulp1 <= ulp_ex && ulp2 <= ulp_ex,
                "Iteration: {n}, Input: {input:e}, Output: ({output1}, {output2}), Expected: ({expected1}, {expected2}), ULP: ({ulp1}, {ulp2}) > {}",
                ulp_ex
        );
    }
}

pub fn test_ff_f(
    fun_fx: fn(f32, f32) -> f32,
    fun_f: fn(Float, &Float) -> Float,
    range1: core::ops::RangeInclusive<f32>,
    range2: core::ops::RangeInclusive<f32>,
    ulp_ex: f32,
) {
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT {
        let input1 = gen_input(&mut rng, range1.clone());
        let input2 = gen_input(&mut rng, range2.clone());
        let output = fun_fx(input1, input2);
        let expected = fun_f(
            Float::with_val(PRECF32, input1),
            &Float::with_val(PRECF32, input2),
        );
        if expected.is_nan() && output.is_nan() {
            continue;
        }
        let ulp = count_ulp(output, &expected);
        assert!(
            ulp <= ulp_ex,
            "Iteration: {n}, Input: ({input1:e}, {input2:e}), Output: {output}, Expected: {expected}, ULP: {ulp} > {}",
            ulp_ex
        );
    }
}
