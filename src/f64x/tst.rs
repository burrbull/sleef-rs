use super::*;

fn gen_input<const N: usize>(
    rng: &mut rand::rngs::ThreadRng,
    range: core::ops::RangeInclusive<f64>,
) -> F64x<N>
{
    let mut arr = [0.; N];
    for reference in arr.iter_mut() {
        *reference = crate::f64::gen_input(rng, range.clone());
    }
    arr.into()
}

pub fn test_f_f<const N: usize>(
    fun_fx: fn(F64x<N>) -> F64x<N>,
    fun_f: fn(rug::Float) -> rug::Float,
    range: core::ops::RangeInclusive<f64>,
    ulp_ex: f64,
)
{
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT_FAST {
        let in_fx = gen_input(&mut rng, range.clone());
        let out_fx = fun_fx(in_fx);
        for i in 0..N {
            let input = in_fx[i];
            let output = out_fx[i];
            let expected = fun_f(rug::Float::with_val(crate::f64::PRECF64, input));
            if expected.is_nan() && output.is_nan() {
                continue;
            }
            let ulp = crate::f64::count_ulp(output, &expected);
            assert!(
                ulp <= ulp_ex,
                "Iteration: {n}, Position: {i}, Input: {input:e}, Output: {output}, Expected: {expected}, ULP: {ulp} > {}",
                ulp_ex
            );
        }
    }
}

pub fn test_f_ff<const N: usize>(
    fun_fx: fn(F64x<N>) -> (F64x<N>, F64x<N>),
    fun_f: fn(rug::Float) -> (rug::Float, rug::Float),
    range: core::ops::RangeInclusive<f64>,
    ulp_ex: f64,
)
{
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT_FAST {
        let in_fx = gen_input(&mut rng, range.clone());
        let (out_fx1, out_fx2) = fun_fx(in_fx);
        for i in 0..N {
            let input = in_fx[i];
            let output1 = out_fx1[i];
            let output2 = out_fx2[i];
            let (expected1, expected2) = fun_f(rug::Float::with_val(crate::f64::PRECF64, input));
            if (expected1.is_nan() && output1.is_nan()) || (expected2.is_nan() && output2.is_nan())
            {
                continue;
            }
            let ulp1 = crate::f64::count_ulp(output1, &expected1);
            let ulp2 = crate::f64::count_ulp(output2, &expected2);
            assert!(
                ulp1 <= ulp_ex && ulp2 <= ulp_ex,
                    "Iteration: {n}, Position: {i}, Input: {input:e}, Output: ({output1}, {output2}), Expected: ({expected1}, {expected2}), ULP: ({ulp1}, {ulp2}) > {}",
                    ulp_ex
            );
        }
    }
}

pub fn test_ff_f<const N: usize>(
    fun_fx: fn(F64x<N>, F64x<N>) -> F64x<N>,
    fun_f: fn(rug::Float, &rug::Float) -> rug::Float,
    range1: core::ops::RangeInclusive<f64>,
    range2: core::ops::RangeInclusive<f64>,
    ulp_ex: f64,
)
{
    let mut rng = rand::thread_rng();
    for n in 0..crate::TEST_REPEAT_FAST {
        let in_fx1 = gen_input(&mut rng, range1.clone());
        let in_fx2 = gen_input(&mut rng, range2.clone());
        let out_fx = fun_fx(in_fx1, in_fx2);
        for i in 0..N {
            let input1 = in_fx1[i];
            let input2 = in_fx2[i];
            let output = out_fx[i];
            let expected = fun_f(
                rug::Float::with_val(crate::f64::PRECF64, input1),
                &rug::Float::with_val(crate::f64::PRECF64, input2),
            );
            if expected.is_nan() && output.is_nan() {
                continue;
            }
            let ulp = crate::f64::count_ulp(output, &expected);
            assert!(
                ulp <= ulp_ex,
                "Iteration: {n}, Position: {i}, Input: ({input1:e}, {input2:e}), Output: {output}, Expected: {expected}, ULP: {ulp} > {}",
                ulp_ex
            );
        }
    }
}
