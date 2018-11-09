//! Functions with 1.5 ULP error bound

use super::*;

/// Complementary error function
///
/// The error bound is max(1.5 ULP, f32::MIN).
pub fn erfcf(mut a: f32) -> f32 {
    let s = a;
    a = fabsfk(a);
    let o0 = a < 1.;
    let o1 = a < 2.2;
    let o2 = a < 4.3;
    let o3 = a < 10.1;
    let u = if o1 {
        df(a, 0.)
    } else {
        df(1., 0.) / df(a, 0.)
    };

    let t = if o0 {
        -0.8638041618e-4_f32
    } else if o1 {
        -0.6236977242e-5
    } else if o2 {
        -0.3869504035e+0
    } else {
        0.1115344167e+1
    }.mul_add(
        u.0,
        if o0 {
            0.6000166177e-3
        } else if o1 {
            0.5749821503e-4
        } else if o2 {
            0.1288077235e+1
        } else {
            -0.9454904199e+0
        },
    ).mul_add(
        u.0,
        if o0 {
            -0.1665703603e-2
        } else if o1 {
            0.6002851478e-5
        } else if o2 {
            -0.1816803217e+1
        } else {
            -0.3667259514e+0
        },
    ).mul_add(
        u.0,
        if o0 {
            0.1795156277e-3
        } else if o1 {
            -0.2851036377e-2
        } else if o2 {
            0.1249150872e+1
        } else {
            0.7155663371e+0
        },
    ).mul_add(
        u.0,
        if o0 {
            0.1914106123e-1
        } else if o1 {
            0.2260518074e-1
        } else if o2 {
            -0.1328857988e+0
        } else {
            -0.1262947265e-1
        },
    );

    let mut d = u * t;
    d += if o0 {
        df(-0.102775359343930288081655368891e+0, 0.)
    } else if o1 {
        df(-0.105247583459338632253369014063e+0, 0.)
    } else if o2 {
        df(-0.482365310333045318680618892669e+0, 0.)
    } else {
        df(-0.498961546254537647970305302739e+0, 0.)
    };
    d *= u;
    d += if o0 {
        df(-0.636619483208481931303752546439e+0, 0.)
    } else if o1 {
        df(-0.635609463574589034216723775292e+0, 0.)
    } else if o2 {
        df(-0.134450203224533979217859332703e-2, 0.)
    } else {
        df(-0.471199543422848492080722832666e-4, 0.)
    };
    d *= u;
    d += if o0 {
        df(-0.112837917790537404939545770596e+1, 0.)
    } else if o1 {
        df(-0.112855987376668622084547028949e+1, 0.)
    } else if o2 {
        df(-0.572319781150472949561786101080e+0, 0.)
    } else {
        df(-0.572364030327966044425932623525e+0, 0.)
    };

    let mut x = (if o1 { d } else { df(-a, 0.) }) * a;
    x = if o1 { x } else { x + d };

    x = expk2f(x);
    x = if o1 { x } else { x * u };

    let mut r = if o3 { x.0 + x.1 } else { 0. };
    if s < 0. {
        r = 2. - r;
    }
    if s.isnan() {
        SLEEF_NAN_F
    } else {
        r
    }
}
