//! Math functions for FORMA runtime

/// Absolute value of an integer
#[no_mangle]
pub extern "C" fn forma_abs_int(n: i64) -> i64 {
    n.abs()
}

/// Absolute value of a float
#[no_mangle]
pub extern "C" fn forma_abs_float(n: f64) -> f64 {
    n.abs()
}

/// Minimum of two integers
#[no_mangle]
pub extern "C" fn forma_min_int(a: i64, b: i64) -> i64 {
    std::cmp::min(a, b)
}

/// Maximum of two integers
#[no_mangle]
pub extern "C" fn forma_max_int(a: i64, b: i64) -> i64 {
    std::cmp::max(a, b)
}

/// Minimum of two floats
#[no_mangle]
pub extern "C" fn forma_min_float(a: f64, b: f64) -> f64 {
    a.min(b)
}

/// Maximum of two floats
#[no_mangle]
pub extern "C" fn forma_max_float(a: f64, b: f64) -> f64 {
    a.max(b)
}

/// Integer power (base^exp)
#[no_mangle]
pub extern "C" fn forma_pow_int(base: i64, exp: i64) -> i64 {
    if exp < 0 {
        return 0; // Integer power with negative exponent
    }
    base.pow(exp as u32)
}

/// Float power
#[no_mangle]
pub extern "C" fn forma_pow_float(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

/// Square root
#[no_mangle]
pub extern "C" fn forma_sqrt(n: f64) -> f64 {
    n.sqrt()
}

/// Floor
#[no_mangle]
pub extern "C" fn forma_floor(n: f64) -> f64 {
    n.floor()
}

/// Ceiling
#[no_mangle]
pub extern "C" fn forma_ceil(n: f64) -> f64 {
    n.ceil()
}

/// Round to nearest integer
#[no_mangle]
pub extern "C" fn forma_round(n: f64) -> f64 {
    n.round()
}

/// Truncate (round toward zero)
#[no_mangle]
pub extern "C" fn forma_trunc(n: f64) -> f64 {
    n.trunc()
}

/// Sine
#[no_mangle]
pub extern "C" fn forma_sin(n: f64) -> f64 {
    n.sin()
}

/// Cosine
#[no_mangle]
pub extern "C" fn forma_cos(n: f64) -> f64 {
    n.cos()
}

/// Tangent
#[no_mangle]
pub extern "C" fn forma_tan(n: f64) -> f64 {
    n.tan()
}

/// Arc sine
#[no_mangle]
pub extern "C" fn forma_asin(n: f64) -> f64 {
    n.asin()
}

/// Arc cosine
#[no_mangle]
pub extern "C" fn forma_acos(n: f64) -> f64 {
    n.acos()
}

/// Arc tangent
#[no_mangle]
pub extern "C" fn forma_atan(n: f64) -> f64 {
    n.atan()
}

/// Arc tangent of y/x (handles quadrants correctly)
#[no_mangle]
pub extern "C" fn forma_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

/// Natural logarithm (ln)
#[no_mangle]
pub extern "C" fn forma_log(n: f64) -> f64 {
    n.ln()
}

/// Base-10 logarithm
#[no_mangle]
pub extern "C" fn forma_log10(n: f64) -> f64 {
    n.log10()
}

/// Base-2 logarithm
#[no_mangle]
pub extern "C" fn forma_log2(n: f64) -> f64 {
    n.log2()
}

/// Exponential (e^n)
#[no_mangle]
pub extern "C" fn forma_exp(n: f64) -> f64 {
    n.exp()
}

/// Modulo for floats (fmod)
#[no_mangle]
pub extern "C" fn forma_fmod(a: f64, b: f64) -> f64 {
    a % b
}

/// Hyperbolic sine
#[no_mangle]
pub extern "C" fn forma_sinh(n: f64) -> f64 {
    n.sinh()
}

/// Hyperbolic cosine
#[no_mangle]
pub extern "C" fn forma_cosh(n: f64) -> f64 {
    n.cosh()
}

/// Hyperbolic tangent
#[no_mangle]
pub extern "C" fn forma_tanh(n: f64) -> f64 {
    n.tanh()
}

/// Check if float is NaN
#[no_mangle]
pub extern "C" fn forma_is_nan(n: f64) -> bool {
    n.is_nan()
}

/// Check if float is infinite
#[no_mangle]
pub extern "C" fn forma_is_infinite(n: f64) -> bool {
    n.is_infinite()
}

/// Check if float is finite (not NaN or infinite)
#[no_mangle]
pub extern "C" fn forma_is_finite(n: f64) -> bool {
    n.is_finite()
}

/// PI constant
#[no_mangle]
pub extern "C" fn forma_pi() -> f64 {
    std::f64::consts::PI
}

/// E constant (Euler's number)
#[no_mangle]
pub extern "C" fn forma_e() -> f64 {
    std::f64::consts::E
}

/// Sign of integer: -1, 0, or 1
#[no_mangle]
pub extern "C" fn forma_sign_int(n: i64) -> i64 {
    match n.cmp(&0) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

/// Sign of float: -1.0, 0.0, or 1.0
#[no_mangle]
pub extern "C" fn forma_sign_float(n: f64) -> f64 {
    if n < 0.0 {
        -1.0
    } else if n > 0.0 {
        1.0
    } else {
        0.0
    }
}

/// Clamp integer to range [min, max]
#[no_mangle]
pub extern "C" fn forma_clamp_int(n: i64, min: i64, max: i64) -> i64 {
    n.clamp(min, max)
}

/// Clamp float to range [min, max]
#[no_mangle]
pub extern "C" fn forma_clamp_float(n: f64, min: f64, max: f64) -> f64 {
    n.clamp(min, max)
}
