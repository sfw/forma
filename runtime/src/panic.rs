//! Panic and error handling for FORMA runtime

use libc::c_char;
use std::ffi::CStr;
use std::process;

/// Panic with a message
/// This function never returns (calls exit)
#[no_mangle]
pub extern "C" fn forma_panic(msg: *const c_char) -> ! {
    if msg.is_null() {
        eprintln!("FORMA panic: (no message)");
    } else {
        unsafe {
            let c_str = CStr::from_ptr(msg);
            match c_str.to_str() {
                Ok(s) => eprintln!("FORMA panic: {}", s),
                Err(_) => eprintln!("FORMA panic: (invalid UTF-8 message)"),
            }
        }
    }
    process::exit(1);
}

/// Panic with a formatted integer message
#[no_mangle]
pub extern "C" fn forma_panic_int(msg: *const c_char, value: i64) -> ! {
    if msg.is_null() {
        eprintln!("FORMA panic: {}", value);
    } else {
        unsafe {
            let c_str = CStr::from_ptr(msg);
            match c_str.to_str() {
                Ok(s) => eprintln!("FORMA panic: {} {}", s, value),
                Err(_) => eprintln!("FORMA panic: {}", value),
            }
        }
    }
    process::exit(1);
}

/// Assert a condition, panic if false
#[no_mangle]
pub extern "C" fn forma_assert(cond: bool, msg: *const c_char) {
    if !cond {
        forma_panic(msg);
    }
}

/// Assert with a value for debugging
#[no_mangle]
pub extern "C" fn forma_assert_eq_int(a: i64, b: i64, msg: *const c_char) {
    if a != b {
        if msg.is_null() {
            eprintln!("FORMA assertion failed: {} != {}", a, b);
        } else {
            unsafe {
                let c_str = CStr::from_ptr(msg);
                match c_str.to_str() {
                    Ok(s) => eprintln!("FORMA assertion failed: {}: {} != {}", s, a, b),
                    Err(_) => eprintln!("FORMA assertion failed: {} != {}", a, b),
                }
            }
        }
        process::exit(1);
    }
}

/// Unreachable code marker
#[no_mangle]
pub extern "C" fn forma_unreachable() -> ! {
    eprintln!("FORMA error: reached unreachable code");
    process::exit(1);
}

/// Index out of bounds panic
#[no_mangle]
pub extern "C" fn forma_bounds_check(index: i64, len: i64) {
    if index < 0 || index >= len {
        eprintln!("FORMA panic: index out of bounds: index {} len {}", index, len);
        process::exit(1);
    }
}

/// Division by zero check
#[no_mangle]
pub extern "C" fn forma_div_check(divisor: i64) {
    if divisor == 0 {
        eprintln!("FORMA panic: division by zero");
        process::exit(1);
    }
}

/// Null pointer check
#[no_mangle]
pub extern "C" fn forma_null_check(ptr: *const u8, msg: *const c_char) {
    if ptr.is_null() {
        if msg.is_null() {
            eprintln!("FORMA panic: null pointer dereference");
        } else {
            unsafe {
                let c_str = CStr::from_ptr(msg);
                match c_str.to_str() {
                    Ok(s) => eprintln!("FORMA panic: null pointer: {}", s),
                    Err(_) => eprintln!("FORMA panic: null pointer dereference"),
                }
            }
        }
        process::exit(1);
    }
}

/// Overflow check for addition
#[no_mangle]
pub extern "C" fn forma_add_overflow_check(a: i64, b: i64) -> i64 {
    match a.checked_add(b) {
        Some(result) => result,
        None => {
            eprintln!("FORMA panic: integer overflow in addition: {} + {}", a, b);
            process::exit(1);
        }
    }
}

/// Overflow check for subtraction
#[no_mangle]
pub extern "C" fn forma_sub_overflow_check(a: i64, b: i64) -> i64 {
    match a.checked_sub(b) {
        Some(result) => result,
        None => {
            eprintln!("FORMA panic: integer overflow in subtraction: {} - {}", a, b);
            process::exit(1);
        }
    }
}

/// Overflow check for multiplication
#[no_mangle]
pub extern "C" fn forma_mul_overflow_check(a: i64, b: i64) -> i64 {
    match a.checked_mul(b) {
        Some(result) => result,
        None => {
            eprintln!("FORMA panic: integer overflow in multiplication: {} * {}", a, b);
            process::exit(1);
        }
    }
}
