//! I/O functions for FORMA runtime

use libc::{c_char, c_int, size_t};
use std::ffi::CStr;
use std::io::{self, BufRead, Write};

/// Print a string to stdout (no newline)
#[no_mangle]
pub extern "C" fn forma_print(s: *const c_char) {
    if s.is_null() {
        return;
    }
    unsafe {
        let c_str = CStr::from_ptr(s);
        if let Ok(rust_str) = c_str.to_str() {
            print!("{}", rust_str);
            let _ = io::stdout().flush();
        }
    }
}

/// Print a string to stdout with newline
#[no_mangle]
pub extern "C" fn forma_println(s: *const c_char) {
    if s.is_null() {
        println!();
        return;
    }
    unsafe {
        let c_str = CStr::from_ptr(s);
        if let Ok(rust_str) = c_str.to_str() {
            println!("{}", rust_str);
        }
    }
}

/// Print an integer to stdout (no newline)
#[no_mangle]
pub extern "C" fn forma_print_int(n: i64) {
    print!("{}", n);
    let _ = io::stdout().flush();
}

/// Print an integer to stdout with newline
#[no_mangle]
pub extern "C" fn forma_println_int(n: i64) {
    println!("{}", n);
}

/// Print a float to stdout (no newline)
#[no_mangle]
pub extern "C" fn forma_print_float(n: f64) {
    print!("{}", n);
    let _ = io::stdout().flush();
}

/// Print a float to stdout with newline
#[no_mangle]
pub extern "C" fn forma_println_float(n: f64) {
    println!("{}", n);
}

/// Print a boolean to stdout (no newline)
#[no_mangle]
pub extern "C" fn forma_print_bool(b: bool) {
    print!("{}", b);
    let _ = io::stdout().flush();
}

/// Print a boolean to stdout with newline
#[no_mangle]
pub extern "C" fn forma_println_bool(b: bool) {
    println!("{}", b);
}

/// Read a line from stdin
/// Returns a heap-allocated null-terminated string that must be freed with forma_str_free
#[no_mangle]
pub extern "C" fn forma_read_line() -> *mut c_char {
    let stdin = io::stdin();
    let mut line = String::new();

    match stdin.lock().read_line(&mut line) {
        Ok(_) => {
            // Remove trailing newline if present
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }

            // Allocate and copy to C string
            let len = line.len();
            unsafe {
                let ptr = libc::malloc(len + 1) as *mut c_char;
                if ptr.is_null() {
                    return std::ptr::null_mut();
                }
                std::ptr::copy_nonoverlapping(line.as_ptr(), ptr as *mut u8, len);
                *ptr.add(len) = 0; // null terminator
                ptr
            }
        }
        Err(_) => std::ptr::null_mut(),
    }
}

/// Print formatted output (for debugging)
#[no_mangle]
pub extern "C" fn forma_debug_print(format: *const c_char, value: i64) {
    if format.is_null() {
        return;
    }
    unsafe {
        let c_str = CStr::from_ptr(format);
        if let Ok(fmt_str) = c_str.to_str() {
            // Simple format: replace {} with value
            let output = fmt_str.replace("{}", &value.to_string());
            println!("{}", output);
        }
    }
}

// Low-level write for when we need to bypass buffering
#[no_mangle]
pub extern "C" fn forma_write_stdout(buf: *const u8, len: size_t) -> c_int {
    if buf.is_null() {
        return -1;
    }
    unsafe {
        let slice = std::slice::from_raw_parts(buf, len);
        match io::stdout().write_all(slice) {
            Ok(_) => len as c_int,
            Err(_) => -1,
        }
    }
}

#[no_mangle]
pub extern "C" fn forma_write_stderr(buf: *const u8, len: size_t) -> c_int {
    if buf.is_null() {
        return -1;
    }
    unsafe {
        let slice = std::slice::from_raw_parts(buf, len);
        match io::stderr().write_all(slice) {
            Ok(_) => len as c_int,
            Err(_) => -1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_int_no_panic() {
        forma_print_int(42);
    }

    #[test]
    fn test_println_int_no_panic() {
        forma_println_int(-1);
    }

    #[test]
    fn test_write_stdout_null_safety() {
        assert_eq!(forma_write_stdout(std::ptr::null(), 10), -1);
    }

    #[test]
    fn test_write_stderr_null_safety() {
        assert_eq!(forma_write_stderr(std::ptr::null(), 10), -1);
    }

    #[test]
    fn test_print_null_safety() {
        forma_print(std::ptr::null());
    }

    #[test]
    fn test_debug_print_null_safety() {
        forma_debug_print(std::ptr::null(), 42);
    }
}
