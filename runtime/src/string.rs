//! String functions for FORMA runtime

use libc::c_char;
use std::ffi::CStr;

/// Get the length of a string
#[no_mangle]
pub extern "C" fn forma_str_len(s: *const c_char) -> i64 {
    if s.is_null() {
        return 0;
    }
    unsafe {
        let c_str = CStr::from_ptr(s);
        c_str.to_bytes().len() as i64
    }
}

/// Concatenate two strings
/// Returns a heap-allocated string that must be freed with forma_str_free
#[no_mangle]
pub extern "C" fn forma_str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    let str_a = if a.is_null() {
        ""
    } else {
        unsafe {
            match CStr::from_ptr(a).to_str() {
                Ok(s) => s,
                Err(_) => return std::ptr::null_mut(),
            }
        }
    };

    let str_b = if b.is_null() {
        ""
    } else {
        unsafe {
            match CStr::from_ptr(b).to_str() {
                Ok(s) => s,
                Err(_) => return std::ptr::null_mut(),
            }
        }
    };

    let result = format!("{}{}", str_a, str_b);
    let len = result.len();

    unsafe {
        let ptr = libc::malloc(len + 1) as *mut c_char;
        if ptr.is_null() {
            return std::ptr::null_mut();
        }
        std::ptr::copy_nonoverlapping(result.as_ptr(), ptr as *mut u8, len);
        *ptr.add(len) = 0; // null terminator
        ptr
    }
}

/// Compare two strings for equality
#[no_mangle]
pub extern "C" fn forma_str_eq(a: *const c_char, b: *const c_char) -> bool {
    if a.is_null() && b.is_null() {
        return true;
    }
    if a.is_null() || b.is_null() {
        return false;
    }

    unsafe {
        let str_a = CStr::from_ptr(a);
        let str_b = CStr::from_ptr(b);
        str_a == str_b
    }
}

/// Compare two strings lexicographically
/// Returns negative if a < b, 0 if equal, positive if a > b
#[no_mangle]
pub extern "C" fn forma_str_cmp(a: *const c_char, b: *const c_char) -> i32 {
    if a.is_null() && b.is_null() {
        return 0;
    }
    if a.is_null() {
        return -1;
    }
    if b.is_null() {
        return 1;
    }

    unsafe {
        let str_a = CStr::from_ptr(a);
        let str_b = CStr::from_ptr(b);
        match str_a.cmp(str_b) {
            std::cmp::Ordering::Less => -1,
            std::cmp::Ordering::Equal => 0,
            std::cmp::Ordering::Greater => 1,
        }
    }
}

/// Convert an integer to a string
/// Returns a heap-allocated string that must be freed with forma_str_free
#[no_mangle]
pub extern "C" fn forma_int_to_str(n: i64) -> *mut c_char {
    let s = n.to_string();
    let len = s.len();

    unsafe {
        let ptr = libc::malloc(len + 1) as *mut c_char;
        if ptr.is_null() {
            return std::ptr::null_mut();
        }
        std::ptr::copy_nonoverlapping(s.as_ptr(), ptr as *mut u8, len);
        *ptr.add(len) = 0; // null terminator
        ptr
    }
}

/// Convert a float to a string
/// Returns a heap-allocated string that must be freed with forma_str_free
#[no_mangle]
pub extern "C" fn forma_float_to_str(n: f64) -> *mut c_char {
    let s = format!("{}", n);
    let len = s.len();

    unsafe {
        let ptr = libc::malloc(len + 1) as *mut c_char;
        if ptr.is_null() {
            return std::ptr::null_mut();
        }
        std::ptr::copy_nonoverlapping(s.as_ptr(), ptr as *mut u8, len);
        *ptr.add(len) = 0; // null terminator
        ptr
    }
}

/// Convert a boolean to a string
/// Returns a heap-allocated string that must be freed with forma_str_free
#[no_mangle]
pub extern "C" fn forma_bool_to_str(b: bool) -> *mut c_char {
    let s = if b { "true" } else { "false" };
    let len = s.len();

    unsafe {
        let ptr = libc::malloc(len + 1) as *mut c_char;
        if ptr.is_null() {
            return std::ptr::null_mut();
        }
        std::ptr::copy_nonoverlapping(s.as_ptr(), ptr as *mut u8, len);
        *ptr.add(len) = 0; // null terminator
        ptr
    }
}

/// Parse a string as an integer
/// Returns 0 if parsing fails
#[no_mangle]
pub extern "C" fn forma_str_to_int(s: *const c_char) -> i64 {
    if s.is_null() {
        return 0;
    }

    unsafe {
        let c_str = CStr::from_ptr(s);
        match c_str.to_str() {
            Ok(rust_str) => rust_str.trim().parse::<i64>().unwrap_or(0),
            Err(_) => 0,
        }
    }
}

/// Parse a string as an integer, returning -1 on failure (for Option handling)
/// This allows distinguishing between "0" and parse failure
#[no_mangle]
pub extern "C" fn forma_str_to_int_opt(s: *const c_char, success: *mut bool) -> i64 {
    if s.is_null() {
        if !success.is_null() {
            unsafe { *success = false };
        }
        return 0;
    }

    unsafe {
        let c_str = CStr::from_ptr(s);
        match c_str.to_str() {
            Ok(rust_str) => match rust_str.trim().parse::<i64>() {
                Ok(n) => {
                    if !success.is_null() {
                        *success = true;
                    }
                    n
                }
                Err(_) => {
                    if !success.is_null() {
                        *success = false;
                    }
                    0
                }
            },
            Err(_) => {
                if !success.is_null() {
                    *success = false;
                }
                0
            }
        }
    }
}

/// Parse a string as a float
/// Returns 0.0 if parsing fails
#[no_mangle]
pub extern "C" fn forma_str_to_float(s: *const c_char) -> f64 {
    if s.is_null() {
        return 0.0;
    }

    unsafe {
        let c_str = CStr::from_ptr(s);
        match c_str.to_str() {
            Ok(rust_str) => rust_str.trim().parse::<f64>().unwrap_or(0.0),
            Err(_) => 0.0,
        }
    }
}

/// Free a string allocated by the runtime
#[no_mangle]
pub extern "C" fn forma_str_free(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            libc::free(s as *mut libc::c_void);
        }
    }
}

/// Get a substring
/// Returns a heap-allocated string that must be freed with forma_str_free
#[no_mangle]
pub extern "C" fn forma_str_substr(s: *const c_char, start: i64, len: i64) -> *mut c_char {
    if s.is_null() || start < 0 || len < 0 {
        return std::ptr::null_mut();
    }

    unsafe {
        let c_str = CStr::from_ptr(s);
        match c_str.to_str() {
            Ok(rust_str) => {
                let start = start as usize;
                let len = len as usize;

                if start >= rust_str.len() {
                    // Return empty string
                    let ptr = libc::malloc(1) as *mut c_char;
                    if !ptr.is_null() {
                        *ptr = 0;
                    }
                    return ptr;
                }

                let end = std::cmp::min(start + len, rust_str.len());
                let substr = &rust_str[start..end];
                let substr_len = substr.len();

                let ptr = libc::malloc(substr_len + 1) as *mut c_char;
                if ptr.is_null() {
                    return std::ptr::null_mut();
                }
                std::ptr::copy_nonoverlapping(substr.as_ptr(), ptr as *mut u8, substr_len);
                *ptr.add(substr_len) = 0;
                ptr
            }
            Err(_) => std::ptr::null_mut(),
        }
    }
}

/// Check if a string contains a substring
#[no_mangle]
pub extern "C" fn forma_str_contains(haystack: *const c_char, needle: *const c_char) -> bool {
    if haystack.is_null() || needle.is_null() {
        return false;
    }

    unsafe {
        let hay = match CStr::from_ptr(haystack).to_str() {
            Ok(s) => s,
            Err(_) => return false,
        };
        let need = match CStr::from_ptr(needle).to_str() {
            Ok(s) => s,
            Err(_) => return false,
        };
        hay.contains(need)
    }
}

/// Find index of substring, returns -1 if not found
#[no_mangle]
pub extern "C" fn forma_str_find(haystack: *const c_char, needle: *const c_char) -> i64 {
    if haystack.is_null() || needle.is_null() {
        return -1;
    }

    unsafe {
        let hay = match CStr::from_ptr(haystack).to_str() {
            Ok(s) => s,
            Err(_) => return -1,
        };
        let need = match CStr::from_ptr(needle).to_str() {
            Ok(s) => s,
            Err(_) => return -1,
        };
        match hay.find(need) {
            Some(idx) => idx as i64,
            None => -1,
        }
    }
}

/// Duplicate a string
/// Returns a heap-allocated string that must be freed with forma_str_free
#[no_mangle]
pub extern "C" fn forma_str_dup(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        let c_str = CStr::from_ptr(s);
        let bytes = c_str.to_bytes_with_nul();
        let len = bytes.len();

        let ptr = libc::malloc(len) as *mut c_char;
        if ptr.is_null() {
            return std::ptr::null_mut();
        }
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr as *mut u8, len);
        ptr
    }
}
