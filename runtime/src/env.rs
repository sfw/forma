//! Environment and command-line argument access for FORMA runtime

use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

/// Return the number of command-line arguments.
#[no_mangle]
pub extern "C" fn forma_args_count() -> i64 {
    std::env::args().count() as i64
}

/// Get the command-line argument at the given index.
/// Returns a newly allocated C string (caller must free with forma_str_free),
/// or null if the index is out of bounds.
#[no_mangle]
pub extern "C" fn forma_args_get(idx: i64) -> *mut c_char {
    if idx < 0 {
        return ptr::null_mut();
    }
    match std::env::args().nth(idx as usize) {
        Some(arg) => CString::new(arg).unwrap_or_default().into_raw(),
        None => ptr::null_mut(),
    }
}

/// Get an environment variable by name.
/// Returns a newly allocated C string (caller must free with forma_str_free),
/// or null if the variable is not set.
#[no_mangle]
pub extern "C" fn forma_env_get(name: *const c_char) -> *mut c_char {
    if name.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let name_str = CStr::from_ptr(name).to_string_lossy();
        match std::env::var(name_str.as_ref()) {
            Ok(val) => CString::new(val).unwrap_or_default().into_raw(),
            Err(_) => ptr::null_mut(),
        }
    }
}

/// Set an environment variable.
#[no_mangle]
pub extern "C" fn forma_env_set(name: *const c_char, value: *const c_char) {
    if name.is_null() || value.is_null() {
        return;
    }
    unsafe {
        let name_str = CStr::from_ptr(name).to_string_lossy().into_owned();
        let val_str = CStr::from_ptr(value).to_string_lossy().into_owned();
        std::env::set_var(&name_str, &val_str);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_args_count_positive() {
        assert!(forma_args_count() > 0);
    }

    #[test]
    fn test_args_get_out_of_bounds() {
        assert!(forma_args_get(9999).is_null());
    }

    #[test]
    fn test_args_get_negative() {
        assert!(forma_args_get(-1).is_null());
    }

    #[test]
    fn test_env_get_null_safety() {
        assert!(forma_env_get(ptr::null()).is_null());
    }
}
