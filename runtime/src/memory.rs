//! Memory allocation functions for FORMA runtime

use libc::size_t;

/// Allocate memory
/// Returns null on failure
#[no_mangle]
pub extern "C" fn forma_alloc(size: size_t) -> *mut u8 {
    if size == 0 {
        return std::ptr::null_mut();
    }

    unsafe {
        let ptr = libc::malloc(size);
        ptr as *mut u8
    }
}

/// Allocate zeroed memory
/// Returns null on failure
#[no_mangle]
pub extern "C" fn forma_alloc_zeroed(size: size_t) -> *mut u8 {
    if size == 0 {
        return std::ptr::null_mut();
    }

    unsafe {
        let ptr = libc::calloc(1, size);
        ptr as *mut u8
    }
}

/// Reallocate memory
/// Returns null on failure (original memory is not freed)
#[no_mangle]
pub extern "C" fn forma_realloc(ptr: *mut u8, new_size: size_t) -> *mut u8 {
    if new_size == 0 {
        forma_dealloc(ptr);
        return std::ptr::null_mut();
    }

    unsafe {
        let new_ptr = libc::realloc(ptr as *mut libc::c_void, new_size);
        new_ptr as *mut u8
    }
}

/// Deallocate memory
#[no_mangle]
pub extern "C" fn forma_dealloc(ptr: *mut u8) {
    if !ptr.is_null() {
        unsafe {
            libc::free(ptr as *mut libc::c_void);
        }
    }
}

/// Copy memory
#[no_mangle]
pub extern "C" fn forma_memcpy(dst: *mut u8, src: *const u8, size: size_t) {
    if dst.is_null() || src.is_null() || size == 0 {
        return;
    }

    unsafe {
        std::ptr::copy_nonoverlapping(src, dst, size);
    }
}

/// Move memory (handles overlapping regions)
#[no_mangle]
pub extern "C" fn forma_memmove(dst: *mut u8, src: *const u8, size: size_t) {
    if dst.is_null() || src.is_null() || size == 0 {
        return;
    }

    unsafe {
        std::ptr::copy(src, dst, size);
    }
}

/// Set memory to a value
#[no_mangle]
pub extern "C" fn forma_memset(ptr: *mut u8, value: i32, size: size_t) {
    if ptr.is_null() || size == 0 {
        return;
    }

    unsafe {
        std::ptr::write_bytes(ptr, value as u8, size);
    }
}

/// Compare memory
/// Returns 0 if equal, negative if a < b, positive if a > b
#[no_mangle]
pub extern "C" fn forma_memcmp(a: *const u8, b: *const u8, size: size_t) -> i32 {
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
        let slice_a = std::slice::from_raw_parts(a, size);
        let slice_b = std::slice::from_raw_parts(b, size);

        for i in 0..size {
            if slice_a[i] < slice_b[i] {
                return -1;
            }
            if slice_a[i] > slice_b[i] {
                return 1;
            }
        }
        0
    }
}

/// Get allocation size (for debugging)
/// Note: This is not reliable for all allocators
#[no_mangle]
pub extern "C" fn forma_alloc_size(_ptr: *const u8) -> size_t {
    // Most allocators don't provide this info
    // Return 0 to indicate unknown
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alloc_dealloc() {
        let ptr = forma_alloc(64);
        assert!(!ptr.is_null());
        forma_dealloc(ptr);
    }

    #[test]
    fn test_zero_alloc() {
        let ptr = forma_alloc(0);
        assert!(ptr.is_null());
    }

    #[test]
    fn test_zeroed_alloc() {
        let ptr = forma_alloc_zeroed(32);
        assert!(!ptr.is_null());
        unsafe {
            for i in 0..32 {
                assert_eq!(*ptr.add(i), 0);
            }
        }
        forma_dealloc(ptr);
    }

    #[test]
    fn test_memcpy() {
        let src: [u8; 4] = [1, 2, 3, 4];
        let mut dst: [u8; 4] = [0; 4];
        forma_memcpy(dst.as_mut_ptr(), src.as_ptr(), 4);
        assert_eq!(dst, [1, 2, 3, 4]);
    }

    #[test]
    fn test_memcmp() {
        let a: [u8; 3] = [1, 2, 3];
        let b: [u8; 3] = [1, 2, 3];
        let c: [u8; 3] = [1, 2, 4];
        assert_eq!(forma_memcmp(a.as_ptr(), b.as_ptr(), 3), 0);
        assert!(forma_memcmp(a.as_ptr(), c.as_ptr(), 3) < 0);
        assert!(forma_memcmp(c.as_ptr(), a.as_ptr(), 3) > 0);
    }

    #[test]
    fn test_memset() {
        let mut buf: [u8; 4] = [0; 4];
        forma_memset(buf.as_mut_ptr(), 0xFF, 4);
        assert_eq!(buf, [0xFF; 4]);
    }

    #[test]
    fn test_null_safety() {
        forma_dealloc(std::ptr::null_mut()); // should not crash
        forma_memcpy(std::ptr::null_mut(), std::ptr::null(), 10); // no-op
        forma_memset(std::ptr::null_mut(), 0, 10); // no-op
        assert_eq!(forma_memcmp(std::ptr::null(), std::ptr::null(), 0), 0);
    }
}
