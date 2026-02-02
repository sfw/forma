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
