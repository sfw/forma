//! Vector (dynamic array) runtime support for FORMA

use std::alloc::{self, Layout};
use std::ptr;

/// Internal representation of a FORMA vector.
#[repr(C)]
pub struct FormaVec {
    data: *mut u8,
    len: usize,
    capacity: usize,
    elem_size: usize,
}

impl FormaVec {
    fn layout_for(elem_size: usize, capacity: usize) -> Layout {
        Layout::from_size_align(elem_size * capacity, 8).unwrap()
    }

    fn grow(&mut self) {
        let new_cap = if self.capacity == 0 { 4 } else { self.capacity * 2 };
        let new_layout = Self::layout_for(self.elem_size, new_cap);
        let new_data = if self.capacity == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            let old_layout = Self::layout_for(self.elem_size, self.capacity);
            unsafe { alloc::realloc(self.data, old_layout, new_layout.size()) }
        };
        if new_data.is_null() {
            alloc::handle_alloc_error(new_layout);
        }
        self.data = new_data;
        self.capacity = new_cap;
    }
}

/// Create a new empty vector with the given element size.
#[no_mangle]
pub extern "C" fn forma_vec_new(elem_size: usize) -> *mut FormaVec {
    let v = Box::new(FormaVec {
        data: ptr::null_mut(),
        len: 0,
        capacity: 0,
        elem_size,
    });
    Box::into_raw(v)
}

/// Return the number of elements in the vector.
#[no_mangle]
pub extern "C" fn forma_vec_len(v: *const FormaVec) -> i64 {
    if v.is_null() {
        return 0;
    }
    unsafe { (*v).len as i64 }
}

/// Push an element onto the end of the vector.
/// `elem` points to `elem_size` bytes that will be copied into the vector.
#[no_mangle]
pub extern "C" fn forma_vec_push(v: *mut FormaVec, elem: *const u8) {
    if v.is_null() || elem.is_null() {
        return;
    }
    unsafe {
        let vec = &mut *v;
        if vec.len >= vec.capacity {
            vec.grow();
        }
        let dest = vec.data.add(vec.len * vec.elem_size);
        ptr::copy_nonoverlapping(elem, dest, vec.elem_size);
        vec.len += 1;
    }
}

/// Get a pointer to the element at the given index.
/// Returns null if the index is out of bounds.
#[no_mangle]
pub extern "C" fn forma_vec_get(v: *const FormaVec, idx: i64) -> *const u8 {
    if v.is_null() || idx < 0 {
        return ptr::null();
    }
    unsafe {
        let vec = &*v;
        let i = idx as usize;
        if i >= vec.len {
            return ptr::null();
        }
        vec.data.add(i * vec.elem_size)
    }
}

/// Set the element at the given index.
/// `elem` points to `elem_size` bytes that will be copied into the vector.
/// No-op if the index is out of bounds.
#[no_mangle]
pub extern "C" fn forma_vec_set(v: *mut FormaVec, idx: i64, elem: *const u8) {
    if v.is_null() || elem.is_null() || idx < 0 {
        return;
    }
    unsafe {
        let vec = &mut *v;
        let i = idx as usize;
        if i >= vec.len {
            return;
        }
        let dest = vec.data.add(i * vec.elem_size);
        ptr::copy_nonoverlapping(elem, dest, vec.elem_size);
    }
}

/// Free the vector and its backing storage.
#[no_mangle]
pub extern "C" fn forma_vec_free(v: *mut FormaVec) {
    if v.is_null() {
        return;
    }
    unsafe {
        let vec = Box::from_raw(v);
        if !vec.data.is_null() && vec.capacity > 0 {
            let layout = FormaVec::layout_for(vec.elem_size, vec.capacity);
            alloc::dealloc(vec.data, layout);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_and_len() {
        let v = forma_vec_new(std::mem::size_of::<i64>());
        assert_eq!(forma_vec_len(v), 0);
        forma_vec_free(v);
    }

    #[test]
    fn test_push_and_get() {
        let v = forma_vec_new(std::mem::size_of::<i64>());
        let val: i64 = 42;
        forma_vec_push(v, &val as *const i64 as *const u8);
        assert_eq!(forma_vec_len(v), 1);
        let ptr = forma_vec_get(v, 0);
        assert!(!ptr.is_null());
        let got = unsafe { *(ptr as *const i64) };
        assert_eq!(got, 42);
        forma_vec_free(v);
    }

    #[test]
    fn test_set() {
        let v = forma_vec_new(std::mem::size_of::<i64>());
        let val: i64 = 10;
        forma_vec_push(v, &val as *const i64 as *const u8);
        let new_val: i64 = 99;
        forma_vec_set(v, 0, &new_val as *const i64 as *const u8);
        let ptr = forma_vec_get(v, 0);
        let got = unsafe { *(ptr as *const i64) };
        assert_eq!(got, 99);
        forma_vec_free(v);
    }

    #[test]
    fn test_out_of_bounds() {
        let v = forma_vec_new(std::mem::size_of::<i64>());
        assert!(forma_vec_get(v, 0).is_null());
        assert!(forma_vec_get(v, -1).is_null());
        forma_vec_free(v);
    }

    #[test]
    fn test_null_safety() {
        assert_eq!(forma_vec_len(ptr::null()), 0);
        forma_vec_push(ptr::null_mut(), ptr::null());
        assert!(forma_vec_get(ptr::null(), 0).is_null());
        forma_vec_free(ptr::null_mut()); // should not crash
    }
}
