# Sprint 24-R: Runtime Library Foundation

**Goal:** Create the FORMA runtime library foundation and get `hello.forma` compiling with LLVM
**Priority:** CRITICAL - Blocks all LLVM compilation
**Depends on:** Sprint 24 LLVM codegen improvements

---

## Overview

FORMA needs a runtime library (like Go's runtime or Rust's libstd) that provides builtin functions for LLVM-compiled binaries. This sprint creates the foundation.

---

## Task 24-R.1: Create Runtime Project Structure

Create the runtime library as a Rust project:

```
forma/
└── runtime/
    ├── Cargo.toml
    └── src/
        ├── lib.rs          # Crate root, re-exports
        ├── memory.rs       # Allocation functions
        ├── string.rs       # String operations
        ├── io.rs           # Print and basic I/O
        ├── math.rs         # Math builtins
        ├── vec.rs          # List operations
        ├── map.rs          # Map operations
        └── panic.rs        # Panic handler
```

### Cargo.toml

```toml
[package]
name = "forma_runtime"
version = "0.1.0"
edition = "2021"

[lib]
name = "forma_runtime"
crate-type = ["staticlib", "cdylib"]

[dependencies]
libc = "0.2"

[profile.release]
opt-level = 3
lto = true
```

### src/lib.rs

```rust
//! FORMA Runtime Library
//!
//! Provides builtin functions for LLVM-compiled FORMA programs.

pub mod memory;
pub mod string;
pub mod io;
pub mod math;
pub mod vec;
pub mod map;
pub mod panic;

// Re-export all public functions
pub use memory::*;
pub use string::*;
pub use io::*;
pub use math::*;
pub use vec::*;
pub use map::*;
pub use panic::*;
```

---

## Task 24-R.2: Implement Core Memory Functions

### src/memory.rs

```rust
use std::alloc::{alloc, dealloc, realloc, Layout};
use std::ptr;

/// Allocate memory of given size
#[no_mangle]
pub extern "C" fn forma_alloc(size: usize) -> *mut u8 {
    if size == 0 {
        return ptr::null_mut();
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, 8);
        alloc(layout)
    }
}

/// Allocate zeroed memory
#[no_mangle]
pub extern "C" fn forma_alloc_zeroed(size: usize) -> *mut u8 {
    if size == 0 {
        return ptr::null_mut();
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, 8);
        std::alloc::alloc_zeroed(layout)
    }
}

/// Deallocate memory
#[no_mangle]
pub extern "C" fn forma_dealloc(ptr: *mut u8, size: usize) {
    if ptr.is_null() || size == 0 {
        return;
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, 8);
        dealloc(ptr, layout);
    }
}

/// Reallocate memory
#[no_mangle]
pub extern "C" fn forma_realloc(ptr: *mut u8, old_size: usize, new_size: usize) -> *mut u8 {
    if ptr.is_null() {
        return forma_alloc(new_size);
    }
    if new_size == 0 {
        forma_dealloc(ptr, old_size);
        return ptr::null_mut();
    }
    unsafe {
        let layout = Layout::from_size_align_unchecked(old_size, 8);
        realloc(ptr, layout, new_size)
    }
}

/// Copy memory
#[no_mangle]
pub extern "C" fn forma_memcpy(dst: *mut u8, src: *const u8, size: usize) {
    if dst.is_null() || src.is_null() || size == 0 {
        return;
    }
    unsafe {
        ptr::copy_nonoverlapping(src, dst, size);
    }
}

/// Set memory to value
#[no_mangle]
pub extern "C" fn forma_memset(ptr: *mut u8, value: u8, size: usize) {
    if ptr.is_null() || size == 0 {
        return;
    }
    unsafe {
        ptr::write_bytes(ptr, value, size);
    }
}
```

---

## Task 24-R.3: Implement Basic I/O

### src/io.rs

```rust
use std::ffi::CStr;
use std::io::{self, Write, BufRead};
use std::os::raw::c_char;

/// Print a null-terminated string to stdout
#[no_mangle]
pub extern "C" fn forma_print(s: *const c_char) {
    if s.is_null() {
        return;
    }
    let c_str = unsafe { CStr::from_ptr(s) };
    if let Ok(str_slice) = c_str.to_str() {
        print!("{}", str_slice);
        let _ = io::stdout().flush();
    }
}

/// Print a null-terminated string to stdout with newline
#[no_mangle]
pub extern "C" fn forma_println(s: *const c_char) {
    if s.is_null() {
        println!();
        return;
    }
    let c_str = unsafe { CStr::from_ptr(s) };
    if let Ok(str_slice) = c_str.to_str() {
        println!("{}", str_slice);
    }
}

/// Print an integer to stdout
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

/// Print a float to stdout
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

/// Print a boolean to stdout
#[no_mangle]
pub extern "C" fn forma_print_bool(b: bool) {
    print!("{}", b);
    let _ = io::stdout().flush();
}

/// Print to stderr
#[no_mangle]
pub extern "C" fn forma_eprint(s: *const c_char) {
    if s.is_null() {
        return;
    }
    let c_str = unsafe { CStr::from_ptr(s) };
    if let Ok(str_slice) = c_str.to_str() {
        eprint!("{}", str_slice);
        let _ = io::stderr().flush();
    }
}

/// Print to stderr with newline
#[no_mangle]
pub extern "C" fn forma_eprintln(s: *const c_char) {
    if s.is_null() {
        eprintln!();
        return;
    }
    let c_str = unsafe { CStr::from_ptr(s) };
    if let Ok(str_slice) = c_str.to_str() {
        eprintln!("{}", str_slice);
    }
}

/// Read a line from stdin, returns allocated string (caller must free)
#[no_mangle]
pub extern "C" fn forma_read_line() -> *mut c_char {
    let stdin = io::stdin();
    let mut line = String::new();
    match stdin.lock().read_line(&mut line) {
        Ok(_) => {
            // Remove trailing newline
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            // Convert to C string
            let c_string = std::ffi::CString::new(line).unwrap();
            c_string.into_raw()
        }
        Err(_) => std::ptr::null_mut(),
    }
}
```

---

## Task 24-R.4: Implement Basic String Operations

### src/string.rs

```rust
use std::ffi::{CStr, CString};
use std::os::raw::c_char;

/// Get string length
#[no_mangle]
pub extern "C" fn forma_str_len(s: *const c_char) -> i64 {
    if s.is_null() {
        return 0;
    }
    unsafe { CStr::from_ptr(s).to_bytes().len() as i64 }
}

/// Concatenate two strings, returns new allocated string
#[no_mangle]
pub extern "C" fn forma_str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    let str_a = if a.is_null() {
        ""
    } else {
        unsafe { CStr::from_ptr(a).to_str().unwrap_or("") }
    };
    let str_b = if b.is_null() {
        ""
    } else {
        unsafe { CStr::from_ptr(b).to_str().unwrap_or("") }
    };

    let result = format!("{}{}", str_a, str_b);
    CString::new(result).unwrap().into_raw()
}

/// Convert integer to string
#[no_mangle]
pub extern "C" fn forma_int_to_str(n: i64) -> *mut c_char {
    let s = n.to_string();
    CString::new(s).unwrap().into_raw()
}

/// Convert float to string
#[no_mangle]
pub extern "C" fn forma_float_to_str(n: f64) -> *mut c_char {
    let s = n.to_string();
    CString::new(s).unwrap().into_raw()
}

/// Convert string to integer, returns 0 on failure
#[no_mangle]
pub extern "C" fn forma_str_to_int(s: *const c_char) -> i64 {
    if s.is_null() {
        return 0;
    }
    let c_str = unsafe { CStr::from_ptr(s) };
    c_str.to_str().ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0)
}

/// Convert string to float, returns 0.0 on failure
#[no_mangle]
pub extern "C" fn forma_str_to_float(s: *const c_char) -> f64 {
    if s.is_null() {
        return 0.0;
    }
    let c_str = unsafe { CStr::from_ptr(s) };
    c_str.to_str().ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0.0)
}

/// Check if string contains substring
#[no_mangle]
pub extern "C" fn forma_str_contains(haystack: *const c_char, needle: *const c_char) -> bool {
    if haystack.is_null() || needle.is_null() {
        return false;
    }
    let h = unsafe { CStr::from_ptr(haystack).to_str().unwrap_or("") };
    let n = unsafe { CStr::from_ptr(needle).to_str().unwrap_or("") };
    h.contains(n)
}

/// Free a string allocated by runtime
#[no_mangle]
pub extern "C" fn forma_str_free(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            let _ = CString::from_raw(s);
        }
    }
}

/// Duplicate a string
#[no_mangle]
pub extern "C" fn forma_str_clone(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return std::ptr::null_mut();
    }
    let c_str = unsafe { CStr::from_ptr(s) };
    CString::new(c_str.to_bytes()).unwrap().into_raw()
}

/// Compare two strings, returns true if equal
#[no_mangle]
pub extern "C" fn forma_str_eq(a: *const c_char, b: *const c_char) -> bool {
    if a.is_null() && b.is_null() {
        return true;
    }
    if a.is_null() || b.is_null() {
        return false;
    }
    let str_a = unsafe { CStr::from_ptr(a) };
    let str_b = unsafe { CStr::from_ptr(b) };
    str_a == str_b
}
```

---

## Task 24-R.5: Implement Math Builtins

### src/math.rs

```rust
/// Absolute value (int)
#[no_mangle]
pub extern "C" fn forma_abs_int(n: i64) -> i64 {
    n.abs()
}

/// Absolute value (float)
#[no_mangle]
pub extern "C" fn forma_abs_float(n: f64) -> f64 {
    n.abs()
}

/// Minimum of two ints
#[no_mangle]
pub extern "C" fn forma_min_int(a: i64, b: i64) -> i64 {
    a.min(b)
}

/// Maximum of two ints
#[no_mangle]
pub extern "C" fn forma_max_int(a: i64, b: i64) -> i64 {
    a.max(b)
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

/// Power (int base, int exp)
#[no_mangle]
pub extern "C" fn forma_pow_int(base: i64, exp: i64) -> i64 {
    if exp < 0 {
        return 0;
    }
    base.pow(exp as u32)
}

/// Power (float)
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

/// Round
#[no_mangle]
pub extern "C" fn forma_round(n: f64) -> f64 {
    n.round()
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

/// Natural log
#[no_mangle]
pub extern "C" fn forma_log(n: f64) -> f64 {
    n.ln()
}

/// Log base 10
#[no_mangle]
pub extern "C" fn forma_log10(n: f64) -> f64 {
    n.log10()
}

/// Exponential (e^n)
#[no_mangle]
pub extern "C" fn forma_exp(n: f64) -> f64 {
    n.exp()
}
```

---

## Task 24-R.6: Implement Panic Handler

### src/panic.rs

```rust
use std::ffi::CStr;
use std::os::raw::c_char;
use std::process;

/// Panic with message
#[no_mangle]
pub extern "C" fn forma_panic(msg: *const c_char) -> ! {
    let message = if msg.is_null() {
        "panic".to_string()
    } else {
        unsafe { CStr::from_ptr(msg).to_str().unwrap_or("panic").to_string() }
    };
    eprintln!("FORMA PANIC: {}", message);
    process::exit(1);
}

/// Assert condition, panic if false
#[no_mangle]
pub extern "C" fn forma_assert(condition: bool, msg: *const c_char) {
    if !condition {
        forma_panic(msg);
    }
}

/// Assert equality
#[no_mangle]
pub extern "C" fn forma_assert_eq_int(a: i64, b: i64) {
    if a != b {
        eprintln!("FORMA PANIC: assertion failed: {} != {}", a, b);
        process::exit(1);
    }
}
```

---

## Task 24-R.7: Update LLVM Codegen for Builtins

**File:** `src/codegen/llvm.rs`

Add a mapping from FORMA builtin names to runtime function names:

```rust
impl<'ctx> LLVMCodegen<'ctx> {
    /// Map FORMA builtin name to runtime function name
    fn runtime_function_name(&self, builtin: &str) -> Option<&'static str> {
        match builtin {
            // I/O
            "print" => Some("forma_println"),
            "println" => Some("forma_println"),
            "eprint" => Some("forma_eprint"),
            "eprintln" => Some("forma_eprintln"),
            "read_line" => Some("forma_read_line"),

            // Strings
            "str_len" => Some("forma_str_len"),
            "str_concat" => Some("forma_str_concat"),
            "str_contains" => Some("forma_str_contains"),
            "int_to_str" => Some("forma_int_to_str"),
            "float_to_str" => Some("forma_float_to_str"),
            "str_to_int" => Some("forma_str_to_int"),
            "str_to_float" => Some("forma_str_to_float"),

            // Math
            "abs" => Some("forma_abs_int"),  // Need type dispatch
            "min" => Some("forma_min_int"),
            "max" => Some("forma_max_int"),
            "pow" => Some("forma_pow_int"),
            "sqrt" => Some("forma_sqrt"),
            "floor" => Some("forma_floor"),
            "ceil" => Some("forma_ceil"),
            "round" => Some("forma_round"),
            "sin" => Some("forma_sin"),
            "cos" => Some("forma_cos"),
            "tan" => Some("forma_tan"),
            "log" => Some("forma_log"),
            "exp" => Some("forma_exp"),

            // Panic
            "panic" => Some("forma_panic"),
            "assert" => Some("forma_assert"),

            _ => None,
        }
    }

    /// Declare a runtime function
    fn declare_runtime_function(&self, name: &str) -> FunctionValue<'ctx> {
        if let Some(existing) = self.module.get_function(name) {
            return existing;
        }

        let (params, ret, variadic) = self.runtime_signature(name);
        let fn_type = ret.fn_type(&params, variadic);
        self.module.add_function(name, fn_type, Some(Linkage::External))
    }

    fn runtime_signature(&self, name: &str) -> (Vec<BasicMetadataTypeEnum<'ctx>>, BasicTypeEnum<'ctx>, bool) {
        let void_type = self.context.void_type();
        let i64_type = self.context.i64_type();
        let f64_type = self.context.f64_type();
        let bool_type = self.context.bool_type();
        let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());

        match name {
            "forma_println" => (vec![ptr_type.into()], void_type.into(), false),
            "forma_println_int" => (vec![i64_type.into()], void_type.into(), false),
            "forma_println_float" => (vec![f64_type.into()], void_type.into(), false),
            "forma_str_len" => (vec![ptr_type.into()], i64_type.into(), false),
            "forma_str_concat" => (vec![ptr_type.into(), ptr_type.into()], ptr_type.into(), false),
            "forma_int_to_str" => (vec![i64_type.into()], ptr_type.into(), false),
            "forma_sqrt" => (vec![f64_type.into()], f64_type.into(), false),
            "forma_panic" => (vec![ptr_type.into()], void_type.into(), false),
            // Add all other signatures...
            _ => panic!("Unknown runtime function: {}", name),
        }
    }
}
```

---

## Task 24-R.8: Implement Linking

Update `forma build` to link the runtime:

**File:** `src/main.rs` or `src/codegen/mod.rs`

```rust
fn compile_and_link(input: &Path, output: &Path) -> Result<(), Error> {
    // 1. Generate LLVM IR and object file
    let obj_file = generate_object_file(input)?;

    // 2. Find runtime library
    let runtime_path = find_runtime_library()?;

    // 3. Link with clang/lld
    let status = std::process::Command::new("clang")
        .arg(&obj_file)
        .arg("-L")
        .arg(&runtime_path)
        .arg("-lforma_runtime")
        .arg("-o")
        .arg(output)
        .status()?;

    if !status.success() {
        return Err(Error::LinkFailed);
    }

    // 4. Clean up temp files
    std::fs::remove_file(&obj_file)?;

    Ok(())
}

fn find_runtime_library() -> Result<PathBuf, Error> {
    // Check in order:
    // 1. FORMA_RUNTIME_PATH env var
    // 2. Next to forma executable
    // 3. Standard install locations

    if let Ok(path) = std::env::var("FORMA_RUNTIME_PATH") {
        return Ok(PathBuf::from(path));
    }

    let exe_dir = std::env::current_exe()?.parent().unwrap().to_path_buf();
    let runtime_path = exe_dir.join("libforma_runtime.a");
    if runtime_path.exists() {
        return Ok(exe_dir);
    }

    Err(Error::RuntimeNotFound)
}
```

---

## Verification

### Build Runtime

```bash
cd forma/runtime
cargo build --release
ls target/release/libforma_runtime.a  # Should exist
```

### Test Linking

```bash
# Create test.c that calls runtime
cat > /tmp/test.c << 'EOF'
extern void forma_println(const char* s);
extern void forma_println_int(long n);
int main() {
    forma_println("Hello from C!");
    forma_println_int(42);
    return 0;
}
EOF

# Compile and link
clang /tmp/test.c -L runtime/target/release -lforma_runtime -o /tmp/test
/tmp/test
# Should print:
# Hello from C!
# 42
```

### Test FORMA Compilation

```bash
cargo build --release
./target/release/forma build examples/hello.forma -o hello
./hello
# Should print: Hello, World!
```

---

## Summary

| Task | Description | Est. Time |
|------|-------------|-----------|
| 24-R.1 | Project structure | 30 min |
| 24-R.2 | Memory functions | 1 hr |
| 24-R.3 | Basic I/O | 1 hr |
| 24-R.4 | String operations | 2 hr |
| 24-R.5 | Math builtins | 1 hr |
| 24-R.6 | Panic handler | 30 min |
| 24-R.7 | LLVM codegen integration | 3 hr |
| 24-R.8 | Linking implementation | 2 hr |
| **TOTAL** | | **~11 hr** |

---

*"A runtime is the bridge between language and machine."*
