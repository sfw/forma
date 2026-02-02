# FORMA Runtime Library Design

**Goal:** Create a production-quality runtime library for LLVM-compiled FORMA binaries
**Status:** Design Document
**Date:** January 26, 2026

---

## Research Summary

### How Go Does It

[Go's runtime](https://pkg.go.dev/runtime) is a comprehensive library that includes:
- **Scheduler:** Manages goroutines (lightweight threads)
- **Garbage Collector:** Concurrent mark-and-sweep
- **Memory Allocator:** Efficient small-object allocation
- **Stack Management:** Growable stacks

Go compiles to a single static binary that includes the runtime. The runtime is written in a mix of Go and assembly. [libgo](https://pkg.go.dev/github.com/gcc-mirror/gcc/libgo/go/runtime) is the GCC implementation.

### How Rust Does It

[Rust's std library](https://doc.rust-lang.org/std/) provides:
- **Core types:** Vec, String, HashMap, etc.
- **I/O abstractions:** File, TcpStream, etc.
- **Threading:** std::thread, channels
- **Runtime initialization:** Stack overflow protection, argument parsing

Rust ships [precompiled libraries](https://doc.rust-lang.org/reference/linkage.html) (libstd, libcore, liballoc) that are linked into the final binary. The runtime is minimal - no GC, no scheduler.

### LLVM's Approach

[LLVM compiler-rt](https://compiler-rt.llvm.org/) provides:
- **Builtins:** Low-level operations (128-bit math, float conversions)
- **Atomics:** Lock-free operations
- **Sanitizers:** Address, thread, memory sanitizers

Languages using LLVM typically create their own runtime and link it with compiler-rt.

---

## FORMA Runtime Architecture

### Design Principles

1. **Single Binary Output:** Like Go, compile to a standalone executable
2. **Minimal Runtime:** Like Rust, no GC pause (use linear types instead)
3. **C ABI Compatibility:** Runtime written in Rust, exposed via C ABI for LLVM linking
4. **Layered Design:** Core → I/O → Async → Database/HTTP

### Component Layers

```
┌─────────────────────────────────────────────────────────────┐
│                    FORMA User Program                       │
├─────────────────────────────────────────────────────────────┤
│  High-Level Runtime (forma_runtime)                         │
│  - HTTP client/server                                       │
│  - Database operations                                      │
│  - JSON parsing                                             │
│  - Async runtime (Tokio)                                    │
├─────────────────────────────────────────────────────────────┤
│  Core Runtime (forma_core)                                  │
│  - Memory allocation (forma_alloc, forma_free)              │
│  - String operations (forma_str_*)                          │
│  - List/Map operations (forma_vec_*, forma_map_*)           │
│  - I/O operations (forma_print, forma_file_*)               │
│  - Math builtins (forma_pow, forma_sqrt, etc.)              │
├─────────────────────────────────────────────────────────────┤
│  Platform Layer                                             │
│  - libc bindings                                            │
│  - OS-specific implementations                              │
├─────────────────────────────────────────────────────────────┤
│  LLVM compiler-rt builtins                                  │
└─────────────────────────────────────────────────────────────┘
```

---

## Runtime Library Structure

### Project Layout

```
forma/
├── runtime/                    # NEW: Runtime library
│   ├── Cargo.toml             # Rust workspace for runtime
│   ├── forma_core/            # Core runtime (required)
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── memory.rs      # Allocation
│   │   │   ├── string.rs      # String operations
│   │   │   ├── vec.rs         # List operations
│   │   │   ├── map.rs         # Map operations
│   │   │   ├── io.rs          # Basic I/O
│   │   │   ├── math.rs        # Math functions
│   │   │   ├── time.rs        # Time functions
│   │   │   └── panic.rs       # Panic handler
│   │   └── Cargo.toml
│   ├── forma_async/           # Async runtime (optional)
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── executor.rs    # Tokio integration
│   │   │   ├── spawn.rs       # Task spawning
│   │   │   └── channel.rs     # Channels
│   │   └── Cargo.toml
│   ├── forma_http/            # HTTP runtime (optional)
│   │   └── ...
│   ├── forma_db/              # Database runtime (optional)
│   │   └── ...
│   └── build.rs               # Builds libforma_runtime.a
└── src/
    └── codegen/
        └── llvm.rs            # Updated to link runtime
```

### C ABI Exports

Every runtime function must be `extern "C"` and `#[no_mangle]`:

```rust
// runtime/forma_core/src/io.rs

use std::ffi::CStr;
use std::os::raw::c_char;

/// Print a string to stdout
#[no_mangle]
pub extern "C" fn forma_print(s: *const c_char) {
    let c_str = unsafe { CStr::from_ptr(s) };
    if let Ok(str_slice) = c_str.to_str() {
        println!("{}", str_slice);
    }
}

/// Print an integer to stdout
#[no_mangle]
pub extern "C" fn forma_print_int(n: i64) {
    println!("{}", n);
}

/// Print a float to stdout
#[no_mangle]
pub extern "C" fn forma_print_float(n: f64) {
    println!("{}", n);
}
```

---

## Builtin Categories and Implementation Priority

### Tier 1: Essential (Sprint 24-25)

Must have for any useful program:

| Category | Functions | Count |
|----------|-----------|-------|
| I/O | print, println, eprint, eprintln, read_line | 5 |
| Strings | str_len, str_concat, str_slice, str_contains, str_split, str_trim, str_to_upper, str_to_lower, str_replace, str_starts_with, str_ends_with, str_to_int, str_to_float, int_to_str, float_to_str | 15 |
| Math | abs, min, max, pow, sqrt, floor, ceil, round, sin, cos, tan, log, exp | 13 |
| Lists | vec_new, vec_len, vec_push, vec_pop, vec_get, vec_set, vec_slice, vec_concat, vec_contains, vec_index_of, vec_map, vec_filter, vec_fold | 13 |
| Maps | map_new, map_len, map_get, map_set, map_remove, map_contains, map_keys, map_values | 8 |
| Types | type_of, clone, is_none, is_some, unwrap, unwrap_or | 6 |
| **Total Tier 1** | | **60** |

### Tier 2: Standard (Sprint 26-27)

Common operations:

| Category | Functions | Count |
|----------|-----------|-------|
| File I/O | file_read, file_write, file_append, file_exists, file_delete, file_copy, dir_list, dir_create, dir_delete | 9 |
| Time | time_now, time_ms, time_sleep, time_format | 4 |
| Random | random_int, random_float, random_range | 3 |
| JSON | json_parse, json_stringify | 2 |
| Environment | env_get, env_set, env_remove, args | 4 |
| Assertions | assert, assert_eq, panic | 3 |
| **Total Tier 2** | | **25** |

### Tier 3: Advanced (Sprint 28-29)

Full-featured applications:

| Category | Functions | Count |
|----------|-----------|-------|
| Async | spawn, await, await_all, await_any, channel_new, channel_send, channel_recv | 7 |
| HTTP | http_get, http_post, http_request, http_serve | 4 |
| Database | db_open, db_close, db_query, db_execute, db_prepare | 5 |
| Regex | regex_match, regex_find, regex_replace | 3 |
| Crypto | hash_sha256, hash_md5, base64_encode, base64_decode | 4 |
| **Total Tier 3** | | **23** |

### Tier 4: FFI (Sprint 30)

Low-level interop:

| Category | Functions | Count |
|----------|-----------|-------|
| Memory | alloc, dealloc, mem_copy, mem_set | 4 |
| Pointers | ptr_offset, ptr_addr, ptr_from_addr, ptr_is_null | 4 |
| C Strings | cstr_to_str, cstr_from_str, cstr_free | 3 |
| **Total Tier 4** | | **11** |

**Grand Total: ~119 runtime functions**

---

## LLVM Codegen Integration

### Declaring External Functions

When LLVM codegen encounters a builtin call, it must:
1. Declare the external function (if not already declared)
2. Generate call instruction with correct ABI

```rust
// src/codegen/llvm.rs

impl<'ctx> LLVMCodegen<'ctx> {
    fn get_or_declare_builtin(&self, name: &str) -> FunctionValue<'ctx> {
        // Check if already declared
        if let Some(func) = self.module.get_function(name) {
            return func;
        }

        // Get signature for builtin
        let (param_types, return_type) = self.builtin_signature(name);

        // Declare external function
        let fn_type = return_type.fn_type(&param_types, false);
        self.module.add_function(name, fn_type, Some(Linkage::External))
    }

    fn builtin_signature(&self, name: &str) -> (Vec<BasicMetadataTypeEnum<'ctx>>, BasicTypeEnum<'ctx>) {
        match name {
            "forma_print" => (
                vec![self.context.i8_type().ptr_type(AddressSpace::default()).into()],
                self.context.void_type().into(),
            ),
            "forma_print_int" => (
                vec![self.context.i64_type().into()],
                self.context.void_type().into(),
            ),
            "forma_str_len" => (
                vec![self.context.i8_type().ptr_type(AddressSpace::default()).into()],
                self.context.i64_type().into(),
            ),
            "forma_vec_new" => (
                vec![],
                self.context.i8_type().ptr_type(AddressSpace::default()).into(),
            ),
            // ... etc for all builtins
            _ => panic!("Unknown builtin: {}", name),
        }
    }
}
```

### Linking the Runtime

```rust
// Build process:
// 1. Compile runtime: cd runtime && cargo build --release
// 2. Produces: runtime/target/release/libforma_runtime.a
// 3. LLVM links: forma build foo.forma -o foo
//    - Generates foo.o from LLVM
//    - Links: clang foo.o -L runtime/target/release -lforma_runtime -o foo
```

---

## Memory Management Strategy

### Current State (Interpreter)

The interpreter uses Rust's memory management - `Value` enum with `Box`, `Vec`, `HashMap` etc. GC is Rust's drop semantics.

### LLVM Runtime Options

**Option A: Reference Counting**
- Every heap value has a refcount
- `forma_retain(ptr)` and `forma_release(ptr)`
- Compiler inserts retain/release calls
- Pros: Deterministic, no pauses
- Cons: Cycles leak, overhead on every copy

**Option B: Tracing GC**
- Runtime includes mark-and-sweep GC
- Requires stack scanning or shadow stack
- Pros: Handles cycles
- Cons: GC pauses, complexity

**Option C: Arena Allocation**
- Allocate in arenas, free entire arena at once
- Good for request-scoped memory
- Pros: Fast, simple
- Cons: Not general-purpose

**Option D: Linear Types (Recommended for FORMA v2)**
- Compiler enforces single ownership
- No runtime GC needed
- Values must be explicitly consumed
- Pros: Zero runtime cost, no leaks
- Cons: Restrictive programming model

### Recommended Approach

**Phase 1 (v1.6):** Reference counting for simplicity
**Phase 2 (v2.0):** Linear types eliminate need for refcounting

```rust
// runtime/forma_core/src/memory.rs

use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct FormaObject {
    refcount: AtomicUsize,
    type_id: u32,
    // data follows...
}

#[no_mangle]
pub extern "C" fn forma_retain(ptr: *mut FormaObject) {
    if !ptr.is_null() {
        unsafe {
            (*ptr).refcount.fetch_add(1, Ordering::Relaxed);
        }
    }
}

#[no_mangle]
pub extern "C" fn forma_release(ptr: *mut FormaObject) {
    if !ptr.is_null() {
        unsafe {
            if (*ptr).refcount.fetch_sub(1, Ordering::Release) == 1 {
                std::sync::atomic::fence(Ordering::Acquire);
                forma_dealloc(ptr);
            }
        }
    }
}
```

---

## String Representation

### Option A: Null-terminated (C strings)
- Compatible with C libraries
- O(n) length calculation
- Can't contain null bytes

### Option B: Length-prefixed (Pascal strings)
- O(1) length
- Can contain null bytes
- Need wrapper for C interop

### Recommended: Fat Pointer

```rust
#[repr(C)]
pub struct FormaString {
    ptr: *const u8,
    len: usize,
    cap: usize,  // For owned strings
}

#[no_mangle]
pub extern "C" fn forma_str_new(ptr: *const u8, len: usize) -> *mut FormaString {
    let s = Box::new(FormaString {
        ptr,
        len,
        cap: len,
    });
    Box::into_raw(s)
}

#[no_mangle]
pub extern "C" fn forma_str_len(s: *const FormaString) -> usize {
    unsafe { (*s).len }
}
```

---

## List Representation

```rust
#[repr(C)]
pub struct FormaVec {
    ptr: *mut u8,      // Pointer to elements
    len: usize,        // Number of elements
    cap: usize,        // Capacity
    elem_size: usize,  // Size of each element
}

#[no_mangle]
pub extern "C" fn forma_vec_new(elem_size: usize) -> *mut FormaVec {
    let v = Box::new(FormaVec {
        ptr: std::ptr::null_mut(),
        len: 0,
        cap: 0,
        elem_size,
    });
    Box::into_raw(v)
}

#[no_mangle]
pub extern "C" fn forma_vec_push(v: *mut FormaVec, elem: *const u8) {
    unsafe {
        let vec = &mut *v;
        if vec.len == vec.cap {
            // Grow capacity
            let new_cap = if vec.cap == 0 { 4 } else { vec.cap * 2 };
            let new_ptr = std::alloc::realloc(
                vec.ptr,
                std::alloc::Layout::from_size_align_unchecked(vec.cap * vec.elem_size, 8),
                new_cap * vec.elem_size,
            );
            vec.ptr = new_ptr;
            vec.cap = new_cap;
        }
        std::ptr::copy_nonoverlapping(
            elem,
            vec.ptr.add(vec.len * vec.elem_size),
            vec.elem_size,
        );
        vec.len += 1;
    }
}

#[no_mangle]
pub extern "C" fn forma_vec_len(v: *const FormaVec) -> usize {
    unsafe { (*v).len }
}

#[no_mangle]
pub extern "C" fn forma_vec_get(v: *const FormaVec, idx: usize) -> *const u8 {
    unsafe {
        let vec = &*v;
        if idx >= vec.len {
            std::ptr::null()
        } else {
            vec.ptr.add(idx * vec.elem_size)
        }
    }
}
```

---

## Build and Distribution

### Building the Runtime

```bash
# Build as static library
cd runtime
cargo build --release
# Produces: target/release/libforma_runtime.a

# For cross-compilation:
cargo build --release --target x86_64-unknown-linux-gnu
cargo build --release --target aarch64-apple-darwin
```

### FORMA Build Process

```bash
# User runs:
forma build myprogram.forma -o myprogram

# Under the hood:
# 1. Parse and type check myprogram.forma
# 2. Generate LLVM IR
# 3. Compile to object file: llc -filetype=obj myprogram.ll -o myprogram.o
# 4. Link with runtime: clang myprogram.o -L$FORMA_RUNTIME -lforma_runtime -o myprogram
```

### Distribution

Distribute precompiled runtime libraries for common targets:
- `libforma_runtime-x86_64-linux.a`
- `libforma_runtime-aarch64-linux.a`
- `libforma_runtime-x86_64-darwin.a`
- `libforma_runtime-aarch64-darwin.a`
- `libforma_runtime-x86_64-windows.lib`

---

## Implementation Sprints

### Sprint 24-R: Runtime Foundation

1. Create `runtime/` project structure
2. Implement `forma_core` with:
   - Memory allocation (forma_alloc, forma_free)
   - Basic I/O (forma_print, forma_print_int)
   - String basics (forma_str_new, forma_str_len)
3. Update LLVM codegen to declare externals
4. Implement linking in `forma build`
5. Test: hello.forma compiles and runs

### Sprint 25-R: Core Runtime

1. Complete string operations (15 functions)
2. Complete math operations (13 functions)
3. Complete list operations (13 functions)
4. Complete map operations (8 functions)
5. Test: fibonacci.forma, comprehensive.forma compile and run

### Sprint 26-R: Standard Runtime

1. File I/O operations (9 functions)
2. Time operations (4 functions)
3. Random operations (3 functions)
4. Environment operations (4 functions)
5. Test: cli_with_db.forma (without DB) compiles

### Sprint 27-R: Advanced Runtime

1. Async executor (Tokio integration)
2. HTTP client/server
3. Database operations
4. Test: async_parallel.forma, web_server.forma compile

---

## Success Criteria

1. **`forma build examples/hello.forma -o hello && ./hello`** works
2. **All 13 examples** compile and run with LLVM backend
3. **Performance:** Within 2x of equivalent Rust code
4. **Binary size:** Reasonable (< 10MB for hello world with static linking)
5. **Cross-platform:** Works on Linux, macOS, Windows

---

## References

- [Go Runtime Package](https://pkg.go.dev/runtime)
- [Rust Standard Library](https://doc.rust-lang.org/std/)
- [LLVM compiler-rt](https://compiler-rt.llvm.org/)
- [Rust Linkage Reference](https://doc.rust-lang.org/reference/linkage.html)

---

*"A language without a runtime is just syntax."*
