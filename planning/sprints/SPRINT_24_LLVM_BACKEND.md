# Sprint 24: LLVM Backend Completion

**Goal:** Make LLVM-compiled code fully functional
**Priority:** HIGH
**Depends on:** None

---

## Overview

The LLVM codegen in `src/codegen/llvm.rs` has several unimplemented handlers that cause compiled programs to fail. This sprint implements all missing handlers.

---

## Task 24.1: Implement Rvalue::Ref Handler

**Location:** `src/codegen/llvm.rs` around line 300-303

### Problem
References (`&x`, `&mut x`) fall through to error handler.

### Solution
```rust
Rvalue::Ref(place, mutability) => {
    // Get pointer to the place
    let ptr = self.place_to_ptr(place);
    // Store as a reference value (pointer in LLVM)
    ptr
}
```

---

## Task 24.2: Implement Rvalue::Deref Handler

**Location:** `src/codegen/llvm.rs` around line 300-303

### Problem
Dereferences (`*ptr`) fall through to error handler.

### Solution
```rust
Rvalue::Deref(operand) => {
    let ptr = self.codegen_operand(operand);
    // Load the value pointed to
    self.builder.build_load(ptr, "deref")
}
```

---

## Task 24.3: Implement Rvalue::Enum Handler

**Location:** `src/codegen/llvm.rs` around line 300-303

### Problem
Enum construction falls through to error handler.

### Solution
```rust
Rvalue::Enum(variant_idx, fields) => {
    // Allocate enum struct: { discriminant: i32, data: union }
    let enum_ty = self.get_enum_type();
    let enum_ptr = self.builder.build_alloca(enum_ty, "enum");

    // Set discriminant
    let disc_ptr = self.builder.build_struct_gep(enum_ptr, 0, "disc_ptr");
    self.builder.build_store(self.i32_const(*variant_idx as i64), disc_ptr);

    // Set fields if any
    if !fields.is_empty() {
        let data_ptr = self.builder.build_struct_gep(enum_ptr, 1, "data_ptr");
        for (i, field) in fields.iter().enumerate() {
            let field_val = self.codegen_operand(field);
            let field_ptr = self.builder.build_struct_gep(data_ptr, i as u32, "field_ptr");
            self.builder.build_store(field_val, field_ptr);
        }
    }

    self.builder.build_load(enum_ptr, "enum_val")
}
```

---

## Task 24.4: Implement Rvalue::Discriminant Handler

**Location:** `src/codegen/llvm.rs` around line 300-303

### Problem
Pattern matching discriminant extraction fails.

### Solution
```rust
Rvalue::Discriminant(place) => {
    let enum_ptr = self.place_to_ptr(place);
    // Discriminant is first field of enum struct
    let disc_ptr = self.builder.build_struct_gep(enum_ptr, 0, "disc_ptr");
    self.builder.build_load(disc_ptr, "discriminant")
}
```

---

## Task 24.5: Implement Rvalue::EnumField Handler

**Location:** `src/codegen/llvm.rs` around line 300-303

### Problem
Extracting fields from enum variants fails.

### Solution
```rust
Rvalue::EnumField(place, field_idx) => {
    let enum_ptr = self.place_to_ptr(place);
    // Data is second field of enum struct
    let data_ptr = self.builder.build_struct_gep(enum_ptr, 1, "data_ptr");
    let field_ptr = self.builder.build_struct_gep(data_ptr, *field_idx as u32, "field_ptr");
    self.builder.build_load(field_ptr, "enum_field")
}
```

---

## Task 24.6: Implement Rvalue::Index Handler

**Location:** `src/codegen/llvm.rs` around line 300-303

### Problem
Array/list indexing (`arr[i]`) fails.

### Solution
```rust
Rvalue::Index(base, index) => {
    let base_ptr = self.codegen_operand(base);
    let idx = self.codegen_operand(index);

    // For lists: get data pointer, then index
    let data_ptr = self.builder.build_struct_gep(base_ptr, 1, "list_data");
    let elem_ptr = unsafe {
        self.builder.build_gep(data_ptr, &[idx], "elem_ptr")
    };
    self.builder.build_load(elem_ptr, "indexed_val")
}
```

---

## Task 24.7: Implement Terminator::Spawn Handler

**Location:** `src/codegen/llvm.rs` terminator handling

### Problem
Async task spawning not implemented for LLVM.

### Solution
For initial implementation, spawn can be a no-op that just calls the function synchronously (async in LLVM is complex):

```rust
Terminator::Spawn { func, args, dest, next } => {
    // Simplified: call synchronously, store future handle
    let result = self.codegen_call(func, args);
    self.codegen_place_store(dest, result);
    self.builder.build_unconditional_branch(self.get_block(*next));
}
```

Note: Full async support in LLVM requires coroutine intrinsics - can be marked as future enhancement.

---

## Task 24.8: Implement Terminator::Await Handler

**Location:** `src/codegen/llvm.rs` terminator handling

### Problem
Async await not implemented for LLVM.

### Solution
```rust
Terminator::Await { future, dest, next } => {
    // Simplified: future is already resolved (from sync spawn)
    let value = self.codegen_operand(future);
    self.codegen_place_store(dest, value);
    self.builder.build_unconditional_branch(self.get_block(*next));
}
```

---

## Task 24.9: Fix Closure Memory Leak

**Location:** `src/codegen/llvm.rs` lines 567-584

### Problem
Closure environments allocated with `malloc` but never freed.

### Solution
Add deallocation when closure goes out of scope:

```rust
// When closure is no longer needed (end of scope or explicit drop):
fn free_closure_env(&self, closure_ptr: PointerValue) {
    let env_ptr = self.builder.build_struct_gep(closure_ptr, 1, "env_ptr");
    let env = self.builder.build_load(env_ptr, "env");

    // Call free on the environment
    let free_fn = self.module.get_function("free").unwrap_or_else(|| {
        let fn_type = self.context.void_type().fn_type(
            &[self.context.i8_type().ptr_type(AddressSpace::Generic).into()],
            false,
        );
        self.module.add_function("free", fn_type, None)
    });

    self.builder.build_call(free_fn, &[env.into()], "");
}
```

Also track closures that need freeing at scope exit.

---

## Task 24.10: Add malloc Null Check

**Location:** `src/codegen/llvm.rs` lines 579-584

### Problem
malloc return value not checked for null.

### Solution
```rust
let malloc_result = self.builder.build_call(malloc_fn, &[size.into()], "malloc_result");
let ptr = malloc_result.try_as_basic_value().left().unwrap().into_pointer_value();

// Check for null
let is_null = self.builder.build_is_null(ptr, "is_null");
let alloc_ok_block = self.context.append_basic_block(self.current_fn, "alloc_ok");
let alloc_fail_block = self.context.append_basic_block(self.current_fn, "alloc_fail");

self.builder.build_conditional_branch(is_null, alloc_fail_block, alloc_ok_block);

// Handle allocation failure
self.builder.position_at_end(alloc_fail_block);
// Call abort() or panic function
let abort_fn = self.module.get_function("abort").unwrap();
self.builder.build_call(abort_fn, &[], "");
self.builder.build_unreachable();

self.builder.position_at_end(alloc_ok_block);
// Continue with ptr
```

---

## Task 24.11: Add Goto Validation

**Location:** `src/codegen/llvm.rs` lines 761-765

### Problem
Invalid branch to missing block creates invalid IR silently.

### Solution
```rust
Terminator::Goto(target) => {
    if let Some(block) = self.blocks.get(target) {
        self.builder.build_unconditional_branch(*block);
    } else {
        // Generate error instead of invalid IR
        panic!("LLVM codegen error: goto to undefined block {}", target);
        // Or better: return Err(CodegenError::UndefinedBlock(*target))
    }
}
```

---

## Verification

After implementation:

```bash
cargo test
cargo build --release

# Test LLVM compilation
./target/release/forma build examples/hello.forma -o hello
./hello

./target/release/forma build examples/fibonacci.forma -o fib
./fib
```

---

## Summary

| Task | Description | Complexity |
|------|-------------|------------|
| 24.1 | Rvalue::Ref | Low |
| 24.2 | Rvalue::Deref | Low |
| 24.3 | Rvalue::Enum | Medium |
| 24.4 | Rvalue::Discriminant | Low |
| 24.5 | Rvalue::EnumField | Low |
| 24.6 | Rvalue::Index | Medium |
| 24.7 | Terminator::Spawn | Medium |
| 24.8 | Terminator::Await | Low |
| 24.9 | Closure memory leak | Medium |
| 24.10 | malloc null check | Low |
| 24.11 | Goto validation | Low |

---

*"Complete the backend, complete the language."*
