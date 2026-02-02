# Sprint 14.6: Fix Import System

**Priority:** CRITICAL - Imports must work for v1.0
**Date:** January 26, 2026

---

## Problem Statement

The import system infrastructure exists but doesn't work in practice:

1. **Silent failures:** Missing modules are silently ignored (dangerous!)
2. **Wrong paths:** Stdlib is in `stdlib/` but code tries `us std.io`
3. **No working test:** No test verifies imports actually work
4. **comprehensive.forma broken:** Uses `us std.io` which doesn't exist

---

## Task 14.6.1: Make Import Failures Produce Errors

**File:** `src/module/loader.rs` (around line 219)

**Current (dangerous):**
```rust
// If neither path exists, we could either error or ignore
// For now, we'll silently ignore missing modules
```

**Fix:** Produce an error for missing modules:

```rust
// If neither path exists, return an error
return Err(ModuleError {
    message: format!(
        "module not found: '{}' (tried {} and {})",
        module_path.join("."),
        file_path.display(),
        cwd_path.display()
    ),
    path: None,
});
```

**Acceptance Criteria:**
- [ ] `us nonexistent.module` produces clear error message
- [ ] Error shows which paths were tried
- [ ] No more silent failures

---

## Task 14.6.2: Organize Stdlib Directory Structure

**Current structure:**
```
forma/
  stdlib/
    core.forma
    vec.forma
    string.forma
    map.forma
    iter.forma
    json.forma
    datetime.forma
```

**New structure (create std/ symlink or move):**
```
forma/
  std/
    core.forma
    vec.forma
    string.forma
    map.forma
    iter.forma
    json.forma
    datetime.forma
    io.forma        # NEW - basic I/O functions
    prelude.forma   # NEW - commonly used re-exports
```

**Option A: Symlink (simpler):**
```bash
cd forma
ln -s stdlib std
```

**Option B: Rename (cleaner):**
```bash
cd forma
mv stdlib std
```

**Recommendation:** Use Option B (rename). Update any references to `stdlib/` in the codebase.

---

## Task 14.6.3: Create std/io.forma

**File:** `std/io.forma` (NEW FILE)

```forma
# FORMA Standard Library - I/O Module
# Basic input/output functions
# Most I/O is handled by builtins, this provides documentation and wrappers

# ============================================================
# Output Functions (Builtins)
# ============================================================

# Print a string to stdout (no newline)
# print(s: Str) -> ()
# Uses builtin: print

# Print a string to stdout with newline
# println(s: Str) -> ()
# Uses builtin: println

# Print formatted string (alias for print with f-string)
# Example: printf(f"Hello {name}!")

# ============================================================
# Input Functions (Builtins)
# ============================================================

# Read a line from stdin
# read_line() -> Str
# Uses builtin: read_line

# Read entire stdin as string
# read_all() -> Str
# Uses builtin: read_all (if implemented)

# ============================================================
# File I/O (Builtins)
# ============================================================

# Read entire file as string
# file_read(path: Str) -> Result[Str, Str]
# Uses builtin: file_read

# Write string to file
# file_write(path: Str, content: Str) -> Result[(), Str]
# Uses builtin: file_write

# Append string to file
# file_append(path: Str, content: Str) -> Result[(), Str]
# Uses builtin: file_append

# Check if file exists
# file_exists(path: Str) -> Bool
# Uses builtin: file_exists

# ============================================================
# Utility Functions
# ============================================================

# Print with automatic newline (wrapper)
f puts(s: Str) -> ()
    println(s)

# Print multiple values on separate lines
f print_lines(lines: [Str]) -> ()
    for line in lines
        println(line)

# Read file or return default on error
f file_read_or(path: Str, default: Str) -> Str
    m file_read(path)
        Ok(content) -> content
        Err(_) -> default
```

---

## Task 14.6.4: Create std/prelude.forma

**File:** `std/prelude.forma` (NEW FILE)

A prelude module that re-exports commonly used functions:

```forma
# FORMA Standard Library - Prelude
# Common functions automatically available
# Use with: us std.prelude

# This module serves as documentation for what's in the prelude.
# In practice, these are all builtins and don't need importing.

# ============================================================
# Core Types (Built-in)
# ============================================================
# Int, Float, Bool, Str, Char
# Option[T] = Some(T) | None
# Result[T, E] = Ok(T) | Err(E)
# Vec[T] = [T]
# Map[K, V] = {K: V}

# ============================================================
# Core Functions (Builtins)
# ============================================================

# I/O
# print(s: Str) -> ()
# println(s: Str) -> ()
# read_line() -> Str

# Type conversion
# int(x) -> Int
# float(x) -> Float
# str(x) -> Str
# bool(x) -> Bool

# Assertions
# assert(condition: Bool) -> ()
# assert_eq(a, b) -> ()

# ============================================================
# Commonly Re-exported
# ============================================================

# From std.vec
# vec_new, vec_push, vec_pop, vec_get, vec_len, vec_is_empty

# From std.string
# str_len, str_concat, str_split, str_trim, str_contains

# From std.map
# map_new, map_insert, map_get, map_remove, map_keys, map_values
```

---

## Task 14.6.5: Fix comprehensive.forma

**File:** `examples/comprehensive.forma`

**Current (broken):**
```forma
us std.io
```

**Options:**
1. Change to `us std.io` (after creating std/io.forma)
2. Remove the import if not needed
3. Change to import something that exists

**Fix:** Since comprehensive.forma only uses `print()` which is a builtin, we can either:
- Remove the import entirely (print is always available)
- Keep it as documentation that std.io exists

**Recommended fix:**
```forma
# Comprehensive FORMA program example

# Import I/O module (print/println are builtins, but this shows import syntax)
us std.io

# ... rest of file
```

---

## Task 14.6.6: Create Import Test

**File:** `tests/forma/test_imports.forma` (NEW FILE)

```forma
# Test Import System
# Verifies that us (use) statements work correctly

# Import the core module
us std.core

# Import specific modules
us std.string
us std.vec

# Test that imported functions are available
f test_core_import() -> Bool
    # min and max should be available from std.core
    result := min(10, 5)
    assert_eq(result, 5)
    true

f test_string_import() -> Bool
    # String functions should be available
    s := "hello world"
    parts := str_split(s, " ")
    assert_eq(vec_len(parts), 2)
    true

f test_vec_import() -> Bool
    # Vec functions should be available
    v := vec_new()
    v = vec_push(v, 1)
    v = vec_push(v, 2)
    assert_eq(vec_len(v), 2)
    true

f run_all_tests() -> Int
    passed := 0

    if test_core_import()
        print("PASS: test_core_import")
        passed = passed + 1
    else
        print("FAIL: test_core_import")

    if test_string_import()
        print("PASS: test_string_import")
        passed = passed + 1
    else
        print("FAIL: test_string_import")

    if test_vec_import()
        print("PASS: test_vec_import")
        passed = passed + 1
    else
        print("FAIL: test_vec_import")

    print("")
    print("Import tests: " + int_to_str(passed) + "/3 passed")

    if passed == 3 { 0 } else { 1 }

f main() -> Int
    run_all_tests()
```

---

## Task 14.6.7: Update Module Loader Search Paths

**File:** `src/module/loader.rs`

The module loader should search in this order:
1. Relative to current file's directory
2. Relative to project root (where forma is run from)
3. In the `std/` directory (for standard library)

**Add stdlib search path:**

```rust
fn resolve_module_path(&self, module_path: &[String]) -> Vec<PathBuf> {
    let mut candidates = Vec::new();

    // 1. Relative to base directory (current file's dir)
    let mut path = self.base_dir.clone();
    for segment in module_path {
        path.push(segment);
    }
    path.set_extension("forma");
    candidates.push(path);

    // 2. Relative to current working directory
    let mut cwd_path = PathBuf::from(".");
    for segment in module_path {
        cwd_path.push(segment);
    }
    cwd_path.set_extension("forma");
    candidates.push(cwd_path);

    // 3. In std/ directory (for stdlib)
    if module_path.first().map(|s| s.as_str()) == Some("std") {
        // std.core -> std/core.forma
        let mut std_path = PathBuf::from("std");
        for segment in module_path.iter().skip(1) {
            std_path.push(segment);
        }
        std_path.set_extension("forma");
        candidates.push(std_path);
    }

    candidates
}
```

Then update `load_imports` to try all candidates and error if none exist.

---

## Task 14.6.8: Verify All Stdlib Modules Are Importable

**Test script:**

```bash
#!/bin/bash
set -e

echo "=== Testing stdlib imports ==="

# Create a test file that imports each stdlib module
cat > /tmp/test_all_imports.forma << 'EOF'
us std.core
us std.vec
us std.string
us std.map
us std.iter
us std.json
us std.datetime
us std.io

f main() -> Int
    print("All imports successful!")
    0
EOF

cargo run --quiet -- run /tmp/test_all_imports.forma

echo "=== All stdlib modules importable ==="
```

---

## Definition of Done

- [ ] Import failures produce clear error messages (not silent)
- [ ] `stdlib/` renamed to `std/`
- [ ] `std/io.forma` created with I/O documentation
- [ ] `std/prelude.forma` created
- [ ] `examples/comprehensive.forma` compiles and runs
- [ ] `tests/forma/test_imports.forma` passes
- [ ] All stdlib modules can be imported with `us std.X`
- [ ] Module loader searches std/ directory

---

## Summary

| Task | Description | Effort |
|------|-------------|--------|
| 14.6.1 | Error on missing imports | 30 min |
| 14.6.2 | Rename stdlib → std | 15 min |
| 14.6.3 | Create std/io.forma | 30 min |
| 14.6.4 | Create std/prelude.forma | 30 min |
| 14.6.5 | Fix comprehensive.forma | 15 min |
| 14.6.6 | Create import test | 30 min |
| 14.6.7 | Update module loader paths | 1 hour |
| 14.6.8 | Verify all imports work | 15 min |
| **Total** | | **~4 hours** |

---

*"Imports should work or fail loudly - never silently."*
