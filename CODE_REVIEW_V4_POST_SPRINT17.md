# FORMA Post-Sprint 17 Code Review

**Date:** January 26, 2026
**Status:** Post-Sprint 17 Verification
**Purpose:** Identify remaining issues after Sprint 17 fixes

---

## Executive Summary

Sprint 17 successfully fixed most critical safety issues. However, verification revealed:

| Category | Fixed | Remaining |
|----------|-------|-----------|
| LLVM Codegen Safety | 90% | 2 minor issues |
| Interpreter Safety | 95% | Builtin arg validation |
| Parser/Lexer Safety | 100% | None |
| Loop Labels | 100% | None |
| Type Inference | 90% | Sentinel IDs still present |
| Formatter | 100% | None |

**New Critical Finding:** 50+ builtin functions don't validate argument counts before array access.

---

## Sprint 17 Verification Results

### ✅ Task 17.1: LLVM Codegen Safety - MOSTLY COMPLETE

**Fixed:**
- Helper functions added (lines 104-134): `as_int_value()`, `as_float_value()`, `as_struct_value()`, `as_pointer_value()`
- Safe helpers used in binary operations (lines 367-398)
- malloc error handling added (lines 502-508)

**Still Needs Work:**
| Line | Issue | Severity |
|------|-------|----------|
| 691 | `into_int_value()` in If terminator | LOW |
| 712 | `into_int_value()` in Switch terminator | LOW |

### ✅ Task 17.2: Interpreter Safety - MOSTLY COMPLETE

**Fixed:**
- Tokio runtime error handling (lines 340-344)
- Compression functions proper error handling (lines 5509-5571)
- Mutex lock error handling (lines 973, 993, 2387, 2432)

**All remaining `unwrap()` calls are SAFE:**
- DateTime epoch fallbacks use `unwrap_or_else`
- Test code uses `unwrap()` appropriately

### ✅ Task 17.3: Parser/Lexer Safety - COMPLETE

**Fixed:**
- `indent_stack.last()` uses `unwrap_or(0)` (lines 310, 318)
- f-string `advance()` uses `if let Some(c)` pattern (lines 823-836)

### ✅ Task 17.4: Loop Labels - COMPLETE

**Fixed:**
- `LoopContext.label` field properly declared (line 70)
- Label passed from AST in all loop contexts (lines 1881, 2007, 2074, 2115)

### ⚠️ Task 17.5: Type Inference Safety - PARTIAL

**Fixed:**
- Fresh var fallbacks replaced with proper errors (per Sprint 17 report)

**Still Needs Work:**
| Line | Issue | Severity |
|------|-------|----------|
| 3173-3180 | Sentinel TypeVar IDs (99999, 99998, 99997) still in use | MEDIUM |

### ✅ Task 17.7: Formatter - COMPLETE

**Fixed:**
- All expression handlers implemented (lines 451-535)
- TupleField, Match, For, While, WhileLet, Loop, Break, Continue all handled

---

## NEW Issues Discovered

### CRITICAL: Builtin Argument Validation Missing

**Impact:** Calling any builtin with wrong argument count causes array index panic

**50+ affected functions in `src/mir/interp.rs`:**

| Function | Line | Args Accessed | Risk |
|----------|------|---------------|------|
| `vec_len` | 1074 | `args[0]` | Panic on 0 args |
| `vec_get` | 1088 | `args[0]`, `args[1]` | Panic on <2 args |
| `vec_first` | 1118 | `args[0]` | Panic on 0 args |
| `vec_last` | 1144 | `args[0]` | Panic on 0 args |
| `str_len` | 1172 | `args[0]` | Panic on 0 args |
| `str_char_at` | 1186 | `args[0]`, `args[1]` | Panic on <2 args |
| `str_substring` | 1214 | `args[0..2]` | Panic on <3 args |
| `channel_send` | 2481 | `args[0]`, `args[1]` | Panic on <2 args |
| `channel_recv` | 2519 | `args[0]` | Panic on 0 args |
| `unwrap` | 1929 | `args[0]` | Panic on 0 args |
| `is_some` | 1993 | `args[0]` | Panic on 0 args |
| `is_none` | 2002 | `args[0]` | Panic on 0 args |
| `is_ok` | 2011 | `args[0]` | Panic on 0 args |
| `is_err` | 2020 | `args[0]` | Panic on 0 args |
| `file_read` | 2031 | `args[0]` | Panic on 0 args |
| ... | ... | ... | 35+ more |

**Only `str` function (line 1054) properly checks `args.is_empty()`**

### HIGH: Async Semantics Still Incorrect

| Issue | Line | Description |
|-------|------|-------------|
| Spawn is synchronous | 952-968 | Work evaluated BEFORE spawn, not during |
| await_any broken | 2458-2462 | Returns first task, doesn't race |
| timeout non-functional | 2395-2413 | Doesn't actually timeout |

### HIGH: LLVM Missing Rvalue Handlers

**10 Rvalue types not handled (fall to error at line 300-303):**

| Rvalue | Use Case |
|--------|----------|
| `Ref` | `&x`, `&mut x` |
| `Deref` | `*ptr` |
| `Tuple` | `(a, b, c)` |
| `Array` | `[1, 2, 3]` |
| `Struct` | `Point { x: 1, y: 2 }` |
| `Enum` | `Option::Some(x)` |
| `Discriminant` | Pattern matching |
| `EnumField` | `match opt { Some(x) => x }` |
| `Field` | `point.x` |
| `TupleField` | `tuple.0` |
| `Index` | `arr[i]` |

**2 Terminator types not handled:**
- `Spawn` - Async task spawning
- `Await` - Async task awaiting

### MEDIUM: MIR Lower.rs expect() calls

**8 internal invariant assertions in `src/mir/lower.rs`:**

| Line | expect() Message |
|------|------------------|
| 2345 | "new_temp called without current function" |
| 2353 | "new_local called without current function" |
| 2648 | "new_block called without current function" |
| 2654 | "emit called without current block" |
| 2656 | "emit called without current function" |
| 2662 | "terminate called without current block" |
| 2664 | "terminate called without current function" |

These represent internal compiler invariants. They could be converted to proper error returns for better error messages.

---

## Remaining Unwrap/Expect Summary

| Directory | Production Code | Test Code | Status |
|-----------|-----------------|-----------|--------|
| `src/codegen/` | 0 | 0 | ✅ CLEAN |
| `src/lexer/` | 0 | 0 | ✅ CLEAN |
| `src/types/` | 0 | 0 | ✅ CLEAN |
| `src/fmt/` | 0 | 0 | ✅ CLEAN |
| `src/parser/` | 0 | 0 | ✅ CLEAN |
| `src/mir/interp.rs` | 0 risky | 40+ safe | ✅ CLEAN |
| `src/mir/lower.rs` | 8 expect() | 10+ safe | ⚠️ INTERNAL |

---

## Recommended Sprint 18 Tasks

### 18.1: Builtin Argument Validation (CRITICAL)
Add argument count checks to all 50+ builtin functions.

### 18.2: Async Semantics Fix (HIGH)
Either fix spawn to be truly async or document it as synchronous.

### 18.3: LLVM Rvalue Handlers (HIGH)
Implement the 10 missing Rvalue handlers for compiled code.

### 18.4: Sentinel TypeVar IDs (MEDIUM)
Replace magic numbers with proper enum or type.

### 18.5: LLVM Terminator Handlers (LOW)
Implement Spawn/Await for compiled code (or document as interpreter-only).

### 18.6: MIR Lowerer Error Handling (LOW)
Convert expect() calls to proper Result returns.

---

## Test Coverage Status

```
Rust unit tests:     250 passing
std/*.forma:         All verified
examples/*.forma:    All verified
```

**Note:** Tests pass because they don't exercise the edge cases found (wrong arg counts, async racing, etc.)

---

*"Verification reveals what testing misses."*
