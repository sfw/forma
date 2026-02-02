# FORMA Comprehensive Code Review V6

**Date:** January 26, 2026
**Status:** Post-Sprint 19 Deep Audit
**Reviewers:** 6 parallel code review agents

---

## Executive Summary

A thorough audit of all major FORMA components revealed **67 issues** across 6 components. While Sprint 17-19 fixed the critical panic points, this review found deeper logic bugs, missing features, and design inconsistencies.

| Component | Critical | High | Medium | Low |
|-----------|----------|------|--------|-----|
| MIR Interpreter | 2 | 5 | 8 | 12 |
| MIR Lowerer | 3 | 3 | 4 | 3 |
| LLVM Codegen | 1 | 4 | 3 | 2 |
| Type Inference | 0 | 4 | 5 | 2 |
| Parser | 0 | 0 | 5 | 6 |
| Lexer/Scanner | 0 | 1 | 5 | 3 |
| **TOTAL** | **6** | **17** | **30** | **28** |

---

## CRITICAL Issues (Must Fix Before Release)

### 1. MIR Lowerer: Coalesce Operator (`??`) is Broken
**File:** `src/mir/lower.rs` lines 1214-1222

```rust
ExprKind::Coalesce(left, right) => {
    // TODO: proper option handling
    let l = self.lower_expr(left);
    if l.is_some() {  // BUG: Checks if LOWERING succeeded, not runtime value!
        l
    } else {
        self.lower_expr(right)
    }
}
```

**Impact:** `a ?? b` never returns `b` unless `a` fails to compile. Completely non-functional.

### 2. MIR Lowerer: If-Expression Always Returns Int
**File:** `src/mir/lower.rs` line 1334

```rust
let result = self.new_temp(Ty::Int); // TODO: proper type
```

**Impact:** `if x then "hello" else "world"` would have type `Int`, causing runtime type errors.

### 3. MIR Lowerer: Try Operator (`?`) Inverted for Result
**File:** `src/mir/lower.rs` lines 1182-1198

The discriminant check uses `> 0` which works for Option (Some=1) but is inverted for Result (Ok=0, Err=1). This means `result?` would early-return on `Ok` values!

### 4. MIR Interpreter: FFI Pointer Operations are Unsafe
**File:** `src/mir/interp.rs` lines 4814-4905

Raw pointer operations (`cstr_to_str`, `mem_copy`, `mem_set`) accept arbitrary addresses from user code without validation. Memory corruption and crashes possible.

### 5. MIR Interpreter: Environment Variables Not Thread-Safe
**File:** `src/mir/interp.rs` lines 3383-3391

```rust
unsafe { std::env::set_var(&name, &value); }
```

With Tokio spawned tasks, `env_set` and `env_remove` can cause data races.

### 6. LLVM Codegen: Closure Memory Leak
**File:** `src/codegen/llvm.rs` lines 567-584

Closure environments are allocated with `malloc` but never freed. Every closure call leaks memory.

---

## HIGH Priority Issues

### MIR Interpreter

| Issue | Lines | Description |
|-------|-------|-------------|
| Spawned task errors swallowed | 1031-1034 | Errors become `Value::Unit` silently |
| await_any drops remaining tasks | 2672-2679 | Uncompleted tasks left running |
| Each spawn creates new runtime | 382-386 | Inefficient, resource exhaustion risk |
| Many builtins missing validate_args! | Various | `type_of`, `time_from_parts`, `map_values`, etc. |
| Undefined locals return Unit | 6299-6303 | Should be errors |

### MIR Lowerer

| Issue | Lines | Description |
|-------|-------|-------------|
| Unknown loop labels print to stderr | 835-838 | Should add to errors list |
| Closure entry_block not set | 1082-1084 | Fragile default assumption |
| Variant discriminant hash is weak | 1704-1712 | Sum of ASCII values, collisions possible |

### LLVM Codegen

| Issue | Lines | Description |
|-------|-------|-------------|
| Missing Rvalue handlers | 377-380 | Ref, Deref, Enum, Discriminant, EnumField, Index |
| Missing Terminator handlers | N/A | Spawn, Await |
| Goto to missing block silent | 761-765 | No error, invalid IR |
| malloc not null-checked | 579-584 | Allocation failure = crash |

### Type Inference

| Issue | Lines | Description |
|-------|-------|-------------|
| FieldShorthand ignores field name | 4507-4510 | `.foo` = `.bar` (no validation) |
| OpShorthand ignores operator | 4513-4519 | `(+ 1)` not type constrained |
| Failed unification corrupts state | 4088, 4457 | Partial substitutions on is_ok() |
| User-defined methods not looked up | 3145 | TODO comment, impl blocks ignored |

---

## MEDIUM Priority Issues

### Parser
- `WhileLet` not parsed (line 2436-2448)
- Loop labels not parsed (lines 2424-2458)
- Brace-style struct patterns not handled (line 2841)
- BitOr/BitAnd operators not parseable (lines 1512, 1551)
- No statement-level error recovery in blocks

### Lexer
- Unicode identifiers not supported
- F-string expressions can't contain strings
- Indentation misalignment silently accepted
- F-strings missing some escape sequences
- Raw strings can't contain backticks

### Type Inference
- ArrayRepeat count type not validated (line 4119)
- Trait validation is shallow (param/return types not checked)
- Empty `{}` always becomes Map, never Set

### MIR Lowerer
- Many PatternKind variants silently ignored
- Many ExprKind variants not implemented
- local_types not saved/restored for closures

---

## Summary by Risk Level

### Release Blockers (6)
1. Coalesce operator broken
2. If-expression type inference wrong
3. Try operator inverted for Result
4. FFI memory safety
5. Environment variable races
6. Closure memory leaks

### Should Fix (17)
- Task error handling
- Missing LLVM handlers
- Type inference silent failures
- Missing builtin validation

### Nice to Have (30)
- Parser completeness
- Lexer edge cases
- Better error messages

### Low Priority (28)
- Code quality improvements
- Documentation
- Performance

---

## Recommendations

### Sprint 20 (Critical Fixes)
1. Fix coalesce operator MIR lowering
2. Fix if-expression type inference
3. Fix try operator for Result types
4. Document FFI as unsafe (or add validation)
5. Add thread-safety warning for env vars
6. Add closure environment free (or document leak)

### Sprint 21 (High Priority)
1. Add missing LLVM Rvalue/Terminator handlers
2. Fix type inference silent failures
3. Add proper error handling for spawned tasks
4. Complete builtin argument validation

### Future
- Parser feature completeness
- Lexer Unicode support
- Better error recovery

---

*"The devil is in the details."*
