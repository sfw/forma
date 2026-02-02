# FORMA v1.2 Comprehensive Code Review

**Date:** January 26, 2026
**Status:** Post-Sprint 16 Review
**Reviewers:** 6 parallel code review agents

---

## Executive Summary

A comprehensive code review of the FORMA compiler identified **87+ issues** across all major components. While the language is functional and passes 250 tests, several areas need attention before a production release.

| Severity | Count | Description |
|----------|-------|-------------|
| **CRITICAL** | 12 | Could crash compiler/runtime, memory safety |
| **HIGH** | 23 | Incorrect behavior, missing features |
| **MEDIUM** | 31 | Incomplete implementations, edge cases |
| **LOW** | 21+ | Code quality, documentation |

---

## Critical Issues (Must Fix)

### 1. LLVM Codegen - Panic Points

| File | Line | Issue |
|------|------|-------|
| `src/codegen/llvm.rs` | 425 | `env_struct_type.size_of().unwrap()` - panics for unsized types |
| `src/codegen/llvm.rs` | 333-334 | `into_int_value()` - panics if float operands passed |
| `src/codegen/llvm.rs` | 489, 693 | `into_struct_value()` - panics if not a struct |
| `src/codegen/llvm.rs` | 432, 698-702 | `into_pointer_value()` - panics if not a pointer |

### 2. Interpreter - Panic Points

| File | Line | Issue |
|------|------|-------|
| `src/mir/interp.rs` | 343 | `expect("failed to create Tokio runtime")` - no graceful fallback |
| `src/mir/interp.rs` | 5514-5515 | gzip_compress `unwrap()` - panics on I/O errors |
| `src/mir/interp.rs` | 5552-5553 | zlib_compress `unwrap()` - same issue |
| `src/mir/interp.rs` | 5789 | `databases.get(&db_id).unwrap()` - race condition |
| `src/mir/interp.rs` | 973, 992 | `spawned_tasks.lock().unwrap()` - poisoned mutex |

### 3. Parser/Lexer - Panic Points

| File | Line | Issue |
|------|------|-------|
| `src/lexer/scanner.rs` | 310, 318 | `indent_stack.last().unwrap()` - could be empty |
| `src/lexer/scanner.rs` | 823, 827, 830 | `advance().unwrap()` in f-string parsing |

### 4. Type System - Silent Failures

| File | Line | Issue |
|------|------|-------|
| `src/types/inference.rs` | 3276-3283 | Field access on unresolved type returns fresh var (hides errors) |
| `src/types/inference.rs` | 4048-4057 | Tuple field access fallback silently succeeds |
| `src/types/inference.rs` | 4421-4437 | Await on non-Task/Future silently returns fresh var |

---

## High Priority Issues

### 5. LLVM Codegen - Missing Handlers

**Missing Rvalue Handlers (11 types):**
- `Ref`, `Deref`, `Tuple`, `Array`, `Struct`, `Enum`
- `Discriminant`, `EnumField`, `Field`, `TupleField`, `Index`

**Missing BinOp Handlers (5 types):**
- `BitAnd`, `BitOr`, `BitXor`, `Shl`, `Shr`

**Missing Float Operations:**
- All binary operations assume integers - no `fadd`, `fsub`, `fmul`, `fdiv`

### 6. LLVM Closure Issues

| Issue | Location | Description |
|-------|----------|-------------|
| Hardcoded return type | llvm.rs:713-717 | All closures assumed to return `i64` |
| Memory leak | llvm.rs:419-432 | Environment `malloc` with no `free` |
| Dead code | llvm.rs:72 | `closure_env_types` HashMap never used |
| No null check | llvm.rs:426-432 | malloc result not checked for null |

### 7. MIR/Interpreter Issues

| Issue | Location | Description |
|-------|----------|-------------|
| Loop labels broken | lower.rs:1881, 2006 | All `label: None` - labeled loops never work |
| Async semantics wrong | interp.rs:952-983 | Values evaluated BEFORE spawn, not async |
| Timeout non-functional | interp.rs:2392-2410 | "we don't actually timeout" |
| await_any incorrect | interp.rs:2443-2459 | Returns first task, no actual racing |
| No borrow checker | N/A | Memory safety not enforced |

### 8. Type System Issues

| Issue | Location | Description |
|-------|----------|-------------|
| Sentinel type vars | inference.rs:2892 | Hardcoded IDs 99999/99998/99997 could collide |
| No exhaustiveness check | inference.rs:4157-4182 | Match expressions not checked for completeness |
| Trait signatures | inference.rs:3786-3806 | Only param count checked, not types |
| No where clauses | inference.rs | Generic bounds never enforced |
| Associated types | types.rs:156 | Defined but never resolved |

### 9. Parser Issues

| Issue | Location | Description |
|-------|----------|-------------|
| WhileLet never produced | parser.rs:2429 | AST node exists but never created |
| Type suffix precedence | parser.rs:1093-1119 | `T?!` may not parse as expected |
| Dead token kinds | token.rs:78-86 | `F, S, E, T, I, M` variants unused |

---

## Medium Priority Issues

### 10. Tooling - Formatter Incomplete

**19+ ExprKind variants format as `?`:**
- `TupleField`, `ArrayRepeat`, `MapOrSet`, `Match`, `For`, `While`
- `WhileLet`, `Loop`, `Block`, `FieldShorthand`, `OpShorthand`
- `Break`, `Continue`, `Async`, `Coalesce`, `Pipeline`
- `AssignOp`, `Cast`, `Unsafe`

### 11. Tooling - REPL Issues

| Issue | Description |
|-------|-------------|
| No multi-line input | Functions must be single-line |
| Definition detection weak | Misses `pub f`, `async f`, `const`, etc. |
| No `:load` command | Can't load external files |
| Reserved identifiers | `__repl_main__`, `__result__` could conflict |

### 12. Tooling - CLI Issues

| Issue | Location | Description |
|-------|----------|-------------|
| Compile not implemented | main.rs:258-266 | Stub only |
| `args` ignored | main.rs:200-201 | Program args not passed to interpreter |
| `check_contracts` ignored | main.rs:268 | Flag does nothing |
| `output` ignored | main.rs:200 | `-o` flag unused |

### 13. Standard Library Issues

| File | Issue |
|------|-------|
| core.forma | `pow` not tail-recursive - stack overflow risk |
| core.forma | `lcm` overflow risk in `abs(a * b)` |
| datetime.forma | Invalid month returns 31 days |
| iter.forma | `range_step` with step=0 infinite loop risk |
| string.forma | Type inference bug - function commented out |
| iter.forma | Deprecated encoding still present |

---

## Low Priority Issues

### 14. Code Quality

| Category | Count | Examples |
|----------|-------|----------|
| TODOs in code | 8 | "TODO: proper type", "TODO: expose via API" |
| Unreachable macros | 3 | parser.rs:707, 799, 2297 |
| Dead code | 4 | Token variants, closure_env_types |
| Inconsistent docs | 5 | Map key type documentation |

### 15. Naming/Documentation

- `vec_tail` returns last element, not tail
- Map generic signature inconsistent (`Map[V]` vs `Map[K,V]`)
- Deprecated functions still exported

---

## Issue Summary by Component

| Component | Critical | High | Medium | Low |
|-----------|----------|------|--------|-----|
| LLVM Codegen | 4 | 8 | 3 | 1 |
| MIR/Interpreter | 5 | 5 | 4 | 2 |
| Type System | 3 | 5 | 4 | 2 |
| Parser/Lexer | 3 | 3 | 2 | 4 |
| Tooling | 0 | 2 | 8 | 5 |
| Standard Library | 0 | 0 | 10 | 7 |
| **TOTAL** | **12** | **23** | **31** | **21** |

---

## Recommended Fix Priority

### Sprint 17.1: Critical Fixes (Safety)
1. Replace all `unwrap()`/`expect()` with proper error handling
2. Fix LLVM `into_*_value()` calls with type checking
3. Fix compression/database panic points
4. Add mutex poisoning handling

### Sprint 17.2: High Priority (Correctness)
1. Fix loop labels implementation
2. Fix async semantics (or document as synchronous)
3. Add missing LLVM Rvalue handlers
4. Add float operation support
5. Fix closure return type handling

### Sprint 17.3: Medium Priority (Completeness)
1. Complete formatter for all ExprKind variants
2. Add multi-line REPL support
3. Implement exhaustiveness checking
4. Add trait signature validation
5. Fix type inference edge cases

### Sprint 17.4: Low Priority (Quality)
1. Remove dead code
2. Resolve TODOs or document as known limitations
3. Fix stdlib edge cases
4. Improve documentation consistency

---

## Test Coverage Analysis

Current: 250 Rust tests passing

**Missing test coverage:**
- LLVM closure indirect calls with various return types
- Loop labels break/continue
- Async parallel execution
- Formatter for all expression types
- Type inference edge cases (fresh var fallbacks)
- Exhaustiveness checking

---

*"Trust, but verify."*
