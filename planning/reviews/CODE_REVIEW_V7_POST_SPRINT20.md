# FORMA Code Review V7 - Post Sprint 20

**Date:** January 26, 2026
**Status:** Sprint 20 Complete, Sprint 21 Ready
**Version:** v1.4

---

## Sprint 20 Results

### ✅ Completed Tasks

| Task | Description | Status |
|------|-------------|--------|
| 20.1 | Fix coalesce operator (`??`) | ✅ FIXED |
| 20.2 | Fix if-expression type inference | ✅ FIXED |
| 20.3 | Fix try operator (`?`) for Result | ✅ FIXED |
| 20.4 | Add validate_args! to all builtins | ✅ FIXED (248 calls) |
| 20.5 | Document FFI as unsafe | ✅ DOCUMENTED |
| 20.6 | Add thread-safety warning for env vars | ✅ DOCUMENTED |
| 20.7 | Document closure memory leak | ✅ DOCUMENTED |
| 20.8 | Fix spawned task error handling | ✅ FIXED |
| 20.9 | Fix await_any task cleanup | ✅ FIXED |

### Critical Issues Status

| Issue | Before Sprint 20 | After Sprint 20 |
|-------|------------------|-----------------|
| Coalesce operator broken | ❌ CRITICAL | ✅ FIXED |
| If-expression type wrong | ❌ CRITICAL | ✅ FIXED |
| Try operator inverted | ❌ CRITICAL | ✅ FIXED |
| FFI memory safety | ⚠️ UNSAFE | 📝 DOCUMENTED |
| Env var thread safety | ⚠️ UNSAFE | 📝 DOCUMENTED |
| Closure memory leak | ⚠️ LEAK | 📝 DOCUMENTED |

---

## Discovered Issue: Pattern Matching

During Sprint 20 verification, a pre-existing bug was found:

### Issue: Option/Result Variant Patterns Not Recognized

**Error:**
```
Unknown struct type in pattern: 'Ok'
Unknown struct type in pattern: 'Some'
```

**Root Cause:**
In `src/types/inference.rs` line 4580-4587, when processing patterns like `Some(x)` or `Ok(value)`:
- The code extracts the last segment of the path ("Some" or "Ok")
- It looks up this name as a type definition
- But "Some" is a variant of "Option", not a type itself
- Option is registered under "Option", not "Some"

**Impact:**
- `forma check std/*.forma` fails
- `forma check examples/*.forma` fails
- Any code using unqualified variant patterns fails type checking

**Solution:** Sprint 21 will add variant-to-enum resolution.

---

## Updated Issue Counts

| Component | Critical | High | Medium | Low | Status |
|-----------|----------|------|--------|-----|--------|
| MIR Interpreter | ~~2~~ 0 | ~~5~~ 2 | 8 | 12 | Sprint 20 fixed critical |
| MIR Lowerer | ~~3~~ 0 | 3 | 4 | 3 | Sprint 20 fixed critical |
| LLVM Codegen | 1 | 4 | 3 | 2 | Documented |
| Type Inference | ~~0~~ **1** | 4 | 5 | 2 | Pattern bug discovered |
| Parser | 0 | 0 | 5 | 6 | Unchanged |
| Lexer/Scanner | 0 | 1 | 5 | 3 | Unchanged |
| **TOTAL** | **2** | **14** | **30** | **28** | Improved |

---

## Sprint 21 Plan (Ready)

### HIGH Priority - Must Fix

**Task 21.1-21.4:** Fix Option/Result Pattern Matching

The pattern matching system needs to resolve variant names (Some, None, Ok, Err) to their parent enum types (Option, Result) when checking patterns.

See: `SPRINT_21_PATTERN_FIX.md`

### Remaining High Priority (from V6 review)

| Issue | Component | Description |
|-------|-----------|-------------|
| Missing LLVM Rvalue handlers | LLVM Codegen | Ref, Deref, Enum, Discriminant, EnumField, Index |
| Missing LLVM Terminator handlers | LLVM Codegen | Spawn, Await |
| FieldShorthand type check | Type Inference | `.foo` = `.bar` not validated |
| OpShorthand type check | Type Inference | `(+ 1)` not constrained |
| Failed unification state | Type Inference | Partial substitutions corrupt state |

---

## Verification Metrics

```
=====================================
FORMA v1.4 Post-Sprint 20
=====================================

Rust Unit Tests:        251 passing
validate_args! calls:   248 (was ~83)
Critical bugs fixed:    3 (coalesce, if-type, try)

Blocking Issues:
- Pattern matching for Option/Result variants
=====================================
```

---

## Recommendations

### Immediate (Sprint 21)
1. **Fix variant pattern resolution** - Required for `forma check` to work
2. This is the primary blocker for release

### Near-term (Sprint 22+)
1. Add missing LLVM handlers for full compiled support
2. Fix type inference edge cases
3. Improve error messages

### Future (v2.0)
1. Linear types + capability-based safety (see MEMORY_SAFETY_DESIGN.md)
2. Parser completeness
3. Lexer Unicode support

---

## Files Modified in Sprint 20

| File | Changes |
|------|---------|
| `src/mir/lower.rs` | Coalesce, if-type, try operator fixes |
| `src/mir/interp.rs` | 162+ new validate_args! calls, task error handling |
| `KNOWN_LIMITATIONS.md` | FFI, env vars, closure leak docs |

---

## Next Steps

1. Run Sprint 21 to fix pattern matching
2. After Sprint 21: `forma check std/*.forma` and `forma check examples/*.forma` should pass
3. Then: Comprehensive test of all examples
4. Release readiness assessment

---

*"Almost there."*
