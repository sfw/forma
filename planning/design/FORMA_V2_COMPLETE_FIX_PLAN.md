# FORMA v2.0 Complete Fix Plan

**Goal:** Fix ALL remaining issues and implement proper memory management
**Status:** Planning Document
**Date:** January 26, 2026

---

## Executive Summary

FORMA v1.5 has **67+ known issues** across all components. This plan addresses every issue systematically, culminating in proper memory safety implementation for v2.0.

---

## Phase 1: Critical & High Priority Fixes (Sprints 24-26)

### Sprint 24: LLVM Backend Completion

**Goal:** Make LLVM-compiled code fully functional

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 24.1 | Implement Rvalue::Ref handler | llvm.rs | 300-303 |
| 24.2 | Implement Rvalue::Deref handler | llvm.rs | 300-303 |
| 24.3 | Implement Rvalue::Enum handler | llvm.rs | 300-303 |
| 24.4 | Implement Rvalue::Discriminant handler | llvm.rs | 300-303 |
| 24.5 | Implement Rvalue::EnumField handler | llvm.rs | 300-303 |
| 24.6 | Implement Rvalue::Index handler | llvm.rs | 300-303 |
| 24.7 | Implement Terminator::Spawn handler | llvm.rs | N/A |
| 24.8 | Implement Terminator::Await handler | llvm.rs | N/A |
| 24.9 | Fix closure memory leak (add free) | llvm.rs | 567-584 |
| 24.10 | Add malloc null check | llvm.rs | 579-584 |
| 24.11 | Add goto validation | llvm.rs | 761-765 |

---

### Sprint 25: Async Runtime Fixes

**Goal:** Fix all async/concurrent execution issues

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 25.1 | Share Tokio runtime across spawns | interp.rs | 382-386 |
| 25.2 | Propagate spawned task errors | interp.rs | 1031-1034 |
| 25.3 | Clean up tasks in await_any | interp.rs | 2672-2679 |
| 25.4 | Add task cancellation support | interp.rs | N/A |
| 25.5 | Thread-safe env var wrapper | interp.rs | 3383-3391 |

---

### Sprint 26: Type Inference Robustness

**Goal:** Fix all type inference edge cases and gaps

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 26.1 | User-defined enum type substitution | inference.rs | pattern code |
| 26.2 | Fix FieldShorthand validation | inference.rs | 4507-4510 |
| 26.3 | Fix OpShorthand validation | inference.rs | 4513-4519 |
| 26.4 | Fix unification state corruption | inference.rs | 4088, 4457 |
| 26.5 | Validate ArrayRepeat count type | inference.rs | 4119 |
| 26.6 | Deep trait validation | inference.rs | 3786-3806 |
| 26.7 | User-defined method lookup | inference.rs | 3145 |
| 26.8 | Audit remaining validate_args! gaps | interp.rs | various |

---

## Phase 2: Parser & Lexer Completeness (Sprints 27-28)

### Sprint 27: Parser Feature Completion

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 27.1 | Implement WhileLet parsing | parser.rs | 2436-2448 |
| 27.2 | Implement loop label parsing | parser.rs | 2424-2458 |
| 27.3 | Implement brace-style struct patterns | parser.rs | 2841 |
| 27.4 | Add BitOr/BitAnd operators | parser.rs | 1512, 1551 |
| 27.5 | Add statement-level error recovery | parser.rs | block parsing |
| 27.6 | Fix unreachable!() as proper errors | parser.rs | 707, 799, 2297 |

---

### Sprint 28: Lexer Improvements

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 28.1 | Unicode identifier support | scanner.rs | identifier code |
| 28.2 | Fix f-string nested expressions | scanner.rs | f-string code |
| 28.3 | Fix indentation misalignment handling | scanner.rs | 313-328 |
| 28.4 | Complete f-string escape sequences | scanner.rs | f-string code |
| 28.5 | Raw string backtick support | scanner.rs | string code |

---

## Phase 3: MIR Lowerer Fixes (Sprint 29)

### Sprint 29: MIR Lowerer Completion

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 29.1 | Loop label error handling | lower.rs | 835-838 |
| 29.2 | Fix closure entry_block handling | lower.rs | 1082-1084 |
| 29.3 | Improve variant discriminant hashing | lower.rs | 1704-1712 |
| 29.4 | Implement missing PatternKind variants | lower.rs | pattern code |
| 29.5 | Implement missing ExprKind variants | lower.rs | expr code |
| 29.6 | Fix closure local_types save/restore | lower.rs | closure code |
| 29.7 | Convert expect() to proper errors | lower.rs | 2345, 2353, etc |

---

## Phase 4: Standard Library Fixes (Sprint 30)

### Sprint 30: Stdlib Robustness

| Task | Description | File |
|------|-------------|------|
| 30.1 | Make pow() iterative (not recursive) | core.forma |
| 30.2 | Fix lcm overflow (divide before multiply) | core.forma |
| 30.3 | Validate month in days_in_month | datetime.forma |
| 30.4 | Guard range_step against step=0 | iter.forma |
| 30.5 | Enable string character functions | string.forma |
| 30.6 | Replace iterator encoding hack | iter.forma |
| 30.7 | Fix vec_tail naming (rename to vec_last) | vec.forma |
| 30.8 | Fix Map generic signature consistency | map.forma |

---

## Phase 5: Tooling & CLI Fixes (Sprint 31)

### Sprint 31: Developer Experience

| Task | Description | File |
|------|-------------|------|
| 31.1 | REPL multi-line input | main.rs |
| 31.2 | REPL improved definition detection | main.rs |
| 31.3 | REPL :load command | main.rs |
| 31.4 | REPL reserved identifier namespacing | main.rs |
| 31.5 | CLI program args passing | main.rs |
| 31.6 | CLI compile command implementation | main.rs |
| 31.7 | CLI --check-contracts implementation | main.rs |
| 31.8 | CLI -o output flag implementation | main.rs |

---

## Phase 6: Memory Safety (Sprints 32-34)

### Sprint 32: FFI Safety Layer

| Task | Description | File |
|------|-------------|------|
| 32.1 | Create SafePtr wrapper type | new file |
| 32.2 | Bounds-checked memory operations | interp.rs |
| 32.3 | Lifetime tracking for FFI pointers | interp.rs |
| 32.4 | Capability-based FFI access | interp.rs |
| 32.5 | FFI safety documentation | SAFETY.md |

---

### Sprint 33: Linear Types Foundation

Based on MEMORY_SAFETY_DESIGN.md:

| Task | Description | File |
|------|-------------|------|
| 33.1 | Add `linear` keyword to lexer | scanner.rs |
| 33.2 | Add `linear` to type syntax | parser.rs |
| 33.3 | LinearityKind enum (Linear, Affine, Regular) | types.rs |
| 33.4 | Use-tracking in type checker | inference.rs |
| 33.5 | Consume-exactly-once validation | inference.rs |
| 33.6 | Drop trait for cleanup | stdlib |

---

### Sprint 34: Capability System

| Task | Description | File |
|------|-------------|------|
| 34.1 | Capability type hierarchy | types.rs |
| 34.2 | FileCapability implementation | stdlib |
| 34.3 | NetworkCapability implementation | stdlib |
| 34.4 | Capability-based I/O wrappers | stdlib |
| 34.5 | Main function capability injection | interp.rs |

---

## Phase 7: Verification & Release (Sprint 35)

### Sprint 35: Final Verification

| Task | Description |
|------|-------------|
| 35.1 | Comprehensive test suite expansion |
| 35.2 | Property-based testing for type system |
| 35.3 | Fuzz testing for parser/lexer |
| 35.4 | Memory leak detection with Valgrind |
| 35.5 | Performance benchmarks |
| 35.6 | Documentation audit |
| 35.7 | CHANGELOG update |
| 35.8 | Version bump to v2.0 |

---

## Summary by Phase

| Phase | Sprints | Focus | Issues Fixed |
|-------|---------|-------|--------------|
| 1 | 24-26 | Critical/High Priority | 19 |
| 2 | 27-28 | Parser/Lexer | 11 |
| 3 | 29 | MIR Lowerer | 7 |
| 4 | 30 | Standard Library | 8 |
| 5 | 31 | Tooling/CLI | 8 |
| 6 | 32-34 | Memory Safety | 14+ |
| 7 | 35 | Verification | N/A |
| **TOTAL** | **12 sprints** | **Complete** | **67+** |

---

## Issue Tracking

### Critical (1)
- [ ] User-defined enum type substitution (Sprint 26)

### High Priority (8)
- [ ] LLVM Rvalue handlers (Sprint 24)
- [ ] LLVM Terminator handlers (Sprint 24)
- [ ] Closure memory leak (Sprint 24)
- [ ] Shared Tokio runtime (Sprint 25)
- [ ] Spawned task errors (Sprint 25)
- [ ] await_any cleanup (Sprint 25)
- [ ] validate_args! gaps (Sprint 26)
- [ ] User-defined methods (Sprint 26)

### Medium Priority (30+)
- [ ] Type inference edge cases (Sprint 26)
- [ ] Parser features (Sprint 27)
- [ ] Lexer features (Sprint 28)
- [ ] MIR lowerer gaps (Sprint 29)
- [ ] Stdlib bugs (Sprint 30)
- [ ] FFI safety (Sprint 32)

### Low Priority (28+)
- [ ] CLI features (Sprint 31)
- [ ] REPL improvements (Sprint 31)
- [ ] Code quality (throughout)

---

## Dependencies

```
Sprint 24 (LLVM) ──────────────────────────────────────────┐
Sprint 25 (Async) ─────────────────────────────────────────┤
Sprint 26 (Types) ─────────────────────────────────────────┼──> Sprint 35 (Release)
Sprint 27 (Parser) ──> Sprint 28 (Lexer) ──────────────────┤
Sprint 29 (MIR) ───────────────────────────────────────────┤
Sprint 30 (Stdlib) ────────────────────────────────────────┤
Sprint 31 (CLI) ───────────────────────────────────────────┤
Sprint 32 (FFI) ──> Sprint 33 (Linear) ──> Sprint 34 (Cap) ┘
```

Sprints 24-31 can largely run in parallel.
Sprints 32-34 (Memory Safety) are sequential.
Sprint 35 requires all others complete.

---

## Estimated Effort

| Phase | Complexity | Est. Time |
|-------|------------|-----------|
| Phase 1 | High | 3-4 days |
| Phase 2 | Medium | 2 days |
| Phase 3 | Medium | 1-2 days |
| Phase 4 | Low | 1 day |
| Phase 5 | Low | 1 day |
| Phase 6 | Very High | 5-7 days |
| Phase 7 | Medium | 2 days |
| **TOTAL** | | **15-19 days** |

---

## Success Criteria

1. **All 67+ issues resolved** - No known bugs in issue tracker
2. **All tests pass** - Unit, integration, and property tests
3. **All examples compile and run** - With both interpreter and LLVM
4. **Memory safety enforced** - Linear types prevent leaks/use-after-free
5. **FFI is safe** - Capability-gated, bounds-checked
6. **Documentation complete** - All features documented

---

*"Do it right, or don't do it at all."*
