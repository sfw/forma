# Sprint 40: CI + Coverage Completion

**Goal:** Close remaining Sprint 39 gaps and make `forma-tests` CI reliably green.
**Estimated Effort:** 2-4 days

---

## Context

Sprint 39 made major progress, but launch-gate verification still found blocking gaps:

1. New `forma-tests` CI job fails with 5 test files:
- `test_async.forma`
- `test_closures.forma`
- `test_contextual_keywords.forma`
- `test_datetime.forma`
- `test_imports.forma`
2. Option/Result negative builtin paths are still not explicitly tested (`unwrap(None)`, `unwrap(Err)`, `expect(None/Err)`, wrong-type inputs).
3. Runtime modules `io/env/panic` still have no direct tests.
4. LSP and LLVM behavior tests are still missing (only compile/lint coverage exists for LLVM).

This sprint is closure work for correctness and CI reliability before launch.

---

## Agent Instructions

1. Treat red CI as highest priority; fix behavior before adding new optional tests.
2. Prefer implementation fixes over weakening tests unless behavior changed intentionally.
3. Keep tests deterministic and CI-safe (no external network dependency, no flaky timing assumptions).
4. For panic-path runtime tests, use subprocess-based assertions so test process survival is guaranteed.
5. If a failing `.forma` test is invalid for current language semantics, update the test and document why.

---

## Task 40.1: Unblock `forma-tests` CI (5 failing `.forma` tests)

**Priority:** P0

**Files (likely):**
- `tests/forma/test_async.forma`
- `tests/forma/test_closures.forma`
- `tests/forma/test_contextual_keywords.forma`
- `tests/forma/test_datetime.forma`
- `tests/forma/test_imports.forma`
- relevant implementation files in `src/` if behavior bugs are root cause

### Requirements

1. Root-cause each of the 5 failures and fix in implementation and/or tests.
2. Keep test intent intact:
- async behavior still validated
- closures/contextual keyword behavior still validated
- datetime arithmetic still validated
- imports still validated
3. Ensure each updated test uses exit `0` on success and non-zero on failure.

### Acceptance Criteria

1. Local run of CI-equivalent loop reports zero fails for non-skipped `.forma` tests.
2. GitHub `forma-tests` job passes.

---

## Task 40.2: Complete Option/Result Negative Builtin Coverage

**Priority:** P0

**Files (likely):**
- `src/mir/interp.rs` test module
- optional additions in `tests/forma/test_error_handling.forma`

### Requirements

1. Add explicit tests for failure/error branches:
- `unwrap(None)`
- `unwrap(Err("..."))`
- `expect(None, "msg")`
- `expect(Err("..."), "msg")`
- wrong-type inputs (for `unwrap`, `expect`, `is_some`, `is_none`, `is_ok`, `is_err`)
2. Assert error shape/message content, not just `is_err`.
3. Preserve existing happy-path coverage.

### Acceptance Criteria

1. All Option/Result helper branches in interpreter are exercised by tests.
2. Regression tests fail if error messages or behavior drift.

---

## Task 40.3: Add Runtime Tests for `io/env/panic`

**Priority:** P1

**Files (likely):**
- `runtime/src/io.rs`
- `runtime/src/env.rs`
- `runtime/src/panic.rs`

### Requirements

1. Add direct tests for `io` and `env` module behavior and null-safety.
2. Add `panic` module tests using subprocess/child-process strategy for functions that terminate (`process::exit`).
3. Keep tests portable across CI Linux/macOS runners where possible.

### Acceptance Criteria

1. `runtime` crate has direct tests in `io`, `env`, and `panic`.
2. `cd runtime && cargo test` includes and passes these new tests.

---

## Task 40.4: Add Direct LSP and LLVM Behavior Tests

**Priority:** P1

**Files (likely):**
- `src/lsp/mod.rs` or new `tests/lsp_tests.rs`
- `src/codegen/llvm.rs` or new feature-gated LLVM test file

### Requirements

1. LSP:
- add smoke tests for core behaviors (diagnostics and at least one of completion/hover).
2. LLVM:
- add feature-gated behavior tests beyond compile/lint (small MIR->LLVM sanity checks).
3. Keep tests lightweight and deterministic.

### Acceptance Criteria

1. LSP has at least one direct regression test.
2. LLVM has at least one direct feature-gated behavior test.

---

## Task 40.5: CI Alignment and Final Gate

**Priority:** P1

**Files (likely):**
- `.github/workflows/ci.yml`
- optional helper scripts if needed

### Requirements

1. Ensure CI executes all intended gates added in Sprint 39/40 without false failures.
2. Keep skip list explicit and documented for truly external-dependency `.forma` tests.
3. Ensure `forma-tests` output is easy to diagnose when failures occur.

### Acceptance Criteria

1. All CI jobs pass on GitHub for this sprint branch.
2. No mismatch between local and CI commands for `.forma` validation.

---

## Verification Checklist

Run and include outcomes in PR summary:

1. `cargo test --all`
2. `cargo clippy --all-targets -- -D warnings`
3. `cargo clippy --all-features --all-targets -- -D warnings`
4. `cargo fmt --all -- --check`
5. `cd runtime && cargo test`
6. CI-equivalent `.forma` loop (same skip list as workflow) with `PASS/FAIL/SKIP` counts
7. `cargo check --features llvm`
8. `cargo test --features llvm --no-run`

---

## Out of Scope

1. New language/runtime features unrelated to test closure.
2. Broad refactors of parser/typechecker/interpreter internals.
3. Non-deterministic network integration expansion.

---

## Definition of Done

1. `forma-tests` CI job is green.
2. Remaining Sprint 39 gaps are fully closed.
3. Option/Result negative paths, runtime `io/env/panic`, and LSP/LLVM behavior each have direct regression tests.
4. Launch-gate validation is fully passable in CI.
