# Sprint 39: Test Coverage Closure

**Goal:** Close all currently identified test-suite blind spots before public launch.
**Estimated Effort:** 3-5 days

---

## Context

Deep review found that large portions of FORMA behavior are either untested or not enforced in CI:

1. `tests/forma/*.forma` is not part of CI.
2. `.forma` test pass/fail exit conventions are inconsistent with CLI semantics.
3. Option/Result-first error handling lacks negative-path coverage.
4. Capability enforcement tests do not cover every gated operation.
5. Runtime crate coverage is thin and mostly excluded from root validation flow.
6. Parser/type negative-path tests are sparse relative to implementation size.
7. Formatter/LSP/error-reporting/LLVM behavior lacks targeted regression tests.

This sprint is test and quality hardening only. No product feature work.

---

## Agent Instructions

1. Treat this sprint as a launch gate, not a refactor sprint.
2. Prefer deterministic tests (no flaky network dependence, no long sleeps).
3. Every new behavior test must include at least one failure-path assertion where applicable.
4. Keep runtime and interpreter safety tests explicit about expected error text and exit behavior.
5. Do not use broad `#[ignore]` or warning suppressions to make tests pass.

---

## Task 39.1: Make `.forma` Integration Tests First-Class and Deterministic

**Priority:** P0

**Files (likely):**
- `.github/workflows/ci.yml`
- `scripts/verify_v1.sh`
- `tests/forma/*.forma` (for exit convention normalization)
- optional test harness under `tests/` (Rust integration test runner)

### Requirements

1. Add CI coverage for `tests/forma/*.forma` (not just Rust unit/integration tests).
2. Normalize pass/fail contract for `.forma` tests to align with CLI exit semantics:
- `0` = pass
- non-zero = fail
3. Remove mixed return conventions in existing test files.
4. Ensure local verification script and CI enforce the same contract.

### Acceptance Criteria

1. CI fails if any `.forma` integration test fails.
2. All `.forma` tests use a single exit convention.
3. `scripts/verify_v1.sh` and CI produce consistent results.

---

## Task 39.2: Exhaustive Option/Result Error-Path Testing

**Priority:** P0

**Files (likely):**
- `tests/forma/test_error_handling.forma`
- `src/mir/interp.rs` test module
- optional dedicated Rust integration test file for runtime-error assertions

### Requirements

1. Add negative tests for:
- `unwrap(None)`, `unwrap(Err(...))`
- `expect(None, msg)`, `expect(Err(...), msg)`
- wrong-type calls (`unwrap(42)`, `is_some(42)`, etc.)
2. Verify both error propagation shape and message quality.
3. Keep existing happy-path coverage.

### Acceptance Criteria

1. Option/Result helper builtins have explicit happy + failure-path tests.
2. Runtime errors are asserted (not just printed).

---

## Task 39.3: Complete Capability Enforcement Test Matrix

**Priority:** P0

**Files (likely):**
- `src/mir/interp.rs` test module

### Requirements

1. Expand capability-denial tests to include all currently gated operations:
- `db_open`
- `http_post_json`
- `http_serve`
- `tcp_connect`
- `tcp_listen`
- `tls_connect`
- `udp_bind`
2. Keep existing file/network/exec denial tests.
3. Add positive-path checks proving grants unblock operations (without requiring brittle external network success).

### Acceptance Criteria

1. Every `require_capability(...)` site has a matching denial test.
2. At least one allow-path test exists per capability class (`read`, `write`, `network`, `exec`, `all`).

---

## Task 39.4: Runtime Crate Coverage and CI Inclusion

**Priority:** P1

**Files (likely):**
- `runtime/src/map.rs`
- `runtime/src/vec.rs`
- `runtime/src/memory.rs`
- `runtime/src/io.rs`
- `runtime/src/env.rs`
- `runtime/src/time.rs`
- `runtime/src/math.rs`
- `runtime/src/panic.rs`
- `.github/workflows/ci.yml`

### Requirements

1. Add unit tests for currently untested runtime modules, focusing on:
- boundary conditions
- null-pointer handling
- allocation/deallocation correctness
- conversion/roundtrip safety
2. Ensure runtime crate tests run in CI (not only manually via `cd runtime`).
3. Keep tests deterministic and fast.

### Acceptance Criteria

1. Runtime modules above have direct tests.
2. CI executes runtime test suite every PR.

---

## Task 39.5: Parser and Type-System Negative-Path Expansion

**Priority:** P1

**Files (likely):**
- `tests/parser_tests.rs`
- `tests/type_tests.rs`
- optional targeted tests in `src/types/inference.rs` module tests

### Requirements

1. Add parser rejection tests for malformed constructs across:
- patterns
- imports/use trees
- generics/type syntax
- assignment/cast/try/coalesce edge syntax
2. Add type-checker/inference negative tests for:
- coercion failures
- mismatched generics
- invalid method/trait resolution cases
- never/option/result mismatch edge cases
3. Assert expected diagnostic class/message where stable.

### Acceptance Criteria

1. Parser negative tests are no longer single-case.
2. Type-system negative tests cover multiple defect classes, not just unifier basics.

---

## Task 39.6: Add Direct Tests for Currently Untested Subsystems

**Priority:** P1

**Files (likely):**
- `src/fmt/mod.rs` (or new `tests/fmt_tests.rs`)
- `src/errors/diagnostic.rs` / `src/errors/report.rs` (or new tests)
- `src/lsp/mod.rs` (or new LSP tests)
- `src/codegen/llvm.rs` (feature-gated tests)

### Requirements

1. Formatter:
- add idempotence and representative formatting regression tests.
2. Error reporting:
- add tests for diagnostic/report formatting and structured fields.
3. LSP:
- add request/response smoke tests for core paths (diagnostics/completion/hover minimum).
4. LLVM:
- add feature-gated behavior tests beyond compile/lint (small MIR->LLVM sanity scenarios).

### Acceptance Criteria

1. Each subsystem above has at least one direct regression test.
2. LLVM behavior tests run when `--features llvm` is enabled.

---

## Verification Checklist

Run and include outcomes in PR summary:

1. `cargo test`
2. `cargo clippy --all-targets -- -D warnings`
3. `cargo clippy --all-features --all-targets -- -D warnings`
4. `cargo check --features llvm`
5. `cargo test --features llvm --no-run`
6. `cd runtime && cargo test`
7. Full `.forma` integration test run (CI-equivalent command) with deterministic pass/fail summary

---

## Out of Scope

1. New language features.
2. Runtime/API redesign.
3. Benchmark/performance optimization not tied to correctness.

---

## Definition of Done

1. All identified coverage gaps are closed or explicitly deferred with justification.
2. `.forma` integration tests are CI-gated with a single pass/fail contract.
3. Option/Result and capability systems have full failure-path regression coverage.
4. Runtime crate and previously untested subsystems have direct tests in CI.
