# Sprint 41: Launch Risk Remediation

**Goal:** Close all currently identified launch risks across security, correctness, operability, and coverage.
**Estimated Effort:** 4-7 days

---

## Context

Recent deep-dive findings identified launch-blocking gaps:

1. Capability model bypass for environment-variable access (`env_get`, `env_vars`) and unsafe pointer/FFI operations.
2. Unchecked `Int -> usize` conversions enabling oversized allocations (DoS risk) in TCP/UDP/TLS/HTTP/FFI paths.
3. `--check-contracts` CLI flag is parsed but functionally ignored.
4. CI-equivalent `.forma` integration suite still has 5 failures.
5. Imported-module diagnostics can report invalid line numbers.
6. `http_serve` has an unbounded blocking loop with no lifecycle controls.
7. Significant test blind spots remain (77 builtins with zero direct references; no direct tests for some runtime/LSP/LLVM paths).

This sprint is launch hardening only. No unrelated feature work.

---

## Agent Instructions

1. Execute tasks in priority order; do not start P1/P2 until P0 is green.
2. Preserve default-safe behavior: deny risky operations unless explicitly granted.
3. Avoid weakening tests to get green; fix implementation first.
4. Add regression tests for every discovered bug path before marking a task complete.
5. Keep CI deterministic and platform-portable.

---

## Task 41.1: Capability Boundary Hardening

**Priority:** P0

**Files (likely):**
- `src/mir/interp.rs`
- `src/main.rs`
- `tests/*` capability test files
- docs (`README.md`, `INSTALL.md` if flags/capabilities are user-facing)

### Requirements

1. Gate env operations:
- `env_get`, `env_vars` require explicit capability.
- `env_set`, `env_remove` require explicit capability.
2. Gate unsafe pointer/FFI operations behind explicit capability:
- `ptr_*`, `str_to_cstr`, `cstr_*`, `alloc`, `alloc_zeroed`, `dealloc`, `mem_copy`, `mem_set`.
3. Decide and implement capability taxonomy explicitly:
- either add new capabilities (`env`, `unsafe`) and CLI flags, or map to existing capabilities with clear rationale.
4. Update capability mapping comments/docs to prevent drift.
5. Add denial + allow-path tests for every newly gated builtin class.

### Acceptance Criteria

1. Sensitive env and unsafe builtins are inaccessible without explicit grants.
2. Every new `require_capability(...)` site has at least one denial test.
3. Capability docs and CLI help match runtime behavior.

---

## Task 41.2: Size Validation and Allocation Guardrails

**Priority:** P0

**Files (likely):**
- `src/mir/interp.rs`
- related tests in `tests/` and interpreter unit tests

### Requirements

1. Replace unchecked `as usize` conversions on user-supplied sizes with validated conversion helpers.
2. Reject negative sizes with clear runtime errors.
3. Add upper bounds for network/body/buffer allocations (document constants and rationale).
4. Apply guards to at least:
- `tcp_read`, `tcp_read_exact`
- `udp_recv`, `udp_recv_from`
- `tls_read`
- `http_serve` request body allocation (`Content-Length`)
- unsafe memory builtins (`alloc`, `dealloc`, `mem_copy`, `mem_set`, `cstr_to_str_len`)
5. Add boundary tests: negative, zero, max-allowed, and overflow-ish values.

### Acceptance Criteria

1. No user-controlled buffer allocation path performs unchecked signed-to-size conversion.
2. Oversized requests return structured errors, not panic/OOM behavior.

---

## Task 41.3: Fix Contract Flag Semantics

**Priority:** P0

**Files (likely):**
- `src/main.rs`
- `src/mir/interp.rs`
- CLI/integration tests

### Requirements

1. Wire `--check-contracts` to runtime behavior.
2. Define desired default explicitly and enforce it consistently:
- Option A: contracts off by default, enabled by flag.
- Option B: contracts on by default; replace flag with `--no-check-contracts`.
3. Remove dead/unused parameter patterns (`_check_contracts`) and keep behavior test-covered.

### Acceptance Criteria

1. CLI flag state changes runtime contract enforcement observably.
2. Tests assert both enabled and disabled behavior.

---

## Task 41.4: Unblock `.forma` CI Failures (5 tests)

**Priority:** P0

**Files (likely):**
- `tests/forma/test_async.forma`
- `tests/forma/test_closures.forma`
- `tests/forma/test_contextual_keywords.forma`
- `tests/forma/test_datetime.forma`
- `tests/forma/test_imports.forma`
- corresponding implementation in `src/types/inference.rs`, `src/mir/interp.rs`, parser/type modules as needed

### Requirements

1. Resolve root causes rather than masking failures:
- async type naming/contract consistency (`Future` vs runtime type strings)
- closure inference regressions
- datetime duration unit consistency (`duration_*` vs `time_add/time_sub`)
- imports type mismatch and associated diagnostics correctness
2. Keep test semantics aligned with language design; update tests only when behavior was intentionally changed.
3. Ensure `.forma` tests use correct exit convention (`0` success).

### Acceptance Criteria

1. CI-equivalent `.forma` loop reports zero failures (excluding explicit external-dependency skips).
2. GitHub `forma-tests` job passes.

---

## Task 41.5: Imported-Module Diagnostic Accuracy

**Priority:** P1

**Files (likely):**
- `src/main.rs`
- parser/type/loader error-span plumbing
- `src/errors/*`
- diagnostic tests

### Requirements

1. Ensure error spans map to the correct file and line for imported/transitive modules.
2. Fix JSON and human output consistency for multi-file errors.
3. Add regression tests where reported line numbers must be within file bounds.

### Acceptance Criteria

1. Reported diagnostic file/line/column always correspond to the actual source file.
2. Regression test fails if line mapping drifts again.

---

## Task 41.6: `http_serve` Lifecycle + Resource Controls

**Priority:** P1

**Files (likely):**
- `src/mir/interp.rs`
- `tests/forma/test_http_server.forma` and/or Rust integration tests

### Requirements

1. Add bounded lifecycle behavior for server execution (testable stop condition):
- e.g., max requests, explicit shutdown signal, or timeout-based termination.
2. Preserve existing simple usage ergonomics.
3. Enforce request body limits and fail with structured errors.

### Acceptance Criteria

1. Server behavior is testable without external manual interruption.
2. Large-body requests are handled safely and predictably.

---

## Task 41.7: Coverage Closure for Untested Surfaces

**Priority:** P1

**Files (likely):**
- `tests/` (new/expanded suites)
- `runtime/src/io.rs`
- `runtime/src/env.rs`
- `runtime/src/panic.rs`
- `src/lsp/mod.rs`
- LLVM tests under feature-gated test files
- `.github/workflows/ci.yml`

### Requirements

1. Reduce builtin blind spots by adding tests for currently unreferenced risky builtins first:
- env
- network read/write helpers
- memory/FFI helpers
2. Add direct tests for runtime modules currently untested (`io`, `env`, `panic`).
3. Add direct LSP smoke tests and at least one feature-gated LLVM behavior test.
4. Add a lightweight coverage audit script/check that reports builtin reference gaps and fails if risky classes regress to untested.

### Acceptance Criteria

1. Risk-class builtins have direct regression coverage.
2. Runtime/LSP/LLVM targeted tests run in CI.
3. Coverage-audit signal is part of launch gate.

---

## Verification Checklist

Run and include results in PR summary:

1. `cargo test --all`
2. `cargo clippy --all-targets -- -D warnings`
3. `cargo clippy --all-features --all-targets -- -D warnings`
4. `cargo check --features llvm`
5. `cargo test --features llvm --no-run`
6. `cd runtime && cargo test`
7. CI-equivalent `.forma` loop used in `.github/workflows/ci.yml`

---

## Out of Scope

1. New language features unrelated to risk remediation.
2. Performance optimization not tied to a risk finding.
3. Major architecture rewrites.

---

## Definition of Done

1. P0 tasks complete and verified.
2. `.forma` CI suite green for all non-skipped tests.
3. Security-sensitive builtins are explicitly capability-gated.
4. DoS-prone size conversions are validated and bounded.
5. Contract flag semantics are correct and tested.
6. Diagnostic mapping for imports is accurate.
7. Launch gate verification passes end-to-end.

---

## Coding Agent Prompt

Use this prompt with a coding agent to execute Sprint 41:

```text
Implement Sprint 41: Launch Risk Remediation in FORMA.

Primary objective: close all launch risks across security, correctness, operability, and coverage.

Execute tasks in this strict order:
1) Capability boundary hardening (env + unsafe pointer/FFI builtins)
2) Size validation/allocation guardrails
3) Fix --check-contracts flag semantics
4) Unblock 5 failing .forma CI tests (async, closures, contextual keywords, datetime, imports)
5) Imported-module diagnostic line/file accuracy
6) http_serve lifecycle/resource controls
7) Coverage closure for risky untested surfaces (runtime io/env/panic + LSP + LLVM + builtin-gap audit)

Constraints:
- Do not weaken tests to hide regressions.
- Add regression tests for every bug fixed.
- Keep behavior deterministic and CI-portable.
- Keep capability mapping and docs synchronized with runtime.

Required verification before finishing:
- cargo test --all
- cargo clippy --all-targets -- -D warnings
- cargo clippy --all-features --all-targets -- -D warnings
- cargo check --features llvm
- cargo test --features llvm --no-run
- cd runtime && cargo test
- CI-equivalent tests/forma loop from .github/workflows/ci.yml

Deliverables:
- code changes
- tests
- CI updates
- PR-style summary listing each task, files changed, and verification output
```
