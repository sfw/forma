# Sprint 45: Coverage + CLI Hardening

**Goal:** Close remaining non-LSP launch gaps in coverage enforcement and CLI error-contract testing.
**Estimated Effort:** 2-4 days

---

## Context

Sprint 44 is already focused on LSP server fixes. This sprint covers the highest-value **non-LSP** gaps still present:

1. Builtin coverage audit is currently informational and not strict enough to prevent regressions.
2. CLI JSON failure behavior is not fully matrix-tested across `run/check/build` error classes.
3. `check --partial` behavior on module/import failures is not explicitly regression-tested.
4. Long-tail builtin surface still has many untested or lightly-tested operations (especially network/time/env helpers).

---

## Agent Instructions

1. Do not touch LSP server implementation in this sprint.
2. Treat this as a launch-gate hardening sprint; prioritize deterministic tests and CI signal quality.
3. Prefer behavior-preserving tests and tooling improvements over refactors.
4. Make CI failures actionable with clear messages and thresholds.

---

## Task 45.1: Make Builtin Coverage Audit Accurate

**Priority:** P0

**Files (likely):**
- `scripts/builtin_coverage.sh`
- optional helper script(s) under `scripts/`

### Requirements

1. Improve builtin detection and test-reference detection so results are meaningful:
- robust builtin extraction from `src/mir/interp.rs`
- include references from Rust tests and `.forma` integration tests
- avoid false positives/false negatives from simplistic `call_builtin(...)`-only matching
2. Output structured summary with:
- total builtins
- tested builtins
- untested builtins
- coverage %
- high-risk untested list (network, env, unsafe, file/db)

### Acceptance Criteria

1. Running `bash scripts/builtin_coverage.sh` gives stable, reproducible metrics.
2. Script output clearly identifies risky untested builtins.

---

## Task 45.2: Enforce Coverage Audit in CI

**Priority:** P0

**Files (likely):**
- `.github/workflows/ci.yml`

### Requirements

1. Convert coverage-audit job from informational-only to enforcement mode (or add a blocking variant).
2. Add explicit regression thresholds, e.g.:
- no increase in untested risky builtin count
- no decrease in total coverage % below baseline
3. Keep report artifact upload for debugging.

### Acceptance Criteria

1. CI fails when coverage thresholds regress.
2. CI output includes clear reason for failure and offending builtin set.

---

## Task 45.3: CLI JSON Failure Matrix Tests (`run/check/build`)

**Priority:** P0

**Files (likely):**
- `tests/cli_tests.rs`
- `tests/fixtures/*`

### Requirements

1. Add explicit tests for JSON-mode failure outputs across commands and error classes:
- lexer error
- parser error
- module/import error
- type error
- build-only late failure class (codegen/link, where deterministic in CI)
2. For each case assert:
- non-zero exit status
- JSON payload present
- `success: false`
- `errors` non-empty
- expected `code` category present

### Acceptance Criteria

1. Missing JSON payload on failure becomes a test failure.
2. `run/check/build` JSON error contracts are consistently enforced.

---

## Task 45.4: `check --partial` Import-Error Contract

**Priority:** P1

**Files (likely):**
- `tests/cli_tests.rs`
- `src/main.rs` (only if behavior mismatch is found)

### Requirements

1. Define intended behavior for `check --partial` when imports fail.
2. Add regression tests that assert this behavior in both human and JSON modes.
3. Ensure behavior is documented in test names/messages.

### Acceptance Criteria

1. `check --partial` import-error behavior is deterministic and test-covered.
2. No silent success on unresolved imports.

---

## Task 45.5: Long-Tail Builtin Coverage Wave 2 (Non-LSP)

**Priority:** P1

**Files (likely):**
- `tests/forma/*.forma`
- interpreter unit tests in `src/mir/interp.rs`

### Requirements

1. Add targeted tests for high-risk long-tail builtins (non-LSP):
- network helpers (`tcp_*`, `udp_*`, `tls_*`, `http_*` read/write/metadata paths)
- env/time helpers (`env_*`, `time_*`, `duration_*`, `timeout`)
- selected file/db helpers still lacking explicit behavioral tests
2. Focus on both happy-path and failure-path assertions.
3. Keep tests CI-friendly and deterministic (use loopback/local fixtures only).

### Acceptance Criteria

1. Reduce untested builtin count meaningfully from current baseline.
2. Risk-class untested builtins reduced with concrete before/after metrics.

---

## Verification Checklist

Run and include results in PR summary:

1. `cargo build --release`
2. `cargo test --all`
3. `cargo clippy --all-targets -- -D warnings`
4. `cargo clippy --all-features --all-targets -- -D warnings`
5. `cargo fmt --all -- --check`
6. `bash scripts/builtin_coverage.sh`
7. Full `.forma` integration loop used by CI
8. `git status --short` and `git log --oneline -n 1`

---

## Out of Scope

1. LSP server implementation changes (covered by Sprint 44).
2. New language features.
3. Major architecture refactors.

---

## Definition of Done

1. Coverage audit is accurate and CI-enforced.
2. CLI JSON failure matrix tests cover `run/check/build` failure classes.
3. `check --partial` import-error behavior is locked by tests.
4. Long-tail high-risk builtin coverage is materially improved.

---

## Coding Agent Prompt

```text
Implement Sprint 45: Coverage + CLI Hardening (non-LSP scope only).

Do not change LSP server code; Sprint 44 owns that.

Execute tasks in order:
1) Make scripts/builtin_coverage.sh accurate for real test references (Rust + .forma)
2) Make coverage audit CI-enforced with regression thresholds
3) Add CLI JSON failure matrix tests for run/check/build (lex/parse/module/type + deterministic build-late failure)
4) Add explicit check --partial import-error contract tests
5) Add long-tail builtin coverage wave 2 for high-risk non-LSP builtins

Constraints:
- Deterministic tests only.
- No weakening existing tests.
- Keep human and JSON output contracts consistent.

Required verification:
- cargo build --release
- cargo test --all
- cargo clippy --all-targets -- -D warnings
- cargo clippy --all-features --all-targets -- -D warnings
- cargo fmt --all -- --check
- bash scripts/builtin_coverage.sh
- CI-equivalent .forma test loop
- git status --short
- git log --oneline -n 1

Deliverables:
- code + test updates
- CI changes
- before/after coverage metrics
- concise summary with exact counts and commit hash
```
