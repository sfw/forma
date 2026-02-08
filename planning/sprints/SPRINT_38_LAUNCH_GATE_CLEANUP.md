# Sprint 38: Launch-Gate Cleanup

**Goal:** Resolve the remaining launch-gate issues found in post-Sprint-37 verification.
**Estimated Effort:** 0.5-1 day

---

## Context

Sprint 37 improved LLVM stability and prerequisite reproducibility, but launch-gate verification found three gaps:

1. `cargo clippy --all-features --all-targets -- -D warnings` fails with LLVM-only lint errors in `src/codegen/llvm.rs`.
2. CI clippy does not run with `--all-features`, so LLVM lint regressions are not blocked.
3. macOS LLVM setup docs hardcode an Apple Silicon Homebrew path and are not portable to Intel macOS.

This sprint is a quality-gate closure sprint. No feature work.

---

## Agent Instructions

1. Keep fixes minimal and behavior-preserving.
2. Prefer code cleanup over warning suppression (`#[allow(...)]` is disallowed unless explicitly justified in PR notes).
3. Keep CI changes scoped to missing coverage.
4. Keep docs concise, copy-pasteable, and architecture-agnostic where possible.
5. Do not touch unrelated runtime/language behavior.

---

## Task 38.1: Fix LLVM All-Features Clippy Failures

**Priority:** P1

**Files (likely):**
- `src/codegen/llvm.rs`

### Requirements

1. Eliminate current all-features clippy failures in LLVM codegen, including:
- `for_kv_map` loops that should use `.values()`
- `unnecessary_map_or` usage that should use `.is_some_and(...)`
- `useless_conversion` `.into()` calls on same-type values
- collapsible nested `if let` chains
2. Preserve generated behavior and existing test outcomes.
3. Do not introduce broad lint suppressions.

### Acceptance Criteria

1. `cargo clippy --all-features --all-targets -- -D warnings` passes.
2. `cargo check --features llvm` still passes.
3. `cargo test --features llvm --no-run` still passes.

---

## Task 38.2: Enforce LLVM Lint Coverage in CI

**Priority:** P1

**Files (likely):**
- `.github/workflows/ci.yml`

### Requirements

1. Ensure CI enforces LLVM-path lint quality, not only default-feature lint.
2. Add one of the following (prefer minimal runtime impact):
- Update existing clippy job to run `--all-features`, or
- Add a dedicated LLVM clippy job with required system deps installed.
3. Keep existing CI jobs functional and readable.

### Acceptance Criteria

1. CI contains an explicit clippy gate covering LLVM feature code paths.
2. CI fails when LLVM-only clippy regressions are introduced.

---

## Task 38.3: Make macOS LLVM Docs Portable (Intel + Apple Silicon)

**Priority:** P2

**Files (likely):**
- `INSTALL.md`
- optional: `README.md` if LLVM setup is duplicated there

### Requirements

1. Replace hardcoded Homebrew library path guidance with dynamic path detection.
2. Keep instructions valid on:
- Apple Silicon (`/opt/homebrew`)
- Intel macOS (`/usr/local`)
3. Include a short verification command snippet users can run after setup.

### Acceptance Criteria

1. macOS setup instructions no longer assume a single Homebrew prefix.
2. Docs remain straightforward and copy-pasteable.

---

## Verification Checklist

Run and include results in PR summary:

1. `cargo test`
2. `cargo clippy --all-targets -- -D warnings`
3. `cargo clippy --all-features --all-targets -- -D warnings`
4. `cargo check --features llvm`
5. `cargo test --features llvm --no-run`

---

## Out of Scope

1. New LLVM backend features.
2. Capability model changes.
3. Broad CI redesign beyond missing launch-gate coverage.

---

## Definition of Done

1. Launch-gate findings from verification are fully closed.
2. LLVM code paths are lint-clean under all-features clippy.
3. CI enforces LLVM lint quality.
4. macOS LLVM docs work for both Intel and Apple Silicon users.
