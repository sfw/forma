# Sprint 37: LLVM Polish

**Goal:** Close the two remaining P2 launch-readiness gaps from Sprint 36 verification.
**Estimated Effort:** 0.5-1 day

---

## Context

Sprint 36 fixed all high-priority launch blockers. Two P2 items remain:

1. `cargo check --features llvm` compiles but still emits warnings in `src/codegen/llvm.rs` (deprecated `ptr_type`, unreachable match arms, dead field).
2. `cargo test --features llvm --no-run` is not deterministic across environments because LLVM+link prerequisites (notably `zstd`) are not documented/enforced clearly enough.

This sprint is a cleanup sprint for launch confidence, not a feature sprint.

---

## Agent Instructions

1. Keep changes minimal, targeted, and low-risk.
2. Do not change language/runtime behavior unless required to remove warning-triggering dead code.
3. Prefer deleting dead/unreachable code over adding `#[allow(...)]`.
4. Preserve existing CLI UX unless explicitly listed below.
5. Update docs and CI only where needed to make LLVM checks reproducible.

---

## Task 37.1: Zero-Warning LLVM Check

**Priority:** P2

**Files (likely):**
- `src/codegen/llvm.rs`

### Requirements

1. Remove deprecated inkwell pointer-type usage:
- Replace typed `.ptr_type(...)` calls with `Context::ptr_type(...)` style where applicable.
2. Remove unreachable fallback match arms currently triggering `unreachable_patterns` warnings.
3. Resolve dead field warning (`closure_env_types`) by removing the field and related dead initialization, or by wiring it into real usage if truly needed.
4. Do not introduce global warning suppressions.

### Acceptance Criteria

1. `cargo check --features llvm` completes with zero warnings from crate code.
2. `cargo clippy --all-targets -- -D warnings` still passes.

---

## Task 37.2: Deterministic LLVM Feature Validation (zstd + toolchain prereqs)

**Priority:** P2

**Files (likely):**
- `INSTALL.md`
- `README.md`
- `.github/workflows/ci.yml`
- optional lightweight script under `scripts/` if useful

### Requirements

1. Document LLVM-feature build/test prerequisites per platform, including `zstd` development libraries needed for linking in LLVM-feature test/build paths.
2. Ensure CI validates LLVM-feature compile health in a reproducible way:
- Install required system packages in a dedicated LLVM job.
- Run at least `cargo check --features llvm`.
- Prefer also running `cargo test --features llvm --no-run` if environment supports it reliably.
3. If you add a preflight script, keep it simple and actionable (clear missing dependency messages).

### Acceptance Criteria

1. A clean machine can follow docs to satisfy LLVM prereqs without guesswork.
2. CI contains an LLVM-specific validation job that enforces these prerequisites.
3. LLVM validation steps pass in CI after changes.

---

## Verification Checklist

Run and include results in the PR summary:

1. `cargo test`
2. `cargo clippy --all-targets -- -D warnings`
3. `cargo check --features llvm`
4. `cargo test --features llvm --no-run` (or clearly document any platform-specific exception and why)

---

## Out of Scope

1. Broad LLVM backend redesign.
2. New MIR/LLVM feature work beyond warning cleanup.
3. Capability/runtime changes unrelated to LLVM/toolchain validation.

---

## Definition of Done

1. No remaining known P2 items from Sprint 36 verification.
2. LLVM feature check path is warning-free and reproducible.
3. Docs + CI provide clear, deterministic guidance for LLVM prerequisites, including `zstd`.
