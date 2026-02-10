# Sprint 50.1: MIR Copy Propagation Soundness Hotfix

## Goal

Fix the semantic unsoundness in MIR copy propagation introduced in Sprint 50 by enforcing reassignment and control-flow safety. Preserve optimization benefits while guaranteeing behavior equivalence with `--no-optimize`.

---

## Why This Sprint Exists

Current copy propagation in `src/mir/optimize.rs` builds a global substitution map and rewrites uses without proving safety across:

1. source local reassignments, and
2. control-flow joins/branches.

This can rewrite a use to a newer value than the original copied value, producing incorrect runtime behavior.

---

## Scope

### 50.1.1 Make Copy Propagation Sound by Construction (P0)

**Files:** `src/mir/optimize.rs`

Replace current global substitution strategy with a conservative, semantics-safe approach.

### Required implementation constraints

Implement **block-local forward propagation only** for Phase 1:

1. Only propagate from `Assign(dest, Use(Copy(src)|Local(src)|Move(src)))`.
2. `dest` must be a compiler temp (`LocalDecl.name == None`).
3. Propagation candidates are valid **only within the same basic block**.
4. Stop propagation when either `src` or `dest` is assigned again.
5. Do not propagate through `Ref`, `Discriminant`, `EnumField`, or contract-sensitive contexts.
6. Do not propagate into/through terminators if safety cannot be proven for block-local ordering.
7. Never create/keep cyclic substitutions.

This is intentionally conservative. Correctness is mandatory; aggressiveness can return in Sprint 51 with data-flow analysis.

### Acceptance criteria

1. No behavior divergence between optimized and non-optimized execution on reassignment/control-flow edge cases.
2. Copy propagation still optimizes trivial safe chains inside a block.
3. Existing optimizer tests pass.

---

### 50.1.2 Add Regression Tests for the Bug Class (P0)

**Files:** `src/mir/optimize.rs` (test module), `tests/cli_tests.rs`, optionally `tests/forma/test_optimization.forma`

Add targeted tests that failed conceptually under old logic:

1. **Reassignment barrier test (unit):**
   - copy temp from `x`,
   - reassign `x`,
   - ensure return/use still points to copied temp semantics.
2. **Branch/join test (unit):**
   - copy in one block,
   - reassign source in successor,
   - verify no illegal substitution through join.
3. **CLI equivalence regression (integration):**
   - run fixture with and without `--no-optimize`,
   - assert identical stdout and exit code.

### Acceptance criteria

1. At least 3 new tests directly covering the unsoundness class.
2. New tests fail on old algorithm and pass with fix.

---

### 50.1.3 Validate Optimizer Invariants Post-Pass (P1)

**Files:** `src/mir/optimize.rs`

After each optimization round (or in debug/test builds), run `validate_mir()` and fail fast in tests if invariants break.

### Acceptance criteria

1. Optimization tests assert `validate_mir(program).is_empty()`.
2. Any malformed MIR from pass bugs is caught immediately.

---

### 50.1.4 Sprint-Report Accuracy Cleanup (P2)

**Files:** sprint notes/docs only as needed

Clarify .forma suite status from this environment:
- `test_contract_errors.forma` is intentional negative.
- `test_tcp.forma` may be environment-dependent and not introduced by Sprint 50.

Do not block merge on infra-dependent networking tests unless regression is proven against `--no-optimize`.

### Acceptance criteria

1. Sprint summary language is precise and non-misleading.

---

## Implementation Notes

Recommended minimal algorithm (safe):

1. For each basic block, scan statements top-to-bottom.
2. Maintain a local map `dest -> src` valid for current scan window.
3. On assignment to any local `l`, remove mappings where `dest == l` or `src == l`.
4. Rewrite operands only using currently valid mappings.
5. Do not carry mapping across block boundaries.

This gives deterministic safety without requiring dominance/use-def infrastructure.

---

## Verification Plan

```bash
# Core
cargo fmt --all -- --check
cargo clippy --all-targets -- -D warnings
cargo test --all

# Targeted optimizer tests
cargo test mir::optimize::tests -- --nocapture

# Optimization equivalence check
./target/release/forma run --allow-all tests/forma/test_optimization.forma > /tmp/opt_on.out
./target/release/forma run --allow-all --no-optimize tests/forma/test_optimization.forma > /tmp/opt_off.out
diff -u /tmp/opt_off.out /tmp/opt_on.out
```

Optional reproduction fixture (for manual validation):

```forma
f main() -> Int
    x := 1
    z := if true then x else 0
    x := 2
    if z == 1 then 0 else 99
```

Expected exit code is identical with and without optimization.

---

## Out of Scope

1. Global data-flow copy propagation across blocks.
2. Dominator tree construction.
3. Dead store elimination.
4. CSE/inlining/loop opts.

---

## Definition of Done

1. Copy propagation no longer performs unsound cross-assignment or cross-flow rewrites.
2. New regression tests for reassignment + branch/join cases are present and passing.
3. `cargo test --all` passes.
4. CLI output/exit equivalence for optimization fixture remains identical (`opt` vs `--no-optimize`).
5. MIR invariants are validated after optimization in tests.

