# Sprint 50: MIR Optimization Pass — Phase 1

## Goal

Add FORMA's first optimization pass over MIR, targeting the highest-impact low-hanging fruit. This sprint focuses on transformations that are safe, well-understood, and produce measurable speedups without requiring complex analysis infrastructure.

**Expected impact:** 15-30% interpreter speedup on typical programs (higher on branch-heavy MIR, lower on builtin-heavy programs).

---

## Background

FORMA currently lowers AST to MIR and either interprets it directly (`forma run`) or hands it to LLVM (`forma build`). There is no optimization between lowering and execution. The lowerer prioritizes correctness and simplicity, producing patterns with significant redundancy:

- Every intermediate value gets its own temporary local
- Constants are re-evaluated (and strings cloned) on every access
- Logical operators (`&&`, `||`) create 4 basic blocks each
- Match expressions create 3-4 blocks per arm
- Chains like `_1 = x; _2 = _1; return _2` are common
- Dead blocks accumulate from if/match lowering

### MIR Structure Reference

```
Program → HashMap<String, Function>
Function → params, locals, Vec<BasicBlock>, entry_block, contracts
BasicBlock → Vec<Statement> + Terminator
Statement → Assign(Local, Rvalue) | IndexAssign | Nop
Terminator → Return | Goto | If | Switch | Call | CallIndirect | ...
Rvalue → Use | BinaryOp | UnaryOp | Ref | Field | Index | ...
Operand → Constant(c) | Local(l) | Copy(l) | Move(l)
```

Key file: `src/mir/mod.rs` (types), `src/mir/lower.rs` (lowering), `src/mir/interp.rs` (execution)

---

## Scope

### 50.1 Optimization Infrastructure

**Priority:** P0
**Files:** `src/mir/optimize.rs` (new), `src/mir/mod.rs`

Create the optimization pass framework:

- New `src/mir/optimize.rs` module with `pub fn optimize(program: &mut Program)`
- Called between `lower()` and `interpret()`/`codegen()` in the pipeline
- Optimization enabled by default, disable with `--no-optimize` CLI flag
- Each optimization is a separate function for testability and ordering
- Pass ordering per round: constant fold → copy propagate → dead block eliminate → peephole
- Run rounds to a fixed point (or max 3 rounds) so one pass can unlock another
- Return per-pass `OptStats` (counts of transforms) for verification/debugging

**Why fixed-point:** copy propagation can expose new constant folds; branch simplification can create new dead blocks; one-shot ordering leaves speed on the table.

### Required Changes

- Add `optimize.rs` to `src/mir/mod.rs`
- Wire into `run()` and `build()` in `src/main.rs` after `lower()` call
- Add `--no-optimize` flag to clap CLI definition
- Add `optimized: bool` field to relevant config (or just check the CLI flag)
- Add a lightweight MIR validity check in tests/debug mode after optimization:
  - all referenced `BlockId`s are in-bounds
  - `entry_block` is in-bounds
  - every block has a terminator

### Acceptance Criteria

1. `optimize()` is called in the pipeline for `run` and `build` commands.
2. `--no-optimize` skips the pass.
3. All existing tests pass with optimization enabled (correctness preserved).
4. Running optimization twice is idempotent (second run reports zero changes).

---

### 50.2 Constant Folding

**Priority:** P0
**Files:** `src/mir/optimize.rs`

Evaluate constant expressions at compile time.

### Patterns to Fold

```
# Arithmetic on constants
Assign(_1, BinaryOp(Add, Constant(Int(1)), Constant(Int(2))))
→ Assign(_1, Use(Constant(Int(3))))

# Boolean operations
Assign(_1, BinaryOp(And, Constant(Bool(true)), Constant(Bool(false))))
→ Assign(_1, Use(Constant(Bool(false))))

# String concatenation of literals (optional, stretch)
# Unary operations
Assign(_1, UnaryOp(Neg, Constant(Int(5))))
→ Assign(_1, Use(Constant(Int(-5))))

# Comparison of constants
Assign(_1, BinaryOp(Lt, Constant(Int(1)), Constant(Int(2))))
→ Assign(_1, Use(Constant(Bool(true))))
```

### Implementation

Walk all statements in all blocks. For each `Assign(local, BinaryOp(op, left, right))` where both operands are `Constant`, evaluate the operation and replace with `Assign(local, Use(Constant(result)))`. Same for `UnaryOp`.

Handle: `Add`, `Sub`, `Mul`, `Div` (guard div-by-zero — leave unfolded), `Mod`, `Lt`, `Le`, `Gt`, `Ge`, `Eq`, `Ne`, `And`, `Or`, `Not`, `Neg`.

**Safety constraints:**
- Preserve runtime semantics: only fold integer arithmetic when checked operations succeed (avoid compile-time overflow behavior drift).
- Leave undefined/problematic cases unfolded (e.g., divide/rem by zero, invalid shifts).
- Never fold operations that could observe side effects (not expected in MIR rvalues here, but keep explicit guard).

### Acceptance Criteria

1. `1 + 2` in source becomes `Constant(Int(3))` in optimized MIR.
2. No division by zero at compile time (leave as runtime).
3. Existing tests pass.

---

### 50.3 Copy Propagation

**Priority:** P0
**Files:** `src/mir/optimize.rs`

Eliminate redundant temporaries by replacing uses of a copy with the original.

### Pattern

```
Assign(_1, Use(Copy(x)))       # _1 is just a copy of x
Assign(_2, BinaryOp(Add, Copy(_1), ...))
→
Assign(_2, BinaryOp(Add, Copy(x), ...))   # Replace _1 with x
# _1 is now dead and can be removed by dead code elimination
```

### Implementation

Build a simple substitution map: for each `Assign(dest, Use(Copy(src)))` or `Assign(dest, Use(Local(src)))`, record `dest → src`. Then walk all subsequent operands in the function and replace occurrences of `dest` with `src`. Chain substitutions: if `_1 → x` and `_2 → _1`, then `_2 → x`.

**Safety constraints:**
- Only propagate if `dest` is assigned exactly once (SSA-like property of temporaries)
- Do not propagate across assignments to `src` (if `src` is reassigned after the copy, the substitution is invalid)
- Do not propagate `ref` bindings
- Do not propagate if `dest` is used in a contract expression (contracts may snapshot values)
- Restrict Phase 1 propagation to compiler temporaries (`LocalDecl.name == None`) to avoid user-local/param alias surprises

### Acceptance Criteria

1. `_1 = x; _2 = _1; return _2` becomes `return x`.
2. Re-assigned locals are not incorrectly propagated.
3. Existing tests pass.

---

### 50.4 Dead Block Elimination

**Priority:** P1
**Files:** `src/mir/optimize.rs`

Remove basic blocks that are unreachable from the entry block.

### Implementation

1. Start from `function.entry_block` (not hardcoded block 0).
2. Walk all reachable blocks via terminators (Goto targets, If/Switch branches, Call return blocks).
3. Remove any block not in the reachable set.
4. Rebuild block vector and remap old `BlockId -> new BlockId` in:
   - `function.entry_block`
   - all terminator targets (`Goto`, `If`, `Switch`, `Call.next`, `CallIndirect.next`, `Spawn.next`, `Await.next`)
5. (Optional but cheap) jump-thread `Goto(bbX)` where `bbX` is a block with no statements and `Goto(bbY)`.

After constant folding, some `If` terminators with constant conditions can be simplified to `Goto`, making one branch unreachable:

```
# Before (after constant folding made condition always true)
If(Constant(Bool(true)), then_block, else_block)
→ Goto(then_block)
# else_block may now be unreachable
```

### Acceptance Criteria

1. Blocks with no predecessors (other than entry) are removed.
2. Constant-condition `If` is simplified to `Goto`.
3. Match `Switch` with known discriminant is simplified to `Goto`.
4. Block IDs are contiguous and all target references are valid after remap.
5. Existing tests pass.

---

### 50.5 Peephole Optimizations

**Priority:** P1
**Files:** `src/mir/optimize.rs`

Small, local pattern replacements within a single block.

### Patterns

```
# Redundant return temp
Assign(_1, Use(op))
Return(Copy(_1))        # _1 used only here
→
Return(op)

# Nop elimination
Statement::Nop → remove

# Identity operations
Assign(_1, BinaryOp(Add, x, Constant(Int(0)))) → Assign(_1, Use(x))
Assign(_1, BinaryOp(Add, Constant(Int(0)), x)) → Assign(_1, Use(x))
Assign(_1, BinaryOp(Mul, x, Constant(Int(1)))) → Assign(_1, Use(x))
Assign(_1, BinaryOp(Mul, Constant(Int(1)), x)) → Assign(_1, Use(x))
Assign(_1, BinaryOp(Mul, x, Constant(Int(0)))) → Assign(_1, Use(Constant(Int(0))))
Assign(_1, BinaryOp(Mul, Constant(Int(0)), x)) → Assign(_1, Use(Constant(Int(0))))

# Double negation
Assign(_1, UnaryOp(Not, _2))
Assign(_3, UnaryOp(Not, _1))       # _1 used only here
→ Assign(_3, Use(_2))
```

### Acceptance Criteria

1. `x + 0`, `x * 1` are eliminated.
2. `x * 0` folds to `0`.
3. Nop statements removed.
4. Redundant return-copy temp pattern is simplified when safe.
5. Existing tests pass.

---

### 50.6 Benchmarking + Regression Tests

**Priority:** P1
**Files:** `tests/mir_optimize_tests.rs` (new), `tests/forma/test_optimization.forma` (new)

### Optimization Unit Tests

Write tests in `tests/mir_optimize_tests.rs` that:
- Construct MIR fragments manually
- Run individual optimization passes
- Assert the resulting MIR is transformed correctly

Cover at minimum:
- Constant fold: int arithmetic, bool logic, comparison, unary
- Constant fold safety: div/rem by zero not folded; overflow-prone expressions not folded
- Copy prop: simple chain, re-assignment safety, multi-hop chain
- Dead block: unreachable else branch, orphan after Goto simplification, block-id remap validity
- Peephole: return temp, identity ops (both sides), nop removal

### Integration Tests

`tests/forma/test_optimization.forma`:
- Program that exercises constant expressions, copy chains, dead branches
- Run with and without `--no-optimize`, verify same stdout/stderr and exit code
- (Correctness test, not speed test)

### Benchmarking (Stretch)

Add a simple wall-clock benchmark script `scripts/bench_optimize.sh`:
- Run a compute-heavy `.forma` program (e.g., fibonacci, sorting) with `--no-optimize` and default
- Report elapsed time for both
- Not CI-enforced, just for manual validation

### Acceptance Criteria

1. At least 12 unit tests covering all 4 optimization passes.
2. Integration test produces identical output with/without optimization.
3. Benchmark shows measurable speedup (target: >15% on compute-heavy code).
4. `OptStats` are printed in benchmark/dev mode to confirm transformations actually fired.

---

## Verification Plan

```bash
# Core checks
cargo fmt --all -- --check
cargo clippy --all-targets -- -D warnings
cargo test --all

# Optimization correctness
./target/release/forma run --allow-all tests/forma/test_optimization.forma
./target/release/forma run --allow-all --no-optimize tests/forma/test_optimization.forma

# Output-equivalence sanity (stdout + exit code)
./target/release/forma run --allow-all tests/forma/test_optimization.forma > /tmp/opt_on.out
./target/release/forma run --allow-all --no-optimize tests/forma/test_optimization.forma > /tmp/opt_off.out
diff -u /tmp/opt_off.out /tmp/opt_on.out

# Existing .forma tests still pass
for f in tests/forma/*.forma; do
    ./target/release/forma run --allow-all "$f"
done

# Optional benchmark
bash scripts/bench_optimize.sh
```

---

## Out of Scope

1. **Use-def chains / liveness analysis** — needed for aggressive dead store elimination, deferred to Phase 2
2. **Function inlining** — requires call graph analysis, deferred to Phase 2
3. **Loop optimizations (LICM, unrolling)** — requires loop detection, deferred to Phase 2
4. **Common subexpression elimination** — requires value numbering, deferred to Phase 2
5. **Interpreter speedups** (Vec-based locals, instruction caching) — separate from MIR optimization, future sprint
6. **LLVM codegen integration** — LLVM already optimizes; MIR opts help interpreter path primarily

---

## Future Phases (Roadmap)

### Phase 2: Data-Flow Optimizations (Sprint 51+)
- Build use-def chain infrastructure
- Dead store elimination (remove writes to never-read locals)
- Common subexpression elimination (especially discriminant hoisting)
- Liveness analysis for better Move/Copy decisions

### Phase 3: Interprocedural Optimizations (Sprint 52+)
- Inlining small, non-recursive functions
- Loop-invariant code motion
- Strength reduction (`x * 2` → `x << 1`)

### Phase 4: Structural Optimizations (future)
- Escape analysis for closures (stack vs heap allocation)
- Alias analysis for ref params
- Specialization (monomorphize generic-like patterns)

---

## Definition of Done

1. `src/mir/optimize.rs` exists with 4 passes: constant fold, copy propagation, dead block elimination, peephole.
2. Optimization runs by default between `lower()` and `interpret()`/`codegen()`.
3. `--no-optimize` flag disables the pass.
4. All existing tests pass with optimization enabled.
5. 12+ unit tests for optimization correctness, including safety guard cases.
6. Integration test confirms output equivalence with/without optimization.
7. Block-ID remapping invariants are validated by tests.
8. Zero compiler warnings, zero clippy warnings, clean formatting.
9. Measurable speedup on compute-heavy programs (manual benchmark).

---

## File Summary

| File | Change |
|------|--------|
| `src/mir/optimize.rs` | New — optimization pass with 4 transforms |
| `src/mir/mod.rs` | Add `pub mod optimize;` |
| `src/main.rs` | Wire `optimize()` into run/build pipeline, add `--no-optimize` flag |
| `tests/mir_optimize_tests.rs` | New — unit tests for each optimization |
| `tests/forma/test_optimization.forma` | New — integration correctness test |
| `scripts/bench_optimize.sh` | New (stretch) — wall-clock benchmark |
| `docs/reference.md` | Document `--no-optimize` flag |
