# Sprints 26-34: Complete FORMA v2.0 Plan

**Goal:** Fix all remaining issues and implement memory safety
**Estimated Total Effort:** 5-7 days

---

## Sprint 26: Type Inference Robustness

### Task 26.1: Fix FieldShorthand Validation
**File:** `src/types/inference.rs` lines 4593-4599

**Problem:** `.foo` shorthand doesn't validate field exists
```rust
ExprKind::FieldShorthand(_field) => {
    let arg_ty = Ty::fresh_var();
    let result_ty = Ty::fresh_var();
    Ok(Ty::Fn(vec![arg_ty], Box::new(result_ty)))
}
```

**Fix:** Extract field name, constrain arg_ty to have that field, set result_ty to field type.

### Task 26.2: Fix OpShorthand Validation
**File:** `src/types/inference.rs` lines 4602-4608

**Problem:** `(+ 1)` doesn't constrain types based on operator
```rust
ExprKind::OpShorthand(_op, operand, _is_left) => {
    let arg_ty = Ty::fresh_var();
    let result_ty = Ty::fresh_var();
    self.infer_expr(operand)?;
    Ok(Ty::Fn(vec![arg_ty], Box::new(result_ty)))
}
```

**Fix:** Constrain arg_ty and result_ty based on operator (e.g., `+` requires numeric types).

### Task 26.3: Fix Unification State Corruption
**File:** `src/types/inference.rs` lines 4177-4179, 4546-4548

**Problem:** Speculative `unify().is_ok()` calls corrupt substitution state

**Fix:** Save unifier state before speculative unification, restore on failure:
```rust
let checkpoint = self.unifier.checkpoint();
if self.unifier.unify(&ty1, &ty2, span).is_err() {
    self.unifier.restore(checkpoint);
}
```

### Task 26.4: Fix ArrayRepeat Count Validation
**File:** `src/types/inference.rs` lines 4206-4209

**Problem:** `[expr; count]` doesn't validate count is Int
```rust
ExprKind::ArrayRepeat(elem, count) => {
    let elem_ty = self.infer_expr(elem)?;
    let _count_ty = self.infer_expr(count)?;  // Discarded!
    Ok(Ty::List(Box::new(elem_ty)))
}
```

**Fix:** Add `self.unifier.unify(&count_ty, &Ty::Int, expr.span)?;`

### Task 26.5: Fix Trait Validation Depth
**File:** `src/types/inference.rs` lines 3649-3683

**Problem:** Only validates parameter count, not types or return types

**Fix:** Validate generic params are declared, return types match, impl blocks match trait.

### Task 26.6: Fix User-Defined Method Lookup
**File:** `src/types/inference.rs` line 3234

**Problem:** Only builtin_methods checked, impl blocks may not be found
```rust
// TODO: Also check user-defined impl blocks and trait implementations
None
```

**Fix:** Add lookup in trait implementations and handle wrapped types.

### Task 26.7: Fix User-Defined Enum Type Substitution
**File:** `src/types/inference.rs` lines 4746-4804

**Problem:** Generic type params not substituted in pattern field types

**Fix:** After unification, apply substitution to field_types:
```rust
let substituted_field_types: Vec<Ty> = field_types.iter()
    .map(|ft| ft.apply(&self.unifier.subst))
    .collect();
```

---

## Sprint 27: Parser Feature Completion

### Task 27.1: Implement WhileLet Parsing
**File:** `src/parser/parser.rs` lines 2436-2448

**Current:** Only parses condition, not pattern
**Fix:** Detect `pattern = expr` after `wh`, parse pattern before `=`

### Task 27.2: Implement Loop Labels
**File:** `src/parser/parser.rs` lines 2424-2458

**Current:** Labels always `None`
**Fix:** Check for `'label:` before loop keyword, parse and pass to ExprKind

### Task 27.3: Implement Brace-Style Struct Patterns
**File:** `src/parser/parser.rs` line 2841

**Current:** Only `Point(x, y)` style
**Fix:** Also handle `Point { x, y }` style with LBrace check

### Task 27.4: Implement BitOr/BitAnd Operators
**File:** `src/parser/parser.rs` lines 1512, 1551

**Current:** Stubbed, skip to next precedence
**Fix:** Parse `|` and `&` as binary operators with proper precedence

### Task 27.5: Add Statement-Level Error Recovery
**File:** `src/parser/parser.rs` lines 2969-2990

**Current:** First error fails entire block
**Fix:** Catch errors, skip to recovery point, continue parsing

---

## Sprint 28: Lexer Improvements

### Task 28.1: Unicode Identifier Support
**File:** `src/lexer/scanner.rs` lines 934-940

**Current:** `c.is_ascii_alphabetic()`
**Fix:** Use `c.is_alphabetic()` for Unicode support

### Task 28.2: Fix F-String Nested Expressions
**File:** `src/lexer/scanner.rs` lines 809-841

**Problem:** Braces in string literals confuse parser
**Fix:** Track string parsing state, skip string literal braces

### Task 28.3: Fix Indentation Misalignment Detection
**File:** `src/lexer/scanner.rs` lines 315-330

**Problem:** Misaligned dedents silently accepted
**Fix:** After dedent loop, verify indent matches stack top, error if not

### Task 28.4: Complete F-String Escape Sequences
**File:** `src/lexer/scanner.rs` lines 774-791

**Missing:** `\0`, `\x`, `\u{}`
**Fix:** Add cases for null, hex, and unicode escapes

### Task 28.5: Raw String Backtick Support
**File:** `src/lexer/scanner.rs` lines 421-447

**Problem:** Can't include backticks in raw strings
**Fix:** Support delimiter counting like `` r#`...`# ``

---

## Sprint 29: MIR Lowerer Completion

### Task 29.1: Fix Loop Label Error Handling
**File:** `src/mir/lower.rs` lines 835-838

**Current:** `eprintln!("Warning: break label...")`
**Fix:** `self.error("break label '{}' not found", span);`

### Task 29.2: Fix Closure Entry Block
**File:** `src/mir/lower.rs` lines 1082-1084

**Missing:** `new_fn.entry_block = entry;`
**Fix:** Add entry block assignment before setting current_fn

### Task 29.3: Fix Variant Discriminant Hash
**File:** `src/mir/lower.rs` lines 1763-1772

**Current:** Sum of ASCII bytes (collisions possible)
**Fix:** Use FNV-1a hash or ordered enum registry

### Task 29.4: Implement Missing PatternKind Variants
**File:** `src/mir/lower.rs` line 1734

**Missing:** Or, Range, Slice, Ref patterns
**Fix:** Add match arms with proper lowering logic

### Task 29.5: Implement Missing ExprKind Variants
**File:** `src/mir/lower.rs`

**Missing:** Check for unhandled expression kinds
**Fix:** Audit AST and add implementations or errors

### Task 29.6: Save/Restore Closure Local Types
**File:** `src/mir/lower.rs` lines 1048-1101

**Missing:** `self.local_types` not saved/restored
**Fix:** Save before closure, restore after

### Task 29.7: Convert expect() to Error Handling
**File:** `src/mir/lower.rs` line 2982

**Current:** `.expect("parse should succeed")`
**Fix:** Match on Result, call `self.error()` on Err

---

## Sprint 30: Stdlib Robustness

### Task 30.1: Make pow() Tail-Recursive
**File:** `std/core.forma` lines 78-87

**Current:** `base * pow(base, exp - 1)` (stack overflow risk)
**Fix:** Use accumulator pattern with exponentiation by squaring

### Task 30.2: Fix lcm() Overflow
**File:** `std/core.forma` lines 94-97

**Current:** `abs(a * b) / gcd(a, b)` (overflow before divide)
**Fix:** `(abs(a) / gcd(a, b)) * abs(b)`

### Task 30.3: Validate days_in_month() Input
**File:** `std/datetime.forma` lines 129-135

**Current:** Returns 31 for invalid months
**Fix:** Return 0 or error for month < 1 or > 12

### Task 30.4: Guard range_step() Against step=0
**File:** `std/iter.forma` lines 105-112

**Current:** Creates iterator that loops infinitely
**Fix:** Return error or empty range for step=0

### Task 30.5: Enable String Character Functions
**File:** `std/string.forma` lines 128-132

**Current:** Commented out due to type inference
**Fix:** Re-enable after Sprint 26 type fixes

### Task 30.6: Fix vec_tail Naming
**File:** `std/vec.forma` lines 39-41

**Current:** Returns last element (should be vec_last)
**Fix:** Rename to vec_last, add proper vec_tail that returns rest

---

## Sprint 31: CLI/REPL Improvements

### Task 31.1: REPL Multi-Line Input
**File:** `src/main.rs`

**Fix:** Detect incomplete expressions, continue reading

### Task 31.2: REPL Definition Detection
**File:** `src/main.rs`

**Fix:** Also detect `pub f`, `async f`, `const`, `type`

### Task 31.3: REPL :load Command
**File:** `src/main.rs`

**Fix:** Add `:load filename` to load external files

### Task 31.4: CLI Program Args Passing
**File:** `src/main.rs`

**Fix:** Pass args after `--` to the FORMA program

### Task 31.5: CLI compile Command
**File:** `src/main.rs`

**Fix:** Implement `forma compile` (alias for build)

---

## Sprint 32: FFI Safety Layer

### Task 32.1: Create SafePtr Wrapper
**File:** New `src/ffi/safe_ptr.rs`

Wrap raw pointers with bounds checking and lifetime tracking.

### Task 32.2: Bounds-Checked Memory Operations
**File:** `src/mir/interp.rs`

Replace raw pointer ops with SafePtr operations.

### Task 32.3: Capability-Based FFI Access
**File:** `src/mir/interp.rs`

Require FFI capability to use unsafe operations.

---

## Sprint 33: Linear Types Foundation

### Task 33.1: Add `linear` Keyword to Lexer
**File:** `src/lexer/scanner.rs`

Add `linear` as reserved keyword.

### Task 33.2: Add Linear Type Syntax to Parser
**File:** `src/parser/parser.rs`

Parse `linear T` type annotations.

### Task 33.3: LinearityKind Enum
**File:** `src/types/mod.rs`

```rust
enum LinearityKind {
    Linear,   // Must use exactly once
    Affine,   // Must use at most once
    Regular,  // Normal (can copy/drop freely)
}
```

### Task 33.4: Use-Tracking in Type Checker
**File:** `src/types/inference.rs`

Track linear value usage, error on double-use or unused.

### Task 33.5: Consume-Exactly-Once Validation
**File:** `src/types/inference.rs`

At scope exit, verify all linear values consumed.

---

## Sprint 34: Capability System

### Task 34.1: Capability Type Hierarchy
**File:** `src/types/mod.rs`

```rust
enum Capability {
    File(PathBuf),
    Network(Host, Port),
    Env,
    Spawn,
}
```

### Task 34.2: FileCapability Implementation
**File:** `src/mir/interp.rs`

Gate file operations on FileCapability.

### Task 34.3: NetworkCapability Implementation
**File:** `src/mir/interp.rs`

Gate network operations on NetworkCapability.

### Task 34.4: Main Function Capability Injection
**File:** `src/mir/interp.rs`

Pass capabilities to main() based on permissions.

---

## Summary Table

| Sprint | Focus | Tasks | Priority |
|--------|-------|-------|----------|
| 26 | Type Inference | 7 | High |
| 27 | Parser | 5 | Medium |
| 28 | Lexer | 5 | Medium |
| 29 | MIR Lowerer | 7 | Medium |
| 30 | Stdlib | 6 | Medium |
| 31 | CLI/REPL | 5 | Low |
| 32 | FFI Safety | 3 | High |
| 33 | Linear Types | 5 | High |
| 34 | Capabilities | 4 | High |
| **Total** | | **47 tasks** | |

---

## Execution Order Recommendation

1. **Sprint 26** (Type Inference) - Fixes enable other features
2. **Sprint 29** (MIR Lowerer) - Core compiler stability
3. **Sprint 30** (Stdlib) - Safe standard library
4. **Sprint 27** (Parser) - Language features
5. **Sprint 28** (Lexer) - Language features
6. **Sprint 31** (CLI/REPL) - Developer experience
7. **Sprints 32-34** (Memory Safety) - v2.0 features

---

*"Quality is not an act, it is a habit."* - Aristotle
