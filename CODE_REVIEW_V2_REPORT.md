# FORMA v1.0 Deep Dive Code Review - Final Report

**Date:** January 25, 2026
**Scope:** Complete codebase review post-Sprint 13
**Status:** Issues identified requiring attention before v1.0

---

## Executive Summary

After comprehensive review of all major components, FORMA has made significant progress but **new critical issues have been identified** that were not in the original CODE_REVIEW_REPORT.md. The Sprint 9-13 work addressed the originally identified issues, but this deeper review reveals additional gaps.

**Overall Assessment: 70% v1.0 Ready**

| Component | Status | Critical Issues |
|-----------|--------|-----------------|
| Lexer | ⚠️ Issues | 3 critical (EOF dedent, m ambiguity, multi-error) |
| Parser | ⚠️ Issues | 2 high (error recovery, contextual keywords edge cases) |
| Type System | ⚠️ Issues | 4 critical (isize/usize, method classification, enum patterns, trait impl) |
| MIR/Interpreter | ⚠️ Issues | 2 critical (enum hash collision - may not be fully fixed, call stack panics) |
| Stdlib | ⚠️ Issues | 5 critical (duration_*, async spawn, CLI args, pow(), iterator encoding) |
| Examples | ⚠️ Broken | 2-3 examples won't compile/run |
| Tooling | ❌ Not Ready | 5 critical (formatter, LSP, grammar, JSON errors, REPL types) |

---

## Part 1: Lexer & Parser Issues

### CRITICAL

#### 1.1 EOF Dedent Generation Bug
**File:** `src/lexer/scanner.rs` lines 89-98

The EOF handling has incorrect dedent generation for nested blocks. When the file ends, pending dedents are calculated incorrectly after the stack is modified.

**Impact:** Deeply nested functions won't get correct dedent sequences.

**Fix:**
```rust
// Calculate pending dedents BEFORE modifying stack
if self.indent_stack.len() > 1 {
    self.pending_dedents = self.indent_stack.len() - 1;  // Calculate first
    self.indent_stack.clear();
    self.indent_stack.push(0);
    return self.make_token(TokenKind::Dedent);
}
```

#### 1.2 Contextual 'm' at EOF Ambiguity
**File:** `src/parser/parser.rs` lines 3129-3186

The `is_match_keyword()` function returns `true` when `m` is at EOF (not in the exclusion list), causing it to be treated as match keyword instead of variable.

**Impact:** Code ending with `m` alone will crash trying to parse as match.

**Fix:** Add `Some(TokenKind::Eof)` to the negative match list.

#### 1.3 Parser Only Returns First Error
**File:** `src/parser/parser.rs` lines 23-59

Error recovery stores but doesn't return multiple errors. Only first error is shown.

**Impact:** Poor developer experience - must fix errors one at a time.

**Fix:** Return `Vec<CompileError>` instead of single error.

### HIGH PRIORITY

#### 1.4 Mismatched Dedent Handling
**File:** `src/lexer/scanner.rs` lines 313-328

Dedenting to a level that doesn't match the indent stack corrupts state.

#### 1.5 F-String Error Locations Wrong
**File:** `src/parser/parser.rs` lines 2057-2088

Errors in f-string expressions show the f-string start location, not actual error position.

---

## Part 2: Type System Issues

### CRITICAL

#### 2.1 Missing Isize/Usize in Unification
**File:** `src/types/inference.rs` lines 2539-2559

The unification engine is missing cases for `Ty::Isize` and `Ty::Usize`. These types will produce confusing errors.

**Fix:** Add two match arms for these types.

#### 2.2 Method Classification Missing Specific Types
**File:** `src/types/inference.rs` lines 3085-3108

`classify_type_for_method()` only handles generic `Int`/`Float`, not `I32`, `F64`, etc.

**Impact:** Method calls on specific numeric types fail with "Unknown" type.

**Fix:** Add cases for all `I8-I128`, `U8-U128`, `F32`, `F64`, `Isize`, `Usize`.

#### 2.3 Enum Pattern Matching Not Validated
**File:** `src/types/inference.rs` lines 4331-4470

Enum patterns in match expressions don't validate variant names or field types.

**Impact:** Invalid enum patterns compile but fail at runtime.

#### 2.4 Trait Implementation Not Checked
**File:** `src/types/inference.rs` line 3080

The TODO comment indicates user-defined impl blocks and trait implementations aren't checked. Method lookup only works for builtins.

**Impact:** Custom methods on user structs don't type-check properly.

### HIGH PRIORITY

#### 2.5 Field Resolution Returns Fresh Variables
When field type can't be determined, returns a fresh type variable instead of error.

---

## Part 3: MIR & Interpreter Issues

### CRITICAL

#### 3.1 Enum Discriminant Hash Collision (Verify Fix)
**File:** `src/mir/lower.rs` line 1538

The original issue used naive byte sum. Sprint 9 reportedly fixed this with FNV-1a, but **verify this is actually in the code**. The review agent found byte-sum code still present.

**Action:** Verify the enum discriminant code uses proper hashing or index-based approach.

#### 3.2 Call Stack Unwrap Panics
**File:** `src/mir/interp.rs` lines 437, 752, 786, 796, 816, 856, 910, 930, 950

Repeated `.unwrap()` on `call_stack.last_mut()` can panic if call stack is empty.

**Impact:** Edge cases could crash the interpreter.

**Fix:** Add defensive checks or use `?` operator.

### HIGH PRIORITY

#### 3.3 Async is Synchronous (Document)
**File:** `src/mir/interp.rs` lines 917-955

The async implementation is synchronous - `sp` doesn't actually spawn, `await` immediately returns. This is fine if documented but misleading.

**Action:** Document this limitation clearly, or implement real threading.

---

## Part 4: Stdlib & Examples Issues

### CRITICAL

#### 4.1 Duration Functions Not Implemented
**File:** `stdlib/datetime.forma` lines 88-102

`duration_seconds`, `duration_minutes`, `duration_hours`, `duration_days` are documented but not implemented as builtins.

**Fix:** Add these to `interp.rs` builtins.

#### 4.2 Async Example Broken
**File:** `examples/async_downloader.forma`

Uses `sp fetch_url(url)` syntax but the full spawn-await chain may not work end-to-end.

**Action:** Test this example actually runs.

#### 4.3 CLI Args Missing
**File:** `examples/cli_with_db.forma`

Uses `args()` builtin that may not be implemented.

**Fix:** Add `args()` builtin or update example.

#### 4.4 Power Function Infinite Loop
**File:** `stdlib/core.forma` lines 77-80

`pow(base, negative_exp)` causes infinite recursion.

**Fix:** Add negative exponent check.

#### 4.5 Iterator Encoding Hack
**File:** `stdlib/iter.forma` lines 180-200

Uses `idx * 1000000 + value` encoding that breaks on large values.

**Fix:** Use proper tuple/struct.

### HIGH PRIORITY

#### 4.6 String Character Functions Disabled
**File:** `stdlib/string.forma` lines 123-134

Character utility functions are commented out despite `char_to_str` existing.

**Fix:** Uncomment these functions.

---

## Part 5: Tooling Issues

### CRITICAL

#### 5.1 Formatter Incomplete
**File:** `src/fmt/mod.rs`

Many expression types just output `"..."` placeholder. Running `forma fmt --write` will corrupt source files.

**Impact:** Formatter is unusable and dangerous.

**Fix:** Implement ALL expression/statement types.

#### 5.2 LSP Missing Essential Methods
**File:** `src/lsp/mod.rs`

- `goto_definition` returns `None` with TODO
- No `references`, `rename`, `symbol` providers
- Completion is context-unaware

**Impact:** IDE experience severely limited.

#### 5.3 Grammar Export Incomplete
**File:** `src/main.rs` lines 1287-1771

Missing from grammar:
- Shorthand keywords (f, s, e, t, i, m)
- Indentation rules
- Operator precedence
- F-string syntax

**Impact:** External tools can't parse FORMA reliably.

#### 5.4 JSON Error Format Wrong
**File:** `src/main.rs` lines 227-256

`end_column` calculation is incorrect. Missing fields that tools expect.

**Impact:** AI tools can't parse error output.

#### 5.5 REPL Type Information Missing
**File:** `src/main.rs` lines 1773-2018

`:type` command just prints "Expression is well-typed" instead of actual type.

**Impact:** Can't inspect types in REPL.

---

## Part 6: Priority Matrix

### Must Fix Before v1.0 (Blockers)

| # | Issue | Component | Effort |
|---|-------|-----------|--------|
| 1 | EOF dedent generation | Lexer | 1 hour |
| 2 | 'm' at EOF ambiguity | Parser | 30 min |
| 3 | Isize/Usize unification | Types | 30 min |
| 4 | Method type classification | Types | 1 hour |
| 5 | Enum discriminant (verify) | MIR | 1 hour |
| 6 | Duration functions | Stdlib | 2 hours |
| 7 | Async example test | Examples | 1 hour |
| 8 | pow() negative check | Stdlib | 15 min |
| 9 | Formatter completeness | Tooling | 4 hours |
| 10 | Grammar export | Tooling | 3 hours |

### Should Fix Before v1.0

| # | Issue | Component | Effort |
|---|-------|-----------|--------|
| 11 | Multi-error reporting | Parser | 2 hours |
| 12 | Enum pattern validation | Types | 3 hours |
| 13 | Call stack safety | Interpreter | 2 hours |
| 14 | CLI args builtin | Stdlib | 1 hour |
| 15 | LSP goto_definition | Tooling | 3 hours |
| 16 | REPL type display | Tooling | 2 hours |
| 17 | JSON error format | Tooling | 2 hours |

### Can Fix Post-v1.0

| # | Issue | Component |
|---|-------|-----------|
| 18 | Iterator encoding hack | Stdlib |
| 19 | String char functions | Stdlib |
| 20 | LSP completeness | Tooling |
| 21 | Async real threading | MIR |
| 22 | Trait impl checking | Types |

---

## Part 7: Verification Steps

Before v1.0 release, run these checks:

```bash
# 1. All tests pass
cargo test

# 2. All examples compile
for f in examples/*.forma; do
    echo "Checking $f"
    cargo run --quiet -- check "$f"
done

# 3. All examples run (where possible)
cargo run --quiet -- run examples/hello.forma
cargo run --quiet -- run examples/factorial.forma
cargo run --quiet -- run examples/async_downloader.forma

# 4. Stdlib compiles
for f in stdlib/*.forma; do
    echo "Checking $f"
    cargo run --quiet -- check "$f"
done

# 5. Grammar export
cargo run -- grammar > /dev/null

# 6. Formatter (on simple file)
cargo run -- fmt examples/hello.forma

# 7. REPL starts
echo ":quit" | cargo run -- repl
```

---

## Part 8: Recommended Sprint 14

Based on this review, here's the recommended work:

### Sprint 14.1: Critical Fixes (Day 1)
- [ ] Fix EOF dedent generation
- [ ] Fix 'm' at EOF ambiguity
- [ ] Add Isize/Usize to unification
- [ ] Add all numeric types to method classification
- [ ] Verify enum discriminant fix
- [ ] Add pow() negative check

### Sprint 14.2: Stdlib & Examples (Day 2)
- [ ] Implement duration_* builtins
- [ ] Add args() builtin
- [ ] Test async_downloader.forma runs
- [ ] Uncomment string char functions
- [ ] Test cli_with_db.forma runs

### Sprint 14.3: Tooling Essentials (Days 3-4)
- [ ] Complete formatter for all constructs
- [ ] Complete grammar export
- [ ] Fix JSON error format
- [ ] Implement REPL :type command
- [ ] Add LSP goto_definition

### Sprint 14.4: Verification (Day 5)
- [ ] Run all verification steps
- [ ] Fix any issues found
- [ ] Update documentation
- [ ] Create CHANGELOG.md

---

## Conclusion

The Sprint 9-13 work addressed the issues from the original CODE_REVIEW_REPORT.md, but this deeper review has uncovered additional gaps, particularly in:

1. **Edge cases** in lexer/parser (EOF handling, contextual keywords)
2. **Type system completeness** (all numeric types, enum patterns)
3. **Stdlib gaps** (duration functions, pow edge case)
4. **Tooling readiness** (formatter, grammar, LSP are incomplete)

**Estimated effort to v1.0:** 4-5 focused days of work on the critical issues.

The core language is solid. These are polish issues, not architectural problems. With this final sprint, FORMA will be production-ready.

---

*"Almost there - one more sprint for a truly production-ready v1.0."*
