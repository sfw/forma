# Sprint 15+: FORMA v1.1 Resolution Plan

**Date:** January 26, 2026
**Goal:** Resolve all v1.1-targeted limitations from KNOWN_LIMITATIONS.md

---

## Executive Summary

After completing Sprint 14.7, FORMA v1.0 is stable with 14 documented limitations. This plan outlines the sprints needed to address the v1.1 items.

**Total Estimated Effort:** 8-10 weeks
**Priority Order:** High-impact user-facing issues first

---

## Sprint 15: Tooling Essentials (2 weeks)

Focus on the most impactful developer experience improvements.

### Sprint 15.1: Multi-Error Reporting
**Effort:** 2-3 days
**Files:** `src/parser/parser.rs`

**Task:** Change parser to collect and return ALL errors, not just the first.

**Current:**
```rust
if first_error.is_none() {
    first_error = Some(e);
}
```

**Target:**
```rust
// Collect all errors
errors.push(e);
// At end, return Vec<CompileError>
```

**Acceptance Criteria:**
- [ ] Parser returns `Vec<CompileError>`
- [ ] All errors shown in one compilation attempt
- [ ] JSON error output includes all errors
- [ ] Existing tests pass

---

### Sprint 15.2: REPL Type Display
**Effort:** 1-2 days
**Files:** `src/main.rs`, `src/types/inference.rs`

**Task:** Make `:type` command show actual inferred type.

**Changes needed:**
1. Expose inferred type from `TypeChecker` API
2. Add `get_expr_type()` method that returns `Option<Ty>`
3. Update REPL `:type` handler to call this and format the result

**Target Output:**
```
forma> :type [1, 2, 3]
Vec[Int]

forma> :type |x| x + 1
(Int) -> Int
```

**Acceptance Criteria:**
- [ ] `:type` shows actual inferred types
- [ ] Generic types displayed properly
- [ ] Function types show signature

---

### Sprint 15.3: Formatter Completeness
**Effort:** 4-5 days
**Files:** `src/fmt/mod.rs`

**Task:** Replace all `"..."` and `"?"` placeholders with actual formatting.

**Constructs to implement:**
1. `Use` items (imports)
2. Pointer types (`Ty::Ptr`)
3. Function types (`Ty::Fn`)
4. Closure expressions
5. Complex patterns (Or patterns, guards)
6. All remaining expression kinds

**Test approach:**
```bash
# Format every file in examples/ and std/
# Compare input vs output - should be semantically equivalent
for f in examples/*.forma std/*.forma; do
    forma fmt "$f" > /tmp/formatted.forma
    forma check /tmp/formatted.forma
done
```

**Acceptance Criteria:**
- [ ] No `"..."` or `"?"` in formatter output
- [ ] All example files format correctly
- [ ] All stdlib files format correctly
- [ ] Round-trip: format(format(x)) == format(x)

---

### Sprint 15.4: LSP Go-to-Definition
**Effort:** 3-4 days
**Files:** `src/lsp/mod.rs`, `src/types/inference.rs`

**Task:** Implement actual go-to-definition.

**Changes needed:**
1. Track definition locations during type checking
2. Store `DefLocation { file, span }` for each symbol
3. Build symbol table with locations
4. Query symbol table in LSP handler

**Data structure:**
```rust
struct SymbolTable {
    definitions: HashMap<SymbolId, DefLocation>,
    references: HashMap<Span, SymbolId>,
}

struct DefLocation {
    file: PathBuf,
    span: Span,
}
```

**Acceptance Criteria:**
- [ ] Jump to function definitions
- [ ] Jump to struct definitions
- [ ] Jump to enum definitions
- [ ] Jump to local variable definitions
- [ ] Works in VS Code with FORMA extension

---

## Sprint 16: Type System Improvements (2 weeks)

### Sprint 16.1: Trait Implementation Checking
**Effort:** 4-5 days
**Files:** `src/types/inference.rs`, `src/types/checker.rs`

**Task:** Validate that impl blocks satisfy trait requirements.

**Checks needed:**
1. All trait methods are implemented
2. Method signatures match (return type, param types)
3. Self type is correct
4. Generic bounds are satisfied

**Error messages:**
```
error: impl Printable for Point missing method 'to_string'
  --> src/point.forma:15:1
   |
15 | i Printable for Point
   | ^^^^^^^^^^^^^^^^^^^^^ missing: to_string(self) -> Str

error: method signature mismatch
  --> src/point.forma:17:5
   |
17 |     f to_string(self) -> Int
   |                         ^^^ expected Str, found Int
```

**Acceptance Criteria:**
- [ ] Missing methods produce clear errors
- [ ] Signature mismatches caught
- [ ] Generic impl blocks validated
- [ ] Error spans point to correct location

---

### Sprint 16.2: Enum Pattern Validation
**Effort:** 3-4 days
**Files:** `src/types/inference.rs`

**Task:** Validate enum variant names and field types at compile time.

**Checks needed:**
1. Variant name exists in enum
2. Correct number of fields
3. Field types unify correctly
4. Suggest similar variant names on typo

**Example errors:**
```
error: unknown variant 'Color::Purple'
  --> src/main.forma:10:5
   |
10 |     Color::Purple -> ...
   |     ^^^^^^^^^^^^^ 'Color' has no variant 'Purple'
   |
   = help: available variants: Red, Green, Blue
   = help: did you mean 'Color::Blue'?
```

**Acceptance Criteria:**
- [ ] Unknown variants caught at compile time
- [ ] Field count mismatches caught
- [ ] Helpful suggestions provided
- [ ] Existing pattern matching tests pass

---

## Sprint 17: Language Features (2 weeks)

### Sprint 17.1: Struct Update Syntax
**Effort:** 2-3 days
**Files:** `src/mir/lower.rs`

**Task:** Implement `{ ..base, field: value }` in MIR lowering.

**Current (TODO comment):**
```rust
// TODO: handle struct update
```

**Implementation:**
1. Copy all fields from base struct
2. Override specified fields with new values
3. Emit MIR for creating new struct value

**MIR sequence:**
```
_1 = base.x          // Copy x from base
_2 = 10              // New value for y
_3 = MakeStruct(_1, _2)  // Create updated struct
```

**Acceptance Criteria:**
- [ ] Basic struct update works
- [ ] Multiple field updates work
- [ ] Nested struct updates work
- [ ] Type checking validates field types

---

### Sprint 17.2: Indirect Closure Calls (LLVM)
**Effort:** 4-5 days
**Files:** `src/codegen/llvm.rs`

**Task:** Support calling closures stored in variables in LLVM codegen.

**Current limitation:** Direct calls work, indirect don't.

**Implementation approach:**
1. Closures represented as fat pointers (fn ptr + env)
2. Indirect call extracts fn ptr from closure value
3. Pass env as implicit first argument

**LLVM IR pattern:**
```llvm
%closure = ...                    ; { fn*, env* }
%fn_ptr = extractvalue %closure, 0
%env_ptr = extractvalue %closure, 1
%result = call %fn_ptr(%env_ptr, %arg1, %arg2)
```

**Acceptance Criteria:**
- [ ] Closures can be stored in variables
- [ ] Closures can be passed to functions
- [ ] Closures can be returned from functions
- [ ] All closure tests pass in compiled mode

---

## Sprint 18: Async & Performance (2 weeks)

### Sprint 18.1: True Async Parallelism
**Effort:** 5-7 days
**Files:** `src/mir/interp.rs`, potentially new `src/runtime/` module

**Task:** Make `sp` actually spawn concurrent tasks.

**Options:**
1. **Tokio integration** - Full async runtime
2. **Rayon for CPU tasks** - Simpler, parallel iterators
3. **Green threads** - Custom lightweight implementation

**Recommended: Tokio with async executor**

**Changes:**
1. `sp expr` creates a tokio task
2. `aw task` awaits the task result
3. `await_all` uses `join_all`
4. Add `#[tokio::main]` to entry point

**Considerations:**
- Need to make interpreter `Send + Sync` or use `Arc<Mutex<>>`
- HTTP calls already async-compatible (reqwest)
- File I/O needs async versions

**Acceptance Criteria:**
- [ ] `sp` spawns actual concurrent tasks
- [ ] `await_all` runs tasks in parallel
- [ ] HTTP downloads are truly concurrent
- [ ] No data races or deadlocks
- [ ] Performance improvement measurable

---

## Sprint 19: Polish (1 week)

### Sprint 19.1: Grammar Export Completeness
**Effort:** 2 days
**Files:** `src/main.rs`

**Task:** Complete the EBNF grammar with missing details.

**Add:**
- Shorthand keyword equivalences (f=fn, s=struct, etc.)
- Indentation rules (INDENT/DEDENT semantics)
- Full operator precedence table
- F-string interpolation syntax

---

### Sprint 19.2: Final Integration Testing
**Effort:** 3 days

**Task:** Comprehensive testing of all v1.1 features.

1. Run full test suite
2. Test all examples compile AND run
3. Test formatter on real-world code
4. Test LSP in VS Code
5. Benchmark async performance
6. Update documentation

---

## Priority Matrix

| Sprint | Priority | Effort | User Impact |
|--------|----------|--------|-------------|
| 15.1 Multi-error | HIGH | 2-3 days | High - better DX |
| 15.2 REPL :type | MEDIUM | 1-2 days | Medium - debugging |
| 15.3 Formatter | HIGH | 4-5 days | High - code quality |
| 15.4 LSP goto-def | HIGH | 3-4 days | High - IDE users |
| 16.1 Trait checking | HIGH | 4-5 days | High - correctness |
| 16.2 Enum validation | MEDIUM | 3-4 days | Medium - safety |
| 17.1 Struct update | MEDIUM | 2-3 days | Medium - convenience |
| 17.2 Closure LLVM | MEDIUM | 4-5 days | Medium - compiled mode |
| 18.1 True async | HIGH | 5-7 days | High - performance |
| 19.x Polish | LOW | 5 days | Low - completeness |

---

## Recommended Execution Order

**Phase 1 (Weeks 1-2): Developer Experience**
1. Sprint 15.1 - Multi-error reporting
2. Sprint 15.2 - REPL type display
3. Sprint 15.4 - LSP go-to-definition

**Phase 2 (Weeks 3-4): Type Safety**
4. Sprint 16.1 - Trait implementation checking
5. Sprint 16.2 - Enum pattern validation

**Phase 3 (Weeks 5-6): Features**
6. Sprint 15.3 - Formatter completeness
7. Sprint 17.1 - Struct update syntax

**Phase 4 (Weeks 7-8): Performance**
8. Sprint 18.1 - True async parallelism
9. Sprint 17.2 - Indirect closure calls

**Phase 5 (Weeks 9-10): Polish**
10. Sprint 19.x - Grammar, testing, docs

---

## Success Criteria for v1.1

- [ ] All items marked "Planned: v1.1" in KNOWN_LIMITATIONS.md resolved
- [ ] Zero `"..."` or `"?"` in formatter output
- [ ] LSP go-to-definition working
- [ ] Multi-error reporting working
- [ ] Trait impl checking complete
- [ ] True async parallelism working
- [ ] All 250+ Rust tests passing
- [ ] All 40+ FORMA integration tests passing
- [ ] All examples compile and run
- [ ] Documentation updated

---

## Claude Code Prompt Template

For each sprint, use this prompt pattern:

```
Read SPRINT_15_V1_1_PLAN.md and implement Sprint 15.X: [Name].

Focus on:
1. [Specific change 1]
2. [Specific change 2]
3. [Specific change 3]

Files to modify:
- [file1.rs]
- [file2.rs]

Run `cargo test` after changes. Create a test file `tests/forma/test_[feature].forma` to verify the fix.

Acceptance criteria from the plan:
- [ ] Criterion 1
- [ ] Criterion 2
```

---

*"v1.1: From working to polished."*
