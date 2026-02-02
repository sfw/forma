# Sprint 14.7: Final Hardening

**Priority:** CRITICAL - Last fixes before v1.0
**Date:** January 26, 2026

---

## Overview

This sprint addresses the remaining issues found during the silent failure audit:
1. Call stack safety (unwrap panics)
2. Formatter completeness verification
3. JSON functionality full verification
4. Document known limitations

---

## Task 14.7.1: Fix Call Stack Unwrap Panics

**File:** `src/mir/interp.rs`

**Problem:** Multiple locations use `.unwrap()` on `call_stack.last_mut()` which will panic if the call stack is empty.

**Locations to fix (search for `call_stack.last_mut().unwrap()`):**
- Line ~437
- Line ~752
- Line ~760
- Line ~786
- Line ~796
- Line ~816
- Line ~856
- Line ~910
- Line ~930
- Line ~950

**Current (dangerous):**
```rust
let frame = self.call_stack.last_mut().unwrap();
```

**Fix Option A - Return Error:**
```rust
let frame = self.call_stack.last_mut()
    .ok_or_else(|| InterpError {
        message: "internal error: empty call stack".to_string()
    })?;
```

**Fix Option B - Add Helper Method:**
```rust
impl Interpreter {
    fn current_frame(&self) -> Result<&CallFrame, InterpError> {
        self.call_stack.last()
            .ok_or_else(|| InterpError {
                message: "internal error: empty call stack".to_string()
            })
    }

    fn current_frame_mut(&mut self) -> Result<&mut CallFrame, InterpError> {
        self.call_stack.last_mut()
            .ok_or_else(|| InterpError {
                message: "internal error: empty call stack".to_string()
            })
    }
}

// Then replace all:
let frame = self.call_stack.last_mut().unwrap();
// With:
let frame = self.current_frame_mut()?;
```

**Recommendation:** Use Option B - add helper methods for cleaner code.

**Acceptance Criteria:**
- [ ] No `.unwrap()` calls on `call_stack.last()` or `call_stack.last_mut()`
- [ ] Empty call stack produces clear error, not panic
- [ ] All existing tests still pass

---

## Task 14.7.2: Fix MIR Lowerer Unwrap Panics

**File:** `src/mir/lower.rs`

**Problem:** Multiple locations use `.unwrap()` on `current_block` and `current_fn`.

**Locations to fix:**
- Line ~312: `self.current_block.unwrap()`
- Line ~313: `self.current_fn.as_ref().unwrap()`
- Line ~317-318: Same pattern
- Line ~702: `func_name.unwrap()`
- Line ~1277: `self.current_fn.as_ref().unwrap().block(self.current_block.unwrap())`
- Line ~1295: Same pattern
- Line ~1519, ~1788, ~1881, ~1934, ~1961: Same pattern
- Line ~2177, ~2184, ~2478, ~2483-2490: Same pattern

**Fix - Add Helper Methods:**
```rust
impl MirLowerer {
    fn current_block_id(&self) -> Result<BlockId, LowerError> {
        self.current_block
            .ok_or_else(|| LowerError::new("internal error: no current block"))
    }

    fn current_function(&self) -> Result<&MirFunction, LowerError> {
        self.current_fn.as_ref()
            .ok_or_else(|| LowerError::new("internal error: no current function"))
    }

    fn current_function_mut(&mut self) -> Result<&mut MirFunction, LowerError> {
        self.current_fn.as_mut()
            .ok_or_else(|| LowerError::new("internal error: no current function"))
    }
}
```

**Acceptance Criteria:**
- [ ] No `.unwrap()` calls on `current_block` or `current_fn`
- [ ] Missing block/function produces clear error
- [ ] All existing tests still pass

---

## Task 14.7.3: Fix Borrow Checker Unwrap Panics

**File:** `src/borrow/checker.rs`

**Problem:** Uses `.unwrap()` on scope stack operations.

**Locations:**
- Line ~223: `self.scope_stack.last_mut().unwrap()`
- Line ~555: Same
- Line ~911: Same
- Line ~943: Same

**Fix:**
```rust
fn current_scope_mut(&mut self) -> Result<&mut HashSet<String>, BorrowError> {
    self.scope_stack.last_mut()
        .ok_or_else(|| BorrowError::new("internal error: empty scope stack"))
}
```

**Acceptance Criteria:**
- [ ] No `.unwrap()` on scope stack
- [ ] Clear error messages for internal errors

---

## Task 14.7.4: Fix String/Bool/Char/Float Literal Pattern Matching

**File:** `src/mir/lower.rs`

**Problem:** In the `lower_match()` function (around line 1399), only integer literal patterns are handled. String, boolean, char, and float literals fall through to the catch-all `_ =>` case which does `Goto(next_test)`, causing wildcard to always match instead.

**Current code (lines 1399-1420):**
```rust
PatternKind::Literal(Literal { kind: LiteralKind::Int(n), .. }) => {
    // Compare integer and branch
    let lit_local = self.new_temp(Ty::Int);
    self.emit(StatementKind::Assign(
        lit_local,
        Rvalue::Use(Operand::Constant(Constant::Int(*n as i64))),
    ));
    let cond_local = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(
        cond_local,
        Rvalue::BinaryOp(
            BinOp::Eq,
            Operand::Copy(scrut_local),
            Operand::Copy(lit_local),
        ),
    ));
    self.terminate(Terminator::If {
        cond: Operand::Copy(cond_local),
        then_block: body_block,
        else_block: next_test,
    });
}
```

**Fix:** Add these additional pattern handlers AFTER the Int handler (after line 1420):

```rust
PatternKind::Literal(Literal { kind: LiteralKind::String(s), .. }) => {
    // Compare string and branch
    let lit_local = self.new_temp(Ty::Str);
    self.emit(StatementKind::Assign(
        lit_local,
        Rvalue::Use(Operand::Constant(Constant::Str(s.clone()))),
    ));
    let cond_local = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(
        cond_local,
        Rvalue::BinaryOp(
            BinOp::Eq,
            Operand::Copy(scrut_local),
            Operand::Copy(lit_local),
        ),
    ));
    self.terminate(Terminator::If {
        cond: Operand::Copy(cond_local),
        then_block: body_block,
        else_block: next_test,
    });
}

PatternKind::Literal(Literal { kind: LiteralKind::Bool(b), .. }) => {
    // Compare bool and branch
    let lit_local = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(
        lit_local,
        Rvalue::Use(Operand::Constant(Constant::Bool(*b))),
    ));
    let cond_local = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(
        cond_local,
        Rvalue::BinaryOp(
            BinOp::Eq,
            Operand::Copy(scrut_local),
            Operand::Copy(lit_local),
        ),
    ));
    self.terminate(Terminator::If {
        cond: Operand::Copy(cond_local),
        then_block: body_block,
        else_block: next_test,
    });
}

PatternKind::Literal(Literal { kind: LiteralKind::Char(c), .. }) => {
    // Compare char and branch
    let lit_local = self.new_temp(Ty::Char);
    self.emit(StatementKind::Assign(
        lit_local,
        Rvalue::Use(Operand::Constant(Constant::Char(*c))),
    ));
    let cond_local = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(
        cond_local,
        Rvalue::BinaryOp(
            BinOp::Eq,
            Operand::Copy(scrut_local),
            Operand::Copy(lit_local),
        ),
    ));
    self.terminate(Terminator::If {
        cond: Operand::Copy(cond_local),
        then_block: body_block,
        else_block: next_test,
    });
}

PatternKind::Literal(Literal { kind: LiteralKind::Float(f), .. }) => {
    // Compare float and branch
    let lit_local = self.new_temp(Ty::Float);
    self.emit(StatementKind::Assign(
        lit_local,
        Rvalue::Use(Operand::Constant(Constant::Float(*f))),
    ));
    let cond_local = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(
        cond_local,
        Rvalue::BinaryOp(
            BinOp::Eq,
            Operand::Copy(scrut_local),
            Operand::Copy(lit_local),
        ),
    ));
    self.terminate(Terminator::If {
        cond: Operand::Copy(cond_local),
        then_block: body_block,
        else_block: next_test,
    });
}
```

**Test case to verify fix (add to tests):**

```forma
# Test string pattern matching
f test_string_match() -> Bool
    s := "hello"
    m s
        "hello" -> true
        "world" -> false
        _ -> false

f test_bool_match() -> Bool
    b := true
    m b
        true -> true
        false -> false

f test_multiple_string_patterns() -> Str
    status := "error"
    m status
        "ok" -> "success"
        "error" -> "failure"
        "pending" -> "waiting"
        _ -> "unknown"
```

**Acceptance Criteria:**
- [ ] String literal patterns match correctly
- [ ] Bool literal patterns match correctly
- [ ] Char literal patterns match correctly
- [ ] Float literal patterns match correctly
- [ ] Wildcard only matches when no literal pattern matches
- [ ] All existing tests still pass

---

## Task 14.7.5: Verify JSON Functionality Completely (ALREADY PASSING)

**Note:** Claude Code already verified JSON stdlib tests pass. This task is to ensure comprehensive coverage.

**Create comprehensive JSON test file:** `tests/forma/test_json_complete.forma`

```forma
# Complete JSON Functionality Test
# Tests ALL json_* builtins and stdlib functions

# ============================================================
# Test JSON Parsing
# ============================================================

f test_json_parse_object() -> Bool
    json_str := '{"name": "Alice", "age": 30}'
    m json_parse(json_str)
        Ok(json) ->
            m json_get_str(json, "name")
                Some(name) -> name == "Alice"
                None -> false
        Err(_) -> false

f test_json_parse_array() -> Bool
    json_str := '[1, 2, 3, 4, 5]'
    m json_parse(json_str)
        Ok(json) ->
            json_array_len(json) == 5
        Err(_) -> false

f test_json_parse_nested() -> Bool
    json_str := '{"user": {"name": "Bob", "email": "bob@example.com"}}'
    m json_parse(json_str)
        Ok(json) ->
            m json_get(json, "user")
                Some(user) ->
                    m json_get_str(user, "email")
                        Some(email) -> email == "bob@example.com"
                        None -> false
                None -> false
        Err(_) -> false

f test_json_parse_invalid() -> Bool
    json_str := '{invalid json}'
    m json_parse(json_str)
        Ok(_) -> false  # Should fail
        Err(_) -> true  # Expected

# ============================================================
# Test JSON Creation
# ============================================================

f test_json_from_str() -> Bool
    json := json_from_str("hello")
    json_is_string(json)

f test_json_from_int() -> Bool
    json := json_from_int(42)
    json_is_number(json)

f test_json_from_float() -> Bool
    json := json_from_float(3.14)
    json_is_number(json)

f test_json_from_bool() -> Bool
    json := json_from_bool(true)
    json_is_bool(json)

f test_json_null() -> Bool
    json := json_null()
    json_is_null(json)

f test_json_object_empty() -> Bool
    json := json_object()
    json_is_object(json) && vec_len(json_keys(json)) == 0

f test_json_array_empty() -> Bool
    json := json_array()
    json_is_array(json) && json_array_len(json) == 0

# ============================================================
# Test JSON Manipulation
# ============================================================

f test_json_set() -> Bool
    json := json_object()
    json2 := json_set(json, "name", json_from_str("Charlie"))
    m json_get_str(json2, "name")
        Some(name) -> name == "Charlie"
        None -> false

f test_json_keys() -> Bool
    json_str := '{"a": 1, "b": 2, "c": 3}'
    m json_parse(json_str)
        Ok(json) ->
            keys := json_keys(json)
            vec_len(keys) == 3
        Err(_) -> false

f test_json_values() -> Bool
    json_str := '{"x": 10, "y": 20}'
    m json_parse(json_str)
        Ok(json) ->
            values := json_values(json)
            vec_len(values) == 2
        Err(_) -> false

f test_json_has() -> Bool
    json_str := '{"exists": true}'
    m json_parse(json_str)
        Ok(json) ->
            json_has(json, "exists") && !json_has(json, "missing")
        Err(_) -> false

# ============================================================
# Test JSON Array Operations
# ============================================================

f test_json_array_get() -> Bool
    json_str := '["a", "b", "c"]'
    m json_parse(json_str)
        Ok(json) ->
            m json_array_get(json, 1)
                Some(elem) ->
                    json_type(elem) == "string"
                None -> false
        Err(_) -> false

# ============================================================
# Test JSON Type Checking
# ============================================================

f test_json_type() -> Bool
    str_json := json_from_str("test")
    int_json := json_from_int(42)
    bool_json := json_from_bool(false)
    null_json := json_null()

    json_type(str_json) == "string" &&
        json_type(int_json) == "number" &&
        json_type(bool_json) == "bool" &&
        json_type(null_json) == "null"

f test_json_is_predicates() -> Bool
    json_is_string(json_from_str("x")) &&
        json_is_number(json_from_int(1)) &&
        json_is_bool(json_from_bool(true)) &&
        json_is_null(json_null()) &&
        json_is_object(json_object()) &&
        json_is_array(json_array())

# ============================================================
# Test JSON Stringify
# ============================================================

f test_json_stringify() -> Bool
    json := json_object()
    json = json_set(json, "key", json_from_str("value"))
    str := json_stringify(json)
    str_contains(str, "key") && str_contains(str, "value")

f test_json_stringify_pretty() -> Bool
    json := json_object()
    json = json_set(json, "formatted", json_from_bool(true))
    str := json_stringify_pretty(json)
    # Pretty print should have newlines
    str_contains(str, "formatted")

# ============================================================
# Test Stdlib Utility Functions (from std/json.forma)
# ============================================================

us std.json

f test_json_get_str_or() -> Bool
    json_str := '{"name": "Dave"}'
    m json_parse(json_str)
        Ok(json) ->
            # Existing key
            val1 := json_get_str_or(json, "name", "default")
            # Missing key
            val2 := json_get_str_or(json, "missing", "default")
            val1 == "Dave" && val2 == "default"
        Err(_) -> false

f test_json_get_int_or() -> Bool
    json_str := '{"count": 42}'
    m json_parse(json_str)
        Ok(json) ->
            val1 := json_get_int_or(json, "count", 0)
            val2 := json_get_int_or(json, "missing", 99)
            val1 == 42 && val2 == 99
        Err(_) -> false

f test_json_is_empty() -> Bool
    empty_obj := json_object()
    empty_arr := json_array()
    non_empty := json_set(json_object(), "x", json_from_int(1))

    json_is_empty(empty_obj) &&
        json_is_empty(empty_arr) &&
        !json_is_empty(non_empty)

f test_json_path() -> Bool
    json_str := '{"user": {"profile": {"name": "Eve"}}}'
    m json_parse(json_str)
        Ok(json) ->
            m json_path(json, "user.profile.name")
                Some(name_json) ->
                    json_type(name_json) == "string"
                None -> false
        Err(_) -> false

# ============================================================
# Run All Tests
# ============================================================

f run_all_tests() -> Int
    passed := 0
    total := 24

    # Parsing tests
    if test_json_parse_object() { passed = passed + 1; print("PASS: parse_object") } else { print("FAIL: parse_object") }
    if test_json_parse_array() { passed = passed + 1; print("PASS: parse_array") } else { print("FAIL: parse_array") }
    if test_json_parse_nested() { passed = passed + 1; print("PASS: parse_nested") } else { print("FAIL: parse_nested") }
    if test_json_parse_invalid() { passed = passed + 1; print("PASS: parse_invalid") } else { print("FAIL: parse_invalid") }

    # Creation tests
    if test_json_from_str() { passed = passed + 1; print("PASS: from_str") } else { print("FAIL: from_str") }
    if test_json_from_int() { passed = passed + 1; print("PASS: from_int") } else { print("FAIL: from_int") }
    if test_json_from_float() { passed = passed + 1; print("PASS: from_float") } else { print("FAIL: from_float") }
    if test_json_from_bool() { passed = passed + 1; print("PASS: from_bool") } else { print("FAIL: from_bool") }
    if test_json_null() { passed = passed + 1; print("PASS: null") } else { print("FAIL: null") }
    if test_json_object_empty() { passed = passed + 1; print("PASS: object_empty") } else { print("FAIL: object_empty") }
    if test_json_array_empty() { passed = passed + 1; print("PASS: array_empty") } else { print("FAIL: array_empty") }

    # Manipulation tests
    if test_json_set() { passed = passed + 1; print("PASS: set") } else { print("FAIL: set") }
    if test_json_keys() { passed = passed + 1; print("PASS: keys") } else { print("FAIL: keys") }
    if test_json_values() { passed = passed + 1; print("PASS: values") } else { print("FAIL: values") }
    if test_json_has() { passed = passed + 1; print("PASS: has") } else { print("FAIL: has") }
    if test_json_array_get() { passed = passed + 1; print("PASS: array_get") } else { print("FAIL: array_get") }

    # Type tests
    if test_json_type() { passed = passed + 1; print("PASS: type") } else { print("FAIL: type") }
    if test_json_is_predicates() { passed = passed + 1; print("PASS: is_predicates") } else { print("FAIL: is_predicates") }

    # Stringify tests
    if test_json_stringify() { passed = passed + 1; print("PASS: stringify") } else { print("FAIL: stringify") }
    if test_json_stringify_pretty() { passed = passed + 1; print("PASS: stringify_pretty") } else { print("FAIL: stringify_pretty") }

    # Stdlib utility tests
    if test_json_get_str_or() { passed = passed + 1; print("PASS: get_str_or") } else { print("FAIL: get_str_or") }
    if test_json_get_int_or() { passed = passed + 1; print("PASS: get_int_or") } else { print("FAIL: get_int_or") }
    if test_json_is_empty() { passed = passed + 1; print("PASS: is_empty") } else { print("FAIL: is_empty") }
    if test_json_path() { passed = passed + 1; print("PASS: path") } else { print("FAIL: path") }

    print("")
    print("JSON tests: " + int_to_str(passed) + "/" + int_to_str(total) + " passed")

    if passed == total { 0 } else { 1 }

f main() -> Int
    run_all_tests()
```

**Acceptance Criteria:**
- [ ] All 24 JSON tests pass
- [ ] Stdlib json functions work via import
- [ ] Error cases handled properly

---

## Task 14.7.6: Document Known Limitations

**File:** `KNOWN_LIMITATIONS.md` (NEW FILE)

Create a document listing known limitations for v1.0:

```markdown
# FORMA v1.0 Known Limitations

This document lists known limitations in FORMA v1.0. These are tracked for future releases.

## Language Features

### Struct Update Syntax
The struct update syntax `{ ..base, field: value }` is parsed but not fully implemented in MIR lowering.
- **Workaround:** Create a new struct with all fields specified.

### Loop Labels
Loop labels for `break` and `continue` are parsed but not implemented.
- **Workaround:** Restructure code to avoid needing labeled breaks.

### Indirect Closure Calls
LLVM codegen doesn't support indirect calls for closures stored in variables.
- **Workaround:** Use direct function calls or interpreter mode.

## Tooling

### LSP Go-to-Definition
The `textDocument/definition` LSP method returns empty results.
- **Status:** Placeholder implementation, will be completed in v1.1.

### Formatter Completeness
The formatter handles most constructs but may output `?` for complex patterns.
- **Workaround:** Review formatted output before committing.

## Standard Library

### Async is Synchronous
The `sp` (spawn) and `aw` (await) keywords work but execute synchronously.
True parallelism is not implemented.
- **Note:** Useful for structuring async code, actual parallelism in v1.1.

## Type System

### Trait Bound Checking
Generic functions with trait bounds parse but bounds aren't fully checked.
- **Workaround:** Rely on runtime errors for now.

---

*These limitations are tracked and planned for future releases.*
```

---

## Task 14.7.7: Final Verification Script

**Create:** `scripts/verify_v1.sh`

```bash
#!/bin/bash
set -e

echo "=========================================="
echo "FORMA v1.0 Final Verification"
echo "=========================================="
echo ""

cd "$(dirname "$0")/.."

echo "=== 1. Cargo Build ==="
cargo build --release
echo "✓ Build successful"
echo ""

echo "=== 2. Rust Unit Tests ==="
cargo test
echo "✓ All Rust tests pass"
echo ""

echo "=== 3. FORMA Integration Tests ==="
FORMA="cargo run --quiet --"
PASSED=0
FAILED=0
for f in tests/forma/*.forma; do
    if $FORMA run "$f" > /dev/null 2>&1; then
        echo "✓ $f"
        PASSED=$((PASSED + 1))
    else
        echo "✗ $f"
        FAILED=$((FAILED + 1))
    fi
done
echo "Integration tests: $PASSED passed, $FAILED failed"
echo ""

echo "=== 4. Examples Compile Check ==="
for f in examples/*.forma; do
    if $FORMA check "$f" > /dev/null 2>&1; then
        echo "✓ $f compiles"
    else
        echo "✗ $f FAILED"
        FAILED=$((FAILED + 1))
    fi
done
echo ""

echo "=== 5. Stdlib Modules ==="
for f in std/*.forma; do
    if $FORMA check "$f" > /dev/null 2>&1; then
        echo "✓ $f"
    else
        echo "✗ $f FAILED"
        FAILED=$((FAILED + 1))
    fi
done
echo ""

echo "=== 6. Import System ==="
cat > /tmp/test_imports.forma << 'EOF'
us std.core
us std.json
us std.vec

f main() -> Int
    print("Imports work!")
    0
EOF
if $FORMA run /tmp/test_imports.forma > /dev/null 2>&1; then
    echo "✓ Import system working"
else
    echo "✗ Import system FAILED"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 7. JSON Complete Test ==="
if $FORMA run tests/forma/test_json_complete.forma; then
    echo "✓ JSON fully functional"
else
    echo "✗ JSON tests failed"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 8. Async Test ==="
if $FORMA run tests/forma/test_async_spawn.forma > /dev/null 2>&1; then
    echo "✓ Async/spawn working"
else
    echo "✗ Async tests failed"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 9. Grammar Export ==="
if $FORMA grammar > /dev/null 2>&1; then
    echo "✓ Grammar export working"
else
    echo "✗ Grammar export failed"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=== 10. Formatter ==="
if $FORMA fmt examples/hello.forma > /dev/null 2>&1; then
    echo "✓ Formatter working"
else
    echo "✗ Formatter failed"
    FAILED=$((FAILED + 1))
fi
echo ""

echo "=========================================="
if [ $FAILED -eq 0 ]; then
    echo "✓ ALL CHECKS PASSED - Ready for v1.0!"
else
    echo "✗ $FAILED checks failed - NOT ready for release"
    exit 1
fi
echo "=========================================="
```

---

## Definition of Done

- [ ] No `.unwrap()` on call_stack in interp.rs
- [ ] No `.unwrap()` on current_block/current_fn in lower.rs
- [ ] No `.unwrap()` on scope_stack in borrow/checker.rs
- [ ] String/Bool/Char/Float literal patterns work in match expressions
- [ ] test_json_complete.forma passes (24/24 tests)
- [ ] KNOWN_LIMITATIONS.md created
- [ ] verify_v1.sh runs successfully
- [ ] All existing tests still pass

---

## Summary

| Task | Description | Effort |
|------|-------------|--------|
| 14.7.1 | Fix interp.rs unwraps | 1 hour |
| 14.7.2 | Fix lower.rs unwraps | 1 hour |
| 14.7.3 | Fix borrow checker unwraps | 30 min |
| 14.7.4 | Fix literal pattern matching (String/Bool/Char/Float) | 1 hour |
| 14.7.5 | Complete JSON test | 30 min |
| 14.7.6 | Document limitations | 30 min |
| 14.7.7 | Verification script | 30 min |
| **Total** | | **~5 hours** |

---

*"No panics, no silent failures, no surprises."*
