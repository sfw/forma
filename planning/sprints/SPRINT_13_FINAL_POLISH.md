# Sprint 13: Final Polish for v1.0

**Priority:** Complete before v1.0 release
**Date:** January 25, 2026

---

## Overview

This sprint covers the remaining items needed for a production-ready v1.0 release. All critical issues from the code review have been addressed. These are the final polish items.

---

## Current Status Summary

### Already Complete ✅

| Sprint | Status |
|--------|--------|
| Sprint 9 (Type Safety) | ✅ 5/5 - Method/field checking, patterns, enums, Option/Result |
| Sprint 10 (Stdlib & Examples) | ✅ 4/4 - Prepared statements, map module, examples fixed |
| Sprint 11 (Polish) | ✅ 3/4 - Error recovery, short-circuit, %= operator |
| Sprint 12 (Contextual Keywords) | ✅ Complete - m/s/f/e/t/i work as identifiers |

### Deferred to v1.1

- **MIR Type Propagation** - Not needed for interpreter, only for LLVM codegen

---

## Sprint 13 Tasks

### Task 13.1: Naming Convention Audit ✅ (Already Done)

**Status:** Already implemented via aliasing in `src/mir/lower.rs`

The method resolution already maps short names to canonical names:
```rust
"is_digit" => "char_is_digit"
"is_alpha" => "char_is_alpha"
"is_alphanumeric" => "char_is_alphanumeric"
"is_whitespace" => "char_is_whitespace"
```

**Action needed:** Just verify the stdlib wrappers in `stdlib/string.forma` are documented properly.

**Acceptance Criteria:**
- [x] `is_digit(c)` works (via builtin alias)
- [x] `char_is_digit(c)` works (canonical name)
- [x] Both are documented in stdlib

---

### Task 13.2: Contract Enforcement (Optional for v1.0)

**File:** `src/mir/interp.rs`

**Current State:** Contracts are parsed (`pre:`, `post:`) and stored in MIR but not enforced at runtime.

**Decision:** This is an advanced feature. For v1.0, contracts can remain as documentation. Mark as v1.1 feature.

**If implementing for v1.0:**

```rust
// Add to Interpreter
fn check_preconditions(&mut self, func: &MirFunction, args: &[Value]) -> Result<(), InterpError> {
    for contract in &func.preconditions {
        // Create a temporary scope with parameter bindings
        let mut contract_scope = HashMap::new();
        for (param, arg) in func.params.iter().zip(args) {
            contract_scope.insert(param.name.clone(), arg.clone());
        }

        // Parse and evaluate the contract expression
        // Note: This requires parsing the contract.expr string at runtime
        // which is complex - may need to store pre-parsed expressions

        // Simplified: just check if contract string contains known patterns
        // Full implementation would need expression evaluation
    }
    Ok(())
}

// Call at function entry in eval_function()
fn eval_function(&mut self, func: &MirFunction, args: Vec<Value>) -> Result<Value, InterpError> {
    self.check_preconditions(func, &args)?;
    // ... existing code ...
    let result = self.eval_body()?;
    self.check_postconditions(func, &result)?;
    Ok(result)
}
```

**Recommendation:** Defer to v1.1. Contracts as documentation are valuable even without runtime enforcement.

---

### Task 13.3: Trait Method Resolution Improvement

**File:** `src/mir/lower.rs` (resolve_method function)

**Current State:** Works when method names are unique across types. If multiple types have same method name (e.g., both `Point` and `Circle` have `area()`), resolution is ambiguous.

**Current workaround in code (lines 1506-1519):**
```rust
if let Some(qualified_names) = self.impl_methods.get(method_name) {
    if qualified_names.len() == 1 {
        return qualified_names[0].clone();
    }
    // Multiple types have this method - check which are available
    // This is a heuristic; proper resolution needs type info
    for qname in qualified_names {
        if self.program.functions.contains_key(qname) {
            return qname.clone();
        }
    }
}
```

**Proper Fix:**
The type checker (Sprint 9) now determines the correct method. The fix is to pass the resolved method name from type checking to MIR lowering.

**Option A: Type Checker Rewrites Method Calls**
During type inference, when we resolve `point.area()`, rewrite the AST to use qualified name `Point::area`.

**Option B: Pass Type Info to Lowerer**
Include receiver type in the MIR method call, allowing lowerer to select correct implementation.

**Implementation (Option A - simpler):**

```rust
// In src/types/inference.rs, after resolving method type
ExprKind::MethodCall(receiver, method, args) => {
    let receiver_ty = self.infer_expr(receiver)?;
    let resolved_ty = self.resolve_type(&receiver_ty);

    // Look up method and get qualified name
    let (method_sig, qualified_name) = self.lookup_method_with_name(&resolved_ty, &method.name)
        .ok_or_else(|| TypeError::new(...))?;

    // Store the qualified name for MIR lowering
    self.method_resolutions.insert(expr.id, qualified_name);

    // ... rest of type checking
}
```

Then in MIR lowering, check `method_resolutions` map first.

**Acceptance Criteria:**
- [ ] `Point::area()` and `Circle::area()` can coexist
- [ ] Calling `p.area()` on a Point calls `Point::area`
- [ ] Calling `c.area()` on a Circle calls `Circle::area`
- [ ] Error if method doesn't exist on type

---

### Task 13.4: Documentation and Examples Review

**Files:** `README.md`, `examples/*.forma`, `stdlib/*.forma`

**Checklist:**
- [ ] README has accurate feature list
- [ ] All examples compile and run correctly
- [ ] Stdlib modules have documentation comments
- [ ] CHANGELOG.md exists with v1.0 changes
- [ ] LICENSE file exists

---

### Task 13.5: Test Coverage Verification

**Current State:**
- 36 Rust unit tests
- 48 parser tests
- 28 lexer tests
- 24 contextual keyword tests
- 33 FORMA integration tests
- 11 examples

**Action:**
```bash
# Run all tests
cargo test

# Run all examples
for f in examples/*.forma; do
    echo "Testing $f"
    cargo run --quiet -- run "$f"
done

# Check all stdlib files compile
for f in stdlib/*.forma; do
    echo "Checking $f"
    cargo run --quiet -- check "$f"
done
```

**Acceptance Criteria:**
- [ ] All cargo tests pass
- [ ] All examples run without error
- [ ] All stdlib files compile without error

---

### Task 13.6: Error Message Quality

**Spot check these error scenarios produce helpful messages:**

```forma
# Unknown method
v := [1, 2, 3]
v.nonexistent()
# Expected: "type 'Vec[Int]' has no method 'nonexistent'. Available methods: push, pop, len, ..."

# Unknown field
s Point { x: Int, y: Int }
p := Point { x: 1, y: 2 }
p.z
# Expected: "type 'Point' has no field 'z'. Available fields: x, y"

# Type mismatch
f add(a: Int, b: Int) -> Int { a + b }
add("hello", 42)
# Expected: "type mismatch: expected Int, found Str"

# Missing field in struct pattern
m p {
    Point { x } => x
}
# Expected: "pattern missing field 'y' (use .. to ignore)"
```

---

## Priority Order

1. **Task 13.5: Test Coverage** - Verify everything works (30 min)
2. **Task 13.4: Documentation** - Polish docs and examples (1 hour)
3. **Task 13.6: Error Messages** - Spot check quality (30 min)
4. **Task 13.3: Trait Resolution** - Only if multiple types with same method name is needed (2-4 hours)
5. **Task 13.2: Contracts** - Defer to v1.1

---

## Definition of Done for v1.0

- [ ] All tests pass (cargo test)
- [ ] All examples compile and run
- [ ] All stdlib files compile
- [ ] README is accurate
- [ ] Error messages are helpful
- [ ] No critical bugs remaining

---

## v1.1 Roadmap (Post-Release)

Items deferred from v1.0:
1. **MIR Type Propagation** - For LLVM backend
2. **Contract Enforcement** - Runtime pre/post condition checking
3. **Full Trait Resolution** - Type-aware method dispatch for overlapping names
4. **LLVM Backend** - Native compilation
5. **Package Manager** - Dependency management

---

*FORMA v1.0: Production-ready AI-optimized language*
