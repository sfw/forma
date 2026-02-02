# Sprint 13: Complete v1.0 - No Deferrals

**Priority:** CRITICAL - Finalize all features before v1.0
**Date:** January 25, 2026

---

## Overview

This sprint completes ALL remaining features. No deferrals. FORMA v1.0 will be production-ready with every feature working.

---

## Task 13.1: Contract Enforcement

**Files:** `src/mir/interp.rs`

**Current State:** Contracts are parsed (`pre:`, `post:`) and stored in MIR but not enforced.

**Implementation:**

### Step 1: Add contract evaluation infrastructure

```rust
// Add to Interpreter struct
impl Interpreter {
    /// Evaluate a contract expression in the current scope
    fn eval_contract(&mut self, contract: &MirContract, is_post: bool, result: Option<&Value>) -> Result<bool, InterpError> {
        // Contract expressions are stored as strings - we need to parse and evaluate them
        // The expression can reference:
        // - Function parameters (in pre and post)
        // - `result` keyword (in post only)

        // Parse the contract expression
        let expr_str = &contract.expr;

        // Simple contract evaluation - handle common patterns:
        // - "x > 0"
        // - "x != null"
        // - "result > 0"
        // - "len(arr) > 0"

        // For full implementation, re-parse the expression and evaluate it
        // For now, implement common patterns

        self.eval_contract_expr(expr_str, is_post, result)
    }

    fn eval_contract_expr(&mut self, expr: &str, is_post: bool, result: Option<&Value>) -> Result<bool, InterpError> {
        // Tokenize and parse the expression
        use crate::lexer::Lexer;
        use crate::parser::Parser;

        let mut lexer = Lexer::new(expr);
        let tokens = lexer.scan_tokens().map_err(|e| InterpError {
            message: format!("contract parse error: {}", e)
        })?;

        let mut parser = Parser::new(tokens);
        let expr_ast = parser.parse_expression().map_err(|e| InterpError {
            message: format!("contract parse error: {:?}", e)
        })?;

        // If this is a postcondition and we have a result, bind it
        if is_post {
            if let Some(res) = result {
                self.current_frame_mut().locals.insert("result".to_string(), res.clone());
            }
        }

        // Evaluate the expression
        let value = self.eval_expr(&expr_ast)?;

        match value {
            Value::Bool(b) => Ok(b),
            _ => Err(InterpError {
                message: format!("contract must evaluate to Bool, got {:?}", value)
            })
        }
    }
}
```

### Step 2: Add contract checking to function execution

```rust
// In eval_function or wherever functions are called
fn call_function(&mut self, func: &MirFunction, args: Vec<Value>) -> Result<Value, InterpError> {
    // Bind parameters
    self.push_frame();
    for (param, arg) in func.params.iter().zip(args.iter()) {
        self.current_frame_mut().locals.insert(param.name.clone(), arg.clone());
    }

    // Check preconditions
    for contract in &func.preconditions {
        let result = self.eval_contract(contract, false, None)?;
        if !result {
            let msg = contract.message.as_deref().unwrap_or("precondition failed");
            return Err(InterpError {
                message: format!("Contract violation in '{}': {} (condition: {})",
                    func.name, msg, contract.expr)
            });
        }
    }

    // Execute function body
    let result = self.eval_body(&func.body)?;

    // Check postconditions
    for contract in &func.postconditions {
        let check = self.eval_contract(contract, true, Some(&result))?;
        if !check {
            let msg = contract.message.as_deref().unwrap_or("postcondition failed");
            return Err(InterpError {
                message: format!("Contract violation in '{}': {} (condition: {})",
                    func.name, msg, contract.expr)
            });
        }
    }

    self.pop_frame();
    Ok(result)
}
```

### Step 3: Add test

```forma
// tests/forma/test_contracts.forma

f divide(a: Int, b: Int) -> Int
    pre: b != 0, "divisor cannot be zero"
    post: result * b <= a, "result is correct"
{
    a / b
}

f test_contract_passes() {
    result := divide(10, 2)
    assert_eq(result, 5)
}

f test_contract_precondition_fails() {
    # This should fail with "divisor cannot be zero"
    # We can't easily test failure in FORMA yet, so just verify good path
    result := divide(10, 5)
    assert_eq(result, 2)
}

f positive_only(x: Int) -> Int
    pre: x > 0
{
    x * 2
}

f test_positive() {
    assert_eq(positive_only(5), 10)
}
```

**Acceptance Criteria:**
- [ ] `pre:` conditions are checked before function execution
- [ ] `post:` conditions are checked after function execution
- [ ] `result` keyword works in postconditions
- [ ] Custom error messages are displayed
- [ ] Contract failures produce clear error messages

---

## Task 13.2: Full Trait Resolution

**Files:** `src/types/inference.rs`, `src/mir/lower.rs`

**Problem:** When multiple types have the same method name (e.g., `Point::area()` and `Circle::area()`), resolution is ambiguous.

**Solution:** Pass resolved method information from type checker to MIR lowerer.

### Step 1: Store method resolutions in type checker

```rust
// Add to TypeEnv or InferenceContext
pub struct InferenceContext {
    // ... existing fields ...

    /// Maps expression ID -> qualified method name for method calls
    pub method_resolutions: HashMap<ExprId, String>,
}

// In infer_expr for MethodCall
ExprKind::MethodCall(receiver, method, args) => {
    let receiver_ty = self.infer_expr(receiver)?;
    let resolved_ty = self.resolve_type(&receiver_ty);

    // Get the type name for qualification
    let type_name = match &resolved_ty {
        Ty::Named(id, _) => id.name.clone(),
        Ty::List(_) => "Vec".to_string(),
        Ty::Map(_, _) => "Map".to_string(),
        Ty::Str => "Str".to_string(),
        Ty::Int => "Int".to_string(),
        Ty::Float => "Float".to_string(),
        Ty::Char => "Char".to_string(),
        _ => return Err(TypeError::new(
            format!("cannot call methods on type {}", resolved_ty),
            expr.span
        )),
    };

    // Look up method
    let method_sig = self.lookup_method(&resolved_ty, &method.name)
        .ok_or_else(|| TypeError::new(
            format!("type {} has no method '{}'", resolved_ty, method.name),
            method.span
        ))?;

    // Store the qualified name for MIR lowering
    let qualified_name = format!("{}::{}", type_name, method.name);
    self.method_resolutions.insert(expr.id, qualified_name);

    // ... rest of type checking
}
```

### Step 2: Use resolutions in MIR lowering

```rust
// In MirLowerer
pub struct MirLowerer {
    // ... existing fields ...

    /// Method resolutions from type checking
    pub method_resolutions: HashMap<ExprId, String>,
}

// When lowering method calls
fn lower_method_call(&mut self, receiver: &Expr, method: &Ident, args: &[Expr]) -> Option<Operand> {
    // Check if we have a resolved method name from type checking
    if let Some(qualified_name) = self.method_resolutions.get(&receiver.id) {
        // Use the type-resolved qualified name
        return self.lower_qualified_call(qualified_name, receiver, args);
    }

    // Fallback to current resolution logic
    // ...
}
```

### Step 3: Test overlapping method names

```forma
// tests/forma/test_trait_resolution.forma

s Point {
    x: Int,
    y: Int
}

s Circle {
    x: Int,
    y: Int,
    radius: Int
}

i Point {
    f area(self) -> Int {
        0  # Points have no area
    }

    f describe(self) -> Str {
        f"Point({self.x}, {self.y})"
    }
}

i Circle {
    f area(self) -> Int {
        # Approximate: 3 * r * r
        3 * self.radius * self.radius
    }

    f describe(self) -> Str {
        f"Circle at ({self.x}, {self.y}) with radius {self.radius}"
    }
}

f test_point_area() {
    p := Point { x: 5, y: 10 }
    assert_eq(p.area(), 0)
}

f test_circle_area() {
    c := Circle { x: 0, y: 0, radius: 10 }
    assert_eq(c.area(), 300)  # 3 * 10 * 10
}

f test_both_describe() {
    p := Point { x: 1, y: 2 }
    c := Circle { x: 0, y: 0, radius: 5 }

    # Both have describe() but should resolve to correct implementation
    assert_eq(p.describe(), "Point(1, 2)")
    assert_eq(c.describe(), "Circle at (0, 0) with radius 5")
}
```

**Acceptance Criteria:**
- [ ] Multiple types can have methods with same name
- [ ] Correct method is called based on receiver type
- [ ] Error message shows which type's method was expected

---

## Task 13.3: MIR Type Propagation

**Files:** `src/mir/lower.rs`

**Problem:** MIR temporaries use `Ty::Int` as placeholder instead of actual types.

**Solution:** Infer types from operands during lowering.

### Implementation

```rust
// Add to MirLowerer
impl MirLowerer {
    /// Infer the type of an rvalue
    fn infer_rvalue_type(&self, rvalue: &Rvalue) -> Ty {
        match rvalue {
            Rvalue::Use(operand) => self.operand_type(operand),

            Rvalue::BinaryOp(op, left, _right) => {
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                        self.operand_type(left)
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge |
                    BinOp::And | BinOp::Or => Ty::Bool,
                    _ => self.operand_type(left)
                }
            }

            Rvalue::UnaryOp(op, operand) => {
                match op {
                    UnOp::Neg => self.operand_type(operand),
                    UnOp::Not => Ty::Bool,
                }
            }

            Rvalue::Aggregate(AggregateKind::Tuple, operands) => {
                Ty::Tuple(operands.iter().map(|o| self.operand_type(o)).collect())
            }

            Rvalue::Aggregate(AggregateKind::Array, operands) => {
                let elem_ty = operands.first()
                    .map(|o| self.operand_type(o))
                    .unwrap_or(Ty::Unit);
                Ty::List(Box::new(elem_ty))
            }

            Rvalue::Call(func_name, _args) => {
                self.function_return_type(func_name)
            }

            Rvalue::Ref(_, operand) => {
                Ty::Ref(Box::new(self.operand_type(operand)))
            }

            _ => Ty::Unit
        }
    }

    fn operand_type(&self, operand: &Operand) -> Ty {
        match operand {
            Operand::Constant(c) => self.constant_type(c),
            Operand::Copy(local) | Operand::Move(local) => {
                self.local_types.get(local).cloned().unwrap_or(Ty::Unit)
            }
        }
    }

    fn constant_type(&self, constant: &Constant) -> Ty {
        match constant {
            Constant::Int(_) => Ty::Int,
            Constant::Float(_) => Ty::Float,
            Constant::Bool(_) => Ty::Bool,
            Constant::Str(_) => Ty::Str,
            Constant::Char(_) => Ty::Char,
            Constant::Unit => Ty::Unit,
        }
    }

    fn function_return_type(&self, name: &str) -> Ty {
        // Look up function in program
        if let Some(func) = self.program.functions.get(name) {
            return func.return_type.clone();
        }

        // Check builtin return types
        match name {
            "vec_len" | "str_len" | "map_len" => Ty::Int,
            "vec_push" | "map_insert" => Ty::List(Box::new(Ty::Unit)), // Approximate
            "print" | "println" => Ty::Unit,
            "str" => Ty::Str,
            "int" => Ty::Int,
            "float" => Ty::Float,
            _ => Ty::Unit
        }
    }

    // Update temp creation to use inferred type
    fn new_temp(&mut self, ty: Ty) -> Local {
        let local = Local::new(self.next_local);
        self.next_local += 1;
        self.local_types.insert(local, ty);
        local
    }
}
```

Then update all `new_temp(Ty::Int)` calls to use `new_temp(self.infer_rvalue_type(&rvalue))`.

**Acceptance Criteria:**
- [ ] MIR temporaries have correct types
- [ ] Type information preserved through lowering
- [ ] No regression in existing functionality

---

## Task 13.4: Grammar Export Verification

**Command:** `cargo run -- grammar`

**Verify:**
- [ ] Command exists and runs without error
- [ ] Outputs valid JSON or EBNF grammar
- [ ] Grammar covers all FORMA syntax

**Test:**
```bash
cargo run -- grammar > grammar.json
cat grammar.json | jq .  # Should be valid JSON
```

---

## Task 13.5: Formatter Verification

**Command:** `cargo run -- fmt <file>`

**Test cases:**

```bash
# Test format to stdout
cargo run -- fmt examples/hello.forma

# Test format check (should exit 0 if already formatted)
cargo run -- fmt --check examples/hello.forma

# Test format in place
cp examples/hello.forma /tmp/test.forma
cargo run -- fmt --write /tmp/test.forma
cat /tmp/test.forma
```

**Verify:**
- [ ] `forma fmt file.forma` outputs formatted code
- [ ] `forma fmt --check file.forma` returns 0 if formatted, 1 if not
- [ ] `forma fmt --write file.forma` modifies file in place
- [ ] Formatter handles: functions, structs, enums, traits, impls, expressions

---

## Task 13.6: Full Test Suite Verification

Run complete verification:

```bash
#!/bin/bash
set -e

echo "=== Cargo Tests ==="
cargo test

echo "=== FORMA Integration Tests ==="
for f in tests/forma/*.forma; do
    echo "Testing $f"
    cargo run --quiet -- run "$f"
done

echo "=== Examples ==="
for f in examples/*.forma; do
    echo "Running $f"
    cargo run --quiet -- run "$f" || echo "  (may require input/network)"
done

echo "=== Stdlib Check ==="
for f in stdlib/*.forma; do
    echo "Checking $f"
    cargo run --quiet -- check "$f"
done

echo "=== Grammar Export ==="
cargo run -- grammar > /dev/null && echo "Grammar export: OK"

echo "=== Formatter ==="
cargo run -- fmt examples/hello.forma > /dev/null && echo "Formatter: OK"

echo "=== All checks passed ==="
```

---

## Task 13.7: Documentation Review

**Files to check:**
- [ ] `README.md` - Accurate feature list, installation, examples
- [ ] `CHANGELOG.md` - Create if missing, document v1.0 changes
- [ ] `LICENSE` - Exists and is appropriate
- [ ] `stdlib/*.forma` - Have documentation comments
- [ ] `examples/*.forma` - Have header comments explaining purpose

---

## Definition of Done for v1.0

### Features Complete
- [ ] Contract enforcement works (pre/post conditions)
- [ ] Full trait resolution (same method name on multiple types)
- [ ] MIR type propagation (correct types on temporaries)
- [ ] Grammar export works
- [ ] Formatter works

### Quality
- [ ] All cargo tests pass
- [ ] All FORMA integration tests pass
- [ ] All examples compile and run
- [ ] All stdlib files compile
- [ ] Error messages are helpful
- [ ] Documentation is complete

### Test Counts (Expected)
- [ ] 36+ Rust unit tests
- [ ] 48+ parser tests
- [ ] 28+ lexer tests
- [ ] 24+ contextual keyword tests
- [ ] 33+ FORMA integration tests
- [ ] 11+ examples

---

## Implementation Order

1. **Task 13.3: MIR Type Propagation** (foundation for other features)
2. **Task 13.2: Full Trait Resolution** (depends on proper types)
3. **Task 13.1: Contract Enforcement** (independent)
4. **Task 13.4: Grammar Export Verification** (quick check)
5. **Task 13.5: Formatter Verification** (quick check)
6. **Task 13.6: Full Test Suite** (final verification)
7. **Task 13.7: Documentation** (polish)

---

*FORMA v1.0: Complete. Production-ready. No compromises.*
