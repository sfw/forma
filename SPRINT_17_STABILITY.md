# Sprint 17: Stability & Robustness

**Goal:** Fix critical and high-priority issues identified in Code Review V3
**Target:** Zero panics in production code paths

---

## Task 17.1: LLVM Codegen Safety

### 17.1.1: Replace panic-inducing type coercions

**File:** `src/codegen/llvm.rs`

Replace all `into_*_value()` calls with safe pattern matching:

```rust
// REPLACE (line 333-334):
let lhs_int = lhs.into_int_value();
let rhs_int = rhs.into_int_value();

// WITH:
fn as_int_value(val: BasicValueEnum<'ctx>) -> Result<IntValue<'ctx>, CodegenError> {
    match val {
        BasicValueEnum::IntValue(i) => Ok(i),
        _ => Err(CodegenError { message: format!("Expected integer, got {:?}", val) }),
    }
}

let lhs_int = self.as_int_value(lhs)?;
let rhs_int = self.as_int_value(rhs)?;
```

Add similar helpers:
```rust
fn as_float_value(val: BasicValueEnum<'ctx>) -> Result<FloatValue<'ctx>, CodegenError>;
fn as_struct_value(val: BasicValueEnum<'ctx>) -> Result<StructValue<'ctx>, CodegenError>;
fn as_pointer_value(val: BasicValueEnum<'ctx>) -> Result<PointerValue<'ctx>, CodegenError>;
```

### 17.1.2: Fix environment size unwrap

**File:** `src/codegen/llvm.rs` line 425

```rust
// REPLACE:
let env_size = env_struct_type.size_of().unwrap();

// WITH:
let env_size = env_struct_type.size_of()
    .ok_or_else(|| CodegenError { message: "Cannot get size of unsized closure environment".to_string() })?;
```

### 17.1.3: Add null check after malloc

**File:** `src/codegen/llvm.rs` after line 432

```rust
// ADD after malloc call:
let is_null = self.builder.build_is_null(env_ptr_i8, "is_null")?;
let null_block = self.context.append_basic_block(current_fn, "malloc_null");
let ok_block = self.context.append_basic_block(current_fn, "malloc_ok");
self.builder.build_conditional_branch(is_null, null_block, ok_block)?;

// In null_block: return error or panic gracefully
self.builder.position_at_end(null_block);
// ... handle allocation failure ...

self.builder.position_at_end(ok_block);
// ... continue with env_ptr_i8 ...
```

---

## Task 17.2: Interpreter Safety

### 17.2.1: Replace Tokio runtime expect

**File:** `src/mir/interp.rs` line 343

```rust
// REPLACE:
runtime: Arc::new(tokio::runtime::Runtime::new()
    .expect("failed to create Tokio runtime")),

// WITH:
runtime: Arc::new(tokio::runtime::Runtime::new()
    .map_err(|e| RuntimeError::new(format!("Failed to create Tokio runtime: {}", e)))?),

// This requires changing Interpreter::new() to return Result
```

### 17.2.2: Fix compression unwraps

**File:** `src/mir/interp.rs` lines 5514-5515, 5552-5553

```rust
// REPLACE gzip_compress:
("gzip_compress", [Value::Str(data)]) => {
    let mut encoder = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
    encoder.write_all(data.as_bytes())
        .map_err(|e| RuntimeError::new(format!("gzip compression failed: {}", e)))?;
    let compressed = encoder.finish()
        .map_err(|e| RuntimeError::new(format!("gzip finalization failed: {}", e)))?;
    Ok(Some(Value::Bytes(compressed)))
}
```

### 17.2.3: Fix mutex unwraps

**File:** `src/mir/interp.rs` lines 973, 992, 2385, 2429

```rust
// REPLACE all occurrences:
let mut tasks = spawned_tasks.lock().unwrap();

// WITH:
let mut tasks = spawned_tasks.lock()
    .map_err(|_| RuntimeError::new("Task registry mutex poisoned"))?;
```

### 17.2.4: Fix database unwrap

**File:** `src/mir/interp.rs` line 5789

```rust
// REPLACE:
let conn = self.databases.get(&db_id).unwrap();

// WITH:
let conn = self.databases.get(&db_id)
    .ok_or_else(|| RuntimeError::new(format!("Invalid database handle: {}", db_id)))?;
```

---

## Task 17.3: Parser/Lexer Safety

### 17.3.1: Fix indent_stack unwraps

**File:** `src/lexer/scanner.rs` lines 310, 318

```rust
// REPLACE:
let current_indent = *self.indent_stack.last().unwrap();

// WITH:
let current_indent = *self.indent_stack.last()
    .ok_or_else(|| ScanError::new("Internal error: empty indent stack", self.current_position()))?;
```

### 17.3.2: Fix f-string advance unwraps

**File:** `src/lexer/scanner.rs` lines 823, 827, 830

```rust
// REPLACE:
self.advance().unwrap()

// WITH (inside match arm that already checked peek()):
self.advance()
    .ok_or_else(|| ScanError::new("Unexpected end of f-string", self.current_position()))?
```

---

## Task 17.4: Loop Labels Fix

### 17.4.1: Set label in loop contexts

**File:** `src/mir/lower.rs`

Find all `LoopContext` creations and pass the label:

```rust
// REPLACE (around lines 1881, 2006, 2072, 2113):
self.loop_stack.push(LoopContext {
    label: None,  // BUG: Always None
    continue_block,
    break_block,
    result_local,
});

// WITH (for ForLoop):
self.loop_stack.push(LoopContext {
    label: for_loop.label.clone(),  // Pass label from AST
    continue_block,
    break_block,
    result_local,
});

// WITH (for WhileLoop):
self.loop_stack.push(LoopContext {
    label: while_loop.label.clone(),
    continue_block,
    break_block,
    result_local,
});
```

### 17.4.2: Fix break/continue label search

**File:** `src/mir/lower.rs` lines 813-848

The existing code already searches by label but since labels are always None, it never finds anything. The fix in 17.4.1 should make this work.

---

## Task 17.5: Type Inference Safety

### 17.5.1: Replace fresh_var fallbacks with errors

**File:** `src/types/inference.rs`

```rust
// REPLACE (line 3276-3283, field access on unresolved var):
Ty::Var(tv) => {
    if let Some(resolved) = self.unifier.subst.get(*tv) {
        self.lookup_field(resolved, field_name, span)
    } else {
        Ok(Ty::fresh_var())  // BAD: Hides error
    }
}

// WITH:
Ty::Var(tv) => {
    if let Some(resolved) = self.unifier.subst.get(*tv) {
        self.lookup_field(resolved, field_name, span)
    } else {
        Err(TypeError::new(
            format!("Cannot access field '{}' on unresolved type", field_name),
            span,
        ))
    }
}
```

Similar fixes for:
- Line 4048-4057 (tuple field access)
- Line 4348-4356 (path expressions)
- Line 4421-4437 (await expression)
- Line 4475-4478 (field/op shorthand)

### 17.5.2: Replace sentinel type variable IDs

**File:** `src/types/inference.rs` lines 2892-2893, 3027-3028

```rust
// REPLACE:
let t_var = Ty::Var(TypeVar { id: 99999 }); // Sentinel value

// WITH:
let t_var = Ty::fresh_var();  // Use proper fresh var generator
```

---

## Task 17.6: LLVM Missing Handlers

### 17.6.1: Add float binary operations

**File:** `src/codegen/llvm.rs` in `compile_binop`

```rust
fn compile_binop(&mut self, op: &BinOp, lhs: BasicValueEnum<'ctx>, rhs: BasicValueEnum<'ctx>)
    -> Result<BasicValueEnum<'ctx>, CodegenError>
{
    // Check if operands are floats
    if lhs.is_float_value() && rhs.is_float_value() {
        let lhs_float = lhs.into_float_value();
        let rhs_float = rhs.into_float_value();
        return match op {
            BinOp::Add => Ok(self.builder.build_float_add(lhs_float, rhs_float, "fadd")?.into()),
            BinOp::Sub => Ok(self.builder.build_float_sub(lhs_float, rhs_float, "fsub")?.into()),
            BinOp::Mul => Ok(self.builder.build_float_mul(lhs_float, rhs_float, "fmul")?.into()),
            BinOp::Div => Ok(self.builder.build_float_div(lhs_float, rhs_float, "fdiv")?.into()),
            BinOp::Rem => Ok(self.builder.build_float_rem(lhs_float, rhs_float, "frem")?.into()),
            BinOp::Eq => Ok(self.builder.build_float_compare(FloatPredicate::OEQ, lhs_float, rhs_float, "feq")?.into()),
            BinOp::Ne => Ok(self.builder.build_float_compare(FloatPredicate::ONE, lhs_float, rhs_float, "fne")?.into()),
            BinOp::Lt => Ok(self.builder.build_float_compare(FloatPredicate::OLT, lhs_float, rhs_float, "flt")?.into()),
            BinOp::Le => Ok(self.builder.build_float_compare(FloatPredicate::OLE, lhs_float, rhs_float, "fle")?.into()),
            BinOp::Gt => Ok(self.builder.build_float_compare(FloatPredicate::OGT, lhs_float, rhs_float, "fgt")?.into()),
            BinOp::Ge => Ok(self.builder.build_float_compare(FloatPredicate::OGE, lhs_float, rhs_float, "fge")?.into()),
            _ => Err(CodegenError { message: format!("Float operation not supported: {:?}", op) }),
        };
    }

    // Existing integer handling...
}
```

### 17.6.2: Add bitwise operations

**File:** `src/codegen/llvm.rs` in `compile_binop`

```rust
// ADD to integer operations match:
BinOp::BitAnd => Ok(self.builder.build_and(lhs_int, rhs_int, "bitand")?.into()),
BinOp::BitOr => Ok(self.builder.build_or(lhs_int, rhs_int, "bitor")?.into()),
BinOp::BitXor => Ok(self.builder.build_xor(lhs_int, rhs_int, "bitxor")?.into()),
BinOp::Shl => Ok(self.builder.build_left_shift(lhs_int, rhs_int, "shl")?.into()),
BinOp::Shr => Ok(self.builder.build_right_shift(lhs_int, rhs_int, false, "shr")?.into()),
```

---

## Task 17.7: Formatter Completeness

### 17.7.1: Add missing expression handlers

**File:** `src/fmt/mod.rs`

Replace the catch-all `_ => self.write("?")` with explicit handlers:

```rust
// ADD before the catch-all:
ExprKind::TupleField(base, idx) => {
    self.format_expr(base);
    self.write(&format!(".{}", idx));
}
ExprKind::Match(scrutinee, arms) => {
    self.write("m ");
    self.format_expr(scrutinee);
    self.newline();
    self.indent += 1;
    for arm in arms {
        self.write_indent();
        self.format_pattern(&arm.pattern);
        self.write(" -> ");
        self.format_expr(&arm.body);
        self.newline();
    }
    self.indent -= 1;
}
ExprKind::For(for_loop) => {
    self.write("fo ");
    self.format_pattern(&for_loop.pattern);
    self.write(" in ");
    self.format_expr(&for_loop.iterable);
    self.newline();
    self.indent += 1;
    self.format_expr(&for_loop.body);
    self.indent -= 1;
}
ExprKind::While(cond, body) => {
    self.write("wh ");
    self.format_expr(cond);
    self.newline();
    self.indent += 1;
    self.format_expr(body);
    self.indent -= 1;
}
ExprKind::Break(label, value) => {
    self.write("break");
    if let Some(l) = label {
        self.write(&format!(" '{}", l.name));
    }
    if let Some(v) = value {
        self.write(" ");
        self.format_expr(v);
    }
}
ExprKind::Continue(label) => {
    self.write("continue");
    if let Some(l) = label {
        self.write(&format!(" '{}", l.name));
    }
}
// ... add remaining handlers
```

---

## Verification

After each task, run:
```bash
cargo test
forma check std/*.forma
forma check examples/*.forma
```

All 250+ tests must pass before proceeding.

---

## Summary

| Task | Description | Priority |
|------|-------------|----------|
| 17.1 | LLVM Codegen Safety | CRITICAL |
| 17.2 | Interpreter Safety | CRITICAL |
| 17.3 | Parser/Lexer Safety | CRITICAL |
| 17.4 | Loop Labels Fix | HIGH |
| 17.5 | Type Inference Safety | HIGH |
| 17.6 | LLVM Missing Handlers | HIGH |
| 17.7 | Formatter Completeness | MEDIUM |

---

*"Safety first."*
