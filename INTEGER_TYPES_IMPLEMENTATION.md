# FORMA Integer Types & Trait Fixes Implementation

**Status:** In Progress
**Last Updated:** January 24, 2026

---

## PART 1: INTEGER TYPES

### Design Decision: Cast Syntax

Use **constructor-style casting**: `T(x)` where T is the target type.

This was chosen because:
- Fewest tokens (AI-optimized)
- Familiar from Python/C++
- Consistent with struct construction `Point(x: 1, y: 2)`
- No conflict with `as` keyword (used for async)

Examples:
```forma
v a: u8 = 255
v b = i32(a)           # Cast u8 to i32
v c = f64(42)          # Int to float
v d = i32(3.14)        # Truncates to 3
```

---

### Task 1.1: Add Integer Types to Type System

**File:** `src/types/types.rs`

Add to the `Ty` enum:
```rust
// Signed integers
I8,
I16,
I32,
I64,
// Unsigned integers
U8,
U16,
U32,
U64,
// Pointer-sized
Isize,
Usize,
// Floats
F32,
F64,
```

Update `Ty::Int` to be an alias for `I64` (or keep both and unify).
Update `Ty::Float` to be an alias for `F64`.

- [ ] Add new variants to Ty enum
- [ ] Update Display impl for Ty to print "i32", "u8", etc.
- [ ] Update type equality/unification to handle aliases

---

### Task 1.2: Update Lexer

**File:** `src/lexer/scanner.rs` or `src/lexer/token.rs`

Add type keywords that resolve to types:
- `i8`, `i16`, `i32`, `i64`
- `u8`, `u16`, `u32`, `u64`
- `isize`, `usize`
- `f32`, `f64`

These should be recognized as type names (identifiers that map to built-in types).

Optional: Add literal suffixes:
- `42i32`, `255u8`, `3.14f32`
- This requires lexer changes to recognize suffix on number literals

- [ ] Add type keywords to lexer
- [ ] (Optional) Add literal suffix support

---

### Task 1.3: Update Parser for Cast Expressions

**File:** `src/parser/parser.rs`

When parsing a call expression like `i32(x)`:
1. Check if the callee is a type name (i8, i16, i32, i64, u8, u16, u32, u64, isize, usize, f32, f64, Int, Float)
2. If so, parse as a cast expression instead of a function call

Option A: Reuse existing `ExprKind::Cast(Box<Expr>, Type)` if it exists
Option B: Add new `ExprKind::Cast` variant

The parser should transform:
- `i32(x)` → `Cast(x, Type::I32)`
- `f64(42)` → `Cast(42, Type::F64)`

- [ ] Detect type-name calls and parse as Cast
- [ ] Add ExprKind::Cast if not present

---

### Task 1.4: Update Type Checker

**File:** `src/types/checker.rs` and `src/types/inference.rs`

Add type checking rules for casts:

**Implicit widening (no cast needed):**
- i8 → i16 → i32 → i64
- u8 → u16 → u32 → u64
- f32 → f64
- Any int → Float

**Explicit cast required:**
- Narrowing: i32 → i8 (may truncate)
- Sign change: i32 → u32, u32 → i32
- Float → Int (truncates)
- Int → smaller Int

**Type checking for Cast expressions:**
```rust
ExprKind::Cast(expr, target_ty) => {
    let source_ty = infer(expr);
    if can_cast(source_ty, target_ty) {
        Ok(target_ty)
    } else {
        Err(TypeError::InvalidCast { from: source_ty, to: target_ty })
    }
}
```

- [ ] Add can_cast() function with rules
- [ ] Handle Cast in type inference
- [ ] Add implicit widening in assignment/comparison contexts

---

### Task 1.5: Update MIR

**File:** `src/mir/mir.rs` and `src/mir/lower.rs`

Option A: Add Cast to Rvalue:
```rust
enum Rvalue {
    // ... existing variants
    Cast(Operand, Ty),  // Cast operand to type
}
```

Option B: Lower casts to function calls to builtin cast functions.

In lowering, handle `ExprKind::Cast`:
```rust
ExprKind::Cast(expr, ty) => {
    let operand = self.lower_expr_to_operand(expr)?;
    let result = self.new_temp(ty);
    self.push_stmt(Statement::Assign(result, Rvalue::Cast(operand, ty)));
    Ok(Operand::Move(result))
}
```

- [ ] Add Cast to Rvalue (or equivalent)
- [ ] Lower Cast expressions to MIR

---

### Task 1.6: Update Interpreter

**File:** `src/mir/interp.rs`

Handle cast operations:

```rust
Rvalue::Cast(operand, target_ty) => {
    let value = self.eval_operand(operand)?;
    self.cast_value(value, target_ty)
}

fn cast_value(&self, value: Value, target: Ty) -> Result<Value, InterpError> {
    match (value, target) {
        // Int to Int (different sizes)
        (Value::Int(n), Ty::I8) => Ok(Value::Int((n as i8) as i64)),
        (Value::Int(n), Ty::I16) => Ok(Value::Int((n as i16) as i64)),
        (Value::Int(n), Ty::I32) => Ok(Value::Int((n as i32) as i64)),
        (Value::Int(n), Ty::I64) => Ok(Value::Int(n)),
        (Value::Int(n), Ty::U8) => Ok(Value::Int((n as u8) as i64)),
        (Value::Int(n), Ty::U16) => Ok(Value::Int((n as u16) as i64)),
        (Value::Int(n), Ty::U32) => Ok(Value::Int((n as u32) as i64)),
        (Value::Int(n), Ty::U64) => Ok(Value::Int((n as u64) as i64)),

        // Int to Float
        (Value::Int(n), Ty::F32) => Ok(Value::Float((n as f32) as f64)),
        (Value::Int(n), Ty::F64) => Ok(Value::Float(n as f64)),

        // Float to Int (truncate)
        (Value::Float(f), Ty::I32) => Ok(Value::Int(f as i64)),
        (Value::Float(f), Ty::I64) => Ok(Value::Int(f as i64)),
        // ... etc

        // Float to Float
        (Value::Float(f), Ty::F32) => Ok(Value::Float((f as f32) as f64)),
        (Value::Float(f), Ty::F64) => Ok(Value::Float(f)),

        _ => Err(InterpError { message: format!("Cannot cast {:?} to {:?}", value, target) })
    }
}
```

Note: The interpreter uses i64 internally for all ints and f64 for all floats. Casts apply the truncation/conversion but store in the same Value variants.

- [ ] Handle Rvalue::Cast in interpreter
- [ ] Implement cast_value() with all conversions

---

### Task 1.7: Update LLVM Codegen

**File:** `src/codegen/llvm.rs`

Use correct LLVM types:
- `i8`, `i16`, `i32`, `i64` for signed integers
- Same bit widths for unsigned (LLVM doesn't distinguish signedness in types)
- `float` for f32, `double` for f64

Add cast instructions:
- `trunc` - truncate to smaller int
- `zext` - zero-extend (unsigned)
- `sext` - sign-extend (signed)
- `fptrunc` - truncate float (f64 → f32)
- `fpext` - extend float (f32 → f64)
- `sitofp` - signed int to float
- `uitofp` - unsigned int to float
- `fptosi` - float to signed int
- `fptoui` - float to unsigned int

- [ ] Map Ty variants to LLVM types
- [ ] Generate cast instructions for Rvalue::Cast

---

### Task 1.8: Tests

Create `tests/forma/test_integers.forma`:
```forma
f main() -> Int {
    # Basic types
    v a: i8 = 127
    v b: u8 = 255
    v c: i32 = 1000000

    # Casting
    v d = i32(a)      # i8 -> i32 (widening)
    v e = u8(c)       # i32 -> u8 (narrowing, truncates)
    v f = f64(c)      # i32 -> f64
    v g = i32(3.7)    # f64 -> i32 (truncates to 3)

    # Arithmetic preserves types
    v sum: i32 = i32(a) + i32(b)

    # Return
    d + g
}
```

Add Rust tests in `tests/type_tests.rs`:
```rust
#[test]
fn test_integer_types() {
    // Test type checking accepts integer types
}

#[test]
fn test_cast_expressions() {
    // Test T(x) cast syntax
}
```

- [ ] Create test_integers.forma
- [ ] Add Rust unit tests
- [ ] Verify all tests pass

---

### Task 1.9: Backward Compatibility

Ensure existing code continues to work:
- `Int` remains valid and means `i64`
- `Float` remains valid and means `f64`
- Existing code using `Int` and `Float` works unchanged

- [ ] Verify backward compatibility

---

### Commit

`feat(types): add integer types (i8-i64, u8-u64) with T(x) cast syntax`

---

## PART 2: TRAIT FIXES (Complete Implementation)

### Current Problem

From the notes:
> Method resolution works when method names are unique across all types. If multiple types implement the same method name, the lowerer can't determine which to call without receiver type info.

The issue is that the MIR lowerer doesn't have access to type information. When it sees `p.display()`, it doesn't know that `p` is a `Point`, so it can't resolve `Point::display`.

### Solution: Pass Type Information to Lowerer

The type checker already resolves types. We need to either:

**Option A: Annotate AST with types**
- After type checking, annotate each expression with its resolved type
- Lowerer can read these annotations

**Option B: Rewrite method calls during type checking**
- Type checker rewrites `p.display()` to `Point::display(p)`
- Lowerer sees explicit qualified call

**Option C: Pass type environment to lowerer**
- Lowerer receives the type checker's results
- Lowerer queries types as needed

**Recommended: Option B** - It's cleanest and doesn't require passing extra state.

---

### Task 2.1: Add Type Annotations to AST

**File:** `src/parser/ast.rs`

Add a `resolved_type` field to `Expr`:
```rust
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub resolved_type: Option<Ty>,  // Filled in by type checker
}
```

Or create a separate typed AST / HIR (more work but cleaner).

- [ ] Add resolved_type to Expr (or create typed AST)

---

### Task 2.2: Fill in Types During Type Checking

**File:** `src/types/checker.rs` or `src/types/inference.rs`

After inferring the type of an expression, store it:
```rust
fn infer_expr(&mut self, expr: &mut Expr) -> Result<Ty, TypeError> {
    let ty = match &expr.kind {
        // ... existing inference logic
    };
    expr.resolved_type = Some(ty.clone());
    Ok(ty)
}
```

This requires the type checker to take `&mut Expr` instead of `&Expr`.

- [ ] Modify type checker to fill resolved_type

---

### Task 2.3: Update Method Resolution in Lowerer

**File:** `src/mir/lower.rs`

When lowering a method call, use the receiver's resolved type:
```rust
ExprKind::MethodCall(receiver, method, args) => {
    // Get receiver type from annotation
    let receiver_ty = receiver.resolved_type.as_ref()
        .ok_or_else(|| LowerError::MissingType)?;

    // Build qualified method name
    let type_name = match receiver_ty {
        Ty::Named(name) => name,
        Ty::Struct(name, _) => name,
        // ... handle other types
        _ => return Err(LowerError::CannotCallMethodOn(receiver_ty))
    };

    let qualified_name = format!("{}::{}", type_name, method.name);

    // Lower as function call with receiver as first arg
    // ...
}
```

- [ ] Use resolved_type in method resolution
- [ ] Build qualified method name from receiver type

---

### Task 2.4: Handle Trait Bounds

**File:** `src/types/checker.rs`

When a function has a trait bound like `f foo[T: Display](x: T)`:
1. Check that calls provide types implementing the trait
2. For method calls on `x`, resolve to the trait method

This is more complex and may be deferred. The immediate fix is just getting concrete type method resolution working.

- [ ] (Optional) Implement trait bound checking

---

### Task 2.5: Tests

Test that methods work with multiple types having the same method name:
```forma
s Point { x: Int, y: Int }
s Circle { radius: Int }

impl Point {
    f describe(&self) -> Str {
        f"Point at ({self.x}, {self.y})"
    }
}

impl Circle {
    f describe(&self) -> Str {
        f"Circle with radius {self.radius}"
    }
}

f main() {
    v p = Point(x: 1, y: 2)
    v c = Circle(radius: 5)

    print(p.describe())  # Should call Point::describe
    print(c.describe())  # Should call Circle::describe
}
```

- [ ] Create test with multiple types, same method name
- [ ] Verify correct method is called for each type

---

### Commit

`fix(types): complete trait method resolution with receiver type info`

---

## VERIFICATION

After completing both parts:

- [ ] `cargo build` - no errors
- [ ] `cargo test` - all tests pass (should be 218+)
- [ ] `cargo build --features llvm` - LLVM still works
- [ ] Test examples still work
- [ ] Update QUICK_WINS_IMPLEMENTATION.md to mark Section 2 and Section 10 as ✅ DONE

---

## NOTES

*Add implementation notes here as you work:*

