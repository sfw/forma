# Sprint 35: Explicit Reference Parameters

**Goal:** Replace implicit copy-back semantics with explicit `ref` parameters for safe, predictable mutation
**Estimated Effort:** 4-6 hours

---

## Problem Statement

The current copy-back implementation:
1. **Hides mutation** - Callers can't tell if a function modifies array arguments
2. **Violates safety goals** - Linear types (Sprint 33) track ownership, but copy-back creates invisible mutation
3. **Performance cost** - O(n) copy in + O(n) copy back for every array argument
4. **Semantic confusion** - Neither pass-by-value nor pass-by-reference

## Solution: Explicit `ref` Parameters

Adopt Rust-style explicit reference parameters:

```forma
# Immutable reference - can read but not modify
f sum(ref arr: [Int]) -> Int
    ...

# Mutable reference - can modify in place
f swap(ref mut arr: [Int], i: Int, j: Int) -> Unit
    temp := arr[i]
    arr[i] := arr[j]
    arr[j] := temp

# Value parameter (default) - receives a copy
f sorted(arr: [Int]) -> [Int]
    # arr is a copy, original unchanged
    ...
```

---

## Task 35.1: Add `ref` Keyword to Lexer

**File:** `src/lexer/scanner.rs`

Add `ref` as a reserved keyword:

```rust
fn keyword_kind(s: &str) -> Option<TokenKind> {
    match s {
        // ... existing keywords ...
        "ref" => Some(TokenKind::Ref),
        // ...
    }
}

pub enum TokenKind {
    // ... existing variants ...
    Ref,
}
```

---

## Task 35.2: Add Reference Parameter Syntax to Parser

**File:** `src/parser/parser.rs`

Update `parse_param()` to handle `ref` and `ref mut`:

```rust
fn parse_param(&mut self) -> ParseResult<Param> {
    // Check for ref modifier
    let pass_mode = if self.check(TokenKind::Ref) {
        self.advance();
        if self.check(TokenKind::Mut) {
            self.advance();
            PassMode::RefMut
        } else {
            PassMode::Ref
        }
    } else {
        PassMode::Value
    };

    // Parse name: Type
    let name = self.expect_ident()?;
    self.expect(TokenKind::Colon)?;
    let ty = self.parse_type()?;

    Ok(Param {
        name,
        ty,
        pass_mode,
        span: ...,
    })
}
```

**File:** `src/ast/mod.rs`

Add PassMode enum:

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PassMode {
    Value,   // Default: copy in, no copy back
    Ref,     // Immutable reference: no copy, read-only
    RefMut,  // Mutable reference: no copy, can modify
}

pub struct Param {
    pub name: Ident,
    pub ty: TypeExpr,
    pub pass_mode: PassMode,
    pub span: Span,
}
```

---

## Task 35.3: Update Type Inference for References

**File:** `src/types/inference.rs`

Track reference types and enforce rules:

```rust
// When inferring function calls:
fn infer_call(&mut self, callee: &Expr, args: &[Expr]) -> Result<Ty, TypeError> {
    // ... get function signature ...

    for (arg, param) in args.iter().zip(params.iter()) {
        let arg_ty = self.infer_expr(arg)?;

        match param.pass_mode {
            PassMode::Value => {
                // Normal: arg copied, types must match
                self.unifier.unify(&arg_ty, &param.ty, arg.span)?;
            }
            PassMode::Ref => {
                // Immutable ref: arg must be a place (variable/field/index)
                if !self.is_place(arg) {
                    return Err(TypeError::RefRequiresPlace {
                        span: arg.span,
                    });
                }
                self.unifier.unify(&arg_ty, &param.ty, arg.span)?;
            }
            PassMode::RefMut => {
                // Mutable ref: arg must be a mutable place
                if !self.is_place(arg) {
                    return Err(TypeError::RefMutRequiresPlace {
                        span: arg.span,
                    });
                }
                if !self.is_mutable(arg) {
                    return Err(TypeError::RefMutRequiresMutable {
                        span: arg.span,
                    });
                }
                // Track that this variable is borrowed mutably
                self.track_mut_borrow(arg)?;
                self.unifier.unify(&arg_ty, &param.ty, arg.span)?;
            }
        }
    }
    // ...
}

fn is_place(&self, expr: &Expr) -> bool {
    matches!(expr.kind,
        ExprKind::Ident(_) |
        ExprKind::Field(_, _) |
        ExprKind::Index(_, _)
    )
}
```

---

## Task 35.4: Update MIR Lowering for References

**File:** `src/mir/lower.rs`

Handle reference parameters in function lowering:

```rust
fn lower_function(&mut self, func: &Function) -> MirFunction {
    // ... setup ...

    for param in &func.params {
        let local = self.new_local(param.ty.clone(), Some(param.name.clone()));

        match param.pass_mode {
            PassMode::Value => {
                // Normal parameter local
                self.vars.insert(param.name.clone(), local);
            }
            PassMode::Ref | PassMode::RefMut => {
                // Reference parameter - store the reference, not a copy
                // Mark as reference in local_types
                self.ref_params.insert(local, param.pass_mode);
                self.vars.insert(param.name.clone(), local);
            }
        }
    }
    // ...
}
```

---

## Task 35.5: Update Interpreter for True References

**File:** `src/mir/interp.rs`

Replace copy-back with actual reference semantics:

```rust
fn call_function(&mut self, func_id: FuncId, args: Vec<Value>, call_info: &CallInfo) -> Result<Value, InterpError> {
    let func = self.get_function(func_id);

    // Build argument list with references where needed
    let mut bound_args = Vec::new();
    let mut ref_bindings: Vec<(Local, RefBinding)> = Vec::new();

    for (i, (arg, param)) in args.iter().zip(func.params.iter()).enumerate() {
        match param.pass_mode {
            PassMode::Value => {
                // Clone the value
                bound_args.push(arg.clone());
            }
            PassMode::Ref | PassMode::RefMut => {
                // Pass a reference to the original location
                let ref_binding = self.create_ref_binding(&call_info.arg_sources[i])?;
                ref_bindings.push((param.local, ref_binding.clone()));
                bound_args.push(Value::Ref(ref_binding));
            }
        }
    }

    // Execute function with references
    let result = self.execute_with_refs(func_id, bound_args, ref_bindings)?;

    Ok(result)
}

// Reference binding points to original storage
#[derive(Clone)]
pub struct RefBinding {
    pub frame_id: FrameId,
    pub local: Local,
    pub mutable: bool,
}

// When reading a ref parameter:
fn get_local(&self, local: Local) -> Value {
    if let Some(ref_binding) = self.ref_bindings.get(&local) {
        // Dereference: read from original location
        self.get_from_frame(ref_binding.frame_id, ref_binding.local)
    } else {
        // Normal local
        self.current_frame().locals[local]
    }
}

// When writing to a ref mut parameter:
fn set_local(&mut self, local: Local, value: Value) -> Result<(), InterpError> {
    if let Some(ref_binding) = self.ref_bindings.get(&local) {
        if !ref_binding.mutable {
            return Err(InterpError::CannotMutateImmutableRef { span: ... });
        }
        // Write to original location
        self.set_in_frame(ref_binding.frame_id, ref_binding.local, value)
    } else {
        // Normal local
        self.current_frame_mut().locals[local] = value;
        Ok(())
    }
}
```

---

## Task 35.6: Remove Copy-Back Hack

**File:** `src/mir/interp.rs`

Remove `call_function_internal_with_copyback` and related code:

```rust
// DELETE: call_function_internal_with_copyback
// DELETE: copyback parameter handling
// DELETE: array argument tracking for copyback

// Simplify call_function_internal to just use the new ref system
fn call_function_internal(&mut self, ...) -> Result<Value, InterpError> {
    // Use the new reference-based system from Task 35.5
    self.call_function(func_id, args, call_info)
}
```

---

## Task 35.7: Update Showcase Examples

Update all 16 showcase examples to use explicit `ref mut` where mutation is needed:

**10_quicksort.forma:**
```forma
# Before (implicit copy-back)
f swap(arr: [Int], i: Int, j: Int) -> Unit

# After (explicit ref mut)
f swap(ref mut arr: [Int], i: Int, j: Int) -> Unit
    temp := arr[i]
    arr[i] := arr[j]
    arr[j] := temp

f partition(ref mut arr: [Int], low: Int, high: Int) -> Int
    # ...
    swap(ref mut arr, i, j)  # Explicit at call site too
    # ...

f quicksort(ref mut arr: [Int], low: Int, high: Int) -> Unit
    # ...
```

**12_linked_list.forma:**
```forma
f list_prepend(ref mut values: [Int], ref mut next: [Int], ...) -> (Int, Int)

# At call site:
result := list_prepend(ref mut values, ref mut next, head, 1, free)
```

**14_sudoku.forma:**
```forma
f set_cell(ref mut grid: [Int], row: Int, col: Int, value: Int) -> Unit

f solve(ref mut grid: [Int]) -> Bool
    # ...
    set_cell(ref mut grid, row, col, value)
    if solve(ref mut grid) then
        return true
    set_cell(ref mut grid, row, col, 0)  # Backtrack
    # ...
```

---

## Task 35.8: Add Borrow Checking (Basic)

**File:** `src/types/inference.rs`

Prevent common reference errors:

```rust
// Track active borrows
struct BorrowTracker {
    immutable_borrows: HashMap<String, Vec<Span>>,
    mutable_borrows: HashMap<String, Span>,
}

impl BorrowTracker {
    fn borrow_immut(&mut self, var: &str, span: Span) -> Result<(), TypeError> {
        // Can't borrow immutably if mutably borrowed
        if let Some(mut_span) = self.mutable_borrows.get(var) {
            return Err(TypeError::BorrowConflict {
                var: var.to_string(),
                existing: *mut_span,
                new: span,
                existing_kind: "mutable",
                new_kind: "immutable",
            });
        }
        self.immutable_borrows.entry(var.to_string())
            .or_default()
            .push(span);
        Ok(())
    }

    fn borrow_mut(&mut self, var: &str, span: Span) -> Result<(), TypeError> {
        // Can't borrow mutably if any borrow exists
        if let Some(mut_span) = self.mutable_borrows.get(var) {
            return Err(TypeError::BorrowConflict {
                var: var.to_string(),
                existing: *mut_span,
                new: span,
                existing_kind: "mutable",
                new_kind: "mutable",
            });
        }
        if let Some(spans) = self.immutable_borrows.get(var) {
            if !spans.is_empty() {
                return Err(TypeError::BorrowConflict {
                    var: var.to_string(),
                    existing: spans[0],
                    new: span,
                    existing_kind: "immutable",
                    new_kind: "mutable",
                });
            }
        }
        self.mutable_borrows.insert(var.to_string(), span);
        Ok(())
    }
}
```

---

## Syntax Summary

| Parameter | Syntax | Semantics |
|-----------|--------|-----------|
| Value (default) | `x: T` | Copy in, no copy back |
| Immutable ref | `ref x: T` | No copy, read-only access |
| Mutable ref | `ref mut x: T` | No copy, can modify original |

| Call Site | Syntax | Meaning |
|-----------|--------|---------|
| Value | `foo(arr)` | Pass copy |
| Ref | `foo(ref arr)` | Pass immutable reference |
| Ref mut | `foo(ref mut arr)` | Pass mutable reference |

---

## Testing

After implementation:

1. Run existing tests: `cargo test` (257 should still pass)
2. Verify showcase examples with explicit refs
3. Test borrow errors:

```forma
# Should error: can't mutably borrow twice
f bad()
    let arr = [1, 2, 3]
    # Error: arr already borrowed mutably
    both_mut(ref mut arr, ref mut arr)

# Should error: can't read immutable ref as mutable
f also_bad(ref x: [Int])
    x[0] := 42  # Error: x is immutable reference
```

---

## Claude Code Prompt

```
Sprint 35: Explicit Reference Parameters for FORMA

Working directory: forma/

## Tasks

### 35.1: Add ref Keyword
File: src/lexer/scanner.rs
Add "ref" to keyword map, add TokenKind::Ref

### 35.2: Parse ref/ref mut Parameters
File: src/parser/parser.rs, src/ast/mod.rs
Add PassMode enum { Value, Ref, RefMut }
Add pass_mode field to Param struct
Update parse_param() to detect `ref` and `ref mut` before param name

### 35.3: Type Inference for References
File: src/types/inference.rs
In infer_call(), check PassMode:
- Ref/RefMut requires argument to be a "place" (variable/field/index)
- RefMut requires the place to be mutable
Add is_place() helper function

### 35.4: MIR Lowering for References
File: src/mir/lower.rs
Track ref_params in lowerer
Mark reference parameters specially in MIR

### 35.5: Interpreter True References
File: src/mir/interp.rs
Add RefBinding struct { frame_id, local, mutable }
Add ref_bindings map to stack frame
When calling with ref param, create RefBinding to original location
get_local/set_local dereference through RefBinding

### 35.6: Remove Copy-Back
File: src/mir/interp.rs
Delete call_function_internal_with_copyback
Delete all copyback-related code
Simplify to use new reference system

### 35.7: Update Showcase Examples
Files: examples/showcase/*.forma
Change functions that mutate arrays to use `ref mut`:
- 10_quicksort: swap, partition, quicksort, sort
- 12_linked_list: list_prepend, list_append, list_reverse
- 13_binary_tree: bst_insert
- 14_sudoku: set_cell, solve
- 15_matrix_mult: mat_set, mat_mult
- 16_game_of_life: set_cell, next_generation, copy_grid
Update call sites to pass `ref mut arr`

### 35.8: Basic Borrow Checking
File: src/types/inference.rs
Add BorrowTracker to prevent:
- Multiple mutable borrows of same variable
- Mutable borrow while immutable borrows exist

## Testing
1. cargo test (all 257 tests pass)
2. ./examples/showcase/verify_all.sh (all 16 pass)
3. Test borrow errors are caught

## Definition of Done
- `ref` and `ref mut` parameters work
- Copy-back code removed
- All showcase examples use explicit refs
- Basic borrow checking prevents obvious errors
```
