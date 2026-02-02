# Sprint 19: Final Polish - Zero Panic Goal

**Goal:** Eliminate remaining panic points and silent failures
**Target:** Production-grade stability

---

## Task 19.1: MIR Lowerer func_name Fix (HIGH)

### Problem
Line 727 in `src/mir/lower.rs` has an unconditional `.unwrap()`:

```rust
self.terminate(Terminator::Call {
    func: func_name.unwrap(),  // PANICS if func_name is None
    ...
});
```

### Solution

**File:** `src/mir/lower.rs`

```rust
// REPLACE (around line 727):
self.terminate(Terminator::Call {
    func: func_name.unwrap(),
    args: mir_args,
    dest: Some(result),
    next: next_block,
});

// WITH:
let func = func_name.ok_or_else(|| LowerError {
    message: "Internal error: function call without function name".to_string(),
    span: expr.span,
})?;

self.terminate(Terminator::Call {
    func,
    args: mir_args,
    dest: Some(result),
    next: next_block,
});
```

---

## Task 19.2: MIR Lowerer expect() to Result (MEDIUM)

### Problem
7 internal expect() calls could be converted to Result for better error handling.

### Solution

**File:** `src/mir/lower.rs`

**Step 1: Add helper that returns Result instead of panicking:**

```rust
// Add near top of impl MIRLowerer:

fn current_function_checked(&mut self) -> Result<&mut Function, LowerError> {
    self.functions.last_mut()
        .ok_or_else(|| LowerError {
            message: "Internal error: no current function context".to_string(),
            span: Span::default(),
        })
}

fn current_block_checked(&self) -> Result<BlockId, LowerError> {
    self.current_block
        .ok_or_else(|| LowerError {
            message: "Internal error: no current block context".to_string(),
            span: Span::default(),
        })
}
```

**Step 2: Update helper methods to use checked versions:**

```rust
// REPLACE new_temp (line ~2345):
fn new_temp(&mut self, ty: Ty) -> Result<Local, LowerError> {
    let func = self.current_function_checked()?;
    let local = Local(func.locals.len());
    func.locals.push(LocalDecl { ty, name: None });
    Ok(local)
}

// REPLACE new_local (line ~2353):
fn new_local(&mut self, ty: Ty, name: Option<String>) -> Result<Local, LowerError> {
    let func = self.current_function_checked()?;
    let local = Local(func.locals.len());
    func.locals.push(LocalDecl { ty, name });
    Ok(local)
}

// REPLACE new_block (line ~2648):
fn new_block(&mut self) -> Result<BlockId, LowerError> {
    let func = self.current_function_checked()?;
    let id = BlockId(func.blocks.len());
    func.blocks.push(Block {
        statements: Vec::new(),
        terminator: None,
    });
    Ok(id)
}

// REPLACE emit (line ~2654):
fn emit(&mut self, kind: StatementKind) -> Result<(), LowerError> {
    let block_id = self.current_block_checked()?;
    let func = self.current_function_checked()?;
    func.blocks[block_id.0].statements.push(Statement {
        kind,
        span: Span::default(),
    });
    Ok(())
}

// REPLACE terminate (line ~2662):
fn terminate(&mut self, term: Terminator) -> Result<(), LowerError> {
    let block_id = self.current_block_checked()?;
    let func = self.current_function_checked()?;
    func.blocks[block_id.0].terminator = Some(term);
    Ok(())
}
```

**Step 3: Update all call sites to use `?` operator:**

Search for all usages of `new_temp`, `new_local`, `new_block`, `emit`, `terminate` and add `?`.

---

## Task 19.3: Parser unreachable!() to Errors (MEDIUM)

### Problem
3 `unreachable!()` macros could be proper parse errors.

### Solution

**File:** `src/parser/parser.rs`

```rust
// REPLACE (line 707):
match self.parse_function(is_async, false, Visibility::Private)? {
    ItemKind::Function(f) => Ok(TraitItem::Function(f)),
    _ => unreachable!(),
}

// WITH:
match self.parse_function(is_async, false, Visibility::Private)? {
    ItemKind::Function(f) => Ok(TraitItem::Function(f)),
    other => Err(ParseError::new(
        format!("Expected function in trait, got {:?}", other),
        self.current_span(),
    )),
}


// REPLACE (line 799):
match self.parse_function(is_async, false, vis)? {
    ItemKind::Function(f) => Ok(ImplItem::Function(f)),
    _ => unreachable!(),
}

// WITH:
match self.parse_function(is_async, false, vis)? {
    ItemKind::Function(f) => Ok(ImplItem::Function(f)),
    other => Err(ParseError::new(
        format!("Expected function in impl, got {:?}", other),
        self.current_span(),
    )),
}


// REPLACE (line 2297):
match else_if.kind {
    ExprKind::If(if_expr) => Some(ElseBranch::ElseIf(if_expr)),
    _ => unreachable!(),
}

// WITH:
match else_if.kind {
    ExprKind::If(if_expr) => Some(ElseBranch::ElseIf(if_expr)),
    _ => {
        return Err(ParseError::new(
            "Expected if expression after 'else if'",
            else_if.span,
        ));
    }
}
```

---

## Task 19.4: Type Inference fresh_var Cleanup (MEDIUM)

### Problem
5 locations use `Ty::fresh_var()` as silent fallbacks instead of proper errors or reserved constants.

### Solution

**File:** `src/types/inference.rs`

**Fix 1: register_builtin_methods (line ~2901)**

```rust
// REPLACE:
let t_var = Ty::fresh_var();

// WITH:
use reserved_type_vars::ELEM_TYPE;
let t_var = Ty::Var(TypeVar { id: ELEM_TYPE });
```

**Fix 2: register_builtin_methods for maps (line ~3035-3036)**

```rust
// REPLACE:
let k_var = Ty::fresh_var();
let v_var = Ty::fresh_var();

// WITH:
use reserved_type_vars::{KEY_TYPE, VALUE_TYPE};
let k_var = Ty::Var(TypeVar { id: KEY_TYPE });
let v_var = Ty::Var(TypeVar { id: VALUE_TYPE });
```

**Fix 3: Lambda parameter fallback (line ~4212)**

```rust
// REPLACE:
Ok(Ty::fresh_var())

// WITH:
Err(TypeError::new(
    "Cannot infer type of lambda parameter",
    span,
))
```

**Fix 4: check_pattern unknown struct (line ~4699)**

```rust
// REPLACE:
Ok(Ty::fresh_var())

// WITH:
Err(TypeError::new(
    format!("Unknown struct type in pattern: {}", name),
    span,
))
```

**Fix 5: collect_pattern_bindings unknown field (line ~4783)**

```rust
// REPLACE:
.unwrap_or_else(Ty::fresh_var)

// WITH:
.ok_or_else(|| TypeError::new(
    format!("Unknown field '{}' in struct pattern", field_name),
    span,
))?
```

---

## Task 19.5: Option/Result TypeVar IDs (LOW)

### Problem
Hard-coded IDs 0 and 1 in Option/Result could conflict with fresh vars.

### Solution

**File:** `src/types/inference.rs`

**Add reserved IDs to module:**

```rust
pub mod reserved_type_vars {
    pub const ELEM_TYPE: u32 = u32::MAX;      // T (element type)
    pub const KEY_TYPE: u32 = u32::MAX - 1;   // K (key type)
    pub const VALUE_TYPE: u32 = u32::MAX - 2; // V (value type)
    pub const OPTION_T: u32 = u32::MAX - 3;   // Option's T
    pub const RESULT_T: u32 = u32::MAX - 4;   // Result's T
    pub const RESULT_E: u32 = u32::MAX - 5;   // Result's E
}
```

**Update Option definition (line ~123):**

```rust
// REPLACE:
("Some".to_string(), vec![Ty::Var(TypeVar { id: 0 })]),

// WITH:
("Some".to_string(), vec![Ty::Var(TypeVar { id: reserved_type_vars::OPTION_T })]),
```

**Update Result definition (lines ~155-156):**

```rust
// REPLACE:
("Ok".to_string(), vec![Ty::Var(TypeVar { id: 0 })]),
("Err".to_string(), vec![Ty::Var(TypeVar { id: 1 })]),

// WITH:
("Ok".to_string(), vec![Ty::Var(TypeVar { id: reserved_type_vars::RESULT_T })]),
("Err".to_string(), vec![Ty::Var(TypeVar { id: reserved_type_vars::RESULT_E })]),
```

---

## Task 19.6: Minor Safety Fixes (LOW)

### 19.6.1: DateTime nested unwrap

**File:** `src/mir/interp.rs` (around line 2961)

```rust
// REPLACE:
let dt = DateTime::from_timestamp(ts, 0).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());

// WITH:
let dt = DateTime::from_timestamp(ts, 0).unwrap_or(DateTime::UNIX_EPOCH);
```

### 19.6.2: JSON serialization unwraps

**File:** `src/main.rs` (multiple locations)

```rust
// REPLACE:
println!("{}", serde_json::to_string_pretty(&output).unwrap());

// WITH:
match serde_json::to_string_pretty(&output) {
    Ok(json) => println!("{}", json),
    Err(e) => eprintln!("Error serializing output: {}", e),
}
```

### 19.6.3: Print unwraps

**File:** `src/errors/report.rs` (lines 29, 45)

```rust
// REPLACE:
report.finish().print((filename, Source::from(source))).unwrap();

// WITH:
let _ = report.finish().print((filename, Source::from(source)));
```

---

## Task 19.7: Borrow Checker expect (LOW)

**File:** `src/borrow/checker.rs` (line 178)

```rust
// REPLACE:
fn current_scope_mut(&mut self) -> &mut HashSet<String> {
    self.scope_stack.last_mut()
        .expect("internal error: empty scope stack in borrow checker")
}

// WITH:
fn current_scope_mut(&mut self) -> Result<&mut HashSet<String>, BorrowError> {
    self.scope_stack.last_mut()
        .ok_or_else(|| BorrowError {
            message: "Internal error: empty scope stack".to_string(),
            span: Span::default(),
        })
}
```

---

## Verification

After each task, run:
```bash
cargo test
cargo clippy -- -D warnings
forma check std/*.forma
forma check examples/*.forma
```

---

## Summary

| Task | Description | Priority | Panic Points Fixed |
|------|-------------|----------|-------------------|
| 19.1 | MIR func_name fix | HIGH | 1 |
| 19.2 | MIR expect to Result | MEDIUM | 7 |
| 19.3 | Parser unreachable fix | MEDIUM | 3 |
| 19.4 | Type inference fresh_var | MEDIUM | 5 |
| 19.5 | Option/Result IDs | LOW | 0 (correctness) |
| 19.6 | Minor safety fixes | LOW | ~10 |
| 19.7 | Borrow checker expect | LOW | 1 |

**Total panic points to fix: ~27**

After Sprint 19: **Near-zero panic points in production code.**

---

*"Production grade."*
