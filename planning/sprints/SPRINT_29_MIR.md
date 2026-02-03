# Sprint 29: MIR Lowerer Completion

**Goal:** Fix edge cases and improve error handling in MIR lowering
**Estimated Effort:** 4-5 hours

---

## Overview

The MIR lowerer (`src/mir/lower.rs`) has 7 issues to address:
1. Loop label error handling uses `eprintln` instead of proper errors
2. Closure entry block not set
3. Variant discriminant hash can have collisions
4. Missing PatternKind variants (Or, Range, Slice, Ref)
5. Missing ExprKind variants (audit needed)
6. Closure local types not saved/restored
7. `expect()` calls should be proper error handling

---

## Task 29.1: Fix Loop Label Error Handling

**File:** `src/mir/lower.rs` lines 835-838, 854-856

**Current Code:**
```rust
} else if let Some(label_ident) = label {
    // Label not found - this would be caught by type checker ideally
    eprintln!("Warning: break label '{}' not found", label_ident.name);
}
```

**Problem:** Using `eprintln` instead of proper error reporting.

**Fix:** Use the lowerer's error mechanism:
```rust
} else if let Some(label_ident) = label {
    self.errors.push(LowerError::new(
        format!("break label '{}' not found in enclosing loop", label_ident.name),
        expr.span,
    ));
}
```

Same fix for `continue` label error at line 855.

**Note:** May need to add an `errors` field and `LowerError` type if not present.

---

## Task 29.2: Fix Closure Entry Block

**File:** `src/mir/lower.rs` lines 1081-1084

**Current Code:**
```rust
// Create entry block
let entry = new_fn.add_block();
self.current_fn = Some(new_fn);
self.current_block = Some(entry);
```

**Problem:** `new_fn.entry_block` is not set before it's moved.

**Fix:** Set entry block before setting current_fn:
```rust
// Create entry block
let entry = new_fn.add_block();
new_fn.entry_block = entry;  // Set entry block!
self.current_fn = Some(new_fn);
self.current_block = Some(entry);
```

---

## Task 29.3: Fix Variant Discriminant Hash

**File:** `src/mir/lower.rs` lines 1763-1772

**Current Code:**
```rust
fn get_variant_discriminant(&self, variant: &str) -> i64 {
    match variant {
        "None" => 0,
        "Some" => 1,
        "Ok" => 0,
        "Err" => 1,
        // For user-defined enums, use a simple hash
        _ => variant.bytes().fold(0i64, |acc, b| acc + b as i64),
    }
}
```

**Problem:** Sum of bytes causes collisions (e.g., "ABC" = "BCA" = "CAB").

**Fix Option A - FNV-1a hash:**
```rust
fn get_variant_discriminant(&self, variant: &str) -> i64 {
    match variant {
        "None" => 0,
        "Some" => 1,
        "Ok" => 0,
        "Err" => 1,
        // Use FNV-1a hash for user-defined variants
        _ => {
            const FNV_OFFSET: u64 = 14695981039346656037;
            const FNV_PRIME: u64 = 1099511628211;
            let hash = variant.bytes().fold(FNV_OFFSET, |acc, b| {
                (acc ^ b as u64).wrapping_mul(FNV_PRIME)
            });
            (hash & 0x7FFFFFFFFFFFFFFF) as i64  // Keep positive
        }
    }
}
```

**Fix Option B - Ordered registry (better for debugging):**
```rust
// Add to Lowerer struct:
variant_registry: HashMap<String, i64>,
next_discriminant: i64,

// In get_variant_discriminant:
fn get_variant_discriminant(&mut self, variant: &str) -> i64 {
    match variant {
        "None" => 0,
        "Some" => 1,
        "Ok" => 0,
        "Err" => 1,
        _ => {
            if let Some(&disc) = self.variant_registry.get(variant) {
                disc
            } else {
                let disc = self.next_discriminant;
                self.next_discriminant += 1;
                self.variant_registry.insert(variant.to_string(), disc);
                disc
            }
        }
    }
}
```

**Recommendation:** Use FNV-1a hash for simplicity (Option A).

---

## Task 29.4: Implement Missing PatternKind Variants

**File:** `src/mir/lower.rs` line 1734

**Current Code:**
```rust
_ => {
    // Unsupported pattern, skip to next
    self.terminate(Terminator::Goto(next_test));
    continue;
}
```

**Missing Patterns:** Or, Range, Slice, Ref

**Fix:** Add handlers for each pattern type:

```rust
PatternKind::Or(patterns) => {
    // Or pattern: p1 | p2 | p3
    // Generate tests for each sub-pattern, any match goes to body
    for sub_pattern in patterns {
        // Create block for this alternative
        let alt_test = self.new_block();
        self.current_block = Some(alt_test);

        // Lower the sub-pattern test
        self.lower_pattern_test(sub_pattern, scrut_local, body_block, next_test);
    }
}

PatternKind::Range(start, end, inclusive) => {
    // Range pattern: 1..5 or 1..=5
    let start_val = self.lower_expr(start)?;
    let end_val = self.lower_expr(end)?;

    // scrut >= start && scrut <= end (or < for exclusive)
    let ge_start = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(ge_start, Rvalue::BinaryOp(
        BinOp::Ge,
        Operand::Local(scrut_local),
        start_val,
    )));

    let cmp_op = if *inclusive { BinOp::Le } else { BinOp::Lt };
    let le_end = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(le_end, Rvalue::BinaryOp(
        cmp_op,
        Operand::Local(scrut_local),
        end_val,
    )));

    let in_range = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(in_range, Rvalue::BinaryOp(
        BinOp::And,
        Operand::Local(ge_start),
        Operand::Local(le_end),
    )));

    self.terminate(Terminator::Branch {
        cond: Operand::Local(in_range),
        then_block: body_block,
        else_block: next_test,
    });
}

PatternKind::Slice(elem_patterns, rest_pattern) => {
    // Slice pattern: [a, b, ..rest]
    // First check length
    let len = self.new_temp(Ty::Int);
    self.emit(StatementKind::Assign(len, Rvalue::Len(scrut_local)));

    let min_len = elem_patterns.len() as i64;
    let has_rest = rest_pattern.is_some();

    // Check length: len >= min_len (if has rest) or len == min_len
    let len_check = self.new_temp(Ty::Bool);
    let cmp_op = if has_rest { BinOp::Ge } else { BinOp::Eq };
    self.emit(StatementKind::Assign(len_check, Rvalue::BinaryOp(
        cmp_op,
        Operand::Local(len),
        Operand::Const(Const::Int(min_len)),
    )));

    let after_len_check = self.new_block();
    self.terminate(Terminator::Branch {
        cond: Operand::Local(len_check),
        then_block: after_len_check,
        else_block: next_test,
    });

    self.current_block = Some(after_len_check);

    // Bind each element
    for (i, elem_pattern) in elem_patterns.iter().enumerate() {
        let elem = self.new_temp(/* element type */);
        self.emit(StatementKind::Assign(elem, Rvalue::Index(
            scrut_local,
            Operand::Const(Const::Int(i as i64)),
        )));
        // Recursively match element pattern
        self.lower_sub_pattern(elem_pattern, elem, next_test);
    }

    // Bind rest if present
    if let Some(rest) = rest_pattern {
        // Slice from min_len to end
        let rest_local = self.new_temp(/* slice type */);
        self.emit(StatementKind::Assign(rest_local, Rvalue::Slice {
            base: scrut_local,
            start: min_len,
            end: None, // to end
        }));
        self.vars.insert(rest.name.clone(), rest_local);
    }

    self.terminate(Terminator::Goto(body_block));
}

PatternKind::Ref(inner, mutability) => {
    // Reference pattern: &x or &mut x
    // Dereference the scrutinee and match inner
    let deref_local = self.new_temp(/* dereferenced type */);
    self.emit(StatementKind::Assign(deref_local, Rvalue::Deref(scrut_local)));
    self.lower_sub_pattern(inner, deref_local, next_test);
    self.terminate(Terminator::Goto(body_block));
}
```

**Note:** You may need helper functions like `lower_sub_pattern` for recursive pattern lowering.

---

## Task 29.5: Audit Missing ExprKind Variants

**File:** `src/mir/lower.rs`

**Action:** Review all ExprKind variants in AST and ensure each has a handler.

Check for these potentially missing handlers:
- `ExprKind::Index` (array/list indexing)
- `ExprKind::Slice` (list slicing `a[1:3]`)
- `ExprKind::Cast` (type casts)
- `ExprKind::Yield` (generators)
- `ExprKind::Try` (`?` operator)
- `ExprKind::Format` (f-strings)

For each missing:
```rust
ExprKind::MissingVariant => {
    self.errors.push(LowerError::new(
        "expression type not yet supported in MIR lowering",
        expr.span,
    ));
    None
}
```

---

## Task 29.6: Save/Restore Closure Local Types

**File:** `src/mir/lower.rs` lines 1048-1101

**Current Code:**
```rust
// Save lowering state
let saved_fn = self.current_fn.take();
let saved_block = self.current_block.take();
let saved_vars = std::mem::take(&mut self.vars);
// ...
// Restore lowering state
self.current_fn = saved_fn;
self.current_block = saved_block;
self.vars = saved_vars;
```

**Problem:** `local_types` map not saved/restored, can cause type confusion.

**Fix:** Also save and restore `local_types` and `var_full_types`:
```rust
// Save lowering state
let saved_fn = self.current_fn.take();
let saved_block = self.current_block.take();
let saved_vars = std::mem::take(&mut self.vars);
let saved_local_types = std::mem::take(&mut self.local_types);
let saved_var_full_types = std::mem::take(&mut self.var_full_types);

// ... closure lowering ...

// Restore lowering state
self.current_fn = saved_fn;
self.current_block = saved_block;
self.vars = saved_vars;
self.local_types = saved_local_types;
self.var_full_types = saved_var_full_types;
```

---

## Task 29.7: Convert expect() to Error Handling

**File:** `src/mir/lower.rs` (search for `.expect(`)

**Current Code (example):**
```rust
.expect("parse should succeed")
```

**Problem:** Panics on unexpected conditions instead of graceful error.

**Fix:** Use match or `ok_or_else`:
```rust
// Before:
let value = some_result.expect("parse should succeed");

// After:
let value = match some_result {
    Ok(v) => v,
    Err(e) => {
        self.errors.push(LowerError::new(
            format!("failed to parse: {}", e),
            span,
        ));
        return None;
    }
};
```

Audit all `.expect()`, `.unwrap()` calls and replace with proper error handling where the condition might actually fail.

---

## Verification

After implementing all tasks:

1. Run the test suite:
```bash
cd forma && cargo test
```

2. Test pattern matching:
```forma
# Range patterns
f grade(score: Int) -> String
    m score
        90..=100 => "A"
        80..89 => "B"
        70..79 => "C"
        _ => "F"

# Or patterns
f is_vowel(c: Char) -> Bool
    m c
        'a' | 'e' | 'i' | 'o' | 'u' => true
        _ => false

# Slice patterns (if supported in parser)
f first_two(list: [Int]) -> (Int, Int)
    m list
        [a, b, ..] => (a, b)
        _ => (0, 0)
```

3. Verify closures work correctly:
```forma
f test_closure() -> Int
    let add = |x, y| x + y
    let result = add(1, 2)
    result

f main() -> Int
    test_closure()
```

---

## Summary

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 29.1 | Loop label error handling | lower.rs | 835-838, 854-856 |
| 29.2 | Closure entry block | lower.rs | 1081-1084 |
| 29.3 | Variant discriminant hash | lower.rs | 1763-1772 |
| 29.4 | Missing PatternKind variants | lower.rs | 1734 |
| 29.5 | Audit ExprKind variants | lower.rs | various |
| 29.6 | Closure local types | lower.rs | 1048-1101 |
| 29.7 | Convert expect() to errors | lower.rs | various |

**Dependencies:** None (standalone MIR changes)

---

## Claude Code Prompt

```
Sprint 29: MIR Lowerer Completion for FORMA

Working directory: forma/

## Tasks

### 29.1: Fix Loop Label Error Handling
File: src/mir/lower.rs (lines 835-838, 854-856)
Replace `eprintln!("Warning: break/continue label...")` with proper
error collection. Add LowerError type if needed:
self.errors.push(LowerError::new(message, span))

### 29.2: Fix Closure Entry Block
File: src/mir/lower.rs (lines 1081-1084)
After `let entry = new_fn.add_block();` add:
`new_fn.entry_block = entry;`
before `self.current_fn = Some(new_fn);`

### 29.3: Fix Variant Discriminant Hash
File: src/mir/lower.rs (lines 1763-1772)
Replace byte sum with FNV-1a hash to prevent collisions:
```rust
const FNV_OFFSET: u64 = 14695981039346656037;
const FNV_PRIME: u64 = 1099511628211;
let hash = variant.bytes().fold(FNV_OFFSET, |acc, b| {
    (acc ^ b as u64).wrapping_mul(FNV_PRIME)
});
(hash & 0x7FFFFFFFFFFFFFFF) as i64
```

### 29.4: Implement Missing PatternKind Variants
File: src/mir/lower.rs (line 1734)
Add handlers for:
- PatternKind::Or - test each alternative, any match goes to body
- PatternKind::Range - check scrut >= start && scrut <= end
- PatternKind::Slice - check length, bind elements and rest
- PatternKind::Ref - dereference and match inner

### 29.5: Audit Missing ExprKind Variants
Grep for `ExprKind::` in AST, verify each has handler in lower.rs.
Add error-producing stub for any missing:
```rust
ExprKind::Missing => {
    self.errors.push(LowerError::new("not supported", span));
    None
}
```

### 29.6: Save/Restore Closure Local Types
File: src/mir/lower.rs (lines 1048-1101)
Add to the save/restore section:
let saved_local_types = std::mem::take(&mut self.local_types);
let saved_var_full_types = std::mem::take(&mut self.var_full_types);
And restore them after closure lowering.

### 29.7: Convert expect() to Error Handling
Search for `.expect(` and `.unwrap()` in lower.rs.
Replace with match or ok_or_else that pushes to self.errors.

## Testing
After changes:
1. cargo test
2. Test pattern matching with ranges, or patterns
3. Test closures with captured variables

All 251+ existing tests must continue to pass.
```
