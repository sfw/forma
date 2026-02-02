# Sprint 20: Critical & High Priority Fixes

**Goal:** Fix all release-blocking bugs identified in Code Review V6
**Priority:** Critical (6) + High (17) = 23 issues

---

## Task 20.1: Fix Coalesce Operator (`??`) - CRITICAL

### Problem
**File:** `src/mir/lower.rs` lines 1214-1222

The coalesce operator checks if *lowering* succeeded instead of the runtime Option value:

```rust
// BUGGY CODE:
ExprKind::Coalesce(left, right) => {
    let l = self.lower_expr(left);
    if l.is_some() {  // Checks RUST Option, not FORMA Option!
        l
    } else {
        self.lower_expr(right)
    }
}
```

### Solution
Replace with proper MIR that checks discriminant at runtime:

```rust
ExprKind::Coalesce(left, right) => {
    // Coalesce operator: left ?? right
    // Returns the inner value of left if it's Some, otherwise evaluates right

    let left_val = self.lower_expr(left)?;

    // Store the left value
    let scrutinee = self.new_temp(Ty::Int);
    self.emit(StatementKind::Assign(scrutinee, Rvalue::Use(left_val)));

    // Get discriminant to check if Some vs None
    let disc = self.new_temp(Ty::Int);
    self.emit(StatementKind::Assign(disc, Rvalue::Discriminant(scrutinee)));

    // Create blocks
    let some_block = self.new_block();   // Left is Some - extract value
    let none_block = self.new_block();   // Left is None - use right
    let continue_block = self.new_block();

    // Result variable (use same type as left's inner type, or Int as fallback)
    let result = self.new_temp(Ty::Int);

    // Branch: discriminant > 0 means Some (has value)
    // Option: None=0, Some=1
    let has_value = self.new_temp(Ty::Bool);
    self.emit(StatementKind::Assign(
        has_value,
        Rvalue::BinaryOp(BinOp::Gt, Operand::Copy(disc), Operand::Constant(Constant::Int(0))),
    ));
    self.terminate(Terminator::If {
        cond: Operand::Local(has_value),
        then_block: some_block,
        else_block: none_block,
    });

    // Some block: extract the inner value
    self.current_block = Some(some_block);
    let extracted = self.new_temp(Ty::Int);
    self.emit(StatementKind::Assign(extracted, Rvalue::EnumField(scrutinee, 0)));
    self.emit(StatementKind::Assign(result, Rvalue::Use(Operand::Local(extracted))));
    self.terminate(Terminator::Goto(continue_block));

    // None block: evaluate and use the right expression
    self.current_block = Some(none_block);
    if let Some(right_val) = self.lower_expr(right) {
        self.emit(StatementKind::Assign(result, Rvalue::Use(right_val)));
    }
    self.terminate(Terminator::Goto(continue_block));

    self.current_block = Some(continue_block);
    Some(Operand::Local(result))
}
```

---

## Task 20.2: Fix If-Expression Type Inference - CRITICAL

### Problem
**File:** `src/mir/lower.rs` line 1334

If-expressions always return `Ty::Int`:

```rust
let result = self.new_temp(Ty::Int); // TODO: proper type
```

### Solution
Infer the type from the then branch. Replace `lower_if` function (lines 1326-1378):

```rust
fn lower_if(&mut self, if_expr: &crate::parser::IfExpr, span: Span) -> Option<Operand> {
    let cond = self.lower_expr(&if_expr.condition)?;

    let then_block = self.new_block();
    let else_block = self.new_block();
    let merge_block = self.new_block();

    // Save the entry block before branching
    let entry_block = self.current_block;

    // Lower then branch first to determine the result type
    self.current_block = Some(then_block);
    let then_val = match &if_expr.then_branch {
        IfBranch::Expr(e) => self.lower_expr(e),
        IfBranch::Block(b) => self.lower_block(b),
    };
    let then_operand = then_val.unwrap_or(Operand::Constant(Constant::Unit));

    // Infer result type from then branch
    let result_ty = self.operand_type(&then_operand);

    // Now create the result variable with the correct type
    let result = self.new_temp(result_ty);

    // Go back to entry block and emit the branch terminator
    self.current_block = entry_block;
    self.terminate(Terminator::If {
        cond,
        then_block,
        else_block,
    });

    // Continue with then branch - emit assignment and goto
    self.current_block = Some(then_block);
    self.emit(StatementKind::Assign(result, Rvalue::Use(then_operand)));
    // Check if block already has terminator before adding goto
    if self.current_function().map(|f| f.blocks.get(then_block.0).and_then(|b| b.terminator.as_ref())).flatten().is_none() {
        self.terminate(Terminator::Goto(merge_block));
    }

    // Else branch
    self.current_block = Some(else_block);
    let else_val = if let Some(else_branch) = &if_expr.else_branch {
        match else_branch {
            ElseBranch::Expr(e) => self.lower_expr(e),
            ElseBranch::Block(b) => self.lower_block(b),
            ElseBranch::ElseIf(elif) => self.lower_if(elif, span),
        }
    } else {
        None
    };
    let else_operand = else_val.unwrap_or(Operand::Constant(Constant::Unit));
    self.emit(StatementKind::Assign(result, Rvalue::Use(else_operand)));
    if self.current_function().map(|f| f.blocks.get(else_block.0).and_then(|b| b.terminator.as_ref())).flatten().is_none() {
        self.terminate(Terminator::Goto(merge_block));
    }

    // Continue at merge block
    self.current_block = Some(merge_block);
    Some(Operand::Local(result))
}
```

---

## Task 20.3: Fix Try Operator (`?`) for Result - CRITICAL

### Problem
**File:** `src/mir/lower.rs` lines 1188-1193

The discriminant check uses `> 0` which works for Option but is **inverted** for Result:
- Option: None=0, Some=1 → `disc > 0` means Some ✓
- Result: Ok=0, Err=1 → `disc > 0` means Err (WRONG!)

### Solution
Add type detection and use appropriate check. Find the Try handling (around line 1160-1212) and update:

```rust
ExprKind::Try(inner) => {
    let inner_val = self.lower_expr(inner)?;

    // Store the value
    let scrutinee = self.new_temp(Ty::Int);
    self.emit(StatementKind::Assign(scrutinee, Rvalue::Use(inner_val)));

    // Get discriminant
    let disc = self.new_temp(Ty::Int);
    self.emit(StatementKind::Assign(disc, Rvalue::Discriminant(scrutinee)));

    // Create blocks
    let continue_block = self.new_block();
    let return_block = self.new_block();
    let merge_block = self.new_block();

    // Determine if this is Option or Result based on type inference
    let inner_ty = self.infer_expr_type(inner);
    let is_result = inner_ty.map(|t| matches!(t, Ty::Result(_, _))).unwrap_or(false);

    // Create the branch condition
    let should_continue = self.new_temp(Ty::Bool);
    if is_result {
        // Result: Ok=0, Err=1 → continue if discriminant == 0
        self.emit(StatementKind::Assign(
            should_continue,
            Rvalue::BinaryOp(BinOp::Eq, Operand::Copy(disc), Operand::Constant(Constant::Int(0))),
        ));
    } else {
        // Option: None=0, Some=1 → continue if discriminant > 0
        self.emit(StatementKind::Assign(
            should_continue,
            Rvalue::BinaryOp(BinOp::Gt, Operand::Copy(disc), Operand::Constant(Constant::Int(0))),
        ));
    }

    self.terminate(Terminator::If {
        cond: Operand::Local(should_continue),
        then_block: continue_block,
        else_block: return_block,
    });

    // Continue block: extract inner value
    self.current_block = Some(continue_block);
    let extracted = self.new_temp(Ty::Int);
    self.emit(StatementKind::Assign(extracted, Rvalue::EnumField(scrutinee, 0)));
    self.terminate(Terminator::Goto(merge_block));

    // Return block: early return with None/Err
    self.current_block = Some(return_block);
    self.terminate(Terminator::Return(Some(Operand::Copy(scrutinee))));

    self.current_block = Some(merge_block);
    Some(Operand::Local(extracted))
}
```

**Note:** This requires `infer_expr_type` method. If it doesn't exist, add a simpler heuristic or check the expression's AST for type annotations.

---

## Task 20.4: Add Missing validate_args! to 162 Builtins - HIGH

### Problem
**File:** `src/mir/interp.rs`

162 builtin functions access `args[0]`, `args[1]`, etc. without validation.

### Solution
Add `validate_args!` macro call to the start of each function.

**Pattern to apply:**

```rust
// BEFORE:
"function_name" => {
    match &args[0] {
        // ...
    }
}

// AFTER:
"function_name" => {
    validate_args!(args, 1, "function_name");
    match &args[0] {
        // ...
    }
}
```

**Full list (162 functions):**

| Line | Function | Args |
|------|----------|------|
| 1962 | map_values | 1 |
| 1980 | type_of | 1 |
| 2870 | mutex_try_lock | 1 |
| 2900 | mutex_unlock | 1 |
| 2913 | mutex_get | 1 |
| 2927 | mutex_set | 2 |
| 2944 | time_from_parts | 6 |
| 2957 | time_format | 2 |
| 2964 | time_format_iso | 1 |
| 2976 | time_parse | 2 |
| 2993 | time_parse_iso | 1 |
| 3009 | time_year | 1 |
| 3015 | time_month | 1 |
| 3021 | time_day | 1 |
| 3027 | time_hour | 1 |
| 3033 | time_minute | 1 |
| 3039 | time_second | 1 |
| 3045 | time_weekday | 1 |
| 3060 | time_add | 2 |
| 3066 | time_sub | 2 |
| 3072 | time_diff | 2 |
| 3080 | base64_encode | 1 |
| 3085 | base64_decode | 1 |
| 3108 | base64_encode_bytes | 1 |
| 3116 | base64_decode_bytes | 1 |
| 3132 | hex_encode | 1 |
| 3137 | hex_decode | 1 |
| 3160 | hex_encode_bytes | 1 |
| 3168 | hex_decode_bytes | 1 |
| 3186 | sha256 | 1 |
| 3194 | sha256_bytes | 1 |
| 3205 | hash_string | 1 |
| 3220 | uuid_parse | 1 |
| 3238 | regex_match | 2 |
| 3247 | regex_find | 2 |
| 3271 | regex_find_all | 2 |
| 3285 | regex_replace | 3 |
| 3295 | regex_replace_all | 3 |
| 3305 | regex_split | 2 |
| 3319 | regex_captures | 2 |
| 3351 | regex_is_valid | 1 |
| 3358 | exec | 1 |
| 3379 | env_set | 2 |
| 3387 | env_remove | 1 |
| 3413 | chdir | 1 |
| 3450 | path_join | 1 |
| 3462 | path_parent | 1 |
| 3479 | path_filename | 1 |
| 3496 | path_stem | 1 |
| 3513 | path_extension | 1 |
| 3530 | path_is_absolute | 1 |
| 3535 | path_is_relative | 1 |
| 3540 | path_absolute | 1 |
| 3556 | file_is_file | 1 |
| 3561 | file_is_dir | 1 |
| 3566 | file_size | 1 |
| 3582 | dir_create | 1 |
| 3598 | dir_create_all | 1 |
| 3614 | dir_remove | 1 |
| 3630 | dir_remove_all | 1 |
| 3646 | dir_list | 1 |
| 3668 | file_copy | 2 |
| 3685 | file_move | 2 |
| 3702 | file_remove | 1 |
| 3720 | http_get | 1 |
| 3744 | http_post | 2 |
| 3770 | http_post_json | 2 |
| 3796 | http_put | 2 |
| 3822 | http_delete | 1 |
| 3849 | http_response | 2 |
| 3859 | http_response_with_headers | 3 |
| 3870 | http_json_response | 2 |
| 3883 | http_redirect | 1 |
| 3894 | http_file_response | 1 |
| 3932 | http_req_json | 1 |
| 3956 | http_req_form | 1 |
| 3980 | http_req_param | 2 |
| 4005 | http_req_header | 2 |
| 4036 | http_serve | 2 |
| 4253 | http_request_new | 3 |
| 4285 | tcp_connect | 2 |
| 4308 | tcp_read | 2 |
| 4338 | tcp_read_exact | 2 |
| 4367 | tcp_read_line | 1 |
| 4404 | tcp_write | 2 |
| 4430 | tcp_write_all | 2 |
| 4456 | tcp_close | 1 |
| 4462 | tcp_set_timeout | 2 |
| 4473 | tcp_peer_addr | 1 |
| 4485 | tcp_local_addr | 1 |
| 4497 | tcp_listen | 2 |
| 4520 | tcp_accept | 1 |
| 4549 | tcp_listener_close | 1 |
| 4557 | udp_bind | 2 |
| 4580 | udp_send_to | 3 |
| 4608 | udp_recv_from | 2 |
| 4643 | udp_close | 1 |
| 4649 | udp_connect | 3 |
| 4676 | udp_send | 2 |
| 4701 | udp_recv | 2 |
| 4732 | dns_lookup | 1 |
| 4754 | dns_reverse_lookup | 1 |
| 4778 | ptr_is_null | 1 |
| 4783 | ptr_offset | 2 |
| 4794 | ptr_addr | 1 |
| 4799 | ptr_from_addr | 1 |
| 4806 | str_to_cstr | 1 |
| 4814 | cstr_to_str | 1 |
| 4825 | cstr_to_str_len | 2 |
| 4837 | cstr_free | 1 |
| 4849 | alloc | 1 |
| 4863 | alloc_zeroed | 1 |
| 4877 | dealloc | 2 |
| 4887 | mem_copy | 3 |
| 4897 | mem_set | 3 |
| 4909-4974 | C type conversions | 1 each (14 functions) |
| 4979 | sizeof | 1 |
| 5001 | json_parse | 1 |
| 5020 | json_stringify | 1 |
| 5028 | json_stringify_pretty | 1 |
| 5036 | json_get | 2 |
| 5059 | json_get_str | 2 |
| 5082 | json_get_int | 2 |
| 5105 | json_get_float | 2 |
| 5128 | json_get_bool | 2 |
| 5151 | json_get_array | 2 |
| 5177 | json_array_get | 2 |
| 5200 | json_array_len | 1 |
| 5209 | json_keys | 1 |
| 5220 | json_values | 1 |
| 5231 | json_type | 1 |
| 5247-5282 | json_is_* | 1 each (6 functions) |
| 5289 | json_from_str | 1 |
| 5296 | json_from_int | 1 |
| 5303 | json_from_float | 1 |
| 5311 | json_from_bool | 1 |
| 5329 | json_set | 3 |
| 5347 | json_has | 2 |
| 5360 | json_to_value | 1 |
| 5396-5447 | sort_* | 1 each (5 functions) |
| 5462 | reverse | 1 |
| 5472 | shuffle | 1 |
| 5486 | min_of | 1 |
| 5506 | max_of | 1 |
| 5526 | sum_of | 1 |
| 5535 | binary_search | 2 |
| 5562-5598 | log_* | 1 each (4 functions) |
| 5610 | log_set_level | 1 |
| 5622 | log_set_format | 1 |
| 5633 | tls_connect | 2 |
| 5674 | tls_read | 2 |
| 5703 | tls_write | 2 |
| 5727 | tls_close | 1 |
| 5735 | gzip_compress | 1 |
| 5752 | gzip_decompress | 1 |
| 5775 | zlib_compress | 1 |
| 5792 | zlib_decompress | 1 |
| 5817 | db_open | 1 |
| 5859 | db_execute | 2 |
| 5882 | db_query | 2 |
| 5940 | db_query_one | 2 |
| 6002 | db_close | 1 |
| 6008 | db_prepare | 2 |
| 6044 | db_execute_prepared | 2 |
| 6105 | db_query_prepared | 2 |
| 6197-6271 | row_* | 1-2 each (7 functions) |

---

## Task 20.5: Document FFI as Unsafe - HIGH

### Problem
FFI pointer operations accept arbitrary addresses without validation.

### Solution
Add clear documentation comments. In `src/mir/interp.rs`:

```rust
// Before cstr_to_str (around line 4814):
// SAFETY: These FFI functions operate on raw pointers provided by user code.
// Passing invalid addresses will cause undefined behavior or crashes.
// Users are responsible for ensuring pointer validity.
```

Also add to `KNOWN_LIMITATIONS.md`:

```markdown
### FFI Safety

The following functions operate on raw pointers and are inherently unsafe:
- `cstr_to_str`, `cstr_to_str_len`, `cstr_free`
- `alloc`, `alloc_zeroed`, `dealloc`
- `mem_copy`, `mem_set`
- `ptr_*` functions

Passing invalid addresses will cause undefined behavior. Use with caution.
```

---

## Task 20.6: Document Environment Variable Thread Safety - HIGH

### Problem
`env_set` and `env_remove` use unsafe operations that can race with spawned tasks.

### Solution
Add documentation in `src/mir/interp.rs` and `KNOWN_LIMITATIONS.md`:

```rust
// Before env_set (around line 3379):
// WARNING: Environment variable modifications are not thread-safe.
// Using env_set/env_remove with spawned tasks may cause data races.
// Prefer passing configuration through function arguments instead.
```

---

## Task 20.7: Document/Fix Closure Memory Leak - HIGH

### Problem
**File:** `src/codegen/llvm.rs` lines 567-584

Closure environments allocated with `malloc` are never freed.

### Solution (Option A - Document):
Add to `KNOWN_LIMITATIONS.md`:

```markdown
### LLVM Closure Memory

Closures in compiled code allocate environment memory that is not automatically freed.
For long-running programs with many closure creations, consider using the interpreter
or restructuring code to avoid closure-heavy patterns.
```

### Solution (Option B - Fix):
This requires reference counting or GC, which is complex. For v1.x, documentation is sufficient.

---

## Task 20.8: Fix Spawned Task Error Handling - HIGH

### Problem
**File:** `src/mir/interp.rs` lines 1031-1034

Errors in spawned closures are silently converted to `Unit`:

```rust
match task_interp.execute(&func) {
    Ok(val) => val,
    Err(_) => Value::Unit,  // Error swallowed!
}
```

### Solution
Return a Result-like value:

```rust
match task_interp.execute(&func) {
    Ok(val) => val,
    Err(e) => {
        // Return as a Result::Err variant
        Value::Enum {
            variant: "Err".to_string(),
            discriminant: 1,
            fields: vec![Value::Str(e.message)],
        }
    }
}
```

Or log the error:

```rust
match task_interp.execute(&func) {
    Ok(val) => val,
    Err(e) => {
        eprintln!("Spawned task error: {}", e.message);
        Value::Unit
    }
}
```

---

## Task 20.9: Fix Undefined Locals Returning Unit - HIGH

### Problem
**File:** `src/mir/interp.rs` lines 6299-6303

Accessing undefined locals silently returns `Unit`:

```rust
let val = frame.locals.get(local).cloned().unwrap_or(Value::Unit);
```

### Solution
Return an error instead:

```rust
let val = frame.locals.get(local).cloned()
    .ok_or_else(|| InterpError {
        message: format!("Internal error: undefined local {:?}", local)
    })?;
```

Apply to all similar patterns (lines 6356, 6392).

---

## Verification

After ALL tasks, run:
```bash
cargo test
cargo clippy -- -D warnings
forma check std/*.forma
forma check examples/*.forma
```

Create test file `examples/test_operators.forma`:
```forma
# Test coalesce
f test_coalesce()
    a: Option[Int] := Some(5)
    b: Option[Int] := None

    x := a ?? 10  # Should be 5
    y := b ?? 10  # Should be 10

    print(f"a ?? 10 = {x}")  # Expected: 5
    print(f"b ?? 10 = {y}")  # Expected: 10

# Test if-expression types
f test_if_types()
    s := if true then "hello" else "world"
    print(f"String result: {s}")  # Should work, not type error

# Test try operator with Result
f test_try() -> Result[Int, Str]
    ok_val: Result[Int, Str] := Ok(42)
    err_val: Result[Int, Str] := Err("failed")

    x := ok_val?   # Should continue with 42
    # y := err_val?  # Should early return with Err

    Ok(x)

f main()
    test_coalesce()
    test_if_types()
    m test_try()
        Ok(n) -> print(f"Got: {n}")
        Err(e) -> print(f"Error: {e}")
```

---

## Summary

| Task | Description | Priority | Effort |
|------|-------------|----------|--------|
| 20.1 | Fix coalesce operator | CRITICAL | Medium |
| 20.2 | Fix if-expression type | CRITICAL | Medium |
| 20.3 | Fix try operator for Result | CRITICAL | Medium |
| 20.4 | Add 162 validate_args! | HIGH | High (repetitive) |
| 20.5 | Document FFI unsafe | HIGH | Low |
| 20.6 | Document env var thread safety | HIGH | Low |
| 20.7 | Document closure memory leak | HIGH | Low |
| 20.8 | Fix spawned task errors | HIGH | Low |
| 20.9 | Fix undefined locals error | HIGH | Low |

---

*"Fix the foundation before adding floors."*
