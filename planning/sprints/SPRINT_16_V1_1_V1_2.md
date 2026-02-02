# Sprint 16: Complete v1.1 and v1.2 Features

**Date:** January 26, 2026
**Goal:** Implement all remaining features for v1.1 and v1.2
**CRITICAL:** Follow these specifications EXACTLY. Do not improvise or create alternative solutions.

---

## Overview

| Version | Features | Effort |
|---------|----------|--------|
| v1.1 | True async (Tokio), LLVM indirect closures | 3-4 days |
| v1.2 | Loop labels, proper tuple iteration, multiline expressions, grammar export | 2-3 days |

**Total: ~6 days of focused work**

---

## PART 1: v1.1 Features

---

## Task 16.1: True Async Parallelism with Tokio

**Files to modify:**
- `src/mir/interp.rs`
- `Cargo.toml` (verify tokio features)

### Step 1: Add Tokio task storage to Interpreter struct

**Location:** `src/mir/interp.rs` - Find the `Interpreter` struct (around line 286)

**ADD these fields to the struct:**
```rust
use std::sync::{Arc, Mutex as StdMutex};
use tokio::task::JoinHandle;

pub struct Interpreter {
    // ... existing fields ...

    /// Tokio runtime for spawning async tasks
    runtime: Arc<tokio::runtime::Runtime>,

    /// Active spawned tasks: maps task ID to JoinHandle
    spawned_tasks: Arc<StdMutex<std::collections::HashMap<u64, JoinHandle<Value>>>>,

    /// Next task ID
    next_task_id: u64,
}
```

### Step 2: Add TokioTask variant to Value enum

**Location:** `src/mir/interp.rs` - Find the `Value` enum (around line 25)

**ADD this variant:**
```rust
pub enum Value {
    // ... existing variants ...

    /// Tokio task handle ID (references spawned_tasks map)
    TokioTask(u64),
}
```

### Step 3: Update Interpreter::new() constructor

**Location:** `src/mir/interp.rs` - Find `Interpreter::new()` (around line 329)

**ADD initialization:**
```rust
impl Interpreter {
    pub fn new(program: Program) -> Self {
        let runtime = Arc::new(
            tokio::runtime::Runtime::new()
                .expect("failed to create Tokio runtime")
        );

        Self {
            program,
            call_stack: Vec::new(),
            max_steps: 1_000_000,
            // ... existing field initializations ...

            // NEW:
            runtime,
            spawned_tasks: Arc::new(StdMutex::new(std::collections::HashMap::new())),
            next_task_id: 0,
        }
    }
}
```

### Step 4: Rewrite Spawn terminator

**Location:** `src/mir/interp.rs` - Find `Terminator::Spawn` handling (around line 933)

**REPLACE the entire Spawn handler with:**
```rust
Terminator::Spawn { expr, dest, next } => {
    // Evaluate the expression to get the value to spawn
    let value = self.eval_operand(&expr)?;

    // Create task ID
    let task_id = self.next_task_id;
    self.next_task_id += 1;

    // Clone runtime for spawning
    let runtime = Arc::clone(&self.runtime);
    let spawned_tasks = Arc::clone(&self.spawned_tasks);

    // Spawn the task onto Tokio runtime
    let handle = runtime.spawn(async move {
        // The value is already evaluated, just return it
        // For true async, this would execute an async block
        value
    });

    // Store the handle
    {
        let mut tasks = spawned_tasks.lock().unwrap();
        tasks.insert(task_id, handle);
    }

    // Store task ID in destination
    let frame = self.current_frame_mut()?;
    if let Some(d) = dest {
        frame.locals.insert(d, Value::TokioTask(task_id));
    }
    frame.current_block = next;
}
```

### Step 5: Rewrite Await terminator

**Location:** `src/mir/interp.rs` - Find `Terminator::Await` handling (around line 953)

**REPLACE the entire Await handler with:**
```rust
Terminator::Await { task, dest, next } => {
    let value = self.eval_operand(&task)?;

    let result = match value {
        Value::TokioTask(task_id) => {
            // Get and remove the task handle
            let handle = {
                let mut tasks = self.spawned_tasks.lock().unwrap();
                tasks.remove(&task_id)
            };

            if let Some(handle) = handle {
                // Block on the async task using runtime
                match self.runtime.block_on(handle) {
                    Ok(val) => val,
                    Err(e) => {
                        return Err(InterpError {
                            message: format!("task panicked: {}", e),
                        });
                    }
                }
            } else {
                return Err(InterpError {
                    message: format!("task {} not found or already awaited", task_id),
                });
            }
        }
        // Backwards compatibility with old Task/Future values
        Value::Task(inner) => *inner,
        Value::Future(inner) => *inner,
        other => other,
    };

    let frame = self.current_frame_mut()?;
    if let Some(d) = dest {
        frame.locals.insert(d, result);
    }
    frame.current_block = next;
}
```

### Step 6: Rewrite sleep_async builtin

**Location:** `src/mir/interp.rs` - Find `"sleep_async"` builtin (around line 2314)

**REPLACE with:**
```rust
"sleep_async" => {
    let ms = match &args[0] {
        Value::Int(n) => *n as u64,
        _ => return Err(InterpError { message: "sleep_async: expected Int".to_string() })
    };

    // Create task ID
    let task_id = self.next_task_id;
    self.next_task_id += 1;

    // Spawn async sleep
    let handle = self.runtime.spawn(async move {
        tokio::time::sleep(tokio::time::Duration::from_millis(ms)).await;
        Value::Unit
    });

    // Store handle
    {
        let mut tasks = self.spawned_tasks.lock().unwrap();
        tasks.insert(task_id, handle);
    }

    Ok(Some(Value::TokioTask(task_id)))
}
```

### Step 7: Rewrite await_all builtin

**Location:** `src/mir/interp.rs` - Find `"await_all"` builtin (around line 2346)

**REPLACE with:**
```rust
"await_all" => {
    let tasks = match &args[0] {
        Value::Array(arr) => arr.clone(),
        _ => return Err(InterpError { message: "await_all: expected array of tasks".to_string() })
    };

    // Collect all task IDs
    let task_ids: Vec<u64> = tasks.iter().filter_map(|t| {
        match t {
            Value::TokioTask(id) => Some(*id),
            _ => None,
        }
    }).collect();

    // Get all handles
    let handles: Vec<_> = {
        let mut task_map = self.spawned_tasks.lock().unwrap();
        task_ids.iter().filter_map(|id| task_map.remove(id)).collect()
    };

    // Await all concurrently using join_all
    let results: Vec<Value> = self.runtime.block_on(async {
        let futures: Vec<_> = handles.into_iter().collect();
        let results = futures::future::join_all(futures).await;
        results.into_iter().map(|r| r.unwrap_or(Value::Unit)).collect()
    });

    Ok(Some(Value::Array(results)))
}
```

### Step 8: Add futures crate to Cargo.toml

**Location:** `Cargo.toml`

**ADD:**
```toml
futures = "0.3"
```

### Step 9: Add imports at top of interp.rs

**Location:** `src/mir/interp.rs` - Add near other imports

**ADD:**
```rust
use std::sync::{Arc, Mutex as StdMutex};
use tokio::task::JoinHandle;
```

### Acceptance Criteria:
- [ ] `sp` spawns actual Tokio tasks
- [ ] `aw` awaits tasks with real blocking
- [ ] `await_all` runs tasks concurrently (use `futures::future::join_all`)
- [ ] `sleep_async` uses `tokio::time::sleep`
- [ ] async_downloader.forma shows parallel execution (faster than sequential)
- [ ] All existing tests pass

---

## Task 16.2: LLVM Indirect Closure Calls

**Files to modify:**
- `src/codegen/llvm.rs`

### Step 1: Add closure-related fields to LLVMCodegen struct

**Location:** `src/codegen/llvm.rs` - Find the `LLVMCodegen` struct (around line 57)

**ADD these fields:**
```rust
pub struct LLVMCodegen<'ctx> {
    // ... existing fields ...

    /// Closure environment types for each closure local
    closure_env_types: HashMap<usize, inkwell::types::StructType<'ctx>>,
}
```

**UPDATE the constructor to initialize:**
```rust
closure_env_types: HashMap::new(),
```

### Step 2: Implement compile_closure method

**Location:** `src/codegen/llvm.rs` - Add this new method

**ADD:**
```rust
fn compile_closure(
    &mut self,
    func_name: &str,
    captures: &[Operand],
) -> Result<BasicValueEnum<'ctx>, CodegenError> {
    // 1. Compile captured values and determine their types
    let mut env_field_types: Vec<BasicTypeEnum> = Vec::new();
    let mut env_values: Vec<BasicValueEnum> = Vec::new();

    for cap in captures {
        let val = self.compile_operand(cap)?;
        env_field_types.push(val.get_type());
        env_values.push(val);
    }

    // 2. Create environment struct type
    let env_struct_type = self.context.struct_type(
        &env_field_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
        false
    );

    // 3. Allocate environment on heap (call malloc)
    let i64_type = self.context.i64_type();
    let i8_ptr_type = self.context.i8_type().ptr_type(inkwell::AddressSpace::default());

    // Get or declare malloc
    let malloc_fn = self.module.get_function("malloc").unwrap_or_else(|| {
        let malloc_type = i8_ptr_type.fn_type(&[i64_type.into()], false);
        self.module.add_function("malloc", malloc_type, None)
    });

    // Calculate size and allocate
    let env_size = env_struct_type.size_of().unwrap();
    let env_ptr_i8 = self.builder
        .build_call(malloc_fn, &[env_size.into()], "env_alloc")
        .map_err(|e| CodegenError { message: format!("malloc call failed: {:?}", e) })?
        .try_as_basic_value()
        .left()
        .ok_or_else(|| CodegenError { message: "malloc returned void".into() })?
        .into_pointer_value();

    // 4. Cast to environment struct pointer
    let env_struct_ptr = self.builder
        .build_pointer_cast(
            env_ptr_i8,
            env_struct_type.ptr_type(inkwell::AddressSpace::default()),
            "env_struct_ptr"
        )
        .map_err(|e| CodegenError { message: format!("pointer cast failed: {:?}", e) })?;

    // 5. Store captured values into environment struct
    for (i, val) in env_values.iter().enumerate() {
        let field_ptr = self.builder
            .build_struct_gep(env_struct_type, env_struct_ptr, i as u32, &format!("env_field_{}", i))
            .map_err(|e| CodegenError { message: format!("struct gep failed: {:?}", e) })?;
        self.builder
            .build_store(field_ptr, *val)
            .map_err(|e| CodegenError { message: format!("store failed: {:?}", e) })?;
    }

    // 6. Get the lifted function pointer
    let lifted_fn = self.functions.get(func_name)
        .copied()
        .ok_or_else(|| CodegenError {
            message: format!("Lifted closure function not found: {}", func_name),
        })?;

    // 7. Cast function pointer to i8*
    let fn_ptr_i8 = self.builder
        .build_pointer_cast(
            lifted_fn.as_global_value().as_pointer_value(),
            i8_ptr_type,
            "fn_ptr_i8"
        )
        .map_err(|e| CodegenError { message: format!("fn pointer cast failed: {:?}", e) })?;

    // 8. Cast env pointer back to i8*
    let env_ptr_i8_final = self.builder
        .build_pointer_cast(env_struct_ptr, i8_ptr_type, "env_ptr_i8")
        .map_err(|e| CodegenError { message: format!("env pointer cast failed: {:?}", e) })?;

    // 9. Create closure fat pointer struct: { i8*, i8* }
    let closure_struct_type = self.context.struct_type(
        &[i8_ptr_type.into(), i8_ptr_type.into()],
        false
    );

    // Build the struct value
    let closure_undef = closure_struct_type.get_undef();
    let closure_with_fn = self.builder
        .build_insert_value(closure_undef, fn_ptr_i8, 0, "closure_fn")
        .map_err(|e| CodegenError { message: format!("insert fn failed: {:?}", e) })?;
    let closure_complete = self.builder
        .build_insert_value(closure_with_fn, env_ptr_i8_final, 1, "closure_env")
        .map_err(|e| CodegenError { message: format!("insert env failed: {:?}", e) })?;

    Ok(closure_complete.into_struct_value().into())
}
```

### Step 3: Add Closure case to compile_rvalue

**Location:** `src/codegen/llvm.rs` - Find `compile_rvalue` method (around line 244)

**ADD this case before the catch-all:**
```rust
Rvalue::Closure { func_name, captures } => {
    self.compile_closure(func_name, captures)
}
```

### Step 4: Implement indirect call handler

**Location:** `src/codegen/llvm.rs` - Find `Terminator::CallIndirect` (around line 581)

**REPLACE the entire handler with:**
```rust
Terminator::CallIndirect { callee, args, dest, next } => {
    let i8_ptr_type = self.context.i8_type().ptr_type(inkwell::AddressSpace::default());
    let i64_type = self.context.i64_type();

    // 1. Compile the closure operand (should be a fat pointer struct)
    let closure_val = self.compile_operand(callee)?;
    let closure_struct = closure_val.into_struct_value();

    // 2. Extract function pointer and environment pointer
    let fn_ptr_i8 = self.builder
        .build_extract_value(closure_struct, 0, "fn_ptr_raw")
        .map_err(|e| CodegenError { message: format!("extract fn_ptr failed: {:?}", e) })?
        .into_pointer_value();
    let env_ptr = self.builder
        .build_extract_value(closure_struct, 1, "env_ptr")
        .map_err(|e| CodegenError { message: format!("extract env_ptr failed: {:?}", e) })?
        .into_pointer_value();

    // 3. Compile arguments (environment pointer is implicit first arg)
    let mut compiled_args: Vec<BasicMetadataValueEnum> = vec![env_ptr.into()];
    for arg in args {
        let val = self.compile_operand(arg)?;
        compiled_args.push(val.into());
    }

    // 4. Build function type: (i8*, args...) -> i64
    // Environment pointer is always first parameter
    let mut param_types: Vec<BasicMetadataTypeEnum> = vec![i8_ptr_type.into()];
    for _ in args {
        param_types.push(i64_type.into());
    }
    let fn_type = i64_type.fn_type(&param_types, false);

    // 5. Cast function pointer to correct type
    let fn_ptr_typed = self.builder
        .build_pointer_cast(
            fn_ptr_i8,
            fn_type.ptr_type(inkwell::AddressSpace::default()),
            "fn_ptr_typed"
        )
        .map_err(|e| CodegenError { message: format!("fn type cast failed: {:?}", e) })?;

    // 6. Build indirect call
    let call = self.builder
        .build_indirect_call(fn_type, fn_ptr_typed, &compiled_args, "indirect_call")
        .map_err(|e| CodegenError { message: format!("indirect call failed: {:?}", e) })?;

    // 7. Store result if there's a destination
    if let Some(local) = dest {
        if let Some(result) = call.try_as_basic_value().left() {
            if let Some(alloca) = self.locals.get(&(local.0 as usize)) {
                self.builder.build_store(*alloca, result)
                    .map_err(|e| CodegenError { message: format!("store failed: {:?}", e) })?;
            }
        }
    }

    // 8. Jump to next block
    if let Some(&bb) = blocks.get(&(next.0 as usize)) {
        self.builder.build_unconditional_branch(bb)
            .map_err(|e| CodegenError { message: format!("branch failed: {:?}", e) })?;
    }
}
```

### Acceptance Criteria:
- [ ] Closures stored in variables work in compiled mode
- [ ] Closures passed to functions work
- [ ] Closures returned from functions work
- [ ] Environment captures work correctly
- [ ] All closure tests pass in both interpreter and compiled mode

---

## PART 2: v1.2 Features

---

## Task 16.3: Loop Labels

**Files to modify:**
- `src/parser/ast.rs`
- `src/parser/parser.rs`
- `src/mir/lower.rs`

### Step 1: Extend LoopContext with label

**Location:** `src/mir/lower.rs` - Find `LoopContext` struct (around line 68)

**CHANGE TO:**
```rust
#[derive(Debug, Clone)]
struct LoopContext {
    label: Option<String>,  // ADD THIS
    continue_block: BlockId,
    break_block: BlockId,
    result_local: Option<Local>,
}
```

### Step 2: Update break handling in lower_expr

**Location:** `src/mir/lower.rs` - Find `ExprKind::Break` handling (around line 809)

**REPLACE with:**
```rust
ExprKind::Break(label, value) => {
    // Find the target loop context by label
    let target_ctx = if let Some(label_ident) = label {
        self.loop_stack.iter().rev()
            .find(|ctx| ctx.label.as_ref() == Some(&label_ident.name))
            .cloned()
    } else {
        self.loop_stack.last().cloned()
    };

    if let Some(ctx) = target_ctx {
        if let Some(val) = value.as_ref().and_then(|v| self.lower_expr(v)) {
            if let Some(result_local) = ctx.result_local {
                self.emit(StatementKind::Assign(result_local, Rvalue::Use(val)));
            }
        }
        self.terminate(Terminator::Goto(ctx.break_block));
    } else if let Some(label_ident) = label {
        // Label not found - this would be caught by type checker ideally
        eprintln!("Warning: break label '{}' not found", label_ident.name);
    }
    None
}
```

### Step 3: Update continue handling in lower_expr

**Location:** `src/mir/lower.rs` - Find `ExprKind::Continue` handling (around line 820)

**REPLACE with:**
```rust
ExprKind::Continue(label) => {
    // Find the target loop context by label
    let target_ctx = if let Some(label_ident) = label {
        self.loop_stack.iter().rev()
            .find(|ctx| ctx.label.as_ref() == Some(&label_ident.name))
            .cloned()
    } else {
        self.loop_stack.last().cloned()
    };

    if let Some(ctx) = target_ctx {
        self.terminate(Terminator::Goto(ctx.continue_block));
    } else if let Some(label_ident) = label {
        eprintln!("Warning: continue label '{}' not found", label_ident.name);
    }
    None
}
```

### Step 4: Update for loop context push

**Location:** `src/mir/lower.rs` - Find for loop handling where `loop_stack.push` is called (around line 1856)

**CHANGE the push to include label:**
```rust
// Extract label from the for expression if present
// For now, use None since AST doesn't carry labels yet
self.loop_stack.push(LoopContext {
    label: None,  // Will be populated when AST is updated
    continue_block: incr_block,
    break_block: exit_block,
    result_local: None,
});
```

### Step 5: Update while loop context push

**Location:** `src/mir/lower.rs` - Find while loop handling (around line 2045)

**CHANGE similarly:**
```rust
self.loop_stack.push(LoopContext {
    label: None,
    continue_block: cond_block,
    break_block: exit_block,
    result_local: None,
});
```

### Step 6: Update infinite loop context push

**Location:** `src/mir/lower.rs` - Find loop handling (around line 2085)

**CHANGE similarly:**
```rust
self.loop_stack.push(LoopContext {
    label: None,
    continue_block: body_block,
    break_block: exit_block,
    result_local: Some(result),
});
```

### Step 7: Parse break labels

**Location:** `src/parser/parser.rs` - Find break parsing (around line 1768)

**REPLACE with:**
```rust
if self.match_token(TokenKind::Br) {
    // Check for optional label (identifier starting with ')
    let label = if let Some(TokenKind::Ident(name)) = self.current().map(|t| &t.kind) {
        if name.starts_with('\'') {
            let label_name = name.clone();
            self.advance();
            Some(Ident {
                name: label_name,
                span: self.previous_span(),
            })
        } else {
            None
        }
    } else {
        None
    };

    let value = if self.check_expr_start() {
        Some(Box::new(self.parse_expr()?))
    } else {
        None
    };
    return Ok(Expr {
        kind: ExprKind::Break(label, value),
        span: start.merge(self.previous_span()),
    });
}
```

### Step 8: Parse continue labels

**Location:** `src/parser/parser.rs` - Find continue parsing (around line 1781)

**REPLACE with:**
```rust
if self.match_token(TokenKind::Ct) {
    // Check for optional label
    let label = if let Some(TokenKind::Ident(name)) = self.current().map(|t| &t.kind) {
        if name.starts_with('\'') {
            let label_name = name.clone();
            self.advance();
            Some(Ident {
                name: label_name,
                span: self.previous_span(),
            })
        } else {
            None
        }
    } else {
        None
    };

    return Ok(Expr {
        kind: ExprKind::Continue(label),
        span: start.merge(self.previous_span()),
    });
}
```

### Acceptance Criteria:
- [ ] `br 'label` breaks to labeled loop
- [ ] `ct 'label` continues labeled loop
- [ ] Unlabeled break/continue still work (innermost loop)
- [ ] All existing loop tests pass

---

## Task 16.4: Proper Tuple Iteration (Remove Encoding Hack)

**Files to modify:**
- `std/iter.forma`

### Step 1: Replace encoded enumerate with struct-based approach

**Location:** `std/iter.forma` - Find enumerate functions (around line 176)

**REPLACE the entire enumerate section with:**
```forma
# ============================================================
# Enumerate - Proper Implementation
# ============================================================

# Enumerated element with index and value
s EnumeratedInt
    index: Int
    value: Int

# Enumerate an integer array - returns array of EnumeratedInt
f array_enumerate(arr: [Int]) -> [EnumeratedInt]
    enumerate_helper(arr, 0, [])

f enumerate_helper(arr: [Int], idx: Int, acc: [EnumeratedInt]) -> [EnumeratedInt]
    if idx >= vec_len(arr)
        acc
    else
        m vec_get(arr, idx)
            Some(x) ->
                elem := EnumeratedInt { index: idx, value: x }
                enumerate_helper(arr, idx + 1, vec_push(acc, elem))
            None -> acc

# ============================================================
# Legacy Encoded Enumerate (Deprecated)
# ============================================================

# DEPRECATED: Use array_enumerate instead
# Kept for backwards compatibility only
# Encode (idx, x) as idx * 1000000 + x (assumes values < 1000000)
f array_enumerate_encoded(arr: [Int]) -> [Int]
    enumerate_loop_encoded(arr, 0, [])

f enumerate_loop_encoded(arr: [Int], idx: Int, acc: [Int]) -> [Int]
    if idx >= vec_len(arr)
        acc
    else
        m vec_get(arr, idx)
            Some(x) ->
                encoded := idx * 1000000 + x
                enumerate_loop_encoded(arr, idx + 1, vec_push(acc, encoded))
            None -> acc

# DEPRECATED: Use elem.index instead
f decode_index(encoded: Int) -> Int
    encoded / 1000000

# DEPRECATED: Use elem.value instead
f decode_value(encoded: Int) -> Int
    encoded % 1000000
```

### Acceptance Criteria:
- [ ] `array_enumerate` returns `[EnumeratedInt]`
- [ ] Can access `.index` and `.value` on elements
- [ ] Legacy encoded functions still work (deprecated)
- [ ] No more 1,000,000 value limit for new code

---

## Task 16.5: Multiline Expression Improvements

**Files to modify:**
- `src/parser/parser.rs`

### Step 1: Allow continuation after binary operators

**Location:** `src/parser/parser.rs` - Find binary expression parsing

**The issue:** When a line ends with a binary operator, the parser doesn't look for the continuation on the next line.

**Find the `parse_binary_expr` or similar function and ensure it skips newlines after operators:**

**ADD after consuming a binary operator:**
```rust
// Skip newlines after binary operator to allow continuation
while self.check(TokenKind::Newline) {
    self.advance();
}
```

### Step 2: Add test case

**Create:** `tests/forma/test_multiline_expr.forma`

```forma
# Test multiline expressions

f test_multiline_and() -> Bool
    a := true
    b := true
    result := a &&
        b
    result

f test_multiline_or() -> Bool
    a := false
    b := true
    result := a ||
        b
    result

f test_multiline_arithmetic() -> Int
    x := 1 +
        2 +
        3
    x

f main() -> Int
    if test_multiline_and() && test_multiline_or() && test_multiline_arithmetic() == 6
        0
    else
        1
```

### Acceptance Criteria:
- [ ] `a &&\n    b` parses correctly
- [ ] `a ||\n    b` parses correctly
- [ ] `a +\n    b` parses correctly
- [ ] Test file passes

---

## Task 16.6: Grammar Export Completeness

**Files to modify:**
- `src/main.rs`

### Step 1: Add shorthand keyword documentation to grammar

**Location:** `src/main.rs` - Find grammar export (around line 1295)

**ADD to the EBNF output:**
```rust
// Add after existing grammar rules
println!("
(* Shorthand Keywords *)
(* These are aliases for common keywords to reduce token count *)
shorthand_keyword = 'f' (* function *)
                  | 's' (* struct *)
                  | 'e' (* enum *)
                  | 't' (* trait *)
                  | 'i' (* impl *)
                  | 'm' (* match *)
                  | 'us' (* use *)
                  | 'wh' (* while *)
                  | 'lp' (* loop *)
                  | 'br' (* break *)
                  | 'ct' (* continue *)
                  | 'ret' (* return *)
                  | 'as' (* async *)
                  | 'sp' (* spawn *)
                  | 'aw' (* await *) ;

(* Indentation Rules *)
(* FORMA uses significant whitespace like Python *)
(* Blocks are delimited by INDENT and DEDENT tokens *)
(* INDENT is generated when indentation increases *)
(* DEDENT is generated when indentation decreases *)
(* Tab characters are not allowed - use spaces only *)
indentation = INDENT statement* DEDENT ;

(* Contextual Keywords *)
(* Single-letter keywords can be used as identifiers when unambiguous *)
(* The parser uses lookahead to determine if f/s/e/t/i/m is a keyword or identifier *)
(* Example: 'f' followed by identifier and '(' is function definition *)
(* Example: 'f' followed by ':' is identifier in struct field *)
");
```

### Step 2: Add operator precedence table

**ADD to grammar output:**
```rust
println!("
(* Operator Precedence (highest to lowest) *)
(* 1. Primary: literals, identifiers, parenthesized expressions *)
(* 2. Postfix: function calls, method calls, field access, indexing *)
(* 3. Unary: -, !, & *)
(* 4. Multiplicative: *, /, % *)
(* 5. Additive: +, - *)
(* 6. Shift: <<, >> *)
(* 7. Bitwise AND: & *)
(* 8. Bitwise XOR: ^ *)
(* 9. Bitwise OR: | *)
(* 10. Comparison: ==, !=, <, >, <=, >= *)
(* 11. Logical AND: && *)
(* 12. Logical OR: || *)
(* 13. Range: .., ..= *)
(* 14. Assignment: =, +=, -=, *=, /=, %= *)
");
```

### Acceptance Criteria:
- [ ] `forma grammar` includes shorthand keywords
- [ ] `forma grammar` includes indentation rules
- [ ] `forma grammar` includes operator precedence
- [ ] Grammar is valid EBNF

---

## Verification

After completing all tasks, run:

```bash
# Build
cargo build --release

# Run all tests
cargo test

# Test async parallelism
cargo run -- run examples/async_downloader.forma

# Test closures in compiled mode (if LLVM available)
cargo run -- compile tests/forma/test_closures.forma

# Test loop labels
cargo run -- run tests/forma/test_loop_labels.forma

# Test multiline expressions
cargo run -- run tests/forma/test_multiline_expr.forma

# Test enumerate
cargo run -- run tests/forma/test_enumerate.forma

# Export grammar
cargo run -- grammar > /tmp/forma_grammar.ebnf

# Full verification
scripts/verify_v1.sh
```

---

## Definition of Done

### v1.1
- [ ] Async spawn creates actual Tokio tasks
- [ ] Await blocks on real async operations
- [ ] await_all runs tasks in parallel
- [ ] sleep_async is non-blocking
- [ ] LLVM indirect closure calls work
- [ ] Closures can be stored in variables (compiled mode)

### v1.2
- [ ] Loop labels parse correctly
- [ ] `br 'label` and `ct 'label` work
- [ ] Proper tuple-based enumerate (no encoding hack)
- [ ] Multiline expressions with operators work
- [ ] Grammar export is complete

### All
- [ ] 288+ tests passing
- [ ] All examples run
- [ ] No regressions

---

## Summary

| Task | Description | Effort |
|------|-------------|--------|
| 16.1 | True async with Tokio | 4-6 hours |
| 16.2 | LLVM indirect closures | 4-6 hours |
| 16.3 | Loop labels | 2-3 hours |
| 16.4 | Proper tuple iteration | 1-2 hours |
| 16.5 | Multiline expressions | 1-2 hours |
| 16.6 | Grammar export | 1 hour |
| **Total** | | **~15-20 hours** |

---

*"v1.1 + v1.2: From good to great."*
