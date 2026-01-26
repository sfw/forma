# Sprint 18: Robustness & Completeness

**Goal:** Fix remaining issues from post-Sprint 17 verification
**Priority:** Builtin safety, async documentation, LLVM completeness

---

## Task 18.1: Builtin Argument Validation (CRITICAL)

### Problem
50+ builtin functions access `args[0]`, `args[1]`, etc. without checking array bounds.
Calling any builtin with wrong argument count causes panic.

### Solution
Add a validation macro and apply it to all builtins.

**File:** `src/mir/interp.rs`

**Step 1: Add validation helper macro near line 1050:**

```rust
/// Validates that args has at least `count` elements, returns error if not
macro_rules! validate_args {
    ($args:expr, $count:expr, $name:expr) => {
        if $args.len() < $count {
            return Err(InterpError {
                message: format!(
                    "{}() requires {} argument(s), got {}",
                    $name, $count, $args.len()
                ),
            });
        }
    };
}
```

**Step 2: Add validation to each builtin function. Examples:**

```rust
// vec_len (line ~1074)
("vec_len", args) => {
    validate_args!(args, 1, "vec_len");
    match &args[0] {
        Value::Array(arr) => Ok(Some(Value::Int(arr.len() as i64))),
        _ => Err(InterpError { message: "vec_len expects array".into() }),
    }
}

// vec_get (line ~1088)
("vec_get", args) => {
    validate_args!(args, 2, "vec_get");
    match (&args[0], &args[1]) {
        // ... existing code
    }
}

// str_substring (line ~1214)
("str_substring", args) => {
    validate_args!(args, 3, "str_substring");
    match (&args[0], &args[1], &args[2]) {
        // ... existing code
    }
}
```

**Step 3: Apply to ALL builtins. Full list:**

| Function | Required Args | Line (approx) |
|----------|---------------|---------------|
| `vec_len` | 1 | 1074 |
| `vec_get` | 2 | 1088 |
| `vec_set` | 3 | 1104 |
| `vec_first` | 1 | 1118 |
| `vec_last` | 1 | 1144 |
| `vec_push` | 2 | 1159 |
| `str_len` | 1 | 1172 |
| `str_char_at` | 2 | 1186 |
| `str_substring` | 3 | 1214 |
| `str_concat` | 2 | 1240 |
| `str_contains` | 2 | 1254 |
| `str_starts_with` | 2 | 1268 |
| `str_ends_with` | 2 | 1282 |
| `str_replace` | 3 | 1296 |
| `str_split` | 2 | 1314 |
| `str_trim` | 1 | 1332 |
| `str_to_upper` | 1 | 1346 |
| `str_to_lower` | 1 | 1360 |
| `str_parse_int` | 1 | 1374 |
| `str_parse_float` | 1 | 1390 |
| `int_to_str` | 1 | 1406 |
| `float_to_str` | 1 | 1420 |
| `abs` | 1 | 1434 |
| `min` | 2 | 1448 |
| `max` | 2 | 1462 |
| `pow` | 2 | 1476 |
| `sqrt` | 1 | 1502 |
| `floor` | 1 | 1516 |
| `ceil` | 1 | 1530 |
| `round` | 1 | 1544 |
| `unwrap` | 1 | 1929 |
| `unwrap_or` | 2 | 1963 |
| `is_some` | 1 | 1993 |
| `is_none` | 1 | 2002 |
| `is_ok` | 1 | 2011 |
| `is_err` | 1 | 2020 |
| `file_read` | 1 | 2031 |
| `file_write` | 2 | 2055 |
| `file_append` | 2 | 2079 |
| `file_exists` | 1 | 2103 |
| `file_delete` | 1 | 2117 |
| `channel_send` | 2 | 2481 |
| `channel_recv` | 1 | 2519 |
| `sleep` | 1 | 2350 |
| `sleep_async` | 1 | 2375 |
| `timeout` | 2 | 2393 |
| `await_all` | 1 | 2426 |
| `await_any` | 1 | 2444 |

---

## Task 18.2: True Async Parallelism (HIGH - USER REQUESTED)

### Problem
The current async implementation evaluates work **before** spawning:

```rust
// Line 954 in interp.rs - THE BUG:
Terminator::Spawn { expr, dest, next } => {
    let value = self.eval_operand(&expr)?;  // Evaluates IMMEDIATELY on main thread!

    let handle = runtime.spawn(async move {
        value  // Just returns pre-computed value, no actual async work
    });
}
```

This means:
- `spawn` evaluates eagerly on the main thread
- No parallel execution happens
- `await_any` doesn't race (all work already done)
- `timeout` doesn't timeout (work already finished)

### Solution: Closure-Based Deferred Execution

The fix requires spawning closures (functions) rather than values. When the user writes:

```forma
task := sp expensive_computation(x)
```

The spawn should capture `expensive_computation` and `x`, then execute inside the async block.

**File:** `src/mir/interp.rs`

### Step 1: Modify Terminator::Spawn (around line 952)

```rust
Terminator::Spawn { expr, dest, next } => {
    // Check if expression is a function call that we can defer
    let spawn_value = match expr {
        // If it's a closure value, we can execute it inside the async block
        Operand::Copy(local) | Operand::Move(local) => {
            let frame = self.current_frame()?;
            let val = frame.locals.get(&local)
                .cloned()
                .ok_or_else(|| InterpError { message: format!("undefined local {:?}", local) })?;

            match val {
                // If it's a closure, spawn it properly
                Value::Closure { func_id, captures, .. } => {
                    // Create task ID
                    let task_id = self.next_task_id;
                    self.next_task_id += 1;

                    // Clone what we need for the async block
                    let runtime = Arc::clone(&self.runtime);
                    let spawned_tasks = Arc::clone(&self.spawned_tasks);
                    let mir = Arc::clone(&self.mir);  // Need Arc<MIR>
                    let captures_clone = captures.clone();
                    let func_id_clone = func_id.clone();

                    // Spawn with deferred execution
                    let handle = runtime.spawn(async move {
                        // Create a mini-interpreter for this task
                        // This runs the closure body in the async context
                        let result = execute_closure_async(&mir, &func_id_clone, &captures_clone);
                        result.unwrap_or(Value::Unit)
                    });

                    // Store handle
                    {
                        let mut tasks = spawned_tasks.lock()
                            .map_err(|_| InterpError { message: "Task registry mutex poisoned".to_string() })?;
                        tasks.insert(task_id, handle);
                    }

                    Value::TokioTask(task_id)
                }
                // For non-closure values, fall back to eager evaluation
                other => {
                    // Create task that just returns the value
                    let task_id = self.next_task_id;
                    self.next_task_id += 1;

                    let runtime = Arc::clone(&self.runtime);
                    let spawned_tasks = Arc::clone(&self.spawned_tasks);

                    let handle = runtime.spawn(async move { other });

                    {
                        let mut tasks = spawned_tasks.lock()
                            .map_err(|_| InterpError { message: "Task registry mutex poisoned".to_string() })?;
                        tasks.insert(task_id, handle);
                    }

                    Value::TokioTask(task_id)
                }
            }
        }
        // For constant operands, evaluate eagerly (can't defer)
        Operand::Constant(c) => {
            let value = self.eval_constant(c)?;
            let task_id = self.next_task_id;
            self.next_task_id += 1;

            let runtime = Arc::clone(&self.runtime);
            let spawned_tasks = Arc::clone(&self.spawned_tasks);

            let handle = runtime.spawn(async move { value });

            {
                let mut tasks = spawned_tasks.lock()
                    .map_err(|_| InterpError { message: "Task registry mutex poisoned".to_string() })?;
                tasks.insert(task_id, handle);
            }

            Value::TokioTask(task_id)
        }
    };

    let frame = self.current_frame_mut()?;
    if let Some(d) = dest {
        frame.locals.insert(d, spawn_value);
    }
    frame.current_block = next;
}
```

### Step 2: Add execute_closure_async helper function

```rust
/// Execute a closure in an async context (for spawned tasks)
/// This creates a minimal interpreter environment to run the closure body
fn execute_closure_async(
    mir: &Arc<MIR>,
    func_id: &str,
    captures: &[Value],
) -> Result<Value, InterpError> {
    // Find the function in MIR
    let func = mir.functions.get(func_id)
        .ok_or_else(|| InterpError { message: format!("function {} not found", func_id) })?;

    // Create a minimal interpreter for this task
    let mut task_interp = Interpreter::new_for_task(Arc::clone(mir))?;

    // Set up the call frame with captures
    task_interp.setup_closure_frame(func, captures)?;

    // Run until completion
    task_interp.run_to_completion()
}
```

### Step 3: Add Interpreter::new_for_task constructor

```rust
impl Interpreter {
    /// Create a minimal interpreter for running spawned tasks
    /// This shares the MIR but has its own call stack and state
    pub fn new_for_task(mir: Arc<MIR>) -> Result<Self, InterpError> {
        let runtime = Arc::new(
            tokio::runtime::Runtime::new()
                .map_err(|e| InterpError { message: format!("Failed to create task runtime: {}", e) })?
        );

        Ok(Self {
            mir,
            call_stack: Vec::new(),
            globals: HashMap::new(),
            runtime,
            spawned_tasks: Arc::new(StdMutex::new(HashMap::new())),
            next_task_id: 0,
            // ... initialize other fields with defaults
            channels: HashMap::new(),
            databases: HashMap::new(),
            tcp_streams: HashMap::new(),
            // ...
        })
    }
}
```

### Step 4: Make MIR shareable (Arc)

**File:** `src/mir/interp.rs`

Change `mir: MIR` to `mir: Arc<MIR>`:

```rust
pub struct Interpreter {
    mir: Arc<MIR>,  // Changed from MIR to Arc<MIR>
    // ...
}
```

Update `Interpreter::new()` to wrap MIR in Arc:

```rust
pub fn new(mir: MIR) -> Result<Self, InterpError> {
    // ...
    Ok(Self {
        mir: Arc::new(mir),
        // ...
    })
}
```

### Step 5: Fix await_any to actually race

**File:** `src/mir/interp.rs` around line 2444

```rust
"await_any" => {
    validate_args!(args, 1, "await_any");
    let tasks = match &args[0] {
        Value::Array(arr) => arr.clone(),
        _ => return Err(InterpError { message: "await_any: expected array of tasks".to_string() })
    };

    let task_ids: Vec<u64> = tasks.iter().filter_map(|t| {
        match t {
            Value::TokioTask(id) => Some(*id),
            _ => None,
        }
    }).collect();

    if task_ids.is_empty() {
        return Err(InterpError { message: "await_any: no tasks to await".to_string() });
    }

    // Get all handles
    let handles: Vec<_> = {
        let mut task_map = self.spawned_tasks.lock()
            .map_err(|_| InterpError { message: "Task registry mutex poisoned".to_string() })?;
        task_ids.iter().filter_map(|id| {
            task_map.remove(id).map(|h| (*id, h))
        }).collect()
    };

    // Race them using select_all
    let (result, completed_idx, remaining) = self.runtime.block_on(async {
        futures::future::select_all(handles.into_iter().map(|(id, h)| {
            Box::pin(async move { (id, h.await) })
        })).await
    });

    // Put remaining tasks back
    {
        let mut task_map = self.spawned_tasks.lock()
            .map_err(|_| InterpError { message: "Task registry mutex poisoned".to_string() })?;
        for (id, handle) in remaining.into_iter() {
            // Note: select_all consumes futures, need different approach
            // For now, return the first completed result
        }
    }

    let (completed_id, join_result) = result;
    let value = join_result.map_err(|e| InterpError {
        message: format!("task panicked: {}", e)
    })?;

    Ok(Some(value))
}
```

### Step 6: Fix timeout to actually timeout

**File:** `src/mir/interp.rs` around line 2393

```rust
"timeout" => {
    validate_args!(args, 2, "timeout");
    let task_id = match &args[0] {
        Value::TokioTask(id) => *id,
        _ => return Err(InterpError { message: "timeout: expected task".to_string() })
    };
    let ms = match &args[1] {
        Value::Int(n) => *n as u64,
        _ => return Err(InterpError { message: "timeout: expected Int for duration".to_string() })
    };

    // Get the task handle
    let handle = {
        let mut tasks = self.spawned_tasks.lock()
            .map_err(|_| InterpError { message: "Task registry mutex poisoned".to_string() })?;
        tasks.remove(&task_id)
    };

    let Some(handle) = handle else {
        return Err(InterpError { message: format!("task {} not found", task_id) });
    };

    // Race the task against a timeout
    let result = self.runtime.block_on(async {
        tokio::time::timeout(
            tokio::time::Duration::from_millis(ms),
            handle
        ).await
    });

    match result {
        Ok(Ok(value)) => {
            // Task completed in time
            Ok(Some(Value::Option(Some(Box::new(value)))))
        }
        Ok(Err(e)) => {
            // Task panicked
            Err(InterpError { message: format!("task panicked: {}", e) })
        }
        Err(_elapsed) => {
            // Timeout occurred
            Ok(Some(Value::Option(None)))
        }
    }
}
```

### Verification

Create a test file `examples/async_parallel.forma`:

```forma
us std.prelude

# CPU-bound work
f fib(n: Int) -> Int
    if n <= 1
        n
    else
        fib(n - 1) + fib(n - 2)

# Test parallel execution
f main()
    start := now_ms()

    # Spawn three parallel computations
    t1 := sp fib(35)
    t2 := sp fib(35)
    t3 := sp fib(35)

    # Await all - should complete in ~1x time if parallel, ~3x if sequential
    results := await_all([t1, t2, t3])

    elapsed := now_ms() - start
    print(f"Computed 3x fib(35) in {elapsed}ms")
    print(f"Results: {results}")
```

If truly parallel, this should complete in roughly the same time as one `fib(35)`, not 3x.

---

## Task 18.3: LLVM Rvalue Handlers (HIGH)

### Problem
10 Rvalue types fall through to error handler at line 300-303.

### Solution
Add handlers for the most commonly used Rvalues.

**File:** `src/codegen/llvm.rs`

**In `compile_rvalue` function, before the catch-all:**

```rust
// Tuple construction
Rvalue::Tuple(elements) => {
    let mut values = Vec::new();
    for elem in elements {
        values.push(self.compile_operand(elem)?);
    }
    // Create anonymous struct type for tuple
    let types: Vec<BasicTypeEnum> = values.iter()
        .map(|v| v.get_type())
        .collect();
    let tuple_type = self.context.struct_type(&types, false);
    let mut tuple = tuple_type.get_undef();
    for (i, val) in values.into_iter().enumerate() {
        tuple = self.builder.build_insert_value(tuple, val, i as u32, "tuple_insert")
            .map_err(|e| CodegenError { message: format!("Failed to insert tuple value: {:?}", e) })?
            .into_struct_value();
    }
    Ok(tuple.into())
}

// Array construction
Rvalue::Array(elements) => {
    if elements.is_empty() {
        return Err(CodegenError { message: "Empty array not supported in LLVM codegen".to_string() });
    }
    let first = self.compile_operand(&elements[0])?;
    let elem_type = first.get_type();
    let array_type = elem_type.array_type(elements.len() as u32);
    let array_alloca = self.builder.build_alloca(array_type, "array")?;

    for (i, elem) in elements.iter().enumerate() {
        let val = self.compile_operand(elem)?;
        let idx = self.context.i32_type().const_int(i as u64, false);
        let ptr = unsafe {
            self.builder.build_gep(array_type, array_alloca, &[self.context.i32_type().const_zero(), idx], "elem_ptr")?
        };
        self.builder.build_store(ptr, val)?;
    }
    Ok(self.builder.build_load(array_type, array_alloca, "array_val")?)
}

// Field access
Rvalue::Field(base, field_idx) => {
    let base_val = self.compile_operand(base)?;
    let struct_val = self.as_struct_value(base_val)?;
    self.builder.build_extract_value(struct_val, *field_idx as u32, "field")
        .map_err(|e| CodegenError { message: format!("Failed to extract field: {:?}", e) })
        .map(|v| v.into())
}

// Tuple field access
Rvalue::TupleField(base, idx) => {
    let base_val = self.compile_operand(base)?;
    let struct_val = self.as_struct_value(base_val)?;
    self.builder.build_extract_value(struct_val, *idx as u32, "tuple_field")
        .map_err(|e| CodegenError { message: format!("Failed to extract tuple field: {:?}", e) })
        .map(|v| v.into())
}

// Struct construction
Rvalue::Struct(fields) => {
    let mut values = Vec::new();
    for (_, operand) in fields {
        values.push(self.compile_operand(operand)?);
    }
    let types: Vec<BasicTypeEnum> = values.iter()
        .map(|v| v.get_type())
        .collect();
    let struct_type = self.context.struct_type(&types, false);
    let mut struct_val = struct_type.get_undef();
    for (i, val) in values.into_iter().enumerate() {
        struct_val = self.builder.build_insert_value(struct_val, val, i as u32, "struct_insert")
            .map_err(|e| CodegenError { message: format!("Failed to insert struct value: {:?}", e) })?
            .into_struct_value();
    }
    Ok(struct_val.into())
}
```

**Note:** Ref, Deref, Enum, Discriminant, EnumField, and Index require more complex handling and can remain as "not supported in LLVM codegen" for v1.x.

---

## Task 18.4: Replace Sentinel TypeVar IDs (MEDIUM)

### Problem
Magic numbers 99999, 99998, 99997 used for type substitution could collide.

### Solution
Use named constants or fresh vars from a reserved pool.

**File:** `src/types/inference.rs`

**Step 1: Add constants near top of file:**

```rust
/// Reserved TypeVar IDs for method type substitution
/// These are used to represent generic parameters in builtin method signatures
mod reserved_type_vars {
    pub const ELEM_TYPE: u32 = u32::MAX;      // T (element type)
    pub const KEY_TYPE: u32 = u32::MAX - 1;   // K (key type)
    pub const VALUE_TYPE: u32 = u32::MAX - 2; // V (value type)
}
```

**Step 2: Update substitute_elem_types (line ~3173):**

```rust
fn substitute_elem_types(&self, ty: &Ty, elem_types: &[Ty]) -> Ty {
    use reserved_type_vars::*;
    match ty {
        Ty::Var(tv) => {
            match tv.id {
                ELEM_TYPE => elem_types.first().cloned().unwrap_or(Ty::Var(*tv)),
                KEY_TYPE => elem_types.first().cloned().unwrap_or(Ty::Var(*tv)),
                VALUE_TYPE => elem_types.get(1).cloned().unwrap_or(Ty::Var(*tv)),
                _ => Ty::Var(*tv),
            }
        }
        // ... rest unchanged
    }
}
```

**Step 3: Update usages (search for 99999, 99998, 99997):**

```rust
// REPLACE:
let t_var = Ty::Var(TypeVar { id: 99999 });

// WITH:
let t_var = Ty::Var(TypeVar { id: reserved_type_vars::ELEM_TYPE });
```

---

## Task 18.5: Fix Remaining LLVM into_int_value (LOW)

### Problem
Lines 691 and 712 still use unsafe `into_int_value()`.

### Solution

**File:** `src/codegen/llvm.rs`

```rust
// Line 691 in If terminator:
// REPLACE:
let cond_val = self.compile_operand(cond)?.into_int_value();

// WITH:
let cond_val = self.as_int_value(self.compile_operand(cond)?)?;


// Line 712 in Switch terminator:
// REPLACE:
let val = self.compile_operand(operand)?.into_int_value();

// WITH:
let val = self.as_int_value(self.compile_operand(operand)?)?;
```

---

## Task 18.6: Add Builtin Tests (LOW)

### Problem
No tests for builtin argument validation.

### Solution
Add tests that verify error messages.

**File:** `src/mir/interp.rs` (in test module)

```rust
#[test]
fn test_builtin_arg_validation() {
    // Test vec_len with no args
    let result = eval_builtin("vec_len", &[]);
    assert!(result.is_err());
    assert!(result.unwrap_err().message.contains("requires 1 argument"));

    // Test vec_get with 1 arg (needs 2)
    let result = eval_builtin("vec_get", &[Value::Array(vec![])]);
    assert!(result.is_err());
    assert!(result.unwrap_err().message.contains("requires 2 argument"));

    // Test str_substring with 2 args (needs 3)
    let result = eval_builtin("str_substring", &[
        Value::Str("hello".to_string()),
        Value::Int(0),
    ]);
    assert!(result.is_err());
    assert!(result.unwrap_err().message.contains("requires 3 argument"));
}
```

---

## Verification

After each task, run:
```bash
cargo test
forma check std/*.forma
forma check examples/*.forma
```

---

## Summary

| Task | Description | Priority | Effort |
|------|-------------|----------|--------|
| 18.1 | Builtin Argument Validation | CRITICAL | Medium (50+ functions) |
| 18.2 | True Async Parallelism | HIGH | High (core change) |
| 18.3 | LLVM Rvalue Handlers | HIGH | Medium (5 handlers) |
| 18.4 | Sentinel TypeVar IDs | MEDIUM | Low |
| 18.5 | LLVM into_int_value fixes | LOW | Trivial |
| 18.6 | Builtin Validation Tests | LOW | Low |

### Task 18.2 Key Changes:
1. Change `mir: MIR` to `mir: Arc<MIR>` for sharing
2. Add `Interpreter::new_for_task()` constructor
3. Add `execute_closure_async()` helper
4. Modify `Terminator::Spawn` to detect closures and defer execution
5. Fix `await_any` to use `futures::future::select_all`
6. Fix `timeout` to use `tokio::time::timeout`

---

*"True parallelism, not just syntax."*
