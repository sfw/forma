# Sprint 14.5: Async Verification & JSON Fix

**Priority:** CRITICAL - Must verify core features work
**Date:** January 25, 2026

---

## Problem Statement

1. **json.forma** reportedly has a type issue - needs investigation and fix
2. **Async/spawn** was not adequately tested - existing tests only check `sleep_async`, not the actual `sp` operator or `await_all`

---

## Task 14.5.1: Fix json.forma Type Issue

**Action:** Run the type checker on json.forma and identify the exact error:

```bash
cargo run --quiet -- check stdlib/json.forma 2>&1
```

**Common issues to check:**
- Is `Json` type registered in the type system?
- Are all json_* builtins registered with type signatures?
- Are the match expressions in utility functions correct?

**Fix the identified issue and ensure:**
```bash
cargo run --quiet -- check stdlib/json.forma
# Should exit with code 0, no output
```

---

## Task 14.5.2: Create Comprehensive Async Tests

**File:** `tests/forma/test_async_spawn.forma` (NEW FILE)

Create a new test file that specifically tests the `sp` operator and `await_all`:

```forma
# Test Async Spawn Module
# Tests for sp (spawn) operator and await_all
# Expected output: All tests pass

# Simple async function that returns a value
as f compute_value(x: Int) -> Int
    x * 2

# Test spawning a single task and awaiting it
f test_spawn_single() -> Bool
    task := sp compute_value(21)
    result := aw task
    result == 42

# Test spawning multiple tasks and await_all
f test_spawn_multiple() -> Bool
    task1 := sp compute_value(10)
    task2 := sp compute_value(20)
    task3 := sp compute_value(30)

    tasks := [task1, task2, task3]
    results := await_all(tasks)

    # Should get [20, 40, 60]
    vec_len(results) == 3 &&
        vec_get(results, 0) == Some(20) &&
        vec_get(results, 1) == Some(40) &&
        vec_get(results, 2) == Some(60)

# Test spawn with different return types
as f returns_string(s: Str) -> Str
    s + " processed"

f test_spawn_string() -> Bool
    task := sp returns_string("hello")
    result := aw task
    result == "hello processed"

# Test spawn in a loop
f test_spawn_loop() -> Bool
    tasks := []
    for i in 0..5
        tasks = vec_push(tasks, sp compute_value(i))

    results := await_all(tasks)

    # Should get [0, 2, 4, 6, 8]
    vec_len(results) == 5 &&
        vec_get(results, 0) == Some(0) &&
        vec_get(results, 4) == Some(8)

# Test that spawn actually creates Task type
f test_spawn_type() -> Bool
    task := sp compute_value(1)
    # The task should be a Task[Int]
    # We can verify by awaiting it
    result := aw task
    result == 2

# Test await_any (if implemented)
f test_await_any() -> Bool
    task1 := sp compute_value(100)
    task2 := sp compute_value(200)

    tasks := [task1, task2]

    # await_any should return the first completed result
    m await_any(tasks)
        Some(result) -> result == 200 || result == 400  # Either 100*2 or 200*2
        None -> false

# Run all tests
f run_all_tests() -> Int
    passed := 0
    total := 6

    if test_spawn_single()
        passed = passed + 1
        print("PASS: test_spawn_single")
    else
        print("FAIL: test_spawn_single")

    if test_spawn_multiple()
        passed = passed + 1
        print("PASS: test_spawn_multiple")
    else
        print("FAIL: test_spawn_multiple")

    if test_spawn_string()
        passed = passed + 1
        print("PASS: test_spawn_string")
    else
        print("FAIL: test_spawn_string")

    if test_spawn_loop()
        passed = passed + 1
        print("PASS: test_spawn_loop")
    else
        print("FAIL: test_spawn_loop")

    if test_spawn_type()
        passed = passed + 1
        print("PASS: test_spawn_type")
    else
        print("FAIL: test_spawn_type")

    if test_await_any()
        passed = passed + 1
        print("PASS: test_await_any")
    else
        print("FAIL: test_await_any")

    print("")
    print("Async spawn tests: " + int_to_str(passed) + "/" + int_to_str(total) + " passed")

    if passed == total { 0 } else { 1 }

f main() -> Int
    run_all_tests()
```

---

## Task 14.5.3: Verify Async Implementation in Interpreter

**File:** `src/mir/interp.rs`

Verify these are implemented:

1. **`sp` (spawn) operator handling:**
   - Should create a `Value::Task` or similar
   - Should store the function and arguments for later execution
   - Check the MIR lowering creates correct spawn instruction

2. **`aw` (await) operator handling:**
   - Should block and get result from Task
   - Should handle Task[T] -> T conversion

3. **`await_all` builtin:**
   - Should accept `[Task[T]]`
   - Should return `[T]` with all results
   - Should execute all tasks (even if synchronously)

4. **`await_any` builtin:**
   - Should accept `[Task[T]]`
   - Should return `Option[T]` with first result

**Check the code handles these cases:**

```rust
// In builtins or special handling
"sp" | "spawn" => {
    // Should create Task value with deferred execution
}

"aw" | "await" => {
    // Should extract result from Task
}

"await_all" => {
    // Should iterate tasks and collect results
}
```

---

## Task 14.5.4: Test async_downloader.forma Actually Runs

**Action:** Manually test the async example:

```bash
# This should actually make HTTP requests and print results
cargo run -- run examples/async_downloader.forma
```

**Expected output:**
```
=== FORMA URL Downloader ===

Downloading 3 URLs in parallel...

Fetching: https://httpbin.org/get
Fetching: https://httpbin.org/ip
Fetching: https://httpbin.org/user-agent
Done: https://httpbin.org/get
Done: https://httpbin.org/ip
Done: https://httpbin.org/user-agent

All downloads complete in XXXms

Summary:
  Successful: 3
  Failed: 0
  Total: 3
```

**If it doesn't work:**
1. Check if `http_get` builtin is implemented
2. Check if `sp` creates proper Task values
3. Check if `await_all` executes the tasks
4. Fix any issues found

---

## Task 14.5.5: Add Synchronous Async Test (No Network)

**File:** `tests/forma/test_async_no_network.forma` (NEW FILE)

A test that verifies async works without needing network:

```forma
# Test async without network dependencies
# This test should always pass regardless of network availability

as f add_numbers(a: Int, b: Int) -> Int
    a + b

as f multiply(x: Int) -> Int
    x * x

as f identity(v: Int) -> Int
    v

f test_basic_spawn_await() -> Bool
    # Spawn a simple computation
    task := sp add_numbers(10, 20)
    result := aw task
    assert_eq(result, 30)

f test_chained_spawn() -> Bool
    # Spawn then use result in another spawn
    t1 := sp add_numbers(5, 5)
    v1 := aw t1

    t2 := sp multiply(v1)
    v2 := aw t2

    assert_eq(v2, 100)  # (5+5)^2 = 100

f test_parallel_spawn() -> Bool
    # Spawn multiple independent tasks
    t1 := sp add_numbers(1, 1)
    t2 := sp add_numbers(2, 2)
    t3 := sp add_numbers(3, 3)

    # Await in different order than spawned
    r3 := aw t3
    r1 := aw t1
    r2 := aw t2

    assert_eq(r1, 2)
    assert_eq(r2, 4)
    assert_eq(r3, 6)

f test_await_all_basic() -> Bool
    tasks := []
    tasks = vec_push(tasks, sp identity(1))
    tasks = vec_push(tasks, sp identity(2))
    tasks = vec_push(tasks, sp identity(3))

    results := await_all(tasks)

    assert_eq(vec_len(results), 3)
    m vec_get(results, 0)
        Some(v) -> assert_eq(v, 1)
        None -> false
    m vec_get(results, 1)
        Some(v) -> assert_eq(v, 2)
        None -> false
    m vec_get(results, 2)
        Some(v) -> assert_eq(v, 3)
        None -> false

f main() -> Int
    passed := 0

    if test_basic_spawn_await()
        print("PASS: test_basic_spawn_await")
        passed = passed + 1
    else
        print("FAIL: test_basic_spawn_await")

    if test_chained_spawn()
        print("PASS: test_chained_spawn")
        passed = passed + 1
    else
        print("FAIL: test_chained_spawn")

    if test_parallel_spawn()
        print("PASS: test_parallel_spawn")
        passed = passed + 1
    else
        print("FAIL: test_parallel_spawn")

    if test_await_all_basic()
        print("PASS: test_await_all_basic")
        passed = passed + 1
    else
        print("FAIL: test_await_all_basic")

    print("")
    print("Async (no network) tests: " + int_to_str(passed) + "/4 passed")

    if passed == 4 { 0 } else { 1 }
```

---

## Definition of Done

- [ ] json.forma compiles without errors
- [ ] test_async_spawn.forma passes (or issues are identified and fixed)
- [ ] test_async_no_network.forma passes
- [ ] async_downloader.forma runs and produces output
- [ ] `sp` operator confirmed working
- [ ] `aw` operator confirmed working
- [ ] `await_all` confirmed working

---

## If Async Is Broken

If testing reveals that `sp`/`aw`/`await_all` don't actually work, document what's missing:

1. **MIR Lowering:** Does `sp expr` lower to correct MIR?
2. **Interpreter:** Does interpreter handle spawn/await?
3. **Task Type:** Is there a `Value::Task` variant?
4. **Execution:** Do tasks actually execute or just return immediately?

Then fix the gaps or document as known limitation for v1.0.

---

*"Trust but verify - especially for async."*
