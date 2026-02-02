# Sprint 22: Fix Failing Example Files

**Goal:** Fix the 2 remaining example files that fail `forma check`
**Priority:** HIGH - Examples should all pass type checking

---

## Problem Summary

After Sprint 21 fixed pattern matching, 11/13 examples pass. Two files still fail:

1. `examples/async_parallel.forma` - Spawn operator usage and missing return type
2. `examples/comprehensive.forma` - Missing return type and potential guard issues

---

## Task 22.1: Fix async_parallel.forma

**File:** `examples/async_parallel.forma`

### Issues

1. **Missing return type on main** (line ~11)
2. **Incorrect spawn usage** (lines 15-17): `sp || fib(30)` passes a closure, but spawn likely expects a direct function call or async expression

### Current Code (Broken)

```forma
f main()
    print("Starting parallel fibonacci computation...")

    start := time_ms()

    // Spawn 3 parallel tasks
    t1 := sp || fib(30)
    t2 := sp || fib(30)
    t3 := sp || fib(30)

    // Wait for all
    results := aw [t1, t2, t3]

    elapsed := time_ms() - start
    print(f"Computed 3x fib(30) in {elapsed}ms")
    print(f"Results: {results}")
```

### Fixed Code

```forma
f main() -> Int
    print("Starting parallel fibonacci computation...")

    start := time_ms()

    // Spawn 3 parallel tasks - use direct function calls
    t1 := sp fib(30)
    t2 := sp fib(30)
    t3 := sp fib(30)

    // Wait for all
    results := aw [t1, t2, t3]

    elapsed := time_ms() - start
    print(f"Computed 3x fib(30) in {elapsed}ms")

    // Print results individually to avoid list-to-string issues
    for r in results
        print(f"Result: {r}")

    0
```

### Changes

1. Add `-> Int` return type to main
2. Change `sp || fib(30)` to `sp fib(30)` (direct call, not closure)
3. Add explicit return value `0`
4. Replace `print(f"Results: {results}")` with a loop to avoid list-to-string conversion issues

---

## Task 22.2: Fix comprehensive.forma

**File:** `examples/comprehensive.forma`

### Issues

1. **Missing return type on main** (line ~53)
2. **Function has no explicit return value**

### Current Code (Broken)

```forma
f main
    // ... body ...
    print(f"Description: {description}")
```

### Fixed Code

```forma
f main() -> Int
    // ... body ...
    print(f"Description: {description}")
    0
```

### Changes

1. Add `()` and `-> Int` to main declaration
2. Add explicit return value `0` at end of function

---

## Task 22.3: Verify All Examples Pass

After fixes, run:

```bash
forma check examples/*.forma
```

All 13 files should pass with no errors.

---

## Verification Checklist

| File | Expected |
|------|----------|
| examples/async_downloader.forma | ✅ Pass |
| examples/async_parallel.forma | ✅ Pass (after fix) |
| examples/cli_with_db.forma | ✅ Pass |
| examples/comprehensive.forma | ✅ Pass (after fix) |
| examples/factorial.forma | ✅ Pass |
| examples/fibonacci.forma | ✅ Pass |
| examples/gcd.forma | ✅ Pass |
| examples/hello.forma | ✅ Pass |
| examples/isprime.forma | ✅ Pass |
| examples/simple.forma | ✅ Pass |
| examples/sum.forma | ✅ Pass |
| examples/test_operators.forma | ✅ Pass |
| examples/web_server.forma | ✅ Pass |

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 22.1 | Fix async_parallel.forma | 1 file |
| 22.2 | Fix comprehensive.forma | 1 file |
| 22.3 | Verify all examples pass | 13 files |

**Expected outcome:** All 13 example files pass `forma check`.

---

*"Examples are documentation that compiles."*
