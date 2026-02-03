# Sprint 30: Stdlib Robustness

**Goal:** Fix edge cases and improve safety in the standard library
**Estimated Effort:** 2-3 hours

---

## Overview

The standard library (`std/`) has 6 issues to address:
1. `pow()` not tail-recursive (stack overflow risk)
2. `lcm()` overflow before divide
3. `days_in_month()` invalid input handling
4. `range_step()` step=0 creates infinite loop
5. String character functions commented out
6. `vec_tail` naming confusion

---

## Task 30.1: Make pow() Tail-Recursive

**File:** `std/core.forma` lines 78-87

**Current Code:**
```forma
f pow(base: Int, exp: Int) -> Int
    if exp < 0 then
        if base == 1 then 1
        else if base == 0 - 1 then
            if exp % 2 == 0 then 1 else 0 - 1
        else 0
    else if exp == 0 then 1
    else if exp == 1 then base
    else base * pow(base, exp - 1)
```

**Problem:** `base * pow(...)` is not tail-recursive - O(n) stack depth.

**Fix:** Use accumulator pattern with exponentiation by squaring:
```forma
# Power function (integer exponent)
# Uses exponentiation by squaring: O(log n) operations
f pow(base: Int, exp: Int) -> Int
    if exp < 0 then
        # Integer power with negative exponent
        if base == 1 then 1
        else if base == 0 - 1 then
            if exp % 2 == 0 then 1 else 0 - 1
        else 0
    else
        pow_acc(base, exp, 1)

# Tail-recursive helper with accumulator
f pow_acc(base: Int, exp: Int, acc: Int) -> Int
    if exp == 0 then acc
    else if exp % 2 == 0 then
        pow_acc(base * base, exp / 2, acc)
    else
        pow_acc(base, exp - 1, acc * base)
```

**Benefits:**
- O(log n) multiplications instead of O(n)
- Tail-recursive (can be optimized to loop)
- No stack overflow for large exponents

---

## Task 30.2: Fix lcm() Overflow

**File:** `std/core.forma` lines 94-97

**Current Code:**
```forma
f lcm(a: Int, b: Int) -> Int
    if a == 0 || b == 0 then 0
    else abs(a * b) / gcd(a, b)
```

**Problem:** `a * b` can overflow before division. For example, `lcm(1000000, 1000000)` would overflow even though the result is just `1000000`.

**Fix:** Divide first, then multiply:
```forma
# LCM using GCD - divide first to prevent overflow
f lcm(a: Int, b: Int) -> Int
    if a == 0 || b == 0 then 0
    else
        g := gcd(a, b)
        abs(a / g) * abs(b)
```

**Why this works:** `gcd(a,b)` divides both `a` and `b`, so `a/gcd(a,b)` is always an integer. The final result is the same but we avoid intermediate overflow.

---

## Task 30.3: Validate days_in_month() Input

**File:** `std/datetime.forma` lines 129-135

**Current Code:**
```forma
f days_in_month(year: Int, month: Int) -> Int
    m month
        1 => 31
        2 => if is_leap_year(year) then 29 else 28
        3 => 31
        # ... etc
        12 => 31
        _ => 31  # Default for invalid month
```

**Problem:** Returns 31 for invalid months (0, 13, -5), which is misleading.

**Fix:** Return 0 for invalid input:
```forma
f days_in_month(year: Int, month: Int) -> Int
    m month
        1 => 31
        2 => if is_leap_year(year) then 29 else 28
        3 => 31
        4 => 30
        5 => 31
        6 => 30
        7 => 31
        8 => 31
        9 => 30
        10 => 31
        11 => 30
        12 => 31
        _ => 0  # Invalid month
```

**Alternative:** Return `Int?` (Option<Int>):
```forma
f days_in_month(year: Int, month: Int) -> Int?
    m month
        1 => Some(31)
        2 => Some(if is_leap_year(year) then 29 else 28)
        # ...
        12 => Some(31)
        _ => None
```

---

## Task 30.4: Guard range_step() Against step=0

**File:** `std/iter.forma` lines 105-112

**Current Code:**
```forma
f range_step(start: Int, end: Int, step: Int) -> StepRange
    StepRange { start: start, end: end, step: step, current: start }

f step_has_next(r: StepRange) -> Bool
    if r.step > 0 then r.current < r.end
    else if r.step < 0 then r.current > r.end
    else false  # step=0 returns false, but...
```

**Problem:** Creating `StepRange` with step=0 silently produces a range where iteration does nothing useful. No error reported.

**Fix Option A:** Return empty range for step=0:
```forma
f range_step(start: Int, end: Int, step: Int) -> StepRange
    if step == 0 then
        # Invalid step - return empty range
        StepRange { start: 0, end: 0, step: 1, current: 1 }
    else
        StepRange { start: start, end: end, step: step, current: start }
```

**Fix Option B:** Return Result type:
```forma
f range_step(start: Int, end: Int, step: Int) -> Result<StepRange, String>
    if step == 0 then
        Err("range_step: step cannot be 0")
    else
        Ok(StepRange { start: start, end: end, step: step, current: start })
```

**Recommendation:** Option A for simplicity, but add a comment noting the behavior.

---

## Task 30.5: Enable String Character Functions

**File:** `std/string.forma` lines 128-132

**Current Code:**
```forma
# String character access functions
# Currently commented out due to type inference issues

# f char_at(s: String, index: Int) -> Char?
#     ...
```

**Problem:** Functions disabled due to Sprint 26 type inference issues.

**After Sprint 26 is complete, re-enable:**
```forma
# Get character at index (0-based)
f char_at(s: String, index: Int) -> Char?
    if index < 0 || index >= len(s) then None
    else Some(s[index])

# Check if string contains character
f contains_char(s: String, c: Char) -> Bool
    i := 0
    wh i < len(s)
        if s[i] == c then return true
        i := i + 1
    false

# Find index of character (-1 if not found)
f index_of(s: String, c: Char) -> Int
    i := 0
    wh i < len(s)
        if s[i] == c then return i
        i := i + 1
    0 - 1
```

---

## Task 30.6: Fix vec_tail Naming

**File:** `std/vec.forma` lines 39-41

**Current Code:**
```forma
# Get tail (last element) of vector
f vec_tail<T>(v: [T]) -> T?
    if vec_is_empty(v) then None
    else Some(v[vec_len(v) - 1])
```

**Problem:** `vec_tail` returns the last element, but "tail" typically means "all but first" in functional programming.

**Fix:** Rename and add proper `vec_tail`:
```forma
# Get last element of vector
f vec_last<T>(v: [T]) -> T?
    if vec_is_empty(v) then None
    else Some(v[vec_len(v) - 1])

# Get tail (all elements except first)
f vec_tail<T>(v: [T]) -> [T]
    if vec_len(v) <= 1 then []
    else v[1..]

# Get init (all elements except last) - complement to vec_last
f vec_init<T>(v: [T]) -> [T]
    if vec_len(v) <= 1 then []
    else v[..vec_len(v) - 1]
```

**Usage:**
```forma
let v = [1, 2, 3, 4]
vec_head(v)   # Some(1)
vec_last(v)   # Some(4)
vec_tail(v)   # [2, 3, 4]
vec_init(v)   # [1, 2, 3]
```

---

## Verification

After implementing all tasks:

1. Run the test suite:
```bash
cd forma && cargo test
```

2. Create test file `tests/stdlib_sprint30.forma`:
```forma
# Sprint 30 Stdlib Tests

# 30.1: pow tail-recursive
let p1 = pow(2, 20)      # 1048576
let p2 = pow(2, 30)      # No stack overflow
print(f"pow(2,20) = {p1}")

# 30.2: lcm no overflow
let l1 = lcm(100000, 100000)  # Should be 100000, not overflow
print(f"lcm(100000, 100000) = {l1}")

# 30.3: days_in_month validation
let d1 = days_in_month(2024, 2)   # 29 (leap year)
let d2 = days_in_month(2024, 13)  # 0 (invalid)
print(f"Feb 2024: {d1}, month 13: {d2}")

# 30.4: range_step guard
let r = range_step(1, 10, 0)
let has = step_has_next(r)
print(f"step=0 has_next: {has}")  # false

# 30.5: String functions (after re-enabling)
# let c = char_at("hello", 1)
# print(f"char_at('hello', 1) = {c}")

# 30.6: vec naming
let v = [1, 2, 3, 4]
let last = vec_last(v)
let tail = vec_tail(v)
print(f"last: {last}, tail: {tail}")

print("Stdlib tests complete")
```

3. Run with interpreter:
```bash
cargo run -- run tests/stdlib_sprint30.forma
```

---

## Summary

| Task | Description | File | Lines |
|------|-------------|------|-------|
| 30.1 | pow() tail-recursive | core.forma | 78-87 |
| 30.2 | lcm() overflow fix | core.forma | 94-97 |
| 30.3 | days_in_month() validation | datetime.forma | 129-135 |
| 30.4 | range_step() step=0 guard | iter.forma | 105-112 |
| 30.5 | String char functions | string.forma | 128-132 |
| 30.6 | vec_tail naming | vec.forma | 39-41 |

**Dependencies:** Task 30.5 depends on Sprint 26 (type inference fixes)

---

## Claude Code Prompt

```
Sprint 30: Stdlib Robustness for FORMA

Working directory: forma/

## Tasks

### 30.1: Make pow() Tail-Recursive
File: std/core.forma (lines 78-87)
Replace recursive `base * pow(base, exp - 1)` with:
1. A new helper `pow_acc(base, exp, acc)` using accumulator pattern
2. Use exponentiation by squaring: if exp is even, square base and halve exp
Result: O(log n) operations, tail-recursive, no stack overflow

### 30.2: Fix lcm() Overflow
File: std/core.forma (lines 94-97)
Change `abs(a * b) / gcd(a, b)` to `abs(a / gcd(a, b)) * abs(b)`
Divide before multiplying to prevent intermediate overflow.

### 30.3: Validate days_in_month() Input
File: std/datetime.forma (lines 129-135)
Change the catch-all `_ => 31` to `_ => 0` for invalid months.
Optionally change return type to Int? and return None for invalid.

### 30.4: Guard range_step() Against step=0
File: std/iter.forma (lines 105-112)
Add check at start of range_step:
if step == 0 then return empty StepRange (start=0, end=0, step=1, current=1)

### 30.5: Enable String Character Functions
File: std/string.forma (lines 128-132)
Uncomment char_at, contains_char, index_of functions.
These should work now that Sprint 26 type inference is complete.

### 30.6: Fix vec_tail Naming
File: std/vec.forma (lines 39-41)
Rename current vec_tail to vec_last (returns last element).
Add new vec_tail that returns [T] (all elements except first).
Add vec_init that returns all elements except last.

## Testing
After changes:
1. cargo test
2. Create tests/stdlib_sprint30.forma testing:
   - pow(2, 30) doesn't stack overflow
   - lcm(100000, 100000) = 100000 (no overflow)
   - days_in_month(2024, 13) = 0
   - range_step(1, 10, 0) creates empty range
   - vec_last([1,2,3]) = Some(3), vec_tail([1,2,3]) = [2,3]
3. cargo run -- run tests/stdlib_sprint30.forma

All 251+ existing tests must continue to pass.
```
