# FORMA Language Reference

A progressive guide to the FORMA programming language.

FORMA is an AI-optimized systems programming language that delivers memory safety without lifetimes, concise syntax that costs fewer tokens, and grammar-constrained generation for correct-by-construction code.

---

## Table of Contents

- [Getting Started](#getting-started)
- [Basics](#basics)
- [Variables & Mutability](#variables--mutability)
- [Types](#types)
- [Control Flow](#control-flow)
- [Functions](#functions)
- [Data Types in Depth](#data-types-in-depth)
- [Structs](#structs)
- [Enums](#enums)
- [Pattern Matching](#pattern-matching)
- [Error Handling](#error-handling)
- [Closures & Higher-Order Functions](#closures--higher-order-functions)
- [Generics](#generics)
- [Traits & Implementations](#traits--implementations)
- [References](#references)
- [Contracts](#contracts)
- [Modules & Imports](#modules--imports)
- [Async & Concurrency](#async--concurrency)
- [Standard Library Overview](#standard-library-overview)
- [Tooling](#tooling)
- [For AI Developers](#for-ai-developers)

---

## Getting Started

### Build & Install

```bash
git clone https://github.com/sfw/forma.git
cd forma
cargo build --release
```

The compiler binary is at `./target/release/forma`.

### Hello World

Create `hello.forma`:

```forma
f main()
    print("Hello, World!")
```

Run it:

```bash
forma run hello.forma
```

Every FORMA program starts at `main`. By default, `main` returns nothing (unit). If you need an explicit exit code, you can declare `f main() -> Int` and return an integer (e.g., `0` for success).

### Key Design Choices

**Why short keywords?** Every token costs money in LLM API calls. `f` instead of `fn` or `function`, `s` instead of `struct`, `m` instead of `match`. FORMA reduces token usage by ~38% compared to Rust.

**Why indentation-based?** Braces are a leading source of syntax errors in AI-generated code. Indentation is unambiguous and reduces the grammar's complexity.

**Why no lifetimes?** 94.8% of AI failures with Rust are lifetime/borrow-checker errors. FORMA uses second-class references that eliminate lifetime annotations entirely while preserving memory safety.

---

## Basics

### Comments

```forma
# This is a line comment
# FORMA uses # for comments (like Python)
```

### Printing

```forma
f main()
    print("Hello!")           # Print any value
    print(42)                 # Integers
    print(3.14)               # Floats
    print(true)               # Booleans
    eprintln("error message") # Print to stderr
```

### String Interpolation

FORMA supports f-strings for embedding expressions in strings:

```forma
f main()
    name := "World"
    age := 25
    print(f"Hello, {name}!")
    print(f"{name} is {age} years old")
    print(f"2 + 3 = {2 + 3}")
```

---

## Variables & Mutability

Variables are declared with `:=` (walrus operator). All variables are mutable by default:

```forma
f main()
    x := 42          # Type inferred as Int
    name := "Alice"   # Type inferred as Str
    pi := 3.14159     # Type inferred as Float

    # Reassignment uses :=
    x := x + 1
    print(x)          # 43
```

You can add explicit type annotations:

```forma
f main()
    x: Int = 42
    name: Str = "Alice"
```

---

## Types

### Primitive Types

| Type | Description | Example |
|------|-------------|---------|
| `Int` | Integer (default) | `42`, `-7` |
| `Float` | Float (default) | `3.14`, `-0.5` |
| `Bool` | Boolean | `true`, `false` |
| `Char` | Unicode character | `'a'`, `'Z'` |
| `Str` | String | `"hello"` |
| `()` | Unit (void) | `()` |

### Sized Integer Types

| Signed | Unsigned |
|--------|----------|
| `i8`, `i16`, `i32`, `i64`, `i128` | `u8`, `u16`, `u32`, `u64`, `u128` |
| `isize` | `usize` |

### Collection Types

| Type | Syntax | Example |
|------|--------|---------|
| List | `[T]` | `[1, 2, 3]` |
| Array (fixed) | `[T; N]` | `[0; 10]` |
| Map | `{K: V}` | `{"a": 1, "b": 2}` |
| Set | `{T}` | — |
| Tuple | `(T, U)` | `(1, "hello")` |

### Special Types

| Type | Syntax | Meaning |
|------|--------|---------|
| Option | `T?` | Value or nothing |
| Result | `T!E` | Value or error |
| Function | `(A, B) -> C` | Function type |
| Never | `!` | Never returns |

---

## Control Flow

### If / Else

`if` expressions always produce a value. The `then` keyword is required for single-expression bodies:

```forma
# Single-expression form
result := if x > 0 then "positive" else "non-positive"

# Block form
if x > 0
    print("positive")
else if x == 0
    print("zero")
else
    print("negative")
```

### While Loops

```forma
i := 0
wh i < 10
    print(i)
    i := i + 1
```

### For Loops

```forma
for item in [1, 2, 3, 4, 5]
    print(item)
```

### Infinite Loops

```forma
lp
    line := read_line()
    if line == "quit" then br
    print(f"You said: {line}")
```

### Loop Control

| Keyword | Long Form | Meaning |
|---------|-----------|---------|
| `br` | `break` | Break out of loop |
| `ct` | `continue` | Skip to next iteration |
| `ret` | `return` | Return from function |

### Labeled Loops

```forma
'outer: for i in [1, 2, 3]
    for j in [10, 20, 30]
        if i * j > 40 then br 'outer
        print(f"{i} * {j} = {i * j}")
```

---

## Functions

Functions are declared with `f`. The last expression is the return value:

```forma
f add(a: Int, b: Int) -> Int
    a + b

f greet(name: Str) -> Str
    f"Hello, {name}!"
```

### Single-Expression Functions

For one-liners, use `=`:

```forma
f double(x: Int) -> Int = x * 2
f is_even(n: Int) -> Bool = n % 2 == 0
```

### Functions Without Return Values

Functions that don't return a meaningful value use `Unit` (or omit the return type):

```forma
f say_hello(name: Str) -> Unit
    print(f"Hello, {name}!")
```

### Early Return

Use `ret` (or `return`) to return early:

```forma
f is_prime(n: Int) -> Bool
    if n < 2 then return false
    if n == 2 then return true
    if n % 2 == 0 then return false
    i := 3
    wh i * i <= n
        if n % i == 0 then return false
        i := i + 2
    true
```

---

## Data Types in Depth

### Lists

Lists are dynamically-sized sequences:

```forma
f main()
    numbers := [1, 2, 3, 4, 5]
    print(len(numbers))        # 5
    print(numbers[0])          # 1 (zero-indexed)

    # Mutation
    numbers[0] := 10
    vec_push(numbers, 6)
    print(numbers)             # [10, 2, 3, 4, 5, 6]
```

### Tuples

Tuples group values of different types. Access fields with `.0`, `.1`, etc.:

```forma
f divide(a: Int, b: Int) -> (Int, Int)
    quotient := a / b
    remainder := a % b
    (quotient, remainder)

f main()
    result := divide(17, 5)
    print(f"{result.0} remainder {result.1}")  # 3 remainder 2
```

### Maps

Maps are key-value collections created with `map_new()`:

```forma
f main()
    scores := map_new()
    map_insert(scores, "Alice", 95)
    map_insert(scores, "Bob", 87)
    print(map_keys(scores))
```

### Strings

Strings support many builtin operations:

```forma
f main()
    s := "Hello, World!"
    print(str_len(s))              # 13
    print(str_contains(s, "World")) # true
    print(str_slice(s, 0, 5))      # "Hello"

    parts := str_split("a,b,c", ",")
    print(parts)                    # ["a", "b", "c"]
```

---

## Structs

Structs are declared with `s`:

```forma
s Point
    x: Float
    y: Float

f main()
    p := Point { x: 3.0, y: 4.0 }
    print(f"({p.x}, {p.y})")
```

### Tuple Structs

```forma
s Color(u8, u8, u8)

f main()
    red := Color(255, 0, 0)
    print(red.0)  # 255
```

### Methods

Add methods to structs with `i` (impl):

```forma
s Point
    x: Float
    y: Float

i Point
    f distance(&self) -> Float
        sqrt(self.x * self.x + self.y * self.y)

    f translate(&self, dx: Float, dy: Float) -> Point
        Point { x: self.x + dx, y: self.y + dy }

f main()
    p := Point { x: 3.0, y: 4.0 }
    print(p.distance())  # 5.0
```

### Generics on Structs

```forma
s Pair[T]
    first: T
    second: T

f main()
    pair := Pair { first: 1, second: 2 }
    print(pair.first)
```

---

## Enums

Enums define types with multiple variants:

```forma
e Direction
    North
    South
    East
    West

e Shape
    Circle(Float)
    Rectangle(Float, Float)
    Triangle(Float, Float, Float)
```

### Inline Syntax

For simple enums, use the pipe-separated syntax:

```forma
e Color = Red | Green | Blue
```

### Using Enums

Enum variants with data are primarily used through pattern matching. The built-in constructors `Some()`, `Ok()`, and `Err()` can be called directly:

```forma
f main()
    opt := Some(42)
    m opt
        Some(v) -> print(f"Got: {v}")
        None -> print("Nothing")

    result := Ok("success")
    m result
        Ok(msg) -> print(msg)
        Err(e) -> print(f"Error: {e}")
```

User-defined enums with unit variants are constructed by name:

```forma
e Direction
    North
    South
    East
    West

f main()
    # Unit variants are referenced by name
    dir := North
```

---

## Pattern Matching

Pattern matching with `m` is exhaustive — the compiler ensures all cases are covered:

```forma
f describe(n: Int) -> Str
    m n
        0 -> "zero"
        1 -> "one"
        _ -> "many"
```

### Destructuring

```forma
f point_info(p: (Int, Int)) -> Str
    m p
        (0, 0) -> "origin"
        (x, 0) -> f"on x-axis at {x}"
        (0, y) -> f"on y-axis at {y}"
        (x, y) -> f"at ({x}, {y})"
```

### Guards

Add conditions to match arms with `if`:

```forma
f classify(n: Int) -> Str
    m n
        n if n < 0 -> "negative"
        0 -> "zero"
        n if n <= 10 -> "small positive"
        _ -> "large positive"
```

### Matching Result and Option

```forma
f handle(result: Int!Str) -> Int
    m result
        Ok(value) -> value
        Err(msg) ->
            print(f"Error: {msg}")
            0

f main()
    print(handle(Ok(42)))    # 42
    print(handle(Err("no"))) # 0
```

### Matching with Structs

```forma
s Point
    x: Int
    y: Int

f describe_point(p: Point) -> Str
    m p
        Point { x: 0, y: 0 } -> "origin"
        Point { x, y } -> f"({x}, {y})"
```

---

## Error Handling

FORMA uses `Result` and `Option` types instead of exceptions. No `try/catch` — errors are values.

### Option Type

`T?` is shorthand for `Option[T]`. A value is either `Some(value)` or `None`:

```forma
f find_index(arr: [Int], target: Int) -> Int?
    i := 0
    wh i < len(arr)
        if arr[i] == target then return Some(i)
        i := i + 1
    None
```

### Result Type

`T!E` is shorthand for `Result[T, E]`. A value is either `Ok(value)` or `Err(error)`:

```forma
f parse_positive(s: Str) -> Int!Str
    n := str_to_int(s)
    m n
        Some(v) if v > 0 -> Ok(v)
        Some(_) -> Err("not positive")
        None -> Err("not a number")
```

### The `?` Operator

The `?` operator propagates errors — if the value is `Err`, return it immediately:

```forma
f read_config(path: Str) -> Str!Str
    content := file_read(path)?    # Returns Err if file_read fails
    Ok(content)
```

### The `!` Operator

The `!` operator unwraps a `Result`, panicking on error:

```forma
f main()
    db := db_open("app.db")!  # Panic if database can't be opened
    print("Database opened")
```

### Null Coalescing with `??`

The `??` operator provides a default value for `None`/`Err`:

```forma
f main()
    name := env_get("USER") ?? "unknown"
    print(f"Hello, {name}")
```

### Option/Result Utilities

#### Builtin Reference

| Function | Signature | Description |
|----------|-----------|-------------|
| `is_some` | `Option[T] -> Bool` | `true` if `Some` |
| `is_none` | `Option[T] -> Bool` | `true` if `None` |
| `is_ok` | `Result[T, E] -> Bool` | `true` if `Ok` |
| `is_err` | `Result[T, E] -> Bool` | `true` if `Err` |
| `unwrap` | `Option[T] -> T` | Extract value, panic on `None` |
| `unwrap_or` | `(Option[T], T) -> T` | Extract value, or return default on `None` |
| `expect` | `(Option[T], Str) -> T` | Extract value, panic with message on `None` |

```forma
# Check status
is_some(Some(42))       # true
is_none(None)           # true
is_ok(Ok(42))           # true
is_err(Err("bad"))      # true

# Unwrapping (Option only)
unwrap(Some(42))             # 42
unwrap_or(Some(42), 0)      # 42
unwrap_or(None, 0)          # 0
expect(Some(42), "missing")  # 42
```

### Pattern Matching on Option/Result

Use `m` (match) to handle both variants with value binding:

```forma
f describe(opt: Int?) -> Str
    m opt
        Some(v) -> f"got {v}"
        None -> "nothing"

f handle(res: Int!Str) -> Str
    m res
        Ok(v) -> f"success: {v}"
        Err(e) -> f"error: {e}"
```

Match guards add conditions after the pattern:

```forma
f classify(opt: Int?) -> Str
    m opt
        Some(n) if n > 0 -> "positive"
        Some(n) if n < 0 -> "negative"
        Some(_) -> "zero"
        None -> "absent"
```

### Practical Patterns

**Safe division:**

```forma
f safe_div(a: Int, b: Int) -> Int?
    if b == 0 then None
    else Some(a / b)

f main()
    result := safe_div(10, 0) ?? 0    # 0
```

**Chaining `?` for error propagation:**

```forma
f parse_positive(s: Str) -> Int!Str
    m str_to_int(s)
        Some(n) if n > 0 -> Ok(n)
        Some(_) -> Err("not positive")
        None -> Err("not a number")

f add_parsed(a: Str, b: Str) -> Int!Str
    x := parse_positive(a)?    # Returns Err early if parsing fails
    y := parse_positive(b)?
    Ok(x + y)
```

**`unwrap_or` vs `??`:**

Both provide a default value for `None`. Use `??` for concise inline expressions:

```forma
f main()
    # These are equivalent for Option values:
    a := unwrap_or(str_to_int("abc"), 0)
    b := str_to_int("abc") ?? 0
```

**Functions that return Option:**

```forma
# str_to_int: Str -> Int?
m str_to_int("42")
    Some(n) -> print(n)    # 42
    None -> print("failed")

# vec_get: ([T], Int) -> T?
arr := [10, 20, 30]
m vec_get(arr, 1)
    Some(v) -> print(v)    # 20
    None -> print("out of bounds")
```

---

## Closures & Higher-Order Functions

Closures are anonymous functions enclosed in `|...|`. Parameters require type annotations:

```forma
f main()
    add := |a: Int, b: Int| a + b
    print(add(3, 4))          # 7

    square := |x: Int| -> Int x * x
    print(square(5))          # 25
```

### As Function Arguments

```forma
f apply(x: Int, func: (Int) -> Int) -> Int
    func(x)

f main()
    result := apply(5, |x: Int| x * 2)
    print(result)  # 10
```

---

## Generics

Functions and types can be parameterized with type variables using `[T]`:

```forma
f identity[T](x: T) -> T
    x

f first[T](list: [T]) -> T?
    if len(list) > 0 then Some(list[0])
    else None

s Stack[T]
    items: [T]
    size: Int
```

Note: FORMA uses `[T]` for generics (not `<T>` like Rust or Java). This avoids ambiguity with comparison operators, making AI generation more reliable.

---

## Traits & Implementations

Traits define shared behavior:

```forma
t Display
    f display(&self) -> Str

t Area
    f area(&self) -> Float
```

Implement traits with `i TraitName for Type`:

```forma
s Circle
    radius: Float

i Area for Circle
    f area(&self) -> Float
        3.14159 * self.radius * self.radius

i Display for Circle
    f display(&self) -> Str
        f"Circle(r={self.radius})"
```

### Trait Bounds

Use `where` clauses to constrain generic types:

```forma
f print_area[T](shape: T) -> Unit where T: Area + Display
    print(f"{shape.display()}: area = {shape.area()}")
```

---

## References

FORMA uses **second-class references** — references exist only as function parameters, never in structs or return types. This eliminates lifetime annotations entirely.

### Ref Parameters

Use `ref` for shared (read-only) access, `ref mut` for mutable access:

```forma
f swap(ref mut arr: [Int], i: Int, j: Int) -> Unit
    temp := arr[i]
    arr[i] := arr[j]
    arr[j] := temp

f sort(ref mut arr: [Int]) -> Unit
    n := len(arr)
    if n > 1 then
        quicksort(ref mut arr, 0, n - 1)
```

### Calling with References

Pass arguments with the matching `ref` or `ref mut`:

```forma
f main()
    data := [3, 1, 2]
    swap(ref mut data, 0, 2)
    print(data)                     # [2, 1, 3]
```

### Borrow Rules

Like Rust, FORMA enforces borrow safety:
- Multiple `ref` borrows allowed simultaneously
- Only one `ref mut` borrow at a time
- Cannot have `ref` and `ref mut` borrows simultaneously

The difference: since references are second-class, these rules are trivially checkable without lifetime analysis.

---

## Contracts

Design-by-contract with `@pre` and `@post` annotations:

```forma
@pre(n >= 0, "n must be non-negative")
@post(result >= 1, "factorial is always positive")
f factorial(n: Int) -> Int
    if n <= 1 then 1
    else n * factorial(n - 1)
```

Contracts are checked at runtime by default:

```bash
forma run myfile.forma
```

Disable runtime contract checks explicitly with:

```bash
forma run --no-check-contracts myfile.forma
```

### Contract Expressions

Contracts support rich expression syntax:

- **`old(expr)`** in `@post` — captures the value of `expr` before the function body executes
- **`forall x in xs: predicate`** — universal quantifier over a collection or range
- **`exists x in xs: predicate`** — existential quantifier
- **`x in collection`** — membership test
- **`A => B`** — implication (if A then B)
- **`result`** — refers to the return value in `@post` conditions

### Named Contract Patterns

FORMA provides 35 named patterns that expand to contract expressions:

#### Numeric Patterns

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@positive(x)` | Both | `x > 0` |
| `@nonnegative(x)` | Both | `x >= 0` |
| `@nonzero(x)` | Both | `x != 0` |
| `@even(x)` | Both | `x % 2 == 0` |
| `@odd(x)` | Both | `x % 2 != 0` |
| `@divisible(x, n)` | Both | `x % n == 0` |
| `@bounded(x, lo, hi)` | Both | `lo <= x <= hi` (inclusive) |
| `@in_range(x, lo, hi)` | Both | `lo < x < hi` (exclusive) |

#### Collection Patterns

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@nonempty(x)` | Pre-only | `x.len() > 0` |
| `@contains(arr, elem)` | Both | `elem` is in `arr` |
| `@all_positive(arr)` | Both | all elements > 0 |
| `@all_nonnegative(arr)` | Both | all elements >= 0 |
| `@all_nonzero(arr)` | Both | all elements != 0 |
| `@valid_index(arr, i)` | Both | `0 <= i < arr.len()` |
| `@valid_range(arr, lo, hi)` | Both | valid slice bounds |

#### Set Relationships

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@subset(a, b)` | Both | all of `a` in `b` |
| `@superset(a, b)` | Both | all of `b` in `a` |
| `@disjoint(a, b)` | Both | no overlap between `a` and `b` |
| `@equals(a, b)` | Both | same elements (set equality) |
| `@same_length(a, b)` | Both | `a.len() == b.len()` |
| `@permutation(a, b)` | Both | same elements with same multiplicities |

#### Sequence Relationships

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@prefix(a, b)` | Both | `a` starts `b` |
| `@suffix(a, b)` | Both | `a` ends `b` |
| `@reversed(a, b)` | Both | `a` is reverse of `b` |
| `@rotated(a, b, k)` | Both | `a` is `b` rotated by `k` (k is normalized via modular arithmetic; empty arrays are trivially rotated) |
| `@unique(x)` | Both | no duplicate elements |

#### Ordering Patterns

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@sorted(x)` | Both | ascending order (allows duplicates) |
| `@sorted_desc(x)` | Both | descending order (allows duplicates) |
| `@strictly_sorted(x)` | Both | ascending, no duplicates |
| `@strictly_sorted_desc(x)` | Both | descending, no duplicates |
| `@sorted_by(arr, field)` | Both | sorted by struct field |
| `@partitioned(arr, pivot)` | Both | partitioned at pivot index |
| `@stable(in, out, field)` | Post-only | stable sort: output is sorted by field, is a permutation of input, and equal-field elements keep original relative order (field is an identifier, e.g. `@stable(items, result, priority)`) |

#### State Patterns

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@unchanged(x)` | Post-only | `x == old(x)` |
| `@pure` | Post-only | no side effects marker |

**Postcondition inference rule:** Patterns default to preconditions. A pattern automatically becomes a postcondition when any of its arguments is the identifier `result` (the function return value). For example, `@sorted(result)` is a postcondition, while `@sorted(items)` is a precondition. Use `@pre(...)` or `@post(...)` to override this behavior explicitly.

### Contract Explainability

The `forma explain` command translates contract expressions into readable English:

```bash
# Human-readable output with box-drawing
forma explain myfile.forma --format human

# Machine-readable JSON
forma explain myfile.forma --format json

# With deterministic example generation
forma explain myfile.forma --format markdown --examples=3 --seed 42
```

Example output:
```
┌─ verified_sort(items: [Int]) -> [Int]
│  Requires:
│    - items is not empty
│  Guarantees:
│    - [@sorted] for every i in 0..result.len() - 1, result[i] is at most result[i + 1]
│    - [@permutation] permutation(items, result)
└─ Examples:
     [valid] ([3]) -> [3]
     [valid] ([0, -6]) -> [-6, 0]
     [invalid] ([]) -> []
```

### Contract Verification

The `forma verify` command runs contract verification over files or directories:

```bash
# Generate a trust report
forma verify src --report --format json --examples 20 --seed 42

# Human-readable report
forma verify src --report --format human
```

Verification runs with capabilities revoked by default (no file/network/exec side effects) unless `--allow-side-effects` is explicitly set. This makes it CI-safe and reproducible.

Example:

```forma
@pre(values.len() > 0)
@post(forall i in 0..result.len()-1: result[i] <= result[i+1])
@post(permutation(values, result))
f sort(values: [Int]) -> [Int]
    sort_ints(values)
```

---

## Modules & Imports

### Importing

Use `us` to import modules:

```forma
us std.io
us std.collections
us std.math
```

Import specific items:

```forma
us collections.{Vec, Map}
```

Import with alias:

```forma
us std.fs -> filesystem
```

### Declaring Modules

```forma
pub md utils
    pub f helper() -> Int
        42
```

---

## Async & Concurrency

### Async Functions

Prefix a function with `as` to make it async:

```forma
as f fetch_data(url: Str) -> Str!Str
    response := http_get(url)?
    Ok(response)
```

### Spawn & Await

Use `sp` to spawn concurrent tasks and `aw` to await results:

```forma
as f main()
    # Spawn concurrent work
    task1 := sp fetch_data("https://api.example.com/a")
    task2 := sp fetch_data("https://api.example.com/b")

    # Await results
    result1 := aw task1
    result2 := aw task2
    print("Both complete!")
```

### Channels

Send messages between concurrent tasks:

```forma
as f main()
    ch := channel_new()
    sender := ch.0
    receiver := ch.1

    sp producer(sender)

    msg := channel_recv(receiver)
    print(msg)
```

### Mutexes

Shared mutable state between tasks:

```forma
mtx := mutex_new(0)
mutex_lock(mtx)
mutex_set(mtx, 42)
mutex_unlock(mtx)
```

---

## Standard Library Overview

FORMA includes 298+ builtin functions. Here are the key categories:

### I/O

| Function | Description |
|----------|-------------|
| `print(value)` | Print to stdout |
| `eprintln(value)` | Print to stderr |
| `str(value)` | Convert any value to string |

### Math

| Function | Description |
|----------|-------------|
| `abs(n)` | Absolute value |
| `sqrt(x)` | Square root |
| `pow(base, exp)` | Exponentiation |
| `sin(x)`, `cos(x)`, `tan(x)` | Trigonometry |
| `log(x)`, `exp(x)` | Logarithm, exponential |
| `floor(x)`, `ceil(x)`, `round(x)` | Rounding |

### String Operations

| Function | Description |
|----------|-------------|
| `str_len(s)` | String length |
| `str_contains(s, sub)` | Check substring |
| `str_starts_with(s, prefix)` | Check prefix |
| `str_ends_with(s, suffix)` | Check suffix |
| `str_split(s, delim)` | Split string |
| `str_trim(s)` | Remove whitespace |
| `str_replace_all(s, old, new)` | Replace all occurrences |
| `str_slice(s, start, end)` | Get substring |
| `str_to_int(s)` | Parse integer |

### Collections

| Function | Description |
|----------|-------------|
| `len(collection)` | Get length |
| `vec_new()` | Create empty list |
| `vec_push(list, item)` | Append item |
| `vec_pop(list)` | Remove last item |
| `vec_slice(list, start, end)` | Get sublist |
| `vec_concat(a, b)` | Concatenate lists |
| `vec_reverse(list)` | Reverse list |
| `sort_ints(list)` | Sort integers |
| `map_new()` | Create empty map |
| `map_get(map, key)` | Get value by key |
| `map_insert(map, key, value)` | Insert entry |
| `map_keys(map)` | Get all keys |

### File I/O

Requires `--allow-read` and/or `--allow-write` capability flags:

| Function | Description |
|----------|-------------|
| `file_read(path)` | Read file to string |
| `file_write(path, content)` | Write content to file |
| `file_exists(path)` | Check if file exists |
| `file_append(path, content)` | Append to file |
| `file_remove(path)` | Delete file |
| `dir_list(path)` | List directory contents |
| `dir_create(path)` | Create directory |
| `path_join(a, b)` | Join path segments |

### JSON

| Function | Description |
|----------|-------------|
| `json_parse(s)` | Parse JSON string |
| `json_stringify(v)` | Convert to JSON |
| `json_stringify_pretty(v)` | Pretty-print JSON |
| `json_get(obj, key)` | Get field |
| `json_get_str(obj, key)` | Get string field |
| `json_get_int(obj, key)` | Get integer field |

### Networking

Requires `--allow-network` capability flag:

| Function | Description |
|----------|-------------|
| `http_get(url)` | HTTP GET request |
| `http_post(url, body)` | HTTP POST request |
| `http_post_json(url, json)` | POST with JSON |
| `http_serve(port, handler)` | Start HTTP server |
| `tcp_connect(host, port)` | TCP connection |
| `tcp_read(conn)` | Read from TCP |
| `tcp_write(conn, data)` | Write to TCP |
| `tls_connect(host, port)` | TLS connection |

### Process / Environment / Unsafe

Capability-gated groups:
- `--allow-exec`: process execution builtins
- `--allow-env`: environment variable builtins (`env_get`, `env_set`, `env_remove`, `env_vars`)
- `--allow-unsafe`: pointer/memory allocation and low-level unsafe builtins

### Database (SQLite)

| Function | Description |
|----------|-------------|
| `db_open(path)` | Open SQLite database |
| `db_open_memory()` | In-memory database |
| `db_execute(db, sql)` | Execute SQL |
| `db_query(db, sql)` | Query rows |
| `db_close(db)` | Close database |
| `row_get_int(row, col)` | Get integer column |
| `row_get_str(row, col)` | Get string column |

### Random

| Function | Description |
|----------|-------------|
| `random()` | Random float [0, 1) |
| `random_int(min, max)` | Random integer in range |
| `random_bool()` | Random boolean |
| `random_choice(list)` | Random element |

### Time

| Function | Description |
|----------|-------------|
| `time_now()` | Unix timestamp (seconds) |
| `time_now_ms()` | Unix timestamp (milliseconds) |
| `time_sleep(seconds)` | Sleep |
| `time_format(ts, fmt)` | Format timestamp |

### Regex

| Function | Description |
|----------|-------------|
| `regex_match(pattern, s)` | Test match |
| `regex_find(pattern, s)` | Find first match |
| `regex_find_all(pattern, s)` | Find all matches |
| `regex_replace(pattern, s, repl)` | Replace first |
| `regex_replace_all(pattern, s, repl)` | Replace all |
| `regex_split(pattern, s)` | Split by pattern |

### Assertions & Errors

| Function | Description |
|----------|-------------|
| `assert(condition)` | Assert or panic |
| `panic(message)` | Panic with message |
| `exit(code)` | Exit process |

---

## Tooling

### CLI Commands

```bash
forma run <file>                   # Run a FORMA program
forma run <file> --dump-mir        # Run with MIR dump
forma run <file> --no-check-contracts # Disable runtime contracts
forma run <file> --no-optimize     # Disable MIR optimization pass
forma run <file> --allow-read      # Allow file reads
forma run <file> --allow-write     # Allow file writes
forma run <file> --allow-network   # Allow networking
forma run <file> --allow-exec      # Allow process execution
forma run <file> --allow-env       # Allow env var access
forma run <file> --allow-unsafe    # Allow pointer/unsafe builtins
forma run <file> --allow-all       # Allow all capabilities (see warning below)
forma check <file>                 # Type check without running
forma check <file> --partial       # Partial checking
forma build <file>                 # Build native executable (LLVM feature)
forma build <file> --no-optimize   # Build without MIR optimization
forma explain <file>               # Explain contracts in plain English
forma explain <file> --examples=3 --seed 42 --format json
forma explain <file> --max-examples 3 --seed 42 --format json
forma verify <file-or-dir> --report --format human
forma verify <file-or-dir> --report --format json --examples 20 --seed 42
forma verify <file-or-dir> --report --max-steps 10000 --timeout 1000
forma lex <file>                   # Dump tokens
forma parse <file>                 # Dump AST
forma grammar --format ebnf        # Export grammar as EBNF
forma grammar --format json        # Export grammar as JSON
forma fmt <file>                   # Format source code
forma repl                         # Interactive REPL
forma new <name>                   # Create new project
forma init                         # Initialize project in current dir
forma typeof <file> --position 5:10  # Query type at position
forma complete <file> --position 5:10  # Get completions
```

### Security Note

**Do not run untrusted FORMA code with `--allow-all`.** This flag enables file I/O, networking, process execution, environment variable access, and unsafe memory operations — equivalent to giving the program full host access.

Prefer least-privilege flags:

```bash
forma run myfile.forma --allow-read --allow-write  # file I/O only
forma run myfile.forma --allow-network             # networking only
```

The `--allow-exec` flag is particularly sensitive — it permits shell command execution via the `exec` builtin and should be treated as equivalent to full shell access.

When running `forma verify`, capabilities are revoked by default. Only use `--allow-side-effects` when you trust the code being verified.

### Verification and Explain Reports

- `forma explain` converts function contracts into readable intent (`human`, `json`, or `markdown`).
- `--examples` generates deterministic I/O samples when paired with `--seed`.
- Use `--examples=N` or `--max-examples N` to request an explicit explain-example count.
- `explain --examples` includes both satisfying examples and counterexamples that violate detected preconditions.
- `forma verify --report` executes generated examples and emits per-function status:
  - `PASS`: all generated examples satisfied contract checks
  - `SKIP`: function could not be safely/meaningfully executed (for example, capability-restricted paths)
  - `WARN`: no contracts defined
  - `FAIL`: one or more generated examples violated checks or errored
- Generated examples run side-effect safe by default; `--allow-side-effects` opts into full capabilities.
- Resource controls for verify:
  - `--max-steps` limits interpreter steps per generated example
  - `--timeout` sets per-example timeout budget in milliseconds

### Error Formats

Human-readable errors (default):

```
error[type_mismatch]: expected Int, found Str
  --> main.forma:5:12
```

JSON errors for AI consumption:

```bash
forma check --error-format json main.forma
```

```json
{
  "success": false,
  "errors": [{
    "file": "main.forma",
    "line": 5,
    "column": 12,
    "severity": "error",
    "code": "type_mismatch",
    "message": "expected Int, found Str"
  }]
}
```

### REPL

```bash
$ forma repl
forma> 2 + 3
5
forma> f greet(name: Str) -> Str = f"Hello, {name}!"
forma> greet("World")
"Hello, World!"
```

---

## For AI Developers

### Grammar-Constrained Generation

FORMA exports its grammar in EBNF and JSON formats, enabling constrained decoding. This means LLMs can generate only syntactically valid FORMA code:

```bash
forma grammar --format ebnf > forma.ebnf
forma grammar --format json > forma.json
```

Use these grammars with constrained decoding toolkits (Outlines, guidance, llama.cpp grammars) to eliminate syntax errors at generation time.

### Structured Errors for Self-Correction

When AI generates code that doesn't type-check, FORMA returns machine-readable errors:

```bash
forma check --error-format json broken.forma
```

Feed the JSON errors back to the LLM for automatic correction. Each error includes:
- Exact file, line, column location
- Error code (parseable category)
- Human-readable message
- Optional fix suggestion

### Type Queries

Query the type of any expression at a given position:

```bash
forma typeof myfile.forma --position "5:10"
```

This enables LLMs to make type-aware completions.

### Completion Suggestions

Get context-aware suggestions at a position:

```bash
forma complete myfile.forma --position "5:10"
```

### Minimal Token Budget

FORMA's short keywords and clean syntax mean AI-generated code costs fewer tokens:

| Concept | Rust | FORMA |
|---------|------|-------|
| Function | `fn` | `f` |
| Struct | `struct` | `s` |
| Enum | `enum` | `e` |
| Match | `match` | `m` |
| Trait | `trait` | `t` |
| Impl | `impl` | `i` |
| While | `while` | `wh` |
| Loop | `loop` | `lp` |
| Break | `break` | `br` |
| Continue | `continue` | `ct` |
| Return | `return` | `ret` |
| Async | `async` | `as` |
| Await | `.await` | `aw` |
| Spawn | `tokio::spawn` | `sp` |
| Use | `use` | `us` |
| Module | `mod` | `md` |
| Unsafe | `unsafe` | `un` |
| Move | `move` | `mv` |

### AI Reference

For a dense, token-efficient reference designed to fit in LLM system prompts, see [AI Reference](ai-reference.md).

---

## Appendix: Complete Keyword Table

| Keyword | Long Form | Purpose |
|---------|-----------|---------|
| `f` | — | Function declaration |
| `s` | — | Struct declaration |
| `e` | — | Enum declaration |
| `t` | — | Trait declaration |
| `i` | — | Impl block |
| `m` | — | Match expression |
| `if` | — | Conditional |
| `then` | — | Then branch (single-expr if) |
| `else` | — | Else branch |
| `for` | — | For loop |
| `in` | — | Iterator binding |
| `wh` | `while` | While loop |
| `lp` | `loop` | Infinite loop |
| `br` | `break` | Break |
| `ct` | `continue` | Continue |
| `ret` | `return` | Return |
| `as` | `async` | Async modifier |
| `aw` | `await` | Await expression |
| `sp` | `spawn` | Spawn task |
| `us` | `use` | Import |
| `md` | `module` | Module declaration |
| `pub` | — | Public visibility |
| `mut` | — | Mutable modifier |
| `ref` | — | Reference parameter |
| `mv` | `move` | Move semantics |
| `un` | `unsafe` | Unsafe block |
| `type` | — | Type alias |
| `where` | — | Trait bounds |
| `linear` | — | Linear type qualifier |
| `affine` | — | Affine type qualifier |
| `true` / `T` | — | Boolean true |
| `false` / `F` | — | Boolean false |
| `none` / `N` | — | None/null value |
| `Some` | — | Option::Some constructor |
| `Ok` / `ok` | — | Result::Ok constructor |
| `Err` / `err` | — | Result::Err constructor |

---

## Appendix: Operators

| Operator | Meaning |
|----------|---------|
| `+`, `-`, `*`, `/`, `%` | Arithmetic |
| `==`, `!=`, `<`, `<=`, `>`, `>=` | Comparison |
| `&&`, `\|\|`, `!` | Logical |
| `&`, `\|`, `^`, `<<`, `>>` | Bitwise |
| `=` | Assignment (struct fields) |
| `:=` | Variable binding / reassignment |
| `+=`, `-=`, `*=`, `/=`, `%=` | Compound assignment |
| `?` | Error propagation |
| `??` | Null coalescing |
| `->` | Return type / match arm |
| `=>` | Fat arrow |
| `..` | Range (exclusive) |
| `..=` | Range (inclusive) |
| `::` | Path separator |
| `.` | Field access |
| `@` | Pattern binding |
