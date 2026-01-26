# Sprint 14: Final Fixes for v1.0

**Priority:** CRITICAL - Last sprint before v1.0
**Date:** January 25, 2026
**Source:** CODE_REVIEW_V2_REPORT.md

---

## Overview

This sprint addresses all remaining issues identified in the deep-dive code review. After this sprint, FORMA will be production-ready for v1.0 release.

---

## Sprint 14.1: Critical Fixes (Priority 1)

### Task 14.1.1: Enum Discriminant Registry (Replace FNV-1a)

**File:** `src/mir/interp.rs`

**Current Problem:** Uses FNV-1a hash which, while better than byte-sum, still has theoretical collision risk and produces non-deterministic discriminant values.

**Required Solution:** Implement index-based registry as originally proposed.

```rust
/// Registry for enum variant discriminants using sequential indices
struct EnumRegistry {
    /// Map from (enum_name, variant_name) -> unique index
    variant_indices: HashMap<(String, String), i64>,
    /// Next available index per enum
    next_index: HashMap<String, i64>,
}

impl EnumRegistry {
    fn new() -> Self {
        Self {
            variant_indices: HashMap::new(),
            next_index: HashMap::new(),
        }
    }

    /// Register a variant and return its discriminant index
    fn register_variant(&mut self, enum_name: &str, variant_name: &str) -> i64 {
        let key = (enum_name.to_string(), variant_name.to_string());

        // Return existing index if already registered
        if let Some(&idx) = self.variant_indices.get(&key) {
            return idx;
        }

        // Allocate next sequential index for this enum
        let idx = self.next_index.entry(enum_name.to_string()).or_insert(0);
        let result = *idx;
        *idx += 1;
        self.variant_indices.insert(key, result);
        result
    }

    /// Get discriminant for a variant (must be pre-registered)
    fn get_discriminant(&self, enum_name: &str, variant_name: &str) -> i64 {
        // Built-in enums have fixed discriminants
        match (enum_name, variant_name) {
            ("Option", "None") => 0,
            ("Option", "Some") => 1,
            ("Result", "Ok") => 1,
            ("Result", "Err") => 0,
            _ => {
                // User-defined enums use registered indices
                let key = (enum_name.to_string(), variant_name.to_string());
                self.variant_indices.get(&key).copied().unwrap_or_else(|| {
                    panic!("BUG: unregistered enum variant: {}::{}", enum_name, variant_name)
                })
            }
        }
    }
}
```

**Update Interpreter struct:**

```rust
pub struct Interpreter {
    // ... existing fields ...

    /// Registry for enum discriminants
    enum_registry: EnumRegistry,
}

impl Interpreter {
    pub fn new(program: &MirProgram) -> Self {
        let mut enum_registry = EnumRegistry::new();

        // Register all enum variants at startup
        for (enum_name, enum_def) in &program.enums {
            for variant in &enum_def.variants {
                enum_registry.register_variant(enum_name, &variant.name);
            }
        }

        Self {
            enum_registry,
            // ... other fields ...
        }
    }
}
```

**Replace all FNV-1a hash calls:**

Find and replace any code that looks like:
```rust
// OLD - remove this
fn variant_discriminant(variant: &str) -> i64 {
    // FNV-1a or byte-sum hash
    ...
}
```

With:
```rust
// NEW - use registry
self.enum_registry.get_discriminant(enum_name, variant_name)
```

**Why this matters:**
- Index 0, 1, 2... is deterministic and debuggable
- Zero collision risk by design
- Matches how Rust, C, and other languages work
- Enables future features like `#[repr(u8)]` control

**Acceptance Criteria:**
- [ ] EnumRegistry struct added
- [ ] All enum variants registered at interpreter startup
- [ ] get_discriminant() used everywhere instead of hash
- [ ] Test: enum with variants "ab" and "ba" work correctly
- [ ] Test: enum with 20+ variants works correctly

---

### Task 14.1.2: Fix EOF Dedent Generation

**File:** `src/lexer/scanner.rs` (around lines 89-98)

**Problem:** When file ends, pending dedents are calculated after modifying the indent stack.

**Current (buggy):**
```rust
if self.indent_stack.len() > 1 {
    self.indent_stack.clear();  // Modifies stack first!
    self.indent_stack.push(0);
    self.pending_dedents = ???  // Too late, stack already cleared
    return self.make_token(TokenKind::Dedent);
}
```

**Fix:**
```rust
if self.indent_stack.len() > 1 {
    // Calculate pending dedents BEFORE modifying stack
    self.pending_dedents = self.indent_stack.len() - 1;
    self.indent_stack.clear();
    self.indent_stack.push(0);
    return self.make_token(TokenKind::Dedent);
}
```

**Acceptance Criteria:**
- [ ] Deeply nested functions (3+ levels) tokenize correctly
- [ ] No extra/missing dedent tokens at EOF

---

### Task 14.1.3: Fix 'm' at EOF Ambiguity

**File:** `src/parser/parser.rs` (in `is_match_keyword()` function)

**Problem:** When `m` is the last token before EOF, it's treated as match keyword instead of identifier.

**Current exclusion list doesn't include EOF:**
```rust
!matches!(next.kind,
    TokenKind::ColonEq |
    TokenKind::Colon |
    TokenKind::Comma |
    // ... etc, but NO TokenKind::Eof!
)
```

**Fix:** Add EOF to the exclusion list:
```rust
!matches!(next.kind,
    TokenKind::ColonEq |
    TokenKind::Colon |
    TokenKind::Comma |
    TokenKind::RParen |
    TokenKind::RBracket |
    TokenKind::RBrace |
    TokenKind::Eq |
    TokenKind::Dot |
    TokenKind::Eof  // ADD THIS
)
```

**Acceptance Criteria:**
- [ ] Code ending with variable `m` parses correctly
- [ ] Match expressions still work

---

### Task 14.1.4: Add Isize/Usize to Unification

**File:** `src/types/inference.rs` (in `unify()` function, around lines 2539-2559)

**Problem:** `Ty::Isize` and `Ty::Usize` are missing from the unification match.

**Add these cases:**
```rust
(Ty::Isize, Ty::Isize) => Ok(()),
(Ty::Usize, Ty::Usize) => Ok(()),
```

**Acceptance Criteria:**
- [ ] Variables of type `isize` and `usize` unify correctly
- [ ] No confusing type errors for pointer-sized integers

---

### Task 14.1.5: Complete Method Type Classification

**File:** `src/types/inference.rs` (in `classify_type_for_method()` around lines 3085-3108)

**Problem:** Only handles generic `Int`/`Float`, not specific types like `I32`, `F64`.

**Current:**
```rust
fn classify_type_for_method(&self, ty: &Ty) -> &'static str {
    match ty {
        Ty::Int => "Int",
        Ty::Float => "Float",
        // Missing: I8, I16, I32, I64, I128, U8, U16, U32, U64, U128, F32, F64, Isize, Usize
        ...
    }
}
```

**Fix - add all numeric types:**
```rust
fn classify_type_for_method(&self, ty: &Ty) -> &'static str {
    match ty {
        // Generic
        Ty::Int => "Int",
        Ty::Float => "Float",

        // Signed integers - treat as Int for method lookup
        Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::I128 | Ty::Isize => "Int",

        // Unsigned integers - treat as Int for method lookup
        Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::U128 | Ty::Usize => "Int",

        // Floats
        Ty::F32 | Ty::F64 => "Float",

        // Other types
        Ty::Str => "Str",
        Ty::Bool => "Bool",
        Ty::Char => "Char",
        Ty::List(_) => "Vec",
        Ty::Map(_, _) => "Map",
        Ty::Named(id, _) => &id.name,  // User-defined types
        _ => "Unknown",
    }
}
```

**Acceptance Criteria:**
- [ ] `x: i32` can call `.abs()` method
- [ ] `y: f64` can call `.floor()` method
- [ ] No "Unknown type" errors for numeric types

---

### Task 14.1.6: Fix pow() Negative Exponent

**File:** `stdlib/core.forma` (around lines 77-80)

**Problem:** Negative exponents cause infinite recursion.

**Current (broken):**
```forma
f pow(base: Int, exp: Int) -> Int {
    if exp == 0 { 1 }
    else if exp == 1 { base }
    else { base * pow(base, exp - 1) }  # Infinite loop if exp < 0!
}
```

**Fix:**
```forma
f pow(base: Int, exp: Int) -> Int {
    if exp < 0 {
        # Integer power with negative exponent is 0 (rounds down)
        # For 1/base^|exp|, result is 0 for |base| > 1
        if base == 1 { 1 }
        else if base == -1 {
            if exp % 2 == 0 { 1 } else { -1 }
        }
        else { 0 }
    }
    else if exp == 0 { 1 }
    else if exp == 1 { base }
    else { base * pow(base, exp - 1) }
}
```

**Acceptance Criteria:**
- [ ] `pow(2, -1)` returns 0 (not infinite loop)
- [ ] `pow(1, -5)` returns 1
- [ ] `pow(-1, -3)` returns -1
- [ ] `pow(2, 10)` still returns 1024

---

## Sprint 14.2: Stdlib & Examples (Priority 2)

### Task 14.2.1: Implement Duration Builtins

**File:** `src/mir/interp.rs` (in builtins section)

**Add these builtins:**

```rust
"duration_seconds" => {
    let secs = args[0].as_int()?;
    Ok(Value::Int(secs * 1000))  // Return milliseconds
}

"duration_minutes" => {
    let mins = args[0].as_int()?;
    Ok(Value::Int(mins * 60 * 1000))
}

"duration_hours" => {
    let hours = args[0].as_int()?;
    Ok(Value::Int(hours * 60 * 60 * 1000))
}

"duration_days" => {
    let days = args[0].as_int()?;
    Ok(Value::Int(days * 24 * 60 * 60 * 1000))
}
```

**Also add type signatures in `src/types/inference.rs`:**

```rust
"duration_seconds" => Some(FunctionType {
    params: vec![Ty::Int],
    return_type: Ty::Int,
}),
"duration_minutes" => Some(FunctionType {
    params: vec![Ty::Int],
    return_type: Ty::Int,
}),
// ... etc
```

**Acceptance Criteria:**
- [ ] `duration_minutes(5)` returns 300000 (milliseconds)
- [ ] `duration_hours(1)` returns 3600000
- [ ] Datetime arithmetic works: `time_now_ms() + duration_hours(1)`

---

### Task 14.2.2: Add args() Builtin

**File:** `src/mir/interp.rs`

**Add CLI argument access:**

```rust
"args" => {
    let args: Vec<Value> = std::env::args()
        .skip(1)  // Skip the program name
        .map(|s| Value::Str(s))
        .collect();
    Ok(Value::Vec(args))
}
```

**Type signature:**
```rust
"args" => Some(FunctionType {
    params: vec![],
    return_type: Ty::List(Box::new(Ty::Str)),
}),
```

**Acceptance Criteria:**
- [ ] `args()` returns command-line arguments
- [ ] `cli_with_db.forma` example compiles

---

### Task 14.2.3: Verify Async Example Works

**File:** `examples/async_downloader.forma`

**Action:** Test that this example actually runs:

```bash
cargo run --quiet -- run examples/async_downloader.forma
```

**If it fails, identify and fix the issue:**
- Is `sp` lowered correctly to MIR?
- Does `await_all` work with Task types?
- Are HTTP requests actually made?

**Acceptance Criteria:**
- [ ] Example runs without error
- [ ] Output shows URLs being fetched
- [ ] Demonstrates concurrent behavior

---

### Task 14.2.4: Uncomment String Character Functions

**File:** `stdlib/string.forma` (lines 123-134)

**Problem:** Functions are commented out despite `char_to_str` existing.

**Action:** Uncomment these functions:

```forma
# Get character at index as string (single char)
f str_char_at_str(s: Str, idx: Int) -> Str? {
    m str_char_at(s, idx) {
        Some(c) => Some(char_to_str(c)),
        None => None
    }
}
```

**Acceptance Criteria:**
- [ ] `str_char_at_str("hello", 0)` returns `Some("h")`
- [ ] Character manipulation works in stdlib

---

## Sprint 14.3: Tooling Essentials (Priority 3)

### Task 14.3.1: Complete Formatter

**File:** `src/fmt/mod.rs`

**Problem:** Many expression types output `"..."` placeholder, which corrupts source files.

**Required:** Implement formatting for ALL expression and statement types:

- [ ] Match expressions
- [ ] For loops
- [ ] While loops
- [ ] Closures/lambdas
- [ ] Field access
- [ ] Method calls
- [ ] Index expressions
- [ ] Struct literals
- [ ] Enum variants
- [ ] Range expressions
- [ ] Async/spawn expressions

**Acceptance Criteria:**
- [ ] `forma fmt examples/comprehensive.forma` produces valid code
- [ ] Round-trip: format then parse produces same AST
- [ ] No `"..."` placeholders in output

---

### Task 14.3.2: Complete Grammar Export

**File:** `src/main.rs` (grammar command, lines ~1287-1771)

**Missing from grammar:**
- Shorthand keywords (f, s, e, t, i, m)
- Indentation/dedent rules
- Operator precedence table
- F-string syntax
- Async syntax (as f, sp, aw)
- Contract syntax (pre:, post:)

**Acceptance Criteria:**
- [ ] Grammar includes all FORMA syntax
- [ ] External tools can parse FORMA using exported grammar

---

### Task 14.3.3: Fix JSON Error Format

**File:** `src/main.rs` (around lines 227-256)

**Problems:**
- `end_column` calculation incorrect
- Missing fields that AI tools expect

**Required JSON format:**
```json
{
  "error": "type_error",
  "message": "type mismatch: expected Int, found Str",
  "location": {
    "file": "example.forma",
    "line": 10,
    "column": 5,
    "end_line": 10,
    "end_column": 15
  },
  "suggestion": "Did you mean to convert with int()?"
}
```

**Acceptance Criteria:**
- [ ] Errors include accurate end_column
- [ ] JSON is valid and parseable
- [ ] Includes suggestion field where applicable

---

### Task 14.3.4: Implement REPL :type Command

**File:** `src/main.rs` (REPL section, around lines 1773-2018)

**Problem:** `:type expr` just says "Expression is well-typed" instead of showing the actual type.

**Fix:** Actually display the inferred type:

```rust
// In REPL :type handler
if input.starts_with(":type ") {
    let expr_str = &input[6..];
    match infer_expression_type(expr_str, &session_env) {
        Ok(ty) => println!("Type: {}", ty),
        Err(e) => println!("Type error: {}", e),
    }
}
```

**Acceptance Criteria:**
- [ ] `:type 42` shows `Int`
- [ ] `:type "hello"` shows `Str`
- [ ] `:type [1, 2, 3]` shows `Vec[Int]`

---

## Sprint 14.4: Verification

### Task 14.4.1: Run Full Test Suite

```bash
#!/bin/bash
set -e

echo "=== Cargo Tests ==="
cargo test

echo "=== FORMA Integration Tests ==="
for f in tests/forma/*.forma; do
    echo "Testing $f"
    cargo run --quiet -- run "$f"
done

echo "=== Examples ==="
for f in examples/*.forma; do
    echo "Checking $f"
    cargo run --quiet -- check "$f"
done

echo "=== Examples Run ==="
cargo run --quiet -- run examples/hello.forma
cargo run --quiet -- run examples/factorial.forma
cargo run --quiet -- run examples/fibonacci.forma

echo "=== Stdlib Check ==="
for f in stdlib/*.forma; do
    echo "Checking $f"
    cargo run --quiet -- check "$f"
done

echo "=== Grammar Export ==="
cargo run -- grammar > /tmp/forma_grammar.json
echo "Grammar exported successfully"

echo "=== Formatter Test ==="
cargo run -- fmt examples/hello.forma > /tmp/formatted.forma
cargo run -- check /tmp/formatted.forma
echo "Formatter works"

echo "=== REPL Test ==="
echo -e ":type 42\n:quit" | cargo run -- repl

echo "=== All checks passed ==="
```

---

### Task 14.4.2: Create CHANGELOG.md

**File:** `CHANGELOG.md` (new file in project root)

```markdown
# FORMA Changelog

## v1.0.0 (January 2026)

### Language Features
- Full type inference with Hindley-Milner algorithm
- Structs, enums, traits, and impl blocks
- Pattern matching with exhaustiveness checking
- Async/await with spawn (`sp`) and `await_all`
- Contracts with `pre:` and `post:` conditions
- Generic types and functions
- Integer types: i8-i64, u8-u64, isize, usize
- Float types: f32, f64
- String interpolation with f-strings

### Standard Library
- core: Math functions (min, max, abs, pow, gcd, lcm)
- vec: Vector operations (push, pop, map, filter, fold)
- string: String manipulation (split, trim, replace)
- map: Key-value collections
- iter: Iteration utilities
- json: JSON parsing and generation
- datetime: Time and duration functions

### Tooling
- `forma run` - Execute FORMA programs
- `forma check` - Type check without running
- `forma fmt` - Code formatter
- `forma repl` - Interactive REPL
- `forma grammar` - Export grammar for tooling
- `forma lsp` - Language Server Protocol support

### Security
- SQL injection protection via prepared statements
- No unsafe memory operations

### AI-Optimized Design
- Token-efficient syntax (short keywords)
- Strong type inference (minimal annotations)
- Contextual keywords (m, s, f, e, t, i work as identifiers)
- Machine-readable error output (JSON format)
```

---

## Definition of Done

### All Must Pass:
- [ ] All cargo tests pass
- [ ] All FORMA integration tests pass
- [ ] All examples compile
- [ ] Key examples run correctly
- [ ] Formatter produces valid code
- [ ] Grammar export complete
- [ ] REPL works with :type command
- [ ] CHANGELOG.md created
- [ ] No critical issues remaining

---

## Summary

| Priority | Tasks | Estimated Time |
|----------|-------|----------------|
| 14.1 Critical | 6 tasks | 4-5 hours |
| 14.2 Stdlib | 4 tasks | 3-4 hours |
| 14.3 Tooling | 4 tasks | 5-6 hours |
| 14.4 Verification | 2 tasks | 1-2 hours |
| **Total** | **16 tasks** | **13-17 hours** |

After this sprint, FORMA v1.0 will be complete and production-ready.

---

*"The final sprint - then FORMA ships."*
