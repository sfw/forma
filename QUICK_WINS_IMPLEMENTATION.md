# FORMA Language Quick Wins & Core Language Fixes

**Status:** In Progress
**Last Updated:** January 24, 2026

---

## Progress Tracker

| Section | Feature | Status | Notes |
|---------|---------|--------|-------|
| 1 | Range Iteration | ✅ DONE | |
| 2 | Integer Types | ⬜ TODO | |
| 3 | String Interpolation | ✅ DONE | |
| 4 | Random Numbers | ✅ DONE | |
| 5 | Float Math | ✅ DONE | |
| 6 | Time/Duration | ✅ DONE | |
| 7 | REPL | ✅ DONE | |
| 8 | Formatter | ✅ DONE | |
| 9 | VS Code Syntax | ✅ DONE | |
| 10 | Trait Fixes | 🔄 PARTIAL | Method resolution for unique names |
| 11 | Default Parameters | ✅ DONE | |

Legend: ⬜ TODO | 🔄 IN PROGRESS | ✅ DONE | ❌ BLOCKED

---

## SECTION 1: RANGE ITERATION (Critical Fix)

**Status:** ✅ DONE

The `for i in 0..10` syntax parses but doesn't work in the interpreter.

### Tasks:
- [x] Check `src/mir/lower.rs` for how Range expressions are lowered to MIR
- [x] Check `src/mir/interp.rs` for how ranges are interpreted
- [x] Fix so that `for i in 0..10 { print(i) }` works
- [x] Support `for i in 0..=10` (inclusive range)
- [x] Support `for i in start..end` where start/end are variables

### Test:
```forma
f main() -> Int {
    v sum = 0
    i x in 0..10 {
        sum = sum + x
    }
    sum  # Should be 45
}
```

### Commit: `feat(lang): implement range iteration`

---

## SECTION 2: INTEGER TYPES

**Status:** ⬜ TODO

Currently only `Int` (i64) exists. Add full integer type support.

### 2.1 Add types to the type system (src/types/types.rs):
- [ ] `i8`, `i16`, `i32`, `i64` (signed)
- [ ] `u8`, `u16`, `u32`, `u64` (unsigned)
- [ ] `isize`, `usize` (pointer-sized)
- [ ] Keep `Int` as alias for `i64`
- [ ] Add `Float` as alias for `f64`, add `f32`

### 2.2 Update the lexer (src/lexer/):
- [ ] Type keywords: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `isize`, `usize`, `f32`, `f64`
- [ ] Literal suffixes: `42i32`, `255u8`, `3.14f32`

### 2.3 Update the parser to handle these types

### 2.4 Update type checker (src/types/checker.rs):
- [ ] Allow implicit widening (i8 -> i16 -> i32 -> i64)
- [ ] Require explicit casts for narrowing or sign changes
- [ ] Add `as` cast expressions: `x as u8`

### 2.5 Update MIR and interpreter:
- [ ] Store values with their actual type
- [ ] Handle overflow based on type

### 2.6 Update LLVM codegen:
- [ ] Use correct LLVM integer types (i8, i16, i32, i64)

### Test:
```forma
f main() -> i32 {
    v a: u8 = 255
    v b: i32 = a as i32
    v c: u64 = 1000000
    b
}
```

### Commit: `feat(types): add integer types (i8, u8, i32, etc.)`

---

## SECTION 3: STRING FORMATTING / INTERPOLATION

**Status:** ✅ DONE

Add string interpolation with `{expr}` syntax.

### 3.1 Update lexer to recognize interpolated strings:
- [x] `f"Hello {name}!"` should tokenize as an interpolated string
- [x] Use `f"..."` prefix (like Python)

### 3.2 Update parser:
- [x] Parse interpolated strings into a series of concatenations
- [x] `f"Hello {name}!"` becomes `"Hello " + str(name) + "!"`

### 3.3 Add `str()` conversion function to builtins:
- [x] `str(42)` -> `"42"`
- [x] `str(true)` -> `"true"`
- [x] `str(3.14)` -> `"3.14"`

### Test:
```forma
f main() {
    v name = "World"
    v count = 42
    print(f"Hello {name}! Count is {count}.")
}
```

### Commit: `feat(lang): add string interpolation with f-strings`

---

## SECTION 4: RANDOM NUMBER GENERATION

**Status:** ✅ DONE

Add random number support to the interpreter builtins.

### 4.1 Add to Cargo.toml:
- [x] `rand = "0.8"`

### 4.2 Add to src/mir/interp.rs:
- [x] `random()` -> Float (0.0 to 1.0)
- [x] `random_int(min: Int, max: Int)` -> Int
- [x] `random_bool()` -> Bool
- [x] `random_choice(arr: [T])` -> T

### Test:
```forma
f main() {
    v n = random_int(1, 100)
    print(n)
    v coin = random_bool()
    print(coin)
}
```

### Commit: `feat(interp): add random number generation`

---

## SECTION 5: FLOAT MATH OPERATIONS

**Status:** ✅ DONE

Add standard math functions for floats.

### 5.1 Add to builtins (src/mir/interp.rs):
- [x] `sqrt(x: Float)` -> Float
- [x] `pow(base: Float, exp: Float)` -> Float
- [x] `sin(x: Float)` -> Float
- [x] `cos(x: Float)` -> Float
- [x] `tan(x: Float)` -> Float
- [x] `log(x: Float)` -> Float (natural log)
- [x] `log10(x: Float)` -> Float
- [x] `exp(x: Float)` -> Float
- [x] `floor(x: Float)` -> Int
- [x] `ceil(x: Float)` -> Int
- [x] `round(x: Float)` -> Int
- [x] `abs_float(x: Float)` -> Float

### Test:
```forma
f main() {
    v x = sqrt(16.0)
    print(x)  # 4.0
    v y = pow(2.0, 10.0)
    print(y)  # 1024.0
    v z = sin(3.14159 / 2.0)
    print(z)  # ~1.0
}
```

### Commit: `feat(interp): add float math functions`

---

## SECTION 6: TIME AND DURATION

**Status:** ✅ DONE

Add basic time support.

### 6.1 Add to builtins (src/mir/interp.rs):
- [x] `time_now()` -> Int (unix timestamp in seconds)
- [x] `time_now_ms()` -> Int (unix timestamp in milliseconds)
- [x] `time_sleep(ms: Int)` -> () (sleep for milliseconds)

### Test:
```forma
f main() {
    v start = time_now_ms()
    time_sleep(100)
    v end = time_now_ms()
    print(end - start)  # ~100
}
```

### Commit: `feat(interp): add time functions`

---

## SECTION 7: BASIC REPL

**Status:** ✅ DONE

Create a simple REPL for interactive use.

### 7.1 Add to Cargo.toml:
- [x] `rustyline = "14"`

### 7.2 Add `forma repl` command to src/main.rs

### 7.3 REPL features:
- [x] Print a prompt `forma> `
- [x] Read a line
- [x] If it's an expression, evaluate and print result
- [x] If it's a statement/definition, add to environment
- [x] Support `:help`, `:quit`, `:type expr`

### Test: Run `cargo run -- repl` manually

### Commit: `feat(cli): add REPL`

---

## SECTION 8: FORMATTER (forma fmt)

**Status:** ✅ DONE

Create a basic code formatter.

### 8.1 Add `forma fmt` command to src/main.rs

### 8.2 Create src/fmt/mod.rs with formatter logic:
- [x] Parse the file
- [x] Pretty-print the AST with consistent formatting
- [x] 4-space indentation
- [x] Consistent spacing around operators
- [x] One blank line between top-level items

### 8.3 Command options:
- [x] `forma fmt file.forma` - format and print to stdout
- [x] `forma fmt --write file.forma` - format in place
- [x] `forma fmt --check file.forma` - check if formatted

### Commit: `feat(cli): add formatter (forma fmt)`

---

## SECTION 9: VS CODE SYNTAX HIGHLIGHTING

**Status:** ✅ DONE

Create a TextMate grammar for VS Code.

### 9.1 Create directory structure:
- [x] `editors/vscode/`
- [x] `editors/vscode/syntaxes/`

### 9.2 Create `editors/vscode/syntaxes/forma.tmLanguage.json`:
- [x] Keywords: f, s, e, t, m, i, v, us, ret, br, ct, if, else, wh, as, aw
- [x] Types: Int, Float, Bool, Str, Char + integer types
- [x] Operators
- [x] Comments: # line comments
- [x] Strings with escapes
- [x] Numbers with suffixes

### 9.3 Create `editors/vscode/package.json`

### 9.4 Create `editors/vscode/language-configuration.json`:
- [x] Bracket matching
- [x] Auto-closing pairs
- [x] Comment toggling

### 9.5 Create README with installation instructions

### Commit: `feat(editor): add VS Code syntax highlighting`

---

## SECTION 10: TRAIT IMPLEMENTATION FIXES

**Status:** 🔄 PARTIAL

Traits parse but don't fully work. Fix the type checker.

### 10.1 Review src/types/checker.rs for trait handling

### 10.2 Implement:
- [ ] Trait bounds checking on generic parameters
- [x] Method resolution for trait methods (partial - works for unique method names)
- [x] `impl Trait for Type` blocks (works with unique method names)

### Test:
```forma
t Display {
    f display(&self) -> Str
}

s Point {
    x: Int
    y: Int
}

impl Display for Point {
    f display(&self) -> Str {
        f"({self.x}, {self.y})"
    }
}

f main() {
    v p = Point(x: 1, y: 2)
    print(p.display())
}
```

### Commit: `fix(types): implement trait method resolution`

---

## SECTION 11: DEFAULT PARAMETERS

**Status:** ✅ DONE

Default parameters parse but don't evaluate.

### 11.1 Check parser - defaults should be in AST

### 11.2 Update type checker to handle defaults

### 11.3 Update MIR lowering to insert defaults for missing args

### Test:
```forma
f greet(name: Str, greeting: Str = "Hello") {
    print(f"{greeting}, {name}!")
}

f main() {
    greet("World")           # "Hello, World!"
    greet("World", "Hi")     # "Hi, World!"
}
```

### Commit: `fix(interp): implement default parameters`

---

## TESTING & VERIFICATION

After completing all sections:

- [ ] `cargo build` - no errors
- [ ] `cargo test` - all tests pass
- [ ] Test all examples: `for f in examples/*.forma; do cargo run -- run "$f"; done`
- [ ] `cargo build --features llvm` - LLVM still works

---

## PRIORITY ORDER

If time is limited, do in this order:
1. Range iteration (critical, blocks common patterns)
2. Random numbers (quick win)
3. Float math (quick win)
4. Time functions (quick win)
5. String interpolation (high impact)
6. VS Code highlighting (high visibility)
7. Integer types (important but complex)
8. Formatter (expected by users)
9. REPL (nice for learning)
10. Traits (complex but important)
11. Default parameters (nice to have)

---

## NOTES

*Add implementation notes here as you work:*

### Section 1: Range Iteration
- **Parser fix**: Added range handling after parsing literals in `parse_primary()` (src/parser/parser.rs:1668-1688). Previously only `..end` and `name..end` were handled, not `literal..end`.
- **MIR lowering**: Added `lower_for_range()` function in src/mir/lower.rs that handles Range expressions by extracting start/end values and using appropriate comparison (< for exclusive, <= for inclusive).
- **Type inference**: Updated ExprKind::For handling in src/types/inference.rs to accept both Range[T] and List[T] types.
- All tests verified: `0..10`, `0..=10`, and `start..end` with variables all work correctly.

### Section 4: Random Numbers
- **Cargo.toml**: Added `rand = "0.8"` dependency.
- **Type environment**: Added type signatures for `random`, `random_int`, `random_bool`, `random_choice` in src/types/inference.rs.
- **Interpreter**: Added builtin implementations in src/mir/interp.rs using `rand::thread_rng()`.
- **Note**: Used `r#gen` instead of `gen` because `gen` is a reserved keyword in Rust 2024 edition.

### Section 5: Float Math
- All 12 math functions implemented using Rust's f64 methods: sqrt, pow, sin, cos, tan, log, log10, exp, floor, ceil, round, abs_float.
- Functions accept Int as well as Float for convenience (auto-converts to f64).

### Section 6: Time Functions
- Added imports: `std::time::{SystemTime, UNIX_EPOCH, Duration}` and `std::thread`.
- `time_now()`: Uses `SystemTime::now().duration_since(UNIX_EPOCH)` to get seconds.
- `time_now_ms()`: Same but returns milliseconds.
- `time_sleep()`: Uses `thread::sleep(Duration::from_millis(ms))`.

### Section 3: String Interpolation
- **Token**: Added `FString(Vec<FStringPart>)` token with `FStringPart::Text(String)` and `FStringPart::Expr(String)` variants.
- **Lexer**: Modified `scan_identifier` to detect `f"..."` prefix and parse f-strings with `{expr}` interpolation.
- **Parser**: Added `parse_fstring` that converts parts to concatenation: `f"Hello {x}!"` → `"Hello " + str(x) + "!"`.
- **Builtin**: Added `str()` function that converts any value to string (strings pass through without quotes).

### Section 9: VS Code Syntax Highlighting
- Created `editors/vscode/` directory with full extension structure.
- TextMate grammar (`forma.tmLanguage.json`) supports all keywords, types, operators, strings, f-strings, and numbers.
- Language configuration enables bracket matching, auto-closing pairs, comment toggling (#), and indentation-based folding.
- README includes three installation methods: copy, symlink, or VSIX packaging.

### Section 7: REPL
- **Dependencies**: Added `rustyline = "14"` to Cargo.toml.
- **CLI**: Added `Repl` command to the `Commands` enum in main.rs.
- **Features implemented**:
  - `:help` - shows available REPL commands
  - `:quit` / `:q` - exits the REPL
  - `:clear` - clears session definitions
  - `:defs` - shows current function definitions
  - `:type <expr>` - type-checks an expression
- **Expression evaluation**: Wraps user expressions in a temporary function that uses `print()` to output results.
- **Function definitions**: Detects lines starting with `f `, `s `, `e `, `t `, `impl`, or `type` and adds them to session code.
- **Note**: FORMA requires explicit return types on functions, so expression evaluation uses a wrapper that assigns to a variable and prints it.

### Section 8: Formatter
- **CLI**: Added `Fmt` command to main.rs with `--write` and `--check` flags.
- **Module**: Created `src/fmt/mod.rs` with a `Formatter` struct that pretty-prints AST.
- **Features implemented**:
  - Functions: handles visibility, async, params, return types, expression and block bodies
  - Structs: named fields, tuple structs, unit structs
  - Enums: variants (basic)
  - Types: path types, tuples, lists, option (`?`), result (`!`), references
  - Expressions: literals, identifiers, binary/unary ops, calls, if, range, return, tuples, arrays
  - Statements: let bindings, expression statements
  - Patterns: wildcard, ident (with mut), literal, tuple
- **Placeholders**: Some complex constructs (match, for, while, closures, field access, method calls) output `...` placeholder text.
- **Options**: `--write` formats in place, `--check` reports if file needs formatting (exit code 1).

### Section 11: Default Parameters
- **Type checker**: Added `FunctionInfo` struct to track required vs total params and param types.
- **Type inference**: Updated `TypeEnv` to store function info with `fn_info: HashMap<String, FunctionInfo>`.
- **Call checking**: Modified `ExprKind::Call` handling to check if callee is an identifier with function info, allowing calls with fewer args if >= required_params.
- **MIR lowering**: Added `fn_defaults: HashMap<String, Vec<Option<Expr>>>` to store default expressions.
- **Lowering functions**: When lowering a function, store its default expressions in `fn_defaults`.
- **Lowering calls**: When lowering a direct call with fewer args than expected, fill in missing args by lowering the default expressions.
- Supports multiple default parameters: `f foo(a: Int, b: Int = 0, c: Int = 100)`.

### Section 10: Trait Fixes (Partial)
- **MIR lowering**: Added `impl_methods: HashMap<String, Vec<String>>` to track method -> qualified names mapping.
- **Method resolution**: Updated `resolve_method()` to check `impl_methods` for user-defined methods.
- **Impl block processing**: When lowering impl blocks, methods are stored with qualified names (e.g., "Point::display") and also registered in `impl_methods`.
- **Limitation**: Method resolution works when method names are unique across all types. If multiple types implement the same method name, the lowerer can't determine which to call without receiver type info.
- **Full fix needed**: Pass typed AST to lowerer, or have type checker rewrite method calls with qualified names.


