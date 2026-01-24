# FORMA Language Quick Wins & Core Language Fixes

**Status:** In Progress
**Last Updated:** January 24, 2026

---

## Progress Tracker

| Section | Feature | Status | Notes |
|---------|---------|--------|-------|
| 1 | Range Iteration | ✅ DONE | |
| 2 | Integer Types | ⬜ TODO | |
| 3 | String Interpolation | ⬜ TODO | |
| 4 | Random Numbers | ⬜ TODO | |
| 5 | Float Math | ⬜ TODO | |
| 6 | Time/Duration | ⬜ TODO | |
| 7 | REPL | ⬜ TODO | |
| 8 | Formatter | ⬜ TODO | |
| 9 | VS Code Syntax | ⬜ TODO | |
| 10 | Trait Fixes | ⬜ TODO | |
| 11 | Default Parameters | ⬜ TODO | |

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

**Status:** ⬜ TODO

Add string interpolation with `{expr}` syntax.

### 3.1 Update lexer to recognize interpolated strings:
- [ ] `f"Hello {name}!"` should tokenize as an interpolated string
- [ ] Use `f"..."` prefix (like Python)

### 3.2 Update parser:
- [ ] Parse interpolated strings into a series of concatenations
- [ ] `f"Hello {name}!"` becomes `"Hello " + str(name) + "!"`

### 3.3 Add `str()` conversion function to builtins:
- [ ] `str(42)` -> `"42"`
- [ ] `str(true)` -> `"true"`
- [ ] `str(3.14)` -> `"3.14"`

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

**Status:** ⬜ TODO

Add random number support to the interpreter builtins.

### 4.1 Add to Cargo.toml:
- [ ] `rand = "0.8"`

### 4.2 Add to src/mir/interp.rs:
- [ ] `random()` -> Float (0.0 to 1.0)
- [ ] `random_int(min: Int, max: Int)` -> Int
- [ ] `random_bool()` -> Bool
- [ ] `random_choice(arr: [T])` -> T

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

**Status:** ⬜ TODO

Add standard math functions for floats.

### 5.1 Add to builtins (src/mir/interp.rs):
- [ ] `sqrt(x: Float)` -> Float
- [ ] `pow(base: Float, exp: Float)` -> Float
- [ ] `sin(x: Float)` -> Float
- [ ] `cos(x: Float)` -> Float
- [ ] `tan(x: Float)` -> Float
- [ ] `log(x: Float)` -> Float (natural log)
- [ ] `log10(x: Float)` -> Float
- [ ] `exp(x: Float)` -> Float
- [ ] `floor(x: Float)` -> Int
- [ ] `ceil(x: Float)` -> Int
- [ ] `round(x: Float)` -> Int
- [ ] `abs_float(x: Float)` -> Float

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

**Status:** ⬜ TODO

Add basic time support.

### 6.1 Add to builtins (src/mir/interp.rs):
- [ ] `time_now()` -> Int (unix timestamp in seconds)
- [ ] `time_now_ms()` -> Int (unix timestamp in milliseconds)
- [ ] `time_sleep(ms: Int)` -> () (sleep for milliseconds)

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

**Status:** ⬜ TODO

Create a simple REPL for interactive use.

### 7.1 Add to Cargo.toml:
- [ ] `rustyline = "14"`

### 7.2 Add `forma repl` command to src/main.rs

### 7.3 REPL features:
- [ ] Print a prompt `forma> `
- [ ] Read a line
- [ ] If it's an expression, evaluate and print result
- [ ] If it's a statement/definition, add to environment
- [ ] Support `:help`, `:quit`, `:type expr`

### Test: Run `cargo run -- repl` manually

### Commit: `feat(cli): add REPL`

---

## SECTION 8: FORMATTER (forma fmt)

**Status:** ⬜ TODO

Create a basic code formatter.

### 8.1 Add `forma fmt` command to src/main.rs

### 8.2 Create src/fmt/mod.rs with formatter logic:
- [ ] Parse the file
- [ ] Pretty-print the AST with consistent formatting
- [ ] 4-space indentation
- [ ] Consistent spacing around operators
- [ ] One blank line between top-level items

### 8.3 Command options:
- [ ] `forma fmt file.forma` - format and print to stdout
- [ ] `forma fmt --write file.forma` - format in place
- [ ] `forma fmt --check file.forma` - check if formatted

### Commit: `feat(cli): add formatter (forma fmt)`

---

## SECTION 9: VS CODE SYNTAX HIGHLIGHTING

**Status:** ⬜ TODO

Create a TextMate grammar for VS Code.

### 9.1 Create directory structure:
- [ ] `editors/vscode/`
- [ ] `editors/vscode/syntaxes/`

### 9.2 Create `editors/vscode/syntaxes/forma.tmLanguage.json`:
- [ ] Keywords: f, s, e, t, m, i, v, us, ret, br, ct, if, else, wh, as, aw
- [ ] Types: Int, Float, Bool, Str, Char + integer types
- [ ] Operators
- [ ] Comments: # line comments
- [ ] Strings with escapes
- [ ] Numbers with suffixes

### 9.3 Create `editors/vscode/package.json`

### 9.4 Create `editors/vscode/language-configuration.json`:
- [ ] Bracket matching
- [ ] Auto-closing pairs
- [ ] Comment toggling

### 9.5 Create README with installation instructions

### Commit: `feat(editor): add VS Code syntax highlighting`

---

## SECTION 10: TRAIT IMPLEMENTATION FIXES

**Status:** ⬜ TODO

Traits parse but don't fully work. Fix the type checker.

### 10.1 Review src/types/checker.rs for trait handling

### 10.2 Implement:
- [ ] Trait bounds checking on generic parameters
- [ ] Method resolution for trait methods
- [ ] `impl Trait for Type` blocks

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

**Status:** ⬜ TODO

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

