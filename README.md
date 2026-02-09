# FORMA

[![CI](https://github.com/sfw/forma/actions/workflows/ci.yml/badge.svg)](https://github.com/sfw/forma/actions/workflows/ci.yml)

**Code that writes itself correctly — and proves it.**

---

## The Problem

AI is writing more code than ever. But the toolchain is broken in six places.

AI models **can't write systems code reliably** — 94.8% of Rust generation failures are lifetime/borrow checker errors, 33.6% are type mismatches, and models routinely hallucinate APIs and syntax. AI models also **waste tokens** — verbose keywords and type annotations drive up API costs and latency. And even when AI does produce working code, **nobody can review it fast enough** — teams are shipping AI-generated functions they haven't fully read, because code review doesn't scale to hundreds of functions a day.

The issue isn't AI capability — it's language design. Rust, C++, and Go were designed for humans with compilers. Not for AI with humans. And none of them give you tools to understand and trust AI-generated code at scale.

## The Solution

**FORMA is the first programming language designed for generative AI — from generation through verification.**

```forma
f fetch_users(db: Database) -> Result[Vec[User], Str]
    rows := db_query(db, "SELECT * FROM users")?
    users := vec_new()
    for row in rows
        user := User {
            id: row_get_int(row, 0),
            name: row_get_str(row, 1),
            active: row_get_bool(row, 2)
        }
        vec_push(users, user)
    Ok(users)
```

Same memory safety as Rust. None of the complexity that trips up AI. Built-in tools to verify what was generated.

FORMA draws from the best ideas in language design: Rust's ownership model, Python's clean syntax, Dafny's contract verification, and Haskell's type inference.

## Why FORMA Works

FORMA solves six problems that every other AI coding stack leaves open.

### 1. Memory Safety Without Lifetimes

FORMA uses **second-class references** — references can't be stored in structs or returned from functions. This eliminates lifetime annotations entirely while preserving memory safety.

```forma
# Rust: fn longest<'a>(x: &'a str, y: &'a str) -> &'a str
# FORMA: No lifetimes needed
f longest(x: Str, y: Str) -> Str
    if len(x) > len(y) then x else y
```

The compiler guarantees memory safety through scope analysis. AI doesn't need to reason about lifetimes because they don't exist.

### 2. Strong Type Inference + Structured Errors

FORMA uses Hindley-Milner type inference, so AI rarely needs explicit type annotations. When types do mismatch, errors are machine-readable JSON with fix suggestions that AI can parse and self-correct:

```json
{
  "error": "type_mismatch",
  "expected": "Int",
  "found": "Str",
  "location": {"line": 5, "column": 12},
  "suggestion": "Use str_parse_int() to convert Str to Int"
}
```

```bash
forma check --error-format json myfile.forma
```

### 3. No API Hallucination

FORMA exports available methods for any type, letting AI tooling constrain generation to real APIs:

```bash
forma typeof myfile.forma --position "5:10"
```

AI pipelines can use this to only generate method calls that actually exist.

### 4. No Syntax Errors

FORMA's grammar is designed for **constrained decoding**. AI models can generate only syntactically valid code:

```bash
# Export grammar for any LLM toolkit
forma grammar --format ebnf > forma.ebnf
forma grammar --format json > forma.json
```

This eliminates syntax errors entirely — not by catching them, but by making them impossible.

### 5. ~35% Fewer Tokens

Every character costs API tokens. FORMA's concise syntax reduces costs and latency:

| Feature | Rust | FORMA |
|---------|------|-------|
| Function | `fn` | `f` |
| Struct | `struct` | `s` |
| Enum | `enum` | `e` |
| Match | `match` | `m` |
| While | `while` | `wh` |
| Return | `return` | `ret` / `return` |
| Use/Import | `use` | `us` |

Across typical codebases, FORMA uses ~35% fewer tokens than equivalent Rust. That's lower API costs, faster generation, and more code fitting in context windows.

### 6. Verifiable AI Intent

When AI writes your code, you need to know what it actually does. FORMA includes first-class verification UX so AI-generated code is not only compilable, but auditable:

```bash
# Explain contract intent in human-readable English
forma explain myfile.forma --format human

# Machine-readable JSON for CI integration
forma explain myfile.forma --format json --examples=3 --seed 42

# Run deterministic contract verification over a file or directory tree
forma verify src --report --format json --examples 20 --seed 42
```

For example, an AI generates a sort function with contracts. `forma explain` translates:

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

`verify --report` produces a trust report showing PASS/FAIL/WARN/SKIP status for every function's contracts — consumable by CI, QA teams, and code reviewers without reading source. Code review stops being "read 500 lines of sort logic" and becomes "confirm that the contract says what I wanted."

`verify` defaults to side-effect-safe execution (capabilities revoked unless `--allow-side-effects` is explicitly set), making it CI-friendly and reproducible.

## Quick Start

```bash
# Clone and build
git clone https://github.com/sfw/forma.git
cd forma
cargo build --release

# Hello World
echo 'f main()
    print("Hello, FORMA!")' > hello.forma
./target/release/forma run hello.forma
```

See [INSTALL.md](INSTALL.md) for detailed build instructions and dependencies.

## Feature Highlights

### Variables

FORMA uses two assignment operators:

```forma
# Immutable binding with =
x = 42
name = "Alice"

# Mutable binding with :=
counter := 0
counter := counter + 1  # reassignment also uses :=
```

Use `=` for values that never change, `:=` for values you'll update.

### Pattern Matching

```forma
e Shape
    Circle(Float)
    Rectangle(Float, Float)
    Triangle(Float, Float, Float)

f area(shape: Shape) -> Float
    m shape
        Circle(r) -> 3.14159 * r * r
        Rectangle(w, h) -> w * h
        Triangle(a, b, c) ->
            s := (a + b + c) / 2.0
            sqrt(s * (s - a) * (s - b) * (s - c))
```

### Async/Await

```forma
as f fetch_data(url: Str) -> Bool
    m http_get(url)
        Ok(_) -> true
        Err(_) -> false

as f main()
    # Spawn concurrent tasks
    task1 := sp fetch_data("https://api.example.com/data1")
    task2 := sp fetch_data("https://api.example.com/data2")

    # Wait for results
    result1 := aw task1
    result2 := aw task2
    print("Both requests complete!")
```

### HTTP Server

```forma
f handle_request(req: HttpRequest) -> HttpResponse
    m req.path
        "/" -> http_response(200, "Welcome!")
        "/api/hello" ->
            name := http_req_param(req, "name")
            m name
                Some(n) -> http_response(200, "Hello, " + n + "!")
                None -> http_response(200, "Hello, World!")
        _ -> http_response(404, "Not Found")

f main()
    print("Server starting on http://localhost:8080")
    result := http_serve(8080, handle_request)
    m result
        Ok(_) -> 0
        Err(e) ->
            print("Error: " + e)
            1
```

### SQLite Database

```forma
f main()
    db := db_open("app.db")!

    db_execute(db, "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)")!
    db_execute(db, "INSERT INTO users (name) VALUES ('Alice')")!

    rows := db_query(db, "SELECT id, name FROM users")!
    for row in rows
        id := row_get_int(row, 0)
        name := row_get_str(row, 1)
        print("User " + int_to_str(id) + ": " + name)

    db_close(db)
```

## Language Features

- **Type inference**: Hindley-Milner style, rarely need type annotations
- **Generics**: Full parametric polymorphism with monomorphization
- **Pattern matching**: Exhaustive, with guards
- **Result types**: No exceptions, explicit error handling with `?` and `!`
- **Linear types**: Affine and linear ownership tracking
- **Capability system**: Fine-grained `read/write/network/exec/env/unsafe` permissions (see [Security Note](#security-note))
- **Contracts**: `@pre`/`@post` with 12 named patterns, `old()`, quantifiers
- **Verification UX**: `forma explain` and `forma verify --report` for contract trust reports
- **Modules**: Simple `us std.collections` imports
- **Async/await**: Native coroutines with spawn
- **HTTP client & server**: Built-in networking primitives
- **TCP/UDP sockets**: Full networking stack with TLS support
- **SQLite**: Embedded database support (rusqlite)
- **Compression**: gzip and zlib built-in
- **Standard library**: 298+ builtin functions
- **FFI**: C interop with `extern` functions and safety layer
- **LLVM backend**: Native compilation (optional, requires LLVM 18)
- **Formatter**: `forma fmt` for consistent code style
- **LSP server**: diagnostics, completions, hover, goto definition, symbols, signature help, formatting, references (single-file)
- **REPL**: Interactive development with `forma repl`
- **Grammar export**: EBNF and JSON for constrained AI decoding

## Security Note

FORMA's capability system gates access to the host system. **Do not run untrusted FORMA code with `--allow-all`.** This flag enables file, network, process execution, environment variable, and unsafe memory operations.

Prefer least-privilege flags for the capabilities you actually need:

```bash
forma run myfile.forma --allow-read              # file reads only
forma run myfile.forma --allow-read --allow-write # file I/O only
forma run myfile.forma --allow-network            # networking only
```

The `--allow-exec` flag is particularly sensitive — it permits shell command execution via the `exec` builtin. Treat it as equivalent to giving the program full shell access.

When running `forma verify`, capabilities are revoked by default. Use `--allow-side-effects` only when you trust the code being verified.

## CI/CD

| Job | What it checks |
|-----|---------------|
| **Test** | `cargo test --all` — 400+ unit and integration tests |
| **Clippy** | `cargo clippy -- -D warnings` — zero warnings |
| **Format** | `cargo fmt --all -- --check` — consistent style |
| **LLVM Feature Check** | Clippy + compile check with `--features llvm` (LLVM 18) |
| **Runtime Crate Tests** | `cd runtime && cargo test` — runtime FFI coverage |
| **FORMA Integration Tests** | Runs all `.forma` integration tests in CI, including expected-failure contract fixtures |
| **Showcase Examples** | Runs all `examples/showcase/*.forma` end-to-end |

## Status

FORMA is in **active development**. The core language and standard library are functional:

- [x] Lexer, parser, type checker
- [x] Borrow checker (second-class references)
- [x] MIR interpreter
- [x] Generics with monomorphization
- [x] Linear types and capability system
- [x] Module system
- [x] Standard library (298+ builtins)
- [x] Grammar export (EBNF, JSON)
- [x] LLVM native compilation (optional feature)
- [x] Package manager (basic, path-based dependencies)
- [x] Async/await with spawn
- [x] HTTP client & server
- [x] TCP/UDP sockets and TLS
- [x] SQLite database
- [x] Compression (gzip, zlib)
- [x] LSP server (diagnostics, completions, hover, goto definition, symbols, signature help, formatting, references [single-file])
- [x] Verification UX (`explain`, `verify --report`)
- [x] Code formatter
- [x] REPL
- [x] 21 showcase examples passing (classic algorithms + verification UX demos)
- [ ] Full LSP (rename/refactor and cross-file references)
- [ ] Package registry

## For AI Developers

FORMA provides first-class tooling for the full AI code generation lifecycle — from generation through review.

### Quick LLM Onboarding

Add this to your system prompt:

```
You are writing code in FORMA, an AI-optimized systems language.
KEYWORDS: f=function, s=struct, e=enum, t=trait, i=impl, m=match
VARIABLES: x = 42 (immutable), y := 0 (mutable), y := y + 1 (reassignment uses :=)
TYPES: Int, Float, Bool, Str, [T]=list, T?=Option, T!E=Result
CONTRACTS: @pre(condition), @post(condition)
```

See the [Language Reference](https://sfw.github.io/forma/reference.html) for full LLM onboarding guidance.

### Tooling

```bash
# Grammar-constrained generation (EBNF or JSON)
forma grammar --format ebnf
forma grammar --format json

# Structured errors for self-correction
forma check --error-format json myfile.forma

# Understand what AI-generated code actually does
forma explain myfile.forma --format human
forma explain myfile.forma --format json --examples=3 --seed 42

# Verify AI-generated code at scale — CI-safe, deterministic
forma verify src --report --format json --examples 20 --seed 42

# Type queries for constrained decoding
forma typeof myfile.forma --position "5:10"
```

The generation side (grammar export, structured errors, token efficiency) means AI writes correct FORMA code. The verification side (`explain`, `verify`) means humans can actually understand and trust it. Together, they close the loop that every other AI coding stack leaves open: *AI writes it, FORMA proves what it does, you decide if that's what you wanted.*

## Documentation

- [Language Reference](docs/reference.md) — Learn FORMA
- [AI Reference](docs/ai-reference.md) — For LLM system prompts
- [Why FORMA?](docs/WHY_FORMA.md) — Design rationale and AI failure analysis

## License

Licensed under the [MIT License](LICENSE-MIT).

---

<p align="center">
  <strong>FORMA</strong><br>
  Code that writes itself correctly — and proves it.
</p>
