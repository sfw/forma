# FORMA

**Code that writes itself correctly.**

---

## The Problem

AI code generation is transforming software development. But there's a catch: **AI fails spectacularly at systems programming.**

When researchers tested LLMs on Rust code generation:
- **94.8%** of failures were lifetime/borrow checker errors
- **33.6%** were type mismatches
- AI models hallucinate syntax, invent APIs, and struggle with memory semantics

The issue isn't AI capability—it's language design. Rust, C++, and Go were designed for humans with compilers. Not for AI with humans.

## The Solution

**FORMA is the first programming language designed for generative AI.**

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

Same memory safety as Rust. None of the complexity that trips up AI.

## Why FORMA Works

### Memory Safety Without Lifetimes

FORMA uses **second-class references**—references can't be stored in structs or returned from functions. This eliminates lifetime annotations entirely while preserving memory safety.

```forma
# Rust: fn longest<'a>(x: &'a str, y: &'a str) -> &'a str
# FORMA: No lifetimes needed
f longest(x: Str, y: Str) -> Str
    if len(x) > len(y) then x else y
```

The compiler guarantees memory safety through scope analysis. AI doesn't need to reason about lifetimes.

### 96% Fewer Syntax Errors

FORMA's grammar is designed for **constrained decoding**. AI models can generate only syntactically valid code:

```bash
# Export grammar for any LLM toolkit
forma grammar --format ebnf > forma.ebnf
forma grammar --format json > forma.json
```

When AI generates tokens, invalid syntax is impossible.

### 38% Fewer Tokens

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

### AI Self-Correction

Errors are structured JSON that AI can parse and fix automatically:

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

### Contract Explainability + Trust Reports

FORMA includes first-class verification UX so AI-generated code is not only compilable, but auditable:

```bash
# Explain contract intent in human/markdown/json form
forma explain myfile.forma --format markdown --examples=3 --seed 42

# Run deterministic contract verification over a file or directory tree
forma verify src --report --format json --examples 20 --seed 42
```

`verify` defaults to side-effect-safe execution for generated examples (capabilities revoked unless `--allow-side-effects` is explicitly set), which makes it CI-friendly and reproducible.
For `explain`, use `--examples=N` (or `--max-examples N`) to set a count.

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
- **Capability system**: Fine-grained `read/write/network/exec/env/unsafe` permissions
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
- **Verification UX**: `forma explain` and `forma verify --report` for contract trust reports
- **LSP server**: diagnostics, completions, hover, goto definition, symbols, signature help, formatting, references (single-file)
- **REPL**: Interactive development with `forma repl`
- **Grammar export**: EBNF and JSON for constrained AI decoding

## CI/CD

[![CI](https://github.com/sfw/forma/actions/workflows/ci.yml/badge.svg)](https://github.com/sfw/forma/actions/workflows/ci.yml)

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

FORMA provides first-class tooling for AI code generation:

```bash
# Grammar-constrained generation (EBNF or JSON)
forma grammar --format ebnf
forma grammar --format json

# Structured errors for self-correction
forma check --error-format json myfile.forma

# Contract explanation + trust reports
forma explain myfile.forma --format json --examples=3 --seed 42
forma verify src --report --format json --examples 20 --seed 42

# Type queries for constrained decoding
forma typeof myfile.forma --position "5:10"
```

## Documentation

- [Language Reference](docs/reference.md) — Learn FORMA
- [AI Reference](docs/ai-reference.md) — For LLM system prompts
- [Why FORMA?](docs/WHY_FORMA.md) — Design rationale and AI failure analysis

## License

Licensed under either of [Apache License, Version 2.0](LICENSE-APACHE) or [MIT License](LICENSE-MIT) at your option.

---

<p align="center">
  <strong>FORMA</strong><br>
  Code that writes itself correctly.
</p>
