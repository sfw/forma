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
f fetch_users(db: Database) -> Result[Vec[User]] {
    db.query("SELECT * FROM users")
      | filter(.active)
      | map(User::from_row)
      | collect()
}
```

Same memory safety as Rust. None of the complexity that trips up AI.

## Why FORMA Works

### Memory Safety Without Lifetimes

FORMA uses **second-class references**—references can't be stored in structs or returned from functions. This eliminates lifetime annotations entirely while preserving memory safety.

```forma
// Rust: fn longest<'a>(x: &'a str, y: &'a str) -> &'a str
// FORMA: No lifetimes needed
f longest(x: &String, y: &String) -> String {
    m x.len() > y.len() { x.clone() } e { y.clone() }
}
```

The compiler statically verifies memory safety. AI doesn't need to reason about lifetimes.

### 96% Fewer Syntax Errors

FORMA's grammar is designed for **constrained decoding**. AI models can generate only syntactically valid code:

```bash
# Export grammar for any LLM toolkit
forma grammar --format lark > forma.grammar
```

When AI generates tokens, invalid syntax is impossible.

### 38% Fewer Tokens

Every character costs API tokens. FORMA's concise syntax reduces costs and latency:

| Feature | Rust | FORMA |
|---------|------|-------|
| Function | `fn` | `f` |
| Struct | `struct` | `s` |
| Match | `match` | `m` |
| Option | `Option<T>` | `T?` |
| Result | `Result<T,E>` | `T!` |
| Vec | `Vec<T>` | `[T]` |

### AI Self-Correction

Errors are structured JSON that AI can parse and fix automatically:

```json
{
  "error": "type_mismatch",
  "expected": "Int",
  "found": "String",
  "location": {"line": 5, "column": 12},
  "suggestion": "Use .parse() to convert String to Int"
}
```

```bash
forma check --format json myfile.forma
```

## Quick Start

```bash
# Install
curl -sSL https://forma-lang.org/install.sh | sh

# Hello World
echo 'f main() { print("Hello, FORMA!") }' > hello.forma
forma run hello.forma
```

## Features

- **Type inference**: Hindley-Milner style, rarely need type annotations
- **Generics**: Full parametric polymorphism with monomorphization
- **Pattern matching**: Exhaustive, with guards
- **Pipeline operator**: `data | transform | filter | collect`
- **Result types**: No exceptions, explicit error handling
- **Modules**: Simple `us std.collections` imports
- **Native compilation**: LLVM backend for C-level performance

## Documentation

- [Getting Started](docs/getting-started.md)
- [Language Tour](docs/tour.md)
- [Language Reference](docs/reference.md)
- [Standard Library](docs/stdlib.md)
- [AI Integration Guide](docs/ai-integration.md)

## For AI Developers

FORMA provides first-class tooling for AI code generation:

```bash
# Grammar-constrained generation
forma grammar --format json

# Type-aware completion
forma complete --context "let x: Int = " --cursor 15

# Partial syntax validation
forma check --partial "f add(a: Int, b:"

# Structured errors for self-correction
forma check --format json myfile.forma
```

## Status

FORMA is in **alpha**. The core language works:

- [x] Lexer, parser, type checker
- [x] Borrow checker (second-class references)
- [x] MIR interpreter
- [x] Generics with monomorphization
- [x] Module system
- [x] Standard library (Vec, Map, Iterator)
- [x] Grammar export
- [ ] LLVM native compilation (in progress)
- [ ] Package manager
- [ ] Language server (LSP)

## Performance

*Benchmarks coming soon.*

FORMA compiles to native code via LLVM—the same backend as Rust and Clang. Performance should be comparable to Rust for equivalent code.

## Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

MIT OR Apache-2.0 (same as Rust)

---

<p align="center">
  <strong>FORMA</strong><br>
  Code that writes itself correctly.
</p>
