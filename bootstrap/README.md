# Forma Bootstrap Experiment

This directory contains an incomplete compiler/bootstrap experiment written in
Forma. It demonstrates lexer, parser, type-checker, MIR, and interpreter data
structures; it is not the authoritative 0.2 compiler and does not yet provide a
complete self-hosting source-to-execution path.

The Rust compiler and `planning/design/FORMA_0_2_SEMANTICS.md` define current
language behavior. Bootstrap sources may deliberately trail the complete language.

## Overview

The bootstrap compiler demonstrates FORMA's capability to implement its own toolchain. It provides:

- **Lexer/Scanner** - Tokenizes FORMA source code
- **Parser** - Builds an Abstract Syntax Tree (AST)
- **Type Checker** - Validates types and infers type information
- **MIR Lowering** - Converts AST to Mid-level Intermediate Representation
- **Interpreter** - Executes MIR programs

## File Structure

### Core Components

| File | Description | Items |
|------|-------------|-------|
| `token.forma` | Token type definitions and constants | ~180 |
| `scanner_v2.forma` | Lexer that tokenizes source strings | ~300 |
| `ast.forma` | AST node type definitions | ~150 |
| `parser.forma` | Recursive descent parser | ~900 |
| `type_checker.forma` | Type inference and checking | ~650 |
| `mir.forma` | MIR type definitions | 73 |
| `lower.forma` | AST to MIR lowering | 168 |
| `interp.forma` | MIR interpreter | 105 |

### Combined Files

| File | Description |
|------|-------------|
| `lexer_v2_combined.forma` | Token + Scanner (runnable) |
| `parser_combined.forma` | Token + AST + Parser |
| `typechecker_combined.forma` | Full frontend pipeline |

### Integration & Tests

| File | Description |
|------|-------------|
| `forma_bootstrap.forma` | Full MIR interpreter with test programs |
| `test_bootstrap.forma` | Unit tests for MIR infrastructure |
| `test_self_host.forma` | Self-compilation tests |

## How Each Phase Works

### 1. Lexical Analysis (Scanner)

```forma
# Create scanner from source
s := scanner_new("f main() -> Int = 42")

# Scan all tokens
tokens := scan_all(source)
```

The scanner produces a list of `Token` values, each containing:
- `kind` - Token type (integer discriminant)
- `lexeme` - Source text
- `span` - Location information

### 2. Parsing

```forma
# Parse tokens into AST
ast := parse(tokens)
```

The parser produces an `AST` containing:
- `items` - Top-level declarations (functions, structs, enums)
- `exprs` - Expression nodes
- `types` - Type nodes

### 3. Type Checking

```forma
# Type check the AST
(ctx, ok) := type_check(ast)
```

Returns a `TypeContext` with inferred types and a success flag.

### 4. MIR Lowering

```forma
# Lower AST to MIR
program := lower_program(ast)
```

Produces a `Program` containing:
- `functions` - List of MIR functions
- `entry` - Index of main function

Each function has:
- `blocks` - Basic blocks with statements
- `locals` - Variable declarations

### 5. Interpretation

```forma
# Execute MIR program
result := interpret(program)
```

The interpreter:
- Creates stack frames for function calls
- Evaluates statements and expressions
- Handles control flow (if/else, loops, returns)
- Supports recursion

## Running the Bootstrap Compiler

### Run Tests

```bash
# Run MIR interpreter tests
cargo run -- run bootstrap/forma_bootstrap.forma

# Run unit tests
cargo run -- run bootstrap/test_bootstrap.forma

# Run self-hosting tests
cargo run -- run bootstrap/test_self_host.forma

# Run lexer tests
cargo run -- run bootstrap/lexer_v2_combined.forma
```

### Example Output

```
=== FORMA Bootstrap Pipeline Tests ===

Test 1: return 42
  PASS
  42
Test 2: 2 + 3 * 4 = 14
  PASS
  14
Test 3: add(10, 20) = 30
  PASS
  30
Test 4: factorial(5) = 120
  PASS
  120

All tests passed!
```

## Known Limitations Compared with the Rust Compiler

| Feature | Bootstrap | Rust Compiler |
|---------|-----------|---------------|
| Type inference | Basic | Full Hindley-Milner |
| Generics | Not yet | Full support |
| Enums | Integer discriminants | Full ADTs |
| Methods | Flattened to functions | Full dispatch |
| Pattern matching | Integer literals only | Full patterns |
| Collections | Via built-ins | Native arrays |
| File I/O | Via built-ins | Native support |
| Error messages | Basic | Rich diagnostics |

## Architecture Decisions

### Integer Discriminants for Variants

Since FORMA enums require complex pattern matching, we use integer discriminants:

```forma
f TK_IDENT() -> Int = 100
f TK_INT() -> Int = 101

s Token
    kind: Int  # Use TK_* constants
    lexeme: Str
```

### Tuple-Based State Threading

Functions that modify state return tuples:

```forma
f scan_token(s: Scanner) -> (Scanner, Token)
f lower_expr(ast: AST, ctx: Ctx) -> (Ctx, Int)
```

### Index-Based References

AST nodes reference other nodes by index into arrays:

```forma
s ExprNode
    kind: Int
    left: Int   # Index into exprs array
    right: Int  # Index into exprs array
```

## Self-Hosting Status

The bootstrap compiler can:

- [x] Tokenize FORMA source files
- [x] Parse FORMA code samples
- [x] Build MIR from AST representations
- [x] Interpret MIR programs
- [x] Handle recursion and function calls
- [ ] Full source-to-execution pipeline
- [ ] Compile itself completely

Do not use this directory as syntax or semantic documentation for agents. Use
`docs/ai-reference.md` and the generated grammar/builtin metadata instead.

## Future Work

1. **Complete Pipeline Integration** - Connect all phases end-to-end
2. **Better Error Handling** - Add Result types and error propagation
3. **Optimization** - Add MIR optimization passes
4. **Code Generation** - Generate native code or WASM
5. **Full Self-Hosting** - Compile the compiler with itself
