# FORMA Changelog

All notable changes to FORMA are documented in this file.

---

## [1.2.0] - January 26, 2026

### 🚀 Feature Complete Release

FORMA v1.2 completes all planned v1.x features, achieving full async parallelism and LLVM closure support.

**250 Rust tests passing** | All std/*.forma and examples/*.forma verified

#### New in v1.2 (Sprint 16)

| Feature | Description |
|---------|-------------|
| **True Async Parallelism** | Tokio runtime integration - `sp` (spawn) now runs truly parallel |
| **LLVM Indirect Closures** | Fat pointer closures work in compiled code |
| **Loop Labels** | `'label: for` with `break 'label` and `continue 'label` |
| **Proper Tuple Iteration** | `EnumeratedInt` struct replaces encoding hack |
| **Multiline Expressions** | Improved newline handling for chained operators |
| **Grammar Export** | Complete EBNF with shorthand keywords and indentation rules |

---

## [1.0.0] - January 26, 2026

### 🎉 Initial Production Release

FORMA v1.0 is the first production-ready release of the AI-optimized programming language.

**288 tests passing** (250 Rust + 38 FORMA integration)

### Core Design Pillars

1. **AI Code Generation First** - No lifetimes, predictable patterns, minimal annotations
2. **Memory Safety Without Lifetimes** - Second-class references, no stored references
3. **Token Efficiency** - Short keywords (`f`, `s`, `e`, `t`, `i`, `m`), concise syntax
4. **Machine-Readable Tooling** - Grammar export, structured JSON errors, type queries
5. **Strong Type Inference** - Hindley-Milner style, AI rarely needs explicit types

### Language Features

- Full type inference with Hindley-Milner algorithm
- Structs (`s Point { x: Int, y: Int }`)
- Enums with associated data (`e Option[T] { Some(T), None }`)
- Traits and implementations (`t Printable`, `i Printable for Point`)
- Pattern matching with `m` (match) expressions
- Struct update syntax (`{ ..base, field: value }`)
- Async/await with spawn (`sp`) and `await_all`
- Contracts with `@pre()` and `@post()` conditions
- Generic types and functions with trait bounds
- Integer types: i8-i128, u8-u128, isize, usize
- Float types: f32, f64
- String interpolation with f-strings (`f"Hello {name}"`)
- Contextual keywords (f, s, e, t, i, m work as identifiers in appropriate contexts)
- Closures (`|x| x + 1`)
- Option (`T?` shorthand) and Result types

### Type System

- Hindley-Milner type inference
- Generic types (`Vec[T]`, `Map[K, V]`)
- Trait bounds on generics
- Method and field type checking
- Struct pattern field validation
- **Enum pattern validation at compile time** (Sprint 15.5)
- **Trait implementation checking** - validates required methods (Sprint 15.4)
- Option/Result unification

### Standard Library (`std/`)

| Module | Description |
|--------|-------------|
| `std/core.forma` | Math utilities (min, max, abs, clamp, pow, gcd, lcm) |
| `std/vec.forma` | Vector operations (push, pop, map, filter, fold, zip) |
| `std/string.forma` | String manipulation (split, trim, replace, contains) |
| `std/json.forma` | JSON parsing, creation, path access |
| `std/io.forma` | File I/O operations |
| `std/iter.forma` | Iterator utilities (enumerate, take, skip) |
| `std/math.forma` | Mathematical functions (sqrt, sin, cos, log) |
| `std/datetime.forma` | Date/time and duration functions |
| `std/prelude.forma` | Auto-imported essentials |

### Tooling

| Command | Description |
|---------|-------------|
| `forma run <file>` | Execute FORMA programs |
| `forma check <file>` | Type check without running |
| `forma fmt <file>` | Code formatter (complete construct support) |
| `forma repl` | Interactive REPL with `:type`, `:ast`, `:help` |
| `forma grammar` | Export EBNF/JSON grammar for external tools |
| `forma lsp` | Language Server Protocol support |

### LSP Features
- Hover information with type display
- Code completion
- **Go-to-definition** (Sprint 15.3)
- Diagnostics with error reporting

### Error Handling
- **Multi-error reporting** - Parser reports ALL errors, not just first (Sprint 15.1)
- Machine-readable JSON error format
- Clear error messages with source locations
- Suggestions for common mistakes

### REPL Features
- `:type <expr>` - **Shows actual inferred type** (Sprint 15.2)
- `:ast <expr>` - Show parsed AST
- `:help` - Command help
- `:quit` - Exit REPL

### Compiler Infrastructure

- Lexer with indentation-based scoping (INDENT/DEDENT tokens)
- Parser with error recovery
- Type checker with full Hindley-Milner inference
- MIR (Mid-level IR) lowering
- Interpreter with 100+ builtins
- LLVM codegen (experimental - closures pending)

---

## Development Sprints

### Sprint 16 - v1.1 + v1.2 Features (Complete)
| Task | Feature |
|------|---------|
| 16.1 | True Async with Tokio (Arc<Runtime>, JoinHandle, block_on) |
| 16.2 | LLVM Indirect Closure Calls (fat pointer {fn_ptr, env_ptr}) |
| 16.3 | Loop Labels (`'label: for` with break/continue support) |
| 16.4 | Proper Tuple Iteration (EnumeratedInt struct) |
| 16.5 | Multiline Expression Improvements (trailing operator handling) |
| 16.6 | Grammar Export Completeness (shorthand keywords, indentation rules) |

### Sprint 15 - Tooling Polish
| Task | Feature |
|------|---------|
| 15.1 | Multi-error reporting - parser returns `Vec<CompileError>` |
| 15.2 | REPL `:type` shows actual inferred types |
| 15.3 | LSP go-to-definition working |
| 15.4 | Trait implementation checking (validates methods & signatures) |
| 15.5 | Enum pattern validation at compile time |
| 15.6 | Formatter handles all constructs (no more `"..."`) |
| 15.7 | Struct update syntax (`{ ..base, field: value }`) |

### Sprint 14.7 - Final Hardening
- Fixed call stack unwrap panics (helper methods with Result)
- Fixed MIR lowerer unwrap panics
- Fixed borrow checker unwrap panics
- String/Bool/Char/Float literal pattern matching in `m` expressions

### Sprint 14.6 - Import System
- Fixed silent import failures (now returns ModuleError)
- Renamed stdlib/ to std/
- Import system with `us std.module` syntax

### Sprint 14.5 - Async & JSON
- Fixed JSON type mapping (`"Json"` => `Ty::Json`)
- Verified async/spawn/await_all functionality
- Fixed async_downloader example

### Sprint 14 - Critical Fixes
- Enum discriminant index-based registry (eliminates hash collisions)
- Type system improvements

### Sprint 12-13 - Language Features
- Contextual keyword parsing (m/s/f/e/t/i work as identifiers)
- Duration builtins
- pow() negative exponent handling

### Sprint 9-11 - Type Safety
- Method/field type checking
- Option/Result unification
- Struct pattern validation

---

## Known Limitations

See [KNOWN_LIMITATIONS.md](KNOWN_LIMITATIONS.md) for complete details.

**v1.2 Status: All v1.x features complete!**

| Item | Status | Notes |
|------|--------|-------|
| True async parallelism | ✅ Fixed | Sprint 16.1 - Tokio integration |
| Indirect closure calls (LLVM) | ✅ Fixed | Sprint 16.2 - Fat pointer closures |
| Loop labels | ✅ Fixed | Sprint 16.3 - `'label: for` syntax |
| Iterator encoding hack | ✅ Fixed | Sprint 16.4 - EnumeratedInt struct |
| Multiline expression edge cases | ✅ Fixed | Sprint 16.5 - Trailing operator handling |
| Grammar export gaps | ✅ Fixed | Sprint 16.6 - Complete EBNF |
| Higher-kinded types | Research | v2.0 (future) |

---

## Upgrading

### From Pre-release

1. Rename `stdlib/` imports to `std/`:
   ```forma
   # Old
   us stdlib.json

   # New
   us std.json
   ```

2. Struct update syntax now works:
   ```forma
   new_point := { ..old_point, x: 10 }
   ```

3. Multiple parse errors are now reported at once.

---

## Contributors

- Language design and implementation by the FORMA team
- Code review and testing assistance by Claude

---

*"Code that writes itself correctly."*
