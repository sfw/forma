# FORMA Changelog

## v1.0.0 (January 2026)

### Language Features
- Full type inference with Hindley-Milner algorithm
- Structs, enums, traits, and impl blocks
- Pattern matching with exhaustiveness checking
- Async/await with spawn (`sp`) and `await_all`
- Contracts with `@pre()` and `@post()` conditions
- Generic types and functions
- Integer types: i8-i64, u8-u64, isize, usize
- Float types: f32, f64
- String interpolation with f-strings
- Contextual keywords (f, s, e, t, i, m work as identifiers)

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
- Contract enforcement at runtime

### AI-Optimized Design
- Token-efficient syntax (short keywords)
- Strong type inference (minimal annotations)
- Contextual keywords (m, s, f, e, t, i work as identifiers)
- Machine-readable error output (JSON format)

### Test Coverage
- 36 Rust unit tests
- 35 FORMA integration tests
- 11 example programs
- 7 stdlib modules
