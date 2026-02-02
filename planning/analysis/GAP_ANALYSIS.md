# FORMA Gap Analysis: Path to Production

**Date:** January 24, 2026
**Goal:** Identify what's missing for FORMA to be a language people can use for real projects.

---

## Executive Summary

FORMA has a solid foundation but needs work in several areas before it's production-ready. The gaps fall into four categories:

| Category | Current State | Production Ready |
|----------|--------------|------------------|
| **Core Language** | 85% | Needs: integer types, float ops, traits |
| **Runtime/Stdlib** | 60% | Needs: networking, async, time, JSON |
| **Tooling** | 70% | Needs: LSP, debugger, formatter |
| **Ecosystem** | 20% | Needs: package registry, FFI, docs |

---

## 1. LANGUAGE FEATURE GAPS

### 1.1 Critical (Blocks Real Use)

| Gap | Description | Effort |
|-----|-------------|--------|
| **Integer Types** | Only `Int` (i64) exists. Need u8, u16, u32, i8, i16, i32, usize | Medium |
| **Float Operations** | Limited float support. Need full math (sin, cos, sqrt, etc.) | Small |
| **Trait Implementation** | Traits parse but aren't fully implemented in type checker | Large |
| **Range Iteration** | `for i in 0..10` doesn't work in interpreter | Small |
| **String Formatting** | No format strings or interpolation | Medium |
| **Error Context** | `?` operator works but no `.context()` chaining | Small |

### 1.2 Important (Limits Usability)

| Gap | Description | Effort |
|-----|-------------|--------|
| **Associated Types** | Trait associated types not implemented | Medium |
| **Default Parameters** | Parsed but not evaluated | Small |
| **Destructuring** | Limited pattern matching in let bindings | Medium |
| **Const Evaluation** | Constants not evaluated at compile time | Medium |
| **impl Blocks** | Methods on types not fully working | Medium |

### 1.3 Nice to Have

| Gap | Description | Effort |
|-----|-------------|--------|
| **Macros** | No macro system | Large |
| **Const Generics** | Parsed but not implemented | Large |
| **GATs** | No generic associated types | Large |
| **Operator Overloading** | No custom operators | Medium |

---

## 2. RUNTIME / STDLIB GAPS

### 2.1 Critical (Can't Build Real Apps Without)

| Gap | Current | Needed | Effort |
|-----|---------|--------|--------|
| **Networking** | ❌ None | TCP/UDP sockets, HTTP client | Large |
| **Async/Await** | ❌ Parsed only | Actual async runtime | Very Large |
| **JSON** | ❌ None | Parse/serialize JSON | Medium |
| **Time/Date** | ❌ None | Timestamps, duration, formatting | Medium |
| **Threads** | ❌ None | Spawn, join, channels | Large |
| **Random** | ❌ None | Random number generation | Small |

### 2.2 Important

| Gap | Current | Needed | Effort |
|-----|---------|--------|--------|
| **Regex** | ❌ None | Pattern matching | Medium |
| **Path Handling** | Basic | Cross-platform paths | Small |
| **Process** | ❌ None | Spawn processes, exec | Medium |
| **Hashing** | ❌ None | SHA, MD5, etc. | Small |
| **Base64** | ❌ None | Encode/decode | Small |
| **Compression** | ❌ None | gzip, zlib | Medium |

### 2.3 What Currently Exists

**Working in interpreter:**
- `print`, `println`
- `vec_*` (new, len, get, push, pop, set, concat, slice, reverse)
- `str_*` (len, char_at, slice, contains, starts_with, ends_with, split, trim, replace_all, concat)
- `map_*` (new, len, get, insert, remove, contains, keys, values)
- `char_*` (is_digit, is_alpha, is_alphanumeric, is_whitespace, to_int, to_str)
- `file_*` (read, write, exists, append)
- `args`, `env_get`
- `type_of`, `panic`, `assert`
- Option/Result helpers (`unwrap`, `expect`, `unwrap_or`, `is_some`, `is_none`, `is_ok`, `is_err`)

---

## 3. TOOLING GAPS

### 3.1 Critical (Expected by All Users)

| Gap | Current | Needed | Effort |
|-----|---------|--------|--------|
| **LSP** | ❌ None | Language server for editors | Large |
| **Formatter** | ❌ None | `forma fmt` | Medium |
| **Linter** | ❌ None | `forma lint` | Medium |
| **REPL** | ❌ None | Interactive shell | Medium |
| **Debugger** | ❌ None | Breakpoints, stepping | Very Large |

### 3.2 Important

| Gap | Current | Needed | Effort |
|-----|---------|--------|--------|
| **Test Framework** | Basic | `forma test` with assertions, fixtures | Medium |
| **Benchmarking** | ❌ None | `forma bench` | Medium |
| **Doc Generation** | ❌ None | `forma doc` → HTML | Medium |
| **Coverage** | ❌ None | Code coverage reports | Medium |
| **Profiler** | ❌ None | Performance profiling | Large |

### 3.3 What Currently Exists

**Working:**
- `forma run` - interpreter execution
- `forma build` - LLVM compilation
- `forma check` - type checking
- `forma check --partial` - partial validation
- `forma lex` / `forma parse` - debugging
- `forma grammar` - grammar export (EBNF, JSON)
- `forma complete` - completion suggestions
- `forma typeof` - type at position
- `forma new` / `forma init` - project scaffolding
- JSON error output for AI tooling

---

## 4. ECOSYSTEM GAPS

### 4.1 Critical

| Gap | Current | Needed | Effort |
|-----|---------|--------|--------|
| **Package Registry** | ❌ None | crates.io equivalent | Very Large |
| **FFI/C Interop** | ❌ None | Call C libraries | Large |
| **Build System** | Basic | Dependency resolution | Large |
| **Versioning** | ❌ None | Semver in forma.toml | Small |

### 4.2 Important

| Gap | Current | Needed | Effort |
|-----|---------|--------|--------|
| **Editor Plugins** | ❌ None | VS Code, Vim, Emacs | Medium |
| **Syntax Highlighting** | ❌ None | TextMate grammar | Small |
| **CI Templates** | ❌ None | GitHub Actions, etc. | Small |
| **Docker Image** | ❌ None | Official forma image | Small |

### 4.3 What Currently Exists

**Working:**
- `forma.toml` project config (basic)
- Module system (`us stdlib.core`)
- Standard library (Vec, Map, String, Iterator, Core)

---

## 5. DOCUMENTATION GAPS

| Gap | Current | Needed |
|-----|---------|--------|
| **Language Tour** | ❌ None | 30-min interactive tutorial |
| **Language Reference** | ❌ None | Complete syntax/semantics |
| **Stdlib Reference** | ❌ None | API docs for all functions |
| **AI Integration Guide** | ❌ None | How to use with LLMs |
| **Error Index** | ❌ None | All error codes explained |
| **Migration Guide** | ❌ None | From Rust/Go to FORMA |
| **Examples Repo** | Basic | Real-world examples |
| **Cookbook** | ❌ None | Common patterns/recipes |

---

## 6. PRIORITIZED ROADMAP

### Phase 1: Minimum Viable Language (4-6 weeks)
*Goal: People can write CLI tools and scripts*

1. **Integer types** (u8, i32, usize, etc.)
2. **Range iteration** (`for i in 0..10`)
3. **String formatting** (`"Hello {name}"`)
4. **JSON parsing** (built-in or stdlib)
5. **Random numbers**
6. **Time/Duration**
7. **Basic LSP** (errors, go-to-definition)
8. **Formatter** (`forma fmt`)
9. **Language Tour** documentation

### Phase 2: Practical Language (2-3 months)
*Goal: People can build HTTP APIs and tools*

1. **Networking** (TCP sockets, HTTP client)
2. **Async/Await runtime**
3. **Threads and channels**
4. **FFI/C interop** (call libc, link C libraries)
5. **Full trait implementation**
6. **Regex**
7. **Process spawning**
8. **Test framework** (`forma test`)
9. **Full LSP** (completion, hover, rename)
10. **Syntax highlighting** (VS Code)

### Phase 3: Production Language (3-6 months)
*Goal: Companies can use FORMA for real systems*

1. **Package registry** (forma.io or similar)
2. **Debugger**
3. **Complete stdlib** (crypto, compression, etc.)
4. **Performance parity with Rust**
5. **Security audit**
6. **Stability guarantees**
7. **Full documentation**
8. **Multiple platform targets**

---

## 7. QUICK WINS (Can Do Now)

These are small gaps that provide outsized value:

| Gap | Effort | Impact |
|-----|--------|--------|
| Range iteration | 1-2 hours | High - very common pattern |
| Random numbers | 2-3 hours | Medium - needed for many apps |
| String interpolation | 4-6 hours | High - QoL improvement |
| Syntax highlighting | 2-3 hours | High - first editor experience |
| REPL | 1-2 days | Medium - great for learning |
| `forma fmt` | 2-3 days | High - expected by everyone |

---

## 8. COMPETITIVE ANALYSIS

What do people expect from a systems language in 2026?

| Feature | Rust | Go | Zig | FORMA |
|---------|------|----|----|-------|
| Memory safety | ✅ | ✅ (GC) | ✅ | ✅ |
| Package manager | ✅ cargo | ✅ go mod | ✅ zig | ⚠️ Basic |
| LSP | ✅ | ✅ | ✅ | ❌ |
| Formatter | ✅ rustfmt | ✅ gofmt | ✅ | ❌ |
| Debugger | ✅ | ✅ | ✅ | ❌ |
| Async | ✅ | ✅ goroutines | ✅ | ❌ |
| FFI | ✅ | ✅ cgo | ✅ | ❌ |
| HTTP stdlib | ⚠️ (crate) | ✅ | ⚠️ | ❌ |
| JSON stdlib | ⚠️ (crate) | ✅ | ⚠️ | ❌ |
| WebAssembly | ✅ | ✅ | ✅ | ❌ |
| REPL | ❌ | ❌ | ❌ | ❌ |
| AI-first tooling | ❌ | ❌ | ❌ | ✅ |

**FORMA's unique advantage:** AI-first tooling (grammar export, structured errors, type-constrained decoding). No other systems language has this.

---

## 9. RECOMMENDATIONS

### For Immediate Focus (Next 2 weeks)
1. Fix range iteration (quick win, high impact)
2. Add random number generation
3. Create basic LSP (errors only)
4. Write Language Tour
5. Create VS Code syntax highlighting

### For Short-Term (Next month)
1. Integer type system
2. String formatting/interpolation
3. JSON support
4. Formatter (`forma fmt`)
5. Time/Duration

### For Medium-Term (Next quarter)
1. Async/await runtime
2. Networking (HTTP client)
3. FFI/C interop
4. Full LSP
5. Package registry planning

---

## 10. CONCLUSION

FORMA is approximately **60% of the way to a usable language**. The core compiler is solid, but the ecosystem around it needs significant work.

**The critical path is:**
1. Basic tooling (LSP, formatter)
2. Essential stdlib (JSON, time, networking)
3. FFI (access existing C libraries)
4. Package manager (share code)

With focused effort, FORMA could reach "usable for real projects" status in 2-3 months.

The AI-first tooling is a genuine differentiator that no other systems language offers. This should be emphasized in marketing while the ecosystem catches up.

---

*"A language is only as good as its ecosystem."*
