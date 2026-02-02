# FORMA Self-Hosting Implementation Plan

## Executive Summary

This document outlines a comprehensive plan to make FORMA self-hosting — capable of compiling itself. The target is a **MIR + Rust Interpreter hybrid bootstrap**, where an FORMA-written compiler generates MIR that the existing Rust interpreter executes.

**Estimated timeline: 6-9 months**

---

## Rationale

### Why Self-Hosting Matters
1. **Dogfooding** — Writing a compiler in FORMA will stress-test the language and expose design issues
2. **Language maturity proof** — Self-hosting demonstrates the language is capable of real systems programming
3. **Bootstrap independence** — Eventually reduces dependency on Rust toolchain

### Why MIR + Rust Interpreter Target
Three bootstrap paths were considered:

| Target | Complexity | Benefit |
|--------|------------|---------|
| Native codegen (LLVM/Cranelift) | Very High | Full performance |
| Bytecode VM | Medium | Portable, but need VM |
| **MIR + Rust Interpreter** | **Lowest** | **Reuses proven interpreter** |

**Chosen: MIR + Rust Interpreter** because:
- The Rust interpreter already exists and works
- MIR is a well-defined CFG-based IR
- Simplest path to bootstrap — focus on compiler logic, not execution
- Native codegen can be added later (Phase 8+)

---

## Current State Assessment

### What Works Today (MIR/Interpreter)
- Basic types: Int, Float, Bool, Char, Str
- Arithmetic, comparisons, logical operators
- Functions with recursion
- If/else expressions, while loops
- Structs (creation, field access)
- Arrays/Tuples (creation, indexing)
- String literals, concatenation
- Basic pattern matching (integer literals only)
- Built-in: `print()`

### Critical Gaps (Parsed but Not Implemented)

| Feature | Status | Impact |
|---------|--------|--------|
| **Generics** | `lower_type()` returns `Ty::Int` for ALL types | Blocks Vec<T>, Option<T>, Result<T,E> |
| **Enums** | No MIR/interpreter representation | Blocks AST types, Option, Result |
| **Pattern Matching** | Only integer literals | Blocks enum destructuring |
| **Methods** | Flattened names, no dispatch | Blocks OOP-style API design |
| **String operations** | Only `print()` exists | Blocks lexer implementation |
| **Collections** | None | Blocks parser (needs Vec for AST) |
| **File I/O** | None | Blocks reading source files |

### Root Cause Analysis

**The generics problem** traces to a single function in `src/mir/lower.rs:919-922`:

```rust
fn lower_type(&self, _ty: &crate::parser::Type) -> Ty {
    // TODO: proper type lowering
    Ty::Int
}
```

This returns `Ty::Int` for ALL types, completely erasing generic type information. Fixing this is Phase 0 priority #1.

---

## Implementation Phases

### Phase 0: Foundation Fixes (2-3 weeks)
**Priority: HIGHEST — blocks everything else**

#### 0.1 Fix Type Lowering
- Implement proper `lower_type()` that preserves generic type parameters
- Add `Ty::Generic(name)`, `Ty::App(base, args)` to MIR type system
- **Files:** `src/mir/lower.rs`, `src/mir/mir.rs`
- **Test:** `f id[T](x: T) -> T = x` should work with any type

#### 0.2 Enum Runtime Representation
- Add `Rvalue::Enum(type_name, variant, Vec<Operand>)` to MIR
- Add `Value::Enum(type_name, variant, Vec<Value>)` to interpreter
- **Files:** `src/mir/mir.rs`, `src/mir/lower.rs`, `src/mir/interp.rs`
- **Test:** `e Color = Red | Green | Blue` and `f main() -> Color = Red`

#### 0.3 Variant Pattern Matching
- Extend `lower_match()` to handle enum variant patterns
- Support binding extraction (`Some(x) => x`)
- **Files:** `src/mir/lower.rs` (lines 662-735), `src/mir/interp.rs`
- **Test:** `m opt { Some(x) => x, None => default }`

---

### Phase 1: Core Data Structures (3-4 weeks)

#### 1.1 Vec<T> Built-in
Add interpreter built-ins:
- `vec_new[T]() -> [T]`
- `vec_push[T](v: &mut [T], elem: T)`
- `vec_pop[T](v: &mut [T]) -> T?`
- `vec_len[T](v: &[T]) -> Int`
- `vec_get[T](v: &[T], idx: Int) -> T?`
- `vec_set[T](v: &mut [T], idx: Int, val: T)`

#### 1.2 String Operations
Essential for lexer implementation:
- `str_len(s: &Str) -> Int`
- `str_char_at(s: &Str, idx: Int) -> Char?`
- `str_slice(s: &Str, start: Int, end: Int) -> Str`
- `str_chars(s: &Str) -> [Char]`
- `char_is_digit(c: Char) -> Bool`
- `char_is_alpha(c: Char) -> Bool`
- `char_is_whitespace(c: Char) -> Bool`
- `char_to_int(c: Char) -> Int`
- `int_to_str(n: Int) -> Str`

#### 1.3 HashMap<K,V> Built-in
For symbol tables and type environments:
- `map_new[K,V]() -> Map[K,V]`
- `map_insert[K,V](m: &mut Map[K,V], key: K, val: V)`
- `map_get[K,V](m: &Map[K,V], key: K) -> V?`
- `map_contains[K,V](m: &Map[K,V], key: K) -> Bool`
- `map_remove[K,V](m: &mut Map[K,V], key: K) -> V?`
- `map_keys[K,V](m: &Map[K,V]) -> [K]`

---

### Phase 2: Method Dispatch (3-4 weeks)

#### 2.1 Fix Method Calls
- Current: `src/mir/lower.rs:325-349` loses receiver type info
- Solution: Resolve methods to qualified `Type::method` names with receiver as first argument

#### 2.2 Basic Trait Support
- Resolve trait method calls to impl methods
- Static dispatch only (no trait objects initially)
- **Files:** `src/mir/lower.rs`, `src/types/inference.rs`

---

### Phase 3: Iteration (2-3 weeks)

#### 3.1 For Loops with Iterators
- Current `lower_for()` is a stub (lines 737-785)
- Desugar to while loop with iterator protocol:
```
for x in items  →  iter := items.iter(); wh let Some(x) = iter.next() { ... }
```

#### 3.2 While-Let
- Implement `wh let pattern = expr` for iterator consumption

---

### Phase 4: Error Handling (2-3 weeks)

#### 4.1 Option<T> and Result<T,E>
- With Phase 0 complete, these are just enums that should work automatically

#### 4.2 Try Operator (?)
- Desugar `expr?` to match with early return on error:
```
expr?  →  m expr { Ok(v) => v, Err(e) => ret Err(e) }
```

---

### Phase 5: File I/O (2 weeks)

#### 5.1 File Built-ins
- `file_read(path: Str) -> Str!IoError`
- `file_write(path: Str, content: Str) -> ()!IoError`
- `file_exists(path: Str) -> Bool`

#### 5.2 CLI Support
- `args() -> [Str]`
- `exit(code: Int) -> !`
- `eprintln(msg: Str)`

---

### Phase 6: Standard Library in FORMA (3-4 weeks)

Write FORMA code for:
- `stdlib/core.forma` — panic, assert, debug utilities
- `stdlib/vec.forma` — Vec wrapper with methods
- `stdlib/string.forma` — StringBuilder, string utilities
- `stdlib/map.forma` — HashMap wrapper with methods

---

### Phase 7: Bootstrap Compiler in FORMA (8-12 weeks)

#### 7.1 Lexer (~1,500 lines)
Rewrite `src/lexer/scanner.rs` in FORMA

#### 7.2 Parser (~3,500 lines)
Rewrite `src/parser/parser.rs` in FORMA
- **Challenge:** Recursive AST types need Box<T> or indices into vectors

#### 7.3 Type Checker (~2,500 lines)
Rewrite type inference in FORMA

#### 7.4 MIR Generator (~3,000 lines)
Generate MIR data structures that the Rust interpreter can execute

#### 7.5 CLI Driver (~500 lines)
Command-line interface matching current `forma run`, `forma check` commands

**Total estimated FORMA code: ~12,000-15,000 lines**

---

## Key Files to Modify

| File | Changes |
|------|---------|
| `src/mir/lower.rs` | Fix `lower_type()`, enum lowering, pattern matching, method dispatch, for loops |
| `src/mir/interp.rs` | Add all built-ins (Vec, String, HashMap, File I/O), enum evaluation |
| `src/mir/mir.rs` | Add `Rvalue::Enum`, extend `Value` type, add generic type representation |
| `src/types/inference.rs` | Ensure generics tracked through inference |

---

## Verification Milestones

### Phase 0 Complete When:
```forma
# Generic identity works
f id[T](x: T) -> T = x
f main() -> Int = id(42)

# Enums work
e Option[T] = Some(T) | None
f main() -> Option[Int] = Some(42)

# Pattern matching works
f unwrap[T](opt: Option[T], default: T) -> T
    m opt
        Some(x) => x
        None => default
```

### Phase 1 Complete When:
```forma
# Vec operations work
f main() -> Int
    v := vec_new()
    vec_push(&mut v, 1)
    vec_push(&mut v, 2)
    vec_len(&v)

# String operations work
f main() -> Int = str_len("hello")
```

### Final Bootstrap Test:
FORMA compiler written in FORMA can compile `examples/factorial.forma` and produce correct output.

---

## Risk Analysis

| Risk | Impact | Mitigation |
|------|--------|------------|
| Generic type erasure harder than expected | High | Phase 0 focuses entirely on this |
| Second-class references limit data structures | Medium | Use index-based iteration, strategic copies |
| Recursive AST types in FORMA | Medium | Implement Box<T> or use vector indices |
| Performance | Low | Acceptable for bootstrap; native compilation comes later |

---

## Timeline Summary

| Phase | Duration | Cumulative |
|-------|----------|------------|
| Phase 0: Foundations | 2-3 weeks | 2-3 weeks |
| Phase 1: Data Structures | 3-4 weeks | 5-7 weeks |
| Phase 2: Methods | 3-4 weeks | 8-11 weeks |
| Phase 3: Iteration | 2-3 weeks | 10-14 weeks |
| Phase 4: Error Handling | 2-3 weeks | 12-17 weeks |
| Phase 5: File I/O | 2 weeks | 14-19 weeks |
| Phase 6: Stdlib | 3-4 weeks | 17-23 weeks |
| Phase 7: Bootstrap | 8-12 weeks | 25-35 weeks |

**Total: 6-9 months**

---

## Open Questions for Discussion

1. **Box<T> vs vector indices** — How should recursive AST types be represented? Box<T> requires heap allocation semantics; vector indices are simpler but change the API.

2. **Trait object support** — Is static dispatch sufficient for bootstrap, or do we need dynamic dispatch (trait objects) for things like AST visitors?

3. **Error recovery** — Should the bootstrap compiler have error recovery for better developer experience, or is fail-fast acceptable?

4. **Incremental compilation** — Not planned for bootstrap, but should the architecture support adding it later?
