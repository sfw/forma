# FORMA v1.2 Known Limitations

**Last Updated:** January 26, 2026
**Status:** Post-Sprint 16 - v1.2 FEATURE COMPLETE

This document lists known limitations in FORMA v1.2, categorized by severity and planned resolution timeline.

---

## Summary

| Category | Fixed | Remaining | Severity |
|----------|-------|-----------|----------|
| Language Features | 5 | 0 | N/A |
| Type System | 5 | 1 | Research |
| Tooling | 6 | 0 | N/A |
| Standard Library | 4 | 0 | N/A |
| Parser/Lexer | 3 | 0 | N/A |

**Total: 24 issues fixed, 1 remaining (research priority - v2.0)**

---

## Recently Fixed (Sprint 14-16)

### Sprint 16 (Just Completed - v1.2)

| Issue | Task | Resolution |
|-------|------|------------|
| True async parallelism | 16.1 | Tokio runtime, TokioTask value, real async spawn/await |
| LLVM indirect closures | 16.2 | Fat pointer {fn_ptr, env_ptr}, compile_closure method |
| Loop labels | 16.3 | `'label: for` syntax, break/continue by label |
| Iterator encoding hack | 16.4 | EnumeratedInt struct with index/value fields |
| Multiline expressions | 16.5 | Trailing operator detection, proper DEDENT tracking |
| Grammar export gaps | 16.6 | Shorthand keywords, indentation rules, operator precedence |

### Sprint 15

| Issue | Task | Resolution |
|-------|------|------------|
| Multi-error reporting | 15.1 | Parser now returns `Vec<CompileError>`, shows all errors |
| REPL type display | 15.2 | `:type` command shows actual inferred types |
| LSP go-to-definition | 15.3 | Jump to function/struct/enum definitions working |
| Trait implementation checking | 15.4 | Validates required methods and signatures |
| Enum pattern validation | 15.5 | Validates variant names at compile time |
| Formatter completeness | 15.6 | All expression/pattern/type kinds handled |
| Struct update syntax | 15.7 | `{ ..base, field: value }` working in MIR |

### Sprint 14-14.7

| Issue | Sprint | Resolution |
|-------|--------|------------|
| Enum discriminant hash collision | 14 | Index-based registry |
| Call stack unwrap panics | 14.7 | Helper methods with Result |
| MIR lowerer unwrap panics | 14.7 | Helper methods with Result |
| Borrow checker unwrap panics | 14.7 | Helper methods with Result |
| String/Bool/Char/Float pattern matching | 14.7 | Added literal handlers in lower_match() |
| Import system silent failures | 14.6 | Returns ModuleError, searches std/ |
| JSON type mapping | 14.5 | Added "Json" => Ty::Json |
| Async example broken | 14.5 | Fixed vec_push reassignment |
| Contextual keyword conflicts (m/s/f/e/t/i) | 12 | Context-aware parser lookahead |

---

## Remaining Limitations

### Type System

#### 1. Higher-Kinded Types (Not Planned)
**Status:** Not supported

Higher-kinded types are not supported and not planned for v1.x.

```forma
# Not supported:
f map_functor[F[_], A, B](fa: F[A], f: (A) -> B) -> F[B]
```

**Rationale:** Complexity vs. benefit for AI code generation.

**Planned:** v2.0 (maybe)

---

## Items Fixed and Verified

| Item | Status | Sprint |
|------|--------|--------|
| True async parallelism | ✅ Fixed | 16.1 |
| LLVM indirect closures | ✅ Fixed | 16.2 |
| Loop labels | ✅ Fixed | 16.3 |
| Iterator encoding hack | ✅ Fixed | 16.4 |
| Multiline expressions | ✅ Fixed | 16.5 |
| Grammar export gaps | ✅ Fixed | 16.6 |
| Multi-error reporting | ✅ Fixed | 15.1 |
| REPL type display | ✅ Fixed | 15.2 |
| LSP go-to-definition | ✅ Fixed | 15.3 |
| Trait implementation checking | ✅ Fixed | 15.4 |
| Enum pattern validation | ✅ Fixed | 15.5 |
| Formatter completeness | ✅ Fixed | 15.6 |
| Struct update syntax | ✅ Fixed | 15.7 |
| Enum discriminant collisions | ✅ Fixed | 14 |
| String pattern matching | ✅ Fixed | 14.7 |
| Import system | ✅ Working | 14.6 |
| JSON stdlib | ✅ Working | 14.5 |
| Async example | ✅ Working | 14.5 |
| Method/field type checking | ✅ Working | 9 |
| Option/Result unification | ✅ Working | 9 |
| Duration builtins | ✅ Implemented | 13 |
| pow() negative check | ✅ Fixed | 13 |
| Call stack safety | ✅ Fixed | 14.7 |
| Contextual keywords | ✅ Fixed | 12 |

---

## Version Roadmap

### v1.0 (January 2026)
- ✅ 288 tests passing (250 Rust + 38 FORMA)
- ✅ All critical issues resolved
- ✅ Production-ready interpreter
- ✅ Full type inference
- ✅ Complete stdlib

### v1.1 (January 2026) - COMPLETE
- ✅ True async parallelism (Tokio)
- ✅ Indirect closure calls (LLVM)

### v1.2 (CURRENT - January 2026) - COMPLETE
- ✅ Loop labels
- ✅ Proper tuple iteration
- ✅ Multiline expression improvements
- ✅ Grammar export completeness

### v2.0 (Future)
- Higher-kinded types (research)

---

## Test Results

```
Rust unit tests:     250 passing
All std/*.forma:     Verified
All examples/*.forma: Verified
```

---

*"v1.2: Feature complete. All planned v1.x features implemented."*
