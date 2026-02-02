# FORMA Self-Hosting Implementation Plan (Expanded)

## Code Review Assessment

After reviewing the compiler implementation, I can confirm the self-hosting plan is well-founded. Here's my assessment of the current state:

### Current Implementation Quality: **B+**

| Component | Lines | Quality | Notes |
|-----------|-------|---------|-------|
| Lexer | ~30KB | Excellent | Proper indentation handling with INDENT/DEDENT |
| Parser | ~91KB | Excellent | Comprehensive AST, handles all v2 syntax |
| Type System | ~68KB | Good | Has H-M infrastructure but needs MIR integration |
| MIR | ~15KB | Good | Clean CFG design |
| Interpreter | ~25KB | Good | Works for basic programs |
| Borrow Checker | ~36KB | Unknown | Needs integration testing |

### Confirmed Critical Gaps

1. **`lower_type()` at line 924-927** - Returns `Ty::Int` for ALL types
2. **No `Value::Enum` in interpreter** - Only structs are supported
3. **No enum MIR representation** - `Rvalue::Enum` doesn't exist
4. **Pattern matching only handles integer literals**
5. **Methods are flattened** - No proper receiver handling

---

## Expanded Phase 0: Foundation Fixes

### 0.1 Fix Type Lowering (Priority: CRITICAL)

**Current Code (src/mir/lower.rs:924-927):**
```rust
fn lower_type(&self, _ty: &crate::parser::Type) -> Ty {
    // TODO: proper type lowering
    Ty::Int
}
```

**Required Implementation:**
```rust
fn lower_type(&self, ty: &crate::parser::Type) -> Ty {
    match &ty.kind {
        TypeKind::Path(path) => {
            let name = &path.segments[0].name.name;
            let args = path.segments[0].args.as_ref()
                .map(|a| a.args.iter().filter_map(|arg| {
                    match arg {
                        GenericArg::Type(t) => Some(self.lower_type(t)),
                        _ => None,
                    }
                }).collect())
                .unwrap_or_default();

            match name.as_str() {
                "Int" => Ty::Int,
                "Bool" => Ty::Bool,
                "Str" => Ty::Str,
                "Float" => Ty::Float,
                "Char" => Ty::Char,
                _ => Ty::Named(TypeId::new(name), args),
            }
        }
        TypeKind::List(inner) => Ty::List(Box::new(self.lower_type(inner))),
        TypeKind::Option(inner) => Ty::Option(Box::new(self.lower_type(inner))),
        TypeKind::Result(ok, err) => Ty::Result(
            Box::new(self.lower_type(ok)),
            Box::new(err.as_ref().map(|e| self.lower_type(e)).unwrap_or(Ty::Str)),
        ),
        TypeKind::Tuple(tys) => Ty::Tuple(tys.iter().map(|t| self.lower_type(t)).collect()),
        TypeKind::Ref(inner, mutable) => Ty::Ref(
            Box::new(self.lower_type(inner)),
            if *mutable { Mutability::Mutable } else { Mutability::Immutable },
        ),
        TypeKind::Fn(params, ret) => Ty::Fn(
            params.iter().map(|t| self.lower_type(t)).collect(),
            Box::new(self.lower_type(ret)),
        ),
        TypeKind::Infer => Ty::fresh_var(),
        TypeKind::Never => Ty::Never,
        _ => Ty::Error,
    }
}
```

**Test Case:**
```forma
f identity[T](x: T) -> T = x
f main() -> Int = identity(42)
```

### 0.2 Add Enum to MIR (Priority: CRITICAL)

**Add to src/mir/mir.rs:**
```rust
// In Rvalue enum, add:
/// Enum variant construction
Enum {
    type_name: String,
    variant: String,
    fields: Vec<Operand>,
},

// In Constant enum, add:
/// Enum variant (for unit variants)
EnumVariant(String, String),  // (type_name, variant_name)
```

**Add to src/mir/interp.rs:**
```rust
// In Value enum, add:
Enum {
    type_name: String,
    variant: String,
    fields: Vec<Value>,
},
```

### 0.3 Enum Pattern Matching (Priority: CRITICAL)

The current `lower_match()` only handles integer literals. Need to extend for enum patterns.

**Key insight:** Enum matching can be lowered to a discriminant switch followed by field extraction.

**Implementation approach:**
1. Each enum variant gets a unique discriminant (0, 1, 2, ...)
2. Switch on discriminant
3. In each branch, extract fields into locals

**Pseudocode:**
```
match opt {
    Some(x) => x + 1,
    None => 0,
}

// Becomes:
_disc = discriminant(opt)
switch _disc {
    0 => {  // Some
        x = extract_field(opt, 0)
        _result = x + 1
        goto exit
    }
    1 => {  // None
        _result = 0
        goto exit
    }
}
exit:
    return _result
```

---

## Expanded Phase 1: Core Data Structures

### 1.1 Vec<T> Implementation Strategy

**Option A: Built-in with Rust backing (Recommended for bootstrap)**
```rust
// In interpreter, handle these function names specially:
"vec_new" => Value::Array(vec![]),
"vec_push" => {
    if let Value::Ref(inner) = &args[0] {
        if let Value::Array(arr) = inner.as_ref() {
            // Mutation through reference...
        }
    }
}
```

**Challenge:** FORMA's second-class references mean we can't store `&mut Vec` - we need to work around this.

**Solution:** Interpret `vec_push(&mut v, x)` as a special form that mutates the local directly.

### 1.2 String Operations - Detailed API

| Function | Signature | Implementation |
|----------|-----------|----------------|
| `str_len` | `(&Str) -> Int` | `s.len()` |
| `str_char_at` | `(&Str, Int) -> Char?` | `s.chars().nth(i)` |
| `str_slice` | `(&Str, Int, Int) -> Str` | `s[start..end]` |
| `str_contains` | `(&Str, &Str) -> Bool` | `s.contains(sub)` |
| `str_starts_with` | `(&Str, &Str) -> Bool` | `s.starts_with(prefix)` |
| `str_split` | `(&Str, &Str) -> [Str]` | `s.split(delim)` |
| `str_trim` | `(&Str) -> Str` | `s.trim()` |
| `str_to_int` | `(&Str) -> Int?` | `s.parse()` |
| `int_to_str` | `(Int) -> Str` | `n.to_string()` |
| `char_is_digit` | `(Char) -> Bool` | `c.is_ascii_digit()` |
| `char_is_alpha` | `(Char) -> Bool` | `c.is_alphabetic()` |
| `char_is_whitespace` | `(Char) -> Bool` | `c.is_whitespace()` |

---

## NEW: Phase 0.5 - Testing Infrastructure

Before proceeding to Phase 1, establish a comprehensive test suite:

### Snapshot Tests for MIR Output
```rust
#[test]
fn test_mir_enum_lowering() {
    let source = r#"
e Color = Red | Green | Blue
f main() -> Color = Red
"#;
    let mir = compile_to_mir(source);
    insta::assert_snapshot!(mir);
}
```

### Property-Based Tests
```rust
#[test]
fn property_arithmetic_correct() {
    // For any two integers, FORMA's + should match Rust's +
    proptest!(|(a: i64, b: i64)| {
        let result = run_forma(&format!("f main() -> Int = {} + {}", a, b));
        prop_assert_eq!(result, Value::Int(a.wrapping_add(b)));
    });
}
```

---

## NEW: Phase 2.5 - Closures and Higher-Order Functions

The self-hosting compiler will need `map`, `filter`, etc. This requires closures.

### Current State
- Parser handles closures (`Closure` in AST)
- No MIR representation

### Required Implementation

**Add to MIR:**
```rust
/// A closure value capturing locals
Rvalue::Closure {
    fn_id: String,      // Generated function ID
    captures: Vec<Local>,
}

Value::Closure {
    fn_name: String,
    env: HashMap<Local, Value>,
}
```

**Lowering:**
1. Extract closure body into a new function
2. Add captured variables as parameters
3. At call site, pass captured values

**Example:**
```forma
f main() -> Int
    x = 10
    add_x = |y| y + x   # Captures x
    add_x(5)            # Returns 15
```

**Becomes:**
```
fn __closure_0(x: Int, y: Int) -> Int = y + x

fn main() -> Int
    x = 10
    closure = Closure(__closure_0, [x])
    call closure(5)
```

---

## NEW: Phase 3.5 - Error Messages in FORMA

For a usable bootstrap compiler, error messages need to be readable.

### Minimum Required
```forma
s CompileError
    message: Str
    file: Str
    line: Int
    column: Int

f format_error(e: &CompileError) -> Str
    "{e.file}:{e.line}:{e.column}: error: {e.message}"
```

### Error Catalog for Bootstrap
| Code | Category | Example |
|------|----------|---------|
| E0001 | Parse | Unexpected token |
| E0002 | Parse | Missing closing delimiter |
| E0100 | Type | Type mismatch |
| E0101 | Type | Unknown type |
| E0200 | Borrow | Cannot borrow as mutable |
| E0201 | Borrow | Value moved |

---

## Revised Timeline with Milestones

| Phase | Duration | Milestone | Verification |
|-------|----------|-----------|--------------|
| 0.1 | 1 week | Type lowering | `f id[T](x: T) -> T = x` works |
| 0.2 | 1 week | Enum MIR | `e Color = Red` works |
| 0.3 | 1 week | Pattern matching | `m opt { Some(x) => x, None => 0 }` works |
| 0.5 | 1 week | Test infrastructure | 90%+ coverage on core paths |
| 1.1 | 2 weeks | Vec built-in | Can build and iterate lists |
| 1.2 | 1 week | String ops | Lexer can be written |
| 1.3 | 1 week | HashMap | Symbol tables work |
| 2.0 | 2 weeks | Method dispatch | `v.push(x)` works |
| 2.5 | 2 weeks | Closures | `items.map(\|x\| x + 1)` works |
| 3.0 | 2 weeks | For loops | `for x in items` works |
| 3.5 | 1 week | Error handling | Readable error messages |
| 4.0 | 1 week | Option/Result | `?` operator works |
| 5.0 | 2 weeks | File I/O | Can read source files |
| 6.0 | 3 weeks | Stdlib | Core utilities in FORMA |
| 7.0 | 10 weeks | Bootstrap compiler | Self-compiles! |

**Total: 28-32 weeks (7-8 months)**

---

## Answering the Open Questions

### 1. Box<T> vs Vector Indices

**Recommendation: Vector indices with arena allocation**

Reason: FORMA's second-class references prevent storing `Box<T>` in structs naturally. Instead:

```forma
s Ast
    nodes: [AstNode]  # All nodes stored here
    root: Int         # Index into nodes

s AstNode
    kind: AstNodeKind
    children: [Int]   # Indices into parent Ast.nodes
```

This is actually common in real compilers (rustc uses this pattern).

### 2. Static vs Dynamic Dispatch

**Recommendation: Static dispatch only for bootstrap**

Reason: Dynamic dispatch (trait objects) requires vtables and runtime type information, which significantly complicates the implementation. Static dispatch through monomorphization is simpler and sufficient for a bootstrap compiler.

### 3. Error Recovery

**Recommendation: Fail-fast for bootstrap**

Reason: Error recovery is complex and not essential for bootstrap. A working compiler that reports the first error clearly is more valuable than one that attempts to continue and produces confusing cascading errors.

### 4. Incremental Compilation

**Recommendation: Design for it, don't implement it**

Approach:
- Keep the architecture modular (separate parsing, type checking, MIR lowering)
- Use content hashing for source files
- But don't implement caching in the bootstrap compiler

---

## Risk Mitigation Strategies

### Risk: Generic Type Erasure Harder Than Expected

**Mitigation:**
1. Start with monomorphization (copy-paste specialized versions)
2. Only support a fixed set of instantiations initially
3. Add proper generic support incrementally

### Risk: Second-Class References Break Patterns

**Mitigation:**
1. Use builder patterns that return new values
2. Use indices instead of pointers
3. Strategic use of `&mut` parameters for in-place modification

### Risk: Bootstrap Compiler Too Slow

**Mitigation:**
1. Profile early, optimize hot paths
2. Consider bytecode compilation instead of interpretation
3. Parallelize independent compilation units

---

## Recommended Next Steps

1. **Immediate (this week):** Fix `lower_type()` - it's blocking everything
2. **Week 2:** Add enum support to MIR and interpreter
3. **Week 3:** Pattern matching for enums
4. **Week 4:** Set up comprehensive test suite with snapshots
5. **Month 2:** Core data structures (Vec, String ops, HashMap)
6. **Month 3:** Methods and closures

The self-hosting plan is solid. The main risk is underestimating Phase 0 - getting the foundations right is critical.
