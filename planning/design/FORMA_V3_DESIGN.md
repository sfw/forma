# FORMA v3: The Definitive AI-First Systems Language

## Design Philosophy

We're not building "a language that AI can use." We're building **the language that makes AI-generated code provably correct, maximally efficient, and impossible to get wrong.**

Every design decision must pass this test:
> "Does this make it harder or easier for an AI to generate correct code?"

If a feature adds complexity that AI will get wrong, we cut it or redesign it.

---

## Core Principles

### 1. **Correctness by Construction**
The language should make it impossible to express incorrect programs, not just catch errors after the fact.

### 2. **Minimal Concept Count**
Every concept an AI must learn is a potential failure point. Fewer concepts = higher accuracy.

### 3. **Unambiguous Syntax**
No context-dependent parsing. No operator precedence surprises. One way to do things.

### 4. **Verifiable Output**
Every program should be verifiable - by the compiler, by formal proofs, or by contracts.

### 5. **Token Efficiency**
Minimize tokens while maintaining clarity. Every saved token improves AI accuracy.

---

## What We Got Right (Keep)

### ✅ Second-Class References
Eliminates 94.8% of Rust's lifetime errors. This is FORMA's killer feature.

### ✅ Indentation-Significant Syntax
Removes brace matching errors. Clear visual structure.

### ✅ Strong Type Inference
Types are checked without annotation burden.

### ✅ Option/Result with ? Operator
Explicit error handling without exceptions.

### ✅ Pattern Matching
Exhaustive matching catches missed cases.

---

## What Needs Rethinking

### ❌ Single-Letter Keywords Were a Mistake

**Problem:** `f`, `s`, `e`, `m`, `t`, `i` conflict with variable names and hurt readability.

**The AI Argument:** Token savings don't matter if the AI gets confused about what's a keyword vs identifier.

**Solution:** Use clear, short keywords that don't conflict:

```
Old     New      Rationale
---     ---      ---------
f       fn       Universal, unambiguous
s       struct   Clear, no conflicts
e       enum     Clear, no conflicts
t       trait    Clear, no conflicts
i       impl     Clear, no conflicts
m       match    Clear, no conflicts
wh      while    Or just 'while'
```

**Token cost:** ~1 extra token per definition
**Clarity gain:** Massive - no parsing ambiguity, no variable name conflicts

### ❌ Match Arm Syntax is Inconsistent

**Problem:** `->` used for both function return types and match arms.

```forma
fn foo(x: Int) -> Int    # -> means "returns"
match x
    Some(v) -> v         # -> means "then"???
```

**Solution:** Use `=>` for match arms (like Rust, Scala):

```forma
fn foo(x: Int) -> Int
match x
    Some(v) => v         # => means "then"
    None => 0
```

### ❌ No Way to Verify Correctness

**Problem:** Code compiles but might be wrong. AI has no feedback beyond "it compiles."

**Solution:** Built-in contracts and verification:

```forma
fn factorial(n: Int) -> Int
    requires n >= 0
    ensures result >= 1
    ensures n > 0 implies result > n

    if n <= 1 then 1 else n * factorial(n - 1)
```

The compiler (or a separate verifier) checks these contracts. AI gets feedback: "contract violated" or "contract verified."

### ❌ Generics Are Syntax-Only

**Problem:** Can't write reusable data structures.

**Solution:** Full monomorphization with simple bounds:

```forma
fn max<T: Ord>(a: T, b: T) -> T
    if a > b then a else b

struct Vec<T>
    data: [T]
    len: Int
```

**Key insight:** Keep generics simple. No higher-kinded types. No associated types (initially). Just type parameters with trait bounds.

### ❌ No Module System

**Problem:** Everything in one file.

**Solution:** Simple, explicit modules:

```forma
# In math/vector.forma
module math.vector

pub struct Vec2
    x: Float
    y: Float

pub fn add(a: Vec2, b: Vec2) -> Vec2
    Vec2 { x: a.x + b.x, y: a.y + b.y }
```

```forma
# In main.forma
use math.vector.{Vec2, add}

fn main()
    v1 = Vec2 { x: 1.0, y: 2.0 }
    v2 = Vec2 { x: 3.0, y: 4.0 }
    v3 = add(v1, v2)
```

---

## New Features for AI-First Design

### 1. **Verification Levels**

Three levels of assurance:

```forma
# Level 1: Type-checked (default)
fn add(a: Int, b: Int) -> Int
    a + b

# Level 2: Contract-checked (runtime or static)
fn divide(a: Int, b: Int) -> Int
    requires b != 0
    ensures result * b <= a
    a / b

# Level 3: Formally verified (theorem prover)
@verified
fn sort<T: Ord>(input: [T]) -> [T]
    requires true
    ensures result.len() == input.len()
    ensures is_sorted(result)
    ensures is_permutation(result, input)
    ...
```

AI can target the appropriate level. Most code is Level 1. Critical code is Level 2 or 3.

### 2. **Constrained Generation API**

Built into the language tooling:

```bash
# Generate code constrained to valid syntax
forma generate --grammar "fn add(a: Int, b: Int) -> Int = ???"

# Generate code constrained to valid types
forma generate --typed "fn ???(items: [Int]) -> Int"

# Generate code that satisfies contracts
forma generate --verified "fn sort<T: Ord>(input: [T]) -> [T] ensures is_sorted(result)"
```

### 3. **Structured Diagnostics (Already Done ✅)**

JSON error output with suggestions. This is critical for AI self-correction.

### 4. **Deterministic Semantics**

No undefined behavior. No implementation-defined behavior. Every program has exactly one meaning.

```forma
# Overflow: defined behavior (wrapping with flag)
x: U8 = 255
y = x + 1  # y = 0, overflow flag set (not undefined!)

# Null: impossible (Option type)
# Use-after-free: impossible (ownership)
# Data races: impossible (no shared mutable state)
```

### 5. **AI-Friendly Standard Library**

Minimal, consistent, documented:

```forma
# Collections: exactly these, no more
Vec<T>       # Dynamic array
Map<K, V>    # Hash map
Set<T>       # Hash set
String       # UTF-8 string

# I/O: exactly these
File.read(path) -> String!
File.write(path, content) -> ()!
print(x)
input() -> String

# No: LinkedList, BTreeMap, VecDeque, 47 string types
# One way to do things.
```

### 6. **Semantic Versioning in the Type System**

Prevent API hallucinations:

```forma
# Library declares version
@api(version = "2.0")
pub fn connect(url: String) -> Connection!

# Old API marked deprecated with migration
@deprecated(since = "2.0", use = "connect")
pub fn open_connection(url: String) -> Connection!
```

AI trained on old code gets compiler guidance to new APIs.

---

## Syntax Redesign Summary

### Before (v2):
```
f read_users(path: Str) -> [User]!
    file = open path?
    content = file.read?
    users = parse_json content?
    users | filter .active

s User
    name: Str
    age: Int

e Status
    Active
    Inactive(reason: Str)
```

### After (v3):
```forma
fn read_users(path: String) -> [User]!
    file = open(path)?
    content = file.read()?
    users = parse_json(content)?
    users.filter(|u| u.active)

struct User
    name: String
    age: Int

enum Status
    Active
    Inactive(reason: String)
```

**Changes:**
- `f` → `fn` (clearer, no conflicts)
- `s` → `struct` (clearer)
- `e` → `enum` (clearer)
- `Str` → `String` (standard naming)
- Explicit function call parens (unambiguous)
- Pipeline `|` removed (method chaining is clearer)

**Token comparison:**
- v2: ~42 tokens
- v3: ~48 tokens
- Cost: ~14% more tokens
- Benefit: Unambiguous, familiar, no parsing edge cases

---

## Architecture Redesign

### Compilation Pipeline

```
Source
   ↓
┌─────────────────────────────────────────────────────┐
│ Frontend (in FORMA - self-hosted)                    │
├─────────────────────────────────────────────────────┤
│ Lexer → Parser → AST                                │
│   ↓                                                 │
│ Name Resolution → Type Inference → Type Checking    │
│   ↓                                                 │
│ Contract Checking (Level 2)                         │
│   ↓                                                 │
│ Borrow Checking                                     │
│   ↓                                                 │
│ MIR Lowering → MIR Optimization                     │
└─────────────────────────────────────────────────────┘
   ↓
┌─────────────────────────────────────────────────────┐
│ Backend (pluggable)                                 │
├─────────────────────────────────────────────────────┤
│ Option A: Interpreter (for bootstrap/testing)       │
│ Option B: LLVM IR → Native Code                     │
│ Option C: WASM                                      │
│ Option D: C (for portability)                       │
└─────────────────────────────────────────────────────┘
   ↓
┌─────────────────────────────────────────────────────┐
│ Verification (optional)                             │
├─────────────────────────────────────────────────────┤
│ Formal verification via SMT solver (Z3)             │
│ For @verified functions only                        │
└─────────────────────────────────────────────────────┘
```

### Tooling

```
forma                    # Compiler
forma run file.forma      # Compile and run
forma build              # Build project
forma test               # Run tests
forma check              # Type check only
forma verify             # Run formal verification
forma fmt                # Format code
forma grammar            # Export grammar (EBNF/JSON)
forma lsp                # Language server
forma new project        # Create new project
forma add dep            # Add dependency
```

### Project Structure

```
my_project/
├── forma.toml           # Project manifest
├── src/
│   ├── main.forma       # Entry point
│   └── lib/
│       ├── mod.forma    # Library root
│       └── ...
├── tests/
│   └── ...
└── target/
    ├── debug/
    └── release/
```

---

## Implementation Plan

### Phase 1: Core Language Redesign (Parallel)

**Track A: Syntax Updates**
- [ ] Update lexer for new keywords (fn, struct, enum, etc.)
- [ ] Update parser for v3 syntax
- [ ] Update all tests and examples
- [ ] Migration tool: v2 → v3

**Track B: Generics**
- [ ] Generic function syntax and parsing
- [ ] Type parameter bounds
- [ ] Monomorphization collection
- [ ] Generic struct/enum support

**Track C: Modules**
- [ ] Module declaration syntax
- [ ] Import/export system
- [ ] File resolution
- [ ] Visibility (pub)

### Phase 2: Verification & Backends (Parallel)

**Track D: Contracts**
- [ ] requires/ensures syntax
- [ ] Runtime contract checking
- [ ] Contract inheritance

**Track E: Native Compilation**
- [ ] LLVM backend setup
- [ ] Basic code generation
- [ ] Optimization passes

**Track F: Standard Library**
- [ ] Vec<T>
- [ ] Map<K,V>, Set<T>
- [ ] String operations
- [ ] File I/O
- [ ] Testing framework

### Phase 3: AI Integration (Parallel)

**Track G: Constrained Generation**
- [ ] Grammar export (done ✅)
- [ ] Type-constrained decoding API
- [ ] IDE integration (LSP)

**Track H: Formal Verification**
- [ ] @verified annotation
- [ ] SMT solver integration (Z3)
- [ ] Proof generation

### Phase 4: Ecosystem

**Track I: Tooling**
- [ ] Package manager
- [ ] Documentation generator
- [ ] Formatter

**Track J: Community**
- [ ] Tutorial
- [ ] Language reference
- [ ] Example projects

---

## Decision Points

### 1. Do we rename keywords?

**Option A:** Keep v2 single-letter keywords (f, s, e, m, t, i)
- Pro: Maximum token efficiency
- Con: Parsing ambiguity, confusion

**Option B:** Use v3 clear keywords (fn, struct, enum, match, trait, impl)
- Pro: Unambiguous, familiar
- Con: ~14% more tokens

**Recommendation:** Option B. The clarity gain outweighs the token cost. AI models are trained on languages with these keywords.

### 2. Do we keep the pipeline operator?

**Option A:** Keep `|` for pipelines
```forma
items | filter(>0) | map(*2) | sum
```

**Option B:** Use method chaining only
```forma
items.filter(|x| x > 0).map(|x| x * 2).sum()
```

**Recommendation:** Option B. The pipeline operator requires special syntax (`>0`, `*2`) that's non-standard. Method chaining is universal.

### 3. How deep does verification go?

**Option A:** Contracts only (runtime checks)
**Option B:** Contracts + optional formal verification
**Option C:** Formal verification required for certain operations

**Recommendation:** Option B. Make it easy to add contracts, make verification available but not required.

### 4. What's the compilation target?

**Option A:** Interpreter only (current)
**Option B:** LLVM (native)
**Option C:** C transpilation (portable)
**Option D:** All of the above

**Recommendation:** Option D. Interpreter for bootstrap/testing, LLVM for performance, C for portability.

---

## Success Metrics

### For Language Design
- [ ] AI can generate valid syntax 99%+ of the time (grammar-constrained)
- [ ] AI can generate type-correct code 95%+ of the time (type-constrained)
- [ ] Zero lifetime/borrow annotation errors (by design)
- [ ] Contract violations caught before runtime

### For Ecosystem
- [ ] Can build a web server
- [ ] Can build a CLI tool
- [ ] Can build the compiler itself (self-hosting)
- [ ] Package ecosystem with 100+ libraries

### For Adoption
- [ ] Featured in AI coding benchmarks
- [ ] Used by AI coding assistants
- [ ] Production deployments

---

## Open Questions

1. **Should we support null?** Current answer: No, use Option<T>. Is this the right call?

2. **Should we support exceptions?** Current answer: No, use Result<T, E>. Is this the right call?

3. **Should we support inheritance?** Current answer: No, use composition + traits. Is this the right call?

4. **How do we handle async?** Goroutines? Futures? Green threads?

5. **How do we handle unsafe code?** Allow it? Forbid it? Sandbox it?

---

## Conclusion

FORMA v3 is a refinement, not a rewrite. The core innovations (second-class references, AI-first tooling) remain. We're fixing the rough edges (keyword conflicts, missing features) and adding the differentiators (verification, constrained generation) that make FORMA the obvious choice for AI-generated systems code.

The goal: **Make it impossible for AI to generate incorrect FORMA code.**
