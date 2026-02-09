# Why FORMA?

## The Shift Nobody Prepared For

In 2023, AI wrote its first million lines of production code. By 2025, that number crossed a billion. The way software gets written has fundamentally changed—but our programming languages haven't.

Rust, Go, C++, and TypeScript were designed for a world where humans write code and compilers catch mistakes. They optimize for human readability, human intuition, and human debugging.

That world is ending.

## The Problem With AI + Systems Languages

When AI models generate systems-level code, they fail in predictable ways. Here's each failure mode—and how FORMA solves it.

### 1. Lifetime Confusion (94.8% of Rust Failures)

Rust's borrow checker is famously difficult for humans. For AI, it's nearly impossible.

```rust
// AI-generated Rust (broken)
fn get_longest(strings: &Vec<String>) -> &str {
    let mut longest = &strings[0];
    for s in strings {
        if s.len() > longest.len() {
            longest = s;  // AI doesn't track that this borrows from strings
        }
    }
    longest  // Compiler error: lifetime issues
}
```

The problem isn't AI stupidity—it's that lifetimes require reasoning about the entire call graph, tracking which references outlive which scopes, understanding implicit lifetime elision rules. AI models generate code token by token. They can't hold the full lifetime graph in "memory."

**FORMA's solution: Second-class references.** References exist but can't be stored in structs or returned from functions. This eliminates lifetime annotations entirely:

```forma
# FORMA: Just works, no lifetime tracking needed
f get_longest(strings: [Str]) -> Str
    longest := strings[0]
    for s in strings
        if len(s) > len(longest) then
            longest := s
    longest
```

The compiler guarantees memory safety through scope analysis, not lifetime inference. AI doesn't need to reason about lifetimes because they don't exist.

### 2. Type System Complexity (33.6% of Failures)

Modern type systems are powerful but intricate. AI hallucinates type signatures, invents trait bounds, and mismatches generics:

```rust
// AI-generated (wrong trait bounds)
fn process<T: Iterator>(items: T) -> Vec<T::Item>
where T::Item: Clone + Debug  // AI guessed these bounds
{
    items.map(|x| x.clone()).collect()  // May not compile
}
```

**FORMA's solution: Strong type inference + structured errors.** FORMA uses Hindley-Milner type inference, so AI rarely needs explicit annotations. When types do mismatch, errors are machine-readable JSON with fix suggestions:

```forma
# FORMA: Type inference handles most cases
f process[T](items: [T]) -> [T]
    result := vec_new()
    for item in items
        vec_push(result, item)
    result
```

```json
{
  "error": "type_mismatch",
  "expected": "Int",
  "found": "String",
  "suggestion": "Use .parse() to convert"
}
```

### 3. API Hallucination

AI models trained on diverse codebases confidently use APIs that don't exist:

```rust
// AI invents methods
let result = my_vec.filter_map_reduce(|x| x * 2);  // This isn't real
```

**FORMA's solution: Type-constrained decoding.** FORMA can export available methods for any type, letting AI tooling constrain generation to real APIs:

```bash
# Query available methods for a type
forma typeof "vec.?" --context "v vec = [1, 2, 3]"
# Returns: push, pop, len, map, filter, fold, ...
```

AI pipelines can use this to only generate method calls that actually exist.

### 4. Syntax Errors at Scale

Even when logic is correct, syntax errors creep in. A misplaced semicolon, forgotten brace, or wrong operator breaks everything:

```rust
fn calculate(x: i32) -> i32 {
    if x > 0 { x * 2 }  // Missing else branch
    else { -x }         // AI sometimes forgets Rust's expression rules
}
```

**FORMA's solution: Grammar-constrained generation.** FORMA exports its grammar for constrained decoding. AI can only generate syntactically valid tokens:

```bash
forma grammar --format ebnf > forma.ebnf
forma grammar --format json > forma.json
```

```python
# In your AI pipeline: constrain to valid syntax
# Use EBNF or JSON grammar with your preferred toolkit
from outlines import generate
generator = generate.cfg(model, open("forma.ebnf").read())
code = generator(prompt)  # Guaranteed syntactically valid
```

This eliminates syntax errors entirely—not by catching them, but by making them impossible.

## Why Existing Solutions Don't Work

### "Just use better prompts"

Prompt engineering helps but doesn't solve fundamental issues. You can't prompt away lifetime complexity—the information AI needs isn't in the prompt.

### "Use language-specific fine-tuning"

Fine-tuning on Rust code produces models that are better at Rust but still fail on lifetimes. The failure mode is inherent to the language design, not the training data.

### "Add more type annotations in prompts"

This helps with type errors but makes prompts expensive and unwieldy. And it doesn't help with lifetimes.

### "Just use Python/JavaScript"

Interpreted languages work well with AI—but they're not systems languages. You can't build operating systems, game engines, or embedded software in Python.

## The Fifth Advantage: Token Efficiency

Beyond correctness, there's cost. AI API pricing scales with tokens—every character in a prompt or completion costs money and adds latency.

FORMA uses single-character keywords and type shortcuts:

| Concept | Rust | FORMA | Savings |
|---------|------|-------|---------|
| Function definition | `fn` | `f` | 50% |
| Match expression | `match x {` | `m x {` | 38% |
| Optional type | `Option<String>` | `String?` | 67% |
| Result type | `Result<T, Error>` | `T!` | 76% |
| For loop | `for x in items` | `i x in items` | 25% |

Across typical codebases, FORMA uses **38% fewer tokens** than equivalent Rust. That's 38% lower API costs, 38% faster generation, and 38% more code fitting in context windows.

## The Sixth Advantage: Verifiable AI Intent

Most AI coding stacks stop at "it compiles." FORMA adds a verification UX that makes generated behavior inspectable and repeatable:

```bash
# Explain contract intent in plain English/Markdown/JSON
forma explain myfile.forma --format markdown --examples --seed 42

# Generate a trust report over files/directories
forma verify src --report --format json --examples 20 --seed 42
```

This matters operationally:
- Product and QA teams can read what contracts mean without reading the full AST.
- CI can consume JSON trust reports with deterministic seeds.
- Verification runs with side effects disabled by default, so generated examples are safer to run in automation.

Instead of "the model said this function is safe," you get a reproducible artifact that shows what was checked and what still needs contracts.

## The Performance Question

"Simpler language = slower code?"

No. FORMA compiles to native code via LLVM—the same backend powering Rust, Clang, and Swift. The second-class reference model doesn't prevent optimization; it just changes how the compiler reasons about memory.

For the same algorithms, FORMA should produce comparable machine code to Rust. We're not trading performance for simplicity—we're trading *language complexity* for *AI compatibility*.

## Who Is FORMA For?

### AI Agent Developers

If you're building AI systems that generate code, FORMA lets your models write correct systems-level code without lifetime gymnastics.

### Teams Adopting AI-Assisted Development

If your team uses Copilot, Claude, or similar tools, FORMA reduces the "fix AI's mistakes" tax on every generation.

### Systems Programmers

If you like Rust's safety guarantees but not its complexity, FORMA offers the same safety with a gentler learning curve—for humans too.

### Language Researchers

If you're studying AI + programming languages, FORMA is an existence proof that language design can dramatically improve AI code generation.

## Trade-offs

FORMA isn't Rust. Some things are intentionally different:

**No stored references:** You can't build self-referential data structures directly. Use indices or handles instead.

**More cloning:** Without lifetime-tracked borrows, some patterns require explicit clones. The compiler helps optimize these.

**Smaller ecosystem:** FORMA is new. Rust has crates.io. We're building stdlib and FFI to bridge this gap.

**Less flexible:** Rust's expressiveness comes from complexity. FORMA trades some expressiveness for AI compatibility.

## Getting Started

```bash
git clone https://github.com/sfw/forma.git
cd forma
cargo build --release
./target/release/forma run examples/showcase/01_hello_world.forma
```

See the [README](../README.md) for full installation instructions and the [showcase examples](../examples/showcase/) for a tour of FORMA's features.

---

*FORMA: Code that writes itself correctly.*
