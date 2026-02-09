# Why FORMA?

## The Shift Nobody Prepared For

In 2023, AI wrote its first million lines of production code. By 2025, that number crossed a billion. The way software gets written has fundamentally changed — but our programming languages haven't.

Rust, Go, C++, and TypeScript were designed for a world where humans write code and compilers catch mistakes. They optimize for human readability, human intuition, and human debugging.

That world is ending.

What's replacing it has six problems that no existing language solves. AI can't handle lifetimes. It hallucinates types and APIs. It generates invalid syntax. It wastes tokens on verbose keywords. And even when it does produce working code, nobody can review it fast enough — teams are shipping AI-generated functions they haven't fully read, because code review doesn't scale to hundreds of functions a day.

FORMA is designed from the ground up to solve all six.

## Six Problems, Six Solutions

### 1. Memory Safety Without Lifetimes

Rust's borrow checker is famously difficult for humans. For AI, it's nearly impossible — 94.8% of AI Rust generation failures are lifetime/borrow checker errors.

The problem isn't AI stupidity — it's that lifetimes require reasoning about the entire call graph, tracking which references outlive which scopes, understanding implicit lifetime elision rules. AI models generate code token by token. They can't hold the full lifetime graph in context.

**FORMA's solution: Second-class references.** References exist but can't be stored in structs or returned from functions. This eliminates lifetime annotations entirely:

```forma
# Rust: fn longest<'a>(x: &'a str, y: &'a str) -> &'a str
# FORMA: Just works, no lifetime tracking needed
f get_longest(strings: [Str]) -> Str
    longest := strings[0]
    for s in strings
        if len(s) > len(longest) then
            longest := s
    longest
```

The compiler guarantees memory safety through scope analysis, not lifetime inference. AI doesn't need to reason about lifetimes because they don't exist.

### 2. Strong Type Inference + Structured Errors

Modern type systems are powerful but intricate. AI hallucinates type signatures, invents trait bounds, and mismatches generics — 33.6% of AI Rust failures are type mismatches.

**FORMA's solution:** Hindley-Milner type inference means AI rarely needs explicit annotations. When types do mismatch, errors are machine-readable JSON with fix suggestions:

```forma
# Type inference handles most cases
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
  "suggestion": "Use str_parse_int() to convert"
}
```

AI can parse these errors and self-correct without human intervention.

### 3. No API Hallucination

AI models trained on diverse codebases confidently use APIs that don't exist:

```rust
// AI invents methods
let result = my_vec.filter_map_reduce(|x| x * 2);  // This isn't real
```

**FORMA's solution: Type-constrained decoding.** FORMA exports available methods for any type, letting AI tooling constrain generation to real APIs:

```bash
# Query available methods for a type
forma typeof "vec.?" --context "v vec = [1, 2, 3]"
# Returns: push, pop, len, map, filter, fold, ...
```

AI pipelines can use this to only generate method calls that actually exist.

### 4. No Syntax Errors

Even when logic is correct, syntax errors creep in. A misplaced semicolon, forgotten brace, or wrong operator breaks everything.

**FORMA's solution: Grammar-constrained generation.** FORMA exports its grammar for constrained decoding. AI can only generate syntactically valid tokens:

```bash
forma grammar --format ebnf > forma.ebnf
forma grammar --format json > forma.json
```

```python
# In your AI pipeline: constrain to valid syntax
from outlines import generate
generator = generate.cfg(model, open("forma.ebnf").read())
code = generator(prompt)  # Guaranteed syntactically valid
```

This eliminates syntax errors entirely — not by catching them, but by making them impossible.

### 5. Token Efficiency

AI API pricing scales with tokens — every character in a prompt or completion costs money and adds latency.

FORMA uses single-character keywords and type shortcuts:

| Concept | Rust | FORMA | Savings |
|---------|------|-------|---------|
| Function definition | `fn` | `f` | 50% |
| Match expression | `match x {` | `m x {` | 38% |
| Optional type | `Option<String>` | `String?` | 67% |
| Result type | `Result<T, Error>` | `T!` | 76% |
| For loop | `for x in items` | `i x in items` | 25% |

Across typical codebases, FORMA uses **38% fewer tokens** than equivalent Rust. That's 38% lower API costs, 38% faster generation, and 38% more code fitting in context windows.

### 6. Verifiable AI Intent

The first five advantages help AI generate correct code. The sixth solves the problem that comes after: **how do you know what it actually does?**

Most AI coding stacks stop at "it compiles." But compilation says nothing about correctness. An AI can generate a sort function that compiles perfectly yet silently drops elements, or a deposit function that compiles but doesn't add the right amount. The trust gap isn't "does it run?" — it's "does it do what I wanted?"

Teams are shipping AI-written functions faster than any human can read them. Code review was already a bottleneck when humans wrote everything. Now AI can produce hundreds of functions a day, and the reviewers are the same three senior engineers. Either review becomes a rubber stamp (dangerous) or a bottleneck that eliminates the speed advantage of AI generation (pointless).

FORMA closes this gap. Contracts declare intent directly on the functions they describe, and the verification UX makes that intent inspectable without reading source:

```bash
# Translate contracts to plain English
forma explain myfile.forma --format human

# Generate a CI-consumable trust report
forma verify src --report --format json --examples 20 --seed 42
```

Here's what that looks like in practice:

```
┌─ verified_sort(items: [Int]) -> [Int]
│  Requires:
│    - items is not empty
│  Guarantees:
│    - [@sorted] for every i in 0..result.len() - 1, result[i] is at most result[i + 1]
│    - [@permutation] permutation(items, result)
└─ Examples:
     [valid] ([3]) -> [3]
     [valid] ([0, -6]) -> [-6, 0]
```

Compare this to the alternatives: manual code review (slow, error-prone, doesn't scale), separate test suites (disconnected from the code they verify), or "trust the model" (not a strategy). FORMA's contracts live on the function they describe, `explain` translates them to English, and `verify` generates evidence that they hold.

This matters operationally:
- Product and QA teams can read what contracts mean without reading source code.
- CI can consume JSON trust reports with deterministic seeds.
- Verification runs with side effects disabled by default, so generated examples are safe to run in automation.

Instead of "the model said this function is safe," you get a reproducible artifact that shows what was checked and what still needs contracts. Code review stops being "read 500 lines of sort logic" and becomes "confirm that the contract says what I wanted."

## Why Existing Solutions Don't Work

### "Just use better prompts"

Prompt engineering helps but doesn't solve fundamental issues. You can't prompt away lifetime complexity — the information AI needs isn't in the prompt. And prompts can't give you verification tools for what gets generated.

### "Use language-specific fine-tuning"

Fine-tuning on Rust code produces models that are better at Rust but still fail on lifetimes. The failure mode is inherent to the language design, not the training data. And fine-tuning doesn't help the reviewer understand what the model produced.

### "Add more type annotations in prompts"

This helps with type errors but makes prompts expensive and unwieldy. It doesn't help with lifetimes, and it does nothing for the trust problem.

### "Just use Python/JavaScript"

Interpreted languages work well with AI — but they're not systems languages. You can't build operating systems, game engines, or embedded software in Python. And dynamically typed languages make the review problem worse, not better — there are no contracts to explain.

## The Performance Question

"Simpler language = slower code?"

No. FORMA compiles to native code via LLVM — the same backend powering Rust, Clang, and Swift. The second-class reference model doesn't prevent optimization; it just changes how the compiler reasons about memory.

For the same algorithms, FORMA should produce comparable machine code to Rust. We're not trading performance for simplicity — we're trading *language complexity* for *AI compatibility*.

## Who Is FORMA For?

### AI Agent Developers

If you're building AI systems that generate code, FORMA lets your models write correct systems-level code without lifetime gymnastics — and gives your users `explain` and `verify` tools to audit what the AI produced.

### Teams Adopting AI-Assisted Development

If your team uses Copilot, Claude, or similar tools, FORMA reduces the "fix AI's mistakes" tax on every generation. More importantly, it solves the review problem: your senior engineers don't need to read every AI-generated function line by line. They read the contract explanations, check the verify report, and focus their time on the functions that lack contracts or fail verification.

### Systems Programmers

If you like Rust's safety guarantees but not its complexity, FORMA offers the same safety with a gentler learning curve — for humans too.

### Language Researchers

If you're studying AI + programming languages, FORMA is an existence proof that language design can dramatically improve AI code generation — and that verification UX can close the trust gap between generation and deployment.

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

*FORMA: Code that writes itself correctly — and proves it.*
