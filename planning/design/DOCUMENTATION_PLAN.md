# FORMA Documentation Plan

## Name & Identity

**Name:** FORMA (Latin: "form, shape, structure")
**Tagline:** "Code that writes itself correctly"
**File extension:** `.forma`
**Positioning:** The world's first programming language designed for Generative AI

## Positioning Statement

FORMA delivers memory safety without the complexity that trips up AI code generation. It's built from the ground up for a world where AI writes most code.

---

## Core Value Propositions

1. **Memory Safety Without Lifetimes**
   - Rust-level guarantees via "second-class references"
   - No lifetime annotations to hallucinate or get wrong
   - AI success rate: 94.8% of Rust's lifetime errors eliminated

2. **96% Fewer Syntax Errors**
   - Grammar designed for constrained decoding
   - AI can generate only valid syntax
   - Exportable grammar for any LLM toolkit

3. **38% Fewer Tokens**
   - Concise syntax reduces API costs
   - Single-character keywords where unambiguous
   - Type shortcuts: `T?` `T!` `[T]` `{K:V}`

4. **AI Self-Correction**
   - Structured JSON error output
   - Errors include fix suggestions
   - AI can parse, understand, and retry automatically

---

## Documentation Architecture

### Layer 1: Marketing & Vision
**Audience**: Decision makers, curious developers, first impressions
**Tone**: Bold, visionary, accessible

| Document | Purpose |
|----------|---------|
| README.md | GitHub landing - hook in 30 seconds |
| Website Landing | Expand the vision, interactive demos |
| "Why FORMA?" | Deeper dive into the problem we solve |
| Comparison Guide | Subtle positioning vs Rust/Go/Zig |

### Layer 2: Human Technical Documentation
**Audience**: Developers learning and using FORMA
**Tone**: Clear, friendly, thorough (Go-style)

| Document | Purpose |
|----------|---------|
| Getting Started | Install, hello world, first project |
| Language Tour | Learn FORMA in 30 minutes |
| Language Reference | Complete syntax and semantics |
| Standard Library | API docs for Vec, Map, Iterator, etc. |
| Memory Model | How second-class references work |
| Error Handling | Result types, propagation, patterns |
| Modules & Packages | Project structure, imports, forma.toml |
| CLI Reference | All commands and flags |

### Layer 3: LLM-Optimized Documentation
**Audience**: AI systems generating FORMA code
**Tone**: Precise, structured, token-efficient

| Document | Purpose |
|----------|---------|
| Grammar Spec | Formal grammar for constrained decoding |
| Error Catalog | Structured JSON of all errors + fixes |
| Type Reference | Concise type rules for inference |
| Pattern Library | Common patterns as copy-paste templates |
| Embedding Guide | How to fine-tune on FORMA |

---

## README.md Structure

```markdown
# FORMA

**The programming language designed for AI.**

[One-liner hook]

## Why FORMA?

[2-3 paragraphs: the problem, the solution, why now]

## Quick Example

[Side-by-side: Rust vs FORMA showing simplicity]

## Key Features

- Memory safety without lifetimes
- Grammar-constrained generation (96% fewer syntax errors)
- Token-efficient syntax (38% reduction)
- AI-readable error messages

## Get Started

[Install command]
[Hello world]

## Documentation

[Links to docs]

## Performance

[Benchmarks vs Rust/Go when available]

## Status

[Current state: alpha/beta, what works]

## Contributing

[How to help]
```

---

## Website Structure

```
/                   Landing - hero, value props, demo
/why                The problem with AI + systems languages
/learn              Interactive tutorial
/docs               Full documentation
/playground         Try FORMA in browser (WASM)
/blog               Updates, deep dives, case studies
/community          Discord, GitHub, contributing
```

---

## Content Priority

### Phase 1: Launch Essentials
- [ ] README.md (compelling, complete)
- [ ] Getting Started guide
- [ ] Language Tour
- [ ] "Why FORMA?" explainer

### Phase 2: Complete Documentation
- [ ] Full Language Reference
- [ ] Standard Library docs
- [ ] CLI Reference
- [ ] Memory Model deep dive

### Phase 3: AI-Specific
- [ ] Grammar export documentation
- [ ] Error catalog (JSON)
- [ ] LLM integration guide
- [ ] Fine-tuning dataset

### Phase 4: Community & Polish
- [ ] Comparison guides (vs Rust, Go, Zig)
- [ ] Blog posts / case studies
- [ ] Video tutorials
- [ ] Playground (WASM)

---

## Open Questions

1. **Name**: Is "FORMA" final? (Accessible, memorable, no conflicts?)
2. **Logo**: What visual identity?
3. **Tagline options**:
   - "The language AI understands"
   - "Memory safety, AI ready"
   - "Systems programming for the AI era"
   - "Code that writes itself correctly"
4. **Demo**: What's the most compelling 10-line example?

---

## Next Steps

1. Draft README.md with full content
2. Write "Why FORMA?" explainer
3. Create language tour outline
4. Design website wireframes
