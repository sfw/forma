# Forma and Agentic Engineering

This document states the research proposition behind Forma 0.2. It is intended to
invite evaluation, not declare the question settled.

## Thesis

When agents produce code at machine speed, the language toolchain should expose
machine-consumable constraints and human-reviewable evidence as first-class
outputs. Correctness cannot be delegated to a model, and governance cannot depend
on people reading every generated line.

Forma therefore treats the following as one workflow:

```text
intent -> generation -> semantic repair -> authority grant -> evidence -> review
```

## Responsibilities

### Human or policy owner

- states the intended behavior and risk tolerance;
- reviews contracts and externally visible interfaces;
- grants capabilities and chooses deployment profiles;
- decides which evidence level is required;
- retains responsibility for accepting residual risk.

### Agent

- writes or revises contracts, tests, and implementation;
- queries grammar, types, completions, and diagnostics;
- explains requested capabilities and unsupported profile requirements;
- responds to counterexamples and compiler feedback;
- never upgrades `TESTED`, `UNKNOWN`, or `SKIPPED` into a proof claim.

### Compiler and runtime

- enforce syntax, types, ownership, loans, effects, and capability gates;
- expose stable machine-readable diagnostics and semantic queries;
- preserve source meaning through optimization and execution;
- report profile and verification boundaries explicitly;
- reject unsupported operations rather than silently approximating them.

## Reference loop

```bash
# 1. Discover the language surface
forma grammar --format json
forma complete app.forma --position "12:8"

# 2. Repair against compiler facts
forma check app.forma --error-format json

# 3. Inspect declared intent
forma explain app.forma --format json --examples=3 --seed 42

# 4. Produce bounded evidence
forma verify app.forma --level test --report --format json

# 5. Run with least authority
forma run app.forma --allow-read
```

A higher-risk workflow can require exhaustive checking for finite inputs, a formal
attempt for supported pure functions, independent tests, code review, or OS
isolation. Forma reports what it can establish; it does not choose organizational
risk policy.

## Review packet

An agent handoff should include:

1. The contract or behavioral intent.
2. Compiler status and machine-readable diagnostics.
3. Required effects and capabilities.
4. Core, Hosted, Native, or Experimental profile requirements.
5. Verification level, bounds, seed, and result.
6. Counterexamples, `UNKNOWN`, `SKIPPED`, and uncontracted functions.
7. Known unsafe or FFI boundaries.
8. The exact compiler version and dependency lockfile.

This packet is a proposed unit of review for agent-generated changes.

## Experiments worth running

The executable [Forge example](../examples/forge/README.md) is the repository's
first controlled experiment: a connected engineering-process graph with
model-directed tools, structured responses, local SQLite or remote PostgreSQL
recall, compaction, bounded parallel nodes, iterative repair, restart replay,
capability boundaries, and causal audit evidence. Its live path uses a real
provider; offline CI tests the infrastructure without substituting scripted
model behavior.

- Compare unconstrained generation with grammar-constrained generation while
  tracking parse, type, ownership, and behavioral failures separately.
- Compare repair loops using prose diagnostics and structured JSON diagnostics.
- Measure whether contract-first generation improves behavioral correctness.
- Measure reviewer accuracy and time with source alone versus the review packet.
- Compare concise and readable aliases using total workflow tokens, not source
  characters alone.
- Test whether profile reports predict interpreter/native deployment failures.
- Red-team capability requests and generated programs under least privilege.

Experiments should publish prompts, models, compiler revision, seeds, programs,
failures, and scoring rules. Forma should earn quantitative claims through
reproduction.

## Non-goals

Forma 0.2 does not claim that:

- a model can generate only correct code;
- grammar constraints prevent semantic defects;
- contracts automatically capture human intent;
- generated tests constitute formal proof;
- all programs fit the formal-verification subset;
- capabilities alone isolate hostile native code;
- the current standard library or package ecosystem is production-complete.

## How to challenge the proposal

Strong contributions include minimal unsoundness examples, ambiguous semantics,
agent workflows that the tooling cannot support, misleading confidence reports,
capability escapes, profile inconsistencies, and evidence that a design choice
does not improve generation or review. See [CONTRIBUTING.md](../CONTRIBUTING.md).
