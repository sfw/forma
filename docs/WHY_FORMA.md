# Why Forma?

Forma is a working proposal for a future in which software agents produce and
revise more code than people can review line by line.

The project does not begin with the claim that AI-generated code is correct. It
begins with the opposite assumption: generation is fallible, review capacity is
finite, and important facts about a program should be available from the language
toolchain rather than reconstructed from prose or intuition.

## The shift

Most mainstream languages were designed around a person writing source and a
compiler checking it. Agentic development adds a fast, iterative producer that
can react to tools but can also invent syntax, APIs, types, authority, and false
confidence.

That changes the design question from “what syntax is pleasant to type?” to:

- What information should constrain generation before text is emitted?
- What compiler results can guide deterministic repair?
- How can ownership and authority stay visible across agent revisions?
- What should a person review when reading every implementation is unrealistic?
- How can tools report evidence without calling a test a proof?

Forma explores these questions in the language, compiler, runtime, and tooling as
one system.

## The proposal

### 1. Constrain syntax from one grammar model

Forma exports EBNF and JSON grammar for constrained-decoding systems. The same
structured model also produces keyword and editor artifacts. This can reduce one
class of generation errors, but it does not guarantee type correctness or program
correctness.

```bash
forma grammar --format ebnf
forma grammar --format json
```

### 2. Make semantic repair machine-readable

Checking can return structured diagnostics, and semantic queries expose types and
completion candidates. An agent can use those results in a repair loop instead of
guessing from compiler prose.

```bash
forma check program.forma --error-format json
forma typeof program.forma --position "5:10"
forma complete program.forma --position "5:10"
```

These tools reduce hallucination risk; they cannot make an unconstrained model
incapable of hallucinating.

### 3. Use affine ownership from the first interpreter

Non-`Copy` values may move or be dropped but cannot be used after a move or
duplicated implicitly. References are deliberately second-class and their loan
regions are inferred. This removes written lifetime parameters without removing
ownership obligations.

```forma
f consume(items: Vec[Item]) -> Unit
f inspect(ref items: Vec[Item]) -> Int
f update(ref mut items: Vec[Item]) -> Unit
```

The interpreter may use managed storage internally, but source programs still
observe moves, loans, and exactly-once destruction.

### 4. Separate effects from authority

Effects describe what a function may attempt. Capabilities grant that authority
to one execution. An agent can therefore write a useful file-processing program
without receiving unrelated network or process authority.

```bash
forma run report.forma --allow-read --allow-write
```

The capability system is interpreter containment, not a complete hostile-code
sandbox. Untrusted execution may also require OS process isolation.

### 5. Turn intent into a reviewable artifact

Contracts let an agent state preconditions and postconditions next to code.
`forma explain` translates that intent for people and tools.

```forma
@nonempty(items)
@sorted(result)
@permutation(items, result)
f ordered(items: [Int]) -> [Int]
    sort_ints(items)
```

This does not solve specification. A wrong contract can be verified successfully.
The human review task becomes smaller and clearer: confirm the contract, authority,
profile, and evidence, then inspect implementation where risk requires it.

### 6. Name the evidence honestly

Forma separates reproducible generated testing, exhaustive checking of supported
finite domains, and formal proof attempts over a supported pure subset.

```bash
forma verify rules.forma --level test --report
forma verify rules.forma --level exhaustive --report
forma verify rules.forma --level formal --report
```

The results—`TESTED`, `EXHAUSTIVE`, `PROVED`, `COUNTEREXAMPLE`, `UNKNOWN`,
`SKIPPED`, and `UNCONTRACTED`—are intentionally not interchangeable.

## Why one semantic spine matters

A language designed for tool-mediated development fails if every tool constructs
its own partial interpretation. Forma’s parser, resolver, type checker, ownership
analysis, effect inference, interpreter, native backend, LSP, formatter, and
verifier share compiler state and typed MIR.

Ownership gates run before optimization. Drop elaboration runs before backend
selection. Profiles propagate through calls. Verification consumes the same
contracts users execute. This architecture is the central proposal—not the short
keywords.

## The agentic workflow

1. A person supplies intent, constraints, and acceptable authority.
2. An agent selects compiler-known syntax and APIs.
3. The grammar constrains syntax where the generation stack supports it.
4. `forma check --error-format json` returns repairable semantic failures.
5. The agent revises until the compiler accepts the program.
6. Runtime capability flags grant only the required authority.
7. Contracts are explained and verified at the strongest supported level.
8. A person reviews the contract, capability set, profile, evidence, and selected
   high-risk implementation details.

The compiler is not the agent, and the agent is not the authority. Each has a
different role.

## What is a hypothesis, not a fact?

Forma deliberately leaves several claims open to measurement:

- Does concise syntax materially lower end-to-end generation cost after prompts,
  diagnostics, and repair turns are included?
- Do constrained grammars improve successful semantic completion, or merely move
  failures from parsing into typing?
- Can agents write useful contracts more reliably than implementations?
- Do reviewers catch more defects when shown contracts, authority, profiles, and
  evidence summaries?
- Does affine ownership without written lifetimes improve generation success while
  retaining an acceptable programming model?
- Can a shared semantic service keep editor, verifier, and backend behavior aligned
  as the language grows?

The repository needs reproducible experiments before making quantitative answers.
Older percentage and “impossible error” claims were removed for this reason.

## Trade-offs

- Second-class references simplify borrowing but rule out some safe patterns.
- Affine values make ownership visible but demand explicit state threading in some
  collection APIs.
- Short canonical keywords reduce source size but can be unfamiliar to people.
- Contracts reduce review scope only when their intent is correct.
- Formal verification is powerful only inside its supported model.
- Capability checks improve least-privilege execution but do not isolate a hostile
  process by themselves.
- Interpreter-first development accelerates semantic work while delaying complete
  native parity.
- A deliberately small ecosystem avoids ambiguous compatibility claims but offers
  fewer libraries than established languages.

## Who should evaluate Forma?

- Researchers exploring languages and tools for coding agents
- Teams designing structured generation, repair, and review loops
- Compiler engineers interested in shared semantic tooling
- Verification researchers working on evidence UX
- Security engineers studying capability-aware generated programs
- Language designers investigating ownership without written lifetimes

It is not yet a replacement for a mature production ecosystem.

## Open questions for the community

- Which program facts should be mandatory in an agent handoff?
- What is the smallest useful contract language for generated application code?
- Should agents be allowed to request capabilities, or only explain why they need
  them for a person or policy engine to grant?
- How should confidence reports compose across packages and network boundaries?
- Which profile boundary best predicts deployability?
- When should an `UNKNOWN` verification result block delivery?
- What evidence would falsify Forma’s core design hypotheses?

These are the discussion. Forma is the executable artifact around them.

## Continue

- Read the [agentic engineering thesis](AGENTIC_ENGINEERING.md).
- Use the [language guide](reference.md).
- Give an agent the [AI quick reference](ai-reference.md).
- Inspect the [0.2 profiles](profiles.md) and
  [implementation status](../planning/design/FORMA_0_2_IMPLEMENTATION_STATUS.md).
- Run the compiler and bring counterexamples to the issue tracker.
