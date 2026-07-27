---
name: forma
description: >
  Forma 0.2 language guide for writing, checking, running, explaining, and
  verifying .forma programs with correct ownership, capabilities, profiles,
  syntax, builtins, and CLI usage.
---

# Forma 0.2 Language Skill

Use this skill when writing or reviewing Forma programs. Forma is an evolving
agent-oriented language prototype; never guess a builtin or turn bounded evidence
into a stronger claim.

## Sources of truth

Use these in order when details conflict:

1. Compiler output from the current checkout
2. Generated `docs/grammar.ebnf`, `docs/grammar.json`, and `docs/builtins.json`
3. `planning/design/FORMA_0_2_SEMANTICS.md` and `docs/profiles.md`
4. `docs/ai-reference.md` and `docs/reference.md`
5. Runnable examples

Files under `planning/research`, `planning/reviews`, and `planning/sprints` are
historical, not normative.

## Required workflow

1. Start from the closest checked example or the AI reference.
2. Use the generated builtin registry for exact signatures and capabilities.
3. Write the smallest program that satisfies the request.
4. Run `forma check --error-format json` and repair compiler-reported failures.
5. Run `forma fmt`.
6. Run with only the required capability flags.
7. If contracts matter, run `explain` and the strongest supported verification
   level, preserving its exact status and bounds.

From this repository, substitute `cargo run --` for `forma` when a built binary is
not available.

## Core syntax

```forma
# immutable binding
limit = 10

# mutable binding and update
count := 0
count := count + 1

# annotated bindings
title: Str = "Forma"
buffer: [Int] := vec_new()

# function; final expression is returned
f add(left: Int, right: Int) -> Int
    left + right

# single-expression function
f square(value: Int) -> Int = value * value
```

Blocks are indentation-based. Comments start with `#`. Generics use square
brackets. Canonical short keywords include `f`, `s`, `e`, `t`, `i`, `m`, `wh`,
`lp`, `br`, `ct`, `ret`, `as`, `aw`, `sp`, `us`, and `md`. Use the generated
keyword catalog for accepted long aliases.

`=` creates an immutable binding. `:=` creates or updates a mutable binding.
Mutability syntax does not decide ownership transfer.

## Affine ownership

Ordinary non-`Copy` values may move or be dropped but cannot be used after a move
or duplicated implicitly.

```forma
f consume(items: Vec[Item]) -> Unit
    # owned parameter; non-Copy argument moves

f inspect(ref items: Vec[Item]) -> Int
    items.len()

f update(ref mut items: Vec[Item]) -> Unit
    # exclusive loan
```

- Assignment, owned calls, returns, and by-value destructuring may move.
- Use `clone(value)` only when explicit duplication is intended.
- `mv value` may force or document a move; it is not required for ordinary
  transfer.
- `ref` is a shared loan; `ref mut` is exclusive.
- References cannot be stored in ordinary aggregates, captured by escaping
  closures, or sent to tasks.
- Return a reference only when derived from a reference parameter.
- Compiler-known ownership traits are `Copy`, `Clone`, `Drop`, `Send`, and `Sync`.

## Common forms

```forma
s Point { x: Int, y: Int }
e Direction = North | South | East | West

t Named
    f name(&self) -> Str

i Point
    f translated(&self, dx: Int, dy: Int) -> Point
        Point { x: self.x + dx, y: self.y + dy }

f describe(value: Bool?) -> Str
    m value
        Some(true) -> "yes"
        Some(false) -> "no"
        None -> "unknown"
```

Public function parameters and return values require annotations. Locals are
inferred. Dispatch is static. Matching finite algebraic domains must be exhaustive;
guards do not establish exhaustiveness.

## Errors and contracts

```forma
f load(path: Str) -> Str!Str
    content = file_read(path)?
    Ok(content)

@pre(n >= 0, "n must be non-negative")
@post(result >= 1)
f factorial(n: Int) -> Int
    if n <= 1 then 1 else n * factorial(n - 1)

@inv(balance >= 0, "balance cannot be negative")
s Account
    balance: Int
```

- `?` propagates a `Result` error.
- `??` supplies an `Option` default.
- `!` unwraps and panics on failure.
- Contracts support `@pre`, `@post`, `@inv`, `result`, `old(...)`, quantifiers,
  and named patterns. `@inv` applies to named structs; fields are in scope, but
  `result` and `old(...)` are not. Invariants are checked after construction,
  at function entry/return, and when `ref mut` returns. Consult
  `docs/ai-reference.md` for the pattern catalog.

## Effects and capabilities

Effects describe possible authority. Capabilities grant it to one execution.
Use only the flags required by `docs/builtins.json`:

```bash
forma run app.forma --allow-read
forma run app.forma --allow-write
forma run app.forma --allow-network
forma run app.forma --allow-exec
forma run app.forma --allow-env
forma run app.forma --allow-unsafe
```

Never use `--allow-all` for untrusted code. Treat `--allow-exec` as shell access.
Capability gating is not complete OS isolation.

## Structured concurrency

```forma
as f compute(value: Int) -> Int
    value * value

as f main()
    task = sp compute(12)
    result = aw task
    print(result)
```

Task captures move, references cannot cross task boundaries, and task handles are
affine. Await, cancel, return, or explicitly detach each handle. Channels and
mutexes are Hosted handles; sending moves the value.

## Profiles

- Core: portable ownership-aware subset.
- Hosted: managed interpreter facilities including dynamic collections and I/O.
- Native: selected runtime-backed native facilities.
- Experimental: weaker compatibility guarantees, including formal verification
  and whole-program LLVM parity.

Profile requirements propagate through calls. Do not claim Native or formal
support without checking the compiler report.

## Verification language

```bash
forma explain rules.forma --format human
forma verify rules.forma --level test --examples 200 --seed 42 --report
forma verify rules.forma --level exhaustive --max-domain 4096 --report
forma verify rules.forma --level formal --report
```

Report exact results: `UNCONTRACTED`, `TESTED`, `COUNTEREXAMPLE`, `EXHAUSTIVE`,
`PROVED`, `UNKNOWN`, or `SKIPPED`. Generated testing never produces `PROVED`.
Include seeds, bounds, unsupported effects, and unknown obligations in summaries.

## Tooling

```bash
forma check program.forma --error-format json
forma fmt program.forma
forma run program.forma
forma grammar --format ebnf
forma grammar --format json
forma typeof program.forma --position "5:10"
forma complete program.forma --position "5:10"
forma lex program.forma
forma parse program.forma
forma repl
forma lsp
forma new demo
forma init
forma build program.forma
```

Native build support requires the optional LLVM feature and is profile-bounded.

## Output requirements

- Return runnable Forma rather than pseudocode when code is requested.
- Include exact commands to check, format, and run it.
- Name every required capability and why it is needed.
- State the expected profile.
- Never invent builtin signatures; consult generated metadata.
- Never call generated tests or unsupported obligations proof.
