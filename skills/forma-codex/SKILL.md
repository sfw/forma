---
name: "forma-codex"
description: "Comprehensive FORMA language and tooling usage guide. Trigger when writing, reading, debugging, explaining, or reviewing `.forma` code; selecting builtins or std modules; working with contracts; exporting grammar; or using FORMA CLI commands (`run`, `check`, `fmt`, `repl`, `new`, `init`, `explain`, `verify`, `grammar`, `typeof`, `complete`, `build`, `lex`, `parse`, `lsp`)."
---

# FORMA Codex

## Scope

Use this skill to build and run FORMA programs correctly and safely.

Do not default to modifying FORMA compiler internals (`src/`, `runtime/`, Rust tests) unless the user explicitly asks for compiler/language development.

## Canonical Sources

Use sources in this order when details conflict:

1. Current compiler output
2. Generated `docs/grammar.*` and `docs/builtins.json`
3. `planning/design/FORMA_0_2_SEMANTICS.md` and `docs/profiles.md`
4. `docs/ai-reference.md` and `docs/reference.md`
5. Checked examples and `README.md`

Historical planning/review/sprint files are non-normative.

## Coverage Rules

- Be inclusive of all FORMA user-facing features, not only a small subset.
- Do not omit contract capabilities, variable-definition variants, grammar export, or CLI tooling.
- Preserve affine moves, explicit clones, loans, and task-handle obligations.
- Distinguish effects from runtime capabilities and state the required profile.
- Preserve exact verification statuses; generated testing is not proof.
- If uncertain about feature details, check `docs/ai-reference.md` before responding.

## Variable Definitions (Complete)

Use all supported binding styles appropriately:

```forma
# Immutable binding
x = 42
name = "Alice"

# Mutable binding and reassignment
counter := 0
counter := counter + 1

# Type-annotated binding
id: Int = 42
title: Str = "FORMA"

# Tuple destructuring
(a, b) := (1, 2)

# Single-expression function (uses =)
f main() -> Int = 0
```

Guideline: Use `=` for values that do not change and `:=` for values that will be updated.

## Core Syntax and Language Features

- Indentation-based blocks (no braces)
- Comments with `#`
- Generics use `[T]`
- Last expression is implicit return
- Pattern matching with `m`
- Function, struct, enum, trait, impl, module, type alias syntax
- Closures with typed parameters
- References: `ref` and `ref mut`
- Async features: `as`, `aw`, `sp`
- Option/Result with `?`, `??`, `!`
- Contracts with `@pre`, `@post`, `old`, quantifiers, named patterns

## Affine Ownership Rules

- Ordinary non-`Copy` values move through assignment, owned calls, returns, and
  by-value destructuring.
- `clone(value)` is explicit duplication; `mv value` may force or document a move.
- `ref` is a shared loan and `ref mut` is exclusive; loan regions are inferred.
- References cannot be stored in ordinary aggregates, captured by escaping
  closures, or sent to tasks.
- A returned reference must be derived from a reference parameter.
- Initialized owned places are destroyed exactly once.
- Compiler-known traits are `Copy`, `Clone`, `Drop`, `Send`, and `Sync`.

## CLI Workflows

Use these commands when working from this repository:

```bash
cargo run -- check path/to/file.forma --error-format json
cargo run -- fmt path/to/file.forma
cargo run -- run path/to/file.forma
cargo run -- explain path/to/file.forma --format human --examples=3 --seed 42
cargo run -- verify path/or/dir --report --format json --examples 20 --seed 42
cargo run -- typeof path/to/file.forma --position "L:C"
cargo run -- complete path/to/file.forma --position "L:C"
cargo run -- grammar --format ebnf
cargo run -- grammar --format json
cargo run -- lex path/to/file.forma
cargo run -- parse path/to/file.forma
cargo run -- repl
cargo run -- lsp
```

Use built binary when needed:

```bash
cargo build --release
./target/release/forma run path/to/file.forma
```

Project scaffolding:

```bash
cargo run -- new my-project
cargo run -- init
```

## Capability Safety Rules

- Always prefer least privilege over `--allow-all`.
- `--allow-all` is equivalent to broad host access.
- Capability mapping:
- File and directory builtins (`file_*`, `dir_*`) need `--allow-read` and/or `--allow-write`.
- Network builtins (`http_*`, `tcp_*`, `udp_*`, `tls_*`, `dns_*`) need `--allow-network`.
- Process builtins (`exec`, process utilities) need `--allow-exec`.
- Env builtins (`env_*`) need `--allow-env`.
- Unsafe/FFI builtins need `--allow-unsafe`.
- `verify` is side-effect-safe by default. Only use `--allow-side-effects` for trusted code that requires it.
- Capability flags are not complete OS isolation for hostile programs.

## Profiles and Verification Confidence

- Core is the portable semantic subset; Hosted uses the managed interpreter;
  Native names selected runtime-backed facilities; Experimental has weaker
  compatibility guarantees.
- Profile requirements propagate through direct calls.
- Use `verify --level test|exhaustive|formal` deliberately.
- Report `UNCONTRACTED`, `TESTED`, `COUNTEREXAMPLE`, `EXHAUSTIVE`, `PROVED`,
  `UNKNOWN`, and `SKIPPED` exactly.
- Never describe generated examples as proof. Formal verification covers only its
  Experimental pure supported subset.

## Contracts (Complete Coverage)

Core contract constructs:

- `@pre(condition[, "message"])`
- `@post(condition[, "message"])`
- `@inv(condition[, "message"])` on named structs
- `result` in postconditions
- `old(expr)` in postconditions
- `forall x in xs: predicate`
- `exists x in xs: predicate`
- `x in collection`
- `A => B` implication

Struct invariant fields are directly in scope. `result` and `old(...)` are not.
Invariants are enforced after construction, at function entry/return, and when
an exclusive `ref mut` borrow returns. Untrusted decoders should validate before
constructing invariant-bearing values.

Named contract patterns (all):

### Numeric Patterns

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@positive(x)` | Both | `x > 0` |
| `@nonnegative(x)` | Both | `x >= 0` |
| `@nonzero(x)` | Both | `x != 0` |
| `@even(x)` | Both | `x % 2 == 0` |
| `@odd(x)` | Both | `x % 2 != 0` |
| `@divisible(x, n)` | Both | `x % n == 0` |
| `@bounded(x, lo, hi)` | Both | `lo <= x <= hi` (inclusive) |
| `@in_range(x, lo, hi)` | Both | `lo < x < hi` (exclusive) |

### Collection Patterns

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@nonempty(x)` | Pre-only | `x.len() > 0` |
| `@contains(arr, elem)` | Both | `elem` is in `arr` |
| `@all_positive(arr)` | Both | all elements > 0 |
| `@all_nonnegative(arr)` | Both | all elements >= 0 |
| `@all_nonzero(arr)` | Both | all elements != 0 |
| `@valid_index(arr, i)` | Both | `0 <= i < arr.len()` |
| `@valid_range(arr, lo, hi)` | Both | valid slice bounds |

### Set Relationships

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@subset(a, b)` | Both | all of `a` in `b` |
| `@superset(a, b)` | Both | all of `b` in `a` |
| `@disjoint(a, b)` | Both | no overlap between `a` and `b` |
| `@equals(a, b)` | Both | same elements (set equality) |
| `@same_length(a, b)` | Both | `a.len() == b.len()` |
| `@permutation(a, b)` | Both | same elements with same multiplicities |

### Sequence Relationships

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@prefix(a, b)` | Both | `a` starts `b` |
| `@suffix(a, b)` | Both | `a` ends `b` |
| `@reversed(a, b)` | Both | `a` is reverse of `b` |
| `@rotated(a, b, k)` | Both | `a` is `b` rotated by `k` |
| `@unique(x)` | Both | no duplicate elements |

### Ordering Patterns

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@sorted(x)` | Both | ascending order (allows duplicates) |
| `@sorted_desc(x)` | Both | descending order (allows duplicates) |
| `@strictly_sorted(x)` | Both | ascending, no duplicates |
| `@strictly_sorted_desc(x)` | Both | descending, no duplicates |
| `@sorted_by(arr, field)` | Both | sorted by struct field |
| `@partitioned(arr, pivot)` | Both | partitioned at pivot index |
| `@stable(in, out, field)` | Post-only | stable sort semantics |

### State Patterns

| Pattern | Context | Meaning |
|---------|---------|---------|
| `@unchanged(x)` | Post-only | `x == old(x)` |
| `@pure` | Post-only | no side effects marker |

Postcondition inference rule:

- Named patterns default to preconditions.
- A named pattern becomes a postcondition when any argument is `result`.
- Use explicit `@pre(...)` or `@post(...)` to override.

Contract tooling commands:

```bash
cargo run -- explain path/to/file.forma --format human
cargo run -- explain path/to/file.forma --format json --examples=3 --seed 42
cargo run -- explain path/to/file.forma --format markdown --examples=3 --seed 42
cargo run -- verify path/or/dir --report --format human
cargo run -- verify path/or/dir --report --format json --examples 20 --seed 42
cargo run -- verify path/or/dir --level exhaustive --max-domain 4096 --report
cargo run -- verify path/or/dir --level formal --report
```

## Grammar Export (Required Coverage)

Use grammar export whenever constrained generation is needed:

```bash
cargo run -- grammar --format ebnf > forma.ebnf
cargo run -- grammar --format json > forma.json
```

Use these outputs with constrained-decoding toolchains to reduce syntax errors in generated FORMA code.

## Feature Checklist (All User-Facing Areas)

- Keywords and literals
- Types (primitive, sized ints, collections, option/result, refs, async types)
- Operators (arithmetic, logical, comparison, assignment, ranges, casts)
- Control flow (`if`, `m`, `wh`, `for`, `lp`, labels, guards)
- Functions, closures, generics, traits, impls
- Structs, enums, tuple structs, destructuring
- Modules/imports/type aliases
- Error handling (`?`, `??`, `!`, option/result utilities)
- Functional builtins (`map`, `filter`, `reduce`, `any`, `all`)
- Async and concurrency (`sp`, `aw`, channels, mutexes)
- Contracts (`@pre`, `@post`, `old`, quantifiers, all named patterns)
- CLI tooling (`run`, `check`, `fmt`, `repl`, `new`, `init`, `explain`, `verify`, `grammar`, `typeof`, `complete`, `build`, `lex`, `parse`, `lsp`)
- Builtin domains: I/O, math, string, collections, file/path, JSON, HTTP/TCP/UDP/TLS/DNS, DB, async, random, time, regex, compression, hashing, process, env, assertions, unsafe/FFI
- Stdlib modules (`std.core`, `std.io`, `std.string`, `std.vec`, `std.iter`, `std.map`)

## Task Playbooks

### Write New `.forma` Programs

1. Start from the closest example in `examples/` or `docs/ai-reference.md`.
2. Draft minimal code first, then add types/contracts as needed.
3. Run `check` with JSON errors and fix deterministic issues.
4. Run `fmt`.
5. Run `run` with only required capability flags.

### Debug Existing `.forma` Programs

1. Reproduce with `check --error-format json` or `run`.
2. Resolve type and pattern-matching issues first.
3. Re-run `check`, then `fmt`, then `run`.
4. If behavior still seems wrong, add contracts and inspect with `explain`/`verify`.

### Use Contracts as Trust Tools

1. Add `@pre` and `@post` to critical functions.
2. Use `explain` to produce human-readable intent.
3. Use `verify --report` to generate evidence artifacts.

### Build Better AI Generation Loops

- Use `grammar --format ebnf|json` for constrained decoding.
- Use `check --error-format json` for machine-fix loops.
- Use `typeof` and `complete` for position-aware generation guidance.
- Use `repl` for quick behavior probes.

## Output Requirements

- Return runnable `.forma` code, not pseudocode.
- Include exact command(s) to run.
- Include required capability flags and why they are needed.
- State the expected Core, Hosted, Native, or Experimental profile.
- Preserve the exact verification confidence status and its bounds.
- If uncertain about a builtin or syntax, consult `docs/ai-reference.md` before answering.
