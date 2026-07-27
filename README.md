# Forma

[![CI](https://github.com/sfw/forma/actions/workflows/ci.yml/badge.svg)](https://github.com/sfw/forma/actions/workflows/ci.yml)

**A programming-language prototype for code you need to trust—especially when an agent writes it.**

Forma asks a concrete question: if software agents generate and revise code faster
than people can read it, what should move into the language and compiler so that
humans can still govern the result?

Version 0.2 explores one answer: a small language with affine ownership,
capability-gated effects, machine-readable compiler tooling, contracts, and
verification reports that distinguish tests from proofs. It is a working research
prototype, not a production-stable ecosystem.

- [Why Forma?](docs/WHY_FORMA.md)
- [Agentic engineering thesis](docs/AGENTIC_ENGINEERING.md)
- [Language guide](docs/reference.md)
- [AI quick reference](docs/ai-reference.md)
- [Feature profiles](docs/profiles.md)
- [Installation](INSTALL.md)

## The agent–compiler–human loop

```text
human intent
    -> agent writes contracts and code
    -> grammar constrains syntax
    -> compiler resolves types, ownership, effects, and profiles
    -> structured diagnostics guide repair
    -> bounded verification produces explicit evidence
    -> human reviews intent, authority, compatibility, and evidence
```

Forma does not assume that generated code is correct. It makes the feedback loop
more inspectable:

- one compiler session supplies facts to checking, execution, tooling, and
  verification;
- non-`Copy` values move or are dropped, rather than being duplicated implicitly;
- effects describe possible authority while runtime capabilities grant it;
- generated tests, exhaustive finite checking, and formal proof attempts produce
  different statuses;
- unsupported behavior remains `UNKNOWN` or `SKIPPED`, never optimistic success;
- support profiles expose interpreter, native, and verification boundaries.

## Quick start

Forma currently builds from source with Rust 1.85 or newer:

```bash
git clone https://github.com/sfw/forma.git
cd forma
cargo build --release
./target/release/forma --version
```

Create `hello.forma`:

```forma
f greet(ref name: Str) -> Unit
    print(f"Hello, {name}!")

f main()
    name = "Forma"
    greet(ref name)
    print(name)
```

Then check and run it:

```bash
./target/release/forma check hello.forma
./target/release/forma run hello.forma
```

`=` creates an immutable binding. `:=` creates or updates a mutable binding.
These operators describe mutability; assigning or passing a non-`Copy` value can
still move it.

## Language model

### Affine ownership

```forma
f consume(items: Vec[Item]) -> Unit
    # items is owned here

f inspect(ref items: Vec[Item]) -> Int
    items.len()

f update(ref mut items: Vec[Item]) -> Unit
    # exclusive loan
```

- Owned parameters move non-`Copy` arguments.
- `ref` creates a shared loan; `ref mut` creates an exclusive loan.
- Loan regions are inferred, so users do not write lifetime parameters.
- References cannot be stored in ordinary aggregates, captured by escaping
  closures, or sent to another task.
- A reference may be returned only when it is derived from a reference parameter.
- `clone(value)` is explicit duplication; `mv value` may document or force a move.
- Initialized owned places are destroyed exactly once.

### Effects and capabilities

Effects are inferred through direct calls and describe what a function may try to
do. Capabilities authorize one execution:

```bash
forma run reader.forma --allow-read
forma run writer.forma --allow-write
forma run client.forma --allow-network
forma run tool.forma --allow-exec
forma run configured.forma --allow-env
forma run ffi.forma --allow-unsafe
```

Prefer least privilege. `--allow-all` grants broad host authority and must not be
used for untrusted code. Capability checks provide interpreter containment; they
are not a complete hostile-code sandbox. OS process isolation is optional defense
in depth for untrusted execution.

### Contracts and evidence

```forma
@pre(n >= 0, "n must be non-negative")
@post(result >= 1)
f factorial(n: Int) -> Int
    if n <= 1 then 1 else n * factorial(n - 1)

@inv(balance >= 0, "balance cannot be negative")
s Account
    balance: Int
```

Struct invariants are type-wide validity rules. The Hosted interpreter checks
them after construction, at function entry and return, and when an exclusive
`ref mut` borrow is returned. A function may update related fields in stages,
but must restore the invariant before the value becomes externally observable.

```bash
forma explain rules.forma --format human
forma verify rules.forma --level test --examples 200 --seed 42 --report
forma verify rules.forma --level exhaustive --max-domain 4096 --report
forma verify rules.forma --level formal --report
```

Reports use distinct confidence states: `UNCONTRACTED`, `TESTED`,
`COUNTEREXAMPLE`, `EXHAUSTIVE`, `PROVED`, `UNKNOWN`, and `SKIPPED`.
Generated examples never produce `PROVED`. Formal verification is Experimental
and limited to a documented pure subset. Invariant obligations appear in
explain/verification output, but struct preservation remains `UNKNOWN` until the
SMT struct subset is implemented.

### Structured concurrency

```forma
as f compute(value: Int) -> Int
    value * value

as f main()
    task = sp compute(12)
    result = aw task
    print(result)
```

Task captures move into the child, references cannot cross task boundaries, and
task handles are affine. Channels and mutexes are Hosted library/runtime handles;
sending a value moves it.

## Support profiles

“Supported” is not one claim in Forma:

| Profile | Meaning |
| --- | --- |
| Core | Portable semantic subset: ownership, scalars, tuples, fixed arrays, structs, selected enums, calls, matches, and loops |
| Hosted | Managed interpreter features such as dynamic collections, strings, files, databases, networking, processes, tasks, channels, and mutexes |
| Native | Runtime-backed facilities currently implemented by the LLVM toolchain |
| Experimental | Weaker compatibility guarantees, including whole-program LLVM parity, SMT verification, and observable user-defined destructor bodies |

Support propagates through direct calls. A wrapper around a Hosted-only operation
does not become Core merely because its own syntax looks portable. See
[docs/profiles.md](docs/profiles.md) for the normative profile definitions.

## Tooling for agents and people

```bash
# Machine-readable correction loop
forma check program.forma --error-format json

# Grammar-constrained generation
forma grammar --format ebnf
forma grammar --format json

# Semantic queries
forma typeof program.forma --position "5:10"
forma complete program.forma --position "5:10"

# Normal development
forma fmt program.forma
forma run program.forma
forma repl
forma lsp
```

The generated artifacts in `docs/grammar.ebnf`, `docs/grammar.json`,
`docs/grammar-keywords.md`, `docs/editor-grammar.json`, and `docs/builtins.json`
are machine-readable sources for agents and editor integrations.

## Packages and editor support

Forma 0.2 supports `forma.toml`, generated `forma.lock` files, and deterministic
local path dependencies. Registry and Git sources are intentionally rejected for
now. The VS Code extension metadata lives in `editors/vscode/`; the compiler also
provides `forma lsp`.

```bash
forma new demo
cd demo
forma run src/main.forma
```

## Worked agentic system: Forge

[Forge](examples/forge/README.md) is a self-correcting engineering-workflow
coordinator written in Forma. A connected process DAG drives parallel reviews,
iterative repair and verification, progress-sensitive strategy changes,
least-authority model-directed tools, restart-safe resume, SQLite or PostgreSQL
history and recall, durable compaction, token/deadline budgets, and append-only
audit events. Its provider path is live; CI checks configuration, protocol
parsing, memory, real task concurrency, and the pure kernel without scripted
model responses.

Forge remains in this repository during the 0.2 series so that a realistic
agentic application continuously pressures the language, compiler, and
documentation together. Every place it requires Rust is recorded in the
[Forge Rust gap ledger](planning/design/FORGE_RUST_GAP_LEDGER.md).

## Project status

Implemented foundations include the shared compiler session, typed
ownership-explicit MIR, ownership and loan analysis, deterministic drop
elaboration, generated grammar artifacts, effect and capability metadata,
structured concurrency checks, deterministic local modules/packages, semantic
tooling, tiered verification, and the published profile model.

Important boundaries remain:

- Forma is 0.x and does not promise source compatibility yet.
- The interpreter is the primary complete execution environment.
- LLVM has a documented Core/Native boundary and remains Experimental as a
  whole until whole-program parity is established.
- Formal verification covers a limited pure subset.
- Registry packages, Git dependencies, rename/refactor support, and richer
  cross-file LSP workflows remain future work.
- Capability flags are not a substitute for OS isolation of hostile code.

The detailed implementation record is in
[planning/design/FORMA_0_2_IMPLEMENTATION_STATUS.md](planning/design/FORMA_0_2_IMPLEMENTATION_STATUS.md).

## Documentation map

| Need | Source |
| --- | --- |
| Understand the proposal | [docs/AGENTIC_ENGINEERING.md](docs/AGENTIC_ENGINEERING.md) and [docs/WHY_FORMA.md](docs/WHY_FORMA.md) |
| Learn the language | [docs/reference.md](docs/reference.md) |
| Give an agent compact instructions | [docs/ai-reference.md](docs/ai-reference.md) |
| Use Forma through Codex skills | [skills/forma-codex/SKILL.md](skills/forma-codex/SKILL.md) |
| Check exact grammar | Generated `docs/grammar.*` artifacts |
| Check exact builtin metadata | Generated [docs/builtins.json](docs/builtins.json) |
| Browse builtin signatures | Generated [docs/builtins.md](docs/builtins.md) |
| Understand backend boundaries | [docs/profiles.md](docs/profiles.md) |
| Install or contribute | [INSTALL.md](INSTALL.md), [CONTRIBUTING.md](CONTRIBUTING.md) |
| Follow changes | [CHANGELOG.md](CHANGELOG.md) |

Historical files under `planning/research/`, `planning/reviews/`, and
`planning/sprints/` document how the prototype evolved. They are non-normative
when they conflict with the 0.2 semantics or current generated artifacts.

## Contributing to the conversation

Forma is intended to be evaluated, challenged, and improved. Useful contributions
include counterexamples to the language model, agent-generation experiments,
diagnostic and verification UX work, soundness reviews, profile tests, runnable
examples, documentation, and compiler implementation work. See
[CONTRIBUTING.md](CONTRIBUTING.md).

## License

Forma is licensed under the [MIT License](LICENSE-MIT).
