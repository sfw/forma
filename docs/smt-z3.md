# Using Forma Formal Verification with Z3

Forma can translate a supported pure subset of its MIR into SMT-LIB and ask
[Z3](https://github.com/Z3Prover/z3) to prove function contracts and struct
invariants. Formal verification is Experimental: `PROVED` applies only to the
reported obligation and supported model. Unsupported work is reported as
`UNKNOWN`, never silently accepted as proof.

## Install Z3

Install Z3 with your system package manager or download a release from the
[official Z3 releases](https://github.com/Z3Prover/z3/releases).

```bash
# macOS with Homebrew
brew install z3

# Ubuntu or Debian
sudo apt-get update
sudo apt-get install z3
```

On Windows, download an official release archive, extract it, and add the
directory containing `z3.exe` to `PATH`.

Confirm that Forma can start the solver:

```bash
z3 --version
forma verify --help
```

## Write a proof target

Formal mode proves declared `@post` conditions and implicit struct-invariant
obligations. Preconditions and valid invariant-bearing parameters are
assumptions.

```forma
@inv(balance >= 0, "balance cannot be negative")
s Account
    balance: Int
    active: Bool

@pre(opening_balance >= 0)
@post(result.balance == opening_balance)
f open_account(opening_balance: Int, active: Bool) -> Account
    Account { balance: opening_balance, active: active }

@post(result.active == account.active)
f normalize_account(account: Account) -> Account
    account.balance = 0
    account
```

For `open_account`, Forma asks Z3 to prove both the explicit postcondition and
the `Account` invariant established by construction and return. For
`normalize_account`, the incoming `Account` invariant is assumed at function
entry, projected mutation is tracked symbolically, and the returned value must
re-establish the invariant.

## Run formal verification

The minimum command is:

```bash
forma verify path/to/file.forma --level formal --report
```

Forma uses `z3` by default. Select it explicitly when clarity matters:

```bash
forma verify path/to/file.forma \
  --level formal \
  --report \
  --solver z3
```

The input may be one `.forma` file or a directory. Directory verification
recursively discovers `.forma` files.

### Select a custom solver path

Pass an executable directly:

```bash
forma verify rules.forma \
  --level formal \
  --report \
  --solver /opt/z3/bin/z3
```

Alternatively, set `FORMA_SMT_SOLVER`:

```bash
export FORMA_SMT_SOLVER=/opt/z3/bin/z3
forma verify rules.forma --level formal --report
```

An explicit `--solver` value takes precedence over `FORMA_SMT_SOLVER`.

## Understand the results

Formal reports use three primary function statuses:

| Status | Meaning |
| --- | --- |
| `PROVED` | Z3 found the negated obligation unsatisfiable within the supported model. |
| `COUNTEREXAMPLE` | Z3 found inputs that can violate an obligation or arithmetic-safety condition. |
| `UNKNOWN` | Forma could not make a proof claim because the construct is unsupported, the solver was unavailable, timed out, returned `unknown`, or assumptions were vacuous. |

Structs with `@inv` clauses also receive an aggregate formal status derived
from the relevant establishment and preservation proofs.

Use JSON output for automation:

```bash
forma verify src \
  --level formal \
  --report \
  --solver z3 \
  --format json
```

The JSON report records the solver command, detected solver version, timeout,
proof policy, per-function results, issues, limitations, and per-struct
invariant status.

## Reject vacuous proofs

A postcondition is not useful if its assumptions can never hold. Before proving
the main obligation, Forma asks Z3 whether parameter domains, arithmetic-safety
requirements, preconditions, and entry invariants are satisfiable.

If those assumptions are unsatisfiable, Forma reports `UNKNOWN` with a vacuity
reason instead of `PROVED`. Review contradictory `@pre` clauses and impossible
struct invariants.

```forma
@pre(value > 0)
@pre(value < 0)
@post(result == value)
f impossible(value: Int) -> Int
    value
```

This function is not proved: no `Int` satisfies both preconditions.

## Inspect counterexamples

When the proof query is satisfiable, Forma reruns the obligation with a model
query and reports source-oriented parameter values. Scalar leaves of structural
inputs use flattened names such as `account__balance`.

An invalid construction, for example:

```forma
@inv(balance >= 0)
s Account
    balance: Int

f invalid_account() -> Account
    Account { balance: -1 }
```

produces `COUNTEREXAMPLE`, not `PROVED`.

Treat counterexamples as reproducible debugging inputs. They demonstrate a
violation in the modeled semantics; they are not generated test samples.

## Retain auditable SMT-LIB

Use `--emit-smt` to keep the generated solver inputs:

```bash
forma verify rules.forma \
  --level formal \
  --report \
  --solver z3 \
  --emit-smt target/formal
```

For each attempted function, Forma writes:

| File | Purpose |
| --- | --- |
| `FILE--FUNCTION.smt2` | Decision-only proof query. `unsat` means the negated obligation has no model. |
| `FILE--FUNCTION.assumptions.smt2` | Satisfiability query for parameter domains, safety requirements, preconditions, and entry invariants. |
| `FILE--FUNCTION.counterexample.smt2` | Proof query plus model-value requests, used only when the decision query returns `sat`. |

You can run the decision query manually:

```bash
z3 target/formal/rules--open_account.smt2
```

Do not use the counterexample file as the primary decision query. If the
obligation is already `unsat`, its following model request may make Z3 print a
model-unavailable error; Forma executes that file only after a `sat` decision.

## Set timeouts

The default solver timeout is 5000 milliseconds per query:

```bash
forma verify rules.forma \
  --level formal \
  --report \
  --solver z3 \
  --solver-timeout 15000
```

A timeout is reported as `UNKNOWN`. Increasing the timeout can help a difficult
supported obligation, but it cannot make an unsupported language construct
verifiable.

## Use formal verification in CI

`--require-proved` fails unless every contract- or invariant-bearing function
reaches `PROVED`:

```bash
forma verify src \
  --level formal \
  --report \
  --solver z3 \
  --require-proved \
  --emit-smt target/formal
```

`--fail-on-unknown` is useful when `UNKNOWN` must fail even without requiring
every obligation to be proved:

```bash
forma verify src \
  --level formal \
  --report \
  --solver z3 \
  --fail-on-unknown
```

Example GitHub Actions steps:

```yaml
- name: Install Z3
  run: |
    sudo apt-get update
    sudo apt-get install -y z3

- name: Prove Forma obligations
  run: |
    ./target/release/forma verify src \
      --level formal \
      --report \
      --format json \
      --solver z3 \
      --require-proved \
      --emit-smt target/formal > formal-report.json
```

Upload `formal-report.json` and `target/formal/` as CI artifacts when proof
review or traceability matters.

## Current supported subset

The current symbolic MIR verifier supports:

- value-returning pure functions;
- acyclic, path-sensitive control flow;
- `Bool` and signed 64-bit `Int`;
- checked integer arithmetic and division safety;
- tuples and named structs composed from supported scalar leaves;
- tuple and field projection;
- structural tuple and struct equality;
- projected struct and tuple-field updates;
- direct pure function calls through symbolic inlining;
- function preconditions and postconditions;
- struct-invariant entry assumptions, construction obligations, direct-call
  boundary checks, and return-preservation obligations.

The following currently report `UNKNOWN`:

- loops and inductive loop invariants;
- recursive or indirect calls;
- effectful functions;
- arrays, dynamic vectors, and indexing;
- enums and general algebraic values;
- reference, dereference, and formal `ref mut` release reasoning;
- unsupported scalar types or contract expressions;
- path sets that exceed the verifier limit;
- solver failures, timeouts, or solver-level `unknown`.

Keep proof targets small, pure, and explicit. Separate an effectful shell from a
pure decision kernel when you want the kernel to be formally verified.

## Troubleshooting

### `cannot start SMT solver`

Run `z3 --version`. If it fails, install Z3 or pass its complete executable path
with `--solver`.

### `UNKNOWN: effectful functions are outside the formal pure subset`

Move file, network, database, environment, process, clock, random, concurrency,
or unsafe operations outside the function being proved.

### `UNKNOWN: formal subset does not yet support loops`

Refactor the proof target into an acyclic helper or use bounded/exhaustive
verification until loop-invariant support is implemented.

### `UNKNOWN: preconditions and parameter domains are unsatisfiable`

The proof was rejected as vacuous. Check contradictory preconditions,
unreachable integer bounds, and incompatible struct invariants.

### Solver timeout

Increase `--solver-timeout`, simplify branching, split the function, or reduce
the number of symbolically inlined calls. A larger timeout should not replace
review of an unexpectedly complex obligation.

### Formal verification passed but runtime behavior differs

Confirm that the behavior is inside the documented formal subset. `PROVED`
does not cover effects, native-backend gaps, or unsupported runtime features.
Retain the report and SMT artifacts so the exact claim remains reviewable.

## Working example

The Switchyard example contains the maintained Z3 regression corpus:

```bash
forma verify examples/switchyard/src/solver_targets.forma \
  --level formal \
  --report \
  --solver z3 \
  --require-proved \
  --emit-smt target/switchyard-smt
```

It exercises scalar control flow, tuple equality, named-struct construction,
projected mutation, and struct-invariant establishment and preservation.
