# FORMA 0.2 Semantic Direction

**Status:** Normative design baseline  
**Audience:** Compiler, runtime, tooling, and documentation contributors

This document records the semantic decisions for FORMA 0.2. Historical sprint and
review documents are non-normative when they conflict with this document.

## Product Direction

FORMA is delivered interpreter-first, with an application-language profile that
can grow into a systems-language implementation. The language semantics are
affine from the beginning: an interpreter may use managed host allocations
internally, but it must preserve source-level moves, borrows, and destruction.

LLVM is an experimental backend for a documented Core subset until it reaches
semantic parity. A bytecode VM may be added later for startup, portability, and
sandboxing; it does not replace the native backend.

## Ownership and Memory

1. Ordinary non-`Copy` values are affine. They may be moved or dropped, but not
   implicitly duplicated.
2. Passing a non-`Copy` value to an owned parameter, assigning it to another
   binding, returning it, or destructuring it by value moves it.
3. `clone(value)` is explicit duplication. Implicit duplication requires `Copy`.
4. Shared references permit reading. Mutable references are exclusive.
5. Loan regions are inferred; users do not write lifetime parameters.
6. References are second-class: they cannot be stored in ordinary aggregates,
   captured by escaping closures, or sent to another task.
7. A returned reference must be derived from a reference parameter under the
   language's reference-elision rules.
8. Values are destroyed exactly once when their owning place leaves scope.
   Partial moves destroy only fields that remain initialized.
9. Default native memory management is ownership-directed destruction. Explicit
   `Shared[T]` and `Weak[T]` types may use reference counting.
10. Interpreter implementation clones must never make a moved source value usable.

Compiler-known traits are `Copy`, `Clone`, `Drop`, `Send`, and `Sync`. Safe code
may only derive or implement them when their structural requirements are met.
Types with `Drop` are never `Copy`.

## Bindings and Parameter Modes

`=` creates an immutable binding. `:=` creates or updates a mutable binding.
These operators describe mutability, not ownership transfer.

```forma
limit = 10
items := load_items()
items := transform(items)
```

Parameters make ownership visible:

```forma
f consume(items: Vec[Item])
f inspect(ref items: Vec[Item])
f update(ref mut items: Vec[Item])
```

Owned parameters move non-`Copy` arguments. `ref` creates a shared loan and
`ref mut` creates an exclusive loan. `mv` may force or document a move but is
not required for ordinary owned transfers.

## Syntax and Grammar

Short keywords are canonical. Readable long aliases are accepted. For example,
`as`/`async`, `aw`/`await`, `sp`/`spawn`, `lp`/`loop`, `br`/`break`, and
`ct`/`continue` normalize to the same semantic nodes.

A structured grammar model is the source of truth for keyword aliases,
operators, precedence, EBNF, JSON grammar, editor metadata, documentation, and
formatter spellings. The hand-written parser remains authoritative for recovery
behavior but must conform to the generated grammar artifacts.

## Types and Traits

FORMA uses rank-1 inference, nominal generics, and nominal traits with static
dispatch. Public functions require parameter and return annotations; local types
are inferred.

Trait coherence rules are:

1. At most one applicable implementation exists for a concrete trait/type pair.
2. An implementation belongs to the package defining either the trait or type.
3. Overlapping blanket implementations and specialization are initially disallowed.
4. Method resolution is deterministic and never depends on source order.
5. Inherent methods take precedence, followed by explicitly imported traits and
   then prelude traits; remaining ambiguity is an error.

## Effects and Capabilities

Effects describe authority a function may use. Capabilities grant authority to a
specific execution. Effects never grant capabilities.

Effects are inferred through the call graph and may be constrained by annotations.
Runtime checks enforce capabilities. Every effectful builtin declares its effects
and capability requirements in a central builtin registry.

Normal programs may read and write files when granted the corresponding authority.
OS-level sandboxing is optional defense in depth for untrusted verification,
playgrounds, build scripts, and third-party execution.

## Unsafe Code and FFI

The interpreter represents unsafe memory through checked allocation handles with
allocation identity, bounds, and generation tracking. Native code may use real raw
pointers only inside `unsafe`. Allocation handles are affine and deallocation
consumes the handle.

Verification refuses or reports `UNKNOWN` for unsafe behavior unless the operation
has an explicit model. Untrusted FFI may additionally run in an isolated process.

## Concurrency

FORMA uses structured concurrency. Child tasks cannot outlive their task scope
unless explicitly detached under a stronger capability.

Task rules are:

- captures move into a task;
- references cannot cross task boundaries;
- shared state uses explicit concurrency-safe ownership;
- task handles are affine and must be awaited, cancelled, returned, or detached;
- cancellation, deadlines, and a subset of parent capabilities propagate;
- channels are a library abstraction and sending moves the sent value.

## Pattern Matching

Exhaustiveness is required for finite algebraic domains including enums, `Bool`,
`Option`, `Result`, and their products. Open or infinite domains require a wildcard
unless a finite refined domain is known. Guards do not establish exhaustiveness.
Unreachable arms are diagnosed.

## Contracts and Verification

One contract language supports runtime checking, generated property testing,
exhaustive finite-domain checking, formal proof, and human/AI explanation.

Confidence statuses are distinct:

- `UNCONTRACTED`
- `TESTED`
- `COUNTEREXAMPLE`
- `EXHAUSTIVE`
- `PROVED`
- `UNKNOWN`
- `SKIPPED`

Generated examples never produce `PROVED`. SMT-backed verification begins with a
pure subset and reports `UNKNOWN` for unsupported constructs.

## Modules and Packages

Initially, one file defines one module. Imports resolve relative to the importing
module or package root, preserve namespaces, and bind explicit exported symbols.
`pub` controls visibility. Canonical module and source IDs drive caching, cycle
detection, and diagnostics.

Packages later add a manifest, lockfile, dependency graph, registry/path/Git
sources, build profiles, and package-root imports without introducing a second
module model.

## Compiler Architecture

The required phase order is:

```text
lossless CST
  -> semantic AST
  -> name resolution
  -> type and trait resolution
  -> effect inference
  -> typed high-level IR
  -> MIR construction
  -> ownership and borrow analysis
  -> drop elaboration
  -> optimization
  -> interpreter / bytecode VM / LLVM
```

All commands use one `CompilerSession`. Tooling queries reuse the same source map,
symbols, finalized types, effects, and diagnostics.

Ownership analysis runs before optimization. Drop elaboration runs before backend
selection. Optimizations must preserve move, loan, and destruction semantics.

## Panic and Destruction

Expected failures use typed results. The initial unrecoverable panic strategy is
abort, not unwinding. Destructors cannot fail, run exactly once in reverse
declaration order, cannot move fields out, and abort if they panic. Safe
self-referential movable types are disallowed.

## Stability

FORMA remains 0.x until grammar, semantics, modules, diagnostics, and backend Core
behavior have a compatibility policy. Features are labeled Core, Hosted, Native,
or Experimental, and documentation states the required profile.
