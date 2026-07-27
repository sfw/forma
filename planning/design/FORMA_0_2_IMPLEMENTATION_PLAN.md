# FORMA 0.2 Implementation Plan

**Depends on:** `FORMA_0_2_SEMANTICS.md`  
**Goal:** Establish one coherent affine language across interpreter, tooling, and
native compilation before expanding feature breadth.

## Milestone 1: Semantic Compiler Spine

1. Introduce a library-level `CompilerSession` and `SourceMap`.
2. Route `check`, `run`, `build`, verification, REPL, and LSP analysis through
   shared phase APIs.
3. Add an ownership-analysis module over typed MIR.
4. Make MIR operands unambiguously `Copy` or `Move`; retire ambiguous local reads.
5. Add place projections for fields, tuple fields, indices, and dereferences.
6. Run ownership analysis before optimization on every executable/checkable path.

Acceptance gates:

- native build cannot skip ownership analysis;
- a non-`Copy` move invalidates the source place in every backend;
- `Copy` of a non-`Copy` type is rejected;
- all existing commands use the shared phase ordering;
- legacy tests remain green while compile-fail ownership tests are added.

## Milestone 2: Loans and Drop Elaboration

1. Implement shared and mutable loans with inferred non-lexical regions.
2. Implement reborrowing, control-flow joins, loops, closure captures, and return
   provenance.
3. Enforce second-class reference restrictions.
4. Add initializedness and partial-move tracking.
5. Insert explicit drops on normal and early-exit control-flow edges.
6. Add compiler-known `Copy`, `Clone`, and `Drop` validation.

Acceptance gates:

- no use-after-move, double mutable loan, mutation during shared loan, double drop,
  or drop of a partially moved field;
- `?`, return, break, contract failure, and abort paths have specified cleanup;
- interpreter and LLVM agree on observable destruction order for Core programs.

## Milestone 3: Authoritative Grammar and Lossless Syntax

1. Create the structured grammar/keyword model.
2. Support short canonical keywords and long aliases in the lexer/parser.
3. Generate EBNF, JSON, editor metadata, and documentation tables.
4. Introduce a lossless CST with comments and literal spellings.
5. Rebuild formatting on the CST and add corpus round-trip checks.

Acceptance gates:

- every generated grammar production is accepted by the parser;
- checked-in grammar artifacts cannot drift;
- formatting preserves comments and semantics for the entire repository;
- `fmt --write` never emits source rejected by `check` when its input checked.

## Milestone 4: Effects, Capabilities, and Builtins

1. Define a central builtin registry with type parameters, ownership modes,
   effects, capabilities, purity, backend support, and verification support.
2. Generate type-environment, LSP, documentation, and backend dispatch metadata.
3. Infer function effects through resolved call graphs.
4. Enforce runtime capabilities from registry metadata.
5. Add optional process/OS isolation for untrusted contract execution.

Acceptance gates:

- every effectful builtin has denied and allowed capability tests;
- no builtin implementation can omit its declared gate;
- spawned tasks inherit only an explicit subset of authority;
- untrusted execution cannot terminate or escape the verification harness.

## Milestone 5: Structured Concurrency

1. Introduce task scopes and affine task handles.
2. Move captures into tasks and enforce `Send`/`Sync` rules.
3. Share runtime resources through explicit safe ownership rather than numeric
   registries copied into child interpreters.
4. Propagate cancellation, deadlines, errors, and capabilities.
5. Rebuild channels as typed library/runtime handles whose sends move values.

Acceptance gates:

- task scopes cannot leak child tasks;
- references cannot cross task boundaries;
- captured resources are closed exactly once;
- interpreter limits cannot be bypassed by spawning work.

## Milestone 6: Modules, Traits, and Semantic Tooling

1. Add module/source IDs, per-module scopes, explicit exports, and importer-relative
   resolution.
2. Enforce trait coherence, orphan rules, deterministic lookup, and no overlap.
3. Require annotated public signatures while retaining local inference.
4. Store finalized types, definitions, references, members, effects, and backend
   support in compiler query results.
5. Replace CLI/LSP type and completion heuristics with semantic queries.

Acceptance gates:

- nested imports resolve relative to the importing module;
- conflicting impls are rejected independent of source order;
- `typeof` returns actual identifier/expression types;
- editor and CLI diagnostics share stable codes and source-aware spans.

## Milestone 7: Verification Levels

1. Rename current generated-example results to contract-testing statuses.
2. Isolate example execution and report tested domains, seeds, and limitations.
3. Add exhaustive finite-domain evaluation.
4. Define the pure formal subset and translate proof obligations to SMT.
5. Report `PROVED`, `COUNTEREXAMPLE`, or `UNKNOWN` without conflating testing.

Acceptance gates:

- generated tests never report `PROVED`;
- solver models produce reproducible source-level counterexamples;
- effects, unsafe operations, and unsupported recursion produce `UNKNOWN` or
  `SKIPPED`, never a false proof.

## Milestone 8: Backend Parity and Packaging

1. Publish Core/Hosted/Native/Experimental feature classifications.
2. Complete LLVM support for typed layouts, fields, collections, enums, drops,
   calls, and required Core builtins.
3. Add interpreter/LLVM differential tests.
4. Convert the repository to a Cargo workspace that builds the runtime together.
5. Evaluate a bytecode VM after semantic and native Core parity is measurable.
6. Add package manifests, lockfiles, dependency sources, and reproducible builds.

Acceptance gates:

- every Core example behaves identically in interpreter and LLVM builds;
- native builds do not depend on manually locating a separately built runtime;
- package resolution is deterministic and captured by a lockfile.

## Continuous Quality Gates

Every milestone maintains:

- formatter and parser round-trip properties;
- ownership compile-pass and compile-fail suites;
- optimizer equivalence over generated MIR;
- builtin behavioral and capability coverage;
- parser, type, MIR, and interpreter fuzzing;
- mutation tests for move invalidation, loans, drops, and capability enforcement;
- documentation examples compiled from their source documents;
- a clean `cargo fmt`, zero-warning Clippy, and passing Rust/runtime/showcase suites.
