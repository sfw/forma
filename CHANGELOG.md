# Forma changelog

This project uses 0.x versions while its grammar, semantics, diagnostics, modules,
and backend compatibility policy are evolving.

## Unreleased

- Nothing yet.

## 0.2.0 — semantic foundation

### Language semantics

- Established affine ownership for ordinary non-`Copy` values.
- Added explicit owned, shared `ref`, and exclusive `ref mut` parameter modes.
- Added inferred non-lexical loans, initializedness tracking, partial moves, and
  deterministic drop elaboration.
- Defined compiler-known `Copy`, `Clone`, `Drop`, `Send`, and `Sync` behavior.
- Defined `=` as immutable binding and `:=` as mutable binding/update.
- Added deterministic trait coherence and method-resolution rules.
- Required exhaustive matching for finite algebraic domains.
- Added named-struct `@inv(condition[, "message"])` declarations with static
  Boolean checking, formatter support, runtime enforcement at construction,
  function entry/return, and `ref mut` return boundaries, plus explain and
  verification-report visibility.

### Compiler architecture

- Added a shared `CompilerSession` and stable source/module/definition identities.
- Added typed ownership-explicit MIR with projected places and explicit Copy/Move
  operands.
- Required ownership and drop gates before optimization and backend selection.
- Unified semantic facts used by checking, execution, verification, formatting,
  and language-server tooling.

### Effects, capabilities, and concurrency

- Centralized builtin signatures, ownership modes, effects, capabilities, and
  backend metadata.
- Added call-graph effect inference and registry-driven runtime capability gates.
- Added affine task handles, moving task captures, `Send`/`Sync` checks, capability
  propagation, and shared channel/mutex state.
- Corrected `sp` to defer the spawned expression and added overlap evidence for
  real concurrent execution.
- Added authenticated general HTTP, TLS PostgreSQL, canonical scoped-path, and
  bounded structured-process primitives for production-shaped hosted programs.

### Verification

- Separated generated testing, exhaustive finite-domain checking, and formal proof
  attempts with `--level test|exhaustive|formal`.
- Added `UNCONTRACTED`, `TESTED`, `COUNTEREXAMPLE`, `EXHAUSTIVE`, `PROVED`,
  `UNKNOWN`, and `SKIPPED` results.
- Added deterministic seeds, execution limits, counterexample reporting, SMT
  timeout and process cleanup, and human/JSON reports.

### Profiles and packages

- Published Core, Hosted, Native, and Experimental support profiles.
- Added transitive per-function backend-support reporting.
- Added deterministic local-path packages with `forma.toml` and generated
  `forma.lock` files.
- Kept registry and Git dependencies explicitly unsupported.
- Added Forge, a live LLM engineering-workflow example with connected DAG
  orchestration, bounded parallel nodes, structured tool use, iterative repair,
  SQLite/PostgreSQL memory, compaction, token/deadline budgets, event replay,
  and restart-safe resume.

### Generated artifacts and tooling

- Added a structured grammar model and generated EBNF, JSON grammar, keyword,
  editor, and builtin metadata.
- Expanded semantic hover, completion, navigation, references, signatures,
  formatting, and CLI queries on shared compiler state.
- Added repository corpus, formatter round-trip, grammar drift, ownership,
  capability, verification, optimizer-equivalence, and backend-differential gates.
- Made registered-builtin behavior/error coverage a 100% CI gate, backed by the
  authoritative builtin registry rather than interpreter-source name matching.
- Corrected `debug`/`info`/`error` dispatch, numeric-width conversion dispatch,
  missing argument validation in C/time conversions, and the read capability for
  `http_file_response` while closing the builtin coverage matrix.
- Made compiler stack policy frontend-independent so the CLI, LSP, tests, and
  embedded callers compile production-sized Forma packages consistently.

## Before 0.2

Earlier sprint notes and historical release labels described prototypes built before
the 0.2 semantic reset. They remain in `planning/` as development history and are
not compatibility claims. The current compiler version and this changelog are the
authoritative public version record.
