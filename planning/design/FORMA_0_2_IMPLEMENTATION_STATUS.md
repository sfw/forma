# Forma 0.2 Implementation Status

This document records the implementation state of
`FORMA_0_2_IMPLEMENTATION_PLAN.md`. It distinguishes the shipped 0.2 semantic
foundation from optional or explicitly later evolution paths.

## Milestone status

| Milestone | Status | Evidence |
| --- | --- | --- |
| 1. Semantic compiler spine | Complete | `CompilerSession`, shared phase ordering, typed ownership-explicit MIR, projected places, pre-optimization ownership gates |
| 2. Loans and drops | Complete for the 0.2 compiler-known model | NLL loan analysis, initializedness and partial moves, CFG and overwrite drop elaboration, abort/contract cleanup, `Copy`/`Clone`/`Drop` validation |
| 3. Grammar and syntax | Complete | authoritative grammar model, aliases, generated EBNF/JSON/editor/keyword artifacts, lossless source model, repository formatter round trips |
| 4. Effects and capabilities | Complete for interpreter containment | central builtin metadata and ownership policy, call-graph effects, registry-driven gates, explicit child-task authority |
| 5. Structured concurrency | Complete for Hosted 0.2 | affine task handles, `Send`/`Sync` checks, shared channel/mutex state, task limits/capability propagation, moving sends |
| 6. Modules, traits, tooling | Complete for the flat-export 0.2 boundary | stable module/source/definition IDs, importer-relative imports, visibility, deterministic ambiguity/coherence checks, semantic `typeof`/hover/completion data |
| 7. Verification levels | Complete | distinct contract-test/exhaustive/formal results, bounded execution, reproducible models, SMT timeout/process-group cleanup, struct-invariant obligations surfaced as `UNKNOWN` rather than false proofs |
| 8. Backend and packaging | Complete for the published profiles | Core LLVM layouts/projections/enums/arrays/loops, differential tests, Cargo workspace runtime, deterministic local-path manifests and lockfiles |

## Continuous gates

- The Rust compiler and runtime workspace passes all targets with all features.
- Clippy passes all targets and features with warnings denied.
- Generated grammar, editor, keyword, and builtin metadata have a checked drift gate.
- Every `.forma` program in `examples/` and `tests/forma/` passes the shared
  compiler pipeline; the integration suite enforces this corpus gate.
- The formatter round-trips the repository corpus, and deterministic parser
  mutation and generated-MIR optimizer equivalence tests run in the normal suite.
- Registry-wide capability tests prove every gated builtin is denied without and
  admitted with its declared capability.
- The registry-driven coverage audit requires executed behavior or deterministic
  error-path evidence for every compiler builtin and fails below 100%. Separate
  registry-wide tests cover dispatch reachability, capability deny/grant policy,
  owned-parameter metadata, and interpreter/native/verification support claims.
- Named-struct invariants are parser/type/formatter/MIR constructs and are
  exercised at construction, function entry/return, and `ref mut` return
  boundaries in both focused tests and the Forge black-box harness.

## Decision-aligned evolution paths

These are not silent omissions from the 0.2 profile:

- OS process isolation remains optional defense in depth for untrusted execution;
  0.2 provides interpreter containment, execution bounds, capability denial, and
  solver process-group termination.
- Registry and Git package sources follow deterministic local path dependencies.
- Hierarchical namespace-preserving module scopes follow the current deterministic
  flat explicit-export boundary. Ambiguous flattened exports are rejected.
- A bytecode VM is deferred until startup or performance measurements justify it.
- User-defined observable destructor bodies remain Experimental; Core uses
  compiler-generated deterministic drop glue.
- Native runtime injection and SMT preservation proofs for struct invariants
  remain profile-labeled evolution work; 0.2 enforces them in the interpreter
  and reports formal preservation as `UNKNOWN`.
- LSP member ranking can grow from semantic type/member data; 0.2 removes the
  separate diagnostic/type heuristics and retains a generic dot-completion fallback.

These boundaries match `FORMA_0_2_SEMANTICS.md` and `docs/profiles.md`; future work
must extend the same compiler/session, module, ownership, and profile models rather
than introducing parallel semantics.
