# FORMA 0.2 Feature Profiles

FORMA 0.2 is interpreter-first. The labels below describe semantic support,
not marketing maturity.

- **Core** is the portable semantic subset: affine moves and loans, deterministic
  compiler drop glue, scalar values, tuples, fixed arrays, structs, uniform
  scalar-payload enums, direct calls, conditionals, finite matches, and loops.
- **Hosted** requires the managed interpreter runtime: dynamic collections,
  strings, files, databases, networking, processes, tasks, channels, and mutexes.
- **Native** identifies runtime-backed facilities implemented by the native
  toolchain, currently selected strings, math, memory, and collections.
- **Experimental** has intentionally weaker compatibility guarantees. LLVM as a
  whole remains Experimental until every Core example passes differential tests;
  SMT verification and user-defined destructor bodies are also Experimental.

Every compiler result contains a per-function backend-support report. Support is
transitive through direct calls: a Core-looking wrapper around a Hosted-only
builtin is reported as unsupported by LLVM and formal verification.

Task captures must implement compiler-known `Send`; shared references and
unshared network/database handles are rejected at the task boundary. Channels
and mutexes are Hosted library/runtime handles with explicitly shared state.

Function contracts and named-struct invariants are checked by the Hosted
interpreter. The compiler parses and type-checks them for every profile, but the
0.2 LLVM path does not yet inject native contract/invariant checks. Verification
reports invariant obligations as `UNKNOWN` until its Experimental SMT subset can
model struct preservation.

Projects use `forma.toml` and a generated `forma.lock`. Forma 0.2 resolves
deterministic local path dependencies from `[deps]`; registry and Git sources
remain future package-manager work and are rejected rather than guessed.
