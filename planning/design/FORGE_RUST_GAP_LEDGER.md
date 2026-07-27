# Forge Rust Gap Ledger

This ledger records every time Forge application or harness work requires Rust.
Its purpose is to turn host-side workarounds into an explicit Forma language,
runtime, standard-library, CLI, or tooling backlog.

The ledger begins before implementation. No Forge-specific Rust support may be
added without a corresponding entry.

## Rules

1. Forge domain logic, graph semantics, scenario execution, fixtures,
   assertions, and application adapters belong in Forma.
2. Rust is permitted for black-box host isolation or behavior Forma cannot yet
   express safely and deterministically.
3. Every Forge-specific Rust helper or compiler/runtime change prompted by Forge
   references a stable ledger identifier in its source or test name:
   `FORGE-RUST-GAP: FRG-###`.
4. An entry describes the user-level need, not merely the Rust implementation.
5. Each entry has a removal criterion and a Forma regression test that must pass
   before the workaround is removed.
6. General compiler bug fixes discovered by Forge are recorded even when the
   final fix necessarily belongs in the Rust compiler.
7. Refactoring existing Rust without exposing a missing Forma capability does
   not create a ledger entry.

## Entry template

### FRG-000 — Short title

- **Status:** Proposed | Active | Resolved | Accepted host boundary
- **Discovered:** YYYY-MM-DD
- **Area:** Language | Runtime | Standard library | CLI | LSP | Test isolation
- **Forge need:** User-visible behavior the project requires.
- **Why Forma was insufficient:** Exact missing or unsafe capability.
- **Rust used:** Narrow description and source locations.
- **Current workaround:** How Forge remains correct in the meantime.
- **Forma improvement:** Proposed language or tooling addition.
- **Removal criterion:** Observable condition that makes the Rust workaround
  unnecessary.
- **Regression evidence:** Forma and black-box tests protecting the improvement.
- **Priority:** Release blocking | High | Medium | Low

## Active entries

None.

## Resolved entries

### FRG-015 — Folder-based custom tools remained ordinary Forma programs

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Application architecture
- **Forge need:** Discover custom model tools from folders while keeping each
  implementation beside its manifest and outside the orchestration core.
- **Why Forma was insufficient:** No language change was required. Forma
  programs can already be launched through a bounded, allowlisted process with
  explicit capabilities and an isolated environment.
- **Rust used:** None in the implementation. The accepted FRG-002 black-box
  boundary runs the end-to-end CI assertion.
- **Current workaround:** None. Each plugin owns `tool.toml` and `tool.forma`;
  Forge validates discovery, role grants, capabilities, named environment
  variables, source presence, execution bounds, and resumability digests.
- **Forma improvement:** Demonstrates a real plugin protocol implemented in
  Forma without a provider SDK or Forge-specific host adapter.
- **Removal criterion:** Met.
- **Regression evidence:** `examples/forge/src/tool_plugin_check.forma` and the
  packaged CLI assertion in `tests/forge_example.rs`.
- **Priority:** High

### FRG-004 — General authenticated HTTP requests with explicit policy

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Runtime
- **Forge need:** Call hosted LLM providers over HTTPS while keeping API keys in
  environment variables and sending them only as authorization headers.
- **Why Forma was insufficient:** `http_post_json` could send JSON but exposed
  no request-header argument. Shelling out to `curl` would put secrets at a
  process boundary and would make the agent harness depend on a host command.
- **Rust used:** The builtin registry, type environment, and MIR interpreter
  expose `http_request(method, url, body, headers, timeout_ms,
  follow_redirects)`, with `http_request_json` retained as a convenience API.
- **Current workaround:** None. Forge uses the general capability-gated HTTP
  primitive and owns all provider protocol logic in Forma.
- **Forma improvement:** Preserve this as a provider-neutral standard HTTP
  primitive rather than adding an OpenAI-specific host adapter.
- **Removal criterion:** Met. Forma can issue authenticated JSON requests
  without a provider-specific adapter or secret-bearing subprocess. Timeouts
  and cancellation remain general HTTP API enhancements rather than Forge
  workarounds.
- **Regression evidence:** Registry/type-environment parity, interpreter
  dispatch coverage, capability denial, argument/header validation tests, and
  Forge's provider adapter using the general request API.
- **Priority:** Release blocking

### FRG-005 — Database resource types in Forma signatures

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Language
- **Forge need:** Put SQLite history and recall behind reusable Forma functions
  whose parameters are database connections, statements, and rows.
- **Why Forma was insufficient:** Builtins returned internal `Database`,
  `Statement`, and `Row` types, but source annotations with those spellings
  became unrelated nominal types and could not be passed back to the builtins.
- **Rust used:** Source-type lowering maps the three public resource spellings
  to their corresponding internal types.
- **Current workaround:** None.
- **Forma improvement:** Completed for the SQLite resource surface; resource
  handles are now expressible at user-defined function boundaries.
- **Removal criterion:** Met when annotated wrapper functions type-check and
  execute using all three SQLite resource types.
- **Regression evidence:** `tests/forma/test_database_resource_signatures.forma`
  and the Forge memory module corpus check.
- **Priority:** Release blocking

### FRG-007 — Human-authored TOML configuration

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Runtime
- **Forge need:** Use commented, sectioned, multiline configuration without
  treating human-authored settings as an API payload.
- **Why Forma was insufficient:** Forma exposed JSON parsing only.
- **Rust used:** General `toml_parse` and `toml_stringify` builtins convert
  between TOML and Forma's dynamic `Json` value.
- **Current workaround:** None. Forge's canonical settings file is TOML while
  workflow graphs and provider payloads remain JSON.
- **Forma improvement:** Completed as provider-neutral configuration support.
- **Removal criterion:** Met.
- **Regression evidence:** TOML round-trip interpreter tests and Forge's
  black-box configuration check.
- **Priority:** High

### FRG-008 — Authenticated PostgreSQL connections

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Runtime
- **Forge need:** Let Forma applications use PostgreSQL remotely while retaining
  SQLite for embedded local state.
- **Why Forma was insufficient:** Every `Database` handle was backed directly
  by SQLite and there was no authenticated remote connection boundary.
- **Rust used:** `db_connect_postgres(url)` creates a TLS-capable PostgreSQL
  connection behind the existing `Database`, statement, query, and row APIs.
- **Current workaround:** None. Applications obtain URLs through `env_get`; no
  credential belongs in TOML or source.
- **Forma improvement:** The public resource contract is backend-neutral;
  SQLite and PostgreSQL differ only at connection creation and SQL dialect.
- **Removal criterion:** Met when PostgreSQL connections support direct and
  prepared execution/query paths under network authority.
- **Regression evidence:** Registry/type parity and network-capability denial;
  live PostgreSQL integration remains opt-in because CI credentials and a
  server are external infrastructure.
- **Priority:** High

### FRG-006 — Program arguments were replaced by the host CLI arguments

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** CLI
- **Forge need:** Select `check` or `run`, a settings path, and a goal through
  arguments passed to the Forma program.
- **Why Forma was insufficient:** The CLI correctly installed program
  arguments in the interpreter environment, but `args()` ignored them and read
  the Rust host process arguments, including Forma's own flags and source path.
- **Rust used:** The interpreter now builds `args()` from its isolated
  `ARGC`/`ARGV_n` overlay and enforces the declared environment capability.
- **Current workaround:** None.
- **Forma improvement:** `args()` now returns exactly the arguments after the
  Forma source file.
- **Removal criterion:** Met.
- **Regression evidence:** CLI argument forwarding tests and Forge's
  black-box configuration check.
- **Priority:** Release blocking

### FRG-001 — Projected drop after an owned aggregate transition

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Language runtime
- **Forge need:** Execute a pure state-machine program that repeatedly consumes
  and returns affine structs containing strings and vectors.
- **Why Forma was insufficient:** The program passed parsing, typing, and
  ownership analysis, but interpreted execution failed with
  `undefined local in projected drop`. MIR normalization treated a projected
  read through a `ref` parameter as an owned partial move.
- **Rust used:** `src/mir/ownership.rs` now normalizes both roots and projections
  of non-owned parameters as borrows.
- **Current workaround:** None; the compiler implements the required ownership
  semantics.
- **Forma improvement:** Completed. Reference parameters are aliases rather than
  owners even when an observing expression would otherwise request an owned MIR
  operand.
- **Removal criterion:** Met. Forge executes its affine transitions without
  source workarounds.
- **Regression evidence:** The
  `normalization_borrows_projections_of_reference_parameters` unit test and
  `tests/forge_example.rs`.
- **Priority:** Release blocking

### FRG-003 — Nested package examples were absent from the compiler corpus

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Test isolation
- **Forge need:** Keep every Forge module compiling as the language evolves.
- **Why Forma was insufficient:** This is repository tooling rather than a Forma
  program capability. The corpus test only found top-level example files and
  skipped package-shaped examples such as `examples/forge/src/`.
- **Rust used:** Recursive example discovery in `tests/corpus_tests.rs`.
- **Current workaround:** None; the shared compiler corpus now includes nested
  Forma files.
- **Forma improvement:** A future package-aware test command could replace
  repository-specific discovery.
- **Removal criterion:** Met for 0.2: all nested `.forma` modules enter the
  shared compiler pipeline.
- **Regression evidence:** `cargo test --test corpus_tests`.
- **Priority:** High

### FRG-009 — Mutable projected assignment through a reference

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Language runtime
- **Forge need:** Update invariant-bearing workflow state through a mutable
  reference without bypassing aggregate validation.
- **Why Forma was insufficient:** MIR assignment resolved the reference local
  itself rather than the referenced projected place for `state.field = value`.
- **Rust used:** Projected-place assignment in `src/mir/interp.rs` resolves
  reference bindings before mutation and rechecks struct invariants.
- **Current workaround:** None.
- **Forma improvement:** Mutable reference projection now follows the same
  ownership and invariant boundary as direct aggregate mutation.
- **Removal criterion:** Met.
- **Regression evidence:** Mutable-borrow struct-invariant interpreter tests and
  Forge transition execution.
- **Priority:** Release blocking

### FRG-010 — Canonical workspace paths and structured child processes

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Runtime
- **Forge need:** Let agents inspect and modify a scoped workspace and run
  verification commands without traversal, symlink escape, shell injection,
  inherited secrets, unbounded output, or an unbounded process lifetime.
- **Why Forma was insufficient:** Lexical path checks could not contain
  symlinks, and the general process API was shell-oriented and unbounded.
- **Rust used:** `path_resolve_within` and `process_run` are capability-gated
  builtins implemented in the interpreter and registered in the type surface.
- **Current workaround:** None. Forge passes an exact executable, JSON argument
  array, canonical cwd, empty environment, allowlist, timeout, and output cap.
- **Forma improvement:** These are provider-neutral hosted primitives rather
  than Forge-specific adapters.
- **Removal criterion:** Met.
- **Regression evidence:** Symlink-escape, allowlist, environment, timeout, and
  output-bound tests plus the Forge black-box check.
- **Priority:** Release blocking

### FRG-011 — Durable workflow replay and safe restart resume

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Standard library/application architecture
- **Forge need:** Recover a paused or interrupted graph from durable evidence
  without restarting completed work.
- **Why Forma was insufficient:** No runtime change was required; Forge had
  persisted messages but not its transition journal.
- **Rust used:** None in the implementation. The accepted FRG-002 black-box
  boundary validates the packaged program.
- **Current workaround:** None. Forge persists ordered events, replays them, and
  refuses unsafe resume when the complete settings or exact graph-content
  digest differs.
- **Forma improvement:** Demonstrates that restart recovery can remain ordinary
  Forma application logic.
- **Removal criterion:** Met.
- **Regression evidence:** Offline event round-trip/replay checks and Forge CLI
  type/corpus tests.
- **Priority:** Release blocking

### FRG-012 — Spawn evaluated work synchronously

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Language runtime
- **Forge need:** Execute independent ready graph nodes concurrently under a
  worker bound.
- **Why Forma was insufficient:** MIR lowering evaluated `sp work()` in the
  parent and spawned only its completed value, so value-only concurrency tests
  passed while no work overlapped.
- **Rust used:** Spawn lowering lifts the inner expression into a zero-argument
  closure executed by the child interpreter. SQLite connections receive a
  finite busy timeout for bounded concurrent writers.
- **Current workaround:** None. Forge batches up to `max_parallel_nodes` and
  joins every started result into coordinator-owned state.
- **Forma improvement:** `sp` now implements its documented deferred execution
  semantics.
- **Removal criterion:** Met.
- **Regression evidence:** A timing regression proves two 200 ms calls overlap;
  existing task/corpus tests and Forge checks remain green.
- **Priority:** Release blocking

### FRG-013 — Backend-neutral prepared SQL placeholders

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** Runtime
- **Forge need:** Use one safe prepared-query implementation for local SQLite
  and remote PostgreSQL history.
- **Why Forma was insufficient:** SQLite accepted `?` while PostgreSQL required
  `$1`; the advertised backend-neutral `Database` API did not normalize them.
- **Rust used:** PostgreSQL prepared paths translate unquoted `?` placeholders
  to `$n`, preserving quoted text/comments and using `??` for a literal
  PostgreSQL question-mark operator.
- **Current workaround:** None.
- **Forma improvement:** Prepared Forma SQL uses `?` consistently across both
  backends.
- **Removal criterion:** Met.
- **Regression evidence:** Quote/comment/operator translation tests and an
  environment-gated live PostgreSQL Forge round trip.
- **Priority:** High

### FRG-014 — Compiler stack depended on the embedding thread

- **Status:** Resolved
- **Discovered:** 2026-07-27
- **Area:** CLI/LSP/compiler API
- **Forge need:** Compile the production-shaped Forge package consistently from
  the CLI, tests, and language server.
- **Why Forma was insufficient:** Recursive compiler phases inherited the
  caller's thread stack. Forge compiled on the main CLI stack but overflowed a
  standard 2 MiB Rust test-worker stack, implying the same risk for editor and
  embedded callers.
- **Rust used:** `CompilerSession::compile_registered` runs the canonical
  pipeline in a scoped, named compiler worker with a defined 8 MiB stack and
  preserves diagnostics and panics.
- **Current workaround:** None; stack policy is now owned by the compiler API
  instead of every caller.
- **Forma improvement:** Compiler resource requirements are stable across
  frontends.
- **Removal criterion:** Met.
- **Regression evidence:** Forge compiles with its parent process limited to a
  2 MiB stack and the recursive repository corpus passes under the default test
  runner.
- **Priority:** Release blocking

## Accepted host boundaries

### FRG-002 — Black-box process and exit-status assertion

- **Status:** Accepted host boundary
- **Discovered:** 2026-07-27
- **Area:** Test isolation
- **Forge need:** Prove in CI that the packaged Forma executable can check and
  run Forge without ambient capabilities, and that its process exits
  successfully with the expected public summary.
- **Why Forma was insufficient:** A program cannot provide independent
  black-box evidence about the executable and process hosting that same program.
- **Rust used:** `tests/forge_example.rs` launches the built Forma CLI, captures
  output and status, and makes no workflow-domain decisions.
- **Current workaround:** Not a workaround; this is the outermost test boundary.
- **Forma improvement:** None required. A generic CLI snapshot harness could
  later remove the small test-specific Rust file.
- **Removal criterion:** Replace only when repository infrastructure offers
  equivalent process isolation and assertions without Forge-specific host code.
- **Regression evidence:** `cargo test --test forge_example`.
- **Priority:** High
