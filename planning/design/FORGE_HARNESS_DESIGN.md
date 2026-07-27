# Forge Harness and Workflow-Graph Design

## Purpose

Forge is the production-shaped Forma example for agentic engineering. Its
application, domain model, live provider adapter, memory system, fixtures,
assertions, and tool loop are written in Forma. A thin Rust black-box runner is
permitted only for host isolation and operating-system behavior that Forma
cannot yet express.
Every such use is recorded in `FORGE_RUST_GAP_LEDGER.md`.

The harness tests the same workflow kernel used by production execution. It does
not maintain a test-only scheduler or a second implementation of workflow
semantics.

## Workflow definition

A workflow is a validated directed acyclic graph:

```text
WorkflowGraph
  nodes: [ProcessNode]
  edges: [ProcessEdge]
  policy: RunPolicy
```

Each node declares:

- a stable node identifier;
- its agent or tool role;
- typed input and output artifact requirements;
- required capabilities;
- timeout and retry policy;
- completion and validation contracts;
- whether failure is required, optional, or recoverable.

Each edge declares:

- source and destination nodes;
- the dependency condition;
- artifact mappings;
- an optional deterministic predicate over completed source data.

The initial edge conditions are:

- `RequiresSuccess`: the destination requires successful source completion;
- `RequiresTerminal`: the destination waits for any terminal source outcome;
- `OnSuccess`: the destination branch is selected on success;
- `OnFailure`: the destination branch is selected on failure.

Retries are node-attempt policy, not back edges. Repeated or iterative work is
represented by a bounded, revisioned repair session rather than by admitting
arbitrary cycles in the workflow definition.

## Self-correcting verification

Verification findings are engineering evidence, not terminal workflow failures.
A self-correcting node owns a progress-sensitive repair policy. Its definition
declares attempts per strategy and the number of autonomous strategy changes.
Runtime state records the selected strategy, verification evidence, immutable
artifact revision, and latest usable artifact.

The conceptual correction loop is represented as an append-only execution
history:

```text
verify revision 1
  -> diagnose findings 1
  -> repair revision 2
  -> verify revision 2
  -> diagnose findings 2
  -> repair revision 3
  -> verify revision 3
```

This preserves the immutable connected-DAG definition: each concrete revision
and verification attempt points forward to a new execution identity. The
runtime never mutates or reconnects an earlier node.

Correction outcomes are:

- `passed`: return the verified revision to the main workflow;
- `findings`: request another repair while progress or strategy budget remains;
- `infrastructure_error`: retry verification without consuming a repair;
- `blocked`: preserve evidence and enter `awaiting_intervention`;
- stagnant strategies exhausted: preserve the latest revision and enter
  `awaiting_intervention`.

An intervention contributes an explicit context artifact and starts a new
strategy epoch. The existing revision, evidence, event history, and capability
boundary remain intact; the workflow continues from preserved state rather than
starting over.

Progress means a lower failure count, lower severity, or a changed failure
fingerprint. Progress renews the per-strategy budget. Identical findings after
the budget is consumed select the next declared strategy. Exhaustion never
discards work and never grants additional capabilities automatically.

Hard failure is reserved for invalid workflow state, corrupt evidence, or a
safety invariant violation. Ordinary verification findings route toward repair,
replanning, or a resumable pause.

## Connectivity and validation

A submitted graph must satisfy all of these conditions before execution:

1. node identifiers are unique;
2. every edge endpoint exists;
3. the graph is acyclic;
4. every node is reachable from at least one root;
5. every node can reach a terminal outcome;
6. the graph is weakly connected, or is normalized under one synthetic run root;
7. every required input has one unambiguous producer or run input;
8. node capabilities are subsets of workflow authority;
9. retry counts, worker bounds, and deadlines are finite;
10. conditional branches do not leave required work permanently ambiguous.

Multiple roots are allowed for parallel discovery work. Internally they share a
synthetic root so one run always has one connected process graph.

Validation returns structured diagnostics tied to node and edge identifiers.
Invalid graphs never enter the scheduler or event journal.

## Execution model

The semantic center of Forge is a pure transition function:

```text
step(current_state, input_event) -> Transition

Transition
  state: WorkflowState
  effects: [DeclaredEffect]
  events: [AuditEvent]
```

The kernel never reads wall time, generates identifiers or randomness, starts
tasks, calls agents, writes files, or accesses SQLite. It declares effects. An
effect interpreter performs them and returns later input events.

The scheduler derives ready nodes from graph dependencies and launches a
bounded batch with Forma's affine `sp`/`await_all` task handles. Each worker
opens its own history connection; SQLite uses a busy timeout and remote workers
use PostgreSQL. The coordinator alone applies graph transitions and joins every
started result before returning a pause. Runtime state is kept separately from
the immutable graph definition. This separation makes graph validation,
replay, visualization, and formal reasoning possible without hosted effects.

## Harness architecture

Forge has one production execution path:

- an immutable validated graph selects ready roles;
- role settings select the model, prompt, budget, and permitted tools;
- a hosted provider returns strict structured results or one tool request;
- a manifest-driven plugin catalog restricts each role's structured tool-name
  schema to the custom Forma tools it is granted;
- the Forma adapter checks tool authority, starts the plugin's colocated
  `tool.forma` with manifest-declared capabilities and environment names, and
  records every exchange;
- SQLite locally or PostgreSQL remotely supplies recent recall, append-only
  events, token accounting, and durable compacted memory;
- the pure kernel accepts the resulting completion, finding, repair, or pause.

There is deliberately no scripted agent implementation. Offline tests validate
protocol parsing, settings, storage, recall, compaction watermarks, graph
invariants, and repair transitions without claiming that generated prose is a
model response.

## Test layers

1. Pure Forma transition and graph-validation tests.
2. Offline Forma infrastructure tests for settings, structured protocol
   parsing, tool policy, SQLite history, recall, and compaction.
3. Forma Hosted integration tests for canonical workspace paths, bounded direct
   processes, authenticated HTTP, SQLite, deadlines, and restart recovery.
4. A thin Rust black-box runner for temporary workspace isolation, child-process
   control, exit status, and CI-level assertions that Forma cannot yet perform.
5. Optional live-provider tests that are credential-gated, network-gated,
   budgeted, non-CI, and evaluated by structural invariants rather than exact
   prose.

Normal CI is offline and deterministic because it tests infrastructure, not a
fake agent. It grants the minimum capability set and never uses `--allow-all`.

## Tool plugin boundary

Every tool is a folder containing a versioned `tool.toml` and its
`tool.forma` implementation. Discovery is deterministic and validates unique
safe names, a fixed source entrypoint, supported capabilities, explicitly named
environment variables, and source presence. A role grant selects which plugin
names appear in that role's strict provider schema.

Forge starts a plugin with the configured Forma interpreter, an empty inherited
environment, only the manifest's named environment values, and only its
declared `read`, `write`, `network`, or `exec` capabilities. The three-argument
protocol carries model input, model payload, and bounded non-secret execution
context. Plugin manifests and source digests participate in the execution
digest, so implementation or contract changes cannot silently alter a resumed
run.

This is a trusted extension boundary rather than a hostile-code sandbox.
Untrusted plugins require operating-system isolation in addition to Forma's
capability checks.

## Evidence and oracles

Every transition emits an append-only audit event containing sequence, logical
time, run identity, optional node and attempt identities, kind, reason, relevant
capability information, artifact identities, and safe payload digests.

The harness checks:

- graph and state invariants;
- required and forbidden events;
- causal event ordering;
- terminal state and artifacts;
- append-only event sequence integrity;
- replay equivalence;
- effect resolution, timeout, or cancellation;
- absence of secret values in recorded payloads.

Golden traces are reserved for a small number of narrative demonstrations.
Most scenarios assert semantic invariants and partial orders so harmless
scheduling changes do not rewrite fixtures.

## First vertical slice

The executable workflow contains repository inventory, parallel
architecture and test review, plan synthesis, implementation, validation,
iterative repair, publication guidance, and a release decision. It persists
prompts, responses, tool evidence, token usage, compacted memory, and resumable
status in SQLite or PostgreSQL. A live run is evaluated through structural
invariants and evidence, never exact prose.

Provider and process operations have individual timeouts. The workflow deadline
and total token budget are enforced before each new effect and pause durably
when exhausted. Cancellation is cooperative at these bounded effect
boundaries; Forge does not claim to preempt a blocking host call mid-request.

## Authoring format decision

External workflow definitions should be JSON so agents, tools, and users can
generate them without recompiling Forge. Built-in demonstrations and tests should
also have typed Forma graph builders. Both forms normalize into the same
`WorkflowGraph` and pass through the same validator.

A separate workflow DSL is deferred until real examples demonstrate that JSON
and typed builders are insufficient.
