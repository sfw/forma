---
name: "forma-codex"
description: "Guidance for using and developing FORMA in this repository. Trigger when working on FORMA (.forma) programs, using FORMA CLI tools (`run`, `check`, `fmt`, `explain`, `verify`, `grammar`, `typeof`, `complete`, `lsp`), or changing compiler/runtime behavior in this codebase."
---

# FORMA Codex

## Goal

Use this skill to produce correct FORMA programs and safe FORMA compiler/runtime changes with fast feedback loops.

## Repo Anchors

- `docs/ai-reference.md`: fastest syntax, builtins, and CLI lookup
- `docs/reference.md`: full language and tooling reference
- `README.md`: project overview, security model, AI-oriented tooling
- `CONTRIBUTING.md`: contributor workflow and quality gates

## Code Layout

- `src/main.rs`: CLI entrypoint and command wiring
- `src/lexer`, `src/parser`, `src/types`, `src/borrow`, `src/mir`, `src/fmt`, `src/lsp`: compiler pipeline modules
- `runtime/`: runtime crate
- `std/`: FORMA standard library modules
- `tests/*.rs`: Rust unit/integration tests
- `tests/forma/`, `tests/fixtures/`: FORMA integration tests and fixtures
- `examples/`, `examples/showcase/`: runnable examples

## FORMA CLI Usage

For local development in this repo, prefer `cargo run -- ...` to run the in-tree compiler without a release build.

```bash
cargo run -- run path/to/file.forma
cargo run -- check path/to/file.forma --error-format json
cargo run -- explain path/to/file.forma --format json --examples=3 --seed 42
cargo run -- verify path/or/dir --report --format json --examples 20 --seed 42
cargo run -- fmt path/to/file.forma
cargo run -- grammar --format ebnf
cargo run -- typeof path/to/file.forma --position "5:10"
cargo run -- complete path/to/file.forma --position "5:10"
cargo run -- lsp
```

For repeated CLI runs against a built binary:

```bash
cargo build --release
./target/release/forma run examples/showcase/01_hello_world.forma
```

## Capability Safety Rules

- Treat `--allow-all` as full host access
- Prefer least-privilege flags: `--allow-read`, `--allow-write`, `--allow-network`, `--allow-exec`, `--allow-env`, `--allow-unsafe`
- `verify` is side-effect-safe by default; only use `--allow-side-effects` when explicitly needed and trusted
- Never run untrusted FORMA code with broad capability flags

## Compiler Development Loop

1. Read only the relevant module(s) first.
2. Apply a minimal patch.
3. Run targeted tests first.
4. Run full checks before finishing.

```bash
cargo test <test_name_or_module>
cargo test
cargo clippy -- -D warnings
cargo fmt --all
```

For contract-tooling changes:

```bash
scripts/test_contracts.sh
```

For quick language regression sanity checks:

```bash
for f in examples/showcase/*.forma; do cargo run --quiet -- run "$f"; done
```

## Task Playbooks

### Write or Fix `.forma` Programs

1. Start from `docs/ai-reference.md` patterns.
2. Draft code with explicit types where ambiguity is likely.
3. Run `check` with JSON diagnostics and fix deterministically.
4. Run with minimal required capability flags.
5. If contracts exist, run `explain` and `verify` to produce evidence.

### Change Language Behavior

1. Update the relevant module under `src/`.
2. Add or update Rust tests in `tests/*.rs`.
3. Add or update `.forma` fixtures in `tests/forma/` or `tests/fixtures/` for end-to-end behavior.
4. Re-run targeted tests, then full test/quality gates.

### Improve AI-Assisted Development Workflow

- Use `grammar` for constrained generation artifacts.
- Use `check --error-format json` for machine-fix loops.
- Use `typeof` and `complete` for position-aware reasoning.
- Use `explain` and `verify --report` for trust artifacts in reviews/CI.

## Output Requirements

- Include exact commands run and key results.
- Include required capability flags when runtime execution is suggested.
- Reference concrete files and tests touched.
- Prefer small, reviewable diffs over broad refactors.
