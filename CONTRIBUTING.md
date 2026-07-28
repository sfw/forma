# Contributing to FORMA

Thank you for helping evaluate and improve Forma 0.2. Contributions may challenge
the design as well as extend it.

## Before You Change Semantics

The normative baseline is `planning/design/FORMA_0_2_SEMANTICS.md`. Feature and
backend claims are defined in `docs/profiles.md`. Generated grammar and builtin
metadata are authoritative for their respective surfaces. Historical research,
review, and sprint files are context, not current specification.

A semantic proposal should explain its effect on ownership, effects/capabilities,
concurrency, verification, profiles, diagnostics, and every backend—not only its
parser shape.

## Getting Started

1. Fork the repository
2. Clone your fork:
   ```bash
   git clone https://github.com/your-username/forma.git
   cd forma
   ```
3. Build and test:
   ```bash
   cargo build
   cargo test
   ```
4. Install the repository hooks:
   ```bash
   python3 -m pip install pre-commit
   pre-commit install
   ```

The pinned TruffleHog hook scans local Git changes for verified secrets before
each commit and fails the commit when it finds one. Its first run downloads and
builds the pinned scanner, so it can take longer than later runs. Confirm the
hook independently with:

```bash
pre-commit run trufflehog
```

Never bypass a finding by adding the credential to Git. Remove and rotate a
real secret before committing. Investigate false positives without exposing the
candidate value in issues, logs, or pull requests.

## Development Workflow

1. Create a feature branch from `main`
2. Make your changes
3. Ensure all tests pass: `cargo test --all-targets`
4. Ensure no clippy warnings: `cargo clippy --all-targets -- -D warnings`
5. Format your code: `cargo fmt --all -- --check`
6. Check generated artifacts and documentation: `bash scripts/check_docs.sh`
7. Preserve 100% registered-builtin behavior/error evidence:
   ```bash
   bash scripts/builtin_coverage.sh --enforce 100
   ```
8. Run the showcase examples when user-facing behavior changes:
   ```bash
   for f in examples/showcase/*.forma; do cargo run --quiet -- run "$f"; done
   ```
9. Submit a pull request

## What to Contribute

- Bug fixes
- New builtin functions
- Improved error messages
- Documentation improvements
- New showcase examples
- IDE support (LSP improvements, editor plugins)
- Performance improvements
- Agent-generation and review experiments with reproducible prompts, models,
  compiler revisions, seeds, and scoring rules
- Soundness counterexamples and capability/profile inconsistencies

## Code Style

- Follow existing patterns in the codebase
- Run `cargo fmt` before committing
- No clippy warnings (`cargo clippy`)
- No compiler warnings
- Add tests for new functionality. A new builtin must include an executed
  behavior or deterministic error-path test; name mentions and capability-only
  denials do not count as behavioral coverage.
- Update the AI reference and both Forma skills when agent-visible behavior changes
- Update profile documentation when backend or verification support changes
- Do not introduce fixed builtin counts; `docs/builtins.json` is generated

## Pull Request Evidence

Describe:

- the user-visible behavior and compatibility impact;
- affected Core, Hosted, Native, or Experimental profiles;
- ownership, capability, and verification implications;
- tests and checked `.forma` examples;
- generated artifacts or documentation that changed;
- known limitations or `UNKNOWN` results.

## Reporting Bugs

Open an issue at https://github.com/sfw/forma/issues with:

- A minimal FORMA program that reproduces the bug
- Expected vs actual behavior
- The output of `forma --version`
- The execution profile and capability flags
- The smallest reproducer, including expected verification status if relevant

Design discussions should identify the hypothesis or trade-off being evaluated.
See `docs/AGENTIC_ENGINEERING.md` for open research questions.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
