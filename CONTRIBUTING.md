# Contributing to FORMA

Thank you for your interest in contributing to FORMA! This document provides guidelines for contributing to the project.

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

## Development Workflow

1. Create a feature branch from `main`
2. Make your changes
3. Ensure all tests pass: `cargo test`
4. Ensure no clippy warnings: `cargo clippy`
5. Format your code: `cargo fmt`
6. Run the showcase examples:
   ```bash
   for f in examples/showcase/*.forma; do cargo run --quiet -- run "$f"; done
   ```
7. Submit a pull request

## What to Contribute

- Bug fixes
- New builtin functions
- Improved error messages
- Documentation improvements
- New showcase examples
- IDE support (LSP improvements, editor plugins)
- Performance improvements

## Code Style

- Follow existing patterns in the codebase
- Run `cargo fmt` before committing
- No clippy warnings (`cargo clippy`)
- No compiler warnings
- Add tests for new functionality

## Reporting Bugs

Open an issue at https://github.com/sfw/forma/issues with:

- A minimal FORMA program that reproduces the bug
- Expected vs actual behavior
- The output of `forma --version`

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
