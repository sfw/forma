# Installing FORMA

## Prerequisites

- **Rust** 1.85+ (edition 2024) — [rustup.rs](https://rustup.rs)

For native compilation (optional):
- **LLVM 18** — required only for `forma build` (the `llvm` feature flag)

## Build from Source

```bash
git clone https://github.com/sfw/forma.git
cd forma
cargo build --release
```

The binary will be at `./target/release/forma`.

### With LLVM Backend (Optional)

To enable native compilation via `forma build`, you need LLVM 18 and its link-time dependencies.

**macOS (Homebrew):**

```bash
brew install llvm@18 zstd
export LLVM_SYS_180_PREFIX="$(brew --prefix llvm@18)"
export LIBRARY_PATH="$(brew --prefix)/lib:$LIBRARY_PATH"
cargo build --release --features llvm
```

**Ubuntu/Debian:**

```bash
# LLVM toolchain + link dependencies
sudo apt install llvm-18-dev libzstd-dev libpolly-18-dev

cargo build --release --features llvm
```

**Fedora/RHEL:**

```bash
sudo dnf install llvm18-devel libzstd-devel

cargo build --release --features llvm
```

**Verify the LLVM build:**

```bash
# Should complete with zero warnings
cargo check --features llvm
```

## Verify Installation

```bash
# Check version
./target/release/forma --version

# Run a program
echo 'f main()
    print("Hello, FORMA!")' > hello.forma
./target/release/forma run hello.forma

# Show contract explanations
./target/release/forma explain hello.forma --format human

# Emit machine-readable diagnostics
./target/release/forma check --error-format json hello.forma

# Run the showcase examples
for f in examples/showcase/*.forma; do
    echo "=== $(basename $f) ==="
    ./target/release/forma run "$f"
done
```

## Add to PATH

```bash
# Option 1: Symlink
sudo ln -s $(pwd)/target/release/forma /usr/local/bin/forma

# Option 2: Add to shell profile
echo 'export PATH="$PATH:/path/to/forma/target/release"' >> ~/.zshrc
```

## IDE Support

FORMA includes a built-in LSP server for editor integration:

```bash
forma lsp
```

Configure your editor's LSP client to use `forma lsp` as the language server command.

## Running Tests

```bash
cargo test
```
