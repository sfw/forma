# FORMA Language Support for VS Code

Syntax highlighting and language-server setup for the Forma 0.2 prototype.

## Features

- Syntax highlighting for `.forma` files
- Comment toggling (`#` comments)
- Bracket matching and auto-closing
- Code folding based on indentation (FORMA uses significant whitespace)
- Optional semantic diagnostics, hover, completion, definitions, references,
  symbols, signatures, and formatting through `forma lsp`

The TextMate grammar supplies highlighting only. Generated keyword/editor metadata
in `docs/grammar-keywords.md` and `docs/editor-grammar.json` is the authoritative
syntax catalog.

## Installation

### Method 1: Copy to Extensions Directory

1. Find your VS Code extensions directory:
   - **macOS**: `~/.vscode/extensions/`
   - **Linux**: `~/.vscode/extensions/`
   - **Windows**: `%USERPROFILE%\.vscode\extensions\`

2. Copy this entire `vscode` folder to the extensions directory:
   ```bash
   cp -r editors/vscode ~/.vscode/extensions/forma-language
   ```

3. Restart VS Code

### Method 2: Symbolic Link (Development)

For active development, you can create a symbolic link:

```bash
# macOS/Linux
ln -s "$(pwd)/editors/vscode" ~/.vscode/extensions/forma-language

# Windows (PowerShell, run as admin)
New-Item -ItemType SymbolicLink -Path "$env:USERPROFILE\.vscode\extensions\forma-language" -Target ".\editors\vscode"
```

### Method 3: Package as VSIX

To create a distributable extension:

```bash
cd editors/vscode
npm install -g @vscode/vsce
vsce package
```

Then install the generated `.vsix` file in VS Code via:
- Extensions view → "..." menu → "Install from VSIX..."

## Supported Syntax

### Keywords
- Declaration: `f` (function), `s` (struct), `e` (enum), `t` (trait), `i` (impl), `m` (match)
- Control: `if`, `then`, `else`, `for`, `in`, `wh` (while), `lp` (loop), `br` (break), `ct` (continue), `ret` (return)
- Modifiers and ownership: `pub`, `mut`, `ref`, `mv` (move), `un` (unsafe), `as`
  (async), `aw` (await), `sp` (spawn), `us` (use), `md` (module)

### Types
- Primitives: `Int`, `Float`, `Bool`, `Str`, `Char`, `Unit`, `Never`
- Integer types: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `isize`, `usize`
- Float types: `f32`, `f64`
- Builtins: `Option`, `Result`, `Vec`, `Map`

### Literals
- Booleans: `true`, `false`, `T`, `F`
- None: `none`, `N`
- Constructors: `Some`, `Ok`, `Err`

### Comments
```forma
# This is a comment
```

### Strings
```forma
"Hello, world!"
f"Hello {name}!"  # f-string interpolation
'c'               # character literal
```

## Example

```forma
# Fibonacci function
f fib(n: Int) -> Int
    if n <= 1 then n else fib(n - 1) + fib(n - 2)

f main() -> Int
    result = fib(10)
    print(f"Fibonacci(10) = {result}")
    0
```

## Language Server

Build the compiler, then configure a VS Code LSP client to start:

```bash
/absolute/path/to/forma lsp
```

The repository extension does not bundle the compiler. LSP support is currently
strongest for a single file; richer cross-file refactoring and rename workflows
remain future work.

## License

MIT
