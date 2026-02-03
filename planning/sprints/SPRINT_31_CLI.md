# Sprint 31: CLI/REPL Improvements

**Goal:** Improve developer experience with better CLI and REPL features
**Estimated Effort:** 3-4 hours

---

## Overview

The CLI and REPL (`src/main.rs`) need 5 improvements:
1. REPL multi-line input
2. REPL definition detection for all types
3. REPL `:load` command
4. CLI program args passing
5. CLI `compile` command alias

---

## Task 31.1: REPL Multi-Line Input

**File:** `src/main.rs` (REPL section)

**Current Behavior:** Each line is treated as complete input.

**Problem:** Can't enter multi-line expressions like:
```forma
let result = if condition
    then foo()
    else bar()
```

**Fix:** Detect incomplete expressions and continue reading:

```rust
fn is_complete_input(input: &str) -> bool {
    // Check for balanced brackets
    let mut paren_depth = 0i32;
    let mut bracket_depth = 0i32;
    let mut brace_depth = 0i32;
    let mut in_string = false;
    let mut in_fstring = false;
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '"' if !in_string => in_string = true,
            '"' if in_string => in_string = false,
            '(' if !in_string => paren_depth += 1,
            ')' if !in_string => paren_depth -= 1,
            '[' if !in_string => bracket_depth += 1,
            ']' if !in_string => bracket_depth -= 1,
            '{' if !in_string => brace_depth += 1,
            '}' if !in_string => brace_depth -= 1,
            _ => {}
        }
    }

    // Check for trailing keywords that expect continuation
    let trimmed = input.trim_end();
    let ends_incomplete = trimmed.ends_with("then")
        || trimmed.ends_with("else")
        || trimmed.ends_with("->")
        || trimmed.ends_with("=")
        || trimmed.ends_with("\\");

    !in_string
        && paren_depth == 0
        && bracket_depth == 0
        && brace_depth == 0
        && !ends_incomplete
}

// In REPL loop:
fn read_complete_input(rl: &mut Editor<()>) -> Option<String> {
    let mut input = String::new();
    let mut prompt = ">>> ";

    loop {
        match rl.readline(prompt) {
            Ok(line) => {
                if !input.is_empty() {
                    input.push('\n');
                }
                input.push_str(&line);

                if is_complete_input(&input) {
                    return Some(input);
                }
                prompt = "... ";  // Continuation prompt
            }
            Err(_) => return None,
        }
    }
}
```

---

## Task 31.2: REPL Definition Detection

**File:** `src/main.rs` (REPL section)

**Current Code:**
```rust
// Check if this is a definition (f, struct, enum, etc)
let is_def = line.starts_with("f ")
    || line.starts_with("struct ")
    || line.starts_with("enum ");
```

**Problem:** Missing `pub f`, `async f`, `const`, `type`, `impl`, `trait`.

**Fix:** Expand detection:
```rust
fn is_definition(input: &str) -> bool {
    let trimmed = input.trim();

    // Function definitions (including modifiers)
    trimmed.starts_with("f ")
        || trimmed.starts_with("pub f ")
        || trimmed.starts_with("async f ")
        || trimmed.starts_with("pub async f ")

    // Type definitions
        || trimmed.starts_with("struct ")
        || trimmed.starts_with("pub struct ")
        || trimmed.starts_with("enum ")
        || trimmed.starts_with("pub enum ")
        || trimmed.starts_with("type ")
        || trimmed.starts_with("pub type ")

    // Constants
        || trimmed.starts_with("const ")
        || trimmed.starts_with("pub const ")

    // Trait/impl
        || trimmed.starts_with("trait ")
        || trimmed.starts_with("pub trait ")
        || trimmed.starts_with("impl ")

    // Module
        || trimmed.starts_with("mod ")
        || trimmed.starts_with("pub mod ")

    // Use/import
        || trimmed.starts_with("use ")
}
```

---

## Task 31.3: REPL :load Command

**File:** `src/main.rs` (REPL section)

**Current:** No file loading in REPL.

**Fix:** Add `:load` command handling:

```rust
fn handle_repl_command(
    input: &str,
    interp: &mut Interpreter,
    loaded_files: &mut Vec<PathBuf>,
) -> CommandResult {
    let parts: Vec<&str> = input.trim().splitn(2, ' ').collect();

    match parts[0] {
        ":load" | ":l" => {
            if parts.len() < 2 {
                return CommandResult::Error("Usage: :load <filename>".into());
            }
            let path = PathBuf::from(parts[1]);
            match load_file(&path, interp) {
                Ok(()) => {
                    loaded_files.push(path.clone());
                    CommandResult::Ok(format!("Loaded: {}", path.display()))
                }
                Err(e) => CommandResult::Error(format!("Load failed: {}", e)),
            }
        }

        ":reload" | ":r" => {
            // Reload all previously loaded files
            for path in loaded_files.clone() {
                if let Err(e) = load_file(&path, interp) {
                    return CommandResult::Error(format!(
                        "Reload failed for {}: {}", path.display(), e
                    ));
                }
            }
            CommandResult::Ok(format!("Reloaded {} files", loaded_files.len()))
        }

        ":type" | ":t" => {
            if parts.len() < 2 {
                return CommandResult::Error("Usage: :type <expression>".into());
            }
            match infer_type(parts[1], interp) {
                Ok(ty) => CommandResult::Ok(format!("{}: {}", parts[1], ty)),
                Err(e) => CommandResult::Error(format!("Type error: {}", e)),
            }
        }

        ":help" | ":h" | ":?" => {
            CommandResult::Ok(REPL_HELP.to_string())
        }

        ":quit" | ":q" => CommandResult::Quit,

        _ => CommandResult::NotCommand,
    }
}

fn load_file(path: &Path, interp: &mut Interpreter) -> Result<(), String> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| format!("Cannot read file: {}", e))?;

    let program = parse_and_lower(&source)
        .map_err(|e| format!("Parse error: {}", e))?;

    interp.load_program(program)
        .map_err(|e| format!("Load error: {}", e))
}

const REPL_HELP: &str = r#"
FORMA REPL Commands:
  :load <file>   Load a FORMA file
  :reload        Reload all loaded files
  :type <expr>   Show type of expression
  :help          Show this help
  :quit          Exit REPL

Shortcuts: :l, :r, :t, :h, :q
"#;
```

---

## Task 31.4: CLI Program Args Passing

**File:** `src/main.rs` (CLI argument parsing)

**Current:** Program arguments not passed to FORMA program.

**Fix:** Pass args after `--` to the program:

```rust
// In CLI arg parsing:
fn parse_cli_args() -> CliArgs {
    let args: Vec<String> = std::env::args().collect();

    // Find -- separator
    let separator_pos = args.iter().position(|a| a == "--");

    let (cli_args, program_args) = match separator_pos {
        Some(pos) => (&args[1..pos], &args[pos+1..]),
        None => (&args[1..], &[] as &[String]),
    };

    // Parse cli_args...
    CliArgs {
        command: parse_command(cli_args),
        program_args: program_args.to_vec(),
    }
}

// When running program:
fn run_program(path: &Path, program_args: &[String]) -> Result<i64, Error> {
    let interp = Interpreter::new();

    // Set program arguments
    interp.set_env_var("ARGV", format_args(program_args));
    interp.set_env_var("ARGC", program_args.len().to_string());

    // Also make available via function
    // args() -> [String]

    interp.run_file(path)
}
```

**Usage:**
```bash
forma run program.forma -- arg1 arg2 arg3
```

**In FORMA:**
```forma
f main() -> Int
    let args = args()
    print(f"Got {len(args)} arguments")
    for arg in args
        print(arg)
    0
```

---

## Task 31.5: CLI compile Command

**File:** `src/main.rs` (CLI commands)

**Current:** Only `build` command exists for LLVM compilation.

**Fix:** Add `compile` as alias:

```rust
enum Command {
    Run(PathBuf),
    Build(PathBuf),
    Compile(PathBuf),  // Alias for Build
    Check(PathBuf),
    Repl,
    Help,
    Version,
}

fn parse_command(args: &[String]) -> Command {
    match args.get(0).map(String::as_str) {
        Some("run") => Command::Run(PathBuf::from(&args[1])),
        Some("build") | Some("compile") => {
            Command::Build(PathBuf::from(&args[1]))
        }
        Some("check") => Command::Check(PathBuf::from(&args[1])),
        Some("repl") | None => Command::Repl,
        Some("--help") | Some("-h") | Some("help") => Command::Help,
        Some("--version") | Some("-V") => Command::Version,
        _ => Command::Help,
    }
}

const HELP_TEXT: &str = r#"
FORMA - AI-Optimized Programming Language

USAGE:
    forma <command> [options] [file] [-- program-args]

COMMANDS:
    run <file>       Interpret a FORMA file
    build <file>     Compile to native executable via LLVM
    compile <file>   Alias for build
    check <file>     Type-check without running
    repl             Start interactive REPL

OPTIONS:
    -h, --help       Show this help
    -V, --version    Show version

EXAMPLES:
    forma run hello.forma
    forma build app.forma
    forma run server.forma -- --port 8080
    forma repl
"#;
```

---

## Verification

After implementing all tasks:

1. Run the test suite:
```bash
cd forma && cargo test
```

2. Test REPL features:
```bash
cargo run -- repl

# Multi-line input (31.1)
>>> let x = if true
...     then 1
...     else 2
x = 1

# Definition types (31.2)
>>> pub async f fetch(url: String) -> String
...     # ... implementation
>>> trait Printable
...     f to_string(self) -> String

# :load command (31.3)
>>> :load examples/fibonacci.forma
Loaded: examples/fibonacci.forma
>>> fib(10)
55

>>> :type fib
fib: (Int) -> Int

# :help
>>> :help
FORMA REPL Commands:
...
```

3. Test CLI args (31.4):
```bash
# Create test file
echo 'f main() -> Int
    let args = args()
    print(f"Args: {args}")
    0' > test_args.forma

cargo run -- run test_args.forma -- hello world
# Output: Args: ["hello", "world"]
```

4. Test compile alias (31.5):
```bash
cargo run -- compile examples/hello.forma
# Same as: cargo run -- build examples/hello.forma
```

---

## Summary

| Task | Description | File | Priority |
|------|-------------|------|----------|
| 31.1 | Multi-line input | main.rs | Medium |
| 31.2 | Definition detection | main.rs | Low |
| 31.3 | :load command | main.rs | High |
| 31.4 | Program args passing | main.rs | Medium |
| 31.5 | compile alias | main.rs | Low |

**Dependencies:** None (standalone CLI changes)

---

## Claude Code Prompt

```
Sprint 31: CLI/REPL Improvements for FORMA

Working directory: forma/

## Tasks

### 31.1: REPL Multi-Line Input
File: src/main.rs
Add is_complete_input(input: &str) -> bool function that checks:
- Balanced parentheses, brackets, braces
- Not ending with: then, else, ->, =, \
- Not in the middle of a string

Modify REPL loop to accumulate lines until complete.
Use ">>> " for first line, "... " for continuation.

### 31.2: REPL Definition Detection
File: src/main.rs
Expand the definition detection to include:
- pub f, async f, pub async f
- pub struct, pub enum
- type, pub type
- const, pub const
- trait, pub trait, impl
- mod, pub mod
- use

### 31.3: REPL :load Command
File: src/main.rs
Add command handling for:
- :load <file> - load and execute file
- :reload - reload all previously loaded files
- :type <expr> - show type of expression
- :help - show help
- :quit - exit

Track loaded files in Vec<PathBuf> for :reload.

### 31.4: CLI Program Args Passing
File: src/main.rs
Find "--" in args to separate CLI args from program args.
Pass program args to interpreter via:
- ARGV env var (space-separated)
- ARGC env var (count)
- args() builtin function returning [String]

### 31.5: CLI compile Command
File: src/main.rs
Add "compile" as an alias for "build" command.
Update help text to show both commands.

## Testing
After changes:
1. cargo test
2. Test REPL multi-line: enter if-then-else across lines
3. Test :load examples/fibonacci.forma
4. Test: forma run test.forma -- arg1 arg2
5. Test: forma compile hello.forma (same as build)

All 251+ existing tests must continue to pass.
```
