#!/usr/bin/env python3
"""Generate the website's exhaustive reference data from compiler artifacts."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_OUTPUT = ROOT.parent / "forma-website" / "reference-data.js"


TYPES = [
    {"name": "Int", "category": "Primitive", "syntax": "Int", "description": "Default signed integer"},
    {"name": "Float", "category": "Primitive", "syntax": "Float", "description": "Default floating-point number"},
    {"name": "Bool", "category": "Primitive", "syntax": "Bool", "description": "Boolean value; literals true/false or T/F"},
    {"name": "Char", "category": "Primitive", "syntax": "Char", "description": "Unicode character"},
    {"name": "Str", "category": "Hosted", "syntax": "Str", "description": "String value"},
    {"name": "Unit", "category": "Primitive", "syntax": "()", "description": "Unit value and return type"},
    {"name": "Never", "category": "Primitive", "syntax": "!", "description": "Never-returning type"},
    {"name": "Sized signed integers", "category": "Numeric", "syntax": "i8 i16 i32 i64 i128 isize", "description": "Explicit signed widths"},
    {"name": "Sized unsigned integers", "category": "Numeric", "syntax": "u8 u16 u32 u64 u128 usize", "description": "Explicit unsigned widths"},
    {"name": "Sized floats", "category": "Numeric", "syntax": "f32 f64", "description": "Explicit floating-point widths"},
    {"name": "Tuple", "category": "Composite", "syntax": "(A, B)", "description": "Fixed heterogeneous product"},
    {"name": "Fixed array", "category": "Core", "syntax": "[T; N]", "description": "Compile-time fixed-length array"},
    {"name": "Dynamic list", "category": "Hosted", "syntax": "[T]", "description": "Managed dynamic sequence"},
    {"name": "Map", "category": "Hosted", "syntax": "{K: V}", "description": "Managed key/value collection"},
    {"name": "Set", "category": "Hosted", "syntax": "{T}", "description": "Managed unique-value collection"},
    {"name": "Option", "category": "Algebraic", "syntax": "T?", "description": "Option[T] shorthand"},
    {"name": "Result", "category": "Algebraic", "syntax": "T!E", "description": "Result[T, E] shorthand"},
    {"name": "Shared reference", "category": "Ownership", "syntax": "&T / ref value: T", "description": "Read-only second-class loan"},
    {"name": "Mutable reference", "category": "Ownership", "syntax": "&mut T / ref mut value: T", "description": "Exclusive second-class loan"},
    {"name": "Function", "category": "Callable", "syntax": "(A, B) -> C", "description": "Function or closure type"},
    {"name": "Task", "category": "Hosted", "syntax": "Task[T]", "description": "Affine structured-concurrency handle"},
    {"name": "Future", "category": "Hosted", "syntax": "Future[T]", "description": "Awaitable value"},
    {"name": "Channel", "category": "Hosted", "syntax": "Sender[T] / Receiver[T]", "description": "Shared channel handles; sending moves T"},
    {"name": "Mutex", "category": "Hosted", "syntax": "Mutex[T]", "description": "Explicit shared synchronization handle"},
    {"name": "Raw pointer", "category": "Unsafe", "syntax": "*T", "description": "Unsafe pointer; requires unsafe authority"},
]


FORMS = [
    {"name": "Immutable binding", "category": "Binding", "syntax": "name = value", "description": "Creates an immutable binding"},
    {"name": "Mutable binding", "category": "Binding", "syntax": "name := value", "description": "Creates a mutable binding"},
    {"name": "Mutable update", "category": "Binding", "syntax": "name := replacement", "description": "Updates an existing mutable binding"},
    {"name": "Annotated binding", "category": "Binding", "syntax": "name: Type = value", "description": "Adds an explicit type; use := for mutable"},
    {"name": "Owned parameter", "category": "Ownership", "syntax": "value: T", "description": "Moves non-Copy arguments"},
    {"name": "Shared parameter", "category": "Ownership", "syntax": "ref value: T", "description": "Creates a shared loan"},
    {"name": "Exclusive parameter", "category": "Ownership", "syntax": "ref mut value: T", "description": "Creates an exclusive mutable loan"},
    {"name": "Explicit clone", "category": "Ownership", "syntax": "clone(value)", "description": "Duplicates a Clone value"},
    {"name": "Explicit move", "category": "Ownership", "syntax": "mv value", "description": "Forces or documents a move"},
    {"name": "Function", "category": "Declaration", "syntax": "f name(args) -> Type", "description": "Block-bodied function"},
    {"name": "Expression function", "category": "Declaration", "syntax": "f name(args) -> Type = expression", "description": "Single-expression function"},
    {"name": "Struct", "category": "Declaration", "syntax": "s Name { field: Type }", "description": "Nominal product type"},
    {"name": "Enum", "category": "Declaration", "syntax": "e Name = A | B(Type)", "description": "Nominal algebraic sum type"},
    {"name": "Trait", "category": "Declaration", "syntax": "t Name", "description": "Nominal static-dispatch interface"},
    {"name": "Implementation", "category": "Declaration", "syntax": "i Trait for Type", "description": "Trait implementation; coherence checked"},
    {"name": "Type alias", "category": "Declaration", "syntax": "type Name = Existing", "description": "Type alias"},
    {"name": "Import", "category": "Module", "syntax": "us module.path", "description": "Imports a module or explicit exports"},
    {"name": "Module", "category": "Module", "syntax": "md name", "description": "Declares a module"},
    {"name": "Match", "category": "Control", "syntax": "m value", "description": "Exhaustive pattern match over finite algebraic domains"},
    {"name": "If expression", "category": "Control", "syntax": "if condition then a else b", "description": "Conditional expression"},
    {"name": "While loop", "category": "Control", "syntax": "wh condition", "description": "Conditional loop"},
    {"name": "For loop", "category": "Control", "syntax": "for value in collection", "description": "Collection iteration"},
    {"name": "Result propagation", "category": "Error", "syntax": "operation()?", "description": "Propagates a Result error"},
    {"name": "Option default", "category": "Error", "syntax": "optional ?? default", "description": "Unwraps Option or supplies a default"},
    {"name": "Unwrap", "category": "Error", "syntax": "fallible!", "description": "Unwraps or aborts; avoid at public boundaries"},
    {"name": "Async function", "category": "Concurrency", "syntax": "as f name()", "description": "Declares an async function"},
    {"name": "Spawn", "category": "Concurrency", "syntax": "task = sp work(value)", "description": "Moves captures into a child task"},
    {"name": "Await", "category": "Concurrency", "syntax": "result = aw task", "description": "Consumes an affine task handle"},
    {"name": "Precondition", "category": "Contract", "syntax": "@pre(condition)", "description": "Required condition at function entry"},
    {"name": "Postcondition", "category": "Contract", "syntax": "@post(condition)", "description": "Guaranteed condition over result/old state"},
    {"name": "Struct invariant", "category": "Contract", "syntax": "@inv(condition[, \"message\"])", "description": "Type-wide validity rule checked at construction and observable boundaries"},
    {"name": "Generated verification", "category": "Evidence", "syntax": "--level test", "description": "Produces TESTED or a counterexample"},
    {"name": "Finite verification", "category": "Evidence", "syntax": "--level exhaustive", "description": "Produces EXHAUSTIVE for a complete supported finite domain"},
    {"name": "Formal attempt", "category": "Evidence", "syntax": "--level formal", "description": "Experimental SMT attempt; may be PROVED, COUNTEREXAMPLE, or UNKNOWN"},
]


CLI = [
    {"name": "run", "category": "Execution", "syntax": "forma run <file>", "description": "Check and interpret a program"},
    {"name": "check", "category": "Compiler", "syntax": "forma check <file>", "description": "Compile through semantic and ownership gates without execution"},
    {"name": "build", "category": "Native", "syntax": "forma build <file>", "description": "Build with the optional LLVM backend"},
    {"name": "fmt", "category": "Tooling", "syntax": "forma fmt <file>", "description": "Format source"},
    {"name": "repl", "category": "Tooling", "syntax": "forma repl", "description": "Interactive evaluator"},
    {"name": "lsp", "category": "Tooling", "syntax": "forma lsp", "description": "Start the language server"},
    {"name": "grammar", "category": "Agent", "syntax": "forma grammar --format ebnf|json", "description": "Export generated grammar"},
    {"name": "typeof", "category": "Agent", "syntax": "forma typeof <file> --position L:C", "description": "Query the semantic type at a position"},
    {"name": "complete", "category": "Agent", "syntax": "forma complete <file> --position L:C", "description": "Query compiler-backed completion candidates"},
    {"name": "explain", "category": "Contracts", "syntax": "forma explain <file> --format human|json|markdown", "description": "Explain contract intent"},
    {"name": "verify", "category": "Contracts", "syntax": "forma verify <path> --level test|exhaustive|formal --report", "description": "Produce bounded verification evidence"},
    {"name": "new", "category": "Project", "syntax": "forma new <name>", "description": "Create a project"},
    {"name": "init", "category": "Project", "syntax": "forma init", "description": "Initialize the current directory"},
    {"name": "lex", "category": "Debug", "syntax": "forma lex <file>", "description": "Print tokens"},
    {"name": "parse", "category": "Debug", "syntax": "forma parse <file>", "description": "Print the parsed AST"},
    {"name": "compile", "category": "Native", "syntax": "forma compile <file>", "description": "Alias for build"},
]

CLI_FLAGS = [
    {"name": "--error-format", "category": "Global flag", "syntax": "--error-format human|json", "description": "Select human or machine-readable diagnostics"},
    {"name": "--help", "category": "Global flag", "syntax": "-h, --help", "description": "Print command help"},
    {"name": "--version", "category": "Global flag", "syntax": "-V, --version", "description": "Print the compiler version"},
    {"name": "--output", "category": "compile/build flag", "syntax": "-o, --output <path>", "description": "Choose the native output path"},
    {"name": "--opt-level", "category": "compile/build flag", "syntax": "-O, --opt-level <0..3>", "description": "Choose the native optimization level"},
    {"name": "--no-optimize", "category": "compile/build/run flag", "syntax": "--no-optimize", "description": "Disable the MIR optimization pass"},
    {"name": "--dump-mir", "category": "run flag", "syntax": "--dump-mir", "description": "Print MIR before interpretation"},
    {"name": "--no-check-contracts", "category": "run flag", "syntax": "--no-check-contracts", "description": "Disable runtime precondition, postcondition, and struct-invariant checks"},
    {"name": "--allow-read", "category": "run capability flag", "syntax": "--allow-read", "description": "Grant file-read capability"},
    {"name": "--allow-write", "category": "run capability flag", "syntax": "--allow-write", "description": "Grant file-write capability"},
    {"name": "--allow-network", "category": "run capability flag", "syntax": "--allow-network", "description": "Grant network capability"},
    {"name": "--allow-exec", "category": "run capability flag", "syntax": "--allow-exec", "description": "Grant process-execution capability"},
    {"name": "--allow-env", "category": "run capability flag", "syntax": "--allow-env", "description": "Grant environment-variable capability"},
    {"name": "--allow-unsafe", "category": "run capability flag", "syntax": "--allow-unsafe", "description": "Grant unsafe/FFI capability"},
    {"name": "--allow-all", "category": "run capability flag", "syntax": "--allow-all", "description": "Grant all runtime capabilities"},
    {"name": "--partial", "category": "check flag", "syntax": "--partial", "description": "Validate incomplete source"},
    {"name": "--position", "category": "complete/typeof flag", "syntax": "--position <line:column>", "description": "Select a one-indexed source position"},
    {"name": "--format", "category": "grammar flag", "syntax": "--format ebnf|json", "description": "Choose grammar output format"},
    {"name": "--write", "category": "grammar flag", "syntax": "--write", "description": "Regenerate checked-in grammar artifacts"},
    {"name": "--check", "category": "grammar flag", "syntax": "--check", "description": "Fail when checked-in grammar artifacts are stale"},
    {"name": "--write", "category": "fmt flag", "syntax": "-w, --write", "description": "Write formatted source back to the file"},
    {"name": "--check", "category": "fmt flag", "syntax": "-c, --check", "description": "Fail when a source file is not formatted"},
    {"name": "--function", "category": "explain flag", "syntax": "--function <name>", "description": "Explain only the named function"},
    {"name": "--format", "category": "explain flag", "syntax": "--format human|json|markdown", "description": "Choose contract explanation format"},
    {"name": "--examples", "category": "explain flag", "syntax": "--examples[=<count>]", "description": "Include deterministic generated examples"},
    {"name": "--seed", "category": "explain flag", "syntax": "--seed <integer>", "description": "Choose the example RNG seed"},
    {"name": "--max-examples", "category": "explain flag", "syntax": "--max-examples <count>", "description": "Alias for --examples <count>"},
    {"name": "--report", "category": "verify flag", "syntax": "--report", "description": "Generate a verification report"},
    {"name": "--format", "category": "verify flag", "syntax": "--format human|json", "description": "Choose verification output format"},
    {"name": "--examples", "category": "verify flag", "syntax": "--examples <count>", "description": "Set generated examples per function"},
    {"name": "--level", "category": "verify flag", "syntax": "--level test|exhaustive|formal", "description": "Choose the verification confidence level"},
    {"name": "--max-domain", "category": "verify flag", "syntax": "--max-domain <size>", "description": "Limit the Cartesian finite domain"},
    {"name": "--seed", "category": "verify flag", "syntax": "--seed <integer>", "description": "Choose the verification RNG seed"},
    {"name": "--max-steps", "category": "verify flag", "syntax": "--max-steps <count>", "description": "Limit interpreter steps per example"},
    {"name": "--timeout", "category": "verify flag", "syntax": "--timeout <milliseconds>", "description": "Limit execution time per example"},
    {"name": "--allow-side-effects", "category": "verify flag", "syntax": "--allow-side-effects", "description": "Allow generated examples to use all capabilities"},
]


STATUSES = [
    {"name": "UNCONTRACTED", "category": "Evidence", "syntax": "UNCONTRACTED", "description": "No contract obligation declared"},
    {"name": "TESTED", "category": "Evidence", "syntax": "TESTED", "description": "Generated examples passed within reported bounds"},
    {"name": "COUNTEREXAMPLE", "category": "Evidence", "syntax": "COUNTEREXAMPLE", "description": "An execution or model violated an obligation"},
    {"name": "EXHAUSTIVE", "category": "Evidence", "syntax": "EXHAUSTIVE", "description": "Every tuple in a supported finite domain was checked"},
    {"name": "PROVED", "category": "Evidence", "syntax": "PROVED", "description": "Supported SMT obligations were discharged"},
    {"name": "UNKNOWN", "category": "Evidence", "syntax": "UNKNOWN", "description": "Unsupported, timed out, too large, or not proved"},
    {"name": "SKIPPED", "category": "Evidence", "syntax": "SKIPPED", "description": "Work was intentionally not attempted"},
]

LITERALS = [
    {"name": "Integer", "category": "Scalar", "syntax": "42 / 0xff / 0b1010 / 0o52", "description": "Decimal, hexadecimal, binary, or octal integer; underscores may separate digits"},
    {"name": "Sized integer", "category": "Scalar", "syntax": "42i8 / 42i16 / 42i32 / 42i64 / 42i128 / 42u8 / 42u16 / 42u32 / 42u64 / 42u128", "description": "Integer literal with an explicit signed or unsigned width suffix"},
    {"name": "Float", "category": "Scalar", "syntax": "3.14 / 1e6 / 1.5e-2", "description": "Decimal floating-point literal with optional exponent"},
    {"name": "Sized float", "category": "Scalar", "syntax": "3.14f32 / 3.14f64", "description": "Floating-point literal with an explicit width suffix"},
    {"name": "Boolean true", "category": "Scalar", "syntax": "true / T", "description": "Canonical and compact true literal"},
    {"name": "Boolean false", "category": "Scalar", "syntax": "false / F", "description": "Canonical and compact false literal"},
    {"name": "None", "category": "Algebraic", "syntax": "none / N / None", "description": "Empty Option value"},
    {"name": "Some", "category": "Algebraic", "syntax": "Some(value)", "description": "Present Option value"},
    {"name": "Ok", "category": "Algebraic", "syntax": "Ok(value) / ok(value)", "description": "Successful Result value"},
    {"name": "Err", "category": "Algebraic", "syntax": "Err(error) / err(error)", "description": "Failed Result value"},
    {"name": "String", "category": "Text", "syntax": "\"text\"", "description": "Escaped string literal"},
    {"name": "Raw string", "category": "Text", "syntax": "r`raw text`", "description": "Backtick-delimited raw string"},
    {"name": "Delimited raw string", "category": "Text", "syntax": "r#`text with ` inside`#", "description": "Hash-delimited raw string; matching hash counts close the literal"},
    {"name": "Interpolated string", "category": "Text", "syntax": "f\"value: {value}\"", "description": "String interpolation"},
    {"name": "Character", "category": "Text", "syntax": "'x'", "description": "Character literal"},
    {"name": "List", "category": "Collection", "syntax": "[a, b, c]", "description": "Dynamic list literal"},
    {"name": "Fixed repeat array", "category": "Collection", "syntax": "[value; N]", "description": "Fixed-length repeated array literal"},
    {"name": "Tuple", "category": "Composite", "syntax": "(a, b)", "description": "Tuple literal"},
    {"name": "Struct", "category": "Composite", "syntax": "Point { x: 1, y: 2 }", "description": "Struct literal"},
    {"name": "Map", "category": "Collection", "syntax": "{\"key\": value}", "description": "Map literal"},
    {"name": "Set", "category": "Collection", "syntax": "{value}", "description": "Set literal"},
]


def contract_patterns() -> list[dict[str, str]]:
    parser = (ROOT / "src" / "parser" / "parser.rs").read_text()
    start = parser.index("fn expand_contract_pattern")
    end = parser.index("_ => return Ok(None)", start)
    names = re.findall(r'^\s+"([a-z_]+)"\s*=>\s*\{', parser[start:end], flags=re.M)
    groups = {
        "Numeric": {"positive", "nonnegative", "nonzero", "even", "odd", "divisible", "bounded", "in_range"},
        "Collection": {"nonempty", "contains", "all_positive", "all_nonnegative", "all_nonzero", "valid_index", "valid_range"},
        "Set relationship": {"subset", "superset", "disjoint", "equals", "same_length", "permutation"},
        "Sequence": {"prefix", "suffix", "reversed", "rotated", "unique"},
        "Ordering": {"sorted", "sorted_desc", "strictly_sorted", "strictly_sorted_desc", "sorted_by", "partitioned", "stable"},
        "State": {"unchanged", "pure"},
    }
    records = []
    for name in names:
        category = next((group for group, members in groups.items() if name in members), "Named pattern")
        records.append(
            {
                "name": name,
                "category": category,
                "syntax": f"@{name}(...)",
                "description": "Compiler-expanded named contract pattern",
            }
        )
    return records


def stdlib_exports() -> list[dict[str, str]]:
    records = []
    pattern = re.compile(
        r"^pub\s+(?:(as)\s+)?(f|s|e|t|type|const)\s+"
        r"([A-Za-z_][A-Za-z0-9_]*)\s*(.*)$"
    )
    kinds = {
        "f": "function",
        "s": "struct",
        "e": "enum",
        "t": "trait",
        "type": "type alias",
        "const": "constant",
    }
    for path in sorted((ROOT / "std").glob("*.forma")):
        module = f"std.{path.stem}"
        for line in path.read_text().splitlines():
            match = pattern.match(line.strip())
            if not match:
                continue
            async_prefix, marker, name, _ = match.groups()
            kind = ("async " if async_prefix else "") + kinds[marker]
            records.append(
                {
                    "name": name,
                    "category": module,
                    "syntax": line.strip().removeprefix("pub "),
                    "kind": kind,
                    "description": f"Public {kind} exported by {module}",
                }
            )
    return records


def build_data() -> dict[str, object]:
    grammar = json.loads((ROOT / "docs" / "grammar.json").read_text())
    builtins = json.loads((ROOT / "docs" / "builtins.json").read_text())
    builtins.sort(key=lambda item: item["name"])
    return {
        "version": grammar["version"],
        "source": {
            "grammar": "docs/grammar.json",
            "builtins": "docs/builtins.json",
            "contracts": "src/parser/parser.rs",
            "stdlib": "std/*.forma",
        },
        "keywords": grammar["keywords"],
        "productions": grammar["productions"],
        "operators": grammar["operators"],
        "types": TYPES,
        "forms": FORMS,
        "literals": LITERALS,
        "contracts": contract_patterns(),
        "stdlib": stdlib_exports(),
        "cli": CLI + CLI_FLAGS,
        "statuses": STATUSES,
        "builtins": builtins,
    }


def render() -> str:
    data = json.dumps(build_data(), ensure_ascii=False, separators=(",", ":"))
    return (
        "/* Generated by forma/scripts/generate_website_reference.py. Do not edit. */\n"
        f"window.FORMA_REFERENCE_DATA={data};\n"
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    output = args.output.resolve()
    rendered = render()
    if args.check:
        if not output.exists() or output.read_text() != rendered:
            print(f"{output} is stale")
            return 1
        print(f"{output} is current")
        return 0
    output.write_text(rendered)
    print(f"Wrote {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
