//! FORMA Compiler CLI
//!
//! Command-line interface for the FORMA compiler.

use clap::{Parser, Subcommand, ValueEnum};
use forma::errors::ErrorContext;
use forma::lexer::Span;
use forma::mir::{Interpreter, Lowerer, Value};
use forma::module::ModuleLoader;
use forma::{
    BorrowChecker, Compilation, CompilerSession, Parser as FormaParser, Scanner, TypeChecker,
};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use serde::Serialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process;

/// Error format for output
#[derive(Clone, Copy, Debug, Default, PartialEq, ValueEnum)]
enum ErrorFormat {
    /// Human-readable error format (default)
    #[default]
    Human,
    /// JSON error format for AI/tooling consumption
    Json,
}

/// Grammar output format
#[derive(Clone, Copy, Debug, Default, ValueEnum)]
enum GrammarFormat {
    /// EBNF notation (default)
    #[default]
    Ebnf,
    /// JSON schema format
    Json,
}

/// Explain command output format
#[derive(Clone, Copy, Debug, Default, ValueEnum)]
enum ExplainFormat {
    /// Human-readable output (default)
    #[default]
    Human,
    /// JSON output for tooling
    Json,
    /// Markdown output
    Markdown,
}

/// Verify command output format
#[derive(Clone, Copy, Debug, Default, ValueEnum)]
enum VerifyFormat {
    /// Human-readable output (default)
    #[default]
    Human,
    /// JSON output for tooling
    Json,
}

#[derive(Clone, Copy, Debug, Default, ValueEnum, PartialEq, Eq)]
enum VerifyLevel {
    /// Reproducible generated contract tests.
    #[default]
    Test,
    /// Enumerate every value of supported finite parameter domains.
    Exhaustive,
    /// Attempt formal proof; unsupported obligations report UNKNOWN.
    Formal,
}

/// A structured error for JSON output
#[derive(Serialize)]
struct JsonError {
    file: String,
    line: u32,
    column: u32,
    end_line: u32,
    end_column: u32,
    severity: String,
    code: String,
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    help: Option<String>,
}

/// Collection of errors for JSON output
#[derive(Serialize)]
struct JsonOutput {
    success: bool,
    errors: Vec<JsonError>,
    #[serde(skip_serializing_if = "Option::is_none")]
    items_count: Option<usize>,
}

#[derive(Parser)]
#[command(name = "forma")]
#[command(version)]
#[command(about = "FORMA compiler - an agent-oriented programming language prototype")]
struct Cli {
    /// Error output format
    #[arg(long, value_enum, default_value = "human", global = true)]
    error_format: ErrorFormat,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a FORMA source file (alias for 'build')
    Compile {
        /// Input file
        file: PathBuf,

        /// Output file (default: input without extension)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Optimization level (0-3)
        #[arg(short = 'O', long, default_value = "0")]
        opt_level: u8,

        /// Disable MIR optimization pass
        #[arg(long)]
        no_optimize: bool,
    },

    /// Run a FORMA program
    Run {
        /// Input file
        file: PathBuf,

        /// Arguments to pass to the program
        args: Vec<String>,

        /// Dump MIR before running (for debugging)
        #[arg(long)]
        dump_mir: bool,

        /// Disable @pre/@post contract checking
        #[arg(long)]
        no_check_contracts: bool,

        /// Disable MIR optimization pass
        #[arg(long)]
        no_optimize: bool,

        /// Allow file read access
        #[arg(long)]
        allow_read: bool,

        /// Allow file write access
        #[arg(long)]
        allow_write: bool,

        /// Allow network access
        #[arg(long)]
        allow_network: bool,

        /// Allow process execution
        #[arg(long)]
        allow_exec: bool,

        /// Allow environment variable access
        #[arg(long)]
        allow_env: bool,

        /// Allow unsafe/FFI operations (pointers, memory allocation)
        #[arg(long)]
        allow_unsafe: bool,

        /// Allow all capabilities
        #[arg(long)]
        allow_all: bool,
    },

    /// Lex a file and print tokens (for debugging)
    Lex {
        /// Input file
        file: PathBuf,
    },

    /// Parse a file and print AST (for debugging)
    Parse {
        /// Input file
        file: PathBuf,
    },

    /// Check a file for errors without compiling
    Check {
        /// Input file
        file: PathBuf,

        /// Enable partial checking (validates incomplete code)
        #[arg(long)]
        partial: bool,
    },

    /// Get completion suggestions at a position
    Complete {
        /// Input file
        file: PathBuf,

        /// Position in format "line:column" (1-indexed)
        #[arg(long)]
        position: String,
    },

    /// Get the type at a position
    Typeof {
        /// Input file
        file: PathBuf,

        /// Position in format "line:column" (1-indexed)
        #[arg(long)]
        position: String,
    },

    /// Build native executable (LLVM)
    Build {
        /// Input file
        file: PathBuf,

        /// Output file (default: input without extension)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Optimization level (0-3)
        #[arg(short = 'O', long, default_value = "0")]
        opt_level: u8,

        /// Disable MIR optimization pass
        #[arg(long)]
        no_optimize: bool,
    },

    /// Export the FORMA grammar
    Grammar {
        /// Output format (ebnf, json)
        #[arg(long, value_enum, default_value = "ebnf")]
        format: GrammarFormat,

        /// Regenerate checked-in grammar artifacts in docs/
        #[arg(long, conflicts_with = "check")]
        write: bool,

        /// Fail if checked-in grammar artifacts differ from generated output
        #[arg(long)]
        check: bool,
    },

    /// Create a new FORMA project
    New {
        /// Project name
        name: String,
    },

    /// Initialize a FORMA project in the current directory
    Init,

    /// Start an interactive REPL
    Repl,

    /// Format FORMA source code
    Fmt {
        /// Input file
        file: PathBuf,

        /// Write formatted output back to file
        #[arg(short, long)]
        write: bool,

        /// Check if file is formatted (exit with error if not)
        #[arg(short, long)]
        check: bool,
    },

    /// Start the LSP server for IDE support
    Lsp,

    /// Explain function contracts in plain English
    Explain {
        /// Input file
        file: PathBuf,

        /// Only explain a specific function name
        #[arg(long)]
        function: Option<String>,

        /// Output format
        #[arg(long, value_enum, default_value = "human")]
        format: ExplainFormat,

        /// Include generated input/output examples (optionally set count)
        #[arg(long, num_args = 0..=1, default_missing_value = "3", require_equals = true)]
        examples: Option<usize>,

        /// RNG seed for deterministic examples
        #[arg(long)]
        seed: Option<u64>,

        /// Number of examples per function (alias for --examples N)
        #[arg(long)]
        max_examples: Option<usize>,
    },

    /// Verify contracts and produce a trust report
    Verify {
        /// Input file or directory
        path: PathBuf,

        /// Generate a verification report
        #[arg(long)]
        report: bool,

        /// Output format
        #[arg(long, value_enum, default_value = "human")]
        format: VerifyFormat,

        /// Number of generated examples per function
        #[arg(long, default_value_t = 20)]
        examples: usize,

        /// Verification confidence level
        #[arg(long, value_enum, default_value = "test")]
        level: VerifyLevel,

        /// Maximum Cartesian finite domain size
        #[arg(long, default_value_t = 4096)]
        max_domain: usize,

        /// RNG seed for deterministic example generation
        #[arg(long, default_value_t = 42)]
        seed: u64,

        /// Max interpreter steps per generated example
        #[arg(long, default_value_t = 10_000)]
        max_steps: usize,

        /// Timeout per generated example in milliseconds
        #[arg(long, default_value_t = 1_000)]
        timeout: u64,

        /// Allow generated examples to run with full capabilities
        #[arg(long)]
        allow_side_effects: bool,
    },
}

fn main() {
    let cli = Cli::parse();
    let error_format = cli.error_format;

    let result = match cli.command {
        Commands::Compile {
            file,
            output,
            opt_level,
            no_optimize,
        } => build(
            &file,
            output.as_deref(),
            opt_level,
            !no_optimize,
            error_format,
        ),
        Commands::Run {
            file,
            args,
            dump_mir,
            no_check_contracts,
            no_optimize,
            allow_read,
            allow_write,
            allow_network,
            allow_exec,
            allow_env,
            allow_unsafe,
            allow_all,
        } => {
            let caps = CapabilityConfig {
                allow_read,
                allow_write,
                allow_network,
                allow_exec,
                allow_env,
                allow_unsafe,
                allow_all,
            };
            run_shared(
                &file,
                &args,
                dump_mir,
                !no_check_contracts,
                !no_optimize,
                &caps,
                error_format,
            )
        }
        Commands::Lex { file } => lex(&file, error_format),
        Commands::Parse { file } => parse(&file, error_format),
        Commands::Check { file, partial } => check_shared(&file, partial, error_format),
        Commands::Complete { file, position } => complete(&file, &position, error_format),
        Commands::Typeof { file, position } => typeof_at(&file, &position, error_format),
        Commands::Build {
            file,
            output,
            opt_level,
            no_optimize,
        } => build(
            &file,
            output.as_deref(),
            opt_level,
            !no_optimize,
            error_format,
        ),
        Commands::Grammar {
            format,
            write,
            check,
        } => grammar(format, write, check),
        Commands::New { name } => new_project(&name),
        Commands::Init => init_project(),
        Commands::Repl => repl(),
        Commands::Fmt { file, write, check } => fmt(&file, write, check, error_format),
        Commands::Lsp => lsp(),
        Commands::Explain {
            file,
            function,
            format,
            examples,
            seed,
            max_examples,
        } => explain(
            &file,
            function.as_deref(),
            format,
            examples,
            seed,
            max_examples,
            error_format,
        ),
        Commands::Verify {
            path,
            report,
            format,
            examples,
            level,
            max_domain,
            seed,
            max_steps,
            timeout,
            allow_side_effects,
        } => verify(
            &path,
            VerifyConfig {
                report,
                format,
                examples,
                level,
                max_domain,
                seed,
                max_steps,
                timeout_ms: timeout,
                allow_side_effects,
            },
            error_format,
        ),
    };

    if let Err(e) = result {
        match error_format {
            ErrorFormat::Human => eprintln!("error: {}", e),
            ErrorFormat::Json => {
                // Error already printed in JSON format
            }
        }
        process::exit(1);
    }
}

/// Helper to print JSON without panicking on serialization errors
fn print_json<T: Serialize>(value: &T) {
    match serde_json::to_string_pretty(value) {
        Ok(json) => println!("{}", json),
        Err(e) => eprintln!("Error serializing output: {}", e),
    }
}

/// Configuration for runtime capabilities.
struct CapabilityConfig {
    allow_read: bool,
    allow_write: bool,
    allow_network: bool,
    allow_exec: bool,
    allow_env: bool,
    allow_unsafe: bool,
    allow_all: bool,
}

impl CapabilityConfig {
    /// Apply capability grants to an interpreter.
    fn apply(&self, interp: &mut Interpreter) {
        if self.allow_all {
            interp.grant_capability("all");
        } else {
            if self.allow_read {
                interp.grant_capability("read");
            }
            if self.allow_write {
                interp.grant_capability("write");
            }
            if self.allow_network {
                interp.grant_capability("network");
            }
            if self.allow_exec {
                interp.grant_capability("exec");
            }
            if self.allow_env {
                interp.grant_capability("env");
            }
            if self.allow_unsafe {
                interp.grant_capability("unsafe");
            }
        }
    }
}

/// Helper to create a JsonError from a span and message
fn span_to_json_error(
    file: &str,
    span: Span,
    code: &str,
    message: &str,
    help: Option<&str>,
) -> JsonError {
    JsonError {
        file: file.to_string(),
        line: span.line as u32,
        column: span.column as u32,
        end_line: span.line as u32,
        end_column: (span.column + (span.end - span.start)) as u32,
        severity: "error".to_string(),
        code: code.to_string(),
        message: message.to_string(),
        help: help.map(|s| s.to_string()),
    }
}

/// Output errors in JSON format
fn output_json_errors(errors: Vec<JsonError>, items_count: Option<usize>) {
    let output = JsonOutput {
        success: errors.is_empty(),
        errors,
        items_count,
    };
    print_json(&output);
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "UPPERCASE")]
enum ContractTestStatus {
    Tested,
    Exhaustive,
    Proved,
    Counterexample,
    Unknown,
    Skipped,
    Untested,
}

#[derive(Clone, Debug, Serialize)]
struct ExplainContract {
    expression: String,
    english: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pattern_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    message: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
struct ExplainExample {
    #[serde(skip_serializing_if = "Option::is_none")]
    kind: Option<String>,
    input: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
struct ExplainFunction {
    name: String,
    signature: String,
    preconditions: Vec<ExplainContract>,
    postconditions: Vec<ExplainContract>,
    #[serde(skip_serializing_if = "Option::is_none")]
    examples: Option<Vec<ExplainExample>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    example_note: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
struct ExplainStruct {
    name: String,
    invariants: Vec<ExplainContract>,
    checked_at: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
struct ExplainOutput {
    structs: Vec<ExplainStruct>,
    functions: Vec<ExplainFunction>,
}

#[derive(Clone, Debug, Serialize)]
struct VerifyFunctionReport {
    name: String,
    status: ContractTestStatus,
    contract_count: usize,
    examples_run: usize,
    examples_passed: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    seed: Option<u64>,
    tested_domain: String,
    limitations: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    skip_reason: Option<String>,
    issues: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
struct VerifyInvariantReport {
    type_name: String,
    invariant_count: usize,
    enforcement: String,
    formal_status: ContractTestStatus,
    limitations: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
struct VerifyFileReport {
    path: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    file_error: Option<String>,
    invariants: Vec<VerifyInvariantReport>,
    functions: Vec<VerifyFunctionReport>,
}

#[derive(Clone, Debug, Default, Serialize)]
struct VerifySummary {
    total_functions: usize,
    functions_with_contracts: usize,
    structs_with_invariants: usize,
    invariant_clauses: usize,
    tested: usize,
    exhaustive: usize,
    proved: usize,
    unknown: usize,
    skipped: usize,
    warnings: usize,
    failures: usize,
    total_examples: usize,
    examples_passed: usize,
}

#[derive(Clone, Debug, Serialize)]
struct VerifyOutput {
    files: Vec<VerifyFileReport>,
    summary: VerifySummary,
}

#[derive(Clone, Debug)]
struct SafetyConfig {
    max_steps: usize,
    timeout_ms: u64,
    allow_side_effects: bool,
}

#[derive(Clone, Copy, Debug)]
struct VerifyConfig {
    report: bool,
    format: VerifyFormat,
    examples: usize,
    level: VerifyLevel,
    max_domain: usize,
    seed: u64,
    max_steps: usize,
    timeout_ms: u64,
    allow_side_effects: bool,
}

fn compile_program_for_analysis_shared(
    file: &Path,
    error_format: ErrorFormat,
    emit_errors: bool,
) -> Result<forma::mir::Program, String> {
    compile_file_shared(file, error_format, emit_errors).map(|compilation| compilation.program)
}

fn compile_file_shared(
    file: &Path,
    error_format: ErrorFormat,
    emit_errors: bool,
) -> Result<Compilation, String> {
    let mut session = CompilerSession::new();
    match session.compile_file(file) {
        Ok(compilation) => Ok(compilation),
        Err(diagnostics) => {
            if emit_errors {
                render_compiler_diagnostics(&session, &diagnostics, error_format);
            }
            Err(format!("{} compiler error(s)", diagnostics.len()))
        }
    }
}

fn render_compiler_diagnostics(
    session: &CompilerSession,
    diagnostics: &[forma::CompilerDiagnostic],
    error_format: ErrorFormat,
) {
    let mut json_errors = Vec::new();
    for diagnostic in diagnostics {
        let filename = session
            .sources()
            .name(diagnostic.source_id)
            .unwrap_or("<unknown>");
        let source = session.sources().source(diagnostic.source_id).unwrap_or("");
        match error_format {
            ErrorFormat::Human => {
                let context = ErrorContext::new(filename, source);
                if let Some(help) = &diagnostic.help {
                    context.error_with_help(diagnostic.span, &diagnostic.message, help);
                } else {
                    context.error(diagnostic.span, &diagnostic.message);
                }
            }
            ErrorFormat::Json => json_errors.push(span_to_json_error(
                filename,
                diagnostic.span,
                diagnostic.phase.code(),
                &diagnostic.message,
                diagnostic.help.as_deref(),
            )),
        }
    }
    if matches!(error_format, ErrorFormat::Json) {
        output_json_errors(json_errors, None);
    }
}

#[allow(dead_code)]
fn compile_program_for_analysis_legacy(
    file: &PathBuf,
    error_format: ErrorFormat,
    emit_errors: bool,
) -> Result<forma::mir::Program, String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();
    let ctx = ErrorContext::new(&filename, &source);
    let mut json_errors: Vec<JsonError> = vec![];

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();
    if !lex_errors.is_empty() {
        if emit_errors {
            for error in &lex_errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(error.span, &error.message),
                    ErrorFormat::Json => json_errors.push(span_to_json_error(
                        &filename,
                        error.span,
                        "LEX",
                        &error.message,
                        None,
                    )),
                }
            }
            if matches!(error_format, ErrorFormat::Json) {
                output_json_errors(json_errors, None);
            }
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let parsed_ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            if emit_errors {
                for error in &errors {
                    match error_format {
                        ErrorFormat::Human => ctx.error_with_help(
                            error.span(),
                            &format!("{}", error),
                            error.help().unwrap_or("check syntax"),
                        ),
                        ErrorFormat::Json => {
                            json_errors.push(span_to_json_error(
                                &filename,
                                error.span(),
                                "PARSE",
                                &format!("{}", error),
                                error.help(),
                            ));
                        }
                    }
                }
                if matches!(error_format, ErrorFormat::Json) {
                    output_json_errors(json_errors, None);
                }
            }
            return Err(format!("{} parse error(s)", errors.len()));
        }
    };

    // Load imports
    let mut module_loader = ModuleLoader::from_source_file(file);
    let ast = match module_loader.load_imports(&parsed_ast) {
        Ok(imported_items) => {
            let mut combined_items = imported_items;
            combined_items.extend(parsed_ast.items);
            forma::parser::SourceFile {
                items: combined_items,
                span: parsed_ast.span,
            }
        }
        Err(e) => {
            let error_span = e.span.unwrap_or(forma::lexer::Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            });
            if emit_errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(error_span, &format!("module error: {}", e)),
                    ErrorFormat::Json => {
                        json_errors.push(span_to_json_error(
                            &filename,
                            error_span,
                            "MODULE",
                            &format!("{}", e),
                            None,
                        ));
                        output_json_errors(json_errors, None);
                    }
                }
            }
            return Err(format!("module error: {}", e));
        }
    };

    // Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check(&ast) {
        if emit_errors {
            for error in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(error.span, &format!("{}", error)),
                    ErrorFormat::Json => json_errors.push(span_to_json_error(
                        &filename,
                        error.span,
                        "TYPE",
                        &format!("{}", error),
                        None,
                    )),
                }
            }
            if matches!(error_format, ErrorFormat::Json) {
                output_json_errors(json_errors, None);
            }
        }
        return Err(format!("{} type error(s)", errors.len()));
    }

    // Borrow check
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&ast) {
        if emit_errors {
            for error in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(error.span, &format!("{}", error)),
                    ErrorFormat::Json => json_errors.push(span_to_json_error(
                        &filename,
                        error.span,
                        "BORROW",
                        &format!("{}", error),
                        None,
                    )),
                }
            }
            if matches!(error_format, ErrorFormat::Json) {
                output_json_errors(json_errors, None);
            }
        }
        return Err(format!("{} borrow error(s)", errors.len()));
    }

    let program = match Lowerer::new().lower(&ast) {
        Ok(prog) => prog,
        Err(errors) => {
            if emit_errors {
                for e in &errors {
                    match error_format {
                        ErrorFormat::Human => ctx.error(e.span, &e.message),
                        ErrorFormat::Json => json_errors.push(span_to_json_error(
                            &filename, e.span, "LOWER", &e.message, None,
                        )),
                    }
                }
                if matches!(error_format, ErrorFormat::Json) {
                    output_json_errors(json_errors, None);
                }
            }
            return Err(format!("{} lowering error(s)", errors.len()));
        }
    };

    Ok(program)
}

fn parse_func_args2(expr: &str, prefix: &str) -> Option<(String, String)> {
    expr.strip_prefix(prefix)?
        .strip_suffix(')')?
        .split_once(", ")
        .map(|(a, b)| (a.to_string(), b.to_string()))
}

fn parse_func_args3(expr: &str, prefix: &str) -> Option<(String, String, String)> {
    let inner = expr.strip_prefix(prefix)?.strip_suffix(')')?;
    let parts: Vec<&str> = inner.splitn(3, ", ").collect();
    if parts.len() == 3 {
        Some((
            parts[0].to_string(),
            parts[1].to_string(),
            parts[2].to_string(),
        ))
    } else {
        None
    }
}

fn pattern_to_english(pattern_name: Option<&str>, expr_string: &str) -> String {
    if let Some(name) = pattern_name {
        let result = match name {
            "even" => expr_string
                .strip_suffix(" % 2 == 0")
                .map(|x| format!("{} is even", x)),
            "odd" => expr_string
                .strip_suffix(" % 2 != 0")
                .map(|x| format!("{} is odd", x)),
            "divisible" => expr_string.find(" % ").and_then(|idx| {
                let x = &expr_string[..idx];
                expr_string[idx + 3..]
                    .strip_suffix(" == 0")
                    .map(|n| format!("{} is divisible by {}", x, n))
            }),
            "in_range" => expr_string.find(" && ").and_then(|idx| {
                let left = &expr_string[..idx];
                let right = &expr_string[idx + 4..];
                left.find(" > ").and_then(|gt| {
                    right.find(" < ").map(|lt| {
                        format!(
                            "{} is strictly between {} and {}",
                            &left[..gt],
                            &left[gt + 3..],
                            &right[lt + 3..]
                        )
                    })
                })
            }),
            "contains" => expr_string.find(" in ").map(|idx| {
                format!(
                    "{} contains {}",
                    &expr_string[idx + 4..],
                    &expr_string[..idx]
                )
            }),
            "all_positive" => expr_string
                .strip_prefix("forall x in ")
                .and_then(|r| r.strip_suffix(": x > 0"))
                .map(|arr| format!("all elements in {} are positive", arr)),
            "all_nonnegative" => expr_string
                .strip_prefix("forall x in ")
                .and_then(|r| r.strip_suffix(": x >= 0"))
                .map(|arr| format!("all elements in {} are non-negative", arr)),
            "all_nonzero" => expr_string
                .strip_prefix("forall x in ")
                .and_then(|r| r.strip_suffix(": x != 0"))
                .map(|arr| format!("all elements in {} are non-zero", arr)),
            "valid_index" => expr_string.find(" >= 0 && ").and_then(|idx| {
                let i = &expr_string[..idx];
                let rest = &expr_string[idx + 9..];
                rest.find(" < ").map(|lt| {
                    let arr = rest[lt + 3..].trim_end_matches(".len()");
                    format!("{} is a valid index for {}", i, arr)
                })
            }),
            "valid_range" => Some("valid slice bounds".to_string()),
            "subset" => expr_string.strip_prefix("forall x in ").and_then(|rest| {
                rest.find(": x in ")
                    .map(|idx| format!("{} is a subset of {}", &rest[..idx], &rest[idx + 7..]))
            }),
            "superset" => expr_string.strip_prefix("forall x in ").and_then(|rest| {
                rest.find(": x in ")
                    .map(|idx| format!("{} is a superset of {}", &rest[idx + 7..], &rest[..idx]))
            }),
            "disjoint" => expr_string.strip_prefix("forall x in ").and_then(|rest| {
                rest.find(": forall y in ").map(|idx| {
                    let a = &rest[..idx];
                    let b_and_pred = &rest[idx + 14..];
                    let b = b_and_pred.split(':').next().unwrap_or("").trim();
                    format!("{} and {} have no common elements", a, b)
                })
            }),
            "equals" => parse_func_args2(expr_string, "set_equals(")
                .map(|(a, b)| format!("{} and {} contain exactly the same elements", a, b)),
            "prefix" => parse_func_args2(expr_string, "is_prefix(")
                .map(|(a, b)| format!("{} is a prefix of {}", a, b)),
            "suffix" => parse_func_args2(expr_string, "is_suffix(")
                .map(|(a, b)| format!("{} is a suffix of {}", a, b)),
            "reversed" => parse_func_args2(expr_string, "is_reversed(")
                .map(|(a, b)| format!("{} is the reverse of {}", a, b)),
            "rotated" => parse_func_args3(expr_string, "is_rotated(")
                .map(|(a, b, k)| format!("{} is a rotation of {} by {}", a, b, k)),
            "strictly_sorted" => expr_string
                .strip_prefix("forall i in 0..")
                .and_then(|rest| {
                    rest.find(".len()")
                        .map(|idx| format!("{} is strictly sorted (no duplicates)", &rest[..idx]))
                }),
            "strictly_sorted_desc" => {
                expr_string
                    .strip_prefix("forall i in 0..")
                    .and_then(|rest| {
                        rest.find(".len()").map(|idx| {
                            format!(
                                "{} is strictly sorted descending (no duplicates)",
                                &rest[..idx]
                            )
                        })
                    })
            }
            "sorted_by" => expr_string
                .strip_prefix("forall i in 0..")
                .and_then(|rest| {
                    rest.find(".len()").and_then(|idx| {
                        let arr = &rest[..idx];
                        rest.split(": ").nth(1).and_then(|after| {
                            after.find("].").and_then(|f_start| {
                                let field_rest = &after[f_start + 2..];
                                field_rest.find(' ').map(|f_end| {
                                    format!("{} is sorted by {} field", arr, &field_rest[..f_end])
                                })
                            })
                        })
                    })
                }),
            "partitioned" => parse_func_args2(expr_string, "is_partitioned(")
                .map(|(arr, pivot)| format!("{} is partitioned at index {}", arr, pivot)),
            "stable" => parse_func_args3(expr_string, "stable(").map(|(input, output, key)| {
                let key = key.trim_matches('"');
                format!(
                    "{} is a stable sort of {} by {} (sorted by {}, permutation preserved, equal-{} elements keep original order)",
                    output, input, key, key, key
                )
            }),
            _ => None,
        };
        if let Some(english) = result {
            return english;
        }
    }
    explain_contract_text(expr_string)
}

fn explain_contract_text(expr: &str) -> String {
    let trimmed = expr.trim();

    // Trivially-true (e.g. @pure pattern)
    if trimmed == "true" {
        return "always holds".to_string();
    }

    // Conjunction: split on && and explain each part
    if trimmed.contains(" && ") {
        let parts: Vec<&str> = trimmed.split(" && ").collect();
        let explained: Vec<String> = parts.iter().map(|p| explain_contract_text(p)).collect();
        return explained.join(", and ");
    }

    // Implication pattern: !A || B → "if A then B"
    if let Some(rest) = trimmed.strip_prefix('!')
        && let Some(idx) = rest.find(" || ")
    {
        let a = rest[..idx].trim();
        let b = rest[idx + 4..].trim();
        return format!("if {} then {}", a, explain_contract_text(b));
    }

    // X == old(X) → "X is unchanged"
    if let Some(idx) = trimmed.find(" == old(") {
        let x = trimmed[..idx].trim();
        if trimmed.ends_with(')') {
            let inner = &trimmed[idx + 8..trimmed.len() - 1];
            if x == inner {
                return format!("{} is unchanged", x);
            }
        }
    }

    // forall VAR in RANGE: PRED
    if let Some(rest) = trimmed.strip_prefix("forall ")
        && let Some(colon_idx) = rest.find(": ")
    {
        let binding = &rest[..colon_idx];
        let pred = &rest[colon_idx + 2..];
        return format!("for every {}, {}", binding, explain_contract_text(pred));
    }

    // exists VAR in COLL: PRED
    if let Some(rest) = trimmed.strip_prefix("exists ")
        && let Some(colon_idx) = rest.find(": ")
    {
        let binding = &rest[..colon_idx];
        let pred = &rest[colon_idx + 2..];
        return format!(
            "there exists some {} where {}",
            binding,
            explain_contract_text(pred)
        );
    }

    // result.len() == X.len() → "result has the same length as X"
    if trimmed.starts_with("result.len() == ") && trimmed.ends_with(".len()") {
        let other = &trimmed[16..trimmed.len() - 6];
        return format!("result has the same length as {}", other);
    }

    // X.len() == Y.len() → "X and Y have the same length"
    if let Some(idx) = trimmed.find(".len() == ") {
        let lhs = &trimmed[..idx];
        let rhs = &trimmed[idx + 10..];
        if let Some(rhs_name) = rhs.strip_suffix(".len()") {
            return format!("{} and {} have the same length", lhs, rhs_name);
        }
    }

    // X.len() > 0 → "X is not empty"
    if trimmed.ends_with(".len() > 0") {
        let name = trimmed.trim_end_matches(".len() > 0");
        return format!("{} is not empty", name);
    }

    // old(EXPR) references → "the original value of EXPR (before the call)"
    let result = trimmed.to_string();
    let result = replace_old_refs(&result);

    // X in Y membership → "X is a member of Y"
    if let Some(idx) = result.find(" in ") {
        // Avoid matching "forall x in" which is handled above
        let before = &result[..idx];
        if !before.contains("forall") && !before.contains("exists") {
            let x = before.trim();
            let y = result[idx + 4..].trim();
            return format!("{} is a member of {}", x, y);
        }
    }

    // Comparison operators with context
    if result.contains(" != 0") {
        return result.replace(" != 0", " is non-zero");
    }
    if result.contains(" >= 0") {
        return result.replace(" >= 0", " is non-negative");
    }
    if result.contains(" > 0") {
        return result.replace(" > 0", " is positive");
    }
    if result.contains(" <= ") {
        return result.replace(" <= ", " is at most ");
    }
    if result.contains(" >= ") {
        return result.replace(" >= ", " is at least ");
    }
    if result.contains("result ==") {
        return result.replacen("result ==", "result equals", 1);
    }

    result
}

/// Replace `old(EXPR)` with "the original value of EXPR (before the call)"
fn replace_old_refs(s: &str) -> String {
    let mut result = s.to_string();
    while let Some(start) = result.find("old(") {
        // Find matching close paren
        let after = &result[start + 4..];
        let mut depth = 1;
        let mut end = None;
        for (i, ch) in after.char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        end = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }
        if let Some(end_idx) = end {
            let inner = &after[..end_idx];
            let replacement = format!("the original value of {} (before the call)", inner);
            result = format!(
                "{}{}{}",
                &result[..start],
                replacement,
                &result[start + 4 + end_idx + 1..]
            );
        } else {
            break;
        }
    }
    result
}

fn format_function_signature(func: &forma::mir::Function) -> String {
    let params = if func.param_names.is_empty() {
        func.params
            .iter()
            .enumerate()
            .map(|(i, (_, ty))| format!("arg{}: {}", i, ty))
            .collect::<Vec<_>>()
            .join(", ")
    } else {
        func.param_names
            .iter()
            .map(|(name, ty)| format!("{}: {}", name, ty))
            .collect::<Vec<_>>()
            .join(", ")
    };
    format!("{}({}) -> {}", func.name, params, func.return_ty)
}

fn is_capability_error(msg: &str) -> bool {
    msg.contains("capability '") && msg.contains("required for operation")
}

fn value_for_type(ty: &forma::types::Ty, rng: &mut StdRng, depth: usize) -> Option<Value> {
    if depth > 4 {
        return None;
    }
    use forma::types::Ty;
    match ty {
        Ty::Int | Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::I128 => {
            Some(Value::Int(rng.gen_range(-8..=8)))
        }
        Ty::UInt | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::U128 | Ty::Isize | Ty::Usize => {
            Some(Value::Int(rng.gen_range(0..=8)))
        }
        Ty::Float | Ty::F32 | Ty::F64 => Some(Value::Float(rng.gen_range(-8.0..=8.0))),
        Ty::Bool => Some(Value::Bool(rng.r#gen())),
        Ty::Char => {
            let chars = ['a', 'b', 'c', 'x', 'y', 'z'];
            Some(Value::Char(chars[rng.gen_range(0..chars.len())]))
        }
        Ty::Str => {
            let samples = ["", "a", "hello", "forma", "123"];
            Some(Value::Str(
                samples[rng.gen_range(0..samples.len())].to_string(),
            ))
        }
        Ty::Unit => Some(Value::Unit),
        Ty::List(inner) => {
            let len = rng.gen_range(0..=3);
            let mut out = Vec::with_capacity(len);
            for _ in 0..len {
                out.push(value_for_type(inner, rng, depth + 1)?);
            }
            Some(Value::Array(out))
        }
        Ty::Array(inner, len) => {
            if *len > 32 {
                return None;
            }
            let mut out = Vec::with_capacity(*len);
            for _ in 0..*len {
                out.push(value_for_type(inner, rng, depth + 1)?);
            }
            Some(Value::Array(out))
        }
        Ty::Tuple(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                out.push(value_for_type(item, rng, depth + 1)?);
            }
            Some(Value::Tuple(out))
        }
        Ty::Option(inner) => {
            if rng.gen_bool(0.35) {
                Some(Value::Enum {
                    type_name: "Option".to_string(),
                    variant: "None".to_string(),
                    fields: vec![],
                })
            } else {
                Some(Value::Enum {
                    type_name: "Option".to_string(),
                    variant: "Some".to_string(),
                    fields: vec![value_for_type(inner, rng, depth + 1)?],
                })
            }
        }
        Ty::Result(ok, err) => {
            if rng.gen_bool(0.65) {
                Some(Value::Enum {
                    type_name: "Result".to_string(),
                    variant: "Ok".to_string(),
                    fields: vec![value_for_type(ok, rng, depth + 1)?],
                })
            } else {
                Some(Value::Enum {
                    type_name: "Result".to_string(),
                    variant: "Err".to_string(),
                    fields: vec![value_for_type(err, rng, depth + 1)?],
                })
            }
        }
        Ty::Map(_, value_ty) => {
            let mut out = HashMap::new();
            let len = rng.gen_range(0..=2);
            for i in 0..len {
                out.insert(format!("k{}", i), value_for_type(value_ty, rng, depth + 1)?);
            }
            Some(Value::Map(out))
        }
        _ => None,
    }
}

fn generate_inputs_for_function(
    func: &forma::mir::Function,
    count: usize,
    seed: u64,
) -> Result<Vec<Vec<Value>>, String> {
    for mode in &func.param_pass_modes {
        if *mode != forma::mir::mir::PassMode::Owned {
            return Err("reference parameters are not supported by verify".to_string());
        }
    }

    let mut rng = StdRng::seed_from_u64(seed);
    let total = count.max(1);
    let mut all = Vec::with_capacity(total);
    for _ in 0..total {
        let mut args = Vec::with_capacity(func.params.len());
        for (_, ty) in &func.params {
            let value = value_for_type(ty, &mut rng, 0).ok_or_else(|| {
                format!("unsupported parameter type for example generation: {}", ty)
            })?;
            args.push(value);
        }
        satisfy_simple_preconditions(func, &mut args);
        all.push(args);
    }
    Ok(all)
}

fn finite_inputs_for_function(
    func: &forma::mir::Function,
    max_domain: usize,
) -> Result<Vec<Vec<Value>>, String> {
    if func
        .param_pass_modes
        .iter()
        .any(|mode| *mode != forma::mir::mir::PassMode::Owned)
    {
        return Err("reference parameters are outside the finite verifier".to_string());
    }
    let mut inputs = vec![Vec::new()];
    for (_, ty) in &func.params {
        let values = finite_values_for_type(ty, max_domain)?;
        if inputs.len().saturating_mul(values.len()) > max_domain {
            return Err(format!(
                "finite domain exceeds configured limit of {max_domain} tuples"
            ));
        }
        let mut product = Vec::with_capacity(inputs.len() * values.len());
        for prefix in &inputs {
            for value in &values {
                let mut tuple = prefix.clone();
                tuple.push(value.clone());
                product.push(tuple);
            }
        }
        inputs = product;
    }
    Ok(inputs)
}

fn finite_values_for_type(ty: &forma::types::Ty, cap: usize) -> Result<Vec<Value>, String> {
    use forma::types::Ty;
    match ty {
        Ty::Bool => Ok(vec![Value::Bool(false), Value::Bool(true)]),
        Ty::Unit => Ok(vec![Value::Unit]),
        Ty::Option(inner) => {
            let values = finite_values_for_type(inner, cap)?;
            if values.len() + 1 > cap {
                return Err(format!("finite `{ty}` domain exceeds limit {cap}"));
            }
            let mut out = vec![Value::Enum {
                type_name: "Option".to_string(),
                variant: "None".to_string(),
                fields: vec![],
            }];
            out.extend(values.into_iter().map(|value| Value::Enum {
                type_name: "Option".to_string(),
                variant: "Some".to_string(),
                fields: vec![value],
            }));
            Ok(out)
        }
        Ty::Result(ok, err) => {
            let ok_values = finite_values_for_type(ok, cap)?;
            let err_values = finite_values_for_type(err, cap)?;
            if ok_values.len() + err_values.len() > cap {
                return Err(format!("finite `{ty}` domain exceeds limit {cap}"));
            }
            Ok(ok_values
                .into_iter()
                .map(|value| Value::Enum {
                    type_name: "Result".to_string(),
                    variant: "Ok".to_string(),
                    fields: vec![value],
                })
                .chain(err_values.into_iter().map(|value| Value::Enum {
                    type_name: "Result".to_string(),
                    variant: "Err".to_string(),
                    fields: vec![value],
                }))
                .collect())
        }
        Ty::Tuple(items) => finite_aggregate_values(items, cap, false),
        Ty::Array(item, len) => finite_aggregate_values(&vec![*item.clone(); *len], cap, true),
        _ => Err(format!(
            "type `{ty}` does not have a compiler-known finite domain"
        )),
    }
}

fn finite_aggregate_values(
    items: &[forma::types::Ty],
    cap: usize,
    array: bool,
) -> Result<Vec<Value>, String> {
    let mut product: Vec<Vec<Value>> = vec![Vec::new()];
    for item in items {
        let values = finite_values_for_type(item, cap)?;
        if product.len().saturating_mul(values.len()) > cap {
            return Err(format!("aggregate finite domain exceeds limit {cap}"));
        }
        let mut next = Vec::with_capacity(product.len() * values.len());
        for prefix in &product {
            for value in &values {
                let mut fields = prefix.clone();
                fields.push(value.clone());
                next.push(fields);
            }
        }
        product = next;
    }
    Ok(product
        .into_iter()
        .map(|values| {
            if array {
                Value::Array(values)
            } else {
                Value::Tuple(values)
            }
        })
        .collect())
}

fn satisfy_simple_preconditions(func: &forma::mir::Function, args: &mut [Value]) {
    let mut apply_to_arg = |idx: usize, rule: &str| {
        if idx >= args.len() {
            return;
        }
        match (rule, &mut args[idx]) {
            ("nonzero", Value::Int(n)) if *n == 0 => *n = 1,
            ("nonzero", Value::Float(n)) if *n == 0.0 => *n = 1.0,
            ("positive", Value::Int(n)) if *n <= 0 => *n = 1,
            ("positive", Value::Float(n)) if *n <= 0.0 => *n = 1.0,
            ("nonnegative", Value::Int(n)) if *n < 0 => *n = 0,
            ("nonnegative", Value::Float(n)) if *n < 0.0 => *n = 0.0,
            ("nonempty", Value::Str(s)) if s.is_empty() => *s = "x".to_string(),
            ("nonempty", Value::Array(v)) if v.is_empty() => v.push(Value::Int(0)),
            _ => {}
        }
    };

    for contract in &func.preconditions {
        let expr = contract.expr_string.replace(' ', "");
        for (idx, (name, _ty)) in func.param_names.iter().enumerate() {
            if expr == format!("{}!=0", name) {
                apply_to_arg(idx, "nonzero");
            } else if expr == format!("{}>0", name) {
                apply_to_arg(idx, "positive");
            } else if expr == format!("{}>=0", name) {
                apply_to_arg(idx, "nonnegative");
            } else if expr == format!("{}.len()>0", name) {
                apply_to_arg(idx, "nonempty");
            }
        }
    }
}

fn zero_for_type(ty: &forma::types::Ty) -> Option<Value> {
    use forma::types::Ty;
    match ty {
        Ty::Int | Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::I128 => Some(Value::Int(0)),
        Ty::UInt | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::U128 | Ty::Isize | Ty::Usize => {
            Some(Value::Int(0))
        }
        Ty::Float | Ty::F32 | Ty::F64 => Some(Value::Float(0.0)),
        _ => None,
    }
}

fn negative_one_for_type(ty: &forma::types::Ty) -> Option<Value> {
    use forma::types::Ty;
    match ty {
        Ty::Int | Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::I128 => Some(Value::Int(-1)),
        Ty::Float | Ty::F32 | Ty::F64 => Some(Value::Float(-1.0)),
        _ => None,
    }
}

fn empty_for_type(ty: &forma::types::Ty) -> Option<Value> {
    use forma::types::Ty;
    match ty {
        Ty::Str => Some(Value::Str(String::new())),
        Ty::List(_) | Ty::Array(_, _) => Some(Value::Array(vec![])),
        _ => None,
    }
}

fn generate_invalid_inputs_for_function(
    func: &forma::mir::Function,
    count: usize,
    seed: u64,
) -> Vec<Vec<Value>> {
    if count == 0 || func.preconditions.is_empty() {
        return vec![];
    }

    let base = match generate_inputs_for_function(func, 1, seed) {
        Ok(mut v) if !v.is_empty() => v.remove(0),
        _ => return vec![],
    };

    let mut out = Vec::new();
    let mut seen = std::collections::HashSet::new();

    for contract in &func.preconditions {
        let expr = contract.expr_string.replace(' ', "");
        for (idx, (name, ty)) in func.param_names.iter().enumerate() {
            let mutated = if expr == format!("{}!=0", name) || expr == format!("{}>0", name) {
                zero_for_type(ty)
            } else if expr == format!("{}>=0", name) {
                negative_one_for_type(ty)
            } else if expr == format!("{}.len()>0", name) {
                empty_for_type(ty)
            } else {
                None
            };

            if let Some(value) = mutated {
                let mut args = base.clone();
                if idx < args.len() {
                    args[idx] = value;
                    let key = args
                        .iter()
                        .map(|v| format!("{}", v))
                        .collect::<Vec<_>>()
                        .join("|");
                    if seen.insert(key) {
                        out.push(args);
                        if out.len() >= count {
                            return out;
                        }
                    }
                }
            }
        }
    }

    out
}

fn run_function_with_safety(
    program: &forma::mir::Program,
    func_name: &str,
    args: &[Value],
    safety: &SafetyConfig,
) -> Result<Value, String> {
    use std::panic::{AssertUnwindSafe, catch_unwind};

    let mut interp = Interpreter::new(program.clone())
        .map_err(|e| format!("Failed to create interpreter: {}", e))?;
    if safety.allow_side_effects {
        interp.grant_capability("all");
    } else {
        interp.revoke_all_capabilities();
    }
    interp.set_max_steps(safety.max_steps);
    interp.set_timeout_ms(Some(safety.timeout_ms));
    match catch_unwind(AssertUnwindSafe(|| interp.run(func_name, args))) {
        Ok(result) => result.map_err(|e| e.to_string()),
        Err(_) => Err("execution panicked".to_string()),
    }
}

fn explain(
    file: &Path,
    function: Option<&str>,
    format: ExplainFormat,
    examples: Option<usize>,
    seed: Option<u64>,
    max_examples: Option<usize>,
    error_format: ErrorFormat,
) -> Result<(), String> {
    let program = compile_program_for_analysis_shared(file, error_format, true)?;
    let mut functions: Vec<_> = program.functions.iter().collect();
    functions.sort_by(|a, b| a.0.cmp(b.0));

    let mut output_functions = Vec::new();
    let safety = SafetyConfig {
        max_steps: 128,
        timeout_ms: 1_000,
        allow_side_effects: false,
    };
    let base_seed = seed.unwrap_or(42);
    let example_count = max_examples.or(examples).unwrap_or(0);
    let include_examples = example_count > 0;

    for (name, func) in functions {
        if name == "main" {
            continue;
        }
        if let Some(target) = function
            && target != name
        {
            continue;
        }

        let preconditions = func
            .preconditions
            .iter()
            .map(|c| ExplainContract {
                expression: c.expr_string.clone(),
                english: pattern_to_english(c.pattern_name.as_deref(), &c.expr_string),
                pattern_name: c.pattern_name.clone(),
                message: c.message.clone(),
            })
            .collect::<Vec<_>>();
        let postconditions = func
            .postconditions
            .iter()
            .map(|c| ExplainContract {
                expression: c.expr_string.clone(),
                english: pattern_to_english(c.pattern_name.as_deref(), &c.expr_string),
                pattern_name: c.pattern_name.clone(),
                message: c.message.clone(),
            })
            .collect::<Vec<_>>();

        let mut examples = None;
        let mut example_note = None;
        if include_examples {
            match generate_inputs_for_function(
                func,
                example_count,
                base_seed.wrapping_add(name.bytes().map(u64::from).sum::<u64>()),
            ) {
                Ok(inputs) => {
                    let generated = inputs
                        .iter()
                        .map(
                            |args| match run_function_with_safety(&program, name, args, &safety) {
                                Ok(value) => ExplainExample {
                                    kind: Some("valid".to_string()),
                                    input: args.iter().map(|v| format!("{}", v)).collect(),
                                    output: Some(format!("{}", value)),
                                    error: None,
                                },
                                Err(e) => ExplainExample {
                                    kind: Some("valid".to_string()),
                                    input: args.iter().map(|v| format!("{}", v)).collect(),
                                    output: None,
                                    error: Some(e),
                                },
                            },
                        )
                        .collect::<Vec<_>>();
                    let mut all_examples = generated;
                    let invalid_inputs = generate_invalid_inputs_for_function(
                        func,
                        example_count.min(2),
                        base_seed
                            .wrapping_add(1_000)
                            .wrapping_add(name.bytes().map(u64::from).sum::<u64>()),
                    );
                    for args in invalid_inputs {
                        let example = match run_function_with_safety(&program, name, &args, &safety)
                        {
                            Ok(value) => ExplainExample {
                                kind: Some("invalid".to_string()),
                                input: args.iter().map(|v| format!("{}", v)).collect(),
                                output: Some(format!("{}", value)),
                                error: None,
                            },
                            Err(e) => ExplainExample {
                                kind: Some("invalid".to_string()),
                                input: args.iter().map(|v| format!("{}", v)).collect(),
                                output: None,
                                error: Some(e),
                            },
                        };
                        all_examples.push(example);
                    }
                    examples = Some(all_examples);
                }
                Err(e) => {
                    example_note = Some(e);
                }
            }
        }

        output_functions.push(ExplainFunction {
            name: name.clone(),
            signature: format_function_signature(func),
            preconditions,
            postconditions,
            examples,
            example_note,
        });
    }

    if let Some(target) = function
        && !output_functions.iter().any(|f| f.name == target)
    {
        return Err(format!("function '{}' not found", target));
    }

    let mut output_structs = program
        .struct_invariants
        .iter()
        .map(|(name, invariants)| ExplainStruct {
            name: name.clone(),
            invariants: invariants
                .iter()
                .map(|invariant| ExplainContract {
                    expression: invariant.expr_string.clone(),
                    english: explain_contract_text(&invariant.expr_string),
                    pattern_name: Some("inv".to_string()),
                    message: invariant.message.clone(),
                })
                .collect(),
            checked_at: vec![
                "construction".to_string(),
                "function entry".to_string(),
                "function return".to_string(),
                "mutable borrow return".to_string(),
            ],
        })
        .collect::<Vec<_>>();
    output_structs.sort_by(|a, b| a.name.cmp(&b.name));

    let output = ExplainOutput {
        structs: output_structs,
        functions: output_functions,
    };
    match format {
        ExplainFormat::Json => print_json(&output),
        ExplainFormat::Markdown => {
            for structure in &output.structs {
                println!("### `s {}`", structure.name);
                println!();
                println!("Maintains:");
                for invariant in &structure.invariants {
                    println!("- [@inv] {}", invariant.english);
                }
                println!();
            }
            for function in &output.functions {
                println!("### `{}`", function.signature);
                if function.preconditions.is_empty() && function.postconditions.is_empty() {
                    println!();
                    println!("No contracts.");
                    println!();
                } else {
                    if !function.preconditions.is_empty() {
                        println!();
                        println!("Requires:");
                        for c in &function.preconditions {
                            if let Some(pattern) = &c.pattern_name {
                                println!("- [@{}] {}", pattern, c.english);
                            } else {
                                println!("- {}", c.english);
                            }
                        }
                    }
                    if !function.postconditions.is_empty() {
                        println!();
                        println!("Guarantees:");
                        for c in &function.postconditions {
                            if let Some(pattern) = &c.pattern_name {
                                println!("- [@{}] {}", pattern, c.english);
                            } else {
                                println!("- {}", c.english);
                            }
                        }
                    }
                    println!();
                }
            }
        }
        ExplainFormat::Human => {
            for structure in &output.structs {
                println!("\u{250c}\u{2500} s {}", structure.name);
                println!("\u{2502}  Maintains:");
                for invariant in &structure.invariants {
                    println!("\u{2502}    - [@inv] {}", invariant.english);
                }
                println!(
                    "\u{2514}\u{2500} Checked at: {}",
                    structure.checked_at.join(", ")
                );
                println!();
            }
            for function in &output.functions {
                let has_contracts =
                    !function.preconditions.is_empty() || !function.postconditions.is_empty();
                let has_examples = function.examples.is_some() || function.example_note.is_some();
                let has_body = has_contracts || has_examples;

                if has_body {
                    println!("\u{250c}\u{2500} {}", function.signature);
                } else {
                    println!("\u{250c}\u{2500} {}", function.signature);
                    println!("\u{2502}  No contracts.");
                    println!("\u{2514}\u{2500}");
                    println!();
                    continue;
                }

                if !function.preconditions.is_empty() {
                    println!("\u{2502}  Requires:");
                    for c in &function.preconditions {
                        if let Some(pattern) = &c.pattern_name {
                            println!("\u{2502}    - [@{}] {}", pattern, c.english);
                        } else {
                            println!("\u{2502}    - {}", c.english);
                        }
                    }
                }
                if !function.postconditions.is_empty() {
                    println!("\u{2502}  Guarantees:");
                    for c in &function.postconditions {
                        if let Some(pattern) = &c.pattern_name {
                            println!("\u{2502}    - [@{}] {}", pattern, c.english);
                        } else {
                            println!("\u{2502}    - {}", c.english);
                        }
                    }
                }
                if let Some(note) = &function.example_note {
                    println!("\u{2514}\u{2500} Examples: skipped ({})", note);
                } else if let Some(examples) = &function.examples {
                    println!("\u{2514}\u{2500} Examples:");
                    for ex in examples {
                        let prefix = if matches!(ex.kind.as_deref(), Some("invalid")) {
                            "[invalid]"
                        } else {
                            "[valid]"
                        };
                        match (&ex.output, &ex.error) {
                            (Some(out), _) => {
                                println!("     {} ({}) -> {}", prefix, ex.input.join(", "), out)
                            }
                            (_, Some(err)) => {
                                println!(
                                    "     {} ({}) -> ERROR: {}",
                                    prefix,
                                    ex.input.join(", "),
                                    err
                                )
                            }
                            _ => {}
                        }
                    }
                } else {
                    println!("\u{2514}\u{2500}");
                }
                println!();
            }
        }
    }
    Ok(())
}

fn collect_forma_files(path: &Path) -> Result<Vec<PathBuf>, String> {
    if path.is_file() {
        return Ok(vec![path.to_path_buf()]);
    }
    if !path.is_dir() {
        return Err(format!("path not found: {}", path.display()));
    }

    let mut out = Vec::new();
    let mut stack = vec![path.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let entries = fs::read_dir(&dir)
            .map_err(|e| format!("failed to read directory '{}': {}", dir.display(), e))?;
        for entry in entries {
            let entry = entry.map_err(|e| format!("failed to read directory entry: {}", e))?;
            let entry_path = entry.path();
            if entry_path.is_dir() {
                stack.push(entry_path);
            } else if entry_path.extension().is_some_and(|ext| ext == "forma") {
                out.push(entry_path);
            }
        }
    }
    out.sort();
    Ok(out)
}

fn verify(path: &Path, config: VerifyConfig, error_format: ErrorFormat) -> Result<(), String> {
    if !config.report {
        return Err("verify currently requires --report".to_string());
    }

    let files = collect_forma_files(path)?;
    if files.is_empty() {
        return Err(format!("no .forma files found under '{}'", path.display()));
    }

    let safety = SafetyConfig {
        max_steps: config.max_steps,
        timeout_ms: config.timeout_ms,
        allow_side_effects: config.allow_side_effects,
    };
    let mut report_files = Vec::new();
    let mut summary = VerifySummary::default();
    let mut had_failures = false;

    if config.allow_side_effects && matches!(config.format, VerifyFormat::Human) {
        eprintln!("WARNING: Running verification with side effects enabled");
    }

    for file in files {
        let display = file.to_string_lossy().to_string();
        let program = match compile_program_for_analysis_shared(&file, error_format, false) {
            Ok(p) => p,
            Err(e) => {
                report_files.push(VerifyFileReport {
                    path: display,
                    file_error: Some(e),
                    invariants: vec![],
                    functions: vec![],
                });
                had_failures = true;
                continue;
            }
        };

        let mut invariant_reports = program
            .struct_invariants
            .iter()
            .map(|(type_name, invariants)| VerifyInvariantReport {
                type_name: type_name.clone(),
                invariant_count: invariants.len(),
                enforcement: "runtime: construction, function entry/return, ref mut return"
                    .to_string(),
                formal_status: ContractTestStatus::Unknown,
                limitations: vec![
                    "struct invariant preservation is outside the current SMT subset".to_string(),
                    "runtime enforcement is not a formal proof".to_string(),
                ],
            })
            .collect::<Vec<_>>();
        invariant_reports.sort_by(|a, b| a.type_name.cmp(&b.type_name));
        summary.structs_with_invariants += invariant_reports.len();
        summary.invariant_clauses += invariant_reports
            .iter()
            .map(|report| report.invariant_count)
            .sum::<usize>();

        let mut function_reports = Vec::new();
        let inferred_effects = forma::builtins::infer_effects(&program);
        let mut funcs: Vec<_> = program.functions.iter().collect();
        funcs.sort_by(|a, b| a.0.cmp(b.0));

        for (name, func) in funcs {
            if name == "main" {
                continue;
            }
            summary.total_functions += 1;

            let contract_count = func.preconditions.len() + func.postconditions.len();
            if contract_count == 0 {
                summary.warnings += 1;
                function_reports.push(VerifyFunctionReport {
                    name: name.clone(),
                    status: ContractTestStatus::Untested,
                    contract_count,
                    examples_run: 0,
                    examples_passed: 0,
                    seed: None,
                    tested_domain: "none".to_string(),
                    limitations: vec!["contract testing is not a formal proof".to_string()],
                    skip_reason: None,
                    issues: vec!["no contracts defined".to_string()],
                });
                continue;
            }
            summary.functions_with_contracts += 1;

            if config.level == VerifyLevel::Formal {
                let (status, skip_reason, issues, limitations) = if inferred_effects
                    .get(name)
                    .is_some_and(|effects| !effects.is_empty())
                {
                    summary.unknown += 1;
                    (
                        ContractTestStatus::Unknown,
                        Some("effectful functions are outside the formal pure subset".to_string()),
                        vec![],
                        vec!["effects never produce a false proof".to_string()],
                    )
                } else {
                    match forma::verify::build_smt_obligation(func) {
                        Err(reason) => {
                            summary.unknown += 1;
                            (
                                ContractTestStatus::Unknown,
                                Some(reason),
                                vec![],
                                vec![
                                    "unsupported obligations are never reported as PROVED"
                                        .to_string(),
                                ],
                            )
                        }
                        Ok(obligation) => {
                            let solver = std::env::var("FORMA_SMT_SOLVER")
                                .unwrap_or_else(|_| "z3".to_string());
                            match forma::verify::run_solver(&obligation, &solver) {
                                forma::verify::FormalResult::Proved => {
                                    summary.proved += 1;
                                    (
                                        ContractTestStatus::Proved,
                                        None,
                                        vec![],
                                        vec![format!(
                                            "proved from a {}-byte SMT-LIB obligation",
                                            obligation.script.len()
                                        )],
                                    )
                                }
                                forma::verify::FormalResult::Counterexample(model) => {
                                    summary.failures += 1;
                                    had_failures = true;
                                    let model = model
                                        .into_iter()
                                        .map(|(name, value)| format!("{name} = {value}"))
                                        .collect::<Vec<_>>()
                                        .join(", ");
                                    (
                                        ContractTestStatus::Counterexample,
                                        None,
                                        vec![format!("solver counterexample: {model}")],
                                        vec![
                                            "counterexample values are solver-reproducible"
                                                .to_string(),
                                        ],
                                    )
                                }
                                forma::verify::FormalResult::Unknown(reason) => {
                                    summary.unknown += 1;
                                    (
                                        ContractTestStatus::Unknown,
                                        Some(reason),
                                        vec![],
                                        vec![format!(
                                            "generated {} bytes of SMT-LIB for the pure subset",
                                            obligation.script.len()
                                        )],
                                    )
                                }
                            }
                        }
                    }
                };
                function_reports.push(VerifyFunctionReport {
                    name: name.clone(),
                    status,
                    contract_count,
                    examples_run: 0,
                    examples_passed: 0,
                    seed: None,
                    tested_domain: "SMT-LIB pure subset".to_string(),
                    limitations,
                    skip_reason,
                    issues,
                });
                continue;
            }

            let function_seed = config
                .seed
                .wrapping_add(name.bytes().map(u64::from).sum::<u64>());
            let inputs = match config.level {
                VerifyLevel::Test => {
                    generate_inputs_for_function(func, config.examples, function_seed)
                }
                VerifyLevel::Exhaustive => finite_inputs_for_function(func, config.max_domain),
                VerifyLevel::Formal => unreachable!(),
            };
            let inputs = match inputs {
                Ok(inputs) => inputs,
                Err(e) => {
                    if config.level == VerifyLevel::Exhaustive {
                        summary.unknown += 1;
                    } else {
                        summary.skipped += 1;
                    }
                    function_reports.push(VerifyFunctionReport {
                        name: name.clone(),
                        status: if config.level == VerifyLevel::Exhaustive {
                            ContractTestStatus::Unknown
                        } else {
                            ContractTestStatus::Skipped
                        },
                        contract_count,
                        examples_run: 0,
                        examples_passed: 0,
                        seed: (config.level == VerifyLevel::Test).then_some(function_seed),
                        tested_domain: "unsupported".to_string(),
                        limitations: vec![if config.level == VerifyLevel::Exhaustive {
                            "parameter domain is not finite or exceeds --max-domain".to_string()
                        } else {
                            "contract testing is not a formal proof".to_string()
                        }],
                        skip_reason: Some(e),
                        issues: vec![],
                    });
                    continue;
                }
            };

            let mut passed = 0;
            let mut issues = Vec::new();
            let mut skip_reason = None;

            for args in &inputs {
                match run_function_with_safety(&program, name, args, &safety) {
                    Ok(_) => passed += 1,
                    Err(e)
                        if config.level == VerifyLevel::Exhaustive
                            && e.contains("Contract violation (precondition)") => {}
                    Err(e) if is_capability_error(&e) => {
                        skip_reason = Some(e);
                        break;
                    }
                    Err(e) => issues.push(e),
                }
            }

            summary.total_examples += inputs.len();
            summary.examples_passed += passed;

            let status = if skip_reason.is_some() {
                summary.skipped += 1;
                ContractTestStatus::Skipped
            } else if issues.is_empty() {
                if config.level == VerifyLevel::Exhaustive {
                    summary.exhaustive += 1;
                    ContractTestStatus::Exhaustive
                } else {
                    summary.tested += 1;
                    ContractTestStatus::Tested
                }
            } else {
                summary.failures += 1;
                had_failures = true;
                ContractTestStatus::Counterexample
            };

            function_reports.push(VerifyFunctionReport {
                name: name.clone(),
                status,
                contract_count,
                examples_run: inputs.len(),
                examples_passed: passed,
                seed: (config.level == VerifyLevel::Test).then_some(function_seed),
                tested_domain: if config.level == VerifyLevel::Exhaustive {
                    format!(
                        "all {} value tuple(s) in the finite parameter domain",
                        inputs.len()
                    )
                } else {
                    format!("{} generated input(s)", inputs.len())
                },
                limitations: if config.level == VerifyLevel::Exhaustive {
                    vec!["EXHAUSTIVE applies only to the reported finite domain".to_string()]
                } else {
                    vec![
                        "generated contract tests sample only the reported inputs".to_string(),
                        "TESTED is not a formal proof".to_string(),
                    ]
                },
                skip_reason,
                issues,
            });
        }

        report_files.push(VerifyFileReport {
            path: display,
            file_error: None,
            invariants: invariant_reports,
            functions: function_reports,
        });
    }

    let output = VerifyOutput {
        files: report_files,
        summary,
    };

    match config.format {
        VerifyFormat::Json => print_json(&output),
        VerifyFormat::Human => {
            println!("FORMA Verification Report");
            println!();
            for file in &output.files {
                println!("{}", file.path);
                if let Some(err) = &file.file_error {
                    println!("  ✗ FILE ERROR: {}", err);
                    println!();
                    continue;
                }
                for invariant in &file.invariants {
                    println!(
                        "  ? INVARIANT {:<21} clauses:{} ({})",
                        invariant.type_name, invariant.invariant_count, invariant.enforcement
                    );
                }
                for function in &file.functions {
                    let badge = match function.status {
                        ContractTestStatus::Tested => "✓ TESTED",
                        ContractTestStatus::Exhaustive => "✓ EXHAUSTIVE",
                        ContractTestStatus::Proved => "✓ PROVED",
                        ContractTestStatus::Counterexample => "✗ COUNTEREXAMPLE",
                        ContractTestStatus::Unknown => "? UNKNOWN",
                        ContractTestStatus::Skipped => "⊘ SKIPPED",
                        ContractTestStatus::Untested => "⚠ UNTESTED",
                    };
                    println!(
                        "  {} {:<24} contracts:{} examples:{}/{}",
                        badge,
                        function.name,
                        function.contract_count,
                        function.examples_passed,
                        function.examples_run
                    );
                    if let Some(reason) = &function.skip_reason {
                        println!("    reason: {}", reason);
                    }
                    for issue in &function.issues {
                        println!("    issue: {}", issue);
                    }
                }
                println!();
            }
            println!("Summary");
            println!("  Functions: {}", output.summary.total_functions);
            println!(
                "  With contracts: {}",
                output.summary.functions_with_contracts
            );
            println!(
                "  Structs with invariants: {}",
                output.summary.structs_with_invariants
            );
            println!("  Invariant clauses: {}", output.summary.invariant_clauses);
            println!("  Tested: {}", output.summary.tested);
            println!("  Exhaustive: {}", output.summary.exhaustive);
            println!("  Proved: {}", output.summary.proved);
            println!("  Unknown: {}", output.summary.unknown);
            println!("  Skipped: {}", output.summary.skipped);
            println!("  Warnings: {}", output.summary.warnings);
            println!("  Failures: {}", output.summary.failures);
            println!(
                "  Examples passed: {}/{}",
                output.summary.examples_passed, output.summary.total_examples
            );
        }
    }

    if had_failures {
        return Err("verification found failures".to_string());
    }
    Ok(())
}

fn run_shared(
    file: &Path,
    program_args: &[String],
    dump_mir: bool,
    check_contracts: bool,
    do_optimize: bool,
    caps: &CapabilityConfig,
    error_format: ErrorFormat,
) -> Result<(), String> {
    let compilation = compile_file_shared(file, error_format, true)?;
    execute_program(
        compilation.program,
        file.to_string_lossy().into_owned(),
        ExecutionOptions {
            program_args,
            dump_mir,
            check_contracts,
            optimize: do_optimize,
            capabilities: caps,
            error_format,
        },
    )
}

struct ExecutionOptions<'a> {
    program_args: &'a [String],
    dump_mir: bool,
    check_contracts: bool,
    optimize: bool,
    capabilities: &'a CapabilityConfig,
    error_format: ErrorFormat,
}

fn execute_program(
    mut program: forma::mir::Program,
    filename: String,
    options: ExecutionOptions<'_>,
) -> Result<(), String> {
    if options.optimize {
        forma::mir::optimize::optimize(&mut program);
    }

    if options.dump_mir {
        eprintln!("=== MIR ===");
        eprintln!("{}", program);
        eprintln!("=== END MIR ===\n");
    }

    let mut json_errors = Vec::new();
    if !program.functions.contains_key("main") {
        if matches!(options.error_format, ErrorFormat::Json) {
            json_errors.push(JsonError {
                file: filename.clone(),
                line: 1,
                column: 1,
                end_line: 1,
                end_column: 1,
                severity: "error".to_string(),
                code: "MAIN".to_string(),
                message: "no 'main' function found".to_string(),
                help: Some("add a main function: f main()".to_string()),
            });
            output_json_errors(json_errors, None);
        }
        return Err("error: no 'main' function found".to_string());
    }

    let mut interp = Interpreter::new(program)
        .map_err(|error| format!("Failed to create interpreter: {error}"))?;
    options.capabilities.apply(&mut interp);
    interp.set_check_contracts(options.check_contracts);
    interp.set_env("ARGC", &options.program_args.len().to_string());
    interp.set_env("ARGV", &options.program_args.join(" "));
    for (index, arg) in options.program_args.iter().enumerate() {
        interp.set_env(&format!("ARGV_{index}"), arg);
    }

    match interp.run("main", &[]) {
        Ok(result) => {
            let exit_code = match result {
                Value::Int(value) => value as i32,
                _ => 0,
            };
            if exit_code != 0 {
                process::exit(exit_code);
            }
            Ok(())
        }
        Err(error) => {
            if let Some(code) = error.requested_exit_code() {
                process::exit(code);
            }
            if matches!(options.error_format, ErrorFormat::Json) {
                output_json_errors(
                    vec![JsonError {
                        file: filename,
                        line: 1,
                        column: 1,
                        end_line: 1,
                        end_column: 1,
                        severity: "error".to_string(),
                        code: "RUNTIME".to_string(),
                        message: error.to_string(),
                        help: None,
                    }],
                    None,
                );
            }
            Err(format!("error[RUNTIME]: {error}"))
        }
    }
}

#[allow(dead_code)]
fn run_legacy(
    file: &PathBuf,
    program_args: &[String],
    dump_mir: bool,
    check_contracts: bool,
    do_optimize: bool,
    caps: &CapabilityConfig,
    error_format: ErrorFormat,
) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();
    let ctx = ErrorContext::new(&filename, &source);
    let mut json_errors: Vec<JsonError> = vec![];

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        for error in &lex_errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &error.message),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "LEX",
                    &error.message,
                    None,
                )),
            }
        }
        if matches!(error_format, ErrorFormat::Json) {
            output_json_errors(json_errors, None);
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let parsed_ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            for error in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error_with_help(
                        error.span(),
                        &format!("{}", error),
                        error.help().unwrap_or("check syntax"),
                    ),
                    ErrorFormat::Json => {
                        json_errors.push(span_to_json_error(
                            &filename,
                            error.span(),
                            "PARSE",
                            &format!("{}", error),
                            error.help(),
                        ));
                    }
                }
            }
            if error_format == ErrorFormat::Json {
                output_json_errors(json_errors, None);
            }
            return Err(format!("{} parse error(s)", errors.len()));
        }
    };

    // Load imports (module system)
    let mut module_loader = ModuleLoader::from_source_file(file);
    let ast = match module_loader.load_imports(&parsed_ast) {
        Ok(imported_items) => {
            // Combine imports with main file items
            let mut combined_items = imported_items;
            combined_items.extend(parsed_ast.items);
            forma::parser::SourceFile {
                items: combined_items,
                span: parsed_ast.span,
            }
        }
        Err(e) => {
            let error_span = e.span.unwrap_or(forma::lexer::Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            });
            match error_format {
                ErrorFormat::Human => {
                    ctx.error(error_span, &format!("module error: {}", e));
                }
                ErrorFormat::Json => {
                    json_errors.push(span_to_json_error(
                        &filename,
                        error_span,
                        "MODULE",
                        &format!("{}", e),
                        None,
                    ));
                    output_json_errors(json_errors, None);
                }
            }
            return Err(format!("module error: {}", e));
        }
    };

    // Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check(&ast) {
        for error in &errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &format!("{}", error)),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "TYPE",
                    &format!("{}", error),
                    None,
                )),
            }
        }
        if matches!(error_format, ErrorFormat::Json) {
            output_json_errors(json_errors, None);
        }
        return Err(format!("{} type error(s)", errors.len()));
    }

    // Borrow check
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&ast) {
        for error in &errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &format!("{}", error)),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "BORROW",
                    &format!("{}", error),
                    None,
                )),
            }
        }
        if matches!(error_format, ErrorFormat::Json) {
            output_json_errors(json_errors, None);
        }
        return Err(format!("{} borrow error(s)", errors.len()));
    }

    // Lower to MIR
    let mut program = match Lowerer::new().lower(&ast) {
        Ok(prog) => prog,
        Err(errors) => {
            for e in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(e.span, &e.message),
                    ErrorFormat::Json => json_errors.push(span_to_json_error(
                        &filename, e.span, "LOWER", &e.message, None,
                    )),
                }
            }
            if matches!(error_format, ErrorFormat::Json) {
                output_json_errors(json_errors, None);
            }
            return Err(format!("{} lowering error(s)", errors.len()));
        }
    };

    // Optimize MIR
    if do_optimize {
        forma::mir::optimize::optimize(&mut program);
    }

    // Dump MIR if requested
    if dump_mir {
        eprintln!("=== MIR ===");
        eprintln!("{}", program);
        eprintln!("=== END MIR ===\n");
    }

    // Check for main function
    if !program.functions.contains_key("main") {
        match error_format {
            ErrorFormat::Human => {}
            ErrorFormat::Json => {
                json_errors.push(JsonError {
                    file: filename.clone(),
                    line: 1,
                    column: 1,
                    end_line: 1,
                    end_column: 1,
                    severity: "error".to_string(),
                    code: "MAIN".to_string(),
                    message: "no 'main' function found".to_string(),
                    help: Some("add a main function: f main()".to_string()),
                });
                output_json_errors(json_errors, None);
            }
        }
        return Err("error: no 'main' function found".to_string());
    }

    // Run the interpreter
    let mut interp =
        Interpreter::new(program).map_err(|e| format!("Failed to create interpreter: {}", e))?;

    // Apply capability grants
    caps.apply(&mut interp);

    // Apply contract checking setting
    interp.set_check_contracts(check_contracts);

    // Pass program arguments as ARGV/ARGC environment variables
    interp.set_env("ARGC", &program_args.len().to_string());
    interp.set_env("ARGV", &program_args.join(" "));
    // Also set individual ARGV_0, ARGV_1, etc.
    for (i, arg) in program_args.iter().enumerate() {
        interp.set_env(&format!("ARGV_{}", i), arg);
    }

    match interp.run("main", &[]) {
        Ok(result) => {
            let exit_code = match &result {
                Value::Int(n) => *n as i32,
                _ => 0,
            };
            if exit_code != 0 {
                process::exit(exit_code);
            }
            Ok(())
        }
        Err(e) => {
            if let Some(code) = e.requested_exit_code() {
                process::exit(code);
            }
            match error_format {
                ErrorFormat::Human => {}
                ErrorFormat::Json => {
                    json_errors.push(JsonError {
                        file: filename.clone(),
                        line: 1,
                        column: 1,
                        end_line: 1,
                        end_column: 1,
                        severity: "error".to_string(),
                        code: "RUNTIME".to_string(),
                        message: e.to_string(),
                        help: None,
                    });
                    output_json_errors(json_errors, None);
                }
            }
            Err(format!("error[RUNTIME]: {}", e))
        }
    }
}

fn lex(file: &PathBuf, error_format: ErrorFormat) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();
    let scanner = Scanner::new(&source);
    let (tokens, errors) = scanner.scan_all();

    match error_format {
        ErrorFormat::Human => {
            for token in &tokens {
                println!(
                    "{:4}:{:<3} {:15} {:?}",
                    token.span.line,
                    token.span.column,
                    format!("{:?}", token.kind).split('(').next().unwrap_or(""),
                    token.lexeme
                );
            }

            if !errors.is_empty() {
                eprintln!("\nErrors:");
                for error in &errors {
                    eprintln!(
                        "  {}:{}: {}",
                        error.span.line, error.span.column, error.message
                    );
                }
                return Err("lexer errors occurred".to_string());
            }

            println!("\n{} tokens", tokens.len());
        }
        ErrorFormat::Json => {
            let json_errors: Vec<JsonError> = errors
                .iter()
                .map(|e| span_to_json_error(&filename, e.span, "LEX", &e.message, None))
                .collect();
            output_json_errors(json_errors, Some(tokens.len()));
            if !errors.is_empty() {
                return Err("lexer errors occurred".to_string());
            }
        }
    }

    Ok(())
}

fn parse(file: &PathBuf, error_format: ErrorFormat) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        match error_format {
            ErrorFormat::Human => {
                for error in &lex_errors {
                    eprintln!(
                        "error[LEX]: {}:{}: {}",
                        error.span.line, error.span.column, error.message
                    );
                }
            }
            ErrorFormat::Json => {
                let json_errors: Vec<JsonError> = lex_errors
                    .iter()
                    .map(|e| span_to_json_error(&filename, e.span, "LEX", &e.message, None))
                    .collect();
                output_json_errors(json_errors, None);
            }
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    let parser = FormaParser::new(&tokens);
    match parser.parse() {
        Ok(ast) => {
            match error_format {
                ErrorFormat::Human => {
                    println!("Successfully parsed {} items:", ast.items.len());
                    for item in &ast.items {
                        print_item(item, 0);
                    }
                }
                ErrorFormat::Json => {
                    output_json_errors(vec![], Some(ast.items.len()));
                }
            }
            Ok(())
        }
        Err(errors) => {
            let mut json_errors = vec![];
            for error in &errors {
                let span = error.span();
                match error_format {
                    ErrorFormat::Human => {
                        eprintln!("error[PARSE]: {}:{}: {}", span.line, span.column, error);
                    }
                    ErrorFormat::Json => {
                        json_errors.push(span_to_json_error(
                            &filename,
                            span,
                            "PARSE",
                            &format!("{}", error),
                            error.help(),
                        ));
                    }
                }
            }
            if error_format == ErrorFormat::Json {
                output_json_errors(json_errors, None);
            }
            Err(format!("{} parse error(s)", errors.len()))
        }
    }
}

fn print_item(item: &forma::parser::Item, indent: usize) {
    let prefix = "  ".repeat(indent);
    match &item.kind {
        forma::parser::ItemKind::Function(f) => {
            let async_str = if f.is_async { "async " } else { "" };
            println!(
                "{}{}fn {} ({} params)",
                prefix,
                async_str,
                f.name.name,
                f.params.len()
            );
        }
        forma::parser::ItemKind::Struct(s) => {
            let fields = match &s.kind {
                forma::parser::StructKind::Named(f) => f.len(),
                forma::parser::StructKind::Tuple(t) => t.len(),
                forma::parser::StructKind::Unit => 0,
            };
            println!("{}struct {} ({} fields)", prefix, s.name.name, fields);
        }
        forma::parser::ItemKind::Enum(e) => {
            println!(
                "{}enum {} ({} variants)",
                prefix,
                e.name.name,
                e.variants.len()
            );
        }
        forma::parser::ItemKind::Trait(t) => {
            println!("{}trait {} ({} items)", prefix, t.name.name, t.items.len());
        }
        forma::parser::ItemKind::Impl(i) => {
            let trait_str = i.trait_.as_ref().map(|_| "trait ").unwrap_or("");
            println!("{}{}impl ({} items)", prefix, trait_str, i.items.len());
        }
        forma::parser::ItemKind::TypeAlias(t) => {
            println!("{}type {}", prefix, t.name.name);
        }
        forma::parser::ItemKind::Use(u) => {
            println!("{}use {:?}", prefix, u.tree);
        }
        forma::parser::ItemKind::Module(m) => {
            let items = m.items.as_ref().map(|i| i.len()).unwrap_or(0);
            println!("{}mod {} ({} items)", prefix, m.name.name, items);
        }
        forma::parser::ItemKind::Const(c) => {
            println!("{}const {}", prefix, c.name.name);
        }
    }
}

fn check_shared(file: &Path, partial: bool, error_format: ErrorFormat) -> Result<(), String> {
    let mut session = CompilerSession::new();
    match session.compile_file(file) {
        Ok(compilation) => {
            let item_count = compilation.ast.items.len();
            match error_format {
                ErrorFormat::Human => println!("No errors found ({} items)", item_count),
                ErrorFormat::Json if partial => print_json(&serde_json::json!({
                    "valid": true,
                    "errors": [],
                    "holes": [],
                    "items": item_count,
                })),
                ErrorFormat::Json => output_json_errors(vec![], Some(item_count)),
            }
            Ok(())
        }
        Err(diagnostics) => {
            if matches!(error_format, ErrorFormat::Human) {
                render_compiler_diagnostics(&session, &diagnostics, error_format);
            } else {
                let json_errors: Vec<JsonError> = diagnostics
                    .iter()
                    .map(|diagnostic| {
                        let filename = session
                            .sources()
                            .name(diagnostic.source_id)
                            .unwrap_or("<unknown>");
                        span_to_json_error(
                            filename,
                            diagnostic.span,
                            diagnostic.phase.code(),
                            &diagnostic.message,
                            diagnostic.help.as_deref(),
                        )
                    })
                    .collect();
                if partial {
                    print_json(&serde_json::json!({
                        "valid": false,
                        "errors": json_errors,
                        "holes": [],
                        "items": 0,
                    }));
                } else {
                    output_json_errors(json_errors, None);
                }
            }
            Err(format!("{} error(s) found", diagnostics.len()))
        }
    }
}

#[allow(dead_code)]
fn check_legacy(file: &PathBuf, partial: bool, error_format: ErrorFormat) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();
    let ctx = ErrorContext::new(&filename, &source);
    let mut json_errors: Vec<JsonError> = vec![];

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        for error in &lex_errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &error.message),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "LEX",
                    &error.message,
                    None,
                )),
            }
        }
        if matches!(error_format, ErrorFormat::Json) {
            output_json_errors(json_errors, None);
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            for error in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error_with_help(
                        error.span(),
                        &format!("{}", error),
                        error.help().unwrap_or("check syntax"),
                    ),
                    ErrorFormat::Json => {
                        json_errors.push(span_to_json_error(
                            &filename,
                            error.span(),
                            "PARSE",
                            &format!("{}", error),
                            error.help(),
                        ));
                    }
                }
            }
            if error_format == ErrorFormat::Json {
                output_json_errors(json_errors, None);
            }
            return Err(format!("{} parse error(s)", errors.len()));
        }
    };

    // Load imports (module system)
    let mut module_loader = ModuleLoader::from_source_file(file);
    let ast = match module_loader.load_imports(&ast) {
        Ok(imported_items) => {
            let mut combined_items = imported_items;
            combined_items.extend(ast.items);
            forma::parser::SourceFile {
                items: combined_items,
                span: ast.span,
            }
        }
        Err(e) => {
            let error_span = e.span.unwrap_or(forma::lexer::Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            });
            match error_format {
                ErrorFormat::Human => {
                    ctx.error(error_span, &format!("module error: {}", e));
                }
                ErrorFormat::Json => {
                    json_errors.push(span_to_json_error(
                        &filename,
                        error_span,
                        "MODULE",
                        &format!("{}", e),
                        None,
                    ));
                    output_json_errors(json_errors, None);
                }
            }
            return Err(format!("module error: {}", e));
        }
    };

    let mut error_count = 0;

    // Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check(&ast) {
        for error in &errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &format!("{}", error)),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "TYPE",
                    &format!("{}", error),
                    None,
                )),
            }
        }
        error_count += errors.len();
    }

    // Borrow check
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&ast) {
        for error in &errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &format!("{}", error)),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "BORROW",
                    &format!("{}", error),
                    None,
                )),
            }
        }
        error_count += errors.len();
    }

    if error_count > 0 {
        if matches!(error_format, ErrorFormat::Json) {
            if partial {
                // Partial check returns structured result even with errors
                let result = serde_json::json!({
                    "valid": false,
                    "errors": json_errors,
                    "holes": [],  // TODO: identify incomplete expressions
                    "items": ast.items.len()
                });
                print_json(&result);
            } else {
                output_json_errors(json_errors, Some(ast.items.len()));
            }
        }
        Err(format!("{} error(s) found", error_count))
    } else {
        match error_format {
            ErrorFormat::Human => println!("No errors found ({} items)", ast.items.len()),
            ErrorFormat::Json => {
                if partial {
                    let result = serde_json::json!({
                        "valid": true,
                        "errors": [],
                        "holes": [],
                        "items": ast.items.len()
                    });
                    print_json(&result);
                } else {
                    output_json_errors(vec![], Some(ast.items.len()));
                }
            }
        }
        Ok(())
    }
}

/// Parse a "line:column" position string
fn parse_position(pos: &str) -> Result<(usize, usize), String> {
    let parts: Vec<&str> = pos.split(':').collect();
    if parts.len() != 2 {
        return Err("Position must be in format 'line:column'".to_string());
    }
    let line = parts[0]
        .parse::<usize>()
        .map_err(|_| "Invalid line number")?;
    let col = parts[1]
        .parse::<usize>()
        .map_err(|_| "Invalid column number")?;
    Ok((line, col))
}

/// Get completion suggestions at a position
fn complete(file: &PathBuf, position: &str, error_format: ErrorFormat) -> Result<(), String> {
    let (line, col) = parse_position(position)?;
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();
    let _ctx = ErrorContext::new(&filename, &source);

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, _lex_errors) = scanner.scan_all();

    // Parse (allow partial)
    let parser = FormaParser::new(&tokens);
    let ast = parser.parse().ok();

    // Find the token at position
    let mut found_token = None;
    let mut prev_tokens: Vec<String> = vec![];
    for token in &tokens {
        if token.span.line == line && token.span.column <= col {
            found_token = Some(token.clone());
        }
        if token.span.line < line || (token.span.line == line && token.span.column < col) {
            prev_tokens.push(format!("{:?}", token.kind));
        }
    }

    // Determine valid completions based on context
    let completions = get_completions_for_context(&prev_tokens, &ast);
    let expected_type = infer_expected_type(&prev_tokens, &ast);

    match error_format {
        ErrorFormat::Human => {
            println!("Completions at {}:{}:", line, col);
            for c in &completions {
                println!("  {}", c);
            }
            if let Some(ty) = &expected_type {
                println!("Expected type: {}", ty);
            }
        }
        ErrorFormat::Json => {
            let result = serde_json::json!({
                "position": { "line": line, "column": col },
                "tokens": completions,
                "expected_type": expected_type,
                "context_token": found_token.map(|t| format!("{:?}", t.kind))
            });
            print_json(&result);
        }
    }
    Ok(())
}

/// Get valid completion tokens for a context
fn get_completions_for_context(
    prev_tokens: &[String],
    _ast: &Option<forma::parser::SourceFile>,
) -> Vec<String> {
    // Basic context-aware completions
    let last = prev_tokens.last().map(|s| s.as_str()).unwrap_or("");

    match last {
        // After keywords that expect expressions
        s if s.contains("Assign") || s.contains("Eq") => {
            vec![
                "identifier".into(),
                "literal".into(),
                "if".into(),
                "m".into(),
                "(expr)".into(),
            ]
        }
        // After 'f' (function definition)
        s if s.contains("Fn") => {
            vec!["identifier".into()]
        }
        // After 's' (struct definition)
        s if s.contains("Struct") => {
            vec!["identifier".into()]
        }
        // After '->' (return type)
        s if s.contains("Arrow") => {
            vec![
                "Int".into(),
                "Str".into(),
                "Bool".into(),
                "Float".into(),
                "Char".into(),
                "[T]".into(),
                "T?".into(),
                "identifier".into(),
            ]
        }
        // After '(' (function params or tuple)
        s if s.contains("LParen") => {
            vec!["identifier".into(), ")".into()]
        }
        // After identifier (could be many things)
        s if s.contains("Ident") => {
            vec![
                ":=".into(),
                "(".into(),
                ".".into(),
                "[".into(),
                "->".into(),
                ":".into(),
            ]
        }
        // Default - statement start
        _ => {
            vec![
                "f".into(),
                "s".into(),
                "e".into(),
                "t".into(),
                "i".into(),
                "if".into(),
                "m".into(),
                "wh".into(),
                "for".into(),
                "return".into(),
                "identifier".into(),
            ]
        }
    }
}

/// Infer the expected type at position
fn infer_expected_type(
    prev_tokens: &[String],
    _ast: &Option<forma::parser::SourceFile>,
) -> Option<String> {
    let last = prev_tokens.last().map(|s| s.as_str()).unwrap_or("");

    // Simple heuristics - in a real implementation this would use the type checker
    if last.contains("If") || last.contains("While") {
        Some("Bool".into())
    } else if last.contains("Arrow") {
        Some("Type".into())
    } else {
        None
    }
}

/// Get the type at a position
fn typeof_at(file: &PathBuf, position: &str, error_format: ErrorFormat) -> Result<(), String> {
    let (line, col) = parse_position(position)?;
    let source = read_file(file)?;

    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();
    if !lex_errors.is_empty() {
        return Err("Lexer errors".into());
    }
    let compilation = compile_file_shared(file, error_format, true)?;
    let token = tokens.iter().find(|token| {
        token.span.line == line
            && token.span.column <= col
            && col < token.span.column + token.span.end.saturating_sub(token.span.start)
    });
    let offset = token.map(|token| token.span.start + col.saturating_sub(token.span.column));
    let result_type = offset
        .and_then(|offset| compilation.type_at_offset(offset))
        .map(ToString::to_string)
        .or_else(|| {
            let name = match token.map(|token| &token.kind) {
                Some(forma::lexer::TokenKind::Ident(name)) => name,
                _ => return None,
            };
            compilation.program.functions.get(name).map(|function| {
                forma::types::Ty::Fn(
                    function.params.iter().map(|(_, ty)| ty.clone()).collect(),
                    Box::new(function.return_ty.clone()),
                )
                .to_string()
            })
        });
    let context = match offset.and_then(|offset| compilation.semantics.definition_at(offset)) {
        Some(definition) => match definition.kind {
            forma::semantic::SymbolKind::Function => "function",
            forma::semantic::SymbolKind::Type => "type",
            forma::semantic::SymbolKind::Module => "module",
            forma::semantic::SymbolKind::Parameter => "parameter",
            forma::semantic::SymbolKind::Local => "variable",
        },
        None if matches!(
            token.map(|token| &token.kind),
            Some(
                forma::lexer::TokenKind::Int(_)
                    | forma::lexer::TokenKind::Float(_)
                    | forma::lexer::TokenKind::String(_)
                    | forma::lexer::TokenKind::Char(_)
                    | forma::lexer::TokenKind::True
                    | forma::lexer::TokenKind::False
            )
        ) =>
        {
            "literal"
        }
        None => "unknown",
    };

    match error_format {
        ErrorFormat::Human => {
            println!("Type at {}:{}:", line, col);
            if let Some(ty) = &result_type {
                println!("  type: {}", ty);
            } else {
                println!("  (no type information available)");
            }
            println!("  context: {}", context);
        }
        ErrorFormat::Json => {
            let result = serde_json::json!({
                "position": { "line": line, "column": col },
                "type": result_type,
                "context": context
            });
            print_json(&result);
        }
    }
    Ok(())
}

/// Find the FORMA runtime library directory containing libforma_runtime.a.
/// Searches in order:
/// Includes the shared Cargo workspace target directory used by Forma 0.2.
#[cfg(feature = "llvm")]
fn find_runtime_lib() -> Option<PathBuf> {
    let lib_name = "libforma_runtime.a";

    // Check FORMA_RUNTIME_LIB env var first
    if let Ok(path) = std::env::var("FORMA_RUNTIME_LIB") {
        let p = PathBuf::from(&path);
        if p.join(lib_name).exists() {
            return Some(p);
        }
        // Maybe they pointed directly at the file
        if p.exists() && p.ends_with(lib_name) {
            return p.parent().map(|p| p.to_path_buf());
        }
    }

    let workspace_target = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/release");
    if workspace_target.join(lib_name).exists() {
        return Some(workspace_target);
    }

    // Get the executable path
    if let Ok(exe) = std::env::current_exe()
        && let Some(exe_dir) = exe.parent()
    {
        // <exe_dir>/../runtime/target/release/
        let candidate = exe_dir.join("../runtime/target/release");
        if candidate.join(lib_name).exists() {
            return Some(candidate);
        }
        // <exe_dir>/runtime/target/release/
        let candidate = exe_dir.join("runtime/target/release");
        if candidate.join(lib_name).exists() {
            return Some(candidate);
        }
    }

    // CWD: ./runtime/target/release/
    let candidate = PathBuf::from("runtime/target/release");
    if candidate.join(lib_name).exists() {
        return Some(candidate);
    }

    None
}

#[cfg(feature = "llvm")]
fn ensure_runtime_lib() -> Result<PathBuf, String> {
    if let Some(path) = find_runtime_lib() {
        return Ok(path);
    }
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let status = std::process::Command::new("cargo")
        .args(["build", "-p", "forma_runtime", "--release", "--locked"])
        .current_dir(&workspace)
        .status()
        .map_err(|error| format!("failed to build bundled Forma runtime: {error}"))?;
    if !status.success() {
        return Err("failed to build bundled Forma runtime".to_string());
    }
    find_runtime_lib().ok_or_else(|| {
        "bundled runtime build succeeded but libforma_runtime.a was not produced".to_string()
    })
}

/// Build native executable using LLVM
#[allow(unused_variables)] // output_path and program are used only when LLVM feature is enabled
#[allow(unreachable_code)] // Ok(()) is reachable only when LLVM feature is enabled
fn build(
    file: &Path,
    output: Option<&Path>,
    opt_level: u8,
    do_optimize: bool,
    error_format: ErrorFormat,
) -> Result<(), String> {
    let mut compilation = compile_file_shared(file, error_format, true)?;
    if let Some(entry) = compilation.program.entry.as_ref()
        && let Some(support) = compilation.backend_support.get(entry)
        && support.native == forma::builtins::Support::Unsupported
    {
        return Err(format!(
            "native backend does not support `{entry}`: {}",
            support.reasons.join("; ")
        ));
    }
    if do_optimize {
        forma::mir::optimize::optimize(&mut compilation.program);
    }
    emit_native(compilation.program, file, output, opt_level, error_format)
}

fn emit_native(
    program: forma::mir::Program,
    file: &Path,
    output: Option<&Path>,
    opt_level: u8,
    error_format: ErrorFormat,
) -> Result<(), String> {
    let output_path = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| file.with_extension(""));
    let filename = file.to_string_lossy().into_owned();

    #[cfg(feature = "llvm")]
    {
        use forma::codegen::LLVMCodegen;
        use inkwell::context::Context;

        let mut json_errors = Vec::new();
        let context = Context::create();
        let mut codegen = LLVMCodegen::new(&context, &filename);
        codegen.set_opt_level(opt_level);

        if std::env::var("FORMA_DEBUG").is_ok() {
            eprintln!("{program}");
        }

        if let Err(error) = codegen.compile(&program) {
            match error_format {
                ErrorFormat::Human => eprintln!("error[CODEGEN]: {error}"),
                ErrorFormat::Json => {
                    json_errors.push(JsonError {
                        file: filename.clone(),
                        line: 1,
                        column: 1,
                        end_line: 1,
                        end_column: 1,
                        severity: "error".to_string(),
                        code: "CODEGEN".to_string(),
                        message: error.to_string(),
                        help: None,
                    });
                    output_json_errors(json_errors, None);
                }
            }
            return Err(format!("Codegen error: {error}"));
        }

        let object_path = output_path.with_extension("o");
        codegen
            .write_object_file(&object_path)
            .map_err(|error| format!("Failed to write object file: {error}"))?;

        let runtime_lib_path = ensure_runtime_lib()?;
        let status = std::process::Command::new("cc")
            .arg(&object_path)
            .arg("-L")
            .arg(&runtime_lib_path)
            .arg("-lforma_runtime")
            .arg("-o")
            .arg(&output_path)
            .status()
            .map_err(|error| format!("Failed to run linker: {error}"))?;
        if !status.success() {
            return Err("Linking failed".into());
        }
        let _ = std::fs::remove_file(&object_path);

        match error_format {
            ErrorFormat::Human => {
                println!("Compiled {} -> {}", file.display(), output_path.display())
            }
            ErrorFormat::Json => print_json(&serde_json::json!({
                "status": "success",
                "input": file.to_string_lossy(),
                "output": output_path.to_string_lossy(),
                "opt_level": opt_level,
            })),
        }
        Ok(())
    }

    #[cfg(not(feature = "llvm"))]
    {
        let _ = (program, file, output_path, opt_level, filename);
        match error_format {
            ErrorFormat::Human => {
                eprintln!("LLVM support not enabled. Rebuild with --features llvm")
            }
            ErrorFormat::Json => print_json(&serde_json::json!({
                "status": "not_available",
                "message": "LLVM support not enabled. Rebuild with --features llvm",
            })),
        }
        Err("LLVM not available".into())
    }
}

#[allow(dead_code, unreachable_code, unused_variables)]
fn build_legacy(
    file: &PathBuf,
    output: Option<&PathBuf>,
    opt_level: u8,
    do_optimize: bool,
    error_format: ErrorFormat,
) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();
    let ctx = ErrorContext::new(&filename, &source);
    let mut json_errors: Vec<JsonError> = vec![];

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        for error in &lex_errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &error.message),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "LEX",
                    &error.message,
                    None,
                )),
            }
        }
        if matches!(error_format, ErrorFormat::Json) {
            output_json_errors(json_errors, None);
        }
        return Err("Lexer errors".into());
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let parsed_ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            for error in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(error.span(), &format!("{}", error)),
                    ErrorFormat::Json => json_errors.push(span_to_json_error(
                        &filename,
                        error.span(),
                        "PARSE",
                        &format!("{}", error),
                        error.help(),
                    )),
                }
            }
            if error_format == ErrorFormat::Json {
                output_json_errors(json_errors, None);
            }
            return Err(format!("{} parse error(s)", errors.len()));
        }
    };

    // Load imports
    let mut module_loader = ModuleLoader::from_source_file(file);
    let ast = match module_loader.load_imports(&parsed_ast) {
        Ok(imported_items) => {
            let mut combined_items = imported_items;
            combined_items.extend(parsed_ast.items);
            forma::parser::SourceFile {
                items: combined_items,
                span: parsed_ast.span,
            }
        }
        Err(e) => {
            let error_span = e.span.unwrap_or(forma::lexer::Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            });
            match error_format {
                ErrorFormat::Human => {
                    ctx.error(error_span, &format!("module error: {}", e));
                }
                ErrorFormat::Json => {
                    json_errors.push(span_to_json_error(
                        &filename,
                        error_span,
                        "MODULE",
                        &format!("{}", e),
                        None,
                    ));
                    output_json_errors(json_errors, None);
                }
            }
            return Err(format!("Module error: {}", e));
        }
    };

    // Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check(&ast) {
        for error in &errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &format!("{}", error)),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "TYPE",
                    &format!("{}", error),
                    None,
                )),
            }
        }
        if matches!(error_format, ErrorFormat::Json) {
            output_json_errors(json_errors, None);
        }
        return Err("Type errors".into());
    }

    // Transitional FORMA 0.2 safety gate. Native builds must run the same
    // source-level reference checks as `check` and `run` while ownership
    // enforcement migrates to typed MIR.
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&ast) {
        for error in &errors {
            match error_format {
                ErrorFormat::Human => ctx.error(error.span, &format!("{}", error)),
                ErrorFormat::Json => json_errors.push(span_to_json_error(
                    &filename,
                    error.span,
                    "BORROW",
                    &format!("{}", error),
                    None,
                )),
            }
        }
        if matches!(error_format, ErrorFormat::Json) {
            output_json_errors(json_errors, None);
        }
        return Err("Borrow errors".into());
    }

    // Determine output path
    let output_path = output.cloned().unwrap_or_else(|| file.with_extension(""));

    // Lower to MIR
    let mut program = match Lowerer::new().lower(&ast) {
        Ok(prog) => prog,
        Err(errors) => {
            for e in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(e.span, &e.message),
                    ErrorFormat::Json => json_errors.push(span_to_json_error(
                        &filename, e.span, "LOWER", &e.message, None,
                    )),
                }
            }
            if matches!(error_format, ErrorFormat::Json) {
                output_json_errors(json_errors, None);
            }
            return Err(format!("{} lowering error(s)", errors.len()));
        }
    };

    // Optimize MIR
    if do_optimize {
        forma::mir::optimize::optimize(&mut program);
    }

    // LLVM codegen
    #[cfg(feature = "llvm")]
    {
        use forma::codegen::LLVMCodegen;
        use inkwell::context::Context;

        let context = Context::create();
        let mut codegen = LLVMCodegen::new(&context, &filename);
        codegen.set_opt_level(opt_level);

        // Dump MIR for debugging (if FORMA_DEBUG is set)
        if std::env::var("FORMA_DEBUG").is_ok() {
            for (name, func) in &program.functions {
                eprintln!("--- MIR function: {} ---", name);
                for (i, local) in func.locals.iter().enumerate() {
                    eprintln!("  local_{}: {:?}", i, local.ty);
                }
                for (i, block) in func.blocks.iter().enumerate() {
                    eprintln!("  block_{}:", i);
                    for stmt in &block.stmts {
                        eprintln!("    {:?}", stmt.kind);
                    }
                    if let Some(ref term) = block.terminator {
                        eprintln!("    terminator: {:?}", term);
                    }
                }
            }
        }

        if let Err(e) = codegen.compile(&program) {
            match error_format {
                ErrorFormat::Human => {
                    eprintln!("error[CODEGEN]: {}", e);
                }
                ErrorFormat::Json => {
                    json_errors.push(JsonError {
                        file: filename.clone(),
                        line: 1,
                        column: 1,
                        end_line: 1,
                        end_column: 1,
                        severity: "error".to_string(),
                        code: "CODEGEN".to_string(),
                        message: e.to_string(),
                        help: None,
                    });
                    output_json_errors(json_errors, None);
                }
            }
            return Err(format!("Codegen error: {}", e));
        }

        // Write object file
        let obj_path = output_path.with_extension("o");
        if let Err(e) = codegen.write_object_file(&obj_path) {
            return Err(format!("Failed to write object file: {}", e));
        }

        // Find the runtime library
        let runtime_lib_path = ensure_runtime_lib()?;

        // Link to executable with the FORMA runtime
        let status = std::process::Command::new("cc")
            .arg(&obj_path)
            .arg("-L")
            .arg(&runtime_lib_path)
            .arg("-lforma_runtime")
            .arg("-o")
            .arg(&output_path)
            .status()
            .map_err(|e| format!("Failed to run linker: {}", e))?;

        if !status.success() {
            return Err("Linking failed".into());
        }

        // Clean up object file
        let _ = std::fs::remove_file(&obj_path);

        match error_format {
            ErrorFormat::Human => {
                println!("Compiled {} -> {}", file.display(), output_path.display());
            }
            ErrorFormat::Json => {
                let result = serde_json::json!({
                    "status": "success",
                    "input": file.to_string_lossy(),
                    "output": output_path.to_string_lossy(),
                    "opt_level": opt_level
                });
                print_json(&result);
            }
        }
    }

    #[cfg(not(feature = "llvm"))]
    {
        match error_format {
            ErrorFormat::Human => {
                eprintln!("LLVM support not enabled. Rebuild with --features llvm");
            }
            ErrorFormat::Json => {
                let result = serde_json::json!({
                    "status": "not_available",
                    "message": "LLVM support not enabled. Rebuild with --features llvm"
                });
                print_json(&result);
            }
        }
        return Err("LLVM not available".into());
    }

    Ok(())
}

/// Create a new FORMA project in a new directory
fn new_project(name: &str) -> Result<(), String> {
    use std::fs;

    // Validate project name
    if name.is_empty() {
        return Err("Project name cannot be empty".into());
    }
    if !name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
    {
        return Err(
            "Project name can only contain letters, numbers, underscores, and hyphens".into(),
        );
    }

    let project_path = PathBuf::from(name);
    if project_path.exists() {
        return Err(format!("Directory '{}' already exists", name));
    }

    // Create project directory
    fs::create_dir(&project_path).map_err(|e| format!("Failed to create directory: {}", e))?;

    // Create forma.toml
    let toml_content = format!(
        r#"[package]
name = "{}"
version = "0.1.0"
authors = []

[deps]
# Add dependencies here
# example = {{ path = "../example" }}
"#,
        name
    );

    fs::write(project_path.join("forma.toml"), toml_content)
        .map_err(|e| format!("Failed to create forma.toml: {}", e))?;
    forma::package::PackageManifest::load(&project_path.join("forma.toml"))?.write_lockfile()?;

    // Create src directory
    fs::create_dir(project_path.join("src"))
        .map_err(|e| format!("Failed to create src directory: {}", e))?;

    // Create main.forma
    let main_content = r#"# Welcome to your new FORMA project!

f main() -> Int
    println("Hello, FORMA!")
    0
"#;

    fs::write(project_path.join("src").join("main.forma"), main_content)
        .map_err(|e| format!("Failed to create main.forma: {}", e))?;

    println!("Created new FORMA project '{}'", name);
    println!("  cd {}", name);
    println!("  forma run src/main.forma");

    Ok(())
}

/// Initialize a FORMA project in the current directory
fn init_project() -> Result<(), String> {
    use std::fs;

    let toml_path = PathBuf::from("forma.toml");
    if toml_path.exists() {
        return Err("forma.toml already exists in this directory".into());
    }

    // Get current directory name for project name
    let current_dir =
        std::env::current_dir().map_err(|e| format!("Failed to get current directory: {}", e))?;
    let name = current_dir
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("project");

    // Create forma.toml
    let toml_content = format!(
        r#"[package]
name = "{}"
version = "0.1.0"
authors = []

[deps]
# Add dependencies here
# example = {{ path = "../example" }}
"#,
        name
    );

    fs::write(&toml_path, toml_content)
        .map_err(|e| format!("Failed to create forma.toml: {}", e))?;
    forma::package::PackageManifest::load(&toml_path)?.write_lockfile()?;

    // Create src directory if it doesn't exist
    let src_path = PathBuf::from("src");
    if !src_path.exists() {
        fs::create_dir(&src_path).map_err(|e| format!("Failed to create src directory: {}", e))?;

        // Create main.forma if src didn't exist
        let main_content = r#"# Welcome to your FORMA project!

f main() -> Int
    println("Hello, FORMA!")
    0
"#;

        fs::write(src_path.join("main.forma"), main_content)
            .map_err(|e| format!("Failed to create main.forma: {}", e))?;
    }

    println!("Initialized FORMA project '{}'", name);
    Ok(())
}

fn grammar(format: GrammarFormat, write: bool, check: bool) -> Result<(), String> {
    if write || check {
        return update_grammar_artifacts(write);
    }
    match format {
        GrammarFormat::Ebnf => print_grammar_ebnf_model(),
        GrammarFormat::Json => print_grammar_json_model(),
    }
    Ok(())
}

fn update_grammar_artifacts(write: bool) -> Result<(), String> {
    let executable = std::env::current_exe()
        .map_err(|error| format!("cannot locate forma executable: {error}"))?;
    let docs = Path::new(env!("CARGO_MANIFEST_DIR")).join("docs");
    for (format, filename) in [("ebnf", "grammar.ebnf"), ("json", "grammar.json")] {
        let output = std::process::Command::new(&executable)
            .args(["grammar", "--format", format])
            .output()
            .map_err(|error| format!("failed to generate {format} grammar: {error}"))?;
        if !output.status.success() {
            return Err(String::from_utf8_lossy(&output.stderr).into_owned());
        }
        let path = docs.join(filename);
        if write {
            fs::write(&path, &output.stdout)
                .map_err(|error| format!("failed to write {}: {error}", path.display()))?;
        } else {
            let checked_in = fs::read(&path)
                .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
            if checked_in != output.stdout {
                return Err(format!(
                    "{} is stale; run `forma grammar --write`",
                    path.display()
                ));
            }
        }
    }
    let editor = serde_json::to_vec_pretty(&forma::grammar::editor_metadata())
        .map_err(|error| format!("failed to serialize editor grammar: {error}"))?;
    let generated = [
        ("editor-grammar.json", editor),
        (
            "grammar-keywords.md",
            forma::grammar::keyword_markdown().into_bytes(),
        ),
        (
            "builtins.json",
            serde_json::to_vec_pretty(&forma::builtins::metadata())
                .map_err(|error| format!("failed to serialize builtin catalog: {error}"))?,
        ),
    ];
    for (filename, contents) in generated {
        let path = docs.join(filename);
        if write {
            fs::write(&path, contents)
                .map_err(|error| format!("failed to write {}: {error}", path.display()))?;
        } else {
            let checked_in = fs::read(&path)
                .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
            if checked_in != contents {
                return Err(format!(
                    "{} is stale; run `forma grammar --write`",
                    path.display()
                ));
            }
        }
    }
    if write {
        println!("Updated generated language metadata in docs/");
    } else {
        println!("Grammar artifacts are current");
    }
    Ok(())
}

fn print_grammar_ebnf_model() {
    print!("{}", forma::grammar::EBNF);
}

fn print_grammar_json_model() {
    print_json(&forma::grammar::model());
}

/// Check if input is complete (no unmatched delimiters, no continuation indicators)
fn is_complete_input(input: &str) -> bool {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return true;
    }

    // Line ending with backslash means continuation
    if trimmed.ends_with('\\') {
        return false;
    }

    // Count unmatched delimiters
    let mut parens = 0i32;
    let mut brackets = 0i32;
    let mut braces = 0i32;
    let mut in_string = false;
    let mut in_char = false;
    let mut escape = false;

    for ch in trimmed.chars() {
        if escape {
            escape = false;
            continue;
        }
        if ch == '\\' && (in_string || in_char) {
            escape = true;
            continue;
        }
        if ch == '"' && !in_char {
            in_string = !in_string;
            continue;
        }
        if ch == '\'' && !in_string {
            in_char = !in_char;
            continue;
        }
        if in_string || in_char {
            continue;
        }
        match ch {
            '(' => parens += 1,
            ')' => parens -= 1,
            '[' => brackets += 1,
            ']' => brackets -= 1,
            '{' => braces += 1,
            '}' => braces -= 1,
            _ => {}
        }
    }

    if parens > 0 || brackets > 0 || braces > 0 {
        return false;
    }

    // Unclosed string or char literal
    if in_string || in_char {
        return false;
    }

    true
}

/// Check if a line looks like a definition start
fn is_definition(line: &str) -> bool {
    let line = line.trim();
    line.starts_with("f ")
        || line.starts_with("s ")
        || line.starts_with("e ")
        || line.starts_with("t ")
        || line.starts_with("impl ")
        || line.starts_with("type ")
        || line.starts_with("const ")
        || line.starts_with("mod ")
        || line.starts_with("use ")
        || line.starts_with("pub f ")
        || line.starts_with("pub s ")
        || line.starts_with("pub e ")
        || line.starts_with("pub t ")
        || line.starts_with("async f ")
        || line.starts_with("pub async f ")
}

/// Start an interactive REPL
fn repl() -> Result<(), String> {
    use rustyline::DefaultEditor;
    use rustyline::error::ReadlineError;

    println!("FORMA REPL v{}", env!("CARGO_PKG_VERSION"));
    println!("Type :help for commands, :quit to exit");
    println!();

    let mut rl =
        DefaultEditor::new().map_err(|e| format!("Failed to initialize readline: {}", e))?;

    // Keep track of defined functions for the session
    let mut session_code = String::new();
    // Track loaded files for :reload
    let mut loaded_files: Vec<PathBuf> = Vec::new();

    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(first_line) => {
                let first_line = first_line.trim().to_string();
                if first_line.is_empty() {
                    continue;
                }

                // Add to history
                let _ = rl.add_history_entry(&first_line);

                // Handle REPL commands (only on single-line, not continuations)
                if first_line.starts_with(':') {
                    match first_line.as_str() {
                        ":quit" | ":q" | ":exit" => {
                            println!("Goodbye!");
                            break;
                        }
                        ":help" | ":h" => {
                            println!("REPL Commands:");
                            println!("  :help, :h       - Show this help");
                            println!("  :quit, :q       - Exit the REPL");
                            println!("  :clear          - Clear session definitions");
                            println!("  :type <expr>    - Show the type of an expression");
                            println!("  :defs           - Show current definitions");
                            println!("  :load <file>    - Load a FORMA source file");
                            println!("  :reload         - Reload all loaded files");
                            println!();
                            println!("Enter FORMA expressions or definitions.");
                            println!("Expressions are evaluated immediately.");
                            println!("Definitions are saved for the session.");
                            println!("Multi-line input continues with \"... \" prompt.");
                            continue;
                        }
                        ":clear" => {
                            session_code.clear();
                            println!("Session cleared.");
                            continue;
                        }
                        ":defs" => {
                            if session_code.is_empty() {
                                println!("No definitions in session.");
                            } else {
                                println!("Session definitions:");
                                println!("{}", session_code);
                            }
                            continue;
                        }
                        ":reload" => {
                            if loaded_files.is_empty() {
                                println!("No files loaded.");
                            } else {
                                let mut new_session = String::new();
                                let mut ok = true;
                                for path in &loaded_files {
                                    match fs::read_to_string(path) {
                                        Ok(contents) => {
                                            new_session.push_str(&contents);
                                            if !contents.ends_with('\n') {
                                                new_session.push('\n');
                                            }
                                        }
                                        Err(e) => {
                                            println!("Error reloading '{}': {}", path.display(), e);
                                            ok = false;
                                            break;
                                        }
                                    }
                                }
                                if ok {
                                    session_code = new_session;
                                    println!("Reloaded {} file(s).", loaded_files.len());
                                }
                            }
                            continue;
                        }
                        cmd if cmd.starts_with(":load ") => {
                            let file_path = cmd[6..].trim();
                            let path = PathBuf::from(file_path);
                            match fs::read_to_string(&path) {
                                Ok(contents) => {
                                    session_code.push_str(&contents);
                                    if !contents.ends_with('\n') {
                                        session_code.push('\n');
                                    }
                                    if !loaded_files.contains(&path) {
                                        loaded_files.push(path);
                                    }
                                    println!("Loaded '{}'.", file_path);
                                }
                                Err(e) => {
                                    println!("Error: cannot read '{}': {}", file_path, e);
                                }
                            }
                            continue;
                        }
                        cmd if cmd.starts_with(":type ") => {
                            let expr = &cmd[6..];
                            repl_type_of(expr, &session_code);
                            continue;
                        }
                        _ => {
                            println!("Unknown command: {}", first_line);
                            println!("Type :help for available commands.");
                            continue;
                        }
                    }
                }

                // Accumulate multi-line input
                let mut input = first_line.clone();

                // For definitions, always try continuation
                // For expressions, check completeness
                let is_def = is_definition(&first_line);

                if !is_complete_input(&input) || (is_def && !input.contains('\n')) {
                    // Read continuation lines
                    loop {
                        match rl.readline("... ") {
                            Ok(cont_line) => {
                                let cont = cont_line.trim_end().to_string();
                                // Empty line terminates multi-line input
                                if cont.is_empty() {
                                    break;
                                }
                                input.push('\n');
                                input.push_str(&cont);
                                if is_complete_input(&input) && !is_def {
                                    break;
                                }
                                // For definitions, keep reading until empty line
                            }
                            Err(ReadlineError::Interrupted) => {
                                println!("^C (input cancelled)");
                                input.clear();
                                break;
                            }
                            Err(ReadlineError::Eof) => {
                                break;
                            }
                            Err(_) => {
                                break;
                            }
                        }
                    }
                }

                if input.is_empty() {
                    continue;
                }

                if is_definition(&input) {
                    // Add to session definitions
                    session_code.push_str(&input);
                    session_code.push('\n');

                    // Try to parse to validate
                    if let Err(e) = repl_validate_code(&session_code) {
                        // Remove the bad definition
                        // Count lines we just added
                        let added_lines = input.lines().count();
                        let all_lines: Vec<&str> = session_code.lines().collect();
                        session_code =
                            all_lines[..all_lines.len().saturating_sub(added_lines)].join("\n");
                        if !session_code.is_empty() {
                            session_code.push('\n');
                        }
                        println!("Error: {}", e);
                    } else {
                        println!("Defined.");
                    }
                } else {
                    // Evaluate as expression
                    repl_eval_expr_shared(&input, &session_code);
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

/// Validate code in the REPL
fn repl_validate_code(code: &str) -> Result<(), String> {
    let scanner = Scanner::new(code);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        return Err(format!("Lexer error: {}", lex_errors[0].message));
    }

    let parser = FormaParser::new(&tokens);
    parser.parse().map_err(|errors| {
        errors
            .iter()
            .map(|e| format!("{}", e))
            .collect::<Vec<_>>()
            .join("; ")
    })?;
    Ok(())
}

/// Evaluate an expression in the REPL
fn repl_eval_expr_shared(expr: &str, session_code: &str) {
    let code = format!(
        "{}\nf __repl_main__() -> Int\n    __result__ := {}\n    print(__result__)\n    0\n",
        session_code, expr
    );
    let mut session = CompilerSession::new();
    let mut compilation = match session.compile_source("<repl>", code) {
        Ok(compilation) => compilation,
        Err(diagnostics) => {
            for diagnostic in diagnostics.iter().take(1) {
                println!(
                    "{} error: {}",
                    diagnostic.phase.code().to_ascii_lowercase(),
                    diagnostic.message
                );
            }
            return;
        }
    };
    forma::mir::optimize::optimize(&mut compilation.program);
    let mut interpreter = match Interpreter::new(compilation.program) {
        Ok(interpreter) => interpreter,
        Err(error) => {
            println!("Failed to create interpreter: {error}");
            return;
        }
    };
    if let Err(error) = interpreter.run("__repl_main__", &[]) {
        println!("Runtime error: {error}");
    }
}

#[allow(dead_code)]
fn repl_eval_expr_legacy(expr: &str, session_code: &str) {
    // Wrap the expression in a main function that prints the result
    // Use print() to output the value since FORMA requires explicit return types
    let code = format!(
        "{}\nf __repl_main__() -> Int\n    __result__ := {}\n    print(__result__)\n    0\n",
        session_code, expr
    );

    // Lex
    let scanner = Scanner::new(&code);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        println!("Error: {}", lex_errors[0].message);
        return;
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            for error in &errors {
                println!("Parse error: {}", error);
            }
            return;
        }
    };

    // Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check(&ast) {
        for error in errors.iter().take(1) {
            println!("Type error: {}", error);
        }
        return;
    }

    // Borrow check
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&ast) {
        for error in errors.iter().take(1) {
            println!("Borrow error: {}", error);
        }
        return;
    }

    // Lower to MIR
    let mut program = match Lowerer::new().lower(&ast) {
        Ok(prog) => prog,
        Err(errors) => {
            for e in errors.iter().take(1) {
                println!("Lowering error: {}", e.message);
            }
            return;
        }
    };

    // Optimize MIR
    forma::mir::optimize::optimize(&mut program);

    // Check for __repl_main__ function
    if !program.functions.contains_key("__repl_main__") {
        println!("Internal error: REPL main function not found");
        return;
    }

    // Run the interpreter (print() inside the code handles output, no need to print return value)
    let mut interp = match Interpreter::new(program) {
        Ok(i) => i,
        Err(e) => {
            println!("Failed to create interpreter: {}", e);
            return;
        }
    };
    if let Err(e) = interp.run("__repl_main__", &[]) {
        println!("Runtime error: {}", e);
    }
}

/// Show the type of an expression in the REPL
fn repl_type_of(expr: &str, session_code: &str) {
    // Wrap the expression as a variable assignment to get its type
    let code = format!(
        "{}\nf __repl_main__() -> Int\n    __result__ := {}\n    0\n",
        session_code, expr
    );

    // Lex
    let scanner = Scanner::new(&code);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        println!("Error: {}", lex_errors[0].message);
        return;
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            for error in &errors {
                println!("Parse error: {}", error);
            }
            return;
        }
    };

    // Type check - the expression type-checks if the code compiles
    let mut type_checker = TypeChecker::new();
    match type_checker.check(&ast) {
        Ok(_) => {
            if let Some(expr_type) = type_checker.type_of("__result__") {
                let finalized = type_checker.finalize(&expr_type);
                println!("{}", finalized);
            } else {
                println!("Expression is well-typed");
            }
        }
        Err(errors) => {
            for error in errors.iter().take(1) {
                println!("Type error: {}", error);
            }
        }
    }
}

/// Format a FORMA source file
fn fmt(file: &PathBuf, write: bool, check: bool, error_format: ErrorFormat) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        match error_format {
            ErrorFormat::Human => {
                for error in &lex_errors {
                    eprintln!(
                        "error[LEX]: {}:{}: {}",
                        error.span.line, error.span.column, error.message
                    );
                }
            }
            ErrorFormat::Json => {
                let json_errors: Vec<JsonError> = lex_errors
                    .iter()
                    .map(|e| span_to_json_error(&filename, e.span, "LEX", &e.message, None))
                    .collect();
                output_json_errors(json_errors, None);
            }
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let _ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            match error_format {
                ErrorFormat::Human => {
                    for error in &errors {
                        eprintln!("error[PARSE]: {}", error);
                    }
                }
                ErrorFormat::Json => {
                    let json_errors: Vec<JsonError> = errors
                        .iter()
                        .map(|e| {
                            span_to_json_error(
                                &filename,
                                e.span(),
                                "PARSE",
                                &format!("{}", e),
                                None,
                            )
                        })
                        .collect();
                    output_json_errors(json_errors, None);
                }
            }
            return Err(format!("{} parse error(s)", errors.len()));
        }
    };

    // Format
    let syntax = forma::LosslessSyntax::parse(&source);
    let formatted = forma::LosslessFormatter::format(&syntax);

    if check {
        // Check mode: compare formatted output with original
        if formatted.trim() == source.trim() {
            println!("{} is formatted", filename);
            Ok(())
        } else {
            println!("{} needs formatting", filename);
            Err("file needs formatting".to_string())
        }
    } else if write {
        // Write mode: write formatted output back to file
        fs::write(file, &formatted)
            .map_err(|e| format!("failed to write '{}': {}", file.display(), e))?;
        println!("Formatted {}", filename);
        Ok(())
    } else {
        // Default: print to stdout
        print!("{}", formatted);
        Ok(())
    }
}

/// Start the LSP server
fn lsp() -> Result<(), String> {
    // Create a tokio runtime for the async LSP server
    let rt =
        tokio::runtime::Runtime::new().map_err(|e| format!("Failed to create runtime: {}", e))?;

    rt.block_on(async {
        forma::lsp::run_server().await;
    });

    Ok(())
}

fn read_file(path: &PathBuf) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("cannot read '{}': {}", path.display(), e))
}
