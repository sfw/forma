//! FORMA Compiler CLI
//!
//! Command-line interface for the FORMA compiler.

use forma::errors::ErrorContext;
use forma::lexer::Span;
use forma::mir::{Interpreter, Lowerer};
use forma::module::ModuleLoader;
use forma::{BorrowChecker, Parser as FormaParser, Scanner, TypeChecker};
use clap::{Parser, Subcommand, ValueEnum};
use serde::Serialize;
use std::fs;
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
#[command(version = "0.1.0")]
#[command(about = "FORMA v2 compiler - AI-optimized systems programming language")]
struct Cli {
    /// Error output format
    #[arg(long, value_enum, default_value = "human", global = true)]
    error_format: ErrorFormat,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a FORMA source file
    Compile {
        /// Input file
        file: PathBuf,

        /// Output file (default: input with .o extension)
        #[arg(short, long)]
        output: Option<PathBuf>,
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

        /// Check @pre/@post contracts at runtime
        #[arg(long)]
        check_contracts: bool,
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
    },

    /// Export the FORMA grammar
    Grammar {
        /// Output format (ebnf, json)
        #[arg(long, value_enum, default_value = "ebnf")]
        format: GrammarFormat,
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
}

fn main() {
    let cli = Cli::parse();
    let error_format = cli.error_format;

    let result = match cli.command {
        Commands::Compile { file, output: _ } => compile(&file, error_format),
        Commands::Run { file, args: _, dump_mir, check_contracts } => run(&file, dump_mir, check_contracts, error_format),
        Commands::Lex { file } => lex(&file, error_format),
        Commands::Parse { file } => parse(&file, error_format),
        Commands::Check { file, partial } => check(&file, partial, error_format),
        Commands::Complete { file, position } => complete(&file, &position, error_format),
        Commands::Typeof { file, position } => typeof_at(&file, &position, error_format),
        Commands::Build { file, output, opt_level } => build(&file, output.as_ref(), opt_level, error_format),
        Commands::Grammar { format } => grammar(format),
        Commands::New { name } => new_project(&name),
        Commands::Init => init_project(),
        Commands::Repl => repl(),
        Commands::Fmt { file, write, check } => fmt(&file, write, check),
        Commands::Lsp => lsp(),
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

fn compile(file: &PathBuf, error_format: ErrorFormat) -> Result<(), String> {
    let _source = read_file(file)?;
    // TODO: Implement full compilation
    match error_format {
        ErrorFormat::Human => println!("Compilation not yet implemented"),
        ErrorFormat::Json => output_json_errors(vec![], None),
    }
    Ok(())
}

fn run(file: &PathBuf, dump_mir: bool, _check_contracts: bool, error_format: ErrorFormat) -> Result<(), String> {
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
            match error_format {
                ErrorFormat::Human => {
                    ctx.error(forma::lexer::Span { start: 0, end: 0, line: 1, column: 1 }, &format!("module error: {}", e));
                }
                ErrorFormat::Json => {
                    json_errors.push(JsonError {
                        file: filename.clone(),
                        line: 1,
                        column: 1,
                        end_line: 1,
                        end_column: 1,
                        severity: "error".to_string(),
                        code: "MODULE".to_string(),
                        message: format!("{}", e),
                        help: None,
                    });
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
    let program = match Lowerer::new().lower(&ast) {
        Ok(prog) => prog,
        Err(errors) => {
            for e in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(e.span, &e.message),
                    ErrorFormat::Json => json_errors.push(span_to_json_error(
                        &filename,
                        e.span,
                        "LOWER",
                        &e.message,
                        None,
                    )),
                }
            }
            if matches!(error_format, ErrorFormat::Json) {
                output_json_errors(json_errors, None);
            }
            return Err(format!("{} lowering error(s)", errors.len()));
        }
    };

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
                    help: Some("add a main function: f main() -> Int = 0".to_string()),
                });
                output_json_errors(json_errors, None);
            }
        }
        return Err("error: no 'main' function found".to_string());
    }

    // Run the interpreter
    let mut interp = Interpreter::new(program)
        .map_err(|e| format!("Failed to create interpreter: {}", e))?;
    match interp.run("main", &[]) {
        Ok(result) => {
            println!("{}", result);
            Ok(())
        }
        Err(e) => {
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
            println!("{}{}fn {} ({} params)", prefix, async_str, f.name.name, f.params.len());
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
            println!("{}enum {} ({} variants)", prefix, e.name.name, e.variants.len());
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

fn check(file: &PathBuf, partial: bool, error_format: ErrorFormat) -> Result<(), String> {
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
    let line = parts[0].parse::<usize>().map_err(|_| "Invalid line number")?;
    let col = parts[1].parse::<usize>().map_err(|_| "Invalid column number")?;
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
fn get_completions_for_context(prev_tokens: &[String], _ast: &Option<forma::parser::SourceFile>) -> Vec<String> {
    // Basic context-aware completions
    let last = prev_tokens.last().map(|s| s.as_str()).unwrap_or("");

    match last {
        // After keywords that expect expressions
        s if s.contains("Assign") || s.contains("Eq") => {
            vec!["identifier".into(), "literal".into(), "if".into(), "m".into(), "(expr)".into()]
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
            vec!["Int".into(), "Str".into(), "Bool".into(), "Float".into(), "Char".into(),
                 "[T]".into(), "T?".into(), "identifier".into()]
        }
        // After '(' (function params or tuple)
        s if s.contains("LParen") => {
            vec!["identifier".into(), ")".into()]
        }
        // After identifier (could be many things)
        s if s.contains("Ident") => {
            vec![":=".into(), "(".into(), ".".into(), "[".into(), "->".into(), ":".into()]
        }
        // Default - statement start
        _ => {
            vec!["f".into(), "s".into(), "e".into(), "t".into(), "i".into(),
                 "if".into(), "m".into(), "wh".into(), "for".into(), "return".into(),
                 "identifier".into()]
        }
    }
}

/// Infer the expected type at position
fn infer_expected_type(prev_tokens: &[String], _ast: &Option<forma::parser::SourceFile>) -> Option<String> {
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
    let filename = file.to_string_lossy().to_string();
    let ctx = ErrorContext::new(&filename, &source);

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        return Err("Lexer errors".into());
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            for error in &errors {
                ctx.error(error.span(), &format!("{}", error));
            }
            return Err(format!("{} parse error(s)", errors.len()));
        }
    };

    // Type check to get type information
    let mut type_checker = TypeChecker::new();
    let _ = type_checker.check(&ast); // We want partial info even if errors

    // Find what's at the position
    let mut result_type: Option<String> = None;
    let mut context = "unknown";

    // Search through tokens to find the one at position
    for token in &tokens {
        if token.span.line == line &&
           token.span.column <= col &&
           col < token.span.column + token.span.end.saturating_sub(token.span.start) {
            // Found token at position - infer its type from context
            match &token.kind {
                forma::lexer::TokenKind::Ident(_) => {
                    // Look up identifier in type environment
                    result_type = Some("(identifier - run type checker for actual type)".into());
                    context = "identifier";
                }
                forma::lexer::TokenKind::Int(_) => {
                    result_type = Some("Int".into());
                    context = "literal";
                }
                forma::lexer::TokenKind::Float(_) => {
                    result_type = Some("Float".into());
                    context = "literal";
                }
                forma::lexer::TokenKind::String(_) => {
                    result_type = Some("Str".into());
                    context = "literal";
                }
                forma::lexer::TokenKind::Char(_) => {
                    result_type = Some("Char".into());
                    context = "literal";
                }
                forma::lexer::TokenKind::True | forma::lexer::TokenKind::False => {
                    result_type = Some("Bool".into());
                    context = "literal";
                }
                _ => {
                    context = "keyword/operator";
                }
            }
            break;
        }
    }

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
/// 1. Next to the forma binary: <exe_dir>/../runtime/target/release/
/// 2. Next to the forma binary: <exe_dir>/runtime/target/release/
/// 3. Current working directory: ./runtime/target/release/
/// 4. FORMA_RUNTIME_LIB environment variable
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

    // Get the executable path
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
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
    }

    // CWD: ./runtime/target/release/
    let candidate = PathBuf::from("runtime/target/release");
    if candidate.join(lib_name).exists() {
        return Some(candidate);
    }

    None
}

/// Build native executable using LLVM
#[allow(unused_variables)] // output_path and program are used only when LLVM feature is enabled
#[allow(unreachable_code)] // Ok(()) is reachable only when LLVM feature is enabled
fn build(file: &PathBuf, output: Option<&PathBuf>, opt_level: u8, error_format: ErrorFormat) -> Result<(), String> {
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
        Err(e) => return Err(format!("Module error: {}", e)),
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
        return Err("Type errors".into());
    }

    // Determine output path
    let output_path = output.cloned().unwrap_or_else(|| {
        file.with_extension("")
    });

    // Lower to MIR
    let program = match Lowerer::new().lower(&ast) {
        Ok(prog) => prog,
        Err(errors) => {
            for e in &errors {
                match error_format {
                    ErrorFormat::Human => ctx.error(e.span, &e.message),
                    ErrorFormat::Json => json_errors.push(span_to_json_error(
                        &filename,
                        e.span,
                        "LOWER",
                        &e.message,
                        None,
                    )),
                }
            }
            if matches!(error_format, ErrorFormat::Json) {
                output_json_errors(json_errors, None);
            }
            return Err(format!("{} lowering error(s)", errors.len()));
        }
    };

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
        let runtime_lib_path = find_runtime_lib()
            .ok_or_else(|| "Cannot find libforma_runtime.a - build the runtime first: cd runtime && cargo build --release".to_string())?;

        // Link to executable with the FORMA runtime
        let status = std::process::Command::new("cc")
            .arg(&obj_path)
            .arg("-L").arg(&runtime_lib_path)
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
    if !name.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-') {
        return Err("Project name can only contain letters, numbers, underscores, and hyphens".into());
    }

    let project_path = PathBuf::from(name);
    if project_path.exists() {
        return Err(format!("Directory '{}' already exists", name));
    }

    // Create project directory
    fs::create_dir(&project_path)
        .map_err(|e| format!("Failed to create directory: {}", e))?;

    // Create forma.toml
    let toml_content = format!(r#"[package]
name = "{}"
version = "0.1.0"
authors = []

[deps]
# Add dependencies here
# example = {{ path = "../example" }}
"#, name);

    fs::write(project_path.join("forma.toml"), toml_content)
        .map_err(|e| format!("Failed to create forma.toml: {}", e))?;

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
    let current_dir = std::env::current_dir()
        .map_err(|e| format!("Failed to get current directory: {}", e))?;
    let name = current_dir
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("project");

    // Create forma.toml
    let toml_content = format!(r#"[package]
name = "{}"
version = "0.1.0"
authors = []

[deps]
# Add dependencies here
# example = {{ path = "../example" }}
"#, name);

    fs::write(&toml_path, toml_content)
        .map_err(|e| format!("Failed to create forma.toml: {}", e))?;

    // Create src directory if it doesn't exist
    let src_path = PathBuf::from("src");
    if !src_path.exists() {
        fs::create_dir(&src_path)
            .map_err(|e| format!("Failed to create src directory: {}", e))?;

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

fn grammar(format: GrammarFormat) -> Result<(), String> {
    match format {
        GrammarFormat::Ebnf => print_grammar_ebnf(),
        GrammarFormat::Json => print_grammar_json(),
    }
    Ok(())
}

fn print_grammar_ebnf() {
    println!(
        r#"(* FORMA Programming Language Grammar - EBNF *)
(* Version 0.1.0 *)

(* ============================================ *)
(* Top Level *)
(* ============================================ *)

Program = {{ Item }} ;

Item = Function
     | Struct
     | Enum
     | Trait
     | Impl
     | TypeAlias
     | Use
     | Module
     | Const
     ;

(* ============================================ *)
(* Items *)
(* ============================================ *)

Function = [ "async" ] "f" Identifier [ GenericParams ] "(" [ ParamList ] ")" [ "->" Type ] FunctionBody ;

FunctionBody = "=" Expression
             | Block
             ;

ParamList = Param {{ "," Param }} [ "," ] ;

Param = [ "mut" ] Identifier ":" Type ;

Struct = "s" Identifier [ GenericParams ] [ "=" StructBody ] ;

StructBody = "{{" [ FieldList ] "}}"          (* named fields *)
           | "(" [ TypeList ] ")"             (* tuple struct *)
           ;                                  (* unit struct if omitted *)

FieldList = Field {{ "," Field }} [ "," ] ;

Field = Identifier ":" Type ;

Enum = "e" Identifier [ GenericParams ] "=" Variant {{ "|" Variant }} ;

Variant = Identifier [ VariantData ] ;

VariantData = "(" [ TypeList ] ")"            (* tuple variant *)
            | "{{" [ FieldList ] "}}"         (* struct variant *)
            ;

Trait = "t" Identifier [ GenericParams ] [ ":" TypeBounds ] "{{" {{ TraitItem }} "}}" ;

TraitItem = Function
          | TypeAlias
          ;

Impl = "impl" [ GenericParams ] [ Type "for" ] Type [ WhereClause ] "{{" {{ ImplItem }} "}}" ;

ImplItem = Function ;

TypeAlias = "type" Identifier [ GenericParams ] "=" Type ;

Use = "use" UsePath ;

UsePath = Identifier {{ "::" Identifier }} [ UseTree ] ;

UseTree = "::" "*"                            (* glob import *)
        | "::" "{{" UseList "}}"              (* specific imports *)
        | "as" Identifier                     (* rename *)
        ;

UseList = UsePath {{ "," UsePath }} [ "," ] ;

Module = "mod" Identifier [ "{{" {{ Item }} "}}" ] ;

Const = "const" Identifier ":" Type "=" Expression ;

(* ============================================ *)
(* Types *)
(* ============================================ *)

Type = TypePath
     | ArrayType
     | TupleType
     | FunctionType
     | ReferenceType
     | PointerType
     | ErrorType
     | OptionType
     | NeverType
     ;

PointerType = "*" Type ;                      (* Raw pointer to T *)

OptionType = Type "?" ;                       (* Option[T] sugar *)

TypePath = [ "::" ] Identifier {{ "::" Identifier }} [ GenericArgs ] ;

ArrayType = "[" Type [ ";" Expression ] "]" ;

TupleType = "(" [ Type {{ "," Type }} [ "," ] ] ")" ;

FunctionType = "fn" "(" [ TypeList ] ")" [ "->" Type ] ;

ReferenceType = "&" [ "mut" ] Type ;

ErrorType = Type "!" Type ;                   (* Result type sugar *)

NeverType = "!" ;

GenericParams = "[" GenericParam {{ "," GenericParam }} [ "," ] "]" ;

GenericParam = Identifier [ ":" TypeBounds ] ;

GenericArgs = "[" Type {{ "," Type }} [ "," ] "]" ;

TypeBounds = Type {{ "+" Type }} ;

TypeList = Type {{ "," Type }} [ "," ] ;

WhereClause = "where" WherePredicate {{ "," WherePredicate }} ;

WherePredicate = Type ":" TypeBounds ;

(* ============================================ *)
(* Expressions *)
(* ============================================ *)

Expression = AssignExpr ;

AssignExpr = OrExpr [ AssignOp AssignExpr ] ;

AssignOp = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&&=" | "||=" ;

OrExpr = AndExpr {{ "||" AndExpr }} ;

AndExpr = CompareExpr {{ "&&" CompareExpr }} ;

CompareExpr = BitwiseOrExpr [ CompareOp BitwiseOrExpr ] ;

CompareOp = "==" | "!=" | "<" | "<=" | ">" | ">=" ;

BitwiseOrExpr = BitwiseXorExpr {{ "|" BitwiseXorExpr }} ;

BitwiseXorExpr = BitwiseAndExpr {{ "^" BitwiseAndExpr }} ;

BitwiseAndExpr = ShiftExpr {{ "&" ShiftExpr }} ;

ShiftExpr = AddExpr {{ ( "<<" | ">>" ) AddExpr }} ;

AddExpr = MulExpr {{ ( "+" | "-" ) MulExpr }} ;

MulExpr = UnaryExpr {{ ( "*" | "/" | "%" ) UnaryExpr }} ;

UnaryExpr = ( "-" | "!" | "&" [ "mut" ] | "*" ) UnaryExpr
          | PostfixExpr
          ;

PostfixExpr = PrimaryExpr {{ Postfix }} ;

Postfix = "(" [ ArgList ] ")"                 (* function call *)
        | "[" Expression "]"                  (* index *)
        | "." Identifier [ GenericArgs ]      (* field/method access *)
        | "." Integer                         (* tuple index *)
        | "?"                                 (* try operator *)
        | "as" Type                           (* type cast *)
        ;

ArgList = Expression {{ "," Expression }} [ "," ] ;

PrimaryExpr = Literal
            | Identifier [ GenericArgs ]
            | "(" Expression ")"              (* parenthesized *)
            | "(" [ Expression {{ "," Expression }} [ "," ] ] ")"  (* tuple *)
            | "[" [ Expression {{ "," Expression }} [ "," ] ] "]"  (* array *)
            | "[" Expression ";" Expression "]"                    (* array repeat *)
            | Block
            | IfExpr
            | MatchExpr
            | WhileExpr
            | ForExpr
            | LoopExpr
            | ReturnExpr
            | BreakExpr
            | ContinueExpr
            | ClosureExpr
            | StructExpr
            | AwaitExpr
            | SpawnExpr
            ;

Block = INDENT {{ Statement }} [ Expression ] DEDENT
      | "{{" {{ Statement }} [ Expression ] "}}"
      ;

(* ============================================ *)
(* Control Flow *)
(* ============================================ *)

IfExpr = "if" Expression Block [ "else" ( IfExpr | Block ) ] ;

MatchExpr = "m" Expression INDENT {{ MatchArm }} DEDENT ;

MatchArm = Pattern {{ "|" Pattern }} [ "if" Expression ] "=>" Expression ;

WhileExpr = "wh" Expression Block
          | "wh" "let" Pattern "=" Expression Block
          ;

ForExpr = "for" Pattern "in" Expression Block ;

LoopExpr = "loop" Block ;

ReturnExpr = "ret" [ Expression ] ;

BreakExpr = "break" [ Expression ] ;

ContinueExpr = "continue" ;

AwaitExpr = "await" Expression ;

SpawnExpr = "spawn" Expression ;

ClosureExpr = "|" [ ParamList ] "|" [ "->" Type ] Expression ;

StructExpr = TypePath "{{" [ FieldInit {{ "," FieldInit }} [ "," ] ] "}}" ;

FieldInit = Identifier [ ":" Expression ] ;

(* ============================================ *)
(* Statements *)
(* ============================================ *)

Statement = LetStatement
          | ExprStatement
          ;

LetStatement = Identifier [ ":" Type ] ":=" Expression ;

ExprStatement = Expression [ ";" ] ;

(* ============================================ *)
(* Patterns *)
(* ============================================ *)

Pattern = LiteralPattern
        | IdentifierPattern
        | WildcardPattern
        | TuplePattern
        | StructPattern
        | EnumPattern
        | RangePattern
        | ReferencePattern
        | OrPattern
        ;

LiteralPattern = Integer | Float | String | Char | "true" | "false" ;

IdentifierPattern = [ "mut" ] Identifier ;

WildcardPattern = "_" ;

TuplePattern = "(" [ Pattern {{ "," Pattern }} [ "," ] ] ")" ;

StructPattern = TypePath "{{" [ FieldPattern {{ "," FieldPattern }} [ "," ] ] "}}" ;

FieldPattern = Identifier [ ":" Pattern ] ;

EnumPattern = TypePath [ "(" [ Pattern {{ "," Pattern }} [ "," ] ] ")" ] ;

RangePattern = Pattern ".." [ "=" ] Pattern ;

ReferencePattern = "&" [ "mut" ] Pattern ;

OrPattern = Pattern {{ "|" Pattern }} ;

(* ============================================ *)
(* Lexical Elements *)
(* ============================================ *)

Identifier = IdentifierStart {{ IdentifierContinue }} ;

IdentifierStart = "a".."z" | "A".."Z" | "_" ;

IdentifierContinue = IdentifierStart | "0".."9" ;

Literal = Integer | Float | String | Char | "true" | "false" ;

Integer = DecimalInteger | HexInteger | BinaryInteger | OctalInteger ;

DecimalInteger = Digit {{ Digit | "_" }} ;

HexInteger = "0x" HexDigit {{ HexDigit | "_" }} ;

BinaryInteger = "0b" ( "0" | "1" ) {{ "0" | "1" | "_" }} ;

OctalInteger = "0o" OctalDigit {{ OctalDigit | "_" }} ;

Float = Digit {{ Digit }} "." Digit {{ Digit }} [ Exponent ] ;

Exponent = ( "e" | "E" ) [ "+" | "-" ] Digit {{ Digit }} ;

String = '"' {{ StringChar | EscapeSequence }} '"' ;

Char = "'" ( CharChar | EscapeSequence ) "'" ;

StringChar = ? any character except '"', '\', or newline ? ;

CharChar = ? any character except "'", '\', or newline ? ;

EscapeSequence = '\' ( 'n' | 'r' | 't' | '\' | '"' | "'" | '0' | 'x' HexDigit HexDigit ) ;

Digit = "0".."9" ;

HexDigit = Digit | "a".."f" | "A".."F" ;

OctalDigit = "0".."7" ;

(* ============================================ *)
(* Comments and Whitespace *)
(* ============================================ *)

LineComment = "//" {{ ? any character except newline ? }} ;

BlockComment = "/*" {{ ? any character ? }} "*/" ;

INDENT = ? increase in indentation level ? ;

DEDENT = ? decrease in indentation level ? ;

(* ============================================ *)
(* Built-in Types *)
(* ============================================ *)

(* Primitive types: Int, Float, Bool, Char, Str, () *)
(* Integer types: i8, i16, i32, i64, u8, u16, u32, u64 *)
(* Generic types: Option[T], Result[T, E], Vec[T], Map[K, V] *)
(* Array types: [T], [T; N] *)
(* Tuple types: (T1, T2, ...) *)
(* Function types: fn(Args) -> Ret *)
(* Reference types: &T, &mut T *)

(* Async types: Task[T], Future[T] *)
(* Channel types: Sender[T], Receiver[T] *)
(* Sync types: Mutex[T], MutexGuard[T] *)

(* Network types: TcpStream, TcpListener, UdpSocket, TlsStream *)
(* Database types: Database, Statement, Row *)
(* HTTP types: HttpRequest, HttpResponse *)

(* C FFI types: CInt, CUInt, CLong, CULong, CFloat, CDouble, CSize *)
(* Pointer types: *T (raw pointer), *Void (void pointer) *)

(* JSON type: Json *)

(* Shorthand Keywords *)
(* These are aliases for common keywords to reduce token count *)
shorthand_keyword = 'f' (* function *)
                  | 's' (* struct *)
                  | 'e' (* enum *)
                  | 't' (* trait *)
                  | 'i' (* impl *)
                  | 'm' (* match *)
                  | 'us' (* use *)
                  | 'wh' (* while *)
                  | 'lp' (* loop *)
                  | 'br' (* break *)
                  | 'ct' (* continue *)
                  | 'ret' (* return *)
                  | 'as' (* async *)
                  | 'sp' (* spawn *)
                  | 'aw' (* await *) ;

(* Indentation Rules *)
(* FORMA uses significant whitespace like Python *)
(* Blocks are delimited by INDENT and DEDENT tokens *)
(* INDENT is generated when indentation increases *)
(* DEDENT is generated when indentation decreases *)
(* Tab characters are not allowed - use spaces only *)
indentation = INDENT statement* DEDENT ;

(* Contextual Keywords *)
(* Single-letter keywords can be used as identifiers when unambiguous *)
(* The parser uses lookahead to determine if f/s/e/t/i/m is a keyword or identifier *)
(* Example: 'f' followed by identifier and '(' is function definition *)
(* Example: 'f' followed by ':' is identifier in struct field *)

(* Operator Precedence (highest to lowest) *)
(* 1. Primary: literals, identifiers, parenthesized expressions *)
(* 2. Postfix: function calls, method calls, field access, indexing *)
(* 3. Unary: -, !, & *)
(* 4. Multiplicative: *, /, % *)
(* 5. Additive: +, - *)
(* 6. Shift: <<, >> *)
(* 7. Bitwise AND: & *)
(* 8. Bitwise XOR: ^ *)
(* 9. Bitwise OR: | *)
(* 10. Comparison: ==, !=, <, >, <=, >= *)
(* 11. Logical AND: && *)
(* 12. Logical OR: || *)
(* 13. Range: .., ..= *)
(* 14. Assignment: =, +=, -=, *=, /=, %= *)

(* End of FORMA Grammar *)
"#
    );
}

fn print_grammar_json() {
    let grammar = serde_json::json!({
        "name": "FORMA",
        "version": "0.1.0",
        "fileExtensions": [".forma"],
        "rules": {
            "Program": {
                "type": "sequence",
                "elements": [{"type": "repeat", "element": "Item"}]
            },
            "Item": {
                "type": "choice",
                "alternatives": ["Function", "Struct", "Enum", "Trait", "Impl", "TypeAlias", "Use", "Module", "Const"]
            },
            "Function": {
                "type": "sequence",
                "elements": [
                    {"type": "optional", "element": {"type": "literal", "value": "async"}},
                    {"type": "literal", "value": "f"},
                    {"type": "ref", "rule": "Identifier"},
                    {"type": "optional", "element": {"type": "ref", "rule": "GenericParams"}},
                    {"type": "literal", "value": "("},
                    {"type": "optional", "element": {"type": "ref", "rule": "ParamList"}},
                    {"type": "literal", "value": ")"},
                    {"type": "optional", "element": {"type": "sequence", "elements": [
                        {"type": "literal", "value": "->"},
                        {"type": "ref", "rule": "Type"}
                    ]}},
                    {"type": "ref", "rule": "FunctionBody"}
                ]
            },
            "Struct": {
                "type": "sequence",
                "elements": [
                    {"type": "literal", "value": "s"},
                    {"type": "ref", "rule": "Identifier"},
                    {"type": "optional", "element": {"type": "ref", "rule": "GenericParams"}},
                    {"type": "optional", "element": {"type": "sequence", "elements": [
                        {"type": "literal", "value": "="},
                        {"type": "ref", "rule": "StructBody"}
                    ]}}
                ]
            },
            "Enum": {
                "type": "sequence",
                "elements": [
                    {"type": "literal", "value": "e"},
                    {"type": "ref", "rule": "Identifier"},
                    {"type": "optional", "element": {"type": "ref", "rule": "GenericParams"}},
                    {"type": "literal", "value": "="},
                    {"type": "ref", "rule": "Variant"},
                    {"type": "repeat", "element": {"type": "sequence", "elements": [
                        {"type": "literal", "value": "|"},
                        {"type": "ref", "rule": "Variant"}
                    ]}}
                ]
            },
            "Trait": {
                "type": "sequence",
                "elements": [
                    {"type": "literal", "value": "t"},
                    {"type": "ref", "rule": "Identifier"},
                    {"type": "optional", "element": {"type": "ref", "rule": "GenericParams"}},
                    {"type": "literal", "value": "{"},
                    {"type": "repeat", "element": {"type": "ref", "rule": "TraitItem"}},
                    {"type": "literal", "value": "}"}
                ]
            },
            "Impl": {
                "type": "sequence",
                "elements": [
                    {"type": "literal", "value": "impl"},
                    {"type": "optional", "element": {"type": "ref", "rule": "GenericParams"}},
                    {"type": "optional", "element": {"type": "sequence", "elements": [
                        {"type": "ref", "rule": "Type"},
                        {"type": "literal", "value": "for"}
                    ]}},
                    {"type": "ref", "rule": "Type"},
                    {"type": "literal", "value": "{"},
                    {"type": "repeat", "element": {"type": "ref", "rule": "ImplItem"}},
                    {"type": "literal", "value": "}"}
                ]
            },
            "Type": {
                "type": "choice",
                "alternatives": ["TypePath", "ArrayType", "TupleType", "FunctionType", "ReferenceType", "NeverType"]
            },
            "Expression": {
                "type": "choice",
                "alternatives": ["Literal", "Identifier", "BinaryExpr", "UnaryExpr", "CallExpr", "IndexExpr", "FieldExpr", "IfExpr", "MatchExpr", "WhileExpr", "ForExpr", "Block", "ClosureExpr"]
            },
            "Statement": {
                "type": "choice",
                "alternatives": ["LetStatement", "ExprStatement"]
            },
            "Pattern": {
                "type": "choice",
                "alternatives": ["LiteralPattern", "IdentifierPattern", "WildcardPattern", "TuplePattern", "StructPattern", "EnumPattern"]
            },
            "primitiveTypes": ["Int", "Float", "Bool", "Char", "Str", "()"],
            "keywords": ["f", "s", "e", "t", "impl", "type", "use", "mod", "const", "if", "else", "m", "wh", "for", "loop", "ret", "break", "continue", "let", "mut", "async", "await", "true", "false", "as", "where", "fn", "Self"],
            "operators": {
                "arithmetic": ["+", "-", "*", "/", "%"],
                "comparison": ["==", "!=", "<", "<=", ">", ">="],
                "logical": ["&&", "||", "!"],
                "bitwise": ["&", "|", "^", "<<", ">>"],
                "assignment": ["=", "+=", "-=", "*=", "/=", "%=", "&&=", "||="],
                "other": ["->", "=>", "::", ":", ".", "..", "..=", "?", ":="]
            }
        }
    });
    print_json(&grammar);
}

/// Start an interactive REPL
fn repl() -> Result<(), String> {
    use rustyline::error::ReadlineError;
    use rustyline::DefaultEditor;

    println!("FORMA REPL v0.1.0");
    println!("Type :help for commands, :quit to exit");
    println!();

    let mut rl = DefaultEditor::new().map_err(|e| format!("Failed to initialize readline: {}", e))?;

    // Keep track of defined functions for the session
    let mut session_code = String::new();

    loop {
        let readline = rl.readline("forma> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                // Add to history
                let _ = rl.add_history_entry(line);

                // Handle REPL commands
                if line.starts_with(':') {
                    match line {
                        ":quit" | ":q" | ":exit" => {
                            println!("Goodbye!");
                            break;
                        }
                        ":help" | ":h" => {
                            println!("REPL Commands:");
                            println!("  :help, :h     - Show this help");
                            println!("  :quit, :q     - Exit the REPL");
                            println!("  :clear        - Clear session definitions");
                            println!("  :type <expr>  - Show the type of an expression");
                            println!("  :defs         - Show current definitions");
                            println!();
                            println!("Enter FORMA expressions or definitions.");
                            println!("Expressions are evaluated immediately.");
                            println!("Function definitions are saved for the session.");
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
                        cmd if cmd.starts_with(":type ") => {
                            let expr = &cmd[6..];
                            repl_type_of(expr, &session_code);
                            continue;
                        }
                        _ => {
                            println!("Unknown command: {}", line);
                            println!("Type :help for available commands.");
                            continue;
                        }
                    }
                }

                // Check if this is a definition (starts with f, s, e, t, etc.)
                let is_definition = line.starts_with("f ") || line.starts_with("s ")
                    || line.starts_with("e ") || line.starts_with("t ")
                    || line.starts_with("impl ") || line.starts_with("type ");

                if is_definition {
                    // Add to session definitions
                    session_code.push_str(line);
                    session_code.push('\n');

                    // Try to parse to validate
                    if let Err(e) = repl_validate_code(&session_code) {
                        // Remove the bad definition
                        let lines: Vec<&str> = session_code.lines().collect();
                        session_code = lines[..lines.len().saturating_sub(1)].join("\n");
                        if !session_code.is_empty() {
                            session_code.push('\n');
                        }
                        println!("Error: {}", e);
                    } else {
                        println!("Defined.");
                    }
                } else {
                    // Evaluate as expression
                    repl_eval_expr(line, &session_code);
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
        errors.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join("; ")
    })?;
    Ok(())
}

/// Evaluate an expression in the REPL
fn repl_eval_expr(expr: &str, session_code: &str) {
    // Wrap the expression in a main function that prints the result
    // Use print() to output the value since FORMA requires explicit return types
    let code = format!(
        "{}\nf __repl_main__() -> Int\n    __result__ := {}\n    print(__result__)\n    0\n",
        session_code,
        expr
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
    let program = match Lowerer::new().lower(&ast) {
        Ok(prog) => prog,
        Err(errors) => {
            for e in errors.iter().take(1) {
                println!("Lowering error: {}", e.message);
            }
            return;
        }
    };

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
        session_code,
        expr
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
fn fmt(file: &PathBuf, write: bool, check: bool) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy().to_string();

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        for error in &lex_errors {
            eprintln!("error[LEX]: {}:{}: {}", error.span.line, error.span.column, error.message);
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    // Parse
    let parser = FormaParser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => {
            for error in &errors {
                eprintln!("error[PARSE]: {}", error);
            }
            return Err(format!("{} parse error(s)", errors.len()));
        }
    };

    // Format
    let mut formatter = forma::Formatter::new();
    let formatted = formatter.format(&ast);

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
    let rt = tokio::runtime::Runtime::new()
        .map_err(|e| format!("Failed to create runtime: {}", e))?;

    rt.block_on(async {
        forma::lsp::run_server().await;
    });

    Ok(())
}

fn read_file(path: &PathBuf) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("cannot read '{}': {}", path.display(), e))
}
