//! ARIA Compiler CLI
//!
//! Command-line interface for the ARIA compiler.

use aria::errors::ErrorContext;
use aria::mir::{Interpreter, Lowerer};
use aria::{BorrowChecker, Parser as AriaParser, Scanner, TypeChecker};
use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;
use std::process;

#[derive(Parser)]
#[command(name = "aria")]
#[command(version = "0.1.0")]
#[command(about = "ARIA v2 compiler - AI-optimized systems programming language")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile an ARIA source file
    Compile {
        /// Input file
        file: PathBuf,

        /// Output file (default: input with .o extension)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Run an ARIA program
    Run {
        /// Input file
        file: PathBuf,

        /// Arguments to pass to the program
        args: Vec<String>,
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
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Compile { file, output: _ } => compile(&file),
        Commands::Run { file, args: _ } => run(&file),
        Commands::Lex { file } => lex(&file),
        Commands::Parse { file } => parse(&file),
        Commands::Check { file } => check(&file),
    };

    if let Err(e) = result {
        eprintln!("error: {}", e);
        process::exit(1);
    }
}

fn compile(file: &PathBuf) -> Result<(), String> {
    let _source = read_file(file)?;
    // TODO: Implement full compilation
    println!("Compilation not yet implemented");
    Ok(())
}

fn run(file: &PathBuf) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy();
    let ctx = ErrorContext::new(&filename, &source);

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        for error in &lex_errors {
            ctx.error(error.span, &error.message);
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    // Parse
    let parser = AriaParser::new(&tokens);
    let ast = parser.parse().map_err(|e| {
        ctx.error_with_help(
            e.span(),
            &format!("{}", e),
            e.help().unwrap_or("check syntax"),
        );
        "parse error".to_string()
    })?;

    // Lower to MIR
    let program = Lowerer::new().lower(&ast).map_err(|errors| {
        for e in &errors {
            ctx.error(e.span, &e.message);
        }
        format!("{} lowering error(s)", errors.len())
    })?;

    // Check for main function
    if !program.functions.contains_key("main") {
        return Err("error: no 'main' function found".to_string());
    }

    // Run the interpreter
    let mut interp = Interpreter::new(program);
    match interp.run("main", &[]) {
        Ok(result) => {
            println!("{}", result);
            Ok(())
        }
        Err(e) => Err(format!("error[RUNTIME]: {}", e)),
    }
}

fn lex(file: &PathBuf) -> Result<(), String> {
    let source = read_file(file)?;
    let scanner = Scanner::new(&source);
    let (tokens, errors) = scanner.scan_all();

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
        for error in errors {
            eprintln!(
                "  {}:{}: {}",
                error.span.line, error.span.column, error.message
            );
        }
        return Err("lexer errors occurred".to_string());
    }

    println!("\n{} tokens", tokens.len());
    Ok(())
}

fn parse(file: &PathBuf) -> Result<(), String> {
    let source = read_file(file)?;
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        for error in &lex_errors {
            eprintln!(
                "error[LEX]: {}:{}: {}",
                error.span.line, error.span.column, error.message
            );
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    let parser = AriaParser::new(&tokens);
    match parser.parse() {
        Ok(ast) => {
            println!("Successfully parsed {} items:", ast.items.len());
            for item in &ast.items {
                print_item(item, 0);
            }
            Ok(())
        }
        Err(e) => {
            let span = e.span();
            eprintln!("error[PARSE]: {}:{}: {}", span.line, span.column, e);
            Err("parse error".to_string())
        }
    }
}

fn print_item(item: &aria::parser::Item, indent: usize) {
    let prefix = "  ".repeat(indent);
    match &item.kind {
        aria::parser::ItemKind::Function(f) => {
            let async_str = if f.is_async { "async " } else { "" };
            println!("{}{}fn {} ({} params)", prefix, async_str, f.name.name, f.params.len());
        }
        aria::parser::ItemKind::Struct(s) => {
            let fields = match &s.kind {
                aria::parser::StructKind::Named(f) => f.len(),
                aria::parser::StructKind::Tuple(t) => t.len(),
                aria::parser::StructKind::Unit => 0,
            };
            println!("{}struct {} ({} fields)", prefix, s.name.name, fields);
        }
        aria::parser::ItemKind::Enum(e) => {
            println!("{}enum {} ({} variants)", prefix, e.name.name, e.variants.len());
        }
        aria::parser::ItemKind::Trait(t) => {
            println!("{}trait {} ({} items)", prefix, t.name.name, t.items.len());
        }
        aria::parser::ItemKind::Impl(i) => {
            let trait_str = i.trait_.as_ref().map(|_| "trait ").unwrap_or("");
            println!("{}{}impl ({} items)", prefix, trait_str, i.items.len());
        }
        aria::parser::ItemKind::TypeAlias(t) => {
            println!("{}type {}", prefix, t.name.name);
        }
        aria::parser::ItemKind::Use(u) => {
            println!("{}use {:?}", prefix, u.tree);
        }
        aria::parser::ItemKind::Module(m) => {
            let items = m.items.as_ref().map(|i| i.len()).unwrap_or(0);
            println!("{}mod {} ({} items)", prefix, m.name.name, items);
        }
        aria::parser::ItemKind::Const(c) => {
            println!("{}const {}", prefix, c.name.name);
        }
    }
}

fn check(file: &PathBuf) -> Result<(), String> {
    let source = read_file(file)?;
    let filename = file.to_string_lossy();
    let ctx = ErrorContext::new(&filename, &source);

    // Lex
    let scanner = Scanner::new(&source);
    let (tokens, lex_errors) = scanner.scan_all();

    if !lex_errors.is_empty() {
        for error in &lex_errors {
            ctx.error(error.span, &error.message);
        }
        return Err(format!("{} lexer error(s)", lex_errors.len()));
    }

    // Parse
    let parser = AriaParser::new(&tokens);
    let ast = parser.parse().map_err(|e| {
        ctx.error_with_help(
            e.span(),
            &format!("{}", e),
            e.help().unwrap_or("check syntax"),
        );
        "parse error".to_string()
    })?;

    let mut error_count = 0;

    // Type check
    let mut type_checker = TypeChecker::new();
    if let Err(errors) = type_checker.check(&ast) {
        for error in &errors {
            ctx.error(error.span, &format!("{}", error));
        }
        error_count += errors.len();
    }

    // Borrow check
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&ast) {
        for error in &errors {
            ctx.error(error.span, &format!("{}", error));
        }
        error_count += errors.len();
    }

    if error_count > 0 {
        Err(format!("{} error(s) found", error_count))
    } else {
        println!("No errors found ({} items)", ast.items.len());
        Ok(())
    }
}

fn read_file(path: &PathBuf) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("cannot read '{}': {}", path.display(), e))
}
