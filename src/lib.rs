//! FORMA v2 Compiler Library
//!
//! FORMA is an AI-optimized systems programming language with Rust-like
//! memory safety but without explicit lifetime annotations.
//!
//! # Key Features
//!
//! - **Second-class references**: References cannot be stored in structs,
//!   eliminating the need for lifetime annotations
//! - **AI-optimized syntax**: Single-character keywords and minimal tokens
//! - **Indentation-significant**: Clean, Python-like block structure
//! - **Strong type inference**: Hindley-Milner style type inference
//!
//! # Modules
//!
//! - [`lexer`]: Tokenizes source code
//! - [`parser`]: Parses tokens into AST
//! - [`types`]: Type system and inference
//! - [`borrow`]: Borrow checker for memory safety
//! - [`mir`]: Mid-level intermediate representation
//! - [`module`]: Module loading and resolution
//! - [`codegen`]: Code generation backends (LLVM)
//! - [`errors`]: Error types and diagnostics

pub mod borrow;
#[cfg(feature = "llvm")]
pub mod codegen;
pub mod errors;
pub mod fmt;
pub mod lexer;
pub mod mir;
pub mod module;
pub mod parser;
pub mod types;

pub use borrow::{BorrowChecker, BorrowError, BorrowErrorKind};
pub use errors::{CompileError, Result};
pub use lexer::{Scanner, Span, Token, TokenKind};
pub use mir::{Interpreter, Lowerer, Program, Value};
pub use module::{ModuleLoader, ModuleError};
pub use parser::{Parser, SourceFile};
pub use types::{TypeChecker, Ty, TypeError};
pub use fmt::Formatter;
