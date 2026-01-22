//! Parser module for the ARIA compiler.
//!
//! This module is responsible for parsing tokens into an Abstract Syntax Tree (AST).
//! It implements a recursive descent parser that handles ARIA's indentation-significant syntax.

pub mod ast;
pub mod parser;

pub use ast::*;
pub use parser::Parser;
