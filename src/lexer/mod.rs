//! Lexer module for the ARIA compiler.
//!
//! This module is responsible for tokenizing ARIA source code into
//! a stream of tokens that can be consumed by the parser.

pub mod scanner;
pub mod token;

pub use scanner::Scanner;
pub use token::{Span, Token, TokenKind};
