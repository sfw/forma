//! Lexer module for the FORMA compiler.
//!
//! This module is responsible for tokenizing FORMA source code into
//! a stream of tokens that can be consumed by the parser.

pub mod scanner;
pub mod token;

pub use scanner::Scanner;
pub use token::{FStringPart, Span, Token, TokenKind};
