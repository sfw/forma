//! Error handling module for the ARIA compiler.
//!
//! This module provides error types, diagnostics, and utilities
//! for generating helpful compiler error messages.

pub mod diagnostic;
pub mod report;

pub use diagnostic::{
    CompileError, Diagnostic, Label, LabelStyle, LexError, MultiResult, ParseError, Result,
    Severity, TypeError,
};
pub use report::{report_error, report_errors, report_warning, ErrorContext};
