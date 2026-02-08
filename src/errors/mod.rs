//! Error handling module for the FORMA compiler.
//!
//! This module provides error types, diagnostics, and utilities
//! for generating helpful compiler error messages.

pub mod diagnostic;
pub mod report;

pub use diagnostic::{
    CompileError, Diagnostic, Label, LabelStyle, LexError, MultiResult, ParseError, Result,
    Severity, TypeError,
};
pub use report::{ErrorContext, report_error, report_errors, report_warning};
