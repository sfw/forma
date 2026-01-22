//! Error reporting and diagnostics for the ARIA compiler.
//!
//! This module provides structured error types and utilities for
//! generating helpful error messages.

use crate::lexer::Span;
use thiserror::Error;

/// A lexer error with source location.
#[derive(Debug, Clone, Error)]
#[error("{message}")]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl LexError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

/// A parser error with source location.
#[derive(Debug, Clone, Error)]
#[error("{message}")]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub help: Option<String>,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            help: None,
        }
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
}

/// A type error with source location.
#[derive(Debug, Clone, Error)]
#[error("{message}")]
pub struct TypeError {
    pub message: String,
    pub span: Span,
    pub expected: Option<String>,
    pub found: Option<String>,
    pub help: Option<String>,
}

impl TypeError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            expected: None,
            found: None,
            help: None,
        }
    }

    pub fn mismatch(expected: impl Into<String>, found: impl Into<String>, span: Span) -> Self {
        Self {
            message: "type mismatch".to_string(),
            span,
            expected: Some(expected.into()),
            found: Some(found.into()),
            help: None,
        }
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

/// Unified compiler error type.
#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("lex error: {0}")]
    Lex(#[from] LexError),

    #[error("parse error: {0}")]
    Parse(#[from] ParseError),

    #[error("type error: {0}")]
    Type(#[from] TypeError),
}

impl CompileError {
    pub fn span(&self) -> Span {
        match self {
            CompileError::Lex(e) => e.span,
            CompileError::Parse(e) => e.span,
            CompileError::Type(e) => e.span,
        }
    }

    pub fn help(&self) -> Option<&str> {
        match self {
            CompileError::Lex(_) => None,
            CompileError::Parse(e) => e.help.as_deref(),
            CompileError::Type(e) => e.help.as_deref(),
        }
    }
}

/// Diagnostic severity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

/// A diagnostic message with optional notes and help text.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub span: Span,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            code: None,
            message: message.into(),
            span,
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            code: None,
            message: message.into(),
            span,
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.notes.push(format!("help: {}", help.into()));
        self
    }
}

/// A label attached to a source span.
#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: String,
    pub style: LabelStyle,
}

impl Label {
    pub fn primary(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            style: LabelStyle::Primary,
        }
    }

    pub fn secondary(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            style: LabelStyle::Secondary,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

/// Result type for compiler operations.
pub type Result<T> = std::result::Result<T, CompileError>;

/// Result type that can collect multiple errors.
pub type MultiResult<T> = std::result::Result<T, Vec<CompileError>>;
