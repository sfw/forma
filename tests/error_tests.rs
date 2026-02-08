//! Tests for FORMA error diagnostic types.

use forma::Span;
use forma::errors::{Diagnostic, Label, LabelStyle, Severity};
use forma::errors::{LexError, ParseError, TypeError};

// ============================================================================
// Error Type Construction
// ============================================================================

#[test]
fn test_lex_error_display() {
    let err = LexError::new("unterminated string", Span::new(0, 0, 0, 10));
    assert_eq!(format!("{err}"), "unterminated string");
}

#[test]
fn test_parse_error_with_help() {
    let err =
        ParseError::new("unexpected token", Span::new(0, 0, 0, 5)).with_help("did you mean '+'?");
    assert_eq!(format!("{err}"), "unexpected token");
    assert_eq!(err.help.as_deref(), Some("did you mean '+'?"));
}

#[test]
fn test_type_error_mismatch() {
    let err = TypeError::mismatch("Int", "Str", Span::new(1, 0, 1, 5));
    assert_eq!(err.expected.as_deref(), Some("Int"));
    assert_eq!(err.found.as_deref(), Some("Str"));
    assert!(format!("{err}").contains("type mismatch"));
}

#[test]
fn test_type_error_span() {
    let span = Span::new(2, 5, 2, 10);
    let err = TypeError::new("bad type", span);
    assert_eq!(err.span(), span);
}

// ============================================================================
// Diagnostic Builder
// ============================================================================

#[test]
fn test_diagnostic_error() {
    let diag = Diagnostic::error("type mismatch", Span::new(0, 0, 0, 5));
    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(diag.message, "type mismatch");
}

#[test]
fn test_diagnostic_warning() {
    let diag = Diagnostic::warning("unused variable", Span::new(1, 0, 1, 3));
    assert_eq!(diag.severity, Severity::Warning);
}

#[test]
fn test_diagnostic_with_labels() {
    let diag = Diagnostic::error("mismatched types", Span::new(0, 0, 0, 10))
        .with_label(Label::primary(Span::new(0, 0, 0, 3), "expected Int"))
        .with_label(Label::secondary(Span::new(0, 5, 0, 10), "found Str"));
    assert_eq!(diag.labels.len(), 2);
    assert_eq!(diag.labels[0].style, LabelStyle::Primary);
    assert_eq!(diag.labels[1].style, LabelStyle::Secondary);
}

#[test]
fn test_diagnostic_with_code_and_note() {
    let diag = Diagnostic::error("borrow conflict", Span::new(0, 0, 0, 5))
        .with_code("E0502")
        .with_note("cannot borrow as mutable because it is also borrowed as immutable");
    assert_eq!(diag.code.as_deref(), Some("E0502"));
    assert_eq!(diag.notes.len(), 1);
}

// ============================================================================
// CompileError conversions
// ============================================================================

#[test]
fn test_compile_error_from_lex() {
    let lex = LexError::new("bad char", Span::new(0, 0, 0, 1));
    let ce: forma::CompileError = lex.into();
    assert!(matches!(ce, forma::CompileError::Lex(_)));
    assert_eq!(ce.span(), Span::new(0, 0, 0, 1));
}

#[test]
fn test_compile_error_from_parse() {
    let pe =
        ParseError::new("unexpected EOF", Span::new(0, 0, 0, 0)).with_help("add closing brace");
    let ce: forma::CompileError = pe.into();
    assert!(matches!(ce, forma::CompileError::Parse(_)));
    assert_eq!(ce.help(), Some("add closing brace"));
}
