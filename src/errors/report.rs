//! Pretty error reporting using ariadne.
//!
//! This module provides nice error output with source context,
//! colored output, and helpful messages.

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::lexer::Span;

/// Report a single error with source context.
pub fn report_error(filename: &str, source: &str, span: Span, message: &str, help: Option<&str>) {
    let offset = span.start;

    let mut report = Report::build(ReportKind::Error, filename, offset)
        .with_message(message)
        .with_label(
            Label::new((filename, offset..span.end))
                .with_message(message)
                .with_color(Color::Red),
        );

    if let Some(h) = help {
        report = report.with_help(h);
    }

    let _ = report.finish().print((filename, Source::from(source)));
}

/// Report a warning with source context.
pub fn report_warning(filename: &str, source: &str, span: Span, message: &str) {
    let offset = span.start;

    let _ = Report::build(ReportKind::Warning, filename, offset)
        .with_message(message)
        .with_label(
            Label::new((filename, offset..span.end))
                .with_message(message)
                .with_color(Color::Yellow),
        )
        .finish()
        .print((filename, Source::from(source)));
}

/// Report multiple errors efficiently.
pub fn report_errors(filename: &str, source: &str, errors: &[(Span, String, Option<String>)]) {
    for (span, message, help) in errors {
        report_error(filename, source, *span, message, help.as_deref());
    }
}

/// Error context for building detailed reports.
pub struct ErrorContext<'a> {
    filename: &'a str,
    source: &'a str,
}

impl<'a> ErrorContext<'a> {
    pub fn new(filename: &'a str, source: &'a str) -> Self {
        Self { filename, source }
    }

    pub fn error(&self, span: Span, message: &str) {
        report_error(self.filename, self.source, span, message, None);
    }

    pub fn error_with_help(&self, span: Span, message: &str, help: &str) {
        report_error(self.filename, self.source, span, message, Some(help));
    }

    pub fn warning(&self, span: Span, message: &str) {
        report_warning(self.filename, self.source, span, message);
    }
}
