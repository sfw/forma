//! Tests for the FORMA formatter.

use forma::{Formatter, Parser, Scanner};

fn format_source(source: &str) -> String {
    let scanner = Scanner::new(source);
    let (tokens, _) = scanner.scan_all();
    let ast = Parser::new(&tokens).parse().expect("parse should succeed");
    let mut fmt = Formatter::new();
    fmt.format(&ast)
}

#[test]
fn test_format_simple_function() {
    let source = "f add(a: Int, b: Int) -> Int = a + b";
    let formatted = format_source(source);
    assert!(formatted.contains("f add(a: Int, b: Int) -> Int"));
}

#[test]
fn test_format_struct() {
    let source = "s Point\n    x: Int\n    y: Int";
    let formatted = format_source(source);
    assert!(formatted.contains("s Point"));
    assert!(formatted.contains("x: Int"));
    assert!(formatted.contains("y: Int"));
}

#[test]
fn test_format_enum() {
    let source = "e Color\n    Red\n    Green\n    Blue";
    let formatted = format_source(source);
    assert!(formatted.contains("e Color"));
    assert!(formatted.contains("Red"));
    assert!(formatted.contains("Green"));
    assert!(formatted.contains("Blue"));
}

#[test]
fn test_format_type_annotation() {
    let source = "f test(x: [Int], y: {Str: Int}) -> Bool\n    true";
    let formatted = format_source(source);
    assert!(formatted.contains("[Int]"));
    assert!(formatted.contains("{Str: Int}"));
    assert!(formatted.contains("Bool"));
}

#[test]
fn test_format_trait() {
    let source = "t Display\n    f display(&self) -> Str";
    let formatted = format_source(source);
    assert!(formatted.contains("t Display"));
    assert!(formatted.contains("f display(&self) -> Str"));
}

#[test]
fn test_format_trait_with_supertrait() {
    let source = "t Ord: Eq\n    f cmp(&self, other: &Self) -> Ordering";
    let formatted = format_source(source);
    assert!(formatted.contains("t Ord: Eq"));
}

#[test]
fn test_format_impl() {
    let source = "i Display for Point\n    f display(&self) -> Str\n        \"point\"";
    let formatted = format_source(source);
    assert!(formatted.contains("i Display for Point"));
    assert!(formatted.contains("f display(&self) -> Str"));
}

#[test]
fn test_format_inherent_impl() {
    let source = "i Point\n    f new(x: Int) -> Point\n        Point { x: x }";
    let formatted = format_source(source);
    assert!(formatted.contains("i Point"));
    assert!(formatted.contains("f new("));
}

#[test]
fn test_format_use_statement() {
    let source = "us std.io";
    let formatted = format_source(source);
    assert!(formatted.contains("us std.io"));
}

#[test]
fn test_format_idempotence() {
    let source = "f double(n: Int) -> Int = n * 2\n\ns Point\n    x: Int\n    y: Int\n";
    let first = format_source(source);
    let second = format_source(&first);
    assert_eq!(first, second, "formatting should be idempotent");
}
