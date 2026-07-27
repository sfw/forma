//! Tests for the FORMA formatter.

use forma::{Formatter, LosslessFormatter, LosslessSyntax, Parser, Scanner};

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

#[test]
fn formatter_preserves_struct_invariants() {
    let source = r#"@inv(balance >= 0, "balance must be non-negative")
@inv(owner.len() > 0)
s Account
    owner: Str
    balance: Int
"#;
    let formatted = format_source(source);
    assert!(formatted.contains("@inv(balance >= 0, \"balance must be non-negative\")"));
    assert!(formatted.contains("@inv(owner.len() > 0)"));
    let reparsed = format_source(&formatted);
    assert_eq!(formatted, reparsed);
}

#[test]
fn formatter_preserves_generic_aliases_and_enum_payloads() {
    let source = r#"pub type Names[T] = [T]

pub e Maybe[T]
    Empty
    Value(T)
    Named(value: T)

pub f identity[T](value: T) -> T = value
"#;
    let formatted = format_source(source);
    assert!(formatted.contains("pub type Names[T] = [T]"));
    assert!(formatted.contains("pub e Maybe[T]"));
    assert!(formatted.contains("Value(T)"));
    assert!(formatted.contains("Named(value: T)"));
    assert!(formatted.contains("pub f identity[T](value: T) -> T"));

    let (tokens, errors) = Scanner::new(&formatted).scan_all();
    assert!(errors.is_empty());
    Parser::new(&tokens)
        .parse()
        .expect("formatted generic source should parse");
}

#[test]
fn lossless_formatter_round_trips_repository_corpus() {
    fn visit(path: &std::path::Path, files: &mut Vec<std::path::PathBuf>) {
        for entry in std::fs::read_dir(path).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                visit(&path, files);
            } else if path
                .extension()
                .is_some_and(|extension| extension == "forma")
            {
                files.push(path);
            }
        }
    }

    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    visit(&root.join("examples"), &mut files);
    visit(&root.join("tests/forma"), &mut files);
    assert!(!files.is_empty());

    for path in files {
        let source = std::fs::read_to_string(&path).unwrap();
        let syntax = LosslessSyntax::parse(source.clone());
        assert_eq!(syntax.reconstructed(), source, "{}", path.display());

        let formatted = LosslessFormatter::format(&syntax);
        let (before_tokens, before_errors) = Scanner::new(&source).scan_all();
        let (after_tokens, after_errors) = Scanner::new(&formatted).scan_all();
        assert_eq!(
            before_errors.len(),
            after_errors.len(),
            "{}",
            path.display()
        );
        if before_errors.is_empty() && Parser::new(&before_tokens).parse().is_ok() {
            assert!(
                Parser::new(&after_tokens).parse().is_ok(),
                "formatted source stopped parsing: {}",
                path.display()
            );
        }
    }
}
