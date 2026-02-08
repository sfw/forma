//! Integration tests for the FORMA lexer.

use forma::{Scanner, TokenKind};

fn tokens(source: &str) -> Vec<TokenKind> {
    let scanner = Scanner::new(source);
    let (tokens, _) = scanner.scan_all();
    tokens.into_iter().map(|t| t.kind).collect()
}

fn has_errors(source: &str) -> bool {
    let scanner = Scanner::new(source);
    let (_, errors) = scanner.scan_all();
    !errors.is_empty()
}

#[test]
fn test_function_declaration() {
    let source = "f add(a: Int, b: Int) -> Int";
    let toks = tokens(source);

    // f is now a contextual keyword - emitted as Ident
    assert!(matches!(toks[0], TokenKind::Ident(ref s) if s == "f"));
    assert!(matches!(toks[1], TokenKind::Ident(_)));
    assert_eq!(toks[2], TokenKind::LParen);
}

#[test]
fn test_struct_declaration() {
    let source = "s Point\n    x: Float\n    y: Float";
    let toks = tokens(source);

    // s is now a contextual keyword - emitted as Ident
    assert!(matches!(toks[0], TokenKind::Ident(ref s) if s == "s"));
    assert!(matches!(toks[1], TokenKind::Ident(ref s) if s == "Point"));
    assert!(toks.contains(&TokenKind::Indent));
}

#[test]
fn test_enum_declaration() {
    let source = "e Direction\n    North\n    South";
    let toks = tokens(source);

    // e is now a contextual keyword - emitted as Ident
    assert!(matches!(toks[0], TokenKind::Ident(ref s) if s == "e"));
    assert!(matches!(toks[1], TokenKind::Ident(ref s) if s == "Direction"));
}

#[test]
fn test_trait_declaration() {
    let source = "t Display\n    f display(&self) -> Str";
    let toks = tokens(source);

    // t is now a contextual keyword - emitted as Ident
    assert!(matches!(toks[0], TokenKind::Ident(ref s) if s == "t"));
    assert!(matches!(toks[1], TokenKind::Ident(ref s) if s == "Display"));
}

#[test]
fn test_impl_block() {
    let source = "i Display for Point\n    f display(&self) -> Str";
    let toks = tokens(source);

    // i is now a contextual keyword - emitted as Ident
    assert!(matches!(toks[0], TokenKind::Ident(ref s) if s == "i"));
    assert!(matches!(toks[1], TokenKind::Ident(ref s) if s == "Display"));
}

#[test]
fn test_match_expression() {
    let source = "m x\n    0 -> \"zero\"\n    _ -> \"other\"";
    let toks = tokens(source);

    // m is now a contextual keyword - emitted as Ident
    assert!(matches!(toks[0], TokenKind::Ident(ref s) if s == "m"));
    assert!(toks.contains(&TokenKind::Arrow));
}

#[test]
fn test_all_keywords() {
    // Single-letter keywords are now contextual - emitted as Ident tokens
    let contextual_keywords = vec![
        ("f", "f"),
        ("s", "s"),
        ("e", "e"),
        ("t", "t"),
        ("i", "i"),
        ("m", "m"),
    ];

    for (source, expected_name) in contextual_keywords {
        let toks = tokens(source);
        assert!(
            matches!(toks[0], TokenKind::Ident(ref s) if s == expected_name),
            "contextual keyword '{}' should produce Ident(\"{}\")",
            source, expected_name
        );
    }

    // Multi-character keywords remain true keywords
    let keywords = vec![
        ("if", TokenKind::If),
        ("then", TokenKind::Then),
        ("else", TokenKind::Else),
        ("for", TokenKind::For),
        ("in", TokenKind::In),
        ("wh", TokenKind::Wh),
        ("lp", TokenKind::Lp),
        ("br", TokenKind::Br),
        ("ct", TokenKind::Ct),
        ("ret", TokenKind::Ret),
        ("as", TokenKind::As),
        ("aw", TokenKind::Aw),
        ("us", TokenKind::Us),
        ("md", TokenKind::Md),
        ("pub", TokenKind::Pub),
        ("mut", TokenKind::Mut),
        ("mv", TokenKind::Mv),
        ("un", TokenKind::Un),
        ("type", TokenKind::Type),
        ("where", TokenKind::Where),
    ];

    for (source, expected) in keywords {
        let toks = tokens(source);
        assert_eq!(
            toks[0], expected,
            "keyword '{}' should produce {:?}",
            source, expected
        );
    }
}

#[test]
fn test_all_operators() {
    let operators = vec![
        ("+", TokenKind::Plus),
        ("-", TokenKind::Minus),
        ("*", TokenKind::Star),
        ("/", TokenKind::Slash),
        ("%", TokenKind::Percent),
        ("==", TokenKind::EqEq),
        ("!=", TokenKind::BangEq),
        ("<", TokenKind::Lt),
        ("<=", TokenKind::LtEq),
        (">", TokenKind::Gt),
        (">=", TokenKind::GtEq),
        ("&&", TokenKind::AmpAmp),
        ("||", TokenKind::PipePipe),
        ("!", TokenKind::Bang),
        ("&", TokenKind::Amp),
        ("|", TokenKind::Pipe),
        ("^", TokenKind::Caret),
        ("<<", TokenKind::LtLt),
        (">>", TokenKind::GtGt),
        ("=", TokenKind::Eq),
        (":=", TokenKind::ColonEq),
        ("+=", TokenKind::PlusEq),
        ("-=", TokenKind::MinusEq),
        ("*=", TokenKind::StarEq),
        ("/=", TokenKind::SlashEq),
        ("?", TokenKind::Question),
        ("??", TokenKind::QuestionQuestion),
        ("->", TokenKind::Arrow),
        ("=>", TokenKind::FatArrow),
        ("..", TokenKind::DotDot),
        ("..=", TokenKind::DotDotEq),
        ("::", TokenKind::ColonColon),
        (".", TokenKind::Dot),
        (",", TokenKind::Comma),
        ("@", TokenKind::At),
    ];

    for (source, expected) in operators {
        let toks = tokens(source);
        assert_eq!(
            toks[0], expected,
            "operator '{}' should produce {:?}",
            source, expected
        );
    }
}

#[test]
fn test_integer_literals() {
    assert_eq!(tokens("0")[0], TokenKind::Int(0));
    assert_eq!(tokens("42")[0], TokenKind::Int(42));
    assert_eq!(tokens("1_000_000")[0], TokenKind::Int(1_000_000));
    assert_eq!(tokens("0xFF")[0], TokenKind::Int(255));
    assert_eq!(tokens("0xDEAD_BEEF")[0], TokenKind::Int(0xDEAD_BEEF));
    assert_eq!(tokens("0b1010")[0], TokenKind::Int(0b1010));
    assert_eq!(tokens("0b1111_0000")[0], TokenKind::Int(0b1111_0000));
    assert_eq!(tokens("0o755")[0], TokenKind::Int(0o755));
}

#[test]
fn test_float_literals() {
    #[allow(clippy::approx_constant)]
    let pi_approx = 3.14;
    assert_eq!(tokens("3.14")[0], TokenKind::Float(pi_approx));
    assert_eq!(tokens("2.5e10")[0], TokenKind::Float(2.5e10));
    assert_eq!(tokens("1.0e-5")[0], TokenKind::Float(1.0e-5));
    assert_eq!(tokens("1E10")[0], TokenKind::Float(1E10));
}

#[test]
fn test_string_literals() {
    assert_eq!(
        tokens("\"hello\"")[0],
        TokenKind::String("hello".to_string())
    );
    assert_eq!(
        tokens("\"with\\nnewline\"")[0],
        TokenKind::String("with\nnewline".to_string())
    );
    assert_eq!(
        tokens("\"with\\ttab\"")[0],
        TokenKind::String("with\ttab".to_string())
    );
    assert_eq!(
        tokens("\"escaped\\\"quote\"")[0],
        TokenKind::String("escaped\"quote".to_string())
    );
}

#[test]
fn test_raw_strings() {
    assert_eq!(
        tokens("`raw string`")[0],
        TokenKind::String("raw string".to_string())
    );
    assert_eq!(
        tokens("`no\\escapes`")[0],
        TokenKind::String("no\\escapes".to_string())
    );
}

#[test]
fn test_char_literals() {
    assert_eq!(tokens("'a'")[0], TokenKind::Char('a'));
    assert_eq!(tokens("'\\n'")[0], TokenKind::Char('\n'));
    assert_eq!(tokens("'\\t'")[0], TokenKind::Char('\t'));
    assert_eq!(tokens("'\\''")[0], TokenKind::Char('\''));
}

#[test]
fn test_boolean_literals() {
    assert_eq!(tokens("T")[0], TokenKind::True);
    assert_eq!(tokens("F")[0], TokenKind::False);
    assert_eq!(tokens("true")[0], TokenKind::True);
    assert_eq!(tokens("false")[0], TokenKind::False);
}

#[test]
fn test_none_literal() {
    assert_eq!(tokens("N")[0], TokenKind::None);
    assert_eq!(tokens("none")[0], TokenKind::None);
}

#[test]
fn test_builtin_constructors() {
    assert_eq!(tokens("Some")[0], TokenKind::Some);
    assert_eq!(tokens("Ok")[0], TokenKind::Ok);
    assert_eq!(tokens("Err")[0], TokenKind::Err);
    assert_eq!(tokens("ok")[0], TokenKind::Ok);
    assert_eq!(tokens("err")[0], TokenKind::Err);
}

#[test]
fn test_comments() {
    let source = "x # this is a comment\ny";
    let toks = tokens(source);

    assert!(matches!(toks[0], TokenKind::Ident(ref s) if s == "x"));
    assert_eq!(toks[1], TokenKind::Newline);
    assert!(matches!(toks[2], TokenKind::Ident(ref s) if s == "y"));
}

#[test]
fn test_indentation_simple() {
    let source = "f foo\n    x = 1";
    let toks = tokens(source);

    assert!(toks.contains(&TokenKind::Indent));
}

#[test]
fn test_indentation_dedent() {
    let source = "f foo\n    x = 1\ny = 2";
    let toks = tokens(source);

    assert!(toks.contains(&TokenKind::Indent));
    assert!(toks.contains(&TokenKind::Dedent));
}

#[test]
fn test_nested_indentation() {
    let source = "if a\n    if b\n        x = 1\n    y = 2\nz = 3";
    let toks = tokens(source);

    // Should have 2 indents and 2 dedents
    let indent_count = toks.iter().filter(|t| **t == TokenKind::Indent).count();
    let dedent_count = toks.iter().filter(|t| **t == TokenKind::Dedent).count();

    assert_eq!(indent_count, 2, "should have 2 indents");
    assert_eq!(dedent_count, 2, "should have 2 dedents");
}

#[test]
fn test_pipeline_operator() {
    let source = "items | filter | map | sum";
    let toks = tokens(source);

    let pipe_count = toks.iter().filter(|t| **t == TokenKind::Pipe).count();
    assert_eq!(pipe_count, 3);
}

#[test]
fn test_error_propagation() {
    let source = "value? result!";
    let toks = tokens(source);

    assert!(toks.contains(&TokenKind::Question));
    assert!(toks.contains(&TokenKind::Bang));
}

#[test]
fn test_generic_brackets() {
    let source = "Map[Str, Int]";
    let toks = tokens(source);

    assert!(toks.contains(&TokenKind::LBracket));
    assert!(toks.contains(&TokenKind::RBracket));
}

#[test]
fn test_unterminated_string_error() {
    assert!(has_errors("\"unterminated"));
}

#[test]
fn test_unterminated_char_error() {
    assert!(has_errors("'"));
    assert!(has_errors("'ab"));
}

#[test]
fn test_invalid_escape_error() {
    assert!(has_errors("\"\\z\""));
}

#[test]
fn test_complete_program() {
    let source = r#"
# A complete FORMA program
us std.io

s Point
    x: Float
    y: Float

f add(a: Int, b: Int) -> Int
    a + b

f main
    x = 42
    y := Point(1.0, 2.0)

    if x > 10
        print "big"
    else
        print "small"

    result = [1, 2, 3] | map(* 2) | sum
"#;

    assert!(!has_errors(source), "complete program should have no errors");
}

// Test that single-letter keywords can be used as variable names
#[test]
fn test_contextual_keywords_as_variables() {
    // m, s, f, e, t, i should all be valid variable names
    let source = "m s f e t i";
    let toks = tokens(source);

    // All should be Ident tokens
    for tok in &toks[..6] {
        assert!(matches!(tok, TokenKind::Ident(_)), "single-letter should be Ident");
    }
}
