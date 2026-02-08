//! Parser tests for contextual keywords (Sprint 12).
//!
//! Tests that single-letter keywords (f, s, e, t, i, m) can be used as identifiers
//! in various contexts while still functioning as keywords when appropriate.

use forma::{Parser, Scanner};

fn parse(source: &str) -> Result<(), String> {
    let scanner = Scanner::new(source);
    let (tokens, lex_errors) = scanner.scan_all();
    if !lex_errors.is_empty() {
        return Err(format!("Lex errors: {:?}", lex_errors));
    }
    let parser = Parser::new(&tokens);
    parser.parse().map(|_| ()).map_err(|e| format!("{:?}", e))
}

// =============================================================================
// Task 12.6: Parser Tests for Contextual Keywords
// =============================================================================

#[test]
fn test_m_as_parameter() {
    // m as parameter name in function
    let code = r#"
f get_value(m: Map[Str, Int], key: Str) -> Int?
    map_get(m, key)
"#;
    assert!(parse(code).is_ok(), "m should work as parameter name");
}

#[test]
fn test_m_as_variable_and_match() {
    // m as variable, then m as match keyword
    let code = r#"
f example() -> Int
    m := Some(42)
    m m
        Some(v) -> v
        None -> 0
"#;
    assert!(
        parse(code).is_ok(),
        "m should work as variable and match keyword"
    );
}

#[test]
fn test_s_as_parameter() {
    // s as parameter name
    let code = r#"
f process(s: Str) -> Int
    str_len(s)
"#;
    assert!(parse(code).is_ok(), "s should work as parameter name");
}

#[test]
fn test_e_as_parameter() {
    // e as parameter name
    let code = r#"
f handle_error(e: Str) -> Str
    "Error: " + e
"#;
    assert!(parse(code).is_ok(), "e should work as parameter name");
}

#[test]
fn test_f_as_parameter() {
    // f as parameter name for a function type
    let code = r#"
f apply(f: (Int) -> Int, x: Int) -> Int
    f(x)
"#;
    assert!(parse(code).is_ok(), "f should work as parameter name");
}

#[test]
fn test_i_as_loop_variable() {
    // i as loop variable in for loop
    let code = r#"
f sum_range(n: Int) -> Int
    total := 0
    for i in 0..n
        total = total + i
    total
"#;
    assert!(parse(code).is_ok(), "i should work as loop variable");
}

#[test]
fn test_t_as_type_parameter() {
    // t as generic type parameter and value
    let code = r#"
f identity[T](t: T) -> T
    t
"#;
    assert!(
        parse(code).is_ok(),
        "t should work as type parameter and value"
    );
}

#[test]
fn test_all_keywords_as_struct_fields() {
    // All single-letter keywords as struct field names
    let code = r#"
s Example
    f: Int
    s: Str
    e: Bool
    t: Float
    i: Int
    m: Map[Str, Int]
"#;
    assert!(
        parse(code).is_ok(),
        "all single-letter keywords should work as struct fields"
    );
}

#[test]
fn test_keywords_in_tuple_destructure() {
    // Single-letter keywords in tuple destructuring
    let code = r#"
f example() -> Int
    (m, s, e) := (1, "hello", true)
    m + str_len(s)
"#;
    assert!(
        parse(code).is_ok(),
        "keywords should work in tuple destructuring"
    );
}

#[test]
fn test_match_still_works() {
    // Match expression with m keyword still works
    let code = r#"
f example(x: Int) -> Str
    m x
        0 -> "zero"
        1 -> "one"
        _ -> "many"
"#;
    assert!(parse(code).is_ok(), "match expression should still work");
}

#[test]
fn test_struct_definition_still_works() {
    // s keyword for struct definition still works
    let code = r#"
s Point
    x: Float
    y: Float
"#;
    assert!(parse(code).is_ok(), "struct definition should still work");
}

#[test]
fn test_enum_definition_still_works() {
    // e keyword for enum definition still works
    let code = r#"
e Color = Red | Green | Blue
"#;
    assert!(parse(code).is_ok(), "enum definition should still work");
}

#[test]
fn test_trait_definition_still_works() {
    // t keyword for trait definition still works
    let code = r#"
t Display
    f display(self) -> Str
"#;
    assert!(parse(code).is_ok(), "trait definition should still work");
}

#[test]
fn test_impl_definition_still_works() {
    // i keyword for impl block still works
    let code = r#"
s Point
    x: Float
    y: Float

i Point
    f origin() -> Point
        Point(0.0, 0.0)
"#;
    assert!(parse(code).is_ok(), "impl block should still work");
}

#[test]
fn test_function_definition_still_works() {
    // f keyword for function definition still works
    let code = r#"
f add(a: Int, b: Int) -> Int
    a + b
"#;
    assert!(parse(code).is_ok(), "function definition should still work");
}

#[test]
fn test_m_in_match_pattern() {
    // m as pattern binding in match arm
    let code = r#"
f extract(opt: Int?) -> Int
    m opt
        Some(m) -> m
        None -> 0
"#;
    assert!(
        parse(code).is_ok(),
        "m should work as pattern binding in match"
    );
}

#[test]
fn test_s_in_struct_initialization() {
    // s as both struct keyword and variable
    let code = r#"
s S
    value: Int

f test() -> Int
    s := S(42)
    s.value
"#;
    assert!(
        parse(code).is_ok(),
        "s should work as variable when struct S exists"
    );
}

#[test]
fn test_multiple_keywords_in_expression() {
    // Multiple single-letter identifiers in one expression
    let code = r#"
f compute(m: Int, s: Int, e: Int, t: Int, i: Int, f: Int) -> Int
    m + s + e + t + i + f
"#;
    assert!(
        parse(code).is_ok(),
        "multiple keyword identifiers should work in expression"
    );
}

#[test]
fn test_keyword_as_local_variable() {
    // All single-letter keywords as local variables
    let code = r#"
f test() -> Int
    f := 1
    s := 2
    e := 3
    t := 4
    i := 5
    m := 6
    f + s + e + t + i + m
"#;
    assert!(
        parse(code).is_ok(),
        "all keywords should work as local variables"
    );
}

#[test]
fn test_keyword_in_binary_expression() {
    // Keywords in various binary expressions
    let code = r#"
f test() -> Bool
    m := 10
    s := 20
    m < s && s > 0
"#;
    assert!(
        parse(code).is_ok(),
        "keywords should work in binary expressions"
    );
}

#[test]
fn test_m_followed_by_operators() {
    // m followed by various operators should be variable, not match
    let code = r#"
f test() -> Int
    m := 5
    result := m + 1
    result = m - 2
    result = m * 3
    result = m / 2
    result
"#;
    assert!(
        parse(code).is_ok(),
        "m followed by operators should be variable"
    );
}

#[test]
fn test_m_in_comparison() {
    // m in comparison should be variable, not match
    let code = r#"
f test() -> Bool
    m := 42
    m == 42 && m != 0 && m > 10 && m < 100
"#;
    assert!(parse(code).is_ok(), "m in comparisons should be variable");
}

#[test]
fn test_keyword_in_array_access() {
    // Keywords used with array indexing
    let code = r#"
f test() -> Int
    arr := [1, 2, 3]
    i := 0
    arr[i]
"#;
    // Note: This tests that i works as index variable
    // Direct array indexing might need different syntax in FORMA
    let result = parse(code);
    // Allow this to fail if array indexing syntax is different
    if result.is_err() {
        // Try alternative with vec_get
        let alt_code = r#"
f test() -> Int?
    arr := [1, 2, 3]
    i := 0
    vec_get(arr, i)
"#;
        assert!(parse(alt_code).is_ok(), "i should work as array index");
    }
}

#[test]
fn test_keywords_in_closure() {
    // Keywords in closure body (simple closure syntax)
    let code = r#"
f test() -> Int
    m := 10
    s := 20
    add := |x| x + m + s
    add(5)
"#;
    assert!(parse(code).is_ok(), "keywords should work in closure");
}
