//! Integration tests for the FORMA borrow checker.

use forma::borrow::{BorrowChecker, BorrowError, BorrowErrorKind};
use forma::{Parser, Scanner};

fn check_source(source: &str) -> Result<(), Vec<BorrowError>> {
    let scanner = Scanner::new(source);
    let (tokens, _errors) = scanner.scan_all();
    let parser = Parser::new(&tokens);
    let ast = parser.parse().expect("parse should succeed");
    let mut checker = BorrowChecker::new();
    checker.check(&ast)
}

fn expect_error(source: &str, expected: fn(&BorrowErrorKind) -> bool) {
    let result = check_source(source);
    assert!(result.is_err(), "expected borrow error but got Ok");
    let errors = result.unwrap_err();
    assert!(
        errors.iter().any(|e| expected(&e.kind)),
        "expected specific error kind, got: {:?}",
        errors.iter().map(|e| &e.kind).collect::<Vec<_>>()
    );
}

// =============================================================================
// Valid Borrowing Patterns
// =============================================================================

#[test]
fn test_simple_function_no_refs() {
    let result = check_source(r#"f add(a: Int, b: Int) -> Int = a + b"#);
    assert!(result.is_ok());
}

#[test]
fn test_function_with_ref_param() {
    let result = check_source(r#"f deref(x: &Int) -> Int = *x"#);
    assert!(result.is_ok());
}

#[test]
fn test_function_with_mut_ref_param() {
    let result = check_source(
        r#"f increment(x: &mut Int)
    *x = *x + 1"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_multiple_immut_borrows() {
    let result = check_source(r#"f use_both(a: &Int, b: &Int) -> Int = *a + *b"#);
    assert!(result.is_ok());
}

#[test]
fn test_return_derived_ref() {
    let result = check_source(r#"f first(data: &[Int]) -> &Int = &data[0]"#);
    assert!(result.is_ok());
}

#[test]
fn test_return_field_ref() {
    let result = check_source(
        r#"
s Point
    x: Int
    y: Int

f get_x(p: &Point) -> &Int = &p.x
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_struct_without_refs() {
    let result = check_source(
        r#"
s Point
    x: Int
    y: Int

f new_point(x: Int, y: Int) -> Point = Point { x: x, y: y }
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_enum_without_refs() {
    let result = check_source(
        r#"
e Option[T]
    Some(T)
    None
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_let_binding() {
    let result = check_source(
        r#"f example() -> Int
    x = 42
    x"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_mutable_let_binding() {
    let result = check_source(
        r#"f example() -> Int
    x := 42
    x = 100
    x"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_if_expression() {
    let result = check_source(r#"f max(a: Int, b: Int) -> Int = if a > b then a else b"#);
    assert!(result.is_ok());
}

#[test]
fn test_match_expression() {
    let result = check_source(
        r#"
f classify(x: Int) -> Str
    m x
        0 -> "zero"
        1 -> "one"
        _ -> "other"
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_for_loop() {
    let result = check_source(
        r#"f sum(items: [Int]) -> Int
    total := 0
    for item in items
        total = total + item
    total"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_while_loop() {
    let result = check_source(
        r#"f countdown(n: Int) -> Int
    x := n
    wh x > 0
        x = x - 1
    x"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_closure() {
    let result = check_source(
        r#"
f apply(f: (Int) -> Int, x: Int) -> Int = f(x)

f double(x: Int) -> Int = apply(|n| n * 2, x)
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_impl_block() {
    let result = check_source(
        r#"
s Counter
    value: Int

i Counter
    f new() -> Counter = Counter { value: 0 }

    f get(&self) -> Int = self.value
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_trait_impl() {
    let result = check_source(
        r#"
t Display
    f display(&self) -> Str

s Point
    x: Int
    y: Int

i Display for Point
    f display(&self) -> Str = "point"
"#,
    );
    assert!(result.is_ok());
}

// =============================================================================
// Second-Class Reference Violations
// =============================================================================

#[test]
fn test_reference_in_struct_field() {
    expect_error(
        r#"
s Bad
    data: &Int
"#,
        |kind| matches!(kind, BorrowErrorKind::ReferenceInStruct { .. }),
    );
}

#[test]
fn test_reference_in_tuple_struct() {
    expect_error(r#"s Wrapper(&Int)"#, |kind| {
        matches!(kind, BorrowErrorKind::ReferenceInStruct { .. })
    });
}

#[test]
fn test_mut_reference_in_struct() {
    expect_error(
        r#"
s MutHolder
    data: &mut Int
"#,
        |kind| matches!(kind, BorrowErrorKind::ReferenceInStruct { .. }),
    );
}

#[test]
fn test_reference_in_array() {
    expect_error(
        r#"f bad() -> Int
    x = 42
    arr = [&x]
    0"#,
        |kind| matches!(kind, BorrowErrorKind::ReferenceInCollection),
    );
}

#[test]
fn test_reference_in_array_repeat() {
    expect_error(
        r#"f bad() -> Int
    x = 42
    arr = [&x; 5]
    0"#,
        |kind| matches!(kind, BorrowErrorKind::ReferenceInCollection),
    );
}

// =============================================================================
// Borrow Conflict Violations
// =============================================================================

#[test]
fn test_double_mut_borrow() {
    expect_error(
        r#"f bad() -> Int
    x := 42
    r1 = &mut x
    r2 = &mut x
    0"#,
        |kind| matches!(kind, BorrowErrorKind::DoubleMutableBorrow { .. }),
    );
}

#[test]
fn test_mixed_borrow_immut_then_mut() {
    expect_error(
        r#"f bad() -> Int
    x := 42
    r1 = &x
    r2 = &mut x
    0"#,
        |kind| matches!(kind, BorrowErrorKind::MixedBorrow { .. }),
    );
}

#[test]
fn test_borrow_while_mut_borrowed() {
    expect_error(
        r#"f bad() -> Int
    x := 42
    r1 = &mut x
    r2 = &x
    0"#,
        |kind| matches!(kind, BorrowErrorKind::BorrowWhileMutBorrow { .. }),
    );
}

#[test]
fn test_mut_borrow_of_immutable() {
    expect_error(
        r#"f bad() -> Int
    x = 42
    r = &mut x
    0"#,
        |kind| matches!(kind, BorrowErrorKind::MutBorrowOfImmutable { .. }),
    );
}

// =============================================================================
// Return Reference Violations
// =============================================================================

#[test]
fn test_return_local_ref() {
    expect_error(
        r#"f bad() -> &Int
    x = 42
    &x"#,
        |kind| matches!(kind, BorrowErrorKind::ReturnLocalReference { .. }),
    );
}

#[test]
fn test_return_local_ref_in_block() {
    // Test that returning a reference to a local from a nested block is caught
    expect_error(
        r#"f bad() -> &Int
    x = 42
    y = &x
    y"#,
        |kind| matches!(kind, BorrowErrorKind::ReturnLocalReference { .. }),
    );
}

// =============================================================================
// Conditional and Loop Tests
// =============================================================================

#[test]
fn test_valid_borrow_in_if() {
    let result =
        check_source(r#"f choose(cond: Bool, a: &Int, b: &Int) -> &Int = if cond then a else b"#);
    assert!(result.is_ok());
}

#[test]
fn test_borrow_in_loop() {
    let result = check_source(
        r#"f process(data: &[Int]) -> Int
    sum := 0
    for item in *data
        sum = sum + item
    sum"#,
    );
    assert!(result.is_ok());
}

// =============================================================================
// Complex Valid Patterns
// =============================================================================

#[test]
fn test_method_chaining() {
    let result = check_source(
        r#"
s Builder
    value: Int

i Builder
    f new() -> Builder = Builder { value: 0 }

    f add(&self, n: Int) -> Builder = Builder { value: self.value + n }
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_generic_struct() {
    let result = check_source(
        r#"
s Container[T]
    value: T

f wrap(x: Int) -> Container[Int] = Container { value: x }
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_multiple_ref_params() {
    let result = check_source(
        r#"f swap_refs(a: &mut Int, b: &mut Int)
    temp = *a
    *a = *b
    *b = temp"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_pipeline_operator() {
    let result = check_source(
        r#"
f double(x: Int) -> Int = x * 2

f add_one(x: Int) -> Int = x + 1

f process(x: Int) -> Int = x | double | add_one
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_try_operator() {
    let result = check_source(
        r#"
f fallible(x: Int) -> Int! = x

f use_fallible(x: Int) -> Int!
    result = fallible(x)?
    result
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_option_coalesce() {
    let result = check_source(r#"f with_default(x: Int?, default: Int) -> Int = x ?? default"#);
    assert!(result.is_ok());
}

// =============================================================================
// Error Message Quality Tests
// =============================================================================

#[test]
fn test_error_contains_variable_name() {
    let result = check_source(
        r#"
s Bad
    my_field: &Int
"#,
    );
    let errors = result.unwrap_err();
    let error_msg = format!("{}", errors[0]);
    assert!(
        error_msg.contains("my_field"),
        "error message should contain field name: {}",
        error_msg
    );
}

#[test]
fn test_error_display_implementations() {
    use forma::lexer::Span;

    let span = Span::new(1, 0, 1, 10);

    // Test UseAfterMove
    let err = BorrowError::new(
        BorrowErrorKind::UseAfterMove {
            name: "x".into(),
            moved_at: span,
        },
        span,
    );
    assert!(format!("{}", err).contains("moved value"));

    // Test DoubleMutableBorrow
    let err = BorrowError::new(
        BorrowErrorKind::DoubleMutableBorrow { name: "y".into() },
        span,
    );
    assert!(format!("{}", err).contains("mutable more than once"));

    // Test MixedBorrow
    let err = BorrowError::new(BorrowErrorKind::MixedBorrow { name: "z".into() }, span);
    assert!(format!("{}", err).contains("mutable while immutable"));

    // Test ReferenceInStruct
    let err = BorrowError::new(
        BorrowErrorKind::ReferenceInStruct { field: "f".into() },
        span,
    );
    assert!(format!("{}", err).contains("reference in struct"));

    // Test ReferenceInCollection
    let err = BorrowError::new(BorrowErrorKind::ReferenceInCollection, span);
    assert!(format!("{}", err).contains("collection"));

    // Test ReturnLocalReference
    let err = BorrowError::new(
        BorrowErrorKind::ReturnLocalReference {
            name: "local".into(),
        },
        span,
    );
    assert!(format!("{}", err).contains("local variable"));
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
fn test_empty_function() {
    let result = check_source(r#"f empty() = ()"#);
    assert!(result.is_ok());
}

#[test]
fn test_unit_struct() {
    let result = check_source(r#"s Unit"#);
    assert!(result.is_ok());
}

#[test]
fn test_multiple_functions() {
    let result = check_source(
        r#"
f first(x: Int) -> Int = x + 1

f second(x: Int) -> Int = first(x) + 1

f third(x: Int) -> Int = second(x) + 1
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_type_alias() {
    let result = check_source(
        r#"
type Meters = Int

f distance(m: Meters) -> Meters = m * 2
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_loop_expression() {
    let result = check_source(
        r#"f infinite() -> Int
    x := 0
    lp
        x = x + 1
        if x > 10 then x else x"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_nested_match() {
    let result = check_source(
        r#"
f get_or_default(x: Int?, default: Int) -> Int
    m x
        0 -> default
        n -> n
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_struct_with_many_fields() {
    let result = check_source(
        r#"
s Person
    name: Str
    age: Int
    height: Float
    active: Bool

f make_person(name: Str) -> Person
    Person { name: name, age: 0, height: 0.0, active: true }
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_inline_enum() {
    let result = check_source(r#"e Bool = True | False"#);
    assert!(result.is_ok());
}

#[test]
fn test_generic_function_return() {
    let result = check_source(
        r#"
s Container[T]
    value: T

f get_value(c: &Container[Int]) -> &Int = &c.value
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_trait_with_supertrait() {
    let result = check_source(
        r#"
t Eq
    f eq(&self, other: &Self) -> Bool

t Ord: Eq
    f cmp(&self, other: &Self) -> Int
"#,
    );
    assert!(result.is_ok());
}
