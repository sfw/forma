//! Integration tests for the FORMA parser.

use forma::parser::*;
use forma::{Parser, Scanner};

fn parse(source: &str) -> Result<SourceFile, Vec<forma::CompileError>> {
    let scanner = Scanner::new(source);
    let (tokens, _) = scanner.scan_all();
    Parser::new(&tokens).parse()
}

fn parse_ok(source: &str) -> SourceFile {
    parse(source).expect("parse should succeed")
}

fn parse_err(source: &str) -> bool {
    parse(source).is_err()
}

// ============================================================================
// Function Parsing
// ============================================================================

#[test]
fn test_simple_function() {
    let ast = parse_ok("f add(a: Int, b: Int) -> Int\n    a + b");
    assert_eq!(ast.items.len(), 1);

    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert_eq!(f.name.name, "add");
        assert_eq!(f.params.len(), 2);
        assert!(f.return_type.is_some());
    } else {
        panic!("expected function");
    }
}

#[test]
fn test_expression_function() {
    let ast = parse_ok("f double(n: Int) -> Int = n * 2");
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert_eq!(f.name.name, "double");
        assert!(matches!(f.body, Some(FnBody::Expr(_))));
    } else {
        panic!("expected function");
    }
}

#[test]
fn test_async_function() {
    let ast = parse_ok("as f fetch(url: Str) -> Data!\n    data");
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert!(f.is_async);
    } else {
        panic!("expected function");
    }
}

#[test]
fn test_function_with_self() {
    let ast = parse_ok("f method(&self) -> Int\n    42");
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert_eq!(f.params.len(), 1);
        assert_eq!(f.params[0].name.name, "self");
    } else {
        panic!("expected function");
    }
}

#[test]
fn test_function_with_defaults() {
    let ast = parse_ok("f connect(host: Str, port: Int = 8080) -> Conn\n    conn");
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert_eq!(f.params.len(), 2);
        assert!(f.params[1].default.is_some());
    } else {
        panic!("expected function");
    }
}

// ============================================================================
// Struct Parsing
// ============================================================================

#[test]
fn test_struct_with_fields() {
    let ast = parse_ok("s Point\n    x: Float\n    y: Float");
    if let ItemKind::Struct(s) = &ast.items[0].kind {
        assert_eq!(s.name.name, "Point");
        if let StructKind::Named(fields) = &s.kind {
            assert_eq!(fields.len(), 2);
        } else {
            panic!("expected named struct");
        }
    } else {
        panic!("expected struct");
    }
}

#[test]
fn test_tuple_struct() {
    let ast = parse_ok("s Color(u8, u8, u8)");
    if let ItemKind::Struct(s) = &ast.items[0].kind {
        if let StructKind::Tuple(types) = &s.kind {
            assert_eq!(types.len(), 3);
        } else {
            panic!("expected tuple struct");
        }
    } else {
        panic!("expected struct");
    }
}

#[test]
fn test_unit_struct() {
    let ast = parse_ok("s Marker");
    if let ItemKind::Struct(s) = &ast.items[0].kind {
        assert!(matches!(s.kind, StructKind::Unit));
    } else {
        panic!("expected struct");
    }
}

#[test]
fn test_generic_struct() {
    let ast = parse_ok("s Box[T]\n    value: T");
    if let ItemKind::Struct(s) = &ast.items[0].kind {
        assert!(s.generics.is_some());
        assert_eq!(s.generics.as_ref().unwrap().params.len(), 1);
    } else {
        panic!("expected struct");
    }
}

// ============================================================================
// Enum Parsing
// ============================================================================

#[test]
fn test_simple_enum() {
    let ast = parse_ok("e Direction\n    North\n    South\n    East\n    West");
    if let ItemKind::Enum(e) = &ast.items[0].kind {
        assert_eq!(e.name.name, "Direction");
        assert_eq!(e.variants.len(), 4);
    } else {
        panic!("expected enum");
    }
}

#[test]
fn test_enum_with_data() {
    let ast = parse_ok("e Option[T]\n    Some(T)\n    None");
    if let ItemKind::Enum(e) = &ast.items[0].kind {
        assert!(e.generics.is_some());
        assert_eq!(e.variants.len(), 2);
        assert!(matches!(e.variants[0].kind, VariantKind::Tuple(_)));
        assert!(matches!(e.variants[1].kind, VariantKind::Unit));
    } else {
        panic!("expected enum");
    }
}

#[test]
fn test_inline_enum() {
    let ast = parse_ok("e Bool = True | False");
    if let ItemKind::Enum(e) = &ast.items[0].kind {
        assert_eq!(e.variants.len(), 2);
    } else {
        panic!("expected enum");
    }
}

// ============================================================================
// Trait Parsing
// ============================================================================

#[test]
fn test_simple_trait() {
    let ast = parse_ok("t Display\n    f display(&self) -> Str");
    if let ItemKind::Trait(t) = &ast.items[0].kind {
        assert_eq!(t.name.name, "Display");
        assert_eq!(t.items.len(), 1);
    } else {
        panic!("expected trait");
    }
}

#[test]
fn test_trait_with_supertrait() {
    let ast = parse_ok("t Ord: Eq\n    f cmp(&self, other: &Self) -> Ordering");
    if let ItemKind::Trait(t) = &ast.items[0].kind {
        assert_eq!(t.supertraits.len(), 1);
    } else {
        panic!("expected trait");
    }
}

// ============================================================================
// Impl Parsing
// ============================================================================

#[test]
fn test_inherent_impl() {
    let ast =
        parse_ok("i Point\n    f new(x: Float, y: Float) -> Point\n        Point { x: x, y: y }");
    if let ItemKind::Impl(i) = &ast.items[0].kind {
        assert!(i.trait_.is_none());
        assert_eq!(i.items.len(), 1);
    } else {
        panic!("expected impl");
    }
}

#[test]
fn test_trait_impl() {
    let ast = parse_ok("i Display for Point\n    f display(&self) -> Str\n        result");
    if let ItemKind::Impl(i) = &ast.items[0].kind {
        assert!(i.trait_.is_some());
    } else {
        panic!("expected impl");
    }
}

// ============================================================================
// Expression Parsing
// ============================================================================

#[test]
fn test_binary_expressions() {
    let ast = parse_ok("f test -> Int = 1 + 2 * 3");
    // Should parse as 1 + (2 * 3) due to precedence
    if let ItemKind::Function(f) = &ast.items[0].kind {
        if let Some(FnBody::Expr(e)) = &f.body {
            assert!(matches!(e.kind, ExprKind::Binary(_, BinOp::Add, _)));
        } else {
            panic!("expected expression body");
        }
    }
}

#[test]
fn test_comparison_expressions() {
    let ast = parse_ok("f test -> Bool = x > 10");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Binary(_, BinOp::Gt, _)));
    }
}

#[test]
fn test_logical_expressions() {
    let ast = parse_ok("f test -> Bool = a && b || c");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        // Should parse as (a && b) || c
        assert!(matches!(e.kind, ExprKind::Binary(_, BinOp::Or, _)));
    }
}

#[test]
fn test_unary_expressions() {
    let ast = parse_ok("f test -> Int = -x");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Unary(UnaryOp::Neg, _)));
    }
}

#[test]
fn test_function_call() {
    let ast = parse_ok("f test -> Int = add(1, 2)");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Call(_, _)));
    }
}

#[test]
fn test_method_call() {
    let ast = parse_ok("f test -> Int = obj.method(arg)");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::MethodCall(_, _, _)));
    }
}

#[test]
fn test_field_access() {
    let ast = parse_ok("f test -> Int = point.x");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Field(_, _)));
    }
}

#[test]
fn test_index_expression() {
    let ast = parse_ok("f test -> Int = arr[0]");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Index(_, _)));
    }
}

#[test]
fn test_array_literal() {
    let ast = parse_ok("f test -> [Int] = [1, 2, 3]");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        if let ExprKind::Array(elements) = &e.kind {
            assert_eq!(elements.len(), 3);
        } else {
            panic!("expected array");
        }
    }
}

#[test]
fn test_tuple_expression() {
    let ast = parse_ok("f test -> (Int, Int) = (1, 2)");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Tuple(_)));
    }
}

#[test]
fn test_struct_literal() {
    let ast = parse_ok("f test -> Point = Point { x: 1.0, y: 2.0 }");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Struct(_, _, _)));
    }
}

#[test]
fn test_if_then_else() {
    let ast = parse_ok("f test -> Int = if x > 0 then 1 else 0");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::If(_)));
    }
}

#[test]
fn test_pipeline() {
    let ast = parse_ok("f test -> Int = x | double | add(1)");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Pipeline(_, _)));
    }
}

#[test]
fn test_try_operator() {
    let ast = parse_ok("f test -> Int! = get_value()?");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Try(_)));
    }
}

#[test]
fn test_closure() {
    let ast = parse_ok("f test -> (Int) -> Int = |x| x * 2");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Closure(_)));
    }
}

#[test]
fn test_cast_expression() {
    // Test T(x) cast syntax
    let ast = parse_ok("f test -> i32 = i32(255)");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Cast(_, _)));
    }
}

#[test]
fn test_cast_float_to_int() {
    let ast = parse_ok("f test -> i32 = i32(3.14)");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Expr(e)) = &f.body
    {
        assert!(matches!(e.kind, ExprKind::Cast(_, _)));
    }
}

// ============================================================================
// Pattern Parsing
// ============================================================================

#[test]
fn test_match_patterns() {
    let source = r#"f test -> Str
    m x
        0 -> "zero"
        1..=9 -> "digit"
        n if n < 0 -> "negative"
        _ -> "other""#;

    let ast = parse_ok(source);
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Block(block)) = &f.body
    {
        assert!(!block.stmts.is_empty());
    }
}

#[test]
fn test_tuple_pattern() {
    let source = "f test\n    (a, b) = get_pair()";
    let ast = parse_ok(source);
    assert_eq!(ast.items.len(), 1);
}

// ============================================================================
// Type Parsing
// ============================================================================

#[test]
fn test_optional_type() {
    let ast = parse_ok("f test -> Int?\n    N");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(ref ty) = f.return_type
    {
        assert!(matches!(ty.kind, TypeKind::Option(_)));
    }
}

#[test]
fn test_result_type() {
    let ast = parse_ok("f test -> Int!\n    ok(42)");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(ref ty) = f.return_type
    {
        assert!(matches!(ty.kind, TypeKind::Result(_, _)));
    }
}

#[test]
fn test_list_type() {
    let ast = parse_ok("f test -> [Int]\n    []");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(ref ty) = f.return_type
    {
        assert!(matches!(ty.kind, TypeKind::List(_)));
    }
}

#[test]
fn test_map_type() {
    let ast = parse_ok("f test -> {Str: Int}\n    {}");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(ref ty) = f.return_type
    {
        assert!(matches!(ty.kind, TypeKind::Map(_, _)));
    }
}

#[test]
fn test_function_type() {
    let ast = parse_ok("f test -> (Int, Int) -> Int\n    add");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(ref ty) = f.return_type
    {
        assert!(matches!(ty.kind, TypeKind::Fn(_, _)));
    }
}

#[test]
fn test_reference_type() {
    let ast = parse_ok("f test(x: &Int) -> Int\n    *x");
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert!(matches!(f.params[0].ty.kind, TypeKind::Ref(_, false)));
    }
}

#[test]
fn test_mutable_reference_type() {
    let ast = parse_ok("f test(x: &mut Int)\n    *x = 1");
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert!(matches!(f.params[0].ty.kind, TypeKind::Ref(_, true)));
    }
}

// ============================================================================
// Statement Parsing
// ============================================================================

#[test]
fn test_let_statement() {
    let ast = parse_ok("f test\n    x = 42\n    y := 0\n    x");
    if let ItemKind::Function(f) = &ast.items[0].kind
        && let Some(FnBody::Block(block)) = &f.body
    {
        assert_eq!(block.stmts.len(), 3);
        // First is immutable let
        if let StmtKind::Let(let_stmt) = &block.stmts[0].kind {
            assert!(!let_stmt.mutable);
        }
        // Second is mutable let
        if let StmtKind::Let(let_stmt) = &block.stmts[1].kind {
            assert!(let_stmt.mutable);
        }
    }
}

// ============================================================================
// Import/Module Parsing
// ============================================================================

#[test]
fn test_use_statement() {
    let ast = parse_ok("us std.io");
    if let ItemKind::Use(u) = &ast.items[0].kind
        && let UseTree::Path(segments, _) = &u.tree
    {
        assert_eq!(segments.len(), 2);
    }
}

#[test]
fn test_use_with_groups() {
    let ast = parse_ok("us std.{io, fs}");
    if let ItemKind::Use(u) = &ast.items[0].kind
        && let UseTree::Path(segments, Some(tree)) = &u.tree
    {
        assert_eq!(segments.len(), 1);
        if let UseTree::Group(trees) = tree.as_ref() {
            assert_eq!(trees.len(), 2);
        }
    }
}

#[test]
fn test_use_glob() {
    let ast = parse_ok("us std.*");
    if let ItemKind::Use(u) = &ast.items[0].kind
        && let UseTree::Path(_, Some(tree)) = &u.tree
    {
        assert!(matches!(tree.as_ref(), UseTree::Glob));
    }
}

// ============================================================================
// Contract Parsing
// ============================================================================

#[test]
fn test_contract_quantifiers_parse() {
    let ast = parse_ok(
        r#"
@post(forall i in 0..result.len()-1: result[i] <= result[i+1])
@post(exists x in result: x > 0)
f ordered(result: [Int]) -> [Int] = result
"#,
    );
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert_eq!(f.postconditions.len(), 2);
    } else {
        panic!("expected function");
    }
}

#[test]
fn test_contract_old_post_only() {
    assert!(parse_err(
        r#"
@pre(old(x) > 0)
f bad(x: Int) -> Int = x
"#
    ));
}

#[test]
fn test_contract_pattern_expansion_sorted() {
    let ast = parse_ok(
        r#"
@sorted(result)
f already_sorted(result: [Int]) -> [Int] = result
"#,
    );
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert_eq!(f.postconditions.len(), 1);
        assert!(f.preconditions.is_empty());
    } else {
        panic!("expected function");
    }
}

#[test]
fn test_contract_pattern_expansion_permutation() {
    let ast = parse_ok(
        r#"
@permutation(items, result)
f same_items(items: [Int], result: [Int]) -> [Int] = result
"#,
    );
    if let ItemKind::Function(f) = &ast.items[0].kind {
        assert_eq!(f.postconditions.len(), 1);
    } else {
        panic!("expected function");
    }
}

#[test]
fn test_contract_pattern_wrong_context() {
    assert!(parse_err(
        r#"
@nonempty(result)
f bad(result: [Int]) -> [Int] = result
"#
    ));
}

// ============================================================================
// Error Cases
// ============================================================================

#[test]
fn test_missing_function_body() {
    // This should be valid - trait method signature
    let ast = parse_ok("t Foo\n    f bar(&self) -> Int");
    assert_eq!(ast.items.len(), 1);
}

#[test]
fn test_unexpected_token() {
    assert!(parse_err("f test -> Int\n    @@@"));
}

// ============================================================================
// Negative Parser Tests
// ============================================================================

fn parse_should_fail(source: &str) {
    let result = parse(source);
    assert!(
        result.is_err(),
        "Expected parse error for: {}",
        source.chars().take(60).collect::<String>()
    );
}

#[test]
fn test_unterminated_string() {
    parse_should_fail("f test -> Str\n    \"hello");
}

#[test]
fn test_missing_closing_paren() {
    parse_should_fail("f test(a: Int -> Int\n    a");
}

#[test]
fn test_missing_closing_bracket() {
    parse_should_fail("f test -> [Int\n    []");
}

#[test]
fn test_double_arrow() {
    parse_should_fail("f test -> -> Int\n    42");
}

#[test]
fn test_invalid_operator_sequence() {
    parse_should_fail("f test -> Int\n    1 + + 2");
}

#[test]
fn test_missing_param_type() {
    parse_should_fail("f test(a) -> Int\n    a");
}

#[test]
fn test_missing_function_name() {
    parse_should_fail("f (a: Int) -> Int\n    a");
}

#[test]
fn test_struct_missing_field_type() {
    parse_should_fail("s Point\n    x\n    y");
}

#[test]
fn test_duplicate_arrow_in_fn_type() {
    parse_should_fail("f test -> (Int) -> -> Int\n    42");
}
