//! Integration tests for the FORMA type system.

use forma::types::{Ty, TypeId};
use forma::{Parser, Scanner, TypeChecker};

fn check_source(source: &str) -> Result<forma::types::TypedAst, Vec<forma::types::TypeError>> {
    let scanner = Scanner::new(source);
    let (tokens, _errors) = scanner.scan_all();
    let parser = Parser::new(&tokens);
    let ast = parser.parse().expect("parse should succeed");
    let mut checker = TypeChecker::new();
    checker.check(&ast)
}

fn get_fn_type(source: &str, fn_name: &str) -> Option<Ty> {
    let scanner = Scanner::new(source);
    let (tokens, _errors) = scanner.scan_all();
    let parser = Parser::new(&tokens);
    let ast = parser.parse().expect("parse should succeed");
    let mut checker = TypeChecker::new();
    let _ = checker.check(&ast);
    checker.type_of(fn_name)
}

#[test]
fn test_simple_function_type() {
    let ty = get_fn_type(
        r#"
f add(a: Int, b: Int) -> Int
    a + b
"#,
        "add",
    );

    assert!(ty.is_some());
    let ty = ty.unwrap();
    match ty {
        Ty::Fn(params, ret) => {
            assert_eq!(params.len(), 2);
            assert_eq!(params[0], Ty::Int);
            assert_eq!(params[1], Ty::Int);
            assert_eq!(*ret, Ty::Int);
        }
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_function_no_params() {
    let ty = get_fn_type(
        r#"
f answer -> Int
    42
"#,
        "answer",
    );

    assert!(ty.is_some());
    let ty = ty.unwrap();
    match ty {
        Ty::Fn(params, ret) => {
            assert_eq!(params.len(), 0);
            assert_eq!(*ret, Ty::Int);
        }
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_struct_type() {
    let result = check_source(
        r#"
s Point
    x: Int
    y: Int
"#,
    );

    assert!(result.is_ok());
}

#[test]
fn struct_invariant_fields_are_type_checked() {
    assert!(
        check_source(
            r#"
@inv(balance >= 0)
@inv(owner.len() > 0)
s Account
    owner: Str
    balance: Int
"#
        )
        .is_ok()
    );
}

#[test]
fn struct_invariant_must_be_boolean() {
    let errors = check_source(
        r#"
@inv(balance)
s Account
    balance: Int
"#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("mismatch"))
    );
}

#[test]
fn struct_invariant_rejects_unknown_fields() {
    let errors = check_source(
        r#"
@inv(missing >= 0)
s Account
    balance: Int
"#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("undefined variable: missing"))
    );
}

#[test]
fn test_enum_type() {
    let result = check_source(
        r#"
e Color
    Red
    Green
    Blue
"#,
    );

    assert!(result.is_ok());
}

#[test]
fn test_enum_with_data() {
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
fn test_list_type() {
    let ty = get_fn_type(
        r#"
f get_first(items: [Int]) -> Int
    0
"#,
        "get_first",
    );

    assert!(ty.is_some());
    let ty = ty.unwrap();
    match ty {
        Ty::Fn(params, ret) => {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0], Ty::List(_)));
            assert_eq!(*ret, Ty::Int);
        }
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_option_type() {
    let ty = get_fn_type(
        r#"
f maybe(x: Int?) -> Int
    0
"#,
        "maybe",
    );

    assert!(ty.is_some());
    let ty = ty.unwrap();
    match ty {
        Ty::Fn(params, _) => {
            assert!(matches!(params[0], Ty::Option(_)));
        }
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_result_type() {
    let ty = get_fn_type(
        r#"
f fallible(x: Int!) -> Int
    0
"#,
        "fallible",
    );

    assert!(ty.is_some());
    let ty = ty.unwrap();
    match ty {
        Ty::Fn(params, _) => {
            assert!(matches!(params[0], Ty::Result(_, _)));
        }
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_reference_type() {
    let ty = get_fn_type(
        r#"
f borrow(x: &Int) -> Int
    *x
"#,
        "borrow",
    );

    assert!(ty.is_some());
    let ty = ty.unwrap();
    match ty {
        Ty::Fn(params, _) => {
            assert!(matches!(params[0], Ty::Ref(_, _)));
        }
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_map_type() {
    let ty = get_fn_type(
        r#"
f lookup(m: {Str: Int}) -> Int
    0
"#,
        "lookup",
    );

    assert!(ty.is_some());
    let ty = ty.unwrap();
    match ty {
        Ty::Fn(params, _) => match &params[0] {
            Ty::Map(k, v) => {
                assert_eq!(**k, Ty::Str);
                assert_eq!(**v, Ty::Int);
            }
            _ => panic!("expected map type"),
        },
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_tuple_type() {
    let ty = get_fn_type(
        r#"
f pair(p: (Int, Str)) -> Int
    0
"#,
        "pair",
    );

    assert!(ty.is_some());
    let ty = ty.unwrap();
    match ty {
        Ty::Fn(params, _) => match &params[0] {
            Ty::Tuple(elems) => {
                assert_eq!(elems.len(), 2);
                assert_eq!(elems[0], Ty::Int);
                assert_eq!(elems[1], Ty::Str);
            }
            _ => panic!("expected tuple type"),
        },
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_function_type_param() {
    // Test function type representation directly
    let fn_type = Ty::Fn(
        vec![Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)), Ty::Int],
        Box::new(Ty::Int),
    );

    match fn_type {
        Ty::Fn(params, _) => match &params[0] {
            Ty::Fn(inner_params, inner_ret) => {
                assert_eq!(inner_params.len(), 1);
                assert_eq!(inner_params[0], Ty::Int);
                assert_eq!(**inner_ret, Ty::Int);
            }
            _ => panic!("expected function type parameter"),
        },
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_trait_definition() {
    let result = check_source(
        r#"
t Display
    f display(&self) -> Str
"#,
    );

    assert!(result.is_ok());
}

#[test]
fn test_impl_block() {
    let result = check_source(
        r#"
s Point
    x: Int
    y: Int

i Point
    f new(x: Int, y: Int) -> Point
        Point { x: x, y: y }
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
    f display(&self) -> Str
        "point"
"#,
    );

    assert!(result.is_ok());
}

#[test]
fn test_conflicting_trait_impls_are_rejected() {
    let errors = check_source(
        r#"
t Display
    f display(&self) -> Str

s Point
    x: Int

i Display for Point
    f display(&self) -> Str = "first"

i Display for Point
    f display(&self) -> Str = "second"
"#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("conflicting implementations"))
    );
}

#[test]
fn disjoint_concrete_generic_impls_are_allowed() {
    let result = check_source(
        r#"
t Marker

s Box[T]
    value: T

i Marker for Box[Int]
i Marker for Box[Str]
"#,
    );
    assert!(result.is_ok(), "{result:?}");
}

#[test]
fn blanket_and_concrete_generic_impls_overlap() {
    let errors = check_source(
        r#"
t Marker

s Box[T]
    value: T

i [T] Marker for Box[T]
i Marker for Box[Int]
"#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("conflicting implementations"))
    );
}

#[test]
fn test_orphan_trait_impl_is_rejected() {
    let errors = check_source(
        r#"
i Foreign for Int
    f value(&self) -> Int = 0
"#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("orphan implementation"))
    );
}

#[test]
fn test_type_alias() {
    let result = check_source(
        r#"
type Meters = Int
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
"#,
    );

    assert!(result.is_ok());
}

#[test]
fn test_type_relations_coerce() {
    use forma::types::TypeRelations;

    // Same type
    assert!(TypeRelations::can_coerce(&Ty::Int, &Ty::Int));

    // Never coerces to anything
    assert!(TypeRelations::can_coerce(&Ty::Never, &Ty::Int));
    assert!(TypeRelations::can_coerce(&Ty::Never, &Ty::Str));

    // Numeric widening
    assert!(TypeRelations::can_coerce(&Ty::I8, &Ty::I16));
    assert!(TypeRelations::can_coerce(&Ty::I8, &Ty::I32));
    assert!(TypeRelations::can_coerce(&Ty::I8, &Ty::I64));
    assert!(TypeRelations::can_coerce(&Ty::F32, &Ty::F64));

    // Can't narrow
    assert!(!TypeRelations::can_coerce(&Ty::I64, &Ty::I32));
    assert!(!TypeRelations::can_coerce(&Ty::F64, &Ty::F32));
}

#[test]
fn test_type_relations_sized() {
    use forma::types::TypeRelations;

    // Primitives are sized
    assert!(TypeRelations::is_sized(&Ty::Int));
    assert!(TypeRelations::is_sized(&Ty::Bool));
    assert!(TypeRelations::is_sized(&Ty::F64));

    // Str is not sized
    assert!(!TypeRelations::is_sized(&Ty::Str));

    // Lists are not sized
    assert!(!TypeRelations::is_sized(&Ty::List(Box::new(Ty::Int))));

    // Tuples of sized types are sized
    assert!(TypeRelations::is_sized(&Ty::Tuple(vec![Ty::Int, Ty::Bool])));
}

#[test]
fn test_type_relations_size_of() {
    use forma::types::TypeRelations;

    assert_eq!(TypeRelations::size_of(&Ty::Unit), Some(0));
    assert_eq!(TypeRelations::size_of(&Ty::Bool), Some(1));
    assert_eq!(TypeRelations::size_of(&Ty::I8), Some(1));
    assert_eq!(TypeRelations::size_of(&Ty::I16), Some(2));
    assert_eq!(TypeRelations::size_of(&Ty::I32), Some(4));
    assert_eq!(TypeRelations::size_of(&Ty::I64), Some(8));
    assert_eq!(TypeRelations::size_of(&Ty::I128), Some(16));

    // Tuple size
    assert_eq!(
        TypeRelations::size_of(&Ty::Tuple(vec![Ty::I32, Ty::I32])),
        Some(8)
    );
}

#[test]
fn test_unification_basic() {
    use forma::lexer::Span;
    use forma::types::Unifier;

    let mut unifier = Unifier::new();
    let span = Span::new(0, 0, 0, 0);

    // Same types unify
    assert!(unifier.unify(&Ty::Int, &Ty::Int, span).is_ok());
    assert!(unifier.unify(&Ty::Bool, &Ty::Bool, span).is_ok());

    // Different primitives don't unify
    assert!(unifier.unify(&Ty::Int, &Ty::Bool, span).is_err());
}

#[test]
fn test_unification_type_var() {
    use forma::lexer::Span;
    use forma::types::{TypeVar, Unifier};

    let mut unifier = Unifier::new();
    let span = Span::new(0, 0, 0, 0);

    let var = TypeVar::fresh();

    // Type var unifies with concrete type
    assert!(unifier.unify(&Ty::Var(var), &Ty::Int, span).is_ok());

    // After unification, applying substitution resolves the var
    let resolved = Ty::Var(var).apply(unifier.substitution());
    assert_eq!(resolved, Ty::Int);
}

#[test]
fn test_unification_compound() {
    use forma::lexer::Span;
    use forma::types::{TypeVar, Unifier};

    let mut unifier = Unifier::new();
    let span = Span::new(0, 0, 0, 0);

    let var = TypeVar::fresh();

    // List[?0] unifies with List[Int]
    let list_var = Ty::List(Box::new(Ty::Var(var)));
    let list_int = Ty::List(Box::new(Ty::Int));

    assert!(unifier.unify(&list_var, &list_int, span).is_ok());

    let resolved = Ty::Var(var).apply(unifier.substitution());
    assert_eq!(resolved, Ty::Int);
}

#[test]
fn test_occurs_check() {
    use forma::lexer::Span;
    use forma::types::{TypeVar, Unifier};

    let mut unifier = Unifier::new();
    let span = Span::new(0, 0, 0, 0);

    let var = TypeVar::fresh();

    // Cannot unify ?0 with List[?0] (infinite type)
    let infinite = Ty::List(Box::new(Ty::Var(var)));
    assert!(unifier.unify(&Ty::Var(var), &infinite, span).is_err());
}

#[test]
fn test_type_scheme_instantiation() {
    use forma::types::{TypeScheme, TypeVar};

    let var = TypeVar::fresh();

    let scheme = TypeScheme {
        vars: vec![var],
        ty: Ty::List(Box::new(Ty::Var(var))),
    };

    let instance1 = scheme.instantiate();
    let instance2 = scheme.instantiate();

    // Each instantiation creates fresh type variables
    assert_ne!(instance1, instance2);
}

#[test]
fn test_type_display() {
    assert_eq!(format!("{}", Ty::Int), "Int");
    assert_eq!(format!("{}", Ty::Bool), "Bool");
    assert_eq!(format!("{}", Ty::Str), "Str");
    assert_eq!(format!("{}", Ty::Unit), "()");
    assert_eq!(format!("{}", Ty::Never), "!");
    assert_eq!(format!("{}", Ty::List(Box::new(Ty::Int))), "[Int]");
    assert_eq!(format!("{}", Ty::Option(Box::new(Ty::Str))), "Str?");
    assert_eq!(
        format!("{}", Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str))),
        "Int!Str"
    );
    assert_eq!(
        format!("{}", Ty::Fn(vec![Ty::Int, Ty::Int], Box::new(Ty::Int))),
        "(Int, Int) -> Int"
    );
    assert_eq!(
        format!("{}", Ty::Tuple(vec![Ty::Int, Ty::Str])),
        "(Int, Str)"
    );
    assert_eq!(
        format!("{}", Ty::Map(Box::new(Ty::Str), Box::new(Ty::Int))),
        "{Str:Int}"
    );
}

#[test]
fn test_named_type() {
    let point_ty = Ty::Named(TypeId::new("Point"), vec![]);
    assert_eq!(format!("{}", point_ty), "Point");

    let generic_ty = Ty::Named(TypeId::new("Container"), vec![Ty::Int]);
    assert_eq!(format!("{}", generic_ty), "Container[Int]");
}

#[test]
fn test_is_copy() {
    // Primitives are Copy
    assert!(Ty::Int.is_copy());
    assert!(Ty::Bool.is_copy());
    assert!(Ty::Char.is_copy());
    assert!(Ty::F64.is_copy());
    assert!(Ty::Unit.is_copy());

    // Str is not Copy
    assert!(!Ty::Str.is_copy());

    // Lists are not Copy
    assert!(!Ty::List(Box::new(Ty::Int)).is_copy());

    // Tuples of Copy types are Copy
    assert!(Ty::Tuple(vec![Ty::Int, Ty::Bool]).is_copy());

    // Tuple containing non-Copy is not Copy
    assert!(!Ty::Tuple(vec![Ty::Int, Ty::Str]).is_copy());
}

#[test]
fn test_is_numeric() {
    assert!(Ty::Int.is_numeric());
    assert!(Ty::I32.is_numeric());
    assert!(Ty::F64.is_numeric());
    assert!(Ty::U8.is_numeric());

    assert!(!Ty::Bool.is_numeric());
    assert!(!Ty::Str.is_numeric());
}

#[test]
fn test_is_integer() {
    assert!(Ty::Int.is_integer());
    assert!(Ty::I32.is_integer());
    assert!(Ty::U64.is_integer());

    assert!(!Ty::F64.is_integer());
    assert!(!Ty::Float.is_integer());
}

#[test]
fn test_is_float() {
    assert!(Ty::Float.is_float());
    assert!(Ty::F32.is_float());
    assert!(Ty::F64.is_float());

    assert!(!Ty::Int.is_float());
    assert!(!Ty::I64.is_float());
}

// ============================================================================
// Negative Type Checking Tests
// ============================================================================

fn check_should_fail(source: &str) {
    let result = check_source(source);
    assert!(
        result.is_err(),
        "Expected type error for: {}",
        source.chars().take(80).collect::<String>()
    );
}

#[test]
fn test_return_type_mismatch() {
    check_should_fail(
        r#"
f test() -> Int = "hello"
"#,
    );
}

#[test]
fn test_binary_op_type_mismatch() {
    check_should_fail(
        r#"
f test() -> Int = 1 + "hello"
"#,
    );
}

#[test]
fn test_if_branch_type_mismatch() {
    check_should_fail(
        r#"
f test(b: Bool) -> Int
    if b then 1 else "no"
"#,
    );
}

#[test]
fn test_wrong_arg_type() {
    check_should_fail(
        r#"
f add(a: Int, b: Int) -> Int = a + b
f test() -> Int = add(1, "two")
"#,
    );
}

#[test]
fn test_public_function_requires_annotated_return_type() {
    let errors = check_source("pub f answer() = 42").unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("must declare its return type"))
    );
    assert!(check_source("pub f answer() -> Int = 42").is_ok());
}

#[test]
fn compiler_known_copy_requires_structurally_copy_fields() {
    assert!(
        check_source(
            r#"
@derive(Copy)
s Point
    x: Int
    y: Bool
"#,
        )
        .is_ok()
    );

    let errors = check_source(
        r#"
@derive(Copy)
s Named
    value: Str
"#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("field is not `Copy`"))
    );
}

#[test]
fn compiler_known_drop_types_cannot_be_copy() {
    let errors = check_source(
        r#"
@derive(Copy, Drop)
s Resource
    id: Int
"#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("cannot implement `Copy`"))
    );
}

#[test]
fn compiler_known_send_requires_task_safe_fields() {
    assert!(
        check_source(
            r#"
@derive(Send)
s Message
    text: Str
"#,
        )
        .is_ok()
    );

    let errors = check_source(
        r#"
@derive(Send)
s Query
    database: Database
"#,
    )
    .unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("field is not task-safe"))
    );
}

#[test]
fn ambiguous_trait_methods_are_rejected_independent_of_source_order() {
    for source in [
        r#"
t Alpha
    f value(&self) -> Int
t Beta
    f value(&self) -> Int
s Thing
f test(x: Thing) -> Int = x.value()
"#,
        r#"
t Beta
    f value(&self) -> Int
t Alpha
    f value(&self) -> Int
s Thing
f test(x: Thing) -> Int = x.value()
"#,
    ] {
        let errors = check_source(source).unwrap_err();
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("ambiguous method 'value'"))
        );
    }
}

#[test]
fn semantic_query_retains_local_expression_types() {
    let source = "f main() -> Int\n    local = 40 + 2\n    local\n";
    let scanner = Scanner::new(source);
    let (tokens, errors) = scanner.scan_all();
    assert!(errors.is_empty());
    let ast = Parser::new(&tokens).parse().unwrap();
    let mut checker = TypeChecker::new();
    checker.check(&ast).unwrap();
    let offset = source.rfind("local").unwrap();
    assert_eq!(checker.type_at_offset(offset), Some(Ty::Int));
}

#[test]
fn test_wrong_arg_count() {
    check_should_fail(
        r#"
f add(a: Int, b: Int) -> Int = a + b
f test() -> Int = add(1)
"#,
    );
}

#[test]
fn test_bool_arithmetic() {
    check_should_fail(
        r#"
f test() -> Int = true + 1
"#,
    );
}

#[test]
fn test_assign_bool_to_int() {
    check_should_fail(
        r#"
f test() -> Int = false
"#,
    );
}

// ============================================================================
// Cast Expression Type Checking
// ============================================================================

#[test]
fn test_cast_int_to_i32() {
    let result = check_source(
        r#"
f cast_test() -> i32 = i32(255)
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_cast_int_to_float() {
    let result = check_source(
        r#"
f cast_test() -> f64 = f64(42)
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_cast_float_to_int() {
    let result = check_source(
        r#"
f cast_test() -> i32 = i32(3.14)
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_integer_type_i8() {
    // Test that i8 type annotation is accepted in return type
    let result = check_source(
        r#"
f test() -> i8 = i8(127)
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn test_integer_type_u8() {
    // Test that u8 type annotation is accepted in return type
    let result = check_source(
        r#"
f test() -> u8 = u8(255)
"#,
    );
    assert!(result.is_ok());
}

#[test]
fn fixed_array_repeat_preserves_its_length() {
    let result = check_source("f zeros() -> [Int; 3] = [0; 3]\n");
    assert!(
        result.is_ok(),
        "matching fixed-array lengths should type-check"
    );
}

#[test]
fn fixed_array_length_mismatch_is_rejected() {
    let errors = check_source("f zeros() -> [Int; 2] = [0; 3]\n").unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("array size mismatch"))
    );
}

#[test]
fn fixed_array_length_must_be_a_literal() {
    let errors = check_source("f bad(n: Int) -> [Int; 2] = [0; n]\n").unwrap_err();
    assert!(errors.iter().any(|error| {
        error
            .message
            .contains("length must be a non-negative integer literal")
    }));
}
