//! Integration tests for the FORMA type system.

use forma::{Parser, Scanner, TypeChecker};
use forma::types::{Ty, TypeId};

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
        Ty::Fn(params, _) => {
            match &params[0] {
                Ty::Map(k, v) => {
                    assert_eq!(**k, Ty::Str);
                    assert_eq!(**v, Ty::Int);
                }
                _ => panic!("expected map type"),
            }
        }
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
        Ty::Fn(params, _) => {
            match &params[0] {
                Ty::Tuple(elems) => {
                    assert_eq!(elems.len(), 2);
                    assert_eq!(elems[0], Ty::Int);
                    assert_eq!(elems[1], Ty::Str);
                }
                _ => panic!("expected tuple type"),
            }
        }
        _ => panic!("expected function type"),
    }
}

#[test]
fn test_function_type_param() {
    // Test function type representation directly
    let fn_type = Ty::Fn(
        vec![Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)), Ty::Int],
        Box::new(Ty::Int)
    );

    match fn_type {
        Ty::Fn(params, _) => {
            match &params[0] {
                Ty::Fn(inner_params, inner_ret) => {
                    assert_eq!(inner_params.len(), 1);
                    assert_eq!(inner_params[0], Ty::Int);
                    assert_eq!(**inner_ret, Ty::Int);
                }
                _ => panic!("expected function type parameter"),
            }
        }
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
    use forma::types::{Unifier, reset_type_var_counter};
    use forma::lexer::Span;

    reset_type_var_counter();
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
    use forma::types::{Unifier, reset_type_var_counter, TypeVar};
    use forma::lexer::Span;

    reset_type_var_counter();
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
    use forma::types::{Unifier, reset_type_var_counter, TypeVar};
    use forma::lexer::Span;

    reset_type_var_counter();
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
    use forma::types::{Unifier, reset_type_var_counter, TypeVar};
    use forma::lexer::Span;

    reset_type_var_counter();
    let mut unifier = Unifier::new();
    let span = Span::new(0, 0, 0, 0);

    let var = TypeVar::fresh();

    // Cannot unify ?0 with List[?0] (infinite type)
    let infinite = Ty::List(Box::new(Ty::Var(var)));
    assert!(unifier.unify(&Ty::Var(var), &infinite, span).is_err());
}

#[test]
fn test_type_scheme_instantiation() {
    use forma::types::{TypeScheme, reset_type_var_counter, TypeVar};

    reset_type_var_counter();
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
    assert_eq!(format!("{}", Ty::Tuple(vec![Ty::Int, Ty::Str])), "(Int, Str)");
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
// Cast Expression Type Checking
// ============================================================================

#[test]
fn test_cast_int_to_i32() {
    let result = check_source(r#"
f cast_test() -> i32 = i32(255)
"#);
    assert!(result.is_ok());
}

#[test]
fn test_cast_int_to_float() {
    let result = check_source(r#"
f cast_test() -> f64 = f64(42)
"#);
    assert!(result.is_ok());
}

#[test]
fn test_cast_float_to_int() {
    let result = check_source(r#"
f cast_test() -> i32 = i32(3.14)
"#);
    assert!(result.is_ok());
}

#[test]
fn test_integer_type_i8() {
    // Test that i8 type annotation is accepted in return type
    let result = check_source(r#"
f test() -> i8 = i8(127)
"#);
    assert!(result.is_ok());
}

#[test]
fn test_integer_type_u8() {
    // Test that u8 type annotation is accepted in return type
    let result = check_source(r#"
f test() -> u8 = u8(255)
"#);
    assert!(result.is_ok());
}
