//! Type inference using Hindley-Milner algorithm.
//!
//! This module implements type inference through unification.
//! Key components:
//! - TypeEnv: Environment mapping names to type schemes
//! - Unifier: Unification algorithm for type equality constraints
//! - InferenceEngine: Walks AST and generates/solves constraints

use std::collections::HashMap;

use crate::lexer::Span;
use crate::parser::{
    BinOp, Block, Expr, ExprKind, FnBody, GenericParam, Generics, Item, ItemKind,
    LiteralKind, Pattern, PatternKind, Stmt, StmtKind, Type as AstType, TypeKind as AstTypeKind,
    UnaryOp, VariantKind, GenericArg,
};

use super::types::{Mutability, Substitution, Ty, TypeId, TypeScheme, TypeVar};

/// Type error during inference.
#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl TypeError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for TypeError {}

/// Type environment mapping names to type schemes.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    /// Variable bindings
    bindings: HashMap<String, TypeScheme>,
    /// Type definitions (struct, enum names to their info)
    types: HashMap<String, TypeDef>,
}

/// Definition of a named type.
#[derive(Debug, Clone)]
pub enum TypeDef {
    Struct {
        type_params: Vec<String>,
        fields: Vec<(String, Ty)>,
    },
    Enum {
        type_params: Vec<String>,
        variants: Vec<(String, Vec<Ty>)>,
    },
    Alias {
        type_params: Vec<String>,
        target: Ty,
    },
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create environment with built-in types.
    pub fn with_builtins() -> Self {
        let mut env = Self::new();

        // Add Option type
        env.types.insert(
            "Option".to_string(),
            TypeDef::Enum {
                type_params: vec!["T".to_string()],
                variants: vec![
                    ("Some".to_string(), vec![Ty::Var(TypeVar { id: 0 })]),
                    ("None".to_string(), vec![]),
                ],
            },
        );

        // Add Option variant constructors
        // Some: T -> Option[T]
        let some_var = TypeVar::fresh();
        let some_type = Ty::Fn(
            vec![Ty::Var(some_var)],
            Box::new(Ty::Option(Box::new(Ty::Var(some_var)))),
        );
        env.bindings.insert(
            "Some".to_string(),
            TypeScheme { vars: vec![some_var], ty: some_type },
        );

        // None: Option[T]
        let none_var = TypeVar::fresh();
        let none_type = Ty::Option(Box::new(Ty::Var(none_var)));
        env.bindings.insert(
            "None".to_string(),
            TypeScheme { vars: vec![none_var], ty: none_type },
        );

        // Add Result type
        env.types.insert(
            "Result".to_string(),
            TypeDef::Enum {
                type_params: vec!["T".to_string(), "E".to_string()],
                variants: vec![
                    ("Ok".to_string(), vec![Ty::Var(TypeVar { id: 0 })]),
                    ("Err".to_string(), vec![Ty::Var(TypeVar { id: 1 })]),
                ],
            },
        );

        // Add Result variant constructors
        // Ok: T -> Result[T, E]
        let ok_t = TypeVar::fresh();
        let ok_e = TypeVar::fresh();
        let ok_type = Ty::Fn(
            vec![Ty::Var(ok_t)],
            Box::new(Ty::Result(Box::new(Ty::Var(ok_t)), Box::new(Ty::Var(ok_e)))),
        );
        env.bindings.insert(
            "Ok".to_string(),
            TypeScheme { vars: vec![ok_t, ok_e], ty: ok_type },
        );

        // Err: E -> Result[T, E]
        let err_t = TypeVar::fresh();
        let err_e = TypeVar::fresh();
        let err_type = Ty::Fn(
            vec![Ty::Var(err_e)],
            Box::new(Ty::Result(Box::new(Ty::Var(err_t)), Box::new(Ty::Var(err_e)))),
        );
        env.bindings.insert(
            "Err".to_string(),
            TypeScheme { vars: vec![err_t, err_e], ty: err_type },
        );

        // ===== Built-in functions =====

        // I/O
        // print: ...Any -> Unit
        let print_var = TypeVar::fresh();
        env.bindings.insert(
            "print".to_string(),
            TypeScheme { vars: vec![print_var], ty: Ty::Fn(vec![Ty::Var(print_var)], Box::new(Ty::Unit)) },
        );

        // str: T -> Str (convert any value to string)
        let str_var = TypeVar::fresh();
        env.bindings.insert(
            "str".to_string(),
            TypeScheme { vars: vec![str_var], ty: Ty::Fn(vec![Ty::Var(str_var)], Box::new(Ty::Str)) },
        );

        // Vec operations
        // vec_new: () -> [T]
        let vec_new_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_new".to_string(),
            TypeScheme { vars: vec![vec_new_t], ty: Ty::Fn(vec![], Box::new(Ty::List(Box::new(Ty::Var(vec_new_t))))) },
        );

        // vec_len: [T] -> Int
        let vec_len_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_len".to_string(),
            TypeScheme { vars: vec![vec_len_t], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Var(vec_len_t)))], Box::new(Ty::Int)) },
        );

        // vec_push: ([T], T) -> [T]
        let vec_push_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_push".to_string(),
            TypeScheme {
                vars: vec![vec_push_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_push_t))), Ty::Var(vec_push_t)],
                    Box::new(Ty::List(Box::new(Ty::Var(vec_push_t))))
                )
            },
        );

        // vec_pop: [T] -> ([T], T?)
        let vec_pop_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_pop".to_string(),
            TypeScheme {
                vars: vec![vec_pop_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_pop_t)))],
                    Box::new(Ty::Tuple(vec![
                        Ty::List(Box::new(Ty::Var(vec_pop_t))),
                        Ty::Option(Box::new(Ty::Var(vec_pop_t)))
                    ]))
                )
            },
        );

        // vec_get: ([T], Int) -> T?
        let vec_get_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_get".to_string(),
            TypeScheme {
                vars: vec![vec_get_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_get_t))), Ty::Int],
                    Box::new(Ty::Option(Box::new(Ty::Var(vec_get_t))))
                )
            },
        );

        // vec_set: ([T], Int, T) -> [T]
        let vec_set_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_set".to_string(),
            TypeScheme {
                vars: vec![vec_set_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_set_t))), Ty::Int, Ty::Var(vec_set_t)],
                    Box::new(Ty::List(Box::new(Ty::Var(vec_set_t))))
                )
            },
        );

        // vec_first: [T] -> T?
        let vec_first_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_first".to_string(),
            TypeScheme {
                vars: vec![vec_first_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_first_t)))],
                    Box::new(Ty::Option(Box::new(Ty::Var(vec_first_t))))
                )
            },
        );

        // vec_last: [T] -> T?
        let vec_last_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_last".to_string(),
            TypeScheme {
                vars: vec![vec_last_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_last_t)))],
                    Box::new(Ty::Option(Box::new(Ty::Var(vec_last_t))))
                )
            },
        );

        // vec_concat: ([T], [T]) -> [T]
        let vec_concat_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_concat".to_string(),
            TypeScheme {
                vars: vec![vec_concat_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_concat_t))), Ty::List(Box::new(Ty::Var(vec_concat_t)))],
                    Box::new(Ty::List(Box::new(Ty::Var(vec_concat_t))))
                )
            },
        );

        // vec_slice: ([T], Int, Int) -> [T]
        let vec_slice_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_slice".to_string(),
            TypeScheme {
                vars: vec![vec_slice_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_slice_t))), Ty::Int, Ty::Int],
                    Box::new(Ty::List(Box::new(Ty::Var(vec_slice_t))))
                )
            },
        );

        // vec_reverse: [T] -> [T]
        let vec_reverse_t = TypeVar::fresh();
        env.bindings.insert(
            "vec_reverse".to_string(),
            TypeScheme {
                vars: vec![vec_reverse_t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_reverse_t)))],
                    Box::new(Ty::List(Box::new(Ty::Var(vec_reverse_t))))
                )
            },
        );

        // String operations
        // str_len: Str -> Int
        env.bindings.insert(
            "str_len".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Int)) },
        );

        // str_char_at: (Str, Int) -> Char?
        env.bindings.insert(
            "str_char_at".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Int], Box::new(Ty::Option(Box::new(Ty::Char)))) },
        );

        // str_slice: (Str, Int, Int) -> Str
        env.bindings.insert(
            "str_slice".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Int, Ty::Int], Box::new(Ty::Str)) },
        );

        // str_contains: (Str, Str) -> Bool
        env.bindings.insert(
            "str_contains".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Bool)) },
        );

        // str_starts_with: (Str, Str) -> Bool
        env.bindings.insert(
            "str_starts_with".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Bool)) },
        );

        // str_ends_with: (Str, Str) -> Bool
        env.bindings.insert(
            "str_ends_with".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Bool)) },
        );

        // str_split: (Str, Str) -> [Str]
        env.bindings.insert(
            "str_split".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::List(Box::new(Ty::Str)))) },
        );

        // str_trim: Str -> Str
        env.bindings.insert(
            "str_trim".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Str)) },
        );

        // str_to_int: Str -> Int?
        env.bindings.insert(
            "str_to_int".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Option(Box::new(Ty::Int)))) },
        );

        // int_to_str: Int -> Str
        env.bindings.insert(
            "int_to_str".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Str)) },
        );

        // str_to_int_radix: (Str, Int) -> Int?
        env.bindings.insert(
            "str_to_int_radix".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Int], Box::new(Ty::Option(Box::new(Ty::Int)))) },
        );

        // str_replace_all: (Str, Str, Str) -> Str
        env.bindings.insert(
            "str_replace_all".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str, Ty::Str], Box::new(Ty::Str)) },
        );

        // str_concat: (Str, Str) -> Str
        env.bindings.insert(
            "str_concat".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Str)) },
        );

        // Char operations
        // char_is_digit: Char -> Bool
        env.bindings.insert(
            "char_is_digit".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Char], Box::new(Ty::Bool)) },
        );

        // char_is_alpha: Char -> Bool
        env.bindings.insert(
            "char_is_alpha".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Char], Box::new(Ty::Bool)) },
        );

        // char_is_alphanumeric: Char -> Bool
        env.bindings.insert(
            "char_is_alphanumeric".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Char], Box::new(Ty::Bool)) },
        );

        // char_is_whitespace: Char -> Bool
        env.bindings.insert(
            "char_is_whitespace".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Char], Box::new(Ty::Bool)) },
        );

        // char_to_int: Char -> Int
        env.bindings.insert(
            "char_to_int".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Char], Box::new(Ty::Int)) },
        );

        // int_to_char: Int -> Char?
        env.bindings.insert(
            "int_to_char".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Option(Box::new(Ty::Char)))) },
        );

        // char_to_str: Char -> Str
        env.bindings.insert(
            "char_to_str".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Char], Box::new(Ty::Str)) },
        );

        // Map operations (using Str keys for simplicity)
        // map_new: () -> Map
        let map_v = TypeVar::fresh();
        env.bindings.insert(
            "map_new".to_string(),
            TypeScheme { vars: vec![map_v], ty: Ty::Fn(vec![], Box::new(Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_v)]))) },
        );

        // map_len: Map -> Int
        let map_len_v = TypeVar::fresh();
        env.bindings.insert(
            "map_len".to_string(),
            TypeScheme { vars: vec![map_len_v], ty: Ty::Fn(vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_len_v)])], Box::new(Ty::Int)) },
        );

        // map_get: (Map, Str) -> V?
        let map_get_v = TypeVar::fresh();
        env.bindings.insert(
            "map_get".to_string(),
            TypeScheme {
                vars: vec![map_get_v],
                ty: Ty::Fn(
                    vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_get_v)]), Ty::Str],
                    Box::new(Ty::Option(Box::new(Ty::Var(map_get_v))))
                )
            },
        );

        // map_insert: (Map, Str, V) -> Map
        let map_insert_v = TypeVar::fresh();
        env.bindings.insert(
            "map_insert".to_string(),
            TypeScheme {
                vars: vec![map_insert_v],
                ty: Ty::Fn(
                    vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_insert_v)]), Ty::Str, Ty::Var(map_insert_v)],
                    Box::new(Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_insert_v)]))
                )
            },
        );

        // map_contains: (Map, Str) -> Bool
        let map_contains_v = TypeVar::fresh();
        env.bindings.insert(
            "map_contains".to_string(),
            TypeScheme {
                vars: vec![map_contains_v],
                ty: Ty::Fn(
                    vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_contains_v)]), Ty::Str],
                    Box::new(Ty::Bool)
                )
            },
        );

        // map_remove: (Map, Str) -> (Map, V?)
        let map_remove_v = TypeVar::fresh();
        env.bindings.insert(
            "map_remove".to_string(),
            TypeScheme {
                vars: vec![map_remove_v],
                ty: Ty::Fn(
                    vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_remove_v)]), Ty::Str],
                    Box::new(Ty::Tuple(vec![
                        Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_remove_v)]),
                        Ty::Option(Box::new(Ty::Var(map_remove_v)))
                    ]))
                )
            },
        );

        // map_keys: Map -> [Str]
        let map_keys_v = TypeVar::fresh();
        env.bindings.insert(
            "map_keys".to_string(),
            TypeScheme {
                vars: vec![map_keys_v],
                ty: Ty::Fn(
                    vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_keys_v)])],
                    Box::new(Ty::List(Box::new(Ty::Str)))
                )
            },
        );

        // map_values: Map -> [V]
        let map_values_v = TypeVar::fresh();
        env.bindings.insert(
            "map_values".to_string(),
            TypeScheme {
                vars: vec![map_values_v],
                ty: Ty::Fn(
                    vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_values_v)])],
                    Box::new(Ty::List(Box::new(Ty::Var(map_values_v))))
                )
            },
        );

        // Debug/Utility operations
        // type_of: T -> Str
        let type_of_t = TypeVar::fresh();
        env.bindings.insert(
            "type_of".to_string(),
            TypeScheme { vars: vec![type_of_t], ty: Ty::Fn(vec![Ty::Var(type_of_t)], Box::new(Ty::Str)) },
        );

        // panic: Str -> !
        env.bindings.insert(
            "panic".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Never)) },
        );

        // assert: (Bool, Str?) -> Unit
        env.bindings.insert(
            "assert".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Bool], Box::new(Ty::Unit)) },
        );

        // unwrap[T]: Option[T] -> T
        let unwrap_t = TypeVar::fresh();
        env.bindings.insert(
            "unwrap".to_string(),
            TypeScheme {
                vars: vec![unwrap_t],
                ty: Ty::Fn(
                    vec![Ty::Named(crate::types::TypeId::new("Option".to_string()), vec![Ty::Var(unwrap_t)])],
                    Box::new(Ty::Var(unwrap_t))
                )
            },
        );

        // expect[T]: (Option[T], Str) -> T
        let expect_t = TypeVar::fresh();
        env.bindings.insert(
            "expect".to_string(),
            TypeScheme {
                vars: vec![expect_t],
                ty: Ty::Fn(
                    vec![Ty::Named(crate::types::TypeId::new("Option".to_string()), vec![Ty::Var(expect_t)]), Ty::Str],
                    Box::new(Ty::Var(expect_t))
                )
            },
        );

        // unwrap_or[T]: (Option[T], T) -> T
        let unwrap_or_t = TypeVar::fresh();
        env.bindings.insert(
            "unwrap_or".to_string(),
            TypeScheme {
                vars: vec![unwrap_or_t],
                ty: Ty::Fn(
                    vec![Ty::Named(crate::types::TypeId::new("Option".to_string()), vec![Ty::Var(unwrap_or_t)]), Ty::Var(unwrap_or_t)],
                    Box::new(Ty::Var(unwrap_or_t))
                )
            },
        );

        // is_some[T]: Option[T] -> Bool
        let is_some_t = TypeVar::fresh();
        env.bindings.insert(
            "is_some".to_string(),
            TypeScheme {
                vars: vec![is_some_t],
                ty: Ty::Fn(
                    vec![Ty::Named(crate::types::TypeId::new("Option".to_string()), vec![Ty::Var(is_some_t)])],
                    Box::new(Ty::Bool)
                )
            },
        );

        // is_none[T]: Option[T] -> Bool
        let is_none_t = TypeVar::fresh();
        env.bindings.insert(
            "is_none".to_string(),
            TypeScheme {
                vars: vec![is_none_t],
                ty: Ty::Fn(
                    vec![Ty::Named(crate::types::TypeId::new("Option".to_string()), vec![Ty::Var(is_none_t)])],
                    Box::new(Ty::Bool)
                )
            },
        );

        // is_ok[T, E]: Result[T, E] -> Bool
        let is_ok_t = TypeVar::fresh();
        let is_ok_e = TypeVar::fresh();
        env.bindings.insert(
            "is_ok".to_string(),
            TypeScheme {
                vars: vec![is_ok_t, is_ok_e],
                ty: Ty::Fn(
                    vec![Ty::Named(crate::types::TypeId::new("Result".to_string()), vec![Ty::Var(is_ok_t), Ty::Var(is_ok_e)])],
                    Box::new(Ty::Bool)
                )
            },
        );

        // is_err[T, E]: Result[T, E] -> Bool
        let is_err_t = TypeVar::fresh();
        let is_err_e = TypeVar::fresh();
        env.bindings.insert(
            "is_err".to_string(),
            TypeScheme {
                vars: vec![is_err_t, is_err_e],
                ty: Ty::Fn(
                    vec![Ty::Named(crate::types::TypeId::new("Result".to_string()), vec![Ty::Var(is_err_t), Ty::Var(is_err_e)])],
                    Box::new(Ty::Bool)
                )
            },
        );

        // ===== File I/O =====
        // file_read: Str -> Result[Str, Str]
        env.bindings.insert(
            "file_read".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Str],
                    Box::new(Ty::Named(crate::types::TypeId::new("Result".to_string()), vec![Ty::Str, Ty::Str]))
                )
            },
        );

        // file_write: (Str, Str) -> Result[Unit, Str]
        env.bindings.insert(
            "file_write".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Str, Ty::Str],
                    Box::new(Ty::Named(crate::types::TypeId::new("Result".to_string()), vec![Ty::Unit, Ty::Str]))
                )
            },
        );

        // file_exists: Str -> Bool
        env.bindings.insert(
            "file_exists".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Bool)) },
        );

        // file_append: (Str, Str) -> Result[Unit, Str]
        env.bindings.insert(
            "file_append".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Str, Ty::Str],
                    Box::new(Ty::Named(crate::types::TypeId::new("Result".to_string()), vec![Ty::Unit, Ty::Str]))
                )
            },
        );

        // ===== CLI support =====
        // args: () -> [Str]
        env.bindings.insert(
            "args".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::List(Box::new(Ty::Str)))) },
        );

        // env_get: Str -> Option[Str]
        env.bindings.insert(
            "env_get".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Str],
                    Box::new(Ty::Named(crate::types::TypeId::new("Option".to_string()), vec![Ty::Str]))
                )
            },
        );

        // exit: Int -> Never
        env.bindings.insert(
            "exit".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Never)) },
        );

        // eprintln: Str -> Unit
        env.bindings.insert(
            "eprintln".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Unit)) },
        );

        // ===== Random number generation =====
        // random() -> Float
        env.bindings.insert(
            "random".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Float)) },
        );

        // random_int(min: Int, max: Int) -> Int
        env.bindings.insert(
            "random_int".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int, Ty::Int], Box::new(Ty::Int)) },
        );

        // random_bool() -> Bool
        env.bindings.insert(
            "random_bool".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Bool)) },
        );

        // random_choice([T]) -> T
        let choice_var = TypeVar::fresh();
        env.bindings.insert(
            "random_choice".to_string(),
            TypeScheme {
                vars: vec![choice_var],
                ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Var(choice_var)))], Box::new(Ty::Var(choice_var)))
            },
        );

        // ===== Float math operations =====
        // sqrt(Float) -> Float
        env.bindings.insert(
            "sqrt".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Float)) },
        );

        // pow(Float, Float) -> Float
        env.bindings.insert(
            "pow".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float, Ty::Float], Box::new(Ty::Float)) },
        );

        // sin(Float) -> Float
        env.bindings.insert(
            "sin".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Float)) },
        );

        // cos(Float) -> Float
        env.bindings.insert(
            "cos".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Float)) },
        );

        // tan(Float) -> Float
        env.bindings.insert(
            "tan".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Float)) },
        );

        // log(Float) -> Float (natural log)
        env.bindings.insert(
            "log".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Float)) },
        );

        // log10(Float) -> Float
        env.bindings.insert(
            "log10".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Float)) },
        );

        // exp(Float) -> Float
        env.bindings.insert(
            "exp".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Float)) },
        );

        // floor(Float) -> Int
        env.bindings.insert(
            "floor".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Int)) },
        );

        // ceil(Float) -> Int
        env.bindings.insert(
            "ceil".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Int)) },
        );

        // round(Float) -> Int
        env.bindings.insert(
            "round".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Int)) },
        );

        // abs_float(Float) -> Float
        env.bindings.insert(
            "abs_float".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Float)) },
        );

        // ===== Time functions =====
        // time_now() -> Int
        env.bindings.insert(
            "time_now".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Int)) },
        );

        // time_now_ms() -> Int
        env.bindings.insert(
            "time_now_ms".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Int)) },
        );

        // time_sleep(Int) -> ()
        env.bindings.insert(
            "time_sleep".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Unit)) },
        );

        env
    }

    /// Look up a variable's type scheme.
    pub fn get(&self, name: &str) -> Option<&TypeScheme> {
        self.bindings.get(name)
    }

    /// Insert a variable binding.
    pub fn insert(&mut self, name: String, scheme: TypeScheme) {
        self.bindings.insert(name, scheme);
    }

    /// Insert a type definition.
    pub fn insert_type(&mut self, name: String, def: TypeDef) {
        self.types.insert(name, def);
    }

    /// Look up a type definition.
    pub fn get_type(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(name)
    }

    /// Get free type variables in the environment.
    pub fn free_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();
        for scheme in self.bindings.values() {
            // Only include variables not bound by the scheme
            for var in scheme.ty.free_vars() {
                if !scheme.vars.contains(&var) && !vars.contains(&var) {
                    vars.push(var);
                }
            }
        }
        vars
    }

    /// Create a child scope.
    pub fn child(&self) -> Self {
        Self {
            bindings: self.bindings.clone(),
            types: self.types.clone(),
        }
    }

    /// Get all defined variable names in the environment.
    pub fn names(&self) -> impl Iterator<Item = &str> {
        self.bindings.keys().map(|s| s.as_str())
    }
}

/// Unification engine for type inference.
pub struct Unifier {
    subst: Substitution,
}

impl Unifier {
    pub fn new() -> Self {
        Self {
            subst: Substitution::new(),
        }
    }

    /// Get the current substitution.
    pub fn substitution(&self) -> &Substitution {
        &self.subst
    }

    /// Take ownership of the substitution.
    pub fn into_substitution(self) -> Substitution {
        self.subst
    }

    /// Unify two types.
    pub fn unify(&mut self, t1: &Ty, t2: &Ty, span: Span) -> Result<(), TypeError> {
        let t1 = t1.apply(&self.subst);
        let t2 = t2.apply(&self.subst);

        match (&t1, &t2) {
            // Same primitive types
            (Ty::Int, Ty::Int)
            | (Ty::I8, Ty::I8)
            | (Ty::I16, Ty::I16)
            | (Ty::I32, Ty::I32)
            | (Ty::I64, Ty::I64)
            | (Ty::I128, Ty::I128)
            | (Ty::UInt, Ty::UInt)
            | (Ty::U8, Ty::U8)
            | (Ty::U16, Ty::U16)
            | (Ty::U32, Ty::U32)
            | (Ty::U64, Ty::U64)
            | (Ty::U128, Ty::U128)
            | (Ty::Float, Ty::Float)
            | (Ty::F32, Ty::F32)
            | (Ty::F64, Ty::F64)
            | (Ty::Bool, Ty::Bool)
            | (Ty::Char, Ty::Char)
            | (Ty::Str, Ty::Str)
            | (Ty::Unit, Ty::Unit)
            | (Ty::Never, Ty::Never) => Ok(()),

            // Type variable unification
            (Ty::Var(v), t) | (t, Ty::Var(v)) => {
                if let Ty::Var(v2) = t {
                    if v == v2 {
                        return Ok(());
                    }
                }
                // Occurs check
                if t.free_vars().contains(v) {
                    return Err(TypeError::new(
                        format!("infinite type: ?{} = {}", v.id, t),
                        span,
                    ));
                }
                self.subst.insert(*v, t.clone());
                Ok(())
            }

            // Never type unifies with anything (it's a bottom type)
            (Ty::Never, _) | (_, Ty::Never) => Ok(()),

            // Error type unifies with anything (for error recovery)
            (Ty::Error, _) | (_, Ty::Error) => Ok(()),

            // Tuple unification
            (Ty::Tuple(ts1), Ty::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err(TypeError::new(
                        format!(
                            "tuple length mismatch: expected {} elements, found {}",
                            ts1.len(),
                            ts2.len()
                        ),
                        span,
                    ));
                }
                for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                    self.unify(t1, t2, span)?;
                }
                Ok(())
            }

            // Array unification
            (Ty::Array(e1, n1), Ty::Array(e2, n2)) => {
                if n1 != n2 {
                    return Err(TypeError::new(
                        format!("array size mismatch: expected {}, found {}", n1, n2),
                        span,
                    ));
                }
                self.unify(e1, e2, span)
            }

            // List unification
            (Ty::List(e1), Ty::List(e2)) => self.unify(e1, e2, span),

            // Map unification
            (Ty::Map(k1, v1), Ty::Map(k2, v2)) => {
                self.unify(k1, k2, span)?;
                self.unify(v1, v2, span)
            }

            // Set unification
            (Ty::Set(e1), Ty::Set(e2)) => self.unify(e1, e2, span),

            // Option unification
            (Ty::Option(t1), Ty::Option(t2)) => self.unify(t1, t2, span),

            // Unify Ty::Option with Ty::Named("Option", [...])
            // This allows T? to unify with Option[T]
            (Ty::Option(inner), Ty::Named(id, args))
            | (Ty::Named(id, args), Ty::Option(inner))
                if id.name == "Option" && args.len() == 1 =>
            {
                self.unify(inner, &args[0], span)
            }

            // Result unification
            (Ty::Result(ok1, err1), Ty::Result(ok2, err2)) => {
                self.unify(ok1, ok2, span)?;
                self.unify(err1, err2, span)
            }

            // Function unification
            (Ty::Fn(p1, r1), Ty::Fn(p2, r2)) => {
                if p1.len() != p2.len() {
                    return Err(TypeError::new(
                        format!(
                            "function arity mismatch: expected {} params, found {}",
                            p1.len(),
                            p2.len()
                        ),
                        span,
                    ));
                }
                for (t1, t2) in p1.iter().zip(p2.iter()) {
                    self.unify(t1, t2, span)?;
                }
                self.unify(r1, r2, span)
            }

            // Reference unification
            (Ty::Ref(t1, m1), Ty::Ref(t2, m2)) => {
                if m1 != m2 {
                    return Err(TypeError::new(
                        format!("mutability mismatch: {:?} vs {:?}", m1, m2),
                        span,
                    ));
                }
                self.unify(t1, t2, span)
            }

            // Pointer unification
            (Ty::Ptr(t1, m1), Ty::Ptr(t2, m2)) => {
                if m1 != m2 {
                    return Err(TypeError::new(
                        format!("mutability mismatch: {:?} vs {:?}", m1, m2),
                        span,
                    ));
                }
                self.unify(t1, t2, span)
            }

            // Named type unification
            (Ty::Named(id1, args1), Ty::Named(id2, args2)) => {
                if id1 != id2 {
                    return Err(TypeError::new(
                        format!("type mismatch: {} vs {}", id1.name, id2.name),
                        span,
                    ));
                }
                if args1.len() != args2.len() {
                    return Err(TypeError::new(
                        format!(
                            "type argument count mismatch: expected {}, found {}",
                            args1.len(),
                            args2.len()
                        ),
                        span,
                    ));
                }
                for (t1, t2) in args1.iter().zip(args2.iter()) {
                    self.unify(t1, t2, span)?;
                }
                Ok(())
            }

            // Mismatch
            _ => Err(TypeError::new(
                format!("type mismatch: expected {}, found {}", t1, t2),
                span,
            )),
        }
    }
}

impl Default for Unifier {
    fn default() -> Self {
        Self::new()
    }
}

/// Type inference engine.
pub struct InferenceEngine {
    env: TypeEnv,
    unifier: Unifier,
    /// Current function's return type (for checking return statements)
    return_type: Option<Ty>,
    /// Current type parameters (for generic functions/structs)
    /// Maps type parameter names (e.g., "T") to their type variables
    type_params: HashMap<String, TypeVar>,
}

impl InferenceEngine {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::with_builtins(),
            unifier: Unifier::new(),
            return_type: None,
            type_params: HashMap::new(),
        }
    }

    pub fn with_env(env: TypeEnv) -> Self {
        Self {
            env,
            unifier: Unifier::new(),
            return_type: None,
            type_params: HashMap::new(),
        }
    }

    /// Set up type parameters from a generic declaration.
    /// Returns the mapping of type param names to fresh type variables.
    fn setup_type_params(&mut self, generics: &Option<Generics>) -> HashMap<String, TypeVar> {
        let mut params = HashMap::new();
        if let Some(g) = generics {
            for param in &g.params {
                if let GenericParam::Type(tp) = param {
                    let tv = TypeVar::fresh();
                    params.insert(tp.name.name.clone(), tv);
                }
            }
        }
        params
    }

    /// Substitute named type parameters with concrete types.
    /// Used when instantiating generic struct types.
    fn substitute_type_params(&self, ty: &Ty, subst: &HashMap<String, Ty>) -> Ty {
        match ty {
            Ty::Var(tv) => {
                // Type variables from struct field definitions might be Named("T", [])
                // since they were created before we had fresh vars
                Ty::Var(*tv)
            }
            Ty::Named(id, args) => {
                // Check if this is a type parameter reference
                if args.is_empty() {
                    if let Some(replacement) = subst.get(&id.name) {
                        return replacement.clone();
                    }
                }
                // Otherwise, recursively substitute in args
                let new_args = args.iter()
                    .map(|a| self.substitute_type_params(a, subst))
                    .collect();
                Ty::Named(id.clone(), new_args)
            }
            Ty::List(elem) => Ty::List(Box::new(self.substitute_type_params(elem, subst))),
            Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(|e| self.substitute_type_params(e, subst)).collect()),
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| self.substitute_type_params(p, subst)).collect(),
                Box::new(self.substitute_type_params(ret, subst)),
            ),
            Ty::Option(inner) => Ty::Option(Box::new(self.substitute_type_params(inner, subst))),
            Ty::Result(ok, err) => Ty::Result(
                Box::new(self.substitute_type_params(ok, subst)),
                Box::new(self.substitute_type_params(err, subst)),
            ),
            Ty::Ref(inner, m) => Ty::Ref(Box::new(self.substitute_type_params(inner, subst)), *m),
            Ty::Ptr(inner, m) => Ty::Ptr(Box::new(self.substitute_type_params(inner, subst)), *m),
            Ty::Map(k, v) => Ty::Map(
                Box::new(self.substitute_type_params(k, subst)),
                Box::new(self.substitute_type_params(v, subst)),
            ),
            Ty::Set(elem) => Ty::Set(Box::new(self.substitute_type_params(elem, subst))),
            // Primitive types don't need substitution
            _ => ty.clone(),
        }
    }

    /// Infer types for a list of items (a module/file).
    pub fn infer_items(&mut self, items: &[Item]) -> Result<(), TypeError> {
        // First pass: collect type definitions
        for item in items {
            self.collect_type_def(item)?;
        }

        // Second pass: collect function signatures
        for item in items {
            self.collect_function_sig(item)?;
        }

        // Third pass: type check function bodies
        for item in items {
            self.check_item(item)?;
        }

        Ok(())
    }

    /// Extract type parameter names from generics.
    fn get_type_params(&self, generics: &Option<crate::parser::Generics>) -> Vec<String> {
        generics
            .as_ref()
            .map(|g| {
                g.params
                    .iter()
                    .filter_map(|p| match p {
                        GenericParam::Type(tp) => Some(tp.name.name.clone()),
                        GenericParam::Const(_) => None,
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Collect a type definition into the environment.
    fn collect_type_def(&mut self, item: &Item) -> Result<(), TypeError> {
        match &item.kind {
            ItemKind::Struct(s) => {
                let type_params = self.get_type_params(&s.generics);

                // Set up type params context before processing fields
                let old_type_params = std::mem::take(&mut self.type_params);
                self.type_params = self.setup_type_params(&s.generics);

                let fields = match &s.kind {
                    crate::parser::StructKind::Named(fields) => {
                        fields
                            .iter()
                            .map(|f| {
                                let ty = self.ast_type_to_ty(&f.ty)?;
                                Ok((f.name.name.clone(), ty))
                            })
                            .collect::<Result<Vec<_>, TypeError>>()?
                    }
                    crate::parser::StructKind::Tuple(types) => {
                        types
                            .iter()
                            .enumerate()
                            .map(|(i, t)| {
                                let ty = self.ast_type_to_ty(t)?;
                                Ok((format!("{}", i), ty))
                            })
                            .collect::<Result<Vec<_>, TypeError>>()?
                    }
                    crate::parser::StructKind::Unit => vec![],
                };

                // Restore old type params
                self.type_params = old_type_params;

                self.env.insert_type(
                    s.name.name.clone(),
                    TypeDef::Struct { type_params, fields },
                );
            }
            ItemKind::Enum(e) => {
                let type_params = self.get_type_params(&e.generics);
                let enum_name = e.name.name.clone();

                // Set up type params context before processing variants
                let old_type_params = std::mem::take(&mut self.type_params);
                self.type_params = self.setup_type_params(&e.generics);

                let variants = e
                    .variants
                    .iter()
                    .map(|v| {
                        let fields = match &v.kind {
                            VariantKind::Unit => vec![],
                            VariantKind::Tuple(types) => {
                                types
                                    .iter()
                                    .map(|t| self.ast_type_to_ty(t))
                                    .collect::<Result<Vec<_>, _>>()?
                            }
                            VariantKind::Named(fields) => {
                                fields
                                    .iter()
                                    .map(|f| self.ast_type_to_ty(&f.ty))
                                    .collect::<Result<Vec<_>, _>>()?
                            }
                        };
                        Ok((v.name.name.clone(), fields))
                    })
                    .collect::<Result<Vec<_>, TypeError>>()?;

                // Restore old type params
                let current_type_params = std::mem::replace(&mut self.type_params, old_type_params);

                // Use the type variables we created for the variant field processing
                let type_var_map: Vec<(String, TypeVar)> = current_type_params
                    .into_iter()
                    .collect();

                // Build the enum type with fresh type variables
                let enum_ty_args: Vec<Ty> = type_var_map.iter().map(|(_, tv)| Ty::Var(*tv)).collect();
                let enum_ty = Ty::Named(TypeId::new(&enum_name), enum_ty_args);

                // Add constructor bindings for each variant
                for (variant_name, field_tys) in &variants {
                    let constructor_type = if field_tys.is_empty() {
                        // Unit variant: just the enum type
                        enum_ty.clone()
                    } else {
                        // Tuple variant: function from fields to enum type
                        Ty::Fn(field_tys.clone(), Box::new(enum_ty.clone()))
                    };

                    let scheme = TypeScheme {
                        vars: type_var_map.iter().map(|(_, tv)| *tv).collect(),
                        ty: constructor_type,
                    };

                    self.env.insert(variant_name.clone(), scheme);
                }

                self.env.insert_type(
                    enum_name,
                    TypeDef::Enum { type_params, variants },
                );
            }
            ItemKind::TypeAlias(t) => {
                let type_params = self.get_type_params(&t.generics);

                let target = if let Some(ty) = &t.ty {
                    self.ast_type_to_ty(ty)?
                } else {
                    Ty::Error
                };

                self.env.insert_type(
                    t.name.name.clone(),
                    TypeDef::Alias { type_params, target },
                );
            }
            _ => {}
        }
        Ok(())
    }

    /// Collect a function signature into the environment.
    fn collect_function_sig(&mut self, item: &Item) -> Result<(), TypeError> {
        if let ItemKind::Function(f) = &item.kind {
            // Set up type parameters for generic functions
            let old_type_params = std::mem::take(&mut self.type_params);
            self.type_params = self.setup_type_params(&f.generics);

            let param_types: Vec<Ty> = f
                .params
                .iter()
                .map(|p| self.ast_type_to_ty(&p.ty))
                .collect::<Result<Vec<_>, _>>()?;

            let return_type = if let Some(ty) = &f.return_type {
                self.ast_type_to_ty(ty)?
            } else {
                Ty::Unit
            };

            let fn_type = Ty::Fn(param_types, Box::new(return_type));

            // For generic functions, create a TypeScheme with the type variables
            let scheme = if f.generics.is_some() {
                let vars: Vec<TypeVar> = self.type_params.values().copied().collect();
                TypeScheme { vars, ty: fn_type }
            } else {
                // Non-generic: generalize normally
                let env_vars = self.env.free_vars();
                TypeScheme::generalize(fn_type, &env_vars)
            };

            self.env.insert(f.name.name.clone(), scheme);

            // Restore old type params
            self.type_params = old_type_params;
        }
        Ok(())
    }

    /// Type check an item.
    fn check_item(&mut self, item: &Item) -> Result<(), TypeError> {
        match &item.kind {
            ItemKind::Function(f) => {
                // Only check if there's a body
                let body = match &f.body {
                    Some(body) => body,
                    None => return Ok(()),
                };

                // Set up type parameters for generic functions
                let old_type_params = std::mem::take(&mut self.type_params);
                self.type_params = self.setup_type_params(&f.generics);

                // Create a new scope for the function body
                let mut body_env = self.env.child();

                // Add parameters to the scope
                for param in &f.params {
                    let ty = self.ast_type_to_ty(&param.ty)?;
                    body_env.insert(param.name.name.clone(), TypeScheme::mono(ty));
                }

                // Set up return type
                let return_type = if let Some(ty) = &f.return_type {
                    self.ast_type_to_ty(ty)?
                } else {
                    Ty::Unit
                };

                let old_return = self.return_type.take();
                self.return_type = Some(return_type.clone());

                // Infer body type
                let old_env = std::mem::replace(&mut self.env, body_env);
                let body_type = match body {
                    FnBody::Expr(expr) => self.infer_expr(expr)?,
                    FnBody::Block(block) => self.infer_block(block)?,
                };
                self.env = old_env;
                self.return_type = old_return;

                // Restore old type params
                self.type_params = old_type_params;

                // Unify body type with return type
                self.unifier.unify(&body_type, &return_type, item.span)?;
            }
            ItemKind::Impl(i) => {
                // Type check impl items
                for impl_item in &i.items {
                    match impl_item {
                        crate::parser::ImplItem::Function(f) => {
                            let item = Item {
                                kind: ItemKind::Function(f.clone()),
                                attrs: vec![],
                                span: f.span,
                            };
                            self.check_item(&item)?;
                        }
                        crate::parser::ImplItem::TypeAlias(_) => {}
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Infer the type of a block.
    pub fn infer_block(&mut self, block: &Block) -> Result<Ty, TypeError> {
        let mut result_type = Ty::Unit;

        for stmt in &block.stmts {
            result_type = self.infer_stmt(stmt)?;
        }

        Ok(result_type)
    }

    /// Infer the type of a statement.
    fn infer_stmt(&mut self, stmt: &Stmt) -> Result<Ty, TypeError> {
        match &stmt.kind {
            StmtKind::Let(l) => {
                // Infer initializer type
                let init_type = self.infer_expr(&l.init)?;

                // If there's an explicit type, unify with it
                let var_type = if let Some(ty) = &l.ty {
                    let declared_type = self.ast_type_to_ty(ty)?;
                    self.unifier.unify(&init_type, &declared_type, stmt.span)?;
                    declared_type
                } else {
                    init_type
                };

                // Bind the pattern
                self.bind_pattern(&l.pattern, &var_type)?;

                Ok(Ty::Unit)
            }
            StmtKind::Expr(e) => {
                self.infer_expr(e)
            }
            StmtKind::Item(item) => {
                self.check_item(item)?;
                Ok(Ty::Unit)
            }
            StmtKind::Empty => Ok(Ty::Unit),
        }
    }

    /// Infer the type of an expression.
    pub fn infer_expr(&mut self, expr: &Expr) -> Result<Ty, TypeError> {
        match &expr.kind {
            ExprKind::Literal(lit) => self.infer_literal(&lit.kind, expr.span),

            ExprKind::Ident(name) => {
                if let Some(scheme) = self.env.get(&name.name) {
                    Ok(scheme.instantiate())
                } else {
                    // Check for similar variable names to provide helpful suggestions
                    let msg = if let Some(suggestion) = self.find_similar_name(&name.name) {
                        format!(
                            "undefined variable: `{}`. Did you mean `{}`?",
                            name.name, suggestion
                        )
                    } else {
                        format!("undefined variable: {}", name.name)
                    };
                    Err(TypeError::new(msg, expr.span))
                }
            }

            ExprKind::Binary(left, op, right) => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;

                match op {
                    // Arithmetic operators
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        self.unifier.unify(&left_ty, &right_ty, expr.span)?;
                        Ok(left_ty)
                    }

                    // Comparison operators
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        self.unifier.unify(&left_ty, &right_ty, expr.span)?;
                        Ok(Ty::Bool)
                    }

                    // Logical operators
                    BinOp::And | BinOp::Or => {
                        self.unifier.unify(&left_ty, &Ty::Bool, expr.span)?;
                        self.unifier.unify(&right_ty, &Ty::Bool, expr.span)?;
                        Ok(Ty::Bool)
                    }

                    // Bitwise operators
                    BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::Shl | BinOp::Shr => {
                        self.unifier.unify(&left_ty, &right_ty, expr.span)?;
                        Ok(left_ty)
                    }
                }
            }

            ExprKind::Unary(op, operand) => {
                let operand_ty = self.infer_expr(operand)?;

                match op {
                    UnaryOp::Neg => Ok(operand_ty),
                    UnaryOp::Not => {
                        self.unifier.unify(&operand_ty, &Ty::Bool, expr.span)?;
                        Ok(Ty::Bool)
                    }
                    UnaryOp::Ref => Ok(Ty::Ref(Box::new(operand_ty), Mutability::Immutable)),
                    UnaryOp::RefMut => Ok(Ty::Ref(Box::new(operand_ty), Mutability::Mutable)),
                    UnaryOp::Deref => {
                        let inner = Ty::fresh_var();
                        let ref_ty = Ty::Ref(Box::new(inner.clone()), Mutability::Immutable);
                        self.unifier.unify(&operand_ty, &ref_ty, expr.span)?;
                        Ok(inner)
                    }
                }
            }

            ExprKind::Call(callee, args) => {
                let callee_ty = self.infer_expr(callee)?;
                let arg_types: Vec<Ty> = args
                    .iter()
                    .map(|a| self.infer_expr(&a.value))
                    .collect::<Result<Vec<_>, _>>()?;

                let result_ty = Ty::fresh_var();
                let expected_fn = Ty::Fn(arg_types, Box::new(result_ty.clone()));
                self.unifier.unify(&callee_ty, &expected_fn, expr.span)?;
                Ok(result_ty)
            }

            ExprKind::MethodCall(receiver, _method, args) => {
                let _receiver_ty = self.infer_expr(receiver)?;
                let _arg_types: Vec<Ty> = args
                    .iter()
                    .map(|a| self.infer_expr(&a.value))
                    .collect::<Result<Vec<_>, _>>()?;

                // Method resolution would require trait lookup
                Ok(Ty::fresh_var())
            }

            ExprKind::Field(base, _field) => {
                let _base_ty = self.infer_expr(base)?;
                // Field lookup would need struct info
                Ok(Ty::fresh_var())
            }

            ExprKind::TupleField(base, index) => {
                let base_ty = self.infer_expr(base)?;
                // For tuple indexing, we need the tuple type
                if let Ty::Tuple(elems) = base_ty.apply(&self.unifier.subst) {
                    if *index < elems.len() {
                        return Ok(elems[*index].clone());
                    }
                }
                Ok(Ty::fresh_var())
            }

            ExprKind::Index(base, index) => {
                let base_ty = self.infer_expr(base)?;
                let index_ty = self.infer_expr(index)?;

                // For list/array indexing
                let elem_ty = Ty::fresh_var();
                let list_ty = Ty::List(Box::new(elem_ty.clone()));

                if self.unifier.unify(&base_ty, &list_ty, expr.span).is_ok() {
                    self.unifier.unify(&index_ty, &Ty::Int, expr.span)?;
                    return Ok(elem_ty);
                }

                // Could be map indexing
                let value_ty = Ty::fresh_var();
                let map_ty = Ty::Map(Box::new(index_ty), Box::new(value_ty.clone()));
                self.unifier.unify(&base_ty, &map_ty, expr.span)?;
                Ok(value_ty)
            }

            ExprKind::Tuple(elems) => {
                let elem_types: Vec<Ty> = elems
                    .iter()
                    .map(|e| self.infer_expr(e))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Ty::Tuple(elem_types))
            }

            ExprKind::Array(elems) => {
                let elem_ty = Ty::fresh_var();
                for elem in elems {
                    let ty = self.infer_expr(elem)?;
                    self.unifier.unify(&ty, &elem_ty, expr.span)?;
                }
                Ok(Ty::List(Box::new(elem_ty)))
            }

            ExprKind::ArrayRepeat(elem, count) => {
                let elem_ty = self.infer_expr(elem)?;
                let _count_ty = self.infer_expr(count)?;
                Ok(Ty::List(Box::new(elem_ty)))
            }

            ExprKind::MapOrSet(entries) => {
                if entries.is_empty() {
                    // Empty - could be map or set
                    Ok(Ty::Map(Box::new(Ty::fresh_var()), Box::new(Ty::fresh_var())))
                } else if entries.first().map(|e| e.value.is_some()).unwrap_or(false) {
                    // Map
                    let key_ty = Ty::fresh_var();
                    let value_ty = Ty::fresh_var();
                    for entry in entries {
                        let kt = self.infer_expr(&entry.key)?;
                        self.unifier.unify(&kt, &key_ty, expr.span)?;
                        if let Some(v) = &entry.value {
                            let vt = self.infer_expr(v)?;
                            self.unifier.unify(&vt, &value_ty, expr.span)?;
                        }
                    }
                    Ok(Ty::Map(Box::new(key_ty), Box::new(value_ty)))
                } else {
                    // Set
                    let elem_ty = Ty::fresh_var();
                    for entry in entries {
                        let ty = self.infer_expr(&entry.key)?;
                        self.unifier.unify(&ty, &elem_ty, expr.span)?;
                    }
                    Ok(Ty::Set(Box::new(elem_ty)))
                }
            }

            ExprKind::If(if_expr) => {
                let cond_ty = self.infer_expr(&if_expr.condition)?;
                self.unifier.unify(&cond_ty, &Ty::Bool, expr.span)?;

                let then_ty = match &if_expr.then_branch {
                    crate::parser::IfBranch::Expr(e) => self.infer_expr(e)?,
                    crate::parser::IfBranch::Block(b) => self.infer_block(b)?,
                };

                if let Some(else_branch) = &if_expr.else_branch {
                    let else_ty = match else_branch {
                        crate::parser::ElseBranch::Expr(e) => self.infer_expr(e)?,
                        crate::parser::ElseBranch::Block(b) => self.infer_block(b)?,
                        crate::parser::ElseBranch::ElseIf(elif) => {
                            let elif_expr = Expr {
                                kind: ExprKind::If(elif.clone()),
                                span: elif.span,
                            };
                            self.infer_expr(&elif_expr)?
                        }
                    };
                    self.unifier.unify(&then_ty, &else_ty, expr.span)?;
                    Ok(then_ty)
                } else {
                    self.unifier.unify(&then_ty, &Ty::Unit, expr.span)?;
                    Ok(Ty::Unit)
                }
            }

            ExprKind::Match(scrutinee, arms) => {
                let scrutinee_ty = self.infer_expr(scrutinee)?;
                let result_ty = Ty::fresh_var();

                for arm in arms {
                    self.check_pattern(&arm.pattern, &scrutinee_ty)?;

                    let mut arm_env = self.env.child();
                    self.collect_pattern_bindings(&arm.pattern, &scrutinee_ty, &mut arm_env)?;

                    if let Some(guard) = &arm.guard {
                        let old_env = std::mem::replace(&mut self.env, arm_env.clone());
                        let guard_ty = self.infer_expr(guard)?;
                        self.env = old_env;
                        self.unifier.unify(&guard_ty, &Ty::Bool, expr.span)?;
                    }

                    let old_env = std::mem::replace(&mut self.env, arm_env);
                    let body_ty = self.infer_expr(&arm.body)?;
                    self.env = old_env;

                    self.unifier.unify(&body_ty, &result_ty, expr.span)?;
                }

                Ok(result_ty)
            }

            ExprKind::Block(b) => self.infer_block(b),

            ExprKind::Closure(c) => {
                let param_types: Vec<Ty> = c
                    .params
                    .iter()
                    .map(|p| {
                        if let Some(ty) = &p.ty {
                            self.ast_type_to_ty(ty)
                        } else {
                            Ok(Ty::fresh_var())
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let mut closure_env = self.env.child();
                for (param, ty) in c.params.iter().zip(param_types.iter()) {
                    closure_env.insert(param.name.name.clone(), TypeScheme::mono(ty.clone()));
                }

                let old_env = std::mem::replace(&mut self.env, closure_env);
                let body_ty = self.infer_expr(&c.body)?;
                self.env = old_env;

                let return_ty = if let Some(ty) = &c.return_type {
                    let declared = self.ast_type_to_ty(ty)?;
                    self.unifier.unify(&body_ty, &declared, expr.span)?;
                    declared
                } else {
                    body_ty
                };

                Ok(Ty::Fn(param_types, Box::new(return_ty)))
            }

            ExprKind::Return(value) => {
                let value_ty = if let Some(v) = value {
                    self.infer_expr(v)?
                } else {
                    Ty::Unit
                };

                if let Some(return_type) = &self.return_type {
                    self.unifier.unify(&value_ty, return_type, expr.span)?;
                }

                Ok(Ty::Never)
            }

            ExprKind::Break(_, value) => {
                if let Some(v) = value {
                    self.infer_expr(v)?;
                }
                Ok(Ty::Never)
            }

            ExprKind::Continue(_) => Ok(Ty::Never),

            ExprKind::For(pattern, iter, body) => {
                let iter_ty = self.infer_expr(iter)?;

                // For loops can iterate over:
                // 1. Ranges: Range[T] yields T
                // 2. Lists/Arrays: [T] yields T
                let elem_ty = Ty::fresh_var();

                // Check if iterator is a Range expression directly
                let is_range = matches!(&iter.kind, ExprKind::Range(_, _, _));

                if is_range {
                    // For range iteration, unify with Range[elem_ty]
                    let range_ty = Ty::Named(TypeId::new("Range"), vec![elem_ty.clone()]);
                    self.unifier.unify(&iter_ty, &range_ty, expr.span)?;
                } else {
                    // For array iteration, unify with List[elem_ty]
                    let list_ty = Ty::List(Box::new(elem_ty.clone()));
                    self.unifier.unify(&iter_ty, &list_ty, expr.span)?;
                }

                let mut loop_env = self.env.child();
                self.collect_pattern_bindings(pattern, &elem_ty, &mut loop_env)?;

                let old_env = std::mem::replace(&mut self.env, loop_env);
                self.infer_block(body)?;
                self.env = old_env;

                Ok(Ty::Unit)
            }

            ExprKind::While(cond, body) => {
                let cond_ty = self.infer_expr(cond)?;
                self.unifier.unify(&cond_ty, &Ty::Bool, expr.span)?;
                self.infer_block(body)?;
                Ok(Ty::Unit)
            }

            ExprKind::WhileLet(pattern, expr_val, body) => {
                let expr_ty = self.infer_expr(expr_val)?;
                self.check_pattern(pattern, &expr_ty)?;

                let mut loop_env = self.env.child();
                self.collect_pattern_bindings(pattern, &expr_ty, &mut loop_env)?;

                let old_env = std::mem::replace(&mut self.env, loop_env);
                self.infer_block(body)?;
                self.env = old_env;

                Ok(Ty::Unit)
            }

            ExprKind::Loop(body) => {
                self.infer_block(body)?;
                Ok(Ty::Never)
            }

            ExprKind::Struct(path, fields, base) => {
                let type_name = path.segments.last().map(|s| s.name.name.as_str()).unwrap_or("");
                let type_id = TypeId::new(type_name);

                // Look up the struct definition to get type parameters and field types
                if let Some(TypeDef::Struct { type_params, fields: def_fields }) = self.env.get_type(type_name).cloned() {
                    // Create fresh type variables for each type parameter
                    let type_args: Vec<Ty> = type_params.iter().map(|_| Ty::fresh_var()).collect();

                    // Build substitution from param names to fresh type vars
                    let subst: HashMap<String, Ty> = type_params.iter()
                        .zip(type_args.iter())
                        .map(|(name, ty)| (name.clone(), ty.clone()))
                        .collect();

                    // Check each provided field and unify with definition
                    for field in fields {
                        if let Some(value) = &field.value {
                            let value_ty = self.infer_expr(value)?;

                            // Find the field in the definition
                            if let Some((_, def_field_ty)) = def_fields.iter().find(|(n, _)| n == &field.name.name) {
                                // Apply type parameter substitution to the definition type
                                let expected_ty = self.substitute_type_params(def_field_ty, &subst);
                                self.unifier.unify(&value_ty, &expected_ty, expr.span)?;
                            }
                        }
                    }

                    if let Some(b) = base {
                        let base_ty = self.infer_expr(b)?;
                        self.unifier.unify(&base_ty, &Ty::Named(type_id.clone(), type_args.clone()), expr.span)?;
                    }

                    Ok(Ty::Named(type_id, type_args))
                } else {
                    // Fallback for unknown struct types - just infer field types
                    for field in fields {
                        if let Some(value) = &field.value {
                            self.infer_expr(value)?;
                        }
                    }
                    if let Some(b) = base {
                        self.infer_expr(b)?;
                    }
                    Ok(Ty::Named(type_id, vec![]))
                }
            }

            ExprKind::Path(p) => {
                let name = p.segments.last().map(|s| s.name.clone()).unwrap_or_default();

                if let Some(scheme) = self.env.get(&name) {
                    Ok(scheme.instantiate())
                } else {
                    Ok(Ty::fresh_var())
                }
            }

            ExprKind::Cast(e, ty) => {
                self.infer_expr(e)?;
                self.ast_type_to_ty(ty)
            }

            ExprKind::Range(start, end, _inclusive) => {
                let ty = Ty::fresh_var();
                if let Some(s) = start {
                    let start_ty = self.infer_expr(s)?;
                    self.unifier.unify(&start_ty, &ty, expr.span)?;
                }
                if let Some(e) = end {
                    let end_ty = self.infer_expr(e)?;
                    self.unifier.unify(&end_ty, &ty, expr.span)?;
                }
                Ok(Ty::Named(TypeId::new("Range"), vec![ty]))
            }

            ExprKind::Pipeline(left, right) => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;
                let result_ty = Ty::fresh_var();
                let expected_fn = Ty::Fn(vec![left_ty], Box::new(result_ty.clone()));
                self.unifier.unify(&right_ty, &expected_fn, expr.span)?;
                Ok(result_ty)
            }

            ExprKind::Paren(e) => self.infer_expr(e),

            ExprKind::Assign(target, value, _mutable) => {
                let target_ty = self.infer_expr(target)?;
                let value_ty = self.infer_expr(value)?;
                self.unifier.unify(&target_ty, &value_ty, expr.span)?;
                Ok(Ty::Unit)
            }

            ExprKind::AssignOp(target, _op, value) => {
                let target_ty = self.infer_expr(target)?;
                let value_ty = self.infer_expr(value)?;
                self.unifier.unify(&target_ty, &value_ty, expr.span)?;
                Ok(Ty::Unit)
            }

            ExprKind::Try(e) => {
                let operand_ty = self.infer_expr(e)?;
                let ok_ty = Ty::fresh_var();
                let err_ty = Ty::fresh_var();
                let result_ty = Ty::Result(Box::new(ok_ty.clone()), Box::new(err_ty));
                self.unifier.unify(&operand_ty, &result_ty, expr.span)?;
                Ok(ok_ty)
            }

            ExprKind::Await(e) => {
                self.infer_expr(e)?;
                Ok(Ty::fresh_var())
            }

            ExprKind::Coalesce(left, right) => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;
                // left should be Option[T], right should be T
                let inner_ty = Ty::fresh_var();
                let opt_ty = Ty::Option(Box::new(inner_ty.clone()));
                self.unifier.unify(&left_ty, &opt_ty, expr.span)?;
                self.unifier.unify(&right_ty, &inner_ty, expr.span)?;
                Ok(inner_ty)
            }

            ExprKind::Async(block) => {
                self.infer_block(block)?;
                Ok(Ty::fresh_var()) // Would be Future[T]
            }

            ExprKind::Unsafe(block) => {
                self.infer_block(block)
            }

            ExprKind::FieldShorthand(_) | ExprKind::OpShorthand(_, _, _) => {
                // These are closure-like constructs
                Ok(Ty::fresh_var())
            }
        }
    }

    /// Infer the type of a literal.
    fn infer_literal(&self, lit: &LiteralKind, _span: Span) -> Result<Ty, TypeError> {
        match lit {
            LiteralKind::Int(_) => Ok(Ty::Int),
            LiteralKind::Float(_) => Ok(Ty::Float),
            LiteralKind::String(_) => Ok(Ty::Str),
            LiteralKind::Char(_) => Ok(Ty::Char),
            LiteralKind::Bool(_) => Ok(Ty::Bool),
            LiteralKind::None => Ok(Ty::Option(Box::new(Ty::fresh_var()))),
        }
    }

    /// Check that a pattern matches a type.
    fn check_pattern(&mut self, pattern: &Pattern, ty: &Ty) -> Result<(), TypeError> {
        match &pattern.kind {
            PatternKind::Wildcard => Ok(()),
            PatternKind::Ident(_, _, _) => Ok(()),
            PatternKind::Literal(lit) => {
                let lit_ty = self.infer_literal(&lit.kind, pattern.span)?;
                self.unifier.unify(&lit_ty, ty, pattern.span)
            }
            PatternKind::Tuple(elems) => {
                if let Ty::Tuple(elem_tys) = ty {
                    if elems.len() != elem_tys.len() {
                        return Err(TypeError::new(
                            format!(
                                "tuple pattern has {} elements but type has {}",
                                elems.len(),
                                elem_tys.len()
                            ),
                            pattern.span,
                        ));
                    }
                    for (p, t) in elems.iter().zip(elem_tys.iter()) {
                        self.check_pattern(p, t)?;
                    }
                    Ok(())
                } else {
                    let elem_tys: Vec<_> = elems.iter().map(|_| Ty::fresh_var()).collect();
                    let tuple_ty = Ty::Tuple(elem_tys.clone());
                    self.unifier.unify(&tuple_ty, ty, pattern.span)?;
                    for (p, t) in elems.iter().zip(elem_tys.iter()) {
                        self.check_pattern(p, t)?;
                    }
                    Ok(())
                }
            }
            PatternKind::List(elems, _rest) => {
                let elem_ty = Ty::fresh_var();
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                self.unifier.unify(&list_ty, ty, pattern.span)?;
                for p in elems {
                    self.check_pattern(p, &elem_ty)?;
                }
                Ok(())
            }
            PatternKind::Struct(_path, fields, _rest) => {
                for field in fields {
                    if let Some(p) = &field.pattern {
                        self.check_pattern(p, &Ty::fresh_var())?;
                    }
                }
                Ok(())
            }
            PatternKind::Or(ps) => {
                for p in ps {
                    self.check_pattern(p, ty)?;
                }
                Ok(())
            }
            PatternKind::Range(start, end, _inclusive) => {
                if let Some(s) = start {
                    self.check_pattern(s, ty)?;
                }
                if let Some(e) = end {
                    self.check_pattern(e, ty)?;
                }
                Ok(())
            }
            PatternKind::Ref(inner, _mutable) => {
                let inner_ty = Ty::fresh_var();
                let ref_ty = Ty::Ref(Box::new(inner_ty.clone()), Mutability::Immutable);
                self.unifier.unify(&ref_ty, ty, pattern.span)?;
                self.check_pattern(inner, &inner_ty)
            }
            PatternKind::Rest => Ok(()),
        }
    }

    /// Collect pattern bindings into the environment.
    fn collect_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        ty: &Ty,
        env: &mut TypeEnv,
    ) -> Result<(), TypeError> {
        match &pattern.kind {
            PatternKind::Wildcard => Ok(()),
            PatternKind::Ident(ident, _mutable, subpattern) => {
                env.insert(ident.name.clone(), TypeScheme::mono(ty.clone()));
                if let Some(sub) = subpattern {
                    self.collect_pattern_bindings(sub, ty, env)?;
                }
                Ok(())
            }
            PatternKind::Tuple(elems) => {
                if let Ty::Tuple(elem_tys) = ty.apply(&self.unifier.subst) {
                    for (p, t) in elems.iter().zip(elem_tys.iter()) {
                        self.collect_pattern_bindings(p, t, env)?;
                    }
                }
                Ok(())
            }
            PatternKind::List(elems, rest) => {
                if let Ty::List(elem_ty) = ty.apply(&self.unifier.subst) {
                    for p in elems {
                        self.collect_pattern_bindings(p, &elem_ty, env)?;
                    }
                    if let Some(r) = rest {
                        self.collect_pattern_bindings(r, ty, env)?;
                    }
                }
                Ok(())
            }
            PatternKind::Struct(_path, fields, _rest) => {
                for field in fields {
                    if let Some(p) = &field.pattern {
                        self.collect_pattern_bindings(p, &Ty::fresh_var(), env)?;
                    } else {
                        // Shorthand: field name is also the binding
                        env.insert(field.name.name.clone(), TypeScheme::mono(Ty::fresh_var()));
                    }
                }
                Ok(())
            }
            PatternKind::Or(ps) => {
                if let Some(first) = ps.first() {
                    self.collect_pattern_bindings(first, ty, env)?;
                }
                Ok(())
            }
            PatternKind::Ref(inner, _mutable) => {
                if let Ty::Ref(inner_ty, _) = ty.apply(&self.unifier.subst) {
                    self.collect_pattern_bindings(inner, &inner_ty, env)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// Bind a pattern to a type (for let statements).
    fn bind_pattern(&mut self, pattern: &Pattern, ty: &Ty) -> Result<(), TypeError> {
        match &pattern.kind {
            PatternKind::Ident(ident, _mutable, _subpattern) => {
                self.env.insert(ident.name.clone(), TypeScheme::mono(ty.clone()));
            }
            PatternKind::Tuple(elems) => {
                if let Ty::Tuple(elem_tys) = ty.apply(&self.unifier.subst) {
                    for (p, t) in elems.iter().zip(elem_tys.iter()) {
                        self.bind_pattern(p, t)?;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Convert an AST type to a Ty.
    pub fn ast_type_to_ty(&self, ast_ty: &AstType) -> Result<Ty, TypeError> {
        match &ast_ty.kind {
            AstTypeKind::Path(p) => {
                let name = p.segments.last().map(|s| s.name.name.as_str()).unwrap_or("");
                let args: Vec<Ty> = p
                    .segments
                    .last()
                    .and_then(|s| s.args.as_ref())
                    .map(|ga| {
                        ga.args
                            .iter()
                            .filter_map(|a| match a {
                                GenericArg::Type(t) => Some(self.ast_type_to_ty(t)),
                                GenericArg::Expr(_) => None,
                            })
                            .collect::<Result<Vec<_>, _>>()
                    })
                    .transpose()?
                    .unwrap_or_default();

                // First check if this is a type parameter
                if let Some(tv) = self.type_params.get(name) {
                    return Ok(Ty::Var(*tv));
                }

                match name {
                    "Int" => Ok(Ty::Int),
                    "i8" => Ok(Ty::I8),
                    "i16" => Ok(Ty::I16),
                    "i32" => Ok(Ty::I32),
                    "i64" => Ok(Ty::I64),
                    "i128" => Ok(Ty::I128),
                    "UInt" | "uint" => Ok(Ty::UInt),
                    "u8" => Ok(Ty::U8),
                    "u16" => Ok(Ty::U16),
                    "u32" => Ok(Ty::U32),
                    "u64" => Ok(Ty::U64),
                    "u128" => Ok(Ty::U128),
                    "Float" => Ok(Ty::Float),
                    "f32" => Ok(Ty::F32),
                    "f64" => Ok(Ty::F64),
                    "Bool" => Ok(Ty::Bool),
                    "Char" => Ok(Ty::Char),
                    "Str" => Ok(Ty::Str),
                    _ => Ok(Ty::Named(TypeId::new(name), args)),
                }
            }
            AstTypeKind::Tuple(elems) => {
                let tys: Vec<Ty> = elems
                    .iter()
                    .map(|t| self.ast_type_to_ty(t))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Ty::Tuple(tys))
            }
            AstTypeKind::List(elem) => {
                let elem_ty = self.ast_type_to_ty(elem)?;
                Ok(Ty::List(Box::new(elem_ty)))
            }
            AstTypeKind::Map(key, value) => {
                let key_ty = self.ast_type_to_ty(key)?;
                let value_ty = self.ast_type_to_ty(value)?;
                Ok(Ty::Map(Box::new(key_ty), Box::new(value_ty)))
            }
            AstTypeKind::Set(elem) => {
                let elem_ty = self.ast_type_to_ty(elem)?;
                Ok(Ty::Set(Box::new(elem_ty)))
            }
            AstTypeKind::Array(elem, _size) => {
                let elem_ty = self.ast_type_to_ty(elem)?;
                // For now, treat as list since size is an expression
                Ok(Ty::List(Box::new(elem_ty)))
            }
            AstTypeKind::Option(inner) => {
                let inner_ty = self.ast_type_to_ty(inner)?;
                Ok(Ty::Option(Box::new(inner_ty)))
            }
            AstTypeKind::Result(ok, err) => {
                let ok_ty = self.ast_type_to_ty(ok)?;
                let err_ty = if let Some(e) = err {
                    self.ast_type_to_ty(e)?
                } else {
                    // Default error type is Str for `T!` syntax
                    Ty::Str
                };
                Ok(Ty::Result(Box::new(ok_ty), Box::new(err_ty)))
            }
            AstTypeKind::Fn(params, ret) => {
                let param_tys: Vec<Ty> = params
                    .iter()
                    .map(|t| self.ast_type_to_ty(t))
                    .collect::<Result<Vec<_>, _>>()?;
                let ret_ty = self.ast_type_to_ty(ret)?;
                Ok(Ty::Fn(param_tys, Box::new(ret_ty)))
            }
            AstTypeKind::Ref(inner, mutable) => {
                let inner_ty = self.ast_type_to_ty(inner)?;
                let mutability = if *mutable {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                };
                Ok(Ty::Ref(Box::new(inner_ty), mutability))
            }
            AstTypeKind::Ptr(inner, mutable) => {
                let inner_ty = self.ast_type_to_ty(inner)?;
                let mutability = if *mutable {
                    Mutability::Mutable
                } else {
                    Mutability::Immutable
                };
                Ok(Ty::Ptr(Box::new(inner_ty), mutability))
            }
            AstTypeKind::Never => Ok(Ty::Never),
            AstTypeKind::Infer => Ok(Ty::fresh_var()),
        }
    }

    /// Get the final type after applying all substitutions.
    pub fn finalize_type(&self, ty: &Ty) -> Ty {
        ty.apply(&self.unifier.subst)
    }

    /// Get the current environment.
    pub fn env(&self) -> &TypeEnv {
        &self.env
    }

    /// Find a similar variable name for typo suggestions.
    fn find_similar_name(&self, name: &str) -> Option<String> {
        let mut best_match: Option<(String, usize)> = None;

        for existing in self.env.names() {
            let dist = Self::edit_distance(name, existing);
            // Only suggest if edit distance is small relative to name length
            if dist <= 2 && dist < name.len() / 2 + 1 {
                match &best_match {
                    None => best_match = Some((existing.to_string(), dist)),
                    Some((_, best_dist)) if dist < *best_dist => {
                        best_match = Some((existing.to_string(), dist))
                    }
                    _ => {}
                }
            }
        }

        best_match.map(|(name, _)| name)
    }

    /// Simple edit distance (Levenshtein distance) between two strings.
    fn edit_distance(a: &str, b: &str) -> usize {
        let a_chars: Vec<char> = a.chars().collect();
        let b_chars: Vec<char> = b.chars().collect();
        let m = a_chars.len();
        let n = b_chars.len();

        if m == 0 { return n; }
        if n == 0 { return m; }

        let mut dp = vec![vec![0; n + 1]; m + 1];
        for i in 0..=m { dp[i][0] = i; }
        for j in 0..=n { dp[0][j] = j; }

        for i in 1..=m {
            for j in 1..=n {
                let cost = if a_chars[i - 1] == b_chars[j - 1] { 0 } else { 1 };
                dp[i][j] = (dp[i - 1][j] + 1)
                    .min(dp[i][j - 1] + 1)
                    .min(dp[i - 1][j - 1] + cost);
            }
        }

        dp[m][n]
    }
}

impl Default for InferenceEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::types::reset_type_var_counter;

    #[test]
    fn test_unify_primitives() {
        let mut unifier = Unifier::new();
        let span = Span::new(0, 0, 0, 0);

        assert!(unifier.unify(&Ty::Int, &Ty::Int, span).is_ok());
        assert!(unifier.unify(&Ty::Bool, &Ty::Bool, span).is_ok());
        assert!(unifier.unify(&Ty::Int, &Ty::Bool, span).is_err());
    }

    #[test]
    fn test_unify_type_var() {
        reset_type_var_counter();
        let mut unifier = Unifier::new();
        let span = Span::new(0, 0, 0, 0);

        let var = TypeVar::fresh();
        assert!(unifier.unify(&Ty::Var(var), &Ty::Int, span).is_ok());

        let resolved = Ty::Var(var).apply(unifier.substitution());
        assert_eq!(resolved, Ty::Int);
    }

    #[test]
    fn test_unify_list() {
        reset_type_var_counter();
        let mut unifier = Unifier::new();
        let span = Span::new(0, 0, 0, 0);

        let var = TypeVar::fresh();
        let list1 = Ty::List(Box::new(Ty::Var(var)));
        let list2 = Ty::List(Box::new(Ty::Int));

        assert!(unifier.unify(&list1, &list2, span).is_ok());

        let resolved = Ty::Var(var).apply(unifier.substitution());
        assert_eq!(resolved, Ty::Int);
    }

    #[test]
    fn test_occurs_check() {
        reset_type_var_counter();
        let mut unifier = Unifier::new();
        let span = Span::new(0, 0, 0, 0);

        let var = TypeVar::fresh();
        let infinite = Ty::List(Box::new(Ty::Var(var)));

        // This should fail the occurs check
        assert!(unifier.unify(&Ty::Var(var), &infinite, span).is_err());
    }

    #[test]
    fn test_unify_function() {
        reset_type_var_counter();
        let mut unifier = Unifier::new();
        let span = Span::new(0, 0, 0, 0);

        let var = TypeVar::fresh();
        let fn1 = Ty::Fn(vec![Ty::Int], Box::new(Ty::Var(var)));
        let fn2 = Ty::Fn(vec![Ty::Int], Box::new(Ty::Bool));

        assert!(unifier.unify(&fn1, &fn2, span).is_ok());

        let resolved = Ty::Var(var).apply(unifier.substitution());
        assert_eq!(resolved, Ty::Bool);
    }
}
