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
    LiteralKind, PassMode, Pattern, PatternKind, Stmt, StmtKind, Type as AstType,
    TypeKind as AstTypeKind, UnaryOp, VariantKind, GenericArg,
};

use super::types::{Mutability, Substitution, Ty, TypeId, TypeScheme, TypeVar};

/// Reserved TypeVar IDs for method type substitution.
/// These are used to represent generic parameters in builtin method signatures.
pub mod reserved_type_vars {
    pub const ELEM_TYPE: u32 = u32::MAX;      // T (element type)
    pub const KEY_TYPE: u32 = u32::MAX - 1;   // K (key type)
    pub const VALUE_TYPE: u32 = u32::MAX - 2; // V (value type)
    pub const OPTION_T: u32 = u32::MAX - 3;   // Option's T
    pub const RESULT_T: u32 = u32::MAX - 4;   // Result's T
    pub const RESULT_E: u32 = u32::MAX - 5;   // Result's E
}

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

/// Function info for tracking default parameters.
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    /// Number of required parameters (those without defaults)
    pub required_params: usize,
    /// Total number of parameters
    pub total_params: usize,
    /// Types of all parameters (for filling in defaults)
    pub param_types: Vec<Ty>,
    /// Pass modes for each parameter (Owned, Ref, RefMut)
    pub param_pass_modes: Vec<PassMode>,
}

#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub name: String,
    pub type_params: Vec<String>,
    pub methods: Vec<TraitMethodInfo>,
}

#[derive(Debug, Clone)]
pub struct TraitMethodInfo {
    pub name: String,
    pub params: Vec<Ty>,
    pub return_type: Ty,
    pub has_default: bool,
}

/// Type environment mapping names to type schemes.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    /// Variable bindings
    bindings: HashMap<String, TypeScheme>,
    /// Type definitions (struct, enum names to their info)
    types: HashMap<String, TypeDef>,
    /// Function info (for tracking default parameters)
    fn_info: HashMap<String, FunctionInfo>,
    /// Trait definitions
    traits: HashMap<String, TraitInfo>,
    /// Maps variant names to their parent enum names (e.g., "Some" -> "Option")
    variant_to_enum: HashMap<String, String>,
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
                    ("Some".to_string(), vec![Ty::Var(TypeVar { id: reserved_type_vars::OPTION_T })]),
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
                    ("Ok".to_string(), vec![Ty::Var(TypeVar { id: reserved_type_vars::RESULT_T })]),
                    ("Err".to_string(), vec![Ty::Var(TypeVar { id: reserved_type_vars::RESULT_E })]),
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

        // Register variant-to-enum mappings for pattern matching
        env.variant_to_enum.insert("Some".to_string(), "Option".to_string());
        env.variant_to_enum.insert("None".to_string(), "Option".to_string());
        env.variant_to_enum.insert("Ok".to_string(), "Result".to_string());
        env.variant_to_enum.insert("Err".to_string(), "Result".to_string());

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

        // abs: Int -> Int
        env.bindings.insert(
            "abs".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // len: [T] -> Int (alias for vec_len)
        let len_t = TypeVar::fresh();
        env.bindings.insert(
            "len".to_string(),
            TypeScheme { vars: vec![len_t], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Var(len_t)))], Box::new(Ty::Int)) },
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

        // map_set: (Map, Str, V) -> () (mutating alias for map_insert used by LLVM backend)
        let map_set_v = TypeVar::fresh();
        env.bindings.insert(
            "map_set".to_string(),
            TypeScheme {
                vars: vec![map_set_v],
                ty: Ty::Fn(
                    vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_set_v)]), Ty::Str, Ty::Var(map_set_v)],
                    Box::new(Ty::Unit)
                )
            },
        );

        // map_free: Map -> ()
        let map_free_v = TypeVar::fresh();
        env.bindings.insert(
            "map_free".to_string(),
            TypeScheme {
                vars: vec![map_free_v],
                ty: Ty::Fn(
                    vec![Ty::Named(TypeId::new("Map"), vec![Ty::Var(map_free_v)])],
                    Box::new(Ty::Unit)
                )
            },
        );

        // vec_free: [T] -> ()
        let vec_free_v = TypeVar::fresh();
        env.bindings.insert(
            "vec_free".to_string(),
            TypeScheme {
                vars: vec![vec_free_v],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Var(vec_free_v)))],
                    Box::new(Ty::Unit)
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

        // sleep_ms(Int) -> () (alias for time_sleep)
        env.bindings.insert(
            "sleep_ms".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Unit)) },
        );

        // args_count() -> Int
        env.bindings.insert(
            "args_count".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Int)) },
        );

        // args_get(Int) -> Str
        env.bindings.insert(
            "args_get".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Str)) },
        );

        // env_set(Str, Str) -> ()
        env.bindings.insert(
            "env_set".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Unit)) },
        );

        // ===== Duration functions =====
        // duration_seconds(Int) -> Int (returns milliseconds)
        env.bindings.insert(
            "duration_seconds".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );
        // duration_minutes(Int) -> Int (returns milliseconds)
        env.bindings.insert(
            "duration_minutes".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );
        // duration_hours(Int) -> Int (returns milliseconds)
        env.bindings.insert(
            "duration_hours".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );
        // duration_days(Int) -> Int (returns milliseconds)
        env.bindings.insert(
            "duration_days".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // ===== Async functions =====
        // sleep_async(Int) -> Future[()]
        env.bindings.insert(
            "sleep_async".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Future(Box::new(Ty::Unit)))) },
        );

        // timeout(Int, Future[T]) -> Result[T, Str]
        let t = TypeVar::fresh();
        env.bindings.insert(
            "timeout".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Int, Ty::Future(Box::new(Ty::Var(t)))],
                    Box::new(Ty::Result(Box::new(Ty::Var(t)), Box::new(Ty::Str)))
                )
            },
        );

        // await_all([Task[T]]) -> [T]
        let t = TypeVar::fresh();
        env.bindings.insert(
            "await_all".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Task(Box::new(Ty::Var(t)))))],
                    Box::new(Ty::List(Box::new(Ty::Var(t))))
                )
            },
        );

        // await_any([Task[T]]) -> T
        let t = TypeVar::fresh();
        env.bindings.insert(
            "await_any".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::List(Box::new(Ty::Task(Box::new(Ty::Var(t)))))],
                    Box::new(Ty::Var(t))
                )
            },
        );

        // ===== Channel functions =====
        // channel_new(Int) -> (Sender[T], Receiver[T])
        let t = TypeVar::fresh();
        env.bindings.insert(
            "channel_new".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Int],
                    Box::new(Ty::Tuple(vec![
                        Ty::Sender(Box::new(Ty::Var(t))),
                        Ty::Receiver(Box::new(Ty::Var(t)))
                    ]))
                )
            },
        );

        // channel_send(Sender[T], T) -> Result[(), Str]
        let t = TypeVar::fresh();
        env.bindings.insert(
            "channel_send".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Sender(Box::new(Ty::Var(t))), Ty::Var(t)],
                    Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))
                )
            },
        );

        // channel_recv(Receiver[T]) -> Result[T, Str]
        let t = TypeVar::fresh();
        env.bindings.insert(
            "channel_recv".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Receiver(Box::new(Ty::Var(t)))],
                    Box::new(Ty::Result(Box::new(Ty::Var(t)), Box::new(Ty::Str)))
                )
            },
        );

        // channel_try_send(Sender[T], T) -> Bool
        let t = TypeVar::fresh();
        env.bindings.insert(
            "channel_try_send".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Sender(Box::new(Ty::Var(t))), Ty::Var(t)],
                    Box::new(Ty::Bool)
                )
            },
        );

        // channel_try_recv(Receiver[T]) -> T?
        let t = TypeVar::fresh();
        env.bindings.insert(
            "channel_try_recv".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Receiver(Box::new(Ty::Var(t)))],
                    Box::new(Ty::Option(Box::new(Ty::Var(t))))
                )
            },
        );

        // channel_close(Sender[T]) -> ()
        let t = TypeVar::fresh();
        env.bindings.insert(
            "channel_close".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Sender(Box::new(Ty::Var(t)))],
                    Box::new(Ty::Unit)
                )
            },
        );

        // ===== Mutex functions =====
        // mutex_new(T) -> Mutex[T]
        let t = TypeVar::fresh();
        env.bindings.insert(
            "mutex_new".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Var(t)],
                    Box::new(Ty::Mutex(Box::new(Ty::Var(t))))
                )
            },
        );

        // mutex_lock(Mutex[T]) -> MutexGuard[T]
        let t = TypeVar::fresh();
        env.bindings.insert(
            "mutex_lock".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Mutex(Box::new(Ty::Var(t)))],
                    Box::new(Ty::MutexGuard(Box::new(Ty::Var(t))))
                )
            },
        );

        // mutex_try_lock(Mutex[T]) -> MutexGuard[T]?
        let t = TypeVar::fresh();
        env.bindings.insert(
            "mutex_try_lock".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::Mutex(Box::new(Ty::Var(t)))],
                    Box::new(Ty::Option(Box::new(Ty::MutexGuard(Box::new(Ty::Var(t))))))
                )
            },
        );

        // mutex_unlock(MutexGuard[T]) -> ()
        let t = TypeVar::fresh();
        env.bindings.insert(
            "mutex_unlock".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::MutexGuard(Box::new(Ty::Var(t)))],
                    Box::new(Ty::Unit)
                )
            },
        );

        // mutex_get(MutexGuard[T]) -> T
        let t = TypeVar::fresh();
        env.bindings.insert(
            "mutex_get".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::MutexGuard(Box::new(Ty::Var(t)))],
                    Box::new(Ty::Var(t))
                )
            },
        );

        // mutex_set(MutexGuard[T], T) -> ()
        let t = TypeVar::fresh();
        env.bindings.insert(
            "mutex_set".to_string(),
            TypeScheme {
                vars: vec![t],
                ty: Ty::Fn(
                    vec![Ty::MutexGuard(Box::new(Ty::Var(t))), Ty::Var(t)],
                    Box::new(Ty::Unit)
                )
            },
        );

        // ===== JSON functions =====
        // json_parse: Str -> Result[Json, Str]
        env.bindings.insert(
            "json_parse".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Json), Box::new(Ty::Str)))) },
        );

        // json_stringify: Json -> Str
        env.bindings.insert(
            "json_stringify".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Str)) },
        );

        // json_stringify_pretty: Json -> Str
        env.bindings.insert(
            "json_stringify_pretty".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Str)) },
        );

        // json_get: (Json, Str) -> Json?
        env.bindings.insert(
            "json_get".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Str], Box::new(Ty::Option(Box::new(Ty::Json)))) },
        );

        // json_get_str: (Json, Str) -> Str?
        env.bindings.insert(
            "json_get_str".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Str], Box::new(Ty::Option(Box::new(Ty::Str)))) },
        );

        // json_get_int: (Json, Str) -> Int?
        env.bindings.insert(
            "json_get_int".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Str], Box::new(Ty::Option(Box::new(Ty::Int)))) },
        );

        // json_get_float: (Json, Str) -> Float?
        env.bindings.insert(
            "json_get_float".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Str], Box::new(Ty::Option(Box::new(Ty::Float)))) },
        );

        // json_get_bool: (Json, Str) -> Bool?
        env.bindings.insert(
            "json_get_bool".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Str], Box::new(Ty::Option(Box::new(Ty::Bool)))) },
        );

        // json_get_array: (Json, Str) -> [Json]?
        env.bindings.insert(
            "json_get_array".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Str], Box::new(Ty::Option(Box::new(Ty::List(Box::new(Ty::Json)))))) },
        );

        // json_array_get: (Json, Int) -> Json?
        env.bindings.insert(
            "json_array_get".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Int], Box::new(Ty::Option(Box::new(Ty::Json)))) },
        );

        // json_array_len: Json -> Int
        env.bindings.insert(
            "json_array_len".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Int)) },
        );

        // json_keys: Json -> [Str]
        env.bindings.insert(
            "json_keys".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::List(Box::new(Ty::Str)))) },
        );

        // json_values: Json -> [Json]
        env.bindings.insert(
            "json_values".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::List(Box::new(Ty::Json)))) },
        );

        // json_has: (Json, Str) -> Bool
        env.bindings.insert(
            "json_has".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Str], Box::new(Ty::Bool)) },
        );

        // json_set: (Json, Str, Json) -> Json
        env.bindings.insert(
            "json_set".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json, Ty::Str, Ty::Json], Box::new(Ty::Json)) },
        );

        // json_type: Json -> Str
        env.bindings.insert(
            "json_type".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Str)) },
        );

        // json_is_null: Json -> Bool
        env.bindings.insert(
            "json_is_null".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Bool)) },
        );

        // json_is_bool: Json -> Bool
        env.bindings.insert(
            "json_is_bool".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Bool)) },
        );

        // json_is_number: Json -> Bool
        env.bindings.insert(
            "json_is_number".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Bool)) },
        );

        // json_is_string: Json -> Bool
        env.bindings.insert(
            "json_is_string".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Bool)) },
        );

        // json_is_array: Json -> Bool
        env.bindings.insert(
            "json_is_array".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Bool)) },
        );

        // json_is_object: Json -> Bool
        env.bindings.insert(
            "json_is_object".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Bool)) },
        );

        // json_from_str: Str -> Json
        env.bindings.insert(
            "json_from_str".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Json)) },
        );

        // json_from_int: Int -> Json
        env.bindings.insert(
            "json_from_int".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Json)) },
        );

        // json_from_float: Float -> Json
        env.bindings.insert(
            "json_from_float".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::Json)) },
        );

        // json_from_bool: Bool -> Json
        env.bindings.insert(
            "json_from_bool".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Bool], Box::new(Ty::Json)) },
        );

        // json_null: () -> Json
        env.bindings.insert(
            "json_null".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Json)) },
        );

        // json_object: () -> Json
        env.bindings.insert(
            "json_object".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Json)) },
        );

        // json_array: () -> Json
        env.bindings.insert(
            "json_array".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Json)) },
        );

        // json_to_value: Json -> T (returns dynamic value)
        let json_to_value_t = TypeVar::fresh();
        env.bindings.insert(
            "json_to_value".to_string(),
            TypeScheme { vars: vec![json_to_value_t], ty: Ty::Fn(vec![Ty::Json], Box::new(Ty::Var(json_to_value_t))) },
        );

        // ===== Sorting functions =====
        // sort_ints: [Int] -> [Int]
        env.bindings.insert(
            "sort_ints".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::List(Box::new(Ty::Int)))) },
        );

        // sort_ints_desc: [Int] -> [Int]
        env.bindings.insert(
            "sort_ints_desc".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::List(Box::new(Ty::Int)))) },
        );

        // sort_floats: [Float] -> [Float]
        env.bindings.insert(
            "sort_floats".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Float))], Box::new(Ty::List(Box::new(Ty::Float)))) },
        );

        // sort_floats_desc: [Float] -> [Float]
        env.bindings.insert(
            "sort_floats_desc".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Float))], Box::new(Ty::List(Box::new(Ty::Float)))) },
        );

        // sort_strings: [Str] -> [Str]
        env.bindings.insert(
            "sort_strings".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Str))], Box::new(Ty::List(Box::new(Ty::Str)))) },
        );

        // sort_strings_desc: [Str] -> [Str]
        env.bindings.insert(
            "sort_strings_desc".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Str))], Box::new(Ty::List(Box::new(Ty::Str)))) },
        );

        // reverse: [T] -> [T]
        let reverse_t = TypeVar::fresh();
        env.bindings.insert(
            "reverse".to_string(),
            TypeScheme { vars: vec![reverse_t], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Var(reverse_t)))], Box::new(Ty::List(Box::new(Ty::Var(reverse_t))))) },
        );

        // shuffle: [T] -> [T]
        let shuffle_t = TypeVar::fresh();
        env.bindings.insert(
            "shuffle".to_string(),
            TypeScheme { vars: vec![shuffle_t], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Var(shuffle_t)))], Box::new(Ty::List(Box::new(Ty::Var(shuffle_t))))) },
        );

        // min_of: [Int] -> Int?
        env.bindings.insert(
            "min_of".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::Option(Box::new(Ty::Int)))) },
        );

        // max_of: [Int] -> Int?
        env.bindings.insert(
            "max_of".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::Option(Box::new(Ty::Int)))) },
        );

        // sum_of: [Int] -> Int
        env.bindings.insert(
            "sum_of".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::Int)) },
        );

        // binary_search: ([Int], Int) -> Int?
        env.bindings.insert(
            "binary_search".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int)), Ty::Int], Box::new(Ty::Option(Box::new(Ty::Int)))) },
        );

        // ===== DateTime functions =====
        // time_from_parts: (Int, Int, Int, Int, Int, Int) -> Int
        env.bindings.insert(
            "time_from_parts".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int, Ty::Int, Ty::Int, Ty::Int, Ty::Int, Ty::Int], Box::new(Ty::Int)) },
        );

        // time_format: (Int, Str) -> Str
        env.bindings.insert(
            "time_format".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int, Ty::Str], Box::new(Ty::Str)) },
        );

        // time_format_iso: Int -> Str
        env.bindings.insert(
            "time_format_iso".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Str)) },
        );

        // time_format_rfc2822: Int -> Str
        env.bindings.insert(
            "time_format_rfc2822".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Str)) },
        );

        // time_parse: (Str, Str) -> Result[Int, Str]
        env.bindings.insert(
            "time_parse".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );

        // time_parse_iso: Str -> Result[Int, Str]
        env.bindings.insert(
            "time_parse_iso".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );

        // time_year: Int -> Int
        env.bindings.insert(
            "time_year".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // time_month: Int -> Int
        env.bindings.insert(
            "time_month".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // time_day: Int -> Int
        env.bindings.insert(
            "time_day".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // time_hour: Int -> Int
        env.bindings.insert(
            "time_hour".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // time_minute: Int -> Int
        env.bindings.insert(
            "time_minute".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // time_second: Int -> Int
        env.bindings.insert(
            "time_second".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // time_weekday: Int -> Int
        env.bindings.insert(
            "time_weekday".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // duration_seconds: Int -> Int
        env.bindings.insert(
            "duration_seconds".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // duration_minutes: Int -> Int
        env.bindings.insert(
            "duration_minutes".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // duration_hours: Int -> Int
        env.bindings.insert(
            "duration_hours".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // duration_days: Int -> Int
        env.bindings.insert(
            "duration_days".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::Int)) },
        );

        // time_add: (Int, Int) -> Int
        env.bindings.insert(
            "time_add".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int, Ty::Int], Box::new(Ty::Int)) },
        );

        // time_sub: (Int, Int) -> Int
        env.bindings.insert(
            "time_sub".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int, Ty::Int], Box::new(Ty::Int)) },
        );

        // time_diff: (Int, Int) -> Int
        env.bindings.insert(
            "time_diff".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int, Ty::Int], Box::new(Ty::Int)) },
        );

        // ===== Encoding functions =====
        // base64_encode: Str -> Str
        env.bindings.insert(
            "base64_encode".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Str)) },
        );

        // base64_decode: Str -> Result[Str, Str]
        env.bindings.insert(
            "base64_decode".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // base64_encode_bytes: [Int] -> Str
        env.bindings.insert(
            "base64_encode_bytes".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::Str)) },
        );

        // base64_decode_bytes: Str -> Result[[Int], Str]
        env.bindings.insert(
            "base64_decode_bytes".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::List(Box::new(Ty::Int))), Box::new(Ty::Str)))) },
        );

        // hex_encode: Str -> Str
        env.bindings.insert(
            "hex_encode".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Str)) },
        );

        // hex_decode: Str -> Result[Str, Str]
        env.bindings.insert(
            "hex_decode".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // hex_encode_bytes: [Int] -> Str
        env.bindings.insert(
            "hex_encode_bytes".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::Str)) },
        );

        // hex_decode_bytes: Str -> Result[[Int], Str]
        env.bindings.insert(
            "hex_decode_bytes".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::List(Box::new(Ty::Int))), Box::new(Ty::Str)))) },
        );

        // ===== Hashing functions =====
        // sha256: Str -> Str
        env.bindings.insert(
            "sha256".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Str)) },
        );

        // sha256_bytes: [Int] -> Str
        env.bindings.insert(
            "sha256_bytes".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::Str)) },
        );

        // hash_string: Str -> Int
        env.bindings.insert(
            "hash_string".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Int)) },
        );

        // ===== UUID functions =====
        // uuid_v4: () -> Str
        env.bindings.insert(
            "uuid_v4".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Str)) },
        );

        // uuid_parse: Str -> Result[Str, Str]
        env.bindings.insert(
            "uuid_parse".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // ===== Regex functions =====
        // regex_match: (Str, Str) -> Bool
        env.bindings.insert(
            "regex_match".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Bool)) },
        );

        // regex_find: (Str, Str) -> Str?
        env.bindings.insert(
            "regex_find".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Option(Box::new(Ty::Str)))) },
        );

        // regex_find_all: (Str, Str) -> [Str]
        env.bindings.insert(
            "regex_find_all".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::List(Box::new(Ty::Str)))) },
        );

        // regex_replace: (Str, Str, Str) -> Str
        env.bindings.insert(
            "regex_replace".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str, Ty::Str], Box::new(Ty::Str)) },
        );

        // regex_replace_all: (Str, Str, Str) -> Str
        env.bindings.insert(
            "regex_replace_all".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str, Ty::Str], Box::new(Ty::Str)) },
        );

        // regex_split: (Str, Str) -> [Str]
        env.bindings.insert(
            "regex_split".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::List(Box::new(Ty::Str)))) },
        );

        // regex_captures: (Str, Str) -> [Str]?
        env.bindings.insert(
            "regex_captures".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Option(Box::new(Ty::List(Box::new(Ty::Str)))))) },
        );

        // regex_is_valid: Str -> Bool
        env.bindings.insert(
            "regex_is_valid".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Bool)) },
        );

        // ===== Process functions =====
        // exec: Str -> Result[(Str, Str, Int), Str]
        env.bindings.insert(
            "exec".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Tuple(vec![Ty::Str, Ty::Str, Ty::Int])), Box::new(Ty::Str)))) },
        );

        // env_set: (Str, Str) -> ()
        env.bindings.insert(
            "env_set".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Unit)) },
        );

        // env_remove: Str -> ()
        env.bindings.insert(
            "env_remove".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Unit)) },
        );

        // env_vars: () -> {Str: Str}
        env.bindings.insert(
            "env_vars".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // pid: () -> Int
        env.bindings.insert(
            "pid".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Int)) },
        );

        // cwd: () -> Str
        env.bindings.insert(
            "cwd".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Str)) },
        );

        // chdir: Str -> Result[(), Str]
        env.bindings.insert(
            "chdir".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // home_dir: () -> Str?
        env.bindings.insert(
            "home_dir".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Option(Box::new(Ty::Str)))) },
        );

        // temp_dir: () -> Str
        env.bindings.insert(
            "temp_dir".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Str)) },
        );

        // ===== Path functions =====
        // path_join: [Str] -> Str
        env.bindings.insert(
            "path_join".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Str))], Box::new(Ty::Str)) },
        );

        // path_parent: Str -> Str?
        env.bindings.insert(
            "path_parent".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Option(Box::new(Ty::Str)))) },
        );

        // path_filename: Str -> Str?
        env.bindings.insert(
            "path_filename".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Option(Box::new(Ty::Str)))) },
        );

        // path_stem: Str -> Str?
        env.bindings.insert(
            "path_stem".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Option(Box::new(Ty::Str)))) },
        );

        // path_extension: Str -> Str?
        env.bindings.insert(
            "path_extension".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Option(Box::new(Ty::Str)))) },
        );

        // path_is_absolute: Str -> Bool
        env.bindings.insert(
            "path_is_absolute".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Bool)) },
        );

        // path_is_relative: Str -> Bool
        env.bindings.insert(
            "path_is_relative".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Bool)) },
        );

        // path_absolute: Str -> Result[Str, Str]
        env.bindings.insert(
            "path_absolute".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // file_is_file: Str -> Bool
        env.bindings.insert(
            "file_is_file".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Bool)) },
        );

        // file_is_dir: Str -> Bool
        env.bindings.insert(
            "file_is_dir".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Bool)) },
        );

        // file_size: Str -> Result[Int, Str]
        env.bindings.insert(
            "file_size".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );

        // dir_create: Str -> Result[(), Str]
        env.bindings.insert(
            "dir_create".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // dir_create_all: Str -> Result[(), Str]
        env.bindings.insert(
            "dir_create_all".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // dir_remove: Str -> Result[(), Str]
        env.bindings.insert(
            "dir_remove".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // dir_remove_all: Str -> Result[(), Str]
        env.bindings.insert(
            "dir_remove_all".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // dir_list: Str -> Result[[Str], Str]
        env.bindings.insert(
            "dir_list".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::List(Box::new(Ty::Str))), Box::new(Ty::Str)))) },
        );

        // file_copy: (Str, Str) -> Result[(), Str]
        env.bindings.insert(
            "file_copy".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // file_move: (Str, Str) -> Result[(), Str]
        env.bindings.insert(
            "file_move".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // file_remove: Str -> Result[(), Str]
        env.bindings.insert(
            "file_remove".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // ===== HTTP functions =====
        // http_get: Str -> Result[(Int, Str, {Str: Str}), Str]
        env.bindings.insert(
            "http_get".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Tuple(vec![Ty::Int, Ty::Str, Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))])), Box::new(Ty::Str)))) },
        );

        // http_post: (Str, Str) -> Result[(Int, Str, {Str: Str}), Str]
        env.bindings.insert(
            "http_post".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Tuple(vec![Ty::Int, Ty::Str, Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))])), Box::new(Ty::Str)))) },
        );

        // http_post_json: (Str, Json) -> Result[(Int, Str, {Str: Str}), Str]
        env.bindings.insert(
            "http_post_json".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Json], Box::new(Ty::Result(Box::new(Ty::Tuple(vec![Ty::Int, Ty::Str, Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))])), Box::new(Ty::Str)))) },
        );

        // http_put: (Str, Str) -> Result[(Int, Str, {Str: Str}), Str]
        env.bindings.insert(
            "http_put".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Tuple(vec![Ty::Int, Ty::Str, Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))])), Box::new(Ty::Str)))) },
        );

        // http_delete: Str -> Result[(Int, Str, {Str: Str}), Str]
        env.bindings.insert(
            "http_delete".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Tuple(vec![Ty::Int, Ty::Str, Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))])), Box::new(Ty::Str)))) },
        );

        // ===== HTTP Server functions =====
        // HttpRequest type: Named struct { method: Str, path: Str, query: {Str: Str}, headers: {Str: Str}, body: Str }
        // HttpResponse type: Named struct { status: Int, headers: {Str: Str}, body: Str }

        // Register HttpRequest struct type
        env.types.insert(
            "HttpRequest".to_string(),
            TypeDef::Struct {
                type_params: vec![],
                fields: vec![
                    ("method".to_string(), Ty::Str),
                    ("path".to_string(), Ty::Str),
                    ("query".to_string(), Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))),
                    ("headers".to_string(), Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))),
                    ("body".to_string(), Ty::Str),
                ],
            },
        );

        // Register HttpResponse struct type
        env.types.insert(
            "HttpResponse".to_string(),
            TypeDef::Struct {
                type_params: vec![],
                fields: vec![
                    ("status".to_string(), Ty::Int),
                    ("headers".to_string(), Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))),
                    ("body".to_string(), Ty::Str),
                ],
            },
        );

        // http_response: (Int, Str) -> HttpResponse
        env.bindings.insert(
            "http_response".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Int, Ty::Str],
                    Box::new(Ty::Named(
                        crate::types::TypeId { name: "HttpResponse".to_string(), module: None },
                        vec![]
                    ))
                )
            },
        );

        // http_response_with_headers: (Int, Str, Map[Str, Str]) -> HttpResponse
        env.bindings.insert(
            "http_response_with_headers".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Int, Ty::Str, Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str))],
                    Box::new(Ty::Named(
                        crate::types::TypeId { name: "HttpResponse".to_string(), module: None },
                        vec![]
                    ))
                )
            },
        );

        // http_json_response: (Int, Json) -> HttpResponse
        env.bindings.insert(
            "http_json_response".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Int, Ty::Json],
                    Box::new(Ty::Named(
                        crate::types::TypeId { name: "HttpResponse".to_string(), module: None },
                        vec![]
                    ))
                )
            },
        );

        // http_redirect: Str -> HttpResponse
        env.bindings.insert(
            "http_redirect".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Str],
                    Box::new(Ty::Named(
                        crate::types::TypeId { name: "HttpResponse".to_string(), module: None },
                        vec![]
                    ))
                )
            },
        );

        // http_file_response: Str -> Result[HttpResponse, Str]
        env.bindings.insert(
            "http_file_response".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Str],
                    Box::new(Ty::Result(
                        Box::new(Ty::Named(
                            crate::types::TypeId { name: "HttpResponse".to_string(), module: None },
                            vec![]
                        )),
                        Box::new(Ty::Str)
                    ))
                )
            },
        );

        // http_req_json: HttpRequest -> Result[Json, Str]
        env.bindings.insert(
            "http_req_json".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Named(
                        crate::types::TypeId { name: "HttpRequest".to_string(), module: None },
                        vec![]
                    )],
                    Box::new(Ty::Result(Box::new(Ty::Json), Box::new(Ty::Str)))
                )
            },
        );

        // http_req_form: HttpRequest -> {Str: Str}
        env.bindings.insert(
            "http_req_form".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Named(
                        crate::types::TypeId { name: "HttpRequest".to_string(), module: None },
                        vec![]
                    )],
                    Box::new(Ty::Map(Box::new(Ty::Str), Box::new(Ty::Str)))
                )
            },
        );

        // http_req_param: (HttpRequest, Str) -> Str?
        env.bindings.insert(
            "http_req_param".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![
                        Ty::Named(
                            crate::types::TypeId { name: "HttpRequest".to_string(), module: None },
                            vec![]
                        ),
                        Ty::Str
                    ],
                    Box::new(Ty::Option(Box::new(Ty::Str)))
                )
            },
        );

        // http_req_header: (HttpRequest, Str) -> Str?
        env.bindings.insert(
            "http_req_header".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![
                        Ty::Named(
                            crate::types::TypeId { name: "HttpRequest".to_string(), module: None },
                            vec![]
                        ),
                        Ty::Str
                    ],
                    Box::new(Ty::Option(Box::new(Ty::Str)))
                )
            },
        );

        // http_serve: (Int, Fn(HttpRequest) -> HttpResponse) -> Result[(), Str]
        let handler_t = TypeVar::fresh();
        env.bindings.insert(
            "http_serve".to_string(),
            TypeScheme {
                vars: vec![handler_t],
                ty: Ty::Fn(
                    vec![
                        Ty::Int,
                        Ty::Fn(
                            vec![Ty::Named(
                                crate::types::TypeId { name: "HttpRequest".to_string(), module: None },
                                vec![]
                            )],
                            Box::new(Ty::Named(
                                crate::types::TypeId { name: "HttpResponse".to_string(), module: None },
                                vec![]
                            ))
                        )
                    ],
                    Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))
                )
            },
        );

        // http_request_new: (Str, Str, Str) -> HttpRequest (for testing)
        env.bindings.insert(
            "http_request_new".to_string(),
            TypeScheme {
                vars: vec![],
                ty: Ty::Fn(
                    vec![Ty::Str, Ty::Str, Ty::Str],
                    Box::new(Ty::Named(
                        crate::types::TypeId { name: "HttpRequest".to_string(), module: None },
                        vec![]
                    ))
                )
            },
        );

        // ===== TCP/UDP Socket functions =====

        // tcp_connect: (Str, Int) -> Result[TcpStream, Str]
        env.bindings.insert(
            "tcp_connect".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Int], Box::new(Ty::Result(Box::new(Ty::TcpStream), Box::new(Ty::Str)))) },
        );

        // tcp_read: (TcpStream, Int) -> Result[Str, Str]
        env.bindings.insert(
            "tcp_read".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream, Ty::Int], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // tcp_read_exact: (TcpStream, Int) -> Result[Str, Str]
        env.bindings.insert(
            "tcp_read_exact".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream, Ty::Int], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // tcp_read_line: TcpStream -> Result[Str, Str]
        env.bindings.insert(
            "tcp_read_line".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // tcp_write: (TcpStream, Str) -> Result[Int, Str]
        env.bindings.insert(
            "tcp_write".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );

        // tcp_write_all: (TcpStream, Str) -> Result[(), Str]
        env.bindings.insert(
            "tcp_write_all".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // tcp_close: TcpStream -> ()
        env.bindings.insert(
            "tcp_close".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream], Box::new(Ty::Unit)) },
        );

        // tcp_set_timeout: (TcpStream, Int) -> ()
        env.bindings.insert(
            "tcp_set_timeout".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream, Ty::Int], Box::new(Ty::Unit)) },
        );

        // tcp_peer_addr: TcpStream -> Str
        env.bindings.insert(
            "tcp_peer_addr".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream], Box::new(Ty::Str)) },
        );

        // tcp_local_addr: TcpStream -> Str
        env.bindings.insert(
            "tcp_local_addr".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpStream], Box::new(Ty::Str)) },
        );

        // tcp_listen: (Str, Int) -> Result[TcpListener, Str]
        env.bindings.insert(
            "tcp_listen".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Int], Box::new(Ty::Result(Box::new(Ty::TcpListener), Box::new(Ty::Str)))) },
        );

        // tcp_accept: TcpListener -> Result[TcpStream, Str]
        env.bindings.insert(
            "tcp_accept".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpListener], Box::new(Ty::Result(Box::new(Ty::TcpStream), Box::new(Ty::Str)))) },
        );

        // tcp_listener_close: TcpListener -> ()
        env.bindings.insert(
            "tcp_listener_close".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TcpListener], Box::new(Ty::Unit)) },
        );

        // UDP functions
        // udp_bind: (Str, Int) -> Result[UdpSocket, Str]
        env.bindings.insert(
            "udp_bind".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Int], Box::new(Ty::Result(Box::new(Ty::UdpSocket), Box::new(Ty::Str)))) },
        );

        // udp_send_to: (UdpSocket, Str, Int, Str) -> Result[Int, Str]
        env.bindings.insert(
            "udp_send_to".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::UdpSocket, Ty::Str, Ty::Int, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );

        // udp_recv_from: (UdpSocket, Int) -> Result[(Str, Str, Int), Str]
        env.bindings.insert(
            "udp_recv_from".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::UdpSocket, Ty::Int], Box::new(Ty::Result(Box::new(Ty::Tuple(vec![Ty::Str, Ty::Str, Ty::Int])), Box::new(Ty::Str)))) },
        );

        // udp_close: UdpSocket -> ()
        env.bindings.insert(
            "udp_close".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::UdpSocket], Box::new(Ty::Unit)) },
        );

        // udp_connect: (UdpSocket, Str, Int) -> Result[(), Str]
        env.bindings.insert(
            "udp_connect".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::UdpSocket, Ty::Str, Ty::Int], Box::new(Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)))) },
        );

        // udp_send: (UdpSocket, Str) -> Result[Int, Str]
        env.bindings.insert(
            "udp_send".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::UdpSocket, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );

        // udp_recv: (UdpSocket, Int) -> Result[Str, Str]
        env.bindings.insert(
            "udp_recv".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::UdpSocket, Ty::Int], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // DNS functions
        // dns_lookup: Str -> Result[[Str], Str]
        env.bindings.insert(
            "dns_lookup".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::List(Box::new(Ty::Str))), Box::new(Ty::Str)))) },
        );

        // dns_reverse_lookup: Str -> Result[Str, Str]
        env.bindings.insert(
            "dns_reverse_lookup".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // ===== C FFI functions =====

        // Pointer operations
        // ptr_null: () -> *Void
        env.bindings.insert(
            "ptr_null".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::CVoid)) },
        );

        // ptr_is_null: *Void -> Bool
        env.bindings.insert(
            "ptr_is_null".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid], Box::new(Ty::Bool)) },
        );

        // ptr_offset: (*Void, Int) -> *Void
        env.bindings.insert(
            "ptr_offset".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid, Ty::Int], Box::new(Ty::CVoid)) },
        );

        // ptr_addr: *Void -> Int
        env.bindings.insert(
            "ptr_addr".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid], Box::new(Ty::Int)) },
        );

        // ptr_from_addr: Int -> *Void
        env.bindings.insert(
            "ptr_from_addr".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::CVoid)) },
        );

        // String conversion
        // str_to_cstr: Str -> *Void
        env.bindings.insert(
            "str_to_cstr".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::CVoid)) },
        );

        // cstr_to_str: *Void -> Str
        env.bindings.insert(
            "cstr_to_str".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid], Box::new(Ty::Str)) },
        );

        // cstr_to_str_len: (*Void, Int) -> Str
        env.bindings.insert(
            "cstr_to_str_len".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid, Ty::Int], Box::new(Ty::Str)) },
        );

        // cstr_free: *Void -> ()
        env.bindings.insert(
            "cstr_free".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid], Box::new(Ty::Unit)) },
        );

        // Memory allocation
        // alloc: Int -> *Void
        env.bindings.insert(
            "alloc".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::CVoid)) },
        );

        // alloc_zeroed: Int -> *Void
        env.bindings.insert(
            "alloc_zeroed".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::CVoid)) },
        );

        // dealloc: (*Void, Int) -> ()
        env.bindings.insert(
            "dealloc".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid, Ty::Int], Box::new(Ty::Unit)) },
        );

        // mem_copy: (*Void, *Void, Int) -> ()
        env.bindings.insert(
            "mem_copy".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid, Ty::CVoid, Ty::Int], Box::new(Ty::Unit)) },
        );

        // mem_set: (*Void, Int, Int) -> ()
        env.bindings.insert(
            "mem_set".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CVoid, Ty::Int, Ty::Int], Box::new(Ty::Unit)) },
        );

        // C type conversions
        // to_cint: Int -> CInt
        env.bindings.insert(
            "to_cint".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::CInt)) },
        );

        // from_cint: CInt -> Int
        env.bindings.insert(
            "from_cint".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CInt], Box::new(Ty::Int)) },
        );

        // to_cuint: Int -> CUInt
        env.bindings.insert(
            "to_cuint".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::CUInt)) },
        );

        // from_cuint: CUInt -> Int
        env.bindings.insert(
            "from_cuint".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CUInt], Box::new(Ty::Int)) },
        );

        // to_clong: Int -> CLong
        env.bindings.insert(
            "to_clong".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::CLong)) },
        );

        // from_clong: CLong -> Int
        env.bindings.insert(
            "from_clong".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CLong], Box::new(Ty::Int)) },
        );

        // to_culong: Int -> CULong
        env.bindings.insert(
            "to_culong".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::CULong)) },
        );

        // from_culong: CULong -> Int
        env.bindings.insert(
            "from_culong".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CULong], Box::new(Ty::Int)) },
        );

        // to_cfloat: Float -> CFloat
        env.bindings.insert(
            "to_cfloat".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::CFloat)) },
        );

        // from_cfloat: CFloat -> Float
        env.bindings.insert(
            "from_cfloat".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CFloat], Box::new(Ty::Float)) },
        );

        // to_cdouble: Float -> CDouble
        env.bindings.insert(
            "to_cdouble".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Float], Box::new(Ty::CDouble)) },
        );

        // from_cdouble: CDouble -> Float
        env.bindings.insert(
            "from_cdouble".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CDouble], Box::new(Ty::Float)) },
        );

        // to_csize: Int -> CSize
        env.bindings.insert(
            "to_csize".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Int], Box::new(Ty::CSize)) },
        );

        // from_csize: CSize -> Int
        env.bindings.insert(
            "from_csize".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::CSize], Box::new(Ty::Int)) },
        );

        // sizeof: Str -> Int
        env.bindings.insert(
            "sizeof".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Int)) },
        );

        // ===== Logging builtins =====
        // log_debug: Str -> ()
        env.bindings.insert(
            "log_debug".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Unit)) },
        );
        // log_info: Str -> ()
        env.bindings.insert(
            "log_info".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Unit)) },
        );
        // log_warn: Str -> ()
        env.bindings.insert(
            "log_warn".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Unit)) },
        );
        // log_error: Str -> ()
        env.bindings.insert(
            "log_error".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Unit)) },
        );
        // log_set_level: Str -> ()
        env.bindings.insert(
            "log_set_level".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Unit)) },
        );
        // log_set_format: Str -> ()
        env.bindings.insert(
            "log_set_format".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Unit)) },
        );

        // ===== TLS builtins =====
        // tls_connect: (Str, Int) -> Result[TlsStream, Str]
        env.bindings.insert(
            "tls_connect".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str, Ty::Int], Box::new(Ty::Result(Box::new(Ty::TlsStream), Box::new(Ty::Str)))) },
        );
        // tls_read: (TlsStream, Int) -> Result[Str, Str]
        env.bindings.insert(
            "tls_read".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TlsStream, Ty::Int], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );
        // tls_write: (TlsStream, Str) -> Result[Int, Str]
        env.bindings.insert(
            "tls_write".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TlsStream, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );
        // tls_close: TlsStream -> ()
        env.bindings.insert(
            "tls_close".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::TlsStream], Box::new(Ty::Unit)) },
        );

        // ===== Compression builtins =====
        // gzip_compress: Str -> [Int]
        env.bindings.insert(
            "gzip_compress".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::List(Box::new(Ty::Int)))) },
        );
        // gzip_decompress: [Int] -> Result[Str, Str]
        env.bindings.insert(
            "gzip_decompress".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );
        // zlib_compress: Str -> [Int]
        env.bindings.insert(
            "zlib_compress".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::List(Box::new(Ty::Int)))) },
        );
        // zlib_decompress: [Int] -> Result[Str, Str]
        env.bindings.insert(
            "zlib_decompress".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::List(Box::new(Ty::Int))], Box::new(Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)))) },
        );

        // ===== SQLite database builtins =====
        // db_open: Str -> Result[Database, Str]
        env.bindings.insert(
            "db_open".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Str], Box::new(Ty::Result(Box::new(Ty::Database), Box::new(Ty::Str)))) },
        );
        // db_open_memory: () -> Result[Database, Str]
        env.bindings.insert(
            "db_open_memory".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![], Box::new(Ty::Result(Box::new(Ty::Database), Box::new(Ty::Str)))) },
        );
        // db_execute: (Database, Str) -> Result[Int, Str]
        env.bindings.insert(
            "db_execute".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Database, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );
        // db_query: (Database, Str) -> Result[[Row], Str]
        env.bindings.insert(
            "db_query".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Database, Ty::Str], Box::new(Ty::Result(Box::new(Ty::List(Box::new(Ty::DbRow))), Box::new(Ty::Str)))) },
        );
        // db_query_one: (Database, Str) -> Result[Row?, Str]
        env.bindings.insert(
            "db_query_one".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Database, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Option(Box::new(Ty::DbRow))), Box::new(Ty::Str)))) },
        );
        // db_close: Database -> ()
        env.bindings.insert(
            "db_close".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Database], Box::new(Ty::Unit)) },
        );
        // db_prepare: (Database, Str) -> Result[Statement, Str]
        env.bindings.insert(
            "db_prepare".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::Database, Ty::Str], Box::new(Ty::Result(Box::new(Ty::Statement), Box::new(Ty::Str)))) },
        );
        // db_execute_prepared: (Statement, [T]) -> Result[Int, Str]
        let exec_t = TypeVar::fresh();
        env.bindings.insert(
            "db_execute_prepared".to_string(),
            TypeScheme { vars: vec![exec_t], ty: Ty::Fn(vec![Ty::Statement, Ty::List(Box::new(Ty::Var(exec_t)))], Box::new(Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str)))) },
        );
        // db_query_prepared: (Statement, [T]) -> Result[[Row], Str]
        let query_t = TypeVar::fresh();
        env.bindings.insert(
            "db_query_prepared".to_string(),
            TypeScheme { vars: vec![query_t], ty: Ty::Fn(vec![Ty::Statement, Ty::List(Box::new(Ty::Var(query_t)))], Box::new(Ty::Result(Box::new(Ty::List(Box::new(Ty::DbRow))), Box::new(Ty::Str)))) },
        );
        // row_get: (Row, Int) -> T? (generic)
        let row_get_t = TypeVar::fresh();
        env.bindings.insert(
            "row_get".to_string(),
            TypeScheme { vars: vec![row_get_t], ty: Ty::Fn(vec![Ty::DbRow, Ty::Int], Box::new(Ty::Option(Box::new(Ty::Var(row_get_t))))) },
        );
        // row_get_int: (Row, Int) -> Int
        env.bindings.insert(
            "row_get_int".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::DbRow, Ty::Int], Box::new(Ty::Int)) },
        );
        // row_get_str: (Row, Int) -> Str
        env.bindings.insert(
            "row_get_str".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::DbRow, Ty::Int], Box::new(Ty::Str)) },
        );
        // row_get_float: (Row, Int) -> Float
        env.bindings.insert(
            "row_get_float".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::DbRow, Ty::Int], Box::new(Ty::Float)) },
        );
        // row_get_bool: (Row, Int) -> Bool
        env.bindings.insert(
            "row_get_bool".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::DbRow, Ty::Int], Box::new(Ty::Bool)) },
        );
        // row_is_null: (Row, Int) -> Bool
        env.bindings.insert(
            "row_is_null".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::DbRow, Ty::Int], Box::new(Ty::Bool)) },
        );
        // row_len: Row -> Int
        env.bindings.insert(
            "row_len".to_string(),
            TypeScheme { vars: vec![], ty: Ty::Fn(vec![Ty::DbRow], Box::new(Ty::Int)) },
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
        // Auto-register variant-to-enum mappings for enum types
        if let TypeDef::Enum { variants, .. } = &def {
            for (variant_name, _) in variants {
                self.variant_to_enum.insert(variant_name.clone(), name.clone());
            }
        }
        self.types.insert(name, def);
    }

    /// Look up a type definition.
    pub fn get_type(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(name)
    }

    /// Look up an enum type by variant name.
    /// Returns the enum name and its TypeDef if the variant is known.
    pub fn get_enum_for_variant(&self, variant_name: &str) -> Option<(&str, &TypeDef)> {
        if let Some(enum_name) = self.variant_to_enum.get(variant_name)
            && let Some(def) = self.types.get(enum_name) {
                return Some((enum_name.as_str(), def));
            }
        None
    }

    pub fn insert_trait(&mut self, name: String, info: TraitInfo) {
        self.traits.insert(name, info);
    }

    pub fn get_trait(&self, name: &str) -> Option<&TraitInfo> {
        self.traits.get(name)
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
            fn_info: self.fn_info.clone(),
            traits: self.traits.clone(),
            variant_to_enum: self.variant_to_enum.clone(),
        }
    }

    /// Get function info (for checking default parameters).
    pub fn get_fn_info(&self, name: &str) -> Option<&FunctionInfo> {
        self.fn_info.get(name)
    }

    /// Insert function info.
    pub fn insert_fn_info(&mut self, name: String, info: FunctionInfo) {
        self.fn_info.insert(name, info);
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

    /// Save a snapshot of the current substitution state.
    /// Use with `restore()` to roll back speculative unification.
    pub fn checkpoint(&self) -> Substitution {
        self.subst.clone()
    }

    /// Restore the substitution to a previously saved state.
    pub fn restore(&mut self, checkpoint: Substitution) {
        self.subst = checkpoint;
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
            | (Ty::Isize, Ty::Isize)
            | (Ty::UInt, Ty::UInt)
            | (Ty::U8, Ty::U8)
            | (Ty::U16, Ty::U16)
            | (Ty::U32, Ty::U32)
            | (Ty::U64, Ty::U64)
            | (Ty::U128, Ty::U128)
            | (Ty::Usize, Ty::Usize)
            | (Ty::Float, Ty::Float)
            | (Ty::F32, Ty::F32)
            | (Ty::F64, Ty::F64)
            | (Ty::Bool, Ty::Bool)
            | (Ty::Char, Ty::Char)
            | (Ty::Str, Ty::Str)
            | (Ty::Unit, Ty::Unit)
            | (Ty::Never, Ty::Never)
            | (Ty::Json, Ty::Json) => Ok(()),

            // Type variable unification
            (Ty::Var(v), t) | (t, Ty::Var(v)) => {
                if let Ty::Var(v2) = t
                    && v == v2 {
                        return Ok(());
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

            // Task unification
            (Ty::Task(t1), Ty::Task(t2)) => self.unify(t1, t2, span),

            // Future unification
            (Ty::Future(t1), Ty::Future(t2)) => self.unify(t1, t2, span),

            // Sender unification
            (Ty::Sender(t1), Ty::Sender(t2)) => self.unify(t1, t2, span),

            // Receiver unification
            (Ty::Receiver(t1), Ty::Receiver(t2)) => self.unify(t1, t2, span),

            // Mutex unification
            (Ty::Mutex(t1), Ty::Mutex(t2)) => self.unify(t1, t2, span),

            // MutexGuard unification
            (Ty::MutexGuard(t1), Ty::MutexGuard(t2)) => self.unify(t1, t2, span),

            // Network type unification
            (Ty::TcpStream, Ty::TcpStream) => Ok(()),
            (Ty::TcpListener, Ty::TcpListener) => Ok(()),
            (Ty::UdpSocket, Ty::UdpSocket) => Ok(()),
            (Ty::TlsStream, Ty::TlsStream) => Ok(()),

            // Database type unification
            (Ty::Database, Ty::Database) => Ok(()),
            (Ty::Statement, Ty::Statement) => Ok(()),
            (Ty::DbRow, Ty::DbRow) => Ok(()),

            // C FFI type unification
            (Ty::CInt, Ty::CInt) => Ok(()),
            (Ty::CUInt, Ty::CUInt) => Ok(()),
            (Ty::CLong, Ty::CLong) => Ok(()),
            (Ty::CULong, Ty::CULong) => Ok(()),
            (Ty::CFloat, Ty::CFloat) => Ok(()),
            (Ty::CDouble, Ty::CDouble) => Ok(()),
            (Ty::CSize, Ty::CSize) => Ok(()),
            (Ty::CVoid, Ty::CVoid) => Ok(()),
            (Ty::RawPtr(t1), Ty::RawPtr(t2)) => self.unify(t1, t2, span),

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

            // Unify Ty::Result with Ty::Named("Result", [...])
            // This allows T! to unify with Result[T, E]
            (Ty::Result(ok, err), Ty::Named(id, args))
            | (Ty::Named(id, args), Ty::Result(ok, err))
                if id.name == "Result" && args.len() == 2 =>
            {
                self.unify(ok, &args[0], span)?;
                self.unify(err, &args[1], span)
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

/// Method signature for type checking method calls.
#[derive(Debug, Clone)]
pub struct MethodSignature {
    /// Parameter types (NOT including self/receiver)
    pub params: Vec<Ty>,
    /// Return type
    pub return_type: Ty,
    /// Whether this method is generic (uses type from receiver)
    pub uses_receiver_type: bool,
}

/// Key for looking up methods: (base type category, method name)
/// Base type category is a string like "Vec", "Map", "Str", etc.
type MethodKey = (String, String);

/// Type inference engine.
/// Tracking info for a linear/affine variable.
#[derive(Debug, Clone)]
struct LinearVarInfo {
    /// The linearity kind of this variable
    linearity: super::types::LinearityKind,
    /// How many times this variable has been used (consumed)
    use_count: usize,
    /// Span where the variable was defined
    def_span: Span,
    /// Span of the last use (if any)
    last_use_span: Option<Span>,
}

pub struct InferenceEngine {
    env: TypeEnv,
    unifier: Unifier,
    /// Current function's return type (for checking return statements)
    return_type: Option<Ty>,
    /// Current type parameters (for generic functions/structs)
    /// Maps type parameter names (e.g., "T") to their type variables
    type_params: HashMap<String, TypeVar>,
    /// Built-in method signatures for Vec, Map, Str, etc.
    builtin_methods: HashMap<MethodKey, MethodSignature>,
    /// Current impl target type (for resolving `Self` in impl blocks)
    impl_self_type: Option<Ty>,
    /// Track where symbols are defined for LSP
    symbol_locations: HashMap<String, (Span, super::checker::DefinitionKind)>,
    /// Linear/affine variable tracking: maps variable name to tracking info
    linear_tracking: HashMap<String, LinearVarInfo>,
}

impl InferenceEngine {
    pub fn new() -> Self {
        let mut engine = Self {
            env: TypeEnv::with_builtins(),
            unifier: Unifier::new(),
            return_type: None,
            type_params: HashMap::new(),
            builtin_methods: HashMap::new(),
            impl_self_type: None,
            symbol_locations: HashMap::new(),
            linear_tracking: HashMap::new(),
        };
        engine.register_builtin_methods();
        engine
    }

    pub fn with_env(env: TypeEnv) -> Self {
        let mut engine = Self {
            env,
            unifier: Unifier::new(),
            return_type: None,
            type_params: HashMap::new(),
            builtin_methods: HashMap::new(),
            impl_self_type: None,
            symbol_locations: HashMap::new(),
            linear_tracking: HashMap::new(),
        };
        engine.register_builtin_methods();
        engine
    }

    // ========================================================================
    // Linear/Affine Type Tracking
    // ========================================================================

    /// Track a new linear or affine variable definition.
    pub fn track_linear_def(&mut self, name: &str, linearity: super::types::LinearityKind, span: Span) {
        if linearity != super::types::LinearityKind::Regular {
            self.linear_tracking.insert(name.to_string(), LinearVarInfo {
                linearity,
                use_count: 0,
                def_span: span,
                last_use_span: None,
            });
        }
    }

    /// Track a use of a variable. Returns an error if it's a use-after-move.
    pub fn track_linear_use(&mut self, name: &str, span: Span) -> Result<(), TypeError> {
        if let Some(info) = self.linear_tracking.get_mut(name) {
            if info.use_count >= 1 {
                // Already used — this is a use-after-move for linear/affine types
                return Err(TypeError {
                    message: format!(
                        "use of moved value: `{}` ({}type used after move)",
                        name, info.linearity
                    ),
                    span,
                });
            }
            info.use_count += 1;
            info.last_use_span = Some(span);
        }
        Ok(())
    }

    /// Check that all linear variables in scope have been used exactly once,
    /// and affine variables at most once. Call at function/scope exit.
    pub fn check_linear_scope_exit(&self) -> Vec<TypeError> {
        let mut errors = Vec::new();
        for (name, info) in &self.linear_tracking {
            match info.linearity {
                super::types::LinearityKind::Linear => {
                    if info.use_count == 0 {
                        errors.push(TypeError {
                            message: format!(
                                "linear value `{}` must be used exactly once, but was never used",
                                name
                            ),
                            span: info.def_span,
                        });
                    }
                    // use_count > 1 is already caught by track_linear_use
                }
                super::types::LinearityKind::Affine => {
                    // Affine values can be dropped (use_count == 0 is OK)
                    // use_count > 1 is already caught by track_linear_use
                }
                super::types::LinearityKind::Regular => {
                    // No restrictions
                }
            }
        }
        errors
    }

    /// Register built-in method signatures for Vec, Map, Str, etc.
    fn register_builtin_methods(&mut self) {
        // Helper to create a method key
        let mk = |type_name: &str, method: &str| -> MethodKey {
            (type_name.to_string(), method.to_string())
        };

        // A placeholder type variable for generic element types
        // At call time, we'll substitute this with the actual element type
        let t_var = Ty::Var(TypeVar { id: reserved_type_vars::ELEM_TYPE });

        // ===== Vec/List methods =====
        // len: [T] -> Int
        self.builtin_methods.insert(mk("Vec", "len"), MethodSignature {
            params: vec![],
            return_type: Ty::Int,
            uses_receiver_type: false,
        });

        // push: ([T], T) -> [T]
        self.builtin_methods.insert(mk("Vec", "push"), MethodSignature {
            params: vec![t_var.clone()],
            return_type: Ty::List(Box::new(t_var.clone())),
            uses_receiver_type: true,
        });

        // pop: [T] -> ([T], T?)
        self.builtin_methods.insert(mk("Vec", "pop"), MethodSignature {
            params: vec![],
            return_type: Ty::Tuple(vec![
                Ty::List(Box::new(t_var.clone())),
                Ty::Option(Box::new(t_var.clone())),
            ]),
            uses_receiver_type: true,
        });

        // get: ([T], Int) -> T?
        self.builtin_methods.insert(mk("Vec", "get"), MethodSignature {
            params: vec![Ty::Int],
            return_type: Ty::Option(Box::new(t_var.clone())),
            uses_receiver_type: true,
        });

        // set: ([T], Int, T) -> [T]
        self.builtin_methods.insert(mk("Vec", "set"), MethodSignature {
            params: vec![Ty::Int, t_var.clone()],
            return_type: Ty::List(Box::new(t_var.clone())),
            uses_receiver_type: true,
        });

        // first: [T] -> T?
        self.builtin_methods.insert(mk("Vec", "first"), MethodSignature {
            params: vec![],
            return_type: Ty::Option(Box::new(t_var.clone())),
            uses_receiver_type: true,
        });

        // last: [T] -> T?
        self.builtin_methods.insert(mk("Vec", "last"), MethodSignature {
            params: vec![],
            return_type: Ty::Option(Box::new(t_var.clone())),
            uses_receiver_type: true,
        });

        // concat: ([T], [T]) -> [T]
        self.builtin_methods.insert(mk("Vec", "concat"), MethodSignature {
            params: vec![Ty::List(Box::new(t_var.clone()))],
            return_type: Ty::List(Box::new(t_var.clone())),
            uses_receiver_type: true,
        });

        // slice: ([T], Int, Int) -> [T]
        self.builtin_methods.insert(mk("Vec", "slice"), MethodSignature {
            params: vec![Ty::Int, Ty::Int],
            return_type: Ty::List(Box::new(t_var.clone())),
            uses_receiver_type: true,
        });

        // reverse: [T] -> [T]
        self.builtin_methods.insert(mk("Vec", "reverse"), MethodSignature {
            params: vec![],
            return_type: Ty::List(Box::new(t_var.clone())),
            uses_receiver_type: true,
        });

        // ===== String methods =====
        // len: Str -> Int
        self.builtin_methods.insert(mk("Str", "len"), MethodSignature {
            params: vec![],
            return_type: Ty::Int,
            uses_receiver_type: false,
        });

        // char_at: (Str, Int) -> Char?
        self.builtin_methods.insert(mk("Str", "char_at"), MethodSignature {
            params: vec![Ty::Int],
            return_type: Ty::Option(Box::new(Ty::Char)),
            uses_receiver_type: false,
        });

        // contains: (Str, Str) -> Bool
        self.builtin_methods.insert(mk("Str", "contains"), MethodSignature {
            params: vec![Ty::Str],
            return_type: Ty::Bool,
            uses_receiver_type: false,
        });

        // starts_with: (Str, Str) -> Bool
        self.builtin_methods.insert(mk("Str", "starts_with"), MethodSignature {
            params: vec![Ty::Str],
            return_type: Ty::Bool,
            uses_receiver_type: false,
        });

        // ends_with: (Str, Str) -> Bool
        self.builtin_methods.insert(mk("Str", "ends_with"), MethodSignature {
            params: vec![Ty::Str],
            return_type: Ty::Bool,
            uses_receiver_type: false,
        });

        // split: (Str, Str) -> [Str]
        self.builtin_methods.insert(mk("Str", "split"), MethodSignature {
            params: vec![Ty::Str],
            return_type: Ty::List(Box::new(Ty::Str)),
            uses_receiver_type: false,
        });

        // trim: Str -> Str
        self.builtin_methods.insert(mk("Str", "trim"), MethodSignature {
            params: vec![],
            return_type: Ty::Str,
            uses_receiver_type: false,
        });

        // to_int: Str -> Int?
        self.builtin_methods.insert(mk("Str", "to_int"), MethodSignature {
            params: vec![],
            return_type: Ty::Option(Box::new(Ty::Int)),
            uses_receiver_type: false,
        });

        // ===== Map methods =====
        let k_var = Ty::Var(TypeVar { id: reserved_type_vars::KEY_TYPE });
        let v_var = Ty::Var(TypeVar { id: reserved_type_vars::VALUE_TYPE });

        // len: Map[K,V] -> Int
        self.builtin_methods.insert(mk("Map", "len"), MethodSignature {
            params: vec![],
            return_type: Ty::Int,
            uses_receiver_type: false,
        });

        // get: (Map[K,V], K) -> V?
        self.builtin_methods.insert(mk("Map", "get"), MethodSignature {
            params: vec![k_var.clone()],
            return_type: Ty::Option(Box::new(v_var.clone())),
            uses_receiver_type: true,
        });

        // insert: (Map[K,V], K, V) -> Map[K,V]
        self.builtin_methods.insert(mk("Map", "insert"), MethodSignature {
            params: vec![k_var.clone(), v_var.clone()],
            return_type: Ty::Map(Box::new(k_var.clone()), Box::new(v_var.clone())),
            uses_receiver_type: true,
        });

        // remove: (Map[K,V], K) -> (Map[K,V], V?)
        self.builtin_methods.insert(mk("Map", "remove"), MethodSignature {
            params: vec![k_var.clone()],
            return_type: Ty::Tuple(vec![
                Ty::Map(Box::new(k_var.clone()), Box::new(v_var.clone())),
                Ty::Option(Box::new(v_var.clone())),
            ]),
            uses_receiver_type: true,
        });

        // contains: (Map[K,V], K) -> Bool
        self.builtin_methods.insert(mk("Map", "contains"), MethodSignature {
            params: vec![k_var.clone()],
            return_type: Ty::Bool,
            uses_receiver_type: true,
        });

        // keys: Map[K,V] -> [K]
        self.builtin_methods.insert(mk("Map", "keys"), MethodSignature {
            params: vec![],
            return_type: Ty::List(Box::new(k_var.clone())),
            uses_receiver_type: true,
        });

        // values: Map[K,V] -> [V]
        self.builtin_methods.insert(mk("Map", "values"), MethodSignature {
            params: vec![],
            return_type: Ty::List(Box::new(v_var.clone())),
            uses_receiver_type: true,
        });

        // ===== Char methods =====
        // is_digit: Char -> Bool
        self.builtin_methods.insert(mk("Char", "is_digit"), MethodSignature {
            params: vec![],
            return_type: Ty::Bool,
            uses_receiver_type: false,
        });

        // is_alpha: Char -> Bool
        self.builtin_methods.insert(mk("Char", "is_alpha"), MethodSignature {
            params: vec![],
            return_type: Ty::Bool,
            uses_receiver_type: false,
        });

        // is_alphanumeric: Char -> Bool
        self.builtin_methods.insert(mk("Char", "is_alphanumeric"), MethodSignature {
            params: vec![],
            return_type: Ty::Bool,
            uses_receiver_type: false,
        });

        // is_whitespace: Char -> Bool
        self.builtin_methods.insert(mk("Char", "is_whitespace"), MethodSignature {
            params: vec![],
            return_type: Ty::Bool,
            uses_receiver_type: false,
        });

        // ===== Int methods =====
        // to_str: Int -> Str
        self.builtin_methods.insert(mk("Int", "to_str"), MethodSignature {
            params: vec![],
            return_type: Ty::Str,
            uses_receiver_type: false,
        });
    }

    /// Look up a method on a type, returning its signature and element types for substitution.
    /// Returns (MethodSignature, element_types) where element_types contains:
    /// - For Vec[T]: the element type T
    /// - For Map[K,V]: the key and value types (K, V)
    /// - For primitives: empty
    fn lookup_method(&self, ty: &Ty, method_name: &str) -> Option<(MethodSignature, Vec<Ty>)> {
        let (type_category, elem_types) = self.classify_type_for_method(ty);

        // Look up in builtin methods
        let key = (type_category, method_name.to_string());
        if let Some(sig) = self.builtin_methods.get(&key) {
            return Some((sig.clone(), elem_types));
        }

        // Check trait definitions for matching method names.
        // If a trait defines a method with this name, return its signature.
        // This enables method calls on types that implement the trait.
        for trait_info in self.env.traits.values() {
            for method in &trait_info.methods {
                if method.name == method_name {
                    let sig = MethodSignature {
                        params: method.params.clone(),
                        return_type: method.return_type.clone(),
                        uses_receiver_type: false,
                    };
                    return Some((sig, elem_types));
                }
            }
        }

        None
    }

    /// Classify a type for method lookup, returning the category name and element types.
    fn classify_type_for_method(&self, ty: &Ty) -> (String, Vec<Ty>) {
        match ty {
            Ty::List(elem) => ("Vec".to_string(), vec![*elem.clone()]),
            Ty::Array(elem, _) => ("Vec".to_string(), vec![*elem.clone()]),
            Ty::Map(key, val) => ("Map".to_string(), vec![*key.clone(), *val.clone()]),
            Ty::Str => ("Str".to_string(), vec![]),
            Ty::Char => ("Char".to_string(), vec![]),
            // Generic integer/float
            Ty::Int => ("Int".to_string(), vec![]),
            Ty::Float => ("Float".to_string(), vec![]),
            // Signed integers - treat as Int for method lookup
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::I128 | Ty::Isize => ("Int".to_string(), vec![]),
            // Unsigned integers - treat as Int for method lookup
            Ty::UInt | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::U128 | Ty::Usize => ("Int".to_string(), vec![]),
            // Float types
            Ty::F32 | Ty::F64 => ("Float".to_string(), vec![]),
            Ty::Bool => ("Bool".to_string(), vec![]),
            // Handle references by looking at the inner type
            Ty::Ref(inner, _) => self.classify_type_for_method(inner),
            // Handle Named types (user-defined structs)
            Ty::Named(id, args) => (id.name.clone(), args.clone()),
            // For type variables, try to resolve through substitution
            Ty::Var(tv) => {
                if let Some(resolved) = self.unifier.subst.get(*tv) {
                    self.classify_type_for_method(resolved)
                } else {
                    ("Unknown".to_string(), vec![])
                }
            }
            _ => ("Unknown".to_string(), vec![]),
        }
    }

    /// Substitute sentinel type variables with actual element types.
    /// Uses reserved_type_vars constants for T (element), K (key), V (value)
    fn substitute_elem_types(&self, ty: &Ty, elem_types: &[Ty]) -> Ty {
        use reserved_type_vars::*;
        match ty {
            Ty::Var(tv) => {
                match tv.id {
                    ELEM_TYPE => elem_types.first().cloned().unwrap_or(Ty::Var(*tv)),
                    KEY_TYPE => elem_types.first().cloned().unwrap_or(Ty::Var(*tv)), // K for maps
                    VALUE_TYPE => elem_types.get(1).cloned().unwrap_or(Ty::Var(*tv)),  // V for maps
                    _ => Ty::Var(*tv),
                }
            }
            Ty::List(elem) => Ty::List(Box::new(self.substitute_elem_types(elem, elem_types))),
            Ty::Option(inner) => Ty::Option(Box::new(self.substitute_elem_types(inner, elem_types))),
            Ty::Result(ok, err) => Ty::Result(
                Box::new(self.substitute_elem_types(ok, elem_types)),
                Box::new(self.substitute_elem_types(err, elem_types)),
            ),
            Ty::Tuple(elems) => Ty::Tuple(
                elems.iter().map(|e| self.substitute_elem_types(e, elem_types)).collect()
            ),
            Ty::Map(k, v) => Ty::Map(
                Box::new(self.substitute_elem_types(k, elem_types)),
                Box::new(self.substitute_elem_types(v, elem_types)),
            ),
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| self.substitute_elem_types(p, elem_types)).collect(),
                Box::new(self.substitute_elem_types(ret, elem_types)),
            ),
            // All other types remain unchanged
            _ => ty.clone(),
        }
    }

    /// Look up a field type on a struct or tuple type.
    fn lookup_field(&self, ty: &Ty, field_name: &str, span: Span) -> Result<Ty, TypeError> {
        match ty {
            // Handle struct types (Named types)
            Ty::Named(type_id, type_args) => {
                // Handle Self type by resolving to the impl target type
                if type_id.name == "Self" {
                    if let Some(self_ty) = &self.impl_self_type {
                        return self.lookup_field(self_ty, field_name, span);
                    }
                    return Err(TypeError::new(
                        "'Self' used outside of impl block".to_string(),
                        span
                    ));
                }

                // Look up the struct definition in the type environment
                if let Some(TypeDef::Struct { type_params, fields }) = self.env.get_type(&type_id.name) {
                    // Find the field
                    if let Some((_, field_ty)) = fields.iter().find(|(name, _)| name == field_name) {
                        // If the struct is generic, substitute type parameters
                        if !type_params.is_empty() && !type_args.is_empty() {
                            let subst: HashMap<String, Ty> = type_params
                                .iter()
                                .zip(type_args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect();
                            return Ok(self.substitute_type_params(field_ty, &subst));
                        }
                        return Ok(field_ty.clone());
                    } else {
                        // Field not found - list available fields
                        let field_names: Vec<_> = fields.iter().map(|(n, _)| n.as_str()).collect();
                        return Err(TypeError::new(
                            format!("type '{}' has no field '{}'. Available fields: {}",
                                    type_id.name, field_name, field_names.join(", ")),
                            span
                        ));
                    }
                }
                Err(TypeError::new(
                    format!("type '{}' has no field '{}'", type_id.name, field_name),
                    span
                ))
            }

            // Handle tuple types (numeric field access: tuple.0, tuple.1, etc.)
            Ty::Tuple(elems) => {
                if let Ok(index) = field_name.parse::<usize>() {
                    if let Some(elem_ty) = elems.get(index) {
                        return Ok(elem_ty.clone());
                    } else {
                        return Err(TypeError::new(
                            format!("tuple index {} out of bounds (tuple has {} elements)",
                                    index, elems.len()),
                            span
                        ));
                    }
                }
                Err(TypeError::new(
                    format!("cannot access field '{}' on tuple type (use numeric indices: .0, .1, etc.)",
                            field_name),
                    span
                ))
            }

            // Handle references by looking through to the inner type
            Ty::Ref(inner, _) => self.lookup_field(inner, field_name, span),

            // Handle type variables by trying to resolve them
            Ty::Var(tv) => {
                if let Some(resolved) = self.unifier.subst.get(*tv) {
                    self.lookup_field(resolved, field_name, span)
                } else {
                    Err(TypeError::new(
                        format!("Cannot access field '{}' on unresolved type", field_name),
                        span,
                    ))
                }
            }

            _ => Err(TypeError::new(
                format!("cannot access field '{}' on type {}", field_name, ty),
                span
            ))
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
                if args.is_empty()
                    && let Some(replacement) = subst.get(&id.name) {
                        return replacement.clone();
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
                let struct_name = s.name.name.clone();

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

                // Restore old type params and save type var map for constructor
                let current_type_params = std::mem::replace(&mut self.type_params, old_type_params);

                // Build type var mapping for constructor type scheme
                let type_var_map: Vec<(String, TypeVar)> = current_type_params
                    .into_iter()
                    .collect();

                // Build the struct type with type arguments
                let struct_ty_args: Vec<Ty> = type_var_map.iter().map(|(_, tv)| Ty::Var(*tv)).collect();
                let struct_ty = Ty::Named(TypeId::new(&struct_name), struct_ty_args);

                // Create constructor type: function from field types to struct type
                let field_tys: Vec<Ty> = fields.iter().map(|(_, ty)| ty.clone()).collect();
                let constructor_type = if field_tys.is_empty() {
                    // Unit struct: just the struct type
                    struct_ty.clone()
                } else {
                    // Struct with fields: function from fields to struct type
                    Ty::Fn(field_tys, Box::new(struct_ty.clone()))
                };

                // Create type scheme with type variables
                let scheme = TypeScheme {
                    vars: type_var_map.iter().map(|(_, tv)| *tv).collect(),
                    ty: constructor_type,
                };

                // Add struct constructor binding
                self.env.insert(struct_name.clone(), scheme);

                self.env.insert_type(
                    struct_name.clone(),
                    TypeDef::Struct { type_params, fields },
                );
                self.symbol_locations.insert(
                    struct_name,
                    (s.name.span, super::checker::DefinitionKind::Struct),
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
                    enum_name.clone(),
                    TypeDef::Enum { type_params, variants },
                );
                self.symbol_locations.insert(
                    enum_name,
                    (e.name.span, super::checker::DefinitionKind::Enum),
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
            ItemKind::Trait(t) => {
                let trait_name = t.name.name.clone();
                let type_params = self.get_type_params(&t.generics);

                let old_type_params = std::mem::take(&mut self.type_params);
                self.type_params = self.setup_type_params(&t.generics);

                let mut methods = Vec::new();
                for trait_item in &t.items {
                    if let crate::parser::TraitItem::Function(f) = trait_item {
                        // Validate that method param types resolve correctly
                        // (references only declared type params)
                        let mut param_types = Vec::new();
                        for p in f.params.iter().filter(|p| p.name.name != "self") {
                            match self.ast_type_to_ty(&p.ty) {
                                Ok(ty) => param_types.push(ty),
                                Err(_) => {
                                    // Silently skip unresolvable types (may reference
                                    // method-level generics not yet supported)
                                    param_types.push(Ty::fresh_var());
                                }
                            }
                        }

                        let return_type = match f.return_type.as_ref() {
                            Some(ty) => self.ast_type_to_ty(ty).unwrap_or(Ty::Unit),
                            None => Ty::Unit,
                        };

                        methods.push(TraitMethodInfo {
                            name: f.name.name.clone(),
                            params: param_types,
                            return_type,
                            has_default: f.body.is_some(),
                        });
                    }
                }

                self.env.insert_trait(trait_name.clone(), TraitInfo {
                    name: trait_name,
                    type_params,
                    methods,
                });

                self.type_params = old_type_params;
            }
            _ => {}
        }
        Ok(())
    }

    /// Collect a function signature into the environment.
    fn collect_function_sig(&mut self, item: &Item) -> Result<(), TypeError> {
        match &item.kind {
            ItemKind::Function(f) => {
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

                let fn_type = Ty::Fn(param_types.clone(), Box::new(return_type));

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
                self.symbol_locations.insert(
                    f.name.name.clone(),
                    (f.name.span, super::checker::DefinitionKind::Function),
                );

                // Track function info for default parameter handling
                let required_params = f.params.iter().filter(|p| p.default.is_none()).count();
                let total_params = f.params.len();
                let param_pass_modes: Vec<PassMode> = f.params.iter().map(|p| p.pass_mode).collect();
                self.env.insert_fn_info(
                    f.name.name.clone(),
                    FunctionInfo {
                        required_params,
                        total_params,
                        param_types,
                        param_pass_modes,
                    },
                );

                // Restore old type params
                self.type_params = old_type_params;
            }
            ItemKind::Impl(i) => {
                // Get the impl target type name
                let type_name = self.get_type_name_from_ast_type(&i.self_type);

                // Collect method signatures from impl block
                for impl_item in &i.items {
                    if let crate::parser::ImplItem::Function(f) = impl_item {
                        // Skip the self parameter for method signature
                        let param_types: Vec<Ty> = f
                            .params
                            .iter()
                            .filter(|p| p.name.name != "self")
                            .map(|p| self.ast_type_to_ty(&p.ty))
                            .collect::<Result<Vec<_>, _>>()?;

                        let return_type = if let Some(ty) = &f.return_type {
                            self.ast_type_to_ty(ty)?
                        } else {
                            Ty::Unit
                        };

                        // Register method in builtin_methods for lookup
                        let method_key = (type_name.clone(), f.name.name.clone());
                        self.builtin_methods.insert(method_key, MethodSignature {
                            params: param_types,
                            return_type,
                            uses_receiver_type: false,
                        });
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Extract the type name from an AST type (for impl block targets).
    fn get_type_name_from_ast_type(&self, ast_ty: &AstType) -> String {
        match &ast_ty.kind {
            AstTypeKind::Path(p) => {
                p.segments.last().map(|s| s.name.name.clone()).unwrap_or_default()
            }
            _ => "Unknown".to_string(),
        }
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
                // Convert the impl target type and set it for Self resolution
                let self_ty = self.ast_type_to_ty(&i.self_type)?;
                let old_self_type = self.impl_self_type.replace(self_ty);

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

                // Validate trait implementation
                if let Some(trait_type) = &i.trait_ {
                    let trait_name = match &trait_type.kind {
                        crate::parser::TypeKind::Path(p) => p.segments.last()
                            .map(|s| s.name.name.clone())
                            .unwrap_or_default(),
                        _ => String::new(),
                    };

                    if let Some(trait_info) = self.env.get_trait(&trait_name) {
                        let mut impl_methods: std::collections::HashSet<String> = std::collections::HashSet::new();
                        for impl_item in &i.items {
                            if let crate::parser::ImplItem::Function(f) = impl_item {
                                impl_methods.insert(f.name.name.clone());
                            }
                        }

                        // Check all required methods are implemented
                        for method in &trait_info.methods {
                            if !method.has_default && !impl_methods.contains(&method.name) {
                                return Err(TypeError::new(
                                    format!(
                                        "missing method '{}' required by trait '{}'",
                                        method.name, trait_name
                                    ),
                                    i.span,
                                ));
                            }
                        }

                        // Validate method signatures
                        for impl_item in &i.items {
                            if let crate::parser::ImplItem::Function(f) = impl_item
                                && let Some(trait_method) = trait_info.methods.iter()
                                    .find(|m| m.name == f.name.name)
                                {
                                    let impl_params: Vec<_> = f.params.iter()
                                        .filter(|p| p.name.name != "self")
                                        .collect();

                                    if impl_params.len() != trait_method.params.len() {
                                        return Err(TypeError::new(
                                            format!(
                                                "method '{}' has {} parameter(s), trait requires {}",
                                                f.name.name, impl_params.len(), trait_method.params.len()
                                            ),
                                            f.span,
                                        ));
                                    }
                                }
                        }
                    }
                }

                // Restore old self type (for nested impls, though unlikely)
                self.impl_self_type = old_self_type;
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
                        // String repetition: Str * Int -> Str
                        if *op == BinOp::Mul {
                            let left_resolved = left_ty.apply(self.unifier.substitution());
                            let right_resolved = right_ty.apply(self.unifier.substitution());
                            if left_resolved == Ty::Str && right_resolved == Ty::Int {
                                return Ok(Ty::Str);
                            }
                        }
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
                // Infer argument types first
                let arg_types: Vec<Ty> = args
                    .iter()
                    .map(|a| self.infer_expr(&a.value))
                    .collect::<Result<Vec<_>, _>>()?;

                // Check if callee is an identifier with function info (for default params)
                if let ExprKind::Ident(name) = &callee.kind
                    && let Some(fn_info) = self.env.get_fn_info(&name.name) {
                        let provided = arg_types.len();
                        let required = fn_info.required_params;
                        let total = fn_info.total_params;

                        // Check if we have enough arguments
                        if provided < required {
                            return Err(TypeError::new(
                                format!(
                                    "function '{}' requires at least {} argument(s), found {}",
                                    name.name, required, provided
                                ),
                                expr.span,
                            ));
                        }

                        if provided > total {
                            return Err(TypeError::new(
                                format!(
                                    "function '{}' takes at most {} argument(s), found {}",
                                    name.name, total, provided
                                ),
                                expr.span,
                            ));
                        }

                        // Validate pass modes match between call site and declaration
                        let param_modes = &fn_info.param_pass_modes;
                        for (i, arg) in args.iter().enumerate().take(provided) {
                            if i < param_modes.len() {
                                let expected = param_modes[i];
                                let actual = arg.pass_mode;
                                match (expected, actual) {
                                    (PassMode::Owned, PassMode::Owned) => {}
                                    (PassMode::Ref, PassMode::Ref) => {
                                        // Verify argument is a place expression
                                        if !Self::is_place_expr(&arg.value) {
                                            return Err(TypeError::new(
                                                "'ref' argument must be a variable, field access, or index expression".to_string(),
                                                arg.span,
                                            ));
                                        }
                                    }
                                    (PassMode::RefMut, PassMode::RefMut) => {
                                        // Verify argument is a place expression
                                        if !Self::is_place_expr(&arg.value) {
                                            return Err(TypeError::new(
                                                "'ref mut' argument must be a variable, field access, or index expression".to_string(),
                                                arg.span,
                                            ));
                                        }
                                    }
                                    (PassMode::Ref, PassMode::Owned) | (PassMode::RefMut, PassMode::Owned) => {
                                        let mode_str = if expected == PassMode::Ref { "ref" } else { "ref mut" };
                                        return Err(TypeError::new(
                                            format!(
                                                "parameter 'argument {}' of '{}' requires '{}' at call site",
                                                i + 1, name.name, mode_str
                                            ),
                                            arg.span,
                                        ));
                                    }
                                    (PassMode::Owned, PassMode::Ref) | (PassMode::Owned, PassMode::RefMut) => {
                                        let mode_str = if actual == PassMode::Ref { "ref" } else { "ref mut" };
                                        return Err(TypeError::new(
                                            format!(
                                                "unexpected '{}' on argument {} of '{}' (parameter is not a reference)",
                                                mode_str, i + 1, name.name
                                            ),
                                            arg.span,
                                        ));
                                    }
                                    (PassMode::Ref, PassMode::RefMut) => {
                                        return Err(TypeError::new(
                                            format!(
                                                "parameter {} of '{}' expects 'ref' but got 'ref mut'",
                                                i + 1, name.name
                                            ),
                                            arg.span,
                                        ));
                                    }
                                    (PassMode::RefMut, PassMode::Ref) => {
                                        return Err(TypeError::new(
                                            format!(
                                                "parameter {} of '{}' expects 'ref mut' but got 'ref'",
                                                i + 1, name.name
                                            ),
                                            arg.span,
                                        ));
                                    }
                                }
                            }
                        }

                        // Basic borrow check: reject same variable as ref mut in multiple args
                        {
                            let mut ref_mut_vars: Vec<(String, usize)> = Vec::new();
                            for (i, arg) in args.iter().enumerate().take(provided) {
                                if arg.pass_mode == PassMode::RefMut
                                    && let ExprKind::Ident(ident) = &arg.value.kind {
                                        for &(ref prev_name, prev_idx) in &ref_mut_vars {
                                            if *prev_name == ident.name {
                                                return Err(TypeError::new(
                                                    format!(
                                                        "cannot pass '{}' as 'ref mut' to both argument {} and argument {} of '{}'",
                                                        ident.name, prev_idx + 1, i + 1, name.name
                                                    ),
                                                    arg.span,
                                                ));
                                            }
                                        }
                                        ref_mut_vars.push((ident.name.clone(), i));
                                    }
                            }
                        }

                        // Build a function type with all params (using defaults for missing ones)
                        let full_arg_types: Vec<Ty> = fn_info.param_types.iter()
                            .enumerate()
                            .map(|(i, param_ty)| {
                                if i < provided {
                                    // Use provided argument type
                                    arg_types[i].clone()
                                } else {
                                    // Use parameter type (from default)
                                    param_ty.clone()
                                }
                            })
                            .collect();

                        let callee_ty = self.infer_expr(callee)?;
                        let result_ty = Ty::fresh_var();
                        let expected_fn = Ty::Fn(full_arg_types, Box::new(result_ty.clone()));
                        self.unifier.unify(&callee_ty, &expected_fn, expr.span)?;
                        return Ok(result_ty);
                    }

                // Standard case: no function info (builtins, closures, etc.)
                let callee_ty = self.infer_expr(callee)?;
                let result_ty = Ty::fresh_var();
                let expected_fn = Ty::Fn(arg_types, Box::new(result_ty.clone()));
                self.unifier.unify(&callee_ty, &expected_fn, expr.span)?;
                Ok(result_ty)
            }

            ExprKind::MethodCall(receiver, method, args) => {
                let receiver_ty = self.infer_expr(receiver)?;

                // Resolve the receiver type through substitutions
                let resolved_ty = receiver_ty.apply(&self.unifier.subst);

                // Look up the method based on receiver type
                let (method_sig, elem_types) = self.lookup_method(&resolved_ty, &method.name)
                    .ok_or_else(|| TypeError::new(
                        format!("type {} has no method '{}'", resolved_ty, method.name),
                        method.span
                    ))?;

                // Infer argument types
                let arg_types: Vec<Ty> = args
                    .iter()
                    .map(|a| self.infer_expr(&a.value))
                    .collect::<Result<Vec<_>, _>>()?;

                // Check argument count
                if arg_types.len() != method_sig.params.len() {
                    return Err(TypeError::new(
                        format!("method '{}' expects {} arguments, found {}",
                                method.name, method_sig.params.len(), arg_types.len()),
                        expr.span
                    ));
                }

                // Unify argument types with parameter types
                // For methods that use receiver type, substitute the sentinel values
                for (arg_ty, param_ty) in arg_types.iter().zip(&method_sig.params) {
                    let expected_ty = self.substitute_elem_types(param_ty, &elem_types);
                    self.unifier.unify(arg_ty, &expected_ty, expr.span)?;
                }

                // Return the method's return type with substituted element types
                let return_ty = self.substitute_elem_types(&method_sig.return_type, &elem_types);
                Ok(return_ty)
            }

            ExprKind::Field(base, field) => {
                let base_ty = self.infer_expr(base)?;
                let resolved_ty = base_ty.apply(&self.unifier.subst);

                // Look up the field type based on the base type
                self.lookup_field(&resolved_ty, &field.name, field.span)
            }

            ExprKind::TupleField(base, index) => {
                let base_ty = self.infer_expr(base)?;
                let resolved = base_ty.apply(&self.unifier.subst);
                // For tuple indexing, we need the tuple type
                if let Ty::Tuple(elems) = &resolved {
                    if *index < elems.len() {
                        return Ok(elems[*index].clone());
                    }
                    return Err(TypeError::new(
                        format!("Tuple index {} out of bounds (tuple has {} elements)", index, elems.len()),
                        expr.span,
                    ));
                }
                Err(TypeError::new(
                    format!("Cannot index non-tuple type {:?} with .{}", resolved, index),
                    expr.span,
                ))
            }

            ExprKind::Index(base, index) => {
                let base_ty = self.infer_expr(base)?;
                let index_ty = self.infer_expr(index)?;

                // For list/array indexing — use checkpoint to avoid corrupting
                // substitution state if this speculative unification fails
                let elem_ty = Ty::fresh_var();
                let list_ty = Ty::List(Box::new(elem_ty.clone()));

                let checkpoint = self.unifier.checkpoint();
                if self.unifier.unify(&base_ty, &list_ty, expr.span).is_ok() {
                    self.unifier.unify(&index_ty, &Ty::Int, expr.span)?;
                    return Ok(elem_ty);
                }
                self.unifier.restore(checkpoint);

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
                let count_ty = self.infer_expr(count)?;
                self.unifier.unify(&count_ty, &Ty::Int, expr.span)?;
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
                            let elif_expr = Expr::new(ExprKind::If(elif.clone()), elif.span,
                            );
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
                            Err(TypeError::new(
                                format!("Cannot infer type of lambda parameter '{}'", p.name.name),
                                p.span,
                            ))
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

            ExprKind::For(_label, pattern, iter, body) => {
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

            ExprKind::While(_label, cond, body) => {
                let cond_ty = self.infer_expr(cond)?;
                self.unifier.unify(&cond_ty, &Ty::Bool, expr.span)?;
                self.infer_block(body)?;
                Ok(Ty::Unit)
            }

            ExprKind::WhileLet(_label, pattern, expr_val, body) => {
                let expr_ty = self.infer_expr(expr_val)?;
                self.check_pattern(pattern, &expr_ty)?;

                let mut loop_env = self.env.child();
                self.collect_pattern_bindings(pattern, &expr_ty, &mut loop_env)?;

                let old_env = std::mem::replace(&mut self.env, loop_env);
                self.infer_block(body)?;
                self.env = old_env;

                Ok(Ty::Unit)
            }

            ExprKind::Loop(_label, body) => {
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
                    Err(TypeError::new(
                        format!("Undefined name '{}'", name),
                        expr.span,
                    ))
                }
            }

            ExprKind::Cast(e, ty) => {
                let source_ty = self.infer_expr(e)?;
                let target_ty = self.ast_type_to_ty(ty)?;

                // Validate the cast is allowed
                let resolved_source = source_ty.apply(&self.unifier.subst);
                if !self.can_cast(&resolved_source, &target_ty) {
                    return Err(TypeError::new(
                        format!("cannot cast {} to {}", resolved_source, target_ty),
                        expr.span,
                    ));
                }

                Ok(target_ty)
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
                // Await extracts the value from a Task[T] or Future[T]
                let inner_ty = self.infer_expr(e)?;
                let result_ty = Ty::fresh_var();
                // Try to unify with Task[T] or Future[T]
                // For now, just return a fresh type variable
                let task_ty = Ty::Task(Box::new(result_ty.clone()));
                let future_ty = Ty::Future(Box::new(result_ty.clone()));
                // Accept either Task or Future — use checkpoint/restore to avoid
                // corrupting substitution if the first speculative unify fails
                let checkpoint = self.unifier.checkpoint();
                if self.unifier.unify(&inner_ty, &task_ty, expr.span).is_ok() {
                    Ok(result_ty)
                } else {
                    self.unifier.restore(checkpoint);
                    let checkpoint2 = self.unifier.checkpoint();
                    if self.unifier.unify(&inner_ty, &future_ty, expr.span).is_ok() {
                        Ok(result_ty)
                    } else {
                        self.unifier.restore(checkpoint2);
                        Err(TypeError::new(
                            format!("Cannot await non-async type {:?}", inner_ty.apply(&self.unifier.subst)),
                            expr.span,
                        ))
                    }
                }
            }

            ExprKind::Spawn(e) => {
                // Spawn takes an async expression and returns Task[T]
                let inner_ty = self.infer_expr(e)?;
                // The result type is Task[T] where T is the return type of the async expression
                let result_ty = Ty::fresh_var();
                // Try to unify with Future[T]
                let future_ty = Ty::Future(Box::new(result_ty.clone()));
                if self.unifier.unify(&inner_ty, &future_ty, expr.span).is_ok() {
                    Ok(Ty::Task(Box::new(result_ty)))
                } else {
                    // Fall back to Task with the inferred type
                    Ok(Ty::Task(Box::new(inner_ty)))
                }
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
                let block_ty = self.infer_block(block)?;
                Ok(Ty::Future(Box::new(block_ty)))
            }

            ExprKind::Unsafe(block) => {
                self.infer_block(block)
            }

            ExprKind::FieldShorthand(_field) => {
                // Field shorthand like .field is a closure taking one argument
                // Constrain: arg_ty must have field `_field.name`
                // TODO: Full validation requires struct type resolution to check
                // that the field exists on the argument type. For now, we create
                // the correct function type shape and defer field validation to
                // the point where the shorthand is applied to a concrete type.
                let arg_ty = Ty::fresh_var();
                let result_ty = Ty::fresh_var();
                Ok(Ty::Fn(vec![arg_ty], Box::new(result_ty)))
            }

            ExprKind::OpShorthand(op, operand, _is_left) => {
                // Op shorthand like (+ 1) or (* _) is a closure
                let operand_ty = self.infer_expr(operand)?;
                // Constrain arg and result types based on operator
                let (arg_ty, result_ty) = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        // Numeric ops: arg same type as operand, result same type
                        (operand_ty.clone(), operand_ty)
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        // Comparison ops: arg same type as operand, result Bool
                        (operand_ty, Ty::Bool)
                    }
                    BinOp::And | BinOp::Or => {
                        // Logical ops: both sides Bool
                        (Ty::Bool, Ty::Bool)
                    }
                    _ => (Ty::fresh_var(), Ty::fresh_var())
                };
                Ok(Ty::Fn(vec![arg_ty], Box::new(result_ty)))
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
            PatternKind::Struct(path, fields, rest) => {
                // Get the struct name from the path
                let struct_name = path.segments.last()
                    .map(|s| s.name.name.clone())
                    .unwrap_or_default();

                // Look up the struct definition
                // First try direct lookup, then check if it's a variant name
                let (resolved_name, struct_def) = if let Some(def) = self.env.get_type(&struct_name) {
                    (struct_name.clone(), Some(def.clone()))
                } else if path.segments.len() == 1 {
                    // Single segment path might be a variant name (e.g., "Some", "Ok")
                    if let Some((enum_name, def)) = self.env.get_enum_for_variant(&struct_name) {
                        (enum_name.to_string(), Some(def.clone()))
                    } else {
                        (struct_name.clone(), None)
                    }
                } else {
                    (struct_name.clone(), None)
                };

                match struct_def {
                    Some(TypeDef::Struct { type_params, fields: struct_fields }) => {
                        // Unify expected type with struct type
                        let struct_ty = if type_params.is_empty() {
                            Ty::Named(TypeId::new(&resolved_name), vec![])
                        } else {
                            // For generic structs, create fresh type variables
                            let type_args: Vec<Ty> = type_params.iter()
                                .map(|_| Ty::fresh_var())
                                .collect();
                            Ty::Named(TypeId::new(&resolved_name), type_args)
                        };
                        self.unifier.unify(&struct_ty, ty, pattern.span)?;

                        // Check each field pattern against actual field types
                        let mut matched_fields = std::collections::HashSet::new();
                        for field_pat in fields {
                            // Find the field in the struct definition
                            let field_def = struct_fields.iter()
                                .find(|(name, _)| name == &field_pat.name.name);

                            if let Some((_, field_ty)) = field_def {
                                matched_fields.insert(field_pat.name.name.clone());

                                // If there's a nested pattern, check it against the field type
                                if let Some(nested_pat) = &field_pat.pattern {
                                    self.check_pattern(nested_pat, field_ty)?;
                                }
                            } else {
                                // Field not found
                                let available: Vec<_> = struct_fields.iter()
                                    .map(|(n, _)| n.as_str())
                                    .collect();
                                return Err(TypeError::new(
                                    format!("struct '{}' has no field '{}'. Available: {}",
                                            struct_name, field_pat.name.name, available.join(", ")),
                                    field_pat.name.span
                                ));
                            }
                        }

                        // If no rest pattern (..), ensure all fields are covered
                        if !rest {
                            for (field_name, _) in &struct_fields {
                                if !matched_fields.contains(field_name) {
                                    return Err(TypeError::new(
                                        format!("pattern missing field '{}' (use .. to ignore remaining fields)",
                                                field_name),
                                        pattern.span
                                    ));
                                }
                            }
                        }

                        Ok(())
                    }
                    Some(TypeDef::Enum { type_params, variants }) => {
                        // This is an enum pattern like Some(x) or Color::Red
                        let enum_ty = if type_params.is_empty() {
                            Ty::Named(TypeId::new(&resolved_name), vec![])
                        } else {
                            let type_args: Vec<Ty> = type_params.iter()
                                .map(|_| Ty::fresh_var())
                                .collect();
                            Ty::Named(TypeId::new(&resolved_name), type_args)
                        };
                        self.unifier.unify(&enum_ty, ty, pattern.span)?;

                        // Get variant name from pattern
                        // If struct_name != resolved_name, struct_name is the variant (e.g., "Some" resolved to "Option")
                        // Otherwise, use the last segment or first field name
                        let variant_name = if struct_name != resolved_name {
                            struct_name.clone()
                        } else if path.segments.len() >= 2 {
                            path.segments.last().map(|s| s.name.name.clone()).unwrap_or_default()
                        } else {
                            fields.first().map(|f| f.name.name.clone()).unwrap_or_default()
                        };

                        // Find variant in enum definition
                        let variant = variants.iter()
                            .find(|(name, _)| name == &variant_name);

                        if let Some((_, field_types)) = variant {
                            // Validate field count
                            let pattern_field_count = fields.len();
                            if pattern_field_count != field_types.len() {
                                return Err(TypeError::new(
                                    format!(
                                        "variant '{}::{}' has {} field(s), pattern has {}",
                                        resolved_name, variant_name, field_types.len(), pattern_field_count
                                    ),
                                    pattern.span,
                                ));
                            }

                            // Apply current substitution to get concrete field types.
                            // Without this, generic enum variants like Some(T) would
                            // have unresolved type variables in field_types.
                            let concrete_field_types: Vec<Ty> = field_types.iter()
                                .map(|ft| ft.apply(self.unifier.substitution()))
                                .collect();

                            // Validate field types using concrete types
                            for (field, expected_ty) in fields.iter().zip(concrete_field_types.iter()) {
                                if let Some(nested_pat) = &field.pattern {
                                    self.check_pattern(nested_pat, expected_ty)?;
                                }
                            }
                            Ok(())
                        } else {
                            let available: Vec<_> = variants.iter()
                                .map(|(name, _)| name.as_str())
                                .collect();
                            Err(TypeError::new(
                                format!(
                                    "enum '{}' has no variant '{}'. Available: {}",
                                    resolved_name, variant_name, available.join(", ")
                                ),
                                pattern.span,
                            ))
                        }
                    }
                    _ => {
                        // Unknown struct type - return error
                        Err(TypeError::new(
                            format!("Unknown struct type in pattern: '{}'", struct_name),
                            pattern.span,
                        ))
                    }
                }
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
            PatternKind::Struct(path, fields, _rest) => {
                // Try to get field types from the struct/enum definition
                let struct_name = path.segments.last()
                    .map(|s| s.name.name.clone())
                    .unwrap_or_default();

                // First try direct lookup, then check if it's a variant name
                let type_def = if let Some(def) = self.env.get_type(&struct_name) {
                    Some((struct_name.clone(), def.clone()))
                } else if path.segments.len() == 1 {
                    // Single segment path might be a variant name (e.g., "Some", "Ok")
                    if let Some((_enum_name, def)) = self.env.get_enum_for_variant(&struct_name) {
                        Some((struct_name.clone(), def.clone()))  // struct_name is the variant
                    } else {
                        None
                    }
                } else {
                    None
                };

                match type_def {
                    Some((_name, TypeDef::Struct { fields: sf, .. })) => {
                        // Handle struct pattern
                        for field in fields {
                            let field_ty = sf.iter()
                                .find(|(n, _)| n == &field.name.name)
                                .map(|(_, ty)| ty.clone())
                                .ok_or_else(|| TypeError::new(
                                    format!("Unknown field '{}' in struct pattern", field.name.name),
                                    field.name.span,
                                ))?;

                            if let Some(p) = &field.pattern {
                                self.collect_pattern_bindings(p, &field_ty, env)?;
                            } else {
                                // Shorthand: field name is also the binding
                                env.insert(field.name.name.clone(), TypeScheme::mono(field_ty));
                            }
                        }
                    }
                    Some((variant_name, TypeDef::Enum { variants, .. })) => {
                        // Handle enum variant pattern (e.g., Some(x), Ok(value))
                        // Get the concrete type being matched
                        let resolved_ty = ty.apply(&self.unifier.subst);

                        // Extract field types directly from the concrete type
                        // This handles built-in Option/Result with their actual type arguments
                        let concrete_field_types: Vec<Ty> = match (&resolved_ty, variant_name.as_str()) {
                            (Ty::Option(inner), "Some") => vec![(**inner).clone()],
                            (Ty::Option(_), "None") => vec![],
                            (Ty::Result(ok_ty, _), "Ok") => vec![(**ok_ty).clone()],
                            (Ty::Result(_, err_ty), "Err") => vec![(**err_ty).clone()],
                            (Ty::Named(_, args), _) => {
                                // For user-defined enums, get field types from definition
                                // and substitute type args if needed
                                if let Some((_, tys)) = variants.iter().find(|(n, _)| n == &variant_name) {
                                    // TODO: Properly substitute type params with args
                                    // For now, use args directly if they match field count
                                    if !args.is_empty() && tys.len() <= args.len() {
                                        args.iter().take(tys.len()).cloned().collect()
                                    } else {
                                        tys.clone()
                                    }
                                } else {
                                    vec![]
                                }
                            }
                            _ => {
                                // Fall back to enum definition's field types
                                variants.iter()
                                    .find(|(n, _)| n == &variant_name)
                                    .map(|(_, tys)| tys.clone())
                                    .unwrap_or_default()
                            }
                        };

                        // Bind each field pattern to its CONCRETE type
                        for (field, concrete_ty) in fields.iter().zip(concrete_field_types.iter()) {
                            if let Some(p) = &field.pattern {
                                self.collect_pattern_bindings(p, concrete_ty, env)?;
                            } else {
                                // The field name is the binding
                                env.insert(field.name.name.clone(), TypeScheme::mono(concrete_ty.clone()));
                            }
                        }
                    }
                    _ => {
                        // Unknown type - fields won't be bound but don't error
                        // (error should be caught in check_pattern)
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
                    "isize" => Ok(Ty::Isize),
                    "usize" => Ok(Ty::Usize),
                    "Float" => Ok(Ty::Float),
                    "f32" => Ok(Ty::F32),
                    "f64" => Ok(Ty::F64),
                    "Bool" => Ok(Ty::Bool),
                    "Char" => Ok(Ty::Char),
                    "Str" | "String" => Ok(Ty::Str),
                    "Unit" => Ok(Ty::Unit),
                    "Json" => Ok(Ty::Json),
                    _ => Ok(Ty::Named(TypeId::new(name), args)),
                }
            }
            AstTypeKind::Tuple(elems) => {
                if elems.is_empty() {
                    return Ok(Ty::Unit);
                }
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

    /// Check if a type can be cast to another type.
    /// Returns true if the cast is valid.
    fn can_cast(&self, from: &Ty, to: &Ty) -> bool {
        // Same type is always valid
        if from == to {
            return true;
        }

        // Check if both are numeric types (int or float)
        let from_numeric = from.is_numeric();
        let to_numeric = to.is_numeric();

        // Numeric to numeric casts are always allowed
        if from_numeric && to_numeric {
            return true;
        }

        // Bool to int is allowed
        if matches!(from, Ty::Bool) && to.is_integer() {
            return true;
        }

        // Char to int is allowed
        if matches!(from, Ty::Char) && to.is_integer() {
            return true;
        }

        // Int to char is allowed (for ASCII conversion)
        if from.is_integer() && matches!(to, Ty::Char) {
            return true;
        }

        // Type variables - allow cast (will be resolved later)
        if matches!(from, Ty::Var(_)) || matches!(to, Ty::Var(_)) {
            return true;
        }

        false
    }

    /// Get the current environment.
    pub fn env(&self) -> &TypeEnv {
        &self.env
    }

    pub fn get_symbol_location(&self, name: &str) -> Option<(Span, super::checker::DefinitionKind)> {
        self.symbol_locations.get(name).copied()
    }

    /// Check whether an expression is a "place" (lvalue) that can be passed by reference.
    fn is_place_expr(expr: &Expr) -> bool {
        matches!(&expr.kind, ExprKind::Ident(_) | ExprKind::Field(_, _) | ExprKind::Index(_, _))
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
        for (i, row) in dp.iter_mut().enumerate().take(m + 1) { row[0] = i; }
        for (j, val) in dp[0].iter_mut().enumerate().take(n + 1) { *val = j; }

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
