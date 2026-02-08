//! Type system for FORMA.
//!
//! This module provides:
//! - Type representations (`types`)
//! - Hindley-Milner type inference (`inference`)
//! - Type checking and validation (`checker`)
//!
//! # Architecture
//!
//! The type system uses constraint-based type inference with unification.
//! Key concepts:
//!
//! - **Type Variables**: Placeholders for unknown types during inference
//! - **Substitution**: Mapping from type variables to concrete types
//! - **Unification**: Algorithm to find a substitution making two types equal
//! - **Type Schemes**: Polymorphic types with universally quantified variables
//!
//! # Usage
//!
//! ```ignore
//! use forma::types::{TypeChecker, Ty};
//!
//! let mut checker = TypeChecker::new();
//! let result = checker.check(&ast)?;
//! ```

pub mod checker;
pub mod inference;
pub mod types;

// Re-export main types
pub use checker::{BoundsChecker, TypeChecker, TypeRelations, TypedAst, TypedItem, TypedItemKind};
pub use inference::{InferenceEngine, TypeDef, TypeEnv, TypeError, Unifier};
pub use types::{
    Capability, EnumInfo, EnvCapability, FileCapability, FunctionInfo, LinearityKind, MethodInfo,
    Mutability, NetworkCapability, StructInfo, Substitution, TraitBound, TraitInfo, Ty, TypeId,
    TypeScheme, TypeVar, VariantFields, VariantInfo, fresh_type_var_id, reset_type_var_counter,
};
