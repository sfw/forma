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
pub use checker::{TypeChecker, TypedAst, TypedItem, TypedItemKind, BoundsChecker, TypeRelations};
pub use inference::{InferenceEngine, TypeEnv, TypeError, Unifier, TypeDef};
pub use types::{
    Capability, EnvCapability, FileCapability, LinearityKind, Mutability, NetworkCapability,
    Substitution, Ty, TypeId, TypeScheme, TypeVar,
    EnumInfo, FunctionInfo, MethodInfo, StructInfo, TraitBound, TraitInfo, VariantFields, VariantInfo,
    fresh_type_var_id, reset_type_var_counter,
};
