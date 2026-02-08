//! Type checker for FORMA.
//!
//! This module provides the high-level type checking API that coordinates
//! type inference, validation, and error reporting.

use std::collections::HashMap;

use crate::lexer::Span;
use crate::parser::{Item, ItemKind, SourceFile};

use super::inference::{InferenceEngine, TypeEnv, TypeError};
use super::types::{Ty, TypeId, TypeScheme};

/// Kinds of definitions that can be jumped to
#[derive(Debug, Clone, Copy)]
pub enum DefinitionKind {
    Function,
    Struct,
    Enum,
    Trait,
    TypeAlias,
    Variable,
    Parameter,
    EnumVariant,
}

/// Type checking context.
pub struct TypeChecker {
    /// The inference engine
    engine: InferenceEngine,
    /// Collected errors
    errors: Vec<TypeError>,
    /// Inferred types for expressions (by span)
    /// TODO: expose via API for IDE features (hover types, etc.)
    #[allow(dead_code)]
    expr_types: HashMap<Span, Ty>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            engine: InferenceEngine::new(),
            errors: Vec::new(),
            expr_types: HashMap::new(),
        }
    }

    /// Check a complete AST.
    pub fn check(&mut self, ast: &SourceFile) -> Result<TypedAst, Vec<TypeError>> {
        // Run type inference
        if let Err(e) = self.engine.infer_items(&ast.items) {
            self.errors.push(e);
        }

        if self.errors.is_empty() {
            Ok(self.build_typed_ast(ast))
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Check a single item.
    pub fn check_item(&mut self, item: &Item) -> Result<(), TypeError> {
        self.engine.infer_items(std::slice::from_ref(item))
    }

    /// Get the type of an identifier.
    pub fn type_of(&self, name: &str) -> Option<Ty> {
        self.engine.env().get(name).map(|s| s.instantiate())
    }

    /// Get the finalized type after substitution.
    pub fn finalize(&self, ty: &Ty) -> Ty {
        self.engine.finalize_type(ty)
    }

    /// Build a typed AST with resolved types.
    fn build_typed_ast(&self, ast: &SourceFile) -> TypedAst {
        let items = ast.items.iter().map(|item| self.type_item(item)).collect();

        TypedAst { items }
    }

    /// Add type information to an item.
    fn type_item(&self, item: &Item) -> TypedItem {
        match &item.kind {
            ItemKind::Function(f) => {
                let ty = self
                    .engine
                    .env()
                    .get(&f.name.name)
                    .map(|s| self.engine.finalize_type(&s.instantiate()))
                    .unwrap_or(Ty::Error);

                TypedItem {
                    kind: TypedItemKind::Function {
                        name: f.name.name.clone(),
                        ty,
                    },
                    span: item.span,
                }
            }
            ItemKind::Struct(s) => TypedItem {
                kind: TypedItemKind::Struct {
                    name: s.name.name.clone(),
                    ty: Ty::Named(TypeId::new(&s.name.name), vec![]),
                },
                span: item.span,
            },
            ItemKind::Enum(e) => TypedItem {
                kind: TypedItemKind::Enum {
                    name: e.name.name.clone(),
                    ty: Ty::Named(TypeId::new(&e.name.name), vec![]),
                },
                span: item.span,
            },
            ItemKind::Trait(t) => TypedItem {
                kind: TypedItemKind::Trait {
                    name: t.name.name.clone(),
                },
                span: item.span,
            },
            ItemKind::Impl(i) => TypedItem {
                kind: TypedItemKind::Impl {
                    trait_name: i.trait_.as_ref().and_then(|t| {
                        if let crate::parser::TypeKind::Path(p) = &t.kind {
                            p.segments.last().map(|s| s.name.name.clone())
                        } else {
                            None
                        }
                    }),
                },
                span: item.span,
            },
            ItemKind::TypeAlias(t) => TypedItem {
                kind: TypedItemKind::TypeAlias {
                    name: t.name.name.clone(),
                },
                span: item.span,
            },
            ItemKind::Use(_) => TypedItem {
                kind: TypedItemKind::Use,
                span: item.span,
            },
            ItemKind::Module(m) => TypedItem {
                kind: TypedItemKind::Module {
                    name: m.name.name.clone(),
                },
                span: item.span,
            },
            ItemKind::Const(c) => TypedItem {
                kind: TypedItemKind::Const {
                    name: c.name.name.clone(),
                },
                span: item.span,
            },
        }
    }

    /// Get collected errors.
    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }

    /// Get the type environment.
    pub fn env(&self) -> &TypeEnv {
        self.engine.env()
    }

    pub fn get_definition_location(&self, name: &str) -> Option<(Span, DefinitionKind)> {
        self.engine.get_symbol_location(name)
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// A typed AST with resolved types.
#[derive(Debug)]
pub struct TypedAst {
    pub items: Vec<TypedItem>,
}

/// A typed item.
#[derive(Debug)]
pub struct TypedItem {
    pub kind: TypedItemKind,
    pub span: Span,
}

/// Kind of typed item.
#[derive(Debug)]
pub enum TypedItemKind {
    Function { name: String, ty: Ty },
    Struct { name: String, ty: Ty },
    Enum { name: String, ty: Ty },
    Trait { name: String },
    Impl { trait_name: Option<String> },
    TypeAlias { name: String },
    Use,
    Module { name: String },
    Const { name: String },
}

/// Trait bounds checker.
pub struct BoundsChecker {
    /// Known trait implementations
    impls: HashMap<(TypeId, TypeId), Vec<Ty>>, // (type, trait) -> type args
}

impl BoundsChecker {
    pub fn new() -> Self {
        Self {
            impls: HashMap::new(),
        }
    }

    /// Register a trait implementation.
    pub fn register_impl(&mut self, ty: TypeId, trait_: TypeId, args: Vec<Ty>) {
        self.impls.insert((ty, trait_), args);
    }

    /// Check if a type implements a trait.
    pub fn implements(&self, ty: &TypeId, trait_: &TypeId) -> bool {
        self.impls.contains_key(&(ty.clone(), trait_.clone()))
    }

    /// Check trait bounds for a type.
    pub fn check_bounds(
        &self,
        ty: &Ty,
        bounds: &[(TypeId, Vec<Ty>)],
        span: Span,
    ) -> Result<(), TypeError> {
        for (trait_id, _args) in bounds {
            match ty {
                Ty::Named(type_id, _) => {
                    if !self.implements(type_id, trait_id) {
                        return Err(TypeError::new(
                            format!(
                                "type {} does not implement trait {}",
                                type_id.name, trait_id.name
                            ),
                            span,
                        ));
                    }
                }
                _ => {
                    // Primitive types have built-in trait impls
                    // For now, skip checking
                }
            }
        }
        Ok(())
    }
}

impl Default for BoundsChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper to check specific type relationships.
pub struct TypeRelations;

impl TypeRelations {
    /// Check if `from` can be coerced to `to`.
    pub fn can_coerce(from: &Ty, to: &Ty) -> bool {
        // Same type always coerces
        if from == to {
            return true;
        }

        match (from, to) {
            // Never coerces to anything
            (Ty::Never, _) => true,

            // Mutable ref can coerce to immutable ref
            (
                Ty::Ref(t1, super::types::Mutability::Mutable),
                Ty::Ref(t2, super::types::Mutability::Immutable),
            ) => t1 == t2,

            // Numeric widening (e.g., i32 -> i64)
            (Ty::I8, Ty::I16 | Ty::I32 | Ty::I64 | Ty::I128 | Ty::Int) => true,
            (Ty::I16, Ty::I32 | Ty::I64 | Ty::I128 | Ty::Int) => true,
            (Ty::I32, Ty::I64 | Ty::I128 | Ty::Int) => true,
            (Ty::I64, Ty::I128 | Ty::Int) => true,

            (Ty::U8, Ty::U16 | Ty::U32 | Ty::U64 | Ty::U128 | Ty::UInt) => true,
            (Ty::U16, Ty::U32 | Ty::U64 | Ty::U128 | Ty::UInt) => true,
            (Ty::U32, Ty::U64 | Ty::U128 | Ty::UInt) => true,
            (Ty::U64, Ty::U128 | Ty::UInt) => true,

            (Ty::F32, Ty::F64 | Ty::Float) => true,

            _ => false,
        }
    }

    /// Check if a type is sized (has known size at compile time).
    pub fn is_sized(ty: &Ty) -> bool {
        match ty {
            // Primitives are sized
            Ty::Int | Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::I128 => true,
            Ty::UInt | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::U128 => true,
            Ty::Float | Ty::F32 | Ty::F64 => true,
            Ty::Bool | Ty::Char | Ty::Unit | Ty::Never => true,

            // Tuples are sized if all elements are sized
            Ty::Tuple(elems) => elems.iter().all(Self::is_sized),

            // Arrays are sized
            Ty::Array(elem, _) => Self::is_sized(elem),

            // References and pointers are sized
            Ty::Ref(_, _) | Ty::Ptr(_, _) => true,

            // Lists, maps, sets are not sized (dynamic)
            Ty::List(_) | Ty::Map(_, _) | Ty::Set(_) => false,

            // Str is not sized (it's a slice)
            Ty::Str => false,

            // Named types depend on their definition
            Ty::Named(_, _) => true, // Assume sized for now

            // Type variables are assumed sized
            Ty::Var(_) => true,

            // Functions are sized (pointer-sized)
            Ty::Fn(_, _) => true,

            _ => true,
        }
    }

    /// Get the size of a type in bytes (if known).
    pub fn size_of(ty: &Ty) -> Option<usize> {
        match ty {
            Ty::Unit => Some(0),
            Ty::Bool | Ty::I8 | Ty::U8 => Some(1),
            Ty::I16 | Ty::U16 => Some(2),
            Ty::I32 | Ty::U32 | Ty::F32 | Ty::Char => Some(4),
            Ty::I64 | Ty::U64 | Ty::F64 => Some(8),
            Ty::I128 | Ty::U128 => Some(16),

            // Platform-dependent
            Ty::Int | Ty::UInt | Ty::Float => None, // Could be 4 or 8

            // Pointer-sized
            Ty::Ref(_, _) | Ty::Ptr(_, _) | Ty::Fn(_, _) => None,

            Ty::Tuple(elems) => {
                let sizes: Option<Vec<usize>> = elems.iter().map(Self::size_of).collect();
                sizes.map(|s| s.iter().sum())
            }

            Ty::Array(elem, n) => Self::size_of(elem).map(|s| s * n),

            _ => None,
        }
    }

    /// Get the alignment of a type in bytes (if known).
    pub fn align_of(ty: &Ty) -> Option<usize> {
        match ty {
            Ty::Unit => Some(1),
            Ty::Bool | Ty::I8 | Ty::U8 => Some(1),
            Ty::I16 | Ty::U16 => Some(2),
            Ty::I32 | Ty::U32 | Ty::F32 | Ty::Char => Some(4),
            Ty::I64 | Ty::U64 | Ty::F64 => Some(8),
            Ty::I128 | Ty::U128 => Some(16),

            Ty::Tuple(elems) => elems.iter().filter_map(Self::align_of).max(),

            Ty::Array(elem, _) => Self::align_of(elem),

            _ => None,
        }
    }
}

/// Type scheme operations for polymorphism.
pub struct Polymorphism;

impl Polymorphism {
    /// Check if a type is more general than another.
    /// A type A is more general than B if B can be obtained by
    /// substituting type variables in A.
    pub fn is_more_general(general: &TypeScheme, specific: &Ty) -> bool {
        // Simple check: instantiate and see if unification succeeds
        let instantiated = general.instantiate();
        let mut unifier = super::inference::Unifier::new();
        let span = Span::new(0, 0, 0, 0);
        unifier.unify(&instantiated, specific, span).is_ok()
    }

    /// Find the most general unifier of two types.
    pub fn mgu(t1: &Ty, t2: &Ty) -> Option<super::types::Substitution> {
        let mut unifier = super::inference::Unifier::new();
        let span = Span::new(0, 0, 0, 0);
        if unifier.unify(t1, t2, span).is_ok() {
            Some(unifier.into_substitution())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_can_coerce_same_type() {
        assert!(TypeRelations::can_coerce(&Ty::Int, &Ty::Int));
        assert!(TypeRelations::can_coerce(&Ty::Bool, &Ty::Bool));
    }

    #[test]
    fn test_can_coerce_never() {
        assert!(TypeRelations::can_coerce(&Ty::Never, &Ty::Int));
        assert!(TypeRelations::can_coerce(&Ty::Never, &Ty::Bool));
    }

    #[test]
    fn test_numeric_widening() {
        assert!(TypeRelations::can_coerce(&Ty::I8, &Ty::I16));
        assert!(TypeRelations::can_coerce(&Ty::I16, &Ty::I32));
        assert!(TypeRelations::can_coerce(&Ty::I32, &Ty::I64));
        assert!(!TypeRelations::can_coerce(&Ty::I64, &Ty::I32));
    }

    #[test]
    fn test_is_sized() {
        assert!(TypeRelations::is_sized(&Ty::Int));
        assert!(TypeRelations::is_sized(&Ty::Bool));
        assert!(TypeRelations::is_sized(&Ty::Tuple(vec![Ty::Int, Ty::Bool])));
        assert!(!TypeRelations::is_sized(&Ty::Str));
        assert!(!TypeRelations::is_sized(&Ty::List(Box::new(Ty::Int))));
    }

    #[test]
    fn test_size_of() {
        assert_eq!(TypeRelations::size_of(&Ty::Unit), Some(0));
        assert_eq!(TypeRelations::size_of(&Ty::Bool), Some(1));
        assert_eq!(TypeRelations::size_of(&Ty::I32), Some(4));
        assert_eq!(TypeRelations::size_of(&Ty::I64), Some(8));
        assert_eq!(
            TypeRelations::size_of(&Ty::Tuple(vec![Ty::I32, Ty::I32])),
            Some(8)
        );
    }
}
