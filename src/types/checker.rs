//! Type checker for FORMA.
//!
//! This module provides the high-level type checking API that coordinates
//! type inference, validation, and error reporting.

use std::collections::{HashMap, HashSet};

use crate::lexer::Span;
use crate::parser::{Item, ItemKind, SourceFile, Type, TypeKind, Visibility};

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
        let local_types: HashSet<_> = ast
            .items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Struct(item) => Some(item.name.name.clone()),
                ItemKind::Enum(item) => Some(item.name.name.clone()),
                ItemKind::TypeAlias(item) => Some(item.name.name.clone()),
                _ => None,
            })
            .collect();
        let local_traits: HashSet<_> = ast
            .items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Trait(item) => Some(item.name.name.clone()),
                _ => None,
            })
            .collect();
        let mut aggregate_fields: HashMap<String, Vec<Type>> = HashMap::new();
        let mut copy_requests: HashMap<String, Span> = HashMap::new();
        let mut clone_requests: HashMap<String, Span> = HashMap::new();
        let mut send_requests: HashMap<String, Span> = HashMap::new();
        let mut sync_requests: HashMap<String, Span> = HashMap::new();
        let mut drop_types = HashSet::new();
        for item in &ast.items {
            let derived: HashSet<_> = item
                .attrs
                .iter()
                .filter(|attribute| attribute.name.name == "derive")
                .flat_map(|attribute| attribute.args.iter().map(|arg| arg.name.name.as_str()))
                .collect();
            let aggregate = match &item.kind {
                ItemKind::Struct(structure) => Some((
                    structure.name.name.clone(),
                    match &structure.kind {
                        crate::parser::StructKind::Named(fields) => {
                            fields.iter().map(|field| field.ty.clone()).collect()
                        }
                        crate::parser::StructKind::Tuple(fields) => fields.clone(),
                        crate::parser::StructKind::Unit => Vec::new(),
                    },
                )),
                ItemKind::Enum(enumeration) => Some((
                    enumeration.name.name.clone(),
                    enumeration
                        .variants
                        .iter()
                        .flat_map(|variant| match &variant.kind {
                            crate::parser::VariantKind::Unit => Vec::new(),
                            crate::parser::VariantKind::Tuple(fields) => fields.clone(),
                            crate::parser::VariantKind::Named(fields) => {
                                fields.iter().map(|field| field.ty.clone()).collect()
                            }
                        })
                        .collect(),
                )),
                _ => None,
            };
            if let Some((name, fields)) = aggregate {
                aggregate_fields.insert(name.clone(), fields);
                if derived.contains("Copy") {
                    copy_requests.insert(name.clone(), item.span);
                }
                if derived.contains("Clone") {
                    clone_requests.insert(name.clone(), item.span);
                }
                if derived.contains("Drop") {
                    drop_types.insert(name.clone());
                }
                if derived.contains("Send") {
                    send_requests.insert(name.clone(), item.span);
                }
                if derived.contains("Sync") {
                    sync_requests.insert(name, item.span);
                }
            }
            if let ItemKind::Impl(implementation) = &item.kind
                && let Some(trait_type) = &implementation.trait_
            {
                let trait_name = ast_type_name(trait_type);
                let self_name = ast_type_name(&implementation.self_type);
                match trait_name.as_str() {
                    "Copy" => {
                        copy_requests.insert(self_name, item.span);
                    }
                    "Clone" => {
                        clone_requests.insert(self_name, item.span);
                    }
                    "Drop" => {
                        drop_types.insert(self_name);
                    }
                    "Send" => {
                        send_requests.insert(self_name, item.span);
                    }
                    "Sync" => {
                        sync_requests.insert(self_name, item.span);
                    }
                    _ => {}
                }
            }
        }
        validate_compiler_known_traits(
            &aggregate_fields,
            &copy_requests,
            &clone_requests,
            &drop_types,
            &mut self.errors,
        );
        validate_send_sync_traits(
            &aggregate_fields,
            &send_requests,
            &sync_requests,
            &mut self.errors,
        );
        let mut trait_impls: Vec<(CoherenceType, CoherenceType)> = Vec::new();
        for item in &ast.items {
            if let ItemKind::Function(function) = &item.kind
                && function.visibility == Visibility::Public
                && function.return_type.is_none()
            {
                self.errors.push(TypeError::new(
                    format!(
                        "public function `{}` must declare its return type",
                        function.name.name
                    ),
                    function.name.span,
                ));
            }
            if let ItemKind::Impl(implementation) = &item.kind
                && let Some(trait_type) = &implementation.trait_
            {
                let trait_name = ast_type_name(trait_type);
                let self_name = ast_type_name(&implementation.self_type);
                let parameters = generic_type_parameters(&implementation.generics);
                let trait_pattern = coherence_type(trait_type, &parameters);
                let self_pattern = coherence_type(&implementation.self_type, &parameters);
                if trait_impls.iter().any(|(existing_trait, existing_self)| {
                    coherence_types_overlap(existing_trait, &trait_pattern)
                        && coherence_types_overlap(existing_self, &self_pattern)
                }) {
                    self.errors.push(TypeError::new(
                        format!(
                            "conflicting implementations of trait `{trait_name}` for `{self_name}`"
                        ),
                        implementation.span,
                    ));
                } else {
                    trait_impls.push((trait_pattern, self_pattern));
                }
                if !local_traits.contains(&trait_name) && !local_types.contains(&self_name) {
                    self.errors.push(TypeError::new(
                        format!(
                            "orphan implementation: either trait `{trait_name}` or type `{self_name}` must be local"
                        ),
                        implementation.span,
                    ));
                }
            }
        }
        // Run type inference
        if let Err(e) = self.engine.infer_items(&ast.items) {
            self.errors.push(e);
        }
        self.expr_types = self
            .engine
            .expression_types()
            .iter()
            .map(|(span, ty)| (*span, self.engine.finalize_type(ty)))
            .collect();

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

    /// Final type of the narrowest expression containing a byte offset.
    pub fn type_at_offset(&self, offset: usize) -> Option<Ty> {
        self.expr_types
            .iter()
            .filter(|(span, _)| span.start <= offset && offset <= span.end)
            .min_by_key(|(span, _)| span.end.saturating_sub(span.start))
            .map(|(_, ty)| ty.clone())
    }

    pub fn expression_type(&self, span: Span) -> Option<Ty> {
        self.expr_types.get(&span).cloned()
    }

    pub fn expression_types(&self) -> &HashMap<Span, Ty> {
        &self.expr_types
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

fn ast_type_name(ty: &Type) -> String {
    match &ty.kind {
        TypeKind::Path(path) => path
            .segments
            .last()
            .map(|segment| segment.name.name.clone())
            .unwrap_or_else(|| "<unknown>".to_string()),
        _ => format!("{:?}", ty.kind),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CoherenceType {
    Any,
    Named(String, Vec<CoherenceType>),
    Tuple(Vec<CoherenceType>),
    List(Box<CoherenceType>),
    Map(Box<CoherenceType>, Box<CoherenceType>),
    Set(Box<CoherenceType>),
    Option(Box<CoherenceType>),
    Result(Box<CoherenceType>, Option<Box<CoherenceType>>),
    Ref(Box<CoherenceType>, bool),
    Ptr(Box<CoherenceType>, bool),
    Never,
}

fn generic_type_parameters(generics: &Option<crate::parser::Generics>) -> HashSet<String> {
    generics
        .iter()
        .flat_map(|generics| &generics.params)
        .filter_map(|parameter| match parameter {
            crate::parser::GenericParam::Type(parameter) => Some(parameter.name.name.clone()),
            crate::parser::GenericParam::Const(_) => None,
        })
        .collect()
}

fn coherence_type(ty: &Type, parameters: &HashSet<String>) -> CoherenceType {
    match &ty.kind {
        TypeKind::Path(path) => {
            let Some(segment) = path.segments.last() else {
                return CoherenceType::Any;
            };
            if parameters.contains(&segment.name.name) {
                return CoherenceType::Any;
            }
            let arguments = segment
                .args
                .iter()
                .flat_map(|arguments| &arguments.args)
                .map(|argument| match argument {
                    crate::parser::GenericArg::Type(ty) => coherence_type(ty, parameters),
                    crate::parser::GenericArg::Expr(_) => CoherenceType::Any,
                })
                .collect();
            CoherenceType::Named(segment.name.name.clone(), arguments)
        }
        TypeKind::Tuple(values) | TypeKind::Fn(values, _) => CoherenceType::Tuple(
            values
                .iter()
                .map(|value| coherence_type(value, parameters))
                .collect(),
        ),
        TypeKind::List(value) | TypeKind::Array(value, _) => {
            CoherenceType::List(Box::new(coherence_type(value, parameters)))
        }
        TypeKind::Map(key, value) => CoherenceType::Map(
            Box::new(coherence_type(key, parameters)),
            Box::new(coherence_type(value, parameters)),
        ),
        TypeKind::Set(value) => CoherenceType::Set(Box::new(coherence_type(value, parameters))),
        TypeKind::Option(value) => {
            CoherenceType::Option(Box::new(coherence_type(value, parameters)))
        }
        TypeKind::Result(ok, error) => CoherenceType::Result(
            Box::new(coherence_type(ok, parameters)),
            error
                .as_ref()
                .map(|error| Box::new(coherence_type(error, parameters))),
        ),
        TypeKind::Ref(value, mutable) => {
            CoherenceType::Ref(Box::new(coherence_type(value, parameters)), *mutable)
        }
        TypeKind::Ptr(value, mutable) => {
            CoherenceType::Ptr(Box::new(coherence_type(value, parameters)), *mutable)
        }
        TypeKind::Infer => CoherenceType::Any,
        TypeKind::Never => CoherenceType::Never,
    }
}

fn coherence_types_overlap(left: &CoherenceType, right: &CoherenceType) -> bool {
    use CoherenceType::*;
    match (left, right) {
        (Any, _) | (_, Any) => true,
        (Named(left_name, left_args), Named(right_name, right_args)) => {
            left_name == right_name
                && left_args.len() == right_args.len()
                && left_args
                    .iter()
                    .zip(right_args)
                    .all(|(left, right)| coherence_types_overlap(left, right))
        }
        (Tuple(left), Tuple(right)) => overlap_slices(left, right),
        (List(left), List(right)) | (Set(left), Set(right)) | (Option(left), Option(right)) => {
            coherence_types_overlap(left, right)
        }
        (Map(left_key, left_value), Map(right_key, right_value)) => {
            coherence_types_overlap(left_key, right_key)
                && coherence_types_overlap(left_value, right_value)
        }
        (Result(left_ok, left_error), Result(right_ok, right_error)) => {
            coherence_types_overlap(left_ok, right_ok)
                && match (left_error, right_error) {
                    (Some(left), Some(right)) => coherence_types_overlap(left, right),
                    (None, None) => true,
                    _ => false,
                }
        }
        (Ref(left, left_mut), Ref(right, right_mut))
        | (Ptr(left, left_mut), Ptr(right, right_mut)) => {
            left_mut == right_mut && coherence_types_overlap(left, right)
        }
        (Never, Never) => true,
        _ => false,
    }
}

fn overlap_slices(left: &[CoherenceType], right: &[CoherenceType]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| coherence_types_overlap(left, right))
}

fn validate_compiler_known_traits(
    aggregates: &HashMap<String, Vec<Type>>,
    copy_requests: &HashMap<String, Span>,
    clone_requests: &HashMap<String, Span>,
    drop_types: &HashSet<String>,
    errors: &mut Vec<TypeError>,
) {
    let mut valid_copy = HashSet::new();
    let mut valid_clone = HashSet::new();
    loop {
        let before = (valid_copy.len(), valid_clone.len());
        for name in clone_requests.keys() {
            if aggregates.get(name).is_some_and(|fields| {
                fields
                    .iter()
                    .all(|field| ast_type_has_property(field, &valid_clone, false))
            }) {
                valid_clone.insert(name.clone());
            }
        }
        for name in copy_requests.keys() {
            if !drop_types.contains(name)
                && aggregates.get(name).is_some_and(|fields| {
                    fields
                        .iter()
                        .all(|field| ast_type_has_property(field, &valid_copy, true))
                })
            {
                valid_copy.insert(name.clone());
            }
        }
        if before == (valid_copy.len(), valid_clone.len()) {
            break;
        }
    }

    for (name, span) in copy_requests {
        if drop_types.contains(name) {
            errors.push(TypeError::new(
                format!("type `{name}` implements `Drop` and therefore cannot implement `Copy`"),
                *span,
            ));
        } else if !valid_copy.contains(name) {
            errors.push(TypeError::new(
                format!("type `{name}` cannot implement `Copy` because a field is not `Copy`"),
                *span,
            ));
        }
    }
    for (name, span) in clone_requests {
        if !valid_clone.contains(name) {
            errors.push(TypeError::new(
                format!("type `{name}` cannot implement `Clone` because a field is not `Clone`"),
                *span,
            ));
        }
    }
}

fn ast_type_has_property(ty: &Type, valid_named: &HashSet<String>, copy: bool) -> bool {
    match &ty.kind {
        TypeKind::Path(path) => {
            let Some(segment) = path.segments.last() else {
                return false;
            };
            let primitive = matches!(
                segment.name.name.as_str(),
                "Int"
                    | "i8"
                    | "i16"
                    | "i32"
                    | "i64"
                    | "i128"
                    | "UInt"
                    | "u8"
                    | "u16"
                    | "u32"
                    | "u64"
                    | "u128"
                    | "isize"
                    | "usize"
                    | "Float"
                    | "f32"
                    | "f64"
                    | "Bool"
                    | "Char"
                    | "Unit"
            );
            let clone_only = !copy && segment.name.name == "Str";
            (primitive || clone_only || valid_named.contains(&segment.name.name))
                && segment.args.as_ref().is_none_or(|args| {
                    args.args.iter().all(|argument| match argument {
                        crate::parser::GenericArg::Type(ty) => {
                            ast_type_has_property(ty, valid_named, copy)
                        }
                        crate::parser::GenericArg::Expr(_) => true,
                    })
                })
        }
        TypeKind::Tuple(fields) => fields
            .iter()
            .all(|field| ast_type_has_property(field, valid_named, copy)),
        TypeKind::Array(element, _) | TypeKind::Option(element) => {
            ast_type_has_property(element, valid_named, copy)
        }
        TypeKind::Result(ok, err) => {
            ast_type_has_property(ok, valid_named, copy)
                && err
                    .as_ref()
                    .is_none_or(|err| ast_type_has_property(err, valid_named, copy))
        }
        TypeKind::List(element) | TypeKind::Set(element) if !copy => {
            ast_type_has_property(element, valid_named, false)
        }
        TypeKind::Map(key, value) if !copy => {
            ast_type_has_property(key, valid_named, false)
                && ast_type_has_property(value, valid_named, false)
        }
        TypeKind::Ref(_, mutable) => !*mutable,
        TypeKind::Ptr(_, _) | TypeKind::Never => true,
        _ => false,
    }
}

fn validate_send_sync_traits(
    aggregates: &HashMap<String, Vec<Type>>,
    send_requests: &HashMap<String, Span>,
    sync_requests: &HashMap<String, Span>,
    errors: &mut Vec<TypeError>,
) {
    for (label, requests) in [("Send", send_requests), ("Sync", sync_requests)] {
        let mut valid = HashSet::new();
        loop {
            let previous = valid.len();
            for name in requests.keys() {
                if aggregates.get(name).is_some_and(|fields| {
                    fields
                        .iter()
                        .all(|field| ast_type_is_task_safe(field, &valid))
                }) {
                    valid.insert(name.clone());
                }
            }
            if previous == valid.len() {
                break;
            }
        }
        for (name, span) in requests {
            if !valid.contains(name) {
                errors.push(TypeError::new(
                    format!(
                        "type `{name}` cannot implement `{label}` because a field is not task-safe"
                    ),
                    *span,
                ));
            }
        }
    }
}

fn ast_type_is_task_safe(ty: &Type, valid_named: &HashSet<String>) -> bool {
    match &ty.kind {
        TypeKind::Path(path) => {
            let Some(segment) = path.segments.last() else {
                return false;
            };
            if matches!(
                segment.name.name.as_str(),
                "TcpStream"
                    | "TcpListener"
                    | "UdpSocket"
                    | "TlsStream"
                    | "Database"
                    | "Statement"
                    | "MutexGuard"
            ) {
                return false;
            }
            (is_builtin_task_safe_type(&segment.name.name)
                || valid_named.contains(&segment.name.name))
                && segment.args.as_ref().is_none_or(|args| {
                    args.args.iter().all(|argument| match argument {
                        crate::parser::GenericArg::Type(ty) => {
                            ast_type_is_task_safe(ty, valid_named)
                        }
                        crate::parser::GenericArg::Expr(_) => true,
                    })
                })
        }
        TypeKind::Tuple(fields) => fields
            .iter()
            .all(|field| ast_type_is_task_safe(field, valid_named)),
        TypeKind::Array(element, _)
        | TypeKind::List(element)
        | TypeKind::Set(element)
        | TypeKind::Option(element) => ast_type_is_task_safe(element, valid_named),
        TypeKind::Map(key, value) | TypeKind::Result(key, Some(value)) => {
            ast_type_is_task_safe(key, valid_named) && ast_type_is_task_safe(value, valid_named)
        }
        TypeKind::Result(ok, None) => ast_type_is_task_safe(ok, valid_named),
        TypeKind::Ref(_, _) | TypeKind::Ptr(_, _) | TypeKind::Fn(_, _) => false,
        TypeKind::Never => true,
        TypeKind::Infer => false,
    }
}

fn is_builtin_task_safe_type(name: &str) -> bool {
    matches!(
        name,
        "Int"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "UInt"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "isize"
            | "usize"
            | "Float"
            | "f32"
            | "f64"
            | "Bool"
            | "Char"
            | "Unit"
            | "Str"
            | "Json"
            | "Sender"
            | "Receiver"
            | "Mutex"
            | "Task"
            | "Future"
    )
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
                Ty::Named(type_id, _) if !self.implements(type_id, trait_id) => {
                    return Err(TypeError::new(
                        format!(
                            "type {} does not implement trait {}",
                            type_id.name, trait_id.name
                        ),
                        span,
                    ));
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
