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
    BinOp, Block, Expr, ExprKind, FnBody, GenericParam, Item, ItemKind,
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
}

impl InferenceEngine {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::with_builtins(),
            unifier: Unifier::new(),
            return_type: None,
        }
    }

    pub fn with_env(env: TypeEnv) -> Self {
        Self {
            env,
            unifier: Unifier::new(),
            return_type: None,
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

                self.env.insert_type(
                    s.name.name.clone(),
                    TypeDef::Struct { type_params, fields },
                );
            }
            ItemKind::Enum(e) => {
                let type_params = self.get_type_params(&e.generics);

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

                self.env.insert_type(
                    e.name.name.clone(),
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

            // Generalize the function type
            let env_vars = self.env.free_vars();
            let scheme = TypeScheme::generalize(fn_type, &env_vars);

            self.env.insert(f.name.name.clone(), scheme);
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
                    Err(TypeError::new(
                        format!("undefined variable: {}", name.name),
                        expr.span,
                    ))
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

                let elem_ty = Ty::fresh_var();
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                self.unifier.unify(&iter_ty, &list_ty, expr.span)?;

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
                    Ty::Named(TypeId::new("Error"), vec![])
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
