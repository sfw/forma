//! Borrow checker for FORMA.
//!
//! Implements second-class reference checking:
//! - References cannot be stored in structs
//! - References cannot be stored in collections
//! - Only one mutable borrow at a time
//! - No mixed borrows (immutable + mutable)
//! - References can only be returned if derived from inputs

use std::collections::{HashMap, HashSet};

use crate::lexer::Span;
use crate::parser::{
    Block, Expr, ExprKind, FnBody, Item, ItemKind, Pattern, PatternKind, SourceFile,
    Stmt, StmtKind, Type as AstType, TypeKind as AstTypeKind, UnaryOp,
};

/// Borrow checking error.
#[derive(Debug, Clone)]
pub struct BorrowError {
    pub kind: BorrowErrorKind,
    pub span: Span,
    pub help: Option<String>,
}

impl BorrowError {
    pub fn new(kind: BorrowErrorKind, span: Span) -> Self {
        Self {
            kind,
            span,
            help: None,
        }
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
}

impl std::fmt::Display for BorrowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            BorrowErrorKind::UseAfterMove { name, moved_at } => {
                write!(
                    f,
                    "use of moved value `{}` (moved at line {})",
                    name, moved_at.line
                )
            }
            BorrowErrorKind::DoubleMutableBorrow { name } => {
                write!(f, "cannot borrow `{}` as mutable more than once", name)
            }
            BorrowErrorKind::MixedBorrow { name } => {
                write!(
                    f,
                    "cannot borrow `{}` as mutable while immutable borrow exists",
                    name
                )
            }
            BorrowErrorKind::MutBorrowOfImmutable { name } => {
                write!(f, "cannot borrow immutable variable `{}` as mutable", name)
            }
            BorrowErrorKind::ReferenceInStruct { field } => {
                write!(f, "cannot store reference in struct field `{}`", field)
            }
            BorrowErrorKind::ReferenceInCollection => {
                write!(f, "cannot store reference in collection")
            }
            BorrowErrorKind::ReturnLocalReference { name } => {
                write!(
                    f,
                    "cannot return reference to local variable `{}`",
                    name
                )
            }
            BorrowErrorKind::BorrowWhileMutBorrow { name } => {
                write!(
                    f,
                    "cannot borrow `{}` while mutable borrow is active",
                    name
                )
            }
            BorrowErrorKind::MoveWhileBorrowed { name } => {
                write!(f, "cannot move `{}` while borrowed", name)
            }
            BorrowErrorKind::AssignWhileBorrowed { name } => {
                write!(f, "cannot assign to `{}` while borrowed", name)
            }
            BorrowErrorKind::UseOfUninitialized { name } => {
                write!(f, "use of possibly uninitialized variable `{}`", name)
            }
        }
    }
}

impl std::error::Error for BorrowError {}

/// Kind of borrow error.
#[derive(Debug, Clone)]
pub enum BorrowErrorKind {
    UseAfterMove { name: String, moved_at: Span },
    DoubleMutableBorrow { name: String },
    MixedBorrow { name: String },
    MutBorrowOfImmutable { name: String },
    ReferenceInStruct { field: String },
    ReferenceInCollection,
    ReturnLocalReference { name: String },
    BorrowWhileMutBorrow { name: String },
    MoveWhileBorrowed { name: String },
    AssignWhileBorrowed { name: String },
    UseOfUninitialized { name: String },
}

/// State of a variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarState {
    /// Variable is valid and owned
    Owned,
    /// Variable has been moved
    Moved(Span),
    /// Variable is immutably borrowed
    ImmutBorrowed(u32), // borrow count
    /// Variable is mutably borrowed
    MutBorrowed,
    /// Variable is uninitialized
    Uninitialized,
}

/// Information about a variable.
#[derive(Debug, Clone)]
struct VarInfo {
    /// Current state
    state: VarState,
    /// Is the variable mutable?
    mutable: bool,
    /// Is this a reference type?
    is_ref: bool,
    /// Where was it defined?
    def_span: Span,
    /// Is this a parameter?
    is_param: bool,
    /// Is this a reference parameter (for return checking)?
    is_ref_param: bool,
}

/// Borrow checker context.
pub struct BorrowChecker {
    /// Variable states in current scope
    vars: HashMap<String, VarInfo>,
    /// Scope stack for nested blocks
    scope_stack: Vec<HashSet<String>>,
    /// Collected errors
    errors: Vec<BorrowError>,
    /// Function parameters that are references (for return checking)
    ref_params: HashSet<String>,
    /// Whether we're in a function that returns a reference
    returns_ref: bool,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            scope_stack: vec![HashSet::new()],
            errors: Vec::new(),
            ref_params: HashSet::new(),
            returns_ref: false,
        }
    }

    /// Check a complete source file.
    pub fn check(&mut self, file: &SourceFile) -> Result<(), Vec<BorrowError>> {
        for item in &file.items {
            self.check_item(item);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Check a single item.
    fn check_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Function(f) => {
                // Reset state for new function
                self.vars.clear();
                self.scope_stack = vec![HashSet::new()];
                self.ref_params.clear();

                // Check if return type is a reference
                self.returns_ref = f
                    .return_type
                    .as_ref()
                    .map(|t| self.is_ref_type(t))
                    .unwrap_or(false);

                // Add parameters to scope
                for param in &f.params {
                    let is_ref = self.is_ref_type(&param.ty);
                    let name = param.name.name.clone();

                    if is_ref {
                        self.ref_params.insert(name.clone());
                    }

                    self.vars.insert(
                        name.clone(),
                        VarInfo {
                            state: VarState::Owned,
                            mutable: false, // params are immutable by default
                            is_ref,
                            def_span: param.span,
                            is_param: true,
                            is_ref_param: is_ref,
                        },
                    );
                    self.scope_stack.last_mut().unwrap().insert(name);
                }

                // Check body
                if let Some(body) = &f.body {
                    match body {
                        FnBody::Expr(expr) => {
                            if self.returns_ref {
                                self.check_return_ref(expr, item.span);
                            } else {
                                self.check_expr(expr);
                            }
                        }
                        FnBody::Block(block) => {
                            self.check_block_with_return(block, item.span);
                        }
                    }
                }
            }
            ItemKind::Struct(s) => {
                // Check that struct fields don't contain references
                match &s.kind {
                    crate::parser::StructKind::Named(fields) => {
                        for field in fields {
                            if self.is_ref_type(&field.ty) {
                                self.errors.push(BorrowError::new(
                                    BorrowErrorKind::ReferenceInStruct {
                                        field: field.name.name.clone(),
                                    },
                                    field.span,
                                ));
                            }
                        }
                    }
                    crate::parser::StructKind::Tuple(types) => {
                        for (i, ty) in types.iter().enumerate() {
                            if self.is_ref_type(ty) {
                                self.errors.push(BorrowError::new(
                                    BorrowErrorKind::ReferenceInStruct {
                                        field: format!("{}", i),
                                    },
                                    ty.span,
                                ));
                            }
                        }
                    }
                    crate::parser::StructKind::Unit => {}
                }
            }
            ItemKind::Impl(i) => {
                for impl_item in &i.items {
                    if let crate::parser::ImplItem::Function(f) = impl_item {
                        let item = Item {
                            kind: ItemKind::Function(f.clone()),
                            attrs: vec![],
                            span: f.span,
                        };
                        self.check_item(&item);
                    }
                }
            }
            _ => {}
        }
    }

    /// Check a block.
    fn check_block(&mut self, block: &Block) {
        self.push_scope();

        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }

        self.pop_scope();
    }

    /// Check a block that's the body of a function, handling implicit returns.
    fn check_block_with_return(&mut self, block: &Block, fn_span: Span) {
        self.push_scope();

        let stmt_count = block.stmts.len();
        for (i, stmt) in block.stmts.iter().enumerate() {
            let is_last = i == stmt_count - 1;

            // For the last statement, if it's an expression and the function
            // returns a reference, check it as a return value
            if is_last && self.returns_ref {
                if let StmtKind::Expr(expr) = &stmt.kind {
                    self.check_return_ref(expr, fn_span);
                    continue;
                }
            }

            self.check_stmt(stmt);
        }

        self.pop_scope();
    }

    /// Check a statement.
    fn check_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Let(l) => {
                // Check initializer first
                self.check_expr(&l.init);

                // Check if type is a reference
                let is_ref = l.ty.as_ref().map(|t| self.is_ref_type(t)).unwrap_or(false);

                // Bind pattern
                self.bind_pattern(&l.pattern, l.mutable, is_ref, stmt.span);
            }
            StmtKind::Expr(expr) => {
                self.check_expr(expr);
            }
            StmtKind::Item(item) => {
                self.check_item(item);
            }
            StmtKind::Empty => {}
        }
    }

    /// Check an expression.
    fn check_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Ident(ident) => {
                // Using a variable
                self.check_use(&ident.name, expr.span);
            }

            ExprKind::Path(p) => {
                // First segment might be a variable
                if let Some(first) = p.segments.first() {
                    self.check_use(&first.name, expr.span);
                }
            }

            ExprKind::Binary(left, _op, right) => {
                self.check_expr(left);
                self.check_expr(right);
            }

            ExprKind::Unary(op, operand) => {
                match op {
                    UnaryOp::Ref => {
                        // Immutable borrow
                        if let Some(name) = self.get_borrowed_name(operand) {
                            self.check_immut_borrow(&name, expr.span);
                        }
                        self.check_expr(operand);
                    }
                    UnaryOp::RefMut => {
                        // Mutable borrow
                        if let Some(name) = self.get_borrowed_name(operand) {
                            self.check_mut_borrow(&name, expr.span);
                        }
                        self.check_expr(operand);
                    }
                    UnaryOp::Deref => {
                        self.check_expr(operand);
                    }
                    _ => {
                        self.check_expr(operand);
                    }
                }
            }

            ExprKind::Call(callee, args) => {
                self.check_expr(callee);
                for arg in args {
                    self.check_expr(&arg.value);
                }
            }

            ExprKind::MethodCall(receiver, _method, args) => {
                self.check_expr(receiver);
                for arg in args {
                    self.check_expr(&arg.value);
                }
            }

            ExprKind::Field(base, _field) => {
                self.check_expr(base);
            }

            ExprKind::TupleField(base, _index) => {
                self.check_expr(base);
            }

            ExprKind::Index(base, index) => {
                self.check_expr(base);
                self.check_expr(index);
            }

            ExprKind::Tuple(elems) => {
                for elem in elems {
                    self.check_expr(elem);
                }
            }

            ExprKind::Array(elems) => {
                // Check for references in array
                for elem in elems {
                    if self.expr_is_ref(elem) {
                        self.errors.push(BorrowError::new(
                            BorrowErrorKind::ReferenceInCollection,
                            elem.span,
                        ));
                    }
                    self.check_expr(elem);
                }
            }

            ExprKind::ArrayRepeat(elem, count) => {
                if self.expr_is_ref(elem) {
                    self.errors.push(BorrowError::new(
                        BorrowErrorKind::ReferenceInCollection,
                        elem.span,
                    ));
                }
                self.check_expr(elem);
                self.check_expr(count);
            }

            ExprKind::MapOrSet(entries) => {
                for entry in entries {
                    if self.expr_is_ref(&entry.key) {
                        self.errors.push(BorrowError::new(
                            BorrowErrorKind::ReferenceInCollection,
                            entry.key.span,
                        ));
                    }
                    self.check_expr(&entry.key);
                    if let Some(value) = &entry.value {
                        if self.expr_is_ref(value) {
                            self.errors.push(BorrowError::new(
                                BorrowErrorKind::ReferenceInCollection,
                                value.span,
                            ));
                        }
                        self.check_expr(value);
                    }
                }
            }

            ExprKind::Struct(_path, fields, base) => {
                for field in fields {
                    if let Some(value) = &field.value {
                        self.check_expr(value);
                    }
                }
                if let Some(b) = base {
                    self.check_expr(b);
                }
            }

            ExprKind::If(if_expr) => {
                self.check_expr(&if_expr.condition);
                match &if_expr.then_branch {
                    crate::parser::IfBranch::Expr(e) => self.check_expr(e),
                    crate::parser::IfBranch::Block(b) => self.check_block(b),
                }
                if let Some(else_branch) = &if_expr.else_branch {
                    match else_branch {
                        crate::parser::ElseBranch::Expr(e) => self.check_expr(e),
                        crate::parser::ElseBranch::Block(b) => self.check_block(b),
                        crate::parser::ElseBranch::ElseIf(elif) => {
                            let elif_expr = Expr::new(ExprKind::If(elif.clone()), elif.span,
                            );
                            self.check_expr(&elif_expr);
                        }
                    }
                }
            }

            ExprKind::Match(scrutinee, arms) => {
                self.check_expr(scrutinee);
                for arm in arms {
                    self.push_scope();
                    self.bind_pattern_for_match(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                    self.pop_scope();
                }
            }

            ExprKind::For(pattern, iter, body) => {
                self.check_expr(iter);
                self.push_scope();
                self.bind_pattern_for_match(pattern);
                self.check_block(body);
                self.pop_scope();
            }

            ExprKind::While(cond, body) => {
                self.check_expr(cond);
                self.check_block(body);
            }

            ExprKind::WhileLet(pattern, expr, body) => {
                self.check_expr(expr);
                self.push_scope();
                self.bind_pattern_for_match(pattern);
                self.check_block(body);
                self.pop_scope();
            }

            ExprKind::Loop(body) => {
                self.check_block(body);
            }

            ExprKind::Block(block) => {
                self.check_block(block);
            }

            ExprKind::Closure(closure) => {
                self.push_scope();
                for param in &closure.params {
                    let is_ref = param.ty.as_ref().map(|t| self.is_ref_type(t)).unwrap_or(false);
                    self.vars.insert(
                        param.name.name.clone(),
                        VarInfo {
                            state: VarState::Owned,
                            mutable: false,
                            is_ref,
                            def_span: param.span,
                            is_param: true,
                            is_ref_param: is_ref,
                        },
                    );
                    self.scope_stack.last_mut().unwrap().insert(param.name.name.clone());
                }
                self.check_expr(&closure.body);
                self.pop_scope();
            }

            ExprKind::Return(value) => {
                if let Some(v) = value {
                    if self.returns_ref {
                        self.check_return_ref(v, expr.span);
                    } else {
                        self.check_expr(v);
                    }
                }
            }

            ExprKind::Break(_, value) => {
                if let Some(v) = value {
                    self.check_expr(v);
                }
            }

            ExprKind::Assign(target, value, _mutable) => {
                // Check if target is borrowed
                if let Some(name) = self.get_borrowed_name(target) {
                    if let Some(info) = self.vars.get(&name) {
                        match info.state {
                            VarState::ImmutBorrowed(_) | VarState::MutBorrowed => {
                                self.errors.push(BorrowError::new(
                                    BorrowErrorKind::AssignWhileBorrowed { name: name.clone() },
                                    expr.span,
                                ));
                            }
                            _ => {}
                        }
                    }
                }
                self.check_expr(value);
            }

            ExprKind::AssignOp(target, _op, value) => {
                self.check_expr(target);
                self.check_expr(value);
            }

            ExprKind::Range(start, end, _inclusive) => {
                if let Some(s) = start {
                    self.check_expr(s);
                }
                if let Some(e) = end {
                    self.check_expr(e);
                }
            }

            ExprKind::Pipeline(left, right) => {
                self.check_expr(left);
                self.check_expr(right);
            }

            ExprKind::Paren(inner) => {
                self.check_expr(inner);
            }

            ExprKind::Try(inner) => {
                self.check_expr(inner);
            }

            ExprKind::Await(inner) => {
                self.check_expr(inner);
            }

            ExprKind::Coalesce(left, right) => {
                self.check_expr(left);
                self.check_expr(right);
            }

            ExprKind::Cast(inner, _ty) => {
                self.check_expr(inner);
            }

            ExprKind::Async(block) => {
                self.check_block(block);
            }

            ExprKind::Unsafe(block) => {
                self.check_block(block);
            }

            ExprKind::Literal(_) | ExprKind::Continue(_) => {}

            ExprKind::FieldShorthand(_) | ExprKind::OpShorthand(_, _, _) => {}
        }
    }

    /// Check that a returned reference is derived from a reference parameter.
    fn check_return_ref(&mut self, expr: &Expr, error_span: Span) {
        match &expr.kind {
            ExprKind::Ident(ident) => {
                // Must be a reference parameter
                if !self.ref_params.contains(&ident.name) {
                    if let Some(info) = self.vars.get(&ident.name) {
                        if !info.is_ref_param {
                            self.errors.push(
                                BorrowError::new(
                                    BorrowErrorKind::ReturnLocalReference {
                                        name: ident.name.clone(),
                                    },
                                    error_span,
                                )
                                .with_help("return value must be derived from a reference parameter"),
                            );
                        }
                    }
                }
            }
            ExprKind::Unary(UnaryOp::Ref | UnaryOp::RefMut, inner) => {
                // Check that inner is derived from ref param
                if let Some(name) = self.get_root_name(inner) {
                    if !self.ref_params.contains(&name) {
                        self.errors.push(
                            BorrowError::new(
                                BorrowErrorKind::ReturnLocalReference { name },
                                error_span,
                            )
                            .with_help("cannot return reference to local variable"),
                        );
                    }
                }
            }
            ExprKind::Field(base, _) | ExprKind::TupleField(base, _) => {
                // Field of a borrowed value is OK if base is from ref param
                self.check_return_ref(base, error_span);
            }
            ExprKind::Index(base, _) => {
                self.check_return_ref(base, error_span);
            }
            ExprKind::Paren(inner) => {
                self.check_return_ref(inner, error_span);
            }
            ExprKind::Block(block) => {
                // Check last expression in block
                if let Some(last) = block.stmts.last() {
                    if let StmtKind::Expr(e) = &last.kind {
                        self.check_return_ref(e, error_span);
                    }
                }
            }
            ExprKind::If(if_expr) => {
                // Both branches must return valid refs
                match &if_expr.then_branch {
                    crate::parser::IfBranch::Expr(e) => self.check_return_ref(e, error_span),
                    crate::parser::IfBranch::Block(b) => {
                        if let Some(last) = b.stmts.last() {
                            if let StmtKind::Expr(e) = &last.kind {
                                self.check_return_ref(e, error_span);
                            }
                        }
                    }
                }
                if let Some(else_branch) = &if_expr.else_branch {
                    match else_branch {
                        crate::parser::ElseBranch::Expr(e) => self.check_return_ref(e, error_span),
                        crate::parser::ElseBranch::Block(b) => {
                            if let Some(last) = b.stmts.last() {
                                if let StmtKind::Expr(e) = &last.kind {
                                    self.check_return_ref(e, error_span);
                                }
                            }
                        }
                        crate::parser::ElseBranch::ElseIf(elif) => {
                            let elif_expr = Expr::new(ExprKind::If(elif.clone()), elif.span,
                            );
                            self.check_return_ref(&elif_expr, error_span);
                        }
                    }
                }
            }
            ExprKind::Match(_, arms) => {
                for arm in arms {
                    self.check_return_ref(&arm.body, error_span);
                }
            }
            _ => {
                // Other expressions - just check normally
                self.check_expr(expr);
            }
        }
    }

    /// Check use of a variable.
    fn check_use(&mut self, name: &str, span: Span) {
        if let Some(info) = self.vars.get(name) {
            match &info.state {
                VarState::Moved(moved_span) => {
                    self.errors.push(
                        BorrowError::new(
                            BorrowErrorKind::UseAfterMove {
                                name: name.to_string(),
                                moved_at: *moved_span,
                            },
                            span,
                        )
                        .with_help("consider cloning the value before moving"),
                    );
                }
                VarState::Uninitialized => {
                    self.errors.push(BorrowError::new(
                        BorrowErrorKind::UseOfUninitialized {
                            name: name.to_string(),
                        },
                        span,
                    ));
                }
                _ => {}
            }
        }
    }

    /// Check immutable borrow.
    fn check_immut_borrow(&mut self, name: &str, span: Span) {
        if let Some(info) = self.vars.get(name) {
            match &info.state {
                VarState::MutBorrowed => {
                    self.errors.push(BorrowError::new(
                        BorrowErrorKind::BorrowWhileMutBorrow {
                            name: name.to_string(),
                        },
                        span,
                    ));
                }
                VarState::Moved(moved_span) => {
                    self.errors.push(BorrowError::new(
                        BorrowErrorKind::UseAfterMove {
                            name: name.to_string(),
                            moved_at: *moved_span,
                        },
                        span,
                    ));
                }
                _ => {
                    // Increment borrow count
                    let new_count = match info.state {
                        VarState::ImmutBorrowed(n) => n + 1,
                        _ => 1,
                    };
                    if let Some(info) = self.vars.get_mut(name) {
                        info.state = VarState::ImmutBorrowed(new_count);
                    }
                }
            }
        }
    }

    /// Check mutable borrow.
    fn check_mut_borrow(&mut self, name: &str, span: Span) {
        if let Some(info) = self.vars.get(name) {
            if !info.mutable && !info.is_param {
                self.errors.push(BorrowError::new(
                    BorrowErrorKind::MutBorrowOfImmutable {
                        name: name.to_string(),
                    },
                    span,
                ));
                return;
            }

            match &info.state {
                VarState::MutBorrowed => {
                    self.errors.push(BorrowError::new(
                        BorrowErrorKind::DoubleMutableBorrow {
                            name: name.to_string(),
                        },
                        span,
                    ));
                }
                VarState::ImmutBorrowed(_) => {
                    self.errors.push(BorrowError::new(
                        BorrowErrorKind::MixedBorrow {
                            name: name.to_string(),
                        },
                        span,
                    ));
                }
                VarState::Moved(moved_span) => {
                    self.errors.push(BorrowError::new(
                        BorrowErrorKind::UseAfterMove {
                            name: name.to_string(),
                            moved_at: *moved_span,
                        },
                        span,
                    ));
                }
                _ => {
                    if let Some(info) = self.vars.get_mut(name) {
                        info.state = VarState::MutBorrowed;
                    }
                }
            }
        }
    }

    /// Get the name being borrowed from an expression.
    fn get_borrowed_name(&self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Ident(ident) => Some(ident.name.clone()),
            ExprKind::Field(base, _) | ExprKind::TupleField(base, _) => {
                self.get_borrowed_name(base)
            }
            ExprKind::Index(base, _) => self.get_borrowed_name(base),
            ExprKind::Paren(inner) => self.get_borrowed_name(inner),
            ExprKind::Unary(UnaryOp::Deref, inner) => self.get_borrowed_name(inner),
            _ => None,
        }
    }

    /// Get the root variable name from an expression.
    fn get_root_name(&self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Ident(ident) => Some(ident.name.clone()),
            ExprKind::Path(p) => p.segments.first().map(|s| s.name.clone()),
            ExprKind::Field(base, _) | ExprKind::TupleField(base, _) => self.get_root_name(base),
            ExprKind::Index(base, _) => self.get_root_name(base),
            ExprKind::Paren(inner) => self.get_root_name(inner),
            ExprKind::Unary(_, inner) => self.get_root_name(inner),
            _ => None,
        }
    }

    /// Check if an expression produces a reference.
    fn expr_is_ref(&self, expr: &Expr) -> bool {
        matches!(
            &expr.kind,
            ExprKind::Unary(UnaryOp::Ref | UnaryOp::RefMut, _)
        )
    }

    /// Bind a pattern (for let statements).
    fn bind_pattern(&mut self, pattern: &Pattern, mutable: bool, is_ref: bool, span: Span) {
        match &pattern.kind {
            PatternKind::Ident(ident, is_mut, _subpattern) => {
                let var_mutable = mutable || *is_mut;
                self.vars.insert(
                    ident.name.clone(),
                    VarInfo {
                        state: VarState::Owned,
                        mutable: var_mutable,
                        is_ref,
                        def_span: span,
                        is_param: false,
                        is_ref_param: false,
                    },
                );
                self.scope_stack.last_mut().unwrap().insert(ident.name.clone());
            }
            PatternKind::Tuple(elems) => {
                for elem in elems {
                    self.bind_pattern(elem, mutable, is_ref, span);
                }
            }
            PatternKind::List(elems, rest) => {
                for elem in elems {
                    self.bind_pattern(elem, mutable, is_ref, span);
                }
                if let Some(r) = rest {
                    self.bind_pattern(r, mutable, is_ref, span);
                }
            }
            PatternKind::Struct(_, fields, _) => {
                for field in fields {
                    if let Some(p) = &field.pattern {
                        self.bind_pattern(p, mutable, is_ref, span);
                    } else {
                        // Shorthand: field name is the binding
                        self.vars.insert(
                            field.name.name.clone(),
                            VarInfo {
                                state: VarState::Owned,
                                mutable,
                                is_ref,
                                def_span: span,
                                is_param: false,
                                is_ref_param: false,
                            },
                        );
                        self.scope_stack.last_mut().unwrap().insert(field.name.name.clone());
                    }
                }
            }
            PatternKind::Or(patterns) => {
                // All alternatives should bind the same names
                if let Some(first) = patterns.first() {
                    self.bind_pattern(first, mutable, is_ref, span);
                }
            }
            _ => {}
        }
    }

    /// Bind a pattern for match arms (always immutable).
    fn bind_pattern_for_match(&mut self, pattern: &Pattern) {
        self.bind_pattern(pattern, false, false, pattern.span);
    }

    /// Check if a type is a reference type.
    fn is_ref_type(&self, ty: &AstType) -> bool {
        matches!(ty.kind, AstTypeKind::Ref(_, _))
    }

    /// Push a new scope.
    fn push_scope(&mut self) {
        self.scope_stack.push(HashSet::new());
    }

    /// Pop the current scope.
    fn pop_scope(&mut self) {
        if let Some(scope_vars) = self.scope_stack.pop() {
            // Release borrows and remove variables going out of scope
            for name in scope_vars {
                self.vars.remove(&name);
            }
        }
    }

    /// Get collected errors.
    pub fn errors(&self) -> &[BorrowError] {
        &self.errors
    }
}

impl Default for BorrowChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Parser, Scanner};

    fn check_source(source: &str) -> Result<(), Vec<BorrowError>> {
        let scanner = Scanner::new(source);
        let (tokens, _) = scanner.scan_all();
        let parser = Parser::new(&tokens);
        let ast = parser.parse().expect("parse should succeed");
        let mut checker = BorrowChecker::new();
        checker.check(&ast)
    }

    #[test]
    fn test_valid_function() {
        let result = check_source(
            r#"
f add(a: Int, b: Int) -> Int
    a + b
"#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_valid_borrow() {
        let result = check_source(
            r#"
f use_ref(data: &Int) -> Int
    *data
"#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_reference_in_struct_error() {
        let result = check_source(
            r#"
s Bad
    ref_field: &Int
"#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(matches!(
            errors[0].kind,
            BorrowErrorKind::ReferenceInStruct { .. }
        ));
    }

    #[test]
    fn test_valid_derived_ref_return() {
        let result = check_source(
            r#"
f first(data: &[Int]) -> &Int
    &data[0]
"#,
        );
        // This should be valid - returning ref derived from input
        assert!(result.is_ok());
    }
}
