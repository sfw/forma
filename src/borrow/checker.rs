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
    Block, Expr, ExprKind, FnBody, Item, ItemKind, Pattern, PatternKind, SourceFile, Stmt,
    StmtKind, Type as AstType, TypeKind as AstTypeKind, UnaryOp,
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
                write!(f, "cannot return reference to local variable `{}`", name)
            }
            BorrowErrorKind::BorrowWhileMutBorrow { name } => {
                write!(f, "cannot borrow `{}` while mutable borrow is active", name)
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
    /// Is this a reference type? (TODO: use for reference tracking)
    is_ref: bool,
    /// Reference parameters from which this value may be derived.
    /// An empty set on a reference value means local/unknown provenance.
    ref_origins: HashSet<String>,
    /// Where was it defined? (TODO: use for better error messages)
    #[allow(dead_code)]
    def_span: Span,
    /// Is this a parameter?
    is_param: bool,
}

/// Borrow checker context.
pub struct BorrowChecker {
    /// Variable states in current scope
    vars: HashMap<String, VarInfo>,
    /// Scope stack for nested blocks
    scope_stack: Vec<HashSet<String>>,
    /// Collected errors
    errors: Vec<BorrowError>,
    /// Whether we're in a function that returns a reference
    returns_ref: bool,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            scope_stack: vec![HashSet::new()],
            errors: Vec::new(),
            returns_ref: false,
        }
    }

    /// Get a mutable reference to the current scope.
    /// If the stack is empty (which should never happen), pushes a new empty scope and reports an error.
    fn current_scope_mut(&mut self) -> &mut HashSet<String> {
        if self.scope_stack.is_empty() {
            eprintln!("Internal error: empty scope stack in borrow checker");
            self.scope_stack.push(HashSet::new());
        }
        self.scope_stack.last_mut().unwrap()
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

    /// Run only the source-level checks that typed MIR cannot represent.
    /// Loan timing, moves, and initializedness are deliberately excluded here
    /// because the MIR analysis performs non-lexical control-flow dataflow.
    pub fn check_structural(&mut self, file: &SourceFile) -> Result<(), Vec<BorrowError>> {
        match self.check(file) {
            Ok(()) => Ok(()),
            Err(errors) => {
                let structural: Vec<_> = errors
                    .into_iter()
                    .filter(|error| {
                        matches!(
                            error.kind,
                            BorrowErrorKind::MutBorrowOfImmutable { .. }
                                | BorrowErrorKind::ReferenceInStruct { .. }
                                | BorrowErrorKind::ReferenceInCollection
                                | BorrowErrorKind::ReturnLocalReference { .. }
                        )
                    })
                    .collect();
                if structural.is_empty() {
                    Ok(())
                } else {
                    Err(structural)
                }
            }
        }
    }

    /// Check a single item.
    fn check_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Function(f) => {
                // Reset state for new function
                self.vars.clear();
                self.scope_stack = vec![HashSet::new()];

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

                    self.vars.insert(
                        name.clone(),
                        VarInfo {
                            state: VarState::Owned,
                            mutable: false, // params are immutable by default
                            is_ref,
                            ref_origins: if is_ref {
                                HashSet::from([name.clone()])
                            } else {
                                HashSet::new()
                            },
                            def_span: param.span,
                            is_param: true,
                        },
                    );
                    self.current_scope_mut().insert(name);
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
            if is_last
                && self.returns_ref
                && let StmtKind::Expr(expr) = &stmt.kind
            {
                self.check_return_ref(expr, fn_span);
                continue;
            }

            self.check_stmt(stmt);
        }

        self.pop_scope();
    }

    /// Check a statement.
    fn check_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Let(l) => {
                let inferred_origins = self.reference_origins(&l.init);
                // Check initializer first
                self.check_expr(&l.init);

                // Check if type is a reference
                let is_ref = l.ty.as_ref().map(|t| self.is_ref_type(t)).unwrap_or(false)
                    || inferred_origins.is_some();

                // Bind pattern
                self.bind_pattern(
                    &l.pattern,
                    l.mutable,
                    is_ref,
                    inferred_origins.unwrap_or_default(),
                    stmt.span,
                );
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
                            let elif_expr = Expr::new(ExprKind::If(elif.clone()), elif.span);
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

            ExprKind::For(_label, pattern, iter, body) => {
                self.check_expr(iter);
                self.push_scope();
                self.bind_pattern_for_match(pattern);
                self.check_block(body);
                self.pop_scope();
            }

            ExprKind::While(_label, cond, body) => {
                self.check_expr(cond);
                self.check_block(body);
            }

            ExprKind::WhileLet(_label, pattern, expr, body) => {
                self.check_expr(expr);
                self.push_scope();
                self.bind_pattern_for_match(pattern);
                self.check_block(body);
                self.pop_scope();
            }

            ExprKind::Loop(_label, body) => {
                self.check_block(body);
            }

            ExprKind::Block(block) => {
                self.check_block(block);
            }

            ExprKind::Closure(closure) => {
                self.push_scope();
                for param in &closure.params {
                    let is_ref = param
                        .ty
                        .as_ref()
                        .map(|t| self.is_ref_type(t))
                        .unwrap_or(false);
                    self.vars.insert(
                        param.name.name.clone(),
                        VarInfo {
                            state: VarState::Owned,
                            mutable: false,
                            is_ref,
                            ref_origins: if is_ref {
                                HashSet::from([param.name.name.clone()])
                            } else {
                                HashSet::new()
                            },
                            def_span: param.span,
                            is_param: true,
                        },
                    );
                    self.current_scope_mut().insert(param.name.name.clone());
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
                if let Some(name) = self.get_borrowed_name(target)
                    && let Some(info) = self.vars.get(&name)
                {
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

            ExprKind::Spawn(inner) => {
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
        if let Some(origins) = self.reference_origins(expr)
            && origins.is_empty()
        {
            let name = self
                .get_root_name(expr)
                .unwrap_or_else(|| "<temporary>".to_string());
            self.errors.push(
                BorrowError::new(BorrowErrorKind::ReturnLocalReference { name }, error_span)
                    .with_help("return value must be derived from a reference parameter"),
            );
        }
        self.check_expr(expr);
    }

    /// Compute the input-reference provenance of an expression. `None` means
    /// the expression is not known to be a reference; `Some(empty)` is a
    /// reference to local or otherwise non-returnable storage.
    fn reference_origins(&self, expr: &Expr) -> Option<HashSet<String>> {
        match &expr.kind {
            ExprKind::Ident(ident) => self
                .vars
                .get(&ident.name)
                .filter(|info| info.is_ref)
                .map(|info| info.ref_origins.clone()),
            ExprKind::Unary(UnaryOp::Ref | UnaryOp::RefMut, inner) => {
                let root = self.get_root_name(inner)?;
                let origins = self
                    .vars
                    .get(&root)
                    .filter(|info| info.is_ref)
                    .map(|info| info.ref_origins.clone())
                    .unwrap_or_default();
                Some(origins)
            }
            ExprKind::Unary(UnaryOp::Deref, inner)
            | ExprKind::Paren(inner)
            | ExprKind::Field(inner, _)
            | ExprKind::TupleField(inner, _)
            | ExprKind::Index(inner, _) => self.reference_origins(inner),
            ExprKind::If(if_expr) => {
                let mut origins = HashSet::new();
                let then_origins = match &if_expr.then_branch {
                    crate::parser::IfBranch::Expr(expr) => self.reference_origins(expr),
                    crate::parser::IfBranch::Block(block) => self.block_reference_origins(block),
                }?;
                if then_origins.is_empty() {
                    return Some(HashSet::new());
                }
                origins.extend(then_origins);
                let else_origins = match if_expr.else_branch.as_ref()? {
                    crate::parser::ElseBranch::Expr(expr) => self.reference_origins(expr),
                    crate::parser::ElseBranch::Block(block) => self.block_reference_origins(block),
                    crate::parser::ElseBranch::ElseIf(nested) => {
                        let nested = Expr::new(ExprKind::If(nested.clone()), nested.span);
                        self.reference_origins(&nested)
                    }
                }?;
                if else_origins.is_empty() {
                    return Some(HashSet::new());
                }
                origins.extend(else_origins);
                Some(origins)
            }
            ExprKind::Match(_, arms) => {
                let mut origins = HashSet::new();
                for arm in arms {
                    let arm_origins = self.reference_origins(&arm.body)?;
                    if arm_origins.is_empty() {
                        return Some(HashSet::new());
                    }
                    origins.extend(arm_origins);
                }
                Some(origins)
            }
            ExprKind::Block(block) => self.block_reference_origins(block),
            ExprKind::Call(_, args) => {
                let mut origins = HashSet::new();
                let mut saw_reference = false;
                for arg in args {
                    if let Some(arg_origins) = self.reference_origins(&arg.value) {
                        saw_reference = true;
                        if arg_origins.is_empty() {
                            return Some(HashSet::new());
                        }
                        origins.extend(arg_origins);
                    }
                }
                saw_reference.then_some(origins)
            }
            ExprKind::MethodCall(receiver, _, args) => {
                let mut origins = self.reference_origins(receiver).unwrap_or_default();
                let mut saw_reference = self.reference_origins(receiver).is_some();
                if saw_reference && origins.is_empty() {
                    return Some(HashSet::new());
                }
                for arg in args {
                    if let Some(arg_origins) = self.reference_origins(&arg.value) {
                        saw_reference = true;
                        if arg_origins.is_empty() {
                            return Some(HashSet::new());
                        }
                        origins.extend(arg_origins);
                    }
                }
                saw_reference.then_some(origins)
            }
            _ => None,
        }
    }

    fn block_reference_origins(&self, block: &Block) -> Option<HashSet<String>> {
        let last = block.stmts.last()?;
        let StmtKind::Expr(expr) = &last.kind else {
            return None;
        };
        self.reference_origins(expr)
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
        self.reference_origins(expr).is_some()
    }

    /// Bind a pattern (for let statements).
    fn bind_pattern(
        &mut self,
        pattern: &Pattern,
        mutable: bool,
        is_ref: bool,
        ref_origins: HashSet<String>,
        span: Span,
    ) {
        match &pattern.kind {
            PatternKind::Ident(ident, is_mut, _subpattern) => {
                let var_mutable = mutable || *is_mut;
                self.vars.insert(
                    ident.name.clone(),
                    VarInfo {
                        state: VarState::Owned,
                        mutable: var_mutable,
                        is_ref,
                        ref_origins: ref_origins.clone(),
                        def_span: span,
                        is_param: false,
                    },
                );
                self.current_scope_mut().insert(ident.name.clone());
            }
            PatternKind::Tuple(elems) => {
                for elem in elems {
                    self.bind_pattern(elem, mutable, is_ref, ref_origins.clone(), span);
                }
            }
            PatternKind::List(elems, rest) => {
                for elem in elems {
                    self.bind_pattern(elem, mutable, is_ref, ref_origins.clone(), span);
                }
                if let Some(r) = rest {
                    self.bind_pattern(r, mutable, is_ref, ref_origins.clone(), span);
                }
            }
            PatternKind::Struct(_, fields, _) => {
                for field in fields {
                    if let Some(p) = &field.pattern {
                        self.bind_pattern(p, mutable, is_ref, ref_origins.clone(), span);
                    } else {
                        // Shorthand: field name is the binding
                        self.vars.insert(
                            field.name.name.clone(),
                            VarInfo {
                                state: VarState::Owned,
                                mutable,
                                is_ref,
                                ref_origins: ref_origins.clone(),
                                def_span: span,
                                is_param: false,
                            },
                        );
                        self.current_scope_mut().insert(field.name.name.clone());
                    }
                }
            }
            PatternKind::Or(patterns) => {
                // All alternatives should bind the same names
                if let Some(first) = patterns.first() {
                    self.bind_pattern(first, mutable, is_ref, ref_origins, span);
                }
            }
            _ => {}
        }
    }

    /// Bind a pattern for match arms (always immutable).
    fn bind_pattern_for_match(&mut self, pattern: &Pattern) {
        self.bind_pattern(pattern, false, false, HashSet::new(), pattern.span);
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
