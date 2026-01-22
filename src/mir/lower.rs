//! AST to MIR lowering.
//!
//! This module transforms the typed AST into MIR, which is a simpler
//! representation that's easier to interpret and compile.

use std::collections::HashMap;

use crate::lexer::Span;
use crate::parser::{
    BinOp as AstBinOp, Block as AstBlock, Expr, ExprKind, FnBody, Function as AstFunction,
    IfBranch, ElseBranch, Item, ItemKind, Literal, LiteralKind, Pattern, PatternKind,
    SourceFile, StmtKind, UnaryOp as AstUnaryOp,
};
use crate::types::Ty;

use super::mir::{
    BinOp, BlockId, Constant, Function, Local, Mutability, Operand, Program,
    Rvalue, Statement, StatementKind, Terminator, UnOp,
};

/// Error during lowering.
#[derive(Debug, Clone)]
pub struct LowerError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lowering error at line {}: {}", self.span.line, self.message)
    }
}

impl std::error::Error for LowerError {}

/// AST to MIR lowerer.
pub struct Lowerer {
    /// The program being built
    program: Program,
    /// Current function being lowered
    current_fn: Option<Function>,
    /// Current block being built
    current_block: Option<BlockId>,
    /// Variable name to local mapping
    vars: HashMap<String, Local>,
    /// Loop context for break/continue
    loop_stack: Vec<LoopContext>,
    /// Errors accumulated during lowering
    errors: Vec<LowerError>,
}

#[derive(Debug, Clone)]
struct LoopContext {
    continue_block: BlockId,
    break_block: BlockId,
    result_local: Option<Local>,
}

impl Lowerer {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            current_fn: None,
            current_block: None,
            vars: HashMap::new(),
            loop_stack: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Lower a source file to MIR.
    pub fn lower(mut self, source: &SourceFile) -> Result<Program, Vec<LowerError>> {
        for item in &source.items {
            self.lower_item(item);
        }

        // Look for main function
        if self.program.functions.contains_key("main") {
            self.program.entry = Some("main".to_string());
        }

        if self.errors.is_empty() {
            Ok(self.program)
        } else {
            Err(self.errors)
        }
    }

    fn lower_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Function(f) => {
                if let Some(mir_fn) = self.lower_function(f) {
                    self.program.functions.insert(mir_fn.name.clone(), mir_fn);
                }
            }
            ItemKind::Impl(impl_block) => {
                // Lower methods in impl block
                for impl_item in &impl_block.items {
                    if let crate::parser::ImplItem::Function(f) = impl_item {
                        if let Some(mir_fn) = self.lower_function(f) {
                            // Use qualified name for methods
                            let name = format!("{}::{}",
                                self.type_to_string(&impl_block.self_type),
                                mir_fn.name
                            );
                            self.program.functions.insert(name, mir_fn);
                        }
                    }
                }
            }
            // Other items don't generate MIR directly
            _ => {}
        }
    }

    fn type_to_string(&self, ty: &crate::parser::Type) -> String {
        match &ty.kind {
            crate::parser::TypeKind::Path(path) => {
                path.segments.iter()
                    .map(|s| s.name.name.clone())
                    .collect::<Vec<_>>()
                    .join("::")
            }
            _ => "Unknown".to_string(),
        }
    }

    fn lower_function(&mut self, f: &AstFunction) -> Option<Function> {
        // Skip functions without bodies (trait methods)
        let body = f.body.as_ref()?;

        // Reset state
        self.vars.clear();
        self.loop_stack.clear();

        // Determine return type
        let return_ty = f.return_type.as_ref()
            .map(|t| self.lower_type(t))
            .unwrap_or(Ty::Unit);

        // Create function
        let mut mir_fn = Function::new(f.name.name.clone(), vec![], return_ty.clone());

        // Add parameters
        for param in &f.params {
            let ty = self.lower_type(&param.ty);
            let local = mir_fn.add_local(ty.clone(), Some(param.name.name.clone()));
            mir_fn.params.push((local, ty));
            self.vars.insert(param.name.name.clone(), local);
        }

        // Create entry block
        let entry = mir_fn.add_block();
        mir_fn.entry_block = entry;

        self.current_fn = Some(mir_fn);
        self.current_block = Some(entry);

        // Lower body
        let result = match body {
            FnBody::Expr(expr) => self.lower_expr(expr),
            FnBody::Block(block) => self.lower_block(block),
        };

        // Add return
        if let Some(result) = result {
            let block = self.current_block.unwrap();
            if self.current_fn.as_ref().unwrap().block(block).terminator.is_none() {
                self.terminate(Terminator::Return(Some(result)));
            }
        } else {
            let block = self.current_block.unwrap();
            if self.current_fn.as_ref().unwrap().block(block).terminator.is_none() {
                self.terminate(Terminator::Return(None));
            }
        }

        self.current_fn.take()
    }

    fn lower_block(&mut self, block: &AstBlock) -> Option<Operand> {
        let mut last_value = None;

        for (i, stmt) in block.stmts.iter().enumerate() {
            let is_last = i == block.stmts.len() - 1;

            match &stmt.kind {
                StmtKind::Let(let_stmt) => {
                    let init = self.lower_expr(&let_stmt.init);
                    if let Some(op) = init {
                        self.bind_pattern(&let_stmt.pattern, op);
                    }
                    last_value = None;
                }
                StmtKind::Expr(expr) => {
                    let value = self.lower_expr(expr);
                    if is_last {
                        last_value = value;
                    }
                }
                StmtKind::Item(item) => {
                    self.lower_item(item);
                    last_value = None;
                }
                StmtKind::Empty => {
                    last_value = None;
                }
            }
        }

        last_value
    }

    fn lower_expr(&mut self, expr: &Expr) -> Option<Operand> {
        match &expr.kind {
            ExprKind::Literal(lit) => {
                Some(Operand::Constant(self.lower_literal(lit)))
            }

            ExprKind::Ident(ident) => {
                if let Some(&local) = self.vars.get(&ident.name) {
                    Some(Operand::Local(local))
                } else {
                    self.error(format!("undefined variable: {}", ident.name), expr.span);
                    None
                }
            }

            ExprKind::Binary(left, op, right) => {
                let l = self.lower_expr(left)?;
                let r = self.lower_expr(right)?;
                let bin_op = self.lower_bin_op(*op);
                let result = self.new_temp(Ty::Int); // TODO: proper type
                self.emit(StatementKind::Assign(result, Rvalue::BinaryOp(bin_op, l, r)));
                Some(Operand::Local(result))
            }

            ExprKind::Unary(op, operand) => {
                match op {
                    AstUnaryOp::Neg => {
                        let op = self.lower_expr(operand)?;
                        let result = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(result, Rvalue::UnaryOp(UnOp::Neg, op)));
                        Some(Operand::Local(result))
                    }
                    AstUnaryOp::Not => {
                        let op = self.lower_expr(operand)?;
                        let result = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(result, Rvalue::UnaryOp(UnOp::Not, op)));
                        Some(Operand::Local(result))
                    }
                    AstUnaryOp::Ref => {
                        if let ExprKind::Ident(ident) = &operand.kind {
                            if let Some(&local) = self.vars.get(&ident.name) {
                                let result = self.new_temp(Ty::Unit); // TODO: proper ref type
                                self.emit(StatementKind::Assign(
                                    result,
                                    Rvalue::Ref(local, Mutability::Immutable),
                                ));
                                return Some(Operand::Local(result));
                            }
                        }
                        self.lower_expr(operand)
                    }
                    AstUnaryOp::RefMut => {
                        if let ExprKind::Ident(ident) = &operand.kind {
                            if let Some(&local) = self.vars.get(&ident.name) {
                                let result = self.new_temp(Ty::Unit);
                                self.emit(StatementKind::Assign(
                                    result,
                                    Rvalue::Ref(local, Mutability::Mutable),
                                ));
                                return Some(Operand::Local(result));
                            }
                        }
                        self.lower_expr(operand)
                    }
                    AstUnaryOp::Deref => {
                        let op = self.lower_expr(operand)?;
                        let result = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(result, Rvalue::Deref(op)));
                        Some(Operand::Local(result))
                    }
                }
            }

            ExprKind::Call(callee, args) => {
                // Get function name
                let func_name = match &callee.kind {
                    ExprKind::Ident(ident) => ident.name.clone(),
                    ExprKind::Path(path) => {
                        path.segments.iter()
                            .map(|s| s.name.clone())
                            .collect::<Vec<_>>()
                            .join("::")
                    }
                    _ => {
                        self.error("invalid callee".to_string(), expr.span);
                        return None;
                    }
                };

                // Lower arguments
                let mut mir_args = Vec::new();
                for arg in args {
                    if let Some(op) = self.lower_expr(&arg.value) {
                        mir_args.push(op);
                    }
                }

                // Create call
                let result = self.new_temp(Ty::Int); // TODO: proper return type
                let next_block = self.new_block();
                self.terminate(Terminator::Call {
                    func: func_name,
                    args: mir_args,
                    dest: Some(result),
                    next: next_block,
                });
                self.current_block = Some(next_block);

                Some(Operand::Local(result))
            }

            ExprKind::MethodCall(receiver, method, args) => {
                // Lower receiver
                let recv = self.lower_expr(receiver)?;

                // Lower arguments
                let mut mir_args = vec![recv];
                for arg in args {
                    if let Some(op) = self.lower_expr(&arg.value) {
                        mir_args.push(op);
                    }
                }

                // Create call (method name will be resolved later)
                let result = self.new_temp(Ty::Int);
                let next_block = self.new_block();
                self.terminate(Terminator::Call {
                    func: method.name.clone(),
                    args: mir_args,
                    dest: Some(result),
                    next: next_block,
                });
                self.current_block = Some(next_block);

                Some(Operand::Local(result))
            }

            ExprKind::If(if_expr) => {
                self.lower_if(if_expr, expr.span)
            }

            ExprKind::Match(scrutinee, arms) => {
                self.lower_match(scrutinee, arms, expr.span)
            }

            ExprKind::For(pattern, iter, body) => {
                self.lower_for(pattern, iter, body, expr.span)
            }

            ExprKind::While(cond, body) => {
                self.lower_while(cond, body, expr.span)
            }

            ExprKind::Loop(body) => {
                self.lower_loop(body, expr.span)
            }

            ExprKind::Block(block) => {
                self.lower_block(block)
            }

            ExprKind::Return(value) => {
                let op = value.as_ref().and_then(|v| self.lower_expr(v));
                self.terminate(Terminator::Return(op));
                None
            }

            ExprKind::Break(_, value) => {
                if let Some(ctx) = self.loop_stack.last().cloned() {
                    if let Some(val) = value.as_ref().and_then(|v| self.lower_expr(v)) {
                        if let Some(result_local) = ctx.result_local {
                            self.emit(StatementKind::Assign(result_local, Rvalue::Use(val)));
                        }
                    }
                    self.terminate(Terminator::Goto(ctx.break_block));
                }
                None
            }

            ExprKind::Continue(_) => {
                if let Some(ctx) = self.loop_stack.last().cloned() {
                    self.terminate(Terminator::Goto(ctx.continue_block));
                }
                None
            }

            ExprKind::Tuple(elements) => {
                let mut ops = Vec::new();
                for elem in elements {
                    if let Some(op) = self.lower_expr(elem) {
                        ops.push(op);
                    }
                }
                let result = self.new_temp(Ty::Unit); // TODO: proper tuple type
                self.emit(StatementKind::Assign(result, Rvalue::Tuple(ops)));
                Some(Operand::Local(result))
            }

            ExprKind::Array(elements) => {
                let mut ops = Vec::new();
                for elem in elements {
                    if let Some(op) = self.lower_expr(elem) {
                        ops.push(op);
                    }
                }
                let result = self.new_temp(Ty::Unit); // TODO: proper array type
                self.emit(StatementKind::Assign(result, Rvalue::Array(ops)));
                Some(Operand::Local(result))
            }

            ExprKind::Index(base, index) => {
                let base_op = self.lower_expr(base)?;
                let index_op = self.lower_expr(index)?;
                let result = self.new_temp(Ty::Int); // TODO: proper element type
                self.emit(StatementKind::Assign(result, Rvalue::Index(base_op, index_op)));
                Some(Operand::Local(result))
            }

            ExprKind::Field(base, field) => {
                let base_op = self.lower_expr(base)?;
                let result = self.new_temp(Ty::Int); // TODO: proper field type
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::Field(base_op, field.name.clone()),
                ));
                Some(Operand::Local(result))
            }

            ExprKind::TupleField(base, index) => {
                let base_op = self.lower_expr(base)?;
                let result = self.new_temp(Ty::Int);
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::TupleField(base_op, *index),
                ));
                Some(Operand::Local(result))
            }

            ExprKind::Assign(target, value, _mutable) => {
                let val = self.lower_expr(value)?;

                // Handle assignment target
                if let ExprKind::Ident(ident) = &target.kind {
                    if let Some(&local) = self.vars.get(&ident.name) {
                        self.emit(StatementKind::Assign(local, Rvalue::Use(val)));
                        return Some(Operand::Local(local));
                    } else {
                        // New binding
                        let local = self.new_local(Ty::Int, Some(ident.name.clone()));
                        self.vars.insert(ident.name.clone(), local);
                        self.emit(StatementKind::Assign(local, Rvalue::Use(val)));
                        return Some(Operand::Local(local));
                    }
                }

                None
            }

            ExprKind::AssignOp(target, op, value) => {
                let target_op = self.lower_expr(target)?;
                let val = self.lower_expr(value)?;
                let bin_op = self.lower_bin_op(*op);

                if let ExprKind::Ident(ident) = &target.kind {
                    if let Some(&local) = self.vars.get(&ident.name) {
                        let result = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            result,
                            Rvalue::BinaryOp(bin_op, target_op, val),
                        ));
                        self.emit(StatementKind::Assign(local, Rvalue::Use(Operand::Local(result))));
                        return Some(Operand::Local(local));
                    }
                }

                None
            }

            ExprKind::Struct(path, fields, base) => {
                let name = path.segments.iter()
                    .map(|s| s.name.name.clone())
                    .collect::<Vec<_>>()
                    .join("::");

                let mut mir_fields = Vec::new();
                for field in fields {
                    let value = field.value.as_ref()
                        .and_then(|v| self.lower_expr(v))
                        .or_else(|| self.vars.get(&field.name.name).map(|&l| Operand::Local(l)));

                    if let Some(val) = value {
                        mir_fields.push((field.name.name.clone(), val));
                    }
                }

                // Handle base (struct update syntax)
                if let Some(_base_expr) = base {
                    // TODO: handle struct update
                }

                let result = self.new_temp(Ty::Unit); // TODO: proper struct type
                self.emit(StatementKind::Assign(result, Rvalue::Struct(name, mir_fields)));
                Some(Operand::Local(result))
            }

            ExprKind::Closure(closure) => {
                // For now, lower closure as a named function
                // TODO: proper closure handling with captures
                let _ = closure;
                self.error("closures not yet supported in MIR".to_string(), expr.span);
                None
            }

            ExprKind::Pipeline(left, right) => {
                // Pipeline `a | b` desugars to `b(a)`
                let arg = self.lower_expr(left)?;

                match &right.kind {
                    ExprKind::Ident(func_name) => {
                        let result = self.new_temp(Ty::Int);
                        let next_block = self.new_block();
                        self.terminate(Terminator::Call {
                            func: func_name.name.clone(),
                            args: vec![arg],
                            dest: Some(result),
                            next: next_block,
                        });
                        self.current_block = Some(next_block);
                        Some(Operand::Local(result))
                    }
                    ExprKind::Call(callee, extra_args) => {
                        if let ExprKind::Ident(func_name) = &callee.kind {
                            let mut args = vec![arg];
                            for extra in extra_args {
                                if let Some(op) = self.lower_expr(&extra.value) {
                                    args.push(op);
                                }
                            }
                            let result = self.new_temp(Ty::Int);
                            let next_block = self.new_block();
                            self.terminate(Terminator::Call {
                                func: func_name.name.clone(),
                                args,
                                dest: Some(result),
                                next: next_block,
                            });
                            self.current_block = Some(next_block);
                            return Some(Operand::Local(result));
                        }
                        None
                    }
                    _ => {
                        self.error("invalid pipeline target".to_string(), expr.span);
                        None
                    }
                }
            }

            ExprKind::Try(inner) => {
                // TODO: proper error handling
                self.lower_expr(inner)
            }

            ExprKind::Coalesce(left, right) => {
                // TODO: proper option handling
                let l = self.lower_expr(left);
                if l.is_some() {
                    l
                } else {
                    self.lower_expr(right)
                }
            }

            ExprKind::Paren(inner) => {
                self.lower_expr(inner)
            }

            ExprKind::Path(path) => {
                // Try to resolve as variable
                if path.segments.len() == 1 {
                    let name = &path.segments[0].name;
                    if let Some(&local) = self.vars.get(name) {
                        return Some(Operand::Local(local));
                    }
                }
                self.error(format!("unresolved path: {:?}", path), expr.span);
                None
            }

            // Not yet implemented
            _ => {
                self.error(format!("unsupported expression: {:?}", expr.kind), expr.span);
                None
            }
        }
    }

    fn lower_if(&mut self, if_expr: &crate::parser::IfExpr, span: Span) -> Option<Operand> {
        let cond = self.lower_expr(&if_expr.condition)?;

        let then_block = self.new_block();
        let else_block = self.new_block();
        let merge_block = self.new_block();

        // Result variable for if expression
        let result = self.new_temp(Ty::Int); // TODO: proper type

        // Branch on condition
        self.terminate(Terminator::If {
            cond,
            then_block,
            else_block,
        });

        // Then branch
        self.current_block = Some(then_block);
        let then_val = match &if_expr.then_branch {
            IfBranch::Expr(e) => self.lower_expr(e),
            IfBranch::Block(b) => self.lower_block(b),
        };
        if let Some(val) = then_val {
            self.emit(StatementKind::Assign(result, Rvalue::Use(val)));
        }
        if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
            self.terminate(Terminator::Goto(merge_block));
        }

        // Else branch
        self.current_block = Some(else_block);
        if let Some(else_branch) = &if_expr.else_branch {
            let else_val = match else_branch {
                ElseBranch::Expr(e) => self.lower_expr(e),
                ElseBranch::Block(b) => self.lower_block(b),
                ElseBranch::ElseIf(elif) => self.lower_if(elif, span),
            };
            if let Some(val) = else_val {
                self.emit(StatementKind::Assign(result, Rvalue::Use(val)));
            }
        }
        if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
            self.terminate(Terminator::Goto(merge_block));
        }

        // Continue at merge block
        self.current_block = Some(merge_block);
        Some(Operand::Local(result))
    }

    fn lower_match(
        &mut self,
        scrutinee: &Expr,
        arms: &[crate::parser::MatchArm],
        _span: Span,
    ) -> Option<Operand> {
        let scrutinee_val = self.lower_expr(scrutinee)?;
        let result = self.new_temp(Ty::Int);
        let exit_block = self.new_block();

        // For now, only handle simple integer patterns
        let mut targets = Vec::new();
        let mut default_arm = None;

        for arm in arms {
            let arm_block = self.new_block();

            match &arm.pattern.kind {
                PatternKind::Literal(Literal { kind: LiteralKind::Int(n), .. }) => {
                    targets.push((*n as i64, arm_block));
                }
                PatternKind::Wildcard => {
                    default_arm = Some(arm_block);
                }
                PatternKind::Ident(ident, _, _) => {
                    // Bind the variable and treat as default
                    self.current_block = Some(arm_block);
                    let local = self.new_local(Ty::Int, Some(ident.name.clone()));
                    self.vars.insert(ident.name.clone(), local);
                    self.emit(StatementKind::Assign(local, Rvalue::Use(scrutinee_val.clone())));

                    if default_arm.is_none() {
                        default_arm = Some(arm_block);
                    }
                }
                _ => {
                    // Unsupported pattern, treat as default
                    if default_arm.is_none() {
                        default_arm = Some(arm_block);
                    }
                }
            }

            // Lower arm body
            self.current_block = Some(arm_block);
            if let Some(val) = self.lower_expr(&arm.body) {
                self.emit(StatementKind::Assign(result, Rvalue::Use(val)));
            }
            self.terminate(Terminator::Goto(exit_block));
        }

        // Create switch
        let default = default_arm.unwrap_or(exit_block);
        self.current_block = Some(self.current_block.unwrap()); // Reset to pre-switch block

        // Actually, we need to switch from the original block
        // For simplicity, create simple if-else chain for now
        let check_block = self.new_block();
        self.terminate(Terminator::Goto(check_block));
        self.current_block = Some(check_block);

        if !targets.is_empty() {
            self.terminate(Terminator::Switch {
                operand: scrutinee_val,
                targets,
                default,
            });
        } else {
            self.terminate(Terminator::Goto(default));
        }

        self.current_block = Some(exit_block);
        Some(Operand::Local(result))
    }

    fn lower_for(
        &mut self,
        pattern: &Pattern,
        iter: &Expr,
        body: &AstBlock,
        _span: Span,
    ) -> Option<Operand> {
        // For now, assume iter is a range or array
        // TODO: proper iterator protocol

        let _iter_val = self.lower_expr(iter)?;

        let cond_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // Push loop context
        self.loop_stack.push(LoopContext {
            continue_block: cond_block,
            break_block: exit_block,
            result_local: None,
        });

        // Jump to condition check
        self.terminate(Terminator::Goto(cond_block));

        // Condition block (simplified - always true for now)
        self.current_block = Some(cond_block);
        // TODO: proper iteration
        self.terminate(Terminator::Goto(exit_block)); // Skip for now

        // Body block
        self.current_block = Some(body_block);

        // Bind loop variable
        if let PatternKind::Ident(ident, _, _) = &pattern.kind {
            let local = self.new_local(Ty::Int, Some(ident.name.clone()));
            self.vars.insert(ident.name.clone(), local);
        }

        self.lower_block(body);
        if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
            self.terminate(Terminator::Goto(cond_block));
        }

        self.loop_stack.pop();
        self.current_block = Some(exit_block);
        None
    }

    fn lower_while(
        &mut self,
        cond: &Expr,
        body: &AstBlock,
        _span: Span,
    ) -> Option<Operand> {
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // Push loop context
        self.loop_stack.push(LoopContext {
            continue_block: cond_block,
            break_block: exit_block,
            result_local: None,
        });

        // Jump to condition check
        self.terminate(Terminator::Goto(cond_block));

        // Condition block
        self.current_block = Some(cond_block);
        let cond_val = self.lower_expr(cond);
        if let Some(c) = cond_val {
            self.terminate(Terminator::If {
                cond: c,
                then_block: body_block,
                else_block: exit_block,
            });
        } else {
            self.terminate(Terminator::Goto(exit_block));
        }

        // Body block
        self.current_block = Some(body_block);
        self.lower_block(body);
        if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
            self.terminate(Terminator::Goto(cond_block));
        }

        self.loop_stack.pop();
        self.current_block = Some(exit_block);
        None
    }

    fn lower_loop(&mut self, body: &AstBlock, _span: Span) -> Option<Operand> {
        let body_block = self.new_block();
        let exit_block = self.new_block();
        let result = self.new_temp(Ty::Int);

        // Push loop context
        self.loop_stack.push(LoopContext {
            continue_block: body_block,
            break_block: exit_block,
            result_local: Some(result),
        });

        // Jump to body
        self.terminate(Terminator::Goto(body_block));

        // Body block
        self.current_block = Some(body_block);
        self.lower_block(body);
        if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
            self.terminate(Terminator::Goto(body_block));
        }

        self.loop_stack.pop();
        self.current_block = Some(exit_block);
        Some(Operand::Local(result))
    }

    fn bind_pattern(&mut self, pattern: &Pattern, value: Operand) {
        match &pattern.kind {
            PatternKind::Ident(ident, _mutable, _) => {
                // Check if variable already exists - if so, update it rather than shadow
                if let Some(&existing_local) = self.vars.get(&ident.name) {
                    self.emit(StatementKind::Assign(existing_local, Rvalue::Use(value)));
                } else {
                    let local = self.new_local(Ty::Int, Some(ident.name.clone()));
                    self.vars.insert(ident.name.clone(), local);
                    self.emit(StatementKind::Assign(local, Rvalue::Use(value)));
                }
            }
            PatternKind::Tuple(patterns) => {
                for (i, pat) in patterns.iter().enumerate() {
                    let elem = self.new_temp(Ty::Int);
                    self.emit(StatementKind::Assign(
                        elem,
                        Rvalue::TupleField(value.clone(), i),
                    ));
                    self.bind_pattern(pat, Operand::Local(elem));
                }
            }
            PatternKind::Wildcard => {
                // Ignore the value
            }
            _ => {
                // Unsupported pattern
            }
        }
    }

    fn lower_literal(&self, lit: &Literal) -> Constant {
        match &lit.kind {
            LiteralKind::Int(n) => Constant::Int(*n as i64),
            LiteralKind::Float(n) => Constant::Float(*n),
            LiteralKind::String(s) => Constant::Str(s.clone()),
            LiteralKind::Char(c) => Constant::Char(*c),
            LiteralKind::Bool(b) => Constant::Bool(*b),
            LiteralKind::None => Constant::Unit,
        }
    }

    fn lower_bin_op(&self, op: AstBinOp) -> BinOp {
        match op {
            AstBinOp::Add => BinOp::Add,
            AstBinOp::Sub => BinOp::Sub,
            AstBinOp::Mul => BinOp::Mul,
            AstBinOp::Div => BinOp::Div,
            AstBinOp::Mod => BinOp::Rem,
            AstBinOp::Eq => BinOp::Eq,
            AstBinOp::Ne => BinOp::Ne,
            AstBinOp::Lt => BinOp::Lt,
            AstBinOp::Le => BinOp::Le,
            AstBinOp::Gt => BinOp::Gt,
            AstBinOp::Ge => BinOp::Ge,
            AstBinOp::And => BinOp::And,
            AstBinOp::Or => BinOp::Or,
            AstBinOp::BitAnd => BinOp::BitAnd,
            AstBinOp::BitOr => BinOp::BitOr,
            AstBinOp::BitXor => BinOp::BitXor,
            AstBinOp::Shl => BinOp::Shl,
            AstBinOp::Shr => BinOp::Shr,
        }
    }

    fn lower_type(&self, _ty: &crate::parser::Type) -> Ty {
        // TODO: proper type lowering
        Ty::Int
    }

    // Helper methods

    fn new_temp(&mut self, ty: Ty) -> Local {
        let func = self.current_fn.as_mut().unwrap();
        func.add_local(ty, None)
    }

    fn new_local(&mut self, ty: Ty, name: Option<String>) -> Local {
        let func = self.current_fn.as_mut().unwrap();
        func.add_local(ty, name)
    }

    fn new_block(&mut self) -> BlockId {
        let func = self.current_fn.as_mut().unwrap();
        func.add_block()
    }

    fn emit(&mut self, kind: StatementKind) {
        let block = self.current_block.unwrap();
        let func = self.current_fn.as_mut().unwrap();
        func.block_mut(block).push(Statement { kind });
    }

    fn terminate(&mut self, term: Terminator) {
        let block = self.current_block.unwrap();
        let func = self.current_fn.as_mut().unwrap();
        func.block_mut(block).terminate(term);
    }

    fn error(&mut self, message: String, span: Span) {
        self.errors.push(LowerError { message, span });
    }
}

impl Default for Lowerer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Parser, Scanner};

    fn lower_source(source: &str) -> Result<Program, Vec<LowerError>> {
        let scanner = Scanner::new(source);
        let (tokens, _) = scanner.scan_all();
        let parser = Parser::new(&tokens);
        let ast = parser.parse().expect("parse should succeed");
        Lowerer::new().lower(&ast)
    }

    #[test]
    fn test_simple_function() {
        let program = lower_source("f add(a: Int, b: Int) -> Int = a + b").unwrap();
        assert!(program.functions.contains_key("add"));
        let func = &program.functions["add"];
        assert_eq!(func.params.len(), 2);
    }

    #[test]
    fn test_if_expression() {
        let program = lower_source("f max(a: Int, b: Int) -> Int = if a > b then a else b").unwrap();
        assert!(program.functions.contains_key("max"));
        // Should have multiple blocks for if/then/else
        assert!(program.functions["max"].blocks.len() >= 3);
    }

    #[test]
    fn test_while_loop() {
        let program = lower_source(
            r#"f countdown(n: Int) -> Int
    x := n
    wh x > 0
        x = x - 1
    x"#,
        ).unwrap();
        assert!(program.functions.contains_key("countdown"));
    }

    #[test]
    fn test_let_binding() {
        let program = lower_source(
            r#"f example() -> Int
    x = 42
    y = x + 1
    y"#,
        ).unwrap();
        assert!(program.functions.contains_key("example"));
    }
}
