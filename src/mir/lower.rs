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
    BinOp, BlockId, Constant, Function, Local, MirContract, Mutability, Operand, Program,
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
    /// Enum variant to (enum_type_name, variant_fields_count) mapping
    enum_variants: HashMap<String, (String, usize)>,
    /// Counter for generating unique closure function names
    closure_counter: u32,
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
            enum_variants: HashMap::new(),
            closure_counter: 0,
        }
    }

    /// Lower a source file to MIR.
    pub fn lower(mut self, source: &SourceFile) -> Result<Program, Vec<LowerError>> {
        // First pass: collect type definitions (enums, structs) so we know about variants
        for item in &source.items {
            if let ItemKind::Enum(e) = &item.kind {
                let enum_name = e.name.name.clone();
                for variant in &e.variants {
                    let field_count = match &variant.kind {
                        crate::parser::VariantKind::Unit => 0,
                        crate::parser::VariantKind::Tuple(fields) => fields.len(),
                        crate::parser::VariantKind::Named(fields) => fields.len(),
                    };
                    self.enum_variants.insert(
                        variant.name.name.clone(),
                        (enum_name.clone(), field_count),
                    );
                }
            }
        }

        // Second pass: lower items (functions, impls, etc.)
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
            ItemKind::Enum(e) => {
                // Collect enum variants for later recognition
                let enum_name = e.name.name.clone();
                for variant in &e.variants {
                    let field_count = match &variant.kind {
                        crate::parser::VariantKind::Unit => 0,
                        crate::parser::VariantKind::Tuple(fields) => fields.len(),
                        crate::parser::VariantKind::Named(fields) => fields.len(),
                    };
                    self.enum_variants.insert(
                        variant.name.name.clone(),
                        (enum_name.clone(), field_count),
                    );
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

        // Copy contracts from AST to MIR
        let mut mir_fn = self.current_fn.take()?;
        for contract in &f.preconditions {
            mir_fn.preconditions.push(MirContract {
                expr_string: format!("{:?}", contract.condition), // TODO: pretty print
                message: contract.message.clone(),
            });
        }
        for contract in &f.postconditions {
            mir_fn.postconditions.push(MirContract {
                expr_string: format!("{:?}", contract.condition),
                message: contract.message.clone(),
            });
        }
        Some(mir_fn)
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
                    // Check if it's a unit enum variant (like None)
                    match ident.name.as_str() {
                        "None" => {
                            let result = self.new_temp(Ty::Option(Box::new(Ty::Unit)));
                            self.emit(StatementKind::Assign(
                                result,
                                Rvalue::Enum {
                                    type_name: "Option".to_string(),
                                    variant: "None".to_string(),
                                    fields: vec![],
                                },
                            ));
                            return Some(Operand::Local(result));
                        }
                        _ => {
                            // Check for user-defined unit enum variants
                            if let Some((enum_name, field_count)) = self.enum_variants.get(&ident.name).cloned() {
                                if field_count == 0 {
                                    // Unit variant
                                    let result = self.new_temp(Ty::Named(
                                        crate::types::TypeId::new(&enum_name),
                                        vec![],
                                    ));
                                    self.emit(StatementKind::Assign(
                                        result,
                                        Rvalue::Enum {
                                            type_name: enum_name,
                                            variant: ident.name.clone(),
                                            fields: vec![],
                                        },
                                    ));
                                    return Some(Operand::Local(result));
                                }
                            }

                            // Check for similar variable names to provide helpful suggestions
                            let similar = self.find_similar_name(&ident.name);
                            let msg = if let Some(suggestion) = similar {
                                format!(
                                    "undefined variable: `{}`. Did you mean `{}`?",
                                    ident.name, suggestion
                                )
                            } else {
                                format!("undefined variable: `{}`", ident.name)
                            };
                            self.error(msg, expr.span);
                            None
                        }
                    }
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
                // Check if this is an enum constructor call like Some(x) or Ok(x)
                if let ExprKind::Ident(ident) = &callee.kind {
                    let (is_enum, type_name, variant) = match ident.name.as_str() {
                        "Some" => (true, "Option".to_string(), "Some".to_string()),
                        "Ok" => (true, "Result".to_string(), "Ok".to_string()),
                        "Err" => (true, "Result".to_string(), "Err".to_string()),
                        _ => (false, String::new(), String::new()),
                    };

                    if is_enum {
                        // Lower arguments as enum fields
                        let field_operands: Vec<Operand> = args
                            .iter()
                            .filter_map(|arg| self.lower_expr(&arg.value))
                            .collect();

                        let result = self.new_temp(Ty::Named(
                            crate::types::TypeId::new(type_name.clone()),
                            vec![],
                        ));
                        self.emit(StatementKind::Assign(
                            result,
                            Rvalue::Enum {
                                type_name,
                                variant,
                                fields: field_operands,
                            },
                        ));
                        return Some(Operand::Local(result));
                    }
                }

                // Check if callee is a path like EnumType::Variant(args)
                if let ExprKind::Path(path) = &callee.kind {
                    if path.segments.len() == 2 {
                        let type_name = &path.segments[0].name;
                        let variant = &path.segments[1].name;

                        // Lower arguments as enum fields
                        let field_operands: Vec<Operand> = args
                            .iter()
                            .filter_map(|arg| self.lower_expr(&arg.value))
                            .collect();

                        let result = self.new_temp(Ty::Named(
                            crate::types::TypeId::new(type_name.clone()),
                            vec![],
                        ));
                        self.emit(StatementKind::Assign(
                            result,
                            Rvalue::Enum {
                                type_name: type_name.clone(),
                                variant: variant.clone(),
                                fields: field_operands,
                            },
                        ));
                        return Some(Operand::Local(result));
                    }
                }

                // Determine if this is a direct function call or an indirect call (closure/HOF)
                let (is_direct, func_name) = match &callee.kind {
                    ExprKind::Ident(ident) => {
                        // Check if this identifier is a known function or a variable (closure)
                        let name = &ident.name;
                        if self.program.functions.contains_key(name)
                            || self.is_builtin(name)
                            || !self.vars.contains_key(name)
                        {
                            // Known function or unknown (will be resolved later)
                            (true, Some(name.clone()))
                        } else {
                            // Variable holding a closure - needs indirect call
                            (false, None)
                        }
                    }
                    ExprKind::Path(path) => {
                        let name = path.segments.iter()
                            .map(|s| s.name.clone())
                            .collect::<Vec<_>>()
                            .join("::");
                        (true, Some(name))
                    }
                    _ => (false, None),  // Expression that evaluates to closure
                };

                // Lower arguments
                let mut mir_args = Vec::new();
                for arg in args {
                    if let Some(op) = self.lower_expr(&arg.value) {
                        mir_args.push(op);
                    }
                }

                let result = self.new_temp(Ty::Int); // TODO: proper return type
                let next_block = self.new_block();

                if is_direct {
                    // Direct function call
                    self.terminate(Terminator::Call {
                        func: func_name.unwrap(),
                        args: mir_args,
                        dest: Some(result),
                        next: next_block,
                    });
                } else {
                    // Indirect call through closure variable
                    let callee_op = self.lower_expr(callee)?;
                    self.terminate(Terminator::CallIndirect {
                        callee: callee_op,
                        args: mir_args,
                        dest: Some(result),
                        next: next_block,
                    });
                }
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

                // Resolve method name to built-in function or qualified name
                let func_name = self.resolve_method(&method.name);

                // Create call
                let result = self.new_temp(Ty::Int);
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
                // Lambda lifting: convert closure to a top-level function

                // Generate unique name for the lifted function
                let func_name = format!("__closure_{}", self.closure_counter);
                self.closure_counter += 1;

                // Find free variables (captured from enclosing scope)
                let param_names: std::collections::HashSet<_> = closure.params.iter()
                    .map(|p| p.name.name.clone())
                    .collect();
                let free_vars = self.find_free_vars(&closure.body, &param_names);

                // Build captured value operands (from current scope)
                let mut captures: Vec<Operand> = Vec::new();
                for var_name in &free_vars {
                    if let Some(&local) = self.vars.get(var_name) {
                        captures.push(Operand::Copy(local));
                    }
                }

                // Save current lowering state
                let saved_fn = self.current_fn.take();
                let saved_block = self.current_block.take();
                let saved_vars = std::mem::take(&mut self.vars);

                // Create the lifted function with signature: fn(captures..., params...)
                let mut params: Vec<(Local, Ty)> = Vec::new();
                let mut new_fn = Function::new(func_name.clone(), vec![], Ty::Int);

                // Add captured variables as parameters first
                for var_name in &free_vars {
                    let local = new_fn.add_local(Ty::Int, Some(var_name.clone()));
                    params.push((local, Ty::Int));
                    self.vars.insert(var_name.clone(), local);
                }

                // Add closure parameters
                for param in &closure.params {
                    let local = new_fn.add_local(Ty::Int, Some(param.name.name.clone()));
                    params.push((local, Ty::Int));
                    self.vars.insert(param.name.name.clone(), local);
                }

                new_fn.params = params;

                // Create entry block
                let entry = new_fn.add_block();
                self.current_fn = Some(new_fn);
                self.current_block = Some(entry);

                // Lower closure body
                if let Some(result) = self.lower_expr(&closure.body) {
                    self.terminate(Terminator::Return(Some(result)));
                } else {
                    self.terminate(Terminator::Return(None));
                }

                // Finalize and add the lifted function
                if let Some(finished_fn) = self.current_fn.take() {
                    self.program.functions.insert(func_name.clone(), finished_fn);
                }

                // Restore lowering state
                self.current_fn = saved_fn;
                self.current_block = saved_block;
                self.vars = saved_vars;

                // Create closure value with captured variables
                let result = self.new_temp(Ty::Int); // TODO: proper closure type
                self.emit(StatementKind::Assign(result, Rvalue::Closure {
                    func_name,
                    captures,
                }));
                Some(Operand::Local(result))
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
                // Try operator: expr? - early return on None/Err
                // Desugars to: match expr { Some(v) -> v, None -> return None }
                //          or: match expr { Ok(v) -> v, Err(e) -> return Err(e) }

                let inner_val = self.lower_expr(inner)?;

                // Store the value
                let scrutinee = self.new_temp(Ty::Int);
                self.emit(StatementKind::Assign(scrutinee, Rvalue::Use(inner_val)));

                // Get discriminant to check if Some/Ok vs None/Err
                let disc = self.new_temp(Ty::Int);
                self.emit(StatementKind::Assign(disc, Rvalue::Discriminant(scrutinee)));

                // Create blocks
                let some_ok_block = self.new_block();  // Some or Ok
                let none_err_block = self.new_block(); // None or Err
                let continue_block = self.new_block();

                // Switch on discriminant:
                // For Option: None=0, Some=1
                // For Result: Ok=0, Err=1
                // We check: if discriminant == 0, it's None (for Option) or Ok (for Result)
                // Since we can't know the type statically, we use a heuristic:
                // Check if discriminant == 1 (Some) for Option, or == 0 (Ok) for Result
                // Actually, let's use: discriminant == 1 means it has a value (Some), else (None) return

                // Branch: discriminant > 0 means Some (has value)
                let has_value = self.new_temp(Ty::Bool);
                self.emit(StatementKind::Assign(
                    has_value,
                    Rvalue::BinaryOp(BinOp::Gt, Operand::Copy(disc), Operand::Constant(Constant::Int(0))),
                ));
                self.terminate(Terminator::If {
                    cond: Operand::Local(has_value),
                    then_block: some_ok_block,
                    else_block: none_err_block,
                });

                // None/Err block: return None (for now, just return the original value)
                self.current_block = Some(none_err_block);
                self.terminate(Terminator::Return(Some(Operand::Copy(scrutinee))));

                // Some/Ok block: extract the value and continue
                self.current_block = Some(some_ok_block);
                let extracted = self.new_temp(Ty::Int);
                self.emit(StatementKind::Assign(extracted, Rvalue::EnumField(scrutinee, 0)));
                self.terminate(Terminator::Goto(continue_block));

                self.current_block = Some(continue_block);
                Some(Operand::Local(extracted))
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
                // Try to resolve as variable first
                if path.segments.len() == 1 {
                    let name = &path.segments[0].name;
                    if let Some(&local) = self.vars.get(name) {
                        return Some(Operand::Local(local));
                    }
                    // Check if it's a unit enum variant (like None)
                    match name.as_str() {
                        "None" => {
                            let result = self.new_temp(Ty::Option(Box::new(Ty::Unit)));
                            self.emit(StatementKind::Assign(
                                result,
                                Rvalue::Enum {
                                    type_name: "Option".to_string(),
                                    variant: "None".to_string(),
                                    fields: vec![],
                                },
                            ));
                            return Some(Operand::Local(result));
                        }
                        _ => {}
                    }
                }
                // Check if this is an enum variant path (like Color::Red)
                if path.segments.len() == 2 {
                    let type_name = &path.segments[0].name;
                    let variant = &path.segments[1].name;

                    // Create unit variant
                    let result = self.new_temp(Ty::Named(
                        crate::types::TypeId::new(type_name.clone()),
                        vec![],
                    ));
                    self.emit(StatementKind::Assign(
                        result,
                        Rvalue::Enum {
                            type_name: type_name.clone(),
                            variant: variant.clone(),
                            fields: vec![],
                        },
                    ));
                    return Some(Operand::Local(result));
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
        // Always assign to result, using Unit if the branch produces no value
        // This ensures the result variable is always defined on all paths
        let then_operand = then_val.unwrap_or(Operand::Constant(Constant::Unit));
        self.emit(StatementKind::Assign(result, Rvalue::Use(then_operand)));
        if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
            self.terminate(Terminator::Goto(merge_block));
        }

        // Else branch
        self.current_block = Some(else_block);
        let else_val = if let Some(else_branch) = &if_expr.else_branch {
            match else_branch {
                ElseBranch::Expr(e) => self.lower_expr(e),
                ElseBranch::Block(b) => self.lower_block(b),
                ElseBranch::ElseIf(elif) => self.lower_if(elif, span),
            }
        } else {
            None
        };
        // Always assign to result, using Unit if the branch produces no value
        let else_operand = else_val.unwrap_or(Operand::Constant(Constant::Unit));
        self.emit(StatementKind::Assign(result, Rvalue::Use(else_operand)));
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
        let scrutinee_op = self.lower_expr(scrutinee)?;

        // Store scrutinee in a local for repeated access
        let scrut_local = self.new_temp(Ty::Unit);
        self.emit(StatementKind::Assign(scrut_local, Rvalue::Use(scrutinee_op)));

        let result = self.new_temp(Ty::Unit);
        let exit_block = self.new_block();

        // Collect arm info for processing
        let mut arm_blocks: Vec<(BlockId, BlockId)> = Vec::new(); // (test_block, body_block)

        for _ in arms {
            let test_block = self.new_block();
            let body_block = self.new_block();
            arm_blocks.push((test_block, body_block));
        }

        // Start by jumping to first test
        if !arm_blocks.is_empty() {
            self.terminate(Terminator::Goto(arm_blocks[0].0));
        } else {
            self.terminate(Terminator::Goto(exit_block));
        }

        // Process each arm
        for (i, arm) in arms.iter().enumerate() {
            let (test_block, body_block) = arm_blocks[i];
            let next_test = if i + 1 < arm_blocks.len() {
                arm_blocks[i + 1].0
            } else {
                exit_block
            };

            self.current_block = Some(test_block);

            match &arm.pattern.kind {
                PatternKind::Wildcard => {
                    // Always matches, go directly to body
                    self.terminate(Terminator::Goto(body_block));
                }

                PatternKind::Ident(ident, _, _) => {
                    // Check if this identifier is a known enum variant (or built-in like None)
                    let is_enum_variant = self.enum_variants.get(&ident.name)
                        .map(|(_, count)| *count == 0)
                        .unwrap_or(false) || ident.name == "None";

                    if is_enum_variant {
                        // Unit variant - compare discriminants
                        let disc_local = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            disc_local,
                            Rvalue::Discriminant(scrut_local),
                        ));

                        let expected_disc = self.get_variant_discriminant(&ident.name);
                        let expected_local = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            expected_local,
                            Rvalue::Use(Operand::Constant(Constant::Int(expected_disc))),
                        ));

                        let cond_local = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(
                            cond_local,
                            Rvalue::BinaryOp(
                                BinOp::Eq,
                                Operand::Copy(disc_local),
                                Operand::Copy(expected_local),
                            ),
                        ));
                        self.terminate(Terminator::If {
                            cond: Operand::Copy(cond_local),
                            then_block: body_block,
                            else_block: next_test,
                        });
                    } else {
                        // Not an enum variant - bind the variable and go to body
                        let local = self.new_local(Ty::Unit, Some(ident.name.clone()));
                        self.vars.insert(ident.name.clone(), local);
                        self.emit(StatementKind::Assign(
                            local,
                            Rvalue::Use(Operand::Copy(scrut_local)),
                        ));
                        self.terminate(Terminator::Goto(body_block));
                    }
                }

                PatternKind::Literal(Literal { kind: LiteralKind::Int(n), .. }) => {
                    // Compare integer and branch
                    let lit_local = self.new_temp(Ty::Int);
                    self.emit(StatementKind::Assign(
                        lit_local,
                        Rvalue::Use(Operand::Constant(Constant::Int(*n as i64))),
                    ));
                    let cond_local = self.new_temp(Ty::Bool);
                    self.emit(StatementKind::Assign(
                        cond_local,
                        Rvalue::BinaryOp(
                            BinOp::Eq,
                            Operand::Copy(scrut_local),
                            Operand::Copy(lit_local),
                        ),
                    ));
                    self.terminate(Terminator::If {
                        cond: Operand::Copy(cond_local),
                        then_block: body_block,
                        else_block: next_test,
                    });
                }

                PatternKind::Struct(path, fields, _) => {
                    // Enum variant pattern: Some(x), None, Color::Red, etc.
                    let variant = if path.segments.len() == 2 {
                        &path.segments[1].name.name
                    } else if path.segments.len() == 1 {
                        &path.segments[0].name.name
                    } else {
                        self.terminate(Terminator::Goto(next_test));
                        continue;
                    };

                    // Get discriminant
                    let disc_local = self.new_temp(Ty::Int);
                    self.emit(StatementKind::Assign(
                        disc_local,
                        Rvalue::Discriminant(scrut_local),
                    ));

                    let variant_disc = self.get_variant_discriminant(variant);
                    let expected = self.new_temp(Ty::Int);
                    self.emit(StatementKind::Assign(
                        expected,
                        Rvalue::Use(Operand::Constant(Constant::Int(variant_disc))),
                    ));

                    let cond = self.new_temp(Ty::Bool);
                    self.emit(StatementKind::Assign(
                        cond,
                        Rvalue::BinaryOp(BinOp::Eq, Operand::Copy(disc_local), Operand::Copy(expected)),
                    ));

                    // Check if there are fields to extract
                    if fields.is_empty() {
                        // Unit variant, go directly to body
                        self.terminate(Terminator::If {
                            cond: Operand::Copy(cond),
                            then_block: body_block,
                            else_block: next_test,
                        });
                    } else {
                        // Create extraction block for binding fields
                        let extract_block = self.new_block();
                        self.terminate(Terminator::If {
                            cond: Operand::Copy(cond),
                            then_block: extract_block,
                            else_block: next_test,
                        });

                        // In extract block: bind fields and goto body
                        self.current_block = Some(extract_block);
                        for (idx, field) in fields.iter().enumerate() {
                            // PatternField has name and optional pattern
                            // For `Some(x)`, name is "x" and we bind it
                            let binding_name = &field.name.name;

                            // Check if there's an explicit sub-pattern
                            if let Some(ref sub_pattern) = field.pattern {
                                match &sub_pattern.kind {
                                    PatternKind::Wildcard => {
                                        // Skip binding
                                    }
                                    PatternKind::Ident(inner_ident, _, _) => {
                                        let field_local = self.new_local(Ty::Unit, Some(inner_ident.name.clone()));
                                        self.vars.insert(inner_ident.name.clone(), field_local);
                                        self.emit(StatementKind::Assign(
                                            field_local,
                                            Rvalue::EnumField(scrut_local, idx),
                                        ));
                                    }
                                    _ => {}
                                }
                            } else {
                                // Bind directly with field name
                                let field_local = self.new_local(Ty::Unit, Some(binding_name.clone()));
                                self.vars.insert(binding_name.clone(), field_local);
                                self.emit(StatementKind::Assign(
                                    field_local,
                                    Rvalue::EnumField(scrut_local, idx),
                                ));
                            }
                        }
                        self.terminate(Terminator::Goto(body_block));
                    }
                }

                _ => {
                    // Unsupported pattern, skip to next
                    self.terminate(Terminator::Goto(next_test));
                    continue;
                }
            }

            // Lower body
            self.current_block = Some(body_block);
            if let Some(val) = self.lower_expr(&arm.body) {
                self.emit(StatementKind::Assign(result, Rvalue::Use(val)));
            }
            if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
                self.terminate(Terminator::Goto(exit_block));
            }
        }

        self.current_block = Some(exit_block);
        Some(Operand::Local(result))
    }

    /// Get the discriminant (tag) value for a variant name.
    ///
    /// Returns a unique integer for each variant to enable pattern matching
    /// via integer comparison. Built-in types have fixed discriminants:
    /// - Option: None=0, Some=1
    /// - Result: Ok=0, Err=1
    ///
    /// User-defined enums use a simple hash of the variant name.
    fn get_variant_discriminant(&self, variant: &str) -> i64 {
        match variant {
            "None" => 0,
            "Some" => 1,
            "Ok" => 0,
            "Err" => 1,
            // For user-defined enums, use a simple hash
            _ => variant.bytes().fold(0i64, |acc, b| acc + b as i64),
        }
    }

    /// Resolve a method name to a function name.
    ///
    /// Maps common method names to their built-in function equivalents.
    /// For example: `.len()` -> `vec_len` or `str_len`, `.push(x)` -> `vec_push`
    fn resolve_method(&self, method_name: &str) -> String {
        match method_name {
            // Vec/Array methods
            "len" => "vec_len".to_string(),
            "push" => "vec_push".to_string(),
            "pop" => "vec_pop".to_string(),
            "get" => "vec_get".to_string(),
            "set" => "vec_set".to_string(),
            "first" => "vec_first".to_string(),
            "last" => "vec_last".to_string(),
            "concat" => "vec_concat".to_string(),
            "slice" => "vec_slice".to_string(),
            "reverse" => "vec_reverse".to_string(),

            // String methods
            "char_at" => "str_char_at".to_string(),
            "contains" => "str_contains".to_string(),
            "starts_with" => "str_starts_with".to_string(),
            "ends_with" => "str_ends_with".to_string(),
            "split" => "str_split".to_string(),
            "trim" => "str_trim".to_string(),
            "to_int" => "str_to_int".to_string(),
            "to_str" => "int_to_str".to_string(),

            // Map methods
            "insert" => "map_insert".to_string(),
            "remove" => "map_remove".to_string(),
            "keys" => "map_keys".to_string(),
            "values" => "map_values".to_string(),

            // Char methods
            "is_digit" => "char_is_digit".to_string(),
            "is_alpha" => "char_is_alpha".to_string(),
            "is_alphanumeric" => "char_is_alphanumeric".to_string(),
            "is_whitespace" => "char_is_whitespace".to_string(),

            // Default: use the method name as-is (for user-defined methods)
            _ => method_name.to_string(),
        }
    }

    fn lower_for(
        &mut self,
        pattern: &Pattern,
        iter: &Expr,
        body: &AstBlock,
        _span: Span,
    ) -> Option<Operand> {
        // For loops can iterate over:
        // 1. Ranges: `for i in 0..10` or `for i in start..end` or `for i in 0..=10`
        // 2. Arrays: `for x in arr` or `for i, x in arr.enumerate()`

        // Check if this is a range iteration
        if let ExprKind::Range(start_opt, end_opt, inclusive) = &iter.kind {
            return self.lower_for_range(pattern, start_opt, end_opt, *inclusive, body);
        }

        // Check if this is an enumerate call: `arr.enumerate()`
        let (iter_expr, is_enumerate) = match &iter.kind {
            ExprKind::MethodCall(receiver, method, _args) if method.name == "enumerate" => {
                (receiver.as_ref(), true)
            }
            _ => (iter, false),
        };

        // Evaluate the iterable (should be an array)
        let iter_val = self.lower_expr(iter_expr)?;

        // Store the array in a local for repeated access
        let arr_local = self.new_temp(Ty::Int);
        self.emit(StatementKind::Assign(arr_local, Rvalue::Use(iter_val)));

        // Create index counter starting at 0
        let idx_local = self.new_temp(Ty::Int);
        self.emit(StatementKind::Assign(idx_local, Rvalue::Use(Operand::Constant(Constant::Int(0)))));

        // Get array length
        let len_local = self.new_temp(Ty::Int);
        let len_block = self.new_block();
        self.terminate(Terminator::Call {
            func: "vec_len".to_string(),
            args: vec![Operand::Copy(arr_local)],
            dest: Some(len_local),
            next: len_block,
        });
        self.current_block = Some(len_block);

        let cond_block = self.new_block();
        let body_block = self.new_block();
        let incr_block = self.new_block();
        let exit_block = self.new_block();

        // Push loop context (continue goes to increment, break goes to exit)
        self.loop_stack.push(LoopContext {
            continue_block: incr_block,
            break_block: exit_block,
            result_local: None,
        });

        // Jump to condition check
        self.terminate(Terminator::Goto(cond_block));

        // Condition block: check if idx < len
        self.current_block = Some(cond_block);
        let cond_val = self.new_temp(Ty::Bool);
        self.emit(StatementKind::Assign(
            cond_val,
            Rvalue::BinaryOp(BinOp::Lt, Operand::Copy(idx_local), Operand::Copy(len_local)),
        ));
        self.terminate(Terminator::If {
            cond: Operand::Local(cond_val),
            then_block: body_block,
            else_block: exit_block,
        });

        // Body block: extract element and bind to pattern
        self.current_block = Some(body_block);

        // Get element at current index: arr[idx]
        let elem_local = self.new_temp(Ty::Int);
        self.emit(StatementKind::Assign(
            elem_local,
            Rvalue::Index(Operand::Copy(arr_local), Operand::Copy(idx_local)),
        ));

        // Bind pattern variable(s)
        match &pattern.kind {
            PatternKind::Ident(ident, _, _) => {
                // Simple pattern: `for x in arr`
                let var_local = self.new_local(Ty::Int, Some(ident.name.clone()));
                self.emit(StatementKind::Assign(var_local, Rvalue::Use(Operand::Copy(elem_local))));
                self.vars.insert(ident.name.clone(), var_local);
            }
            PatternKind::Tuple(patterns) if is_enumerate && patterns.len() == 2 => {
                // Enumerate pattern: `for i, x in arr.enumerate()`
                // First element is index, second is value
                if let PatternKind::Ident(idx_ident, _, _) = &patterns[0].kind {
                    let idx_var = self.new_local(Ty::Int, Some(idx_ident.name.clone()));
                    self.emit(StatementKind::Assign(idx_var, Rvalue::Use(Operand::Copy(idx_local))));
                    self.vars.insert(idx_ident.name.clone(), idx_var);
                }
                if let PatternKind::Ident(val_ident, _, _) = &patterns[1].kind {
                    let val_var = self.new_local(Ty::Int, Some(val_ident.name.clone()));
                    self.emit(StatementKind::Assign(val_var, Rvalue::Use(Operand::Copy(elem_local))));
                    self.vars.insert(val_ident.name.clone(), val_var);
                }
            }
            _ => {
                // Fallback: try to bind as simple identifier
                if let PatternKind::Ident(ident, _, _) = &pattern.kind {
                    let var_local = self.new_local(Ty::Int, Some(ident.name.clone()));
                    self.emit(StatementKind::Assign(var_local, Rvalue::Use(Operand::Copy(elem_local))));
                    self.vars.insert(ident.name.clone(), var_local);
                }
            }
        }

        // Execute loop body
        self.lower_block(body);

        // If body didn't terminate, go to increment
        if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
            self.terminate(Terminator::Goto(incr_block));
        }

        // Increment block: idx = idx + 1
        self.current_block = Some(incr_block);
        self.emit(StatementKind::Assign(
            idx_local,
            Rvalue::BinaryOp(BinOp::Add, Operand::Copy(idx_local), Operand::Constant(Constant::Int(1))),
        ));
        self.terminate(Terminator::Goto(cond_block));

        self.loop_stack.pop();
        self.current_block = Some(exit_block);
        None
    }

    /// Lower a for loop with range iteration: `for i in 0..10` or `for i in start..=end`
    fn lower_for_range(
        &mut self,
        pattern: &Pattern,
        start_opt: &Option<Box<Expr>>,
        end_opt: &Option<Box<Expr>>,
        inclusive: bool,
        body: &AstBlock,
    ) -> Option<Operand> {
        // Get the start value (default to 0 if not specified)
        let start_val = if let Some(start_expr) = start_opt {
            self.lower_expr(start_expr)?
        } else {
            Operand::Constant(Constant::Int(0))
        };

        // Get the end value (required for iteration)
        let end_val = if let Some(end_expr) = end_opt {
            self.lower_expr(end_expr)?
        } else {
            // Open-ended ranges like `0..` are not supported in for loops
            // Default to start value (empty iteration)
            start_val.clone()
        };

        // Store the end value in a local
        let end_local = self.new_temp(Ty::Int);
        self.emit(StatementKind::Assign(end_local, Rvalue::Use(end_val)));

        // Create the loop variable, initialized to start
        let idx_local = self.new_temp(Ty::Int);
        self.emit(StatementKind::Assign(idx_local, Rvalue::Use(start_val)));

        let cond_block = self.new_block();
        let body_block = self.new_block();
        let incr_block = self.new_block();
        let exit_block = self.new_block();

        // Push loop context (continue goes to increment, break goes to exit)
        self.loop_stack.push(LoopContext {
            continue_block: incr_block,
            break_block: exit_block,
            result_local: None,
        });

        // Jump to condition check
        self.terminate(Terminator::Goto(cond_block));

        // Condition block: check if idx < end (exclusive) or idx <= end (inclusive)
        self.current_block = Some(cond_block);
        let cond_val = self.new_temp(Ty::Bool);
        let cmp_op = if inclusive { BinOp::Le } else { BinOp::Lt };
        self.emit(StatementKind::Assign(
            cond_val,
            Rvalue::BinaryOp(cmp_op, Operand::Copy(idx_local), Operand::Copy(end_local)),
        ));
        self.terminate(Terminator::If {
            cond: Operand::Local(cond_val),
            then_block: body_block,
            else_block: exit_block,
        });

        // Body block: bind pattern variable to current index value
        self.current_block = Some(body_block);

        // Bind the loop variable based on pattern
        if let PatternKind::Ident(ident, _, _) = &pattern.kind {
            let var_local = self.new_local(Ty::Int, Some(ident.name.clone()));
            self.emit(StatementKind::Assign(var_local, Rvalue::Use(Operand::Copy(idx_local))));
            self.vars.insert(ident.name.clone(), var_local);
        }

        // Execute loop body
        self.lower_block(body);

        // If body didn't terminate, go to increment
        if self.current_fn.as_ref().unwrap().block(self.current_block.unwrap()).terminator.is_none() {
            self.terminate(Terminator::Goto(incr_block));
        }

        // Increment block: idx = idx + 1
        self.current_block = Some(incr_block);
        self.emit(StatementKind::Assign(
            idx_local,
            Rvalue::BinaryOp(BinOp::Add, Operand::Copy(idx_local), Operand::Constant(Constant::Int(1))),
        ));
        self.terminate(Terminator::Goto(cond_block));

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

    /// Convert an AST type to a MIR type.
    ///
    /// This function handles all FORMA type syntax including:
    /// - Primitive types: Int, Bool, Str, Float, Char
    /// - Sized integers: i8, i16, i32, i64, u8, u16, u32, u64
    /// - Compound types: tuples, arrays, lists, maps, sets
    /// - Generic types: Option[T], Result[T, E]
    /// - Reference types: &T, &mut T
    /// - Function types: (A, B) -> C
    /// - User-defined types: structs and enums
    fn lower_type(&self, ty: &crate::parser::Type) -> Ty {
        use crate::parser::TypeKind as AstTypeKind;
        use crate::types::TypeId;

        match &ty.kind {
            AstTypeKind::Path(path) => {
                // Get the first segment (e.g., "Int", "Option", "MyStruct")
                let first_seg = &path.segments[0];
                let name = &first_seg.name.name;

                // Lower any generic arguments
                let type_args: Vec<Ty> = first_seg.args.as_ref()
                    .map(|args| {
                        args.args.iter().filter_map(|arg| {
                            match arg {
                                crate::parser::GenericArg::Type(t) => Some(self.lower_type(t)),
                                _ => None,
                            }
                        }).collect()
                    })
                    .unwrap_or_default();

                // Match built-in types
                match name.as_str() {
                    "Int" => Ty::Int,
                    "Bool" => Ty::Bool,
                    "Str" => Ty::Str,
                    "Float" => Ty::Float,
                    "Char" => Ty::Char,
                    "Unit" | "()" => Ty::Unit,
                    // Sized integer types
                    "i8" => Ty::I8,
                    "i16" => Ty::I16,
                    "i32" => Ty::I32,
                    "i64" => Ty::I64,
                    "i128" => Ty::I128,
                    "u8" => Ty::U8,
                    "u16" => Ty::U16,
                    "u32" => Ty::U32,
                    "u64" => Ty::U64,
                    "u128" => Ty::U128,
                    "UInt" => Ty::UInt,
                    // Floating point variants
                    "f32" => Ty::F32,
                    "f64" => Ty::F64,
                    // Generic built-ins
                    "Option" => {
                        if type_args.len() == 1 {
                            Ty::Option(Box::new(type_args[0].clone()))
                        } else {
                            Ty::Option(Box::new(Ty::Error))
                        }
                    }
                    "Result" => {
                        let ok_ty = type_args.get(0).cloned().unwrap_or(Ty::Error);
                        let err_ty = type_args.get(1).cloned().unwrap_or(Ty::Str);
                        Ty::Result(Box::new(ok_ty), Box::new(err_ty))
                    }
                    // User-defined type
                    _ => Ty::Named(TypeId::new(name.clone()), type_args),
                }
            }

            AstTypeKind::List(inner) => {
                Ty::List(Box::new(self.lower_type(inner)))
            }

            AstTypeKind::Option(inner) => {
                Ty::Option(Box::new(self.lower_type(inner)))
            }

            AstTypeKind::Result(ok, err) => {
                let ok_ty = self.lower_type(ok);
                let err_ty = err.as_ref()
                    .map(|e| self.lower_type(e))
                    .unwrap_or(Ty::Str);
                Ty::Result(Box::new(ok_ty), Box::new(err_ty))
            }

            AstTypeKind::Tuple(tys) => {
                Ty::Tuple(tys.iter().map(|t| self.lower_type(t)).collect())
            }

            AstTypeKind::Ref(inner, is_mut) => {
                let mutability = if *is_mut {
                    crate::types::Mutability::Mutable
                } else {
                    crate::types::Mutability::Immutable
                };
                Ty::Ref(Box::new(self.lower_type(inner)), mutability)
            }

            AstTypeKind::Ptr(inner, is_mut) => {
                let mutability = if *is_mut {
                    crate::types::Mutability::Mutable
                } else {
                    crate::types::Mutability::Immutable
                };
                Ty::Ptr(Box::new(self.lower_type(inner)), mutability)
            }

            AstTypeKind::Fn(params, ret) => {
                let param_tys: Vec<Ty> = params.iter().map(|t| self.lower_type(t)).collect();
                let ret_ty = self.lower_type(ret);
                Ty::Fn(param_tys, Box::new(ret_ty))
            }

            AstTypeKind::Array(inner, _size_expr) => {
                // For now, use a default size - proper const evaluation would be needed
                Ty::Array(Box::new(self.lower_type(inner)), 0)
            }

            AstTypeKind::Map(key, value) => {
                Ty::Map(Box::new(self.lower_type(key)), Box::new(self.lower_type(value)))
            }

            AstTypeKind::Set(inner) => {
                Ty::Set(Box::new(self.lower_type(inner)))
            }

            AstTypeKind::Infer => Ty::Var(crate::types::TypeVar::fresh()),

            AstTypeKind::Never => Ty::Never,
        }
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

    /// Find a similar variable name for typo suggestions.
    ///
    /// Uses a simple Levenshtein-like edit distance comparison
    /// to find names that are likely typos of the given name.
    fn find_similar_name(&self, name: &str) -> Option<String> {
        let mut best_match: Option<(String, usize)> = None;

        for existing in self.vars.keys() {
            let dist = Self::edit_distance(name, existing);
            // Only suggest if edit distance is small relative to name length
            // and less than half the name length
            if dist <= 2 && dist < name.len() / 2 + 1 {
                match &best_match {
                    None => best_match = Some((existing.clone(), dist)),
                    Some((_, best_dist)) if dist < *best_dist => {
                        best_match = Some((existing.clone(), dist))
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

        if m == 0 {
            return n;
        }
        if n == 0 {
            return m;
        }

        let mut dp = vec![vec![0; n + 1]; m + 1];

        for i in 0..=m {
            dp[i][0] = i;
        }
        for j in 0..=n {
            dp[0][j] = j;
        }

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

    /// Find free variables in an expression that aren't in the given bound set.
    /// Used for closure capture analysis.
    fn find_free_vars(&self, expr: &Expr, bound: &std::collections::HashSet<String>) -> Vec<String> {
        let mut free = Vec::new();
        self.collect_free_vars(expr, bound, &mut free);
        // Remove duplicates while preserving order
        let mut seen = std::collections::HashSet::new();
        free.retain(|v| seen.insert(v.clone()));
        free
    }

    fn collect_free_vars(&self, expr: &Expr, bound: &std::collections::HashSet<String>, free: &mut Vec<String>) {
        match &expr.kind {
            ExprKind::Ident(ident) => {
                let name = &ident.name;
                // Check if it's a free variable (not bound, not a known function, not a builtin)
                if !bound.contains(name)
                    && !self.program.functions.contains_key(name)
                    && !self.is_builtin(name)
                    && !self.enum_variants.contains_key(name)
                    && self.vars.contains_key(name)
                {
                    free.push(name.clone());
                }
            }
            ExprKind::Literal(_) => {}
            ExprKind::Unary(_, e) => self.collect_free_vars(e, bound, free),
            ExprKind::Binary(left, _, right) => {
                self.collect_free_vars(left, bound, free);
                self.collect_free_vars(right, bound, free);
            }
            ExprKind::Call(callee, args) => {
                self.collect_free_vars(callee, bound, free);
                for arg in args {
                    self.collect_free_vars(&arg.value, bound, free);
                }
            }
            ExprKind::MethodCall(receiver, _, args) => {
                self.collect_free_vars(receiver, bound, free);
                for arg in args {
                    self.collect_free_vars(&arg.value, bound, free);
                }
            }
            ExprKind::If(if_expr) => {
                // Collect from condition
                self.collect_free_vars(&if_expr.condition, bound, free);
                // Collect from then branch
                match &if_expr.then_branch {
                    IfBranch::Expr(e) => self.collect_free_vars(e, bound, free),
                    IfBranch::Block(block) => {
                        for stmt in &block.stmts {
                            if let StmtKind::Expr(e) = &stmt.kind {
                                self.collect_free_vars(e, bound, free);
                            }
                        }
                    }
                }
                // Collect from else branch
                if let Some(else_branch) = &if_expr.else_branch {
                    match else_branch {
                        ElseBranch::Expr(e) => self.collect_free_vars(e, bound, free),
                        ElseBranch::Block(block) => {
                            for stmt in &block.stmts {
                                if let StmtKind::Expr(e) = &stmt.kind {
                                    self.collect_free_vars(e, bound, free);
                                }
                            }
                        }
                        ElseBranch::ElseIf(nested_if) => {
                            // Recurse as an if expression
                            self.collect_free_vars(&Expr {
                                kind: ExprKind::If(nested_if.clone()),
                                span: expr.span,
                            }, bound, free);
                        }
                    }
                }
            }
            ExprKind::Block(block) => {
                for stmt in &block.stmts {
                    if let StmtKind::Expr(e) = &stmt.kind {
                        self.collect_free_vars(e, bound, free);
                    }
                }
            }
            ExprKind::Tuple(exprs) => {
                for e in exprs {
                    self.collect_free_vars(e, bound, free);
                }
            }
            ExprKind::Array(exprs) => {
                for e in exprs {
                    self.collect_free_vars(e, bound, free);
                }
            }
            ExprKind::Index(base, idx) => {
                self.collect_free_vars(base, bound, free);
                self.collect_free_vars(idx, bound, free);
            }
            ExprKind::Field(e, _) => {
                self.collect_free_vars(e, bound, free);
            }
            ExprKind::Closure(closure) => {
                // Add closure params to bound set for nested closure
                let mut inner_bound = bound.clone();
                for param in &closure.params {
                    inner_bound.insert(param.name.name.clone());
                }
                self.collect_free_vars(&closure.body, &inner_bound, free);
            }
            // Handle other cases conservatively
            _ => {}
        }
    }

    fn is_builtin(&self, name: &str) -> bool {
        matches!(name,
            "print" | "vec_new" | "vec_push" | "vec_pop" | "vec_len" | "vec_get" |
            "vec_first" | "vec_last" | "vec_concat" | "vec_reverse" | "vec_slice" |
            "str_len" | "str_char_at" | "str_slice" | "str_contains" | "str_starts_with" |
            "str_ends_with" | "str_split" | "str_trim" | "str_to_int" | "int_to_str" |
            "str_concat" | "map_new" | "map_insert" | "map_get" | "map_contains" |
            "map_remove" | "map_len" | "map_keys" | "char_is_digit" | "char_is_alpha" |
            "char_is_alphanumeric" | "char_is_whitespace" | "char_to_int"
        )
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
