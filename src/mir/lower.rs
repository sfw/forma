//! AST to MIR lowering.
//!
//! This module transforms the typed AST into MIR, which is a simpler
//! representation that's easier to interpret and compile.

use std::collections::HashMap;

use crate::lexer::Span;
use crate::parser::{
    BinOp as AstBinOp, Block as AstBlock, ElseBranch, Expr, ExprKind, FnBody,
    Function as AstFunction, IfBranch, Item, ItemKind, Literal, LiteralKind, Pattern, PatternKind,
    SourceFile, StmtKind, UnaryOp as AstUnaryOp,
};
use crate::types::Ty;

use super::mir::{
    BinOp, BlockId, Constant, Function, Local, MirContract, Mutability, Operand, PassMode, Program,
    Rvalue, Statement, StatementKind, Terminator, UnOp,
};

/// Convert AST PassMode to MIR PassMode.
fn lower_pass_mode(ast_mode: crate::parser::PassMode) -> PassMode {
    match ast_mode {
        crate::parser::PassMode::Owned => PassMode::Owned,
        crate::parser::PassMode::Ref => PassMode::Ref,
        crate::parser::PassMode::RefMut => PassMode::RefMut,
    }
}

/// Error during lowering.
#[derive(Debug, Clone)]
pub struct LowerError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "lowering error at line {}: {}",
            self.span.line, self.message
        )
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
    /// Variable name to type name mapping (for method resolution)
    var_types: HashMap<String, String>,
    /// Variable name to full Ty mapping (for type propagation)
    var_full_types: HashMap<String, Ty>,
    /// Local to Ty mapping (for type propagation)
    local_types: HashMap<Local, Ty>,
    /// Loop context for break/continue
    loop_stack: Vec<LoopContext>,
    /// Errors accumulated during lowering
    errors: Vec<LowerError>,
    /// Enum variant to (enum_type_name, variant_fields_count) mapping
    enum_variants: HashMap<String, (String, usize)>,
    /// Counter for generating unique closure function names
    closure_counter: u32,
    /// Function default parameter expressions: fn_name -> list of defaults (None if no default)
    fn_defaults: HashMap<String, Vec<Option<Expr>>>,
    /// Method to qualified name mapping: method_name -> list of qualified names (Type::method)
    impl_methods: HashMap<String, Vec<String>>,
    /// Function return types for proper call type inference
    fn_return_types: HashMap<String, Ty>,
}

#[derive(Debug, Clone)]
struct LoopContext {
    label: Option<String>,
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
            var_types: HashMap::new(),
            var_full_types: HashMap::new(),
            local_types: HashMap::new(),
            loop_stack: Vec::new(),
            errors: Vec::new(),
            enum_variants: HashMap::new(),
            closure_counter: 0,
            fn_defaults: HashMap::new(),
            impl_methods: HashMap::new(),
            fn_return_types: HashMap::new(),
        }
    }

    /// Get the current block ID, returning an error if none is set.
    fn current_block_id(&self) -> Result<BlockId, LowerError> {
        self.current_block.ok_or_else(|| LowerError {
            message: "internal error: no current block".to_string(),
            span: Span {
                start: 0,
                end: 0,
                line: 0,
                column: 0,
            },
        })
    }

    /// Get a reference to the current function, returning an error if none is set.
    fn current_function(&self) -> Result<&Function, LowerError> {
        self.current_fn.as_ref().ok_or_else(|| LowerError {
            message: "internal error: no current function".to_string(),
            span: Span {
                start: 0,
                end: 0,
                line: 0,
                column: 0,
            },
        })
    }

    /// Get a mutable reference to the current function, returning an error if none is set.
    fn current_function_mut(&mut self) -> Result<&mut Function, LowerError> {
        self.current_fn.as_mut().ok_or_else(|| LowerError {
            message: "internal error: no current function".to_string(),
            span: Span {
                start: 0,
                end: 0,
                line: 0,
                column: 0,
            },
        })
    }

    /// Lower a source file to MIR.
    pub fn lower(mut self, source: &SourceFile) -> Result<Program, Vec<LowerError>> {
        // First pass: collect type definitions (enums, structs) so we know about variants
        for item in &source.items {
            if let ItemKind::Enum(e) = &item.kind {
                let enum_name = e.name.name.clone();
                for (idx, variant) in e.variants.iter().enumerate() {
                    let field_count = match &variant.kind {
                        crate::parser::VariantKind::Unit => 0,
                        crate::parser::VariantKind::Tuple(fields) => fields.len(),
                        crate::parser::VariantKind::Named(fields) => fields.len(),
                    };
                    self.enum_variants
                        .insert(variant.name.name.clone(), (enum_name.clone(), field_count));
                    // Register in program for runtime discriminant lookup
                    self.program
                        .enum_variants
                        .insert((enum_name.clone(), variant.name.name.clone()), idx);
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
                    if let crate::parser::ImplItem::Function(f) = impl_item
                        && let Some(mir_fn) = self.lower_function(f)
                    {
                        // Use qualified name for methods
                        let qualified_name = format!(
                            "{}::{}",
                            self.type_to_string(&impl_block.self_type),
                            mir_fn.name
                        );
                        // Track method -> qualified name mapping for method resolution
                        self.impl_methods
                            .entry(mir_fn.name.clone())
                            .or_default()
                            .push(qualified_name.clone());
                        self.program.functions.insert(qualified_name, mir_fn);
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
                    self.enum_variants
                        .insert(variant.name.name.clone(), (enum_name.clone(), field_count));
                }
            }
            // Other items don't generate MIR directly
            _ => {}
        }
    }

    fn type_to_string(&self, ty: &crate::parser::Type) -> String {
        match &ty.kind {
            crate::parser::TypeKind::Path(path) => path
                .segments
                .iter()
                .map(|s| s.name.name.clone())
                .collect::<Vec<_>>()
                .join("::"),
            _ => "Unknown".to_string(),
        }
    }

    /// Pretty-print an expression for contract error messages
    fn expr_to_string(&self, expr: &Expr) -> String {
        use crate::parser::ast::{BinOp, ExprKind, UnaryOp};

        match &expr.kind {
            ExprKind::Literal(lit) => match &lit.kind {
                crate::parser::ast::LiteralKind::Int(n) => n.to_string(),
                crate::parser::ast::LiteralKind::Float(f) => f.to_string(),
                crate::parser::ast::LiteralKind::String(s) => format!("\"{}\"", s),
                crate::parser::ast::LiteralKind::Char(c) => format!("'{}'", c),
                crate::parser::ast::LiteralKind::Bool(b) => b.to_string(),
                crate::parser::ast::LiteralKind::None => "None".to_string(),
            },
            ExprKind::Ident(ident) => ident.name.clone(),
            ExprKind::Binary(left, op, right) => {
                let op_str = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::Div => "/",
                    BinOp::Mod => "%",
                    BinOp::Eq => "==",
                    BinOp::Ne => "!=",
                    BinOp::Lt => "<",
                    BinOp::Le => "<=",
                    BinOp::Gt => ">",
                    BinOp::Ge => ">=",
                    BinOp::And => "&&",
                    BinOp::Or => "||",
                    BinOp::BitAnd => "&",
                    BinOp::BitOr => "|",
                    BinOp::BitXor => "^",
                    BinOp::Shl => "<<",
                    BinOp::Shr => ">>",
                };
                format!(
                    "{} {} {}",
                    self.expr_to_string(left),
                    op_str,
                    self.expr_to_string(right)
                )
            }
            ExprKind::Unary(op, operand) => {
                let op_str = match op {
                    UnaryOp::Neg => "-",
                    UnaryOp::Not => "!",
                    UnaryOp::Ref => "&",
                    UnaryOp::RefMut => "&mut ",
                    UnaryOp::Deref => "*",
                };
                format!("{}{}", op_str, self.expr_to_string(operand))
            }
            ExprKind::Field(receiver, field) => {
                format!("{}.{}", self.expr_to_string(receiver), field.name)
            }
            ExprKind::Call(callee, args) => {
                let args_str: Vec<String> =
                    args.iter().map(|a| self.expr_to_string(&a.value)).collect();
                format!("{}({})", self.expr_to_string(callee), args_str.join(", "))
            }
            ExprKind::MethodCall(receiver, method, args) => {
                let args_str: Vec<String> =
                    args.iter().map(|a| self.expr_to_string(&a.value)).collect();
                format!(
                    "{}.{}({})",
                    self.expr_to_string(receiver),
                    method.name,
                    args_str.join(", ")
                )
            }
            _ => format!("{:?}", expr.kind),
        }
    }

    fn lower_function(&mut self, f: &AstFunction) -> Option<Function> {
        // Skip functions without bodies (trait methods)
        let body = f.body.as_ref()?;

        // Store default parameter expressions for later use when lowering calls
        let defaults: Vec<Option<Expr>> = f.params.iter().map(|p| p.default.clone()).collect();
        if defaults.iter().any(|d| d.is_some()) {
            self.fn_defaults.insert(f.name.name.clone(), defaults);
        }

        // Reset state
        self.vars.clear();
        self.loop_stack.clear();

        // Determine return type
        let return_ty = f
            .return_type
            .as_ref()
            .map(|t| self.lower_type(t))
            .unwrap_or(Ty::Unit);

        // Create function
        let mut mir_fn = Function::new(f.name.name.clone(), vec![], return_ty.clone());

        // Add parameters
        for param in &f.params {
            let ty = self.lower_type(&param.ty);
            let local = mir_fn.add_local(ty.clone(), Some(param.name.name.clone()));
            mir_fn.params.push((local, ty.clone()));
            mir_fn.param_names.push((param.name.name.clone(), ty));
            mir_fn
                .param_pass_modes
                .push(lower_pass_mode(param.pass_mode));
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
            let block = self.current_block_id().ok()?;
            if self
                .current_function()
                .ok()?
                .block(block)
                .terminator
                .is_none()
            {
                self.terminate(Terminator::Return(Some(result)));
            }
        } else {
            let block = self.current_block_id().ok()?;
            if self
                .current_function()
                .ok()?
                .block(block)
                .terminator
                .is_none()
            {
                self.terminate(Terminator::Return(None));
            }
        }

        // Copy contracts from AST to MIR
        let mut mir_fn = self.current_fn.take()?;
        for contract in &f.preconditions {
            mir_fn.preconditions.push(MirContract {
                expr_string: self.expr_to_string(&contract.condition),
                message: contract.message.clone(),
                condition: Some(contract.condition.clone()),
            });
        }
        for contract in &f.postconditions {
            mir_fn.postconditions.push(MirContract {
                expr_string: self.expr_to_string(&contract.condition),
                message: contract.message.clone(),
                condition: Some(contract.condition.clone()),
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
                    // Try to infer type from init expression for method resolution
                    let inferred_type = self.infer_receiver_type(&let_stmt.init);

                    let init = self.lower_expr(&let_stmt.init);
                    if let Some(op) = init {
                        // Record the type if we inferred one and the pattern is an identifier
                        if let Some(type_name) = inferred_type
                            && let PatternKind::Ident(ident, _, _) = &let_stmt.pattern.kind
                        {
                            self.var_types.insert(ident.name.clone(), type_name);
                        }
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
            ExprKind::Literal(lit) => Some(Operand::Constant(self.lower_literal(lit))),

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
                            Some(Operand::Local(result))
                        }
                        _ => {
                            // Check for user-defined unit enum variants
                            if let Some((enum_name, field_count)) =
                                self.enum_variants.get(&ident.name).cloned()
                                && field_count == 0
                            {
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
                // Handle short-circuit evaluation for && and ||
                match op {
                    AstBinOp::And => {
                        // a && b: only evaluate b if a is true
                        let result = self.new_temp(Ty::Bool);
                        let l = self.lower_expr(left)?;

                        let eval_right_block = self.new_block();
                        let short_circuit_block = self.new_block();
                        let merge_block = self.new_block();

                        // If left is true, evaluate right; else short-circuit to false
                        self.terminate(Terminator::If {
                            cond: l,
                            then_block: eval_right_block,
                            else_block: short_circuit_block,
                        });

                        // Evaluate right operand
                        self.current_block = Some(eval_right_block);
                        let r = self.lower_expr(right)?;
                        self.emit(StatementKind::Assign(result, Rvalue::Use(r)));
                        self.terminate(Terminator::Goto(merge_block));

                        // Short-circuit: left was false, result is false
                        self.current_block = Some(short_circuit_block);
                        self.emit(StatementKind::Assign(
                            result,
                            Rvalue::Use(Operand::Constant(Constant::Bool(false))),
                        ));
                        self.terminate(Terminator::Goto(merge_block));

                        self.current_block = Some(merge_block);
                        Some(Operand::Local(result))
                    }
                    AstBinOp::Or => {
                        // a || b: only evaluate b if a is false
                        let result = self.new_temp(Ty::Bool);
                        let l = self.lower_expr(left)?;

                        let short_circuit_block = self.new_block();
                        let eval_right_block = self.new_block();
                        let merge_block = self.new_block();

                        // If left is true, short-circuit to true; else evaluate right
                        self.terminate(Terminator::If {
                            cond: l,
                            then_block: short_circuit_block,
                            else_block: eval_right_block,
                        });

                        // Short-circuit: left was true, result is true
                        self.current_block = Some(short_circuit_block);
                        self.emit(StatementKind::Assign(
                            result,
                            Rvalue::Use(Operand::Constant(Constant::Bool(true))),
                        ));
                        self.terminate(Terminator::Goto(merge_block));

                        // Evaluate right operand
                        self.current_block = Some(eval_right_block);
                        let r = self.lower_expr(right)?;
                        self.emit(StatementKind::Assign(result, Rvalue::Use(r)));
                        self.terminate(Terminator::Goto(merge_block));

                        self.current_block = Some(merge_block);
                        Some(Operand::Local(result))
                    }
                    _ => {
                        // Other binary ops: evaluate both operands
                        let l = self.lower_expr(left)?;
                        let r = self.lower_expr(right)?;
                        let bin_op = self.lower_bin_op(*op);
                        let result_ty = self.binary_op_result_type(bin_op, &l, &r);
                        let result = self.new_temp(result_ty);
                        self.emit(StatementKind::Assign(
                            result,
                            Rvalue::BinaryOp(bin_op, l, r),
                        ));
                        Some(Operand::Local(result))
                    }
                }
            }

            ExprKind::Unary(op, operand) => match op {
                AstUnaryOp::Neg => {
                    let operand_ty = self.infer_expr_type(operand);
                    let op = self.lower_expr(operand)?;
                    let result = self.new_temp(operand_ty);
                    self.emit(StatementKind::Assign(
                        result,
                        Rvalue::UnaryOp(UnOp::Neg, op),
                    ));
                    Some(Operand::Local(result))
                }
                AstUnaryOp::Not => {
                    let op = self.lower_expr(operand)?;
                    let result = self.new_temp(Ty::Bool);
                    self.emit(StatementKind::Assign(
                        result,
                        Rvalue::UnaryOp(UnOp::Not, op),
                    ));
                    Some(Operand::Local(result))
                }
                AstUnaryOp::Ref => {
                    if let ExprKind::Ident(ident) = &operand.kind
                        && let Some(&local) = self.vars.get(&ident.name)
                    {
                        let inner_ty = self.local_types.get(&local).cloned().unwrap_or(Ty::Unit);
                        let result = self.new_temp(Ty::Ref(
                            Box::new(inner_ty),
                            crate::types::Mutability::Immutable,
                        ));
                        self.emit(StatementKind::Assign(
                            result,
                            Rvalue::Ref(local, Mutability::Immutable),
                        ));
                        return Some(Operand::Local(result));
                    }
                    self.lower_expr(operand)
                }
                AstUnaryOp::RefMut => {
                    if let ExprKind::Ident(ident) = &operand.kind
                        && let Some(&local) = self.vars.get(&ident.name)
                    {
                        let inner_ty = self.local_types.get(&local).cloned().unwrap_or(Ty::Unit);
                        let result = self.new_temp(Ty::Ref(
                            Box::new(inner_ty),
                            crate::types::Mutability::Mutable,
                        ));
                        self.emit(StatementKind::Assign(
                            result,
                            Rvalue::Ref(local, Mutability::Mutable),
                        ));
                        return Some(Operand::Local(result));
                    }
                    self.lower_expr(operand)
                }
                AstUnaryOp::Deref => {
                    let operand_ty = self.infer_expr_type(operand);
                    let inner_ty = match operand_ty {
                        Ty::Ref(inner, _) => *inner,
                        _ => Ty::Unit,
                    };
                    let op = self.lower_expr(operand)?;
                    let result = self.new_temp(inner_ty);
                    self.emit(StatementKind::Assign(result, Rvalue::Deref(op)));
                    Some(Operand::Local(result))
                }
            },

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
                if let ExprKind::Path(path) = &callee.kind
                    && path.segments.len() == 2
                {
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
                        let name = path
                            .segments
                            .iter()
                            .map(|s| s.name.clone())
                            .collect::<Vec<_>>()
                            .join("::");
                        (true, Some(name))
                    }
                    _ => (false, None), // Expression that evaluates to closure
                };

                // Lower arguments
                let mut mir_args = Vec::new();
                let mut mir_arg_pass_modes: Vec<PassMode> = Vec::new();
                for arg in args {
                    if let Some(op) = self.lower_expr(&arg.value) {
                        mir_args.push(op);
                        mir_arg_pass_modes.push(lower_pass_mode(arg.pass_mode));
                    }
                }

                // Fill in default arguments if needed
                if is_direct
                    && let Some(fn_name_ref) = &func_name
                    && let Some(defaults) = self.fn_defaults.get(fn_name_ref).cloned()
                {
                    // Add default values for missing arguments
                    for i in mir_args.len()..defaults.len() {
                        if let Some(Some(default_expr)) = defaults.get(i)
                            && let Some(op) = self.lower_expr(default_expr)
                        {
                            mir_args.push(op);
                        }
                    }
                }

                // Get return type for the function
                let return_ty = if let Some(ref name) = func_name {
                    self.get_function_return_type(name)
                } else {
                    self.infer_expr_type(expr)
                };
                let result = self.new_temp(return_ty);
                let next_block = self.new_block();

                if is_direct {
                    // Direct function call
                    let func = match func_name {
                        Some(name) => name,
                        None => {
                            eprintln!(
                                "Internal error: function call without function name at {:?}",
                                expr.span
                            );
                            return None;
                        }
                    };

                    self.terminate(Terminator::Call {
                        func,
                        args: mir_args,
                        arg_pass_modes: mir_arg_pass_modes,
                        dest: Some(result),
                        next: next_block,
                    });
                } else {
                    // Indirect call through closure variable
                    let callee_op = self.lower_expr(callee)?;
                    self.terminate(Terminator::CallIndirect {
                        callee: callee_op,
                        args: mir_args,
                        arg_pass_modes: mir_arg_pass_modes,
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

                // Try to infer receiver type from expression for method resolution
                let receiver_type = self.infer_receiver_type(receiver);

                // Resolve method name to built-in function or qualified name
                let func_name =
                    self.resolve_method_with_type(&method.name, receiver_type.as_deref());

                // Create call with proper return type
                let return_ty = self.get_method_return_type(&method.name);
                let result = self.new_temp(return_ty);
                let next_block = self.new_block();
                self.terminate(Terminator::Call {
                    func: func_name,
                    args: mir_args,
                    arg_pass_modes: vec![],
                    dest: Some(result),
                    next: next_block,
                });
                self.current_block = Some(next_block);

                Some(Operand::Local(result))
            }

            ExprKind::If(if_expr) => self.lower_if(if_expr, expr.span),

            ExprKind::Match(scrutinee, arms) => self.lower_match(scrutinee, arms, expr.span),

            ExprKind::For(label, pattern, iter, body) => self.lower_for(
                label.as_ref().map(|l| l.name.clone()),
                pattern,
                iter,
                body,
                expr.span,
            ),

            ExprKind::While(label, cond, body) => self.lower_while(
                label.as_ref().map(|l| l.name.clone()),
                cond,
                body,
                expr.span,
            ),

            ExprKind::Loop(label, body) => {
                self.lower_loop(label.as_ref().map(|l| l.name.clone()), body, expr.span)
            }

            ExprKind::Block(block) => self.lower_block(block),

            ExprKind::Return(value) => {
                let op = value.as_ref().and_then(|v| self.lower_expr(v));
                self.terminate(Terminator::Return(op));
                None
            }

            ExprKind::Break(label, value) => {
                // Find the target loop context by label
                let target_ctx = if let Some(label_ident) = label {
                    self.loop_stack
                        .iter()
                        .rev()
                        .find(|ctx| ctx.label.as_ref() == Some(&label_ident.name))
                        .cloned()
                } else {
                    self.loop_stack.last().cloned()
                };

                if let Some(ctx) = target_ctx {
                    if let Some(val) = value.as_ref().and_then(|v| self.lower_expr(v))
                        && let Some(result_local) = ctx.result_local
                    {
                        self.emit(StatementKind::Assign(result_local, Rvalue::Use(val)));
                    }
                    self.terminate(Terminator::Goto(ctx.break_block));
                } else if let Some(label_ident) = label {
                    self.error(
                        format!("break label '{}' not found", label_ident.name),
                        expr.span,
                    );
                }
                None
            }

            ExprKind::Continue(label) => {
                // Find the target loop context by label
                let target_ctx = if let Some(label_ident) = label {
                    self.loop_stack
                        .iter()
                        .rev()
                        .find(|ctx| ctx.label.as_ref() == Some(&label_ident.name))
                        .cloned()
                } else {
                    self.loop_stack.last().cloned()
                };

                if let Some(ctx) = target_ctx {
                    self.terminate(Terminator::Goto(ctx.continue_block));
                } else if let Some(label_ident) = label {
                    self.error(
                        format!("continue label '{}' not found", label_ident.name),
                        expr.span,
                    );
                }
                None
            }

            ExprKind::Tuple(elements) => {
                // Collect element types for tuple type
                let elem_types: Vec<Ty> =
                    elements.iter().map(|e| self.infer_expr_type(e)).collect();
                let mut ops = Vec::new();
                for elem in elements {
                    if let Some(op) = self.lower_expr(elem) {
                        ops.push(op);
                    }
                }
                let result = self.new_temp(Ty::Tuple(elem_types));
                self.emit(StatementKind::Assign(result, Rvalue::Tuple(ops)));
                Some(Operand::Local(result))
            }

            ExprKind::Array(elements) => {
                // Get element type from first element
                let elem_ty = elements
                    .first()
                    .map(|e| self.infer_expr_type(e))
                    .unwrap_or(Ty::Unit);
                let mut ops = Vec::new();
                for elem in elements {
                    if let Some(op) = self.lower_expr(elem) {
                        ops.push(op);
                    }
                }
                let result = self.new_temp(Ty::List(Box::new(elem_ty)));
                self.emit(StatementKind::Assign(result, Rvalue::Array(ops)));
                Some(Operand::Local(result))
            }

            ExprKind::Index(base, index) => {
                // Infer element type from base array/string type
                let base_ty = self.infer_expr_type(base);
                let elem_ty = match base_ty {
                    Ty::List(inner) => *inner,
                    Ty::Str => Ty::Char,
                    _ => Ty::Unit,
                };
                let base_op = self.lower_expr(base)?;
                let index_op = self.lower_expr(index)?;
                let result = self.new_temp(elem_ty);
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::Index(base_op, index_op),
                ));
                Some(Operand::Local(result))
            }

            ExprKind::Field(base, field) => {
                // For field access, we'd need struct field type info
                // Use expression type inference as best effort
                let field_ty = self.infer_expr_type(expr);
                let base_op = self.lower_expr(base)?;
                let result = self.new_temp(field_ty);
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::Field(base_op, field.name.clone()),
                ));
                Some(Operand::Local(result))
            }

            ExprKind::TupleField(base, index) => {
                // Get tuple element type if possible
                let base_ty = self.infer_expr_type(base);
                let elem_ty = match base_ty {
                    Ty::Tuple(elems) => elems.get(*index).cloned().unwrap_or(Ty::Unit),
                    _ => Ty::Unit,
                };
                let base_op = self.lower_expr(base)?;
                let result = self.new_temp(elem_ty);
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::TupleField(base_op, *index),
                ));
                Some(Operand::Local(result))
            }

            ExprKind::Assign(target, value, _mutable) => {
                let value_ty = self.infer_expr_type(value);
                let val = self.lower_expr(value)?;

                // Handle assignment target
                if let ExprKind::Ident(ident) = &target.kind {
                    if let Some(&local) = self.vars.get(&ident.name) {
                        self.emit(StatementKind::Assign(local, Rvalue::Use(val)));
                        return Some(Operand::Local(local));
                    } else {
                        // New binding with inferred type
                        let local = self.new_local(value_ty, Some(ident.name.clone()));
                        self.vars.insert(ident.name.clone(), local);
                        self.emit(StatementKind::Assign(local, Rvalue::Use(val)));
                        return Some(Operand::Local(local));
                    }
                }

                // Handle index assignment: arr[i] := value
                if let ExprKind::Index(base, idx) = &target.kind
                    && let ExprKind::Ident(ident) = &base.kind
                    && let Some(&local) = self.vars.get(&ident.name)
                {
                    let idx_op = self.lower_expr(idx)?;
                    self.emit(StatementKind::IndexAssign(local, idx_op, val));
                    return Some(Operand::Constant(Constant::Unit));
                }

                None
            }

            ExprKind::AssignOp(target, op, value) => {
                let target_op = self.lower_expr(target)?;
                let val = self.lower_expr(value)?;
                let bin_op = self.lower_bin_op(*op);

                if let ExprKind::Ident(ident) = &target.kind
                    && let Some(&local) = self.vars.get(&ident.name)
                {
                    let result = self.new_temp(Ty::Int);
                    self.emit(StatementKind::Assign(
                        result,
                        Rvalue::BinaryOp(bin_op, target_op, val),
                    ));
                    self.emit(StatementKind::Assign(
                        local,
                        Rvalue::Use(Operand::Local(result)),
                    ));
                    return Some(Operand::Local(local));
                }

                None
            }

            ExprKind::Struct(path, fields, base) => {
                let name = path
                    .segments
                    .iter()
                    .map(|s| s.name.name.clone())
                    .collect::<Vec<_>>()
                    .join("::");

                let mut mir_fields = Vec::new();
                for field in fields {
                    let value = field
                        .value
                        .as_ref()
                        .and_then(|v| self.lower_expr(v))
                        .or_else(|| self.vars.get(&field.name.name).map(|&l| Operand::Local(l)));

                    if let Some(val) = value {
                        mir_fields.push((field.name.name.clone(), val));
                    }
                }

                // Handle base (struct update syntax)
                if let Some(base_expr) = base {
                    // Lower the base expression
                    let base_op = self.lower_expr(base_expr)?;

                    // Get struct field names from type info
                    if let Some(struct_fields) = self.get_struct_fields(&name) {
                        let existing_field_names: std::collections::HashSet<_> =
                            mir_fields.iter().map(|(n, _)| n.clone()).collect();

                        // Copy fields from base that aren't explicitly set
                        for (field_name, field_ty) in struct_fields {
                            if !existing_field_names.contains(&field_name) {
                                let field_local = self.new_temp(field_ty);
                                self.emit(StatementKind::Assign(
                                    field_local,
                                    Rvalue::Field(base_op.clone(), field_name.clone()),
                                ));
                                mir_fields.push((field_name, Operand::Local(field_local)));
                            }
                        }
                    }
                }

                // Use proper struct type
                let struct_ty = Ty::Named(crate::types::TypeId::new(&name), vec![]);
                let result = self.new_temp(struct_ty);
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::Struct(name, mir_fields),
                ));
                Some(Operand::Local(result))
            }

            ExprKind::Closure(closure) => {
                // Lambda lifting: convert closure to a top-level function

                // Generate unique name for the lifted function
                let func_name = format!("__closure_{}", self.closure_counter);
                self.closure_counter += 1;

                // Find free variables (captured from enclosing scope)
                let param_names: std::collections::HashSet<_> =
                    closure.params.iter().map(|p| p.name.name.clone()).collect();
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
                let saved_local_types = std::mem::take(&mut self.local_types);

                // Infer closure parameter and return types
                let param_types: Vec<Ty> = closure
                    .params
                    .iter()
                    .map(|p| {
                        p.ty.as_ref()
                            .map(|t| self.lower_type(t))
                            .unwrap_or(Ty::Unit)
                    })
                    .collect();
                let return_ty = closure
                    .return_type
                    .as_ref()
                    .map(|t| self.lower_type(t))
                    .unwrap_or(Ty::Unit);

                // Create the lifted function with signature: fn(captures..., params...)
                let mut params: Vec<(Local, Ty)> = Vec::new();
                let mut new_fn = Function::new(func_name.clone(), vec![], return_ty.clone());

                // Add captured variables as parameters first
                for var_name in &free_vars {
                    let captured_ty = self
                        .var_full_types
                        .get(var_name)
                        .cloned()
                        .unwrap_or(Ty::Unit);
                    let local = new_fn.add_local(captured_ty.clone(), Some(var_name.clone()));
                    params.push((local, captured_ty));
                    self.vars.insert(var_name.clone(), local);
                }

                // Add closure parameters
                for (param, param_ty) in closure.params.iter().zip(param_types.iter()) {
                    let local = new_fn.add_local(param_ty.clone(), Some(param.name.name.clone()));
                    params.push((local, param_ty.clone()));
                    self.vars.insert(param.name.name.clone(), local);
                }

                new_fn.params = params;

                // Create entry block and set it
                let entry = new_fn.add_block();
                new_fn.entry_block = entry;
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
                    self.program
                        .functions
                        .insert(func_name.clone(), finished_fn);
                }

                // Restore lowering state
                self.current_fn = saved_fn;
                self.current_block = saved_block;
                self.vars = saved_vars;
                self.local_types = saved_local_types;

                // Create closure value with proper type
                let closure_ty = Ty::Fn(param_types, Box::new(return_ty));
                let result = self.new_temp(closure_ty);
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::Closure {
                        func_name,
                        captures,
                    },
                ));
                Some(Operand::Local(result))
            }

            ExprKind::Pipeline(left, right) => {
                // Pipeline `a | b` desugars to `b(a)`
                let arg = self.lower_expr(left)?;

                match &right.kind {
                    ExprKind::Ident(func_name) => {
                        let return_ty = self.get_function_return_type(&func_name.name);
                        let result = self.new_temp(return_ty);
                        let next_block = self.new_block();
                        self.terminate(Terminator::Call {
                            func: func_name.name.clone(),
                            args: vec![arg],
                            arg_pass_modes: vec![],
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
                            let return_ty = self.get_function_return_type(&func_name.name);
                            let result = self.new_temp(return_ty);
                            let next_block = self.new_block();
                            self.terminate(Terminator::Call {
                                func: func_name.name.clone(),
                                args,
                                arg_pass_modes: vec![],
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
                let some_ok_block = self.new_block(); // Some or Ok
                let none_err_block = self.new_block(); // None or Err
                let continue_block = self.new_block();

                // Switch on discriminant:
                // For Option: None=0, Some=1 - continue on disc==1 (Some)
                // For Result: Ok=0, Err=1 - continue on disc==0 (Ok)
                // Determine if this is Option or Result type
                let inner_type = self.infer_expr_type(inner);
                let is_result = matches!(inner_type, Ty::Result(_, _));

                // Branch based on type:
                // - Option: discriminant > 0 means Some (has value, continue)
                // - Result: discriminant == 0 means Ok (has value, continue)
                let has_value = self.new_temp(Ty::Bool);
                if is_result {
                    // Result: Ok has discriminant 0
                    self.emit(StatementKind::Assign(
                        has_value,
                        Rvalue::BinaryOp(
                            BinOp::Eq,
                            Operand::Copy(disc),
                            Operand::Constant(Constant::Int(0)),
                        ),
                    ));
                } else {
                    // Option: Some has discriminant 1
                    self.emit(StatementKind::Assign(
                        has_value,
                        Rvalue::BinaryOp(
                            BinOp::Gt,
                            Operand::Copy(disc),
                            Operand::Constant(Constant::Int(0)),
                        ),
                    ));
                }
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
                self.emit(StatementKind::Assign(
                    extracted,
                    Rvalue::EnumField(scrutinee, 0),
                ));
                self.terminate(Terminator::Goto(continue_block));

                self.current_block = Some(continue_block);
                Some(Operand::Local(extracted))
            }

            ExprKind::Coalesce(left, right) => {
                // Coalesce operator: left ?? right
                // Returns the inner value of left if it's Some, otherwise evaluates right

                let left_val = self.lower_expr(left)?;

                // Store the left value
                let scrutinee = self.new_temp(Ty::Int);
                self.emit(StatementKind::Assign(scrutinee, Rvalue::Use(left_val)));

                // Get discriminant to check if Some vs None
                let disc = self.new_temp(Ty::Int);
                self.emit(StatementKind::Assign(disc, Rvalue::Discriminant(scrutinee)));

                // Create blocks
                let some_block = self.new_block(); // Left is Some - extract value
                let none_block = self.new_block(); // Left is None - use right
                let continue_block = self.new_block();

                // Result variable (use same type as left's inner type, or Int as fallback)
                let result = self.new_temp(Ty::Int);

                // Branch: discriminant > 0 means Some (has value)
                // Option: None=0, Some=1
                let has_value = self.new_temp(Ty::Bool);
                self.emit(StatementKind::Assign(
                    has_value,
                    Rvalue::BinaryOp(
                        BinOp::Gt,
                        Operand::Copy(disc),
                        Operand::Constant(Constant::Int(0)),
                    ),
                ));
                self.terminate(Terminator::If {
                    cond: Operand::Local(has_value),
                    then_block: some_block,
                    else_block: none_block,
                });

                // Some block: extract the inner value
                self.current_block = Some(some_block);
                let extracted = self.new_temp(Ty::Int);
                self.emit(StatementKind::Assign(
                    extracted,
                    Rvalue::EnumField(scrutinee, 0),
                ));
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::Use(Operand::Local(extracted)),
                ));
                self.terminate(Terminator::Goto(continue_block));

                // None block: evaluate and use the right expression
                self.current_block = Some(none_block);
                if let Some(right_val) = self.lower_expr(right) {
                    self.emit(StatementKind::Assign(result, Rvalue::Use(right_val)));
                }
                self.terminate(Terminator::Goto(continue_block));

                self.current_block = Some(continue_block);
                Some(Operand::Local(result))
            }

            ExprKind::Paren(inner) => self.lower_expr(inner),

            ExprKind::Cast(inner, target_ty) => {
                let operand = self.lower_expr(inner)?;
                let target = self.lower_type(target_ty);
                let result = self.new_temp(target.clone());
                self.emit(StatementKind::Assign(result, Rvalue::Cast(operand, target)));
                Some(Operand::Local(result))
            }

            ExprKind::Path(path) => {
                // Try to resolve as variable first
                if path.segments.len() == 1 {
                    let name = &path.segments[0].name;
                    if let Some(&local) = self.vars.get(name) {
                        return Some(Operand::Local(local));
                    }
                    // Check if it's a unit enum variant (like None)
                    if name.as_str() == "None" {
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

            ExprKind::Async(block) => {
                // Lower async block - for now, just lower the block and wrap in Future

                self.lower_block(block)
            }

            ExprKind::Await(inner) => {
                // Lower await expression
                let task_operand = self.lower_expr(inner)?;
                let result = self.new_temp(Ty::fresh_var());
                let next_block = self.new_block();
                self.terminate(Terminator::Await {
                    task: task_operand,
                    dest: Some(result),
                    next: next_block,
                });
                self.current_block = Some(next_block);
                Some(Operand::Local(result))
            }

            ExprKind::Spawn(inner) => {
                // Lower spawn expression
                let expr_operand = self.lower_expr(inner)?;
                let result = self.new_temp(Ty::Task(Box::new(Ty::fresh_var())));
                let next_block = self.new_block();
                self.terminate(Terminator::Spawn {
                    expr: expr_operand,
                    dest: Some(result),
                    next: next_block,
                });
                self.current_block = Some(next_block);
                Some(Operand::Local(result))
            }

            ExprKind::WhileLet(label, pattern, value, body) => {
                // while-let: wh Some(x) = expr { ... }
                // Similar to while, but condition is pattern match
                let cond_block = self.new_block();
                let body_block = self.new_block();
                let exit_block = self.new_block();

                self.loop_stack.push(LoopContext {
                    label: label.as_ref().map(|l| l.name.clone()),
                    continue_block: cond_block,
                    break_block: exit_block,
                    result_local: None,
                });

                self.terminate(Terminator::Goto(cond_block));
                self.current_block = Some(cond_block);

                // Evaluate the value expression
                let val = self.lower_expr(value)?;
                let scrut_local = if let Operand::Local(l) = val {
                    l
                } else {
                    let temp = self.new_temp(Ty::fresh_var());
                    self.emit(StatementKind::Assign(temp, Rvalue::Use(val)));
                    temp
                };

                // Check if pattern matches (simplified: only Some(x) supported)
                match &pattern.kind {
                    PatternKind::Struct(path, fields, _) => {
                        let variant_name = path
                            .segments
                            .last()
                            .map(|s| s.name.name.as_str())
                            .unwrap_or("");
                        let disc = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            disc,
                            Rvalue::Discriminant(scrut_local),
                        ));
                        let expected = self.get_variant_discriminant(variant_name);
                        let exp_local = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            exp_local,
                            Rvalue::Use(Operand::Constant(Constant::Int(expected))),
                        ));
                        let cond = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(
                            cond,
                            Rvalue::BinaryOp(
                                BinOp::Eq,
                                Operand::Copy(disc),
                                Operand::Copy(exp_local),
                            ),
                        ));
                        self.terminate(Terminator::If {
                            cond: Operand::Copy(cond),
                            then_block: body_block,
                            else_block: exit_block,
                        });

                        // Bind fields in body block
                        self.current_block = Some(body_block);
                        for (idx, field) in fields.iter().enumerate() {
                            let field_local =
                                self.new_local(Ty::Unit, Some(field.name.name.clone()));
                            self.vars.insert(field.name.name.clone(), field_local);
                            self.emit(StatementKind::Assign(
                                field_local,
                                Rvalue::EnumField(scrut_local, idx),
                            ));
                        }
                    }
                    PatternKind::Ident(ident, _, _)
                        if ident.name == "Some" || self.enum_variants.contains_key(&ident.name) =>
                    {
                        let disc = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            disc,
                            Rvalue::Discriminant(scrut_local),
                        ));
                        let expected = self.get_variant_discriminant(&ident.name);
                        let exp_local = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            exp_local,
                            Rvalue::Use(Operand::Constant(Constant::Int(expected))),
                        ));
                        let cond = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(
                            cond,
                            Rvalue::BinaryOp(
                                BinOp::Eq,
                                Operand::Copy(disc),
                                Operand::Copy(exp_local),
                            ),
                        ));
                        self.terminate(Terminator::If {
                            cond: Operand::Copy(cond),
                            then_block: body_block,
                            else_block: exit_block,
                        });
                        self.current_block = Some(body_block);
                    }
                    _ => {
                        self.terminate(Terminator::Goto(body_block));
                        self.current_block = Some(body_block);
                    }
                }

                self.lower_block(body);
                if self
                    .current_function()
                    .ok()
                    .and_then(|f| {
                        self.current_block_id()
                            .ok()
                            .map(|b| f.block(b).terminator.is_none())
                    })
                    .unwrap_or(false)
                {
                    self.terminate(Terminator::Goto(cond_block));
                }

                self.loop_stack.pop();
                self.current_block = Some(exit_block);
                Some(Operand::Constant(Constant::Unit))
            }

            ExprKind::Range(start, end, inclusive) => {
                // Range expression - create a Range struct/tuple
                // For now, create as a tuple (start, end, inclusive)
                let start_val = if let Some(s) = start {
                    self.lower_expr(s)?
                } else {
                    Operand::Constant(Constant::Int(0))
                };
                let end_val = if let Some(e) = end {
                    self.lower_expr(e)?
                } else {
                    Operand::Constant(Constant::Int(i64::MAX))
                };
                let incl_val = Operand::Constant(Constant::Bool(*inclusive));

                let result = self.new_temp(Ty::Tuple(vec![Ty::Int, Ty::Int, Ty::Bool]));
                self.emit(StatementKind::Assign(
                    result,
                    Rvalue::Tuple(vec![start_val, end_val, incl_val]),
                ));
                Some(Operand::Local(result))
            }

            ExprKind::ArrayRepeat(value, count) => {
                // [value; count] - array with repeated value
                // Lower as a call to a builtin that creates repeated arrays
                let val = self.lower_expr(value)?;
                let cnt = self.lower_expr(count)?;
                let result = self.new_temp(Ty::List(Box::new(Ty::fresh_var())));
                let next_block = self.new_block();
                self.terminate(Terminator::Call {
                    func: "array_repeat".to_string(),
                    args: vec![val, cnt],
                    arg_pass_modes: vec![],
                    dest: Some(result),
                    next: next_block,
                });
                self.current_block = Some(next_block);
                Some(Operand::Local(result))
            }

            ExprKind::MapOrSet(entries) => {
                // Map or set literal { k: v, ... } or { a, b, ... }
                // Build as a series of map_set calls
                let result = self.new_temp(Ty::Map(
                    Box::new(Ty::fresh_var()),
                    Box::new(Ty::fresh_var()),
                ));

                // First create an empty map
                let init_block = self.new_block();
                self.terminate(Terminator::Call {
                    func: "map_new".to_string(),
                    args: vec![],
                    arg_pass_modes: vec![],
                    dest: Some(result),
                    next: init_block,
                });
                self.current_block = Some(init_block);

                // Then insert each entry
                for entry in entries {
                    let key = self.lower_expr(&entry.key)?;
                    let val = if let Some(v) = &entry.value {
                        self.lower_expr(v)?
                    } else {
                        // Set literal - value is unit
                        Operand::Constant(Constant::Bool(true))
                    };
                    let next = self.new_block();
                    self.terminate(Terminator::Call {
                        func: "map_set".to_string(),
                        args: vec![Operand::Copy(result), key, val],
                        arg_pass_modes: vec![],
                        dest: None,
                        next,
                    });
                    self.current_block = Some(next);
                }

                Some(Operand::Local(result))
            }

            ExprKind::FieldShorthand(field) => {
                // .field shorthand closure - desugar to |x| x.field
                // For now, error - this should be desugared earlier
                self.error(
                    format!(
                        "field shorthand '.{}' should be desugared before MIR lowering",
                        field.name
                    ),
                    expr.span,
                );
                None
            }

            ExprKind::OpShorthand(op, operand, left) => {
                // Operator shorthand like (+ 10) - desugar to |x| x + 10
                // For now, error - this should be desugared earlier
                let _ = (op, operand, left);
                self.error(
                    "operator shorthand should be desugared before MIR lowering".to_string(),
                    expr.span,
                );
                None
            }

            ExprKind::Unsafe(block) => {
                // Unsafe block - lower as regular block (safety checking done elsewhere)
                self.lower_block(block)
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn lower_if(&mut self, if_expr: &crate::parser::IfExpr, span: Span) -> Option<Operand> {
        let cond = self.lower_expr(&if_expr.condition)?;

        let then_block = self.new_block();
        let else_block = self.new_block();
        let merge_block = self.new_block();

        // Infer result type from the then branch expression (without lowering yet)
        let result_ty = match &if_expr.then_branch {
            IfBranch::Expr(e) => self.infer_expr_type(e),
            IfBranch::Block(_) => Ty::Unit, // Block expressions return Unit unless last expr
        };

        // Result variable for if expression with inferred type
        let result = self.new_temp(result_ty);

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
        if self
            .current_function()
            .ok()?
            .block(self.current_block_id().ok()?)
            .terminator
            .is_none()
        {
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
        if self
            .current_function()
            .ok()?
            .block(self.current_block_id().ok()?)
            .terminator
            .is_none()
        {
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
        self.emit(StatementKind::Assign(
            scrut_local,
            Rvalue::Use(scrutinee_op),
        ));

        let result = self.new_temp(Ty::Unit);
        let exit_block = self.new_block();

        // Collect arm info for processing
        let mut arm_blocks: Vec<(BlockId, BlockId, Option<BlockId>)> = Vec::new(); // (test_block, body_block, guard_block)

        for arm in arms {
            let test_block = self.new_block();
            let body_block = self.new_block();
            let guard_block = if arm.guard.is_some() {
                Some(self.new_block())
            } else {
                None
            };
            arm_blocks.push((test_block, body_block, guard_block));
        }

        // Start by jumping to first test
        if !arm_blocks.is_empty() {
            self.terminate(Terminator::Goto(arm_blocks[0].0));
        } else {
            self.terminate(Terminator::Goto(exit_block));
        }

        // Process each arm
        for (i, arm) in arms.iter().enumerate() {
            let (test_block, body_block, guard_block) = arm_blocks[i];
            // The target after a successful pattern match: guard block if guard exists, else body
            let pattern_target = guard_block.unwrap_or(body_block);
            let next_test = if i + 1 < arm_blocks.len() {
                arm_blocks[i + 1].0
            } else {
                exit_block
            };

            self.current_block = Some(test_block);

            match &arm.pattern.kind {
                PatternKind::Wildcard => {
                    // Always matches, go directly to body (or guard)
                    self.terminate(Terminator::Goto(pattern_target));
                }

                PatternKind::Ident(ident, _, _) => {
                    // Check if this identifier is a known enum variant (or built-in like None)
                    let is_enum_variant = self
                        .enum_variants
                        .get(&ident.name)
                        .map(|(_, count)| *count == 0)
                        .unwrap_or(false)
                        || ident.name == "None";

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
                            then_block: pattern_target,
                            else_block: next_test,
                        });
                    } else {
                        // Not an enum variant - bind the variable and go to body (or guard)
                        let local = self.new_local(Ty::Unit, Some(ident.name.clone()));
                        self.vars.insert(ident.name.clone(), local);
                        self.emit(StatementKind::Assign(
                            local,
                            Rvalue::Use(Operand::Copy(scrut_local)),
                        ));
                        self.terminate(Terminator::Goto(pattern_target));
                    }
                }

                PatternKind::Literal(Literal {
                    kind: LiteralKind::Int(n),
                    ..
                }) => {
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
                        then_block: pattern_target,
                        else_block: next_test,
                    });
                }

                PatternKind::Literal(Literal {
                    kind: LiteralKind::String(s),
                    ..
                }) => {
                    // Compare string and branch
                    let lit_local = self.new_temp(Ty::Str);
                    self.emit(StatementKind::Assign(
                        lit_local,
                        Rvalue::Use(Operand::Constant(Constant::Str(s.clone()))),
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
                        then_block: pattern_target,
                        else_block: next_test,
                    });
                }

                PatternKind::Literal(Literal {
                    kind: LiteralKind::Bool(b),
                    ..
                }) => {
                    // Compare bool and branch
                    let lit_local = self.new_temp(Ty::Bool);
                    self.emit(StatementKind::Assign(
                        lit_local,
                        Rvalue::Use(Operand::Constant(Constant::Bool(*b))),
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
                        then_block: pattern_target,
                        else_block: next_test,
                    });
                }

                PatternKind::Literal(Literal {
                    kind: LiteralKind::Char(c),
                    ..
                }) => {
                    // Compare char and branch
                    let lit_local = self.new_temp(Ty::Char);
                    self.emit(StatementKind::Assign(
                        lit_local,
                        Rvalue::Use(Operand::Constant(Constant::Char(*c))),
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
                        then_block: pattern_target,
                        else_block: next_test,
                    });
                }

                PatternKind::Literal(Literal {
                    kind: LiteralKind::Float(f),
                    ..
                }) => {
                    // Compare float and branch
                    let lit_local = self.new_temp(Ty::Float);
                    self.emit(StatementKind::Assign(
                        lit_local,
                        Rvalue::Use(Operand::Constant(Constant::Float(*f))),
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
                        then_block: pattern_target,
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
                        Rvalue::BinaryOp(
                            BinOp::Eq,
                            Operand::Copy(disc_local),
                            Operand::Copy(expected),
                        ),
                    ));

                    // Check if there are fields to extract
                    if fields.is_empty() {
                        // Unit variant, go directly to body (or guard)
                        self.terminate(Terminator::If {
                            cond: Operand::Copy(cond),
                            then_block: pattern_target,
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

                        // In extract block: bind fields, then goto guard or body
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
                                        let field_local = self
                                            .new_local(Ty::Unit, Some(inner_ident.name.clone()));
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
                                let field_local =
                                    self.new_local(Ty::Unit, Some(binding_name.clone()));
                                self.vars.insert(binding_name.clone(), field_local);
                                self.emit(StatementKind::Assign(
                                    field_local,
                                    Rvalue::EnumField(scrut_local, idx),
                                ));
                            }
                        }
                        self.terminate(Terminator::Goto(pattern_target));
                    }
                }

                PatternKind::Or(patterns) => {
                    // Or pattern: try each alternative, if any matches go to body
                    // Create a block for each alternative and chain them
                    let mut alt_blocks = Vec::new();
                    for _ in 0..patterns.len() {
                        alt_blocks.push(self.new_block());
                    }

                    // Start with the first alternative
                    self.terminate(Terminator::Goto(alt_blocks[0]));

                    for (i, pat) in patterns.iter().enumerate() {
                        self.current_block = Some(alt_blocks[i]);
                        let next_alt = if i + 1 < alt_blocks.len() {
                            alt_blocks[i + 1]
                        } else {
                            next_test
                        };

                        // For simple patterns (literal, ident), inline the check
                        match &pat.kind {
                            PatternKind::Literal(Literal {
                                kind: LiteralKind::Int(n),
                                ..
                            }) => {
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
                                    else_block: next_alt,
                                });
                            }
                            PatternKind::Ident(ident, _, _) => {
                                // Check if enum variant
                                let is_variant = self
                                    .enum_variants
                                    .get(&ident.name)
                                    .map(|(_, c)| *c == 0)
                                    .unwrap_or(false)
                                    || ident.name == "None";
                                if is_variant {
                                    let disc = self.new_temp(Ty::Int);
                                    self.emit(StatementKind::Assign(
                                        disc,
                                        Rvalue::Discriminant(scrut_local),
                                    ));
                                    let exp_disc = self.get_variant_discriminant(&ident.name);
                                    let exp = self.new_temp(Ty::Int);
                                    self.emit(StatementKind::Assign(
                                        exp,
                                        Rvalue::Use(Operand::Constant(Constant::Int(exp_disc))),
                                    ));
                                    let cond = self.new_temp(Ty::Bool);
                                    self.emit(StatementKind::Assign(
                                        cond,
                                        Rvalue::BinaryOp(
                                            BinOp::Eq,
                                            Operand::Copy(disc),
                                            Operand::Copy(exp),
                                        ),
                                    ));
                                    self.terminate(Terminator::If {
                                        cond: Operand::Copy(cond),
                                        then_block: body_block,
                                        else_block: next_alt,
                                    });
                                } else {
                                    // Bind variable and match
                                    let local = self.new_local(Ty::Unit, Some(ident.name.clone()));
                                    self.vars.insert(ident.name.clone(), local);
                                    self.emit(StatementKind::Assign(
                                        local,
                                        Rvalue::Use(Operand::Copy(scrut_local)),
                                    ));
                                    self.terminate(Terminator::Goto(body_block));
                                }
                            }
                            PatternKind::Wildcard => {
                                self.terminate(Terminator::Goto(body_block));
                            }
                            _ => {
                                // Complex nested or-pattern alternatives not yet supported
                                self.terminate(Terminator::Goto(next_alt));
                            }
                        }
                    }
                    // Don't fall through to body_block setup - handled above
                    continue;
                }

                PatternKind::Range(start, end, inclusive) => {
                    // Range pattern: check if scrutinee is within range
                    let mut conditions = Vec::new();

                    if let Some(start_pat) = start
                        && let PatternKind::Literal(Literal {
                            kind: LiteralKind::Int(n),
                            ..
                        }) = &start_pat.kind
                    {
                        let start_val = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            start_val,
                            Rvalue::Use(Operand::Constant(Constant::Int(*n as i64))),
                        ));
                        let ge_cond = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(
                            ge_cond,
                            Rvalue::BinaryOp(
                                BinOp::Ge,
                                Operand::Copy(scrut_local),
                                Operand::Copy(start_val),
                            ),
                        ));
                        conditions.push(ge_cond);
                    }

                    if let Some(end_pat) = end
                        && let PatternKind::Literal(Literal {
                            kind: LiteralKind::Int(n),
                            ..
                        }) = &end_pat.kind
                    {
                        let end_val = self.new_temp(Ty::Int);
                        self.emit(StatementKind::Assign(
                            end_val,
                            Rvalue::Use(Operand::Constant(Constant::Int(*n as i64))),
                        ));
                        let cmp_op = if *inclusive { BinOp::Le } else { BinOp::Lt };
                        let le_cond = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(
                            le_cond,
                            Rvalue::BinaryOp(
                                cmp_op,
                                Operand::Copy(scrut_local),
                                Operand::Copy(end_val),
                            ),
                        ));
                        conditions.push(le_cond);
                    }

                    // Combine conditions with AND
                    let final_cond = if conditions.is_empty() {
                        let t = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(
                            t,
                            Rvalue::Use(Operand::Constant(Constant::Bool(true))),
                        ));
                        t
                    } else if conditions.len() == 1 {
                        conditions[0]
                    } else {
                        let combined = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(
                            combined,
                            Rvalue::BinaryOp(
                                BinOp::And,
                                Operand::Copy(conditions[0]),
                                Operand::Copy(conditions[1]),
                            ),
                        ));
                        combined
                    };

                    self.terminate(Terminator::If {
                        cond: Operand::Copy(final_cond),
                        then_block: pattern_target,
                        else_block: next_test,
                    });
                }

                PatternKind::Ref(inner, _is_mut) => {
                    // Reference pattern: dereference and match inner
                    // For now, just bind the value directly (simplified)
                    if let PatternKind::Ident(ident, _, _) = &inner.kind {
                        let local = self.new_local(Ty::Unit, Some(ident.name.clone()));
                        self.vars.insert(ident.name.clone(), local);
                        self.emit(StatementKind::Assign(
                            local,
                            Rvalue::Use(Operand::Copy(scrut_local)),
                        ));
                    }
                    self.terminate(Terminator::Goto(pattern_target));
                }

                PatternKind::Rest => {
                    // Rest pattern (used in list/tuple patterns) - matches anything
                    self.terminate(Terminator::Goto(pattern_target));
                }

                _ => {
                    // Remaining unsupported patterns - emit error and skip
                    self.error(
                        format!("unsupported pattern: {:?}", arm.pattern.kind),
                        arm.pattern.span,
                    );
                    self.terminate(Terminator::Goto(next_test));
                    continue;
                }
            }

            // Lower guard (if present)
            if let Some(guard_blk) = guard_block {
                self.current_block = Some(guard_blk);
                if let Some(ref guard_expr) = arm.guard {
                    if let Some(guard_val) = self.lower_expr(guard_expr) {
                        let guard_cond = self.new_temp(Ty::Bool);
                        self.emit(StatementKind::Assign(guard_cond, Rvalue::Use(guard_val)));
                        self.terminate(Terminator::If {
                            cond: Operand::Copy(guard_cond),
                            then_block: body_block,
                            else_block: next_test,
                        });
                    } else {
                        // Guard expression failed to lower, skip to next arm
                        self.terminate(Terminator::Goto(next_test));
                    }
                }
            }

            // Lower body
            self.current_block = Some(body_block);
            if let Some(val) = self.lower_expr(&arm.body) {
                self.emit(StatementKind::Assign(result, Rvalue::Use(val)));
            }
            if self
                .current_function()
                .ok()?
                .block(self.current_block_id().ok()?)
                .terminator
                .is_none()
            {
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
            // For user-defined enums, use FNV-1a hash to avoid collisions
            // (ASCII sum would collide e.g., "ab" == "ba")
            _ => {
                const FNV_OFFSET: u64 = 14695981039346656037;
                const FNV_PRIME: u64 = 1099511628211;
                let hash = variant.bytes().fold(FNV_OFFSET, |acc, b| {
                    (acc ^ b as u64).wrapping_mul(FNV_PRIME)
                });
                // Map to i64, keeping lower 63 bits positive
                (hash & 0x7FFFFFFFFFFFFFFF) as i64
            }
        }
    }

    /// Try to infer the receiver type from an expression.
    /// Returns the type name if it can be determined from the expression structure.
    fn infer_receiver_type(&self, expr: &Expr) -> Option<String> {
        match &expr.kind {
            // Variable reference - look up in var_types map
            ExprKind::Ident(ident) => self.var_types.get(&ident.name).cloned(),
            // Struct construction clearly tells us the type
            ExprKind::Struct(path, _, _) => path.segments.first().map(|seg| seg.name.name.clone()),
            // Function call result - could be a constructor
            ExprKind::Call(callee, _) => {
                if let ExprKind::Ident(ident) = &callee.kind {
                    // Check if this is a struct constructor (first char is uppercase)
                    if ident
                        .name
                        .chars()
                        .next()
                        .map(|c| c.is_uppercase())
                        .unwrap_or(false)
                    {
                        return Some(ident.name.clone());
                    }
                }
                None
            }
            // Field access - the base determines the type
            ExprKind::Field(base, _) => self.infer_receiver_type(base),
            // Method call result - can't easily determine
            _ => None,
        }
    }

    /// Resolve a method name to a function name with optional type hint.
    ///
    /// Maps common method names to their built-in function equivalents.
    /// For example: `.len()` -> `vec_len` or `str_len`, `.push(x)` -> `vec_push`
    fn resolve_method_with_type(&self, method_name: &str, receiver_type: Option<&str>) -> String {
        // If we have a receiver type and this method exists for that type, use it
        if let Some(type_name) = receiver_type {
            let qualified = format!("{}::{}", type_name, method_name);
            if let Some(qualified_names) = self.impl_methods.get(method_name)
                && qualified_names.contains(&qualified)
            {
                return qualified;
            }
            // Also check if function exists directly
            if self.program.functions.contains_key(&qualified) {
                return qualified;
            }
        }

        // Fall back to standard method resolution
        self.resolve_method(method_name)
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

            // Check impl methods (from impl blocks)
            // If there's exactly one qualified name for this method, use it
            // If multiple types have this method, we need receiver type info for proper resolution
            // For now, try all qualified names and use the first one that exists in program
            _ => {
                if let Some(qualified_names) = self.impl_methods.get(method_name) {
                    if qualified_names.len() == 1 {
                        return qualified_names[0].clone();
                    }
                    // Multiple types have this method - check which are available
                    // This is a heuristic; proper resolution needs type info
                    for qname in qualified_names {
                        if self.program.functions.contains_key(qname) {
                            return qname.clone();
                        }
                    }
                    // If none found yet (still lowering), return first one
                    if !qualified_names.is_empty() {
                        return qualified_names[0].clone();
                    }
                }
                // Default: use the method name as-is
                method_name.to_string()
            }
        }
    }

    fn lower_for(
        &mut self,
        label: Option<String>,
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
            return self.lower_for_range(
                label.clone(),
                pattern,
                start_opt,
                end_opt,
                *inclusive,
                body,
            );
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
        self.emit(StatementKind::Assign(
            idx_local,
            Rvalue::Use(Operand::Constant(Constant::Int(0))),
        ));

        // Get array length
        let len_local = self.new_temp(Ty::Int);
        let len_block = self.new_block();
        self.terminate(Terminator::Call {
            func: "vec_len".to_string(),
            args: vec![Operand::Copy(arr_local)],
            arg_pass_modes: vec![],
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
            label,
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
            Rvalue::BinaryOp(
                BinOp::Lt,
                Operand::Copy(idx_local),
                Operand::Copy(len_local),
            ),
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
                self.emit(StatementKind::Assign(
                    var_local,
                    Rvalue::Use(Operand::Copy(elem_local)),
                ));
                self.vars.insert(ident.name.clone(), var_local);
            }
            PatternKind::Tuple(patterns) if is_enumerate && patterns.len() == 2 => {
                // Enumerate pattern: `for i, x in arr.enumerate()`
                // First element is index, second is value
                if let PatternKind::Ident(idx_ident, _, _) = &patterns[0].kind {
                    let idx_var = self.new_local(Ty::Int, Some(idx_ident.name.clone()));
                    self.emit(StatementKind::Assign(
                        idx_var,
                        Rvalue::Use(Operand::Copy(idx_local)),
                    ));
                    self.vars.insert(idx_ident.name.clone(), idx_var);
                }
                if let PatternKind::Ident(val_ident, _, _) = &patterns[1].kind {
                    let val_var = self.new_local(Ty::Int, Some(val_ident.name.clone()));
                    self.emit(StatementKind::Assign(
                        val_var,
                        Rvalue::Use(Operand::Copy(elem_local)),
                    ));
                    self.vars.insert(val_ident.name.clone(), val_var);
                }
            }
            _ => {
                // Fallback: try to bind as simple identifier
                if let PatternKind::Ident(ident, _, _) = &pattern.kind {
                    let var_local = self.new_local(Ty::Int, Some(ident.name.clone()));
                    self.emit(StatementKind::Assign(
                        var_local,
                        Rvalue::Use(Operand::Copy(elem_local)),
                    ));
                    self.vars.insert(ident.name.clone(), var_local);
                }
            }
        }

        // Execute loop body
        self.lower_block(body);

        // If body didn't terminate, go to increment
        if self
            .current_function()
            .ok()?
            .block(self.current_block_id().ok()?)
            .terminator
            .is_none()
        {
            self.terminate(Terminator::Goto(incr_block));
        }

        // Increment block: idx = idx + 1
        self.current_block = Some(incr_block);
        self.emit(StatementKind::Assign(
            idx_local,
            Rvalue::BinaryOp(
                BinOp::Add,
                Operand::Copy(idx_local),
                Operand::Constant(Constant::Int(1)),
            ),
        ));
        self.terminate(Terminator::Goto(cond_block));

        self.loop_stack.pop();
        self.current_block = Some(exit_block);
        None
    }

    /// Lower a for loop with range iteration: `for i in 0..10` or `for i in start..=end`
    fn lower_for_range(
        &mut self,
        label: Option<String>,
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
            label,
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
            self.emit(StatementKind::Assign(
                var_local,
                Rvalue::Use(Operand::Copy(idx_local)),
            ));
            self.vars.insert(ident.name.clone(), var_local);
        }

        // Execute loop body
        self.lower_block(body);

        // If body didn't terminate, go to increment
        if self
            .current_function()
            .ok()?
            .block(self.current_block_id().ok()?)
            .terminator
            .is_none()
        {
            self.terminate(Terminator::Goto(incr_block));
        }

        // Increment block: idx = idx + 1
        self.current_block = Some(incr_block);
        self.emit(StatementKind::Assign(
            idx_local,
            Rvalue::BinaryOp(
                BinOp::Add,
                Operand::Copy(idx_local),
                Operand::Constant(Constant::Int(1)),
            ),
        ));
        self.terminate(Terminator::Goto(cond_block));

        self.loop_stack.pop();
        self.current_block = Some(exit_block);
        None
    }

    fn lower_while(
        &mut self,
        label: Option<String>,
        cond: &Expr,
        body: &AstBlock,
        _span: Span,
    ) -> Option<Operand> {
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let exit_block = self.new_block();

        // Push loop context
        self.loop_stack.push(LoopContext {
            label,
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
        if self
            .current_function()
            .ok()?
            .block(self.current_block_id().ok()?)
            .terminator
            .is_none()
        {
            self.terminate(Terminator::Goto(cond_block));
        }

        self.loop_stack.pop();
        self.current_block = Some(exit_block);
        None
    }

    fn lower_loop(
        &mut self,
        label: Option<String>,
        body: &AstBlock,
        _span: Span,
    ) -> Option<Operand> {
        let body_block = self.new_block();
        let exit_block = self.new_block();
        let result = self.new_temp(Ty::Int);

        // Push loop context
        self.loop_stack.push(LoopContext {
            label,
            continue_block: body_block,
            break_block: exit_block,
            result_local: Some(result),
        });

        // Jump to body
        self.terminate(Terminator::Goto(body_block));

        // Body block
        self.current_block = Some(body_block);
        self.lower_block(body);
        if self
            .current_function()
            .ok()?
            .block(self.current_block_id().ok()?)
            .terminator
            .is_none()
        {
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
                let type_args: Vec<Ty> = first_seg
                    .args
                    .as_ref()
                    .map(|args| {
                        args.args
                            .iter()
                            .filter_map(|arg| match arg {
                                crate::parser::GenericArg::Type(t) => Some(self.lower_type(t)),
                                _ => None,
                            })
                            .collect()
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
                    // Pointer-sized integers
                    "isize" => Ty::Isize,
                    "usize" => Ty::Usize,
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
                        let ok_ty = type_args.first().cloned().unwrap_or(Ty::Error);
                        let err_ty = type_args.get(1).cloned().unwrap_or(Ty::Str);
                        Ty::Result(Box::new(ok_ty), Box::new(err_ty))
                    }
                    // User-defined type
                    _ => Ty::Named(TypeId::new(name.clone()), type_args),
                }
            }

            AstTypeKind::List(inner) => Ty::List(Box::new(self.lower_type(inner))),

            AstTypeKind::Option(inner) => Ty::Option(Box::new(self.lower_type(inner))),

            AstTypeKind::Result(ok, err) => {
                let ok_ty = self.lower_type(ok);
                let err_ty = err.as_ref().map(|e| self.lower_type(e)).unwrap_or(Ty::Str);
                Ty::Result(Box::new(ok_ty), Box::new(err_ty))
            }

            AstTypeKind::Tuple(tys) => Ty::Tuple(tys.iter().map(|t| self.lower_type(t)).collect()),

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

            AstTypeKind::Map(key, value) => Ty::Map(
                Box::new(self.lower_type(key)),
                Box::new(self.lower_type(value)),
            ),

            AstTypeKind::Set(inner) => Ty::Set(Box::new(self.lower_type(inner))),

            AstTypeKind::Infer => Ty::Var(crate::types::TypeVar::fresh()),

            AstTypeKind::Never => Ty::Never,
        }
    }

    // Helper methods

    fn new_temp(&mut self, ty: Ty) -> Local {
        let func = match self.current_function_mut() {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Internal error: new_temp - {}", e.message);
                return Local(0);
            }
        };
        let local = func.add_local(ty.clone(), None);
        self.local_types.insert(local, ty);
        local
    }

    fn new_local(&mut self, ty: Ty, name: Option<String>) -> Local {
        let func = match self.current_function_mut() {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Internal error: new_local - {}", e.message);
                return Local(0);
            }
        };
        let local = func.add_local(ty.clone(), name.clone());
        self.local_types.insert(local, ty.clone());
        if let Some(n) = name {
            self.var_full_types.insert(n, ty);
        }
        local
    }

    // ========================================================================
    // Type Inference Helpers for MIR Type Propagation
    // ========================================================================

    /// Infer the type of an expression (for MIR type propagation)
    fn infer_expr_type(&self, expr: &Expr) -> Ty {
        match &expr.kind {
            ExprKind::Literal(lit) => self.literal_type(lit),

            ExprKind::Ident(ident) => {
                // Check variable types
                if let Some(ty) = self.var_full_types.get(&ident.name) {
                    return ty.clone();
                }
                // Check for known enum variants
                if let Some((enum_name, _)) = self.enum_variants.get(&ident.name) {
                    return Ty::Named(crate::types::TypeId::new(enum_name), vec![]);
                }
                // Default to Unit if unknown
                Ty::Unit
            }

            ExprKind::Binary(_left, op, right) => {
                match op {
                    // Comparison and logical operators return Bool
                    AstBinOp::Eq
                    | AstBinOp::Ne
                    | AstBinOp::Lt
                    | AstBinOp::Le
                    | AstBinOp::Gt
                    | AstBinOp::Ge
                    | AstBinOp::And
                    | AstBinOp::Or => Ty::Bool,
                    // Arithmetic operators return same type as operands
                    AstBinOp::Add
                    | AstBinOp::Sub
                    | AstBinOp::Mul
                    | AstBinOp::Div
                    | AstBinOp::Mod
                    | AstBinOp::BitAnd
                    | AstBinOp::BitOr
                    | AstBinOp::BitXor
                    | AstBinOp::Shl
                    | AstBinOp::Shr => {
                        // Use right operand type (left and right should match)
                        self.infer_expr_type(right)
                    }
                }
            }

            ExprKind::Unary(op, operand) => match op {
                AstUnaryOp::Not => Ty::Bool,
                AstUnaryOp::Neg => self.infer_expr_type(operand),
                AstUnaryOp::Ref => Ty::Ref(
                    Box::new(self.infer_expr_type(operand)),
                    crate::types::Mutability::Immutable,
                ),
                AstUnaryOp::RefMut => Ty::Ref(
                    Box::new(self.infer_expr_type(operand)),
                    crate::types::Mutability::Mutable,
                ),
                AstUnaryOp::Deref => match self.infer_expr_type(operand) {
                    Ty::Ref(inner, _) => *inner,
                    _ => Ty::Unit,
                },
            },

            ExprKind::Call(callee, _args) => {
                // Try to get function return type
                if let ExprKind::Ident(ident) = &callee.kind {
                    return self.get_function_return_type(&ident.name);
                }
                if let ExprKind::Path(path) = &callee.kind {
                    let name = path
                        .segments
                        .iter()
                        .map(|s| s.name.clone())
                        .collect::<Vec<_>>()
                        .join("::");
                    return self.get_function_return_type(&name);
                }
                Ty::Unit
            }

            ExprKind::MethodCall(_receiver, method, _args) => {
                self.get_method_return_type(&method.name)
            }

            ExprKind::Tuple(elements) => {
                Ty::Tuple(elements.iter().map(|e| self.infer_expr_type(e)).collect())
            }

            ExprKind::Array(elements) => {
                let elem_ty = elements
                    .first()
                    .map(|e| self.infer_expr_type(e))
                    .unwrap_or(Ty::Unit);
                Ty::List(Box::new(elem_ty))
            }

            ExprKind::Index(arr, _idx) => match self.infer_expr_type(arr) {
                Ty::List(elem) => *elem,
                Ty::Str => Ty::Char,
                _ => Ty::Unit,
            },

            ExprKind::Field(expr, _field) => {
                // For field access, we'd need struct field type info
                // For now, use Unit and let type checker handle it
                let _ = self.infer_expr_type(expr);
                Ty::Unit
            }

            ExprKind::Struct(path, _fields, _base) => {
                let name = path
                    .segments
                    .last()
                    .map(|s| s.name.name.clone())
                    .unwrap_or_default();
                Ty::Named(crate::types::TypeId::new(&name), vec![])
            }

            ExprKind::If(_) | ExprKind::Match(_, _) | ExprKind::Block(_) => {
                // These need deeper analysis; use Unit for now
                Ty::Unit
            }

            ExprKind::Closure(closure) => {
                let param_types: Vec<Ty> = closure
                    .params
                    .iter()
                    .map(|p| {
                        p.ty.as_ref()
                            .map(|t| self.lower_type(t))
                            .unwrap_or(Ty::Unit)
                    })
                    .collect();
                let return_ty = closure
                    .return_type
                    .as_ref()
                    .map(|t| self.lower_type(t))
                    .unwrap_or(Ty::Unit);
                Ty::Fn(param_types, Box::new(return_ty))
            }

            ExprKind::Range(start, end, _inclusive) => {
                // Range produces an iterator/list of elements
                let elem_ty = start
                    .as_ref()
                    .or(end.as_ref())
                    .map(|e| self.infer_expr_type(e))
                    .unwrap_or(Ty::Int);
                Ty::List(Box::new(elem_ty))
            }

            ExprKind::Cast(_expr, target) => self.lower_type(target),

            ExprKind::Try(inner) => {
                // Result<T, E>? -> T
                match self.infer_expr_type(inner) {
                    Ty::Result(ok, _) => *ok,
                    Ty::Option(inner) => *inner,
                    other => other,
                }
            }

            ExprKind::Await(inner) => {
                // Task<T>.await -> T
                match self.infer_expr_type(inner) {
                    Ty::Task(inner) => *inner,
                    other => other,
                }
            }

            ExprKind::Spawn(_) => Ty::Task(Box::new(Ty::Unit)),

            _ => Ty::Unit,
        }
    }

    /// Get the type of a literal
    fn literal_type(&self, lit: &Literal) -> Ty {
        match &lit.kind {
            LiteralKind::Int(_) => Ty::Int,
            LiteralKind::Float(_) => Ty::Float,
            LiteralKind::String(_) => Ty::Str,
            LiteralKind::Char(_) => Ty::Char,
            LiteralKind::Bool(_) => Ty::Bool,
            LiteralKind::None => Ty::Option(Box::new(Ty::Unit)),
        }
    }

    /// Get the type of an operand
    fn operand_type(&self, operand: &Operand) -> Ty {
        match operand {
            Operand::Constant(c) => self.constant_type(c),
            Operand::Local(local) | Operand::Copy(local) | Operand::Move(local) => {
                self.local_types.get(local).cloned().unwrap_or(Ty::Unit)
            }
        }
    }

    /// Get the type of a constant
    fn constant_type(&self, constant: &Constant) -> Ty {
        // Use the type method provided by Constant
        constant.ty()
    }

    /// Get the return type of a function
    fn get_function_return_type(&self, name: &str) -> Ty {
        // Check registered function return types
        if let Some(ty) = self.fn_return_types.get(name) {
            return ty.clone();
        }

        // Check program functions
        if let Some(func) = self.program.functions.get(name) {
            return func.return_ty.clone();
        }

        // Builtin function return types
        match name {
            // Vector operations
            "vec_len" | "str_len" | "map_len" => Ty::Int,
            "vec_get" => Ty::Option(Box::new(Ty::Unit)),
            "vec_push" | "vec_pop" | "vec_clear" => Ty::Unit,
            "vec_is_empty" | "str_contains" | "str_starts_with" | "str_ends_with" => Ty::Bool,
            "vec_first" | "vec_last" => Ty::Option(Box::new(Ty::Unit)),

            // String operations
            "str_concat" | "str_trim" | "str_upper" | "str_lower" | "str_replace"
            | "str_substring" | "str_repeat" | "str_reverse" | "str_pad_left" | "str_pad_right" => {
                Ty::Str
            }
            "str_split" | "str_lines" | "str_chars" | "str_bytes" => Ty::List(Box::new(Ty::Str)),
            "str_find" | "str_rfind" => Ty::Option(Box::new(Ty::Int)),
            "str_parse_int" => Ty::Option(Box::new(Ty::Int)),
            "str_parse_float" => Ty::Option(Box::new(Ty::Float)),

            // Map operations
            "map_get" => Ty::Option(Box::new(Ty::Unit)),
            "map_insert" | "map_remove" | "map_clear" => Ty::Unit,
            "map_contains_key" => Ty::Bool,
            "map_keys" | "map_values" => Ty::List(Box::new(Ty::Unit)),

            // Math operations
            "abs" | "min" | "max" => Ty::Int,
            "sqrt" | "sin" | "cos" | "tan" | "log" | "exp" | "pow" | "floor" | "ceil" | "round" => {
                Ty::Float
            }

            // I/O operations
            "print" | "println" | "eprint" | "eprintln" => Ty::Unit,
            "read_line" => Ty::Result(Box::new(Ty::Str), Box::new(Ty::Str)),
            "read_file" | "write_file" => Ty::Result(Box::new(Ty::Unit), Box::new(Ty::Str)),

            // Type conversions
            "int" | "Int" => Ty::Int,
            "float" | "Float" => Ty::Float,
            "str" | "Str" => Ty::Str,
            "bool" | "Bool" => Ty::Bool,

            // Random
            "rand_int" => Ty::Int,
            "rand_float" => Ty::Float,
            "rand_bool" => Ty::Bool,

            // Time
            "time_now" => Ty::Int,
            "time_format" => Ty::Str,

            _ => Ty::Unit,
        }
    }

    /// Get the return type of a method
    fn get_method_return_type(&self, method_name: &str) -> Ty {
        // Common method return types
        match method_name {
            "len" => Ty::Int,
            "is_empty" => Ty::Bool,
            "get" | "first" | "last" => Ty::Option(Box::new(Ty::Unit)),
            "push" | "pop" | "clear" | "insert" | "remove" => Ty::Unit,
            "contains" | "starts_with" | "ends_with" => Ty::Bool,
            "to_string" | "display" | "describe" => Ty::Str,
            "clone" | "copy" => Ty::Unit, // Will be same as receiver
            "map" | "filter" | "collect" => Ty::List(Box::new(Ty::Unit)),
            "fold" | "reduce" => Ty::Unit,
            "sum" | "product" | "count" => Ty::Int,
            "min" | "max" => Ty::Option(Box::new(Ty::Unit)),
            "sort" | "reverse" => Ty::Unit,
            "join" => Ty::Str,
            "split" | "lines" | "chars" => Ty::List(Box::new(Ty::Str)),
            "trim" | "upper" | "lower" => Ty::Str,
            "parse_int" => Ty::Option(Box::new(Ty::Int)),
            "parse_float" => Ty::Option(Box::new(Ty::Float)),
            _ => Ty::Unit,
        }
    }

    /// Infer result type for binary operations
    fn binary_op_result_type(&self, op: BinOp, left: &Operand, _right: &Operand) -> Ty {
        match op {
            // Comparison operators always return Bool
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => Ty::Bool,
            // Logical operators return Bool
            BinOp::And | BinOp::Or => Ty::Bool,
            // Arithmetic and bitwise operators return same type as operands
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Rem
            | BinOp::BitAnd
            | BinOp::BitOr
            | BinOp::BitXor
            | BinOp::Shl
            | BinOp::Shr => self.operand_type(left),
        }
    }

    fn new_block(&mut self) -> BlockId {
        let func = match self.current_function_mut() {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Internal error: new_block - {}", e.message);
                return BlockId(0);
            }
        };
        func.add_block()
    }

    fn emit(&mut self, kind: StatementKind) {
        let block = match self.current_block_id() {
            Ok(b) => b,
            Err(e) => {
                eprintln!("Internal error: emit block - {}", e.message);
                return;
            }
        };
        let func = match self.current_function_mut() {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Internal error: emit func - {}", e.message);
                return;
            }
        };
        func.block_mut(block).push(Statement { kind });
    }

    fn terminate(&mut self, term: Terminator) {
        let block = match self.current_block_id() {
            Ok(b) => b,
            Err(e) => {
                eprintln!("Internal error: terminate block - {}", e.message);
                return;
            }
        };
        let func = match self.current_function_mut() {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Internal error: terminate func - {}", e.message);
                return;
            }
        };
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

        for (i, row) in dp.iter_mut().enumerate().take(m + 1) {
            row[0] = i;
        }
        for (j, val) in dp[0].iter_mut().enumerate().take(n + 1) {
            *val = j;
        }

        for i in 1..=m {
            for j in 1..=n {
                let cost = if a_chars[i - 1] == b_chars[j - 1] {
                    0
                } else {
                    1
                };
                dp[i][j] = (dp[i - 1][j] + 1)
                    .min(dp[i][j - 1] + 1)
                    .min(dp[i - 1][j - 1] + cost);
            }
        }

        dp[m][n]
    }

    /// Find free variables in an expression that aren't in the given bound set.
    /// Used for closure capture analysis.
    fn find_free_vars(
        &self,
        expr: &Expr,
        bound: &std::collections::HashSet<String>,
    ) -> Vec<String> {
        let mut free = Vec::new();
        self.collect_free_vars(expr, bound, &mut free);
        // Remove duplicates while preserving order
        let mut seen = std::collections::HashSet::new();
        free.retain(|v| seen.insert(v.clone()));
        free
    }

    fn collect_free_vars(
        &self,
        expr: &Expr,
        bound: &std::collections::HashSet<String>,
        free: &mut Vec<String>,
    ) {
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
                            self.collect_free_vars(
                                &Expr::new(ExprKind::If(nested_if.clone()), expr.span),
                                bound,
                                free,
                            );
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
        matches!(
            name,
            "print"
                | "vec_new"
                | "vec_push"
                | "vec_pop"
                | "vec_len"
                | "vec_get"
                | "vec_first"
                | "vec_last"
                | "vec_concat"
                | "vec_reverse"
                | "vec_slice"
                | "str_len"
                | "str_char_at"
                | "str_slice"
                | "str_contains"
                | "str_starts_with"
                | "str_ends_with"
                | "str_split"
                | "str_trim"
                | "str_to_int"
                | "int_to_str"
                | "str_concat"
                | "map_new"
                | "map_insert"
                | "map_get"
                | "map_contains"
                | "map_remove"
                | "map_len"
                | "map_keys"
                | "char_is_digit"
                | "char_is_alpha"
                | "char_is_alphanumeric"
                | "char_is_whitespace"
                | "char_to_int"
        )
    }

    fn get_struct_fields(&self, _struct_name: &str) -> Option<Vec<(String, Ty)>> {
        // Look up struct definition in var_types or type registry
        // For now, return None and rely on type checker having validated
        // In a full implementation, this would query the struct registry
        None
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
        let program =
            lower_source("f max(a: Int, b: Int) -> Int = if a > b then a else b").unwrap();
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
        )
        .unwrap();
        assert!(program.functions.contains_key("countdown"));
    }

    #[test]
    fn test_let_binding() {
        let program = lower_source(
            r#"f example() -> Int
    x = 42
    y = x + 1
    y"#,
        )
        .unwrap();
        assert!(program.functions.contains_key("example"));
    }
}
