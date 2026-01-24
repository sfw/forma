//! LLVM IR code generation for ARIA.
//!
//! This module lowers ARIA MIR to LLVM IR using the inkwell crate.
//!
//! # Supported Features
//! - Integer arithmetic
//! - Boolean operations
//! - Function calls
//! - Control flow (if/else, while)
//! - Local variables
//!
//! # Usage
//! ```ignore
//! use aria::codegen::LLVMCodegen;
//! use aria::mir::Program;
//!
//! let codegen = LLVMCodegen::new("my_module");
//! codegen.compile(&program)?;
//! codegen.write_object_file("output.o")?;
//! ```

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, IntPredicate};
use std::collections::HashMap;
use std::path::Path;

use crate::mir::{
    BasicBlock, BinOp, Constant, Function, Operand, Program, Rvalue,
    Statement, StatementKind, Terminator, UnOp,
};
use crate::types::Ty;

/// Error during LLVM code generation.
#[derive(Debug)]
pub struct CodegenError {
    pub message: String,
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "codegen error: {}", self.message)
    }
}

impl std::error::Error for CodegenError {}

/// LLVM code generator for ARIA programs.
pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    /// Map from MIR function names to LLVM functions
    functions: HashMap<String, FunctionValue<'ctx>>,
    /// Map from local variable indices to stack allocations
    locals: HashMap<usize, PointerValue<'ctx>>,
    /// Map from local variable indices to their LLVM types
    local_types: HashMap<usize, BasicTypeEnum<'ctx>>,
    /// Current function being compiled
    current_function: Option<FunctionValue<'ctx>>,
    /// Optimization level
    opt_level: OptimizationLevel,
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Create a new code generator with the given module name.
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            functions: HashMap::new(),
            locals: HashMap::new(),
            local_types: HashMap::new(),
            current_function: None,
            opt_level: OptimizationLevel::Default,
        }
    }

    /// Set the optimization level.
    pub fn set_opt_level(&mut self, level: u8) {
        self.opt_level = match level {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Less,
            2 => OptimizationLevel::Default,
            _ => OptimizationLevel::Aggressive,
        };
    }

    /// Compile a MIR program to LLVM IR.
    pub fn compile(&mut self, program: &Program) -> Result<(), CodegenError> {
        // First pass: declare all functions
        for (_name, func) in &program.functions {
            self.declare_function(func)?;
        }

        // Second pass: compile function bodies
        for (_name, func) in &program.functions {
            self.compile_function(func)?;
        }

        Ok(())
    }

    /// Declare a function (create signature without body).
    fn declare_function(&mut self, func: &Function) -> Result<(), CodegenError> {
        let return_type = self.lower_type(&func.return_ty)?;
        let param_types: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|(_, ty)| self.lower_type(ty).map(|t| t.into()))
            .collect::<Result<Vec<_>, _>>()?;

        let fn_type = match return_type {
            BasicTypeEnum::IntType(t) => t.fn_type(&param_types, false),
            BasicTypeEnum::FloatType(t) => t.fn_type(&param_types, false),
            BasicTypeEnum::PointerType(t) => t.fn_type(&param_types, false),
            _ => {
                // Default to i64 for unknown types
                self.context.i64_type().fn_type(&param_types, false)
            }
        };

        let fn_value = self.module.add_function(&func.name, fn_type, None);
        self.functions.insert(func.name.clone(), fn_value);

        Ok(())
    }

    /// Compile a function body.
    fn compile_function(&mut self, func: &Function) -> Result<(), CodegenError> {
        let fn_value = self
            .functions
            .get(&func.name)
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!("Function {} not declared", func.name),
            })?;

        self.current_function = Some(fn_value);
        self.locals.clear();
        self.local_types.clear();

        // Create entry block
        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        // Allocate locals
        for (i, local) in func.locals.iter().enumerate() {
            let ty = self.lower_type(&local.ty)?;
            let alloca = self.builder.build_alloca(ty, &format!("local_{}", i))
                .map_err(|e| CodegenError { message: format!("alloca failed: {:?}", e) })?;
            self.locals.insert(i, alloca);
            self.local_types.insert(i, ty);
        }

        // Store function parameters into their locals
        for (i, param) in fn_value.get_param_iter().enumerate() {
            if let Some(alloca) = self.locals.get(&i) {
                self.builder.build_store(*alloca, param)
                    .map_err(|e| CodegenError { message: format!("store failed: {:?}", e) })?;
            }
        }

        // Create basic blocks for each MIR block
        let mut blocks: HashMap<usize, inkwell::basic_block::BasicBlock> = HashMap::new();
        for (i, _) in func.blocks.iter().enumerate() {
            let bb = self
                .context
                .append_basic_block(fn_value, &format!("bb_{}", i));
            blocks.insert(i, bb);
        }

        // Jump from entry to first block
        if let Some(&first_block) = blocks.get(&0) {
            self.builder.build_unconditional_branch(first_block)
                .map_err(|e| CodegenError { message: format!("branch failed: {:?}", e) })?;
        }

        // Compile each block
        for (i, block) in func.blocks.iter().enumerate() {
            if let Some(&bb) = blocks.get(&i) {
                self.builder.position_at_end(bb);
                self.compile_block(block, &blocks)?;
            }
        }

        self.current_function = None;
        Ok(())
    }

    /// Compile a basic block.
    fn compile_block(
        &mut self,
        block: &BasicBlock,
        blocks: &HashMap<usize, inkwell::basic_block::BasicBlock>,
    ) -> Result<(), CodegenError> {
        // Compile statements
        for stmt in &block.stmts {
            self.compile_statement(stmt)?;
        }

        // Compile terminator
        if let Some(ref term) = block.terminator {
            self.compile_terminator(term, blocks)?;
        }

        Ok(())
    }

    /// Compile a statement.
    fn compile_statement(&mut self, stmt: &Statement) -> Result<(), CodegenError> {
        match &stmt.kind {
            StatementKind::Assign(local, rvalue) => {
                let value = self.compile_rvalue(rvalue)?;
                let idx = local.0 as usize;
                // Get alloca and type before mutable borrow
                let alloca = self.locals.get(&idx).copied();
                let target_ty = self.local_types.get(&idx).copied();

                if let Some(alloca) = alloca {
                    // Coerce value if needed (e.g., i1 to i64 for comparison results)
                    let coerced_value = self.coerce_value(value, target_ty)?;
                    self.builder.build_store(alloca, coerced_value)
                        .map_err(|e| CodegenError { message: format!("store failed: {:?}", e) })?;
                }
            }
            StatementKind::Nop => {}
        }
        Ok(())
    }

    /// Compile an rvalue to produce a value.
    fn compile_rvalue(&mut self, rvalue: &Rvalue) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match rvalue {
            Rvalue::Use(operand) => self.compile_operand(operand),
            Rvalue::BinaryOp(op, left, right) => {
                let lhs = self.compile_operand(left)?;
                let rhs = self.compile_operand(right)?;
                self.compile_binop(*op, lhs, rhs)
            }
            Rvalue::UnaryOp(op, operand) => {
                let val = self.compile_operand(operand)?;
                self.compile_unaryop(*op, val)
            }
            // Note: function calls are handled in Terminator::Call, not in Rvalue
            _ => Err(CodegenError {
                message: format!("Unsupported rvalue: {:?}", rvalue),
            }),
        }
    }

    /// Compile an operand.
    fn compile_operand(&mut self, operand: &Operand) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match operand {
            Operand::Local(local) | Operand::Copy(local) | Operand::Move(local) => {
                let idx = local.0 as usize;
                if let Some(alloca) = self.locals.get(&idx) {
                    let ty = self.local_types.get(&idx)
                        .copied()
                        .unwrap_or_else(|| self.context.i64_type().into());
                    self.builder.build_load(ty, *alloca, "load")
                        .map_err(|e| CodegenError { message: format!("load failed: {:?}", e) })
                } else {
                    Err(CodegenError {
                        message: format!("Unknown local: {}", local.0),
                    })
                }
            }
            Operand::Constant(constant) => {
                match constant {
                    Constant::Int(n) => {
                        let val = self.context.i64_type().const_int(*n as u64, true);
                        Ok(val.into())
                    }
                    Constant::Bool(b) => {
                        let val = self.context.bool_type().const_int(if *b { 1 } else { 0 }, false);
                        Ok(val.into())
                    }
                    Constant::Float(f) => {
                        let val = self.context.f64_type().const_float(*f);
                        Ok(val.into())
                    }
                    Constant::Char(c) => {
                        let val = self.context.i32_type().const_int(*c as u64, false);
                        Ok(val.into())
                    }
                    Constant::Str(s) => {
                        // Create a global string constant
                        let str_val = self.context.const_string(s.as_bytes(), true);
                        let global = self.module.add_global(str_val.get_type(), None, "str");
                        global.set_constant(true);
                        global.set_initializer(&str_val);
                        Ok(global.as_pointer_value().into())
                    }
                    Constant::Unit => {
                        // Unit type as i8 zero
                        Ok(self.context.i8_type().const_zero().into())
                    }
                }
            }
        }
    }

    /// Compile a binary operation.
    fn compile_binop(
        &mut self,
        op: BinOp,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let lhs_int = lhs.into_int_value();
        let rhs_int = rhs.into_int_value();

        let result: IntValue = match op {
            BinOp::Add => self.builder.build_int_add(lhs_int, rhs_int, "add")
                .map_err(|e| CodegenError { message: format!("add failed: {:?}", e) })?,
            BinOp::Sub => self.builder.build_int_sub(lhs_int, rhs_int, "sub")
                .map_err(|e| CodegenError { message: format!("sub failed: {:?}", e) })?,
            BinOp::Mul => self.builder.build_int_mul(lhs_int, rhs_int, "mul")
                .map_err(|e| CodegenError { message: format!("mul failed: {:?}", e) })?,
            BinOp::Div => self.builder.build_int_signed_div(lhs_int, rhs_int, "div")
                .map_err(|e| CodegenError { message: format!("div failed: {:?}", e) })?,
            BinOp::Rem => self.builder.build_int_signed_rem(lhs_int, rhs_int, "rem")
                .map_err(|e| CodegenError { message: format!("rem failed: {:?}", e) })?,
            BinOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, lhs_int, rhs_int, "eq")
                .map_err(|e| CodegenError { message: format!("eq failed: {:?}", e) })?,
            BinOp::Ne => self.builder.build_int_compare(IntPredicate::NE, lhs_int, rhs_int, "ne")
                .map_err(|e| CodegenError { message: format!("ne failed: {:?}", e) })?,
            BinOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, lhs_int, rhs_int, "lt")
                .map_err(|e| CodegenError { message: format!("lt failed: {:?}", e) })?,
            BinOp::Le => self.builder.build_int_compare(IntPredicate::SLE, lhs_int, rhs_int, "le")
                .map_err(|e| CodegenError { message: format!("le failed: {:?}", e) })?,
            BinOp::Gt => self.builder.build_int_compare(IntPredicate::SGT, lhs_int, rhs_int, "gt")
                .map_err(|e| CodegenError { message: format!("gt failed: {:?}", e) })?,
            BinOp::Ge => self.builder.build_int_compare(IntPredicate::SGE, lhs_int, rhs_int, "ge")
                .map_err(|e| CodegenError { message: format!("ge failed: {:?}", e) })?,
            BinOp::And => self.builder.build_and(lhs_int, rhs_int, "and")
                .map_err(|e| CodegenError { message: format!("and failed: {:?}", e) })?,
            BinOp::Or => self.builder.build_or(lhs_int, rhs_int, "or")
                .map_err(|e| CodegenError { message: format!("or failed: {:?}", e) })?,
            _ => {
                return Err(CodegenError {
                    message: format!("Unsupported binary operator: {:?}", op),
                })
            }
        };

        Ok(result.into())
    }

    /// Compile a unary operation.
    fn compile_unaryop(
        &mut self,
        op: UnOp,
        val: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let int_val = val.into_int_value();
        let result = match op {
            UnOp::Neg => {
                self.builder.build_int_neg(int_val, "neg")
                    .map_err(|e| CodegenError { message: format!("neg failed: {:?}", e) })?
            }
            UnOp::Not | UnOp::BitNot => {
                self.builder.build_not(int_val, "not")
                    .map_err(|e| CodegenError { message: format!("not failed: {:?}", e) })?
            }
        };
        Ok(result.into())
    }

    /// Coerce a value to match a target type (e.g., i1 to i64 for comparisons stored in Int).
    fn coerce_value(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target_ty: Option<BasicTypeEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let Some(target) = target_ty else {
            return Ok(value);
        };

        // Handle integer type mismatches
        if let (BasicValueEnum::IntValue(iv), BasicTypeEnum::IntType(target_int)) = (value, target) {
            let src_width = iv.get_type().get_bit_width();
            let dst_width = target_int.get_bit_width();

            if src_width < dst_width {
                // Zero-extend smaller integers (e.g., i1 comparison result to i64)
                let extended = self.builder.build_int_z_extend(iv, target_int, "zext")
                    .map_err(|e| CodegenError { message: format!("zext failed: {:?}", e) })?;
                return Ok(extended.into());
            } else if src_width > dst_width {
                // Truncate larger integers
                let truncated = self.builder.build_int_truncate(iv, target_int, "trunc")
                    .map_err(|e| CodegenError { message: format!("trunc failed: {:?}", e) })?;
                return Ok(truncated.into());
            }
        }

        Ok(value)
    }

    /// Compile a block terminator.
    fn compile_terminator(
        &mut self,
        terminator: &Terminator,
        blocks: &HashMap<usize, inkwell::basic_block::BasicBlock>,
    ) -> Result<(), CodegenError> {
        match terminator {
            Terminator::Return(operand) => {
                if let Some(op) = operand {
                    let val = self.compile_operand(op)?;
                    self.builder.build_return(Some(&val))
                        .map_err(|e| CodegenError { message: format!("return failed: {:?}", e) })?;
                } else {
                    self.builder.build_return(None)
                        .map_err(|e| CodegenError { message: format!("return failed: {:?}", e) })?;
                }
            }
            Terminator::Goto(target) => {
                if let Some(&bb) = blocks.get(&(target.0 as usize)) {
                    self.builder.build_unconditional_branch(bb)
                        .map_err(|e| CodegenError { message: format!("branch failed: {:?}", e) })?;
                }
            }
            Terminator::If { cond, then_block, else_block } => {
                let cond_val = self.compile_operand(cond)?.into_int_value();
                let then_bb = blocks.get(&(then_block.0 as usize)).copied().ok_or_else(|| CodegenError {
                    message: "Missing then block".into(),
                })?;
                let else_bb = blocks.get(&(else_block.0 as usize)).copied().ok_or_else(|| CodegenError {
                    message: "Missing else block".into(),
                })?;

                // Convert to bool if needed (compare != 0 for non-bool types)
                let cond_bool = if cond_val.get_type().get_bit_width() == 1 {
                    cond_val
                } else {
                    let zero = cond_val.get_type().const_zero();
                    self.builder.build_int_compare(IntPredicate::NE, cond_val, zero, "tobool")
                        .map_err(|e| CodegenError { message: format!("cmp failed: {:?}", e) })?
                };

                self.builder.build_conditional_branch(cond_bool, then_bb, else_bb)
                    .map_err(|e| CodegenError { message: format!("branch failed: {:?}", e) })?;
            }
            Terminator::Switch { operand, targets, default } => {
                let val = self.compile_operand(operand)?.into_int_value();
                let default_bb = blocks.get(&(default.0 as usize)).copied().ok_or_else(|| CodegenError {
                    message: "Missing default block".into(),
                })?;

                // Build switch cases
                let cases: Vec<(IntValue, inkwell::basic_block::BasicBlock)> = targets
                    .iter()
                    .filter_map(|(value, target)| {
                        blocks.get(&(target.0 as usize)).map(|&bb| {
                            let const_val = self.context.i64_type().const_int(*value as u64, false);
                            (const_val, bb)
                        })
                    })
                    .collect();

                self.builder.build_switch(val, default_bb, &cases)
                    .map_err(|e| CodegenError { message: format!("switch failed: {:?}", e) })?;
            }
            Terminator::Call { func, args, dest, next } => {
                let fn_value = self
                    .functions
                    .get(func)
                    .copied()
                    .ok_or_else(|| CodegenError {
                        message: format!("Unknown function: {}", func),
                    })?;

                let compiled_args: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|a| self.compile_operand(a).map(|v| v.into()))
                    .collect::<Result<Vec<_>, _>>()?;

                let call = self
                    .builder
                    .build_call(fn_value, &compiled_args, "call")
                    .map_err(|e| CodegenError { message: format!("call failed: {:?}", e) })?;

                // Store result if there's a destination
                if let Some(local) = dest {
                    if let Some(result) = call.try_as_basic_value().left() {
                        if let Some(alloca) = self.locals.get(&(local.0 as usize)) {
                            self.builder.build_store(*alloca, result)
                                .map_err(|e| CodegenError { message: format!("store failed: {:?}", e) })?;
                        }
                    }
                }

                // Jump to next block
                if let Some(&bb) = blocks.get(&(next.0 as usize)) {
                    self.builder.build_unconditional_branch(bb)
                        .map_err(|e| CodegenError { message: format!("branch failed: {:?}", e) })?;
                }
            }
            Terminator::CallIndirect { callee: _, args: _, dest: _, next } => {
                // TODO: Implement indirect calls for closures
                // For now, just jump to the next block
                if let Some(&bb) = blocks.get(&(next.0 as usize)) {
                    self.builder.build_unconditional_branch(bb)
                        .map_err(|e| CodegenError { message: format!("branch failed: {:?}", e) })?;
                }
            }
            Terminator::Unreachable => {
                self.builder.build_unreachable()
                    .map_err(|e| CodegenError { message: format!("unreachable failed: {:?}", e) })?;
            }
        }
        Ok(())
    }

    /// Lower an ARIA type to an LLVM type.
    fn lower_type(&self, ty: &Ty) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
        match ty {
            Ty::Int | Ty::I64 => Ok(self.context.i64_type().into()),
            Ty::I32 => Ok(self.context.i32_type().into()),
            Ty::I16 => Ok(self.context.i16_type().into()),
            Ty::I8 => Ok(self.context.i8_type().into()),
            Ty::Bool => Ok(self.context.bool_type().into()),
            Ty::Float | Ty::F64 => Ok(self.context.f64_type().into()),
            Ty::F32 => Ok(self.context.f32_type().into()),
            Ty::Unit => Ok(self.context.i8_type().into()), // Unit as i8
            Ty::Str => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            _ => {
                // Default to i64 for complex types
                Ok(self.context.i64_type().into())
            }
        }
    }

    /// Write the module to an object file.
    pub fn write_object_file(&self, path: &Path) -> Result<(), CodegenError> {
        Target::initialize_native(&InitializationConfig::default()).map_err(|e| CodegenError {
            message: format!("Failed to initialize LLVM: {}", e),
        })?;

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).map_err(|e| CodegenError {
            message: format!("Failed to get target: {:?}", e),
        })?;

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                self.opt_level,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| CodegenError {
                message: "Failed to create target machine".into(),
            })?;

        machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| CodegenError {
                message: format!("Failed to write object file: {:?}", e),
            })?;

        Ok(())
    }

    /// Write the module to LLVM IR text file.
    pub fn write_llvm_ir(&self, path: &Path) -> Result<(), CodegenError> {
        self.module.print_to_file(path).map_err(|e| CodegenError {
            message: format!("Failed to write IR: {:?}", e),
        })?;
        Ok(())
    }

    /// Get the LLVM IR as a string.
    pub fn get_llvm_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }
}
