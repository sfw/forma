//! LLVM IR code generation for FORMA.
//!
//! This module lowers FORMA MIR to LLVM IR using the inkwell crate.
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
//! use forma::codegen::LLVMCodegen;
//! use forma::mir::Program;
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
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
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

/// LLVM code generator for FORMA programs.
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
    /// Closure environment types for each closure local
    closure_env_types: HashMap<usize, inkwell::types::StructType<'ctx>>,
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
            closure_env_types: HashMap::new(),
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

    /// Safely convert a BasicValueEnum to IntValue.
    fn as_int_value(&self, val: BasicValueEnum<'ctx>) -> Result<IntValue<'ctx>, CodegenError> {
        match val {
            BasicValueEnum::IntValue(i) => Ok(i),
            _ => Err(CodegenError { message: format!("Expected integer, got {:?}", val.get_type()) }),
        }
    }

    /// Safely convert a BasicValueEnum to FloatValue.
    fn as_float_value(&self, val: BasicValueEnum<'ctx>) -> Result<FloatValue<'ctx>, CodegenError> {
        match val {
            BasicValueEnum::FloatValue(f) => Ok(f),
            _ => Err(CodegenError { message: format!("Expected float, got {:?}", val.get_type()) }),
        }
    }

    /// Safely convert a BasicValueEnum to StructValue.
    fn as_struct_value(&self, val: BasicValueEnum<'ctx>) -> Result<StructValue<'ctx>, CodegenError> {
        match val {
            BasicValueEnum::StructValue(s) => Ok(s),
            _ => Err(CodegenError { message: format!("Expected struct, got {:?}", val.get_type()) }),
        }
    }

    /// Safely convert a BasicValueEnum to PointerValue.
    fn as_pointer_value(&self, val: BasicValueEnum<'ctx>) -> Result<PointerValue<'ctx>, CodegenError> {
        match val {
            BasicValueEnum::PointerValue(p) => Ok(p),
            _ => Err(CodegenError { message: format!("Expected pointer, got {:?}", val.get_type()) }),
        }
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
            Rvalue::Cast(operand, target_ty) => {
                let val = self.compile_operand(operand)?;
                self.compile_cast(val, target_ty)
            }
            Rvalue::Closure { func_name, captures } => {
                self.compile_closure(func_name, captures)
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
        // Check if operands are floats
        if lhs.is_float_value() && rhs.is_float_value() {
            let lhs_float = self.as_float_value(lhs)?;
            let rhs_float = self.as_float_value(rhs)?;
            return match op {
                BinOp::Add => Ok(self.builder.build_float_add(lhs_float, rhs_float, "fadd")
                    .map_err(|e| CodegenError { message: format!("fadd failed: {:?}", e) })?.into()),
                BinOp::Sub => Ok(self.builder.build_float_sub(lhs_float, rhs_float, "fsub")
                    .map_err(|e| CodegenError { message: format!("fsub failed: {:?}", e) })?.into()),
                BinOp::Mul => Ok(self.builder.build_float_mul(lhs_float, rhs_float, "fmul")
                    .map_err(|e| CodegenError { message: format!("fmul failed: {:?}", e) })?.into()),
                BinOp::Div => Ok(self.builder.build_float_div(lhs_float, rhs_float, "fdiv")
                    .map_err(|e| CodegenError { message: format!("fdiv failed: {:?}", e) })?.into()),
                BinOp::Rem => Ok(self.builder.build_float_rem(lhs_float, rhs_float, "frem")
                    .map_err(|e| CodegenError { message: format!("frem failed: {:?}", e) })?.into()),
                BinOp::Eq => Ok(self.builder.build_float_compare(FloatPredicate::OEQ, lhs_float, rhs_float, "feq")
                    .map_err(|e| CodegenError { message: format!("feq failed: {:?}", e) })?.into()),
                BinOp::Ne => Ok(self.builder.build_float_compare(FloatPredicate::ONE, lhs_float, rhs_float, "fne")
                    .map_err(|e| CodegenError { message: format!("fne failed: {:?}", e) })?.into()),
                BinOp::Lt => Ok(self.builder.build_float_compare(FloatPredicate::OLT, lhs_float, rhs_float, "flt")
                    .map_err(|e| CodegenError { message: format!("flt failed: {:?}", e) })?.into()),
                BinOp::Le => Ok(self.builder.build_float_compare(FloatPredicate::OLE, lhs_float, rhs_float, "fle")
                    .map_err(|e| CodegenError { message: format!("fle failed: {:?}", e) })?.into()),
                BinOp::Gt => Ok(self.builder.build_float_compare(FloatPredicate::OGT, lhs_float, rhs_float, "fgt")
                    .map_err(|e| CodegenError { message: format!("fgt failed: {:?}", e) })?.into()),
                BinOp::Ge => Ok(self.builder.build_float_compare(FloatPredicate::OGE, lhs_float, rhs_float, "fge")
                    .map_err(|e| CodegenError { message: format!("fge failed: {:?}", e) })?.into()),
                _ => Err(CodegenError { message: format!("Float operation not supported: {:?}", op) }),
            };
        }

        // Integer operations (using safe helper)
        let lhs_int = self.as_int_value(lhs)?;
        let rhs_int = self.as_int_value(rhs)?;

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
            // Bitwise operations
            BinOp::BitAnd => self.builder.build_and(lhs_int, rhs_int, "bitand")
                .map_err(|e| CodegenError { message: format!("bitand failed: {:?}", e) })?,
            BinOp::BitOr => self.builder.build_or(lhs_int, rhs_int, "bitor")
                .map_err(|e| CodegenError { message: format!("bitor failed: {:?}", e) })?,
            BinOp::BitXor => self.builder.build_xor(lhs_int, rhs_int, "bitxor")
                .map_err(|e| CodegenError { message: format!("bitxor failed: {:?}", e) })?,
            BinOp::Shl => self.builder.build_left_shift(lhs_int, rhs_int, "shl")
                .map_err(|e| CodegenError { message: format!("shl failed: {:?}", e) })?,
            BinOp::Shr => self.builder.build_right_shift(lhs_int, rhs_int, false, "shr")
                .map_err(|e| CodegenError { message: format!("shr failed: {:?}", e) })?,
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
        let int_val = self.as_int_value(val)?;
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

    fn compile_closure(
        &mut self,
        func_name: &str,
        captures: &[Operand],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // 1. Compile captured values and determine their types
        let mut env_field_types: Vec<BasicTypeEnum> = Vec::new();
        let mut env_values: Vec<BasicValueEnum> = Vec::new();

        for cap in captures {
            let val = self.compile_operand(cap)?;
            env_field_types.push(val.get_type());
            env_values.push(val);
        }

        // 2. Create environment struct type
        let env_struct_type = self.context.struct_type(
            &env_field_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
            false
        );

        // 3. Allocate environment on heap (call malloc)
        let i64_type = self.context.i64_type();
        let i8_ptr_type = self.context.i8_type().ptr_type(inkwell::AddressSpace::default());

        // Get or declare malloc
        let malloc_fn = self.module.get_function("malloc").unwrap_or_else(|| {
            let malloc_type = i8_ptr_type.fn_type(&[i64_type.into()], false);
            self.module.add_function("malloc", malloc_type, None)
        });

        // Calculate size and allocate
        let env_size = env_struct_type.size_of()
            .ok_or_else(|| CodegenError { message: "Cannot get size of unsized closure environment".to_string() })?;
        let malloc_result = self.builder
            .build_call(malloc_fn, &[env_size.into()], "env_alloc")
            .map_err(|e| CodegenError { message: format!("malloc call failed: {:?}", e) })?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| CodegenError { message: "malloc returned void".into() })?;
        let env_ptr_i8 = self.as_pointer_value(malloc_result)?;

        // 4. Cast to environment struct pointer
        let env_struct_ptr = self.builder
            .build_pointer_cast(
                env_ptr_i8,
                env_struct_type.ptr_type(inkwell::AddressSpace::default()),
                "env_struct_ptr"
            )
            .map_err(|e| CodegenError { message: format!("pointer cast failed: {:?}", e) })?;

        // 5. Store captured values into environment struct
        for (i, val) in env_values.iter().enumerate() {
            let field_ptr = self.builder
                .build_struct_gep(env_struct_type, env_struct_ptr, i as u32, &format!("env_field_{}", i))
                .map_err(|e| CodegenError { message: format!("struct gep failed: {:?}", e) })?;
            self.builder
                .build_store(field_ptr, *val)
                .map_err(|e| CodegenError { message: format!("store failed: {:?}", e) })?;
        }

        // 6. Get the lifted function pointer
        let lifted_fn = self.functions.get(func_name)
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!("Lifted closure function not found: {}", func_name),
            })?;

        // 7. Cast function pointer to i8*
        let fn_ptr_i8 = self.builder
            .build_pointer_cast(
                lifted_fn.as_global_value().as_pointer_value(),
                i8_ptr_type,
                "fn_ptr_i8"
            )
            .map_err(|e| CodegenError { message: format!("fn pointer cast failed: {:?}", e) })?;

        // 8. Cast env pointer back to i8*
        let env_ptr_i8_final = self.builder
            .build_pointer_cast(env_struct_ptr, i8_ptr_type, "env_ptr_i8")
            .map_err(|e| CodegenError { message: format!("env pointer cast failed: {:?}", e) })?;

        // 9. Create closure fat pointer struct: { i8*, i8* }
        let closure_struct_type = self.context.struct_type(
            &[i8_ptr_type.into(), i8_ptr_type.into()],
            false
        );

        // Build the struct value
        let closure_undef = closure_struct_type.get_undef();
        let closure_with_fn = self.builder
            .build_insert_value(closure_undef, fn_ptr_i8, 0, "closure_fn")
            .map_err(|e| CodegenError { message: format!("insert fn failed: {:?}", e) })?;
        let closure_complete = self.builder
            .build_insert_value(closure_with_fn, env_ptr_i8_final, 1, "closure_env")
            .map_err(|e| CodegenError { message: format!("insert env failed: {:?}", e) })?;

        // closure_complete is an AggregateValueEnum, convert safely
        match closure_complete {
            inkwell::values::AggregateValueEnum::StructValue(sv) => Ok(sv.into()),
            _ => Err(CodegenError { message: "Expected struct value for closure".to_string() }),
        }
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

    /// Compile a cast operation.
    fn compile_cast(
        &mut self,
        value: BasicValueEnum<'ctx>,
        target_ty: &Ty,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let target_llvm_ty = self.lower_type(target_ty)?;

        match (value, target_llvm_ty) {
            // Int to Int cast
            (BasicValueEnum::IntValue(iv), BasicTypeEnum::IntType(target_int)) => {
                let src_width = iv.get_type().get_bit_width();
                let dst_width = target_int.get_bit_width();

                if src_width == dst_width {
                    Ok(value)
                } else if src_width < dst_width {
                    // Widening: use sign-extend for signed types, zero-extend for unsigned
                    // For simplicity, use sign-extend (signed semantics)
                    let extended = self.builder.build_int_s_extend(iv, target_int, "sext")
                        .map_err(|e| CodegenError { message: format!("sext failed: {:?}", e) })?;
                    Ok(extended.into())
                } else {
                    // Narrowing: truncate
                    let truncated = self.builder.build_int_truncate(iv, target_int, "trunc")
                        .map_err(|e| CodegenError { message: format!("trunc failed: {:?}", e) })?;
                    Ok(truncated.into())
                }
            }
            // Int to Float cast
            (BasicValueEnum::IntValue(iv), BasicTypeEnum::FloatType(target_float)) => {
                let result = self.builder.build_signed_int_to_float(iv, target_float, "sitofp")
                    .map_err(|e| CodegenError { message: format!("sitofp failed: {:?}", e) })?;
                Ok(result.into())
            }
            // Float to Int cast
            (BasicValueEnum::FloatValue(fv), BasicTypeEnum::IntType(target_int)) => {
                let result = self.builder.build_float_to_signed_int(fv, target_int, "fptosi")
                    .map_err(|e| CodegenError { message: format!("fptosi failed: {:?}", e) })?;
                Ok(result.into())
            }
            // Float to Float cast
            (BasicValueEnum::FloatValue(fv), BasicTypeEnum::FloatType(target_float)) => {
                let src_ty = fv.get_type();
                // Compare by checking if converting to f64 (bigger) or f32 (smaller)
                if src_ty == target_float {
                    Ok(value)
                } else if target_float == self.context.f64_type() {
                    // Extend f32 to f64
                    let extended = self.builder.build_float_ext(fv, target_float, "fpext")
                        .map_err(|e| CodegenError { message: format!("fpext failed: {:?}", e) })?;
                    Ok(extended.into())
                } else {
                    // Truncate f64 to f32
                    let truncated = self.builder.build_float_trunc(fv, target_float, "fptrunc")
                        .map_err(|e| CodegenError { message: format!("fptrunc failed: {:?}", e) })?;
                    Ok(truncated.into())
                }
            }
            // Same type or other - just return
            _ => Ok(value),
        }
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
            Terminator::CallIndirect { callee, args, dest, next } => {
                let i8_ptr_type = self.context.i8_type().ptr_type(inkwell::AddressSpace::default());
                let i64_type = self.context.i64_type();

                // 1. Compile the closure operand (should be a fat pointer struct)
                let closure_val = self.compile_operand(callee)?;
                let closure_struct = self.as_struct_value(closure_val)?;

                // 2. Extract function pointer and environment pointer
                let fn_ptr_raw = self.builder
                    .build_extract_value(closure_struct, 0, "fn_ptr_raw")
                    .map_err(|e| CodegenError { message: format!("extract fn_ptr failed: {:?}", e) })?;
                let fn_ptr_i8 = self.as_pointer_value(fn_ptr_raw.into())?;
                let env_ptr_raw = self.builder
                    .build_extract_value(closure_struct, 1, "env_ptr")
                    .map_err(|e| CodegenError { message: format!("extract env_ptr failed: {:?}", e) })?;
                let env_ptr = self.as_pointer_value(env_ptr_raw.into())?;

                // 3. Compile arguments (environment pointer is implicit first arg)
                let mut compiled_args: Vec<BasicMetadataValueEnum> = vec![env_ptr.into()];
                for arg in args {
                    let val = self.compile_operand(arg)?;
                    compiled_args.push(val.into());
                }

                // 4. Build function type: (i8*, args...) -> i64
                // Environment pointer is always first parameter
                let mut param_types: Vec<BasicMetadataTypeEnum> = vec![i8_ptr_type.into()];
                for _ in args {
                    param_types.push(i64_type.into());
                }
                let fn_type = i64_type.fn_type(&param_types, false);

                // 5. Cast function pointer to correct type
                let fn_ptr_typed = self.builder
                    .build_pointer_cast(
                        fn_ptr_i8,
                        fn_type.ptr_type(inkwell::AddressSpace::default()),
                        "fn_ptr_typed"
                    )
                    .map_err(|e| CodegenError { message: format!("fn type cast failed: {:?}", e) })?;

                // 6. Build indirect call
                let call = self.builder
                    .build_indirect_call(fn_type, fn_ptr_typed, &compiled_args, "indirect_call")
                    .map_err(|e| CodegenError { message: format!("indirect call failed: {:?}", e) })?;

                // 7. Store result if there's a destination
                if let Some(local) = dest {
                    if let Some(result) = call.try_as_basic_value().left() {
                        if let Some(alloca) = self.locals.get(&(local.0 as usize)) {
                            self.builder.build_store(*alloca, result)
                                .map_err(|e| CodegenError { message: format!("store failed: {:?}", e) })?;
                        }
                    }
                }

                // 8. Jump to next block
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

    /// Lower an FORMA type to an LLVM type.
    fn lower_type(&self, ty: &Ty) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
        match ty {
            // Signed integers
            Ty::I8 => Ok(self.context.i8_type().into()),
            Ty::I16 => Ok(self.context.i16_type().into()),
            Ty::I32 => Ok(self.context.i32_type().into()),
            Ty::Int | Ty::I64 | Ty::Isize => Ok(self.context.i64_type().into()),
            Ty::I128 => Ok(self.context.i128_type().into()),
            // Unsigned integers (LLVM doesn't distinguish signedness in types)
            Ty::U8 => Ok(self.context.i8_type().into()),
            Ty::U16 => Ok(self.context.i16_type().into()),
            Ty::U32 => Ok(self.context.i32_type().into()),
            Ty::UInt | Ty::U64 | Ty::Usize => Ok(self.context.i64_type().into()),
            Ty::U128 => Ok(self.context.i128_type().into()),
            // Floats
            Ty::F32 => Ok(self.context.f32_type().into()),
            Ty::Float | Ty::F64 => Ok(self.context.f64_type().into()),
            // Other
            Ty::Bool => Ok(self.context.bool_type().into()),
            Ty::Char => Ok(self.context.i32_type().into()),
            Ty::Unit => Ok(self.context.i8_type().into()),
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
