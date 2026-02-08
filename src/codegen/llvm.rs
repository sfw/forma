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

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::{
    BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue, StructValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use std::collections::HashMap;
use std::path::Path;

use crate::mir::{
    BasicBlock, BinOp, Constant, Function, Operand, Program, Rvalue, Statement, StatementKind,
    Terminator, UnOp,
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

    /// Safely convert a BasicValueEnum to IntValue.
    fn as_int_value(&self, val: BasicValueEnum<'ctx>) -> Result<IntValue<'ctx>, CodegenError> {
        match val {
            BasicValueEnum::IntValue(i) => Ok(i),
            _ => Err(CodegenError {
                message: format!("Expected integer, got {:?}", val.get_type()),
            }),
        }
    }

    /// Safely convert a BasicValueEnum to FloatValue.
    fn as_float_value(&self, val: BasicValueEnum<'ctx>) -> Result<FloatValue<'ctx>, CodegenError> {
        match val {
            BasicValueEnum::FloatValue(f) => Ok(f),
            _ => Err(CodegenError {
                message: format!("Expected float, got {:?}", val.get_type()),
            }),
        }
    }

    /// Safely convert a BasicValueEnum to StructValue.
    fn as_struct_value(
        &self,
        val: BasicValueEnum<'ctx>,
    ) -> Result<StructValue<'ctx>, CodegenError> {
        match val {
            BasicValueEnum::StructValue(s) => Ok(s),
            _ => Err(CodegenError {
                message: format!("Expected struct, got {:?}", val.get_type()),
            }),
        }
    }

    /// Safely convert a BasicValueEnum to PointerValue.
    fn as_pointer_value(
        &self,
        val: BasicValueEnum<'ctx>,
    ) -> Result<PointerValue<'ctx>, CodegenError> {
        match val {
            BasicValueEnum::PointerValue(p) => Ok(p),
            _ => Err(CodegenError {
                message: format!("Expected pointer, got {:?}", val.get_type()),
            }),
        }
    }

    /// Compile a MIR program to LLVM IR.
    pub fn compile(&mut self, program: &Program) -> Result<(), CodegenError> {
        // First pass: declare all functions
        for func in program.functions.values() {
            self.declare_function(func)?;
        }

        // Second pass: compile function bodies
        for func in program.functions.values() {
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
            let alloca = self
                .builder
                .build_alloca(ty, &format!("local_{}", i))
                .map_err(|e| CodegenError {
                    message: format!("alloca failed: {:?}", e),
                })?;
            self.locals.insert(i, alloca);
            self.local_types.insert(i, ty);
        }

        // Store function parameters into their locals
        for (i, param) in fn_value.get_param_iter().enumerate() {
            if let Some(alloca) = self.locals.get(&i) {
                self.builder
                    .build_store(*alloca, param)
                    .map_err(|e| CodegenError {
                        message: format!("store failed: {:?}", e),
                    })?;
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
            self.builder
                .build_unconditional_branch(first_block)
                .map_err(|e| CodegenError {
                    message: format!("branch failed: {:?}", e),
                })?;
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
                    // Check if we need to re-type the local (e.g., MIR said Unit/Int but
                    // we actually got a pointer or float from a runtime call result)
                    let needs_retype = target_ty.is_some_and(|t| {
                        value.get_type() != t
                            && !matches!(
                                (value, t),
                                // Allow int width coercion (handled by coerce_value)
                                (BasicValueEnum::IntValue(_), BasicTypeEnum::IntType(_))
                            )
                    });

                    if needs_retype {
                        // Re-allocate with the correct type
                        self.store_builtin_result(value, &Some(*local))?;
                    } else {
                        // Coerce value if needed (e.g., i1 to i64 for comparison results)
                        let coerced_value = self.coerce_value(value, target_ty)?;
                        self.builder
                            .build_store(alloca, coerced_value)
                            .map_err(|e| CodegenError {
                                message: format!("store failed: {:?}", e),
                            })?;
                    }
                }
            }
            StatementKind::IndexAssign(_local, _index, _value) => {
                return Err(CodegenError {
                    message: "IndexAssign is not yet supported in LLVM codegen".to_string(),
                });
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
            Rvalue::Closure {
                func_name,
                captures,
            } => self.compile_closure(func_name, captures),
            // Tuple construction
            Rvalue::Tuple(elements) => {
                let mut values = Vec::new();
                for elem in elements {
                    values.push(self.compile_operand(elem)?);
                }
                // Create anonymous struct type for tuple
                let types: Vec<BasicTypeEnum> = values.iter().map(|v| v.get_type()).collect();
                let tuple_type = self.context.struct_type(&types, false);
                let mut tuple = tuple_type.get_undef();
                for (i, val) in values.into_iter().enumerate() {
                    tuple = self
                        .builder
                        .build_insert_value(tuple, val, i as u32, "tuple_insert")
                        .map_err(|e| CodegenError {
                            message: format!("Failed to insert tuple value: {:?}", e),
                        })?
                        .into_struct_value();
                }
                Ok(tuple.into())
            }
            // Array construction
            Rvalue::Array(elements) => {
                if elements.is_empty() {
                    return Err(CodegenError {
                        message: "Empty array not supported in LLVM codegen".to_string(),
                    });
                }
                let first = self.compile_operand(&elements[0])?;
                let elem_type = first.get_type();
                let array_type = elem_type.array_type(elements.len() as u32);
                let array_alloca = self
                    .builder
                    .build_alloca(array_type, "array")
                    .map_err(|e| CodegenError {
                        message: format!("array alloca failed: {:?}", e),
                    })?;

                for (i, elem) in elements.iter().enumerate() {
                    let val = self.compile_operand(elem)?;
                    let idx = self.context.i32_type().const_int(i as u64, false);
                    let ptr = unsafe {
                        self.builder
                            .build_gep(
                                array_type,
                                array_alloca,
                                &[self.context.i32_type().const_zero(), idx],
                                "elem_ptr",
                            )
                            .map_err(|e| CodegenError {
                                message: format!("array gep failed: {:?}", e),
                            })?
                    };
                    self.builder
                        .build_store(ptr, val)
                        .map_err(|e| CodegenError {
                            message: format!("array store failed: {:?}", e),
                        })?;
                }
                Ok(self
                    .builder
                    .build_load(array_type, array_alloca, "array_val")
                    .map_err(|e| CodegenError {
                        message: format!("array load failed: {:?}", e),
                    })?)
            }
            // Field access by name - requires struct type info to map name to index
            Rvalue::Field(_base, field_name) => {
                // TODO: Need struct type info to map field name to index
                // For now, return error since we can't resolve field names without type info
                Err(CodegenError {
                    message: format!(
                        "Field access by name '{}' not yet supported in LLVM codegen - need type info",
                        field_name
                    ),
                })
            }
            // Tuple field access
            Rvalue::TupleField(base, idx) => {
                let base_val = self.compile_operand(base)?;
                let struct_val = self.as_struct_value(base_val)?;
                self.builder
                    .build_extract_value(struct_val, *idx as u32, "tuple_field")
                    .map_err(|e| CodegenError {
                        message: format!("Failed to extract tuple field: {:?}", e),
                    })
            }
            // Struct construction
            Rvalue::Struct(_name, fields) => {
                let mut values = Vec::new();
                for (_, operand) in fields {
                    values.push(self.compile_operand(operand)?);
                }
                let types: Vec<BasicTypeEnum> = values.iter().map(|v| v.get_type()).collect();
                let struct_type = self.context.struct_type(&types, false);
                let mut struct_val = struct_type.get_undef();
                for (i, val) in values.into_iter().enumerate() {
                    struct_val = self
                        .builder
                        .build_insert_value(struct_val, val, i as u32, "struct_insert")
                        .map_err(|e| CodegenError {
                            message: format!("Failed to insert struct value: {:?}", e),
                        })?
                        .into_struct_value();
                }
                Ok(struct_val.into())
            }
            // Reference creation: &place or &mut place
            Rvalue::Ref(local, _mutability) => {
                let idx = local.0 as usize;
                if let Some(alloca) = self.locals.get(&idx) {
                    // Return the pointer to the local
                    Ok((*alloca).into())
                } else {
                    Err(CodegenError {
                        message: format!("Cannot create reference to unknown local: {}", local.0),
                    })
                }
            }
            // Dereference: *operand
            Rvalue::Deref(operand) => {
                let ptr_val = self.compile_operand(operand)?;
                let ptr = self.as_pointer_value(ptr_val)?;
                // Load the value through the pointer
                // Default to i64 type if we don't know the pointee type
                let pointee_ty = self.context.i64_type();
                self.builder
                    .build_load(pointee_ty, ptr, "deref")
                    .map_err(|e| CodegenError {
                        message: format!("deref load failed: {:?}", e),
                    })
            }
            // Enum construction: Some(42), None, Ok(x), Err(e), etc.
            Rvalue::Enum {
                type_name: _,
                variant: _,
                fields,
            } => {
                // Enum layout: { i32 discriminant, field0, field1, ... }
                // For simplicity, compute discriminant from variant name hash
                // In practice, we'd use a proper enum registry
                let i32_type = self.context.i32_type();
                let i64_type = self.context.i64_type();

                // Compile field values
                let mut field_values: Vec<BasicValueEnum> = Vec::new();
                for field_op in fields {
                    field_values.push(self.compile_operand(field_op)?);
                }

                // Build struct type for enum: { i32, fields... }
                let mut field_types: Vec<BasicTypeEnum> = vec![i32_type.into()];
                for fv in &field_values {
                    field_types.push(fv.get_type());
                }
                // Pad to ensure enum has at least 2 fields (discriminant + data)
                if field_types.len() == 1 {
                    field_types.push(i64_type.into());
                }

                let enum_type = self.context.struct_type(&field_types, false);
                let mut enum_val = enum_type.get_undef();

                // Insert discriminant (use 0 for now - proper mapping would use enum registry)
                let disc_val = i32_type.const_int(0, false);
                enum_val = self
                    .builder
                    .build_insert_value(enum_val, disc_val, 0, "enum_disc")
                    .map_err(|e| CodegenError {
                        message: format!("insert discriminant failed: {:?}", e),
                    })?
                    .into_struct_value();

                // Insert fields
                for (i, fv) in field_values.into_iter().enumerate() {
                    enum_val = self
                        .builder
                        .build_insert_value(enum_val, fv, (i + 1) as u32, "enum_field")
                        .map_err(|e| CodegenError {
                            message: format!("insert enum field failed: {:?}", e),
                        })?
                        .into_struct_value();
                }

                Ok(enum_val.into())
            }
            // Get enum discriminant for pattern matching
            Rvalue::Discriminant(local) => {
                let idx = local.0 as usize;
                if let Some(alloca) = self.locals.get(&idx) {
                    // Load the enum value
                    let local_ty = self
                        .local_types
                        .get(&idx)
                        .copied()
                        .unwrap_or_else(|| self.context.i64_type().into());
                    let enum_val = self
                        .builder
                        .build_load(local_ty, *alloca, "enum_val")
                        .map_err(|e| CodegenError {
                            message: format!("load enum failed: {:?}", e),
                        })?;

                    // Try to extract discriminant (first field, i32)
                    if let BasicValueEnum::StructValue(sv) = enum_val {
                        let disc =
                            self.builder
                                .build_extract_value(sv, 0, "disc")
                                .map_err(|e| CodegenError {
                                    message: format!("extract discriminant failed: {:?}", e),
                                })?;
                        // Extend i32 to i64 for consistency
                        if let BasicValueEnum::IntValue(iv) = disc {
                            let extended = self
                                .builder
                                .build_int_z_extend(iv, self.context.i64_type(), "disc_ext")
                                .map_err(|e| CodegenError {
                                    message: format!("extend discriminant failed: {:?}", e),
                                })?;
                            Ok(extended.into())
                        } else {
                            Ok(disc)
                        }
                    } else {
                        // Not a struct - return 0 as discriminant
                        Ok(self.context.i64_type().const_zero().into())
                    }
                } else {
                    Err(CodegenError {
                        message: format!("Cannot get discriminant of unknown local: {}", local.0),
                    })
                }
            }
            // Extract field from enum variant
            Rvalue::EnumField(local, field_idx) => {
                let idx = local.0 as usize;
                if let Some(alloca) = self.locals.get(&idx) {
                    let local_ty = self
                        .local_types
                        .get(&idx)
                        .copied()
                        .unwrap_or_else(|| self.context.i64_type().into());
                    let enum_val = self
                        .builder
                        .build_load(local_ty, *alloca, "enum_val")
                        .map_err(|e| CodegenError {
                            message: format!("load enum failed: {:?}", e),
                        })?;

                    if let BasicValueEnum::StructValue(sv) = enum_val {
                        // Field 0 is discriminant, so add 1 to field_idx
                        let field = self
                            .builder
                            .build_extract_value(sv, (*field_idx + 1) as u32, "enum_field")
                            .map_err(|e| CodegenError {
                                message: format!("extract enum field failed: {:?}", e),
                            })?;
                        Ok(field)
                    } else {
                        Err(CodegenError {
                            message: "EnumField on non-struct value".to_string(),
                        })
                    }
                } else {
                    Err(CodegenError {
                        message: format!("Cannot extract field from unknown local: {}", local.0),
                    })
                }
            }
            // Index access: array[index]
            Rvalue::Index(base, index) => {
                let base_val = self.compile_operand(base)?;
                let idx_val = self.compile_operand(index)?;
                let idx_int = self.as_int_value(idx_val)?;

                // Handle array types
                if let BasicValueEnum::ArrayValue(arr) = base_val {
                    // For array values, we need to alloca, store, then GEP
                    let arr_ty = arr.get_type();
                    let elem_ty = arr_ty.get_element_type();
                    let alloca =
                        self.builder
                            .build_alloca(arr_ty, "arr_tmp")
                            .map_err(|e| CodegenError {
                                message: format!("alloca failed: {:?}", e),
                            })?;
                    self.builder
                        .build_store(alloca, arr)
                        .map_err(|e| CodegenError {
                            message: format!("store failed: {:?}", e),
                        })?;

                    let zero = self.context.i64_type().const_zero();
                    let gep = unsafe {
                        self.builder
                            .build_gep(arr_ty, alloca, &[zero, idx_int], "elem_ptr")
                            .map_err(|e| CodegenError {
                                message: format!("gep failed: {:?}", e),
                            })?
                    };
                    self.builder
                        .build_load(elem_ty, gep, "elem")
                        .map_err(|e| CodegenError {
                            message: format!("load failed: {:?}", e),
                        })
                } else if let BasicValueEnum::PointerValue(ptr) = base_val {
                    // Pointer indexing - assume i64 elements
                    let elem_ty = self.context.i64_type();
                    let gep = unsafe {
                        self.builder
                            .build_gep(elem_ty, ptr, &[idx_int], "elem_ptr")
                            .map_err(|e| CodegenError {
                                message: format!("gep failed: {:?}", e),
                            })?
                    };
                    self.builder
                        .build_load(elem_ty, gep, "elem")
                        .map_err(|e| CodegenError {
                            message: format!("load failed: {:?}", e),
                        })
                } else {
                    Err(CodegenError {
                        message: format!("Index on unsupported type: {:?}", base_val.get_type()),
                    })
                }
            }
        }
    }

    /// Compile an operand.
    fn compile_operand(&mut self, operand: &Operand) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match operand {
            Operand::Local(local) | Operand::Copy(local) | Operand::Move(local) => {
                let idx = local.0 as usize;
                if let Some(alloca) = self.locals.get(&idx) {
                    let ty = self
                        .local_types
                        .get(&idx)
                        .copied()
                        .unwrap_or_else(|| self.context.i64_type().into());
                    self.builder
                        .build_load(ty, *alloca, "load")
                        .map_err(|e| CodegenError {
                            message: format!("load failed: {:?}", e),
                        })
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
                        let val = self
                            .context
                            .bool_type()
                            .const_int(if *b { 1 } else { 0 }, false);
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
                BinOp::Add => Ok(self
                    .builder
                    .build_float_add(lhs_float, rhs_float, "fadd")
                    .map_err(|e| CodegenError {
                        message: format!("fadd failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Sub => Ok(self
                    .builder
                    .build_float_sub(lhs_float, rhs_float, "fsub")
                    .map_err(|e| CodegenError {
                        message: format!("fsub failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Mul => Ok(self
                    .builder
                    .build_float_mul(lhs_float, rhs_float, "fmul")
                    .map_err(|e| CodegenError {
                        message: format!("fmul failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Div => Ok(self
                    .builder
                    .build_float_div(lhs_float, rhs_float, "fdiv")
                    .map_err(|e| CodegenError {
                        message: format!("fdiv failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Rem => Ok(self
                    .builder
                    .build_float_rem(lhs_float, rhs_float, "frem")
                    .map_err(|e| CodegenError {
                        message: format!("frem failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Eq => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, lhs_float, rhs_float, "feq")
                    .map_err(|e| CodegenError {
                        message: format!("feq failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Ne => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::ONE, lhs_float, rhs_float, "fne")
                    .map_err(|e| CodegenError {
                        message: format!("fne failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Lt => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lhs_float, rhs_float, "flt")
                    .map_err(|e| CodegenError {
                        message: format!("flt failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Le => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OLE, lhs_float, rhs_float, "fle")
                    .map_err(|e| CodegenError {
                        message: format!("fle failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Gt => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lhs_float, rhs_float, "fgt")
                    .map_err(|e| CodegenError {
                        message: format!("fgt failed: {:?}", e),
                    })?
                    .into()),
                BinOp::Ge => Ok(self
                    .builder
                    .build_float_compare(FloatPredicate::OGE, lhs_float, rhs_float, "fge")
                    .map_err(|e| CodegenError {
                        message: format!("fge failed: {:?}", e),
                    })?
                    .into()),
                _ => Err(CodegenError {
                    message: format!("Float operation not supported: {:?}", op),
                }),
            };
        }

        // Integer operations (using safe helper)
        let lhs_int = self.as_int_value(lhs)?;
        let rhs_int = self.as_int_value(rhs)?;

        let result: IntValue = match op {
            BinOp::Add => self
                .builder
                .build_int_add(lhs_int, rhs_int, "add")
                .map_err(|e| CodegenError {
                    message: format!("add failed: {:?}", e),
                })?,
            BinOp::Sub => self
                .builder
                .build_int_sub(lhs_int, rhs_int, "sub")
                .map_err(|e| CodegenError {
                    message: format!("sub failed: {:?}", e),
                })?,
            BinOp::Mul => self
                .builder
                .build_int_mul(lhs_int, rhs_int, "mul")
                .map_err(|e| CodegenError {
                    message: format!("mul failed: {:?}", e),
                })?,
            BinOp::Div => self
                .builder
                .build_int_signed_div(lhs_int, rhs_int, "div")
                .map_err(|e| CodegenError {
                    message: format!("div failed: {:?}", e),
                })?,
            BinOp::Rem => self
                .builder
                .build_int_signed_rem(lhs_int, rhs_int, "rem")
                .map_err(|e| CodegenError {
                    message: format!("rem failed: {:?}", e),
                })?,
            BinOp::Eq => self
                .builder
                .build_int_compare(IntPredicate::EQ, lhs_int, rhs_int, "eq")
                .map_err(|e| CodegenError {
                    message: format!("eq failed: {:?}", e),
                })?,
            BinOp::Ne => self
                .builder
                .build_int_compare(IntPredicate::NE, lhs_int, rhs_int, "ne")
                .map_err(|e| CodegenError {
                    message: format!("ne failed: {:?}", e),
                })?,
            BinOp::Lt => self
                .builder
                .build_int_compare(IntPredicate::SLT, lhs_int, rhs_int, "lt")
                .map_err(|e| CodegenError {
                    message: format!("lt failed: {:?}", e),
                })?,
            BinOp::Le => self
                .builder
                .build_int_compare(IntPredicate::SLE, lhs_int, rhs_int, "le")
                .map_err(|e| CodegenError {
                    message: format!("le failed: {:?}", e),
                })?,
            BinOp::Gt => self
                .builder
                .build_int_compare(IntPredicate::SGT, lhs_int, rhs_int, "gt")
                .map_err(|e| CodegenError {
                    message: format!("gt failed: {:?}", e),
                })?,
            BinOp::Ge => self
                .builder
                .build_int_compare(IntPredicate::SGE, lhs_int, rhs_int, "ge")
                .map_err(|e| CodegenError {
                    message: format!("ge failed: {:?}", e),
                })?,
            BinOp::And => self
                .builder
                .build_and(lhs_int, rhs_int, "and")
                .map_err(|e| CodegenError {
                    message: format!("and failed: {:?}", e),
                })?,
            BinOp::Or => {
                self.builder
                    .build_or(lhs_int, rhs_int, "or")
                    .map_err(|e| CodegenError {
                        message: format!("or failed: {:?}", e),
                    })?
            }
            // Bitwise operations
            BinOp::BitAnd => self
                .builder
                .build_and(lhs_int, rhs_int, "bitand")
                .map_err(|e| CodegenError {
                    message: format!("bitand failed: {:?}", e),
                })?,
            BinOp::BitOr => self
                .builder
                .build_or(lhs_int, rhs_int, "bitor")
                .map_err(|e| CodegenError {
                    message: format!("bitor failed: {:?}", e),
                })?,
            BinOp::BitXor => self
                .builder
                .build_xor(lhs_int, rhs_int, "bitxor")
                .map_err(|e| CodegenError {
                    message: format!("bitxor failed: {:?}", e),
                })?,
            BinOp::Shl => self
                .builder
                .build_left_shift(lhs_int, rhs_int, "shl")
                .map_err(|e| CodegenError {
                    message: format!("shl failed: {:?}", e),
                })?,
            BinOp::Shr => self
                .builder
                .build_right_shift(lhs_int, rhs_int, false, "shr")
                .map_err(|e| CodegenError {
                    message: format!("shr failed: {:?}", e),
                })?,
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
            UnOp::Neg => self
                .builder
                .build_int_neg(int_val, "neg")
                .map_err(|e| CodegenError {
                    message: format!("neg failed: {:?}", e),
                })?,
            UnOp::Not | UnOp::BitNot => {
                self.builder
                    .build_not(int_val, "not")
                    .map_err(|e| CodegenError {
                        message: format!("not failed: {:?}", e),
                    })?
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
        let env_struct_type = self.context.struct_type(&env_field_types, false);

        // 3. Allocate environment on heap (call malloc)
        let i64_type = self.context.i64_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Get or declare malloc
        let malloc_fn = self.module.get_function("malloc").unwrap_or_else(|| {
            let malloc_type = ptr_type.fn_type(&[i64_type.into()], false);
            self.module.add_function("malloc", malloc_type, None)
        });

        // Calculate size and allocate
        let env_size = env_struct_type.size_of().ok_or_else(|| CodegenError {
            message: "Cannot get size of unsized closure environment".to_string(),
        })?;
        let malloc_result = self
            .builder
            .build_call(malloc_fn, &[env_size.into()], "env_alloc")
            .map_err(|e| CodegenError {
                message: format!("malloc call failed: {:?}", e),
            })?
            .try_as_basic_value()
            .left()
            .ok_or_else(|| CodegenError {
                message: "malloc returned void".into(),
            })?;
        let env_ptr_raw = self.as_pointer_value(malloc_result)?;

        // Check if malloc returned null
        let is_null = self
            .builder
            .build_is_null(env_ptr_raw, "is_null")
            .map_err(|e| CodegenError {
                message: format!("is_null check failed: {:?}", e),
            })?;

        // Get or declare abort for allocation failure
        let abort_fn = self.module.get_function("abort").unwrap_or_else(|| {
            let abort_type = self.context.void_type().fn_type(&[], false);
            self.module.add_function("abort", abort_type, None)
        });

        // Create basic blocks for null check
        let current_fn = self.current_function.ok_or_else(|| CodegenError {
            message: "No current function for malloc null check".to_string(),
        })?;
        let alloc_ok = self.context.append_basic_block(current_fn, "alloc_ok");
        let alloc_fail = self.context.append_basic_block(current_fn, "alloc_fail");

        self.builder
            .build_conditional_branch(is_null, alloc_fail, alloc_ok)
            .map_err(|e| CodegenError {
                message: format!("cond branch failed: {:?}", e),
            })?;

        // In alloc_fail: call abort and unreachable
        self.builder.position_at_end(alloc_fail);
        self.builder
            .build_call(abort_fn, &[], "")
            .map_err(|e| CodegenError {
                message: format!("abort call failed: {:?}", e),
            })?;
        self.builder.build_unreachable().map_err(|e| CodegenError {
            message: format!("unreachable failed: {:?}", e),
        })?;

        // Continue in alloc_ok
        self.builder.position_at_end(alloc_ok);

        // 4. Cast to environment struct pointer (no-op with opaque pointers)
        let env_struct_ptr = self
            .builder
            .build_pointer_cast(env_ptr_raw, ptr_type, "env_struct_ptr")
            .map_err(|e| CodegenError {
                message: format!("pointer cast failed: {:?}", e),
            })?;

        // 5. Store captured values into environment struct
        for (i, val) in env_values.iter().enumerate() {
            let field_ptr = self
                .builder
                .build_struct_gep(
                    env_struct_type,
                    env_struct_ptr,
                    i as u32,
                    &format!("env_field_{}", i),
                )
                .map_err(|e| CodegenError {
                    message: format!("struct gep failed: {:?}", e),
                })?;
            self.builder
                .build_store(field_ptr, *val)
                .map_err(|e| CodegenError {
                    message: format!("store failed: {:?}", e),
                })?;
        }

        // 6. Get the lifted function pointer
        let lifted_fn = self
            .functions
            .get(func_name)
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!("Lifted closure function not found: {}", func_name),
            })?;

        // 7. Cast function pointer to opaque ptr
        let fn_ptr_cast = self
            .builder
            .build_pointer_cast(
                lifted_fn.as_global_value().as_pointer_value(),
                ptr_type,
                "fn_ptr_cast",
            )
            .map_err(|e| CodegenError {
                message: format!("fn pointer cast failed: {:?}", e),
            })?;

        // 8. Cast env pointer to opaque ptr
        let env_ptr_cast = self
            .builder
            .build_pointer_cast(env_struct_ptr, ptr_type, "env_ptr_cast")
            .map_err(|e| CodegenError {
                message: format!("env pointer cast failed: {:?}", e),
            })?;

        // 9. Create closure fat pointer struct: { ptr, ptr }
        let closure_struct_type = self
            .context
            .struct_type(&[ptr_type.into(), ptr_type.into()], false);

        // Build the struct value
        let closure_undef = closure_struct_type.get_undef();
        let closure_with_fn = self
            .builder
            .build_insert_value(closure_undef, fn_ptr_cast, 0, "closure_fn")
            .map_err(|e| CodegenError {
                message: format!("insert fn failed: {:?}", e),
            })?;
        let closure_complete = self
            .builder
            .build_insert_value(closure_with_fn, env_ptr_cast, 1, "closure_env")
            .map_err(|e| CodegenError {
                message: format!("insert env failed: {:?}", e),
            })?;

        // closure_complete is an AggregateValueEnum, convert safely
        match closure_complete {
            inkwell::values::AggregateValueEnum::StructValue(sv) => Ok(sv.into()),
            _ => Err(CodegenError {
                message: "Expected struct value for closure".to_string(),
            }),
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
        if let (BasicValueEnum::IntValue(iv), BasicTypeEnum::IntType(target_int)) = (value, target)
        {
            let src_width = iv.get_type().get_bit_width();
            let dst_width = target_int.get_bit_width();

            if src_width < dst_width {
                // Zero-extend smaller integers (e.g., i1 comparison result to i64)
                let extended = self
                    .builder
                    .build_int_z_extend(iv, target_int, "zext")
                    .map_err(|e| CodegenError {
                        message: format!("zext failed: {:?}", e),
                    })?;
                return Ok(extended.into());
            } else if src_width > dst_width {
                // Truncate larger integers
                let truncated = self
                    .builder
                    .build_int_truncate(iv, target_int, "trunc")
                    .map_err(|e| CodegenError {
                        message: format!("trunc failed: {:?}", e),
                    })?;
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
                    let extended = self
                        .builder
                        .build_int_s_extend(iv, target_int, "sext")
                        .map_err(|e| CodegenError {
                            message: format!("sext failed: {:?}", e),
                        })?;
                    Ok(extended.into())
                } else {
                    // Narrowing: truncate
                    let truncated = self
                        .builder
                        .build_int_truncate(iv, target_int, "trunc")
                        .map_err(|e| CodegenError {
                            message: format!("trunc failed: {:?}", e),
                        })?;
                    Ok(truncated.into())
                }
            }
            // Int to Float cast
            (BasicValueEnum::IntValue(iv), BasicTypeEnum::FloatType(target_float)) => {
                let result = self
                    .builder
                    .build_signed_int_to_float(iv, target_float, "sitofp")
                    .map_err(|e| CodegenError {
                        message: format!("sitofp failed: {:?}", e),
                    })?;
                Ok(result.into())
            }
            // Float to Int cast
            (BasicValueEnum::FloatValue(fv), BasicTypeEnum::IntType(target_int)) => {
                let result = self
                    .builder
                    .build_float_to_signed_int(fv, target_int, "fptosi")
                    .map_err(|e| CodegenError {
                        message: format!("fptosi failed: {:?}", e),
                    })?;
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
                    let extended = self
                        .builder
                        .build_float_ext(fv, target_float, "fpext")
                        .map_err(|e| CodegenError {
                            message: format!("fpext failed: {:?}", e),
                        })?;
                    Ok(extended.into())
                } else {
                    // Truncate f64 to f32
                    let truncated = self
                        .builder
                        .build_float_trunc(fv, target_float, "fptrunc")
                        .map_err(|e| CodegenError {
                            message: format!("fptrunc failed: {:?}", e),
                        })?;
                    Ok(truncated.into())
                }
            }
            // Same type or other - just return
            _ => Ok(value),
        }
    }

    /// Check if a function name is a FORMA builtin that should be routed to the runtime.
    fn is_builtin(&self, name: &str) -> bool {
        matches!(
            name,
            "print"
                | "eprintln"
                | "str"
                | "str_len"
                | "str_concat"
                | "str_contains"
                | "str_starts_with"
                | "str_ends_with"
                | "str_split"
                | "str_trim"
                | "str_to_int"
                | "str_replace_all"
                | "str_char_at"
                | "str_slice"
                | "int_to_str"
                | "char_to_str"
                | "type_of"
                | "panic"
                | "assert"
                | "exit"
                | "unwrap"
                | "expect"
                | "unwrap_or"
                | "is_some"
                | "is_none"
                | "is_ok"
                | "is_err"
                | "sqrt"
                | "pow"
                | "sin"
                | "cos"
                | "tan"
                | "log"
                | "log10"
                | "exp"
                | "floor"
                | "ceil"
                | "round"
                | "abs_float"
                | "vec_new"
                | "vec_len"
                | "vec_push"
                | "vec_pop"
                | "vec_get"
                | "vec_set"
                | "vec_first"
                | "vec_last"
                | "vec_concat"
                | "vec_free"
                | "map_new"
                | "map_len"
                | "map_get"
                | "map_set"
                | "map_insert"
                | "map_contains"
                | "map_remove"
                | "map_free"
                | "time_now"
                | "time_now_ms"
                | "time_sleep"
                | "sleep_ms"
                | "args_count"
                | "args_get"
                | "env_get"
                | "env_set"
                | "random"
                | "random_int"
                | "random_bool"
                | "args"
                | "alloc"
                | "alloc_zeroed"
                | "dealloc"
                | "mem_copy"
                | "mem_set"
                | "file_read"
                | "file_write"
                | "file_exists"
        )
    }

    /// Declare a runtime function as an external C symbol and return its FunctionValue.
    /// This lazily declares functions so they appear in the LLVM module only when needed.
    fn get_or_declare_runtime_function(
        &self,
        name: &str,
    ) -> Result<FunctionValue<'ctx>, CodegenError> {
        // Check if already declared
        if let Some(f) = self.module.get_function(name) {
            return Ok(f);
        }

        let i64_type = self.context.i64_type();
        let f64_type = self.context.f64_type();
        let bool_type = self.context.bool_type();
        let void_type = self.context.void_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let fn_type = match name {
            // I/O - void functions taking a pointer (C string)
            "forma_println" | "forma_print" => void_type.fn_type(&[ptr_type.into()], false),
            "forma_println_int" | "forma_print_int" => void_type.fn_type(&[i64_type.into()], false),
            "forma_println_float" | "forma_print_float" => {
                void_type.fn_type(&[f64_type.into()], false)
            }
            "forma_print_bool" | "forma_println_bool" => {
                void_type.fn_type(&[bool_type.into()], false)
            }
            "forma_read_line" => ptr_type.fn_type(&[], false),

            // String operations
            "forma_str_len" => i64_type.fn_type(&[ptr_type.into()], false),
            "forma_str_concat" => ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
            "forma_str_eq" => bool_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
            "forma_str_contains" => bool_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
            "forma_str_find" => i64_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
            "forma_str_substr" => {
                ptr_type.fn_type(&[ptr_type.into(), i64_type.into(), i64_type.into()], false)
            }
            "forma_str_dup" => ptr_type.fn_type(&[ptr_type.into()], false),
            "forma_str_free" => void_type.fn_type(&[ptr_type.into()], false),
            "forma_int_to_str" => ptr_type.fn_type(&[i64_type.into()], false),
            "forma_float_to_str" => ptr_type.fn_type(&[f64_type.into()], false),
            "forma_bool_to_str" => ptr_type.fn_type(&[bool_type.into()], false),
            "forma_str_to_int" => i64_type.fn_type(&[ptr_type.into()], false),
            "forma_str_to_float" => f64_type.fn_type(&[ptr_type.into()], false),

            // Math
            "forma_abs_int" => i64_type.fn_type(&[i64_type.into()], false),
            "forma_abs_float" => f64_type.fn_type(&[f64_type.into()], false),
            "forma_min_int" | "forma_max_int" | "forma_pow_int" => {
                i64_type.fn_type(&[i64_type.into(), i64_type.into()], false)
            }
            "forma_sqrt" | "forma_floor" | "forma_ceil" | "forma_round" | "forma_sin"
            | "forma_cos" | "forma_tan" | "forma_log" | "forma_log10" | "forma_exp"
            | "forma_asin" | "forma_acos" | "forma_atan" => {
                f64_type.fn_type(&[f64_type.into()], false)
            }
            "forma_pow_float" | "forma_atan2" | "forma_fmod" => {
                f64_type.fn_type(&[f64_type.into(), f64_type.into()], false)
            }
            "forma_pi" | "forma_e" => f64_type.fn_type(&[], false),

            // Memory
            "forma_alloc" => ptr_type.fn_type(&[i64_type.into()], false),
            "forma_alloc_zeroed" => ptr_type.fn_type(&[i64_type.into()], false),
            "forma_dealloc" => void_type.fn_type(&[ptr_type.into()], false),
            "forma_memcpy" | "forma_memmove" => {
                void_type.fn_type(&[ptr_type.into(), ptr_type.into(), i64_type.into()], false)
            }
            "forma_memset" => void_type.fn_type(
                &[
                    ptr_type.into(),
                    self.context.i32_type().into(),
                    i64_type.into(),
                ],
                false,
            ),

            // Vector operations
            "forma_vec_new" => ptr_type.fn_type(&[i64_type.into()], false),
            "forma_vec_len" => i64_type.fn_type(&[ptr_type.into()], false),
            "forma_vec_push" => void_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
            "forma_vec_get" => ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false),
            "forma_vec_set" => {
                void_type.fn_type(&[ptr_type.into(), i64_type.into(), ptr_type.into()], false)
            }
            "forma_vec_free" => void_type.fn_type(&[ptr_type.into()], false),

            // Map operations
            "forma_map_new" => ptr_type.fn_type(&[], false),
            "forma_map_len" => i64_type.fn_type(&[ptr_type.into()], false),
            "forma_map_get" => ptr_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
            "forma_map_set" => {
                void_type.fn_type(&[ptr_type.into(), ptr_type.into(), ptr_type.into()], false)
            }
            "forma_map_contains" => bool_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
            "forma_map_remove" => bool_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),
            "forma_map_free" => void_type.fn_type(&[ptr_type.into()], false),

            // Time
            "forma_time_now_ms" => i64_type.fn_type(&[], false),
            "forma_sleep_ms" => void_type.fn_type(&[i64_type.into()], false),

            // Environment / args
            "forma_args_count" => i64_type.fn_type(&[], false),
            "forma_args_get" => ptr_type.fn_type(&[i64_type.into()], false),
            "forma_env_get" => ptr_type.fn_type(&[ptr_type.into()], false),
            "forma_env_set" => void_type.fn_type(&[ptr_type.into(), ptr_type.into()], false),

            // Panic / error handling
            "forma_panic" => void_type.fn_type(&[ptr_type.into()], false),
            "forma_assert" => void_type.fn_type(&[bool_type.into(), ptr_type.into()], false),
            "forma_unreachable" => void_type.fn_type(&[], false),
            "forma_bounds_check" => void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            "forma_div_check" => void_type.fn_type(&[i64_type.into()], false),

            _ => {
                return Err(CodegenError {
                    message: format!("Unknown runtime function: {}", name),
                });
            }
        };

        Ok(self
            .module
            .add_function(name, fn_type, Some(inkwell::module::Linkage::External)))
    }

    /// Store the result of a builtin call into a destination local.
    /// If the result type doesn't match the local's alloca type, re-allocates
    /// the local with the correct type so subsequent loads get the right type.
    fn store_builtin_result(
        &mut self,
        result: BasicValueEnum<'ctx>,
        dest: &Option<crate::mir::Local>,
    ) -> Result<(), CodegenError> {
        let Some(local) = dest else { return Ok(()) };
        let idx = local.0 as usize;
        let target_ty = self.local_types.get(&idx).copied();

        // Check if the result type matches the local's declared type
        let needs_realloc = target_ty.is_some_and(|t| result.get_type() != t);

        if needs_realloc {
            // Re-create the alloca with the actual result type
            // We need to position at the entry block temporarily
            let current_fn = self.current_function.ok_or_else(|| CodegenError {
                message: "No current function".to_string(),
            })?;
            let entry = current_fn
                .get_first_basic_block()
                .ok_or_else(|| CodegenError {
                    message: "No entry block".to_string(),
                })?;

            // Save current position
            let current_block = self
                .builder
                .get_insert_block()
                .ok_or_else(|| CodegenError {
                    message: "No current block".to_string(),
                })?;

            // Insert alloca at the start of the entry block
            if let Some(first_inst) = entry.get_first_instruction() {
                self.builder.position_before(&first_inst);
            } else {
                self.builder.position_at_end(entry);
            }

            let new_alloca = self
                .builder
                .build_alloca(result.get_type(), &format!("local_{}_retyped", idx))
                .map_err(|e| CodegenError {
                    message: format!("alloca failed: {:?}", e),
                })?;

            // Update the local
            self.locals.insert(idx, new_alloca);
            self.local_types.insert(idx, result.get_type());

            // Restore position
            self.builder.position_at_end(current_block);

            // Store the result
            self.builder
                .build_store(new_alloca, result)
                .map_err(|e| CodegenError {
                    message: format!("store failed: {:?}", e),
                })?;
        } else if let Some(alloca) = self.locals.get(&idx).copied() {
            self.builder
                .build_store(alloca, result)
                .map_err(|e| CodegenError {
                    message: format!("store failed: {:?}", e),
                })?;
        }

        Ok(())
    }

    /// Helper: call a runtime function with given args, store result in dest using store_builtin_result.
    fn call_runtime_and_store(
        &mut self,
        runtime_name: &str,
        call_args: &[BasicValueEnum<'ctx>],
        label: &str,
        dest: &Option<crate::mir::Local>,
    ) -> Result<(), CodegenError> {
        let f = self.get_or_declare_runtime_function(runtime_name)?;
        let args_meta: Vec<BasicMetadataValueEnum> =
            call_args.iter().map(|a| (*a).into()).collect();
        let call = self
            .builder
            .build_call(f, &args_meta, label)
            .map_err(|e| CodegenError {
                message: format!("call failed: {:?}", e),
            })?;
        if let Some(result) = call.try_as_basic_value().left() {
            self.store_builtin_result(result, dest)?;
        }
        Ok(())
    }

    /// Dispatch a print call for a single value based on its LLVM type.
    fn emit_print_value(&mut self, val: BasicValueEnum<'ctx>) -> Result<(), CodegenError> {
        match val {
            BasicValueEnum::PointerValue(_) => {
                let f = self.get_or_declare_runtime_function("forma_println")?;
                self.builder
                    .build_call(f, &[val.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            BasicValueEnum::IntValue(iv) => {
                if iv.get_type().get_bit_width() == 1 {
                    let f = self.get_or_declare_runtime_function("forma_println_bool")?;
                    self.builder
                        .build_call(f, &[val.into()], "")
                        .map_err(|e| CodegenError {
                            message: format!("call failed: {:?}", e),
                        })?;
                } else {
                    let f = self.get_or_declare_runtime_function("forma_println_int")?;
                    self.builder
                        .build_call(f, &[val.into()], "")
                        .map_err(|e| CodegenError {
                            message: format!("call failed: {:?}", e),
                        })?;
                }
            }
            BasicValueEnum::FloatValue(_) => {
                let f = self.get_or_declare_runtime_function("forma_println_float")?;
                self.builder
                    .build_call(f, &[val.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            _ => {
                let f = self.get_or_declare_runtime_function("forma_println_int")?;
                let zero = self.context.i64_type().const_zero();
                self.builder
                    .build_call(f, &[zero.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
        }
        Ok(())
    }

    /// Compile a builtin function call by dispatching to the appropriate runtime function(s).
    fn compile_builtin_call(
        &mut self,
        func_name: &str,
        args: &[Operand],
        dest: &Option<crate::mir::Local>,
        blocks: &HashMap<usize, inkwell::basic_block::BasicBlock>,
        next: &crate::mir::BlockId,
    ) -> Result<(), CodegenError> {
        match func_name {
            "print" | "eprintln" => {
                for arg in args {
                    let val = self.compile_operand(arg)?;
                    self.emit_print_value(val)?;
                }
            }
            "str_len" => {
                let val = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_str_len", &[val], "str_len", dest)?;
            }
            "str_concat" => {
                let a = self.compile_operand(&args[0])?;
                let b = self.compile_operand(&args[1])?;
                self.call_runtime_and_store("forma_str_concat", &[a, b], "str_concat", dest)?;
            }
            "str_contains" => {
                let a = self.compile_operand(&args[0])?;
                let b = self.compile_operand(&args[1])?;
                self.call_runtime_and_store("forma_str_contains", &[a, b], "str_contains", dest)?;
            }
            "int_to_str" => {
                let val = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_int_to_str", &[val], "int_to_str", dest)?;
            }
            "str_to_int" => {
                let val = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_str_to_int", &[val], "str_to_int", dest)?;
            }
            "str" => {
                // str(value) - convert any value to string
                let val = self.compile_operand(&args[0])?;
                let runtime_fn = match val {
                    BasicValueEnum::IntValue(iv) if iv.get_type().get_bit_width() == 1 => {
                        "forma_bool_to_str"
                    }
                    BasicValueEnum::IntValue(_) => "forma_int_to_str",
                    BasicValueEnum::FloatValue(_) => "forma_float_to_str",
                    BasicValueEnum::PointerValue(_) => "forma_str_dup",
                    _ => "forma_int_to_str",
                };
                self.call_runtime_and_store(runtime_fn, &[val], "to_str", dest)?;
            }
            "panic" => {
                let val = self.compile_operand(&args[0])?;
                let f = self.get_or_declare_runtime_function("forma_panic")?;
                self.builder
                    .build_call(f, &[val.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
                self.builder.build_unreachable().map_err(|e| CodegenError {
                    message: format!("unreachable failed: {:?}", e),
                })?;
                return Ok(()); // Don't branch after noreturn
            }
            "assert" => {
                let cond = self.compile_operand(&args[0])?;
                let msg_ptr = if args.len() > 1 {
                    self.compile_operand(&args[1])?
                } else {
                    let msg = self.context.const_string(b"assertion failed", true);
                    let global = self.module.add_global(msg.get_type(), None, "assert_msg");
                    global.set_constant(true);
                    global.set_initializer(&msg);
                    global.as_pointer_value().into()
                };
                let f = self.get_or_declare_runtime_function("forma_assert")?;
                self.builder
                    .build_call(f, &[cond.into(), msg_ptr.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            "exit" => {
                let code = self.compile_operand(&args[0])?;
                let exit_fn = self.module.get_function("exit").unwrap_or_else(|| {
                    let exit_type = self
                        .context
                        .void_type()
                        .fn_type(&[self.context.i32_type().into()], false);
                    self.module.add_function(
                        "exit",
                        exit_type,
                        Some(inkwell::module::Linkage::External),
                    )
                });
                let code_i32 = if let BasicValueEnum::IntValue(iv) = code {
                    if iv.get_type().get_bit_width() != 32 {
                        self.builder
                            .build_int_truncate(iv, self.context.i32_type(), "trunc")
                            .map_err(|e| CodegenError {
                                message: format!("trunc failed: {:?}", e),
                            })?
                            .into()
                    } else {
                        code
                    }
                } else {
                    code
                };
                self.builder
                    .build_call(exit_fn, &[code_i32.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
                self.builder.build_unreachable().map_err(|e| CodegenError {
                    message: format!("unreachable failed: {:?}", e),
                })?;
                return Ok(());
            }
            // Math f64->f64
            "sqrt" | "sin" | "cos" | "tan" | "log" | "log10" | "exp" | "floor" | "ceil"
            | "round" | "abs_float" => {
                let val = self.compile_operand(&args[0])?;
                let runtime_name = format!("forma_{}", func_name);
                self.call_runtime_and_store(&runtime_name, &[val], func_name, dest)?;
            }
            // Math (f64, f64)->f64
            "pow" => {
                let base = self.compile_operand(&args[0])?;
                let exp = self.compile_operand(&args[1])?;
                self.call_runtime_and_store("forma_pow_float", &[base, exp], "pow", dest)?;
            }
            // Memory
            "alloc" => {
                let size = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_alloc", &[size], "alloc", dest)?;
            }
            "dealloc" => {
                let ptr = self.compile_operand(&args[0])?;
                let f = self.get_or_declare_runtime_function("forma_dealloc")?;
                self.builder
                    .build_call(f, &[ptr.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            // Vector operations
            "vec_new" => {
                let elem_size = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_vec_new", &[elem_size], "vec_new", dest)?;
            }
            "vec_len" => {
                let v = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_vec_len", &[v], "vec_len", dest)?;
            }
            "vec_push" => {
                let v = self.compile_operand(&args[0])?;
                let elem = self.compile_operand(&args[1])?;
                let f = self.get_or_declare_runtime_function("forma_vec_push")?;
                self.builder
                    .build_call(f, &[v.into(), elem.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            "vec_get" => {
                let v = self.compile_operand(&args[0])?;
                let idx = self.compile_operand(&args[1])?;
                self.call_runtime_and_store("forma_vec_get", &[v, idx], "vec_get", dest)?;
            }
            "vec_set" => {
                let v = self.compile_operand(&args[0])?;
                let idx = self.compile_operand(&args[1])?;
                let elem = self.compile_operand(&args[2])?;
                let f = self.get_or_declare_runtime_function("forma_vec_set")?;
                self.builder
                    .build_call(f, &[v.into(), idx.into(), elem.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            "vec_free" => {
                let v = self.compile_operand(&args[0])?;
                let f = self.get_or_declare_runtime_function("forma_vec_free")?;
                self.builder
                    .build_call(f, &[v.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            // Map operations
            "map_new" => {
                self.call_runtime_and_store("forma_map_new", &[], "map_new", dest)?;
            }
            "map_len" => {
                let m = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_map_len", &[m], "map_len", dest)?;
            }
            "map_get" => {
                let m = self.compile_operand(&args[0])?;
                let key = self.compile_operand(&args[1])?;
                self.call_runtime_and_store("forma_map_get", &[m, key], "map_get", dest)?;
            }
            "map_set" | "map_insert" => {
                let m = self.compile_operand(&args[0])?;
                let key = self.compile_operand(&args[1])?;
                let val = self.compile_operand(&args[2])?;
                let f = self.get_or_declare_runtime_function("forma_map_set")?;
                self.builder
                    .build_call(f, &[m.into(), key.into(), val.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            "map_contains" => {
                let m = self.compile_operand(&args[0])?;
                let key = self.compile_operand(&args[1])?;
                self.call_runtime_and_store("forma_map_contains", &[m, key], "map_contains", dest)?;
            }
            "map_remove" => {
                let m = self.compile_operand(&args[0])?;
                let key = self.compile_operand(&args[1])?;
                self.call_runtime_and_store("forma_map_remove", &[m, key], "map_remove", dest)?;
            }
            "map_free" => {
                let m = self.compile_operand(&args[0])?;
                let f = self.get_or_declare_runtime_function("forma_map_free")?;
                self.builder
                    .build_call(f, &[m.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            // Time
            "time_now_ms" | "time_now" => {
                self.call_runtime_and_store("forma_time_now_ms", &[], "time_now_ms", dest)?;
            }
            "sleep_ms" | "time_sleep" => {
                let ms = self.compile_operand(&args[0])?;
                let f = self.get_or_declare_runtime_function("forma_sleep_ms")?;
                self.builder
                    .build_call(f, &[ms.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            // Environment / args
            "args_count" => {
                self.call_runtime_and_store("forma_args_count", &[], "args_count", dest)?;
            }
            "args_get" => {
                let idx = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_args_get", &[idx], "args_get", dest)?;
            }
            "env_get" => {
                let name = self.compile_operand(&args[0])?;
                self.call_runtime_and_store("forma_env_get", &[name], "env_get", dest)?;
            }
            "env_set" => {
                let name = self.compile_operand(&args[0])?;
                let val = self.compile_operand(&args[1])?;
                let f = self.get_or_declare_runtime_function("forma_env_set")?;
                self.builder
                    .build_call(f, &[name.into(), val.into()], "")
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;
            }
            _ => {
                return Err(CodegenError {
                    message: format!(
                        "Builtin '{}' not yet implemented in LLVM codegen",
                        func_name
                    ),
                });
            }
        }

        // Branch to next block (unless we already returned for noreturn functions)
        if let Some(&bb) = blocks.get(&(next.0 as usize)) {
            self.builder
                .build_unconditional_branch(bb)
                .map_err(|e| CodegenError {
                    message: format!("branch failed: {:?}", e),
                })?;
        }

        Ok(())
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
                    self.builder
                        .build_return(Some(&val))
                        .map_err(|e| CodegenError {
                            message: format!("return failed: {:?}", e),
                        })?;
                } else {
                    self.builder.build_return(None).map_err(|e| CodegenError {
                        message: format!("return failed: {:?}", e),
                    })?;
                }
            }
            Terminator::Goto(target) => {
                let bb = blocks
                    .get(&(target.0 as usize))
                    .ok_or_else(|| CodegenError {
                        message: format!("Goto: target block {} not found", target.0),
                    })?;
                self.builder
                    .build_unconditional_branch(*bb)
                    .map_err(|e| CodegenError {
                        message: format!("branch failed: {:?}", e),
                    })?;
            }
            Terminator::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_operand = self.compile_operand(cond)?;
                let cond_val = self.as_int_value(cond_operand)?;
                let then_bb = blocks
                    .get(&(then_block.0 as usize))
                    .copied()
                    .ok_or_else(|| CodegenError {
                        message: "Missing then block".into(),
                    })?;
                let else_bb = blocks
                    .get(&(else_block.0 as usize))
                    .copied()
                    .ok_or_else(|| CodegenError {
                        message: "Missing else block".into(),
                    })?;

                // Convert to bool if needed (compare != 0 for non-bool types)
                let cond_bool = if cond_val.get_type().get_bit_width() == 1 {
                    cond_val
                } else {
                    let zero = cond_val.get_type().const_zero();
                    self.builder
                        .build_int_compare(IntPredicate::NE, cond_val, zero, "tobool")
                        .map_err(|e| CodegenError {
                            message: format!("cmp failed: {:?}", e),
                        })?
                };

                self.builder
                    .build_conditional_branch(cond_bool, then_bb, else_bb)
                    .map_err(|e| CodegenError {
                        message: format!("branch failed: {:?}", e),
                    })?;
            }
            Terminator::Switch {
                operand,
                targets,
                default,
            } => {
                let switch_operand = self.compile_operand(operand)?;
                let val = self.as_int_value(switch_operand)?;
                let default_bb =
                    blocks
                        .get(&(default.0 as usize))
                        .copied()
                        .ok_or_else(|| CodegenError {
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

                self.builder
                    .build_switch(val, default_bb, &cases)
                    .map_err(|e| CodegenError {
                        message: format!("switch failed: {:?}", e),
                    })?;
            }
            Terminator::Call {
                func,
                args,
                arg_pass_modes: _,
                dest,
                next,
            } => {
                // Check if this is a builtin function that should go to the runtime
                if self.is_builtin(func) {
                    return self.compile_builtin_call(func, args, dest, blocks, next);
                }

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
                    .map_err(|e| CodegenError {
                        message: format!("call failed: {:?}", e),
                    })?;

                // Store result if there's a destination
                if let Some(result) = call.try_as_basic_value().left() {
                    self.store_builtin_result(result, dest)?;
                }

                // Jump to next block
                if let Some(&bb) = blocks.get(&(next.0 as usize)) {
                    self.builder
                        .build_unconditional_branch(bb)
                        .map_err(|e| CodegenError {
                            message: format!("branch failed: {:?}", e),
                        })?;
                }
            }
            Terminator::CallIndirect {
                callee,
                args,
                arg_pass_modes: _,
                dest,
                next,
            } => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let i64_type = self.context.i64_type();

                // 1. Compile the closure operand (should be a fat pointer struct)
                let closure_val = self.compile_operand(callee)?;
                let closure_struct = self.as_struct_value(closure_val)?;

                // 2. Extract function pointer and environment pointer
                let fn_ptr_raw = self
                    .builder
                    .build_extract_value(closure_struct, 0, "fn_ptr_raw")
                    .map_err(|e| CodegenError {
                        message: format!("extract fn_ptr failed: {:?}", e),
                    })?;
                let fn_ptr_i8 = self.as_pointer_value(fn_ptr_raw)?;
                let env_ptr_raw = self
                    .builder
                    .build_extract_value(closure_struct, 1, "env_ptr")
                    .map_err(|e| CodegenError {
                        message: format!("extract env_ptr failed: {:?}", e),
                    })?;
                let env_ptr = self.as_pointer_value(env_ptr_raw)?;

                // 3. Compile arguments (environment pointer is implicit first arg)
                let mut compiled_args: Vec<BasicMetadataValueEnum> = vec![env_ptr.into()];
                for arg in args {
                    let val = self.compile_operand(arg)?;
                    compiled_args.push(val.into());
                }

                // 4. Build function type: (i8*, args...) -> i64
                // Environment pointer is always first parameter
                let mut param_types: Vec<BasicMetadataTypeEnum> = vec![ptr_type.into()];
                for _ in args {
                    param_types.push(i64_type.into());
                }
                let fn_type = i64_type.fn_type(&param_types, false);

                // 5. Cast function pointer to correct type
                let fn_ptr_typed = self
                    .builder
                    .build_pointer_cast(fn_ptr_i8, ptr_type, "fn_ptr_typed")
                    .map_err(|e| CodegenError {
                        message: format!("fn type cast failed: {:?}", e),
                    })?;

                // 6. Build indirect call
                let call = self
                    .builder
                    .build_indirect_call(fn_type, fn_ptr_typed, &compiled_args, "indirect_call")
                    .map_err(|e| CodegenError {
                        message: format!("indirect call failed: {:?}", e),
                    })?;

                // 7. Store result if there's a destination
                if let Some(local) = dest
                    && let Some(result) = call.try_as_basic_value().left()
                    && let Some(alloca) = self.locals.get(&(local.0 as usize))
                {
                    self.builder
                        .build_store(*alloca, result)
                        .map_err(|e| CodegenError {
                            message: format!("store failed: {:?}", e),
                        })?;
                }

                // 8. Jump to next block
                if let Some(&bb) = blocks.get(&(next.0 as usize)) {
                    self.builder
                        .build_unconditional_branch(bb)
                        .map_err(|e| CodegenError {
                            message: format!("branch failed: {:?}", e),
                        })?;
                }
            }
            // Spawn an async task - for LLVM, execute synchronously
            Terminator::Spawn { expr, dest, next } => {
                // In LLVM backend, we don't have true async support
                // Execute the expression synchronously and store the result
                let val = self.compile_operand(expr)?;

                // Store result if there's a destination
                if let Some(local) = dest
                    && let Some(alloca) = self.locals.get(&(local.0 as usize))
                {
                    self.builder
                        .build_store(*alloca, val)
                        .map_err(|e| CodegenError {
                            message: format!("store failed: {:?}", e),
                        })?;
                }

                // Jump to next block
                if let Some(&bb) = blocks.get(&(next.0 as usize)) {
                    self.builder
                        .build_unconditional_branch(bb)
                        .map_err(|e| CodegenError {
                            message: format!("branch failed: {:?}", e),
                        })?;
                } else {
                    return Err(CodegenError {
                        message: format!("Spawn: missing next block {}", next.0),
                    });
                }
            }
            // Await a task/future - for LLVM, the value is already resolved
            Terminator::Await { task, dest, next } => {
                // In LLVM backend without true async, the task value IS the result
                let val = self.compile_operand(task)?;

                // Store result if there's a destination
                if let Some(local) = dest
                    && let Some(alloca) = self.locals.get(&(local.0 as usize))
                {
                    self.builder
                        .build_store(*alloca, val)
                        .map_err(|e| CodegenError {
                            message: format!("store failed: {:?}", e),
                        })?;
                }

                // Jump to next block
                if let Some(&bb) = blocks.get(&(next.0 as usize)) {
                    self.builder
                        .build_unconditional_branch(bb)
                        .map_err(|e| CodegenError {
                            message: format!("branch failed: {:?}", e),
                        })?;
                } else {
                    return Err(CodegenError {
                        message: format!("Await: missing next block {}", next.0),
                    });
                }
            }
            Terminator::Unreachable => {
                self.builder.build_unreachable().map_err(|e| CodegenError {
                    message: format!("unreachable failed: {:?}", e),
                })?;
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
            // Unit is used as a placeholder type for temporaries in MIR;
            // use i64 so it can hold any integer/pointer-sized value without truncation
            Ty::Unit => Ok(self.context.i64_type().into()),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn make_empty_main() -> Program {
        let mut functions = HashMap::new();
        let entry_block = BasicBlock {
            id: crate::mir::BlockId(0),
            stmts: vec![],
            terminator: Some(Terminator::Return(Some(Operand::Constant(Constant::Int(
                0,
            ))))),
        };
        let func = Function {
            name: "main".to_string(),
            params: vec![],
            param_names: vec![],
            param_pass_modes: vec![],
            return_ty: Ty::Int,
            locals: vec![],
            blocks: vec![entry_block],
            entry_block: crate::mir::BlockId(0),
            preconditions: vec![],
            postconditions: vec![],
        };
        functions.insert("main".to_string(), func);
        Program {
            functions,
            entry: Some("main".to_string()),
            enum_variants: HashMap::new(),
        }
    }

    #[test]
    fn test_compile_empty_main() {
        let program = make_empty_main();
        let ctx = Context::create();
        let mut codegen = LLVMCodegen::new(&ctx, "test");
        assert!(codegen.compile(&program).is_ok());
        let ir = codegen.get_llvm_ir();
        assert!(
            ir.contains("define"),
            "IR should contain function definitions"
        );
    }

    #[test]
    fn test_compile_integer_arithmetic() {
        let mut functions = HashMap::new();
        let result_local = crate::mir::Local(0);
        let entry_block = BasicBlock {
            id: crate::mir::BlockId(0),
            stmts: vec![Statement {
                kind: StatementKind::Assign(
                    result_local,
                    Rvalue::BinaryOp(
                        BinOp::Add,
                        Operand::Constant(Constant::Int(2)),
                        Operand::Constant(Constant::Int(3)),
                    ),
                ),
            }],
            terminator: Some(Terminator::Return(Some(Operand::Local(result_local)))),
        };
        let func = Function {
            name: "main".to_string(),
            params: vec![],
            param_names: vec![],
            param_pass_modes: vec![],
            return_ty: Ty::Int,
            locals: vec![crate::mir::LocalDecl {
                ty: Ty::Int,
                name: Some("result".to_string()),
            }],
            blocks: vec![entry_block],
            entry_block: crate::mir::BlockId(0),
            preconditions: vec![],
            postconditions: vec![],
        };
        functions.insert("main".to_string(), func);
        let program = Program {
            functions,
            entry: Some("main".to_string()),
            enum_variants: HashMap::new(),
        };

        let ctx = Context::create();
        let mut codegen = LLVMCodegen::new(&ctx, "test_arith");
        assert!(codegen.compile(&program).is_ok());
        let ir = codegen.get_llvm_ir();
        // LLVM may constant-fold 2+3=5, so just verify compilation succeeded
        // and produced valid IR with a main function
        assert!(ir.contains("main"), "IR should contain main function");
    }
}
