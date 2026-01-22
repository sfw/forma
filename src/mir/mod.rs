//! Mid-level Intermediate Representation (MIR) for ARIA.
//!
//! The MIR module provides a simplified representation of ARIA programs
//! that is easier to interpret and compile than the AST.
//!
//! # Structure
//!
//! MIR uses a control-flow graph (CFG) representation where:
//! - Programs consist of functions
//! - Functions consist of basic blocks
//! - Basic blocks contain statements and end with a terminator
//! - Statements are simple assignments
//! - Terminators control flow (return, goto, branch, call)
//!
//! # Modules
//!
//! - [`mir`]: MIR data structures
//! - [`lower`]: AST to MIR lowering
//! - [`interp`]: Simple MIR interpreter
//!
//! # Example
//!
//! ```ignore
//! use aria::mir::{Program, Lowerer};
//!
//! let mir = Lowerer::new().lower(&typed_ast)?;
//! let result = Interpreter::new().run(&mir, "main", &[])?;
//! ```

pub mod interp;
pub mod lower;
pub mod mir;

pub use interp::{Interpreter, InterpError, Value};
pub use lower::{Lowerer, LowerError};
pub use mir::{
    BasicBlock, BinOp, BlockId, Constant, Function, Local, LocalDecl, Mutability, Operand,
    Program, Rvalue, Statement, StatementKind, Terminator, UnOp,
};
