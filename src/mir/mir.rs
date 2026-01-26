//! Mid-level Intermediate Representation (MIR) for FORMA.
//!
//! MIR is a simplified representation of the program that:
//! - Uses a control-flow graph (CFG) structure
//! - Has explicit basic blocks and terminators
//! - Uses simple three-address code style instructions
//! - Is fully typed
//!
//! This makes it easier to:
//! - Interpret for testing
//! - Generate machine code
//! - Perform optimizations

use std::collections::HashMap;
use std::fmt;

use crate::types::Ty;

/// A unique identifier for a function in MIR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FnId(pub u32);

/// A unique identifier for a basic block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

/// A unique identifier for a local variable/temporary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Local(pub u32);

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.0)
    }
}

/// A MIR program - collection of functions.
#[derive(Debug, Clone)]
pub struct Program {
    pub functions: HashMap<String, Function>,
    pub entry: Option<String>,
    /// Enum variant registry: maps (enum_name, variant_name) -> variant index
    pub enum_variants: HashMap<(String, String), usize>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            entry: None,
            enum_variants: HashMap::new(),
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

/// A contract condition (precondition or postcondition).
#[derive(Debug, Clone)]
pub struct MirContract {
    /// The expression as a string (for error messages)
    pub expr_string: String,
    /// Optional error message
    pub message: Option<String>,
    /// The actual condition expression (for evaluation)
    pub condition: Option<Box<crate::parser::Expr>>,
}

/// A function in MIR.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(Local, Ty)>,
    /// Parameter names for contract evaluation (parallel to params)
    pub param_names: Vec<(String, Ty)>,
    pub return_ty: Ty,
    pub locals: Vec<LocalDecl>,
    pub blocks: Vec<BasicBlock>,
    pub entry_block: BlockId,
    /// Preconditions (@pre) - checked at function entry
    pub preconditions: Vec<MirContract>,
    /// Postconditions (@post) - checked at function exit
    pub postconditions: Vec<MirContract>,
}

impl Function {
    pub fn new(name: String, params: Vec<(Local, Ty)>, return_ty: Ty) -> Self {
        Self {
            name,
            params,
            param_names: Vec::new(),
            return_ty,
            locals: Vec::new(),
            blocks: Vec::new(),
            entry_block: BlockId(0),
            preconditions: Vec::new(),
            postconditions: Vec::new(),
        }
    }

    /// Add a new local variable and return its ID.
    pub fn add_local(&mut self, ty: Ty, name: Option<String>) -> Local {
        let id = Local(self.locals.len() as u32);
        self.locals.push(LocalDecl { ty, name });
        id
    }

    /// Add a new basic block and return its ID.
    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(BasicBlock::new(id));
        id
    }

    /// Get a mutable reference to a block.
    pub fn block_mut(&mut self, id: BlockId) -> &mut BasicBlock {
        &mut self.blocks[id.0 as usize]
    }

    /// Get a reference to a block.
    pub fn block(&self, id: BlockId) -> &BasicBlock {
        &self.blocks[id.0 as usize]
    }
}

/// Declaration of a local variable.
#[derive(Debug, Clone)]
pub struct LocalDecl {
    pub ty: Ty,
    pub name: Option<String>,
}

/// A basic block - a sequence of statements ending with a terminator.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub stmts: Vec<Statement>,
    pub terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            stmts: Vec::new(),
            terminator: None,
        }
    }

    pub fn push(&mut self, stmt: Statement) {
        self.stmts.push(stmt);
    }

    pub fn terminate(&mut self, term: Terminator) {
        self.terminator = Some(term);
    }
}

/// A statement in MIR - assigns a value to a local.
#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    /// Assign a value to a local: `_0 = rvalue`
    Assign(Local, Rvalue),
    /// No-op (placeholder)
    Nop,
}

/// An rvalue - something that produces a value.
#[derive(Debug, Clone)]
pub enum Rvalue {
    /// Use a value directly
    Use(Operand),
    /// Binary operation: `operand op operand`
    BinaryOp(BinOp, Operand, Operand),
    /// Unary operation: `op operand`
    UnaryOp(UnOp, Operand),
    /// Create a reference: `&place` or `&mut place`
    Ref(Local, Mutability),
    /// Dereference a pointer: `*operand`
    Deref(Operand),
    /// Tuple construction
    Tuple(Vec<Operand>),
    /// Array construction
    Array(Vec<Operand>),
    /// Struct construction
    Struct(String, Vec<(String, Operand)>),
    /// Enum variant construction: `Color::Red` or `Some(42)`
    ///
    /// Creates an enum value with the specified type name, variant, and fields.
    /// - `type_name`: The enum type (e.g., "Option", "Result", "Color")
    /// - `variant`: The variant name (e.g., "Some", "None", "Ok", "Err")
    /// - `fields`: Values for tuple-style variants (empty for unit variants)
    ///
    /// # Examples
    /// ```text
    /// // None -> Enum { type_name: "Option", variant: "None", fields: [] }
    /// // Some(42) -> Enum { type_name: "Option", variant: "Some", fields: [42] }
    /// ```
    Enum {
        type_name: String,
        variant: String,
        fields: Vec<Operand>,
    },
    /// Get discriminant (tag) of an enum value for pattern matching.
    ///
    /// Returns an integer that uniquely identifies the variant.
    /// Used to implement match expressions by comparing discriminants.
    Discriminant(Local),
    /// Extract a field from an enum variant by index.
    ///
    /// Used after matching to extract bound values from variants like `Some(x)`.
    /// The index is 0-based for the variant's fields.
    EnumField(Local, usize),
    /// Field access: `operand.field`
    Field(Operand, String),
    /// Tuple field access: `operand.0`
    TupleField(Operand, usize),
    /// Index access: `operand[index]`
    Index(Operand, Operand),
    /// Cast: `operand as ty`
    Cast(Operand, Ty),
    /// Closure creation with captured values
    ///
    /// Creates a closure value that packages a function with its captured environment.
    /// - `func_name`: The lifted function that implements the closure body
    /// - `captures`: Values captured from the enclosing scope
    Closure {
        func_name: String,
        captures: Vec<Operand>,
    },
}

/// An operand - something that can be used as input.
#[derive(Debug, Clone)]
pub enum Operand {
    /// A constant value
    Constant(Constant),
    /// A local variable
    Local(Local),
    /// Copy from a local (for Copy types)
    Copy(Local),
    /// Move from a local
    Move(Local),
}

/// A constant value.
#[derive(Debug, Clone)]
pub enum Constant {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Char(char),
    Str(String),
}

impl Constant {
    pub fn ty(&self) -> Ty {
        match self {
            Constant::Unit => Ty::Unit,
            Constant::Bool(_) => Ty::Bool,
            Constant::Int(_) => Ty::Int,
            Constant::Float(_) => Ty::Float,
            Constant::Char(_) => Ty::Char,
            Constant::Str(_) => Ty::Str,
        }
    }
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical
    And,
    Or,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

/// Mutability.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Immutable,
    Mutable,
}

/// A terminator - ends a basic block with control flow.
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Return from the function
    Return(Option<Operand>),
    /// Unconditional jump
    Goto(BlockId),
    /// Conditional branch
    If {
        cond: Operand,
        then_block: BlockId,
        else_block: BlockId,
    },
    /// Switch/match on an integer value
    Switch {
        operand: Operand,
        targets: Vec<(i64, BlockId)>,
        default: BlockId,
    },
    /// Function call
    Call {
        func: String,
        args: Vec<Operand>,
        dest: Option<Local>,
        next: BlockId,
    },
    /// Indirect function call (for closures and higher-order functions)
    ///
    /// Calls a function value stored in an operand. The callee can be:
    /// - A closure value with captured environment
    /// - A function reference
    CallIndirect {
        callee: Operand,
        args: Vec<Operand>,
        dest: Option<Local>,
        next: BlockId,
    },
    /// Spawn an async task
    ///
    /// Takes an async expression and spawns it as a concurrent task.
    /// Returns a Task[T] handle.
    Spawn {
        expr: Operand,
        dest: Option<Local>,
        next: BlockId,
    },
    /// Await a task or future
    ///
    /// Waits for a Task[T] or Future[T] to complete and extracts the value.
    Await {
        task: Operand,
        dest: Option<Local>,
        next: BlockId,
    },
    /// Unreachable (for diverging code paths)
    Unreachable,
}

/// Display implementation for readable MIR output.
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, func) in &self.functions {
            writeln!(f, "fn {}:", name)?;
            write!(f, "{}", func)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Parameters
        write!(f, "  params: [")?;
        for (i, (local, ty)) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", local, ty)?;
        }
        writeln!(f, "]")?;
        writeln!(f, "  return: {}", self.return_ty)?;

        // Locals
        writeln!(f, "  locals:")?;
        for (i, decl) in self.locals.iter().enumerate() {
            let name = decl.name.as_deref().unwrap_or("_");
            writeln!(f, "    _{}: {} ({})", i, decl.ty, name)?;
        }

        // Blocks
        for block in &self.blocks {
            writeln!(f)?;
            writeln!(f, "  {}:", block.id)?;
            for stmt in &block.stmts {
                writeln!(f, "    {}", stmt)?;
            }
            if let Some(term) = &block.terminator {
                writeln!(f, "    {}", term)?;
            }
        }

        Ok(())
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            StatementKind::Assign(local, rvalue) => write!(f, "{} = {}", local, rvalue),
            StatementKind::Nop => write!(f, "nop"),
        }
    }
}

impl fmt::Display for Rvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rvalue::Use(op) => write!(f, "{}", op),
            Rvalue::BinaryOp(op, l, r) => write!(f, "{} {:?} {}", l, op, r),
            Rvalue::UnaryOp(op, operand) => write!(f, "{:?} {}", op, operand),
            Rvalue::Ref(local, Mutability::Immutable) => write!(f, "&{}", local),
            Rvalue::Ref(local, Mutability::Mutable) => write!(f, "&mut {}", local),
            Rvalue::Deref(op) => write!(f, "*{}", op),
            Rvalue::Tuple(ops) => {
                write!(f, "(")?;
                for (i, op) in ops.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", op)?;
                }
                write!(f, ")")
            }
            Rvalue::Array(ops) => {
                write!(f, "[")?;
                for (i, op) in ops.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", op)?;
                }
                write!(f, "]")
            }
            Rvalue::Struct(name, fields) => {
                write!(f, "{} {{ ", name)?;
                for (i, (field, op)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field, op)?;
                }
                write!(f, " }}")
            }
            Rvalue::Enum { type_name, variant, fields } => {
                write!(f, "{}::{}", type_name, variant)?;
                if !fields.is_empty() {
                    write!(f, "(")?;
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", field)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Rvalue::Discriminant(local) => write!(f, "discriminant({})", local),
            Rvalue::EnumField(local, idx) => write!(f, "{}.{}", local, idx),
            Rvalue::Field(op, field) => write!(f, "{}.{}", op, field),
            Rvalue::TupleField(op, idx) => write!(f, "{}.{}", op, idx),
            Rvalue::Index(base, idx) => write!(f, "{}[{}]", base, idx),
            Rvalue::Cast(op, ty) => write!(f, "{} as {}", op, ty),
            Rvalue::Closure { func_name, captures } => {
                write!(f, "closure {}(", func_name)?;
                for (i, cap) in captures.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", cap)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Constant(c) => write!(f, "{}", c),
            Operand::Local(l) => write!(f, "{}", l),
            Operand::Copy(l) => write!(f, "copy {}", l),
            Operand::Move(l) => write!(f, "move {}", l),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Unit => write!(f, "()"),
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::Int(n) => write!(f, "{}", n),
            Constant::Float(n) => write!(f, "{}", n),
            Constant::Char(c) => write!(f, "'{}'", c),
            Constant::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Return(None) => write!(f, "return"),
            Terminator::Return(Some(op)) => write!(f, "return {}", op),
            Terminator::Goto(block) => write!(f, "goto {}", block),
            Terminator::If {
                cond,
                then_block,
                else_block,
            } => {
                write!(f, "if {} then {} else {}", cond, then_block, else_block)
            }
            Terminator::Switch {
                operand,
                targets,
                default,
            } => {
                write!(f, "switch {} [", operand)?;
                for (val, block) in targets {
                    write!(f, " {} => {},", val, block)?;
                }
                write!(f, " _ => {} ]", default)
            }
            Terminator::Call {
                func,
                args,
                dest,
                next,
            } => {
                if let Some(d) = dest {
                    write!(f, "{} = ", d)?;
                }
                write!(f, "call {}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") -> {}", next)
            }
            Terminator::CallIndirect {
                callee,
                args,
                dest,
                next,
            } => {
                if let Some(d) = dest {
                    write!(f, "{} = ", d)?;
                }
                write!(f, "call_indirect {}(", callee)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") -> {}", next)
            }
            Terminator::Spawn { expr, dest, next } => {
                if let Some(d) = dest {
                    write!(f, "{} = ", d)?;
                }
                write!(f, "spawn {} -> {}", expr, next)
            }
            Terminator::Await { task, dest, next } => {
                if let Some(d) = dest {
                    write!(f, "{} = ", d)?;
                }
                write!(f, "await {} -> {}", task, next)
            }
            Terminator::Unreachable => write!(f, "unreachable"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let mut func = Function::new("add".to_string(), vec![], Ty::Int);

        // Add locals for parameters and return
        let a = func.add_local(Ty::Int, Some("a".to_string()));
        let b = func.add_local(Ty::Int, Some("b".to_string()));
        let result = func.add_local(Ty::Int, Some("result".to_string()));

        // Create entry block
        let entry = func.add_block();

        // Add statements
        func.block_mut(entry).push(Statement {
            kind: StatementKind::Assign(
                result,
                Rvalue::BinaryOp(BinOp::Add, Operand::Local(a), Operand::Local(b)),
            ),
        });

        // Terminate with return
        func.block_mut(entry)
            .terminate(Terminator::Return(Some(Operand::Local(result))));

        // Verify structure
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(func.locals.len(), 3);
    }

    #[test]
    fn test_if_branch() {
        let mut func = Function::new("abs".to_string(), vec![], Ty::Int);

        let x = func.add_local(Ty::Int, Some("x".to_string()));
        let result = func.add_local(Ty::Int, None);

        let entry = func.add_block();
        let then_block = func.add_block();
        let else_block = func.add_block();
        let exit = func.add_block();

        // Entry: branch on x < 0
        let cond = func.add_local(Ty::Bool, None);
        func.block_mut(entry).push(Statement {
            kind: StatementKind::Assign(
                cond,
                Rvalue::BinaryOp(
                    BinOp::Lt,
                    Operand::Local(x),
                    Operand::Constant(Constant::Int(0)),
                ),
            ),
        });
        func.block_mut(entry).terminate(Terminator::If {
            cond: Operand::Local(cond),
            then_block,
            else_block,
        });

        // Then: result = -x
        func.block_mut(then_block).push(Statement {
            kind: StatementKind::Assign(result, Rvalue::UnaryOp(UnOp::Neg, Operand::Local(x))),
        });
        func.block_mut(then_block).terminate(Terminator::Goto(exit));

        // Else: result = x
        func.block_mut(else_block).push(Statement {
            kind: StatementKind::Assign(result, Rvalue::Use(Operand::Local(x))),
        });
        func.block_mut(else_block)
            .terminate(Terminator::Goto(exit));

        // Exit: return result
        func.block_mut(exit)
            .terminate(Terminator::Return(Some(Operand::Local(result))));

        assert_eq!(func.blocks.len(), 4);
    }
}
