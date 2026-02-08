//! Abstract Syntax Tree definitions for FORMA.
//!
//! This module defines all AST nodes that represent parsed FORMA programs.

use crate::lexer::Span;

/// A complete source file.
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub items: Vec<Item>,
    pub span: Span,
}

/// Top-level items in a source file.
#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub attrs: Vec<Attribute>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function(Function),
    Struct(Struct),
    Enum(Enum),
    Trait(Trait),
    Impl(Impl),
    TypeAlias(TypeAlias),
    Use(Use),
    Module(Module),
    Const(Const),
}

/// An attribute like `@test` or `@derive(Debug, Clone)`.
#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Ident,
    pub args: Vec<AttrArg>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct AttrArg {
    pub name: Ident,
    pub value: Option<Literal>,
    /// Expression argument (used by @pre/@post contracts)
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

/// A contract (precondition or postcondition) for a function.
#[derive(Debug, Clone)]
pub struct Contract {
    /// The condition expression that must be true
    pub condition: Box<Expr>,
    /// Optional message for error reporting
    pub message: Option<String>,
    pub span: Span,
}

/// A function definition.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub generics: Option<Generics>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Option<FnBody>,
    pub is_async: bool,
    pub is_unsafe: bool,
    pub visibility: Visibility,
    /// Preconditions (@pre) - checked at function entry
    pub preconditions: Vec<Contract>,
    /// Postconditions (@post) - checked at function exit
    pub postconditions: Vec<Contract>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FnBody {
    /// Single expression: `f foo -> Int = 42`
    Expr(Box<Expr>),
    /// Block body
    Block(Block),
}

/// How a parameter is passed: by value (owned), by shared reference, or by mutable reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PassMode {
    /// Pass by value (default)
    #[default]
    Owned,
    /// Pass by shared reference (`ref`)
    Ref,
    /// Pass by mutable reference (`ref mut`)
    RefMut,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Ident,
    pub ty: Type,
    pub default: Option<Expr>,
    pub pass_mode: PassMode,
    pub span: Span,
}

/// A struct definition.
#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Ident,
    pub generics: Option<Generics>,
    pub kind: StructKind,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StructKind {
    /// Named fields: `s Point { x: Int, y: Int }`
    Named(Vec<Field>),
    /// Tuple struct: `s Point(Int, Int)`
    Tuple(Vec<Type>),
    /// Unit struct: `s Marker`
    Unit,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Ident,
    pub ty: Type,
    pub default: Option<Expr>,
    pub visibility: Visibility,
    pub span: Span,
}

/// An enum definition.
#[derive(Debug, Clone)]
pub struct Enum {
    pub name: Ident,
    pub generics: Option<Generics>,
    pub variants: Vec<Variant>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Ident,
    pub kind: VariantKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum VariantKind {
    /// Unit variant: `None`
    Unit,
    /// Tuple variant: `Some(T)`
    Tuple(Vec<Type>),
    /// Named fields: `Rect { w: Int, h: Int }`
    Named(Vec<Field>),
}

/// A trait definition.
#[derive(Debug, Clone)]
pub struct Trait {
    pub name: Ident,
    pub generics: Option<Generics>,
    pub supertraits: Vec<TypeBound>,
    pub items: Vec<TraitItem>,
    pub visibility: Visibility,
    pub is_unsafe: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TraitItem {
    Function(Function),
    TypeAlias(TypeAlias),
}

/// An impl block.
#[derive(Debug, Clone)]
pub struct Impl {
    pub generics: Option<Generics>,
    pub trait_: Option<Type>,
    pub self_type: Type,
    pub where_clause: Option<WhereClause>,
    pub items: Vec<ImplItem>,
    pub is_unsafe: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ImplItem {
    Function(Function),
    TypeAlias(TypeAlias),
}

/// A type alias: `type Meters = Int`
#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: Ident,
    pub generics: Option<Generics>,
    pub ty: Option<Type>,
    pub span: Span,
}

/// A use/import statement.
#[derive(Debug, Clone)]
pub struct Use {
    pub tree: UseTree,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UseTree {
    /// `us std.io`
    Path(Vec<Ident>, Option<Box<UseTree>>),
    /// `us std.io -> io_module`
    Rename(Vec<Ident>, Ident),
    /// `us std.{io, fs}`
    Group(Vec<UseTree>),
    /// `us std.*`
    Glob,
}

/// A module definition.
#[derive(Debug, Clone)]
pub struct Module {
    pub name: Ident,
    pub items: Option<Vec<Item>>,
    pub visibility: Visibility,
    pub span: Span,
}

/// A constant definition: `PI :: 3.14159`
#[derive(Debug, Clone)]
pub struct Const {
    pub name: Ident,
    pub ty: Option<Type>,
    pub value: Expr,
    pub visibility: Visibility,
    pub span: Span,
}

// ============================================================================
// Generics
// ============================================================================

#[derive(Debug, Clone)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum GenericParam {
    Type(TypeParam),
    Const(ConstParam),
}

#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: Ident,
    pub bounds: Vec<TypeBound>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ConstParam {
    pub name: Ident,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypeBound {
    pub path: TypePath,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WherePredicate {
    pub ty: Type,
    pub bounds: Vec<TypeBound>,
    pub span: Span,
}

// ============================================================================
// Types
// ============================================================================

/// Linearity qualifier for types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Linearity {
    /// Regular type (can be copied and dropped freely)
    #[default]
    Regular,
    /// Linear type (must be used exactly once)
    Linear,
    /// Affine type (must be used at most once)
    Affine,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
    /// Linearity qualifier (linear, affine, or regular)
    pub linearity: Linearity,
}

impl Type {
    /// Create a new Type with regular (default) linearity.
    pub fn new(kind: TypeKind, span: Span) -> Self {
        Self {
            kind,
            span,
            linearity: Linearity::Regular,
        }
    }

    /// Create a new Type with the given linearity.
    pub fn with_linearity(kind: TypeKind, span: Span, linearity: Linearity) -> Self {
        Self {
            kind,
            span,
            linearity,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    /// Named type: `Int`, `Vec[T]`
    Path(TypePath),
    /// Tuple type: `(Int, Str)`
    Tuple(Vec<Type>),
    /// List shortcut: `[T]`
    List(Box<Type>),
    /// Map shortcut: `{K: V}`
    Map(Box<Type>, Box<Type>),
    /// Set shortcut: `{T}`
    Set(Box<Type>),
    /// Array: `[T; N]`
    Array(Box<Type>, Box<Expr>),
    /// Option shortcut: `T?`
    Option(Box<Type>),
    /// Result shortcut: `T!` or `T!E`
    Result(Box<Type>, Option<Box<Type>>),
    /// Function type: `(A, B) -> C`
    Fn(Vec<Type>, Box<Type>),
    /// Reference: `&T` or `&mut T`
    Ref(Box<Type>, bool),
    /// Pointer: `*T` or `*mut T`
    Ptr(Box<Type>, bool),
    /// Inferred type: `_`
    Infer,
    /// Never type: `!`
    Never,
}

#[derive(Debug, Clone)]
pub struct TypePath {
    pub segments: Vec<TypePathSegment>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypePathSegment {
    pub name: Ident,
    pub args: Option<GenericArgs>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericArgs {
    pub args: Vec<GenericArg>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum GenericArg {
    Type(Type),
    Expr(Expr),
}

// ============================================================================
// Expressions
// ============================================================================

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    /// Create a new expression.
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Literal: `42`, `"hello"`, `true`
    Literal(Literal),
    /// Identifier: `foo`
    Ident(Ident),
    /// Path: `std.io.read`
    Path(Path),
    /// Binary operation: `a + b`
    Binary(Box<Expr>, BinOp, Box<Expr>),
    /// Unary operation: `-x`, `!x`, `&x`
    Unary(UnaryOp, Box<Expr>),
    /// Function call: `foo(a, b)`
    Call(Box<Expr>, Vec<Arg>),
    /// Method call: `obj.method(args)`
    MethodCall(Box<Expr>, Ident, Vec<Arg>),
    /// Field access: `obj.field`
    Field(Box<Expr>, Ident),
    /// Tuple field access: `tuple.0`
    TupleField(Box<Expr>, usize),
    /// Index: `arr[i]`
    Index(Box<Expr>, Box<Expr>),
    /// Tuple: `(a, b, c)`
    Tuple(Vec<Expr>),
    /// Array/List: `[1, 2, 3]`
    Array(Vec<Expr>),
    /// Array with repeat: `[0; 10]`
    ArrayRepeat(Box<Expr>, Box<Expr>),
    /// Map/Set literal: `{a: 1, b: 2}` or `{1, 2, 3}`
    MapOrSet(Vec<MapEntry>),
    /// Struct instantiation: `Point(x: 1, y: 2)` or `Point { x: 1, y: 2 }`
    Struct(TypePath, Vec<FieldInit>, Option<Box<Expr>>),
    /// If expression
    If(Box<IfExpr>),
    /// Match expression
    Match(Box<Expr>, Vec<MatchArm>),
    /// For loop with optional label: `'label: fo x in iter`
    For(Option<Ident>, Pattern, Box<Expr>, Block),
    /// While loop with optional label: `'label: wh cond`
    While(Option<Ident>, Box<Expr>, Block),
    /// While-let: `wh Some(x) = iter.next`
    WhileLet(Option<Ident>, Pattern, Box<Expr>, Block),
    /// Infinite loop with optional label: `'label: lp`
    Loop(Option<Ident>, Block),
    /// Block expression
    Block(Block),
    /// Closure: `|x, y| x + y`
    Closure(Closure),
    /// Field shorthand closure: `.name`
    FieldShorthand(Ident),
    /// Operator shorthand: `(+ 10)`, `(> 0)`
    OpShorthand(BinOp, Box<Expr>, bool),
    /// Return: `ret x`
    Return(Option<Box<Expr>>),
    /// Break: `br` or `br value`
    Break(Option<Ident>, Option<Box<Expr>>),
    /// Continue: `ct`
    Continue(Option<Ident>),
    /// Async block: `as { ... }`
    Async(Block),
    /// Await: `aw expr`
    Await(Box<Expr>),
    /// Spawn: `sp expr` - spawns an async task, returns Task[T]
    Spawn(Box<Expr>),
    /// Error propagation: `expr?`
    Try(Box<Expr>),
    /// Null coalesce: `expr ?? default`
    Coalesce(Box<Expr>, Box<Expr>),
    /// Range: `a..b` or `a..=b`
    Range(Option<Box<Expr>>, Option<Box<Expr>>, bool),
    /// Pipeline: `a | b`
    Pipeline(Box<Expr>, Box<Expr>),
    /// Grouped expression: `(expr)`
    Paren(Box<Expr>),
    /// Assignment: `x = 1` or `x := 1`
    Assign(Box<Expr>, Box<Expr>, bool),
    /// Compound assignment: `x += 1`
    AssignOp(Box<Expr>, BinOp, Box<Expr>),
    /// Cast: `x as T` (not in FORMA v2 grammar, placeholder)
    Cast(Box<Expr>, Type),
    /// Unsafe block
    Unsafe(Block),
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Expr,
    pub then_branch: IfBranch,
    pub else_branch: Option<ElseBranch>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum IfBranch {
    /// Inline: `if cond then expr`
    Expr(Box<Expr>),
    /// Block: `if cond\n    body`
    Block(Block),
}

#[derive(Debug, Clone)]
pub enum ElseBranch {
    /// `else expr`
    Expr(Box<Expr>),
    /// `else { block }`
    Block(Block),
    /// `else if ...`
    ElseIf(Box<IfExpr>),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Vec<ClosureParam>,
    pub return_type: Option<Type>,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub name: Ident,
    pub ty: Option<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MapEntry {
    pub key: Expr,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldInit {
    pub name: Ident,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Arg {
    pub name: Option<Ident>,
    pub value: Expr,
    pub pass_mode: PassMode,
    pub span: Span,
}

// ============================================================================
// Operators
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,    // -
    Not,    // !
    Ref,    // &
    RefMut, // &mut
    Deref,  // *
}

// ============================================================================
// Patterns
// ============================================================================

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    /// Wildcard: `_`
    Wildcard,
    /// Identifier binding: `x` or `mut x`
    Ident(Ident, bool, Option<Box<Pattern>>),
    /// Literal: `42`, `"hello"`
    Literal(Literal),
    /// Tuple: `(a, b, c)`
    Tuple(Vec<Pattern>),
    /// List/Array: `[a, b, ..rest]`
    List(Vec<Pattern>, Option<Box<Pattern>>),
    /// Struct pattern: `Point(x, y)` or `Point { x, y }`
    Struct(TypePath, Vec<PatternField>, bool),
    /// Or pattern: `A | B | C`
    Or(Vec<Pattern>),
    /// Range: `0..10` or `'a'..='z'`
    Range(Option<Box<Pattern>>, Option<Box<Pattern>>, bool),
    /// Reference: `&x` or `&mut x`
    Ref(Box<Pattern>, bool),
    /// Rest: `..`
    Rest,
}

#[derive(Debug, Clone)]
pub struct PatternField {
    pub name: Ident,
    pub pattern: Option<Pattern>,
    pub span: Span,
}

// ============================================================================
// Statements
// ============================================================================

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// Item declaration (function, struct, etc.)
    Item(Item),
    /// Variable binding: `x = 1` or `x := 1`
    Let(LetStmt),
    /// Expression statement
    Expr(Expr),
    /// Empty statement (just a newline or semicolon)
    Empty,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub pattern: Pattern,
    pub ty: Option<Type>,
    pub init: Expr,
    pub mutable: bool,
    pub span: Span,
}

// ============================================================================
// Common Types
// ============================================================================

/// An identifier.
#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

/// A path like `std.io.read`.
#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<Ident>,
    pub span: Span,
}

/// Visibility modifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Private,
    Public,
    Crate,
}

/// A literal value.
#[derive(Debug, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int(i128),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    None,
}
