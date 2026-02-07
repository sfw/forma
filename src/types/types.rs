//! Internal type representations for the FORMA type system.
//!
//! This module defines the types used by the compiler for type checking
//! and inference. These are separate from the AST types and represent
//! the semantic meaning of types.

use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicU32, Ordering};

/// Counter for generating unique type variable IDs.
static TYPE_VAR_COUNTER: AtomicU32 = AtomicU32::new(0);

/// Generate a fresh type variable ID.
pub fn fresh_type_var_id() -> u32 {
    TYPE_VAR_COUNTER.fetch_add(1, Ordering::SeqCst)
}

/// Reset the type variable counter (useful for tests).
pub fn reset_type_var_counter() {
    TYPE_VAR_COUNTER.store(0, Ordering::SeqCst);
}

/// A type in the FORMA type system.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    /// Type variable (for inference)
    Var(TypeVar),

    /// Primitive types
    Int,
    I8,
    I16,
    I32,
    I64,
    I128,
    UInt,
    U8,
    U16,
    U32,
    U64,
    U128,
    // Pointer-sized integers
    Isize,
    Usize,
    Float,
    F32,
    F64,
    Bool,
    Char,
    Str,
    /// JSON value type (dynamic)
    Json,

    /// Task type for spawned async computations
    Task(Box<Ty>),

    /// Future type for async function returns
    Future(Box<Ty>),

    /// Channel sender type
    Sender(Box<Ty>),

    /// Channel receiver type
    Receiver(Box<Ty>),

    /// Mutex type for synchronization
    Mutex(Box<Ty>),

    /// Mutex guard type (holds lock)
    MutexGuard(Box<Ty>),

    /// TCP stream for network connections
    TcpStream,

    /// TCP listener for accepting connections
    TcpListener,

    /// UDP socket for datagram communication
    UdpSocket,

    /// TLS stream for encrypted connections
    TlsStream,

    /// SQLite database connection
    Database,

    /// SQLite prepared statement
    Statement,

    /// Database row (columns as a tuple)
    DbRow,

    // === C FFI Types ===
    /// C int (platform-specific, typically 32-bit)
    CInt,
    /// C unsigned int
    CUInt,
    /// C long (platform-specific)
    CLong,
    /// C unsigned long
    CULong,
    /// C float (32-bit)
    CFloat,
    /// C double (64-bit)
    CDouble,
    /// C size_t
    CSize,
    /// Raw pointer to T
    RawPtr(Box<Ty>),
    /// Void pointer (*Void)
    CVoid,

    /// Unit type ()
    Unit,

    /// Never type !
    Never,

    /// Tuple type (T1, T2, ...)
    Tuple(Vec<Ty>),

    /// Array type [T; N]
    Array(Box<Ty>, usize),

    /// List type [T]
    List(Box<Ty>),

    /// Map type {K: V}
    Map(Box<Ty>, Box<Ty>),

    /// Set type {T}
    Set(Box<Ty>),

    /// Option type T?
    Option(Box<Ty>),

    /// Result type T!E
    Result(Box<Ty>, Box<Ty>),

    /// Function type (args) -> ret
    Fn(Vec<Ty>, Box<Ty>),

    /// Reference type &T or &mut T
    Ref(Box<Ty>, Mutability),

    /// Pointer type *T or *mut T
    Ptr(Box<Ty>, Mutability),

    /// Named type (struct, enum, trait object)
    Named(TypeId, Vec<Ty>),

    /// Type alias
    Alias(TypeId, Vec<Ty>),

    /// Associated type (Self.Item)
    Associated(Box<Ty>, String),

    /// Error type (for error recovery)
    Error,
}

/// Mutability marker.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Immutable,
    Mutable,
}

/// Linearity kind for the type system.
///
/// Controls how values of a type may be used:
/// - `Regular`: No restrictions (can be copied and dropped freely)
/// - `Affine`: Can be used at most once (may be dropped without use)
/// - `Linear`: Must be used exactly once (cannot be dropped or duplicated)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum LinearityKind {
    /// No linearity restrictions
    #[default]
    Regular,
    /// Must be used at most once (can be dropped)
    Affine,
    /// Must be used exactly once (cannot be dropped)
    Linear,
}

impl LinearityKind {
    /// Returns true if values of this linearity can be implicitly copied.
    pub fn can_copy(&self) -> bool {
        matches!(self, LinearityKind::Regular)
    }

    /// Returns true if values of this linearity can be dropped without use.
    pub fn can_drop(&self) -> bool {
        matches!(self, LinearityKind::Regular | LinearityKind::Affine)
    }

    /// Combine two linearity kinds for composite types.
    /// The result is the most restrictive of the two.
    pub fn combine(self, other: LinearityKind) -> LinearityKind {
        match (self, other) {
            (LinearityKind::Linear, _) | (_, LinearityKind::Linear) => LinearityKind::Linear,
            (LinearityKind::Affine, _) | (_, LinearityKind::Affine) => LinearityKind::Affine,
            _ => LinearityKind::Regular,
        }
    }
}

impl std::fmt::Display for LinearityKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinearityKind::Regular => write!(f, ""),
            LinearityKind::Affine => write!(f, "affine "),
            LinearityKind::Linear => write!(f, "linear "),
        }
    }
}

/// Capability types for controlling access to system resources.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Capability {
    /// File system access
    File(FileCapability),
    /// Network access
    Network(NetworkCapability),
    /// Environment variable access
    Env(EnvCapability),
    /// Spawn processes/threads
    Spawn,
    /// FFI (foreign function interface) access
    Ffi,
}

impl std::fmt::Display for Capability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Capability::File(fc) => write!(f, "file({})", fc),
            Capability::Network(nc) => write!(f, "network({})", nc),
            Capability::Env(ec) => write!(f, "env({})", ec),
            Capability::Spawn => write!(f, "spawn"),
            Capability::Ffi => write!(f, "ffi"),
        }
    }
}

/// File system capability with path restrictions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileCapability {
    /// Allowed path prefix (e.g., "/tmp", "." for current dir)
    pub path: String,
    /// Read permission
    pub read: bool,
    /// Write permission
    pub write: bool,
}

impl std::fmt::Display for FileCapability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let perms = match (self.read, self.write) {
            (true, true) => "rw",
            (true, false) => "r",
            (false, true) => "w",
            (false, false) => "none",
        };
        write!(f, "{}:{}", self.path, perms)
    }
}

/// Network capability with host/port restrictions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NetworkCapability {
    /// Allowed host pattern (e.g., "*.example.com", "*" for any)
    pub host: String,
    /// Allowed port (0 means any port)
    pub port: u16,
}

impl std::fmt::Display for NetworkCapability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.port == 0 {
            write!(f, "{}", self.host)
        } else {
            write!(f, "{}:{}", self.host, self.port)
        }
    }
}

/// Environment variable access capability.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnvCapability {
    /// Allowed env var pattern ("*" for any, or specific name)
    pub pattern: String,
}

impl std::fmt::Display for EnvCapability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pattern)
    }
}

/// A type variable for inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub id: u32,
}

impl TypeVar {
    pub fn fresh() -> Self {
        Self {
            id: fresh_type_var_id(),
        }
    }
}

/// Unique identifier for a named type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId {
    pub name: String,
    pub module: Option<String>,
}

impl TypeId {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            module: None,
        }
    }

    pub fn with_module(name: impl Into<String>, module: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            module: Some(module.into()),
        }
    }
}

impl Ty {
    /// Create a fresh type variable.
    pub fn fresh_var() -> Self {
        Ty::Var(TypeVar::fresh())
    }

    /// Check if this type contains any type variables.
    pub fn has_vars(&self) -> bool {
        match self {
            Ty::Var(_) => true,
            Ty::Tuple(tys) => tys.iter().any(|t| t.has_vars()),
            Ty::Array(ty, _) => ty.has_vars(),
            Ty::List(ty) => ty.has_vars(),
            Ty::Map(k, v) => k.has_vars() || v.has_vars(),
            Ty::Set(ty) => ty.has_vars(),
            Ty::Task(ty) => ty.has_vars(),
            Ty::Future(ty) => ty.has_vars(),
            Ty::Sender(ty) => ty.has_vars(),
            Ty::Receiver(ty) => ty.has_vars(),
            Ty::Mutex(ty) => ty.has_vars(),
            Ty::MutexGuard(ty) => ty.has_vars(),
            Ty::RawPtr(ty) => ty.has_vars(),
            Ty::Option(ty) => ty.has_vars(),
            Ty::Result(ok, err) => ok.has_vars() || err.has_vars(),
            Ty::Fn(params, ret) => params.iter().any(|t| t.has_vars()) || ret.has_vars(),
            Ty::Ref(ty, _) => ty.has_vars(),
            Ty::Ptr(ty, _) => ty.has_vars(),
            Ty::Named(_, args) => args.iter().any(|t| t.has_vars()),
            Ty::Alias(_, args) => args.iter().any(|t| t.has_vars()),
            Ty::Associated(ty, _) => ty.has_vars(),
            _ => false,
        }
    }

    /// Get all free type variables in this type.
    pub fn free_vars(&self) -> Vec<TypeVar> {
        let mut vars = Vec::new();
        self.collect_vars(&mut vars);
        vars
    }

    fn collect_vars(&self, vars: &mut Vec<TypeVar>) {
        match self {
            Ty::Var(v) => {
                if !vars.contains(v) {
                    vars.push(*v);
                }
            }
            Ty::Tuple(tys) => {
                for ty in tys {
                    ty.collect_vars(vars);
                }
            }
            Ty::Array(ty, _) => ty.collect_vars(vars),
            Ty::List(ty) => ty.collect_vars(vars),
            Ty::Map(k, v) => {
                k.collect_vars(vars);
                v.collect_vars(vars);
            }
            Ty::Set(ty) => ty.collect_vars(vars),
            Ty::Task(ty) => ty.collect_vars(vars),
            Ty::Future(ty) => ty.collect_vars(vars),
            Ty::Sender(ty) => ty.collect_vars(vars),
            Ty::Receiver(ty) => ty.collect_vars(vars),
            Ty::Mutex(ty) => ty.collect_vars(vars),
            Ty::MutexGuard(ty) => ty.collect_vars(vars),
            Ty::RawPtr(ty) => ty.collect_vars(vars),
            Ty::Option(ty) => ty.collect_vars(vars),
            Ty::Result(ok, err) => {
                ok.collect_vars(vars);
                err.collect_vars(vars);
            }
            Ty::Fn(params, ret) => {
                for ty in params {
                    ty.collect_vars(vars);
                }
                ret.collect_vars(vars);
            }
            Ty::Ref(ty, _) => ty.collect_vars(vars),
            Ty::Ptr(ty, _) => ty.collect_vars(vars),
            Ty::Named(_, args) => {
                for ty in args {
                    ty.collect_vars(vars);
                }
            }
            Ty::Alias(_, args) => {
                for ty in args {
                    ty.collect_vars(vars);
                }
            }
            Ty::Associated(ty, _) => ty.collect_vars(vars),
            _ => {}
        }
    }

    /// Apply a substitution to this type.
    pub fn apply(&self, subst: &Substitution) -> Ty {
        match self {
            Ty::Var(v) => subst.get(*v).cloned().unwrap_or_else(|| self.clone()),
            Ty::Tuple(tys) => Ty::Tuple(tys.iter().map(|t| t.apply(subst)).collect()),
            Ty::Array(ty, n) => Ty::Array(Box::new(ty.apply(subst)), *n),
            Ty::List(ty) => Ty::List(Box::new(ty.apply(subst))),
            Ty::Map(k, v) => Ty::Map(Box::new(k.apply(subst)), Box::new(v.apply(subst))),
            Ty::Set(ty) => Ty::Set(Box::new(ty.apply(subst))),
            Ty::Task(ty) => Ty::Task(Box::new(ty.apply(subst))),
            Ty::Future(ty) => Ty::Future(Box::new(ty.apply(subst))),
            Ty::Sender(ty) => Ty::Sender(Box::new(ty.apply(subst))),
            Ty::Receiver(ty) => Ty::Receiver(Box::new(ty.apply(subst))),
            Ty::Mutex(ty) => Ty::Mutex(Box::new(ty.apply(subst))),
            Ty::MutexGuard(ty) => Ty::MutexGuard(Box::new(ty.apply(subst))),
            Ty::RawPtr(ty) => Ty::RawPtr(Box::new(ty.apply(subst))),
            Ty::Option(ty) => Ty::Option(Box::new(ty.apply(subst))),
            Ty::Result(ok, err) => {
                Ty::Result(Box::new(ok.apply(subst)), Box::new(err.apply(subst)))
            }
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|t| t.apply(subst)).collect(),
                Box::new(ret.apply(subst)),
            ),
            Ty::Ref(ty, m) => Ty::Ref(Box::new(ty.apply(subst)), *m),
            Ty::Ptr(ty, m) => Ty::Ptr(Box::new(ty.apply(subst)), *m),
            Ty::Named(id, args) => {
                Ty::Named(id.clone(), args.iter().map(|t| t.apply(subst)).collect())
            }
            Ty::Alias(id, args) => {
                Ty::Alias(id.clone(), args.iter().map(|t| t.apply(subst)).collect())
            }
            Ty::Associated(ty, name) => {
                Ty::Associated(Box::new(ty.apply(subst)), name.clone())
            }
            _ => self.clone(),
        }
    }

    /// Check if this type is a numeric type.
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Ty::Int
                | Ty::I8
                | Ty::I16
                | Ty::I32
                | Ty::I64
                | Ty::I128
                | Ty::UInt
                | Ty::U8
                | Ty::U16
                | Ty::U32
                | Ty::U64
                | Ty::U128
                | Ty::Isize
                | Ty::Usize
                | Ty::Float
                | Ty::F32
                | Ty::F64
        )
    }

    /// Check if this type is an integer type.
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Ty::Int
                | Ty::I8
                | Ty::I16
                | Ty::I32
                | Ty::I64
                | Ty::I128
                | Ty::UInt
                | Ty::U8
                | Ty::U16
                | Ty::U32
                | Ty::U64
                | Ty::U128
                | Ty::Isize
                | Ty::Usize
        )
    }

    /// Check if this type is a floating point type.
    pub fn is_float(&self) -> bool {
        matches!(self, Ty::Float | Ty::F32 | Ty::F64)
    }

    /// Check if this type is Copy (trivially copyable).
    pub fn is_copy(&self) -> bool {
        match self {
            Ty::Int
            | Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::I128
            | Ty::UInt
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::U128
            | Ty::Isize
            | Ty::Usize
            | Ty::Float
            | Ty::F32
            | Ty::F64
            | Ty::Bool
            | Ty::Char
            | Ty::Unit
            | Ty::Never => true,
            Ty::Tuple(tys) => tys.iter().all(|t| t.is_copy()),
            Ty::Array(ty, _) => ty.is_copy(),
            Ty::Ref(_, _) => true,
            Ty::Ptr(_, _) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Var(v) => write!(f, "?{}", v.id),
            Ty::Int => write!(f, "Int"),
            Ty::I8 => write!(f, "i8"),
            Ty::I16 => write!(f, "i16"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "i64"),
            Ty::I128 => write!(f, "i128"),
            Ty::UInt => write!(f, "UInt"),
            Ty::U8 => write!(f, "u8"),
            Ty::U16 => write!(f, "u16"),
            Ty::U32 => write!(f, "u32"),
            Ty::U64 => write!(f, "u64"),
            Ty::U128 => write!(f, "u128"),
            Ty::Isize => write!(f, "isize"),
            Ty::Usize => write!(f, "usize"),
            Ty::Float => write!(f, "Float"),
            Ty::F32 => write!(f, "f32"),
            Ty::F64 => write!(f, "f64"),
            Ty::Bool => write!(f, "Bool"),
            Ty::Char => write!(f, "Char"),
            Ty::Str => write!(f, "Str"),
            Ty::Unit => write!(f, "()"),
            Ty::Never => write!(f, "!"),
            Ty::Tuple(tys) => {
                write!(f, "(")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            Ty::Array(ty, n) => write!(f, "[{};{}]", ty, n),
            Ty::List(ty) => write!(f, "[{}]", ty),
            Ty::Map(k, v) => write!(f, "{{{}:{}}}", k, v),
            Ty::Set(ty) => write!(f, "{{{}}}", ty),
            Ty::Task(ty) => write!(f, "Task[{}]", ty),
            Ty::Future(ty) => write!(f, "Future[{}]", ty),
            Ty::Sender(ty) => write!(f, "Sender[{}]", ty),
            Ty::Receiver(ty) => write!(f, "Receiver[{}]", ty),
            Ty::Mutex(ty) => write!(f, "Mutex[{}]", ty),
            Ty::MutexGuard(ty) => write!(f, "MutexGuard[{}]", ty),
            Ty::TcpStream => write!(f, "TcpStream"),
            Ty::TcpListener => write!(f, "TcpListener"),
            Ty::UdpSocket => write!(f, "UdpSocket"),
            Ty::TlsStream => write!(f, "TlsStream"),
            Ty::Database => write!(f, "Database"),
            Ty::Statement => write!(f, "Statement"),
            Ty::DbRow => write!(f, "Row"),
            Ty::CInt => write!(f, "CInt"),
            Ty::CUInt => write!(f, "CUInt"),
            Ty::CLong => write!(f, "CLong"),
            Ty::CULong => write!(f, "CULong"),
            Ty::CFloat => write!(f, "CFloat"),
            Ty::CDouble => write!(f, "CDouble"),
            Ty::CSize => write!(f, "CSize"),
            Ty::RawPtr(ty) => write!(f, "*{}", ty),
            Ty::CVoid => write!(f, "*Void"),
            Ty::Option(ty) => write!(f, "{}?", ty),
            Ty::Result(ok, err) => write!(f, "{}!{}", ok, err),
            Ty::Fn(params, ret) => {
                write!(f, "(")?;
                for (i, ty) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ") -> {}", ret)
            }
            Ty::Ref(ty, Mutability::Immutable) => write!(f, "&{}", ty),
            Ty::Ref(ty, Mutability::Mutable) => write!(f, "&mut {}", ty),
            Ty::Ptr(ty, Mutability::Immutable) => write!(f, "*{}", ty),
            Ty::Ptr(ty, Mutability::Mutable) => write!(f, "*mut {}", ty),
            Ty::Named(id, args) => {
                write!(f, "{}", id.name)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (i, ty) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", ty)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Ty::Alias(id, args) => {
                write!(f, "{}", id.name)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (i, ty) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", ty)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Ty::Associated(ty, name) => write!(f, "{}.{}", ty, name),
            Ty::Json => write!(f, "Json"),
            Ty::Error => write!(f, "<error>"),
        }
    }
}

/// A substitution mapping type variables to types.
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    map: HashMap<u32, Ty>,
}

impl Substitution {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, var: TypeVar, ty: Ty) {
        self.map.insert(var.id, ty);
    }

    pub fn get(&self, var: TypeVar) -> Option<&Ty> {
        self.map.get(&var.id)
    }

    pub fn contains(&self, var: TypeVar) -> bool {
        self.map.contains_key(&var.id)
    }

    /// Compose two substitutions: self ∘ other
    /// (apply other first, then self)
    pub fn compose(&self, other: &Substitution) -> Substitution {
        let mut result = Substitution::new();

        // Apply self to all types in other
        for (&var, ty) in &other.map {
            result.map.insert(var, ty.apply(self));
        }

        // Add all bindings from self that aren't in other
        for (&var, ty) in &self.map {
            result.map.entry(var).or_insert_with(|| ty.clone());
        }

        result
    }

    /// Get the number of bindings.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

/// A type scheme (polymorphic type).
/// Represents ∀a1, a2, ... an. T
#[derive(Debug, Clone)]
pub struct TypeScheme {
    /// Quantified type variables
    pub vars: Vec<TypeVar>,
    /// The underlying type
    pub ty: Ty,
}

impl TypeScheme {
    /// Create a monomorphic type scheme (no quantified variables).
    pub fn mono(ty: Ty) -> Self {
        Self { vars: vec![], ty }
    }

    /// Instantiate the type scheme with fresh type variables.
    pub fn instantiate(&self) -> Ty {
        if self.vars.is_empty() {
            return self.ty.clone();
        }

        let mut subst = Substitution::new();
        for var in &self.vars {
            subst.insert(*var, Ty::fresh_var());
        }
        self.ty.apply(&subst)
    }

    /// Generalize a type over the given free variables.
    pub fn generalize(ty: Ty, env_vars: &[TypeVar]) -> Self {
        let free = ty.free_vars();
        let vars: Vec<_> = free
            .into_iter()
            .filter(|v| !env_vars.contains(v))
            .collect();
        Self { vars, ty }
    }
}

impl fmt::Display for TypeScheme {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.vars.is_empty() {
            write!(f, "{}", self.ty)
        } else {
            write!(f, "∀")?;
            for (i, var) in self.vars.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "?{}", var.id)?;
            }
            write!(f, ". {}", self.ty)
        }
    }
}

/// A trait bound.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraitBound {
    pub trait_id: TypeId,
    pub args: Vec<Ty>,
}

impl fmt::Display for TraitBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.trait_id.name)?;
        if !self.args.is_empty() {
            write!(f, "[")?;
            for (i, ty) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", ty)?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

/// Information about a struct type.
#[derive(Debug, Clone)]
pub struct StructInfo {
    pub id: TypeId,
    pub type_params: Vec<String>,
    pub fields: Vec<(String, Ty)>,
}

/// Information about an enum type.
#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub id: TypeId,
    pub type_params: Vec<String>,
    pub variants: Vec<VariantInfo>,
}

/// Information about an enum variant.
#[derive(Debug, Clone)]
pub struct VariantInfo {
    pub name: String,
    pub fields: VariantFields,
}

#[derive(Debug, Clone)]
pub enum VariantFields {
    Unit,
    Tuple(Vec<Ty>),
    Named(Vec<(String, Ty)>),
}

/// Information about a trait.
#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub id: TypeId,
    pub type_params: Vec<String>,
    pub supertraits: Vec<TraitBound>,
    pub methods: Vec<MethodInfo>,
    pub associated_types: Vec<String>,
}

/// Information about a method.
#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<Ty>,
    pub return_type: Ty,
    pub has_default: bool,
}

/// Information about a function.
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<(String, Ty)>,
    pub return_type: Ty,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_display() {
        assert_eq!(format!("{}", Ty::Int), "Int");
        assert_eq!(format!("{}", Ty::List(Box::new(Ty::Int))), "[Int]");
        assert_eq!(
            format!("{}", Ty::Option(Box::new(Ty::Str))),
            "Str?"
        );
        assert_eq!(
            format!("{}", Ty::Result(Box::new(Ty::Int), Box::new(Ty::Str))),
            "Int!Str"
        );
    }

    #[test]
    fn test_substitution() {
        reset_type_var_counter();
        let v1 = TypeVar::fresh();
        let v2 = TypeVar::fresh();

        let mut subst = Substitution::new();
        subst.insert(v1, Ty::Int);

        let ty = Ty::Tuple(vec![Ty::Var(v1), Ty::Var(v2)]);
        let result = ty.apply(&subst);

        assert_eq!(
            result,
            Ty::Tuple(vec![Ty::Int, Ty::Var(v2)])
        );
    }

    #[test]
    fn test_type_scheme_instantiate() {
        reset_type_var_counter();
        let v = TypeVar::fresh();
        let scheme = TypeScheme {
            vars: vec![v],
            ty: Ty::List(Box::new(Ty::Var(v))),
        };

        let ty1 = scheme.instantiate();
        let ty2 = scheme.instantiate();

        // Each instantiation should produce different type variables
        assert_ne!(ty1, ty2);
    }

    #[test]
    fn test_is_copy() {
        assert!(Ty::Int.is_copy());
        assert!(Ty::Bool.is_copy());
        assert!(Ty::Tuple(vec![Ty::Int, Ty::Bool]).is_copy());
        assert!(!Ty::Str.is_copy());
        assert!(!Ty::List(Box::new(Ty::Int)).is_copy());
    }
}
