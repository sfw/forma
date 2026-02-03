# Sprints 32-34: Memory Safety Features

**Goal:** Implement foundational memory safety and capability-based security for FORMA v2.0
**Estimated Effort:** 6-8 hours total

---

## Sprint 32: FFI Safety Layer (3 tasks)

### Task 32.1: Create SafePtr Wrapper

**New File:** `src/ffi/safe_ptr.rs`

Create a safe pointer wrapper that tracks bounds and lifetime:

```rust
use std::ptr::NonNull;

/// A pointer wrapper with bounds checking and lifetime tracking.
#[derive(Clone)]
pub struct SafePtr<T> {
    ptr: NonNull<T>,
    len: usize,          // Number of elements
    offset: usize,       // Current offset from base
    generation: u64,     // Allocation generation for use-after-free detection
}

impl<T> SafePtr<T> {
    /// Create a new SafePtr from a slice.
    pub fn from_slice(slice: &mut [T], generation: u64) -> Self {
        SafePtr {
            ptr: NonNull::new(slice.as_mut_ptr()).expect("null pointer"),
            len: slice.len(),
            offset: 0,
            generation,
        }
    }

    /// Get element at index with bounds checking.
    pub fn get(&self, index: usize) -> Option<&T> {
        if index + self.offset >= self.len {
            return None;
        }
        unsafe {
            Some(&*self.ptr.as_ptr().add(self.offset + index))
        }
    }

    /// Get mutable element at index with bounds checking.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index + self.offset >= self.len {
            return None;
        }
        unsafe {
            Some(&mut *self.ptr.as_ptr().add(self.offset + index))
        }
    }

    /// Create a sub-slice with bounds checking.
    pub fn slice(&self, start: usize, end: usize) -> Option<SafePtr<T>> {
        if start > end || end + self.offset > self.len {
            return None;
        }
        Some(SafePtr {
            ptr: self.ptr,
            len: self.len,
            offset: self.offset + start,
            generation: self.generation,
        })
    }

    /// Check if pointer is still valid (not freed).
    pub fn is_valid(&self, current_generation: u64) -> bool {
        self.generation == current_generation
    }

    /// Get remaining length from current offset.
    pub fn remaining(&self) -> usize {
        self.len - self.offset
    }
}

/// Memory arena for tracking allocations and generations.
pub struct MemoryArena {
    generation: u64,
    allocations: Vec<(u64, Vec<u8>)>,  // (generation, data)
}

impl MemoryArena {
    pub fn new() -> Self {
        MemoryArena {
            generation: 0,
            allocations: Vec::new(),
        }
    }

    pub fn alloc(&mut self, size: usize) -> SafePtr<u8> {
        self.generation += 1;
        let mut data = vec![0u8; size];
        let ptr = SafePtr::from_slice(&mut data, self.generation);
        self.allocations.push((self.generation, data));
        ptr
    }

    pub fn current_generation(&self) -> u64 {
        self.generation
    }
}
```

### Task 32.2: Bounds-Checked Memory Operations

**File:** `src/mir/interp.rs`

Replace raw pointer operations with SafePtr:

```rust
// Before:
fn array_get(&self, array: &Value, index: i64) -> Value {
    match array {
        Value::List(items) => {
            items.get(index as usize).cloned().unwrap_or(Value::Unit)
        }
        _ => Value::Unit,
    }
}

// After:
fn array_get(&self, array: &Value, index: i64) -> Result<Value, RuntimeError> {
    match array {
        Value::List(items) => {
            if index < 0 || index as usize >= items.len() {
                return Err(RuntimeError::IndexOutOfBounds {
                    index,
                    len: items.len(),
                    span: self.current_span(),
                });
            }
            Ok(items[index as usize].clone())
        }
        _ => Err(RuntimeError::TypeError {
            expected: "array".into(),
            found: array.type_name(),
            span: self.current_span(),
        }),
    }
}

// Add RuntimeError type:
#[derive(Debug)]
pub enum RuntimeError {
    IndexOutOfBounds {
        index: i64,
        len: usize,
        span: Span,
    },
    TypeError {
        expected: String,
        found: String,
        span: Span,
    },
    UseAfterFree {
        span: Span,
    },
    CapabilityRequired {
        capability: String,
        span: Span,
    },
}
```

### Task 32.3: Capability-Based FFI Access

**File:** `src/mir/interp.rs`

Require FFI capability for unsafe operations:

```rust
// Add to Interpreter struct:
pub struct Interpreter {
    // ... existing fields ...
    capabilities: HashSet<Capability>,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Capability {
    File(PathBuf),
    FileRead(PathBuf),
    FileWrite(PathBuf),
    Network(String, u16),  // host, port
    Env,
    EnvRead,
    EnvWrite,
    Spawn,
    Ffi(String),  // FFI module name
}

impl Interpreter {
    pub fn require_capability(&self, cap: &Capability) -> Result<(), RuntimeError> {
        // Check for exact capability or broader capability
        let has = match cap {
            Capability::FileRead(p) => {
                self.capabilities.contains(cap)
                    || self.capabilities.contains(&Capability::File(p.clone()))
            }
            Capability::FileWrite(p) => {
                self.capabilities.contains(cap)
                    || self.capabilities.contains(&Capability::File(p.clone()))
            }
            Capability::EnvRead => {
                self.capabilities.contains(cap)
                    || self.capabilities.contains(&Capability::Env)
            }
            Capability::EnvWrite => {
                self.capabilities.contains(cap)
                    || self.capabilities.contains(&Capability::Env)
            }
            _ => self.capabilities.contains(cap),
        };

        if has {
            Ok(())
        } else {
            Err(RuntimeError::CapabilityRequired {
                capability: format!("{:?}", cap),
                span: self.current_span(),
            })
        }
    }

    // Gate FFI calls:
    pub fn call_ffi(&mut self, module: &str, func: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.require_capability(&Capability::Ffi(module.to_string()))?;
        // ... proceed with FFI call
    }
}
```

---

## Sprint 33: Linear Types Foundation (5 tasks)

### Task 33.1: Add `linear` Keyword to Lexer

**File:** `src/lexer/scanner.rs`

Add `linear` and `affine` as reserved keywords:

```rust
// In keyword map:
fn keyword_kind(s: &str) -> Option<TokenKind> {
    match s {
        // ... existing keywords ...
        "linear" => Some(TokenKind::Linear),
        "affine" => Some(TokenKind::Affine),
        "move" => Some(TokenKind::Move),
        // ...
    }
}

// Add TokenKind variants:
pub enum TokenKind {
    // ... existing variants ...
    Linear,
    Affine,
    Move,
}
```

### Task 33.2: Add Linear Type Syntax to Parser

**File:** `src/parser/parser.rs`

Parse `linear T` and `affine T` type annotations:

```rust
// In parse_type:
fn parse_type(&mut self) -> ParseResult<TypeExpr> {
    // Check for linearity modifier
    let linearity = if self.check(TokenKind::Linear) {
        self.advance();
        LinearityKind::Linear
    } else if self.check(TokenKind::Affine) {
        self.advance();
        LinearityKind::Affine
    } else {
        LinearityKind::Regular
    };

    // Parse base type
    let base_type = self.parse_base_type()?;

    Ok(TypeExpr {
        kind: base_type.kind,
        linearity,
        span: base_type.span,
    })
}

// In AST types:
#[derive(Clone, Debug, PartialEq)]
pub struct TypeExpr {
    pub kind: TypeKind,
    pub linearity: LinearityKind,
    pub span: Span,
}
```

### Task 33.3: LinearityKind Enum

**File:** `src/types/mod.rs`

```rust
/// Linearity classification for types.
///
/// - Linear: Must be used exactly once (like Rust's ownership without Copy)
/// - Affine: Must be used at most once (can be dropped without use)
/// - Regular: Can be used any number of times (has implicit Copy/Clone)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum LinearityKind {
    Linear,   // use exactly once
    Affine,   // use at most once
    Regular,  // use freely
}

impl LinearityKind {
    /// Can this value be implicitly copied?
    pub fn can_copy(&self) -> bool {
        matches!(self, LinearityKind::Regular)
    }

    /// Can this value be implicitly dropped?
    pub fn can_drop(&self) -> bool {
        !matches!(self, LinearityKind::Linear)
    }

    /// Combine linearities (for composite types).
    pub fn combine(self, other: LinearityKind) -> LinearityKind {
        match (self, other) {
            (LinearityKind::Linear, _) | (_, LinearityKind::Linear) => LinearityKind::Linear,
            (LinearityKind::Affine, _) | (_, LinearityKind::Affine) => LinearityKind::Affine,
            _ => LinearityKind::Regular,
        }
    }
}

// Update Ty enum:
#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    // ... existing variants ...
    Linear(Box<Ty>),   // linear T
    Affine(Box<Ty>),   // affine T
}
```

### Task 33.4: Use-Tracking in Type Checker

**File:** `src/types/inference.rs`

Track linear value usage:

```rust
// Add to InferenceContext:
pub struct InferenceContext {
    // ... existing fields ...

    /// Track usage of linear/affine variables
    /// Key: variable name, Value: (linearity, use_count, defined_at, used_at)
    linear_tracking: HashMap<String, LinearTracking>,
}

#[derive(Clone, Debug)]
struct LinearTracking {
    linearity: LinearityKind,
    use_count: usize,
    defined_at: Span,
    first_use: Option<Span>,
    moved_at: Option<Span>,
}

impl InferenceContext {
    fn track_linear_def(&mut self, name: &str, linearity: LinearityKind, span: Span) {
        if linearity != LinearityKind::Regular {
            self.linear_tracking.insert(name.to_string(), LinearTracking {
                linearity,
                use_count: 0,
                defined_at: span,
                first_use: None,
                moved_at: None,
            });
        }
    }

    fn track_linear_use(&mut self, name: &str, span: Span) -> Result<(), TypeError> {
        if let Some(tracking) = self.linear_tracking.get_mut(name) {
            // Check if already moved
            if let Some(moved_at) = tracking.moved_at {
                return Err(TypeError::UseAfterMove {
                    name: name.to_string(),
                    original_use: moved_at,
                    second_use: span,
                });
            }

            tracking.use_count += 1;
            if tracking.first_use.is_none() {
                tracking.first_use = Some(span);
            }

            // For linear types, mark as moved on first use
            if tracking.linearity == LinearityKind::Linear
                || tracking.linearity == LinearityKind::Affine
            {
                tracking.moved_at = Some(span);
            }
        }
        Ok(())
    }
}
```

### Task 33.5: Consume-Exactly-Once Validation

**File:** `src/types/inference.rs`

At scope exit, verify all linear values consumed:

```rust
impl InferenceContext {
    fn check_linear_scope_exit(&mut self, scope_span: Span) -> Vec<TypeError> {
        let mut errors = Vec::new();

        for (name, tracking) in &self.linear_tracking {
            match tracking.linearity {
                LinearityKind::Linear => {
                    if tracking.use_count == 0 {
                        errors.push(TypeError::LinearValueUnused {
                            name: name.clone(),
                            defined_at: tracking.defined_at,
                            scope_end: scope_span,
                        });
                    } else if tracking.use_count > 1 {
                        errors.push(TypeError::LinearValueUsedMultipleTimes {
                            name: name.clone(),
                            use_count: tracking.use_count,
                            defined_at: tracking.defined_at,
                        });
                    }
                }
                LinearityKind::Affine => {
                    if tracking.use_count > 1 {
                        errors.push(TypeError::AffineValueUsedMultipleTimes {
                            name: name.clone(),
                            use_count: tracking.use_count,
                            defined_at: tracking.defined_at,
                        });
                    }
                }
                LinearityKind::Regular => {}
            }
        }

        errors
    }
}

// Add TypeError variants:
pub enum TypeError {
    // ... existing variants ...

    UseAfterMove {
        name: String,
        original_use: Span,
        second_use: Span,
    },
    LinearValueUnused {
        name: String,
        defined_at: Span,
        scope_end: Span,
    },
    LinearValueUsedMultipleTimes {
        name: String,
        use_count: usize,
        defined_at: Span,
    },
    AffineValueUsedMultipleTimes {
        name: String,
        use_count: usize,
        defined_at: Span,
    },
}
```

---

## Sprint 34: Capability System (4 tasks)

### Task 34.1: Capability Type Hierarchy

**File:** `src/types/mod.rs`

```rust
use std::path::PathBuf;

/// Capability types for effect tracking and permission control.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Capability {
    /// File system access
    File(FileCapability),

    /// Network access
    Network(NetworkCapability),

    /// Environment variable access
    Env(EnvCapability),

    /// Process spawning
    Spawn,

    /// FFI access to specific module
    Ffi(String),

    /// Custom capability
    Custom(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileCapability {
    pub path: PathBuf,
    pub read: bool,
    pub write: bool,
    pub execute: bool,
}

impl FileCapability {
    pub fn read_only(path: PathBuf) -> Self {
        FileCapability { path, read: true, write: false, execute: false }
    }

    pub fn write_only(path: PathBuf) -> Self {
        FileCapability { path, read: false, write: true, execute: false }
    }

    pub fn read_write(path: PathBuf) -> Self {
        FileCapability { path, read: true, write: true, execute: false }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NetworkCapability {
    pub host: String,
    pub port: Option<u16>,  // None = all ports
    pub protocol: Protocol,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Protocol {
    Tcp,
    Udp,
    Http,
    Https,
    Any,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EnvCapability {
    Read(Option<String>),   // None = all vars, Some(name) = specific var
    Write(Option<String>),
    Full,
}
```

### Task 34.2: FileCapability Implementation

**File:** `src/mir/interp.rs`

Gate file operations on FileCapability:

```rust
impl Interpreter {
    fn builtin_read_file(&mut self, path: &str) -> Result<Value, RuntimeError> {
        let path = PathBuf::from(path);

        // Require read capability
        self.require_capability(&Capability::File(FileCapability::read_only(path.clone())))?;

        match std::fs::read_to_string(&path) {
            Ok(content) => Ok(Value::String(content)),
            Err(e) => Err(RuntimeError::IoError {
                operation: "read_file",
                path: path.clone(),
                error: e.to_string(),
            }),
        }
    }

    fn builtin_write_file(&mut self, path: &str, content: &str) -> Result<Value, RuntimeError> {
        let path = PathBuf::from(path);

        // Require write capability
        self.require_capability(&Capability::File(FileCapability::write_only(path.clone())))?;

        match std::fs::write(&path, content) {
            Ok(()) => Ok(Value::Unit),
            Err(e) => Err(RuntimeError::IoError {
                operation: "write_file",
                path: path.clone(),
                error: e.to_string(),
            }),
        }
    }
}
```

### Task 34.3: NetworkCapability Implementation

**File:** `src/mir/interp.rs`

Gate network operations on NetworkCapability:

```rust
impl Interpreter {
    fn builtin_http_get(&mut self, url: &str) -> Result<Value, RuntimeError> {
        // Parse URL to extract host and port
        let url_parsed = url::Url::parse(url)
            .map_err(|e| RuntimeError::InvalidUrl { url: url.to_string(), error: e.to_string() })?;

        let host = url_parsed.host_str()
            .ok_or(RuntimeError::InvalidUrl { url: url.to_string(), error: "no host".into() })?;

        let port = url_parsed.port_or_known_default();
        let protocol = match url_parsed.scheme() {
            "http" => Protocol::Http,
            "https" => Protocol::Https,
            _ => return Err(RuntimeError::UnsupportedProtocol { scheme: url_parsed.scheme().into() }),
        };

        // Require network capability
        self.require_capability(&Capability::Network(NetworkCapability {
            host: host.to_string(),
            port,
            protocol,
        }))?;

        // Proceed with HTTP request
        // ... implementation using reqwest or similar
    }

    fn builtin_tcp_connect(&mut self, host: &str, port: u16) -> Result<Value, RuntimeError> {
        self.require_capability(&Capability::Network(NetworkCapability {
            host: host.to_string(),
            port: Some(port),
            protocol: Protocol::Tcp,
        }))?;

        // ... TCP connection implementation
    }
}
```

### Task 34.4: Main Function Capability Injection

**File:** `src/mir/interp.rs`

Pass capabilities to main() based on permissions:

```rust
impl Interpreter {
    /// Create interpreter with capabilities from config.
    pub fn with_capabilities(config: &CapabilityConfig) -> Self {
        let mut interp = Interpreter::new();
        interp.capabilities = config.to_capability_set();
        interp
    }

    /// Run program with specific capabilities.
    pub fn run_with_capabilities(
        &mut self,
        program: Program,
        capabilities: HashSet<Capability>,
    ) -> Result<Value, RuntimeError> {
        self.capabilities = capabilities;
        self.run(program)
    }
}

/// Configuration for capability grants.
#[derive(Clone, Debug)]
pub struct CapabilityConfig {
    /// File paths with their permissions
    pub files: Vec<(PathBuf, FilePermissions)>,

    /// Network hosts/ports allowed
    pub network: Vec<NetworkRule>,

    /// Environment access
    pub env: EnvAccess,

    /// Can spawn processes
    pub spawn: bool,

    /// Allowed FFI modules
    pub ffi_modules: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct FilePermissions {
    pub read: bool,
    pub write: bool,
}

#[derive(Clone, Debug)]
pub enum NetworkRule {
    AllowHost(String),
    AllowHostPort(String, u16),
    AllowAll,
    DenyAll,
}

#[derive(Clone, Debug)]
pub enum EnvAccess {
    None,
    ReadOnly,
    ReadWrite,
    Specific(Vec<String>),
}

// CLI integration:
fn run_with_capability_flags(path: &Path, args: &CliArgs) -> Result<Value, Error> {
    let mut config = CapabilityConfig::default();

    // Parse capability flags
    if args.allow_all_files {
        config.files.push((PathBuf::from("/"), FilePermissions { read: true, write: true }));
    }
    if let Some(ref paths) = args.allow_read {
        for p in paths {
            config.files.push((PathBuf::from(p), FilePermissions { read: true, write: false }));
        }
    }
    if args.allow_network {
        config.network.push(NetworkRule::AllowAll);
    }
    if args.allow_env {
        config.env = EnvAccess::ReadWrite;
    }

    let interp = Interpreter::with_capabilities(&config);
    // ... run program
}
```

---

## FORMA Syntax Examples

After implementing Sprints 32-34:

```forma
# Linear type - must be used exactly once
f use_resource(r: linear Resource) -> Result<(), Error>
    r.consume()  # OK - used exactly once

# Affine type - can be dropped, but not used twice
f maybe_use(r: affine Resource) -> Unit
    if condition
        r.consume()  # OK - used once
    # OK if not used - affine can be dropped

# Error: using linear value twice
f bad_linear(r: linear Resource) -> Unit
    r.foo()  # First use - moves r
    r.bar()  # ERROR: use after move

# Capability-gated file access
async f read_config(cap: FileCapability<"/etc/app.conf">) -> String
    read_file("/etc/app.conf")  # OK - have capability

# Main with capabilities
f main(
    file_cap: FileCapability<"./data">,
    net_cap: NetworkCapability<"api.example.com", 443>
) -> Int
    # Can read/write ./data directory
    # Can connect to api.example.com:443
    # Cannot access other files or network
    0
```

---

## Verification

After implementing all tasks:

1. Run the test suite:
```bash
cd forma && cargo test
```

2. Test linear types:
```forma
# test_linear.forma
f test_linear()
    let r: linear Resource = Resource::new()
    r.use()  # OK
    # r.use()  # Uncomment to test error

f test_affine()
    let r: affine Resource = Resource::new()
    if true
        r.use()
    # OK - affine can be dropped
```

3. Test capabilities:
```bash
# Run with no capabilities - should fail
cargo run -- run test_file.forma

# Run with file capability
cargo run -- run --allow-read ./data test_file.forma
```

---

## Summary

| Sprint | Task | Description |
|--------|------|-------------|
| 32 | 32.1 | SafePtr wrapper |
| 32 | 32.2 | Bounds-checked memory ops |
| 32 | 32.3 | Capability-based FFI |
| 33 | 33.1 | `linear` keyword |
| 33 | 33.2 | Linear type syntax |
| 33 | 33.3 | LinearityKind enum |
| 33 | 33.4 | Use-tracking |
| 33 | 33.5 | Scope exit validation |
| 34 | 34.1 | Capability type hierarchy |
| 34 | 34.2 | FileCapability |
| 34 | 34.3 | NetworkCapability |
| 34 | 34.4 | Main capability injection |

**Total:** 12 tasks across 3 sprints

---

## Claude Code Prompt

```
Sprints 32-34: Memory Safety for FORMA

Working directory: forma/

## Sprint 32: FFI Safety Layer

### 32.1: Create SafePtr Wrapper
New file: src/ffi/safe_ptr.rs
Create SafePtr<T> with:
- Bounds checking (get, get_mut, slice)
- Generation tracking for use-after-free detection
- MemoryArena for allocation tracking

### 32.2: Bounds-Checked Memory Operations
File: src/mir/interp.rs
Replace array indexing with bounds-checked version.
Add RuntimeError enum with IndexOutOfBounds, UseAfterFree variants.

### 32.3: Capability-Based FFI Access
File: src/mir/interp.rs
Add capabilities field to Interpreter.
Add require_capability() method.
Gate FFI calls with capability check.

## Sprint 33: Linear Types Foundation

### 33.1: Add linear/affine Keywords
File: src/lexer/scanner.rs
Add "linear", "affine", "move" to keyword map.
Add TokenKind::Linear, Affine, Move.

### 33.2: Linear Type Syntax
File: src/parser/parser.rs
In parse_type(), check for Linear/Affine token before base type.
Add linearity field to TypeExpr.

### 33.3: LinearityKind Enum
File: src/types/mod.rs
Add LinearityKind { Linear, Affine, Regular } with:
- can_copy(), can_drop() methods
- combine() for composite types

### 33.4: Use-Tracking
File: src/types/inference.rs
Add linear_tracking map to InferenceContext.
Track definitions with track_linear_def().
Check uses with track_linear_use() - error on use-after-move.

### 33.5: Scope Exit Validation
File: src/types/inference.rs
Add check_linear_scope_exit() that verifies:
- Linear values used exactly once
- Affine values used at most once
Add TypeError variants for violations.

## Sprint 34: Capability System

### 34.1: Capability Type Hierarchy
File: src/types/mod.rs
Add Capability enum with File, Network, Env, Spawn, Ffi variants.
Add FileCapability, NetworkCapability, EnvCapability structs.

### 34.2: FileCapability Implementation
File: src/mir/interp.rs
Gate read_file, write_file on FileCapability.
Check path matches capability path.

### 34.3: NetworkCapability Implementation
File: src/mir/interp.rs
Gate http_get, tcp_connect on NetworkCapability.
Parse URL to extract host/port for capability check.

### 34.4: Main Capability Injection
File: src/mir/interp.rs
Add CapabilityConfig struct.
Add --allow-read, --allow-write, --allow-network CLI flags.
Pass capabilities to main() based on flags.

## Testing
After each sprint:
1. cargo test
2. Test linear type errors (use-after-move, unused)
3. Test capability denials (no capability -> error)
4. Test capability grants (with capability -> success)

All 251+ existing tests must continue to pass.
```
