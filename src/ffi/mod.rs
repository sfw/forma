//! FFI Safety Layer for FORMA.
//!
//! Provides memory-safe wrappers for foreign function interface operations:
//! - `SafePtr<T>`: Bounds-checked pointer with generation tracking
//! - `MemoryArena`: Allocation tracking with use-after-free detection
//! - `Capability`: Permission-based access control for FFI operations

pub mod safe_ptr;

pub use safe_ptr::{MemoryArena, SafePtr, PtrError};
