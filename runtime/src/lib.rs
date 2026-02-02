//! FORMA Runtime Library
//!
//! This library provides the runtime functions needed by LLVM-compiled FORMA programs.
//! All functions use the C ABI for compatibility with the generated LLVM IR.

#![allow(clippy::missing_safety_doc)]

pub mod io;
pub mod math;
pub mod memory;
pub mod panic;
pub mod string;

// Re-export all public functions at the crate root for convenience
pub use io::*;
pub use math::*;
pub use memory::*;
pub use panic::*;
pub use string::*;
